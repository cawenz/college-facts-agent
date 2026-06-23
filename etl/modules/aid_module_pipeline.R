# =============================================================================
# Financial Aid Module   (repo version)
# Holy Cross peer-comparison project
#
# Reads:  output/schools.csv   (built by R/schools_pipeline.R)
#         data/IPEDS*.Rda      (committed in the repo)
#         Academic Insights API for the 2 CDS metrics
#
# Writes: output/aid_facts.csv      one row per unitid x year x metric
#         output/aid_variables.csv  one row per metric (metadata)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(httr2); library(jsonlite)
})

# Source shared schools/IPEDS helpers

# ---- CONFIG ---------------------------------------------------------------
AID_CONFIG <- list(
  collection_years = 2020:2024,
  ai_dataset = "undergraduate",
  ai_key     = Sys.getenv("ACADEMIC_INSIGHTS_API_KEY"),
  ai_base    = "https://ai.usnews.com/api/v1/client_api",
  ai_metric_ids = list(
    pct_need_met       = 374,
    pct_need_fully_met = 366
  )
)

# =============================================================================
# 1. Table-name resolution + helpers
# =============================================================================
SFX <- c(`2020`="1920",`2021`="2021",`2022`="2122",`2023`="2223",`2024`="2324")
tbl_for <- function(stem, year) {
  s <- SFX[as.character(year)]
  switch(stem,
         HD        = paste0("HD", year),
         SFA_FT    = if (year == 2024) "SFA2324" else paste0("SFA", s, "_P1"),
         SFA_NP    = if (year == 2024) "COST2_2024_NetPrice" else paste0("SFA", s, "_P2"),
         COST_TUIT = if (year == 2024) "COST1_2024" else paste0("IC", year, "_AY"),
         stem)
}

col_of <- function(stem, year, varname) {
  df <- get_table(year, tbl_for(stem, year))
  if (is.null(df)) return(tibble(unitid = integer(), value = numeric()))
  vc <- names(df)[names(df) == toupper(varname)][1]
  if (is.na(vc)) { warning(sprintf("%s absent from %s (collection %d)",
                                   varname, tbl_for(stem, year), year))
    return(tibble(unitid = integer(), value = numeric())) }
  df %>% transmute(unitid, value = suppressWarnings(as.numeric(.data[[vc]])))
}

newest_netprice <- function(df, base) {
  cand <- grep(paste0("^", base, "[0-9]$"), names(df), value = TRUE)
  if (!length(cand)) return(NA_character_)
  cand[order(as.integer(str_sub(cand, -1)), decreasing = TRUE)][1]
}

# =============================================================================
# 2. The 8 IPEDS aid metrics
# =============================================================================
build_ipeds_aid <- function(unitids_by_year, cfg) {
  message("Pulling 9 IPEDS aid metrics ...")
  out <- list()
  for (yr in cfg$collection_years) {
    uids <- unitids_by_year[[as.character(yr)]]
    
    direct <- c(pct_pell = "PGRNT_P", pct_any_grant = "AGRNT_P",
                avg_inst_grant = "IGRNT_A", pct_federal_loan = "FLOAN_P",
                avg_federal_loan = "FLOAN_A",
                pell_count = "UPGRNTN")
    for (nm in names(direct)) {
      d <- col_of("SFA_FT", yr, direct[[nm]]) %>% filter(unitid %in% uids)
      if (nrow(d)) out[[length(out)+1]] <-
          d %>% mutate(year = yr, metric = nm, var_type = "raw")
    }
    
    np <- get_table(yr, tbl_for("SFA_NP", yr))
    if (!is.null(np)) {
      # IPEDS Net Price columns are sector-specific. Public 4-yr schools
      # report under NPIST / NPIS41 (in-state Title IV recipients);
      # private nonprofits report under NPGRN / NPT41 (grant recipients,
      # no in-state distinction). We pull both column families and
      # coalesce per row so each institution picks up the column its
      # sector actually populates. Definitions differ slightly across
      # sectors (Title IV vs grant-recipient cohort); the documented
      # caveat lives in aid_variables.csv's coverage_note.
      v_pub_all <- newest_netprice(np, "NPIST")
      v_pri_all <- newest_netprice(np, "NPGRN")
      v_pub_low <- newest_netprice(np, "NPIS41")
      v_pri_low <- newest_netprice(np, "NPT41")

      pull_coalesced <- function(np, col_pub, col_pri, metric_name) {
        if (is.na(col_pub) && is.na(col_pri)) return(NULL)
        np %>%
          mutate(
            .pub = if (!is.na(col_pub))
              suppressWarnings(as.numeric(.data[[col_pub]])) else NA_real_,
            .pri = if (!is.na(col_pri))
              suppressWarnings(as.numeric(.data[[col_pri]])) else NA_real_
          ) %>%
          transmute(unitid, value = coalesce(.pub, .pri)) %>%
          filter(unitid %in% uids, !is.na(value)) %>%
          mutate(year = yr, metric = metric_name, var_type = "raw")
      }

      d <- pull_coalesced(np, v_pub_all, v_pri_all, "avg_net_price_aided")
      if (!is.null(d) && nrow(d)) out[[length(out)+1]] <- d
      d <- pull_coalesced(np, v_pub_low, v_pri_low, "avg_net_price_income_0_30k")
      if (!is.null(d) && nrow(d)) out[[length(out)+1]] <- d
    }
  }
  raw <- bind_rows(out)
  
  tuition <- map_dfr(cfg$collection_years, function(yr) {
    td <- get_table(yr, tbl_for("COST_TUIT", yr))
    if (is.null(td)) return(tibble())
    nm <- names(td)
    if ("TUITION2" %in% nm) {
      fee <- if ("FEE2" %in% nm) suppressWarnings(as.numeric(td$FEE2)) else 0
      td %>% transmute(unitid, year = yr,
                       tuition_fees = suppressWarnings(as.numeric(TUITION2)) + fee)
    } else {
      cand <- grep("^CHG1PY[0-9]$", nm, value = TRUE)
      if (!length(cand)) return(tibble())
      v <- cand[order(as.integer(str_sub(cand, -1)), decreasing = TRUE)][1]
      td %>% transmute(unitid, year = yr,
                       tuition_fees = suppressWarnings(as.numeric(.data[[v]])))
    }
  })
  discount <- raw %>% filter(metric == "avg_inst_grant") %>%
    select(unitid, year, inst_grant = value) %>%
    inner_join(tuition, by = c("unitid","year")) %>%
    transmute(unitid, year, metric = "inst_discount_rate",
              value = inst_grant / tuition_fees, var_type = "computed") %>%
    filter(is.finite(value))
  
  bind_rows(raw, discount)
}

# =============================================================================
# 3. The 2 CDS metrics (paged by metric x year)
# =============================================================================
search_ai_metrics <- function(cfg, contains = NULL) {
  q <- list(); if (!is.null(contains)) q$description_contains <- contains
  as_tibble(ai_get(cfg, paste0("metrics/", cfg$ai_dataset), query = q))
}

build_cds_aid <- function(cfg) {
  ids <- unlist(cfg$ai_metric_ids); ids <- ids[!is.na(ids)]
  if (!length(ids)) { message("No CDS metric IDs set - skipping."); return(tibble()) }
  message("Pulling 2 CDS metrics (paged by metric x year, IPEDS->AI year mapping) ...")
  # Iterate over our IPEDS-naming years but ask AI for each year's AI-equivalent.
  grid <- expand.grid(metric_id = ids, ipeds_year = cfg$collection_years)
  facts <- pmap_dfr(grid, function(metric_id, ipeds_year) {
    ai_year <- ipeds_to_ai_year(ipeds_year)
    res <- ai_get(cfg, paste0("facts/", cfg$ai_dataset), query = list(
      metric_ids = metric_id, years = ai_year, all_data = "true"))
    df <- as_tibble(res)
    n  <- nrow(df)
    if (n >= 5000)
      warning(sprintf("metric %s / AI %d (IPEDS %d) returned %d rows - likely TRUNCATED",
                      metric_id, ai_year, ipeds_year, n))
    message(sprintf("  metric %s / AI %d (IPEDS %d): %d rows",
                    metric_id, ai_year, ipeds_year, n))
    if (!n) return(tibble())
    df %>% mutate(year = ipeds_year)   # store under our IPEDS-convention year
  })
  if (!nrow(facts) || !"school_ipeds_id" %in% names(facts))
    return(tibble())
  id_map <- tibble(metric_id = unlist(cfg$ai_metric_ids),
                   metric    = names(cfg$ai_metric_ids))
  facts %>%
    transmute(unitid = as.integer(school_ipeds_id), year = as.integer(year),
              metric_id = as.integer(metric_id), value = as.numeric(value)) %>%
    inner_join(id_map, by = "metric_id") %>%
    transmute(unitid, year, metric, value, var_type = "external")
}

# =============================================================================
# 4. aid_variables.csv  - the variable metadata for this module
# =============================================================================
build_aid_variables <- function() {
  tribble(
    ~metric,                       ~display_name,                                            ~source,         ~ipeds_table_or_formula,                                ~use_type,     ~comparison_scope,  ~format,       ~neche_peer_set, ~neche_dashboard, ~coverage_note,
    "avg_net_price_aided",         "Average net price - all aided students",                 "ipeds",         "SFA_NP: NPIST# (public) coalesced with NPGRN# (private)", "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            "Public institutions report under NPIST (in-state Title IV recipients); private nonprofits report under NPGRN (grant recipients). Definitions are very similar but not identical.",
    "avg_net_price_income_0_30k",  "Average net price - lowest income band ($0-30k)",        "ipeds",         "SFA_NP: NPIS41# (public) coalesced with NPT41# (private)", "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            "Public institutions report under NPIS41 (in-state); private nonprofits report under NPT41. Income band 1 (0-30k) in both.",
    "pct_pell",                    "Percent receiving Pell grants (first-time full-time UG)","ipeds",         "SFA_FT.PGRNT_P",                                       "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             NA_character_,
    "pell_count",                  "Number of all undergraduates awarded Pell grants",       "ipeds",         "SFA_FT.UPGRNTN",                                       "descriptive", "within_category",  "count",       TRUE,            TRUE,             "All-UG population; pct_pell measures first-time full-time only",
    "pct_any_grant",               "Percent receiving any grant or scholarship aid",         "ipeds",         "SFA_FT.AGRNT_P",                                       "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            NA_character_,
    "avg_inst_grant",              "Average institutional grant per recipient",              "ipeds",         "SFA_FT.IGRNT_A",                                       "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            NA_character_,
    "inst_discount_rate",          "Institutional discount rate",                            "ipeds_derived", "SFA_FT.IGRNT_A / (TUITION2 + FEE2 or CHG1PY*)",        "clustering",  "cross_category",   "ratio",       FALSE,           FALSE,            "In-state tuition used for public institutions",
    "pct_federal_loan",            "Percent borrowing federal loans",                        "ipeds",         "SFA_FT.FLOAN_P",                                       "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            NA_character_,
    "avg_federal_loan",            "Average federal loan per borrower",                      "ipeds",         "SFA_FT.FLOAN_A",                                       "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            NA_character_,
    "pct_need_met",                "Average percent of need met (freshmen)",                 "cds_ai",        "Academic Insights metric_id 374",                      "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Survey respondents only (~45%)",
    "pct_need_fully_met",          "Percent of students whose need was fully met (freshmen)","cds_ai",        "Academic Insights metric_id 366",                      "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Survey respondents only (~45%)"
  ) %>%
    mutate(category = "aid", notes = NA_character_) %>%
    select(metric, category, display_name, source, ipeds_table_or_formula,
           use_type, comparison_scope, format, neche_peer_set, neche_dashboard,
           coverage_note, notes)
}

# =============================================================================
# 5. COVERAGE REPORT
# =============================================================================
aid_coverage_report <- function(facts, schools) {
  # Each metric's target year set is inferred from the data itself: a metric's
  # "universe" is schools x the years that metric actually populates. This
  # treats single-snapshot variables (1 year) and multi-year variables (5
  # years) on equal footing - both report coverage relative to their target
  # year range rather than the full panel.
  metric_year_pairs <- facts %>% distinct(metric, year)
  
  schools_min <- schools %>% transmute(unitid, control_grp)
  uni <- metric_year_pairs %>% tidyr::crossing(schools_min)
  
  have <- facts %>% distinct(unitid, year, metric) %>% mutate(has_data = TRUE)
  
  cov <- uni %>%
    left_join(have, by = c("unitid", "year", "metric")) %>%
    mutate(has_data = !is.na(has_data)) %>%
    group_by(metric, control_grp) %>%
    summarise(pct_covered = round(100 * mean(has_data)),
              n_with_data = sum(has_data),
              n_universe  = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = control_grp,
                       values_from = c(pct_covered, n_with_data, n_universe))
  
  # n_years column makes single-snapshot variables visible at a glance
  years_per_metric <- metric_year_pairs %>% count(metric, name = "n_years")
  cov %>% left_join(years_per_metric, by = "metric") %>% arrange(metric)
}

# =============================================================================
# 6. RUN
# =============================================================================
run_aid_module <- function(cfg = AID_CONFIG) {
  message("== Financial Aid Module ==")
  
  schools_csv <- .out_path("schools.csv")
  if (!file.exists(schools_csv))
    stop(schools_csv, " not found. Run build_schools() first.")
  schools <- as_tibble(read.csv(schools_csv, stringsAsFactors = FALSE))
  message(sprintf("  loaded %s: %d institutions", schools_csv, nrow(schools)))
  
  unitids_by_year <- map(cfg$collection_years, function(yr) {
    hd <- get_table(yr, paste0("HD", yr))
    if (is.null(hd)) return(integer())
    hd %>% filter(suppressWarnings(as.integer(SECTOR)) %in% c(1, 2)) %>%
      pull(unitid) %>% as.integer()
  }) %>% setNames(as.character(cfg$collection_years))
  
  ipeds_aid <- build_ipeds_aid(unitids_by_year, cfg)
  cds_aid   <- build_cds_aid(cfg)
  aid_facts <- bind_rows(ipeds_aid, cds_aid) %>%
    semi_join(schools, by = "unitid") %>%
    arrange(unitid, year, metric)
  
  aid_variables <- build_aid_variables()
  
  write.csv(aid_facts,     .out_path("aid_facts.csv"),     row.names = FALSE)
  write.csv(aid_variables, .out_path("aid_variables.csv"), row.names = FALSE)
  message(sprintf("Wrote %s, %s",
                  .out_path("aid_facts.csv"), .out_path("aid_variables.csv")))
  
  message("\nCoverage (% of universe with a value), by metric and control group:")
  print(aid_coverage_report(aid_facts, schools))
  
  invisible(list(facts = aid_facts, variables = aid_variables, schools = schools))
}

# -----------------------------------------------------------------------------
# Usage:
#   res_aid <- run_aid_module()
# -----------------------------------------------------------------------------