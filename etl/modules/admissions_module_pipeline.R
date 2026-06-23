# =============================================================================
# Admissions Module
# Holy Cross peer-comparison project
#
# Reads:  output/schools.csv  (built by R/schools_pipeline.R)
#         data/IPEDS*.Rda      (committed in the repo)
#         Academic Insights API for the 3 CDS-unique metrics
#
# Writes: output/adm_facts.csv      one row per unitid x year x metric
#         output/adm_variables.csv  one row per metric (metadata)
#
# Variables:
#   IPEDS  - acceptance_rate, yield_rate, application_volume
#            sat_mid50 (descriptive only), act_mid50 (descriptive only)
#            pct_submitting_sat, pct_submitting_act
#            yield_men_vs_women (exploratory)
#   CDS/AI - pct_top10_hs, ed_acceptance_rate (raw),
#            ed_share_of_applications (derived). Metric IDs confirmed
#            at build time via search_ai_metrics; start as NA placeholders)
#
# The Carnegie academic-concentration pair (apm_max_cip2percent,
# apm_max_cip2_name) is an institutional attribute and lives in schools.csv,
# not in adm_facts. Documented in the methodology as admissions-relevant.
#
# YEAR-AWARE VARIABLE HANDLING (similar to aid module)
#   * APPLCN / ADMSSN / ENRLT are direct columns in 2020-21 ADM, but in 2024
#     and forward they're absent - replaced by gender breakouts (APPLCNM,
#     APPLCNW, APPLCNAN, APPLCNUN). The pipeline tries the total first and
#     falls back to summing components.
#   * SAT/ACT 50th-percentile scores (SATVR50, SATMT50, ACTCM50) exist in
#     newer collections but not 2020-21. Falls back to 25/75 midpoint.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(httr2); library(jsonlite)
})

# Source shared schools / IPEDS helpers (load_collection, get_table, ai_get)
# source("R/schools_pipeline.R")

# ---- CONFIG ---------------------------------------------------------------
ADM_CONFIG <- list(
  collection_years = 2020:2024,
  ai_dataset = "undergraduate",
  ai_key     = Sys.getenv("ACADEMIC_INSIGHTS_API_KEY"),
  ai_base    = "https://ai.usnews.com/api/v1/client_api",
  # CDS-unique metric IDs from Academic Insights.
  # pct_top10_hs and ed_acceptance_rate are pulled directly and written as
  # facts. ed_applicants_count is pulled internally as scaffolding for
  # ed_share_of_applications - it is NOT written to facts.
  ai_metric_ids = list(
    pct_top10_hs       = 8L,            # confirmed
    ed_acceptance_rate = 100L,          # confirmed
    ed_applicants_count = 194L          # internal use only (not in facts)
  ),
  # which IDs become facts (the others are pulled but not written out)
  ai_facts_metrics = c("pct_top10_hs", "ed_acceptance_rate")
)

# =============================================================================
# 1. Year-aware ADM table helpers
# =============================================================================

# Get an ADM column, returning (unitid, value). Tries the direct column first;
# if absent, sums a list of fallback component columns (e.g. gender breakouts).
adm_col_or_sum <- function(year, total_col, fallback_cols = NULL) {
  adm <- get_table(year, paste0("ADM", year))
  if (is.null(adm)) return(tibble(unitid = integer(), value = numeric()))
  if (total_col %in% names(adm)) {
    return(adm %>% transmute(unitid,
                             value = suppressWarnings(as.numeric(.data[[total_col]]))))
  }
  if (length(fallback_cols)) {
    present <- intersect(fallback_cols, names(adm))
    if (length(present)) {
      vals <- adm %>%
        select(unitid, all_of(present)) %>%
        mutate(across(-unitid, ~ suppressWarnings(as.numeric(.x))))
      return(vals %>%
               mutate(value = rowSums(across(-unitid), na.rm = TRUE)) %>%
               select(unitid, value))
    }
  }
  warning(sprintf("ADM%d has neither %s nor any of %s",
                  year, total_col, paste(fallback_cols, collapse = "/")))
  tibble(unitid = integer(), value = numeric())
}

# Pull a direct column from DRVADM if available (precomputed rates).
drvadm_col <- function(year, varname) {
  drv <- get_table(year, paste0("DRVADM", year))
  if (is.null(drv) || !varname %in% names(drv))
    return(tibble(unitid = integer(), value = numeric()))
  drv %>% transmute(unitid, value = suppressWarnings(as.numeric(.data[[varname]])))
}

# Compute a SAT/ACT mid50 score across the relevant ADM columns. Uses the
# 50th percentile directly when present (newer collections); otherwise the
# 25/75 midpoint. For SAT: sum(verbal mid + math mid). For ACT: composite mid.
adm_sat_mid50 <- function(year) {
  adm <- get_table(year, paste0("ADM", year))
  if (is.null(adm)) return(tibble(unitid = integer(), value = numeric()))
  mid <- function(col50, col25, col75) {
    if (col50 %in% names(adm))
      suppressWarnings(as.numeric(adm[[col50]]))
    else if (col25 %in% names(adm) && col75 %in% names(adm))
      (suppressWarnings(as.numeric(adm[[col25]])) +
         suppressWarnings(as.numeric(adm[[col75]]))) / 2
    else rep(NA_real_, nrow(adm))
  }
  vr <- mid("SATVR50", "SATVR25", "SATVR75")
  mt <- mid("SATMT50", "SATMT25", "SATMT75")
  tibble(unitid = adm$unitid, value = vr + mt) %>% filter(is.finite(value))
}
adm_act_mid50 <- function(year) {
  adm <- get_table(year, paste0("ADM", year))
  if (is.null(adm)) return(tibble(unitid = integer(), value = numeric()))
  v <- if ("ACTCM50" %in% names(adm))
    suppressWarnings(as.numeric(adm$ACTCM50))
  else if (all(c("ACTCM25","ACTCM75") %in% names(adm)))
    (suppressWarnings(as.numeric(adm$ACTCM25)) +
       suppressWarnings(as.numeric(adm$ACTCM75))) / 2
  else rep(NA_real_, nrow(adm))
  tibble(unitid = adm$unitid, value = v) %>% filter(is.finite(value))
}

# =============================================================================
# 2. The 7 IPEDS admissions metrics + 1 exploratory
# =============================================================================
build_ipeds_admissions <- function(unitids_by_year, cfg) {
  message("Pulling IPEDS admissions metrics ...")
  out <- list()
  
  for (yr in cfg$collection_years) {
    uids <- unitids_by_year[[as.character(yr)]]
    
    # raw totals (each handles year-to-year drift in the ADM table)
    applcn <- adm_col_or_sum(yr, "APPLCN",
                             c("APPLCNM", "APPLCNW", "APPLCNAN", "APPLCNUN")) %>%
      rename(applcn = value)
    admssn <- adm_col_or_sum(yr, "ADMSSN",
                             c("ADMSSNM", "ADMSSNW", "ADMSSNAN", "ADMSSNUN")) %>%
      rename(admssn = value)
    enrlt  <- adm_col_or_sum(yr, "ENRLT",
                             c("ENRLFTM", "ENRLFTW", "ENRLFTAN", "ENRLFTUN",
                               "ENRLPTM", "ENRLPTW", "ENRLPTAN", "ENRLPTUN")) %>%
      rename(enrlt = value)
    
    if (nrow(applcn) && nrow(admssn)) {
      # acceptance_rate - prefer DRVADM DVADM01 if present; else compute
      acc <- drvadm_col(yr, "DVADM01")
      if (!nrow(acc)) {
        acc <- applcn %>% inner_join(admssn, by = "unitid") %>%
          transmute(unitid, value = 100 * admssn / applcn) %>%
          filter(is.finite(value))
      }
      out[[length(out)+1]] <- acc %>% filter(unitid %in% uids) %>%
        mutate(year = yr, metric = "acceptance_rate",
               var_type = ifelse(nrow(drvadm_col(yr, "DVADM01")), "raw", "computed"))
    }
    if (nrow(admssn) && nrow(enrlt)) {
      # yield_rate - prefer DRVADM DVADM04 if present; else compute
      yld <- drvadm_col(yr, "DVADM04")
      if (!nrow(yld)) {
        yld <- admssn %>% inner_join(enrlt, by = "unitid") %>%
          transmute(unitid, value = 100 * enrlt / admssn) %>%
          filter(is.finite(value))
      }
      out[[length(out)+1]] <- yld %>% filter(unitid %in% uids) %>%
        mutate(year = yr, metric = "yield_rate",
               var_type = ifelse(nrow(drvadm_col(yr, "DVADM04")), "raw", "computed"))
    }
    if (nrow(applcn)) {
      out[[length(out)+1]] <- applcn %>% rename(value = applcn) %>%
        filter(unitid %in% uids) %>%
        mutate(year = yr, metric = "application_volume", var_type = "raw")
    }
    
    # test scores - descriptive only, but stored same as any other metric
    sat <- adm_sat_mid50(yr)
    if (nrow(sat))
      out[[length(out)+1]] <- sat %>% filter(unitid %in% uids) %>%
      mutate(year = yr, metric = "sat_mid50", var_type = "computed")
    
    act <- adm_act_mid50(yr)
    if (nrow(act))
      out[[length(out)+1]] <- act %>% filter(unitid %in% uids) %>%
      mutate(year = yr, metric = "act_mid50", var_type = "computed")
    
    # submission rates - direct ADM columns, present across all years
    adm <- get_table(yr, paste0("ADM", yr))
    if (!is.null(adm)) {
      if ("SATPCT" %in% names(adm))
        out[[length(out)+1]] <- adm %>%
          transmute(unitid, value = suppressWarnings(as.numeric(SATPCT))) %>%
          filter(unitid %in% uids, is.finite(value)) %>%
          mutate(year = yr, metric = "pct_submitting_sat", var_type = "raw")
      if ("ACTPCT" %in% names(adm))
        out[[length(out)+1]] <- adm %>%
          transmute(unitid, value = suppressWarnings(as.numeric(ACTPCT))) %>%
          filter(unitid %in% uids, is.finite(value)) %>%
          mutate(year = yr, metric = "pct_submitting_act", var_type = "raw")
    }
    
    # exploratory: yield men vs women gap (women - men, percentage points)
    ym <- drvadm_col(yr, "DVADM05") %>% rename(yld_m = value)
    yw <- drvadm_col(yr, "DVADM06") %>% rename(yld_w = value)
    if (nrow(ym) && nrow(yw)) {
      out[[length(out)+1]] <- ym %>% inner_join(yw, by = "unitid") %>%
        transmute(unitid, value = yld_w - yld_m) %>%
        filter(unitid %in% uids, is.finite(value)) %>%
        mutate(year = yr, metric = "yield_men_vs_women", var_type = "computed")
    }
  }
  
  bind_rows(out)
}

# =============================================================================
# 3. CDS / Academic Insights metrics
# =============================================================================
# Pulls every metric ID in cfg$ai_metric_ids. The caller decides which become
# facts (cfg$ai_facts_metrics) vs which are scaffolding for computed metrics
# (e.g. ed_applicants_count is used only to derive ed_share_of_applications).
build_cds_admissions <- function(cfg) {
  ids <- unlist(cfg$ai_metric_ids)
  ids <- ids[!is.na(ids)]
  if (!length(ids)) {
    message("No CDS metric IDs set for admissions - skipping. ",
            "Confirm via search_ai_metrics() and fill ADM_CONFIG$ai_metric_ids.")
    return(tibble())
  }
  message(sprintf("Pulling %d CDS admissions metrics (paged by metric x year, IPEDS->AI year mapping) ...",
                  length(ids)))
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
                   metric    = names(cfg$ai_metric_ids)) %>%
    filter(!is.na(metric_id))
  facts %>%
    transmute(unitid = as.integer(school_ipeds_id), year = as.integer(year),
              metric_id = as.integer(metric_id), value = as.numeric(value)) %>%
    inner_join(id_map, by = "metric_id") %>%
    transmute(unitid, year, metric, value, var_type = "external")
}

# =============================================================================
# 3b. ed_share_of_applications - computed from CDS ED applicants + IPEDS APPLCN
# =============================================================================
build_ed_share <- function(cds_all, ipeds_all) {
  ed_apps <- cds_all %>%
    filter(metric == "ed_applicants_count") %>%
    select(unitid, year, ed_apps = value)
  total_apps <- ipeds_all %>%
    filter(metric == "application_volume") %>%
    select(unitid, year, applcn = value)
  ed_apps %>%
    inner_join(total_apps, by = c("unitid", "year")) %>%
    transmute(unitid, year,
              metric = "ed_share_of_applications",
              value  = 100 * ed_apps / applcn,
              var_type = "computed") %>%
    filter(is.finite(value))
}


# =============================================================================
# 4. adm_variables.csv - the variable metadata for this module
# =============================================================================
build_adm_variables <- function(cfg) {
  ai_id <- function(nm) {
    v <- cfg$ai_metric_ids[[nm]]
    if (is.na(v)) "Academic Insights metric_id TBD" else
      sprintf("Academic Insights metric_id %d", v)
  }
  tribble(
    ~metric,                ~display_name,                                            ~source,         ~ipeds_table_or_formula,                                                ~use_type,       ~comparison_scope,    ~format,       ~coverage_note,
    "acceptance_rate",      "Acceptance rate",                                        "ipeds_derived", "DRVADM.DVADM01, or ADM.ADMSSN/APPLCN with gender-breakout fallback",   "clustering",    "cross_category",     "percentage",  NA_character_,
    "yield_rate",           "Yield rate",                                             "ipeds_derived", "DRVADM.DVADM04, or ADM.ENRLT/ADMSSN with gender-breakout fallback",    "clustering",    "cross_category",     "percentage",  NA_character_,
    "application_volume",   "Number of applications",                                 "ipeds",         "ADM.APPLCN, with gender-breakout fallback",                            "clustering",    "within_category",    "count",       NA_character_,
    "sat_mid50",            "SAT mid-50% composite score",                            "ipeds_derived", "ADM.SATVR50 + SATMT50, fallback to (SATVR25+SATVR75)/2 + (SATMT25+SATMT75)/2", "descriptive",  "descriptive",  "score",       "Submission bias under test-optional policies; not for clustering",
    "act_mid50",            "ACT mid-50% composite score",                            "ipeds_derived", "ADM.ACTCM50, fallback to (ACTCM25+ACTCM75)/2",                          "descriptive",   "descriptive",        "score",       "Kept separate from SAT (no concordance); not for clustering",
    "pct_submitting_sat",   "Percent of first-time students submitting SAT scores",   "ipeds",         "ADM.SATPCT",                                                            "clustering",    "cross_category",     "percentage",  "Companion to sat_mid50",
    "pct_submitting_act",   "Percent of first-time students submitting ACT scores",   "ipeds",         "ADM.ACTPCT",                                                            "clustering",    "cross_category",     "percentage",  "Companion to act_mid50",
    "yield_men_vs_women",   "Yield gap (women minus men), percentage points",         "ipeds_derived", "DRVADM.DVADM06 - DVADM05",                                              "exploratory",   "within_category",    "percentage",  "Exploratory only; not a core variable",
    "pct_top10_hs",         "Percent of enrolled in top 10% of HS class",             "cds_ai",            ai_id("pct_top10_hs"),                                                   "clustering",    "cross_category",     "percentage",  "Survey respondents only (~45%)",
    "ed_acceptance_rate",   "Early decision acceptance rate",                         "cds_ai",            ai_id("ed_acceptance_rate"),                                             "clustering",    "cross_category",     "percentage",  "Survey respondents only (~45%); NA distinct from 'no ED program'",
    "ed_share_of_applications","Early decision applicants as % of total applications","cds_ai_derived",    "Academic Insights metric_id 194 / IPEDS ADM.APPLCN",                    "clustering",    "within_category",    "percentage",  "Numerator covers ~45% of universe; denominator near-universal"
  ) %>%
    mutate(category = "admissions",
           neche_peer_set = FALSE,
           neche_dashboard = FALSE,
           notes = NA_character_) %>%
    select(metric, category, display_name, source, ipeds_table_or_formula,
           use_type, comparison_scope, format, neche_peer_set, neche_dashboard,
           coverage_note, notes)
}

# =============================================================================
# 5. COVERAGE REPORT
# =============================================================================
adm_coverage_report <- function(facts, schools) {
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
run_admissions_module <- function(cfg = ADM_CONFIG) {
  message("== Admissions Module ==")
  
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
  
  ipeds_adm <- build_ipeds_admissions(unitids_by_year, cfg)
  cds_all   <- build_cds_admissions(cfg)
  
  # Compute ED share BEFORE filtering ed_applicants_count out of cds output
  ed_share <- if (nrow(cds_all) && "ed_applicants_count" %in% cds_all$metric) {
    build_ed_share(cds_all, ipeds_adm)
  } else tibble()
  
  # Keep only the CDS metrics that become facts; ed_applicants_count is
  # scaffolding for the share computation, not a fact in its own right.
  cds_adm <- cds_all %>% filter(metric %in% cfg$ai_facts_metrics)
  
  adm_facts <- bind_rows(ipeds_adm, cds_adm, ed_share) %>%
    semi_join(schools, by = "unitid") %>%
    arrange(unitid, year, metric)
  
  adm_variables <- build_adm_variables(cfg)
  
  write.csv(adm_facts,     .out_path("adm_facts.csv"),     row.names = FALSE)
  write.csv(adm_variables, .out_path("adm_variables.csv"), row.names = FALSE)
  message(sprintf("Wrote %s, %s",
                  .out_path("adm_facts.csv"), .out_path("adm_variables.csv")))
  
  message("\nCoverage (% of universe with a value), by metric and control group:")
  print(adm_coverage_report(adm_facts, schools))
  
  invisible(list(facts = adm_facts, variables = adm_variables, schools = schools))
}

# -----------------------------------------------------------------------------
# Usage:
#   setwd("path/to/peer_schools")
#   Sys.setenv(ACADEMIC_INSIGHTS_API_KEY = "...")     # or via .Renviron
#   source("R/schools_pipeline.R");      build_schools()
#   source("R/admissions_module_pipeline.R")
#   res_adm <- run_admissions_module()
# -----------------------------------------------------------------------------