# =============================================================================
# Outcomes Module
# Holy Cross peer-comparison project
#
# Reads:  output/schools.csv     (built by R/schools_pipeline.R)
#         data/IPEDS*.Rda
#         Academic Insights API for the first-gen graduation gap
#         College Scorecard API for earnings + loan repayment
#         schools.csv.earnings_ratio (from Carnegie 2025, joined in schools)
#
# Writes: output/out_facts.csv      one row per unitid x year x metric
#         output/out_variables.csv  one row per metric (metadata)
#
# Variables (~12 total):
#   IPEDS (DRVGR, DRVOM, EF*D, DRVC)
#     grad_rate_6yr, grad_rate_4yr (clustering, cross_category)
#     retention_rate (clustering, cross_category)
#     transfer_out_rate (clustering, cross_category)
#     pell_grad_gap (clustering, cross_category - equity measure)
#     grad_rate_men_vs_women (exploratory, within_category)
#     doctoral_degrees_awarded (descriptive, within_category, NECHE)
#   Scorecard (latest cohort available; documented in methodology)
#     median_earnings_10yr (clustering, cross_category)
#     median_earnings_6yr  (clustering, cross_category)
#     loan_repayment_rate  (clustering, cross_category)
#   Academic Insights
#     first_gen_grad_rate_6yr (clustering, cross_category - id 1116, confirmed)
#   Carnegie (from schools.csv, written here as a fact)
#     earnings_ratio (clustering, cross_category, source=ccihe)
#
# YEAR HANDLING
#   IPEDS metrics are panel-year specific (2020-21 through 2024-25). Scorecard
#   metrics are "latest available" because the cohort lag is long (10-year
#   post-entry earnings observed today were earned by ~2012 entrants). They
#   are written under the latest panel year with a methodology note.
#   Carnegie earnings_ratio is a one-time 2025 snapshot, also written under
#   the latest panel year.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(httr2); library(jsonlite)
})


# ---- CONFIG ---------------------------------------------------------------
OUTCOMES_CONFIG <- list(
  collection_years = 2020:2024,
  ai_dataset = "undergraduate",
  ai_key     = Sys.getenv("ACADEMIC_INSIGHTS_API_KEY"),
  ai_base    = "https://ai.usnews.com/api/v1/client_api",
  scorecard_key = Sys.getenv("SCORECARD_API_KEY"),
  # AI first-gen 6-year graduation rate - id 1116 (confirmed via diagnostic).
  # Stored as a level rather than a gap; downstream applications construct
  # comparisons against grad_rate_6yr or other reference rates.
  ai_metric_ids = list(
    first_gen_grad_rate_6yr = 1116L
  ),
  # Some AI metrics (first-gen rates from the Outcome Measures family) are
  # cohort-anchored, not year-anchored. The /facts endpoint returns zero
  # rows when filtered by panel year. List those here so build_cds_outcomes
  # pulls them once (without year filter) and writes under the latest
  # panel year, same convention as Scorecard.
  ai_metrics_no_year_filter = c("first_gen_grad_rate_6yr"),
  # Scorecard field paths (latest cohort only). The API uses dotted paths
  # for nested JSON; we map each to a metric name.
  scorecard_fields = list(
    median_earnings_10yr = "latest.earnings.10_yrs_after_entry.median",
    median_earnings_6yr  = "latest.earnings.6_yrs_after_entry.median",
    loan_repayment_rate  = "latest.repayment.3_yr_repayment.completers_rate"
  ),
  # Scorecard returns rates as decimal proportions (0-1); the rest of the
  # project stores rates on a 0-100 scale (IPEDS and AI both use percentages
  # natively). To keep the format consistent, the build function multiplies
  # any metric named here by 100 on the way in. Currency / count fields are
  # excluded - they need no transformation.
  scorecard_rate_metrics = c("loan_repayment_rate")
)

# =============================================================================
# 1. IPEDS pulls
# =============================================================================

# Pull a single column from a table, returning (unitid, value). Wraps the
# common pattern with a clean fallback if either is missing.
drv_col <- function(year, table, varname) {
  df <- get_table(year, table)
  if (is.null(df) || !varname %in% names(df))
    return(tibble(unitid = integer(), value = numeric()))
  df %>% transmute(unitid, value = suppressWarnings(as.numeric(.data[[varname]])))
}

build_ipeds_outcomes <- function(unitids_by_year, cfg) {
  message("Pulling IPEDS outcomes metrics ...")
  out <- list()
  
  for (yr in cfg$collection_years) {
    uids <- unitids_by_year[[as.character(yr)]]
    
    # Direct columns from DRVGR (graduation + transfer-out, BA-within-N-years)
    direct_drvgr <- c(
      grad_rate_6yr      = "GBA6RTT",
      grad_rate_4yr      = "GBA4RTT",
      transfer_out_rate  = "TRRTTOT"
    )
    drvgr_tbl <- paste0("DRVGR", yr)
    for (nm in names(direct_drvgr)) {
      d <- drv_col(yr, drvgr_tbl, direct_drvgr[[nm]]) %>%
        filter(unitid %in% uids, is.finite(value))
      if (nrow(d))
        out[[length(out)+1]] <- d %>%
          mutate(year = yr, metric = nm, var_type = "raw")
    }
    
    # Exploratory: grad rate gap, women minus men
    rw <- drv_col(yr, drvgr_tbl, "GBA6RTW") %>% rename(rw = value)
    rm <- drv_col(yr, drvgr_tbl, "GBA6RTM") %>% rename(rm = value)
    if (nrow(rw) && nrow(rm)) {
      out[[length(out)+1]] <- rw %>% inner_join(rm, by = "unitid") %>%
        transmute(unitid, value = rw - rm) %>%
        filter(unitid %in% uids, is.finite(value)) %>%
        mutate(year = yr, metric = "grad_rate_men_vs_women", var_type = "computed")
    }
    
    # Retention from EF{yr}D (full-time retention rate)
    ret <- drv_col(yr, paste0("EF", yr, "D"), "RET_PCF") %>%
      filter(unitid %in% uids, is.finite(value))
    if (nrow(ret))
      out[[length(out)+1]] <- ret %>%
      mutate(year = yr, metric = "retention_rate", var_type = "raw")
    
    # Pell graduation gap from DRVOM (Pell minus non-Pell, 6-year)
    om <- get_table(yr, paste0("DRVOM", yr))
    if (!is.null(om) && all(c("OM1PELLAWDP6", "OM1NPELAWDP6") %in% names(om))) {
      pg <- om %>%
        transmute(unitid,
                  pell    = suppressWarnings(as.numeric(OM1PELLAWDP6)),
                  nonpell = suppressWarnings(as.numeric(OM1NPELAWDP6))) %>%
        filter(unitid %in% uids, is.finite(pell), is.finite(nonpell)) %>%
        transmute(unitid, value = pell - nonpell)
      if (nrow(pg))
        out[[length(out)+1]] <- pg %>%
          mutate(year = yr, metric = "pell_grad_gap", var_type = "computed")
    }
    
    # Doctoral degrees awarded - research/scholarship only (NECHE measure)
    doc <- drv_col(yr, paste0("DRVC", yr), "DOCDEGRS") %>%
      filter(unitid %in% uids, is.finite(value))
    if (nrow(doc))
      out[[length(out)+1]] <- doc %>%
      mutate(year = yr, metric = "doctoral_degrees_awarded", var_type = "raw")
  }
  
  bind_rows(out)
}

# =============================================================================
# 2. Scorecard pulls (latest cohort, written under most recent panel year)
# =============================================================================
build_scorecard_outcomes <- function(cfg) {
  if (cfg$scorecard_key == "") {
    message("SCORECARD_API_KEY not set; skipping Scorecard pull.")
    return(tibble())
  }
  message("Pulling earnings + repayment from College Scorecard ...")
  
  fields <- c("id", unlist(cfg$scorecard_fields))
  raw <- tryCatch(
    scorecard_get(cfg, fields, query = list(
      `school.degrees_awarded.predominant__range` = "3..4",
      `school.ownership` = "1,2"
    )),
    error = function(e) {
      warning(sprintf("Scorecard pull failed: %s", conditionMessage(e)))
      tibble()
    })
  if (!nrow(raw) || !"id" %in% names(raw)) return(tibble())
  
  latest_year <- max(cfg$collection_years)
  rate_metrics <- cfg$scorecard_rate_metrics %||% character(0)
  out <- list()
  for (nm in names(cfg$scorecard_fields)) {
    field <- cfg$scorecard_fields[[nm]]
    if (!field %in% names(raw)) {
      warning(sprintf("Scorecard field %s missing for metric %s", field, nm))
      next
    }
    d <- raw %>%
      transmute(unitid = as.integer(id),
                value  = suppressWarnings(as.numeric(.data[[field]]))) %>%
      filter(!is.na(unitid), is.finite(value))
    # Normalize decimal rates (0-1) to project's 0-100 scale.
    if (nm %in% rate_metrics) d <- d %>% mutate(value = value * 100)
    if (nrow(d))
      out[[length(out)+1]] <- d %>%
      mutate(year = latest_year, metric = nm, var_type = "external")
  }
  bind_rows(out)
}

# =============================================================================
# 3. CDS / Academic Insights
# =============================================================================
build_cds_outcomes <- function(cfg) {
  ids <- unlist(cfg$ai_metric_ids)
  ids <- ids[!is.na(ids)]
  if (!length(ids)) {
    message("No CDS metric IDs set for outcomes - skipping.")
    return(tibble())
  }
  
  # Split metrics by whether they accept a year filter on /facts.
  # Cohort-anchored AI metrics (first-gen rates) return zero rows when
  # filtered by year; we pull them once and write under the latest panel year.
  no_year_names <- intersect(cfg$ai_metrics_no_year_filter %||% character(),
                             names(cfg$ai_metric_ids))
  no_year_ids   <- unlist(cfg$ai_metric_ids[no_year_names])
  no_year_ids   <- no_year_ids[!is.na(no_year_ids)]
  yearly_ids    <- ids[!names(ids) %in% no_year_names]
  
  message(sprintf(
    "Pulling %d CDS outcomes metrics (yearly, IPEDS->AI mapping) + %d (cohort-anchored, single pull) ...",
    length(yearly_ids), length(no_year_ids)))
  
  # --- yearly metrics: paged by metric x year, IPEDS->AI year mapping ---
  facts_yearly <- if (length(yearly_ids)) {
    grid <- expand.grid(metric_id = yearly_ids, ipeds_year = cfg$collection_years)
    pmap_dfr(grid, function(metric_id, ipeds_year) {
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
      df %>% mutate(year = ipeds_year)  # store under our IPEDS-convention year
    })
  } else tibble()
  
  # --- cohort-anchored metrics: pull once, translate AI year to IPEDS year ---
  facts_no_year <- if (length(no_year_ids)) {
    map_dfr(no_year_ids, function(metric_id) {
      res <- ai_get(cfg, paste0("facts/", cfg$ai_dataset), query = list(
        metric_ids = metric_id, all_data = "true"))
      df <- as_tibble(res)
      n  <- nrow(df)
      message(sprintf("  metric %s (cohort-anchored): %d rows", metric_id, n))
      if (!n) return(tibble())
      # AI's `year` is spring-year; translate to IPEDS fall-year for facts.
      # The resulting year may fall outside our panel range (e.g., AI's 2026
      # becomes IPEDS's 2025, one academic year newer than IPEDS final data).
      df %>% mutate(year = ai_to_ipeds_year(year))
    })
  } else tibble()
  
  facts <- bind_rows(facts_yearly, facts_no_year)
  
  # Robustness: if every pull returned zero rows, facts has no columns -
  # skip the transmute that assumes the AI response shape.
  if (!nrow(facts) || !"school_ipeds_id" %in% names(facts)) {
    warning("No CDS outcomes rows returned for any metric; skipping.")
    return(tibble())
  }
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
# 4. Carnegie earnings_ratio (read from schools.csv, written as a fact)
# =============================================================================
build_carnegie_outcomes <- function(schools, cfg) {
  if (!"earnings_ratio" %in% names(schools)) {
    message("schools.csv has no earnings_ratio column - skipping.")
    return(tibble())
  }
  message("Reading earnings_ratio from schools.csv ...")
  latest_year <- max(cfg$collection_years)
  schools %>%
    transmute(unitid, value = suppressWarnings(as.numeric(earnings_ratio))) %>%
    filter(is.finite(value)) %>%
    mutate(year = latest_year,
           metric = "earnings_ratio",
           var_type = "external")
}

# =============================================================================
# 4b. Program counts from Completions
#
# Counts distinct CIP codes where the institution awarded ≥1 degree in
# the most recent panel year, separated by award level. Two granularities:
#   - CIP6 (6-digit) = count of "programs" in the IPEDS-classification sense
#   - CIP2 (2-digit) = count of broad academic families (e.g., Engineering,
#                       Biological Sciences, Business as a whole)
#
# Note: counts "programs that produced at least one graduate in the panel
# year", which slightly under-counts programs that exist but had zero
# completions that year. Acceptable for an IR-style summary; the
# coverage_note flags it.
#
# Single snapshot under the latest panel year (per the methodology choice
# for similar snapshot-style variables in this module).
# =============================================================================
build_program_counts <- function(unitids_by_year, cfg) {
  latest_year <- max(cfg$collection_years)
  uids <- unitids_by_year[[as.character(latest_year)]]

  ca <- get_table(latest_year, paste0("C", latest_year, "_A"))
  if (is.null(ca)) {
    message("  C", latest_year, "_A not available; skipping program counts.")
    return(tibble())
  }
  message(sprintf("Counting distinct CIPs from C%d_A (%d rows) ...",
                  latest_year, nrow(ca)))

  ca_pri <- ca %>%
    filter(suppressWarnings(as.integer(MAJORNUM)) == 1L,
           !is.na(CIPCODE),
           CIPCODE != "99",
           CIPCODE != "99.0000",
           suppressWarnings(as.integer(CTOTALT)) > 0)

  ca_ug   <- ca_pri %>%
    filter(suppressWarnings(as.integer(AWLEVEL)) == 5L)
  ca_grad <- ca_pri %>%
    filter(suppressWarnings(as.integer(AWLEVEL)) %in% c(7L, 17L, 18L, 19L))

  count_programs <- function(df) {
    if (!nrow(df)) return(tibble(unitid = integer(),
                                  n_cip6 = integer(), n_cip2 = integer()))
    df %>%
      group_by(unitid) %>%
      summarise(n_cip6 = n_distinct(CIPCODE),
                n_cip2 = n_distinct(substr(as.character(CIPCODE), 1, 2)),
                .groups = "drop") %>%
      mutate(unitid = as.integer(unitid))
  }

  ug_counts   <- count_programs(ca_ug)   %>% filter(unitid %in% uids)
  grad_counts <- count_programs(ca_grad) %>% filter(unitid %in% uids)

  rows <- list()
  push <- function(df, metric_name, col) {
    if (nrow(df))
      rows[[length(rows)+1]] <<- df %>%
        transmute(unitid, value = .data[[col]]) %>%
        filter(is.finite(value)) %>%
        mutate(year = latest_year, metric = metric_name,
               var_type = "computed")
  }
  push(ug_counts,   "n_undergrad_programs",       "n_cip6")
  push(ug_counts,   "n_undergrad_cip2_families",  "n_cip2")
  push(grad_counts, "n_grad_programs",            "n_cip6")
  push(grad_counts, "n_grad_cip2_families",       "n_cip2")

  bind_rows(rows)
}

# =============================================================================
# 5. out_variables.csv
# =============================================================================
build_out_variables <- function(cfg) {
  ai_id <- function(nm) {
    v <- cfg$ai_metric_ids[[nm]]
    if (is.na(v)) "Academic Insights metric_id TBD" else
      sprintf("Academic Insights metric_id %d", v)
  }
  tribble(
    ~metric,                    ~display_name,                                          ~source,         ~ipeds_table_or_formula,                                          ~use_type,     ~comparison_scope,  ~format,       ~neche_peer_set, ~neche_dashboard, ~coverage_note,
    # IPEDS clustering
    "grad_rate_6yr",            "6-year bachelor's graduation rate",                    "ipeds",         "DRVGR.GBA6RTT",                                                  "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             NA_character_,
    "grad_rate_4yr",            "4-year bachelor's graduation rate",                    "ipeds",         "DRVGR.GBA4RTT",                                                  "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            NA_character_,
    "retention_rate",           "Full-time first-year retention rate",                  "ipeds",         "EF{yr}D.RET_PCF",                                                "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             NA_character_,
    "transfer_out_rate",        "Transfer-out rate from entering cohort",               "ipeds",         "DRVGR.TRRTTOT",                                                  "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            NA_character_,
    "pell_grad_gap",            "Pell minus non-Pell 6-year award rate, pp",            "ipeds_derived", "DRVOM.OM1PELLAWDP6 - OM1NPELAWDP6",                              "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             "Negative values mean Pell students do worse",
    # IPEDS exploratory + NECHE
    "grad_rate_men_vs_women",   "Grad rate gap, women minus men (6-year), pp",          "ipeds_derived", "DRVGR.GBA6RTW - GBA6RTM",                                        "exploratory", "within_category",  "percentage",  FALSE,           FALSE,            "Exploratory only",
    "doctoral_degrees_awarded", "Research/scholarship doctorates awarded",              "ipeds",         "DRVC.DOCDEGRS",                                                  "descriptive", "within_category",  "count",       TRUE,            TRUE,             "NECHE peer-set member. Excludes professional-practice doctorates (DOCDEGPP).",
    # Program counts from Completions (single snapshot, latest panel year)
    "n_undergrad_programs",     "Number of distinct undergraduate programs (CIP6, primary major)", "ipeds_derived", "C{yr}_A: distinct CIPCODE where AWLEVEL=5, MAJORNUM=1, CTOTALT>0", "descriptive", "within_category", "count", FALSE, FALSE, "Counts CIP codes that produced at least one bachelor's graduate in the latest panel year. Slightly under-counts programs that exist but had zero completions.",
    "n_undergrad_cip2_families","Number of distinct undergraduate CIP2 families",       "ipeds_derived", "C{yr}_A: distinct CIPCODE first-2 where AWLEVEL=5, MAJORNUM=1",  "descriptive", "within_category", "count", FALSE, FALSE, "Broader grouping than n_undergrad_programs; each CIP2 is a major academic family (e.g., 11 = Computer Science, 14 = Engineering, 26 = Biology).",
    "n_grad_programs",          "Number of distinct graduate programs (CIP6, primary major)",     "ipeds_derived", "C{yr}_A: distinct CIPCODE where AWLEVEL in {7,17,18,19}, MAJORNUM=1, CTOTALT>0", "descriptive", "within_category", "count", FALSE, FALSE, "Includes Master's, research doctorates, professional-practice doctorates, and other doctorates. Counts programs that produced at least one graduate in the latest panel year.",
    "n_grad_cip2_families",     "Number of distinct graduate CIP2 families",            "ipeds_derived", "C{yr}_A: distinct CIPCODE first-2 where AWLEVEL in {7,17,18,19}, MAJORNUM=1", "descriptive", "within_category", "count", FALSE, FALSE, "Broader grouping than n_grad_programs.",
    # Scorecard
    "median_earnings_10yr",     "Median earnings 10 yrs after entry",                   "scorecard",     "earnings.10_yrs_after_entry.median (latest)",                    "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            "Latest available Scorecard cohort; long lag from current year",
    "median_earnings_6yr",      "Median earnings 6 yrs after entry",                    "scorecard",     "earnings.6_yrs_after_entry.median (latest)",                     "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            "Latest available Scorecard cohort",
    "loan_repayment_rate",      "3-year loan repayment rate (completers)",              "scorecard",     "repayment.3_yr_repayment.completers (latest)",                   "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Latest available Scorecard cohort",
    # CDS
    "first_gen_grad_rate_6yr",  "First-gen 6-year graduation rate",                     "cds_ai",        ai_id("first_gen_grad_rate_6yr"),                                 "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Survey respondents only (~45%); rate, not gap. Construct comparisons downstream.",
    # Carnegie
    "earnings_ratio",           "Actual vs expected earnings (CCIHE SAEC)",             "ccihe",         "Carnegie 2025 Public Data File.earnings_ratio",                  "clustering",  "cross_category",   "ratio",       FALSE,           FALSE,            "One-time CCIHE snapshot; written under latest panel year"
  ) %>%
    mutate(category = "outcomes", notes = NA_character_) %>%
    select(metric, category, display_name, source, ipeds_table_or_formula,
           use_type, comparison_scope, format, neche_peer_set, neche_dashboard,
           coverage_note, notes)
}

# =============================================================================
# 6. COVERAGE REPORT
# =============================================================================
out_coverage_report <- function(facts, schools) {
  # Each metric's target year set is inferred from the data itself: a metric's
  # "universe" is schools x the years that metric actually populates. This
  # treats single-snapshot variables (1 year) and multi-year variables (5
  # years) on equal footing - both report coverage relative to their target
  # year range rather than the full panel. Without this, Scorecard / Carnegie
  # snapshot variables show coverage diluted by ~5x because they only populate
  # one of five panel years.
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
# 7. RUN
# =============================================================================
run_outcomes_module <- function(cfg = OUTCOMES_CONFIG) {
  message("== Outcomes Module ==")
  
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
  
  ipeds_out     <- build_ipeds_outcomes(unitids_by_year, cfg)
  scorecard_out <- build_scorecard_outcomes(cfg)
  cds_out       <- build_cds_outcomes(cfg)
  carnegie_out  <- build_carnegie_outcomes(schools, cfg)
  programs_out  <- build_program_counts(unitids_by_year, cfg)

  out_facts <- bind_rows(ipeds_out, scorecard_out, cds_out,
                         carnegie_out, programs_out) %>%
    semi_join(schools, by = "unitid") %>%
    arrange(unitid, year, metric)
  
  out_variables <- build_out_variables(cfg)
  
  write.csv(out_facts,     .out_path("out_facts.csv"),     row.names = FALSE)
  write.csv(out_variables, .out_path("out_variables.csv"), row.names = FALSE)
  message(sprintf("Wrote %s, %s",
                  .out_path("out_facts.csv"), .out_path("out_variables.csv")))
  
  message("\nCoverage (% of universe with a value), by metric and control group:")
  print(out_coverage_report(out_facts, schools))
  
  invisible(list(facts = out_facts, variables = out_variables, schools = schools))
}

# -----------------------------------------------------------------------------
# Usage:
#   setwd("path/to/peer_schools")
#   Sys.setenv(ACADEMIC_INSIGHTS_API_KEY = "...")
#   Sys.setenv(SCORECARD_API_KEY = "...")     # also via .Renviron
#   source("R/schools_pipeline.R");           build_schools()
#   source("R/outcomes_module_pipeline.R")
# -----------------------------------------------------------------------------
#   res_out <- run_outcomes_module()
# All metric IDs are pre-set.
# -----------------------------------------------------------------------------