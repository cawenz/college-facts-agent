# =============================================================================
# Enrollment & Student Body Module
# Holy Cross peer-comparison project
#
# Reads:  output/schools.csv  (built by R/schools_pipeline.R)
#         data/IPEDS*.Rda      (committed in the repo)
#         Academic Insights API for residential_share (CDS metric)
#         College Scorecard API for pct_first_generation, median_family_income
#
# Writes: output/enr_facts.csv      one row per unitid x year x metric
#         output/enr_variables.csv  one row per metric (metadata)
#
# Variables (21 total):
#   NECHE peer-set (clustering, cross_category, neche_peer_set=TRUE)
#     total_enrollment, undergraduate_enrollment, first_time_enrollment,
#     full_time_enrollment
#   Other IPEDS clustering
#     pct_undergrad, pct_part_time, pct_age_25plus, pct_white,
#     pct_international, pct_bipoc
#     transfer_in_enrollment (within_category)
#   Detailed race/ethnicity (descriptive)
#     pct_black, pct_hispanic, pct_asian, pct_nhpi, pct_aian,
#     pct_two_or_more, pct_race_unknown
#   CDS/AI (clustering, yearly pulls)
#     residential_share (AI metric 74)
#   Scorecard (clustering, single 2024 snapshot)
#     pct_first_generation, median_family_income
#
# DATA SOURCE
#   Almost everything comes from the IPEDS DRVEF{year} derived table, which
#   precomputes the headcounts and percentages we need. The pipeline reads
#   the columns directly with no row-level filtering required. DRVEF exists
#   in every panel year (2020-21 through 2024-25).
#
# THE 3-LEVEL RACE/ETHNICITY TRIO
#   pct_white          = PCTENRWH
#   pct_international  = PCTENRNR (IPEDS "U.S. nonresident", same population)
#   pct_bipoc          = 100 - PCTENRWH - PCTENRNR - PCTENRUN
#                        (excludes "race/ethnicity unknown" so high-unknown
#                         institutions are not misclassified as BIPOC-heavy)
#   The trio will not always sum to 100% - pct_race_unknown captures the rest
#   and is carried as descriptive.
#
# SCORECARD FIRST-GEN AND FAMILY INCOME
#   Both variables draw from College Scorecard's "latest" snapshot (year-
#   prefixed paths are NULL for these fields, so a single snapshot is the
#   only option). Both reflect FAFSA filers only - higher-income non-filers
#   are excluded. Direction-of-signal is correct for clustering; absolute
#   values typically run a couple percentage points high (first-gen) or
#   low (family income) relative to the true UG-population values.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(httr2); library(jsonlite)
})

# ---- CONFIG ---------------------------------------------------------------
ENR_CONFIG <- list(
  collection_years = 2020:2024,
  ai_dataset = "undergraduate",
  ai_key     = Sys.getenv("ACADEMIC_INSIGHTS_API_KEY"),
  ai_base    = "https://ai.usnews.com/api/v1/client_api",
  scorecard_key = Sys.getenv("SCORECARD_API_KEY"),
  # AI: residential_share = metric 74 (CDS-sourced, yearly pulls).
  # Originally we also intended to use AI for pct_first_generation, but
  # diagnostic testing (see methodology) showed that none of AI's first-gen
  # percentage metrics returned clean usable data for our panel: 731 was
  # outside our panel years, 1014 was OM-cohort-specific (HC = 42), and
  # 1027/1028/1029 had unclear units (HC = 0.4). Scorecard's
  # share_firstgeneration is now the source for pct_first_generation.
  ai_metric_ids = list(
    residential_share = 74L
  ),
  # Scorecard fields (latest snapshot, written under latest panel year).
  # Both reflect FAFSA filers only, not the full undergraduate body -
  # documented honestly in the variables coverage_note and methodology.
  scorecard_fields = list(
    pct_first_generation = "latest.student.share_firstgeneration",
    median_family_income = "latest.student.demographics.median_family_income"
  ),
  # Scorecard returns rates as decimal proportions (0-1); the rest of the
  # project stores rates on a 0-100 scale. The build function multiplies any
  # metric named here by 100 on the way in. Currency fields are excluded -
  # they need no transformation.
  scorecard_rate_metrics = c("pct_first_generation")
)

# =============================================================================
# 1. IPEDS DRVEF pulls
# =============================================================================
# Read a single column from DRVEF{year}, returning (unitid, value). Returns
# empty tibble if either the table or the column is missing.
drvef_col <- function(year, varname) {
  df <- get_table(year, paste0("DRVEF", year))
  if (is.null(df)) {
    warning(sprintf("DRVEF%d not found", year))
    return(tibble(unitid = integer(), value = numeric()))
  }
  if (!varname %in% names(df)) {
    warning(sprintf("DRVEF%d has no column %s", year, varname))
    return(tibble(unitid = integer(), value = numeric()))
  }
  df %>% transmute(unitid, value = suppressWarnings(as.numeric(.data[[varname]])))
}

# DRVEF columns we pull directly, each becoming a metric of the same shape.
# (year column added in the outer loop.)
DRVEF_MAP <- tribble(
  ~drvef_col, ~metric,                   ~var_type,
  "ENRTOT",   "total_enrollment",        "raw",
  "EFUG",     "undergraduate_enrollment","raw",
  "EFUG1ST",  "first_time_enrollment",   "raw",
  "ENRFT",    "full_time_enrollment",    "raw",
  "EFUGTRN",  "transfer_in_enrollment",  "raw",
  "DVEF15",   "pct_age_25plus",          "raw",
  "PCTENRWH", "pct_white",               "raw",
  "PCTENRNR", "pct_international",       "raw",
  "PCTENRBK", "pct_black",               "raw",
  "PCTENRHS", "pct_hispanic",            "raw",
  "PCTENRAS", "pct_asian",               "raw",
  "PCTENRNH", "pct_nhpi",                "raw",
  "PCTENRAN", "pct_aian",                "raw",
  "PCTENR2M", "pct_two_or_more",         "raw",
  "PCTENRUN", "pct_race_unknown",        "raw"
)

build_ipeds_enrollment <- function(unitids_by_year, cfg) {
  message("Pulling IPEDS enrollment metrics from DRVEF ...")
  out <- list()
  
  for (yr in cfg$collection_years) {
    uids <- unitids_by_year[[as.character(yr)]]
    
    # Direct DRVEF columns
    for (i in seq_len(nrow(DRVEF_MAP))) {
      d <- drvef_col(yr, DRVEF_MAP$drvef_col[i]) %>% filter(unitid %in% uids)
      if (!nrow(d)) next
      out[[length(out)+1]] <- d %>%
        mutate(year = yr,
               metric   = DRVEF_MAP$metric[i],
               var_type = DRVEF_MAP$var_type[i])
    }
    
    # Computed: pct_undergrad = EFUG / ENRTOT
    ug  <- drvef_col(yr, "EFUG")  %>% rename(efug  = value)
    tot <- drvef_col(yr, "ENRTOT") %>% rename(enrtot = value)
    if (nrow(ug) && nrow(tot)) {
      ug_join <- ug %>% inner_join(tot, by = "unitid") %>%
        filter(unitid %in% uids)
      out[[length(out)+1]] <- ug_join %>%
        transmute(unitid, value = 100 * efug / enrtot) %>%
        filter(is.finite(value)) %>%
        mutate(year = yr, metric = "pct_undergrad", var_type = "computed")
      # Complement: pct_graduate. Algebraic complement of pct_undergrad,
      # exposed as its own variable so codebook readers and the side-by-side
      # view can pick it up directly without needing to invert in their
      # heads. Tagged descriptive (would double-count signal if clustered).
      out[[length(out)+1]] <- ug_join %>%
        transmute(unitid, value = 100 * (enrtot - efug) / enrtot) %>%
        filter(is.finite(value)) %>%
        mutate(year = yr, metric = "pct_graduate", var_type = "computed")
    }
    
    # Computed: pct_part_time = ENRPT / ENRTOT
    pt <- drvef_col(yr, "ENRPT") %>% rename(enrpt = value)
    if (nrow(pt) && nrow(tot)) {
      out[[length(out)+1]] <- pt %>% inner_join(tot, by = "unitid") %>%
        filter(unitid %in% uids) %>%
        transmute(unitid, value = 100 * enrpt / enrtot) %>%
        filter(is.finite(value)) %>%
        mutate(year = yr, metric = "pct_part_time", var_type = "computed")
    }
    
    # Computed: pct_bipoc = 100 - white - international - unknown
    wh <- drvef_col(yr, "PCTENRWH") %>% rename(white = value)
    nr <- drvef_col(yr, "PCTENRNR") %>% rename(intl  = value)
    un <- drvef_col(yr, "PCTENRUN") %>% rename(unk   = value)
    bipoc <- wh %>%
      inner_join(nr, by = "unitid") %>%
      inner_join(un, by = "unitid") %>%
      filter(unitid %in% uids) %>%
      transmute(unitid, value = pmax(0, 100 - white - intl - unk))
    if (nrow(bipoc))
      out[[length(out)+1]] <- bipoc %>%
      filter(is.finite(value)) %>%
      mutate(year = yr, metric = "pct_bipoc", var_type = "computed")
  }
  
  bind_rows(out)
}

# =============================================================================
# 2. CDS / Academic Insights metrics
# =============================================================================
build_cds_enrollment <- function(cfg) {
  ids <- unlist(cfg$ai_metric_ids)
  ids <- ids[!is.na(ids)]
  if (!length(ids)) {
    message("No CDS metric IDs set for enrollment - skipping. ",
            "Confirm via search_ai_metrics() and fill ENR_CONFIG$ai_metric_ids.")
    return(tibble())
  }
  message(sprintf("Pulling %d CDS enrollment metrics (paged by metric x year, IPEDS->AI year mapping) ...",
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
# 3. Scorecard pulls (single snapshot, written under latest panel year)
# =============================================================================
# pct_first_generation and median_family_income both come from the College
# Scorecard. Scorecard exposes year-prefixed paths only for some fields;
# share_firstgeneration and median_family_income are "latest" only - they
# don't get archived under year prefixes the way grad rates or earnings do.
# This is a single snapshot, written under the latest panel year. Same
# convention as Scorecard earnings/repayment in the outcomes module.
#
# Both fields reflect FAFSA filers only. share_firstgeneration is the
# proportion of FAFSA filers whose parents do not hold a bachelor's degree;
# median_family_income is the median family income among FAFSA filers.
# Neither captures the full UG body, since higher-income non-filers are
# excluded. The variable coverage_note documents this honestly.
build_scorecard_enrollment <- function(cfg) {
  if (cfg$scorecard_key == "") {
    message("SCORECARD_API_KEY not set; skipping Scorecard pull.")
    return(tibble())
  }
  message("Pulling first-gen + family income from College Scorecard ...")
  
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
# 4. enr_variables.csv - the variable metadata for this module
# =============================================================================
build_enr_variables <- function(cfg) {
  ai_id <- function(nm) {
    v <- cfg$ai_metric_ids[[nm]]
    if (is.na(v)) "Academic Insights metric_id TBD" else
      sprintf("Academic Insights metric_id %d", v)
  }
  tribble(
    ~metric,                     ~display_name,                                          ~source,         ~ipeds_table_or_formula,                                ~use_type,     ~comparison_scope,  ~format,       ~neche_peer_set, ~neche_dashboard, ~coverage_note,
    # NECHE peer-set: clustering, cross_category, neche_peer_set=TRUE
    "total_enrollment",          "Total fall enrollment",                                "ipeds",         "DRVEF.ENRTOT",                                         "clustering",  "cross_category",   "count",       TRUE,            TRUE,             NA_character_,
    "undergraduate_enrollment",  "Undergraduate fall enrollment",                        "ipeds",         "DRVEF.EFUG",                                           "clustering",  "cross_category",   "count",       TRUE,            TRUE,             NA_character_,
    "first_time_enrollment",     "First-time degree-seeking UG enrollment",              "ipeds",         "DRVEF.EFUG1ST",                                        "clustering",  "cross_category",   "count",       TRUE,            TRUE,             NA_character_,
    "full_time_enrollment",      "Full-time fall enrollment",                            "ipeds",         "DRVEF.ENRFT",                                          "clustering",  "cross_category",   "count",       TRUE,            TRUE,             NA_character_,
    # Other IPEDS clustering
    "pct_undergrad",             "Undergraduate share of total enrollment",              "ipeds_derived", "DRVEF.EFUG / ENRTOT",                                  "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            NA_character_,
    "pct_graduate",              "Graduate share of total enrollment",                   "ipeds_derived", "100 - (DRVEF.EFUG / ENRTOT)",                          "descriptive", "cross_category",   "percentage",  FALSE,           FALSE,            "Algebraic complement of pct_undergrad; tagged descriptive to avoid double-counting in clustering.",
    "pct_part_time",             "Part-time share of total enrollment",                  "ipeds_derived", "DRVEF.ENRPT / ENRTOT",                                 "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            NA_character_,
    "pct_age_25plus",            "Percent of undergraduates age 25-64",                  "ipeds",         "DRVEF.DVEF15",                                         "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            NA_character_,
    "transfer_in_enrollment",    "Transfer-in undergraduate enrollment",                 "ipeds",         "DRVEF.EFUGTRN",                                        "clustering",  "within_category",  "count",       FALSE,           TRUE,             NA_character_,
    "pct_white",                 "Percent of enrollment that is White",                  "ipeds",         "DRVEF.PCTENRWH",                                       "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             "3-level trio component",
    "pct_international",         "Percent of enrollment that is U.S. nonresident",       "ipeds",         "DRVEF.PCTENRNR",                                       "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             "3-level trio component; visa status, not race",
    "pct_bipoc",                 "Percent of enrollment that is BIPOC",                  "ipeds_derived", "100 - PCTENRWH - PCTENRNR - PCTENRUN",                 "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "3-level trio component; excludes race/ethnicity unknown",
    # Detailed race/ethnicity - descriptive
    "pct_black",                 "Percent Black or African American",                    "ipeds",         "DRVEF.PCTENRBK",                                       "descriptive", "cross_category",   "percentage",  FALSE,           TRUE,             "Detailed breakdown; trio is clustering version",
    "pct_hispanic",              "Percent Hispanic or Latino",                           "ipeds",         "DRVEF.PCTENRHS",                                       "descriptive", "cross_category",   "percentage",  FALSE,           TRUE,             "Detailed breakdown; trio is clustering version",
    "pct_asian",                 "Percent Asian",                                        "ipeds",         "DRVEF.PCTENRAS",                                       "descriptive", "cross_category",   "percentage",  FALSE,           TRUE,             "Detailed breakdown; trio is clustering version",
    "pct_nhpi",                  "Percent Native Hawaiian or Pacific Islander",          "ipeds",         "DRVEF.PCTENRNH",                                       "descriptive", "cross_category",   "percentage",  FALSE,           TRUE,             "Detailed breakdown; trio is clustering version",
    "pct_aian",                  "Percent American Indian or Alaska Native",             "ipeds",         "DRVEF.PCTENRAN",                                       "descriptive", "cross_category",   "percentage",  FALSE,           TRUE,             "Detailed breakdown; trio is clustering version",
    "pct_two_or_more",           "Percent two or more races",                            "ipeds",         "DRVEF.PCTENR2M",                                       "descriptive", "cross_category",   "percentage",  FALSE,           TRUE,             "Detailed breakdown; trio is clustering version",
    "pct_race_unknown",          "Percent race/ethnicity unknown",                       "ipeds",         "DRVEF.PCTENRUN",                                       "descriptive", "cross_category",   "percentage",  FALSE,           TRUE,             "Transparency variable; excluded from BIPOC computation",
    # CDS / AI
    "residential_share",         "Share of undergraduates living on campus",             "cds_ai",        ai_id("residential_share"),                             "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Survey respondents only (~45%)",
    # Scorecard (single 2024 snapshot)
    "pct_first_generation",      "Percent first-generation undergraduates",              "scorecard",     "latest.student.share_firstgeneration (Scorecard)",     "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Single 2024 snapshot; FAFSA-filer denominator (not all UG); ~58% project coverage. First-gen share is structurally stable enough for clustering but not for year-over-year trends.",
    "median_family_income",      "Median family income (FAFSA filers)",                  "scorecard",     "latest.student.demographics.median_family_income (Scorecard)", "clustering", "cross_category", "currency", FALSE,           FALSE,            "Single 2024 snapshot; FAFSA-filer denominator only - higher-income non-filers are excluded, so value typically understates true student-body family income. Useful for clustering but not as an absolute income measure."
  ) %>%
    mutate(category = "enrollment", notes = NA_character_) %>%
    select(metric, category, display_name, source, ipeds_table_or_formula,
           use_type, comparison_scope, format, neche_peer_set, neche_dashboard,
           coverage_note, notes)
}

# =============================================================================
# 5. COVERAGE REPORT
# =============================================================================
enr_coverage_report <- function(facts, schools) {
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
# 6. RUN
# =============================================================================
run_enrollment_module <- function(cfg = ENR_CONFIG) {
  message("== Enrollment & Student Body Module ==")
  
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
  
  ipeds_enr <- build_ipeds_enrollment(unitids_by_year, cfg)
  cds_enr   <- build_cds_enrollment(cfg)
  scorecard_enr <- build_scorecard_enrollment(cfg)
  enr_facts <- bind_rows(ipeds_enr, cds_enr, scorecard_enr) %>%
    semi_join(schools, by = "unitid") %>%
    arrange(unitid, year, metric)
  
  enr_variables <- build_enr_variables(cfg)
  
  write.csv(enr_facts,     .out_path("enr_facts.csv"),     row.names = FALSE)
  write.csv(enr_variables, .out_path("enr_variables.csv"), row.names = FALSE)
  message(sprintf("Wrote %s, %s",
                  .out_path("enr_facts.csv"), .out_path("enr_variables.csv")))
  
  message("\nCoverage (% of universe with a value), by metric and control group:")
  print(enr_coverage_report(enr_facts, schools))
  
  invisible(list(facts = enr_facts, variables = enr_variables, schools = schools))
}

# -----------------------------------------------------------------------------
# Usage:
#   setwd("path/to/peer_schools")
#   Sys.setenv(ACADEMIC_INSIGHTS_API_KEY = "...")     # or via .Renviron
#   Sys.setenv(SCORECARD_API_KEY = "...")             # required for first-gen + family income
#   source("R/schools_pipeline.R");      build_schools()
#   source("R/enrollment_module_pipeline.R")
#
#   # residential_share (AI metric 74) is preset. To verify or change:
#   search_ai_metrics(ENR_CONFIG, contains = "residential")
#   ENR_CONFIG$ai_metric_ids$residential_share <- 74
#
#   # pct_first_generation and median_family_income use Scorecard (preset).
#   # Both are single 2024 snapshots, FAFSA-filer denominator.
# -----------------------------------------------------------------------------
#
#   res_enr <- run_enrollment_module()
# -----------------------------------------------------------------------------