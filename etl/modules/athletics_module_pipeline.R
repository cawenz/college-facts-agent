# =============================================================================
# Athletics Module   (repo version)
# Holy Cross peer-comparison project
#
# Reads:
#   output/schools.csv                         (built by R/schools_pipeline.R)
#   data/eada_2024_25/instLevel.xlsx           EADA institution-level table
#   data/eada_2024_25/schools.xlsx             EADA sport-level table
#   data/eada_conferences.csv                  (built by R/scrape_eada_conferences.py;
#                                              one row per EADA institution with a
#                                              Wikipedia-derived conference)
#
# Writes:
#   output/ath_facts.csv          one row per unitid x year=2024 x metric
#   output/ath_variables.csv      one row per metric (catalog)
#   output/schools_athletics.csv  one row per unitid: athletics_body,
#                                 athletics_division, athletics_conference,
#                                 has_football, athletics_classification_*
#
# Why a separate schools_athletics.csv (instead of patching schools.csv)?
#   schools_pipeline.R already owns the schools.csv classification join (IPEDS
#   HD + Carnegie). Adding EADA there would make every schools-pipeline run
#   depend on EADA files being present. Keeping the EADA categorical extras
#   in a sibling file lets shiny_app/global.R join them on at app start,
#   with a graceful fall-back if the file is missing.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(stringr); library(tibble)
})

# ---- repo paths (resolve relative to wherever this is sourced from) -------
.repo_root <- function() getwd()
.data_path <- function(...) file.path(.repo_root(), "data", ...)
.out_path  <- function(...) file.path(.repo_root(), "output", ...)
dir.create(.out_path(), showWarnings = FALSE, recursive = TRUE)

# ---- CONFIG ---------------------------------------------------------------
ATH_CONFIG <- list(
  # The EADA 2024-25 release reports on academic year 2024-25. We tag the
  # snapshot with year = 2024 to mirror the integer-year convention used
  # by the other module pipelines.
  release_year   = 2024L,
  eada_dir       = "eada_2024_25",
  inst_file      = "instLevel.xlsx",
  sports_file    = "schools.xlsx",
  conferences_csv = "eada_conferences.csv"
)

# =============================================================================
# 1. EADA classification parser
# =============================================================================
#
# EADA's `classification_name` looks like:
#   "NCAA Division I-FBS"
#   "NCAA Division III with football"
#   "NAIA Division II"
#   "Other"
# We split this into a clean (body, division) pair. has_football is computed
# separately from the sport-level data (more accurate than the suffix).

.parse_athletics_class <- function(class_name) {
  s <- as.character(class_name)
  s_low <- tolower(s)
  body <- dplyr::case_when(
    is.na(s)                          ~ NA_character_,
    grepl("^ncaa",  s_low)            ~ "NCAA",
    grepl("^naia",  s_low)            ~ "NAIA",
    grepl("^njcaa", s_low)            ~ "NJCAA",
    grepl("^cccaa", s_low)            ~ "CCCAA",
    grepl("^nwac",  s_low)            ~ "NWAC",
    grepl("^uscaa", s_low)            ~ "USCAA",
    grepl("^nccaa", s_low)            ~ "NCCAA",
    TRUE                              ~ "Other"
  )
  div <- dplyr::case_when(
    is.na(s)                                                     ~ NA_character_,
    grepl("division\\s*i(\\b|-)",        s_low) & body == "NCAA" ~ "D1",
    grepl("division\\s*ii(\\b|-|\\s)",   s_low) & body == "NCAA" ~ "D2",
    grepl("division\\s*iii(\\b|-|\\s)",  s_low) & body == "NCAA" ~ "D3",
    body == "NAIA"                                                ~ "NAIA",
    TRUE                                                          ~ "Other"
  )
  tibble::tibble(athletics_body = body, athletics_division = div)
}

# =============================================================================
# 2. Build athletics facts + categorical extras
# =============================================================================

build_ath_data <- function(cfg = ATH_CONFIG) {
  inst_path  <- .data_path(cfg$eada_dir, cfg$inst_file)
  sport_path <- .data_path(cfg$eada_dir, cfg$sports_file)
  conf_path  <- .data_path(cfg$conferences_csv)

  if (!file.exists(inst_path))
    stop("EADA instLevel.xlsx not found at ", inst_path)
  if (!file.exists(sport_path))
    stop("EADA schools.xlsx (sport-level) not found at ", sport_path)

  message(sprintf("  reading EADA instLevel from %s ...", inst_path))
  inst <- read_excel(inst_path)

  message(sprintf("  reading EADA sport-level from %s ...", sport_path))
  sports <- read_excel(sport_path)

  # --- sport-level aggregation (one row per institution) -------------------
  sport_agg <- sports %>%
    mutate(
      unitid       = as.integer(unitid),
      PARTIC_MEN   = suppressWarnings(as.numeric(PARTIC_MEN)),
      PARTIC_WOMEN = suppressWarnings(as.numeric(PARTIC_WOMEN))
    ) %>%
    group_by(unitid) %>%
    summarise(
      mens_varsity_sports   = sum((tidyr::replace_na(PARTIC_MEN,   0)) > 0),
      womens_varsity_sports = sum((tidyr::replace_na(PARTIC_WOMEN, 0)) > 0),
      has_football          = any(tolower(trimws(Sports)) == "football", na.rm = TRUE),
      sports_list           = paste(sort(unique(Sports[!is.na(Sports)])),
                                     collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(total_varsity_sports = mens_varsity_sports + womens_varsity_sports)

  # --- institution-level join ---------------------------------------------
  joined <- inst %>%
    mutate(unitid = as.integer(unitid)) %>%
    left_join(sport_agg, by = "unitid") %>%
    mutate(
      male_athletes_undup   = suppressWarnings(as.numeric(UNDUP_CT_PARTIC_MEN)),
      female_athletes_undup = suppressWarnings(as.numeric(UNDUP_CT_PARTIC_WOMEN)),
      male_athletes_dup     = suppressWarnings(as.numeric(IL_SUM_PARTIC_MEN)),
      female_athletes_dup   = suppressWarnings(as.numeric(IL_SUM_PARTIC_WOMEN)),
      ef_total              = suppressWarnings(as.numeric(EFTotalCount)),
      ef_male               = suppressWarnings(as.numeric(EFMaleCount)),
      ef_female             = suppressWarnings(as.numeric(EFFemaleCount))
    ) %>%
    mutate(
      total_athletes_undup = tidyr::replace_na(male_athletes_undup,  0) +
                              tidyr::replace_na(female_athletes_undup, 0),
      pct_athletes_overall = dplyr::if_else(
        is.finite(ef_total) & ef_total > 0,
        total_athletes_undup / ef_total, NA_real_),
      pct_male_athletes    = dplyr::if_else(
        is.finite(ef_male) & ef_male > 0,
        male_athletes_undup / ef_male, NA_real_),
      pct_female_athletes  = dplyr::if_else(
        is.finite(ef_female) & ef_female > 0,
        female_athletes_undup / ef_female, NA_real_),
      multi_sport_ratio    = dplyr::if_else(
        is.finite(total_athletes_undup) & total_athletes_undup > 0,
        (tidyr::replace_na(male_athletes_dup,   0) +
         tidyr::replace_na(female_athletes_dup, 0)) / total_athletes_undup,
        NA_real_)
    )

  # --- conference join ----------------------------------------------------
  if (file.exists(conf_path)) {
    conf <- read_csv(conf_path, show_col_types = FALSE) %>%
      mutate(unitid = as.integer(unitid)) %>%
      transmute(unitid, athletics_conference = conference)
    message(sprintf("  joined %d Wikipedia-derived conferences from %s",
                    nrow(conf), conf_path))
  } else {
    message(sprintf(
      "  conferences CSV not found at %s; athletics_conference will be NA. ",
      conf_path),
      "Run python R/scrape_eada_conferences.py first to populate it.")
    conf <- tibble::tibble(unitid = integer(),
                            athletics_conference = character())
  }
  joined <- joined %>% left_join(conf, by = "unitid")

  # --- parsed body / division --------------------------------------------
  cls <- .parse_athletics_class(joined$classification_name)
  joined$athletics_body     <- cls$athletics_body
  joined$athletics_division <- cls$athletics_division

  joined
}

# =============================================================================
# 3. Long-form facts (unitid x year x metric x value)
# =============================================================================

# Variables that flow through the facts/variables pattern (used in the wide
# matrix that compute_peers() and the dashboard read from). Categorical
# fields (body, division, conference, has_football) live in schools_athletics.csv,
# not in facts.
.FACTS_METRICS <- c(
  "mens_varsity_sports",
  "womens_varsity_sports",
  "total_varsity_sports",
  "male_athletes_undup",
  "female_athletes_undup",
  "total_athletes_undup",
  "pct_athletes_overall",
  "pct_male_athletes",
  "pct_female_athletes",
  "multi_sport_ratio"
)

build_ath_facts <- function(joined, cfg = ATH_CONFIG) {
  joined %>%
    select(unitid, all_of(.FACTS_METRICS)) %>%
    pivot_longer(-unitid, names_to = "metric", values_to = "value") %>%
    filter(is.finite(value)) %>%
    mutate(year = cfg$release_year) %>%
    select(unitid, year, metric, value) %>%
    arrange(unitid, metric)
}

# Percentages live as 0-1 ratios in EADA; the rest of the app expresses
# percentages as 0-100. Convert here so format = "percentage" displays
# consistently with the other modules' percentage variables.
.scale_percentage_facts <- function(facts) {
  pct_metrics <- c("pct_athletes_overall", "pct_male_athletes",
                   "pct_female_athletes")
  facts %>%
    mutate(value = if_else(metric %in% pct_metrics, value * 100, value))
}

# =============================================================================
# 4. Variable catalog
# =============================================================================

build_ath_variables <- function() {
  tibble::tribble(
    ~metric,                  ~display_name,                                  ~source,         ~ipeds_table_or_formula,                                                       ~use_type,     ~comparison_scope, ~format,      ~neche_dashboard, ~coverage_note,
    # Clustering subset: three orthogonal signals — intensity (% of body),
    # breadth (number of sports), culture (multi-sport rate). Everything
    # else is descriptive (visible in the inspector and Side-by-Side, but
    # not folded into the peer distance to avoid double-counting since the
    # counts are linearly dependent with their percentages and totals).
    "pct_athletes_overall",   "Athletes as % of full-time UG enrollment",      "eada_derived",  "total_athletes_undup / EFTotalCount (EADA's full-time UG headcount)",          "clustering",  "cross_category",  "percentage", TRUE,             "Ranges from ~1% at large D-I publics to >40% at small D-III LACs. HC (D-I, Patriot League) has unusually high participation for a D-I school: ~23% of UG enrollment.",
    "total_varsity_sports",   "Total varsity sports",                          "eada_derived",  "mens_varsity_sports + womens_varsity_sports",                                  "clustering",  "cross_category",  "count",      FALSE,            "Breadth of program.",
    "multi_sport_ratio",      "Multi-sport athlete ratio",                     "eada_derived",  "(IL_SUM_PARTIC_MEN + IL_SUM_PARTIC_WOMEN) / total_athletes_undup",            "clustering",  "cross_category",  "ratio",      FALSE,            "Average sports per athlete. LACs run 1.20-1.35 (lots of two-sport athletes); D-I near 1.00.",
    # Descriptive variables — visible everywhere but excluded from theme weights.
    "mens_varsity_sports",    "Men's varsity sports",                          "eada_derived",  "count of sports in EADA schools.xlsx where PARTIC_MEN > 0",                    "descriptive", "cross_category",  "count",      FALSE,            "EADA 2024-25; ~2,037 schools filed. Single-sex institutions, service academies, and schools without intercollegiate athletics are missing.",
    "womens_varsity_sports",  "Women's varsity sports",                        "eada_derived",  "count of sports in EADA schools.xlsx where PARTIC_WOMEN > 0",                  "descriptive", "cross_category",  "count",      FALSE,            "Same as above.",
    "male_athletes_undup",    "Male athletes (unduplicated)",                  "eada",          "EADA instLevel.UNDUP_CT_PARTIC_MEN",                                           "descriptive", "cross_category",  "count",      FALSE,            "Unduplicated headcount: multi-sport athletes counted once.",
    "female_athletes_undup",  "Female athletes (unduplicated)",                "eada",          "EADA instLevel.UNDUP_CT_PARTIC_WOMEN",                                         "descriptive", "cross_category",  "count",      FALSE,            "Unduplicated headcount: multi-sport athletes counted once.",
    "total_athletes_undup",   "Total athletes (unduplicated)",                 "eada_derived",  "male_athletes_undup + female_athletes_undup",                                  "descriptive", "cross_category",  "count",      FALSE,            "Use this in any 'athletes / enrollment' calculation.",
    "pct_male_athletes",      "Male athletes as % of male full-time UG",       "eada_derived",  "male_athletes_undup / EFMaleCount",                                            "descriptive", "cross_category",  "percentage", FALSE,            "EADA's gender-specific UG denominator.",
    "pct_female_athletes",    "Female athletes as % of female full-time UG",   "eada_derived",  "female_athletes_undup / EFFemaleCount",                                        "descriptive", "cross_category",  "percentage", FALSE,            "EADA's gender-specific UG denominator."
  ) %>%
    mutate(category        = "athletics",
           neche_peer_set  = FALSE,
           notes           = NA_character_) %>%
    select(metric, category, display_name, source, ipeds_table_or_formula,
           use_type, comparison_scope, format, neche_peer_set, neche_dashboard,
           coverage_note, notes)
}

# =============================================================================
# 5. Categorical extras for schools.csv
# =============================================================================

build_schools_athletics <- function(joined) {
  joined %>%
    transmute(
      unitid                         = as.integer(unitid),
      athletics_body,
      athletics_division,
      athletics_conference,
      has_football                   = tidyr::replace_na(has_football, FALSE),
      athletics_classification_raw   = classification_name,
      athletics_classification_code  = suppressWarnings(as.integer(ClassificationCode)),
      athletics_classification_other = ClassificationOther,
      athletics_sports_list          = sports_list
    ) %>%
    arrange(unitid)
}

# =============================================================================
# 6. Coverage report (mirrors aid_coverage_report's shape)
# =============================================================================

ath_coverage_report <- function(facts, schools) {
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
  years_per_metric <- metric_year_pairs %>% count(metric, name = "n_years")
  cov %>% left_join(years_per_metric, by = "metric") %>% arrange(metric)
}

# =============================================================================
# 7. RUN
# =============================================================================

run_ath_module <- function(cfg = ATH_CONFIG) {
  message("== Athletics Module (EADA) ==")

  schools_csv <- .out_path("schools.csv")
  if (!file.exists(schools_csv))
    stop(schools_csv, " not found. Run R/schools_pipeline.R first.")
  schools <- as_tibble(read.csv(schools_csv, stringsAsFactors = FALSE))
  message(sprintf("  loaded %s: %d institutions", schools_csv, nrow(schools)))

  joined <- build_ath_data(cfg)
  message(sprintf("  joined sport-level and inst-level rows: %d EADA institutions",
                  nrow(joined)))

  ath_facts <- joined %>%
    build_ath_facts(cfg) %>%
    .scale_percentage_facts() %>%
    semi_join(schools, by = "unitid")        # restrict to project universe

  schools_ath <- joined %>%
    build_schools_athletics() %>%
    semi_join(schools, by = "unitid")

  ath_variables <- build_ath_variables()

  write.csv(ath_facts,     .out_path("ath_facts.csv"),         row.names = FALSE)
  write.csv(ath_variables, .out_path("ath_variables.csv"),     row.names = FALSE)
  write.csv(schools_ath,   .out_path("schools_athletics.csv"), row.names = FALSE)
  message(sprintf("Wrote %s, %s, %s",
                  .out_path("ath_facts.csv"),
                  .out_path("ath_variables.csv"),
                  .out_path("schools_athletics.csv")))

  message(sprintf("\nFacts: %d rows across %d metrics, %d institutions.",
                  nrow(ath_facts),
                  length(unique(ath_facts$metric)),
                  length(unique(ath_facts$unitid))))
  message(sprintf("Categorical extras: %d institutions with EADA classification, %d with conference.",
                  sum(!is.na(schools_ath$athletics_body)),
                  sum(!is.na(schools_ath$athletics_conference))))

  message("\nCoverage (% of universe with a value), by metric and control group:")
  print(ath_coverage_report(ath_facts, schools))

  invisible(list(facts = ath_facts,
                 variables = ath_variables,
                 schools_athletics = schools_ath))
}

# -----------------------------------------------------------------------------
# Usage:
#   res_ath <- run_ath_module()
# -----------------------------------------------------------------------------
