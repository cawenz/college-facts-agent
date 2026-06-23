# =============================================================================
# Shared Schools Pipeline   (repo version)
# Holy Cross peer-comparison project
#
# REPO LAYOUT ASSUMPTION
#   This script expects to be sourced from the repo root, with:
#     data/IPEDS{year-range}.Rda          IPEDS collection bundles
#     data/2025-Public-Data-File.xlsx     Carnegie 2025 public data file
#     output/                              destination for produced CSVs
#
# Loads IPEDS collections directly via load() rather than ipeds::load_ipeds()
# so the repo is portable across machines without needing the ipeds package
# to be configured to find a specific download directory.
#
# OUTPUTS  (written to output/)
#   schools.csv       one row per institution; institutional attributes,
#                     classifications (IPEDS + US News + Carnegie), value
#                     labels for coded fields.
#   value_labels.csv  canonical (table_name, variable, code) -> label table.
#
# REQUIRED PACKAGES
#   install.packages(c("dplyr","tidyr","purrr","stringr","httr2","jsonlite","readxl"))
#   (the ipeds package is NO LONGER required for this pipeline)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(httr2); library(jsonlite); library(readxl)
})

# ---- repo paths (resolve relative to wherever this is sourced from) -------
.repo_root  <- function() getwd()
.data_path  <- function(...) file.path(.repo_root(), "data", ...)
.out_path   <- function(...) file.path(.repo_root(), "output", ...)
dir.create(.out_path(), showWarnings = FALSE, recursive = TRUE)

# ---- CONFIG ---------------------------------------------------------------
SCHOOLS_CONFIG <- list(
  collection_years = 2020:2024,
  keep_sectors     = c(1, 2),
  ai_base    = "https://ai.usnews.com/api/v1/client_api",
  ai_dataset = "undergraduate",
  ai_key     = Sys.getenv("ACADEMIC_INSIGHTS_API_KEY"),
  scorecard_key = Sys.getenv("SCORECARD_API_KEY"),
  ranked_classes = c("national-universities", "national-liberal-arts-colleges"),
  labels_year = 2024,
  carnegie_file = .data_path("2025-Public-Data-File.xlsx"),
  # Washington Monthly College Guide — downloaded annually from
  # https://washingtonmonthly.com/<year>-college-guide/ (look for the
  # "download the full data set" link on any of the category pages).
  # Pattern: data/washington_monthly_<year>.xlsx. Build helper picks the
  # latest year present.
  washington_monthly_file = .data_path("washington_monthly_2025.xlsx"),
  # Forbes America's Top Colleges — scraped annually with
  # R/scrape_forbes_rankings.py. Forbes has no public data feed, so the
  # script uses Playwright to load forbes.com/top-colleges/ and dump
  # rank + name + state + type. No IPEDS in the source; build_forbes()
  # matches names to schools by normalized exact match within state.
  forbes_file = .data_path("forbes_top_colleges_2025.csv"),
  # US News overall (within-category) rank — Academic Insights metric_id 24
  # ("Overall Rank"). Confirmed via
  #   search_ai_metrics(SCHOOLS_CONFIG, contains = "rank")
  # Pulled for the latest AI-equivalent year. Rank is a per-institution
  # snapshot, not a longitudinal facts series, so we keep only the latest
  # year and store as usnews_rank.
  usnews_rank_metric_id = 24L,
  labeled_fields = c(
    "sector", "control", "iclevel",
    "hbcu", "hospital", "medical", "tribal",
    "instcat", "locale", "instsize",
    "basic2021", "setting2025", "highest_degree_2025",
    "ic2025size", "ic2025alf", "apm", "gpm"
  )
)

# =============================================================================
# 1. IPEDS retrieval helpers - direct .Rda load, no ipeds package required
# =============================================================================
.IPEDS_CACHE <- if (exists(".IPEDS_CACHE")) .IPEDS_CACHE else new.env()

# IPEDS .Rda files are named IPEDS{prev_year}-{yy}.Rda for a collection year.
# load_ipeds(Y+1) returns the collection-year-Y tables. So the file holding
# collection year 2023 is IPEDS2023-24.Rda; collection year 2024 -> IPEDS2024-25.Rda.
.rda_path_for_collection <- function(collection_year) {
  yy <- sprintf("%02d", (collection_year + 1) %% 100)
  .data_path(sprintf("IPEDS%d-%s.Rda", collection_year, yy))
}

load_collection <- function(collection_year) {
  key <- as.character(collection_year)
  if (is.null(.IPEDS_CACHE[[key]])) {
    rda <- .rda_path_for_collection(collection_year)
    if (!file.exists(rda)) {
      warning(sprintf("Missing IPEDS file for collection %d: %s",
                      collection_year, rda))
      .IPEDS_CACHE[[key]] <- NULL
    } else {
      message(sprintf("  load(%s)  ->  collection year %d",
                      basename(rda), collection_year))
      env <- new.env()
      load(rda, envir = env)
      # the package convention is that the loaded object is named `db`
      if (!"db" %in% ls(env)) {
        warning(sprintf("Expected object 'db' in %s, found: %s",
                        rda, paste(ls(env), collapse = ", ")))
        .IPEDS_CACHE[[key]] <- NULL
      } else {
        .IPEDS_CACHE[[key]] <- env$db
      }
    }
  }
  .IPEDS_CACHE[[key]]
}

# Generic table fetch (used for valueSets, varTable, etc.)
get_raw_table <- function(collection_year, table_name) {
  lst <- load_collection(collection_year)
  if (is.null(lst)) return(NULL)
  hit <- names(lst)[toupper(names(lst)) == toupper(table_name)]
  if (!length(hit)) return(NULL)
  as_tibble(lst[[hit[1]]])
}

# Table fetch with UNITID normalisation (used by module pipelines)
get_table <- function(collection_year, table_name) {
  df <- get_raw_table(collection_year, table_name)
  if (is.null(df)) return(NULL)
  names(df) <- toupper(gsub("[.]", "_", names(df)))
  if (!"UNITID" %in% names(df)) return(NULL)
  df %>% rename(unitid = UNITID) %>% mutate(unitid = as.integer(unitid))
}

# =============================================================================
# 2. Academic Insights helper
# =============================================================================
ai_get <- function(cfg, path, query = list()) {
  if (cfg$ai_key == "") stop("ACADEMIC_INSIGHTS_API_KEY not set.")
  request(cfg$ai_base) %>% req_url_path_append(path) %>%
    req_url_query(!!!query) %>%
    req_headers(Authorization = cfg$ai_key, Accept = "application/json") %>%
    req_user_agent("hc-peer-pipeline") %>%
    req_throttle(rate = 30 / 60) %>%
    req_retry(max_tries = 5) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE)
}

# Discovery helper — query the AI metrics catalog and (optionally) filter
# by a substring of the description. Use this to find metric_ids before
# wiring them into any *_CONFIG$ai_metric_ids list. Example:
#   search_ai_metrics(SCHOOLS_CONFIG, contains = "rank")
# Also defined in R/aid_module_pipeline.R for the same purpose; kept here
# so schools_pipeline.R is self-contained.
search_ai_metrics <- function(cfg, contains = NULL) {
  q <- list(); if (!is.null(contains)) q$description_contains <- contains
  as_tibble(ai_get(cfg, paste0("metrics/", cfg$ai_dataset), query = q))
}

# Year-naming conventions:
#   IPEDS uses fall-year (HD2024 = fall 2024 collection = academic year 2024-25)
#   Academic Insights publishes data with a 2-year lag relative to IPEDS year.
#   Empirical verification (Holy Cross applicants, metric_id 1, across 7 years):
#     AI year 2022 = IPEDS year 2020 (both = 2020-21 academic year)
#     AI year 2024 = IPEDS year 2022 (both = 2022-23 academic year)
#     AI year 2026 = IPEDS year 2024 (both = 2024-25 academic year)
#   So AI year Y refers to the same academic year as IPEDS year Y - 2.
# Our facts tables use IPEDS-naming throughout. These two helpers translate
# between the conventions when we read from or write to AI:
#   ipeds_to_ai_year(2024) -> 2026  (ask AI for the right year when our IPEDS code says 2024)
#   ai_to_ipeds_year(2026) -> 2024  (convert an AI-returned year to facts-table year)
ipeds_to_ai_year <- function(y) as.integer(y) + 2L
ai_to_ipeds_year <- function(y) as.integer(y) - 2L

# =============================================================================
# 2b. College Scorecard helper - paged fetch
# =============================================================================
# Scorecard API:
#   base:   https://api.data.gov/ed/collegescorecard/v1/schools
#   key:    SCORECARD_API_KEY (api_key query parameter)
#   paging: per_page up to 100; iterate page index 0, 1, ...
#   fields: comma-separated list of dotted field paths, e.g.
#           "id,school.name,school.accreditor"
# Returns the concatenated "results" arrays as a tibble.
scorecard_get <- function(cfg, fields, query = list(), per_page = 100) {
  if (cfg$scorecard_key == "")
    stop("SCORECARD_API_KEY not set in environment.")
  
  base <- "https://api.data.gov/ed/collegescorecard/v1/schools"
  out  <- list()
  page <- 0L
  repeat {
    q <- c(list(api_key = cfg$scorecard_key,
                fields  = paste(fields, collapse = ","),
                per_page = per_page,
                page = page),
           query)
    resp <- request(base) %>%
      req_url_query(!!!q) %>%
      req_user_agent("hc-peer-pipeline") %>%
      req_throttle(rate = 60 / 60) %>%
      req_retry(max_tries = 5) %>%
      req_perform() %>%
      resp_body_json(simplifyVector = TRUE)
    res <- resp$results
    if (is.null(res) || length(res) == 0) break
    out[[length(out) + 1]] <- as_tibble(res)
    total <- resp$metadata$total %||% 0
    page  <- page + 1L
    if (page * per_page >= total) break
  }
  if (!length(out)) return(tibble())
  bind_rows(out)
}

# small null-coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# 3. US News classification (Academic Insights, by state)
# =============================================================================
build_classification <- function(cfg) {
  message("Pulling US News classification (schools, by state) ...")
  states <- c(state.abb, "DC", "PR")
  raw <- map_dfr(states, function(st) {
    df <- tryCatch(
      as_tibble(ai_get(cfg, paste0("schools/", cfg$ai_dataset),
                       query = list(state = st))),
      error = function(e) { warning(sprintf("state %s failed: %s",
                                            st, conditionMessage(e))); tibble() })
    message(sprintf("  %s: %d schools", st, nrow(df)))
    df
  })
  if (!nrow(raw) || !"ipeds_id" %in% names(raw)) {
    warning("classification pull returned nothing usable"); return(tibble())
  }
  raw %>%
    filter(!is.na(ipeds_id)) %>%
    transmute(unitid = as.integer(ipeds_id), usnews_classification = classification) %>%
    distinct(unitid, .keep_all = TRUE) %>%
    # PROVISIONAL value derived from classification membership. The
    # authoritative override later in build_schools() sets this from
    # !is.na(usnews_rank) once the rank table has been joined; this
    # fallback is what we use when the rank metric is not configured.
    # Includes regional-colleges-* because US News publishes numeric
    # overall ranks for that group too — they belong in the ranked
    # universe alongside the regional universities.
    mutate(in_ranked_universe =
             usnews_classification %in% cfg$ranked_classes |
             grepl("^regional-universities-", usnews_classification) |
             grepl("^regional-colleges-",      usnews_classification))
}

# =============================================================================
# 3a. US News overall rank (Academic Insights, single metric, latest year)
# =============================================================================
# Returns tibble(unitid, usnews_rank). NA for unranked schools.
# Helper to discover the right metric_id, run once when wiring this up:
#   search_ai_metrics(SCHOOLS_CONFIG, contains = "rank")
# then set SCHOOLS_CONFIG$usnews_rank_metric_id.
build_usnews_rank <- function(cfg) {
  mid <- cfg$usnews_rank_metric_id
  if (is.null(mid) || is.na(mid)) {
    message("SCHOOLS_CONFIG$usnews_rank_metric_id not set; skipping rank pull. ",
            "Discover the metric_id via search_ai_metrics(SCHOOLS_CONFIG, contains = \"rank\").")
    return(tibble(unitid = integer(), usnews_rank = integer()))
  }
  # Latest AI year corresponds to our latest collection year.
  ai_year <- ipeds_to_ai_year(max(cfg$collection_years))
  message(sprintf("Pulling US News rank (metric_id %d, AI year %d) ...",
                  mid, ai_year))
  res <- tryCatch(
    ai_get(cfg, paste0("facts/", cfg$ai_dataset),
           query = list(metric_ids = mid, years = ai_year, all_data = "true")),
    error = function(e) {
      warning(sprintf("rank facts pull failed: %s", conditionMessage(e)))
      NULL
    })
  df <- as_tibble(res)
  # facts/{dataset} keys IPEDS as school_ipeds_id (not ipeds_id, which is
  # the schools/{dataset} convention). The numeric rank is in `value`.
  if (!nrow(df) || !"school_ipeds_id" %in% names(df) || !"value" %in% names(df)) {
    warning("rank facts pull returned nothing usable")
    return(tibble(unitid = integer(), usnews_rank = integer()))
  }
  out <- df %>%
    filter(!is.na(school_ipeds_id)) %>%
    transmute(unitid      = as.integer(school_ipeds_id),
              usnews_rank = suppressWarnings(as.integer(value))) %>%
    distinct(unitid, .keep_all = TRUE)
  message(sprintf("  pulled rank for %d institutions", nrow(out)))
  out
}

# =============================================================================
# 3b. College Scorecard - accreditor (institutional attribute)
# =============================================================================
# Pulls each school's accreditor name from the latest Scorecard release.
# Used as a filter/scope field on schools.csv, not as a clustering variable.
# Returns tibble(unitid, accreditor) or tibble() on failure.
build_accreditor <- function(cfg) {
  if (cfg$scorecard_key == "") {
    warning("SCORECARD_API_KEY not set; skipping accreditor pull.")
    return(tibble(unitid = integer(), accreditor = character()))
  }
  message("Pulling accreditor from College Scorecard ...")
  fields <- c("id", "school.accreditor")
  # Filter to Title-IV-participating SECTOR 1/2 (public/private NFP 4-year)
  # to stay within the universe and avoid pulling the entire Scorecard catalog.
  raw <- tryCatch(
    scorecard_get(cfg, fields, query = list(
      `school.degrees_awarded.predominant__range` = "3..4",
      `school.ownership` = "1,2"
    )),
    error = function(e) {
      warning(sprintf("Scorecard pull failed: %s", conditionMessage(e)))
      tibble()
    })
  if (!nrow(raw) || !"id" %in% names(raw)) {
    warning("Scorecard accreditor pull returned nothing usable")
    return(tibble(unitid = integer(), accreditor = character()))
  }
  acc_col <- if ("school.accreditor" %in% names(raw)) "school.accreditor" else
    grep("accreditor", names(raw), value = TRUE)[1]
  if (is.na(acc_col)) {
    warning("Scorecard response missing accreditor field")
    return(tibble(unitid = integer(), accreditor = character()))
  }
  out <- raw %>%
    transmute(unitid = as.integer(id),
              accreditor = as.character(.data[[acc_col]])) %>%
    filter(!is.na(unitid)) %>%
    distinct(unitid, .keep_all = TRUE)
  message(sprintf("  pulled accreditor for %d institutions", nrow(out)))
  out
}

# =============================================================================
# 3c. Washington Monthly College Guide rankings
# =============================================================================
# Reads the publicly distributed WM College Guide spreadsheet (downloaded
# manually each year from washingtonmonthly.com/<year>-college-guide/). The
# file has four sheets:
#   "All"           — every 4-year institution WM ranks, including National
#                     Universities (no separate sheet for them)
#   "Master's"      — Master's universities (category-specific rank)
#   "Baccalaureate" — Bachelor's colleges
#   "Liberal Arts"  — National Liberal Arts colleges
# Schools in Master's/Bacc/LA also appear in "All", but with a different
# (master-list) rank. For peer comparison we surface the more specific
# category-rank when available, and the "All" rank for National Universities
# that have no category sheet.
#
# Returns tibble(unitid, wamo_rank, wamo_category).
#   wamo_category ∈ {"Liberal Arts", "Baccalaureate", "Master's", "National"}
#   "National" means the school was only in "All" — by elimination, a
#   National University.
build_washington_monthly <- function(cfg) {
  fp <- cfg$washington_monthly_file
  if (is.null(fp) || !file.exists(fp)) {
    warning(sprintf("Washington Monthly file not found at '%s'; ",
                    fp %||% "<unset>"),
            "schools will be built without WM rankings.")
    return(tibble(unitid = integer(),
                  wamo_rank = integer(),
                  wamo_category = character()))
  }
  message(sprintf("Loading Washington Monthly rankings: %s ...",
                  basename(fp)))
  sheets <- c("All", "Master's", "Baccalaureate", "Liberal Arts")
  # Header row is row 1 inside the sheet's data area; readxl's skip = 1
  # advances past the title row that sits above it in each sheet.
  read_sheet <- function(s) {
    d <- suppressWarnings(suppressMessages(
      readxl::read_excel(fp, sheet = s, skip = 1)))
    d %>%
      transmute(unitid    = suppressWarnings(as.integer(UnitID)),
                wamo_rank = suppressWarnings(as.integer(Rank)),
                sheet     = s) %>%
      filter(!is.na(unitid), !is.na(wamo_rank))
  }
  stacked <- bind_rows(lapply(sheets, read_sheet))

  # For schools present in a category sheet, prefer the category rank;
  # for schools only in "All", treat them as National Universities.
  cat_rows <- stacked %>% filter(sheet != "All")
  only_all <- stacked %>%
    filter(sheet == "All", !unitid %in% cat_rows$unitid) %>%
    mutate(sheet = "National")
  out <- bind_rows(cat_rows, only_all) %>%
    transmute(unitid,
              wamo_rank,
              wamo_category = sheet) %>%
    distinct(unitid, .keep_all = TRUE)

  by_cat <- out %>% count(wamo_category, name = "n")
  message(sprintf("  %d schools with a WM rank (%s)",
                  nrow(out),
                  paste(sprintf("%s=%d", by_cat$wamo_category, by_cat$n),
                        collapse = ", ")))
  out
}

# =============================================================================
# 3d. Forbes America's Top Colleges
# =============================================================================
# Reads the CSV produced by R/scrape_forbes_rankings.py and matches each
# Forbes row to a unitid via normalized exact match within state, then a
# small fuzzy fallback for the residual. Returns tibble(unitid, forbes_rank).
#
# Matching strategy (kept simple — Forbes publishes ~500 well-known
# schools; the exact-within-state path catches the overwhelming majority):
#   1. lowercase + strip punctuation + collapse whitespace + drop
#      trailing "university"/"college" filler
#   2. inner-join on (norm_name, stabbr=state)
#   3. base::adist() fuzzy match on the residual, within same state, with
#      a tight edit-distance cap (max 4) so loose matches are rejected
# Unmatched rows are written to data/forbes_unmatched_<year>.csv as an
# audit trail — review and add explicit overrides to FORBES_OVERRIDES
# below if you spot a real school being missed.
#
# Requires an institutional lookup df with columns (unitid, instnm, stabbr).
FORBES_OVERRIDES <- c(
  # Hand-curated forbes_name -> unitid for stubborn mismatches. Add
  # entries after reviewing data/forbes_top_colleges_<year>_unmatched.csv.
  # Key is the exact `name` value from the Forbes CSV.
  "Virginia Tech"                                     = "233921",  # Virginia Polytechnic Institute and State University
  "CUNY, Baruch College"                              = "190664",  # CUNY Bernard M Baruch College
  "Louisiana State University"                        = "159391",  # LSU and A&M College
  "CUNY, The City College of New York"                = "190150",  # CUNY City College
  "Sewanee—University of the South"              = "221768",  # University of the South (em-dash variant)
  "The Citadel"                                       = "217864",  # The Citadel-Military College of South Carolina
  "Indiana University-Purdue University, Indianapolis" = "151111", # IUPUI legacy unitid (split since 2024)
  "SUNY, College at New Paltz"                        = "196097",  # SUNY at New Paltz
  "St. Joseph's College (NY)"                         = "194161",  # St Joseph's College-New York
  "Montana Tech of the University of Montana"         = "180489",  # Montana Technological University
  "SUNY College at Old Westbury"                      = "196219",  # SUNY College at Old Westbury
  "SUNY Cortland"                                     = "196060"   # SUNY College at Cortland
)

.forbes_normalize <- function(s) {
  s <- tolower(s %||% "")
  # Strip punctuation BEFORE any keyword passes
  s <- gsub("[[:punct:]]+", " ", s)
  s <- gsub("\\s+", " ", s)
  s <- trimws(s)

  # ---- IPEDS-side suffixes we know cause misses ----
  # "-Main Campus" appears on many multi-campus state systems (Pitt,
  # UVA, Georgia Tech, etc.). Drop these BEFORE the generic "campus"
  # strip so we don't leave a dangling "main".
  s <- gsub("\\s+main campus\\s*$", "", s)
  # Generic trailing " campus" — "University of Washington Seattle Campus"
  # -> "university of washington seattle". This is intentionally NOT
  # combined with a city-name strip; we want UT Austin / UT Dallas /
  # UT El Paso etc. to stay distinct.
  s <- gsub("\\s+campus\\s*$", "", s)
  # Some IPEDS names repeat the city as both the dash-suffix and the
  # implied location ("University of Pittsburgh-Pittsburgh Campus").
  # After the punct + campus strip we get "...pittsburgh pittsburgh".
  # Collapse duplicate adjacent tokens of length > 3 (length guard
  # avoids collapsing legitimate "york new york" style sequences).
  s <- gsub("\\b(\\w{4,})\\s+\\1\\b", "\\1", s)
  # Columbia in IPEDS is "Columbia University in the City of New York".
  s <- gsub("\\s+in the city of new york\\s*$", "", s)
  # Cooper Union etc. have very long formal names.
  s <- gsub("\\s+for the advancement of.*$", "", s)
  # Tulane is "Tulane University of Louisiana" in IPEDS.
  s <- gsub("\\s+of louisiana\\s*$", "", s)

  # ---- Forbes-side patterns ----
  # Trailing 2-letter state disambiguator: "trinity college ct" -> "trinity college"
  s <- gsub("\\s+[a-z]{2}\\s*$", "", s)
  # "CUNY, X" -> "x"  /  "X, SUNY" -> "x"  (system name carries no info
  # after the campus-name token is identified)
  s <- gsub("^cuny\\s+", "", s)
  s <- gsub("\\s+suny\\s*$", "", s)
  s <- gsub("^suny\\s+", "", s)
  # Filler words
  s <- gsub("^the\\s+", "", s)
  s <- gsub("\\s+the\\s+", " ", s)
  s <- gsub("\\s+at\\s+", " ", s)

  s <- gsub("\\s+", " ", s)
  trimws(s)
}

build_forbes <- function(cfg, id_lookup) {
  fp <- cfg$forbes_file
  if (is.null(fp) || !file.exists(fp)) {
    message(sprintf("Forbes file not found at '%s'; skipping.",
                    fp %||% "<unset>"))
    return(tibble(unitid = integer(), forbes_rank = integer()))
  }
  if (is.null(id_lookup) ||
      !all(c("unitid", "instnm", "stabbr") %in% names(id_lookup))) {
    warning("build_forbes: id_lookup needs (unitid, instnm, stabbr); skipping")
    return(tibble(unitid = integer(), forbes_rank = integer()))
  }
  message(sprintf("Loading Forbes rankings: %s ...", basename(fp)))
  forbes <- suppressMessages(readr::read_csv(fp, show_col_types = FALSE))
  forbes$norm_name <- .forbes_normalize(forbes$name)
  forbes$state     <- toupper(trimws(as.character(forbes$state)))

  lk <- id_lookup %>%
    transmute(unitid    = as.integer(unitid),
              norm_name = .forbes_normalize(instnm),
              state     = toupper(trimws(stabbr)))

  # 1. Hand-curated overrides
  if (length(FORBES_OVERRIDES)) {
    ov <- tibble::tibble(name = names(FORBES_OVERRIDES),
                          unitid = as.integer(FORBES_OVERRIDES))
    fmatch_ov <- forbes %>% inner_join(ov, by = "name")
    remaining <- forbes %>% anti_join(fmatch_ov, by = "rank")
  } else {
    # No overrides set — nothing matched yet, all rows still candidates.
    # (Avoids anti_join() on an empty zero-column tibble, which errors
    # because the `by` columns don't exist.)
    fmatch_ov <- tibble(rank = integer(), unitid = integer())
    remaining <- forbes
  }

  # 2. Exact normalized match within state
  fmatch_x <- remaining %>%
    inner_join(lk, by = c("norm_name", "state"))
  remaining <- remaining %>% anti_join(fmatch_x, by = "rank")

  # 3. Fuzzy fallback within state (base R adist, tight cap)
  fmatch_f <- list()
  for (i in seq_len(nrow(remaining))) {
    fr <- remaining[i, ]
    pool <- lk[lk$state == fr$state, , drop = FALSE]
    if (!nrow(pool)) next
    d <- as.integer(adist(fr$norm_name, pool$norm_name,
                           ignore.case = TRUE, partial = FALSE))
    j <- which.min(d)
    # Allow slightly more slack here than before — normalize() already
    # removes the easy collisions; remaining variation is mostly
    # campus-name differences within the same state.
    if (length(j) && d[j] <= 6) {
      fmatch_f[[length(fmatch_f) + 1]] <- tibble(
        rank = fr$rank, name = fr$name, state = fr$state,
        unitid = pool$unitid[j])
    }
  }
  fmatch_f <- bind_rows(fmatch_f)

  matched <- bind_rows(
    fmatch_ov %>% transmute(rank, unitid),
    fmatch_x  %>% transmute(rank, unitid),
    fmatch_f  %>% transmute(rank, unitid)
  ) %>%
    distinct(unitid, .keep_all = TRUE)

  out <- matched %>%
    transmute(unitid = as.integer(unitid),
              forbes_rank = as.integer(rank))

  unmatched <- forbes %>% anti_join(matched, by = "rank")
  if (nrow(unmatched)) {
    out_path <- .data_path(sub("\\.csv$",
                                "_unmatched.csv",
                                basename(fp)))
    suppressMessages(readr::write_csv(unmatched, out_path))
    message(sprintf("  matched %d of %d Forbes schools to unitid (%d unmatched -> %s)",
                    nrow(out), nrow(forbes), nrow(unmatched),
                    basename(out_path)))
  } else {
    message(sprintf("  matched all %d Forbes schools to unitid", nrow(out)))
  }
  out
}

# =============================================================================
# 4. Carnegie 2025 Public Data File - data + value labels
# =============================================================================
build_carnegie <- function(cfg) {
  if (!file.exists(cfg$carnegie_file)) {
    warning(sprintf("Carnegie file not found at '%s'. Schools will be built ",
                    "without Carnegie classifications.", cfg$carnegie_file))
    return(list(data = tibble(unitid = integer()), labels = tibble()))
  }
  message(sprintf("Loading Carnegie 2025 data file: %s ...",
                  basename(cfg$carnegie_file)))
  
  wanted <- c(
    "unitid",
    "ic2025", "ic2025name",
    "saec2025", "saec2025name",
    "research2025", "research2025name",
    "setting2025", "highest_degree_2025", "basic2021",
    "ic2025size", "ic2025alf", "apm", "gpm",
    # academic concentration (one-time Carnegie snapshot; documented as an
    # admissions-relevant institutional attribute)
    "apm_max_cip2percent", "apm_max_cip2_name",
    # earnings_ratio: CCIHE's SAEC computation of earnings vs expected
    # earnings given demographics. Outcomes-relevant; kept here as a
    # one-time snapshot. source = ccihe in outcomes_variables.csv.
    "earnings_ratio",
    "pbi", "annhsi", "aanapisi", "hsi", "nasnti", "womenonly",
    "rpu", "cce", "lpp"
  )
  d <- readxl::read_excel(cfg$carnegie_file, sheet = "data", na = c("", "NA")) %>%
    select(any_of(wanted)) %>%
    mutate(unitid = suppressWarnings(as.integer(unitid))) %>%
    filter(!is.na(unitid))
  
  rn <- c(ic2025name = "ic2025_label",
          saec2025name = "saec2025_label",
          research2025name = "research2025_label")
  for (old in names(rn)) {
    if (old %in% names(d)) d <- rename(d, !!rn[[old]] := !!sym(old))
  }
  message(sprintf("  loaded Carnegie data: %d institutions, %d columns",
                  nrow(d), ncol(d)))
  
  vraw <- readxl::read_excel(cfg$carnegie_file, sheet = "values",
                             col_names = c("variable", "code", "label"),
                             skip = 1, na = c("", "NA"))
  v <- vraw %>%
    fill(variable, .direction = "down") %>%
    filter(!is.na(code), !is.na(label)) %>%
    mutate(variable = toupper(trimws(as.character(variable))),
           code     = as.character(code),
           label    = as.character(label))
  shared <- v %>% filter(grepl("^APM AND GPM$", variable))
  if (nrow(shared)) {
    v <- v %>% filter(!grepl("^APM AND GPM$", variable)) %>%
      bind_rows(shared %>% mutate(variable = "APM"),
                shared %>% mutate(variable = "GPM"))
  }
  v <- v %>% mutate(variable = ifelse(variable == "SAEC25", "SAEC2025", variable))
  v <- v %>%
    transmute(table_name = "Carnegie2025", variable, code, label) %>%
    distinct(table_name, variable, code, .keep_all = TRUE)
  
  message(sprintf("  loaded Carnegie value labels: %d rows across %d variables",
                  nrow(v), n_distinct(v$variable)))
  
  list(data = d, labels = v)
}

# =============================================================================
# 5. IPEDS value labels (returns tibble; combined with Carnegie before writing)
# =============================================================================
build_ipeds_value_labels <- function(cfg) {
  yr <- cfg$labels_year
  vs_name <- paste0("valueSets", sprintf("%02d", yr %% 100))
  message(sprintf("Building IPEDS value labels from %s ...", vs_name))
  vs <- get_raw_table(yr, vs_name)
  if (is.null(vs)) {
    warning(sprintf("%s not found in collection %d", vs_name, yr))
    return(tibble(table_name=character(), variable=character(),
                  code=character(), label=character()))
  }
  names(vs) <- tolower(names(vs))
  pick <- function(...) { hits <- intersect(c(...), names(vs)); if (length(hits)) hits[1] else NA_character_ }
  tcol <- pick("tablename", "table_name", "table")
  vcol <- pick("varname", "variable", "varnme")
  ccol <- pick("codevalue", "code", "value")
  lcol <- pick("valuelabel", "label", "labelvalue")
  if (any(is.na(c(tcol, vcol, ccol, lcol)))) {
    warning("valueSets has unexpected columns: ",
            paste(names(vs), collapse = ", "))
    return(tibble(table_name=character(), variable=character(),
                  code=character(), label=character()))
  }
  out <- tibble(
    table_name = as.character(vs[[tcol]]),
    variable   = toupper(as.character(vs[[vcol]])),
    code       = as.character(vs[[ccol]]),
    label      = as.character(vs[[lcol]])
  ) %>% distinct(table_name, variable, code, .keep_all = TRUE)
  message(sprintf("  IPEDS: %d rows across %d variables",
                  nrow(out), n_distinct(out$variable)))
  out
}

attach_label <- function(df, code_col, variable, labels,
                         label_col = paste0(code_col, "_label")) {
  if (!code_col %in% names(df) || !nrow(labels)) return(df)
  lk <- labels %>% filter(toupper(.data$variable) == toupper(!!variable)) %>%
    distinct(code, label)
  if (!nrow(lk)) return(df)
  df %>%
    mutate(.code_lookup = as.character(.data[[code_col]])) %>%
    left_join(lk, by = c(".code_lookup" = "code")) %>%
    rename(!!label_col := label) %>%
    select(-.code_lookup)
}

# =============================================================================
# 5b. RELIGIOUS AFFILIATION DECODE
# =============================================================================
# IPEDS IC table's RELAFFIL field uses a numeric code. Codes verified against
# the official IPEDS IC2023 codebook (RELAFFIL data dictionary).
#
# We return three columns:
#   religious_affiliation_code  : raw IPEDS integer code
#   religious_affiliation       : human-readable denomination label
#   religious_tradition         : broader tradition rollup (Catholic /
#                                 Protestant / Other Christian / Jewish /
#                                 Other / NA)
#
# The tradition rollup is useful for clustering because the specific
# denomination granularity (50+ codes) is rarely meaningful for institutional
# comparison, while "Catholic vs Mainline Protestant vs Other Christian vs
# Jewish vs Other" captures most of the variation institutions actually care
# about.
#
# Tradition assignments follow standard categorical conventions:
#   Catholic         : Roman Catholic only
#   Protestant       : denominationally-specific Protestant traditions
#                      (Lutheran, Methodist, Presbyterian, Baptist, etc.)
#   Other Christian  : Eastern Orthodox, LDS, nondenominational/ecumenical,
#                      multi-denominational
#   Jewish           : Jewish
#   Other            : Muslim, Buddhist, Unitarian Universalist, and the
#                      catch-all "Other (none of the above)"
#
# Codes -1 ("Not reported") and -2 ("Not applicable") map to NA tradition.
# Any code we don't recognize is labeled "Other religious" rather than NA,
# so downstream filters don't silently drop rows.
.RELAFFIL_LOOKUP <- tribble(
  ~code, ~label,                                              ~tradition,
  -1L,   "Not reported",                                      NA_character_,
  -2L,   "Not applicable",                                    NA_character_,
  22L,   "American Evangelical Lutheran Church",              "Protestant",
  24L,   "African Methodist Episcopal Zion Church",           "Protestant",
  27L,   "Assemblies of God Church",                          "Protestant",
  28L,   "Brethren Church",                                   "Protestant",
  30L,   "Roman Catholic",                                    "Catholic",
  33L,   "Wisconsin Evangelical Lutheran Synod",              "Protestant",
  34L,   "Christ and Missionary Alliance Church",             "Protestant",
  35L,   "Christian Reformed Church",                         "Protestant",
  36L,   "Evangelical Congregational Church",                 "Protestant",
  37L,   "Evangelical Covenant Church of America",            "Protestant",
  38L,   "Evangelical Free Church of America",                "Protestant",
  39L,   "Evangelical Lutheran Church",                       "Protestant",
  40L,   "International United Pentecostal Church",           "Protestant",
  41L,   "Free Will Baptist Church",                          "Protestant",
  42L,   "Interdenominational",                               "Other Christian",
  43L,   "Mennonite Brethren Church",                         "Protestant",
  44L,   "Moravian Church",                                   "Protestant",
  45L,   "North American Baptist",                            "Protestant",
  47L,   "Pentecostal Holiness Church",                       "Protestant",
  48L,   "Christian Churches and Churches of Christ",         "Protestant",
  49L,   "Reformed Church in America",                        "Protestant",
  50L,   "Episcopal Church, Reformed",                        "Protestant",
  51L,   "African Methodist Episcopal",                       "Protestant",
  52L,   "American Baptist",                                  "Protestant",
  53L,   "American Lutheran",                                 "Protestant",
  54L,   "Baptist",                                           "Protestant",
  55L,   "Christian Methodist Episcopal",                     "Protestant",
  57L,   "Church of God",                                     "Protestant",
  58L,   "Church of Brethren",                                "Protestant",
  59L,   "Church of the Nazarene",                            "Protestant",
  60L,   "Cumberland Presbyterian",                           "Protestant",
  61L,   "Christian Church (Disciples of Christ)",            "Protestant",
  64L,   "Free Methodist",                                    "Protestant",
  65L,   "Friends",                                           "Protestant",
  66L,   "Presbyterian Church (USA)",                         "Protestant",
  67L,   "Lutheran Church in America",                        "Protestant",
  68L,   "Lutheran Church - Missouri Synod",                  "Protestant",
  69L,   "Mennonite Church",                                  "Protestant",
  71L,   "United Methodist",                                  "Protestant",
  73L,   "Protestant Episcopal",                              "Protestant",
  74L,   "Churches of Christ",                                "Protestant",
  75L,   "Southern Baptist",                                  "Protestant",
  76L,   "United Church of Christ",                           "Protestant",
  77L,   "Protestant, not specified",                         "Protestant",
  78L,   "Multiple Protestant Denomination",                  "Protestant",
  79L,   "Other Protestant",                                  "Protestant",
  80L,   "Jewish",                                            "Jewish",
  81L,   "Reformed Presbyterian Church",                      "Protestant",
  84L,   "United Brethren Church",                            "Protestant",
  87L,   "Missionary Church Inc",                             "Protestant",
  88L,   "Undenominational",                                  "Other Christian",
  89L,   "Wesleyan",                                          "Protestant",
  91L,   "Greek Orthodox",                                    "Other Christian",
  92L,   "Russian Orthodox",                                  "Other Christian",
  93L,   "Unitarian Universalist",                            "Other",
  94L,   "The Church of Jesus Christ of Latter-day Saints",   "Other Christian",
  95L,   "Seventh Day Adventist",                             "Protestant",
  97L,   "The Presbyterian Church in America",                "Protestant",
  99L,   "Other (none of the above)",                         "Other",
  100L,  "Original Free Will Baptist",                        "Protestant",
  101L,  "Ecumenical Christian",                              "Other Christian",
  102L,  "Evangelical Christian",                             "Other Christian",
  103L,  "Presbyterian",                                      "Protestant",
  104L,  "Virginia Baptist General Association",              "Protestant",
  105L,  "General Baptist",                                   "Protestant",
  106L,  "Muslim",                                            "Other",
  107L,  "Plymouth Brethren",                                 "Protestant",
  108L,  "Non-Denominational",                                "Other Christian",
  109L,  "Buddhist/Buddhism",                                 "Other",
  110L,  "Orthodox Christian",                                "Other Christian"
)

.relaffil_label <- function(code) {
  out <- .RELAFFIL_LOOKUP$label[match(code, .RELAFFIL_LOOKUP$code)]
  # Recognized but unmapped codes (e.g., new denomination in a future IPEDS
  # release) become "Other religious" rather than NA, so they remain
  # filterable instead of silently dropping.
  ifelse(is.na(code), NA_character_,
         ifelse(is.na(out) & code > 0, "Other religious", out))
}

.relaffil_tradition <- function(code) {
  out <- .RELAFFIL_LOOKUP$tradition[match(code, .RELAFFIL_LOOKUP$code)]
  # Codes that map to NA tradition stay NA. Both -1 ("Not reported") and
  # -2 ("Not applicable") get NA tradition; the absence of a tradition is
  # the correct semantic for those institutions.
  out
}

# =============================================================================
# 6. BUILD schools.csv
# =============================================================================
build_schools <- function(cfg = SCHOOLS_CONFIG) {
  message("== Building schools.csv ==")
  
  all_hd <- map_dfr(cfg$collection_years, function(yr) {
    hd <- get_table(yr, paste0("HD", yr))
    if (is.null(hd)) return(tibble())
    grab <- function(col) if (col %in% names(hd)) hd[[col]] else NA
    
    tibble(
      unitid     = hd$unitid,
      year       = yr,
      instnm     = grab("INSTNM"),
      sector     = suppressWarnings(as.integer(grab("SECTOR"))),
      control    = suppressWarnings(as.integer(grab("CONTROL"))),
      iclevel    = suppressWarnings(as.integer(grab("ICLEVEL"))),
      stabbr     = grab("STABBR"),
      longitud   = suppressWarnings(as.numeric(grab("LONGITUD"))),
      latitude   = suppressWarnings(as.numeric(grab("LATITUDE"))),
      hbcu       = suppressWarnings(as.integer(grab("HBCU"))),
      hospital   = suppressWarnings(as.integer(grab("HOSPITAL"))),
      medical    = suppressWarnings(as.integer(grab("MEDICAL"))),
      tribal     = suppressWarnings(as.integer(grab("TRIBAL"))),
      instcat    = suppressWarnings(as.integer(grab("INSTCAT"))),
      locale     = suppressWarnings(as.integer(grab("LOCALE"))),
      instsize   = suppressWarnings(as.integer(grab("INSTSIZE")))
    ) %>%
      filter(sector %in% cfg$keep_sectors)
  })
  
  # Religious affiliation pulled once from IC2023 (the field changes very
  # slowly for institutions; using a single year and applying uniformly is
  # a documented simplification, similar to how Carnegie classifications
  # are applied). RELAFFIL lives in IC (Institutional Characteristics),
  # not HD (Header). If IC2023 isn't loadable for any reason, religious
  # affiliation columns will be all NA and the rest of the pipeline still
  # runs.
  relaffil_lookup <- {
    ic23 <- tryCatch(get_table(2023, "IC2023"), error = function(e) NULL)
    if (is.null(ic23) || !"RELAFFIL" %in% names(ic23)) {
      message("  Note: IC2023 not available or missing RELAFFIL; religious affiliation will be NA")
      tibble(unitid = integer(), relaffil = integer())
    } else {
      tibble(unitid = ic23$unitid,
             relaffil = suppressWarnings(as.integer(ic23$RELAFFIL)))
    }
  }
  
  schools <- all_hd %>%
    arrange(unitid, desc(year)) %>%
    group_by(unitid) %>%
    summarise(
      latest_year = first(year),
      instnm      = first(instnm),
      sector      = first(sector),
      control     = first(control),
      iclevel     = first(iclevel),
      stabbr      = first(stabbr),
      longitud    = first(longitud),
      latitude    = first(latitude),
      hbcu        = first(hbcu),
      hospital    = first(hospital),
      medical     = first(medical),
      tribal      = first(tribal),
      instcat     = first(instcat),
      locale      = first(locale),
      instsize    = first(instsize),
      .groups     = "drop"
    ) %>%
    mutate(control_grp = case_when(
      sector == 1 ~ "public",
      sector == 2 ~ "private_nfp",
      TRUE        ~ "other"
    )) %>%
    left_join(relaffil_lookup, by = "unitid") %>%
    mutate(
      religious_affiliation_code = relaffil,
      religious_affiliation      = .relaffil_label(relaffil),
      religious_tradition        = .relaffil_tradition(relaffil)
    )
  
  classn <- build_classification(cfg)
  schools <- schools %>%
    left_join(classn, by = "unitid") %>%
    mutate(in_ranked_universe = ifelse(is.na(in_ranked_universe),
                                       FALSE, in_ranked_universe))

  # US News overall rank — separate AI pull because rank is a metric
  # (facts/) not an institutional attribute (schools/). Returns an empty
  # tibble when the metric_id is unset, so the join is a no-op until
  # SCHOOLS_CONFIG$usnews_rank_metric_id is filled in.
  usn_rank <- build_usnews_rank(cfg)
  if (nrow(usn_rank)) {
    schools <- schools %>% left_join(usn_rank, by = "unitid")
    # Authoritative definition: in_ranked_universe = does the school
    # have a numeric US News overall rank? This is what the flag is
    # meant to mean ("US News actively ranks this school"). The
    # provisional classification-based value computed earlier is now
    # superseded — most importantly, this correctly includes Regional
    # Colleges (which DO have numeric ranks despite their classification
    # being excluded from the earlier ranked_classes membership rule).
    schools <- schools %>%
      mutate(in_ranked_universe = !is.na(usnews_rank))
  }

  # Washington Monthly rankings (offline XLSX). Empty tibble when the
  # file is missing, so this is a no-op until the file is dropped in.
  wamo <- build_washington_monthly(cfg)
  if (nrow(wamo)) {
    schools <- schools %>% left_join(wamo, by = "unitid")
  }

  # Forbes America's Top Colleges — matched to unitid via instnm + stabbr
  # because the Forbes CSV doesn't carry IPEDS. No-op when the CSV is
  # missing (run R/scrape_forbes_rankings.py to produce it).
  forbes <- build_forbes(cfg, schools %>% select(unitid, instnm, stabbr))
  if (nrow(forbes)) {
    schools <- schools %>% left_join(forbes, by = "unitid")
  }
  
  carnegie <- build_carnegie(cfg)
  if (nrow(carnegie$data)) {
    schools <- schools %>% left_join(carnegie$data, by = "unitid")
  }
  
  accreditor <- build_accreditor(cfg)
  if (nrow(accreditor)) {
    schools <- schools %>% left_join(accreditor, by = "unitid")
  }
  
  ipeds_labels <- build_ipeds_value_labels(cfg)
  all_labels <- bind_rows(ipeds_labels, carnegie$labels) %>%
    distinct(table_name, variable, code, .keep_all = TRUE)
  write.csv(all_labels, .out_path("value_labels.csv"), row.names = FALSE)
  message(sprintf("Wrote %s: %d rows", .out_path("value_labels.csv"),
                  nrow(all_labels)))
  
  for (f in cfg$labeled_fields) {
    schools <- attach_label(schools, f, toupper(f), all_labels)
  }
  
  message(sprintf("  %d distinct schools; %d in ranked universe; %d with Carnegie ic2025; %d with accreditor; %d with religious affiliation",
                  nrow(schools),
                  sum(schools$in_ranked_universe, na.rm = TRUE),
                  if ("ic2025" %in% names(schools)) sum(!is.na(schools$ic2025)) else 0,
                  if ("accreditor" %in% names(schools)) sum(!is.na(schools$accreditor)) else 0,
                  sum(!is.na(schools$religious_tradition) &
                        schools$religious_affiliation != "Not applicable", na.rm = TRUE)))
  
  write.csv(schools, .out_path("schools.csv"), row.names = FALSE)
  message(sprintf("Wrote %s", .out_path("schools.csv")))
  invisible(schools)
}

# -----------------------------------------------------------------------------
# Usage:
#   setwd("path/to/hc-peer")
#   Sys.setenv(ACADEMIC_INSIGHTS_API_KEY = "...")
#   schools <- build_schools()
# Produces output/schools.csv and output/value_labels.csv.
# -----------------------------------------------------------------------------