# =============================================================================
# Finance & Resources Module (combined)
# Holy Cross peer-comparison project
#
# Reads:  output/schools.csv      (built by R/schools_pipeline.R)
#         data/IPEDS*.Rda
#         Academic Insights API for the 2 CDS class-size metrics
#
# Writes: output/fin_facts.csv      one row per unitid x year x metric
#         output/fin_variables.csv  one row per metric (metadata)
#
# Variables (~17 total), in two category labels within one module:
#
# FINANCE (8) - category="finance"
#   endowment_per_fte                                clustering, cross_category
#   endowment_coverage_years                         clustering, cross_category
#   tuition_share_of_expenses                        clustering, cross_category
#   core_expenses_per_fte                            clustering, cross_category
#   operating_margin_ex_inv_return_per_fte           clustering, cross_category
#   net_assets_per_fte                               clustering, cross_category
#   published_tuition_fees                           clustering, cross_category
#   herd_avg                                         descriptive, within_category, NECHE
#
# RESOURCES (9) - category="resources"
#   student_faculty_ratio                            clustering, cross_category (computed)
#   tenure_track_share                               clustering, cross_category
#   avg_ft_faculty_salary                            clustering, cross_category
#   instruction_per_fte                              clustering, within_category
#   academic_support_per_fte                         clustering, within_category
#   student_services_per_fte                         clustering, within_category
#   instructional_share                              clustering, cross_category
#   pct_classes_under_20                             clustering, cross_category
#   pct_classes_50plus                               clustering, cross_category
#
# DROPPED FROM v1
#   pct_faculty_full_time                            S{yr}_IS is full-time only by IPEDS survey
#                                                    design; reconstructing FT/PT requires raw
#                                                    PT-staff data not currently pulled.
#   endowment_dependence (investment return / rev)   Volatile due to FASB investment-return
#   total_core_revenue_per_fte (rev / FTE)           accounting (markets move revenue, not
#   operating_margin_per_fte ((rev-exp) / FTE)       institutional fundamentals). Replaced
#   tuition_dependence (tuition / rev)               with expense-denominated alternatives.
#
# FORM AWARENESS
#   Institutions report finance data on different forms by sector:
#     SECTOR 1 (public 4-yr)      -> Form F2 (GASB)   -> table F{yr}_F2
#     SECTOR 2 (private NFP 4-yr) -> Form F1A (FASB)  -> table F{yr}_F1A
#   The DRVF{yr} derived table precomputes cross-form values with column
#   prefixes F1* (GASB) and F2* (FASB), allowing us to read both forms with
#   the same code path. This module leans on DRVF wherever possible.
#
# DEFINITIONAL DEFAULTS
#   net_assets = total net assets at year-end (F2 H02 for FASB, F1 H02 for GASB)
#   operating_margin_ex_inv_return = (core_rev × (1 - INVRPC/100) - core_exp) / FTE
#     backs out IPEDS's investment-return share from FASB revenue to give a
#     structurally stable operating-margin reading.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(httr2); library(jsonlite); library(readxl)
})


# ---- CONFIG ---------------------------------------------------------------
FIN_CONFIG <- list(
  collection_years = 2020:2024,
  ai_dataset = "undergraduate",
  ai_key     = Sys.getenv("ACADEMIC_INSIGHTS_API_KEY"),
  ai_base    = "https://ai.usnews.com/api/v1/client_api",
  scorecard_key = Sys.getenv("SCORECARD_API_KEY"),
  carnegie_file = .data_path("2025-Public-Data-File.xlsx"),
  # Two CDS-unique class-size metrics. IDs TBD at build time via
  # search_ai_metrics(FIN_CONFIG, contains = "class size")
  ai_metric_ids = list(
    pct_classes_under_20 = NA_integer_,
    pct_classes_50plus   = NA_integer_
  )
)

# =============================================================================
# 1. Form-aware finance pull from DRVF
# =============================================================================
# Sector 1 (public) uses F1* columns (GASB); sector 2 (private NFP) uses F2*.
# This helper takes one F1 column and one F2 column and returns a tibble with
# unitid + value, picking the right column per institution based on SECTOR.
drvf_form_aware <- function(year, f1_col, f2_col, sectors_df) {
  drvf <- get_table(year, paste0("DRVF", year))
  if (is.null(drvf)) return(tibble(unitid = integer(), value = numeric()))
  
  have_f1 <- f1_col %in% names(drvf)
  have_f2 <- f2_col %in% names(drvf)
  if (!have_f1 && !have_f2) return(tibble(unitid = integer(), value = numeric()))
  
  d <- drvf %>%
    select(unitid, any_of(c(f1_col, f2_col))) %>%
    left_join(sectors_df, by = "unitid") %>%
    mutate(value = case_when(
      sector == 1 & have_f1 ~ suppressWarnings(as.numeric(.data[[f1_col]])),
      sector == 2 & have_f2 ~ suppressWarnings(as.numeric(.data[[f2_col]])),
      TRUE ~ NA_real_
    )) %>%
    select(unitid, value) %>%
    filter(is.finite(value))
  d
}

build_drvf_metrics <- function(unitids_by_year, sectors_by_year, cfg) {
  message("Pulling DRVF (form-aware finance metrics) ...")
  # Map: metric -> (F1 column for GASB/public, F2 column for FASB/private)
  # NOTE: tuition_dependence, endowment_dependence, total_core_revenue_per_fte,
  # and operating_margin_per_fte were dropped from this module in favor of
  # stable alternatives (see the computed block below). They were volatile
  # because IPEDS's "core revenue" for FASB schools includes investment return,
  # which fluctuates with markets.
  drvf_map <- tribble(
    ~metric,                       ~f1_col,    ~f2_col,    ~var_type,
    "endowment_per_fte",           "F1ENDMFT", "F2ENDMFT", "raw",
    "instruction_per_fte",         "F1INSTFT", "F2INSTFT", "raw",
    "academic_support_per_fte",    "F1ACSPFT", "F2ACSPFT", "raw",
    "student_services_per_fte",    "F1STSVFT", "F2STSVFT", "raw",
    "instructional_share",         "F1INSTPC", "F2INSTPC", "raw"
  )
  
  out <- list()
  for (yr in cfg$collection_years) {
    uids    <- unitids_by_year[[as.character(yr)]]
    sectors <- sectors_by_year[[as.character(yr)]]
    if (!length(uids)) next
    
    for (i in seq_len(nrow(drvf_map))) {
      d <- drvf_form_aware(yr, drvf_map$f1_col[i], drvf_map$f2_col[i],
                           sectors_df = sectors) %>%
        filter(unitid %in% uids)
      if (nrow(d))
        out[[length(out) + 1]] <- d %>%
          mutate(year = yr,
                 metric = drvf_map$metric[i],
                 var_type = drvf_map$var_type[i])
    }
    
    # =========================================================================
    # 4 STABLE FINANCE METRICS (replacing volatile predecessors)
    # All share FTE + core_exp denominators, so computed together for clarity.
    #
    # 1. core_expenses_per_fte:
    #    F1COREXP / FTE   (GASB)
    #    F2COREXP / FTE   (FASB)
    #
    # 2. tuition_share_of_expenses:
    #    100 * (F1TUFEFT * FTE) / F1COREXP   (GASB)
    #    100 * (F2TUFEFT * FTE) / F2COREXP   (FASB)
    #    Equivalent: 100 * tuition_per_fte / core_exp_per_fte
    #
    # 3. endowment_coverage_years:
    #    (F1ENDMFT * FTE) / F1COREXP   (GASB)
    #    (F2ENDMFT * FTE) / F2COREXP   (FASB)
    #    Equivalent: endowment_per_fte / core_exp_per_fte
    #
    # 4. operating_margin_ex_inv_return_per_fte:
    #    (F1CORREV * (1 - F1INVRPC/100) - F1COREXP) / FTE   (GASB)
    #    (F2CORREV * (1 - F2INVRPC/100) - F2COREXP) / FTE   (FASB)
    #    Backs out the investment-return share of revenue from F*INVRPC,
    #    leaves only operating revenue, subtracts expenses, divides by FTE.
    # =========================================================================
    drvf <- get_table(yr, paste0("DRVF", yr))
    drvef <- get_table(yr, paste0("DRVEF", yr))
    needed_cols <- c("F1CORREV","F1COREXP","F1INVRPC","F1TUFEFT","F1ENDMFT",
                     "F2CORREV","F2COREXP","F2INVRPC","F2TUFEFT","F2ENDMFT")
    if (!is.null(drvf) && !is.null(drvef) &&
        all(needed_cols %in% names(drvf)) &&
        "FTE" %in% names(drvef)) {
      
      pick <- function(d, sec_df, f1, f2) {
        d %>% select(unitid, all_of(c(f1, f2))) %>%
          left_join(sec_df, by = "unitid") %>%
          mutate(val = case_when(
            sector == 1 ~ suppressWarnings(as.numeric(.data[[f1]])),
            sector == 2 ~ suppressWarnings(as.numeric(.data[[f2]])),
            TRUE        ~ NA_real_
          )) %>%
          select(unitid, val)
      }
      
      rev_d   <- pick(drvf, sectors, "F1CORREV", "F2CORREV") %>% rename(rev = val)
      exp_d   <- pick(drvf, sectors, "F1COREXP", "F2COREXP") %>% rename(exp_t = val)
      inv_d   <- pick(drvf, sectors, "F1INVRPC", "F2INVRPC") %>% rename(inv_pct = val)
      tuit_d  <- pick(drvf, sectors, "F1TUFEFT", "F2TUFEFT") %>% rename(tuit_per_fte = val)
      endow_d <- pick(drvf, sectors, "F1ENDMFT", "F2ENDMFT") %>% rename(endow_per_fte = val)
      fte_d   <- drvef %>% transmute(unitid, fte = suppressWarnings(as.numeric(FTE)))
      
      base <- rev_d %>%
        inner_join(exp_d,   by = "unitid") %>%
        inner_join(inv_d,   by = "unitid") %>%
        inner_join(tuit_d,  by = "unitid") %>%
        inner_join(endow_d, by = "unitid") %>%
        inner_join(fte_d,   by = "unitid") %>%
        filter(unitid %in% uids,
               is.finite(rev), is.finite(exp_t), is.finite(fte), fte > 0)
      
      # Metric 1: core_expenses_per_fte
      m1 <- base %>%
        filter(is.finite(exp_t)) %>%
        transmute(unitid, value = exp_t / fte)
      if (nrow(m1))
        out[[length(out) + 1]] <- m1 %>%
        mutate(year = yr, metric = "core_expenses_per_fte", var_type = "computed")
      
      # Metric 2: tuition_share_of_expenses
      m2 <- base %>%
        filter(is.finite(tuit_per_fte), is.finite(exp_t), exp_t > 0) %>%
        transmute(unitid, value = 100 * tuit_per_fte * fte / exp_t)
      if (nrow(m2))
        out[[length(out) + 1]] <- m2 %>%
        mutate(year = yr, metric = "tuition_share_of_expenses", var_type = "computed")
      
      # Metric 3: endowment_coverage_years
      m3 <- base %>%
        filter(is.finite(endow_per_fte), is.finite(exp_t), exp_t > 0) %>%
        transmute(unitid, value = endow_per_fte * fte / exp_t)
      if (nrow(m3))
        out[[length(out) + 1]] <- m3 %>%
        mutate(year = yr, metric = "endowment_coverage_years", var_type = "computed")
      
      # Metric 4: operating_margin_ex_inv_return_per_fte
      # Sanity bounds: inv_pct in (-100, 200) - skip values outside this range
      m4 <- base %>%
        filter(is.finite(inv_pct), inv_pct > -100, inv_pct < 200) %>%
        transmute(unitid,
                  value = (rev * (1 - inv_pct / 100) - exp_t) / fte)
      if (nrow(m4))
        out[[length(out) + 1]] <- m4 %>%
        mutate(year = yr,
               metric = "operating_margin_ex_inv_return_per_fte",
               var_type = "computed")
    }
  }
  
  bind_rows(out)
}

# =============================================================================
# 2. Net assets from raw F1A/F2 tables (no DRVF equivalent)
# =============================================================================
# F1A.F1H02 = FASB Total net assets at year end
# F2.F2H02  = GASB Total net position at year end
# F{yr}_F1A and F{yr}_F2 table names follow F{2-digit yr}{2-digit nextyr} pattern.
build_net_assets <- function(unitids_by_year, sectors_by_year, cfg) {
  message("Pulling net assets from F1A/F2 ...")
  out <- list()
  for (yr in cfg$collection_years) {
    uids    <- unitids_by_year[[as.character(yr)]]
    sectors <- sectors_by_year[[as.character(yr)]]
    drvef   <- get_table(yr, paste0("DRVEF", yr))
    if (is.null(drvef) || !"FTE" %in% names(drvef)) next
    fte <- drvef %>% transmute(unitid, fte = suppressWarnings(as.numeric(FTE)))
    
    # IPEDS finance tables use FY-ending-year suffix, not collection year.
    # For collection year 2024 (2024-25 academic year), the finance survey
    # reports FY2023-24 data, in table F2324_F1A (FASB) / F2324_F2 (GASB).
    f_2digit <- function(y) sprintf("%02d", y %% 100)
    suffix <- paste0(f_2digit(yr - 1), f_2digit(yr))
    candidates_f1a <- c(paste0("F", suffix, "_F1A"))
    candidates_f2  <- c(paste0("F", suffix, "_F2"))
    
    f1a <- NULL
    for (cand in candidates_f1a) {
      f1a <- get_table(yr, cand)
      if (!is.null(f1a)) break
    }
    f2 <- NULL
    for (cand in candidates_f2) {
      f2 <- get_table(yr, cand)
      if (!is.null(f2)) break
    }
    if (is.null(f1a) && is.null(f2)) next
    
    # FASB private NFP: F1H02 = Total net assets, year end
    fasb_part <- if (!is.null(f1a) && "F1H02" %in% names(f1a)) {
      f1a %>% transmute(unitid, net_assets = suppressWarnings(as.numeric(F1H02)))
    } else tibble(unitid = integer(), net_assets = numeric())
    
    # GASB public: F2H02 = Total net position, year end (rough analog)
    gasb_part <- if (!is.null(f2) && "F2H02" %in% names(f2)) {
      f2 %>% transmute(unitid, net_assets = suppressWarnings(as.numeric(F2H02)))
    } else tibble(unitid = integer(), net_assets = numeric())
    
    combined <- bind_rows(fasb_part, gasb_part) %>%
      filter(unitid %in% uids, is.finite(net_assets)) %>%
      inner_join(fte, by = "unitid") %>%
      filter(is.finite(fte), fte > 0) %>%
      transmute(unitid, value = net_assets / fte)
    
    if (nrow(combined))
      out[[length(out) + 1]] <- combined %>%
      mutate(year = yr,
             metric = "net_assets_per_fte",
             var_type = "computed")
  }
  bind_rows(out)
}

# =============================================================================
# 3. Published tuition & fees (tuition + required fees combined)
# =============================================================================
# 2020-2023: IC{yr}_AY - TUITION2 (in-state tuition) + FEE2 (in-state fees)
# 2024:      COST1_2024.CHG2AY3 (in-state tuition+fees combined, current year)
# For private NFP institutions, in-state == in-district == out-of-state.
# For public institutions, this captures the in-state rate (the canonical
# published price for state residents).
build_tuition <- function(unitids_by_year, cfg) {
  message("Pulling published tuition & fees ...")
  out <- list()
  for (yr in cfg$collection_years) {
    uids <- unitids_by_year[[as.character(yr)]]
    if (yr == 2024) {
      d <- get_table(yr, "COST1_2024")
      if (is.null(d) || !"CHG2AY3" %in% names(d)) next
      vv <- d %>% transmute(unitid, value = suppressWarnings(as.numeric(CHG2AY3)))
    } else {
      d <- get_table(yr, paste0("IC", yr, "_AY"))
      if (is.null(d) || !all(c("TUITION2", "FEE2") %in% names(d))) next
      vv <- d %>%
        transmute(unitid,
                  tuition = suppressWarnings(as.numeric(TUITION2)),
                  fee     = suppressWarnings(as.numeric(FEE2))) %>%
        # Combined: tuition + fees. If either is NA but the other is present,
        # we accept a partial value (fees are often small) rather than dropping.
        transmute(unitid,
                  value = coalesce(tuition, 0) + coalesce(fee, 0)) %>%
        # But if both were missing/zero, that's not a real value
        filter(value > 0)
    }
    vv <- vv %>% filter(unitid %in% uids, is.finite(value))
    if (nrow(vv))
      out[[length(out) + 1]] <- vv %>%
      mutate(year = yr,
             metric = "published_tuition_fees",
             var_type = "raw")
  }
  bind_rows(out)
}

# =============================================================================
# 4. HERD R&D from Carnegie 2025 - one-time average, written under 2024
# =============================================================================
build_herd <- function(schools, cfg) {
  message("Reading HERD R&D average from Carnegie 2025 ...")
  # The Carnegie file has multiple sheets; the main data sheet is named "data"
  # (matches the build_carnegie() convention in schools_pipeline.R).
  # NECHE peer-grouping uses a 3-year HERD average; the field name typically
  # contains "herd" - locate it dynamically.
  if (!file.exists(cfg$carnegie_file)) return(tibble())
  ws <- tryCatch(read_excel(cfg$carnegie_file, sheet = "data",
                            na = c("", "NA")),
                 error = function(e) NULL)
  if (is.null(ws)) {
    warning("Could not read 'data' sheet from Carnegie file")
    return(tibble())
  }
  names(ws) <- tolower(names(ws))
  
  # Candidate column names
  candidates <- c("herd_3yr_avg", "herd_avg", "herd_3yravg",
                  "herd3yravg", "rd_avg_herd")
  hit <- candidates[candidates %in% names(ws)][1]
  if (is.na(hit)) {
    # Try fuzzy match on anything containing "herd"
    herd_cols <- grep("herd", names(ws), value = TRUE)
    if (!length(herd_cols)) {
      warning("No HERD column found in Carnegie file - check column names: ",
              paste(grep("rd|research", names(ws), value = TRUE), collapse = ", "))
      return(tibble())
    }
    hit <- herd_cols[1]
    message(sprintf("  Using Carnegie column: %s", hit))
  }
  
  latest_year <- max(cfg$collection_years)
  ws %>%
    transmute(unitid = as.integer(unitid),
              value  = suppressWarnings(as.numeric(.data[[hit]]))) %>%
    filter(!is.na(unitid), is.finite(value), value > 0) %>%
    semi_join(schools, by = "unitid") %>%
    mutate(year = latest_year,
           metric = "herd_avg",
           var_type = "external")
}

# =============================================================================
# 5. Resources from S/SAL/EAP/DRVHR tables
# =============================================================================
build_resources <- function(unitids_by_year, cfg) {
  message("Pulling resources metrics (faculty, staff, salary) ...")
  out <- list()
  for (yr in cfg$collection_years) {
    uids <- unitids_by_year[[as.character(yr)]]
    
    # student_faculty_ratio: STUFACR isn't in IPEDS tables (verified empirically).
    # Computed as standard NSC formula: student FTE / instructional staff FTE.
    drvef <- get_table(yr, paste0("DRVEF", yr))
    drvhr <- get_table(yr, paste0("DRVHR", yr))
    if (!is.null(drvef) && !is.null(drvhr) &&
        "FTE" %in% names(drvef) && "SFTEINST" %in% names(drvhr)) {
      fte  <- drvef %>% transmute(unitid, fte = suppressWarnings(as.numeric(FTE)))
      inst <- drvhr %>% transmute(unitid, inst_fte = suppressWarnings(as.numeric(SFTEINST)))
      d <- fte %>% inner_join(inst, by = "unitid") %>%
        filter(unitid %in% uids, is.finite(fte), is.finite(inst_fte), inst_fte > 0) %>%
        transmute(unitid, value = fte / inst_fte)
      if (nrow(d))
        out[[length(out) + 1]] <- d %>%
        mutate(year = yr,
               metric = "student_faculty_ratio",
               var_type = "computed")
    }
    
    # Salaries from SAL{yr}_IS (instructional staff salary, all ranks).
    # SAL_IS is in LONG format with one row per (unitid, academic rank), so we
    # aggregate to one weighted-average salary per institution:
    #   value = sum(total outlay across ranks) / sum(staff count across ranks).
    sal <- get_table(yr, paste0("SAL", yr, "_IS"))
    if (!is.null(sal) && all(c("SAOUTLT", "SATOTLT") %in% names(sal))) {
      d <- sal %>%
        transmute(unitid,
                  outlay = suppressWarnings(as.numeric(SAOUTLT)),
                  count  = suppressWarnings(as.numeric(SATOTLT))) %>%
        filter(unitid %in% uids, is.finite(outlay), is.finite(count), count > 0) %>%
        group_by(unitid) %>%
        summarise(total_outlay = sum(outlay, na.rm = TRUE),
                  total_count  = sum(count, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(total_count > 0) %>%
        transmute(unitid, value = total_outlay / total_count)
      if (nrow(d))
        out[[length(out) + 1]] <- d %>%
          mutate(year = yr,
                 metric = "avg_ft_faculty_salary",
                 var_type = "computed")
    }
    
    # Tenure-track share via row aggregation from S{yr}_IS (long format).
    # IPEDS HR survey (post-2017 redesign) FACSTAT codes in S_IS:
    #   0  = Total instructional staff (denominator)
    #   10 = With faculty status, total (parent of 20-40)
    #   20 = Tenured
    #   30 = On tenure track
    #   40 = Not on tenure track (with faculty status, has tenure system)
    #   41-45 = Subdivisions of 40 by contract type
    #   50 = Without faculty status
    # We filter to ARANK==0 (all ranks combined) for one row per FACSTAT,
    # then compute tenure_track_share = (FACSTAT 20 + 30) / FACSTAT 0.
    # If FACSTAT 20+30 is empty but FACSTAT 40 is populated, the institution
    # has no tenure system - record NA rather than 0%.
    s_is <- get_table(yr, paste0("S", yr, "_IS"))
    if (!is.null(s_is) && all(c("FACSTAT","ARANK","HRTOTLT") %in% names(s_is))) {
      base <- s_is %>%
        filter(ARANK == 0) %>%
        transmute(unitid, FACSTAT, hr = suppressWarnings(as.numeric(HRTOTLT))) %>%
        filter(is.finite(hr))
      
      tot <- base %>% filter(FACSTAT == 0) %>%
        transmute(unitid, total = hr)
      tnr <- base %>% filter(FACSTAT %in% c(20, 30)) %>%
        group_by(unitid) %>%
        summarise(track = sum(hr, na.rm = TRUE), .groups = "drop")
      no_tenure <- base %>% filter(FACSTAT == 40) %>%
        transmute(unitid, no_tenure_count = hr)
      
      d <- tot %>%
        left_join(tnr, by = "unitid") %>%
        left_join(no_tenure, by = "unitid") %>%
        filter(unitid %in% uids, is.finite(total), total > 0) %>%
        # If FACSTAT 20+30 is 0 but FACSTAT 40 has staff, institution has
        # no tenure system - record NA rather than 0% share.
        transmute(unitid,
                  value = if_else(
                    coalesce(track, 0) == 0 &
                      !is.na(no_tenure_count) & no_tenure_count > 0,
                    NA_real_,
                    100 * coalesce(track, 0) / total))
      d <- d %>% filter(!is.na(value))
      if (nrow(d))
        out[[length(out) + 1]] <- d %>%
        mutate(year = yr,
               metric = "tenure_track_share",
               var_type = "computed")
    }
    
    # Note: pct_faculty_full_time intentionally not computed in this version.
    # S{yr}_IS is full-time instructional staff only by IPEDS survey design;
    # part-time data would require pulling from S_OC or a separate PT table.
    # Documented honestly in methodology; can be revisited if needed.
  }
  bind_rows(out)
}

# =============================================================================
# 6. CDS class-size metrics from Academic Insights
# =============================================================================
build_cds_fin <- function(cfg) {
  ids <- unlist(cfg$ai_metric_ids)
  ids <- ids[!is.na(ids)]
  if (!length(ids)) {
    message("No CDS class-size IDs set - skipping. ",
            "Confirm via search_ai_metrics() and fill FIN_CONFIG$ai_metric_ids.")
    return(tibble())
  }
  message(sprintf("Pulling %d CDS class-size metrics (IPEDS->AI year mapping) ...",
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
    df %>% mutate(year = ipeds_year)
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
# 7. fin_variables.csv
# =============================================================================
build_fin_variables <- function(cfg) {
  ai_id <- function(nm) {
    v <- cfg$ai_metric_ids[[nm]]
    if (is.na(v)) "Academic Insights metric_id TBD" else
      sprintf("Academic Insights metric_id %d", v)
  }
  tribble(
    ~metric,                       ~display_name,                                              ~source,         ~ipeds_table_or_formula,                                         ~category,    ~use_type,     ~comparison_scope,  ~format,       ~neche_peer_set, ~neche_dashboard, ~coverage_note,
    # FINANCE (8)
    "endowment_per_fte",                       "Endowment assets per FTE",                                       "ipeds",         "DRVF.F1ENDMFT (GASB) / F2ENDMFT (FASB)",                        "finance",    "clustering",  "cross_category",   "currency",    FALSE,           TRUE,             NA_character_,
    "endowment_coverage_years",                "Endowment value as years of operating expenses",                 "ipeds_derived", "(endowment_per_fte * FTE) / core_expenses",                     "finance",    "clustering",  "cross_category",   "ratio",       FALSE,           FALSE,            "Higher = financially stronger. Most LACs 1-5; wealthiest research 10+",
    "tuition_share_of_expenses",               "Net tuition revenue as % of core expenses",                      "ipeds_derived", "100 * (tuition_per_fte * FTE) / core_expenses",                 "finance",    "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             "Stable analog of tuition_dependence; uses expenses (no investment-return distortion)",
    "core_expenses_per_fte",                   "Total core operating expenses per FTE",                          "ipeds_derived", "(DRVF.F1COREXP or F2COREXP) / DRVEF.FTE",                       "finance",    "clustering",  "cross_category",   "currency",    FALSE,           TRUE,             NA_character_,
    "operating_margin_ex_inv_return_per_fte",  "Operating margin per FTE, excluding investment return",          "ipeds_derived", "(rev * (1 - INVRPC/100) - core_exp) / FTE",                     "finance",    "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            "Negative values common for endowment-rich schools that fund operations from endowment income",
    "net_assets_per_fte",                      "Total net assets per FTE (year end)",                            "ipeds_derived", "F1A.F1H02 (FASB) / F2.F2H02 (GASB) / DRVEF.FTE",                "finance",    "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            "FASB net assets vs GASB net position - documented in methodology",
    "published_tuition_fees",                  "Published tuition + required fees (in-state, UG, AY)",           "ipeds",         "IC{yr}_AY.TUITION2 + FEE2 / COST1_2024.CHG2AY3",                "finance",    "clustering",  "cross_category",   "currency",    FALSE,           TRUE,             "Combined tuition + fees. In-state used for public institutions.",
    "herd_avg",                                "HERD R&D expenditures, 3-year average (Carnegie)",               "ccihe",         "Carnegie 2025 Public Data File.herd_avg",                       "finance",    "descriptive", "within_category",  "currency",    TRUE,            FALSE,            "NECHE peer-set member. One-time Carnegie snapshot (FY2020-2023 avg) under 2024.",
    # RESOURCES (10)
    "student_faculty_ratio",       "Student-to-faculty ratio (computed)",                      "ipeds_derived", "DRVEF.FTE / DRVHR.SFTEINST",                                    "resources",  "clustering",  "cross_category",   "ratio",       FALSE,           TRUE,             "Computed (STUFACR not in IPEDS); matches NSC's published S-F ratio formula",
    "tenure_track_share",          "% of instructional staff tenured or on tenure track",      "ipeds_derived", "S{yr}_IS (FACSTAT 20+30) / (FACSTAT 0), filtered to ARANK==0", "resources",  "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "NA if institution has no tenure system (FACSTAT 40 populated, 20+30 empty)",
    "avg_ft_faculty_salary",       "Average full-time instructional salary",                   "ipeds_derived", "SAL{yr}_IS.SAOUTLT / SATOTLT",                                  "resources",  "clustering",  "cross_category",   "currency",    FALSE,           FALSE,            NA_character_,
    "instruction_per_fte",         "Instruction expenses per FTE",                             "ipeds",         "DRVF.F1INSTFT (GASB) / F2INSTFT (FASB)",                        "resources",  "clustering",  "within_category",  "currency",    FALSE,           TRUE,             NA_character_,
    "academic_support_per_fte",    "Academic support expenses per FTE",                        "ipeds",         "DRVF.F1ACSPFT (GASB) / F2ACSPFT (FASB)",                        "resources",  "clustering",  "within_category",  "currency",    FALSE,           TRUE,             NA_character_,
    "student_services_per_fte",    "Student services expenses per FTE",                        "ipeds",         "DRVF.F1STSVFT (GASB) / F2STSVFT (FASB)",                        "resources",  "clustering",  "within_category",  "currency",    FALSE,           TRUE,             NA_character_,
    "instructional_share",         "Instruction as % of total core expenses",                  "ipeds",         "DRVF.F1INSTPC (GASB) / F2INSTPC (FASB)",                        "resources",  "clustering",  "cross_category",   "percentage",  FALSE,           TRUE,             NA_character_,
    "pct_classes_under_20",        "% of undergrad classes with fewer than 20 students",       "cds_ai",        ai_id("pct_classes_under_20"),                                   "resources",  "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Survey respondents only (~45%)",
    "pct_classes_50plus",          "% of undergrad classes with 50 or more students",          "cds_ai",        ai_id("pct_classes_50plus"),                                     "resources",  "clustering",  "cross_category",   "percentage",  FALSE,           FALSE,            "Survey respondents only (~45%)"
  ) %>%
    mutate(notes = NA_character_) %>%
    select(metric, category, display_name, source, ipeds_table_or_formula,
           use_type, comparison_scope, format, neche_peer_set, neche_dashboard,
           coverage_note, notes)
}

# =============================================================================
# 8. COVERAGE REPORT
# =============================================================================
fin_coverage_report <- function(facts, schools) {
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
# 9. RUN
# =============================================================================
run_finance_resources_module <- function(cfg = FIN_CONFIG) {
  message("== Finance & Resources Module ==")
  
  schools_csv <- .out_path("schools.csv")
  if (!file.exists(schools_csv))
    stop(schools_csv, " not found. Run build_schools() first.")
  schools <- as_tibble(read.csv(schools_csv, stringsAsFactors = FALSE))
  message(sprintf("  loaded %s: %d institutions", schools_csv, nrow(schools)))
  
  # Per-year unitid + sector lookup (used by form-aware DRVF pulls)
  unitids_by_year <- list()
  sectors_by_year <- list()
  for (yr in cfg$collection_years) {
    hd <- get_table(yr, paste0("HD", yr))
    if (is.null(hd)) {
      unitids_by_year[[as.character(yr)]] <- integer()
      sectors_by_year[[as.character(yr)]] <- tibble(unitid = integer(), sector = integer())
      next
    }
    keep <- hd %>%
      filter(suppressWarnings(as.integer(SECTOR)) %in% c(1, 2)) %>%
      transmute(unitid = as.integer(unitid),
                sector = as.integer(SECTOR))
    unitids_by_year[[as.character(yr)]] <- keep$unitid
    sectors_by_year[[as.character(yr)]] <- keep %>% select(unitid, sector)
  }
  
  drvf_metrics   <- build_drvf_metrics(unitids_by_year, sectors_by_year, cfg)
  net_assets     <- build_net_assets(unitids_by_year, sectors_by_year, cfg)
  tuition        <- build_tuition(unitids_by_year, cfg)
  herd           <- build_herd(schools, cfg)
  resources      <- build_resources(unitids_by_year, cfg)
  cds            <- build_cds_fin(cfg)
  
  fin_facts <- bind_rows(drvf_metrics, net_assets, tuition, herd, resources, cds) %>%
    semi_join(schools, by = "unitid") %>%
    arrange(unitid, year, metric)
  
  fin_variables <- build_fin_variables(cfg)
  
  write.csv(fin_facts,     .out_path("fin_facts.csv"),     row.names = FALSE)
  write.csv(fin_variables, .out_path("fin_variables.csv"), row.names = FALSE)
  message(sprintf("Wrote %s, %s",
                  .out_path("fin_facts.csv"), .out_path("fin_variables.csv")))
  
  message("\nCoverage (% of universe with a value), by metric and control group:")
  print(fin_coverage_report(fin_facts, schools))
  
  invisible(list(facts = fin_facts, variables = fin_variables, schools = schools))
}

# -----------------------------------------------------------------------------
# Usage:
#   setwd("path/to/peer_schools")
#   Sys.setenv(ACADEMIC_INSIGHTS_API_KEY = "...")
#   source("R/schools_pipeline.R");           build_schools()
#   source("R/finance_resources_module_pipeline.R")
#
#   # confirm 2 CDS class-size metric IDs (or leave NA to skip):
#   search_ai_metrics(FIN_CONFIG, contains = "class size")
#   FIN_CONFIG$ai_metric_ids$pct_classes_under_20 <- <id>
#   FIN_CONFIG$ai_metric_ids$pct_classes_50plus   <- <id>
#
#   res_fin <- run_finance_resources_module()
# -----------------------------------------------------------------------------