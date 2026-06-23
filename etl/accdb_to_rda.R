# =============================================================================
# accdb_to_rda.R — convert local NCES IPEDS Access databases into the
#                  data/IPEDS{collection}-{yy}.Rda bundles the pipeline expects.
#
# This is the PRIMARY, self-owned data-layer path. You download the official
# Access database(s) by hand (a stable step you control), and this script
# converts each into a list named `db` of survey tables — with NO dependency on
# the unmaintained jbryer/ipeds download logic. (etl/build_rda.R remains as a
# convenience fallback that auto-downloads via that package.)
#
# ── ONE MANUAL STEP PER YEAR (~2 min) ───────────────────────────────────────
#   1. NCES IPEDS → Use the Data → Download Access Database:
#        https://nces.ed.gov/ipeds/use-the-data/download-access-database
#   2. Download the FINAL release (preferred; provisional only if Final isn't out)
#      for each collection year you want.
#   3. Put the .accdb (or older .mdb) files in  data/accdb/  — gitignored, large,
#      and always re-downloadable, so they don't belong in git.
#
# ── DEPENDENCIES ────────────────────────────────────────────────────────────
#   System:  mdbtools     →  apt install mdbtools      (reads Access files on Linux)
#   R:       Hmisc        →  install.packages("Hmisc") (mdb.get() wraps mdbtools)
#   Note: the original bundles were made with Hmisc::mdb.get via the ipeds
#   package, so using it here maximizes drop-in parity. Confirm with verify_against().
#
# ── RUN ─────────────────────────────────────────────────────────────────────
#   Rscript etl/accdb_to_rda.R           # converts everything in data/accdb/
#
#   # then confirm parity against an existing known-good bundle, e.g.:
#   Rscript -e 'source("etl/accdb_to_rda.R"); \
#               verify_against("data/IPEDS2023-24.Rda", "data/IPEDS2023-24.backup.Rda")'
# =============================================================================

# If verify_against() reveals table-name differences vs your existing bundles
# (e.g. Access stores "valuesets23" but the pipeline reads "valueSets23"), map
# them here as  c(access_name = pipeline_name)  and they'll be applied on convert.
RENAME_MAP <- c(
  # "valuesets23" = "valueSets23"
)

# Collection year is the FIRST 4-digit year in the filename
# ("IPEDS_2023-24_Final.accdb" -> 2023). Bundle is named IPEDS{2023}-{24}.Rda.
parse_collection_year <- function(fname) {
  m <- regmatches(fname, regexpr("(19|20)\\d{2}", fname))
  if (!length(m)) NA_integer_ else as.integer(m)
}

apply_renames <- function(db) {
  if (length(RENAME_MAP)) {
    hit <- names(db) %in% names(RENAME_MAP)
    names(db)[hit] <- RENAME_MAP[names(db)[hit]]
  }
  db
}

# ── Convert a single Access DB to one .Rda bundle ───────────────────────────
accdb_to_rda <- function(accdb_path, collection_year, out_dir = "data") {
  ending <- collection_year + 1L
  yy     <- sprintf("%02d", ending %% 100)
  out    <- file.path(out_dir, sprintf("IPEDS%d-%s.Rda", collection_year, yy))

  message(sprintf("Converting %s  ->  %s", basename(accdb_path), basename(out)))

  # lowernames = FALSE keeps the original UPPERCASE IPEDS variable names
  # (the modules reference APPLCN, INSTNM, etc.). tables = NULL reads every table.
  db <- Hmisc::mdb.get(accdb_path, tables = NULL, lowernames = FALSE)
  if (is.data.frame(db)) db <- list(db)              # single-table safety
  if (is.null(names(db)) || !length(db))
    stop("mdb.get returned no named tables for ", accdb_path)

  db <- apply_renames(db)
  save(db, file = out)                                # object MUST be named `db`
  message(sprintf("  wrote %d tables", length(db)))

  data.frame(
    file            = basename(out),
    collection_year = collection_year,
    ending_year     = ending,
    n_tables        = length(db),
    source_accdb    = basename(accdb_path),
    built_on        = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
}

# ── Convert everything in data/accdb/ and write a manifest ──────────────────
convert_all <- function(accdb_dir = file.path("data", "accdb"), out_dir = "data") {
  if (!requireNamespace("Hmisc", quietly = TRUE))
    stop("Package 'Hmisc' is required: install.packages('Hmisc')")

  files <- list.files(accdb_dir, pattern = "\\.(accdb|mdb)$",
                      full.names = TRUE, ignore.case = TRUE)
  if (!length(files))
    stop("No .accdb/.mdb files in ", accdb_dir,
         " — download them from NCES first (see header).")

  rows <- list()
  for (f in files) {
    cy <- parse_collection_year(basename(f))
    if (is.na(cy)) { warning("Could not parse year from ", basename(f), "; skipping."); next }
    rows[[length(rows) + 1L]] <- accdb_to_rda(f, cy, out_dir = out_dir)
  }
  if (!length(rows)) stop("Nothing converted.")

  manifest <- do.call(rbind, rows)
  manifest$md5 <- vapply(file.path(out_dir, manifest$file),
                         function(p) tools::md5sum(p)[[1]], character(1))
  write.csv(manifest, file.path(out_dir, "ipeds_manifest.csv"), row.names = FALSE)
  message("wrote ", file.path(out_dir, "ipeds_manifest.csv"))
  invisible(manifest)
}

# ── Parity check: does a freshly-converted bundle match a known-good one? ────
# Run this ONCE against an existing bundle for the same year before trusting the
# converter. It compares table names, row/col counts, and column-name sets.
verify_against <- function(new_rda, reference_rda) {
  e1 <- new.env(); e2 <- new.env()
  load(new_rda, envir = e1); load(reference_rda, envir = e2)
  a <- get("db", e1); b <- get("db", e2)

  cat("Tables only in NEW:      ", paste(setdiff(names(a), names(b)), collapse = ", "), "\n")
  cat("Tables only in REFERENCE:", paste(setdiff(names(b), names(a)), collapse = ", "), "\n")

  common <- intersect(names(a), names(b))
  res <- do.call(rbind, lapply(common, function(t) data.frame(
    table      = t,
    new_rows   = nrow(a[[t]]), ref_rows = nrow(b[[t]]),
    new_cols   = ncol(a[[t]]), ref_cols = ncol(b[[t]]),
    cols_match = setequal(names(a[[t]]), names(b[[t]])),
    stringsAsFactors = FALSE
  )))

  mism <- subset(res, new_rows != ref_rows | new_cols != ref_cols | !cols_match)
  if (nrow(mism) == 0)
    cat("\nPARITY OK: all common tables match on rows, columns, and column names.\n")
  else { cat("\nMISMATCHES (investigate before trusting the converter):\n"); print(mism) }
  invisible(res)
}

# Run conversion when invoked as a script (not when sourced for the helpers).
if (sys.nframe() == 0) convert_all()
