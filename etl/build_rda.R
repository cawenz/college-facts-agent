# =============================================================================
# build_rda.R — convenience fallback that auto-downloads NCES IPEDS Access DBs
#               and converts them to data/IPEDS{collection}-{yy}.Rda bundles via
#               the (unmaintained) jbryer/ipeds R package.
#
# PREFER etl/accdb_to_rda.R. Use this only if you don't want to download the
# Access DBs by hand. The package is the fragile link — NCES URLs and table
# layouts have drifted since the last release. Pin a known-good SHA, and run
# verify_against() (in accdb_to_rda.R) on the result before trusting it.
#
# ── DEPENDENCIES ────────────────────────────────────────────────────────────
#   System:  mdbtools  →  apt install mdbtools
#   R:       remotes, Hmisc, ipeds (jbryer/ipeds @ pinned SHA)
#
# ── USAGE ───────────────────────────────────────────────────────────────────
#   Rscript etl/build_rda.R                  # builds every supported year
#   Rscript -e 'source("etl/build_rda.R"); build_rda_year(2024)'   # one year
# =============================================================================

# Pin a SHA that you've verified produces the same bundles as accdb_to_rda.R.
# Update only after re-running verify_against() against a known-good bundle.
IPEDS_PKG_SHA <- "PIN_ME"     # e.g. "abc1234..."

# Which collection years this script supports producing.
SUPPORTED_YEARS <- 2010:2024

install_ipeds_pkg <- function() {
  if (requireNamespace("ipeds", quietly = TRUE)) return(invisible())
  if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
  if (IPEDS_PKG_SHA == "PIN_ME")
    stop("Pin IPEDS_PKG_SHA before running build_rda.R — see file header.")
  remotes::install_github(paste0("jbryer/ipeds@", IPEDS_PKG_SHA))
}

# Build one bundle (collection year = fall-year of the cycle).
# Writes data/IPEDS{cy}-{yy}.Rda where the object is a list named `db`.
build_rda_year <- function(collection_year, out_dir = "data") {
  install_ipeds_pkg()
  stopifnot(collection_year %in% SUPPORTED_YEARS)

  ending <- collection_year + 1L
  yy     <- sprintf("%02d", ending %% 100)
  out    <- file.path(out_dir, sprintf("IPEDS%d-%s.Rda", collection_year, yy))

  message(sprintf("Downloading + converting %d-%s ...", collection_year, yy))

  # The package's download helper drops Access DBs under data/downloaded/ and
  # mdb.get-converts them. Exact API varies by jbryer/ipeds SHA — if this errors,
  # check the package's README at the pinned SHA.
  db <- ipeds::load_ipeds(year = collection_year, cache_dir = "data/downloaded")
  save(db, file = out)
  message(sprintf("  wrote %s (%d tables)", out, length(db)))
  invisible(out)
}

build_all <- function() {
  for (cy in SUPPORTED_YEARS) {
    tryCatch(build_rda_year(cy),
             error = function(e) warning(sprintf("year %d failed: %s", cy, conditionMessage(e))))
  }
}

if (sys.nframe() == 0) build_all()
