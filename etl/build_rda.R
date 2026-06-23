# =============================================================================
# accdb_to_rda  -  Windows-native conversion of an IPEDS .accdb to .Rda
# Holy Cross peer-comparison project
#
# WHEN YOU NEED THIS
#   The repo ships pre-built .Rda files for the years in the analytic panel.
#   When a new IPEDS collection is released and you want to add it, this
#   utility converts the downloaded .accdb to the .Rda format the pipeline
#   loads. Run once per new year, then commit the .Rda to the repo.
#
# REQUIREMENTS
#   - The Microsoft Access ODBC driver (ships with Office, or install the
#     "Microsoft Access Database Engine 2016 Redistributable" - free).
#   - install.packages(c("odbc", "DBI"))
#
# OUTPUT NAMING (matches the format the pipeline expects in data/)
#   IPEDS{prev_year}-{yy}.Rda   e.g.  IPEDS2025-26.Rda for collection year 2025
# =============================================================================

suppressPackageStartupMessages({
  library(odbc); library(DBI)
})

accdb_to_rda <- function(accdb_path, rda_path, verbose = TRUE) {
  if (!file.exists(accdb_path)) stop("Not found: ", accdb_path)
  accdb_abs <- normalizePath(accdb_path, winslash = "\\", mustWork = TRUE)
  conn_str <- sprintf(
    "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=%s;", accdb_abs
  )
  
  if (verbose) message("Opening: ", accdb_abs)
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_str)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tables <- DBI::dbListTables(con)
  tables <- tables[!grepl("^MSys", tables, ignore.case = TRUE)]
  if (!length(tables)) stop("No user tables found in ", accdb_abs)
  if (verbose) message(sprintf("Found %d tables. Reading...", length(tables)))
  
  db <- vector("list", length(tables))
  names(db) <- tables
  for (i in seq_along(tables)) {
    if (verbose) message(sprintf("  [%2d/%2d] %s", i, length(tables), tables[i]))
    db[[i]] <- tryCatch(
      DBI::dbGetQuery(con, sprintf("SELECT * FROM [%s]", tables[i])),
      error = function(e) {
        warning(sprintf("Could not read table '%s': %s",
                        tables[i], conditionMessage(e)))
        NULL })
  }
  db <- db[!vapply(db, is.null, logical(1))]
  
  if (verbose) message(sprintf("Saving %d tables to %s", length(db), rda_path))
  save(db, file = rda_path)
  invisible(db)
}

# -----------------------------------------------------------------------------
# Example - convert a new collection year, then commit the resulting .Rda:
#
#   setwd("path/to/hc-peer")
#   source("R/accdb_to_rda.R")
#
#   # Adjust these for the year you're adding:
#   accdb <- "C:/Users/me/Downloads/IPEDS202526.accdb"
#   rda   <- "data/IPEDS2025-26.Rda"
#
#   db <- accdb_to_rda(accdb, rda)
#   # then: git add data/IPEDS2025-26.Rda && git commit -m "Add 2025-26 IPEDS"
# -----------------------------------------------------------------------------