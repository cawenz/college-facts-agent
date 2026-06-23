# run_all.R - sources the backbone and every module in order, then writes
# all artifacts under output/. Run from the repo root:
#
#   Rscript etl/run_all.R
#
# Requires:
#   - data/ populated with IPEDS*.Rda + auxiliary files (see docs/data_sources.md)
#   - ACADEMIC_INSIGHTS_API_KEY in environment (or .Renviron)
#   - Optional: SCORECARD_API_KEY for outcomes module's earnings detail

stopifnot(file.exists("etl/schools_pipeline.R"))

# 1. Backbone: builds output/schools.csv + output/value_labels.csv
source("etl/schools_pipeline.R")
schools <- build_schools()

# 2. Modules. Each writes <module>_facts.csv and <module>_variables.csv into output/.
modules <- c(
  "etl/modules/admissions_module_pipeline.R",
  "etl/modules/aid_module_pipeline.R",
  "etl/modules/athletics_module_pipeline.R",
  "etl/modules/enrollment_module_pipeline.R",
  "etl/modules/finance_module_pipeline.R",
  "etl/modules/outcomes_module_pipeline.R"
)

runners <- c(
  "run_admissions_module",
  "run_aid_module",
  "run_ath_module",
  "run_enrollment_module",
  "run_finance_resources_module",
  "run_outcomes_module"
)

for (i in seq_along(modules)) {
  message("\n== Sourcing ", modules[i], " ==")
  source(modules[i])
  fn <- get(runners[i])
  res <- fn()
  message("  wrote: ", paste(names(res), collapse = ", "))
}

message("\nETL complete. CSVs in output/:")
message(paste("  -", list.files("output", pattern = "\\.csv$"), collapse = "\n"))
