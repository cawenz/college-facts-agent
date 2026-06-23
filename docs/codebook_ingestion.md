# Adding a new variable to the catalog

The agent reads its tool-layer descriptions from the `variables` table.
Adding a new fact-style metric means writing it through the ETL so that
both `facts` and `variables` rows land in Postgres.

## Pattern (IPEDS-sourced metric)

1. **Pick a module.** Aid? Admissions? Outcomes? Each module file
   (`etl/modules/<mod>_module_pipeline.R`) owns one theme.
2. **Define the metric in the module's `build_<mod>_variables()` function.**
   Add a row to the `tribble(...)` with these fields:

   ```r
   "your_metric_name",   "Display Name shown to users",
   "ipeds",              "IPEDS_TABLE: COLUMN (or formula)",
   "clustering",         # or "descriptive"
   "cross_category",
   "currency",           # or "percent", "ratio", "count", "code"
   FALSE, FALSE,         # neche_peer_set, neche_dashboard (legacy)
   "Any coverage caveat",
   ```

3. **Compute the metric in the same module.** Add the derivation that
   pulls from `get_table(year, "ADM<year>")` (or wherever) and emits a
   long-format tibble: `unitid, year, metric, value`.
4. **Update `data/variables_descriptions.csv`.** Add a plain-English
   definition. This is what the agent uses to decide if your metric is
   relevant to a user question.
5. **Run the module standalone to verify:**
   ```bash
   Rscript -e 'source("etl/schools_pipeline.R"); build_schools();
               source("etl/modules/<mod>_module_pipeline.R");
               run_<mod>_module()'
   ```
   Inspect `output/<mod>_facts.csv` for the new metric.
6. **Reload Postgres:** `python load/load_to_postgres.py`. The integrity
   check will fail if you forgot the `variables` row.

## Pattern (Academic Insights metric)

Same as above, but step 3 pulls from `ai_get()` with the metric id
configured in the module's `*_CONFIG$ai_metric_ids` list:

```r
FIN_CONFIG$ai_metric_ids$your_metric_name <- "<academic_insights_metric_id>"
```

Then in the module's collection loop, you call `ai_get(cfg,
paste0("facts/", cfg$ai_dataset), query = list(metric_id = ..., ...))`.

## Pattern (computed / derived metric)

For metrics that are functions of others (like `inst_discount_rate =
avg_inst_grant / published_tuition_fees`):

1. Compute the input metrics in their natural modules.
2. In one of the modules, after the input metrics are built, derive your
   metric by joining the long-format facts:

   ```r
   discount <- raw %>%
     filter(metric == "avg_inst_grant") %>%
     inner_join(raw %>% filter(metric == "published_tuition_fees"),
                by = c("unitid", "year")) %>%
     transmute(unitid, year, metric = "your_derived_metric",
               value = value.x / value.y)
   bind_rows(raw, discount)
   ```

3. Add the row to `build_<mod>_variables()` with `source = "ipeds_derived"`
   and a `ipeds_table_or_formula` that explains the formula.

## What NOT to do

- Don't add metrics directly to Postgres. Always go through the CSV layer
  so the metric is reproducible.
- Don't hardcode the metric name in `agent/tools.py`. The whole point of
  the catalog-driven design is that the agent discovers metrics from the
  `variables` table.
- Don't skip `variables_descriptions.csv`. Without a plain-English
  description, the agent has no way to know when a user question is
  asking about your metric.
