# Rankings as a first-class facts theme

Right now rankings are joined into the `schools` directory by
`etl/schools_pipeline.R`. That works for the three rankings we carry today
(US News, Washington Monthly, Forbes) because each school has one
rank-per-source per year.

If we add more rankings (Niche, Princeton Review, WSJ/THE, regional
versions of WaMo, etc.), or want to track rank history across years, the
schools-as-wide-table model breaks. The right move at that point:

1. Promote rankings to its own module: `etl/modules/rankings_module_pipeline.R`.
2. Emit `rnk_facts.csv` with `(unitid, year, metric, value)` where the
   `metric` is e.g. `usnews_rank_2024`, `wamo_rank_2024`.
3. Keep the current single-rank columns in `schools` as a denormalized
   convenience for the most-recent year.
4. Move the ranking-source-specific cleaning logic from
   `schools_pipeline.R` into the new module.

The agent's `get_ranking(unitid, source)` tool would query the new
`rnk_facts` table; the existing wide columns in `schools` stay for
backwards compatibility but are deprecated.

## When to do this

When you're about to add a 4th ranking source, or when someone asks
"how has Holy Cross's US News rank changed since 2018?" and the answer
requires data we're not storing.
