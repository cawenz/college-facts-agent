# Data Sources

Every fact in the agent traces back to one of the sources below. The
`variables.source` column in Postgres carries the source key; the
`variables.ipeds_table_or_formula` column carries the specific table or
derivation.

## IPEDS (public domain)

Integrated Postsecondary Education Data System, administered by NCES.

- **Where it lives:** `data/IPEDS<YYYY-YY>.Rda`, one bundle per collection
  cycle. Each bundle is a named list of tables (HD, IC, ADM, SFA, EF, GR,
  C, F, etc.) mirroring the IPEDS Access database.
- **How it's loaded:** `get_table(year, table_name)` in
  `etl/schools_pipeline.R` reads the bundle and returns the table. Year
  here is the fall cohort year (e.g. `2024` for the 2024-25 cycle).
- **How to refresh:** `Rscript etl/build_rda.R` downloads the source
  Access databases and re-emits the .Rda files. Requires `mdbtools` and
  the pinned `jbryer/ipeds` R package. NCES typically releases each cycle
  in waves (admissions ~6 months ahead of cost/finance), so a fresh
  bundle may be incomplete until late in the year.
- **What we use:**
  - `HD` — institutional directory (instnm, stabbr, sector, etc.)
  - `IC` and `IC_AY` — institutional characteristics + cost charges
  - `ADM` — admissions counts (applications, admits, enrolls)
  - `SFA` — student financial aid (grant aid, net price)
  - `EF` — enrollment by race/sex
  - `GR`, `OM` — graduation rates, outcome measures
  - `C` — completions by program
  - `F` — finance (revenues, expenses, assets)

## Academic Insights (licensed)

Custom dataset from Academic Insights LLC, used primarily for Common Data
Set (CDS) detail that IPEDS doesn't capture cleanly (test-score detail,
admissions yield by category, class-size distributions).

- **Where it lives:** API only — no local copy. Calls go through `ai_get()`
  in `etl/schools_pipeline.R`.
- **Auth:** `ACADEMIC_INSIGHTS_API_KEY` env var.
- **Dataset id and metric ids** are pinned in each module's `*_CONFIG` list.
  Updating to a new dataset version means updating those constants.
- **Caveat:** because it's licensed, raw responses must not be redistributed.
  Only derived facts land in `output/`. The agent must surface AI-sourced
  facts with the `source` field set to "academic_insights" so downstream
  consumers can filter.

## College Scorecard (public domain)

US Department of Education's institution-level outcome data — earnings,
debt, repayment rates.

- **Where it lives:** API (`api.data.gov/ed/collegescorecard`).
- **Auth:** `SCORECARD_API_KEY`.
- **Used by:** `outcomes_module_pipeline.R` for median earnings, debt
  measures, and the subset of completion-rate metrics that Scorecard
  reports differently from IPEDS.

## EADA (public domain)

Equity in Athletics Disclosure Act dump — required for any school
receiving federal aid that has an athletics program.

- **Where it lives:** `data/eada_2024_25/` (raw xlsx exports), and
  `data/eada_conferences.csv` (derived from the Python scraper).
- **What's missing from EADA itself:** conference assignments. EADA
  publishes division (D1/D2/D3/NAIA) but not the specific conference. The
  scraper at `etl/scrapers/eada_conferences.py` fills that gap.
- **Used by:** `athletics_module_pipeline.R`.

## US News (licensed)

US News & World Report's annual Best Colleges public data file.

- **Where it lives:** `data/2025-Public-Data-File.xlsx`. Released yearly,
  hand-purchased.
- **What we use:** the published `usnews_rank` and `usnews_classification`
  fields, joined into `output/schools.csv` by the backbone.
- **Caveat:** US News' license restricts redistribution of the underlying
  data — only the rank itself can be surfaced. The agent must NOT echo
  back the full data file or derived US News indicators.

## Washington Monthly (public)

Washington Monthly's annual rankings, focused on social-mobility outcomes.

- **Where it lives:** `data/washington_monthly_2025.xlsx`.
- **What we use:** the `wamo_rank` column joined into the schools
  directory. WaMo splits its ranking by category (national universities,
  liberal arts, regional); the column carries the rank as published.

## Forbes (public)

Forbes Top Colleges ranking.

- **Where it lives:** `data/forbes_top_colleges_2025.csv` (cleaned) and
  `data/forbes_page_2025.html` (raw HTML cached).
- **Refresh:** `python etl/scrapers/forbes_rankings.py` — re-scrapes the
  current Forbes page and emits the cleaned CSV. The HTML is cached so we
  can re-derive without hitting the live site.
- **What we use:** `forbes_rank` into the schools directory.

## Custom (project-internal)

- `data/variables_descriptions.csv` — plain-English definitions for every
  metric. Curated; powers the agent's tool descriptions and response
  framing. **Edit this CSV** when adding a new variable so the agent can
  describe it without code changes.

## Year conventions

All `year` values in `facts.year` are **fall cohort years**:
- `2024` = fall 2024 entering cohort
- For admissions, this is the applying cohort
- For finance, this is the fiscal year reported in the matching IPEDS cycle
- For graduation rates, this is the cohort year that the rate measures
  (so a 6yr grad rate for `year=2018` measures fall-2018 entrants who
  graduated by spring 2024)

Different sources release data on different schedules:

| Source           | Lag from cohort year | Notes |
|------------------|----------------------|-------|
| IPEDS ADM        | ~9 months            | Released ~April after the prior fall |
| IPEDS IC_AY      | ~12 months           | Cost data lags ADM |
| IPEDS F          | ~18 months           | Finance lags both |
| Scorecard        | ~3 years             | Earnings are post-graduation |
| EADA             | ~6 months            | Annual fall release |
| US News          | ~6 months            | Annual September release |
| Washington Mo.   | ~9 months            | Annual September release |
| Forbes           | ~6 months            | Annual September release |
| Academic Insights| varies               | Refresh cadence per metric |

## Licensing summary

| Source             | Redistribute facts? | Notes |
|--------------------|---------------------|-------|
| IPEDS              | yes                 | Public domain |
| Scorecard          | yes                 | Public domain |
| EADA               | yes                 | Public |
| Washington Monthly | yes (with attribution) | Public |
| Forbes             | rank only           | Source on display |
| US News            | rank only           | License restricts derived data |
| Academic Insights  | derived facts only  | Never raw API responses |
