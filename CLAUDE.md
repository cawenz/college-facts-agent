# College Facts Agent

A tool-calling agent that answers factual questions about U.S. colleges and
universities from a local database built from IPEDS, College Scorecard, and the
US News (Academic Insights / CDS-aligned) API. Inference runs on a hosted model
API; the data stays local. Built and hosted by a solo maintainer on a Linux
home server (Lenovo ThinkCentre M720q), for personal use plus a few colleagues.

## Data flow

```
R ETL (etl/) ──> CSVs (output/) ──> loader (load/) ──> Postgres ──> FastAPI agent (agent/) ──> Haiku | Gemini
```

Only the user's question and the specific tool *results* the agent returns ever
leave the box. The raw dataset never does.

## Repo map

- `etl/schools_pipeline.R` — backbone. Builds `schools.csv` (one row per
  institution, decoded labels) and `value_labels.csv` (code→label). Also defines
  the shared helpers (`get_table`, `ai_get`, `scorecard_get`, `.out_path`) that
  every module sources.
- `etl/accdb_to_rda.R` — **primary** data-layer script. Converts NCES Access DBs
  you download by hand into the IPEDS `.Rda` bundles in `data/`. See "Data
  provenance" below.
- `etl/build_rda.R` — convenience fallback that auto-downloads + converts via the
  (unmaintained) `jbryer/ipeds` package. Prefer `accdb_to_rda.R`.
- `etl/modules/*_pipeline.R` — one module per domain (admissions, aid,
  athletics, enrollment, finance, outcomes). Each reads `schools.csv` + IPEDS
  `.Rda` + APIs and writes `{module}_facts.csv` and `{module}_variables.csv`.
- `data/` — raw inputs (IPEDS `.Rda` bundles, Carnegie xlsx). Repo root.
- `output/` — built CSVs. Repo root.
- `load/` — (to write) `schema.sql` + loader that lands the CSVs in Postgres.
- `agent/` — FastAPI agent. `app.py` (loop + `/chat` + auth), `tools.py`
  (tool layer), `config.py` (model switch).
- `docs/`, `tests/` — notes and the eval harness.

## Commands

Run R from the repo root (path helpers expect `data/` and `output/` there):

```bash
# Produce the IPEDS .Rda bundles (skip if data/IPEDS*.Rda already present).
# Primary: download Access DBs into data/accdb/ by hand, then convert:
Rscript etl/accdb_to_rda.R
# Fallback (auto-download via jbryer/ipeds): Rscript etl/build_rda.R
# See "Data provenance".

# Backbone (writes output/schools.csv, output/value_labels.csv)
Rscript -e 'source("etl/schools_pipeline.R"); build_schools()'

# A module — must source the backbone first for shared helpers
Rscript -e 'source("etl/schools_pipeline.R"); source("etl/modules/admissions_module_pipeline.R"); run_admissions_module()'

# Everything (once etl/run_all.R is written)
Rscript etl/run_all.R

# Load CSVs into Postgres (once load/ is written)
psql "$DB_DSN" -f load/schema.sql
python load/load_to_postgres.py

# Run the agent
uvicorn agent.app:app --host 127.0.0.1 --port 8000
```

## Environment variables

`ACADEMIC_INSIGHTS_API_KEY` (US News), `SCORECARD_API_KEY`, `DB_DSN`,
`AGENT_MODEL` (`haiku`|`gemini`), `ANTHROPIC_API_KEY` or `GEMINI_API_KEY`,
`API_TOKENS` (comma-separated bearer tokens for testers). Names live in
`.env.example`; never commit real values.

## Architecture principles (durable — keep these true)

- **Decouple the model.** The agent talks to one OpenAI-compatible interface via
  LiteLLM. Haiku vs Gemini is the `AGENT_MODEL` env var, not a code change.
- **Catalog as contract.** The `*_variables.csv` catalog is the single source of
  truth that BOTH the ETL and the agent read. Adding a variable = extraction
  logic + one catalog row; it then becomes answerable by the agent automatically,
  carrying its `source`, `format`, and `coverage_note`. Do not hardcode field
  lists in the agent — read the catalog.
- **Relational, not RAG.** Structured data is served by parameterized SQL through
  tools, never vector retrieval. The model never writes SQL — it picks a tool and
  arguments; field names are validated against the catalog before any query runs.
- **Long facts format.** Facts are `(unitid, year, metric, value)`. New metrics
  never require a schema migration.
- **Grounding.** Every figure the agent reports must carry its source and data
  year from the tool result. If a tool returns null, say the data isn't
  available — never estimate. Surface `coverage_note` caveats (e.g. "based on the
  ~45% of schools reporting to the CDS") instead of overclaiming.

## Conventions & gotchas

- **Year naming.** IPEDS uses fall-year (HD2024 = 2024-25). The US News/Academic
  Insights API lags by 2 years: AI year Y = IPEDS year Y−2. Facts tables use
  IPEDS naming throughout; `ipeds_to_ai_year()` / `ai_to_ipeds_year()` translate.
- **Variable churn across years is handled in code, keep it that way.** ADM
  totals (APPLCN/ADMSSN/ENRLT) became gender breakouts in 2024+; SAT 50th
  percentile is absent in 2020-21. Modules try the direct column, then fall back
  (sum components / 25-75 midpoint / DRVADM precomputed). Preserve this pattern
  when adding variables.
- **Categorical codes are decoded via `value_labels.csv`**, not inline maps.
- **Universe scope.** `keep_sectors = c(1, 2)` limits everything to public and
  private-nonprofit institutions. The agent silently inherits this — it will have
  nothing to say about community colleges or for-profits until the scope widens.
- **IPEDS structural churn.** Components move between years (e.g. net price moved
  from SFA to the new Cost (CST) component in 2024-25). The per-year extraction
  logic absorbs this; don't assume a metric lives in the same file every year.

## Data provenance — the `.Rda` bundles (critical)

The pipeline reads `data/IPEDS{collection}-{yy}.Rda`, each a list named `db` of
one collection year's survey tables. The pipeline loads them with bare `load()`
(decoupled at run time), but they are reproducible build artifacts, not
irreplaceable inputs. Two ways to produce them:

**Primary — `etl/accdb_to_rda.R` (self-owned, robust).** You download the
official Access DB(s) by hand from NCES → Use the Data → Download Access
Database, drop them in `data/accdb/` (gitignored), and run `Rscript
etl/accdb_to_rda.R`. It converts each via `Hmisc::mdb.get` (same reader the
original bundles used), writes the bundles + `data/ipeds_manifest.csv` (md5 +
build date), and provides `verify_against(new, reference)` to confirm parity
with an existing bundle before trusting it. No dependency on package download
logic. Deps: `mdbtools` (`apt install mdbtools`) + `Hmisc`.

**Fallback — `etl/build_rda.R` (convenience).** Auto-downloads + converts via
`remotes::install_github("jbryer/ipeds@<SHA>")`. Pin the SHA — the package is
unmaintained (last built ~2023) and pulls from NCES URLs that drift. Handy, but
the download logic is the fragile part, which is why `accdb_to_rda.R` is primary.

Prefer the Final release over Provisional; record which per year, since
provisional numbers change. If both Access-based paths ever break,
`stanislavzza/IPEDSR` (duckdb, 2004–present, actively maintained) is the likely
migration target, but its naming differs so the module extraction would adapt.

## Never do

- **Commit policy.** The IPEDS `.Rda` bundles ARE committed, via **git-lfs**
  (public domain; lfs keeps history clean). Never commit `.env`, API keys,
  `output/` (build artifacts that contain licensed US News–derived values), or
  the `data/accdb/` raw download cache. The US News / Academic Insights data is
  **licensed** — it must never be committed or pushed.
- **Never send the raw dataset to the model API.** Only selected tool results go
  out. Use the `source` field to gate `cds_ai`-sourced values if needed.
- **Never let the model write SQL** or reach fields outside the catalog.
- **Never reproduce US News content** beyond what the license permits.

## Current state

- **Done:** mature R ETL, 7 modules, 50+ variables, `value_labels.csv`,
  per-metric coverage reporting, year-aware extraction.
- **Scaffolded (needs rework):** the Python agent (`app.py`, `tools.py`,
  `schema.sql`). `tools.py` currently uses a hardcoded `FIELD_REGISTRY` — this
  must be replaced by reading the `variables` catalog from Postgres.
- **Not yet built:** the CSV→Postgres loader; `etl/run_all.R`; the codebook
  ingestion path; the auth-for-colleagues hardening; the eval harness.

## Next steps (priority order)

1. Write `load/schema.sql` + `load/load_to_postgres.py`: union `*_facts.csv` into
   one `facts` table, stack `*_variables.csv` into one `variables` catalog, plus
   `schools` and `value_labels`. Four tables.
2. Rework `agent/tools.py` to be **catalog-driven** over those tables (drop the
   hardcoded registry; validate requested metrics against the `variables` table).
3. Write `etl/run_all.R` and fix the `source("R/...")` path references to `etl/`.
4. Codebook ingestion (`docs/codebook_ingestion.md`): a repeatable path to add
   IPEDS variables — extraction + a `variables` catalog row with correct metadata.
5. Eval harness (`tests/eval_questions.yaml`): question/expected-source pairs run
   on every change, to catch a number that lost its year or source.
6. Decide whether to widen `keep_sectors` for a general-purpose agent.

## Open decisions

- Universe scope (sectors 1,2 only vs. all institutions).
- Whether `cds_ai` values get special handling before leaving the box for the API.
- Local inference later: revisit Ollama/vLLM if/when the RTX 3090 build happens;
  the LiteLLM decoupling means that's a config swap, not a rewrite.

See @README.md for the human quickstart and @agent/README.md for agent details.
