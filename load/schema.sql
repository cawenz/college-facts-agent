-- Postgres schema for the college facts catalog.
-- Four tables: schools, variables, facts, value_labels.
-- The facts table is long (unitid, year, metric, value) and unions all six
-- *_facts.csv files. variables is the metric catalog, unioning all six
-- *_variables.csv files.
--
-- Run: psql $DB_DSN -f load/schema.sql   (or via load/load_to_postgres.py)
--
-- DESIGN NOTES
--  * schools is loaded AS-IS from output/schools.csv: one column per CSV
--    column, in CSV order, so a new ranking/attribute column in the ETL needs
--    only a matching column added here before reload (no transform step).
--    Categorical code columns (sector, ic2025, basic2021, apm, ...) are kept
--    as TEXT so they join to value_labels.code (also TEXT). Their decoded
--    *_label companions are carried alongside.
--  * R writes missing values as the literal NA; the loader passes NULL 'NA'
--    to COPY, so the NUMERIC/BOOLEAN columns below accept those as NULL.

BEGIN;

DROP TABLE IF EXISTS facts CASCADE;
DROP TABLE IF EXISTS variables CASCADE;
DROP TABLE IF EXISTS value_labels CASCADE;
DROP TABLE IF EXISTS schools CASCADE;

-- schools: column set + order mirror output/schools.csv exactly.
CREATE TABLE schools (
    unitid                     INTEGER PRIMARY KEY,
    latest_year                INTEGER,
    instnm                     TEXT NOT NULL,
    sector                     TEXT,
    control                    TEXT,
    iclevel                    TEXT,
    stabbr                     TEXT,
    longitud                   NUMERIC,
    latitude                   NUMERIC,
    hbcu                       TEXT,
    hospital                   TEXT,
    medical                    TEXT,
    tribal                     TEXT,
    instcat                    TEXT,
    locale                     TEXT,
    instsize                   TEXT,
    control_grp                TEXT,
    relaffil                   TEXT,
    religious_affiliation_code TEXT,
    religious_affiliation      TEXT,
    religious_tradition        TEXT,
    usnews_classification      TEXT,
    in_ranked_universe         BOOLEAN,
    usnews_rank                NUMERIC,
    wamo_rank                  NUMERIC,
    wamo_category              TEXT,
    forbes_rank                NUMERIC,
    ic2025                     TEXT,
    ic2025_label               TEXT,
    saec2025                   TEXT,
    saec2025_label             TEXT,
    research2025               TEXT,
    research2025_label         TEXT,
    setting2025                TEXT,
    highest_degree_2025        TEXT,
    basic2021                  TEXT,
    ic2025size                 TEXT,
    ic2025alf                  TEXT,
    apm                        TEXT,
    gpm                        TEXT,
    apm_max_cip2percent        NUMERIC,
    apm_max_cip2_name          TEXT,
    earnings_ratio             NUMERIC,
    pbi                        TEXT,
    annhsi                     TEXT,
    aanapisi                   TEXT,
    hsi                        TEXT,
    nasnti                     TEXT,
    womenonly                  TEXT,
    rpu                        TEXT,
    cce                        TEXT,
    lpp                        TEXT,
    accreditor                 TEXT,
    sector_label               TEXT,
    control_label              TEXT,
    iclevel_label              TEXT,
    hbcu_label                 TEXT,
    hospital_label             TEXT,
    medical_label              TEXT,
    tribal_label               TEXT,
    instcat_label              TEXT,
    locale_label               TEXT,
    instsize_label             TEXT,
    basic2021_label            TEXT,
    setting2025_label          TEXT,
    highest_degree_2025_label  TEXT,
    ic2025size_label           TEXT,
    ic2025alf_label            TEXT,
    apm_label                  TEXT,
    gpm_label                  TEXT,
    -- Athletics institutional attributes, merged in from output/schools_athletics.csv
    -- (the athletics module's per-school attribute file, keyed by unitid). These
    -- are categorical attributes, not longitudinal facts, so they live here.
    athletics_body                  TEXT,
    athletics_division              TEXT,
    athletics_conference            TEXT,
    has_football                    BOOLEAN,
    athletics_classification_raw    TEXT,
    athletics_classification_code   TEXT,
    athletics_classification_other  TEXT,
    athletics_sports_list           TEXT
);
COMMENT ON TABLE schools IS
  'Institution directory, loaded as-is from output/schools.csv (one column per CSV column, CSV order). Code columns are TEXT to join value_labels; *_label columns carry the decoded text.';

CREATE TABLE variables (
    metric                 TEXT PRIMARY KEY,
    category               TEXT,
    display_name           TEXT NOT NULL,
    source                 TEXT,
    ipeds_table_or_formula TEXT,
    use_type               TEXT,
    comparison_scope       TEXT,
    format                 TEXT,
    neche_peer_set         BOOLEAN,
    neche_dashboard        BOOLEAN,
    coverage_note          TEXT,
    notes                  TEXT
);
COMMENT ON TABLE variables IS
  'Metric catalog. Unions all *_variables.csv. Every metric in facts.metric must have a row here.';

CREATE TABLE facts (
    unitid    INTEGER NOT NULL REFERENCES schools(unitid) ON DELETE RESTRICT,
    year      INTEGER NOT NULL,
    metric    TEXT    NOT NULL REFERENCES variables(metric) ON DELETE RESTRICT,
    value     NUMERIC,
    var_type  TEXT,
    PRIMARY KEY (unitid, year, metric)
);
COMMENT ON TABLE facts IS
  'Long-format facts. value is NUMERIC; categorical codes are joined via value_labels by (metric, value).';

CREATE INDEX facts_metric_idx        ON facts (metric);
CREATE INDEX facts_year_idx          ON facts (year);
CREATE INDEX facts_unitid_metric_idx ON facts (unitid, metric);

CREATE TABLE value_labels (
    table_name TEXT NOT NULL,
    variable   TEXT NOT NULL,
    code       TEXT NOT NULL,
    label      TEXT NOT NULL,
    PRIMARY KEY (table_name, variable, code)
);
COMMENT ON TABLE value_labels IS
  'IPEDS / Carnegie code-to-label maps. Loaded as-is from output/value_labels.csv.';

COMMIT;
