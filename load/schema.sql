-- Postgres schema for the college facts catalog.
-- Four tables: schools, variables, facts, value_labels.
-- The facts table is long (unitid, year, metric, value) and unions all six
-- *_facts.csv files. variables is the metric catalog, unioning all six
-- *_variables.csv files.
--
-- Run: psql $DB_DSN -f load/schema.sql

BEGIN;

DROP TABLE IF EXISTS facts CASCADE;
DROP TABLE IF EXISTS variables CASCADE;
DROP TABLE IF EXISTS value_labels CASCADE;
DROP TABLE IF EXISTS schools CASCADE;

CREATE TABLE schools (
    unitid                INTEGER PRIMARY KEY,
    instnm                TEXT    NOT NULL,
    stabbr                TEXT,
    city                  TEXT,
    sector                TEXT,
    control               TEXT,
    carnegie_basic_2021   TEXT,
    in_ranked_universe    BOOLEAN,
    usnews_classification TEXT,
    usnews_rank           NUMERIC,
    wamo_rank             NUMERIC,
    forbes_rank           NUMERIC,
    longitude             NUMERIC,
    latitude              NUMERIC
);
COMMENT ON TABLE schools IS
  'Institution directory. Loaded as-is from output/schools.csv. Extend with new ranking columns by altering this table before reload.';

CREATE TABLE variables (
    metric                TEXT    PRIMARY KEY,
    category              TEXT,
    display_name          TEXT    NOT NULL,
    source                TEXT,
    ipeds_table_or_formula TEXT,
    use_type              TEXT,
    comparison_scope      TEXT,
    format                TEXT,
    coverage_note         TEXT,
    notes                 TEXT
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

CREATE INDEX facts_metric_idx       ON facts (metric);
CREATE INDEX facts_year_idx         ON facts (year);
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
