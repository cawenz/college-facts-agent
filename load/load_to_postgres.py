"""
Idempotent loader: schema.sql + CSVs under output/ -> Postgres.

Usage:
    python load/load_to_postgres.py

Reads DB_DSN from env. Truncates and reloads every run. Integrity checks:
- every metric in facts must have a row in variables
- every unitid in facts must exist in schools
Orphans are reported (and the load aborts if --strict is passed).

TODO (post-scaffold):
- streaming copy via psycopg's copy_expert for the long facts table
- progress bar
- a --diff mode that compares to a prior load before truncating
"""
from __future__ import annotations

import argparse
import csv
import os
import sys
from pathlib import Path

import psycopg
from psycopg import sql

ROOT = Path(__file__).resolve().parents[1]
OUTPUT_DIR = ROOT / "output"
SCHEMA_SQL = ROOT / "load" / "schema.sql"

MODULES = ["adm", "aid", "ath", "enr", "fin", "out"]


def get_dsn() -> str:
    dsn = os.getenv("DB_DSN")
    if not dsn:
        sys.exit("DB_DSN not set. See .env.example.")
    return dsn


def apply_schema(conn: psycopg.Connection) -> None:
    with conn.cursor() as cur:
        cur.execute(SCHEMA_SQL.read_text())
    conn.commit()
    print(f"Applied schema from {SCHEMA_SQL}")


def copy_csv(conn: psycopg.Connection, table: str, path: Path, columns: list[str]) -> int:
    """Copy a CSV into `table` via psycopg's COPY ... FROM STDIN. Returns row count."""
    if not path.exists():
        print(f"  skip: {path.name} (missing)")
        return 0
    n = 0
    with conn.cursor() as cur:
        copy_sql = sql.SQL("COPY {table} ({cols}) FROM STDIN WITH CSV HEADER").format(
            table=sql.Identifier(table),
            cols=sql.SQL(", ").join(sql.Identifier(c) for c in columns),
        )
        with cur.copy(copy_sql) as cp, open(path, "rb") as f:
            f.readline()  # consume header (we still pass HEADER to be defensive)
            f.seek(0)
            while chunk := f.read(1 << 20):
                cp.write(chunk)
        n = cur.rowcount
    return n


def load_schools(conn: psycopg.Connection) -> int:
    path = OUTPUT_DIR / "schools.csv"
    # columns come from the CSV header; if you add ranking sources, update schema.sql first
    with open(path) as f:
        header = next(csv.reader(f))
    return copy_csv(conn, "schools", path, header)


def load_variables(conn: psycopg.Connection) -> int:
    total = 0
    for mod in MODULES:
        path = OUTPUT_DIR / f"{mod}_variables.csv"
        if not path.exists():
            continue
        with open(path) as f:
            header = next(csv.reader(f))
        cols = [c for c in header if c in {
            "metric", "category", "display_name", "source",
            "ipeds_table_or_formula", "use_type", "comparison_scope",
            "format", "coverage_note", "notes"
        }]
        total += copy_csv(conn, "variables", path, cols)
    return total


def load_facts(conn: psycopg.Connection) -> int:
    total = 0
    for mod in MODULES:
        path = OUTPUT_DIR / f"{mod}_facts.csv"
        if not path.exists():
            continue
        with open(path) as f:
            header = next(csv.reader(f))
        cols = [c for c in header if c in {"unitid", "year", "metric", "value", "var_type"}]
        total += copy_csv(conn, "facts", path, cols)
    return total


def load_value_labels(conn: psycopg.Connection) -> int:
    path = OUTPUT_DIR / "value_labels.csv"
    if not path.exists():
        return 0
    with open(path) as f:
        header = next(csv.reader(f))
    return copy_csv(conn, "value_labels", path, header)


def integrity_checks(conn: psycopg.Connection, strict: bool) -> None:
    queries = {
        "facts metrics not in variables":
            "SELECT DISTINCT f.metric FROM facts f LEFT JOIN variables v USING (metric) "
            "WHERE v.metric IS NULL LIMIT 20",
        "facts unitids not in schools":
            "SELECT DISTINCT f.unitid FROM facts f LEFT JOIN schools s USING (unitid) "
            "WHERE s.unitid IS NULL LIMIT 20",
    }
    issues = 0
    for label, q in queries.items():
        with conn.cursor() as cur:
            cur.execute(q)
            rows = cur.fetchall()
        if rows:
            issues += len(rows)
            print(f"  ! {label} ({len(rows)} sample):")
            for r in rows:
                print(f"    - {r[0]}")
    if issues == 0:
        print("  OK: no orphans")
    elif strict:
        sys.exit("Integrity check failed (--strict). Aborting.")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--strict", action="store_true",
                        help="abort if integrity checks find orphan rows")
    args = parser.parse_args()

    dsn = get_dsn()
    with psycopg.connect(dsn) as conn:
        apply_schema(conn)
        n_schools   = load_schools(conn);       print(f"schools: {n_schools:,}")
        n_vars      = load_variables(conn);     print(f"variables: {n_vars:,}")
        n_facts     = load_facts(conn);         print(f"facts: {n_facts:,}")
        n_labels    = load_value_labels(conn);  print(f"value_labels: {n_labels:,}")
        conn.commit()
        print("\nIntegrity checks:")
        integrity_checks(conn, strict=args.strict)


if __name__ == "__main__":
    main()
