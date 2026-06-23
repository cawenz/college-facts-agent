"""
Catalog-driven tool layer for the agent.

STATUS: stub. The intent is for the agent to NOT hardcode field lists; instead
it reads the `variables` table at startup, exposes a small number of generic
tools, and lets the model select metrics by name + description.

Suggested tool surface (small, generic, composable):

  search_metrics(query: str, limit: int = 20) -> list[Metric]
    full-text-ish lookup against variables.display_name + notes.

  get_fact(unitid: int, metric: str, year: int | None = None) -> Fact
    most recent year if year=None; returns value + format + source caveat.

  list_schools(name_query: str | None = None, state: str | None = None,
               sector: str | None = None, limit: int = 25) -> list[School]
    resolves school name -> unitid before calling get_fact.

  get_ranking(unitid: int, source: Literal["usnews","wamo","forbes"]) -> Ranking
    pulls from the schools table (rankings are stored there, not in facts).

  get_value_label(metric: str, code: str) -> str
    resolves a categorical code (e.g. Carnegie basic 2021).

TODO:
  - tools.json compatible schemas for litellm
  - per-tool authz (read-only here, but the pattern matters when we add writes)
"""
from __future__ import annotations

import os
from typing import Iterable

import psycopg


def _conn() -> psycopg.Connection:
    return psycopg.connect(os.environ["DB_DSN"])


def search_metrics(query: str, limit: int = 20) -> list[dict]:
    """Stub: ILIKE match on display_name and notes."""
    sql = """
        SELECT metric, display_name, source, format, coverage_note
        FROM variables
        WHERE display_name ILIKE %(q)s OR notes ILIKE %(q)s OR metric ILIKE %(q)s
        ORDER BY display_name
        LIMIT %(lim)s
    """
    with _conn() as c, c.cursor() as cur:
        cur.execute(sql, {"q": f"%{query}%", "lim": limit})
        cols = [d.name for d in cur.description]
        return [dict(zip(cols, r)) for r in cur.fetchall()]


def get_fact(unitid: int, metric: str, year: int | None = None) -> dict | None:
    """Stub: returns the most recent (unitid, metric) row when year is None."""
    if year is None:
        sql = """
            SELECT unitid, year, metric, value
            FROM facts
            WHERE unitid = %(u)s AND metric = %(m)s
            ORDER BY year DESC
            LIMIT 1
        """
        params = {"u": unitid, "m": metric}
    else:
        sql = """
            SELECT unitid, year, metric, value
            FROM facts
            WHERE unitid = %(u)s AND metric = %(m)s AND year = %(y)s
        """
        params = {"u": unitid, "m": metric, "y": year}
    with _conn() as c, c.cursor() as cur:
        cur.execute(sql, params)
        row = cur.fetchone()
        if not row:
            return None
        cols = [d.name for d in cur.description]
        return dict(zip(cols, row))


def list_schools(name_query: str | None = None, state: str | None = None,
                 sector: str | None = None, limit: int = 25) -> list[dict]:
    """Stub: simple filters over the schools directory."""
    where, params = ["TRUE"], {"lim": limit}
    if name_query:
        where.append("instnm ILIKE %(n)s"); params["n"] = f"%{name_query}%"
    if state:
        where.append("stabbr = %(s)s"); params["s"] = state
    if sector:
        where.append("sector = %(sec)s"); params["sec"] = sector
    sql = (
        "SELECT unitid, instnm, stabbr, sector, control, usnews_rank, wamo_rank, forbes_rank "
        "FROM schools WHERE " + " AND ".join(where) +
        " ORDER BY instnm LIMIT %(lim)s"
    )
    with _conn() as c, c.cursor() as cur:
        cur.execute(sql, params)
        cols = [d.name for d in cur.description]
        return [dict(zip(cols, r)) for r in cur.fetchall()]
