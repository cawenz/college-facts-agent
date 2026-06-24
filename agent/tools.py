"""
Catalog-driven tool layer for the agent.

This is the ONLY module that touches the database. The model never sees the
data and never writes SQL — it picks one of the functions below by name and
passes arguments; we run parameterized SQL and hand back a small, grounded
result (every figure carries its source + year + caveats).

Tool surface (built incrementally):
  [x] search_metrics(query)            -- discover what's answerable (the catalog)
  [x] resolve_school(name, state?)     -- name -> unitid, with disambiguation
  [x] get_facts(unitid, metrics[], yr) -- the workhorse; validates + grounds
  [x] get_school(unitid)               -- attributes + rankings (non-facts data)
  [x] decode(variable, code?)          -- categorical code -> label

Design rules that apply to every tool here:
  * Parameterized SQL only. Values are bound, never string-formatted in.
  * Metric/field names are validated against the `variables` catalog before
    any query that uses them runs (see future get_facts).
  * Results carry provenance (source, year, format, coverage_note) so the
    model cannot report a number stripped of its caveats.
  * A missing row returns None / [] — the agent says "not available", it does
    not estimate.
"""
from __future__ import annotations

import os

import psycopg


def _dsn() -> str:
    dsn = os.getenv("DB_DSN")
    if not dsn:
        raise RuntimeError("DB_DSN not set; see .env.example")
    return dsn


def _connect() -> psycopg.Connection:
    # One connection per call for now: this is a low-traffic personal agent, so
    # the connect cost is fine and it keeps each tool self-contained. When app.py
    # is wired we'll likely swap this for a psycopg_pool.ConnectionPool.
    # autocommit + read_only is defense-in-depth: a SELECT-only surface should
    # not be able to write even if a future query is buggy.
    conn = psycopg.connect(_dsn(), autocommit=True)
    conn.read_only = True
    return conn


def _rows(cur) -> list[dict]:
    cols = [d.name for d in cur.description]
    return [dict(zip(cols, r)) for r in cur.fetchall()]


# Hard cap on rows any single tool will return, so the model can't pull the
# whole catalog/table into a prompt by asking for a huge limit.
_MAX_LIMIT = 100

# Cap on how many metrics one get_facts call will resolve, to bound output.
_MAX_METRICS = 25

# Licensing policy for third-party content (US News / CDS-derived facts, and the
# branded rankings). The maintainer holds a US News data license, so the policy
# is ANNOTATE: values leave the box WITH full attribution (source + edition) so
# the model cites them correctly. Set _LICENSE_POLICY = "withhold" to null these
# values at the two chokepoints below -- the policy decision lives in one place.
_LICENSED_SOURCES = {"cds_ai", "cds_ai_derived"}
_LICENSE_POLICY = "annotate"  # "annotate" | "withhold"

# Ranking provenance. Edition years track what the ETL currently loads:
#   US News  -> Academic Insights AI year 2026 = the "2026 Best Colleges"
#               edition, released Sept 2025 (ipeds_to_ai_year(2024) = 2026).
#   Washington Monthly -> data/washington_monthly_2025.xlsx (2025 edition)
#   Forbes   -> data/forbes_top_colleges_2025.csv (2025 edition)
# UPDATE these when the ETL refreshes to a newer edition. (Longer term: store
# the vintage in the DB during ETL instead of hardcoding it here.)
_RANKING_META = {
    "usnews": {"source": "U.S. News & World Report", "edition": "2026"},
    "wamo":   {"source": "Washington Monthly",       "edition": "2025"},
    "forbes": {"source": "Forbes",                    "edition": "2025"},
}


def _apply_source_policy(facts: list[dict]) -> list[dict]:
    """Gate facts before they leave the box (one of two license chokepoints).

    Default "annotate" is a passthrough: licensed values already carry `source`
    and `coverage_note` from the JOIN. "withhold" nulls any licensed-source value.
    """
    if _LICENSE_POLICY == "withhold":
        for f in facts:
            if f.get("source") in _LICENSED_SOURCES:
                f["value"] = None
                f["withheld"] = True
    return facts


def _apply_ranking_policy(rankings: dict) -> dict:
    """Gate the branded rankings leaving the box (the second license chokepoint).

    "annotate" (default): return ranks with their source + edition attached.
    "withhold": null the rank values.
    """
    if _LICENSE_POLICY == "withhold":
        for key in ("usnews", "washington_monthly", "forbes"):
            block = rankings.get(key)
            if isinstance(block, dict):
                block["rank"] = None
                block["withheld"] = True
    return rankings


def search_metrics(query: str, limit: int = 25) -> list[dict]:
    """Discover answerable metrics by keyword.

    Substring-matches `query` against the human-meaningful catalog fields and
    returns each metric's name plus the metadata the model needs to choose it
    and report it honestly. An empty query lists the catalog (up to `limit`).

    Returns: list of
        {metric, display_name, category, source, format, coverage_note}
    """
    limit = max(1, min(limit, _MAX_LIMIT))
    sql = """
        SELECT metric, display_name, category, source, format, coverage_note
        FROM variables
        WHERE metric        ILIKE %(q)s
           OR display_name  ILIKE %(q)s
           OR category      ILIKE %(q)s
           OR notes         ILIKE %(q)s
        ORDER BY category, display_name
        LIMIT %(lim)s
    """
    with _connect() as conn, conn.cursor() as cur:
        cur.execute(sql, {"q": f"%{query}%", "lim": limit})
        return _rows(cur)


# Institution aliases: normalized query -> unitid. A pragmatic seed of the
# alias mechanism (later fed by Scorecard's school.alias, generated acronyms,
# and resolutions learned from usage). resolve_school consults this first and
# PINS the aliased school to the top of the candidate list without hiding the
# other substring matches. Hand-set for the maintainer's context for now:
# early users at the College of the Holy Cross say "Holy Cross" to mean it.
_ALIASES: dict[str, int] = {
    "holy cross": 166124,   # College of the Holy Cross (Worcester, MA)
}

# Shared SELECT column list for school candidate rows. A trusted constant, so
# it is safe to interpolate into SQL text.
_SCHOOL_COLS = "unitid, instnm, stabbr, sector_label, control_grp, usnews_rank"


def _normalize_alias_key(s: str) -> str:
    return " ".join(s.lower().split())


def resolve_school(name: str, state: str | None = None, limit: int = 10) -> list[dict]:
    """Resolve a school name to candidate unitid(s), best match first.

    This intentionally returns a RANKED CANDIDATE LIST, not a single answer:
    many queries are ambiguous ("Holy Cross" matches four institutions). Each
    candidate carries disambiguating attributes plus a `match` quality so the
    caller can decide whether it is confident enough to proceed, or should ask
    the user which institution they mean.

    Ordering: exact name matches are pinned to the top; everything else is
    ranked by recognizability (in the ranked universe, then by US News rank),
    with prefix matches only breaking ties, then alphabetical. Prominence is
    usually a better proxy for which school a casual query means than
    prefix-vs-partial ("Holy Cross" -> College of the Holy Cross, not the
    Indiana one that happens to start with the words).

    match quality:
      "alias"   - matched a known alias and was pinned to the top (see _ALIASES;
                  e.g. "Holy Cross" -> College of the Holy Cross)
      "exact"   - name equals instnm (case-insensitive)
      "prefix"  - instnm starts with name
      "partial" - name appears somewhere in instnm

    Args:
      name:  free-text school name (substring match against instnm)
      state: optional 2-letter postal code to narrow (e.g. "MA")
      limit: max candidates (capped at _MAX_LIMIT)

    Returns: list of
      {unitid, instnm, stabbr, sector_label, control_grp, usnews_rank, match}
    """
    if not name or not name.strip():
        return []
    name = name.strip()
    limit = max(1, min(limit, _MAX_LIMIT))
    alias_unitid = _ALIASES.get(_normalize_alias_key(name))
    params = {"q": name, "like": f"%{name}%", "lim": limit}
    where = ["instnm ILIKE %(like)s"]
    state_norm = state.strip().upper() if state and state.strip() else None
    if state_norm:
        where.append("stabbr = %(st)s")
        params["st"] = state_norm
    # The match tier is computed once in SQL and reused for both the returned
    # `match` label and the ORDER BY, so they can never disagree.
    sql = f"""
        SELECT {_SCHOOL_COLS},
               CASE
                 WHEN lower(instnm) = lower(%(q)s) THEN 'exact'
                 WHEN instnm ILIKE %(q)s || '%%'   THEN 'prefix'
                 ELSE 'partial'
               END AS match
        FROM schools
        WHERE {" AND ".join(where)}
        ORDER BY
          CASE WHEN lower(instnm) = lower(%(q)s) THEN 0 ELSE 1 END,  -- exact pinned to top
          in_ranked_universe DESC,                                   -- then recognizability
          usnews_rank ASC NULLS LAST,
          CASE WHEN instnm ILIKE %(q)s || '%%' THEN 0 ELSE 1 END,    -- prefix only breaks ties
          instnm ASC
        LIMIT %(lim)s
    """
    with _connect() as conn, conn.cursor() as cur:
        cur.execute(sql, params)
        rows = _rows(cur)
        # Alias pre-check: pin the aliased school to the top (match="alias"),
        # unless an explicit state filter points elsewhere. The other substring
        # candidates still follow, so the default is confident but overridable
        # -- never a silent single pick.
        if alias_unitid is not None:
            cur.execute(
                f"SELECT {_SCHOOL_COLS}, 'alias' AS match FROM schools WHERE unitid = %(au)s",
                {"au": alias_unitid},
            )
            arows = _rows(cur)
            arow = arows[0] if arows else None
            if arow and (state_norm is None or arow["stabbr"] == state_norm):
                rows = [arow] + [r for r in rows if r["unitid"] != alias_unitid]
                rows = rows[:limit]
        return rows


# Grounded fact columns: every value comes back wrapped with the provenance the
# model needs to report it honestly (display_name, year, source, format, and any
# coverage caveat).
_FACT_SELECT = """
    f.metric, v.display_name, f.year, f.value,
    v.source, v.format, v.coverage_note
"""


def get_facts(
    unitid: int,
    metrics: list[str],
    year: int | None = None,
    all_years: bool = False,
) -> dict:
    """Fetch grounded facts for one school across one or more metrics.

    Requested metrics are validated against the `variables` catalog before any
    facts query runs: a name that is not in the catalog never reaches the facts
    table. Each returned value is JOINed to its catalog row, so it carries its
    source, data year, format, and coverage caveat.

    Year axis:
      all_years=True   -> the full time series for each metric (year ignored)
      year=<int>       -> just that collection year
      year=None        -> the latest available year per metric (default)

    Returns a dict with three buckets, so absence is explicit (never guessed):
      {
        "unitid": int,
        "facts": [ {metric, display_name, year, value, source, format,
                    coverage_note}, ... ],   # found + grounded
        "no_data": [str, ...],        # valid metric, but no value for this school
        "unknown_metrics": [str, ...] # not in the catalog at all (re-check name)
      }
    """
    requested = [m.strip() for m in (metrics or []) if m and m.strip()]
    requested = requested[:_MAX_METRICS]
    if not requested:
        return {"unitid": unitid, "facts": [], "no_data": [], "unknown_metrics": []}

    with _connect() as conn, conn.cursor() as cur:
        # 1) Validate against the catalog. This is the "model can't reach a
        #    field outside the catalog" guarantee — and, since metric names are
        #    now whitelist-checked, it doubles as injection defense for the
        #    ANY() filter below.
        cur.execute(
            "SELECT metric FROM variables WHERE metric = ANY(%(ms)s)",
            {"ms": requested},
        )
        known = {r[0] for r in cur.fetchall()}
        known_list = [m for m in requested if m in known]
        unknown = [m for m in requested if m not in known]

        # 2) Fetch facts for the valid metrics, JOINed to the catalog.
        facts: list[dict] = []
        if known_list:
            params = {"u": unitid, "ms": known_list}
            if all_years:
                sql = f"""
                    SELECT {_FACT_SELECT}
                    FROM facts f JOIN variables v ON v.metric = f.metric
                    WHERE f.unitid = %(u)s AND f.metric = ANY(%(ms)s)
                    ORDER BY f.metric, f.year
                """
            elif year is not None:
                params["y"] = year
                sql = f"""
                    SELECT {_FACT_SELECT}
                    FROM facts f JOIN variables v ON v.metric = f.metric
                    WHERE f.unitid = %(u)s AND f.metric = ANY(%(ms)s)
                      AND f.year = %(y)s
                    ORDER BY f.metric
                """
            else:
                # latest available year per metric
                sql = f"""
                    SELECT DISTINCT ON (f.metric) {_FACT_SELECT}
                    FROM facts f JOIN variables v ON v.metric = f.metric
                    WHERE f.unitid = %(u)s AND f.metric = ANY(%(ms)s)
                    ORDER BY f.metric, f.year DESC
                """
            cur.execute(sql, params)
            facts = _rows(cur)

    facts = _apply_source_policy(facts)
    present = {f["metric"] for f in facts}
    no_data = [m for m in known_list if m not in present]
    return {
        "unitid": unitid,
        "facts": facts,
        "no_data": no_data,
        "unknown_metrics": unknown,
    }


# Curated columns for get_school: decoded *_label values, raw code twins
# dropped. The grouping into identity/classification/rankings/athletics happens
# in Python from this flat row.
_SCHOOL_DETAIL_COLS = """
    instnm, stabbr, latitude, longitud,
    control_grp, iclevel_label,
    basic2021_label, ic2025_label, setting2025_label,
    religious_affiliation, religious_tradition, accreditor, hbcu_label,
    usnews_rank, usnews_classification, wamo_rank, wamo_category, forbes_rank,
    in_ranked_universe,
    athletics_body, athletics_division, athletics_conference, has_football,
    athletics_sports_list
"""


def get_school(unitid: int) -> dict | None:
    """Fetch one school's non-facts attributes: identity, classification,
    rankings, and athletics.

    This is the companion to get_facts: facts are longitudinal numbers, while
    these are current per-school attributes that live in the `schools` table.
    Rankings (US News / Washington Monthly / Forbes) live here, not in facts.

    Returns a grouped dict (decoded labels, raw code columns dropped), or None
    if the unitid is not in the directory:
      {
        "unitid": int,
        "identity":       {name, state, latitude, longitude},
        "classification": {control, level, carnegie_basic_2021, carnegie_2025,
                           setting_2025, religious_affiliation,
                           religious_tradition, accreditor, hbcu},
        "rankings":       {usnews: {rank, category, source, edition},
                           washington_monthly: {rank, category, source, edition},
                           forbes: {rank, source, edition}, in_ranked_universe},
        "athletics":      {body, division, conference, has_football, sports},
      }
    """
    with _connect() as conn, conn.cursor() as cur:
        cur.execute(
            f"SELECT {_SCHOOL_DETAIL_COLS} FROM schools WHERE unitid = %(u)s",
            {"u": unitid},
        )
        rows = _rows(cur)
    if not rows:
        return None
    r = rows[0]
    return {
        "unitid": unitid,
        "identity": {
            "name": r["instnm"],
            "state": r["stabbr"],
            "latitude": r["latitude"],
            "longitude": r["longitud"],
        },
        "classification": {
            "control": r["control_grp"],
            "level": r["iclevel_label"],
            "carnegie_basic_2021": r["basic2021_label"],
            "carnegie_2025": r["ic2025_label"],
            "setting_2025": r["setting2025_label"],
            "religious_affiliation": r["religious_affiliation"],
            "religious_tradition": r["religious_tradition"],
            "accreditor": r["accreditor"],
            "hbcu": r["hbcu_label"],
        },
        "rankings": _apply_ranking_policy({
            "usnews": {
                "rank": r["usnews_rank"],
                "category": r["usnews_classification"],
                **_RANKING_META["usnews"],
            },
            "washington_monthly": {
                "rank": r["wamo_rank"],
                "category": r["wamo_category"],
                **_RANKING_META["wamo"],
            },
            "forbes": {
                "rank": r["forbes_rank"],
                **_RANKING_META["forbes"],
            },
            "in_ranked_universe": r["in_ranked_universe"],
        }),
        "athletics": {
            "body": r["athletics_body"],
            "division": r["athletics_division"],
            "conference": r["athletics_conference"],
            "has_football": r["has_football"],
            "sports": r["athletics_sports_list"],
        },
    }


def decode(
    variable: str,
    code: str | None = None,
    table: str | None = None,
    limit: int = _MAX_LIMIT,
) -> list[dict]:
    """Resolve a categorical code to its label via value_labels.

    value_labels is keyed by (table_name, variable, code). `variable` is the
    IPEDS/Carnegie field name (e.g. "SECTOR", "BASIC2021"), matched
    case-insensitively. Most variables map to a single table_name, but a few
    (CONTROL, TRIBAL, ...) appear under more than one survey with different code
    sets -- pass `table` to disambiguate, or read the table_name on each row.

    Modes:
      code given -> the label(s) for that code
      code None  -> enumerate all (code, label) for the variable (capped)

    Returns: list of {table_name, variable, code, label}. Empty if no match
    (the agent says the code is unknown; it does not invent a label).
    """
    if not variable or not variable.strip():
        return []
    limit = max(1, min(limit, _MAX_LIMIT))
    params = {"v": variable.strip().upper(), "lim": limit}
    where = ["upper(variable) = %(v)s"]
    if code is not None and str(code).strip() != "":
        where.append("code = %(c)s")
        params["c"] = str(code).strip()
    if table and table.strip():
        where.append("table_name = %(t)s")
        params["t"] = table.strip()
    sql = f"""
        SELECT table_name, variable, code, label
        FROM value_labels
        WHERE {" AND ".join(where)}
        ORDER BY table_name, code
        LIMIT %(lim)s
    """
    with _connect() as conn, conn.cursor() as cur:
        cur.execute(sql, params)
        return _rows(cur)
