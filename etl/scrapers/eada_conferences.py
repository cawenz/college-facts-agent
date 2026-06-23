"""
scrape_eada_conferences.py
==========================

Build data/eada_conferences.csv from:
  - data/eada_2024_25/instLevel.xlsx    (EADA's institution-level table)
  - Wikipedia institution-list tables for NCAA D-I, D-II, D-III, NAIA

EADA does NOT publish a conference column. This script scrapes Wikipedia's
maintained institution-list tables (which DO have conference) and joins
them to the EADA name -> IPEDS UnitID index using a three-tier matching
strategy:

  1. Explicit override     — hand-curated dict for known mismatches
  2. Normalized exact match — lowercase + strip campus suffixes + collapse
                              filler words; most names match exactly here
  3. Fuzzy match (cutoff 0.95) — difflib on the normalized form, with a
                                  tight cutoff to avoid false positives
                                  like "Siena University" -> "Iona University"

Three service academies (Air Force, Army, Navy) are intentionally skipped
because EADA doesn't include them.

Output files (written to data/):
  eada_conferences.csv             one row per matched school
                                   columns: unitid, conference, match_method
  eada_conferences_unmatched.csv   audit trail of Wikipedia rows that
                                   couldn't be matched to an EADA UnitID

Usage:
    pip install pandas openpyxl lxml html5lib
    python R/scrape_eada_conferences.py
"""

import io
import re
import sys
from collections import defaultdict
from difflib import get_close_matches
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd
import urllib.request


# Wikipedia rejects requests with the default Python User-Agent (HTTP 403).
# Identify the scraper honestly per Wikimedia's user-agent policy.
# https://meta.wikimedia.org/wiki/User-Agent_policy
USER_AGENT = (
    "peer_schools-EADA-conference-scraper/1.0 "
    "(https://github.com/; institutional research; contact via repo)"
)

# ----------------------------------------------------------------------------
# Paths (resolve relative to repo root)
# ----------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parent.parent
EADA_INST = REPO_ROOT / "data" / "eada_2024_25" / "instLevel.xlsx"
OUT_CSV = REPO_ROOT / "data" / "eada_conferences.csv"
UNMATCHED_CSV = REPO_ROOT / "data" / "eada_conferences_unmatched.csv"

WIKI_PAGES = {
    "D-I":  "https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_institutions",
    "D-II": "https://en.wikipedia.org/wiki/List_of_NCAA_Division_II_institutions",
    "D-III":"https://en.wikipedia.org/wiki/List_of_NCAA_Division_III_institutions",
    "NAIA": "https://en.wikipedia.org/wiki/List_of_NAIA_institutions",
}

# Column-name keywords that identify the conference column on each page.
# D-I uses "Primary" (primary conference); D-II/D-III/NAIA use "Conference".
CONFERENCE_COL_KEYWORDS = ("Conference", "Primary")


# ----------------------------------------------------------------------------
# Name normalization for matching
# ----------------------------------------------------------------------------
# Common IPEDS suffixes that appear in the EADA institution_name but not on
# Wikipedia (e.g. "Pennsylvania State University-Main Campus" -> "Penn State").
CAMPUS_SUFFIXES = (
    "main campus|pittsburgh campus|fort collins|college park|ann arbor|"
    "twin cities|tempe|norman campus|columbia|stillwater|san luis obispo|"
    "springfield|new york|oxford|metropolitan campus|kent|at kent|"
    "campus immersion|seattle campus|tuscarawas|trumbull"
)
FILLER = {"the", "of", "at", "in", "and"}


def normalize(s: str) -> str:
    """Lowercase, strip suffixes/punctuation, collapse filler words."""
    s = str(s).lower().strip()
    s = re.sub(rf"-({CAMPUS_SUFFIXES})$", "", s)
    s = re.sub(r"\s+main campus$", "", s)
    s = s.replace("&", " and ").replace("st.", "st")
    s = re.sub(r"\bsaint\b", "st", s)
    s = re.sub(r"[^a-z0-9\s]+", " ", s)
    tokens = [t for t in re.sub(r"\s+", " ", s).split() if t not in FILLER]
    return " ".join(tokens)


# ----------------------------------------------------------------------------
# Hand-curated overrides
# ----------------------------------------------------------------------------
# Wikipedia name on the left -> exact EADA institution_name on the right.
# Use None to deliberately skip (e.g. service academies aren't in EADA).
#
# After the first run, check data/eada_conferences_unmatched.csv and add
# entries here for cases the fuzzy match misses or gets wrong.
OVERRIDES: Dict[str, Optional[str]] = {
    # Service academies — not in EADA (skip)
    "Air Force Academy": None,
    "United States Air Force Academy": None,
    "Army": None,
    "United States Military Academy": None,
    "Navy": None,
    "United States Naval Academy": None,
    # Known IPEDS canonical name variations
    "The Ohio State University": "Ohio State University-Main Campus",
    "Leland Stanford Junior University": "Stanford University",
    "State University of New York at Binghamton": "Binghamton University",
    "Concordia University–St. Paul":     "Concordia University-Saint Paul",
    "Concordia University-St. Paul":     "Concordia University-Saint Paul",
    "Hawai'i Pacific University":        "Hawaii Pacific University",

    # D-I master-list normalization gaps. EADA uses the full IPEDS name
    # with "at Campus" / "-Main Campus" suffixes that the normalizer
    # doesn't always know to strip; Wikipedia uses the colloquial form.
    "Alabama Agricultural and Mechanical University":   "Alabama A & M University",
    "Iowa State University of Science and Technology":  "Iowa State University",
    "Duquesne University of the Holy Spirit":           "Duquesne University",
    "North Carolina State University":                  "North Carolina State University at Raleigh",
    "Kent State University":                            "Kent State University at Kent",
    "Arizona State University":                         "Arizona State University Campus Immersion",
    "Oklahoma State University–Stillwater":             "Oklahoma State University-Main Campus",
    "Oklahoma State University-Stillwater":             "Oklahoma State University-Main Campus",
    "Siena University":                                 "Siena College",
    "North Dakota State University of Agriculture and Applied Sciences": "North Dakota State University-Main Campus",
    "North Carolina Agricultural and Technical State University": "North Carolina A & T State University",
}


# ----------------------------------------------------------------------------
# Wikipedia table fetch + parse
# ----------------------------------------------------------------------------
def _has_conf_col(cols: List[str]) -> bool:
    """True if any column name contains a conference keyword."""
    return any(any(k in c for k in CONFERENCE_COL_KEYWORDS) for c in cols)


def fetch_wiki_table(url: str) -> pd.DataFrame:  # noqa: D401
    """Find the LARGEST wikitable with both a School/Institution column and a
    conference-like column. The 'largest' tiebreak matters on the D-I page
    where multiple tables include both columns but only the master table
    has the full school list."""
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(req, timeout=30) as resp:
        html = resp.read().decode("utf-8", errors="replace")
    tables = pd.read_html(io.StringIO(html))

    candidates = []
    for t in tables:
        cols = [str(c).strip() for c in t.columns]
        has_school = any("School" in c or "Institution" in c for c in cols)
        if has_school and _has_conf_col(cols):
            candidates.append(t)
    if not candidates:
        raise RuntimeError(f"No suitable institutions table found at {url}")
    return max(candidates, key=len)


def extract_pairs(df: pd.DataFrame) -> List[Tuple[str, str]]:
    """Pull [(school, conference), ...] from a Wikipedia institutions table."""
    school_col = next(
        c for c in df.columns if "School" in str(c) or "Institution" in str(c)
    )
    conf_col = next(
        c for c in df.columns
        if any(k in str(c) for k in CONFERENCE_COL_KEYWORDS)
    )
    pairs = []
    for _, row in df[[school_col, conf_col]].dropna().iterrows():
        school = re.sub(r"\[.*?\]", "", str(row[school_col])).strip()
        conf = re.sub(r"\[.*?\]", "", str(row[conf_col])).strip()
        # Drop parentheticals like "(Football only in 2027)" — they bloat
        # the conference value without adding signal.
        conf = conf.split("(")[0].strip()
        if school and conf and school != "nan" and conf != "nan":
            pairs.append((school, conf))
    return pairs


# ----------------------------------------------------------------------------
# EADA name index
# ----------------------------------------------------------------------------
def build_eada_index(
    inst_path: Path,
) -> Tuple[Dict[str, int], Dict[str, List[str]]]:
    """Read EADA instLevel.xlsx. Return:
        name_to_uid:    {EADA institution_name -> unitid}
        norm_to_names:  {normalized name -> [original EADA names]}
    Most normalized names map to a single original; collisions are surfaced
    in the matcher as 'ambiguous' rather than guessed.
    """
    inst = pd.read_excel(inst_path, usecols=["unitid", "institution_name"])
    name_to_uid: dict[str, int] = {}
    norm_to_names: dict[str, list[str]] = defaultdict(list)
    for _, row in inst.iterrows():
        nm, uid = row["institution_name"], row["unitid"]
        if pd.isna(nm) or pd.isna(uid):
            continue
        nm = str(nm).strip()
        try:
            uid_i = int(uid)
        except (TypeError, ValueError):
            continue
        name_to_uid[nm] = uid_i
        norm_to_names[normalize(nm)].append(nm)
    return name_to_uid, dict(norm_to_names)


# ----------------------------------------------------------------------------
# Three-tier matcher
# ----------------------------------------------------------------------------
def match_pair(
    wiki_school: str,
    name_to_uid: Dict[str, int],
    norm_to_names: Dict[str, List[str]],
    overrides: Dict[str, Optional[str]],
) -> Tuple[Optional[int], str, str]:
    """Return (unitid, eada_name, method) or (None, '', reason)."""
    if wiki_school in overrides:
        v = overrides[wiki_school]
        if v is None:
            return None, "", "explicit_skip"
        if v in name_to_uid:
            return name_to_uid[v], v, "override"
        return None, "", f"override_missing:{v}"

    nm = normalize(wiki_school)
    if nm in norm_to_names:
        candidates = norm_to_names[nm]
        if len(candidates) == 1:
            return name_to_uid[candidates[0]], candidates[0], "exact_norm"
        return None, "", f"ambiguous:{candidates}"

    close = get_close_matches(nm, list(norm_to_names.keys()), n=1, cutoff=0.95)
    if close:
        c = norm_to_names[close[0]][0]
        return name_to_uid[c], c, "fuzzy_0.95+"
    return None, "", "no_match"


# ----------------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------------
def main() -> None:
    if not EADA_INST.exists():
        sys.exit(f"EADA file not found: {EADA_INST}")

    print(f"Reading EADA institutions from {EADA_INST}...")
    name_to_uid, norm_to_names = build_eada_index(EADA_INST)
    print(f"  Loaded {len(name_to_uid)} EADA institutions")

    all_rows = []
    for div_label, url in WIKI_PAGES.items():
        print(f"\nFetching {div_label} from {url}...")
        try:
            df = fetch_wiki_table(url)
        except Exception as e:
            print(f"  FAILED: {e}")
            continue
        pairs = extract_pairs(df)
        print(f"  Parsed {len(pairs)} (school, conference) pairs")

        matched_n = 0
        for school, conf in pairs:
            uid, ename, method = match_pair(
                school, name_to_uid, norm_to_names, OVERRIDES
            )
            all_rows.append({
                "division":    div_label,
                "wiki_school": school,
                "conference":  conf,
                "unitid":      uid,
                "eada_name":   ename,
                "method":      method,
            })
            if uid is not None:
                matched_n += 1
        print(f"  Matched {matched_n} / {len(pairs)}")

    results = pd.DataFrame(all_rows)

    # If a school appears in multiple divisions (rare; Wikipedia sometimes
    # double-lists during conference moves), prefer override > exact > fuzzy.
    method_rank = {"override": 0, "exact_norm": 1, "fuzzy_0.95+": 2}
    results["rank"] = results["method"].map(method_rank).fillna(99)
    matched = (
        results.dropna(subset=["unitid"])
        .sort_values(["unitid", "rank"])
        .drop_duplicates(subset="unitid", keep="first")
        .copy()
    )

    # ---- Write the clean two-column CSV the R pipeline consumes ----
    out_df = matched[["unitid", "conference", "method"]].rename(
        columns={"method": "match_method"}
    )
    out_df["unitid"] = out_df["unitid"].astype(int)
    out_df = out_df.sort_values("unitid").reset_index(drop=True)
    OUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    out_df.to_csv(OUT_CSV, index=False)
    print(f"\nWrote {len(out_df)} rows to {OUT_CSV.relative_to(REPO_ROOT)}")

    # ---- Audit trail: every Wikipedia row that DIDN'T match ----
    unmatched = results[results["unitid"].isna()].copy()
    if len(unmatched):
        unmatched.to_csv(UNMATCHED_CSV, index=False)
        print(
            f"Wrote {len(unmatched)} unmatched rows to "
            f"{UNMATCHED_CSV.relative_to(REPO_ROOT)}"
        )
        print("\nFirst 10 unmatched (for OVERRIDES dict triage):")
        print(
            unmatched[["division", "wiki_school", "conference", "method"]]
            .head(10)
            .to_string(index=False)
        )
    else:
        print("All Wikipedia rows matched. No unmatched audit file written.")


if __name__ == "__main__":
    main()
