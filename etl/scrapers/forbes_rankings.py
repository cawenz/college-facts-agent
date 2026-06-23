"""
scrape_forbes_rankings.py
=========================

Build data/forbes_top_colleges_<year>.csv from
  https://www.forbes.com/top-colleges/

Forbes' ranking page is JavaScript-rendered with lazy-loaded rows (the
table only contains a handful of rows on initial paint; scrolling down
appends the rest). Static-HTTP scraping (urllib / requests) returns an
empty table. So we use Playwright (headless Chromium) to load the page,
scroll until the row count plateaus, then read the DOM directly.

Forbes uses Cloudflare-style bot defenses. The default headless config
sometimes hits a challenge page; if that happens, re-run with
HEADLESS=0 to launch a visible browser and solve any human-check
manually. The script also writes a snapshot of the page HTML at
data/forbes_page_<year>.html so you can inspect what the scraper saw.

Output files (written to data/):
  forbes_top_colleges_<year>.csv     one row per ranked school
                                     columns: rank, name, state, type
  forbes_page_<year>.html            captured page HTML (audit / debug)

The CSV does NOT contain IPEDS UnitIDs. R's build_forbes() in
schools_pipeline.R matches names + states to schools.csv via the same
normalize-then-fuzzy approach the EADA scraper uses.

Usage:
    pip install playwright
    playwright install chromium
    python R/scrape_forbes_rankings.py              # headless
    HEADLESS=0 python R/scrape_forbes_rankings.py   # visible browser
    YEAR=2025 python R/scrape_forbes_rankings.py    # override year tag

Re-run annually after Forbes releases the new list (typically late
August). If the HTML structure changes, update SELECTORS below.
"""

from __future__ import annotations

import csv
import os
import re
import sys
from pathlib import Path
from typing import List, Optional

try:
    from playwright.sync_api import sync_playwright, TimeoutError as PWTimeout
except ImportError:
    sys.exit(
        "Playwright not installed.\n"
        "  pip install playwright\n"
        "  playwright install chromium\n"
    )

# ----------------------------------------------------------------------------
# Config — adjust if Forbes' DOM changes
# ----------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = REPO_ROOT / "data"

URL = "https://www.forbes.com/top-colleges/"
YEAR = os.environ.get("YEAR", "2025")
HEADLESS = os.environ.get("HEADLESS", "1") != "0"
OUT_CSV = DATA_DIR / f"forbes_top_colleges_{YEAR}.csv"
OUT_HTML = DATA_DIR / f"forbes_page_{YEAR}.html"

# Multiple CSS selectors are tried; Forbes has rotated naming over the
# years (table.fcs-table, ol.list-promo, div[data-ga-track="ranking"], …).
# Order matters — the first that finds rows wins.
ROW_SELECTORS = [
    "table tbody tr",                   # most common
    "div.fcs-table div[role='row']",    # 2024 list shape
    "ol li",                            # fallback
]

# Inside a row, try these for each cell. We don't care about column
# headers — we infer rank from positional / numeric pattern.
CELL_SELECTOR = "td, div[role='cell'], span.col"

USER_AGENT = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_5) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/127.0.0.0 Safari/537.36"
)

# When scrolling, declare "done" after this many consecutive scrolls
# return zero new rows.
SCROLL_STABLE_PASSES = 4
SCROLL_DELTA_PX = 2500
SCROLL_DELAY_MS = 800
MAX_SCROLLS = 200       # hard backstop so a layout glitch can't loop forever
TARGET_ROWS = 500       # Forbes publishes a top-500 list


def _state_code_from(text: str) -> Optional[str]:
    """Pull a trailing two-letter state code out of a cell, e.g. 'MA'.
    Forbes typically renders state as a short suffix or its own cell.
    """
    m = re.search(r"\b([A-Z]{2})\b\s*$", (text or "").strip())
    return m.group(1) if m else None


def _looks_like_rank(text: str) -> Optional[int]:
    """Return the integer rank if the text is a bare rank cell."""
    t = (text or "").strip().lstrip("#")
    if t.isdigit() and 1 <= int(t) <= 999:
        return int(t)
    return None


TYPE_RE     = re.compile(r"^\s*(Public|Private(?:\s+not-for-profit)?)\s*$",
                          re.IGNORECASE)
STATE_RE    = re.compile(r"^\s*[A-Z]{2}\s*$")
MONEY_RE    = re.compile(r"^[$\-+]?[\d,]+(\.\d+)?%?$")
TRAIL_ST_RE = re.compile(r",\s*([A-Z]{2})\s*$")


def _parse_row(cells: List[str]) -> Optional[dict]:
    """Convert a list of cell strings into a {rank, name, state, type}
    record, or None if the row doesn't look like a ranking row.

    Forbes' table columns are typically ordered:
        rank | name | state | type | <metrics: salary, debt, ...>
    so we classify cells by content and take the *first* name candidate
    rather than the longest — "Private not-for-profit" is 22 chars,
    which used to outrun "Columbia University" (19) under a longest-wins
    rule and produce garbage names for ~10% of rows.
    """
    rank: Optional[int] = None
    name: Optional[str] = None
    state: Optional[str] = None
    forbes_type: Optional[str] = None
    name_candidates: List[str] = []

    for raw in cells:
        c = (raw or "").strip()
        if not c:
            continue
        # 1. Rank — first numeric cell in 1..999
        if rank is None:
            r = _looks_like_rank(c)
            if r is not None:
                rank = r
                continue
        # 2. Type cell — exact-match Public / Private / Private not-for-profit
        if TYPE_RE.match(c):
            if forbes_type is None:
                forbes_type = c
            continue
        # 3. Bare state code — "MA", "NY", ...
        if STATE_RE.match(c):
            if state is None:
                state = c
            continue
        # 4. Currency / numeric metric — skip outright
        if MONEY_RE.match(c):
            continue
        # 5. Anything else > 4 chars is a name candidate
        if len(c) > 4:
            name_candidates.append(c)

    # First name candidate in column order = the school name.
    if name_candidates:
        name = name_candidates[0]
        # Trailing ", XX" -> pull off state if we didn't already get one
        m = TRAIL_ST_RE.search(name)
        if m and state is None:
            state = m.group(1)
            name = TRAIL_ST_RE.sub("", name).strip()

    if rank is None or name is None:
        return None
    return {"rank": rank,
            "name": name,
            "state": state or "",
            "type": forbes_type or ""}


def scrape() -> List[dict]:
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=HEADLESS)
        ctx = browser.new_context(user_agent=USER_AGENT,
                                   viewport={"width": 1400, "height": 1000})
        page = ctx.new_page()

        print(f"Loading {URL} (headless={HEADLESS}) ...", flush=True)
        # Forbes runs constant background ad / analytics traffic, so
        # `networkidle` never resolves and goto() hangs the full 60s.
        # `domcontentloaded` returns as soon as the DOM is parsed; we
        # then rely on wait_for_selector below to know when the actual
        # ranking rows are populated. Errors here are non-fatal — the
        # selector wait will still try to find rows.
        try:
            page.goto(URL, wait_until="domcontentloaded", timeout=45_000)
        except PWTimeout:
            print("  domcontentloaded timed out; continuing anyway", flush=True)

        # Forbes sometimes shows a privacy / cookie banner that covers the
        # list. Try to dismiss it. Non-fatal if absent.
        for label in ("Accept", "Accept All", "I Agree", "OK"):
            try:
                page.get_by_role("button", name=label).first.click(timeout=2000)
                print(f"  dismissed banner: {label}")
                break
            except PWTimeout:
                pass
            except Exception:
                pass

        # Find a working row selector before we start scrolling.
        used_selector = None
        for sel in ROW_SELECTORS:
            try:
                page.wait_for_selector(sel, timeout=8_000)
                if page.locator(sel).count() >= 5:
                    used_selector = sel
                    break
            except PWTimeout:
                continue

        if used_selector is None:
            OUT_HTML.write_text(page.content(), encoding="utf-8")
            sys.exit(
                f"Could not find any ranking rows. Page HTML dumped to:\n"
                f"  {OUT_HTML}\n"
                f"Update ROW_SELECTORS in this script after inspecting it."
            )
        print(f"  using row selector: {used_selector}", flush=True)

        # Forbes paginates 50 schools per page across 10 pages. We click
        # the "Next" button between pages and accumulate row content as
        # we go. Detect end-of-list by Next being disabled OR by reaching
        # MAX_PAGES as a safety net.
        out: List[dict] = []
        seen_keys = set()
        MAX_PAGES = 15

        def harvest_current_page() -> int:
            """Extract rows visible right now, append to `out`. Returns
            the count of new entries added (skipping duplicates)."""
            rows = page.locator(used_selector).all()
            added = 0
            for row in rows:
                try:
                    cells = [c.strip() for c in row.locator(CELL_SELECTOR)
                                                  .all_inner_texts()]
                except Exception:
                    continue
                if not cells:
                    txt = (row.inner_text() or "").strip()
                    if not txt:
                        continue
                    cells = [t.strip() for t in re.split(r"\n+", txt) if t.strip()]
                parsed = _parse_row(cells)
                if parsed is None:
                    continue
                key = (parsed["rank"], parsed["name"])
                if key in seen_keys:
                    continue
                seen_keys.add(key)
                out.append(parsed)
                added += 1
            return added

        for page_num in range(1, MAX_PAGES + 1):
            # Scroll the table into view in case lazy chunks need it
            try:
                page.locator(used_selector).first.scroll_into_view_if_needed(
                    timeout=2000)
            except Exception:
                pass
            page.wait_for_timeout(400)
            added = harvest_current_page()
            print(f"  page {page_num}: +{added} new (total {len(out)})",
                  flush=True)

            # Click "Next" to advance — bail out if it's missing or disabled.
            next_btn = page.locator('button[aria-label="Next"]').first
            try:
                if next_btn.count() == 0:
                    print("  no Next button found; stopping", flush=True)
                    break
                if next_btn.is_disabled():
                    print("  Next button disabled; reached last page",
                          flush=True)
                    break
                next_btn.scroll_into_view_if_needed(timeout=2000)
                next_btn.click(timeout=5000)
            except Exception as e:
                print(f"  Next click failed ({e}); stopping", flush=True)
                break

            # Wait for the table to repaint. Forbes' table rows are reused
            # (same DOM nodes, updated content), so a row-count diff
            # doesn't fire. Instead we wait for the first cell's text to
            # change vs. what we just saw.
            try:
                first_cell_now = page.locator(used_selector).nth(0)\
                                     .locator(CELL_SELECTOR).first\
                                     .inner_text(timeout=3000)
                # Poll for change with a 4s budget
                for _ in range(20):
                    page.wait_for_timeout(200)
                    new_first = page.locator(used_selector).nth(0)\
                                    .locator(CELL_SELECTOR).first\
                                    .inner_text(timeout=1000)
                    if new_first != first_cell_now:
                        break
            except Exception:
                page.wait_for_timeout(1500)

        OUT_HTML.write_text(page.content(), encoding="utf-8")
        print(f"  collected {len(out)} unique schools across pages",
              flush=True)
        browser.close()

    return sorted(out, key=lambda r: r["rank"])


def main() -> int:
    rows = scrape()
    if not rows:
        print("ERROR: no rows extracted. See", OUT_HTML)
        return 2

    with OUT_CSV.open("w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["rank", "name", "state", "type"])
        w.writeheader()
        w.writerows(rows)
    print(f"Wrote {OUT_CSV}: {len(rows)} schools")

    # Stat the rank range and a couple sanity hits
    ranks = [r["rank"] for r in rows]
    print(f"  rank range: {min(ranks)} ... {max(ranks)}")
    by_state = {}
    for r in rows:
        by_state[r["state"]] = by_state.get(r["state"], 0) + 1
    top_states = sorted(by_state.items(), key=lambda kv: -kv[1])[:5]
    print(f"  top states: {top_states}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
