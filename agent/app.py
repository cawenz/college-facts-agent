"""
FastAPI agent: the model loop over the catalog tool layer (tools.py).

app.py owns the conversation with the model; it never touches the DB directly.
The model picks tools + arguments, we execute them via tools.py and feed the
results back, until the model produces a final grounded answer.

Built incrementally:
  [x] tool schemas + dispatch + system prompt   (model-facing plumbing)
  [x] run_turn(): the tool-calling loop
  [x] /chat wiring + bearer auth
  [ ] uvicorn run + end-to-end curl
"""
from __future__ import annotations

import json
from pathlib import Path

import litellm
from fastapi import FastAPI, Header, HTTPException
from fastapi.responses import HTMLResponse

import config
import tools

app = FastAPI(title="College Facts Agent")

_STATIC = Path(__file__).resolve().parent / "static"


# --- Tool surface exposed to the model (OpenAI-style function schemas) --------
# Descriptions are load-bearing: they are how the model decides which tool to
# call and in what order. Keep them tight and action-oriented.
TOOL_SCHEMAS = [
    {
        "type": "function",
        "function": {
            "name": "search_metrics",
            "description": (
                "Search the catalog of answerable metrics by keyword. Use this "
                "to discover which metrics exist and their EXACT names before "
                "calling get_facts. Returns metric, display_name, category, "
                "source, format, and any coverage caveat."
            ),
            "parameters": {
                "type": "object",
                "properties": {
                    "query": {
                        "type": "string",
                        "description": "Keyword(s) to match against metric names/descriptions, e.g. 'graduation', 'net price'.",
                    },
                    "limit": {"type": "integer", "description": "Max results (default 25)."},
                },
                "required": ["query"],
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "resolve_school",
            "description": (
                "Resolve a school name to candidate unitid(s), best match first. "
                "ALWAYS call this to get a unitid before get_facts or get_school. "
                "Returns a ranked candidate list with a match quality; if several "
                "are plausible, use context or ask the user which one. 'Holy Cross' "
                "resolves to College of the Holy Cross."
            ),
            "parameters": {
                "type": "object",
                "properties": {
                    "name": {"type": "string", "description": "Free-text school name."},
                    "state": {"type": "string", "description": "Optional 2-letter postal code to narrow, e.g. 'MA'."},
                    "limit": {"type": "integer", "description": "Max candidates (default 10)."},
                },
                "required": ["name"],
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "get_facts",
            "description": (
                "Fetch grounded fact values for ONE school across one or more "
                "metrics. Metric names are validated against the catalog. Returns "
                "three buckets: 'facts' (each with source/year/format/coverage_note), "
                "'no_data' (valid metric but no value for this school -> say it is "
                "not available), and 'unknown_metrics' (not in the catalog -> find "
                "the right name with search_metrics). year=null gives the latest "
                "year per metric; set all_years=true for the full time series."
            ),
            "parameters": {
                "type": "object",
                "properties": {
                    "unitid": {"type": "integer", "description": "From resolve_school."},
                    "metrics": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Exact metric names (from search_metrics/the catalog).",
                    },
                    "year": {"type": "integer", "description": "Specific collection year; omit for the latest."},
                    "all_years": {"type": "boolean", "description": "True to return the full time series."},
                },
                "required": ["unitid", "metrics"],
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "get_school",
            "description": (
                "Fetch one school's NON-fact attributes: identity, classification "
                "(control, Carnegie, religious affiliation, accreditor), rankings "
                "(US News / Washington Monthly / Forbes, each with source + edition "
                "year), and athletics (NCAA division, conference, football). "
                "Rankings live HERE, not in get_facts."
            ),
            "parameters": {
                "type": "object",
                "properties": {
                    "unitid": {"type": "integer", "description": "From resolve_school."},
                },
                "required": ["unitid"],
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "decode",
            "description": (
                "Resolve a categorical IPEDS/Carnegie code to its label via "
                "value_labels (e.g. variable='SECTOR', code='2'). Omit code to "
                "enumerate all options for a variable. Mostly a fallback -- "
                "get_school already returns decoded labels."
            ),
            "parameters": {
                "type": "object",
                "properties": {
                    "variable": {"type": "string", "description": "Field name, e.g. 'SECTOR', 'BASIC2021'."},
                    "code": {"type": "string", "description": "The code to decode; omit to list all options."},
                    "table": {"type": "string", "description": "Optional table_name to disambiguate a variable that spans tables."},
                },
                "required": ["variable"],
            },
        },
    },
]

# name -> callable. Every schema name must have a dispatch entry (and vice versa).
DISPATCH = {
    "search_metrics": tools.search_metrics,
    "resolve_school": tools.resolve_school,
    "get_facts": tools.get_facts,
    "get_school": tools.get_school,
    "decode": tools.decode,
}

assert {s["function"]["name"] for s in TOOL_SCHEMAS} == set(DISPATCH), (
    "TOOL_SCHEMAS and DISPATCH must cover exactly the same tools"
)


SYSTEM_PROMPT = """
You are a precise factual assistant answering questions about U.S. colleges and
universities from a local, curated catalog. You may ONLY use the tools provided;
never use outside knowledge for any figure.

Workflow:
- Turn a school name into a unitid with resolve_school before any other lookup.
  If several candidates are plausible, pick using context or ask the user.
  ("Holy Cross" means College of the Holy Cross.)
- Find exact metric names with search_metrics; fetch numbers with get_facts;
  fetch attributes and rankings with get_school.

Grounding rules (non-negotiable):
- Every figure you report MUST carry its data year and source, e.g.
  "57% (IPEDS, 2024)" or "#27, U.S. News 2026 edition".
- If a metric is in 'no_data', or a tool returns nothing, say the data is not
  available. NEVER estimate, infer, or guess a number.
- Always surface 'coverage_note' caveats (e.g. survey-based ~45% coverage).
- If get_facts returns 'unknown_metrics', correct the name via search_metrics.
- The catalog covers public and private-nonprofit 4-year institutions only;
  it has nothing to say about community colleges or for-profits.
""".strip()


# --- Auth (bearer token) ------------------------------------------------------
def _check_token(authorization: str | None) -> None:
    valid = config.API_TOKENS
    if not valid:
        return  # auth disabled in dev if API_TOKENS is empty
    if not authorization or not authorization.startswith("Bearer "):
        raise HTTPException(status_code=401, detail="missing bearer token")
    token = authorization.removeprefix("Bearer ").strip()
    if token not in valid:
        raise HTTPException(status_code=401, detail="invalid token")


# --- The tool-calling loop ----------------------------------------------------
# Cap on model<->tool round-trips per question, so a confused model can't loop
# forever (each iteration is one model call plus any tools it requests).
_MAX_TOOL_ITERS = 6


def run_turn(message: str) -> dict:
    """Answer one question: drive the model<->tool loop to a grounded reply.

    Returns {"reply": str, "trace": [{"tool", "args"}, ...]} where trace lists
    the tools the model invoked (handy for debugging and transparency).
    """
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": message},
    ]
    trace: list[dict] = []

    for _ in range(_MAX_TOOL_ITERS):
        resp = litellm.completion(
            model=config.MODEL_ID,
            messages=messages,
            tools=TOOL_SCHEMAS,
            temperature=0,
        )
        choice = resp.choices[0].message
        tool_calls = choice.tool_calls or []

        if not tool_calls:
            return {"reply": choice.content or "", "trace": trace}

        # Record the assistant's tool-call turn explicitly (provider-agnostic).
        messages.append({
            "role": "assistant",
            "content": choice.content,
            "tool_calls": [
                {
                    "id": tc.id,
                    "type": "function",
                    "function": {"name": tc.function.name, "arguments": tc.function.arguments},
                }
                for tc in tool_calls
            ],
        })

        # Execute each requested tool and feed the result back.
        for tc in tool_calls:
            name = tc.function.name
            try:
                args = json.loads(tc.function.arguments or "{}")
            except json.JSONDecodeError:
                args = {}
            fn = DISPATCH.get(name)
            if fn is None:
                result = {"error": f"unknown tool {name!r}"}
            else:
                try:
                    result = fn(**args)
                except Exception as e:  # surface the error to the model, don't crash
                    result = {"error": f"{type(e).__name__}: {e}"}
            trace.append({"tool": name, "args": args})
            messages.append({
                "role": "tool",
                "tool_call_id": tc.id,
                # default=str handles Decimal (Postgres NUMERIC) -> JSON.
                "content": json.dumps(result, default=str),
            })

    return {
        "reply": "I wasn't able to finish that within the tool-call limit.",
        "trace": trace,
    }


# --- Endpoints ----------------------------------------------------------------
@app.get("/", response_class=HTMLResponse)
def index() -> str:
    # Read fresh each request so the page can be edited without a server restart.
    return (_STATIC / "index.html").read_text(encoding="utf-8")


@app.get("/health")
def health() -> dict:
    return {"status": "ok", "model": config.MODEL_ID}


@app.post("/chat")
def chat(payload: dict, authorization: str | None = Header(default=None)) -> dict:
    _check_token(authorization)
    message = (payload or {}).get("message")
    if not message or not str(message).strip():
        raise HTTPException(status_code=400, detail="missing 'message'")
    return run_turn(str(message))
