"""
FastAPI agent skeleton.

STATUS: scaffold. This file is a stub. A real implementation should come from
the design-conversation `app.py`, or be written against:
  - litellm for model abstraction (Claude / Gemini / Haiku)
  - psycopg for the facts catalog
  - the catalog-driven tool layer in tools.py

The first cut should expose:
  POST /chat    { "message": "...", "session_id": "..." } -> { "reply": "..." }
  GET  /health
with bearer-token auth (env API_TOKENS, comma-separated).

TODO:
  - load_dotenv() and config.py
  - tool loop reading variables catalog -> dynamic tool list
  - per-token session state in Postgres or sqlite
  - rate limiting
"""
from fastapi import FastAPI, Header, HTTPException
import os

app = FastAPI(title="College Facts Agent (scaffold)")


def _check_token(authorization: str | None) -> None:
    valid = set((os.getenv("API_TOKENS") or "").split(","))
    valid.discard("")
    if not valid:
        return  # auth disabled in dev if API_TOKENS is empty
    if not authorization or not authorization.startswith("Bearer "):
        raise HTTPException(status_code=401, detail="missing bearer token")
    token = authorization.removeprefix("Bearer ").strip()
    if token not in valid:
        raise HTTPException(status_code=401, detail="invalid token")


@app.get("/health")
def health() -> dict:
    return {"status": "ok"}


@app.post("/chat")
def chat(payload: dict, authorization: str | None = Header(default=None)) -> dict:
    _check_token(authorization)
    return {
        "reply": "TODO: wire up the agent loop. See tools.py and CLAUDE.md.",
        "echo": payload,
    }
