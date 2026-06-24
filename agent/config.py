"""Centralized config. Read once, validate, expose constants."""
from __future__ import annotations

import os

from dotenv import load_dotenv

load_dotenv()


def _required(name: str) -> str:
    v = os.getenv(name)
    if not v:
        raise RuntimeError(f"{name} not set. See .env.example.")
    return v


DB_DSN = os.getenv("DB_DSN", "")
AGENT_MODEL = os.getenv("AGENT_MODEL", "haiku")
API_TOKENS = {t for t in (os.getenv("API_TOKENS") or "").split(",") if t}

# Map the friendly AGENT_MODEL switch to a concrete LiteLLM model id, so
# swapping providers is an env change, not a code change. AGENT_MODEL may also
# be any raw LiteLLM model string, which falls through unchanged.
_MODEL_IDS = {
    "haiku":  "anthropic/claude-haiku-4-5-20251001",
    "gemini": "gemini/gemini-2.5-flash",
}
MODEL_ID = _MODEL_IDS.get(AGENT_MODEL, AGENT_MODEL)

# Optional - only required when the underlying provider is selected
ANTHROPIC_API_KEY = os.getenv("ANTHROPIC_API_KEY", "")
GEMINI_API_KEY = os.getenv("GEMINI_API_KEY", "")
