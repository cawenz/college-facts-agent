# agent/

FastAPI service that answers natural-language questions about college facts
by querying the Postgres catalog written by the ETL.

## Status

Scaffold. `app.py`, `tools.py`, and `config.py` are stubs - functional enough
to start the server and hit `/health`, but the model loop and full tool layer
are TODO. See top-of-file docstrings.

## Run

```bash
cd agent
python -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt
cp ../.env.example ../.env   # fill in
uvicorn app:app --reload --port 8000
```

```bash
curl localhost:8000/health
curl -X POST localhost:8000/chat \
  -H 'authorization: Bearer tester1-secret' \
  -H 'content-type: application/json' \
  -d '{"message": "what is Holy Cross 4yr graduation rate?"}'
```

## Where the data comes from

`tools.py` reads from the `facts`, `variables`, `schools`, and `value_labels`
tables - all populated by `python load/load_to_postgres.py` after the R ETL
writes CSVs under `output/`. See the root `CLAUDE.md` for the full picture.
