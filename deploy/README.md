# deploy/

Always-on hosting for the agent via systemd.

`college-facts-agent.service` runs `uvicorn app:app` from the repo's `.venv`,
reading secrets from `.env`. It auto-starts on boot and restarts on failure.

## Install (one time)

```bash
sudo cp deploy/college-facts-agent.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now college-facts-agent
systemctl status college-facts-agent --no-pager
```

The agent then serves on `http://<server-ip>:8000/` (and `:8000/health`).

## Operate

```bash
journalctl -u college-facts-agent -f          # tail logs
sudo systemctl restart college-facts-agent     # after a code or .env change
sudo systemctl stop college-facts-agent        # stop
```

## Notes

- Edit `.env` then `restart` to pick up new keys / `AGENT_MODEL`.
- The unit binds `0.0.0.0:8000` (LAN-reachable, gated by the bearer token in
  `API_TOKENS`). For internet exposure, front it with HTTPS + a reverse proxy
  and switch the bind to `127.0.0.1`.
- Optional LAN-only firewall: `sudo ufw allow from 192.168.0.0/16 to any port 8000`.
