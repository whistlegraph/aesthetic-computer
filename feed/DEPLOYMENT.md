# Feed Server Deployment

## Current: dp1-feed-v2 (Go + Postgres)

As of April 2026, `feed.aesthetic.computer` runs **dp1-feed-v2** — a Go binary
backed by PostgreSQL 16, hosted on the lith VPS (`209.38.133.33`).

- **Source**: https://github.com/display-protocol/dp1-feed-v2
- **Binary**: `/opt/dp1-feed/dp1-feed`
- **Config**: `/opt/dp1-feed/config.yaml` (source of truth: `lith/dp1-feed-config.yaml`)
- **Env**: `/opt/dp1-feed/.env` (API key, signing key, database URL)
- **Service**: `systemctl {start,stop,restart,status} dp1-feed`
- **Logs**: `journalctl -u dp1-feed -f`
- **Port**: 8787 (reverse proxied via Caddy)
- **Database**: PostgreSQL `dp1_feed` on localhost, user `dp1feed`

## Rebuilding

```bash
ssh root@lith.aesthetic.computer
cd /opt/dp1-feed-build/dp1-feed-v2
git pull
CGO_ENABLED=0 go build -o dp1-feed ./cmd/server
cp dp1-feed /opt/dp1-feed/dp1-feed
systemctl restart dp1-feed
```

## Initial Setup

Run `lith/scripts/setup-dp1-feed.sh` on the VPS. It installs Go, PostgreSQL,
clones the repos, builds the binary, and creates the systemd service.

## API Endpoints

- `GET /health` — liveness
- `GET /api/v1` — API info
- `GET /api/v1/playlists` — list playlists
- `GET /api/v1/channels` — list channels (requires extensions enabled)
- `GET /api/v1/playlist-groups` — list playlist groups
- `GET /api/v1/playlist-items` — list playlist items
- `PUT /api/v1/registry/channels` — update curated channel registry

Auth: `Authorization: Bearer <API_KEY>` for all write operations.

## Legacy (archived)

The previous V1 deployment used Cloudflare Workers (TypeScript/Hono) with KV
storage. That code lives in `feed/dp1-feed/` and is no longer deployed.
