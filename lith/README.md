# lith

Secrets and runtime env for the Aesthetic Computer monolith deploy.

`lith/deploy.fish` expects:

- `aesthetic-computer-vault/lith/.env`

That file is uploaded to:

- `/opt/ac/system/.env`

Why `system/.env` on the server:

- [`lith.service`](/workspaces/aesthetic-computer/lith/lith.service) uses `EnvironmentFile=/opt/ac/system/.env`
- The monolith serves the main site and API from the shared `system/` tree

Minimum required keys:

- `NODE_ENV=production`
- `CONTEXT=production`
- `DEPLOY_SECRET=...`

Optional product analytics keys:

- `POSTHOG_PROJECT_TOKEN=phc_...` — enables the privacy-minimized browser client
- `POSTHOG_API_HOST=https://us.i.posthog.com` — US or EU Cloud ingestion host
- `POSTHOG_SERVER_ENDPOINT_EVENTS=true` — separately enables anonymous endpoint aggregates
- `POSTHOG_OSKIEWAR_EVENTS=true` — separately enables minimized Oskiewar server milestones

See [`docs/POSTHOG.md`](../docs/POSTHOG.md) for the endpoint inventory, privacy
contract, event schemas, validation, and rollback.

Recommended workflow:

1. Copy `.env.example` to `.env`
2. Fill in the real production values
3. Re-run `fish vault-tool.fish status` to confirm `lith/.env` is tracked
4. Deploy with `fish /workspaces/aesthetic-computer/lith/deploy.fish`
