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

### Easel inference configuration

Production loads `OPENROUTER_API_KEY` from
`/etc/aesthetic-computer/easel-inference.env` through
`/etc/systemd/system/lith.service.d/40-easel-inference.conf`. This separate,
root-only environment survives deploys that replace `/opt/ac/system/.env` with
an older fleet copy. Keep it synchronized when rotating the key in the vault's
`lith/.env`, then restart `lith`. The vault `lith/.env.keys` manifest also requires
this key so deployment validation rejects incomplete environments.

### Image generation

`/api/flux` uses Cloudflare Workers AI FLUX Schnell at four steps and
1024×1024. Set `CLOUDFLARE_ACCOUNT_ID` and a scoped `CLOUDFLARE_AI_TOKEN`
in the canonical Lith env. `IMAGE_MONTHLY_BUDGET_USD` defaults to 5;
`IMAGE_DAILY_BUDGET_USD` defaults to 0.5. Set either to 0 to stop inference.

Mongo collection `image-generation-budget` reserves 634 micro-USD per
attempt before the provider request, including failed attempts. Both caps
are shared across processes and survive restarts, resetting at UTC month/day
boundaries. Database failure stops generation. There are no automatic paid
retries or premium fallbacks. The Cloudflare free quota can stop requests
earlier; free-quota exhaustion returns 429 until midnight UTC.

The reservation rounds up the September 2026 four-step image rate of
$0.0006336. Revisit it when changing the model, dimensions, steps, or
[Cloudflare pricing](https://developers.cloudflare.com/workers-ai/platform/pricing/).
Caps cover this endpoint's inference, excluding account plan fees and other
Cloudflare workloads.
