# Help host

The public AI helper retired on October 12, 2026. `/help` on Aesthetic Computer
opens the human chat for help from @jeffrey. The former `/api/help/chat` endpoint
returns HTTP 410 without authenticating or starting a model.

The DigitalOcean host remains in use:

- `help.service` runs `/opt/help/bridge/aa-bridge.mjs` on port 3004 for the
  authenticated `/aa` and whitelisted `/pp` APIs.
- `aesel-relay.service` runs the private provider relay on loopback port 3006.
  Its saved sessions stay under `/var/lib/aesel-relay/sessions`.
- A MacBook reverse tunnel listens on loopback port 3005. The current Caddy
  routes use 3004 and 3006; they do not use that tunnel.

[`Caddyfile`](Caddyfile) is the host's routing configuration. Validate it with
`caddy validate --config /etc/caddy/Caddyfile --adapter caddyfile` before reload.
The private Aesel relay has its own [deployment instructions](relay/README.md).

To update the bridge, upload `bridge/aa-bridge.mjs` to
`/opt/help/bridge/aa-bridge.mjs`, run `node --check` there, check for active child
processes, and restart only `help.service`. Do not stop the Aesel relay when
retiring or changing public help routes. The old `deploy.fish` installs the
historical port-3004 proxy and is not the running bridge's deployment command.
