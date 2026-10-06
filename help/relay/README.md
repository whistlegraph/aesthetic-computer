# Personal Aesel relay

`help.aesthetic.computer/api/aesel` runs the same ClaudeServer and AppServer
adapters as Aesel. The verified `ADMIN_SUB` alone can connect, using their AC
access token. Provider credentials stay on the server. Existing help routes
continue on port 3004; Caddy routes `/api/aesel/*` to loopback port 3006.

From the terminal, with `ac login` already complete:

```sh
aes --backend relay --model claude-sonnet-4-6
# Once Codex is authenticated on the VPS:
AESEL_RELAY_PROVIDER=codex aes --backend relay --model gpt-6-astra
```

Work runs in a separate directory on the VPS, not the client's local checkout.
Tool approvals return to the terminal. Closing the client detaches without
cancelling the turn. `--resume <relay-session-uuid>` reconnects; the relay replays
its saved events and only unresolved approvals. No filesystem synchronization
or phone integration is implicit in this backend.

## Protocol

Every private request needs `Authorization: Bearer <AC access token>`.

- `POST /api/aesel/sessions`: `{provider:"claude"|"codex", model?, effort?, instructions?}`.
- `POST /api/aesel/sessions/:id/turn`: `{requestId:<UUID>, text, images?:[{mimeType,data:<base64>}]}`.
- `GET /api/aesel/sessions/:id?after=<sequence>`: events (up to 500), pending approvals, request statuses.
- `POST /api/aesel/sessions/:id/respond`: `{id, result}` matching an outstanding engine request.
- `POST /api/aesel/sessions/:id/interrupt`: `{}`.

Submit each attempt with a stable request UUID. Retries with the same UUID
return its status without executing again. The full input, including image
bytes, is written before acknowledgement. Events are appended as they arrive;
a service restart marks unfinished attempts interrupted and preserves input.
A new request UUID is an explicit retry. State lives under
`/var/lib/aesel-relay/sessions`, private to the service account. Nothing deletes
saved drawings automatically. This does not save an unsent phone draft.

## Host setup

Use a dedicated `aesel` system account with home `/var/lib/aesel-relay`.
Install provider CLIs under `/opt/aesel-relay/runtime` (verified versions:
Claude Code 2.1.289 and Codex 0.160.0). The relay unit runs as that account,
with a read-only system and private temporary directory.

Root-owned `/etc/aesel-relay.env` (0600) supplies `ADMIN_SUB`, `AUTH0_DOMAIN`
and `CLAUDE_CODE_OAUTH_TOKEN`. `claude setup-token` supplies Claude's credential;
do not put it in source, a client bundle, logs or chat. The existing help
subscription token can be reused. API-key environment variables are removed
before starting adapters, preventing implicit API billing fallback.

Codex uses its own login in the service account's home:

```sh
sudo -u aesel HOME=/var/lib/aesel-relay \
  /opt/aesel-relay/runtime/node_modules/.bin/codex login --device-auth
```

After login, set `CODEX_ENABLED=1` in the private environment file and restart
the relay. Do not copy credentials from a live prompt's process. Subscription
limits still apply. Claude's `costUSD` with `costBasis:"list"` is a list-price
usage estimate, not proof of an extra charge to the subscription.

`help/relay/deploy.sh` installs the source and systemd unit. Set `HELP_SSH_KEY`
when deploying from an isolated checkout. Caddy configuration:

```caddy
handle /api/aesel/* {
    reverse_proxy 127.0.0.1:3006
}
handle {
    reverse_proxy localhost:3004
}
```

Validate with `node --test help/relay/service.test.mjs` and the existing Aesel
provider adapter tests. Production checks must include unauthorized rejection
and a real authenticated model turn. Whistlegraph still uses the messages API;
it needs a client adapter for this session protocol before switching traffic.
