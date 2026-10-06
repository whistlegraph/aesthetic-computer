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
is implicit in this terminal backend.

Whistlegraph's verified Jeffrey account uses a separate phone adapter with
Claude Opus 5. Its four permitted tools (`write_piece`, `edit_piece`,
`ac_preview`, `ac_api`) execute on the phone; built-in server tools are disabled.
The phone journals request IDs and tool results so reconnecting can recover
accepted work. Generation and four-frame visual review both use the relay.
Unsent chalk is saved atomically in the native app's local storage.

## Protocol

Every private request needs `Authorization: Bearer <AC access token>`.

- `POST /api/aesel/sessions`: `{provider:"claude"|"codex", model?, effort?, instructions?, clientTools?}`.
- `POST /api/aesel/sessions/:id/turn`: `{requestId:<UUID>, text, images?:[{mimeType,data:<base64>}]}`.
- `GET /api/aesel/sessions/:id?after=<sequence>`: events (up to 500), pending approvals, request statuses.
- `POST /api/aesel/sessions/:id/respond`: `{id, result}` matching an outstanding engine request.
- `POST /api/aesel/sessions/:id/interrupt`: `{}`.
- `POST /api/aesel/transcription-session`: a 60-second OpenAI client secret for
  `gpt-live-transcribe`, 24 kHz mono PCM, with manual commit. The phone streams
  microphone chunks immediately and displays deltas during recording.
- `POST /api/aesel/transcribe`: `{audio:<base64 mono 16 kHz PCM WAV>}`, at most
  46 seconds. Whisper returns final text and measured word timestamps after a
  performance or when on-device recognition has no words. Audio is transient
  here; the original recording remains on the phone.

Speech uses the separate `WHISTLEGRAPH_TRANSCRIPTION_KEY` server credential
and the OpenAI API, independently of personal Claude/Codex subscriptions.
The key is captured in the speech handlers and removed from the environment
before any provider CLI can inherit it. Neither API keys nor recordings are
stored in speech session logs. Both speech routes use the same verified-owner
gate. Live text is provisional; network arrival times are never used as word
timestamps. Validate with `node --test help/relay/transcription.test.mjs`.

Submit each attempt with a stable request UUID. Retries with the same UUID
return its status without executing again. The full input, including image
bytes, is written before acknowledgement. Events are appended as they arrive;
a service restart marks unfinished attempts interrupted and preserves input.
A new request UUID is an explicit retry. State lives under
`/var/lib/aesel-relay/sessions`, private to the service account. Nothing deletes
saved drawings automatically. Tool-free phone review (`clientTools: []`)
keeps image bytes transient and disables Claude session persistence. Its
request record retains only the image count. Native local draft storage is
independent of the relay.

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
and a real authenticated model turn. In the phone checkout, run
`node apple/whistlegraph/Tests/personal-relay-live.mjs` after bundling to check
real runtime generation and four-frame review without the messages API.


## Whistlegraph trials

`AESEL_RELAY_TRIALS_FILE` points to a mode-0640 root:aesel JSON file containing
`[{"sub":"verified Auth0 subject","expiresAt":"ISO-8601 UTC expiry"}]`.
Resolve subjects from the account database; handles supplied by a client are
never grants. Remove the entry to revoke immediately. Identity lookups cache
for one minute, but grants and expiry are checked on every request.

`GET /api/aesel/access` returns personal-model capabilities to a verified account.
Trial access permits only Claude Opus 5 / GPT-6 Astra with declared phone tools,
including an empty list for visual review. Sessions belong to the creating
subject; legacy sessions belong only to the owner. Trial access never permits
terminal tools or the unmetered speech endpoints. The service Codex config must
remain free of MCP servers; phone sessions do not inject AC/media MCPs.
Whisper speech uses Lith's separately metered `/api/whistlegraph-transcribe`.
