# Agent Memory (Local-First)

This system provides fast local working memory with optional remote sync.

## Goals

- Keep working-memory reads local and fast.
- Avoid plaintext remote transcript storage.
- Support continuity across laptops with `remember` lineage.
- Keep Docker/devcontainer rebuilds smooth (no new npm deps).

## Local Store

Default location:

- `~/.ac-agent-memory`

Override:

- `AGENT_MEMORY_HOME=/some/path`

Layout:

- `sessions/*.json` - session metadata
- `events/*.ndjson` - encrypted append-only events
- `checkpoints/*.ndjson` - redacted checkpoints
- `queue/outbound.ndjson` - remote sync queue (optional)
- `device-id` - stable local device id
- `key.bin` - local AES key (if `AGENT_MEMORY_KEY` is not provided)

## Security

- Event payloads are encrypted locally with AES-256-GCM.
- Redaction runs for previews/summaries before indexing/sync.
- Raw plaintext is not written to remote by default.

Optional key management:

- `AGENT_MEMORY_KEY=base64:<32-byte-key>`
- or `AGENT_MEMORY_KEY=<hex64>`

If unset, a local key file is generated automatically.

## CLI

```bash
# List sessions
node memory/cli.mjs list

# Log event (from JSON stdin)
echo '{"provider":"codex","role":"user","text":"hello"}' | node memory/cli.mjs event --session my-session

# Manual checkpoint
node memory/cli.mjs checkpoint --session my-session --reason manual

# Continue by lineage (new session seeded from checkpoint)
node memory/cli.mjs remember --from my-session

# Health/status
node memory/cli.mjs doctor

# Print reproducible env profile (for other machines)
node memory/cli.mjs profile --include-key --exports
```

NPM shortcuts:

- `npm run agent-memory:doctor`
- `npm run agent-memory:profile`
- `npm run agent-memory:set-device -- --id thinkpad-x1-main`
- `npm run agent-memory:sync-codex`
- `npm run agent-memory:install-hooks`

## Hook Integration

- `.claude/settings.json` already calls `memory/hook.mjs` on `UserPromptSubmit`.
- `memory/hook.mjs` writes each hook event to local encrypted memory.
- `.githooks/post-commit` logs a commit event, imports recent Codex prompts, and flushes remote queue.
- Install repo hooks once with:
`npm run agent-memory:install-hooks`

## Multi-Vendor + Multi-Machine

For Codex + Claude side-by-side across 2-3 machines, use shared key + per-machine device id:

```bash
# Shared across all your machines (same value everywhere)
export AGENT_MEMORY_KEY="base64:<32-byte-key>"

# Unique per machine/container
export AGENT_DEVICE_ID="thinkpad-x1-main"

# Optional defaults per terminal/session
export AGENT_MEMORY_PROJECT="aesthetic-computer"
export AGENT_MEMORY_PROVIDER="claude"   # or codex
export AGENT_MEMORY_TICKET="ticket-4821"
```

Recommended session id convention when multiple tickets are active:

```text
<ticket>-<provider>-<device>-<short-ts>
```

Examples:

- `ticket-4821-claude-thinkpadx1-20260312t1940`
- `ticket-4821-codex-thinkpadp1-20260312t1942`

Side-by-side quick commands:

```bash
# Claude terminal
AGENT_MEMORY_PROVIDER=claude AGENT_SESSION_ID=ticket-4821-claude-thinkpadx1-20260312t1940 node memory/cli.mjs event --text "start"

# Codex terminal
AGENT_MEMORY_PROVIDER=codex AGENT_SESSION_ID=ticket-4821-codex-thinkpadp1-20260312t1942 node memory/cli.mjs event --text "start"
```

Reproducible setup flow (recommended):

1. On your primary machine, generate and print a portable profile:
`node memory/cli.mjs profile --include-key --exports`
2. Copy those exports into your private machine env (`.devcontainer/envs/devcontainer.env` or shell profile).
3. On each machine, set a unique device id:
`npm run agent-memory:set-device -- --id <unique-machine-id>`
4. Validate with:
`npm run agent-memory:doctor`

Template for Docker/devcontainer env wiring:

- `.devcontainer/envs/agent-memory.env.example`
- `.devcontainer/devcontainer.json` now forwards `AGENT_MEMORY_*` vars from host `localEnv`

## Optional Remote Sync

Remote sync is disabled by default.

Enable queue + flush:

```bash
export AGENT_MEMORY_REMOTE_ENABLED=true
export AGENT_MEMORY_REMOTE_URL="https://aesthetic.computer/api/agent-memory-ingest"
export AGENT_MEMORY_REMOTE_TOKEN="<token>"
```

Flush queued records:

```bash
node memory/cli.mjs flush-remote
```

Remote ingest endpoint:

- `system/netlify/functions/agent-memory-ingest.mjs`

Recommended for siphoned database setup:

- `AGENT_MEMORY_MONGODB_CONNECTION_STRING`
- `AGENT_MEMORY_MONGODB_NAME`
- `AGENT_MEMORY_INGEST_TOKEN` (or reuse `AGENT_MEMORY_REMOTE_TOKEN`)

## Docker / Rebuild Notes

- Uses built-in Node modules only (`crypto`, `fs`, etc.).
- No package-lock changes required.
- No Dockerfile changes required for basic local-first mode.
