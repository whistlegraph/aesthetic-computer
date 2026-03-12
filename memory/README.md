# Agent Memory

Top-level home for the local-first agent memory feature.

## Files

- `cli.mjs` - operator CLI (`list`, `remember`, `checkpoint`, `doctor`, `profile`, `flush-remote`)
- `hook.mjs` - Claude/Codex hook entrypoint for prompt logging
- `codex-sync.mjs` - imports recent Codex session messages from `~/.codex/sessions`
- `install-hooks.mjs` - configures git to use `.githooks` and enables memory hooks
- `store.mjs` - encrypted local store + session/checkpoint operations
- `crypto.mjs` - AES-256-GCM key management and encrypt/decrypt helpers
- `redact.mjs` - best-effort secret redaction for previews/summaries
- `remote.mjs` - optional outbound queue + remote flush adapter
- `index.mjs` - public exports

## Quick start

```bash
node memory/cli.mjs doctor
node memory/cli.mjs list
node memory/codex-sync.mjs
node memory/install-hooks.mjs
```

See `docs/AGENT-MEMORY-LOCAL-FIRST.md` for full setup and multi-machine workflow.
