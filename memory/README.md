# Agent Memory

Top-level home for the local-first agent memory feature.

## Files

- `cli.mjs` - operator CLI (`list`, `search`, `remember`, `checkpoint`, `doctor`, `profile`, `flush-remote`)
- `memory-mcp.mjs` - MCP server: search our prompt history across the fleet (`memory_search`, `memory_sessions`, `memory_machines`)
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
node memory/cli.mjs search "menu band rejection"   # this machine's history
node memory/codex-sync.mjs
node memory/install-hooks.mjs
```

## Searching the fleet

Each Mac's store is encrypted under a key that never leaves it, so there is no
central index to query. `memory-mcp.mjs` inverts that: it ships the QUERY to
each machine over ssh, each one decrypts and searches its own store, and the
(redacted) hits are merged here — so "when did we decide X" finds it whether it
was said on blueberry or neo, and no key crosses the network.

Registered in `.mcp.json` as **memory**, so any session gets `memory_search`,
`memory_sessions`, and `memory_machines` by name. The fleet list is the one
`frame`/`puppet` already keep at `~/.config/slab/puppet.json` — the Mac you're
on answers as `local` (labeled with its real hostname in results), and any
machine that's asleep is named in the output rather than silently skipped.

See `docs/AGENT-MEMORY-LOCAL-FIRST.md` for full setup and multi-machine workflow.
