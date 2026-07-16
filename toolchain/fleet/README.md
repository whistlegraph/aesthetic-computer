# fleet

A single source of truth for **"what machines do I have access to, their
capabilities, designations, and live status."** Code is public-safe; the machine
DATA (IPs, ssh keys, roles) stays in the private vault.

Three moving parts:

1. **`machines.json`** (vault, canonical) → the source of truth @jeffrey edits.
2. **`normalize-machines.mjs`** → enriches it into the fleet schema (adds
   `designation`, `capabilities[]`, `status`, tailnet cross-refs) and writes
   `machines.normalized.json` to the vault *for review* — it never overwrites
   the canonical file.
3. **`fleet-mcp.mjs`** → a stdio MCP server that merges the (normalized) static
   registry with live `tailscale status`, so any agent can discover the fleet.

```
vault/machines.json  ──normalize-machines.mjs──▶  vault/machines.normalized.json
                                                          │
                          tailscale status --json  ──────▶ fleet-mcp.mjs  ──▶ agents
```

## Schema (proposed)

Each machine gains, on top of its existing fields:

| field | meaning |
|-------|---------|
| `name` | canonical registry key (often == tailnet short name) |
| `designation` | ONE primary fleet role (see below) |
| `capabilities[]` | composable tags: `gpu`, `cuda`, `mlx`, `unreal`, `docker`, `macos-automation`, `screen-capture`, `chromium-pool`, `ffmpeg-render`, `always-on`, `git-remote`, `mongodb`, `redis`, `mail`, `web-host`, `build-macos`, `build-ios`, `tailnet-api`, `art-display` |
| `tailscale` | `{ name, ip }` — magic-DNS short name for live-status matching |
| `status` | `{ source, key }` — how liveness resolves (`tailscale` / `http` / `lan` / `none`) |
| `hardware` | `{ model, chip, cores, memory, gpu }` |
| `fleetRole` | one-line human summary |

**Designations:** `agent-endpoint` (hosts a hermes gateway + identity) ·
`compute-node` (headless capability provider over HTTP MCP on the tailnet) ·
`control` (human/agent-driven author box, holds git auth) · `build` (build/CI) ·
`service` (db/session/mail/web) · `display` · `legacy`.

The full vocabulary with descriptions lives in the `_schema` block of the
normalized file and is queryable via the `fleet_designations` MCP tool.

## Usage

```bash
# regenerate the review file after editing the vault registry
node toolchain/fleet/normalize-machines.mjs

# CLI smoke test (same code the MCP runs)
node toolchain/fleet/fleet-mcp.mjs list
node toolchain/fleet/fleet-mcp.mjs find gpu
node toolchain/fleet/fleet-mcp.mjs machine poorslice
```

Data path is resolved in order: `$FLEET_MACHINES` → vault
`machines.normalized.json` → vault `machines.json`. Tailscale binary:
`$TAILSCALE_BIN` → `/Applications/Tailscale.app/...` → `tailscale` on PATH.

## Register as an MCP

**Claude Code** (`~/aesthetic-computer/.mcp.json`, alongside frame/puppet):

```json
"fleet": {
  "type": "stdio",
  "command": "node",
  "args": ["toolchain/fleet/fleet-mcp.mjs"]
}
```

**hermes** (`config.yaml`), same stdio contract:

```yaml
mcpServers:
  fleet:
    command: node
    args: ["toolchain/fleet/fleet-mcp.mjs"]
    # env:
    #   FLEET_MACHINES: /path/to/machines.normalized.json
```

## Dashboard design note

See `DASHBOARD.md` for the fleet/auth dashboard design + recommendation.
