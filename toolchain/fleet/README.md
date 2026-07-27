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

The compute path adds two intentionally narrow pieces:

4. **`worker.mjs`** → a bearer-authenticated HTTP worker bound only to the
   host's Tailscale address. It accepts typed FFmpeg jobs, never shell commands.
5. **`submit.mjs`** → hashes and uploads inputs, selects a worker with guard
   headroom, follows progress, then downloads and verifies the artifact hash.

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

On fleet Macs, `fleet_cleaner` resolves “call the cleaner” to the safe local
disk cleanup in `toolchain/macos/cleaner.sh`. It applies known-regenerable cache
cleanup by default, offers report-only mode, and never thins APFS snapshots
unless that separate option is explicitly requested. The report inventories
the broader AC/media/developer storage surfaces; verified DigitalOcean-backed
media pruning is another explicit option and is not part of the weekly run.

## Usage

```bash
# regenerate the review file after editing the vault registry
node toolchain/fleet/normalize-machines.mjs

# CLI smoke test (same code the MCP runs)
node toolchain/fleet/fleet-mcp.mjs list
node toolchain/fleet/fleet-mcp.mjs find gpu
node toolchain/fleet/fleet-mcp.mjs machine poorslice
```

## Fleet rendering

Install a worker on the current Mac:

```bash
toolchain/macos/performance-guard.sh --install
toolchain/fleet/install-worker.sh --name neo --role interactive
```

Provision a reachable fleet Mac with the same private token and its own guard:

```bash
toolchain/fleet/deploy-worker.sh blueberry blueberry light /Users/jas/aesthetic-computer
toolchain/fleet/deploy-worker.sh chicken chicken balanced
toolchain/fleet/deploy-worker.sh panda panda balanced
toolchain/fleet/deploy-worker.sh poorslice poorslice heavy /Users/aesthetic/aesthetic-computer
```

Deploy the persistent stat-TV build without compiling on the target:

```bash
slab/menubar-swift/deploy-host.sh blueberry
```

If PoorSlice is asleep, `toolchain/fleet/queue-poorslice-install.sh` installs a
small two-minute launchd retry. It completes both worker and stat-TV deployment
when PoorSlice next joins the tailnet and leaves a local success marker.

Submit a render. Arguments use named input and output placeholders, so no
arbitrary filesystem path or remote shell is exposed:

```bash
npm run fleet:ffmpeg -- \
  --input src=/path/source.mov \
  --output /path/result.mp4 \
  --duration 30 -- \
  -i @input:src -c:v libx264 -c:a aac @output
```

Selection prefers PoorSlice, then the 16GB Chicken/Panda build nodes, then
Blueberry, and admits Neo only while its interactive guard has headroom. Use
`--host NAME` to request one worker. Each worker runs one FFmpeg process group
at utility QoS, supports status and cancellation, and refuses new jobs during
guard pressure. Inputs are cached by SHA-256; returned artifacts are verified
before their final local rename.

Worker state lives in `~/.local/share/ac-fleet-worker/`; the shared bearer token
lives in `~/.config/ac-fleet-worker/token` with mode 0600. The service listens
on tailnet port 5263, not LAN or public interfaces.

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
