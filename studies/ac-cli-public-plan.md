# Study: Public `kidlisp` CLI (npm + Deno)

**Date:** 2026-03-09  
**Context:** Aesthetic Computer already has a strong internal command surface in `.devcontainer/config.fish`, plus public Netlify APIs (`/api/store-kidlisp`, `/api/store-clock`, `/api/store-piece`, `/api/tv`, `/api/api-docs`). This study proposes a public, pipe-friendly CLI anyone can install from npm and Deno, while keeping internal-only operations separate.

---

## Short Answer

Yes, this is very feasible, and we can do it with low risk by:
- Building one public `kidlisp` CLI package first with clean JSON/NDJSON output.
- Reusing existing API/auth patterns (`tezos/ac-login.mjs`, `system/netlify/functions/api-docs.mjs`).
- Mapping internal fish commands into public-safe equivalents.
- Keeping infrastructure/devcontainer-specific commands in an optional internal plugin.

## Naming Decision

Use `kidlisp` as the primary CLI binary name.

Validation snapshot (2026-03-09):
- local command: free
- npm package: free
- PyPI name: free
- crates.io name: free
- Homebrew formula: free

---

## Product Goal

Ship a public `kidlisp` CLI that can:
- Be installed from npm and usable from Deno.
- Work well in pipelines (`stdout` machine-readable, `stderr` errors).
- Publish, fetch, and inspect KidLisp, clock, and piece data via AC APIs.
- Support "run/preview" workflows for clocks and KidLisp.
- Expose a stable command grammar that can grow over time.

## Brand + Expansion Strategy

Use `kidlisp` as the sticky, memorable front door, then expand it into broader AC workflows over time.

Practical shape:
- Keep the root command short and iconic: `kidlisp`.
- Start with core creative flows (`publish`, `get`, `recent`, `validate`, `tree`).
- Add AC platform capabilities under clear namespaces as growth happens:
  - `kidlisp clock ...`
  - `kidlisp piece ...`
  - `kidlisp tv ...`
  - `kidlisp auth ...`
  - `kidlisp api ...`
- Keep advanced/internal operations as optional plugins so the main CLI stays clean.

## Tooling + Help Experience

The single `kidlisp` binary should feel friendly for first-time users and fast for shell users.

Core UX rules:
- `kidlisp` (no args) shows a short command dashboard with the top 6 workflows.
- `kidlisp --help` stays concise and example-driven.
- `kidlisp help <command>` prints focused help + copy-paste examples.
- Every command supports `--json` and prints pipe-safe output.
- Errors always include a direct next command to fix the issue.

Recommended starter commands:
- `kidlisp quickstart` (install check + first publish flow)
- `kidlisp doctor` (token/network/env diagnostics)
- `kidlisp examples` (small runnable snippets)
- `kidlisp completion <bash|zsh|fish>` (shell completion scripts)

---

## Non-Goals (Phase 1)

- Porting every internal fish helper 1:1.
- Bundling privileged internal ops (host SSH, emacs daemon control, private infra tools).
- Replacing MCP (`@aesthetic.computer/mcp`). CLI and MCP should share core API code.

---

## Existing Building Blocks

- Internal command inventory and behavior:
  - `.devcontainer/config.fish`
- Existing auth flow and token cache:
  - `tezos/ac-login.mjs` (`~/.ac-token`)
- Public API docs endpoint:
  - `system/netlify/functions/api-docs.mjs`
- Core publish/read endpoints:
  - `system/netlify/functions/store-kidlisp.mjs`
  - `system/netlify/functions/store-clock.mjs`
  - `system/netlify/functions/store-piece.mjs`
- Existing npm package + API wrapper pattern:
  - `mcp-server/`

---

## Proposed Public Command Surface (v0)

### Auth
- `kidlisp auth login`
- `kidlisp auth status`
- `kidlisp auth token`
- `kidlisp auth logout`

### API Discovery + Raw Access
- `kidlisp api docs [--json]`
- `kidlisp api list [--json]`
- `kidlisp api call <path> [--method GET|POST] [--data ...]`

### KidLisp
- `kidlisp publish [--file path | --stdin | --source "..."]`
- `kidlisp get <code>`
- `kidlisp recent [--limit N] [--sort recent|hits] [--handle @x]`
- `kidlisp stats functions [--limit N]`
- `kidlisp tree <code|@handle> [--source] [--json]` (from `kidlisp/tools/source-tree.mjs`)
- `kidlisp validate [--file path | --stdin | --source "..."]`

### Clock
- `kidlisp clock publish [--file path | --stdin | --source "..."]`
- `kidlisp clock get <code>`
- `kidlisp clock recent [--limit N] [--sort recent|hits]`
- `kidlisp clock now`

### Piece
- `kidlisp piece publish [--file path | --stdin | --source "..."] [--name name]`

### TV / Playlists / Inspect
- `kidlisp tv [--types kidlisp,clock,piece] [--limit N] [--format dp1|json]`
- `kidlisp inspect endpoint <name-or-path>`
- `kidlisp inspect handle <@handle>`
- `kidlisp inspect piece <code>`

### Pipe Utility
- `kidlisp pipe build-stream` (stdin passthrough + POST, based on internal `ac-pipe`)

---

## Pipe-First Contract

The CLI should be deterministic in shell pipelines:
- If output is piped or `--json/--ndjson` is set: machine format only on `stdout`.
- Human messages and progress always on `stderr`.
- Standard exit codes:
  - `0` success
  - `2` usage/validation
  - `3` auth required/expired
  - `4` remote API error
  - `5` network/timeout

Recommended output flags:
- `--json` single JSON object
- `--ndjson` one JSON object per line
- `--raw` emit only the requested field/body
- `--quiet` suppress non-essential stderr

---

## Internal Fish -> Public CLI Mapping

| Internal command | Public CLI target | Notes |
|---|---|---|
| `ac-login`, `ac-token` | `kidlisp auth ...` | Reuse OAuth/token file pattern |
| `st` / `ac-st` | `kidlisp tree` | Add clean JSON mode for pipelines |
| `ac-tv` | `kidlisp tv` | Keep query passthrough |
| `ac-pipe` | `kidlisp pipe build-stream` | Same stdin streaming idea |
| `ac-kidlisp` | `kidlisp ...` | Public subset only |
| `ac-ff1` | `kidlisp device ...` (optional plugin) | Keep out of core v0 |
| `ac-function` | `kidlisp logs ...` (internal plugin) | Requires netlify/dev setup |
| `ac-record` | `kidlisp render ...` (phase 2) | Wrap recording orchestrator |

---

## Runtime + Distribution Plan

### Phase 1 distribution
- Publish npm package: `kidlisp`
- Binary: `kidlisp`
- Install/use:
  - `npm i -g kidlisp`
  - `npx kidlisp ...`
  - `deno run -A npm:kidlisp ...`
  - `deno install -gA -n kidlisp npm:kidlisp`

### Phase 2 distribution (optional but recommended)
- Publish a thin JSR package/wrapper for best Deno UX:
  - `jsr:@aesthetic-computer/kidlisp`

---

## Proposed Repository Layout

```text
kidlisp-cli/
  package.json
  README.md
  src/
    bin.ts
    core/
      config.ts
      auth.ts
      http.ts
      output.ts
      errors.ts
    commands/
      auth.ts
      api.ts
      kidlisp.ts
      clock.ts
      piece.ts
      tv.ts
      inspect.ts
      pipe.ts
  tests/
    cli.spec.ts
    smoke.spec.ts
```

---

## Delivery Phases

### Phase 0: Contract + Inventory (1-2 days)
- Lock command names, flags, and output schema.
- Mark each command as `public`, `auth`, or `internal`.
- Define stable JSON response shapes.

### Phase 1: CLI Core + npm Release (3-5 days)
- Build parser + output layer + error model.
- Implement `auth`, `api`, `publish/get/recent`, `clock publish/get/recent/now`, `piece publish`, `tv`.
- Publish `kidlisp@0.1.0`.

### Phase 2: Inspect + Pipe + Local Run Helpers (3-4 days)
- Add `inspect` family.
- Add `pipe build-stream`.
- Add KidLisp/clock run helpers (`--open`, URL emit, and local validation mode).

### Phase 3: Deno First-Class + Hardening (2-3 days)
- Add Deno install docs and smoke tests.
- Optional JSR wrapper publish.
- Finalize CI matrix and shell examples.

---

## Testing + CI

### Automated
- Unit tests for argument parsing and output modes.
- Integration tests against production-safe read endpoints.
- Mocked tests for mutating endpoints.
- Snapshot tests for `--json` and `--ndjson`.

### CI matrix
- Node LTS + current.
- Deno latest.
- Linux + macOS runners.

---

## Security + Safety Rules

- Prefer `AC_TOKEN` env, fallback to `~/.ac-token`.
- Never print token values unless explicitly requested (`kidlisp auth token`).
- Redact auth headers in debug logs.
- Add `--confirm` for destructive/mutating operations where relevant.
- Keep internal/privileged commands in separate plugin package.

---

## First Milestone Definition of Done

`v0.1.0` is done when:
- A new user can install and run from npm and Deno.
- These pipeline examples work:
  - `echo "(wipe blue)" | kidlisp publish --stdin --json | jq -r .code`
  - `kidlisp recent --limit 20 --ndjson | jq -r .code`
  - `kidlisp clock publish --file melody.txt --json`
  - `kidlisp api docs --json | jq '.endpoints | length'`
- CLI docs include copy-paste quickstart for bash/zsh/fish.

---

## Recommendation

Start now with a small public core and a strict pipe contract.  
Do not port internal fish tooling wholesale. Instead, split into:
- `kidlisp` (public, stable, scriptable)
- optional internal plugin package for infra/devcontainer operations.
