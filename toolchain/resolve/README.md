# DaVinci Resolve control (MCP) — research + setup kit

Goal: drive DaVinci Resolve from Claude/our stack — first **locally on this Mac**
to test, then **remotely on a client's Mac**. Status as of 2026-06-19: prep only;
Resolve is not installed on this machine yet.

## What's already out there (don't rebuild)

- **Resolve ships an official scripting API** (Python + Lua). The relevant toggle is
  `Preferences → System → General → External scripting using`:
  `None` / `Local` (same machine) / **`Network`** (remote host). Python + Network mode
  require **Resolve Studio** (the free version is Lua-only, local-only).
- **A mature MCP already wraps the API:**
  [`samuelgursky/davinci-resolve-mcp`](https://github.com/samuelgursky/davinci-resolve-mcp)
  — ~1.3k★, actively maintained (shipped a release 2026-06-19), Python, **stdio** transport,
  macOS/Win/Linux, Studio 18.5+. 32 compound tools by default (341 granular), ~100% API
  coverage. Covers project/timeline/media-pool/render/color/Fusion/Fairlight plus
  `execute_python` / `execute_lua`.
  - Alt: [`apvlv/davinci-resolve-mcp`](https://github.com/apvlv/davinci-resolve-mcp), Tooflex.
- **Gap:** every existing MCP is a *local stdio process* — assumes Claude and Resolve on the
  same Mac. None do remote / fleet / telemetry. That's our value-add (Tier 2).

## Tier 1 — local (this Mac), to test

1. Install DaVinci Resolve (Studio for the full API).
2. Resolve → Preferences → System → General → External scripting using → **Local**.
3. `./toolchain/resolve/setup.fish` (preflight, then runs `npx davinci-resolve-mcp setup`,
   which auto-writes the Claude Code MCP config).
4. Restart Claude Code → `davinci-resolve` tools appear.

The upstream installer manages its own Python copy + env vars (`RESOLVE_SCRIPT_API`,
`RESOLVE_SCRIPT_LIB`, `PYTHONPATH`), so we don't hand-maintain a `.mcp.json` entry.

## Tier 2 — remote (client's Mac) — to build

Existing MCPs can't do remote. Two transports, both leaning on code we already have:

- **(a) Resolve Network mode** — set scripting to `Network`, run the Python MCP on the
  client's Resolve box, expose over our fleet transport. Needs a network shim (MCP is
  stdio-only today). Simplest conceptually.
- **(b) puppet-style daemon over SSH-multiplex** *(recommended)* — fork
  `slab/bin/puppet.mjs` so instead of CDP it pipes commands to a Resolve Python session on
  the target Mac; `slab/bin/frame.mjs` captures timeline/UI state back; `system/netlify/
  functions/boot-log.mjs` pattern tracks each render-job lifecycle. Reuses the most proven
  fleet code and matches how neo/blueberry/panda already talk.

Reusable foundations (from a stack survey):
| Need | Have | File |
|---|---|---|
| MCP server scaffold | `@modelcontextprotocol/sdk` + Zod | `ants/mail-mcp/server.mjs` |
| Remote command multiplexer | daemon + unix socket + SSH-mux | `slab/bin/puppet.mjs`, `frame.mjs` |
| Native macOS window control | Accessibility API (no AppleScript) | `slab/menubar-swift/.../AXTiler.swift` |
| Fleet sync | geckos.io WebRTC + Multipeer | `session-server/session.mjs` |
| Job telemetry | boot-phase logging → Mongo | `system/netlify/functions/boot-log.mjs` |
| Render farm | ffmpeg/sharp job queue | `oven/` |

## Context

Originated from a request by Greg (+1 908 963 1175). Texted him both repo URLs +
the Studio/scripting-prefs note on 2026-06-19, asking what his specific issue is and
whether he's tried these in his workflow yet. Awaiting reply before committing to Tier 2.
