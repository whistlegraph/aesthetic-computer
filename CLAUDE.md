# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository. It is kept deliberately short (target: under 200 lines); domain detail lives in on-demand files noted throughout — read those when working in their area.

## Execution Environment

Read `ENVIRONMENT.md` and resolve the active environment before applying
editor-specific instructions from any score. Claude Terminal on macOS uses its
local shell directly; Emacs MCP/fishy instructions apply only when the active
environment is `emacs`.

## Project Overview

Aesthetic Computer (AC) is a mobile-first runtime and social network for creative computing. It's designed as a musical instrument-like interface where users discover memorizable paths through commands and published "pieces" (interactive programs). The system supports both JavaScript (.mjs) and KidLisp (.lisp) pieces.

## Agent Memory (Local-First)

When @jeffrey is working, Claude hook events are written to a local encrypted memory store first.

- **Hook**: `.claude/settings.json` → `UserPromptSubmit` → `node memory/hook.mjs`
- **Git hook**: `.githooks/post-commit` → commit log + Codex import + remote flush
- **Local store**: `~/.ac-agent-memory` (overridable via `AGENT_MEMORY_HOME`); AES-256-GCM; redacted before indexing/sync
- **CLI**: `node memory/cli.mjs` (`list`, `remember`, `checkpoint`, `doctor`, `profile`, `flush-remote`); Codex import via `node memory/codex-sync.mjs`
- Remote writes are optional and disabled by default (`AGENT_MEMORY_REMOTE_ENABLED=true` + `AGENT_MEMORY_REMOTE_URL=...` to enable). `remember` continuity is lineage (`remembered_from`), not session takeover.

## AestheticAnts & Score.md

Automated maintenance system ("AA"). Main score: `SCORE.md`; ant mindset/rules: `ants/mindset-and-rules.md`. Read both before contributing.

**Important:** Do not modify `ants/mindset-and-rules.md` unless you are the queen (@jeffrey).

## The Hand (Code Style)

`HAND.md` at the repo root is the code-style guide — companion to `papers/VOICE.md` (prose). Read it before writing or carving core code. Key idea: leaves (pieces) stay small and can be loop-generated; the foundational libs are the instrument and want knowability over raw terseness. The active "rehandify" campaign and its guardrails live at the bottom of that file.

## Development Commands

### Running the Development Environment

```bash
npm run aesthetic         # Run all servers (site, session, edge, stripe) — primary dev command (alias: npm run ac)
npm run site              # Main dev server (port 8888)
npm run server:session    # Session backend (port 8889)
npm run stripe            # Stripe webhook listener
```

### Testing

```bash
npm test                  # All tests
npm run test:kidlisp      # KidLisp tests (watch; :direct for no watch)
npm run test:perf         # Performance tests (:chrome, :lighthouse variants)
```

### Health Check (Doctor)

Stack-wide preflight before debugging — tells you *which* layer is sick:

```bash
npm run doctor            # full sweep; -- --local / -- --prod / -- --strict
```

Stopped dev servers read as ⚠️ (advisory); only prod site + CDN are critical. Add checks in the `CHECKS` array of `toolchain/doctor.mjs` (dependency-free).

### Pieces, Sessions, Assets

```bash
npm run new piece-name "Description"   # New piece from blank.mjs template
npm run session:alive                  # List active session backends
npm run server:session:logs ID         # Logs for a session (session:reset ID to terminate)
npm run session:publish                # Deploy session server
npm run assets:sync:down               # Sync assets from DO Spaces (:up to push)
```

### AC Native OS (fedac/native/)

**Routine OTA releases are built remotely on the oven.** When fedac/native/ changes land on `origin/main`, oven's git poller auto-triggers a build.

```bash
ac-os oven          # Trigger remote OTA build for HEAD
ac-os oven status   # Show oven queue + recent builds
ac-os oven watch    # Tail active build logs (SSE)
ac-os oven cancel   # Cancel active oven job
```

**Use `ac-os oven` for OTA releases — not `ac-os upload`.** `ac-os upload` is a local-build-and-push fallback that requires a clean tree and has historically auto-stashed uncommitted work in ways that strand changes.

Local-only commands (rarely needed): `ac-os build` (binary → initramfs → kernel), `ac-os flash`, `ac-os upload`, `ac-os flash+upload`.

**Critical:** `ac-os upload` always does a full rebuild before uploading. The kernel embeds the git hash and build name at compile time (`AC_GIT_HASH`, `AC_BUILD_NAME` in the Makefile). Uploading without rebuilding would serve a stale kernel with the wrong version string.

### Notation

- **compush** - commit & push. If the commit touches live-served paths (`system/public/**`, `system/netlify/functions/**`), follow with `fish lith/deploy.fish` unprompted — pushing alone doesn't put it in production.
- **sticky the X** - on a macOS host, run `node toolchain/macos/sticky.mjs` on X — translucent, larger-text Stickies note sized to fit and centered. See `toolchain/macos/README.md`.

## Architecture

### Core Components

1. **Boot** (`system/public/aesthetic.computer/boot.mjs`) — entry point; loads BIOS, service-worker module caching, WebSocket module loader for hot reload, boot telemetry to `/api/boot-log`.
2. **BIOS** (`system/public/aesthetic.computer/bios.mjs`) — main runtime coordinator; piece lifecycle, API surface, routing/navigation.
3. **Disk** (`system/public/aesthetic.computer/lib/disk.mjs`) — large (~572KB) core API for pieces: graphics primitives, audio, input, UI. All pieces talk to AC through the Disk API.
4. **Module loader** (`system/public/aesthetic.computer/module-loader.mjs`) — WebSocket dynamic loading; hot reload in dev; prefetches common modules.

### Pieces

All pieces live in `system/public/aesthetic.computer/disks/` (`.mjs` and `.lisp`). **The authoring guide — lifecycle functions, API surface, event patterns, multiplayer dual-channel, UI components, publishing — is `system/public/aesthetic.computer/disks/CLAUDE.md`** (loads automatically when working there). Canonical multiplayer reference: `squash.mjs`.

### Servers and Services

1. **System Server** (`system/` + `lith/`) — production is **lith**: Express + Caddy monolith on a DigitalOcean VPS (`lith.aesthetic.computer`), deployed with `fish lith/deploy.fish`, pulling from the tangled knot `git@knot.aesthetic.computer:aesthetic.computer/core`. Netlify is no longer the host. Dev: `npm run site` (port 8888). Backend function handlers live in `system/netlify/functions/` — path is historical; lith's Express adapts each file as a route, so keep adding endpoints there.
2. **Session Server** (`session-server/`) — per-session backend via Jamsocket; Geckos.io WebSocket+UDP for chat, multiplayer, real-time state; Redis for sync.
3. **Feed Server** (`dp1-feed/`) — Cloudflare Worker for activity feeds, deployed separately.

### KidLisp

Minimal Lisp dialect for generative art (118 built-ins across 12 categories). **Comprehensive docs: `kidlisp/README.md`.** Evaluator: `system/public/aesthetic.computer/lib/kidlisp.mjs`; storage API: `system/netlify/functions/store-kidlisp.mjs`; tools in `kidlisp/tools/` (`./kidlisp/tools/source-tree.mjs $cow`, `get-source.mjs $piece-code`; dev server must be running).

### Data Storage

**MongoDB** (users, handles, chat, moods) · **Redis** (session state) · **Firebase** (auth, messaging) · **DO Spaces** (asset CDN).

### Routing and URLs

Pieces are URL-addressable: `aesthetic.computer/piece-name`, params `piece-name:p1:p2`, user pieces `@handle/piece-name`, QR via `share piece-name`.

## Development Workflow

- **Codespaces**: server at `https://{CODESPACE_NAME}-8888.app.github.dev` (`echo $CODESPACE_NAME`).
- **Hot reload**: piece changes reflect on save via the module loader; WebSocket status shows in the boot canvas; use `channel custom-name` for multi-device testing.

## Important Directories

- `system/public/aesthetic.computer/disks/` - All pieces (+ authoring guide CLAUDE.md)
- `system/public/aesthetic.computer/lib/` - Shared libraries
- `system/netlify/functions/` - Serverless backend functions (served by lith)
- `session-server/` - Real-time session backend
- `shared/` - Code shared between system and session servers
- `kidlisp/` - KidLisp docs and tools
- `spec/` - Jasmine tests for KidLisp
- `ants/` - AestheticAnts automated maintenance

## Notes

- `.mjs` ES modules throughout
- When making changes, consult `ants/mindset-and-rules.md` for the ant operating philosophy
