# VSCode Extension: Dashboard & AT Firehose

## Vision

Replace the current "Source Changes" welcome panel with a **live dashboard** that acts as a firehose for all Aesthetic Computer platform activity — git changes, AT Protocol events, and Tangled knot repo activity — all in one view.

## Current State

The welcome panel (`extension.ts:966-1100`) is a simple git status viewer:
- Runs `git status --porcelain` + `git rev-parse --abbrev-ref HEAD` for the main repo + vault
- Displays file list with status badges (M/A/D/?)
- Click-to-open files in editor
- Debounced refresh on file changes (300ms)

**Known bug:** `git rev-parse --abbrev-ref HEAD` fails on repos with no commits (e.g. freshly cloned submodules), showing a raw error in the UI instead of a graceful fallback.

---

## Phase 1: Fix the Git Error + Improve Layout

**Goal:** Make the existing git status panel work correctly and look better.

- [ ] Handle `rev-parse` failure gracefully — show `(no commits)` or `(init)` instead of the raw error
- [ ] Improve the visual layout: tighter spacing, better typography, cleaner status badges
- [ ] Add a collapsible section header for "Source Changes" (so it can coexist with new sections)
- [ ] Add timestamp showing last refresh time

## Phase 2: Dashboard Shell

**Goal:** Transform the panel from a single-purpose git viewer into a multi-section dashboard.

- [ ] Redesign the HTML structure with distinct dashboard sections:
  1. **Source Changes** — the existing git status (cleaned up from Phase 1)
  2. **AT Firehose** — live AT Protocol events (Phase 3)
  3. **Tangled** — knot/repo activity (Phase 4)
- [ ] Add a sticky nav/header with section toggles (show/hide each section)
- [ ] Support auto-scroll vs. paused mode (like a terminal — new events stream in, but scrolling up pauses the feed)
- [ ] Add theme-aware styling for all new sections (dark/light support already exists)
- [ ] Consider making this a sidebar webview instead of (or in addition to) an editor panel

## Phase 3: AT Protocol Firehose

**Goal:** Show live AT Protocol activity happening on the Aesthetic Computer PDS.

The AC platform already has deep atproto integration:
- PDS at `at.aesthetic.computer`
- Lexicons: `computer.aesthetic.tape`, `computer.aesthetic.mood`, `computer.aesthetic.painting`, etc.
- Existing scripts in `at/scripts/atproto/`
- News posted to atproto via `system/backend/news-atproto.mjs`
- Tapes synced to atproto via `system/backend/tape-atproto.mjs`

### What to show in the firehose:
- [ ] **New tapes** — when users record/publish tapes (video content)
- [ ] **Moods** — mood updates from users (`computer.aesthetic.mood`)
- [ ] **Paintings** — shared paintings (`computer.aesthetic.painting`)
- [ ] **News posts** — new news articles published
- [ ] **Handle registrations/updates** — new users or handle changes
- [ ] **Standard site updates** — page edits, new pages

### Implementation options:
- **Option A: WebSocket relay** — Add a firehose WebSocket endpoint to the session server or a new lightweight service that subscribes to the PDS `com.atproto.sync.subscribeRepos` firehose and relays events to the extension.
- **Option B: Polling** — Periodically query `com.atproto.repo.listRecords` for each collection on the PDS. Simpler but less real-time.
- **Option C: Direct firehose** — Connect the extension directly to `wss://at.aesthetic.computer/xrpc/com.atproto.sync.subscribeRepos` and decode the CBOR/CAR stream in the extension itself. Most direct but complex.

### Recommended: Option A (WebSocket relay)
The extension already has a WebSocket connection to the session server (`extension.ts:2132`). We can add a `firehose:event` message type that the session server relays from the PDS subscription. This keeps the extension lightweight and reuses existing infrastructure.

### Event display format:
Each event in the feed should show:
- Timestamp
- Event type icon/badge (tape, mood, painting, news, etc.)
- Actor handle (clickable — opens their AC profile)
- Brief content preview (tape thumbnail, mood text, painting preview, etc.)
- Link to view the full content on aesthetic.computer

## Phase 4: Tangled Knot Activity

**Goal:** Show activity from the Tangled knot (AT Protocol-native git hosting).

The AC repo is mirrored on Tangled at `tangled.org/aesthetic.computer/core`. The knot server runs on the same PDS droplet (`knot.aesthetic.computer`).

### What to show:
- [ ] **Commits** — recent commits pushed to the Tangled knot
- [ ] **Issues/PRs** — if Tangled supports these (check API)
- [ ] **Stars/forks** — social activity on the repo
- [ ] **Cross-reference with GitHub** — show if a commit exists on both GitHub and Tangled

### Implementation:
- [ ] Research Tangled API (likely atproto-based — check `tangled.org` for API docs)
- [ ] The knot server is at `knot.aesthetic.computer` — check what XRPC endpoints it exposes
- [ ] Display as a feed of events similar to the AT firehose section
- [ ] Include both the main `aesthetic-computer` repo and any other knot-hosted repos

## Phase 5: Polish & Interaction

- [ ] Add filtering — filter firehose by event type, user, time range
- [ ] Add notification badges on the sidebar icon when new events arrive
- [ ] Keyboard shortcuts for navigating the dashboard
- [ ] Click-through actions: clicking a tape opens it, clicking a commit shows the diff, etc.
- [ ] Sound/visual ping option for specific event types (e.g., new user signups)
- [ ] Consider a compact "ticker" mode that shows a single scrolling line of recent events in the status bar

---

## Architecture Notes

### Data flow
```
PDS (at.aesthetic.computer)
  └─ com.atproto.sync.subscribeRepos (firehose)
       └─ Session Server (relay)
            └─ WebSocket → VSCode Extension
                 └─ Dashboard Webview

Knot Server (knot.aesthetic.computer)
  └─ XRPC / atproto API
       └─ Session Server or direct polling
            └─ WebSocket → VSCode Extension
                 └─ Dashboard Webview

Local Git Repos
  └─ git status --porcelain (child_process)
       └─ Extension host
            └─ postMessage → Dashboard Webview
```

### Key files to modify
- `vscode-extension/extension.ts` — welcome panel HTML + git status logic
- `session-server/session.mjs` — add firehose relay endpoint
- `vscode-extension/embedded.js` — may need message bridging updates
- `vscode-extension/package.json` — new commands, settings

### Dependencies to consider
- `@atproto/api` — already in `system/package.json`
- CBOR decoding for firehose (if doing direct connection)
- The extension currently bundles with esbuild — any new deps need to be bundleable
