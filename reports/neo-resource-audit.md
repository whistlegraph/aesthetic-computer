# Neo resource audit — 2026-06-11

Snapshot of what eats the 8 GB MacBook Neo's RAM/CPU during a normal
working morning, what was changed in response, and what's left as
operating practice. Companion to `reports/js-tooling-audit.md`.

## Findings (mid-morning, 5 Claude sessions + netlify dev up)

- **Swap was 5.2 GB used of 6 GB** with only 28% memory free — the
  machine sits one heavy job away from thrashing at all times.
- **`netlify dev`'s esbuild service** (`zip-it-and-ship-it`) holds
  **~420–510 MB resident permanently** and bursts to 80% CPU while
  functions rebuild. Biggest single resident process tied to dev work.
- **Claude sessions: ~690 MB across 6 processes**, and each session
  spawns its own MCP stack — at audit time **60 MCP processes / 309 MB**
  (6× emacs-mcp, 6× mail-mcp, ~16 chrome-devtools-mcp instances each
  with a watchdog sidecar). None orphaned — all parented to live
  sessions — it's pure per-session multiplication.
- **Chrome: 19 processes / ~450 MB** (a single renderer hit 280 MB).
- **`mu` (mail indexer)** spikes to ~380 MB / 30% CPU while indexing.
- **ffmpeg**: 145 files under `pop/` spawn it; only one passes
  `-threads`. Default = all 6 cores at normal priority, so a render
  competes head-on with the UI and every Claude session.

## What changed

1. **Slab menubar telemetry** (`slab/menubar-swift`):
   - New monospaced row under Status: `mem 28% free · swap 5.2/6G ·
     load 4.2`, orange when free < 15% or swap > 90% full.
   - Its submenu lists the top 6 processes by resident memory
     (`MB / %CPU / name`) + "Open Activity Monitor".
   - Each live `/pop` render row now shows the driver process's RSS
     (via `proc_pid_rusage`) next to its progress bar, so a running job
     is tied to what it costs. Children (ffmpeg/python) surface in the
     hogs list.
   - Sources: `StateSnapshot.swift` (`SystemStats`, `readSystemStats()`,
     `residentMB(of:)`), `MenuBuilder.swift` (`buildSystem`), rebuild
     with `slab/menubar-swift/install.sh`.
2. **QoS shims** (`toolchain/shims/ffmpeg`, `ffprobe`): exec the real
   binary under `taskpolicy -c utility`. Renders use all idle cores at
   full speed but yield to the UI under contention. Wired ahead of
   homebrew in `~/.config/fish/config.fish` on the Neo, so anything
   spawned from a shell (including node children resolving `ffmpeg`
   via PATH) inherits it. Scripts that hardcode
   `/opt/homebrew/bin/ffmpeg` bypass the shim.

## Round 2 (same day): MCP de-duplication for parallel sessions

Claude Code spawns every **stdio** MCP server once per session, so N
parallel sessions pay N copies. Fixes, in order of leverage:

1. **mail-mcp + emacs-mcp → shared HTTP daemons.** Both servers grew a
   `--http [port]` mode (stdio default untouched — codespaces keeps
   using `.mcp.json` as-is). On the Neo they run once under launchd
   (`computer.aesthetic.mail-mcp` :7765, `computer.aesthetic.emacs-mcp`
   :7766 — `toolchain/mcp/install-daemons.sh`), and local-scope http
   entries in `~/.claude.json` shadow the stdio ones. 12 processes
   across 6 sessions → 2 total, shared by any number of sessions.
2. **chrome-devtools telemetry watchdogs killed.** All three entries
   (local/chicken/panda) now pass `--no-usage-statistics`, which stops
   the ClearcutLogger from spawning a watchdog sidecar per instance
   (was ~16 extra processes).
3. **npx wrapper dropped.** Entries point at a pinned global install
   (`npm i -g chrome-devtools-mcp`, fnm default bin) instead of
   `npx -y chrome-devtools-mcp@latest` — no registry check and no npx
   process tree per session. Update deliberately manual now: `npm i -g
   chrome-devtools-mcp` to bump.
4. **Local chrome-devtools is lazy.** `toolchain/mcp/minis-mcp.mjs` is
   group-based: the local server enables on
   browser/chrome/devtools/screenshot/lighthouse/webpage/headless
   mentions (stamp `~/.claude/chrome-mcp-active`), auto-disables after
   12 h idle — same mechanism the chicken/panda pair already used.
   Sessions that never touch a browser no longer spawn it at all.

Existing sessions keep their old processes until restarted; the savings
land as terminals close and reopen.

## Deliberately not done

- **Global `NODE_OPTIONS=--max-old-space-size`**: on an 8 GB machine
  V8's default old-space cap is already ~2 GB, and the actual hogs
  (esbuild = Go, ffmpeg = C, Chrome) aren't V8 heap. A global cap
  changes nothing except adding a new crash mode for legit big renders.
- **Wrapping `npm run site` in taskpolicy**: package.json scripts are
  shared with codespaces/linux; esbuild's CPU burn is bursty, not
  constant. Revisit only if it shows up pinned in the new hogs list.

## Operating practice (the stuff config can't fix)

- Don't leave `netlify dev` running when not doing site work — it's
  ~600 MB (node + esbuild service) of permanent residency.
- Each Claude session costs ~100–150 MB **plus ~60 MB of MCP servers**;
  five idle sessions ≈ 1 GB. Close finished terminals. If the
  chicken/panda DevTools MCPs keep appearing in sessions that never use
  them, shorten the minis-mcp hook's 12 h idle expiry.
- Existing rules still apply: one Chromium, one ffmpeg, one whisper at
  a time; vitest `--maxWorkers=1` (see agent memory "8 GB RAM
  concurrency limits").
- Swap doesn't shrink once grown — after a heavy day the 5 GB swap file
  stays. A reboot is the only real reset.
