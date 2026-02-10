# Repository Guidelines

## Project Structure & Module Organization
- `system/` holds the main web app (Netlify config, public assets, templates, scripts) and is the primary entry for feature work.  
- `ac-electron/` is the macOS/Windows desktop Electron app (AC Pane). Edit files here when working on the desktop app.
- `session-server/` contains the real-time session backend (Jamsocket).  
- `shared/` offers reusable browser/server utilities; use these before adding new helpers.  
- `utilities/` scripts generate or sync assets (e.g., `generate-new-piece.mjs`).  
- `tests/` covers integration/performance; `spec/` is for KidLisp language specs; project-wide docs live at the repo root. Place new feature code next to its runtime (client in `system/`, server in `session-server/`).
- `reference/strudel/` contains a local clone of the Strudel live coding project (from Codeberg). Key packages for audio/scheduling: `packages/core/cyclist.mjs` (event scheduler), `packages/core/pattern.mjs` (`.onTrigger()` callback for hooking into pattern events), `packages/webaudio/` (Web Audio integration), `packages/draw/` (visualization sync). Use this reference when working on Strudel integration in `kidlisp.com`.

## Build, Test, and Development Commands
Run from the repo root unless noted:
- `npm run aesthetic` — starts site, session server, edge services, Stripe mocks, and URL helper; use for full-stack local work.  
- `npm run site` — launches the client stack only (`system/`).  
- `npm test` — runs the top-level smoke/integration tests.  
- `npm run test:perf` or `npm run test:perf:chrome` — performance harnesses in `tests/performance/`.  
- `npm run test:kidlisp` — watches and runs KidLisp specs in `spec/`.  
- `npm run url` — prints your local tunnel URL for hitting the app in browsers/devices.

## Fish Shell Commands (DevContainer)
The devcontainer provides these `ac-*` commands (defined in `.devcontainer/config.fish`):

### Machine & SSH Helpers
- `ac-host` — List all machines from `vault/machines.json` with SSH connection info.
- `ac-host <key> ssh` — SSH to a specific machine (e.g., `ac-host jeffrey-macbook ssh`).
- `ac-host <key> ip` — Get just the IP address of a machine.
- `ac-host <key> info` — Show full JSON config for a machine.
- `ac-machines` — Alias for `ac-host` (list all machines).

### FF1 Art Computer
- `ac-ff1` — Show FF1 info and available commands.
- `ac-ff1 scan` — Find FF1 via MacBook mDNS (discovers IP changes).
- `ac-ff1 ping` — Check if FF1 is responding.
- `ac-ff1 cast <url>` — Send a URL to display on FF1 (e.g., `ac-ff1 cast https://aesthetic.computer/purple`).
- `ac-ff1 tunnel` — Create SSH tunnel for local FF1 development (localhost:1111 → FF1).

### Electron App (Mac Host)
- `ac-electron-restart` — Restart the Electron app on the host Mac.
- `ac-electron-stop` — Stop the Electron app.
- `ac-electron-start` — Start the Electron app.
- `ac-electron-dev` — Toggle dev mode (on/off/status). When enabled, the production app loads files from the repo.
- `ac-electron-reload` — Reload Electron windows (triggers Cmd+R).

**Electron App Source**: `ac-electron/` contains the desktop app:
- `main.js` — Main process (window management, IPC handlers, tray menu)
- `preload.js` — Preload for BrowserWindows (exposes `window.ac` API)
- `webview-preload.js` — Preload for webviews (exposes `window.acElectron` for window control)
- `renderer/flip-view.html` — Main AC Pane UI (3D flip card with webview + terminal)
- `renderer/preferences.html` — Preferences window

**Dev Mode**: Create `~/.ac-electron-dev` on the Mac host to make the production app load renderer files from `~/aesthetic-computer/ac-electron/` instead of the app bundle. This enables live iteration without rebuilding:
1. `ac-electron-dev on` — Enable dev mode
2. Edit files in `ac-electron/`
3. `ac-electron-reload` or Cmd+R in app — See changes
4. `ac-electron-dev off` — Disable when done

### Development
- `ac-tv` — Query TV API endpoints.
- `ac-pack` — Package a piece for NFT minting.
- `ac-keep` — Mint a KidLisp piece as an NFT.
- `ac-repl` — Start KidLisp REPL.
- `ac-emacs-restart` — Restart Emacs daemon.

## Emacs Terminal Buffers
The development environment uses Emacs with multiple named terminal buffers (eat terminals). When the user refers to these by name or nickname, use the Emacs MCP tools (`mcp_emacs_*`) instead of `run_in_terminal`:

- `🐟-fishy` or "fishy" — Main fish shell terminal for general commands
- `🩸-artery` or "artery" — Artery service logs
- `💳-stripe-print` — Stripe print logs
- `🤖-chat-system` — Chat system logs
- `📋-session` — Session server logs
- `🌐-site` — Site/web server logs
- `🔴-redis` — Redis logs
- `🖼️-views` — Views logs
- `🤖-llm` — LLM service logs
- `💥-crash-diary` — Crash logs
- `📊-top` — System monitoring (top)
- `🧪-kidlisp` — KidLisp test runner
- `📦-media` — Media service logs
- `🔥-oven` — Oven service logs
- `🔖-bookmarks` — Bookmarks
- `⏰-chat-clock` — Chat clock logs
- `🧠-chat-sotce` — Sotce chat logs
- `🎫-stripe-ticket` — Stripe ticket logs
- `⚡-url` — URL service logs
- `🚇-tunnel` — Tunnel logs

**Usage**: When asked to run commands in "fishy" or any named terminal, use:
1. `mcp_emacs_emacs_switch_buffer` to switch to the buffer
2. `mcp_emacs_emacs_send_keys` to send the command
3. Send a newline character to execute

Emacs tabs can be switched with `(tab-bar-select-tab N)` via `mcp_emacs_execute_emacs_lisp`.

## Coding Style & Naming Conventions
- JavaScript/TypeScript modules use ESM (`.mjs`); prefer 2-space indentation and trailing commas.  
- Run Prettier where available (`npx prettier --write <files>`); respect existing file conventions (some legacy scripts mix shell/Fish).  
- Name new pieces and assets with lowercase-hyphenated paths; colocate tests as `<name>.test.mjs` near related code or under `tests/`.  
- Keep configuration in `.env`-style files out of version control; use sample files if needed.

## Testing Guidelines
- Add focused `.test.mjs` files; use Jasmine for KidLisp specs and Node test runners for integration.  
- Prefer running `npm test` before PRs; include performance runs when touching rendering/audio paths.  
- For perf tests, document baseline numbers and hardware in the PR description.  
- Snapshot or fixture data should live in `tests/` subfolders; avoid large binaries in git.

## Commit & Pull Request Guidelines
- Follow the existing history: short, action-led subjects (`fix: ...`, `docs: ...`, or descriptive phrases).  
- PRs should describe the change, runtime targets (web/session), and how to reproduce/test.  
- Link related issues or user-facing commands; add screenshots or clips for UI/visual changes.  
- Keep commits logically scoped; prefer small, reviewable units over large drops.  
- Ensure `npm test` (and relevant perf or KidLisp suites) are green before requesting review.

## Security & Configuration Tips
- Never commit secrets; use environment variables and the Netlify/Jamsocket env management scripts in `system/`.  
- When syncing assets, use `npm run assets:sync:down`/`up` and verify paths under `system/public/assets`.  
- Treat production endpoints in scripts as sensitive; dry-run changes against local or dev tunnels first.
