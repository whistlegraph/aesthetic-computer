# Emacs Stack Report — Docker Container Context (2026-01-21)

## Purpose
Document how the Emacs stack works inside the dev container, why the UI can appear as only `*scratch*`, and how to reliably bring back the full Aesthetic tab layout after crashes.

## Primary Entry Points
1. **Daemon start script**
   - The container starts Emacs in daemon mode with the repo config:
     - Config path: [dotfiles/dot_config/emacs.el](dotfiles/dot_config/emacs.el)
     - Startup logic: [ .devcontainer/config.fish](.devcontainer/config.fish)

2. **Crash monitor / watchdog**
   - Keeps the daemon alive and restarts it if it becomes unresponsive.
   - Crash monitor script: [monitor-emacs.sh](monitor-emacs.sh)
   - Logs: [.emacs-logs](.emacs-logs)

3. **Terminal client**
   - `emacsclient -t` or `ac-aesthetic` connects a terminal frame.
   - The first terminal frame triggers the backend that creates tabs and Eat terminals.

## Core Runtime Components
### `aesthetic-backend`
Defined in [dotfiles/dot_config/emacs.el](dotfiles/dot_config/emacs.el).
- Creates tab bar layout and terminal buffers.
- Runs all `ac-*` commands that launch web servers, sessions, Redis, oven, etc.
- Switches to a target tab after creation (default: `artery`).

### `ac--maybe-start-backend`
Also in [dotfiles/dot_config/emacs.el](dotfiles/dot_config/emacs.el).
- Registered via `after-make-frame-functions`.
- Only runs when a **terminal frame** connects.
- That’s why headless daemon restarts show `*scratch*` only until a terminal frame connects.

### Eat terminals
Each tab uses Eat to run a fish command (e.g., `ac-redis`, `ac-site`, `ac-session`).
If Emacs restarts without a terminal frame, **none of these processes are re-spawned** until `aesthetic-backend` runs again.

## Normal Startup Flow (Expected)
1. Daemon starts via `.devcontainer/config.fish`.
2. Terminal frame connects (`ac-aesthetic` or `emacsclient -t`).
3. `ac--maybe-start-backend` calls `aesthetic-backend`.
4. Tabs and Eat buffers are created in order.
5. After a delay, Emacs switches to the target tab (`artery`).

## Crash + Auto-Restart Flow (Observed)
1. Crash monitor restarts the daemon.
2. Daemon is alive **but no terminal frame is connected**.
3. `aesthetic-backend` does **not** run automatically.
4. User sees only `*scratch*` and no tabs.

**This is expected behavior** given the current hook architecture.

## Why You See Only `*scratch*`
The backend startup is tied to `after-make-frame-functions` and specifically checks for terminal frames. When the daemon restarts in the background, no terminal client connects, so the backend never runs. This leaves a minimal Emacs session without tabs or Eat buffers.

## Recovery Steps
If Emacs restarts and only `*scratch*` is visible:
1. Connect a terminal frame:
   - `emacsclient -t`
2. If the tabs do not appear, explicitly run:
   - `M-x aesthetic-backend`
   - Or evaluate: `(aesthetic-backend "artery")`

## Signals and Logs to Verify State
- **Is the daemon running?**
  - `pgrep -f "emacs.*daemon"`
- **Did `aesthetic-backend` run?**
  - Check [ .emacs-logs/emacs-debug.log](.emacs-logs/emacs-debug.log) for:
    - `Starting aesthetic-backend with target-tab: artery`
- **Did tabs get created?**
  - `emacsclient -e '(tab-bar-tabs)'`

## Known Failure Modes
1. **Crash without terminal reconnect**
   - Result: only `*scratch*` and no tabs.
2. **Backend delay warnings**
   - Log entry: `WARNING: aesthetic-backend has been running for 30+ seconds`.
   - Usually indicates slow-starting processes in Eat buffers.
3. **CDP tunnel failure**
   - Log entry: `CDP tunnel failed: exited abnormally with code 1`.
   - Non-fatal, but indicates a missing tunnel endpoint.

## Suggested Improvements (Optional)
To avoid manual recovery after crashes:
1. **Crash monitor should run `aesthetic-backend` after restart.**
2. Or **auto-connect a terminal client** (`emacsclient -t`) after daemon start.
3. Add a lightweight sanity check:
   - If no tabs beyond `*scratch*`, trigger `aesthetic-backend` automatically.

## Quick Reference
- Main config: [dotfiles/dot_config/emacs.el](dotfiles/dot_config/emacs.el)
- Daemon start: [.devcontainer/config.fish](.devcontainer/config.fish)
- Crash monitor: [monitor-emacs.sh](monitor-emacs.sh)
- Debug log: [.emacs-logs/emacs-debug.log](.emacs-logs/emacs-debug.log)
- Crash diary: [.emacs-logs/crashes.log](.emacs-logs/crashes.log)