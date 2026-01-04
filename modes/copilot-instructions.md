# Aesthetic Computer

**Environment:** Devcontainer in `/workspaces/aesthetic-computer` monorepo (Fedora Linux).

## Architecture Overview
Copilot CLI runs inside a layered environment:
1. **Electron app** (aesthetic-desktop) — hosts VS Code
2. **Emacs daemon** — manages terminal buffers via `eat-mode`
3. **Copilot CLI** — runs in terminal, can control Emacs via MCP

The **Emacs MCP Server** (`artery/emacs-mcp.mjs`) bridges Copilot to Emacs, enabling:
- Executing Elisp code
- Sending keys to terminal buffers
- Reading buffer contents
- Switching between buffers/tabs

## Key Paths
- **Vault:** `aesthetic-computer-vault/` — credentials, machine configs. **Never copy secrets.**
- **Emacs MCP:** `artery/emacs-mcp.mjs` — bridges Copilot ↔ Emacs
- **Artery TUI:** `artery/artery-tui.mjs` — main interactive interface, run via Emacs MCP
- **KidLisp:** `kidlisp/` (interpreter), `system/public/aesthetic.computer/disks/` (pieces)
- **Tezos:** `tezos/keeps.mjs` — NFT contract CLI

## Workflow
1. Run commands through **Emacs MCP** (`mcp_emacs_*` tools) when possible
2. Artery TUI is the primary control interface for AC
3. Check `vault/machines.json` to identify current machine context
4. Dev server: `./aesthetic-launch.sh` (usually auto-running)

## File Editing Preferences
- **Prefer Emacs MCP** (`mcp_emacs_execute_emacs_lisp`) for file edits over `sed`, `awk`, or `run_in_terminal`
- Use `find-file`, `search-forward`, `replace-match`, `insert`, `save-buffer` for reliable edits
- For shell commands, use **fishy** buffer (`mcp_emacs_emacs_send_keys buffer="🐟-fishy"`) over VS Code terminal
- Avoid `cat > file << EOF` heredocs — they crash the devcontainer
- Use `node --check file.mjs` to verify syntax after edits

## Rules
- Keep responses concise
- Use existing tooling (artery, emacs-mcp) over raw terminal
- Never expose or copy vault secrets
- **NEVER** call `get_task_output` on the "💻 Aesthetic" task — it freezes the devcontainer

- **NEVER** use heredoc/cat file creation in terminal (e.g., `cat > file << EOF`) — crashes devcontainer
- **Scratch directory**: Use `scratch/` (gitignored) for temp files via `create_file` tool

## Git & Merge Safety 🔒

### Critical Files (Protected by Pre-Commit Hook)
These files are frequently clobbered during bad merges. A pre-commit hook (`.githooks/pre-commit`) will **block commits** if they lose >30% of lines:
- `artery/artery-tui.mjs` (~7200+ lines)
- `dotfiles/dot_config/emacs.el` (~1200+ lines)
- `system/public/aesthetic.computer/lib/udp.mjs` (~170+ lines)
- `system/public/aesthetic.computer/lib/logs.mjs`
- `system/public/aesthetic.computer/lib/speaker.mjs`
- `system/public/aesthetic.computer/bios.mjs`

### Why Clobbering Happens
In VS Code devcontainer, merge conflicts can accidentally replace local files with older remote versions when:
- Using "Accept Current" or "Accept Incoming" without reviewing
- Auto-merging picks the wrong version
- Large files with complex changes get resolved incorrectly

### Safe Merge Process
1. **Before merging**: `git stash` any uncommitted changes
2. **Pull with rebase preferred**: `git pull --rebase origin main`
3. **If conflicts occur**:
   - For critical files above, **always** choose "Accept Both" and manually resolve
   - Check `git diff --stat` before committing to verify line counts
   - Large negative numbers (e.g., `-5000`) = likely clobbering!
4. **If hook blocks commit**: 
   - Review the warning carefully
   - Restore from good commit: `git show <good-commit>:path/to/file > path/to/file`
   - Only use `git commit --no-verify` if deletion is intentional

### Recovering Clobbered Files
```bash
# Find the last good version
git log --oneline -20 -- path/to/file

# Restore from a known good commit
git show <commit-hash>:path/to/file > path/to/file

# Common restore pattern for artery-tui
git show HEAD~5:artery/artery-tui.mjs | wc -l  # Check line count
git show HEAD~5:artery/artery-tui.mjs > artery/artery-tui.mjs  # Restore
```

### Hook Configuration
The hooks are in `.githooks/` and enabled via:
```bash
git config core.hooksPath .githooks
```
This is set per-repo and should persist across container rebuilds.

## Fishy Terminal 🐟
When user says **"tell the fishy"** or **"fishy run"**, execute commands in the `🐟-fishy` emacs buffer:
```elisp
;; Send command to fishy
(mcp_emacs_emacs_send_keys buffer="🐟-fishy" keys="your-command-here\n")
;; Or use process-send-string for reliability
(process-send-string (get-process "fishy") "command\n")
```
- **Buffer name**: `🐟-fishy`
- **Tab name**: `fishy` (second tab after artery)
- **Use case**: Quick shell commands, file operations, git, npm, etc.
- Prefer fishy over `run_in_terminal` when user explicitly mentions it

## Emacs/Artery Platform Management

### Architecture
The aesthetic platform runs as:
1. **Emacs daemon** — background process managing multiple terminal tabs
2. **"💻 Aesthetic" VS Code task** — keeps emacsclient connected, displays the TUI
3. **Artery TUI** — interactive interface in the first emacs tab

### Starting the Platform
- **Normal start**: The "💻 Aesthetic" task auto-runs on folder open
- **Manual start**: Run the task from VS Code's Task menu or command palette
- The task runs `./aesthetic-launch.sh` which:
  1. Waits for `.waiter` file (created by `entry.fish` during container init)
  2. Ensures emacs daemon is ready
  3. Connects `emacsclient -nw -c` with `aesthetic-backend`

### Stopping the Platform
- **Stop task only**: Kill the "💻 Aesthetic" task in VS Code (emacs daemon stays running)
- **Full stop**: `pkill -9 emacs; pkill -9 emacsclient` or use `ac-emacs-kill`

### Emacs Crash Monitor (Auto-Recovery)
The platform automatically starts a crash monitor that detects when emacs dies:
- **Auto-restarts** emacs daemon with correct config (`-q -l emacs.el`)
- **Max 5 restarts** within 5 minutes before giving up
- **Logs to** `.emacs-logs/crashes.log`
- **Warning file** `/tmp/emacs-crash-warning` — created on auto-recovery

**CRITICAL**: The crash monitor MUST restart emacs with `emacs -q --daemon -l $config_path`
to load the custom config. Using just `emacs --daemon` loads `~/.emacs` which breaks everything.

### Restarting After Issues
If emacs MCP tools fail or the "💻 Aesthetic" task is frozen:
1. **Check health**: `ac-emacs-health-check` — verifies daemon AND correct config loaded
2. **Check status**: `ac-emacs-status` — is daemon running & responsive?
3. **If wrong config**: `ac-emacs-restart` — kills and restarts with correct config
4. **Restart task**: Must manually restart "💻 Aesthetic" task in VS Code
5. **If `.waiter` missing**: `touch /home/me/.waiter` before restarting task

### Quick Reference Commands (fish shell)
| Command | Description |
|---------|-------------|
| `ac-emacs-status` | Check daemon status (running/responsive) |
| `ac-emacs-status` | Check daemon status (running/responsive) |
| `ac-emacs-health-check` | Verify daemon + correct config loaded |
| `ac-emacs-restart` | Kill & restart emacs daemon |
| `ac-emacs-kill` | Kill all emacs processes |
| `ac-restart` | Full restart: daemon + reconnect artery |
| `check-daemon` | Alias for status check |
| `restart-daemon` | Alias for restart |

### Troubleshooting
- **Task shows "Configuring..." forever**: `.waiter` file missing — run `touch /home/me/.waiter`
- **Emacs unresponsive**: `ac-emacs-restart` then restart VS Code task (watchdog may auto-fix)
- **MCP tools hang**: Emacs daemon frozen — watchdog should auto-recover, else manual restart
- **Tabs not created**: `emacsclient` exited before timers completed — restart task
- **Wrong config loaded**: Run `ac-emacs-health-check` — if it shows "wrong config", run `ac-emacs-restart`

## MCP State Awareness 🔍
Before sending commands to emacs buffers, **always check state first** using these functions:

### Get Full State (recommended first call)
```elisp
(ac-mcp-format-state)
;; Returns: "Tab: artery | Buffer: 🩸-artery | Tabs: [artery, fishy, ...] | Terminals: 🐟-fishy(●), 🧪-kidlisp(○), ..."
;; ● = process running, ○ = process exited/dead
```

### Check Specific Buffer/Terminal
```elisp
(ac-mcp-get-eat-status "🐟-fishy")  ;; Returns: 'running, 'exited, 'not-eat, or 'not-found

(ac-mcp-get-buffer-state "🐟-fishy")
;; Returns: ((exists . t) (mode . "eat-mode") (process . "run") (alive . t) ...)
```

### Workflow Pattern
1. **Before sending keys**: Check `(ac-mcp-get-eat-status "buffer-name")` is `running`
2. **If exited**: Either restart process or inform user buffer is dead
3. **Common dead buffers**: `🧪-kidlisp`, `⚡-url`, `🚇-tunnel`, `🔖-bookmarks` often exit after tasks complete

### Restart Dead Fishy
```elisp
(ac-restart-fishy)  ;; Restarts fish process in 🐟-fishy buffer
```

## Prompt Completion Notification 🔔
**IMPORTANT**: At the END of every response, call this to flash the screen:
```elisp
(ac-notify-done)
```
This flashes the terminal 3 times so the user knows Copilot is done (useful when managing multiple machines).

## CDP Tunnel (VS Code on Windows Host)
The CDP tunnel forwards localhost:9333 to host's VS Code for remote control:
- **`ac-cdp-tunnel`** — start/restart the CDP SSH tunnel
- **`ac-cdp-status`** — check if tunnel is running and port accessible
- Artery TUI header shows CDP status (● online / ○ offline)

## KidLisp.com Development 🎨

### Quick Start (Artery TUI)
Press **`G`** in Artery TUI → **KidLisp Dev** mode:
1. Opens the KidLisp.com VS Code webview panel
2. Connects to the iframe via CDP WebSocket
3. Mirrors all `console.log`, `console.error`, etc. to Artery TUI
4. Live scrollable log viewer with color-coded levels

**Controls in KidLisp Dev mode:**
- `C` — Clear console
- `R` — Reconnect CDP if disconnected
- `O` — Re-open KidLisp window
- `↑/↓` or `J/K` — Scroll logs
- `Q` or `Esc` — Exit to main menu

### Key Files
- **Main**: [system/public/kidlisp.com/index.html](system/public/kidlisp.com/index.html)
- **JS modules**: `system/public/kidlisp.com/js/` (state, events, playback, logger)
- **CSS modules**: `system/public/kidlisp.com/css/` (modular styles)
- **Test suite**: [artery/test-kidlisp.mjs](artery/test-kidlisp.mjs)

### CDP Testing (Advanced)

When modifying `system/public/kidlisp.com/index.html`, test via Chrome DevTools Protocol:

#### Finding the KidLisp.com Page
```bash
# List available CDP targets (with Host header for container)
curl -s -H "Host: localhost" http://host.docker.internal:9222/json | jq '.[] | select(.url | contains("kidlisp.com"))'
```

Look for the iframe with `url` containing `kidlisp.com`. Note the `id` (e.g., `BDE027EA5BC03BEA659F08B7AAD3F31B`).

### Connecting to KidLisp.com
```javascript
const ws = new (require('ws'))('ws://host.docker.internal:9222/devtools/page/<PAGE_ID>', {
  headers: { Host: 'localhost' }
});
```

### Test Pattern for Card Interactions
```javascript
// Evaluate JS in the kidlisp.com context
const evalJS = async code => {
  const res = await send({method:'Runtime.evaluate', params:{expression:code}});
  return res.result?.value;
};

// Card counts
await evalJS('document.querySelectorAll(".book-frame").length');   // main stack
await evalJS('document.querySelectorAll(".discard-card").length'); // discard pile

// Trigger interactions
await evalJS('document.querySelector(".book-stack")?.click()');    // advance card
await evalJS('document.querySelector(".discard-pile")?.click()');  // undo/reset
```

### Artery Test Scripts
- `node artery/test-kidlisp.mjs` — Full test suite
- `node artery/kidlisp-util.mjs help` — Utility commands
- `node artery/inspect-cards.mjs` — Card system inspection

### KidLisp.com Key Files
- **Main**: `system/public/kidlisp.com/index.html` — Editor + card deck
- **Cards SVG**: `system/public/kidlisp.com/cards/svg/` — LaTeX-generated cards
- **Styles**: Inline `<style>` in index.html

## Development

### URLs
- **Local**: `https://localhost:8888`
- **Pieces**: `https://localhost:8888/piece-name`
- **Params**: `https://localhost:8888/piece:param0:param1~flag0~flag1`

### Core Directories
- `system/public/aesthetic.computer/disks/` — pieces (`.mjs` or `.lisp`)
- `system/public/aesthetic.computer/lib/` — shared libraries
- `kidlisp/` — KidLisp interpreter
- `nanos/` — backend serverless functions

### Piece Template (JavaScript)
```javascript
function boot({ api, params }) { /* init */ }
function paint({ wipe, ink, box, screen }) {
  wipe(0, 50, 100);
  ink(255, 255, 0).box(10, 10, 50, 50);
}
function sim() { /* logic */ }
function act({ event: e }) { if (e.is("touch")) { } }
function meta() { return { title: "Name", desc: "Desc" }; }
export { boot, paint, sim, act, meta };
```

### KidLisp Basics
```lisp
(wipe "black")              ; clear screen
(ink "red")                 ; set color
(line 0 0 width height)     ; draw line
(box x y w h)               ; draw rectangle
(def x 10)                  ; define variable
(repeat 5 i (line i*10 0 i*10 height))  ; loop
(tap (ink "yellow") (box 50 50 100 100)) ; click handler
(0.5s (zoom 0.97))          ; timed execution
```

Key KidLisp: `wipe`, `ink`, `line`, `box`, `def`, `now`, `repeat`, `later`, `tap`, `draw`, `wiggle`, `rainbow`, `frame`, `width`, `height`
