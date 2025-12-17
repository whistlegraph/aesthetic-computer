# Aesthetic Computer

**Environment:** Devcontainer in `/workspaces/aesthetic-computer` monorepo (Fedora Linux).

## Key Paths
- **Vault:** `aesthetic-computer-vault/` — credentials, machine configs. **Never copy secrets.**
- **Artery TUI:** `artery/artery-tui.mjs` — main interactive interface, run via Emacs MCP
- **KidLisp:** `kidlisp/` (interpreter), `system/public/aesthetic.computer/disks/` (pieces)
- **Tezos:** `tezos/keeps.mjs` — NFT contract CLI

## Workflow
1. Run commands through **Emacs MCP** (`mcp_emacs_*` tools) when possible
2. Artery TUI is the primary control interface for AC
3. Check `vault/machines.json` to identify current machine context
4. Dev server: `./aesthetic-launch.sh` (usually auto-running)

## Rules
- Keep responses concise
- Use existing tooling (artery, emacs-mcp) over raw terminal
- Never expose or copy vault secrets
- **NEVER** call `get_task_output` on the "💻 Aesthetic" task — it freezes the devcontainer

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

### Emacs Watchdog (Auto-Recovery)
The platform automatically starts a watchdog process that monitors emacs health:
- **Detects unresponsive daemons** — restarts if `emacsclient` times out
- **Detects sustained high CPU** — restarts if CPU > 85% for 30+ seconds (3 checks)
- **Logs to** `.emacs-logs/watchdog.log`
- **Warning file** `/tmp/emacs-watchdog-warning` — created on auto-recovery

| Command | Description |
|---------|-------------|
| `ac-watchdog-status` | Check emacs + watchdog status |
| `ac-watchdog-start` | Manually start watchdog |
| `ac-watchdog-stop` | Stop the watchdog |
| `ac-watchdog-logs` | Tail watchdog logs |
| `ac-watchdog-ack` | Clear recovery warning |

### Restarting After Issues
If emacs MCP tools fail or the "💻 Aesthetic" task is frozen:
1. **Check status**: `ac-emacs-status` — is daemon running & responsive?
2. **If unresponsive**: `ac-emacs-restart` — kills zombie emacs, starts fresh daemon
3. **Restart task**: Must manually restart "💻 Aesthetic" task in VS Code
4. **If `.waiter` missing**: `touch /home/me/.waiter` before restarting task

### Quick Reference Commands (fish shell)
| Command | Description |
|---------|-------------|
| `ac-emacs-status` | Check daemon status (running/responsive) |
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

## CDP Tunnel (VS Code on Windows Host)
The CDP tunnel forwards localhost:9333 to host's VS Code for remote control:
- **`ac-cdp-tunnel`** — start/restart the CDP SSH tunnel
- **`ac-cdp-status`** — check if tunnel is running and port accessible
- Artery TUI header shows CDP status (● online / ○ offline)

## KidLisp.com Development via CDP

When modifying `system/public/kidlisp.com/index.html`, test via Chrome DevTools Protocol:

### Finding the KidLisp.com Page
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
