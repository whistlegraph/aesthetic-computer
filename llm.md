# Aesthetic Computer - Architecture Reference for LLMs

> **Last Updated:** 2025-12-07  
> **Purpose:** Comprehensive guide for AI assistants working with this codebase

---

## 🎯 Project Overview

**Aesthetic Computer (AC)** is a creative coding platform that runs in the browser, featuring:
- A **piece-based architecture** (small interactive programs called "pieces")
- **KidLisp** - a Lisp-based visual programming language
- **WebGPU rendering** with 2D/3D graphics
- **Real-time collaboration** and multiplayer support
- **Cross-platform** deployment (web, iOS, Android, desktop)

---

## 🏗️ Core Architecture

### System Layers

```
┌─────────────────────────────────────────────────────────────────────┐
│                        VS Code / Emacs Frontend                      │
│                    (development environment)                         │
├─────────────────────────────────────────────────────────────────────┤
│  🩸 Artery (CDP)  │  🧠 Emacs MCP  │  🌐 Browser (localhost:8888)  │
├─────────────────────────────────────────────────────────────────────┤
│                         Aesthetic Computer Core                       │
│   /system         - Main web application (Netlify Functions)         │
│   /system/public  - Static frontend assets                           │
│   /kidlisp        - KidLisp language runtime                         │
├─────────────────────────────────────────────────────────────────────┤
│                         Infrastructure                                │
│   Redis │ Session Server │ Stripe │ Cloudflare Tunnel │ Chat Bots   │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 📁 Directory Structure

### Top-Level Directories

| Directory | Purpose |
|-----------|---------|
| `/system` | Main web app - Netlify dev server, functions, frontend |
| `/kidlisp` | KidLisp language: parser, interpreter, pieces |
| `/kidlisp.com` | KidLisp.com website (separate site) |
| `/artery` | 🩸 CDP-based control interface for AC |
| `/dotfiles` | Configuration files (Emacs, shell, etc.) |
| `/vscode-extension` | VS Code extension for AC panel |
| `/aesthetic-computer-vault` | Private pieces and secrets |

### Key Files

| File | Purpose |
|------|---------|
| `/dotfiles/dot_config/emacs.el` | Emacs configuration with `aesthetic-backend` |
| `/.devcontainer/config.fish` | Fish shell with 50+ `ac-*` functions |
| `/artery/artery.mjs` | CDP connection library |
| `/artery/artery-tui.mjs` | Interactive TUI for AC control |
| `/artery/emacs-mcp.mjs` | MCP server for VS Code ↔ Emacs |
| `/.vscode/mcp.json` | MCP server configuration |

---

## 🧠 Emacs Integration

### The `aesthetic-backend` Function

Located in `/dotfiles/dot_config/emacs.el`, this is the main entry point for the development environment:

```elisp
(defun aesthetic-backend (target-tab)
  "Creates the tabbed development environment with eat terminals"
  ...)
```

**Creates tabs:**
1. `artery` → 🩸-artery buffer (runs `ac-artery-dev`)
2. `status` → ⚡-url, 🚇-tunnel
3. `stripe` → 💳-stripe-print, 🎫-stripe-ticket  
4. `chat` → 🤖-chat-system, 🧠-chat-sotce, ⏰-chat-clock
5. `web 1/2` → 🌐-site, 📋-session
6. `web 2/2` → 🔴-redis, 🔖-bookmarks, 🔥-oven
7. `tests` → 🧪-kidlisp

**Emoji to command mapping:**
```elisp
'(("artery" . "🩸") ("url" . "⚡") ("tunnel" . "🚇") 
  ("stripe-print" . "💳") ("stripe-ticket" . "🎫")
  ("chat-system" . "🤖") ("chat-sotce" . "🧠") ("chat-clock" . "⏰")
  ("site" . "🌐") ("session" . "📋") ("redis" . "🔴") 
  ("bookmarks" . "🔖") ("kidlisp" . "🧪") ("oven" . "🔥"))
```

### Starting the Environment

```bash
# From fish shell
aesthetic          # Waits for config, starts Emacs daemon, opens aesthetic-backend
aesthetic-direct   # Skips wait, goes straight to aesthetic-backend
platform           # Quick reconnect to existing daemon
```

---

## 🩸 Artery System

### What is Artery?

**Artery** is a **Chrome DevTools Protocol (CDP)** bridge that allows controlling the AC browser instance programmatically. It connects to VS Code's embedded Chromium via port 9222/9224.

### Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│ VS Code (Electron) ─────────────── CDP Port 9222 ──────────────────│
│     │                                     │                        │
│     ├── Workbench (main page)             │                        │
│     └── AC Panel (iframe)  ◄──────────────┼─── Artery Connection   │
│            └── localhost:8888             │                        │
└───────────────────────────────────────────┴────────────────────────┘
```

### CLI Commands

```bash
# Basic commands
artery              # Show help
artery panel        # Open AC sidebar panel
artery jump prompt  # Navigate to piece
artery current      # Show current piece
artery repl         # Interactive REPL with console streaming

# Emacs integration
artery emacs              # Check Emacs connection
artery emacs "(version)"  # Execute elisp
artery emacs-buffers      # List Emacs buffers

# Split/Multiplayer
artery frames             # List all AC frames
artery player 1           # Connect to Player 1 (top split)
artery player 2           # Connect to Player 2 (bottom split)

# KidLisp.com
artery kidlisp            # Open KidLisp.com window in VS Code
artery kidlisp-test       # Run all 26 KidLisp tests
artery kidlisp-test basic # Run specific test suite
                          # Suites: basic, editor, playback, ui, console, examples, errors

# VS Code Utilities
artery close-editors      # Close all VS Code editor tabs

# Testing
artery hiphop             # Hip-hop beat generator test
artery 1v1                # Split-screen 1v1 test
artery perf 10            # Performance monitor (10 seconds)
```

### 🌸 Poppy (Default Log Capture)

Use **Poppy** when you need runtime console logs. It opens the AC panel,
jumps to the piece, and streams logs until you stop it.

```bash
node artery/poppy.mjs <piece>

# Examples
node artery/poppy.mjs notepat
node artery/poppy.mjs prompt
```

**Default behavior:** Prefer Poppy for debugging instead of asking for manual
copy/paste of logs.

### TUI Mode (`artery-tui.mjs`)

Interactive curses-style interface with:
- `[p]` Open AC Panel
- `[j]` Jump to piece
- `[r]` Enter REPL
- `[e]` Emacs mode
- `[t]` Performance test
- `[s]` Split-screen mode

### KidLisp Test Suite (`test-kidlisp.mjs`)

Automated test harness for KidLisp.com with 26 tests using direct CDP connection:

```bash
artery kidlisp-test              # Run all tests
artery kidlisp-test basic        # Run basic suite only
artery kidlisp-test editor       # Test editor operations
artery kidlisp-test playback     # Test play/stop/clear
artery kidlisp-test ui           # Test buttons and theme
artery kidlisp-test console      # Test console output
artery kidlisp-test examples     # Test loading examples
artery kidlisp-test errors       # Test error handling
```

**Test Categories:**
| Suite | Tests |
|-------|-------|
| basic | Connection, disk state, API availability |
| editor | Code insertion, selection, clear operations |
| playback | Play, stop, clear, state transitions |
| ui | Theme toggle, example buttons, console visibility |
| console | Log output, clear console, display |
| examples | Loading spiral, bounce, ripple examples |
| errors | Syntax errors, undefined symbols, error recovery |

### CDP Keyboard Simulation

Artery uses `Input.dispatchKeyEvent` CDP method to simulate VS Code keyboard shortcuts:

```javascript
// Example: Send Ctrl+K, W chord to close all editors
send('Input.dispatchKeyEvent', {
  type: 'keyDown',
  modifiers: 2, // Ctrl
  key: 'k',
  code: 'KeyK',
  windowsVirtualKeyCode: 75
});
```

This allows invoking VS Code commands without needing an open webview.

---

## 🔌 MCP (Model Context Protocol) Integration

### What is MCP?

MCP allows VS Code Copilot Chat to call external tools. We have a custom Node.js MCP server that bridges Copilot ↔ Emacs.

### Configuration (`.vscode/mcp.json`)

```json
{
  "servers": {
    "emacs": {
      "type": "stdio",
      "command": "node",
      "args": ["/workspaces/aesthetic-computer/artery/emacs-mcp.mjs"],
      "env": {
        "EMACSCLIENT": "/usr/sbin/emacsclient"
      }
    }
  }
}
```

### Available MCP Tools

| Tool | Description |
|------|-------------|
| `mcp_emacs_execute_emacs_lisp` | Execute arbitrary elisp code |
| `mcp_emacs_emacs_list_buffers` | List all open buffers |
| `mcp_emacs_emacs_switch_buffer` | Switch to a buffer |
| `mcp_emacs_emacs_send_keys` | Send keystrokes to eat terminal |
| `mcp_emacs_emacs_get_buffer_content` | Read buffer content |

### How It Works

```
┌──────────────────────────────────────────────────────────────────┐
│  VS Code Copilot Chat                                            │
│        │                                                         │
│        ▼                                                         │
│  mcp.json → node emacs-mcp.mjs (stdio JSON-RPC)                 │
│        │                                                         │
│        ▼                                                         │
│  emacsclient --eval "(elisp code)"                              │
│        │                                                         │
│        ▼                                                         │
│  Emacs Daemon (eat terminals running fish → ac-* commands)       │
└──────────────────────────────────────────────────────────────────┘
```

---

## 🐟 Fish Shell Commands

### Core AC Commands

| Command | Description |
|---------|-------------|
| `ac` | cd to AC root, or `ac piece-name` to jump |
| `ac-site` | Start main dev server (Netlify + esbuild) |
| `ac-session` | Session server |
| `ac-url` | URL generation / shortening |
| `ac-tunnel` | Cloudflare tunnel for public access |
| `ac-artery` | Interactive artery TUI |
| `ac-artery-dev` | Artery TUI with hot reload |

### Recording & Packaging

| Command | Description |
|---------|-------------|
| `ac-record $piece` | Record MP4/GIF of a piece |
| `ac-pack $piece` | Package for Teia (IPFS art platform) |
| `ac-keep $piece` | Create self-contained HTML bundle |
| `ac-ship` | Package as Electron desktop app |

### Testing

| Command | Description |
|---------|-------------|
| `test-notepat` | Automated notepat testing |
| `test-line` | Line drawing test |
| `test-melody` | Music generation test |

### KidLisp

| Command | Description |
|---------|-------------|
| `st $piece` | Show source tree for a piece |
| `kidlisp` | KidLisp probe for kidlisp.com |

---

## 🎮 Piece System

### What is a Piece?

A "piece" is a self-contained interactive program. Each piece has:
- A main JavaScript/KidLisp file
- Optional boot, act, sim, paint, beat functions
- Access to the AC graphics/sound API

### Piece Locations

```
/system/public/aesthetic.computer/disks/   # Main pieces
/kidlisp/pieces/                            # KidLisp pieces
/aesthetic-computer-vault/                  # Private pieces
```

### Key Pieces

| Piece | Description |
|-------|-------------|
| `prompt` | Default landing - command prompt interface |
| `notepat` | Music pattern sequencer |
| `line` | Drawing tool |
| `wand` | 3D wand visualization |
| `bios` | System configuration |

---

## 🌐 Development Server

### URLs

| URL | Service |
|-----|---------|
| `https://localhost:8888` | Main AC site |
| `https://localhost:8889` | Jump/control API |
| `http://localhost:3000` | Session server |
| Port 9222/9224 | CDP (Chrome DevTools Protocol) |

### Starting Development

```bash
# Option 1: Full environment via Emacs
aesthetic

# Option 2: Individual services
ac-site        # Main server
ac-session     # Session server  
ac-tunnel      # Public tunnel
ac-artery-dev  # Artery with hot reload
```

---

## 🔧 Common Development Tasks

### Jump to a Piece

```bash
# Via fish
ac prompt
ac notepat

# Via artery CLI
artery jump prompt

# Via artery TUI
ac-artery → [j] → type piece name

# Via MCP (from Copilot Chat)
# Use mcp_emacs_emacs_send_keys to send to artery buffer
```

### Open AC Panel in VS Code

```bash
# Via artery
artery panel

# Via TUI
ac-artery → [p]
```

### Monitor Console Logs

```bash
# Via artery REPL (streams live)
artery repl

# Via TUI
ac-artery → [r]
```

### Control from Emacs

```elisp
;; Switch buffers
(switch-to-buffer "🩸-artery")

;; Send keys to eat terminal
(with-current-buffer "🩸-artery"
  (eat-term-send-string eat-terminal "prompt\n"))
```

---

## 🧪 Testing

### Artery Tests

```bash
artery hiphop     # Music generation
artery trapwaltz  # 3/4 time + trap
artery 1v1        # Split-screen multiplayer
```

### Unit Tests

```bash
# Via fish
test-notepat
test-line
test-melody

# These use artery internally to control AC
```

---

## 🐛 Debugging Tips

### Check Emacs Daemon

```bash
check-daemon       # Status check
restart-daemon     # Force restart
```

### Check Artery Connection

```bash
artery             # Shows help if connected
artery frames      # Lists all AC iframes
```

### View Logs

```bash
# Emacs debug log
cat /tmp/emacs-debug.log

# AC site logs
# Look in the 🌐-site buffer in Emacs
```

### CDP Issues

If artery can't connect:
1. Make sure AC panel is open in VS Code
2. Check `host.docker.internal` resolves (Docker Desktop)
3. On Linux, socat may need to forward port 9222→9224

---

## 📚 Key Concepts

### eat Terminal

Emacs Application Terminal - runs fish shell inside Emacs buffers. Each tab in `aesthetic-backend` is an eat terminal running an `ac-*` command.

### CDP (Chrome DevTools Protocol)

Protocol for browser automation. Artery uses it to:
- Find AC iframe targets
- Execute JavaScript in browser context
- Capture console logs
- Send input events

### MCP (Model Context Protocol)

Anthropic's protocol for AI tool calling. Our `emacs-mcp.mjs` server exposes Emacs operations to VS Code Copilot Chat via JSON-RPC over stdio.

---

## 🚀 Quick Reference

### Start Development
```bash
aesthetic                    # Full environment
```

### Control AC
```bash
artery repl                  # Interactive REPL
ac-artery                    # TUI interface
```

### From VS Code Copilot
```
# Available MCP tools:
- mcp_emacs_execute_emacs_lisp
- mcp_emacs_emacs_list_buffers
- mcp_emacs_emacs_switch_buffer
- mcp_emacs_emacs_send_keys
- mcp_emacs_emacs_get_buffer_content
```

---

## 📝 Notes for AI Assistants

1. **Buffer Naming**: Emacs buffers use emoji prefixes like `🩸-artery`, `🌐-site`
2. **eat Terminals**: Use `eat-term-send-string` to send commands to fish
3. **Artery vs MCP**: Artery controls AC browser; MCP controls Emacs
4. **Fish Shell**: User's shell - no heredocs, use printf/echo instead
5. **Devcontainer**: Running in Docker on Fedora Linux
6. **Port Mapping**: CDP on 9222 (host) may map to 9224 (container via socat)

---

*This document serves as an LLM-readable map of the Aesthetic Computer development environment.*
