# AC Electron App Plan

## Executive Summary

A single **Aesthetic Computer Electron App** with two modes:

1. **Production Mode** (default) — Clean, user-facing AC experience pointing to `aesthetic.computer`
2. **Development Mode** — Full dev environment with terminal, artery-tui, AI assistants, localhost

Users launch into Production mode by default. Developers can unlock Dev mode via a hotkey or hidden menu.

---

## Dual-Mode Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        AC Electron App                                       │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │                    MODE SWITCHER                                    │    │
│  │         [Production]  ←→  [Development]                             │    │
│  │         Cmd+Shift+D or Menu → "Enter Dev Mode"                      │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│                                                                             │
├─────────────────────────────────┬───────────────────────────────────────────┤
│                                 │                                           │
│   🎨 PRODUCTION MODE (default)  │   🔧 DEVELOPMENT MODE                     │
│                                 │                                           │
│   • Full-window AC webview      │   • Split panes (webview + terminal)      │
│   • https://aesthetic.computer  │   • http://localhost:8888                 │
│   • Minimal chrome              │   • artery-tui in devcontainer            │
│   • No terminal                 │   • AI assistant panel                    │
│   • No dev tools by default     │   • DevTools access                       │
│   • Auto-update prompts         │   • Log viewer, test runner               │
│                                 │                                           │
│   Target: End users, artists    │   Target: AC developers                   │
│                                 │                                           │
└─────────────────────────────────┴───────────────────────────────────────────┘
```

---

## Mode Details

### Production Mode (Default)

**What users see:**
```
┌─────────────────────────────────────────────────────────────────┐
│ ◀ ▶  aesthetic.computer/prompt                    ─ □ ✕        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│                                                                 │
│                    [ AC WebView ]                               │
│                                                                 │
│              https://aesthetic.computer/prompt                  │
│                                                                 │
│                                                                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Features:**
- Clean, borderless webview (or minimal titlebar)
- URL bar for navigation between pieces
- Back/forward buttons
- Fullscreen support (F11 or Cmd+Ctrl+F)
- Native menu bar with standard items
- "About AC" dialog with version info
- Auto-update notifications

**Hidden dev mode trigger:**
- `Cmd+Shift+D` (Mac) / `Ctrl+Shift+D` (Win/Linux)
- Or: Menu → View → "Enter Development Mode..."
- Or: CLI flag `--dev` or `--development`
- Prompts: "Development mode requires Docker. Continue?"

**Command Line Usage:**
```bash
# Launch in production mode (default)
./AC.app/Contents/MacOS/AC
# or just double-click the app

# Launch directly into dev mode
./AC.app/Contents/MacOS/AC --dev

# Launch with specific piece
./AC.app/Contents/MacOS/AC --piece prompt
./AC.app/Contents/MacOS/AC --dev --piece wand
```

### Development Mode

**What developers see:**
```
┌─────────────────────────────────────────────────────────────────────────────┐
│ AC Dev                                    [Production ↔ Dev]    ─ □ ✕       │
├────────────────────────────────────┬────────────────────────────────────────┤
│                                    │                                        │
│   ┌────────────────────────────┐   │   ┌────────────────────────────────┐   │
│   │                            │   │   │      AI Assistant              │   │
│   │   AC WebView (localhost)   │   │   │                                │   │
│   │                            │   │   │  > How do I add a new piece?   │   │
│   │   http://localhost:8888    │   │   │                                │   │
│   │                            │   │   │  Claude: To create a new       │   │
│   │                            │   │   │  piece, run...                 │   │
│   └────────────────────────────┘   │   │                                │   │
│                                    │   └────────────────────────────────┘   │
├────────────────────────────────────┴────────────────────────────────────────┤
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │  artery-tui (devcontainer)                                            │ │
│  │  ╔═══════════════════════════════════════════════════════════════╗    │ │
│  │  ║ ▣ AESTHETIC COMPUTER                    ● L● P● CDP●         ║    │ │
│  │  ╠═══════════════════════════════════════════════════════════════╣    │ │
│  │  ║ [R]un [T]est [L]ogs [W]atch [B]uild [Q]uit                   ║    │ │
│  │  ╚═══════════════════════════════════════════════════════════════╝    │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
```

**Features:**
- Everything from the original dev plan
- "Return to Production" button/hotkey
- Remembers last layout

---

## Mode Switching Implementation

**main.js:**
```javascript
import { app, BrowserWindow, ipcMain, globalShortcut } from 'electron';
import Store from 'electron-store';

const store = new Store();
let mainWindow;
let currentMode = 'production'; // or 'development'

const URLS = {
  production: 'https://aesthetic.computer',
  development: 'http://localhost:8888'
};

function createWindow() {
  mainWindow = new BrowserWindow({
    width: currentMode === 'production' ? 1280 : 1600,
    height: currentMode === 'production' ? 800 : 1000,
    titleBarStyle: currentMode === 'production' ? 'hiddenInset' : 'default',
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      nodeIntegration: false,
      contextIsolation: true,
    }
  });

  if (currentMode === 'production') {
    loadProductionMode();
  } else {
    loadDevelopmentMode();
  }
}

function loadProductionMode() {
  currentMode = 'production';
  // Load simple webview-only renderer
  mainWindow.loadFile('renderer/production.html');
  // Hide dev-only menu items
  updateMenuForMode('production');
}

async function loadDevelopmentMode() {
  // Check if Docker is available
  const dockerAvailable = await checkDocker();
  if (!dockerAvailable) {
    dialog.showErrorBox(
      'Docker Required',
      'Development mode requires Docker Desktop to be running.\n\n' +
      'Please start Docker and try again.'
    );
    return;
  }

  currentMode = 'development';
  // Load full dev renderer with terminal panes
  mainWindow.loadFile('renderer/development.html');
  updateMenuForMode('development');
  
  // Start/connect to devcontainer
  await ensureDevcontainer();
}

function toggleMode() {
  if (currentMode === 'production') {
    loadDevelopmentMode();
  } else {
    loadProductionMode();
  }
}

// Register hotkey
app.whenReady().then(() => {
  globalShortcut.register('CommandOrControl+Shift+D', toggleMode);
  createWindow();
});

// IPC handler for renderer
ipcMain.handle('switch-mode', (event, mode) => {
  if (mode === 'development') {
    loadDevelopmentMode();
  } else {
    loadProductionMode();
  }
});
```

---

## App Distribution Strategy

### Single App, Dual Purpose

| Aspect | Decision |
|--------|----------|
| **App name** | "Aesthetic Computer" (not "AC Dev") |
| **Bundle ID** | `computer.aesthetic.app` |
| **Default mode** | Production |
| **Dev mode** | Hidden but accessible |
| **Target audience** | Everyone (users) + Developers (with Docker) |

### Why Not Two Separate Apps?

1. **Simpler distribution** — One download, one update stream
2. **Dogfooding** — Developers use the same app as users
3. **Quick testing** — Switch to production to verify deployed changes
4. **Smaller footprint** — Dev dependencies only load when needed

---

## Current State Analysis

### Existing Infrastructure

| Component | Location | Purpose |
|-----------|----------|---------|
| **Devcontainer** | `.devcontainer/` | Fedora-based container with all deps (Node, Deno, Emacs, Python, etc.) |
| **artery-tui** | `artery/artery-tui.mjs` | 7200+ LOC terminal UI for server control, logs, testing, CDP integration |
| **Tauri Overlay** | `ac-event-daemon/` | Rust/Tauri transparent notification overlays |
| **Dark Window** | `dark-window/` | Remote Chromium conductor for cloud debugging |
| **start-devcontainer.sh** | Root | CLI script for `devcontainer up/exec` without VSCode |

### Pain Points with Current VSCode Setup
1. **Heavy IDE overhead** - VSCode runs inside/alongside container, consumes resources
2. **Context switching** - Multiple windows: VSCode, browser preview, terminal
3. **Extension bloat** - Extensions don't always play nice with devcontainers
4. **Port forwarding friction** - Manual port management for preview URLs
5. **No native AC integration** - Preview is always a separate browser tab

### Advantages of Current Setup
1. **VSCode's debugging** - Breakpoints, variable inspection work well
2. **Extensions ecosystem** - Git, Copilot, Prettier, etc.
3. **Familiar** - Most devs know VSCode

---

## Proposed Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                     AC Electron Development App                              │
├────────────────────────────────────┬────────────────────────────────────────┤
│                                    │                                        │
│     ┌─────────────────────────┐    │    ┌─────────────────────────────┐     │
│     │                         │    │    │       AI Assistant          │     │
│     │   AC WebView Preview    │    │    │  ┌─────────────────────┐   │     │
│     │                         │    │    │  │ Claude Code / Copilot│   │     │
│     │   • Live reload         │    │    │  │ CLI integration      │   │     │
│     │   • DevTools access     │    │    │  └─────────────────────┘   │     │
│     │   • Piece switching     │    │    │                            │     │
│     │   • Touch simulation    │    │    │   Chat / Command input     │     │
│     │                         │    │    │                            │     │
│     └─────────────────────────┘    │    └─────────────────────────────┘     │
│                                    │                                        │
├────────────────────────────────────┴────────────────────────────────────────┤
│                                                                             │
│    ┌───────────────────────────────────────────────────────────────────┐    │
│    │                     Embedded Terminal                             │    │
│    │                                                                   │    │
│    │   ┌───────────────────────────────────────────────────────────┐   │    │
│    │   │  artery-tui (runs in devcontainer via docker exec)        │   │    │
│    │   │                                                           │   │    │
│    │   │  ╔═══════════════════════════════════════════════════╗    │   │    │
│    │   │  ║ ▣ AESTHETIC COMPUTER          ● L● P● CDP●       ║    │   │    │
│    │   │  ╠═══════════════════════════════════════════════════╣    │   │    │
│    │   │  ║ [R]un [T]est [L]ogs [W]atch [B]uild [Q]uit       ║    │   │    │
│    │   │  ╠═══════════════════════════════════════════════════╣    │   │    │
│    │   │  ║ 12:34 Server started on :8888                    ║    │   │    │
│    │   │  ║ 12:34 WebSocket connected                        ║    │   │    │
│    │   │  ╚═══════════════════════════════════════════════════╝    │   │    │
│    │   └───────────────────────────────────────────────────────────┘   │    │
│    └───────────────────────────────────────────────────────────────────┘    │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Core Components

### 1. Electron Shell (`ac-electron/`)

**Tech Stack:**
- Electron 28+ (Chromium 120+)
- xterm.js for terminal emulation
- node-pty for PTY communication
- electron-builder for packaging

**Key Features:**
- **Frameless/custom titlebar** - AC branded, minimal chrome
- **Multiple panes** - Resizable split views
- **Dark theme** - Consistent with AC aesthetic
- **Hotkey-driven** - vim-like navigation

### 2. WebView Panel

**Implementation:**
```javascript
// Main process creates BrowserView
const acView = new BrowserView({
  webPreferences: {
    nodeIntegration: false,
    contextIsolation: true,
    sandbox: true,
  }
});

// Load local dev server or production
acView.webContents.loadURL('http://localhost:8888');
```

**Features:**
- Live preview of AC running in devcontainer
- DevTools accessible via hotkey (Cmd+Shift+I)
- URL bar for piece switching (`/prompt`, `/wand`, etc.)
- Touch/gesture simulation for mobile testing
- Screenshot/recording for debugging

### 3. Terminal Panel (artery-tui in container)

**Implementation:**
```javascript
import { spawn } from 'node-pty';

// Exec into running container and launch artery-tui
const shell = spawn('docker', [
  'exec', '-it', 'aesthetic',
  'fish', '-c', 'cd /workspaces/aesthetic-computer && node artery/artery-tui.mjs'
], {
  name: 'xterm-256color',
  cols: 120,
  rows: 40,
});
```

**Features:**
- Full ANSI/256-color/sixel support via xterm.js
- Auto-reconnect on container restart
- Multiple terminals (tabs)
- Fish shell fallback when artery-tui exits

### 4. AI Assistant Integration

**Option A: Claude Code CLI**
```javascript
// Spawn Claude Code in a dedicated terminal pane
const claude = spawn('claude', [], {
  cwd: '/workspaces/aesthetic-computer',
  env: { ...process.env, ANTHROPIC_API_KEY: apiKey }
});
```

**Option B: GitHub Copilot CLI**
```javascript
// Use gh copilot suggest/explain
const copilot = spawn('gh', ['copilot', 'suggest', '-t', 'shell'], {
  cwd: workspacePath
});
```

**Option C: Custom Chat Panel**
- Direct API integration with Claude/GPT
- Context from current file, logs, artery state
- Inline code suggestions
- "Ask about this error" from log viewer

---

## Development Phases

### Phase 1: Minimal Viable Electron Shell (2-3 days)

**Deliverables:**
- [ ] Basic Electron app with custom titlebar
- [ ] WebView loading `localhost:8888`
- [ ] Single terminal pane running `docker exec` → artery-tui
- [ ] Basic hotkey support (Cmd+1 = WebView, Cmd+2 = Terminal)

**File Structure:**
```
ac-electron/
├── package.json
├── main.js           # Electron main process
├── preload.js        # Secure bridge
├── renderer/
│   ├── index.html
│   ├── styles.css
│   └── app.js
├── src/
│   ├── terminal.js   # xterm.js + node-pty wrapper
│   ├── webview.js    # AC preview management
│   └── ipc.js        # IPC handlers
└── scripts/
    └── start-container.js  # devcontainer up/exec logic
```

### Phase 2: Multi-Pane Layout (1-2 days)

**Deliverables:**
- [ ] Resizable split panes (horizontal/vertical)
- [ ] Layout persistence
- [ ] Pane maximization (double-click or hotkey)
- [ ] Status bar with container status, ports, branch

### Phase 3: AI Assistant Panel (2-3 days)

**Deliverables:**
- [ ] Dedicated pane for AI chat
- [ ] Claude Code CLI integration (spawn in terminal)
- [ ] Copilot CLI fallback
- [ ] Context injection (current logs, errors)

### Phase 4: Developer Experience Polish (2-3 days)

**Deliverables:**
- [ ] Cmd+K command palette
- [ ] Quick piece switcher
- [ ] Log search/filter
- [ ] Theme switching (light/dark/system)
- [ ] Auto-update mechanism

### Phase 5: Packaging & Distribution (1-2 days)

**Deliverables:**
- [ ] macOS .dmg (Apple Silicon + Intel universal binary)
- [ ] Windows NSIS installer + portable .exe
- [ ] Linux AppImage + .deb + .rpm
- [ ] Auto-update via electron-updater
- [ ] Code signing (macOS notarization, Windows Authenticode)

---

## Cross-Platform Build & Auto-Update

### electron-builder Configuration

**package.json:**
```json
{
  "name": "ac-dev",
  "version": "0.1.0",
  "main": "main.js",
  "scripts": {
    "start": "electron .",
    "build": "electron-builder --mac --win --linux",
    "build:mac": "electron-builder --mac --universal",
    "build:win": "electron-builder --win --x64",
    "build:linux": "electron-builder --linux"
  },
  "build": {
    "appId": "computer.aesthetic.dev",
    "productName": "AC Dev",
    "copyright": "Copyright © 2025 Aesthetic Computer",
    "directories": {
      "output": "dist",
      "buildResources": "build"
    },
    "files": [
      "main.js",
      "preload.js",
      "renderer/**/*",
      "src/**/*",
      "node_modules/**/*"
    ],
    "mac": {
      "category": "public.app-category.developer-tools",
      "target": [
        { "target": "dmg", "arch": ["universal"] },
        { "target": "zip", "arch": ["universal"] }
      ],
      "hardenedRuntime": true,
      "gatekeeperAssess": false,
      "entitlements": "build/entitlements.mac.plist",
      "entitlementsInherit": "build/entitlements.mac.plist",
      "notarize": {
        "teamId": "YOUR_TEAM_ID"
      }
    },
    "win": {
      "target": [
        { "target": "nsis", "arch": ["x64"] },
        { "target": "portable", "arch": ["x64"] }
      ],
      "sign": "./scripts/sign-windows.js"
    },
    "linux": {
      "target": ["AppImage", "deb", "rpm"],
      "category": "Development",
      "maintainer": "hi@aesthetic.computer"
    },
    "publish": {
      "provider": "github",
      "owner": "aesthetic-computer",
      "repo": "ac-dev",
      "releaseType": "release"
    }
  },
  "devDependencies": {
    "electron": "^33.0.0",
    "electron-builder": "^25.0.0"
  },
  "dependencies": {
    "electron-updater": "^6.3.0",
    "xterm": "^5.3.0",
    "xterm-addon-fit": "^0.8.0",
    "xterm-addon-webgl": "^0.16.0",
    "node-pty": "^1.0.0"
  }
}
```

### Auto-Updater Implementation

**main.js:**
```javascript
import { app, BrowserWindow, dialog } from 'electron';
import { autoUpdater } from 'electron-updater';
import log from 'electron-log';

// Configure logging
autoUpdater.logger = log;
autoUpdater.logger.transports.file.level = 'info';

// Disable auto-download, let user decide
autoUpdater.autoDownload = false;
autoUpdater.autoInstallOnAppQuit = true;

function setupAutoUpdater(mainWindow) {
  // Check for updates on startup (after 3s delay)
  setTimeout(() => {
    autoUpdater.checkForUpdates();
  }, 3000);

  // Also check every 4 hours
  setInterval(() => {
    autoUpdater.checkForUpdates();
  }, 4 * 60 * 60 * 1000);

  autoUpdater.on('update-available', (info) => {
    dialog.showMessageBox(mainWindow, {
      type: 'info',
      title: 'Update Available',
      message: `AC Dev ${info.version} is available.`,
      detail: 'Would you like to download it now?',
      buttons: ['Download', 'Later'],
      defaultId: 0,
    }).then((result) => {
      if (result.response === 0) {
        autoUpdater.downloadUpdate();
      }
    });
  });

  autoUpdater.on('update-downloaded', (info) => {
    dialog.showMessageBox(mainWindow, {
      type: 'info',
      title: 'Update Ready',
      message: `AC Dev ${info.version} has been downloaded.`,
      detail: 'Restart now to install the update?',
      buttons: ['Restart', 'Later'],
      defaultId: 0,
    }).then((result) => {
      if (result.response === 0) {
        autoUpdater.quitAndInstall(false, true);
      }
    });
  });

  autoUpdater.on('error', (err) => {
    log.error('Auto-updater error:', err);
  });
}
```

### Release Workflow (GitHub Actions)

**.github/workflows/release.yml:**
```yaml
name: Build & Release

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    strategy:
      matrix:
        include:
          - os: macos-latest
            platform: mac
          - os: windows-latest
            platform: win
          - os: ubuntu-latest
            platform: linux

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: 24.18.1
          cache: 'npm'

      - name: Install dependencies
        run: npm ci

      - name: Build (macOS)
        if: matrix.platform == 'mac'
        env:
          CSC_LINK: ${{ secrets.MAC_CERTS }}
          CSC_KEY_PASSWORD: ${{ secrets.MAC_CERTS_PASSWORD }}
          APPLE_ID: ${{ secrets.APPLE_ID }}
          APPLE_APP_SPECIFIC_PASSWORD: ${{ secrets.APPLE_APP_SPECIFIC_PASSWORD }}
          APPLE_TEAM_ID: ${{ secrets.APPLE_TEAM_ID }}
        run: npm run build:mac

      - name: Build (Windows)
        if: matrix.platform == 'win'
        env:
          WIN_CSC_LINK: ${{ secrets.WIN_CERTS }}
          WIN_CSC_KEY_PASSWORD: ${{ secrets.WIN_CERTS_PASSWORD }}
        run: npm run build:win

      - name: Build (Linux)
        if: matrix.platform == 'linux'
        run: npm run build:linux

      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: ${{ matrix.platform }}-build
          path: dist/*

  release:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - name: Download all artifacts
        uses: actions/download-artifact@v4

      - name: Create GitHub Release
        uses: softprops/action-gh-release@v1
        with:
          files: |
            mac-build/*.dmg
            mac-build/*.zip
            win-build/*.exe
            linux-build/*.AppImage
            linux-build/*.deb
          draft: false
          prerelease: false
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### Platform-Specific Considerations

| Platform | Package Format | Code Signing | Auto-Update Source |
|----------|---------------|--------------|-------------------|
| **macOS** | .dmg (universal), .zip | Apple notarization required | GitHub Releases |
| **Windows** | NSIS installer, portable .exe | Authenticode (optional but recommended) | GitHub Releases |
| **Linux** | AppImage, .deb, .rpm | N/A | AppImage self-update or GitHub |

### Alternative Update Providers

electron-builder supports multiple update providers:

```json
{
  "publish": [
    {
      "provider": "github",
      "owner": "aesthetic-computer",
      "repo": "ac-dev"
    },
    {
      "provider": "generic",
      "url": "https://releases.aesthetic.computer/ac-dev"
    },
    {
      "provider": "s3",
      "bucket": "ac-releases",
      "region": "us-west-2"
    }
  ]
}
```

**Recommended:** Start with GitHub Releases (free, built-in), migrate to S3/custom CDN if download speeds become an issue.

---

## Technical Decisions

### Why Electron over Tauri?

| Factor | Electron | Tauri |
|--------|----------|-------|
| **xterm.js support** | Native, mature | Requires webview hacks |
| **node-pty** | Works directly | Need Rust PTY bridge |
| **Existing code** | artery-tui is JS | Would need port |
| **Package size** | ~150MB | ~10MB |
| **Performance** | Good enough | Better |
| **Dev speed** | Faster (JS stack) | Slower (Rust) |

**Verdict:** Electron wins for this use case due to xterm.js/node-pty ecosystem.

### Container Communication

**Option A: Docker CLI** (Recommended for v1)
```bash
docker exec -it aesthetic fish -c "..."
```
- Simple, reliable
- Works with existing devcontainer

**Option B: SSH to Container**
- More complex setup
- Better for remote containers

**Option C: devcontainer CLI**
```bash
devcontainer exec --workspace-folder . fish
```
- Official tooling
- Handles edge cases

### AI Integration Strategy

**Recommended:** Start with **Claude Code CLI** because:
1. Agentic capabilities (can edit files, run commands)
2. Better context handling than raw API
3. Already works in terminal

**Fallback:** Direct Claude API for chat-only mode when CLI unavailable.

---

## Advantages Over VSCode

| Aspect | VSCode Devcontainer | AC Electron App |
|--------|--------------------|--------------------|
| **Startup time** | 10-30s | 2-5s |
| **Memory usage** | 500MB-1GB+ | 150-300MB |
| **AC preview** | Separate browser | Integrated pane |
| **artery-tui** | Separate terminal | Integrated pane |
| **AI assistant** | Extension + chat | Dedicated panel |
| **Context focus** | Generic IDE | AC-specific |
| **Branding** | Microsoft | Aesthetic Computer |

---

## Risks & Mitigations

### Risk: Losing VSCode debugging
**Mitigation:** 
- Add "Open in VSCode" button for complex debugging sessions
- Integrate Chrome DevTools for JS debugging
- CDP tunnel already exists in artery

### Risk: Maintenance burden
**Mitigation:**
- Keep it minimal—don't rebuild VSCode
- artery-tui is already 7200 LOC, this adds ~1000-2000
- Share code between artery-tui and Electron app

### Risk: Platform-specific bugs
**Mitigation:**
- Focus on macOS first (primary dev platform)
- Use electron-builder's proven cross-platform tooling

---

## Success Metrics

1. **Startup to productive** < 10 seconds
2. **Single window** for 90% of AC development tasks
3. **AI assistance** accessible without context switching
4. **Live preview** stays in sync with code changes
5. **Developer happiness** measurable via dogfooding

---

## Next Steps

1. **Validate with prototype** - 1-day spike to test xterm.js + docker exec
2. **Review with team** - Get buy-in on architecture
3. **Create `ac-electron/` directory** - Initialize project
4. **Phase 1 implementation** - Start with MVP

---

## References

- [Electron Docs](https://www.electronjs.org/docs)
- [xterm.js](https://xtermjs.org/) - Terminal emulator
- [node-pty](https://github.com/microsoft/node-pty) - PTY bindings
- [electron-builder](https://www.electron.build/) - Packaging
- [ARTERY-TUI-REDESIGN.md](../artery/ARTERY-TUI-REDESIGN.md) - Existing TUI plans
- [Claude Code](https://docs.anthropic.com/en/docs/claude-code) - AI CLI
- [GitHub Copilot CLI](https://githubnext.com/projects/copilot-cli/) - Shell suggestions

---

## Appendix: Alternative Approaches Considered

### A. Web-based IDE (CodeMirror/Monaco in browser)
- **Pros:** No Electron, runs anywhere
- **Cons:** No native terminal, no node-pty, can't docker exec

### B. Tauri App
- **Pros:** Smaller binary, Rust perf
- **Cons:** No xterm.js, would need to rewrite artery-tui

### C. Native macOS App (Swift/SwiftUI)
- **Pros:** Best macOS integration
- **Cons:** macOS only, significant rewrite

### D. tmux/Emacs-only workflow
- **Pros:** Already partially working (artery-emacs.mjs exists)
- **Cons:** Steep learning curve, no graphical preview

**Decision:** Electron provides the best balance of development speed, ecosystem, and cross-platform support for this use case.
