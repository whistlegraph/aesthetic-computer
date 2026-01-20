# Aesthetic Computer - Electron App

## Starting the App

### With Auto-Restart (Recommended)
Use the wrapper scripts to automatically restart when requested:

**macOS/Linux:**
```bash
./restart-electron.sh
```

**Windows:**
```powershell
.\restart-electron.ps1
```

### Without Auto-Restart
```bash
npm start
```

## Rebooting from Container

From inside the devcontainer (Emacs terminal):
```bash
node ac-electron/trigger-reboot.js
```

This creates a marker file that the Electron app watches. When detected:
- App exits with code 42
- Wrapper script automatically restarts it
- Without wrapper, you'll need to manually reopen the app

## Development

### Modes
- **Development** (default): `npm start` or `npm run dev`
  - Connects to `localhost:8888`
  - Shows orange accent color
  - Includes integrated terminal

- **Production**: `npm start -- --production`
  - Connects to `https://aesthetic.computer`
  - Shows green accent color

- **Shell Only**: `npm start -- --shell`
  - Terminal-only window (purple accent)
  - Connects to devcontainer

### Building
```bash
npm run build        # All platforms
npm run build:mac    # macOS universal binary
npm run build:win    # Windows x64
npm run build:linux  # Linux AppImage + deb
```

## Architecture

- **main.js** - Electron main process
  - Window management (AC Pane)
  - IPC bridge for artery communication
  - File watcher for reboot requests
  
- **renderer/** - Frontend HTML/JS
  - `flip-view.html` - AC Pane (3D flip webview + terminal)

- **preload.js** - Secure IPC bridge

## IPC Handlers

From renderer processes:

```javascript
const { ipcRenderer } = require('electron');

// Reboot the app
await ipcRenderer.invoke('app-reboot');

// Switch modes
await ipcRenderer.invoke('switch-mode', 'ac-pane');

// Check Docker/container status
const dockerOk = await ipcRenderer.invoke('check-docker');
const containerOk = await ipcRenderer.invoke('check-container');

// Start devcontainer
await ipcRenderer.invoke('start-container');

// Connect to PTY
const connected = await ipcRenderer.invoke('connect-pty');
```

## Terminal Integration

Currently uses **xterm.js** with WebGL acceleration.

See [RIO-TERMINAL.md](./RIO-TERMINAL.md) for info on migrating to Rio Terminal (GPU-accelerated, WebAssembly-based).

## Troubleshooting

**App won't start:**
- Check if another instance is running
- Try `pkill Electron` then restart

**Terminal not connecting:**
- Ensure devcontainer is running: `docker ps | grep aesthetic`
- Restart container: `docker restart aesthetic`

**Reboot doesn't work:**
- Make sure you're using the wrapper scripts
- Check marker file: `ls ac-electron/.reboot-requested`
- See [REBOOT-ELECTRON.md](./REBOOT-ELECTRON.md) for details
