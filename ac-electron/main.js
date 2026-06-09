/**
 * Aesthetic.Computer - Electron Main Process
 *
 * Single window type:
 * - AC Pane (3D flip view with front webview + back terminal)
 */

const { app, BrowserWindow, ipcMain, globalShortcut, Menu, Tray, dialog, shell, nativeImage, screen, Notification, net, session, systemPreferences } = require('electron');
const path = require('path');
const fs = require('fs');
const http = require('http');
const crypto = require('crypto');
const { spawn, execSync } = require('child_process');

// FF1 Bridge - local server for kidlisp.com to communicate with FF1 devices
const ff1Bridge = require('./ff1-bridge');

// =============================================================================
// DEV MODE - Load files from repo instead of app bundle
// =============================================================================
// To enable: touch ~/.ac-electron-dev
// To disable: rm ~/.ac-electron-dev
// When enabled, renderer files load from ~/aesthetic-computer/ac-electron/
// This lets you iterate on the Electron app without rebuilding!
// =============================================================================
const DEV_FLAG_PATH = path.join(require('os').homedir(), '.ac-electron-dev');
const DEV_REPO_PATH = path.join(require('os').homedir(), 'aesthetic-computer', 'ac-electron');
const isDevMode = fs.existsSync(DEV_FLAG_PATH) && fs.existsSync(DEV_REPO_PATH);

if (isDevMode) {
  console.log('[main] 🔧 DEV MODE ENABLED - loading files from:', DEV_REPO_PATH);
}

// Detect PaperWM tiling window manager (GNOME extension)
let isPaperWM = false;
if (process.platform === 'linux') {
  try {
    const extensions = execSync('gnome-extensions list --enabled 2>/dev/null', { encoding: 'utf8', timeout: 3000 });
    isPaperWM = extensions.includes('paperwm');
    if (isPaperWM) console.log('[main] PaperWM detected - using tiling-friendly window mode');
  } catch (e) {
    // Not GNOME or gnome-extensions not available
  }
}

// =============================================================================
// 🎵 Drag-drop audio onto Dock icon / running window
// Drag an mp3/wav/flac/ogg/m4a onto the app and the `play` piece opens it.
// Files are served from a localhost streaming http server (Range supported)
// so scrubbing works and we don't blow up memory on big files.
// =============================================================================
const AC_DROP_AUDIO_EXTS = new Set([
  '.mp3', '.wav', '.flac', '.ogg', '.m4a', '.aac', '.opus',
]);
const AC_DROP_MIME = {
  '.mp3': 'audio/mpeg',
  '.wav': 'audio/wav',
  '.flac': 'audio/flac',
  '.ogg': 'audio/ogg',
  '.m4a': 'audio/mp4',
  '.aac': 'audio/aac',
  '.opus': 'audio/ogg',
};
const acDropTokens = new Map();        // token -> { path, mime, name }
let acDropPendingPayload = null;       // queued for the next ready window
let acDropAudioServer = null;
let acDropAudioServerPort = null;

function acDropIsAudio(filePath) {
  if (!filePath) return false;
  return AC_DROP_AUDIO_EXTS.has(path.extname(filePath).toLowerCase());
}

function acDropMimeFor(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  return AC_DROP_MIME[ext] || 'application/octet-stream';
}

function acDropEnsureAudioServer() {
  if (acDropAudioServer) return Promise.resolve(acDropAudioServerPort);
  return new Promise((resolve, reject) => {
    const server = http.createServer((req, res) => {
      try {
        const m = req.url.match(/^\/files\/([A-Za-z0-9_-]+)(?:\?.*)?$/);
        if (!m) { res.writeHead(404); res.end('not found'); return; }
        const entry = acDropTokens.get(m[1]);
        if (!entry) { res.writeHead(404); res.end('expired'); return; }
        let stat;
        try { stat = fs.statSync(entry.path); }
        catch (e) { res.writeHead(404); res.end('missing'); return; }

        const total = stat.size;
        const range = req.headers.range;
        const baseHeaders = {
          'Content-Type': entry.mime,
          'Accept-Ranges': 'bytes',
          'Access-Control-Allow-Origin': '*',
          'Cache-Control': 'no-store',
        };

        if (range) {
          const rm = range.match(/bytes=(\d*)-(\d*)/);
          if (!rm) { res.writeHead(416); res.end(); return; }
          let start = rm[1] ? parseInt(rm[1], 10) : 0;
          let end   = rm[2] ? parseInt(rm[2], 10) : total - 1;
          if (isNaN(start) || isNaN(end) || start > end || end >= total) {
            res.writeHead(416, { ...baseHeaders, 'Content-Range': `bytes */${total}` });
            res.end(); return;
          }
          res.writeHead(206, {
            ...baseHeaders,
            'Content-Range': `bytes ${start}-${end}/${total}`,
            'Content-Length': end - start + 1,
          });
          if (req.method === 'HEAD') { res.end(); return; }
          fs.createReadStream(entry.path, { start, end }).pipe(res);
        } else {
          res.writeHead(200, { ...baseHeaders, 'Content-Length': total });
          if (req.method === 'HEAD') { res.end(); return; }
          fs.createReadStream(entry.path).pipe(res);
        }
      } catch (e) {
        try { res.writeHead(500); res.end(String(e?.message || e)); } catch {}
      }
    });
    server.on('error', reject);
    server.listen(0, '127.0.0.1', () => {
      acDropAudioServer = server;
      acDropAudioServerPort = server.address().port;
      console.log('[ac-drop] loopback audio server on 127.0.0.1:' + acDropAudioServerPort);
      resolve(acDropAudioServerPort);
    });
  });
}

async function acDropRegisterFile(filePath) {
  if (!acDropIsAudio(filePath)) return null;
  if (!fs.existsSync(filePath)) {
    console.warn('[ac-drop] dropped file missing:', filePath);
    return null;
  }
  const port = await acDropEnsureAudioServer();
  const token = crypto.randomBytes(8).toString('hex');
  const name = path.basename(filePath);
  const mime = acDropMimeFor(filePath);
  acDropTokens.set(token, { path: filePath, mime, name });
  return { url: `http://127.0.0.1:${port}/files/${token}`, name, mime };
}

function acDropBroadcast(payload) {
  let any = false;
  for (const entry of windows.values()) {
    const w = entry?.window;
    if (w && !w.isDestroyed() && entry.mode === 'ac-pane') {
      any = true;
      try { w.webContents.send('ac-dropped-file', payload); } catch {}
      try { if (w.isMinimized()) w.restore(); w.focus(); } catch {}
    }
  }
  return any;
}

async function acDropHandleFile(filePath) {
  const payload = await acDropRegisterFile(filePath);
  if (!payload) return;
  console.log('[ac-drop] dropped file ready:', payload.name, payload.url);
  const delivered = acDropBroadcast(payload);
  if (!delivered) {
    acDropPendingPayload = payload;
    if (typeof openAcPaneWindow === 'function') {
      try {
        const opened = await openAcPaneWindow();
        const win = opened?.window;
        if (win && !win.isDestroyed()) {
          win.webContents.once('did-finish-load', () => {
            if (acDropPendingPayload) {
              try { win.webContents.send('ac-dropped-file', acDropPendingPayload); } catch {}
              // Leave acDropPendingPayload set — flip-view re-sends on webview
              // dom-ready, and we want the in-page handler to win.
            }
          });
        }
      } catch (e) {
        console.warn('[ac-drop] failed to open ac pane for dropped file:', e?.message || e);
      }
    }
  }
}

// macOS: file drops on the Dock icon arrive via the 'open-file' event.
// This handler MUST be registered before 'ready' / 'will-finish-launching'
// fires, so we attach it at module load.
app.on('open-file', (event, filePath) => {
  event.preventDefault();
  console.log('[ac-drop] open-file (mac):', filePath);
  if (acDropIsAudio(filePath)) {
    // If app isn't ready yet, queue once whenReady resolves.
    if (app.isReady()) acDropHandleFile(filePath);
    else app.whenReady().then(() => acDropHandleFile(filePath));
  }
});

// Windows / Linux: route subsequent launches through the primary instance so
// drag-onto-icon (and Open With ...) hits second-instance instead of starting
// a fresh app.
const acDropSingleLock = app.requestSingleInstanceLock();
if (!acDropSingleLock) {
  app.quit();
} else {
  app.on('second-instance', (_event, argv) => {
    for (const arg of argv) {
      if (acDropIsAudio(arg)) {
        app.whenReady().then(() => acDropHandleFile(arg));
      }
    }
    // Surface an existing window when re-launched with a file.
    for (const entry of windows.values()) {
      const w = entry?.window;
      if (w && !w.isDestroyed()) {
        try { if (w.isMinimized()) w.restore(); w.focus(); } catch {}
        break;
      }
    }
  });
}

// Helper to get file path (repo in dev mode, bundle in production)
function getAppPath(relativePath) {
  if (isDevMode) {
    return path.join(DEV_REPO_PATH, relativePath);
  }
  return path.join(__dirname, relativePath);
}

// macOS Tahoe + Chromium fontations workaround (testing with Electron 39 / Chromium M142)
// Disable problematic font features that may trigger fontations_ffi crash
app.commandLine.appendSwitch('disable-features', 'FontationsFontBackend,Fontations');
if (process.platform === 'darwin') {
  app.commandLine.appendSwitch('use-angle', 'metal');
} else if (process.platform === 'linux') {
  app.commandLine.appendSwitch('enable-gpu-rasterization');
  app.commandLine.appendSwitch('enable-zero-copy');
  app.commandLine.appendSwitch('ignore-gpu-blocklist');
  app.commandLine.appendSwitch('enable-native-gpu-memory-buffers');
  app.commandLine.appendSwitch('enable-accelerated-video-decode');
  app.commandLine.appendSwitch('enable-webgl2-compute-context');
  app.commandLine.appendSwitch('canvas-oop-rasterization');
}
// Performance: no throttling, more memory, high-priority audio
app.commandLine.appendSwitch('disable-renderer-backgrounding');
app.commandLine.appendSwitch('disable-background-timer-throttling');
app.commandLine.appendSwitch('disable-backgrounding-occluded-windows');
app.commandLine.appendSwitch('js-flags', '--max-old-space-size=4096');
app.commandLine.appendSwitch('autoplay-policy', 'no-user-gesture-required');

// Preferences storage
const PREFS_PATH = path.join(app.getPath('userData'), 'preferences.json');
let preferences = {
  showTrayTitle: true,
  trayTitleText: 'Aesthetic.Computer',  // Short text next to tray icon
  launchAtLogin: true,
  defaultMode: 'ac-pane',
  // Window float behavior. When true, new AC Pane / Notepat windows
  // open with alwaysOnTop so they float above other apps (the old
  // default). When false (new default), they behave like normal windows
  // and can be ordered underneath. Toggled from the tray menu.
  alwaysOnTop: false,
  // When the Notepat Overlay is open, should it pass mouse events
  // through to the app underneath (visual-only widget) or capture them
  // (playable floating piano)? Toggled from the tray while the overlay
  // is open.
  overlayClickThrough: false,
};

function loadPreferences() {
  try {
    if (fs.existsSync(PREFS_PATH)) {
      const data = fs.readFileSync(PREFS_PATH, 'utf8');
      preferences = { ...preferences, ...JSON.parse(data) };
    }
  } catch (e) {
    console.warn('[prefs] Failed to load preferences:', e.message);
  }
}

function savePreferences() {
  try {
    fs.writeFileSync(PREFS_PATH, JSON.stringify(preferences, null, 2));
  } catch (e) {
    console.warn('[prefs] Failed to save preferences:', e.message);
  }
}

// Auto-updater (only in production builds)
let autoUpdater;
let autoUpdaterError = null;
let updateDownloaded = false;
let updateAvailable = null; // { version, url, releaseNotes }
let trayBlinkInterval = null;
let trayIconState = 'normal'; // 'normal', 'update', 'blink'
let originalTrayIcon = null;
let updateTrayIcon = null;
const UPDATE_CHECK_INTERVAL = 60 * 60 * 1000; // Check every hour
const SILO_RELEASE_URL = 'https://silo.aesthetic.computer/desktop/latest';

try {
  autoUpdater = require('electron-updater').autoUpdater;
  
  // Configure auto-updater for silent background downloads
  autoUpdater.autoDownload = true;  // Download automatically in background
  autoUpdater.autoInstallOnAppQuit = true; // Install on next launch
  
  autoUpdater.on('checking-for-update', () => {
    console.log('[updater] Checking for updates...');
  });
  
  autoUpdater.on('update-available', (info) => {
    console.log('[updater] Update available:', info.version, '- downloading in background...');
  });
  
  autoUpdater.on('update-not-available', () => {
    console.log('[updater] App is up to date');
  });
  
  autoUpdater.on('download-progress', (progress) => {
    console.log(`[updater] Downloading: ${Math.round(progress.percent)}%`);
  });
  
  autoUpdater.on('update-downloaded', (info) => {
    console.log('[updater] Update downloaded:', info.version);
    updateDownloaded = true;
    
    // Show subtle notification - update will install on next launch
    if (Notification.isSupported()) {
      const notification = new Notification({
        title: 'Update Ready',
        body: `v${info.version} will install on next launch. Click to restart now.`,
        icon: path.join(__dirname, 'build', 'icon.png'),
        silent: true
      });
      notification.on('click', () => {
        autoUpdater.quitAndInstall(false, true);
      });
      notification.show();
    }
  });
  
  autoUpdater.on('error', (err) => {
    console.error('[updater] Error:', err.message);
  });
  
} catch (e) {
  autoUpdaterError = e.message;
  console.log('[updater] Auto-updater not available:', e.message);
}

// Start periodic update checks
function startUpdateChecks() {
  console.log('[updater] startUpdateChecks called, autoUpdater:', !!autoUpdater, 'isPackaged:', app.isPackaged);
  if (!autoUpdater) {
    console.log('[updater] Skipping - autoUpdater not loaded, error:', autoUpdaterError);
    return;
  }
  if (!app.isPackaged) {
    console.log('[updater] Skipping - not a packaged build');
    return;
  }
  
  const checkForUpdates = () => {
    if (!updateDownloaded) {
      console.log('[updater] Running update check...');
      autoUpdater.checkForUpdates().catch(err => {
        console.log('[updater] Check failed:', err.message);
      });
    }
  };
  
  // Initial check after 5 seconds
  setTimeout(checkForUpdates, 5000);
  
  // Then check every hour
  setInterval(checkForUpdates, UPDATE_CHECK_INTERVAL);
}

// Silo release checking (works in dev mode too)
function checkSiloForUpdates() {
  const currentVersion = app.getVersion();
  console.log('[silo] Checking for updates, current version:', currentVersion);

  const request = net.request(SILO_RELEASE_URL);
  request.setHeader('User-Agent', 'Aesthetic-Computer-Electron');

  let responseData = '';

  request.on('response', (response) => {
    response.on('data', (chunk) => {
      responseData += chunk.toString();
    });

    response.on('end', () => {
      try {
        const release = JSON.parse(responseData);
        const latestVersion = release.version;

        if (latestVersion && isNewerVersion(latestVersion, currentVersion)) {
          console.log('[silo] Update available:', latestVersion);
          const platformKey = process.platform === 'darwin' ? 'mac'
                            : process.platform === 'win32' ? 'win' : 'linux';
          updateAvailable = {
            version: latestVersion,
            url: release.platforms?.[platformKey]?.url || 'https://aesthetic.computer/desktop',
            releaseNotes: release.releaseNotes,
            publishedAt: release.publishedAt
          };
          startTrayBlink();
          rebuildTrayMenu();
        } else {
          console.log('[silo] App is up to date');
        }
      } catch (e) {
        console.warn('[silo] Failed to parse release info:', e.message);
      }
    });
  });

  request.on('error', (err) => {
    console.warn('[silo] Failed to check for updates:', err.message);
  });

  request.end();
}

// Compare semantic versions
function isNewerVersion(latest, current) {
  const parseVersion = (v) => v.split('.').map(n => parseInt(n, 10) || 0);
  const latestParts = parseVersion(latest);
  const currentParts = parseVersion(current);
  
  for (let i = 0; i < 3; i++) {
    const l = latestParts[i] || 0;
    const c = currentParts[i] || 0;
    if (l > c) return true;
    if (l < c) return false;
  }
  return false;
}

// Start blinking tray icon
function startTrayBlink() {
  if (trayBlinkInterval || !tray) return;
  
  console.log('[tray] Starting update blink indicator');
  let blinkOn = true;
  
  trayBlinkInterval = setInterval(() => {
    if (!tray) {
      stopTrayBlink();
      return;
    }
    
    if (blinkOn) {
      // Show update indicator (colored dot or different icon)
      if (updateTrayIcon) {
        tray.setImage(updateTrayIcon);
      }
      // Also update title to show update available
      if (process.platform === 'darwin') {
        tray.setTitle('⬆️ Update');
      }
    } else {
      // Show normal icon
      if (originalTrayIcon) {
        tray.setImage(originalTrayIcon);
      }
      if (process.platform === 'darwin') {
        updateTrayTitle();
      }
    }
    blinkOn = !blinkOn;
  }, 1500); // Blink every 1.5 seconds
}

// Stop blinking
function stopTrayBlink() {
  if (trayBlinkInterval) {
    clearInterval(trayBlinkInterval);
    trayBlinkInterval = null;
  }
  if (tray && originalTrayIcon) {
    tray.setImage(originalTrayIcon);
    updateTrayTitle();
  }
}

// Create update indicator icon (adds a colored badge)
function createUpdateIcon(baseIcon) {
  if (process.platform === 'darwin') {
    // On macOS, we can't easily modify template images, so we'll use title instead
    return baseIcon;
  }
  
  // For Windows/Linux, create a modified icon with a badge
  try {
    const size = baseIcon.getSize();
    const canvas = nativeImage.createEmpty();
    // For now, just return the base icon - could enhance with badge overlay later
    return baseIcon;
  } catch (e) {
    return baseIcon;
  }
}

// Start silo update checks (works in both dev and production)
function startSiloUpdateChecks() {
  // Initial check after 10 seconds
  setTimeout(checkSiloForUpdates, 10000);

  // Then check every hour
  setInterval(checkSiloForUpdates, UPDATE_CHECK_INTERVAL);
}

// Set app name before anything else
app.setName('Aesthetic.Computer');
process.title = 'Aesthetic.Computer';

// Set dock icon on macOS
if (process.platform === 'darwin') {
  const iconPath = path.join(__dirname, 'build', 'icon.png');
  try {
    const icon = nativeImage.createFromPath(iconPath);
    if (!icon.isEmpty()) {
      app.dock.setIcon(icon);
    }
  } catch (e) {
    console.warn('Could not set dock icon:', e.message);
  }
}

// On macOS, hide the dock icon when there are no visible windows so the app
// acts as a pure menubar daemon. Shown again as soon as any window opens.
function syncDockVisibility() {
  if (process.platform !== 'darwin' || !app.dock) return;
  const hasVisibleWindow = BrowserWindow.getAllWindows().some(
    (w) => !w.isDestroyed() && w.isVisible()
  );
  if (hasVisibleWindow) {
    if (!app.dock.isVisible()) app.dock.show().catch(() => {});
  } else {
    if (app.dock.isVisible()) app.dock.hide();
  }
}

// Try to load node-pty for real terminal support
let pty;
try {
  pty = require('node-pty');
} catch (e) {
  console.warn('node-pty not available, terminal features will be limited');
}

// Parse command line args
const args = process.argv.slice(2);
const startInDevMode = args.includes('--dev') || args.includes('--development');
// NOTE: only read `--piece <name>` when the flag is actually present —
// args.indexOf('--piece') is -1 when absent, and args[-1 + 1] = args[0] would
// wrongly grab the first arg (e.g. '--dev'), making the piece '--dev', which
// 404s on localhost and falls back to prompt-on-production.
const pieceFlagIdx = args.indexOf('--piece');
const pieceArg =
  args.find((a) => a.startsWith('--piece=')) ||
  (pieceFlagIdx >= 0 ? args[pieceFlagIdx + 1] : undefined);
const initialPiece = pieceArg?.replace('--piece=', '') || 'prompt';

// URLs - nogap removes the aesthetic gap border for desktop mode
const URLS = {
  production: `https://aesthetic.computer/${initialPiece}?nogap=true`,
  development: `http://localhost:8888/${initialPiece}?nogap=true`
};

// Track all windows: windowId -> { window, mode, ptyProcess? }
const windows = new Map();
let windowIdCounter = 0;
let focusedWindowId = null;
let mainWindowId = null;  // Track the "main" window for tray title display
let currentPiece = 'prompt'; // Current piece/prompt shown in main window

// Find docker binary path (needed for packaged app which may not have PATH set)
function getDockerPath() {
  if (process.platform === 'darwin') {
    const dockerLocations = [
      '/opt/homebrew/bin/docker',     // Apple Silicon Homebrew
      '/usr/local/bin/docker',        // Intel Homebrew / Docker Desktop
      '/Applications/Docker.app/Contents/Resources/bin/docker'
    ];
    for (const loc of dockerLocations) {
      if (fs.existsSync(loc)) {
        return loc;
      }
    }
  }
  return 'docker';
}

// Check if Docker is available
async function checkDocker() {
  const dockerPath = getDockerPath();
  return new Promise((resolve) => {
    const docker = spawn(dockerPath, ['info'], { stdio: 'pipe' });
    docker.on('close', (code) => resolve(code === 0));
    docker.on('error', () => resolve(false));
  });
}

// Check if devcontainer is running
async function checkDevcontainer() {
  const dockerPath = getDockerPath();
  return new Promise((resolve) => {
    const docker = spawn(dockerPath, ['ps', '--filter', 'name=aesthetic', '--format', '{{.Names}}'], { stdio: 'pipe' });
    let output = '';
    docker.stdout.on('data', (data) => output += data.toString());
    docker.on('close', () => resolve(output.includes('aesthetic')));
    docker.on('error', () => resolve(false));
  });
}

// Check if container exists (running or stopped)
async function checkContainerExists() {
  const dockerPath = getDockerPath();
  return new Promise((resolve) => {
    const docker = spawn(dockerPath, ['ps', '-a', '--filter', 'name=aesthetic', '--format', '{{.Names}}'], { stdio: 'pipe' });
    let output = '';
    docker.stdout.on('data', (data) => output += data.toString());
    docker.on('close', () => resolve(output.includes('aesthetic')));
    docker.on('error', () => resolve(false));
  });
}

// Start an existing stopped container
async function startExistingContainer() {
  const dockerPath = getDockerPath();
  return new Promise((resolve, reject) => {
    console.log('Starting existing container...');
    const docker = spawn(dockerPath, ['start', 'aesthetic'], { stdio: 'pipe' });
    docker.on('close', (code) => {
      if (code === 0) {
        console.log('Container started successfully');
        resolve(true);
      } else {
        reject(new Error(`docker start exited with code ${code}`));
      }
    });
    docker.on('error', (err) => reject(err));
  });
}

// Start the devcontainer
async function startDevcontainer() {
  return new Promise((resolve, reject) => {
    // For packaged app, the workspace is one level up from the app bundle's Resources folder
    // When running in dev: __dirname is ac-electron, workspace is ..
    // When packaged: __dirname is inside app.asar, need to find actual workspace
    let workspaceFolder;
    if (__dirname.includes('app.asar')) {
      // Packaged app - assume workspace is at a known location or use env var
      workspaceFolder = process.env.AC_WORKSPACE || path.join(process.env.HOME, 'Desktop/code/aesthetic-computer');
    } else {
      workspaceFolder = path.resolve(__dirname, '..');
    }
    console.log('Starting devcontainer in:', workspaceFolder);
    
    // Find devcontainer CLI
    let devcontainerPath = 'devcontainer';
    if (process.platform === 'darwin') {
      const locations = [
        '/opt/homebrew/bin/devcontainer',
        '/usr/local/bin/devcontainer',
        path.join(process.env.HOME, '.npm-global/bin/devcontainer'),
        path.join(process.env.HOME, 'node_modules/.bin/devcontainer')
      ];
      for (const loc of locations) {
        if (fs.existsSync(loc)) {
          devcontainerPath = loc;
          break;
        }
      }
    }
    console.log('Using devcontainer CLI:', devcontainerPath);
    
    const devcontainer = spawn(devcontainerPath, ['up', '--workspace-folder', workspaceFolder], {
      stdio: 'pipe',
      env: {
        ...process.env,
        PATH: `${process.env.PATH}:/usr/local/bin:/opt/homebrew/bin`
      }
    });
    
    let output = '';
    devcontainer.stdout.on('data', (data) => {
      output += data.toString();
      console.log('[devcontainer]', data.toString());
    });
    devcontainer.stderr.on('data', (data) => {
      console.error('[devcontainer error]', data.toString());
    });
    
    devcontainer.on('close', (code) => {
      if (code === 0) {
        resolve(true);
      } else {
        reject(new Error(`devcontainer exited with code ${code}`));
      }
    });
    devcontainer.on('error', (err) => reject(err));
  });
}

// Get the focused window's mode
function getFocusedWindowMode() {
  if (focusedWindowId && windows.has(focusedWindowId)) {
    const mode = windows.get(focusedWindowId).mode;
    if (mode === 'production' || mode === 'development') {
      return mode;
    }
  }
  return (app.isPackaged && !startInDevMode) ? 'production' : 'development';
}

// Get the focused window
function getFocusedWindow() {
  if (focusedWindowId && windows.has(focusedWindowId)) {
    return windows.get(focusedWindowId).window;
  }
  return null;
}

function createMenu() {
  const isMac = process.platform === 'darwin';
  
  const template = [
    // App menu (macOS only)
    ...(isMac ? [{
      label: 'Aesthetic.Computer', // Explicit name for menu bar
      submenu: [
        {
          label: 'About Aesthetic.Computer',
          click: () => {
            dialog.showMessageBox({
              type: 'info',
              title: 'About Aesthetic.Computer',
              message: 'Aesthetic.Computer',
              detail: `Version ${app.getVersion()}`,
              buttons: ['OK']
            });
          }
        },
        { type: 'separator' },
        {
          label: 'Check for Updates...',
          click: () => {
            if (autoUpdater) {
              if (updateDownloaded) {
                dialog.showMessageBox({
                  type: 'info',
                  title: 'Update Ready',
                  message: 'An update has already been downloaded.',
                  detail: 'It will be installed when you restart the app.',
                  buttons: ['Restart Now', 'Later'],
                  defaultId: 0
                }).then(({ response }) => {
                  if (response === 0) {
                    autoUpdater.quitAndInstall(false, true);
                  }
                });
              } else {
                autoUpdater.checkForUpdates().then(result => {
                  if (!result || !result.updateInfo || result.updateInfo.version === app.getVersion()) {
                    dialog.showMessageBox({
                      type: 'info',
                      title: 'No Updates',
                      message: 'You are running the latest version.',
                      buttons: ['OK']
                    });
                  }
                }).catch(err => {
                  dialog.showMessageBox({
                    type: 'error',
                    title: 'Update Check Failed',
                    message: 'Could not check for updates.',
                    detail: err.message,
                    buttons: ['OK']
                  });
                });
              }
            } else {
              // Show why auto-updater isn't available
              const reason = autoUpdaterError 
                ? `Failed to load: ${autoUpdaterError}`
                : app.isPackaged 
                  ? 'Unknown error' 
                  : 'Only available in packaged builds';
              dialog.showMessageBox({
                type: 'info',
                title: 'Updates',
                message: 'Auto-updates are not available.',
                detail: reason,
                buttons: ['OK']
              });
            }
          }
        },
        { type: 'separator' },
        {
          label: 'Preferences...',
          accelerator: 'Cmd+,',
          click: () => openPreferencesWindow()
        },
        { type: 'separator' },
        { role: 'hide' },
        { role: 'hideOthers' },
        { role: 'unhide' },
        { type: 'separator' },
        { role: 'quit' }
      ]
    }] : []),
    // File menu
    {
      label: 'File',
      submenu: [
        {
          label: 'New AC Pane',
          accelerator: 'CmdOrCtrl+N',
          click: () => openAcPaneWindow()
        },
        { type: 'separator' },
        isMac ? { role: 'close' } : { role: 'quit' }
      ]
    },
    // Go menu (navigation)
    {
      label: 'Go',
      submenu: [
        {
          label: 'Back',
          accelerator: 'CmdOrCtrl+[',
          click: () => getFocusedWindow()?.webContents.send('go-back')
        },
        {
          label: 'Forward',
          accelerator: 'CmdOrCtrl+]',
          click: () => getFocusedWindow()?.webContents.send('go-forward')
        },
        { type: 'separator' },
        {
          label: 'Focus Location',
          accelerator: 'CmdOrCtrl+L',
          click: () => getFocusedWindow()?.webContents.send('focus-location')
        },
        { type: 'separator' },
        {
          label: 'Prompt',
          click: () => navigateTo('prompt')
        },
        {
          label: 'Wand',
          click: () => navigateTo('wand')
        },
        {
          label: 'Nopaint',
          click: () => navigateTo('nopaint')
        },
        {
          label: 'Whistlegraph',
          click: () => navigateTo('whistlegraph')
        }
      ]
    },
    // View menu
    {
      label: 'View',
      submenu: [
        { role: 'reload' },
        { role: 'forceReload' },
        { type: 'separator' },
        { role: 'resetZoom' },
        { role: 'zoomIn' },
        { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' },
        { type: 'separator' },
        {
          label: 'Developer Tools',
          accelerator: isMac ? 'Cmd+Option+I' : 'Ctrl+Shift+I',
          click: () => getFocusedWindow()?.webContents.send('toggle-devtools')
        }
      ]
    },
    // Window menu
    {
      label: 'Window',
      submenu: [
        { role: 'minimize' },
        { role: 'zoom' },
        ...(isMac ? [
          { type: 'separator' },
          { role: 'front' }
        ] : [
          { role: 'close' }
        ])
      ]
    }
  ];

  const menu = Menu.buildFromTemplate(template);
  Menu.setApplicationMenu(menu);
}

function navigateTo(piece) {
  const mode = getFocusedWindowMode();
  const baseUrl = mode === 'production' ? 'https://aesthetic.computer' : 'http://localhost:8888';
  getFocusedWindow()?.webContents.send('navigate', `${baseUrl}/${piece}?nogap=true`);
}

// ========== Experimental: native notepat (macOS) ==========
// ac-native is currently a Linux-only bare-metal runtime (see fedac/native/).
// A macOS build doesn't exist yet — when someone adds one and drops the
// resulting Mach-O at any of the paths below (or points $AC_NATIVE_BIN at
// it), the tray item "Launch Native Notepat" starts working automatically.
// Until then, clicking the item shows an installation hint dialog.
function resolveNativeBinary() {
  const repoRoot = path.resolve(__dirname, '..');
  const candidates = [
    process.env.AC_NATIVE_BIN,
    path.join(repoRoot, 'fedac', 'native', 'build', 'ac-native-macos'),
    path.join(repoRoot, 'fedac', 'native', 'build', 'ac-native'),
    '/usr/local/bin/ac-native',
  ].filter(Boolean);
  for (const p of candidates) {
    try {
      if (fs.statSync(p).isFile()) return { path: p, candidates };
    } catch (_) { /* not found, keep looking */ }
  }
  return { path: null, candidates };
}

function launchNativeNotepat() {
  if (process.platform !== 'darwin') {
    dialog.showMessageBox({
      type: 'info',
      title: 'macOS only',
      message: 'Native notepat is a macOS-only experimental feature.',
    });
    return;
  }
  const { path: binPath, candidates } = resolveNativeBinary();
  if (!binPath) {
    dialog.showMessageBox({
      type: 'warning',
      title: 'ac-native binary not found',
      message: 'No macOS build of ac-native is installed.',
      detail:
        'ac-native is currently Linux-only bare metal. To enable this menu item, ' +
        'build a macOS target from fedac/native/ (no Makefile target exists yet) and ' +
        'drop the Mach-O binary at one of these paths, or point $AC_NATIVE_BIN at it:\n\n' +
        candidates.map((p) => '  • ' + p).join('\n'),
    });
    return;
  }
  const repoRoot = path.resolve(__dirname, '..');
  const pieceFile = path.join(repoRoot, 'fedac', 'native', 'pieces', 'notepat.mjs');
  try {
    console.log('[native-notepat] spawning:', binPath, pieceFile);
    const child = spawn(binPath, [pieceFile], {
      detached: true,
      stdio: 'ignore',
    });
    child.on('error', (err) => {
      console.warn('[native-notepat] spawn error:', err);
      dialog.showMessageBox({
        type: 'error',
        title: 'Native notepat failed to start',
        message: err.message,
      });
    });
    child.unref();
  } catch (err) {
    dialog.showMessageBox({
      type: 'error',
      title: 'Native notepat failed to start',
      message: err.message,
    });
  }
}

// ========== System Tray ==========
let tray = null;
let notepatTray = null;
let trayContextMenu = null; // Stored so macOS left-click can pop it explicitly.

// Parse the macOS/Windows accent color into {r,g,b}. Returns null when the
// platform exposes no accent color (so the pals logo keeps its native pink).
function getAccentRGB() {
  try {
    const hex = systemPreferences.getAccentColor?.();
    if (!hex || hex.length < 6) return null;
    return {
      r: parseInt(hex.slice(0, 2), 16),
      g: parseInt(hex.slice(2, 4), 16),
      b: parseInt(hex.slice(4, 6), 16),
    };
  } catch (_) {
    return null;
  }
}

// Build the pals tray icon from the vector source. The pals SVG is a single
// flat-fill path, so — like MenuBand's IconTinter — we recolor it to the macOS
// system accent at runtime (here by swapping the fill hex before rasterizing),
// then rasterize crisp with sharp. Shrunk a touch below the menubar height and
// given a tight, hard drop shadow.
async function buildPalsTrayImage() {
  let sharp;
  try {
    sharp = require('sharp');
  } catch (e) {
    console.warn('[tray] sharp unavailable, falling back to raw icon:', e.message);
    return null;
  }

  const dir = app.isPackaged
    ? process.resourcesPath
    : path.join(__dirname, 'build', 'icons');
  const svgPath = path.join(dir, 'pals.svg');
  if (!fs.existsSync(svgPath)) {
    console.warn('[tray] pals.svg not found:', svgPath);
    return null;
  }

  // Always render the pals as PINK line-art (its native look) — ignore the
  // system accent, and don't flood-fill it into a solid silhouette.
  const PINK = '#cd5c9b';
  console.log('[tray] pals icon — pink line-art');

  const svg = fs.readFileSync(svgPath, 'utf8');
  // Recolor the single-fill outline vector to pink; a faint same-color stroke
  // keeps the thin line crisp at small sizes.
  const pinkSvg = svg.replace(
    /fill="#[0-9a-fA-F]{3,8}"/g,
    `fill="${PINK}" stroke="${PINK}" stroke-width="0.5" stroke-linejoin="round"`
  );
  const master = await sharp(Buffer.from(pinkSvg), { density: 1200 })
    .trim()
    .png()
    .toBuffer();

  const SHRINK = 0.94;       // logo height relative to the full menubar slot
  const out = nativeImage.createEmpty();
  let added = 0;

  for (const scale of [1, 2, 3]) {
    try {
      const W = 34 * scale, H = 22 * scale;
      const lh = Math.max(1, Math.round(22 * SHRINK * scale));

      const logo = await sharp(master)
        .resize({ height: lh, kernel: 'lanczos3' })
        .ensureAlpha()
        .raw()
        .toBuffer({ resolveWithObject: true });
      const lw = logo.info.width;
      const logoBuf = logo.data; // RGBA

      const ox = Math.round((W - lw) / 2);
      const oy = Math.max(0, Math.round((H - lh) / 2));

      const canvas = Buffer.alloc(W * H * 4, 0); // BGRA straight-alpha
      const put = (x, y, b, g, r, a) => {
        if (x < 0 || y < 0 || x >= W || y >= H || a <= 0) return;
        const idx = (y * W + x) * 4;
        const sa = a / 255;
        const da = canvas[idx + 3] / 255;
        const oa = sa + da * (1 - sa);
        if (oa <= 0) return;
        canvas[idx]     = Math.round((b * sa + canvas[idx]     * da * (1 - sa)) / oa);
        canvas[idx + 1] = Math.round((g * sa + canvas[idx + 1] * da * (1 - sa)) / oa);
        canvas[idx + 2] = Math.round((r * sa + canvas[idx + 2] * da * (1 - sa)) / oa);
        canvas[idx + 3] = Math.round(oa * 255);
      };

      // The pink line-art, no drop shadow.
      for (let y = 0; y < lh; y++)
        for (let x = 0; x < lw; x++) {
          const j2 = (y * lw + x) * 4;
          const a = logoBuf[j2 + 3];
          if (a > 0) put(ox + x, oy + y, logoBuf[j2 + 2], logoBuf[j2 + 1], logoBuf[j2], a);
        }

      out.addRepresentation({ scaleFactor: scale, width: W, height: H, buffer: canvas });
      added++;
    } catch (e) {
      console.warn('[tray] Failed to build pals rep @' + scale + 'x:', e.message);
    }
  }

  return added > 0 ? out : null;
}

async function createSystemTray() {
  // The pals tray icon: shrunk just below full menubar height, with a soft
  // drop shadow and a hue tint that follows the macOS accent color.
  let icon = process.platform === 'darwin' ? await buildPalsTrayImage() : null;

  if (!icon || icon.isEmpty()) {
    // Fallback — load the raw PNG (Windows/Linux, or if processing failed).
    let iconPath;
    if (process.platform === 'darwin') {
      iconPath = app.isPackaged
        ? path.join(process.resourcesPath, 'palsTray.png')
        : path.join(__dirname, 'build', 'icons', 'palsTray.png');
    } else {
      iconPath = app.isPackaged
        ? path.join(process.resourcesPath, 'tray-icon.png')
        : path.join(__dirname, 'build', 'icons', '16x16.png');
    }
    console.log('[main] Loading tray icon from:', iconPath);
    icon = nativeImage.createFromPath(iconPath);
  }

  if (!icon || icon.isEmpty()) {
    console.warn('[main] Tray icon is empty!');
    return;
  }

  // Pals is full color — do NOT mark it as a template image (template
  // images get auto-tinted to monochrome by AppKit).

  // Store original icon for blink toggling
  originalTrayIcon = icon;
  updateTrayIcon = createUpdateIcon(icon);

  tray = new Tray(icon);
  tray.setToolTip('Aesthetic.Computer');
  console.log('[main] System tray created successfully');

  // Build and set the context menu
  rebuildTrayMenu();

  if (process.platform === 'darwin') {
    // macOS: pop the menu manually so a plain left-click reliably opens it
    // (relying on setContextMenu alone left clicks dead in some sessions).
    const popMenu = () => {
      if (trayContextMenu) tray.popUpContextMenu(trayContextMenu);
    };
    tray.on('click', popMenu);
    tray.on('right-click', popMenu);

    // Re-tint the icon live when the user changes their accent color.
    try {
      systemPreferences.on('accent-color-changed', () => {
        buildPalsTrayImage()
          .then((fresh) => {
            if (fresh && !fresh.isEmpty()) {
              originalTrayIcon = fresh;
              updateTrayIcon = createUpdateIcon(fresh);
              if (tray && trayIconState === 'normal') tray.setImage(fresh);
            }
          })
          .catch(() => {});
        // Re-tint every AC window's chrome to the new accent.
        const acc = getAccentRGB();
        if (acc) {
          for (const w of BrowserWindow.getAllWindows()) {
            try { w.webContents.send('ac-accent', acc); } catch (_) {}
          }
        }
      });
    } catch (_) {}
  } else {
    // Windows/Linux: click toggles the window.
    tray.on('click', () => {
      const allWindows = BrowserWindow.getAllWindows();
      if (allWindows.length > 0) {
        const win = allWindows[0];
        if (win.isVisible()) {
          win.hide();
        } else {
          win.show();
        }
      } else {
        openAcPaneWindow();
      }
    });
  }

  // Set initial tray title
  updateTrayTitle();
}

// Rebuild the tray context menu (called when update becomes available)
function rebuildTrayMenu() {
  if (!tray) return;
  
  const isMac = process.platform === 'darwin';
  const menuItems = [];
  
  // Update available section (if applicable)
  if (updateAvailable) {
    menuItems.push({
      label: `🆕 Update Available: v${updateAvailable.version}`,
      click: () => {
        shell.openExternal(updateAvailable.url);
        stopTrayBlink();
      }
    });
    menuItems.push({
      label: 'Download Update',
      click: () => {
        shell.openExternal(updateAvailable.url);
        stopTrayBlink();
      }
    });
    menuItems.push({
      label: 'Dismiss',
      click: () => {
        updateAvailable = null;
        stopTrayBlink();
        rebuildTrayMenu();
      }
    });
    menuItems.push({ type: 'separator' });
  }
  
  // 🟢 The nom muncher games.
  menuItems.push({ label: 'numbnom — numbers', click: () => navigateToPiece('numbnom') });
  menuItems.push({ label: 'engnom — english', click: () => navigateToPiece('engnom') });
  menuItems.push({ label: 'mexinom — español', click: () => navigateToPiece('mexinom') });
  menuItems.push({ label: 'dannom — dansk', click: () => navigateToPiece('dannom') });
  menuItems.push({ label: 'rusnom — русский', click: () => navigateToPiece('rusnom') });
  menuItems.push({ label: 'notenom — music', click: () => navigateToPiece('notenom') });

  menuItems.push({ type: 'separator' });

  // Prompt (home).
  menuItems.push({ label: 'prompt', click: () => navigateToPiece('prompt') });

  menuItems.push({ type: 'separator' });

  menuItems.push({
    label: 'Quit',
    accelerator: isMac ? 'Cmd+Q' : 'Alt+F4',
    click: () => app.quit()
  });
  
  const contextMenu = Menu.buildFromTemplate(menuItems);
  trayContextMenu = contextMenu;
  if (process.platform === 'darwin') {
    // macOS pops trayContextMenu explicitly from the click handler — binding
    // it via setContextMenu here would race that and can swallow the click.
  } else {
    tray.setContextMenu(contextMenu);
  }
}

function createNotepatTray() {
  if (notepatTray) return;

  let iconPath;
  if (process.platform === 'darwin') {
    iconPath = app.isPackaged
      ? path.join(process.resourcesPath, 'notepatTrayTemplate.png')
      : path.join(__dirname, 'build', 'icons', 'notepatTrayTemplate.png');
  } else {
    iconPath = app.isPackaged
      ? path.join(process.resourcesPath, 'notepatTrayTemplate.png')
      : path.join(__dirname, 'build', 'icons', 'notepatTrayTemplate.png');
  }

  const icon = nativeImage.createFromPath(iconPath);
  if (icon.isEmpty()) {
    console.warn('[notepat-tray] Icon empty at', iconPath);
    return;
  }
  if (process.platform === 'darwin') icon.setTemplateImage(true);

  notepatTray = new Tray(icon);
  notepatTray.setToolTip('Notepat');
  if (process.platform === 'darwin') notepatTray.setTitle('notepat');

  const open = () => openNotepatWindow();
  notepatTray.on('click', open);

  const menu = Menu.buildFromTemplate([
    { label: 'Open Notepat', click: open },
    { label: 'Open Notepat Overlay 🪟', click: () => openNotepatOverlayWindow() },
    { type: 'separator' },
    {
      label: 'Launch Native Notepat (experimental)',
      enabled: process.platform === 'darwin',
      click: () => launchNativeNotepat(),
    },
    { type: 'separator' },
    {
      label: 'Overlay: Click-Through',
      type: 'checkbox',
      checked: !!preferences.overlayClickThrough,
      click: (item) => {
        preferences.overlayClickThrough = !!item.checked;
        savePreferences();
        if (notepatOverlayWindow && !notepatOverlayWindow.isDestroyed()) {
          notepatOverlayWindow.setIgnoreMouseEvents(preferences.overlayClickThrough, { forward: true });
        }
      },
    },
    { type: 'separator' },
    { label: 'Quit', accelerator: process.platform === 'darwin' ? 'Cmd+Q' : 'Alt+F4', click: () => app.quit() },
  ]);
  notepatTray.setContextMenu(menu);
}

// ========== End System Tray ==========

// Update the tray title text (shown next to icon in menu bar).
// The pals logo speaks for itself — never render piece-name text next to
// it. Callers (update flow etc.) can still pass an explicit string for
// transient states like "⬆️ Update".
function updateTrayTitle(text) {
  if (!tray) return;
  if (process.platform === 'darwin') {
    tray.setTitle(text !== undefined ? text : '');
  }
}

// Preferences window
let preferencesWindow = null;

function openPreferencesWindow() {
  if (preferencesWindow && !preferencesWindow.isDestroyed()) {
    preferencesWindow.focus();
    return;
  }
  
  preferencesWindow = new BrowserWindow({
    width: 480,
    height: 400,
    resizable: false,
    minimizable: false,
    maximizable: false,
    title: 'Preferences',
    titleBarStyle: 'hiddenInset',
    backgroundColor: '#1a1a2e',
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false
    }
  });
  
  preferencesWindow.loadFile(getAppPath('renderer/preferences.html'));
  
  preferencesWindow.on('closed', () => {
    preferencesWindow = null;
  });
}

// Get the focused window or first available
function getFocusedWindow() {
  let win = BrowserWindow.getFocusedWindow();
  if (!win) {
    const allWindows = BrowserWindow.getAllWindows();
    win = allWindows.find(w => w.isVisible() && !w.isDestroyed());
  }
  return win;
}

// Navigate a window to a specific piece
function navigateToPiece(piece) {
  const win = getFocusedWindow();
  const mode = getFocusedWindowMode();
  const baseUrl = mode === 'production' ? 'https://aesthetic.computer' : 'http://localhost:8888';
  const url = `${baseUrl}/${piece}?nogap=true`;
  if (win) {
    win.webContents.send('navigate', url);
  } else {
    // No window open, create one and navigate
    openAcPaneWindow().then(result => {
      if (!result?.window) return;
      result.window.webContents.once('did-finish-load', () => {
        result.window.webContents.send('navigate', url);
      });
    });
  }
}

// Open a new AC Pane window
async function openAcPaneWindow(options = {}) {
  return openAcPaneWindowInternal(options);
}

// Open a standalone Notepat window — compact window dedicated to the /notepat piece
let notepatWindow = null;
function openNotepatWindow() {
  if (notepatWindow && !notepatWindow.isDestroyed()) {
    if (notepatWindow.isMinimized()) notepatWindow.restore();
    notepatWindow.show();
    notepatWindow.focus();
    return notepatWindow;
  }

  const baseUrl = startInDevMode ? 'http://localhost:8888' : 'https://aesthetic.computer';

  // Match the AC Pane (prompt) window chrome flags exactly so the two
  // windows feel like a matched set — frameless, transparent, float-on-top.
  const notepatOpts = {
    width: 480,
    height: 360,
    minWidth: 320,
    minHeight: 260,
    title: 'Notepat',
    frame: false,
    transparent: !isPaperWM,
    hasShadow: isPaperWM,
    alwaysOnTop: !isPaperWM && preferences.alwaysOnTop,
    backgroundColor: isPaperWM ? '#000000' : '#00000000',
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      webviewTag: true,
      backgroundThrottling: false,
    },
  };
  if (isPaperWM) notepatOpts.type = 'normal';
  notepatWindow = new BrowserWindow(notepatOpts);

  notepatWindow.loadFile(
    getAppPath('renderer/notepat-view.html'),
    { query: { piece: 'notepat', base: baseUrl } },
  );

  const windowId = windowIdCounter++;
  windows.set(windowId, { window: notepatWindow, mode: 'notepat' });

  notepatWindow.on('closed', () => {
    windows.delete(windowId);
    notepatWindow = null;
  });

  return notepatWindow;
}

// Open a Notepat Overlay — transparent, chromeless, screen-saver-level
// window that floats above every app and follows the user across Spaces.
// Like a macOS HUD / widget. Click-through can be toggled from the tray
// so the overlay becomes visual-only (mouse events pass to whatever's
// behind it). The notepat piece reads ?overlay=true and skips its
// background wipe so the desktop shows through between keys.
let notepatOverlayWindow = null;
function openNotepatOverlayWindow() {
  if (notepatOverlayWindow && !notepatOverlayWindow.isDestroyed()) {
    notepatOverlayWindow.show();
    notepatOverlayWindow.focus();
    return notepatOverlayWindow;
  }

  const baseUrl = startInDevMode ? 'http://localhost:8888' : 'https://aesthetic.computer';
  const display = screen.getPrimaryDisplay().workAreaSize;
  const w = 340, h = 180;

  notepatOverlayWindow = new BrowserWindow({
    width: w,
    height: h,
    x: display.width - w - 24,
    y: display.height - h - 24,
    minWidth: 220,
    minHeight: 130,
    title: 'Notepat Overlay',
    frame: false,
    transparent: true,
    hasShadow: false,
    resizable: true,
    skipTaskbar: true,
    backgroundColor: '#00000000',
    type: process.platform === 'darwin' ? 'panel' : undefined,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      webviewTag: true,
      backgroundThrottling: false,
    },
  });

  // screen-saver level sits above fullscreen apps on macOS. Follow the
  // user across Spaces (and into fullscreen Spaces) so the overlay is
  // actually always visible.
  notepatOverlayWindow.setAlwaysOnTop(true, 'screen-saver');
  notepatOverlayWindow.setVisibleOnAllWorkspaces(true, { visibleOnFullScreen: true });

  if (preferences.overlayClickThrough) {
    notepatOverlayWindow.setIgnoreMouseEvents(true, { forward: true });
  }

  notepatOverlayWindow.loadFile(
    getAppPath('renderer/notepat-overlay.html'),
    { query: { piece: 'notepat', base: baseUrl } },
  );

  const windowId = windowIdCounter++;
  windows.set(windowId, { window: notepatOverlayWindow, mode: 'notepat-overlay' });

  notepatOverlayWindow.on('closed', () => {
    windows.delete(windowId);
    notepatOverlayWindow = null;
  });

  return notepatOverlayWindow;
}

// Open KidLisp window (kidlisp.com)
function openKidLispWindow() {
  const { width, height } = screen.getPrimaryDisplay().workAreaSize;
  const winWidth = Math.min(1200, width * 0.8);
  const winHeight = Math.min(800, height * 0.8);
  
  const win = new BrowserWindow({
    width: winWidth,
    height: winHeight,
    title: 'KidLisp',
    backgroundColor: '#0a0a12',
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
    },
    titleBarStyle: 'hiddenInset',
    trafficLightPosition: { x: 15, y: 12 },
  });
  
  win.loadURL('https://kidlisp.com');
  
  // Track it
  const windowId = windowIdCounter++;
  windows.set(windowId, { window: win, mode: 'kidlisp' });
  
  win.on('closed', () => {
    windows.delete(windowId);
  });
  
  return win;
}

// ========== CSS 3D Flip View ==========
let ptyProcessFor3D = null;
let lastKnownCols = 120;
let lastKnownRows = 40;

// Calculate offset position for new windows to avoid overlap
function getOffsetWindowPosition(sourceWindow, index = 0) {
  const offset = 40; // Pixels to offset each new window
  const { width, height } = screen.getPrimaryDisplay().workAreaSize;
  
  let baseX, baseY;
  
  if (sourceWindow && !sourceWindow.isDestroyed()) {
    const [srcX, srcY] = sourceWindow.getPosition();
    const [srcW, srcH] = sourceWindow.getSize();
    // Position to the right of source window
    baseX = srcX + srcW + 20;
    baseY = srcY;
    // If it would go off screen, wrap to upper-left with offset
    if (baseX + 680 > width) {
      baseX = 50 + (index * offset);
      baseY = 50 + (index * offset);
    }
  } else {
    // No source window, use cascading from top-left
    baseX = 100 + (index * offset);
    baseY = 100 + (index * offset);
  }
  
  // Apply index offset for multiple windows
  const x = Math.min(baseX + (index * offset), width - 680);
  const y = Math.min(baseY + (index * offset), height - 520);
  
  return { x: Math.max(0, x), y: Math.max(0, y) };
}

async function openAcPaneWindowInternal(options = {}) {
  const { sourceWindow = null, index = 0 } = options;
  
  // Start with a smaller, cuter window - extra height for mode tags at bottom
  const winWidth = 330;
  const winHeight = 290;
  
  // Calculate position to avoid overlap
  const { x, y } = getOffsetWindowPosition(sourceWindow, index);
  
  // 🪟 Native macOS chrome: the standard OS title bar + traffic lights + native
  // resize, with a full-bleed webview below it. Set false to fall back to the
  // custom card chrome.
  const NATIVE_CHROME = !isPaperWM;
  const winOpts = {
    width: winWidth,
    height: winHeight,
    x,
    y,
    minWidth: 260,
    minHeight: 220,
    title: options.piece || 'prompt',
    frame: NATIVE_CHROME ? true : false,
    transparent: NATIVE_CHROME ? false : !isPaperWM,
    hasShadow: NATIVE_CHROME ? true : isPaperWM,
    alwaysOnTop: !isPaperWM && preferences.alwaysOnTop,
    backgroundColor: NATIVE_CHROME ? '#111114' : (isPaperWM ? '#000000' : '#00000000'),
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      webviewTag: true,
      backgroundThrottling: false,
    },
  };
  // On PaperWM, use 'normal' type so the WM tiles it instead of floating
  if (isPaperWM) winOpts.type = 'normal';
  const win = new BrowserWindow(winOpts);
  // Keep the native title bar showing the current piece (driven by
  // 'set-current-piece'), not the flip-view document <title> ("AC Pane").
  if (NATIVE_CHROME) win.on('page-title-updated', (e) => e.preventDefault());

  // Plumb the launch piece + environment base into the flip-view shell so
  // `--piece=NAME` (and `--dev`) actually route. The shell reads ?piece= and
  // ?base= in initWebviewSrc(); without these it always falls back to prompt
  // on production. Only the pane explicitly given a piece overrides the
  // default — extra panes opened later still start at the prompt.
  const baseUrl = startInDevMode ? 'http://localhost:8888' : 'https://aesthetic.computer';
  const query = { base: baseUrl };
  if (options.piece) query.piece = options.piece;
  if (isPaperWM) query.wm = 'paper';
  if (NATIVE_CHROME) query.chrome = 'native';
  // Tint the window chrome to the macOS system accent (kept in sync below via
  // the 'accent-color-changed' broadcast). Backgrounds still follow light/dark.
  const _acc = getAccentRGB();
  if (_acc) query.accent = [_acc.r, _acc.g, _acc.b].map((c) => c.toString(16).padStart(2, '0')).join('');
  win.loadFile(getAppPath('renderer/flip-view.html'), { query });

  // 🎮 Grant sticky user activation to the host page so navigator.getGamepads()
  // works without the user clicking host chrome first. Chromium gates the
  // Gamepad API behind sticky activation; in a <webview>-host setup the user
  // typically clicks inside the webview and the host never receives one,
  // which made the host→guest gamepad bridge silently send empty snapshots.
  win.webContents.once('did-finish-load', () => {
    win.webContents.executeJavaScript('1', true).catch(() => {});
  });

  // Track it
  const windowId = windowIdCounter++;
  windows.set(windowId, { window: win, mode: 'ac-pane' });
  
  // Track focus
  win.on('focus', () => {
    focusedWindowId = windowId;
  });
  
  // Register Cmd+F to flip ALL windows, Cmd+B to flip focused window only
  const flipAllShortcut = process.platform === 'darwin' ? 'CommandOrControl+F' : 'Ctrl+F';
  const flipOneShortcut = process.platform === 'darwin' ? 'CommandOrControl+B' : 'Ctrl+B';
  globalShortcut.register(flipAllShortcut, () => {
    for (const entry of windows.values()) {
      const w = entry.window || entry;
      if (w && !w.isDestroyed()) {
        w.webContents.send('toggle-flip');
      }
    }
  });
  globalShortcut.register(flipOneShortcut, () => {
    if (focusedWindowId && windows.has(focusedWindowId)) {
      const w = windows.get(focusedWindowId).window;
      if (w && !w.isDestroyed()) {
        w.webContents.send('toggle-flip');
      }
    }
  });
  
  win.on('closed', () => {
    windows.delete(windowId);
    if (focusedWindowId === windowId) {
      focusedWindowId = null;
    }
    // Only unregister global shortcuts when the last window closes
    if (windows.size === 0) {
      globalShortcut.unregister(flipAllShortcut);
      globalShortcut.unregister(flipOneShortcut);
      globalShortcut.unregister('CommandOrControl+Plus');
      globalShortcut.unregister('CommandOrControl+=');
      globalShortcut.unregister('CommandOrControl+-');
      globalShortcut.unregister('CommandOrControl+0');
    }
    if (ptyProcessFor3D) {
      ptyProcessFor3D.kill();
      ptyProcessFor3D = null;
    }
  });
  
  // PTY for terminal side
  ipcMain.on('connect-flip-pty', (event) => {
    if (!pty) {
      event.sender.send('flip-pty-data', '\r\n\x1b[31mError: node-pty not available\x1b[0m\r\n');
      return;
    }
    
    const dockerPath = getDockerPath();
    
    // Start container and emacs daemon if needed
    ensureContainerForFlip(dockerPath, event.sender);
  });
  
  ipcMain.on('flip-pty-input', (event, data) => {
    // Check if we're waiting for a prompt response
    if (pendingContainerPrompt) {
      // Collect input until Enter is pressed
      if (data === '\r' || data === '\n') {
        handleContainerPromptResponse(pendingInputBuffer || '');
        pendingInputBuffer = '';
      } else if (data === '\x7f' || data === '\b') {
        // Backspace
        if (pendingInputBuffer && pendingInputBuffer.length > 0) {
          pendingInputBuffer = pendingInputBuffer.slice(0, -1);
          event.sender.send('flip-pty-data', '\b \b');
        }
      } else if (data.length === 1 && data.charCodeAt(0) >= 32) {
        // Printable character
        pendingInputBuffer = (pendingInputBuffer || '') + data;
        event.sender.send('flip-pty-data', data);
      }
      return;
    }
    
    if (ptyProcessFor3D) {
      ptyProcessFor3D.write(data);
    }
  });
  
  // Update terminal dimensions from renderer
  ipcMain.on('flip-pty-resize', (event, cols, rows) => {
    lastKnownCols = cols;
    lastKnownRows = rows;
    if (ptyProcessFor3D) {
      try {
        ptyProcessFor3D.resize(cols, rows);
      } catch (err) {
        console.warn('[main] Flip PTY resize failed:', err.message);
      }
    }
  });
  
  // Allow renderer to query current dimensions
  ipcMain.handle('get-terminal-size', () => ({ cols: lastKnownCols, rows: lastKnownRows }));
  
  // Register zoom shortcuts for flip view
  globalShortcut.register('CommandOrControl+Plus', () => {
    if (win && !win.isDestroyed()) {
      win.webContents.send('zoom-in');
    }
  });
  globalShortcut.register('CommandOrControl+=', () => {
    if (win && !win.isDestroyed()) {
      win.webContents.send('zoom-in');
    }
  });
  globalShortcut.register('CommandOrControl+-', () => {
    if (win && !win.isDestroyed()) {
      win.webContents.send('zoom-out');
    }
  });
  globalShortcut.register('CommandOrControl+0', () => {
    if (win && !win.isDestroyed()) {
      win.webContents.send('zoom-reset');
    }
  });
  
  // Custom window resize handler
  ipcMain.on('resize-window', (event, bounds) => {
    const senderWindow = BrowserWindow.fromWebContents(event.sender);
    if (senderWindow) {
      senderWindow.setBounds({
        x: Math.round(bounds.x),
        y: Math.round(bounds.y),
        width: Math.round(bounds.width),
        height: Math.round(bounds.height)
      });
    }
  });
  
  return { window: win, windowId };
}

// State for interactive prompt
let pendingContainerPrompt = null;
let pendingInputBuffer = '';

async function ensureContainerForFlip(dockerPath, webContents) {
  const { spawn } = require('child_process');
  
  // First check if Docker is available at all
  const dockerAvailable = await new Promise((resolve) => {
    const check = spawn(dockerPath, ['info'], { stdio: 'pipe' });
    check.on('close', (code) => resolve(code === 0));
    check.on('error', () => resolve(false));
  });
  
  if (!dockerAvailable) {
    webContents.send('flip-pty-data', '\x1b[33mDocker not available. Starting local shell...\x1b[0m\r\n\r\n');
    startLocalShellForFlip(webContents);
    return;
  }
  
  // Check if container exists
  const containerExists = await new Promise((resolve) => {
    const check = spawn(dockerPath, ['ps', '-a', '--filter', 'name=aesthetic', '--format', '{{.Names}}'], { stdio: 'pipe' });
    let output = '';
    check.stdout.on('data', (data) => output += data.toString());
    check.on('close', () => resolve(output.includes('aesthetic')));
    check.on('error', () => resolve(false));
  });
  
  if (!containerExists) {
    webContents.send('flip-pty-data', '\x1b[33mNo AC container found. Starting local shell...\x1b[0m\r\n');
    webContents.send('flip-pty-data', '\x1b[90m(Run "npm run aesthetic" in the repo to start the devcontainer)\x1b[0m\r\n\r\n');
    startLocalShellForFlip(webContents);
    return;
  }
  
  // Ask user if they want to start the devcontainer
  webContents.send('flip-pty-data', '\x1b[36mStart devcontainer with emacs? [y/n]: \x1b[0m');
  
  // Wait for user input
  pendingContainerPrompt = { dockerPath, webContents };
}

// Handle user response to container prompt
function handleContainerPromptResponse(response) {
  if (!pendingContainerPrompt) return false;
  
  const { dockerPath, webContents } = pendingContainerPrompt;
  pendingContainerPrompt = null;
  
  const answer = response.trim().toLowerCase();
  
  if (answer === 'y' || answer === 'yes') {
    webContents.send('flip-pty-data', 'y\r\n');
    startContainerAndEmacs(dockerPath, webContents);
    return true;
  } else {
    webContents.send('flip-pty-data', answer ? `${answer}\r\n` : 'n\r\n');
    webContents.send('flip-pty-data', '\x1b[33mStarting local shell instead...\x1b[0m\r\n\r\n');
    startLocalShellForFlip(webContents);
    return true;
  }
}

// Emacs log file path (inside container, but we'll also keep a local copy)
const EMACS_LOG_PATH = '/tmp/ac-emacs-daemon.log';
const LOCAL_EMACS_LOG_PATH = path.join(app.getPath('userData'), 'emacs-daemon.log');

// Write to local emacs log
function writeEmacsLog(content, append = true) {
  try {
    if (append) {
      fs.appendFileSync(LOCAL_EMACS_LOG_PATH, content);
    } else {
      fs.writeFileSync(LOCAL_EMACS_LOG_PATH, content);
    }
  } catch (e) {
    console.warn('[main] Failed to write emacs log:', e.message);
  }
}

// Read local emacs log
function readEmacsLog() {
  try {
    if (fs.existsSync(LOCAL_EMACS_LOG_PATH)) {
      return fs.readFileSync(LOCAL_EMACS_LOG_PATH, 'utf-8');
    }
  } catch (e) {
    console.warn('[main] Failed to read emacs log:', e.message);
  }
  return null;
}

async function startContainerAndEmacs(dockerPath, webContents) {
  const { spawn } = require('child_process');
  
  // Start fresh log for this session
  const timestamp = new Date().toISOString();
  writeEmacsLog(`\n\n========== EMACS SESSION START: ${timestamp} ==========\n`, false);
  
  // Restart container to ensure clean state
  webContents.send('flip-pty-data', '\x1b[33mRestarting container...\x1b[0m\r\n');
  writeEmacsLog(`[${timestamp}] Restarting container...\n`);
  await new Promise((resolve) => {
    const restart = spawn(dockerPath, ['restart', 'aesthetic'], { stdio: 'pipe' });
    restart.on('close', () => resolve());
    restart.on('error', () => resolve());
  });
  
  // Wait for container to be ready
  await new Promise(r => setTimeout(r, 2000));
  writeEmacsLog(`Container restarted, waiting for ready state...\n`);
  
  // Kill any existing emacs daemon first
  writeEmacsLog(`Killing any existing emacs daemon...\n`);
  await new Promise((resolve) => {
    const kill = spawn(dockerPath, ['exec', 'aesthetic', 'pkill', '-9', '-f', 'emacs.*daemon'], { stdio: 'pipe' });
    kill.on('close', () => resolve());
    kill.on('error', () => resolve());
  });
  
  await new Promise(r => setTimeout(r, 500));
  
  // Start emacs daemon with AC config
  webContents.send('flip-pty-data', '\x1b[33mStarting emacs daemon with AC config...\x1b[0m\r\n');
  const configPath = '/home/me/aesthetic-computer/dotfiles/dot_config/emacs.el';
  writeEmacsLog(`Starting emacs daemon with config: ${configPath}\n`);
  writeEmacsLog(`Command: emacs -q --daemon -l ${configPath}\n`);
  
  // Start daemon and capture output
  const emacsStartResult = await new Promise((resolve) => {
    let output = '';
    const emacs = spawn(dockerPath, ['exec', 'aesthetic', 'emacs', '-q', '--daemon', '-l', configPath], { stdio: 'pipe' });
    emacs.stdout.on('data', (d) => {
      output += d.toString();
      writeEmacsLog(`[stdout] ${d.toString()}`);
    });
    emacs.stderr.on('data', (d) => {
      output += d.toString();
      writeEmacsLog(`[stderr] ${d.toString()}`);
    });
    emacs.on('close', (code) => resolve({ code, output }));
    emacs.on('error', (err) => resolve({ code: -1, output: err.message }));
  });
  
  writeEmacsLog(`\nEmacs daemon exit code: ${emacsStartResult.code}\n`);
  console.log('[main] Emacs daemon started, code:', emacsStartResult.code);
  if (emacsStartResult.output) {
    console.log('[main] Emacs output:', emacsStartResult.output.substring(0, 500));
  }
  
  // Wait for emacs daemon to be responsive (poll with emacsclient -e t)
  webContents.send('flip-pty-data', '\x1b[33mWaiting for emacs daemon...\x1b[0m\r\n');
  writeEmacsLog(`Waiting for emacs daemon to be responsive...\n`);
  let emacsReady = false;
  for (let i = 0; i < 30; i++) { // Max 30 seconds
    const ready = await new Promise((resolve) => {
      const check = spawn(dockerPath, ['exec', 'aesthetic', 'emacsclient', '-e', 't'], { stdio: 'pipe' });
      check.on('close', (code) => resolve(code === 0));
      check.on('error', () => resolve(false));
    });
    if (ready) {
      emacsReady = true;
      break;
    }
    await new Promise(r => setTimeout(r, 1000));
    webContents.send('flip-pty-data', '.');
    writeEmacsLog(`.`);
  }
  
  if (!emacsReady) {
    writeEmacsLog(`\nFAILED: Emacs daemon did not become responsive after 30 seconds\n`);
    webContents.send('flip-pty-data', '\r\n\x1b[31mEmacs daemon failed to start.\x1b[0m\r\n');
    // Show the log to the user
    const log = readEmacsLog();
    if (log) {
      webContents.send('flip-pty-data', '\r\n\x1b[33m--- Emacs Startup Log ---\x1b[0m\r\n');
      webContents.send('flip-pty-data', log.split('\n').slice(-30).join('\r\n') + '\r\n');
      webContents.send('flip-pty-data', '\x1b[33m--- End Log ---\x1b[0m\r\n\r\n');
    }
    webContents.send('flip-pty-data', '\x1b[90mFalling back to fish shell...\x1b[0m\r\n');
    startLocalShellForFlip(webContents);
    return;
  }
  
  writeEmacsLog(`\nEmacs daemon ready!\n`);
  
  webContents.send('flip-pty-data', ' ready!\r\n');
  
  // Request current terminal size from renderer before spawning PTY
  webContents.send('flip-pty-data', '\x1b[33mConnecting to emacs...\x1b[0m\r\n');
  writeEmacsLog(`Connecting emacsclient...\n`);
  
  // Get the last known size (will be updated by renderer)
  const { cols, rows } = await new Promise((resolve) => {
    // Request size update from renderer
    webContents.send('request-terminal-size');
    // Wait a moment for any pending resize to arrive
    setTimeout(() => {
      resolve({ cols: lastKnownCols || 120, rows: lastKnownRows || 40 });
    }, 100);
  });
  
  console.log('[main] Starting PTY with size:', cols, 'x', rows);
  writeEmacsLog(`PTY size: ${cols}x${rows}\n`);
  writeEmacsLog(`Command: emacsclient -nw -c --eval "(aesthetic-backend 'artery)"\n`);
  
  // Connect PTY - run aesthetic-backend to set up tabs
  ptyProcessFor3D = pty.spawn(dockerPath, [
    'exec', '-it', '-e', 'LANG=en_US.UTF-8', '-e', 'LC_ALL=en_US.UTF-8',
    '-e', `COLUMNS=${cols}`, '-e', `LINES=${rows}`,
    'aesthetic',
    'emacsclient', '-nw', '-c', '--eval', "(aesthetic-backend 'artery)"
  ], {
    name: 'xterm-256color',
    cols: cols,
    rows: rows,
    cwd: process.env.HOME,
    env: { ...process.env, TERM: 'xterm-256color', LANG: 'en_US.UTF-8', LC_ALL: 'en_US.UTF-8' }
  });
  
  let firstDataReceived = false;
  let sessionStartTime = Date.now();
  
  ptyProcessFor3D.onData((data) => {
    if (!webContents.isDestroyed()) {
      webContents.send('flip-pty-data', data);
      
      // After first data, send a resize to ensure emacs picks it up
      if (!firstDataReceived) {
        firstDataReceived = true;
        writeEmacsLog(`First data received, emacsclient connected.\n`);
        // Give emacs a moment to initialize, then force resize
        setTimeout(() => {
          if (ptyProcessFor3D) {
            console.log('[main] Sending post-connect resize:', lastKnownCols, 'x', lastKnownRows);
            try {
              ptyProcessFor3D.resize(lastKnownCols, lastKnownRows);
            } catch (e) {
              console.warn('[main] Post-connect resize failed:', e.message);
            }
            // Send another resize after a longer delay to catch any late initialization
            setTimeout(() => {
              if (ptyProcessFor3D) {
                try {
                  ptyProcessFor3D.resize(lastKnownCols, lastKnownRows);
                } catch (e) {}
              }
            }, 1000);
          }
        }, 500);
      }
    }
  });
  
  ptyProcessFor3D.onExit(({ exitCode, signal }) => {
    const duration = Math.round((Date.now() - sessionStartTime) / 1000);
    const exitInfo = `Session ended after ${duration}s - exit code: ${exitCode}, signal: ${signal}`;
    writeEmacsLog(`\n${exitInfo}\n`);
    console.log('[main]', exitInfo);
    
    if (!webContents.isDestroyed()) {
      webContents.send('flip-pty-data', '\r\n\x1b[33m[Session ended]\x1b[0m\r\n');
      
      // If session was very short (< 5 seconds) or crashed, show the log
      if (duration < 5 || exitCode !== 0) {
        const log = readEmacsLog();
        if (log) {
          webContents.send('flip-pty-data', '\r\n\x1b[33m--- Session Log (last 40 lines) ---\x1b[0m\r\n');
          const lines = log.split('\n').slice(-40);
          webContents.send('flip-pty-data', lines.join('\r\n') + '\r\n');
          webContents.send('flip-pty-data', '\x1b[33m--- End Log ---\x1b[0m\r\n');
          webContents.send('flip-pty-data', `\x1b[90mFull log: ${LOCAL_EMACS_LOG_PATH}\x1b[0m\r\n`);
        }
      }
    }
  });
}

// Start a local shell when docker isn't available
function startLocalShellForFlip(webContents) {
  // Find the best available shell
  const shellOptions = process.platform === 'darwin' 
    ? ['/opt/homebrew/bin/fish', '/usr/local/bin/fish', '/bin/zsh', '/bin/bash']
    : ['/usr/bin/fish', '/bin/bash', '/bin/sh'];
  
  let shellPath = '/bin/sh';
  for (const s of shellOptions) {
    if (fs.existsSync(s)) {
      shellPath = s;
      break;
    }
  }
  
  console.log('[main] Starting local shell:', shellPath);
  
  ptyProcessFor3D = pty.spawn(shellPath, [], {
    name: 'xterm-256color',
    cols: 120,
    rows: 40,
    cwd: process.env.HOME,
    env: { ...process.env, TERM: 'xterm-256color', LANG: 'en_US.UTF-8', LC_ALL: 'en_US.UTF-8' },
    env: { ...process.env, TERM: 'xterm-256color' }
  });
  
  ptyProcessFor3D.onData((data) => {
    if (!webContents.isDestroyed()) {
      webContents.send('flip-pty-data', data);
    }
  });
  
  ptyProcessFor3D.onExit(() => {
    if (!webContents.isDestroyed()) {
      webContents.send('flip-pty-data', '\r\n\x1b[33m[Session ended]\x1b[0m\r\n');
    }
  });
}

// Get the repo path (where this Electron app lives, which is inside aesthetic-computer repo)
function getRepoPath() {
  // In development, __dirname is ac-electron/, in packaged app it's inside Resources/app.asar
  // The repo is the parent of ac-electron
  const devRepoPath = path.resolve(__dirname, '..');
  if (fs.existsSync(path.join(devRepoPath, '.git'))) {
    return devRepoPath;
  }
  // For packaged app, try common locations
  const homeRepoPath = path.join(process.env.HOME, 'Desktop', 'code', 'aesthetic-computer');
  if (fs.existsSync(path.join(homeRepoPath, '.git'))) {
    return homeRepoPath;
  }
  return null;
}

// IPC: register a file dropped on a running AC window (path comes from
// renderer's HTML5 drop event — Electron's File object exposes .path).
ipcMain.handle('ac:register-dropped-file', async (_event, filePath) => {
  if (!filePath) return { ok: false, reason: 'no-path' };
  if (!acDropIsAudio(filePath)) return { ok: false, reason: 'not-audio' };
  await acDropHandleFile(filePath);
  return { ok: true };
});

// IPC Handlers for preferences
ipcMain.handle('get-preferences', () => preferences);

ipcMain.handle('set-preferences', (event, newPrefs) => {
  preferences = { ...preferences, ...newPrefs };
  savePreferences();
  updateTrayTitle();
  
  // Handle launch at login
  if (process.platform === 'darwin' || process.platform === 'win32') {
    app.setLoginItemSettings({
      openAtLogin: preferences.launchAtLogin,
      openAsHidden: true
    });
  }
  
  return preferences;
});

ipcMain.handle('set-tray-title', (event, text) => {
  updateTrayTitle(text);
});

// Update the current piece name (called when webview navigates)
ipcMain.handle('set-current-piece', (event, pieceName) => {
  currentPiece = pieceName || 'prompt';
  console.log('[main] Current piece updated:', currentPiece);
  updateTrayTitle();
  // Reflect the current piece in the native window title bar.
  try {
    const win = BrowserWindow.fromWebContents(event.sender);
    if (win && !win.isDestroyed()) win.setTitle(currentPiece);
  } catch (_) {}
  return currentPiece;
});

// Mark a window as the "main" window
ipcMain.handle('set-main-window', (event) => {
  const win = BrowserWindow.fromWebContents(event.sender);
  for (const [id, data] of windows) {
    if (data.window === win) {
      mainWindowId = id;
      console.log('[main] Main window set to:', id);
      return true;
    }
  }
  return false;
});

// IPC Handlers for welcome screen
ipcMain.handle('get-repo-path', () => {
  const repoPath = getRepoPath();
  if (repoPath) {
    // Return just the base name for cleaner display
    return { path: repoPath, name: path.basename(repoPath) };
  }
  return null;
});

ipcMain.handle('get-git-user', async () => {
  try {
    const name = execSync('git config user.name', { encoding: 'utf8', timeout: 3000 }).trim();
    const email = execSync('git config user.email', { encoding: 'utf8', timeout: 3000 }).trim();
    return { name, email };
  } catch (e) {
    return null;
  }
});

ipcMain.handle('start-flip-devcontainer', async (event) => {
  const dockerPath = getDockerPath();
  startContainerAndEmacs(dockerPath, event.sender);
  return { success: true };
});

ipcMain.handle('start-flip-local-shell', async (event) => {
  startLocalShellForFlip(event.sender);
  return { success: true };
});

// IPC Handlers
ipcMain.handle('get-mode', (event) => {
  // Find which window sent this
  const win = BrowserWindow.fromWebContents(event.sender);
  for (const [id, data] of windows) {
    if (data.window === win) return data.mode;
  }
  return 'production';
});

ipcMain.handle('get-urls', () => URLS);

// Get app info for desktop.mjs piece
ipcMain.handle('get-app-info', () => {
  return {
    version: app.getVersion(),
    electron: process.versions.electron,
    chrome: process.versions.chrome,
    platform: process.platform,
    arch: process.arch,
    isPackaged: app.isPackaged,
    updateAvailable: updateAvailable,
    latestVersion: updateAvailable?.version || null,
  };
});

// CDP (Chrome DevTools Protocol) info
ipcMain.handle('get-cdp-info', () => {
  const args = process.argv.join(' ');
  const cdpMatch = args.match(/--remote-debugging-port=(\d+)/);
  const inspectMatch = args.match(/--inspect=(\d+)/);
  return {
    enabled: !!cdpMatch,
    port: cdpMatch ? cdpMatch[1] : null,
    inspectPort: inspectMatch ? inspectMatch[1] : null
  };
});

ipcMain.handle('check-docker', async () => {
  console.log('[main] check-docker called');
  const result = await checkDocker();
  console.log('[main] check-docker result:', result);
  return result;
});

ipcMain.handle('check-container', async () => {
  console.log('[main] check-container called');
  const result = await checkDevcontainer();
  console.log('[main] check-container result:', result);
  return result;
});

ipcMain.handle('check-container-exists', async () => {
  console.log('[main] check-container-exists called');
  const result = await checkContainerExists();
  console.log('[main] check-container-exists result:', result);
  return result;
});

ipcMain.handle('start-existing-container', async () => {
  console.log('[main] start-existing-container called');
  try {
    await startExistingContainer();
    console.log('[main] start-existing-container success');
    return { success: true };
  } catch (err) {
    console.error('[main] start-existing-container error:', err);
    return { success: false, error: err.message };
  }
});

ipcMain.handle('start-container', async () => {
  console.log('[main] start-container called');
  try {
    await startDevcontainer();
    console.log('[main] start-container success');
    return { success: true };
  } catch (err) {
    console.error('[main] start-container error:', err);
    return { success: false, error: err.message };
  }
});

ipcMain.handle('stop-container', async () => {
  console.log('[main] stop-container called');
  try {
    const dockerPath = getDockerPath();
    await new Promise((resolve, reject) => {
      const docker = spawn(dockerPath, ['stop', 'aesthetic'], { stdio: 'pipe' });
      docker.on('close', (code) => {
        if (code === 0) {
          console.log('[main] Container stopped successfully');
          resolve(true);
        } else {
          reject(new Error(`docker stop exited with code ${code}`));
        }
      });
      docker.on('error', (err) => reject(err));
    });
    return { success: true };
  } catch (err) {
    console.error('[main] stop-container error:', err);
    return { success: false, error: err.message };
  }
});

ipcMain.handle('stop-container-aggressive', async () => {
  console.log('[main] stop-container-aggressive called');
  try {
    const dockerPath = getDockerPath();
    
    // Kill all processes in the container first
    console.log('[main] Killing all processes in container...');
    await new Promise((resolve) => {
      const kill = spawn(dockerPath, ['exec', 'aesthetic', 'pkill', '-9', '-f', '.*'], { stdio: 'pipe' });
      kill.on('close', () => resolve());
      kill.on('error', () => resolve());
    });
    
    // Give it a moment
    await new Promise(r => setTimeout(r, 500));
    
    // Force stop with timeout
    console.log('[main] Force stopping container...');
    await new Promise((resolve, reject) => {
      const docker = spawn(dockerPath, ['stop', '-t', '2', 'aesthetic'], { stdio: 'pipe' });
      docker.on('close', (code) => {
        console.log('[main] Container force stopped with code:', code);
        resolve(true);
      });
      docker.on('error', (err) => {
        console.error('[main] Error force stopping:', err);
        resolve(true); // Resolve anyway
      });
    });
    
    // Kill it if still running
    console.log('[main] Ensuring container is killed...');
    await new Promise((resolve) => {
      const kill = spawn(dockerPath, ['kill', 'aesthetic'], { stdio: 'pipe' });
      kill.on('close', () => resolve());
      kill.on('error', () => resolve());
    });
    
    return { success: true };
  } catch (err) {
    console.error('[main] stop-container-aggressive error:', err);
    return { success: false, error: err.message };
  }
});

// =============================================================================
// USB Flash Handlers (Linux only)
// =============================================================================

ipcMain.handle('usb:list-devices', async () => {
  console.log('[main] usb:list-devices called');
  if (process.platform !== 'linux') {
    return { success: false, error: 'USB flashing is only supported on Linux' };
  }

  try {
    const output = await new Promise((resolve, reject) => {
      const proc = spawn('lsblk', [
        '-J', '-d', '-o', 'NAME,SIZE,MODEL,TRAN,RM,HOTPLUG,TYPE'
      ], { stdio: 'pipe' });
      let stdout = '';
      let stderr = '';
      proc.stdout.on('data', (d) => stdout += d.toString());
      proc.stderr.on('data', (d) => stderr += d.toString());
      proc.on('close', (code) => {
        if (code === 0) resolve(stdout);
        else reject(new Error(`lsblk exited with code ${code}: ${stderr}`));
      });
      proc.on('error', (err) => reject(err));
    });

    const parsed = JSON.parse(output);
    const devices = (parsed.blockdevices || []).filter(dev =>
      dev.type === 'disk' &&
      (dev.tran === 'usb' || dev.rm === true || dev.rm === '1' ||
       dev.hotplug === true || dev.hotplug === '1')
    ).map(dev => ({
      name: dev.name,
      path: `/dev/${dev.name}`,
      size: dev.size,
      model: (dev.model || 'Unknown USB Device').trim(),
    }));

    console.log('[main] Found USB devices:', devices);
    return { success: true, devices };
  } catch (err) {
    console.error('[main] usb:list-devices error:', err);
    return { success: false, error: err.message };
  }
});

ipcMain.handle('usb:flash-image', async (event, { url, devicePath, filename }) => {
  console.log('[main] usb:flash-image called:', { url, devicePath, filename });
  if (process.platform !== 'linux') {
    return { success: false, error: 'USB flashing is only supported on Linux' };
  }

  if (!/^\/dev\/sd[a-z]$/.test(devicePath) && !/^\/dev\/nvme\d+n\d+$/.test(devicePath)) {
    return { success: false, error: `Invalid device path: ${devicePath}` };
  }

  const sender = event.sender;
  const tmpFile = path.join(app.getPath('temp'), filename || 'ac-os.img');

  const sendProgress = (stage, percent, message) => {
    if (!sender.isDestroyed()) {
      sender.send('usb:flash-progress', { stage, percent, message });
    }
  };

  try {
    // Phase 1: Download image to temp file
    sendProgress('download', 0, 'Starting download...');

    await new Promise((resolve, reject) => {
      const file = fs.createWriteStream(tmpFile);
      const request = net.request(url);
      let totalBytes = 0;
      let receivedBytes = 0;

      request.on('response', (response) => {
        const cl = response.headers['content-length'];
        totalBytes = parseInt(Array.isArray(cl) ? cl[0] : cl || '0', 10);

        response.on('data', (chunk) => {
          file.write(chunk);
          receivedBytes += chunk.length;
          if (totalBytes > 0) {
            const pct = Math.round((receivedBytes / totalBytes) * 100);
            sendProgress('download', pct,
              `Downloading: ${(receivedBytes / 1e9).toFixed(1)}GB / ${(totalBytes / 1e9).toFixed(1)}GB`);
          }
        });

        response.on('end', () => file.end(() => resolve()));
        response.on('error', (err) => { file.destroy(); reject(err); });
      });

      request.on('error', (err) => { file.destroy(); reject(err); });
      request.end();
    });

    sendProgress('download', 100, 'Download complete');

    // Phase 2: Unmount all partitions on the device
    sendProgress('unmount', 0, `Unmounting ${devicePath}...`);

    await new Promise((resolve) => {
      const umount = spawn('sh', ['-c', `umount ${devicePath}* 2>/dev/null`], { stdio: 'pipe' });
      umount.on('close', () => resolve());
      umount.on('error', () => resolve());
    });

    sendProgress('unmount', 100, 'Device unmounted');

    // Phase 3: Flash with dd via pkexec (privilege escalation)
    sendProgress('write', 0, `Writing to ${devicePath}...`);

    const imageSize = fs.statSync(tmpFile).size;

    await new Promise((resolve, reject) => {
      const dd = spawn('pkexec', [
        'dd', `if=${tmpFile}`, `of=${devicePath}`,
        'bs=4M', 'conv=fsync', 'status=progress'
      ], { stdio: ['pipe', 'pipe', 'pipe'] });

      let lastPercent = 0;

      dd.stderr.on('data', (data) => {
        const text = data.toString();
        const match = text.match(/(\d+)\s+bytes/);
        if (match && imageSize > 0) {
          const written = parseInt(match[1], 10);
          const pct = Math.round((written / imageSize) * 100);
          if (pct > lastPercent) {
            lastPercent = pct;
            sendProgress('write', pct,
              `Writing: ${(written / 1e9).toFixed(1)}GB / ${(imageSize / 1e9).toFixed(1)}GB`);
          }
        }
      });

      dd.on('close', (code) => {
        if (code === 0) resolve();
        else reject(new Error(`dd exited with code ${code}`));
      });
      dd.on('error', (err) => reject(err));
    });

    sendProgress('write', 100, 'Write complete');

    // Phase 4: Sync and eject
    sendProgress('eject', 0, 'Syncing and ejecting...');

    await new Promise((resolve) => {
      const sync = spawn('sync', [], { stdio: 'pipe' });
      sync.on('close', () => resolve());
      sync.on('error', () => resolve());
    });

    await new Promise((resolve) => {
      const eject = spawn('eject', [devicePath], { stdio: 'pipe' });
      eject.on('close', () => resolve());
      eject.on('error', () => resolve());
    });

    sendProgress('eject', 100, 'Device ejected safely');

    // Cleanup temp file
    try { fs.unlinkSync(tmpFile); } catch {}

    return { success: true };
  } catch (err) {
    console.error('[main] usb:flash-image error:', err);
    try { fs.unlinkSync(tmpFile); } catch {}
    return { success: false, error: err.message };
  }
});

ipcMain.handle('open-shell', async () => {
  console.log('[main] open-shell called');
  await openAcPaneWindow();
  return { success: true };
});

// PTY handlers for AC Pane windows (legacy path)
ipcMain.handle('connect-pty', async (event) => {
  console.log('[main] connect-pty called');
  if (!pty) {
    console.error('[main] node-pty not available');
    return false;
  }

  const win = BrowserWindow.fromWebContents(event.sender);
  let windowId = null;
  let winData = null;
  
  for (const [id, data] of windows) {
    if (data.window === win) {
      windowId = id;
      winData = data;
      break;
    }
  }

  if (!winData || winData.mode !== 'ac-pane') {
    console.error('connect-pty called on non-AC Pane window');
    return false;
  }

  try {
    // Spawn docker exec to get into the container with fish shell
    const dockerPath = getDockerPath();
    
    console.log('[main] Spawning PTY with docker at:', dockerPath);
    
    // Ensure PATH includes common locations for packaged app
    const extraPaths = '/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin';
    const fullPath = process.env.PATH ? `${process.env.PATH}:${extraPaths}` : extraPaths;
    
    const shellProcess = pty.spawn(dockerPath, ['exec', '-it', 'aesthetic', 'fish'], {
      name: 'xterm-256color',
      cols: 120,
      rows: 30,
      cwd: process.env.HOME,
      env: { 
        ...process.env, 
        TERM: 'xterm-256color',
        // Tell programs not to query for colors (eat/vim/etc respect this)
        COLORTERM: 'truecolor',
        // Prevent OSC query responses by indicating we don't support them
        VTE_VERSION: '',  // Empty signals no VTE terminal
        PATH: fullPath
      },
    });

    winData.ptyProcess = shellProcess;

    // Forward PTY output to renderer
    shellProcess.onData((data) => {
      console.log('[main] PTY data received, length:', data.length, 'preview:', data.substring(0, 50).replace(/\n/g, '\\n'));
      if (!win.isDestroyed()) {
        win.webContents.send('pty-data', data);
        console.log('[main] PTY data sent to renderer');
      } else {
        console.log('[main] Window destroyed, cannot send PTY data');
      }
    });

    shellProcess.onExit(({ exitCode }) => {
      console.log('[main] PTY exited with code:', exitCode);
      if (!win.isDestroyed()) {
        win.webContents.send('pty-exit', exitCode);
      }
    });

    return true;
  } catch (err) {
    console.error('[main] PTY spawn failed:', err.message);
    console.error('[main] This usually means node-pty needs rebuilding for Electron.');
    console.error('[main] Run: cd ac-electron && npx electron-rebuild');
    return false;
  }
});

ipcMain.on('pty-input', (event, data) => {
  const win = BrowserWindow.fromWebContents(event.sender);
  for (const [id, winData] of windows) {
    if (winData.window === win && winData.ptyProcess) {
      winData.ptyProcess.write(data);
      break;
    }
  }
});

ipcMain.on('pty-resize', (event, cols, rows) => {
  const win = BrowserWindow.fromWebContents(event.sender);
  for (const [id, winData] of windows) {
    if (winData.window === win && winData.ptyProcess) {
      try {
        winData.ptyProcess.resize(cols, rows);
      } catch (err) {
        // PTY may not be ready yet or already exited
        console.warn('[main] PTY resize failed:', err.message);
      }
      break;
    }
  }
});

// Global window move handler (for alt+scroll drag in any window)
ipcMain.on('move-window', (event, position) => {
  const senderWindow = BrowserWindow.fromWebContents(event.sender);
  if (senderWindow) {
    senderWindow.setPosition(Math.round(position.x), Math.round(position.y));
  }
});

// Open/close windows from inside the embedded AC prompt (via webview popup interception)
ipcMain.handle('ac-open-window', async (event, { url, index = 0, total = 1 } = {}) => {
  console.log('[main] ac-open-window called with url:', url, 'index:', index, 'total:', total);
  const sourceWindow = BrowserWindow.fromWebContents(event.sender);
  const { window: newWindow } = await openAcPaneWindowInternal({ sourceWindow, index });
  console.log('[main] openAcPaneWindow returned:', !!newWindow);
  if (url && newWindow) {
    newWindow.webContents.once('did-finish-load', () => {
      if (!newWindow.isDestroyed()) {
        console.log('[main] Sending navigate to new window:', url);
        newWindow.webContents.send('navigate', url);
      }
    });
  }
  return { success: true };
});

ipcMain.handle('ac-close-window', async (event) => {
  console.log('[main] ac-close-window called');
  const senderWindow = BrowserWindow.fromWebContents(event.sender);
  console.log('[main] sender window found:', !!senderWindow);
  if (senderWindow && !senderWindow.isDestroyed()) {
    console.log('[main] Closing window');
    senderWindow.close();
  }
  return { success: true };
});

// Open external URL in the system's default browser
ipcMain.handle('open-external-url', async (event, url) => {
  console.log('[main] Opening external URL:', url);
  shell.openExternal(url);
  return { success: true };
});

ipcMain.handle('switch-mode', async (event, mode) => {
  // Always open AC Pane
  await openAcPaneWindow();
  return 'ac-pane';
});

ipcMain.handle('spawn-terminal', async (event, command) => {
  // This would use node-pty in a real implementation
  return { success: true };
});

// Reboot/restart the Electron app
ipcMain.handle('app-reboot', async () => {
  console.log('[main] Rebooting app...');
  app.relaunch();
  app.quit();
  return { success: true };
});

// ========== IPC Bridge for Artery/Tests ==========
// Forward commands from external tools to webview and vice versa

// Navigate to a piece (from shell/artery-tui)
ipcMain.on('ac-navigate', (event, piece) => {
  // Forward to all AC panes
  for (const [id, winData] of windows) {
    if (winData.mode === 'ac-pane') {
      winData.window.webContents.send('ac-navigate', piece);
    }
  }
});

// Set environment (local/prod)
ipcMain.on('ac-set-env', (event, env) => {
  for (const [id, winData] of windows) {
    if (winData.mode === 'ac-pane') {
      winData.window.webContents.send('ac-set-env', env);
    }
  }
});

// Evaluate JavaScript in webview
ipcMain.handle('ac-eval', async (event, code) => {
  return new Promise((resolve) => {
    // Find the AC Pane
    for (const [id, winData] of windows) {
      if (winData.mode === 'ac-pane') {
        // Set up one-time listener for result
        const handler = (event, result) => {
          ipcMain.removeListener('ac-eval-result', handler);
          resolve(result);
        };
        ipcMain.on('ac-eval-result', handler);
        winData.window.webContents.send('ac-eval', code);
        // Timeout after 10 seconds
        setTimeout(() => {
          ipcMain.removeListener('ac-eval-result', handler);
          resolve({ success: false, error: 'Timeout' });
        }, 10000);
        return;
      }
    }
    resolve({ success: false, error: 'No AC Pane found' });
  });
});

// Get current state
ipcMain.handle('ac-get-state', async (event) => {
  return new Promise((resolve) => {
    for (const [id, winData] of windows) {
      if (winData.mode === 'ac-pane') {
        const handler = (event, state) => {
          ipcMain.removeListener('ac-state', handler);
          resolve(state);
        };
        ipcMain.on('ac-state', handler);
        winData.window.webContents.send('ac-get-state');
        setTimeout(() => {
          ipcMain.removeListener('ac-state', handler);
          resolve(null);
        }, 5000);
        return;
      }
    }
    resolve(null);
  });
});

// Take screenshot
ipcMain.handle('ac-screenshot', async (event) => {
  return new Promise((resolve) => {
    for (const [id, winData] of windows) {
      if (winData.mode === 'ac-pane') {
        const handler = (event, result) => {
          ipcMain.removeListener('ac-screenshot-result', handler);
          resolve(result);
        };
        ipcMain.on('ac-screenshot-result', handler);
        winData.window.webContents.send('ac-screenshot');
        setTimeout(() => {
          ipcMain.removeListener('ac-screenshot-result', handler);
          resolve({ success: false, error: 'Timeout' });
        }, 10000);
        return;
      }
    }
    resolve({ success: false, error: 'No AC Pane found' });
  });
});

// Reload webview
ipcMain.on('ac-reload', (event) => {
  for (const [id, winData] of windows) {
    if (winData.mode === 'ac-pane') {
      winData.window.webContents.send('ac-reload');
    }
  }
});

// ~ command: toggle DevTools docked at bottom, navigating to Console panel
ipcMain.on('open-devtools', (event) => {
  const win = BrowserWindow.fromWebContents(event.sender) || BrowserWindow.getFocusedWindow();
  if (!win) return;
  if (win.webContents.isDevToolsOpened()) {
    win.webContents.closeDevTools();
  } else {
    win.webContents.openDevTools({ mode: 'bottom' });
    win.webContents.once('devtools-opened', () => {
      try {
        win.webContents.devToolsWebContents?.executeJavaScript(
          'DevToolsAPI.showPanel("console")'
        ).catch(() => {});
      } catch (e) { /* ignore if DevTools API unavailable */ }
    });
  }
});

// App lifecycle
app.whenReady().then(async () => {
  loadPreferences();

  // Always sync the OS login-item setting with our stored preference on boot.
  // This way the preference survives updates / re-signing and is always honored.
  if (process.platform === 'darwin' || process.platform === 'win32') {
    try {
      app.setLoginItemSettings({
        openAtLogin: preferences.launchAtLogin,
        openAsHidden: true,
      });
    } catch (e) {
      console.warn('[main] setLoginItemSettings failed:', e.message);
    }
  }

  // Detect whether this launch was triggered by macOS at login (or with the
  // hidden flag). If so, we stay in menubar-daemon mode and don't pop the AC
  // window — the user will open things explicitly from the tray.
  let launchedSilently = false;
  if (process.platform === 'darwin') {
    try {
      const lis = app.getLoginItemSettings();
      launchedSilently = !!(lis.wasOpenedAtLogin || lis.wasOpenedAsHidden);
    } catch (e) {
      /* noop */
    }
  }
  // Hide the dock immediately so launch-at-login has zero visible footprint
  // until the user clicks a tray icon.
  if (launchedSilently) syncDockVisibility();

  // Allow clipboard read/write for AC content loaded over https/http or in
  // webviews. `navigator.clipboard.readText()` in bios.mjs (paste button +
  // TextInput paste in prompt/chat) is gated behind these permissions in
  // Electron; without handlers the request silently fails and nothing pastes.
  const allowClipboard = (permission) =>
    permission === 'clipboard-read' ||
    permission === 'clipboard-sanitized-write' ||
    permission === 'clipboard-write';
  const installClipboardPermissions = (sess) => {
    if (!sess || sess.__acClipboardWired) return;
    sess.__acClipboardWired = true;
    sess.setPermissionRequestHandler((_wc, permission, callback) => {
      if (allowClipboard(permission)) return callback(true);
      callback(false);
    });
    sess.setPermissionCheckHandler((_wc, permission) => {
      return allowClipboard(permission);
    });
  };
  installClipboardPermissions(session.defaultSession);

  createMenu();
  createSystemTray();
  // createNotepatTray();  // disabled — single pals systray icon only
  
  // Start FF1 Bridge server for kidlisp.com integration
  ff1Bridge.startBridge();
  
  // Start GitHub update checks (works in dev and production)
  startSiloUpdateChecks();
  
  // Check for updates on startup (production builds only)
  if (autoUpdater && !startInDevMode && app.isPackaged) {
    setTimeout(() => {
      console.log('[updater] Checking for updates on startup...');
      autoUpdater.checkForUpdates().catch(err => {
        console.log('[updater] Update check failed:', err.message);
      });
    }, 3000); // Wait 3s after startup
  }
  
  // Watch for reboot request file from devcontainer (electric-snake-bite)
  const REBOOT_MARKER = path.join(__dirname, '.reboot-requested');
  const checkRebootMarker = () => {
    if (fs.existsSync(REBOOT_MARKER)) {
      console.log('[main] ⚡🐍 Electric Snake Bite detected! Relaunching...');
      fs.unlinkSync(REBOOT_MARKER); // Clean up marker
      // Relaunch the app then quit current instance
      app.relaunch();
      app.quit();
    }
  };
  
  // Check every 2 seconds for reboot marker
  setInterval(checkRebootMarker, 2000);
  setInterval(checkRebootMarker, 2000);
  
  // Cold-launch with a file argument (Windows/Linux drop-onto-icon, or
  // Open With ... from a file manager). macOS routes this through
  // 'open-file' so we only need argv here.
  const acDropColdLaunchFile = process.argv.slice(1).find(acDropIsAudio);

  // Create initial window(s)
  // When launched silently at login, stay in menubar-daemon mode: no AC
  // window, no dock icon. The user opens things explicitly from the tray.
  if (!launchedSilently || acDropColdLaunchFile) {
    openAcPaneWindow({ piece: initialPiece });
  }

  if (acDropColdLaunchFile) {
    acDropHandleFile(acDropColdLaunchFile);
  }

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      openAcPaneWindow();
    }
  });

  // Keep dock visibility in sync with window state on macOS so the app
  // fades out of the dock / app-switcher whenever all windows close, and
  // comes back as soon as a window opens.
  app.on('browser-window-created', (_, win) => {
    syncDockVisibility();
    win.on('show', syncDockVisibility);
    win.on('hide', syncDockVisibility);
    win.on('closed', syncDockVisibility);
  });
  
  // Allow webview preload scripts (required for webview-preload.js to work)
  app.on('web-contents-created', (_, contents) => {
    installClipboardPermissions(contents.session);
    contents.on('will-attach-webview', (wawevent, webPreferences, params) => {
      // Verify the preload path is our legitimate webview-preload.js
      const preloadPath = params.preload;
      if (preloadPath) {
        // Use dev path if in dev mode, otherwise use bundled path
        const expectedPreload = getAppPath('webview-preload.js');
        // Allow if it matches our webview-preload.js
        if (preloadPath.includes('webview-preload.js')) {
          // Set the absolute path for the preload
          webPreferences.preload = expectedPreload;
          console.log('[main] Allowing webview preload:', expectedPreload);
        } else {
          // Block unknown preload scripts for security
          console.warn('[main] Blocking unknown webview preload:', preloadPath);
          delete webPreferences.preload;
        }
      }
    });
  });
});

// Handle webview window.open() calls (popups) from guest content
// This is required in Electron 12+ as new-window event is deprecated
app.on('web-contents-created', (event, contents) => {
  // Only handle webviews
  if (contents.getType() === 'webview') {
    contents.on('will-navigate', (navEvent, url) => {
      if (!url) return;
      if (url.startsWith('ac://close')) {
        navEvent.preventDefault();
        const allWindows = BrowserWindow.getAllWindows();
        for (const win of allWindows) {
          if (!win.isDestroyed() && win.webContents.id === contents.hostWebContents?.id) {
            win.close();
            break;
          }
        }
        return;
      }
      // Handle 'local' / 'prod' commands - switch between dev and production servers
      try {
        const navUrl = new URL(url);
        const pathname = navUrl.pathname.replace(/^\//, '');
        if (pathname === 'local' || pathname === 'prod') {
          navEvent.preventDefault();
          const base = pathname === 'local'
            ? 'http://localhost:8888'
            : 'https://aesthetic.computer';
          const hostWin = BrowserWindow.getAllWindows().find(win =>
            !win.isDestroyed() && win.webContents.id === contents.hostWebContents?.id
          );
          if (hostWin) {
            console.log(`[main] Switching to ${pathname} server: ${base}`);
            hostWin.webContents.send('navigate', `${base}/prompt?desktop`);
          }
          return;
        }
      } catch (e) {}
      if (url.startsWith('ac://open')) {
        navEvent.preventDefault();
        let targetUrl = '';
        try {
          const urlObj = new URL(url);
          targetUrl = urlObj.searchParams.get('url') || '';
        } catch (err) {
          console.warn('[main] Failed to parse ac://open URL:', url, err.message);
        }
        openAcPaneWindow().then(({ window: newWin }) => {
          if (newWin && !newWin.isDestroyed()) {
            newWin.webContents.once('did-finish-load', () => {
              if (!newWin.isDestroyed() && targetUrl) {
                newWin.webContents.send('navigate', targetUrl);
              }
            });
          }
        });
      }
    });

    contents.setWindowOpenHandler(({ url }) => {
      console.log('[main] Webview window.open:', url);
      
      // Handle ac://close request (from prompt.mjs '-' command)
      if (url.startsWith('ac://close')) {
        // Find the parent BrowserWindow of this webview
        const allWindows = BrowserWindow.getAllWindows();
        for (const win of allWindows) {
          if (!win.isDestroyed() && win.webContents.id === contents.hostWebContents?.id) {
            win.close();
            break;
          }
        }
        return { action: 'deny' };
      }
      
      // Check if this is an external URL (not aesthetic.computer or localhost)
      // External URLs should open in the system's default browser
      try {
        const urlObj = new URL(url);
        const isExternal = !urlObj.hostname.includes('aesthetic.computer') &&
                          !urlObj.hostname.includes('localhost') &&
                          !urlObj.hostname.includes('127.0.0.1') &&
                          !url.startsWith('ac://');
        
        if (isExternal) {
          console.log('[main] Opening external URL in system browser:', url);
          shell.openExternal(url);
          return { action: 'deny' };
        }
      } catch (e) {
        console.warn('[main] Failed to parse URL:', url, e.message);
      }
      
      // Handle new window request (from prompt.mjs '+' command)
      // Open a new AC Pane and navigate to the URL
      // Find the source window from the webview's host
      const sourceWindow = BrowserWindow.getAllWindows().find(win => 
        !win.isDestroyed() && win.webContents.id === contents.hostWebContents?.id
      );
      openAcPaneWindowInternal({ sourceWindow, index: 0 }).then(({ window: newWin }) => {
        if (newWin && !newWin.isDestroyed()) {
          newWin.webContents.once('did-finish-load', () => {
            if (!newWin.isDestroyed()) {
              newWin.webContents.send('navigate', url);
            }
          });
        }
      });
      
      return { action: 'deny' }; // We handle it ourselves
    });
  }
});

app.on('window-all-closed', () => {
  // Keep tray icons (AC + Notepat) alive on macOS so the menu bar remains an entry point.
  // Cmd+Q still quits explicitly. On Windows/Linux, close-all still quits.
  if (process.platform !== 'darwin') {
    app.quit();
    return;
  }
  // Drop out of the dock / app-switcher — pure menubar daemon mode.
  syncDockVisibility();
});

app.on('will-quit', () => {
  globalShortcut.unregisterAll();
  
  // Stop FF1 Bridge server
  ff1Bridge.stopBridge();
  
  // Kill all PTY processes
  for (const [id, winData] of windows) {
    if (winData.ptyProcess) {
      winData.ptyProcess.kill();
    }
  }
  
  // NOTE: We intentionally do NOT stop the devcontainer when the app quits.
  // The devcontainer should keep running for the VS Code workspace.
});

// Handle certificate errors for localhost in dev mode
app.on('certificate-error', (event, webContents, url, error, certificate, callback) => {
  if (url.startsWith('https://localhost')) {
    event.preventDefault();
    callback(true);
  } else {
    callback(false);
  }
});

// Handle SIGTERM/SIGINT for clean shutdown (when killed via pkill, etc)
function cleanup() {
  console.log('[main] Cleanup triggered');
  
  // Kill all PTY processes
  for (const [id, winData] of windows) {
    if (winData.ptyProcess) {
      winData.ptyProcess.kill();
    }
  }
  
  // NOTE: We intentionally do NOT stop the devcontainer.
  // It should keep running for the VS Code workspace.
  
  process.exit(0);
}

process.on('SIGTERM', cleanup);
process.on('SIGINT', cleanup);
