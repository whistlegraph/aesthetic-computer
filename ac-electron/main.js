/**
 * Aesthetic Computer - Electron Main Process
 *
 * Single window type:
 * - AC Pane (3D flip view with front webview + back terminal)
 */

const { app, BrowserWindow, ipcMain, globalShortcut, Menu, Tray, dialog, shell, nativeImage, screen, Notification, net } = require('electron');
const path = require('path');
const fs = require('fs');
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
// Use Metal for GPU acceleration on macOS
app.commandLine.appendSwitch('use-angle', 'metal');

// Preferences storage
const PREFS_PATH = path.join(app.getPath('userData'), 'preferences.json');
let preferences = {
  showTrayTitle: true,
  trayTitleText: 'AC',  // Short text next to tray icon
  launchAtLogin: false,
  defaultMode: 'ac-pane'
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
app.setName('Aesthetic Computer');
process.title = 'Aesthetic Computer';

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
const pieceArg = args.find(a => a.startsWith('--piece=')) || args[args.indexOf('--piece') + 1];
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
      label: 'Aesthetic Computer', // Explicit name for menu bar
      submenu: [
        {
          label: 'About Aesthetic Computer',
          click: () => {
            dialog.showMessageBox({
              type: 'info',
              title: 'About Aesthetic Computer',
              message: 'Aesthetic Computer',
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

// ========== System Tray ==========
let tray = null;

function createSystemTray() {
  // Use template icon for proper macOS menu bar appearance
  // Template images should be black with transparency, macOS will invert for dark mode
  let iconPath;
  if (process.platform === 'darwin') {
    // In production, icons are in Resources folder; in dev, in build/icons
    if (app.isPackaged) {
      iconPath = path.join(process.resourcesPath, 'trayTemplate.png');
    } else {
      iconPath = path.join(__dirname, 'build', 'icons', 'trayTemplate.png');
    }
  } else {
    // Windows/Linux - use packaged icon in production
    if (app.isPackaged) {
      iconPath = path.join(process.resourcesPath, 'tray-icon.png');
    } else {
      iconPath = path.join(__dirname, 'build', 'icons', '16x16.png');
    }
  }
  
  console.log('[main] Loading tray icon from:', iconPath);
  const icon = nativeImage.createFromPath(iconPath);
  
  if (icon.isEmpty()) {
    console.warn('[main] Tray icon is empty! Path:', iconPath);
    return;
  }
  
  // Make it template on macOS for proper dark/light mode support
  if (process.platform === 'darwin') {
    icon.setTemplateImage(true);
  }
  
  // Store original icon for blink toggling
  originalTrayIcon = icon;
  updateTrayIcon = createUpdateIcon(icon);
  
  tray = new Tray(icon);
  tray.setToolTip('Aesthetic Computer');
  console.log('[main] System tray created successfully');
  
  // Build and set the context menu
  rebuildTrayMenu();
  
  // On macOS, single click shows menu, on Windows/Linux it toggles window
  if (process.platform !== 'darwin') {
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
  
  // File-like section
  menuItems.push({
    label: 'Show/Hide',
    accelerator: isMac ? 'Cmd+H' : 'Ctrl+H',
    click: () => {
      const allWindows = BrowserWindow.getAllWindows();
      if (allWindows.length > 0) {
        const win = allWindows[0];
        if (win.isVisible()) {
          allWindows.forEach(w => w.hide());
        } else {
          allWindows.forEach(w => w.show());
        }
      } else {
        openAcPaneWindow();
      }
    }
  });
  
  menuItems.push({ type: 'separator' });
  
  menuItems.push({
    label: 'New AC Pane',
    accelerator: isMac ? 'Cmd+N' : 'Ctrl+N',
    click: () => openAcPaneWindow()
  });
  
  // Quick DevTools access (especially for Windows)
  menuItems.push({
    label: 'Open DevTools',
    accelerator: 'F12',
    click: () => {
      const win = BrowserWindow.getFocusedWindow() || BrowserWindow.getAllWindows()[0];
      if (win) {
        // Try to open webview devtools if flip-view
        win.webContents.send('open-devtools');
        // Also open main window devtools
        win.webContents.openDevTools({ mode: 'detach' });
      }
    }
  });
  
  menuItems.push({ type: 'separator' });
  
  // FF1 Art Computer Bridge section
  const ff1Devices = ff1Bridge.getDevices();
  const ff1Running = ff1Bridge.isRunning();
  menuItems.push({
    label: '🖼️ FF1 Art Computer',
    submenu: [
      {
        label: ff1Running ? '✓ Bridge Running (port 19999)' : '✗ Bridge Not Running',
        enabled: false
      },
      {
        label: `Devices Found: ${ff1Devices.length}`,
        enabled: false
      },
      { type: 'separator' },
      ...ff1Devices.map(device => ({
        label: `${device.deviceId} (${device.ip})`,
        submenu: [
          {
            label: 'Cast Current Piece',
            click: () => {
              const win = getFocusedWindow();
              if (win) {
                const mode = getFocusedWindowMode();
                const baseUrl = mode === 'production' ? 'https://aesthetic.computer' : 'http://localhost:8888';
                const piece = currentPiece || 'prompt';
                const url = `${baseUrl}/${piece}`;
                // Send cast request through the bridge
                const http = require('http');
                const payload = JSON.stringify({
                  deviceId: device.deviceId,
                  playlist: [{ url, duration: 0 }]
                });
                const req = http.request({
                  hostname: '127.0.0.1',
                  port: 19999,
                  path: '/cast',
                  method: 'POST',
                  headers: { 'Content-Type': 'application/json' }
                });
                req.write(payload);
                req.end();
              }
            }
          },
          {
            label: 'Open Device IP',
            click: () => shell.openExternal(`http://${device.ip}:1111`)
          }
        ]
      })),
      ...(ff1Devices.length === 0 ? [{
        label: 'No devices found',
        enabled: false
      }] : []),
      { type: 'separator' },
      {
        label: 'Scan for Devices',
        click: () => {
          ff1Bridge.scanForDevices();
          setTimeout(() => rebuildTrayMenu(), 3000);
        }
      },
      {
        label: 'Open KidLisp.com Editor',
        click: () => shell.openExternal('https://kidlisp.com')
      },
      { type: 'separator' },
      {
        label: 'About FF1 Bridge',
        click: () => {
          dialog.showMessageBox({
            type: 'info',
            title: 'FF1 Art Computer Bridge',
            message: 'FF1 Bridge',
            detail: 'The FF1 Bridge allows kidlisp.com to send KidLisp pieces to FF1 Art Computers on your local network.\n\nThe bridge runs on port 19999 and automatically discovers FF1 devices via mDNS.\n\nVisit kidlisp.com to create and cast pieces!'
          });
        }
      }
    ]
  });
  
  menuItems.push({ type: 'separator' });
  
  // Edit section
  menuItems.push({
    label: 'Edit',
    submenu: [
      { role: 'undo' },
      { role: 'redo' },
      { type: 'separator' },
      { role: 'cut' },
      { role: 'copy' },
      { role: 'paste' },
      { role: 'selectAll' }
    ]
  });
  
  // View section  
  menuItems.push({
    label: 'View',
    submenu: [
      { role: 'reload' },
      { role: 'forceReload' },
      { role: 'toggleDevTools' },
      { type: 'separator' },
      { role: 'resetZoom' },
      { role: 'zoomIn' },
      { role: 'zoomOut' },
      { type: 'separator' },
      { role: 'togglefullscreen' }
    ]
  });
  
  // Navigate to pieces
  menuItems.push({
    label: 'Navigate',
    submenu: [
      {
        label: 'Home (prompt)',
        click: () => navigateToPiece('prompt')
      },
      {
        label: 'Starfield',
        click: () => navigateToPiece('starfield')
      },
      {
        label: '1v1',
        click: () => navigateToPiece('1v1')
      },
      { type: 'separator' },
      {
        label: 'Custom Piece...',
        click: () => {
          const win = getFocusedWindow();
          if (win) {
            win.webContents.executeJavaScript(`
              const piece = prompt('Enter piece name:');
              if (piece) window.location.href = window.location.origin + '/' + piece + '?nogap';
            `);
          }
        }
      }
    ]
  });
  
  menuItems.push({ type: 'separator' });
  
  // Settings
  menuItems.push({
    label: 'Preferences...',
    accelerator: isMac ? 'Cmd+,' : 'Ctrl+,',
    click: () => openPreferencesWindow()
  });
  
  menuItems.push({ type: 'separator' });
  
  // Help section
  menuItems.push({
    label: 'Help',
    submenu: [
      {
        label: 'Documentation',
        click: () => shell.openExternal('https://aesthetic.computer/docs')
      },
      {
        label: 'GitHub Repository',
        click: () => shell.openExternal('https://github.com/whistlegraph/aesthetic-computer')
      },
      {
        label: 'Check for Updates',
        click: () => {
          checkSiloForUpdates();
          if (!updateAvailable) {
            dialog.showMessageBox({
              type: 'info',
              title: 'No Updates',
              message: 'You\'re running the latest version!',
              detail: `Current version: ${app.getVersion()}`
            });
          }
        }
      },
      { type: 'separator' },
      {
        label: `About Aesthetic Computer`,
        click: () => {
          dialog.showMessageBox({
            type: 'info',
            title: 'About Aesthetic Computer',
            message: 'Aesthetic Computer',
            detail: `Version: ${app.getVersion()}\nElectron: ${process.versions.electron}\nChrome: ${process.versions.chrome}\nNode: ${process.versions.node}`
          });
        }
      }
    ]
  });
  
  menuItems.push({ type: 'separator' });
  
  menuItems.push({
    label: 'Quit',
    accelerator: isMac ? 'Cmd+Q' : 'Alt+F4',
    click: () => app.quit()
  });
  
  const contextMenu = Menu.buildFromTemplate(menuItems);
  tray.setContextMenu(contextMenu);
}

// ========== End System Tray ==========

// Update the tray title text (shown next to icon in menu bar)
function updateTrayTitle(text) {
  if (!tray) return;
  if (process.platform === 'darwin') {
    if (text !== undefined) {
      // Explicit text provided
      tray.setTitle(text);
    } else if (preferences.showTrayTitle) {
      // Show current piece name if available, otherwise use preference text
      const displayText = currentPiece ? `/${currentPiece}` : (preferences.trayTitleText || '');
      tray.setTitle(displayText);
    } else {
      tray.setTitle('');
    }
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
  
  // Start with a wide window - extra height for mode tags at bottom
  const winWidth = 680;
  const winHeight = 520;
  
  // Calculate position to avoid overlap
  const { x, y } = getOffsetWindowPosition(sourceWindow, index);
  
  const win = new BrowserWindow({
    width: winWidth,
    height: winHeight,
    x,
    y,
    minWidth: 320,
    minHeight: 240,
    title: 'AC Pane',
    frame: false,
    transparent: !isPaperWM,
    hasShadow: isPaperWM,
    alwaysOnTop: !isPaperWM,
    backgroundColor: isPaperWM ? '#000000' : '#00000000',
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      webviewTag: true, // Enable webview for front side
    },
  });
  
  win.loadFile(getAppPath('renderer/flip-view.html'));
  
  // Track it
  const windowId = windowIdCounter++;
  windows.set(windowId, { window: win, mode: 'ac-pane' });
  
  // Track focus
  win.on('focus', () => {
    focusedWindowId = windowId;
  });
  
  // Register global shortcut Cmd+B (Mac) / Ctrl+B (Windows/Linux) to toggle flip
  const flipShortcut = process.platform === 'darwin' ? 'CommandOrControl+B' : 'Ctrl+B';
  globalShortcut.register(flipShortcut, () => {
    // Send toggle to the flip view window
    if (win && !win.isDestroyed()) {
      win.webContents.send('toggle-flip');
    }
  });
  
  win.on('closed', () => {
    windows.delete(windowId);
    if (focusedWindowId === windowId) {
      focusedWindowId = null;
    }
    globalShortcut.unregister(flipShortcut);
    globalShortcut.unregister('CommandOrControl+Plus');
    globalShortcut.unregister('CommandOrControl+=');
    globalShortcut.unregister('CommandOrControl+-');
    globalShortcut.unregister('CommandOrControl+0');
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

// App lifecycle
app.whenReady().then(async () => {
  loadPreferences();
  createMenu();
  createSystemTray();
  
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
  
  // Create initial window(s)
  // Always start with an AC Pane window
  openAcPaneWindow();

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      openAcPaneWindow();
    }
  });
  
  // Allow webview preload scripts (required for webview-preload.js to work)
  app.on('web-contents-created', (_, contents) => {
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
  // Quit when all windows are closed, even on macOS
  app.quit();
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
