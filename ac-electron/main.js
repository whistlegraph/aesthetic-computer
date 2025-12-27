/**
 * Aesthetic Computer - Electron Main Process
 * 
 * Three window types:
 * - Production windows: Full webview to aesthetic.computer (green accent)
 * - Development windows: Webview to localhost:8888 (orange accent) 
 * - Shell windows: Terminal with devcontainer + emacs (purple accent)
 */

const { app, BrowserWindow, ipcMain, globalShortcut, Menu, dialog, shell, nativeImage, screen, Notification } = require('electron');
const path = require('path');
const fs = require('fs');
const { spawn, execSync } = require('child_process');

// Auto-updater (only in production builds)
let autoUpdater;
let updateDownloaded = false;
const UPDATE_CHECK_INTERVAL = 60 * 60 * 1000; // Check every hour

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
  console.log('[updater] Auto-updater not available (dev mode):', e.message);
}

// Start periodic update checks
function startUpdateChecks() {
  if (!autoUpdater || !app.isPackaged) return;
  
  const checkForUpdates = () => {
    if (!updateDownloaded) {
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
  console.warn('node-pty not available, shell windows will be limited');
}

// Parse command line args
const args = process.argv.slice(2);
const startInDevMode = args.includes('--dev') || args.includes('--development');
const startWithShell = args.includes('--shell');
const pieceArg = args.find(a => a.startsWith('--piece=')) || args[args.indexOf('--piece') + 1];
const initialPiece = pieceArg?.replace('--piece=', '') || 'prompt';

// URLs
const URLS = {
  production: `https://aesthetic.computer/${initialPiece}`,
  development: `http://localhost:8888/${initialPiece}`
};

// Track all windows: windowId -> { window, mode, ptyProcess? }
const windows = new Map();
let windowIdCounter = 0;
let focusedWindowId = null;
let shellWindowId = null; // Singleton tracking for shell window

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
    return windows.get(focusedWindowId).mode;
  }
  return 'production';
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
              detail: `Version ${app.getVersion()}\n\nA creative coding platform.`,
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
              dialog.showMessageBox({
                type: 'info',
                title: 'Updates',
                message: 'Auto-updates are not available in development mode.',
                buttons: ['OK']
              });
            }
          }
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
          label: 'New Window',
          accelerator: 'CmdOrCtrl+N',
          click: () => createWindow('production')
        },
        {
          label: 'New Development Window',
          accelerator: 'CmdOrCtrl+Shift+N',
          click: () => openDevWindow()
        },
        {
          label: 'New 3D View (Experimental)',
          accelerator: 'CmdOrCtrl+Shift+3',
          click: () => open3DWindow()
        },
        {
          label: 'New Shell',
          accelerator: 'CmdOrCtrl+Shift+T',
          click: () => openShellWindow()
        },
        { type: 'separator' },
        {
          label: 'KidLisp',
          accelerator: 'CmdOrCtrl+K',
          click: () => openKidLispWindow()
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
  getFocusedWindow()?.webContents.send('navigate', `${baseUrl}/${piece}`);
}

function createWindow(mode = 'production') {
  const windowId = ++windowIdCounter;
  const isShell = mode === 'shell';
  const isDev = mode === 'development';
  
  // Different colors: green=prod, orange=dev, purple=shell
  const bgColor = isShell ? '#0a0012' : (isDev ? '#0a0a12' : '#000a0a');
  const titleSuffix = isShell ? ' [Shell]' : (isDev ? ' [DEV]' : '');
  
  // Shell and Dev windows need nodeIntegration for xterm
  // Dev window now has an integrated terminal overlay
  const webPreferences = (isShell || isDev) ? {
    nodeIntegration: true,
    contextIsolation: false,
    webviewTag: true, // Dev needs webview for AC content
  } : {
    preload: path.join(__dirname, 'preload.js'),
    nodeIntegration: false,
    contextIsolation: true,
    webviewTag: true,
  };
  
  // Get screen dimensions for smart positioning
  const primaryDisplay = screen.getPrimaryDisplay();
  const { width: screenWidth, height: screenHeight } = primaryDisplay.workAreaSize;
  
  // Calculate window sizes and positions
  let winWidth, winHeight, winX, winY;
  
  if (isDev) {
    // Dev window: compact, cute size - positioned center-right
    winWidth = 900;
    winHeight = 700;
    winX = Math.floor((screenWidth - winWidth) / 2) + 100;
    winY = Math.floor((screenHeight - winHeight) / 2);
  } else if (isShell) {
    // Standalone shell: compact, right side of screen
    winWidth = 800;
    winHeight = 600;
    winX = screenWidth - winWidth - 50;
    winY = Math.floor((screenHeight - winHeight) / 2);
  } else {
    // Production: centered, compact
    winWidth = 1024;
    winHeight = 768;
    winX = Math.floor((screenWidth - winWidth) / 2);
    winY = Math.floor((screenHeight - winHeight) / 2);
  }
  
  const windowOptions = {
    width: winWidth,
    height: winHeight,
    x: winX,
    y: winY,
    minWidth: 600,
    minHeight: 400,
    // Frameless window for dev mode - just frame: false, no titleBarStyle
    frame: !isDev,
    backgroundColor: bgColor,
    title: `Aesthetic Computer${titleSuffix}`,
    webPreferences,
    show: false,
  };

  const win = new BrowserWindow(windowOptions);
  
  // Store window reference
  windows.set(windowId, { window: win, mode, ptyProcess: null });
  
  // Track focus
  win.on('focus', () => {
    focusedWindowId = windowId;
  });

  // Show when ready
  win.once('ready-to-show', () => {
    win.show();
    // Auto-open devtools in dev mode
    if (isDev) {
      win.webContents.openDevTools({ mode: 'detach' });
    }
  });

  // Load appropriate HTML
  const htmlFile = isShell ? 'shell.html' : (isDev ? 'development.html' : 'production.html');
  win.loadFile(path.join(__dirname, 'renderer', htmlFile));

  // Cleanup on close
  win.on('closed', () => {
    // Kill PTY if this was a shell window
    const winData = windows.get(windowId);
    if (winData?.ptyProcess) {
      winData.ptyProcess.kill();
    }
    windows.delete(windowId);
    if (focusedWindowId === windowId) {
      focusedWindowId = null;
    }
    // Clear shell singleton reference
    if (shellWindowId === windowId) {
      shellWindowId = null;
    }
  });
  
  return { window: win, windowId };
}

// Open a new dev window - now uses 3D view
async function openDevWindow() {
  return open3DWindow();
}

// Open a shell window (singleton - starts container and emacs)
async function openShellWindow() {
  // If shell window already exists, focus it
  if (shellWindowId && windows.has(shellWindowId)) {
    const shellWin = windows.get(shellWindowId).window;
    if (shellWin && !shellWin.isDestroyed()) {
      shellWin.focus();
      return { window: shellWin, windowId: shellWindowId };
    }
  }
  
  // Check Docker first
  const dockerOk = await checkDocker();
  if (!dockerOk) {
    dialog.showErrorBox(
      'Docker Required',
      'Shell mode requires Docker Desktop to be running.\n\n' +
      'Please start Docker Desktop and try again.'
    );
    return null;
  }

  const result = createWindow('shell');
  shellWindowId = result.windowId;
  return result;
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

async function open3DWindow() {
  // Start with a wide window
  const winWidth = 680;
  const winHeight = 480;
  
  const win = new BrowserWindow({
    width: winWidth,
    height: winHeight,
    minWidth: 320,
    minHeight: 240,
    title: 'Aesthetic Computer',
    frame: false,
    transparent: true,
    hasShadow: false,
    backgroundColor: '#00000000',
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      webviewTag: true, // Enable webview for front side
    },
  });
  
  win.loadFile(path.join(__dirname, 'renderer', 'flip-view.html'));
  
  // Track it
  const windowId = windowIdCounter++;
  windows.set(windowId, { window: win, mode: '3d' });
  
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
    if (ptyProcessFor3D) {
      ptyProcessFor3D.write(data);
    }
  });
  
  ipcMain.on('flip-pty-resize', (event, cols, rows) => {
    if (ptyProcessFor3D) {
      try {
        ptyProcessFor3D.resize(cols, rows);
      } catch (err) {
        console.warn('[main] Flip PTY resize failed:', err.message);
      }
    }
  });
  
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
  
  return win;
}

async function ensureContainerForFlip(dockerPath, webContents) {
  const { spawn } = require('child_process');
  
  // Check if container exists
  const containerExists = await new Promise((resolve) => {
    const check = spawn(dockerPath, ['ps', '-a', '--filter', 'name=aesthetic', '--format', '{{.Names}}'], { stdio: 'pipe' });
    let output = '';
    check.stdout.on('data', (data) => output += data.toString());
    check.on('close', () => resolve(output.includes('aesthetic')));
    check.on('error', () => resolve(false));
  });
  
  if (!containerExists) {
    webContents.send('flip-pty-data', '\x1b[31mNo container found. Please run devcontainer first.\x1b[0m\r\n');
    return;
  }
  
  // Always restart container to ensure clean state
  webContents.send('flip-pty-data', '\x1b[33mRestarting container...\x1b[0m\r\n');
  await new Promise((resolve) => {
    const restart = spawn(dockerPath, ['restart', 'aesthetic'], { stdio: 'pipe' });
    restart.on('close', () => resolve());
    restart.on('error', () => resolve());
  });
  
  // Wait for container to be ready
  await new Promise(r => setTimeout(r, 2000));
  
  // Start emacs daemon fresh
  webContents.send('flip-pty-data', '\x1b[33mStarting emacs daemon...\x1b[0m\r\n');
  await new Promise((resolve) => {
    const emacs = spawn(dockerPath, ['exec', 'aesthetic', 'emacs', '--daemon'], { stdio: 'pipe' });
    emacs.on('close', () => resolve());
    emacs.on('error', () => resolve());
  });
  
  // Wait for emacs to be ready
  await new Promise(r => setTimeout(r, 1000));
  
  // Connect PTY
  ptyProcessFor3D = pty.spawn(dockerPath, [
    'exec', '-it', 'aesthetic',
    'fish', '-c', 'emacsclient -nw -a "" || fish'
  ], {
    name: 'xterm-256color',
    cols: 120,
    rows: 40,
    cwd: process.env.HOME,
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

ipcMain.handle('open-shell', async () => {
  console.log('[main] open-shell called');
  await openShellWindow();
  return { success: true };
});

// PTY handlers for shell windows
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

  if (!winData || (winData.mode !== 'shell' && winData.mode !== 'development')) {
    console.error('connect-pty called on non-shell/dev window');
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

ipcMain.handle('switch-mode', async (event, mode) => {
  // In multi-window mode, we open a new window instead of switching
  if (mode === 'development') {
    await openDevWindow();
  } else if (mode === 'shell') {
    await openShellWindow();
  } else {
    createWindow('production');
  }
  return mode;
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
// Forward commands from shell to webview and vice versa

// Navigate to a piece (from shell/artery-tui)
ipcMain.on('ac-navigate', (event, piece) => {
  // Forward to all dev windows
  for (const [id, winData] of windows) {
    if (winData.mode === 'development') {
      winData.window.webContents.send('ac-navigate', piece);
    }
  }
});

// Set environment (local/prod)
ipcMain.on('ac-set-env', (event, env) => {
  for (const [id, winData] of windows) {
    if (winData.mode === 'development') {
      winData.window.webContents.send('ac-set-env', env);
    }
  }
});

// Evaluate JavaScript in webview
ipcMain.handle('ac-eval', async (event, code) => {
  return new Promise((resolve) => {
    // Find the dev window
    for (const [id, winData] of windows) {
      if (winData.mode === 'development') {
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
    resolve({ success: false, error: 'No dev window found' });
  });
});

// Get current state
ipcMain.handle('ac-get-state', async (event) => {
  return new Promise((resolve) => {
    for (const [id, winData] of windows) {
      if (winData.mode === 'development') {
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
      if (winData.mode === 'development') {
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
    resolve({ success: false, error: 'No dev window found' });
  });
});

// Reload webview
ipcMain.on('ac-reload', (event) => {
  for (const [id, winData] of windows) {
    if (winData.mode === 'development') {
      winData.window.webContents.send('ac-reload');
    }
  }
});

// App lifecycle
app.whenReady().then(async () => {
  createMenu();
  
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
  
  // Create initial window(s) based on CLI args
  // Default to dev window with integrated terminal overlay (press Cmd+` to toggle)
  // Use --production flag to open production window instead
  if (startWithShell) {
    await openShellWindow();
  } else if (startInDevMode || !args.includes('--production')) {
    // Default to dev mode
    openDevWindow();
  } else {
    createWindow('production');
  }

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      openDevWindow();
    }
  });
});

app.on('window-all-closed', () => {
  // Quit when all windows are closed, even on macOS
  app.quit();
});

app.on('will-quit', () => {
  globalShortcut.unregisterAll();
  
  // Kill all PTY processes
  for (const [id, winData] of windows) {
    if (winData.ptyProcess) {
      winData.ptyProcess.kill();
    }
  }
  
  // Stop the devcontainer when the app quits
  try {
    require('child_process').execSync('docker stop aesthetic', { timeout: 5000 });
    console.log('[main] Stopped devcontainer');
  } catch (e) {
    console.log('[main] Could not stop devcontainer:', e.message);
  }
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
  
  // Stop the devcontainer on cleanup
  try {
    require('child_process').execSync('docker stop aesthetic', { timeout: 5000 });
    console.log('[main] Stopped devcontainer');
  } catch (e) {
    console.log('[main] Could not stop devcontainer:', e.message);
  }
  
  process.exit(0);
}

process.on('SIGTERM', cleanup);
process.on('SIGINT', cleanup);
