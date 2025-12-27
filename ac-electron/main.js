/**
 * Aesthetic Computer - Electron Main Process
 * 
 * Three window types:
 * - Production windows: Full webview to aesthetic.computer (green accent)
 * - Development windows: Webview to localhost:8888 (orange accent) 
 * - Shell windows: Terminal with devcontainer + emacs (purple accent)
 */

const { app, BrowserWindow, ipcMain, globalShortcut, Menu, dialog, shell, nativeImage, screen } = require('electron');
const path = require('path');
const fs = require('fs');
const { spawn, execSync } = require('child_process');

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

// Check if Docker is available
async function checkDocker() {
  return new Promise((resolve) => {
    const docker = spawn('docker', ['info'], { stdio: 'pipe' });
    docker.on('close', (code) => resolve(code === 0));
    docker.on('error', () => resolve(false));
  });
}

// Check if devcontainer is running
async function checkDevcontainer() {
  return new Promise((resolve) => {
    const docker = spawn('docker', ['ps', '--filter', 'name=aesthetic', '--format', '{{.Names}}'], { stdio: 'pipe' });
    let output = '';
    docker.stdout.on('data', (data) => output += data.toString());
    docker.on('close', () => resolve(output.includes('aesthetic')));
    docker.on('error', () => resolve(false));
  });
}

// Start the devcontainer
async function startDevcontainer() {
  return new Promise((resolve, reject) => {
    const workspaceFolder = path.resolve(__dirname, '..');
    console.log('Starting devcontainer in:', workspaceFolder);
    
    const devcontainer = spawn('devcontainer', ['up', '--workspace-folder', workspaceFolder], {
      stdio: 'pipe'
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
              detail: 'Version 0.1.0\n\nA creative coding platform.',
              buttons: ['OK']
            });
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
          label: 'New Shell',
          accelerator: 'CmdOrCtrl+Shift+T',
          click: () => openShellWindow()
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
    // Dev window: nearly full screen (has integrated terminal overlay)
    winWidth = screenWidth - 100;
    winHeight = screenHeight - 50;
    winX = 50;
    winY = 0;
  } else if (isShell) {
    // Standalone shell: right side of screen
    winWidth = Math.floor(screenWidth * 0.45);
    winHeight = screenHeight - 50;
    winX = Math.floor(screenWidth * 0.55);
    winY = 0;
  } else {
    // Production: centered
    winWidth = 1280;
    winHeight = 800;
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
    // Use system chrome (default title bar)
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

// Open a new dev window (with container check)
async function openDevWindow() {
  return createWindow('development');
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
    // Use full path to docker in case PATH isn't set correctly in Electron
    const dockerPath = process.platform === 'darwin' 
      ? '/usr/local/bin/docker'  // Common Docker Desktop location
      : 'docker';
    
    console.log('[main] Spawning PTY with docker at:', dockerPath);
    
    const shellProcess = pty.spawn(dockerPath, ['exec', '-it', 'aesthetic', 'fish'], {
      name: 'xterm-256color',
      cols: 120,
      rows: 30,
      cwd: process.env.HOME,
      env: { 
        ...process.env, 
        TERM: 'xterm-256color',
        PATH: process.env.PATH + ':/usr/local/bin:/opt/homebrew/bin'
      },
    });

    winData.ptyProcess = shellProcess;

    // Forward PTY output to renderer
    shellProcess.onData((data) => {
      if (!win.isDestroyed()) {
        win.webContents.send('pty-data', data);
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
      winData.ptyProcess.resize(cols, rows);
      break;
    }
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
  
  // Watch for reboot request file from devcontainer
  const REBOOT_MARKER = path.join(__dirname, '.reboot-requested');
  const checkRebootMarker = () => {
    if (fs.existsSync(REBOOT_MARKER)) {
      console.log('[main] Reboot marker detected, exiting with code 42...');
      fs.unlinkSync(REBOOT_MARKER); // Clean up marker
      // Exit with code 42 - wrapper script should restart us
      app.exit(42);
    }
  };
  
  // Check every 2 seconds for reboot marker
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
  // On macOS, apps typically stay open when all windows are closed
  // On other platforms, quit unless we're intentionally relaunching
  if (process.platform !== 'darwin' && !app.isReady()) {
    return; // Don't quit during startup
  }
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('will-quit', () => {
  globalShortcut.unregisterAll();
  
  // Kill all PTY processes
  for (const [id, winData] of windows) {
    if (winData.ptyProcess) {
      winData.ptyProcess.kill();
    }
  }
  
  // Stop the devcontainer when quitting
  try {
    execSync('docker stop aesthetic 2>/dev/null || true', { 
      timeout: 5000,
      stdio: 'ignore' 
    });
    console.log('[main] Stopped devcontainer on quit');
  } catch (e) {
    // Ignore errors - container might not be running
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
  
  // Stop the devcontainer
  try {
    execSync('docker stop aesthetic 2>/dev/null || true', { 
      timeout: 5000,
      stdio: 'ignore' 
    });
    console.log('[main] Stopped devcontainer on cleanup');
  } catch (e) {
    // Ignore
  }
  
  process.exit(0);
}

process.on('SIGTERM', cleanup);
process.on('SIGINT', cleanup);
