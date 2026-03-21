/**
 * Offscreen Rendering Manager
 * 
 * Creates two invisible offscreen BrowserWindows:
 * - Front: AC webview (localhost:8888 or aesthetic.computer)
 * - Back: Terminal with xterm.js + emacs
 * 
 * Captures frames via paint events and sends to main renderer as textures.
 */

const { BrowserWindow, ipcMain } = require('electron');
const path = require('path');
const { spawn } = require('child_process');
const fs = require('fs');

// Find docker binary path
function getDockerPath() {
  if (process.platform === 'darwin') {
    const dockerLocations = [
      '/opt/homebrew/bin/docker',
      '/usr/local/bin/docker',
      '/Applications/Docker.app/Contents/Resources/bin/docker'
    ];
    for (const loc of dockerLocations) {
      if (fs.existsSync(loc)) return loc;
    }
  }
  return 'docker';
}

let pty;
try {
  pty = require('node-pty');
} catch (e) {
  console.warn('[offscreen] node-pty not available');
}

// Try to load native OSR GPU addon for shared texture support
let osrGpu;
try {
  osrGpu = require('./native/osr-gpu');
  osrGpu.initContext();
  console.log('[offscreen] Native OSR GPU addon loaded - using shared textures');
} catch (e) {
  console.warn('[offscreen] Native OSR GPU addon not available:', e.message);
  osrGpu = null;
}

class OffscreenManager {
  constructor() {
    this.frontWindow = null;
    this.backWindow = null;
    this.mainWindow = null;
    this.frameRate = 30;
    this.ptyProcess = null;
  }

  /**
   * Initialize offscreen rendering with the main 3D window
   */
  init(mainWindow, options = {}) {
    this.mainWindow = mainWindow;
    this.frameRate = options.frameRate || 30;
    
    // Listen for start signal from renderer
    ipcMain.on('start-offscreen-rendering', () => {
      console.log('[offscreen] Starting offscreen rendering...');
      this.createOffscreenWindows(options);
    });
    
    // Handle PTY connection for offscreen terminal
    ipcMain.on('connect-offscreen-pty', (event) => {
      console.log('[offscreen] Connecting PTY for terminal...');
      this.connectPTY(event.sender);
    });
    
    // Handle PTY input from offscreen terminal
    ipcMain.on('offscreen-pty-input', (event, data) => {
      if (this.ptyProcess) {
        this.ptyProcess.write(data);
      }
    });
    
    // ========== Input Event Forwarding ==========
    // Forward mouse events to front/back windows
    ipcMain.on('forward-mouse', (event, { target, type, x, y, button, clickCount }) => {
      const targetWindow = target === 'front' ? this.frontWindow : this.backWindow;
      if (!targetWindow || targetWindow.isDestroyed()) return;
      
      const wc = targetWindow.webContents;
      const modifiers = [];
      
      if (type === 'mouseDown') {
        wc.sendInputEvent({ type: 'mouseDown', x, y, button, clickCount, modifiers });
      } else if (type === 'mouseUp') {
        wc.sendInputEvent({ type: 'mouseUp', x, y, button, clickCount, modifiers });
      } else if (type === 'mouseMove') {
        wc.sendInputEvent({ type: 'mouseMove', x, y, modifiers });
      } else if (type === 'mouseWheel') {
        wc.sendInputEvent({ type: 'mouseWheel', x, y, deltaX: 0, deltaY: button, modifiers });
      }
    });
    
    // Forward keyboard events to front/back windows
    ipcMain.on('forward-key', (event, { target, type, keyCode, modifiers }) => {
      const targetWindow = target === 'front' ? this.frontWindow : this.backWindow;
      if (!targetWindow || targetWindow.isDestroyed()) return;
      
      const wc = targetWindow.webContents;
      
      if (type === 'keyDown') {
        wc.sendInputEvent({ type: 'keyDown', keyCode, modifiers: modifiers || [] });
      } else if (type === 'keyUp') {
        wc.sendInputEvent({ type: 'keyUp', keyCode, modifiers: modifiers || [] });
      } else if (type === 'char') {
        wc.sendInputEvent({ type: 'char', keyCode, modifiers: modifiers || [] });
      }
    });
    
    // Forward text input directly to PTY for terminal
    ipcMain.on('forward-pty-input', (event, data) => {
      if (this.ptyProcess) {
        this.ptyProcess.write(data);
      }
    });
  }

  connectPTY(webContents) {
    if (!pty) {
      webContents.send('pty-data', '\r\n\x1b[31mError: node-pty not available\x1b[0m\r\n');
      return;
    }
    
    const dockerPath = getDockerPath();
    
    // First check if container is running, start it if needed
    this.ensureContainerRunning(dockerPath).then(() => {
      // Spawn docker exec into the aesthetic container with emacsclient
      this.ptyProcess = pty.spawn(dockerPath, [
        'exec', '-it', 'aesthetic',
        'fish', '-c', 'emacsclient -nw -a "" || fish'
      ], {
        name: 'xterm-256color',
        cols: 120,
        rows: 40,
        cwd: process.env.HOME,
        env: { ...process.env, TERM: 'xterm-256color' }
      });
      
      this.ptyProcess.onData((data) => {
        if (!webContents.isDestroyed()) {
          webContents.send('pty-data', data);
        }
      });
      
      this.ptyProcess.onExit(() => {
        if (!webContents.isDestroyed()) {
          webContents.send('pty-data', '\r\n\x1b[33m[Session ended]\x1b[0m\r\n');
        }
      });
      
      console.log('[offscreen] PTY connected');
    }).catch((err) => {
      webContents.send('pty-data', `\r\n\x1b[31mError starting container: ${err.message}\x1b[0m\r\n`);
    });
  }
  
  async ensureContainerRunning(dockerPath) {
    const { spawn } = require('child_process');
    
    // Check if container is running
    const isRunning = await new Promise((resolve) => {
      const check = spawn(dockerPath, ['ps', '--filter', 'name=aesthetic', '--format', '{{.Names}}'], { stdio: 'pipe' });
      let output = '';
      check.stdout.on('data', (data) => output += data.toString());
      check.on('close', () => resolve(output.includes('aesthetic')));
      check.on('error', () => resolve(false));
    });
    
    if (isRunning) {
      console.log('[offscreen] Container already running');
      return;
    }
    
    // Check if container exists but is stopped
    const exists = await new Promise((resolve) => {
      const check = spawn(dockerPath, ['ps', '-a', '--filter', 'name=aesthetic', '--format', '{{.Names}}'], { stdio: 'pipe' });
      let output = '';
      check.stdout.on('data', (data) => output += data.toString());
      check.on('close', () => resolve(output.includes('aesthetic')));
      check.on('error', () => resolve(false));
    });
    
    if (exists) {
      console.log('[offscreen] Starting stopped container...');
      await new Promise((resolve, reject) => {
        const start = spawn(dockerPath, ['start', 'aesthetic'], { stdio: 'pipe' });
        start.on('close', (code) => code === 0 ? resolve() : reject(new Error(`Failed to start container: code ${code}`)));
        start.on('error', reject);
      });
      
      // Wait a moment for container to be ready
      await new Promise(r => setTimeout(r, 1000));
      
      // Start emacs daemon
      console.log('[offscreen] Starting emacs daemon...');
      await new Promise((resolve) => {
        const emacs = spawn(dockerPath, ['exec', 'aesthetic', 'emacs', '--daemon'], { stdio: 'pipe' });
        emacs.on('close', () => resolve());
        emacs.on('error', () => resolve()); // Don't fail if emacs daemon already running
      });
      
      await new Promise(r => setTimeout(r, 500));
      return;
    }
    
    throw new Error('Container "aesthetic" does not exist. Run devcontainer first.');
  }

  createOffscreenWindows(options) {
    // Always use production for the front view (starfield)
    const url = 'https://aesthetic.computer/starfield';
    
    // Use shared texture mode if native addon is available
    const useSharedTexture = osrGpu !== null;
    
    // ========== Front Window (AC App) ==========
    this.frontWindow = new BrowserWindow({
      width: 1280,
      height: 800,
      show: false, // Offscreen - never shown
      webPreferences: {
        offscreen: {
          useSharedTexture: useSharedTexture
        },
        nodeIntegration: false,
        contextIsolation: true
      }
    });
    
    this.frontWindow.webContents.setFrameRate(this.frameRate);
    
    this.frontWindow.webContents.on('paint', (event, dirty, image) => {
      if (this.mainWindow && !this.mainWindow.isDestroyed()) {
        // Check if we have shared texture (GPU zero-copy path)
        if (event.texture && osrGpu) {
          try {
            const { data, width, height } = osrGpu.copyTextureToBuffer(event.texture.textureInfo);
            this.mainWindow.webContents.send('front-frame', {
              width,
              height,
              data: Buffer.from(data) // Convert to Buffer for IPC
            });
            // CRITICAL: Release the texture back to the pool
            event.texture.release();
          } catch (e) {
            console.error('[offscreen] Failed to copy shared texture:', e);
          }
        } else {
          // Fallback: Use bitmap (slower but works without native addon)
          const size = image.getSize();
          const bitmap = image.toBitmap();
          this.mainWindow.webContents.send('front-frame', {
            width: size.width,
            height: size.height,
            data: bitmap // Raw RGBA Uint8Array
          });
        }
      }
    });
    
    // Force periodic repainting
    this.frontWindow.webContents.on('did-finish-load', () => {
      console.log('[offscreen] Front window loaded');
      // Invalidate to trigger paint
      this.frontWindow.webContents.invalidate();
    });
    
    this.frontWindow.loadURL(url);
    console.log(`[offscreen] Front window loading: ${url}`);
    
    // ========== Back Window (Terminal) ==========
    this.backWindow = new BrowserWindow({
      width: 1280,
      height: 800,
      show: false, // Offscreen - never shown
      webPreferences: {
        offscreen: {
          useSharedTexture: useSharedTexture
        },
        nodeIntegration: true,
        contextIsolation: false
      }
    });
    
    this.backWindow.webContents.setFrameRate(this.frameRate);
    
    this.backWindow.webContents.on('paint', (event, dirty, image) => {
      if (this.mainWindow && !this.mainWindow.isDestroyed()) {
        // Check if we have shared texture (GPU zero-copy path)
        if (event.texture && osrGpu) {
          try {
            const { data, width, height } = osrGpu.copyTextureToBuffer(event.texture.textureInfo);
            this.mainWindow.webContents.send('back-frame', {
              width,
              height,
              data: Buffer.from(data)
            });
            event.texture.release();
          } catch (e) {
            console.error('[offscreen] Failed to copy shared texture:', e);
          }
        } else {
          // Fallback: Use bitmap
          const size = image.getSize();
          const bitmap = image.toBitmap();
          this.mainWindow.webContents.send('back-frame', {
            width: size.width,
            height: size.height,
            data: bitmap
          });
        }
      }
    });
    
    this.backWindow.webContents.on('did-finish-load', () => {
      console.log('[offscreen] Back window loaded');
      this.backWindow.webContents.invalidate();
    });
    
    // Load the terminal renderer
    this.backWindow.loadFile(path.join(__dirname, 'renderer', 'terminal-offscreen.html'));
    console.log('[offscreen] Back window loading terminal...');
  }

  /**
   * Navigate front window to a different piece
   */
  navigateFront(url) {
    if (this.frontWindow && !this.frontWindow.isDestroyed()) {
      this.frontWindow.loadURL(url);
    }
  }

  /**
   * Send command to terminal
   */
  sendToTerminal(data) {
    if (this.backWindow && !this.backWindow.isDestroyed()) {
      this.backWindow.webContents.send('terminal-input', data);
    }
  }

  /**
   * Clean up
   */
  destroy() {
    if (this.frontWindow && !this.frontWindow.isDestroyed()) {
      this.frontWindow.destroy();
    }
    if (this.backWindow && !this.backWindow.isDestroyed()) {
      this.backWindow.destroy();
    }
    if (this.ptyProcess) {
      this.ptyProcess.kill();
      this.ptyProcess = null;
    }
    // Clean up native OSR GPU context
    if (osrGpu) {
      try {
        osrGpu.destroyContext();
      } catch (e) {
        console.warn('[offscreen] Failed to destroy OSR GPU context:', e);
      }
    }
  }
}

module.exports = OffscreenManager;
