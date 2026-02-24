/**
 * Aesthetic Computer - Preload Script
 * Exposes safe APIs to renderer process
 * 
 * Note: This script is used for production/development windows (contextIsolation=true)
 * Shell windows use nodeIntegration=true and don't need this bridge.
 */

const { contextBridge, ipcRenderer } = require('electron');

// Only use contextBridge when contextIsolation is enabled
// (Shell windows have nodeIntegration=true and contextIsolation=false)
try {
  contextBridge.exposeInMainWorld('ac', {
    // Mode management
    getMode: () => ipcRenderer.invoke('get-mode'),
    getUrls: () => ipcRenderer.invoke('get-urls'),
    switchMode: (mode) => ipcRenderer.invoke('switch-mode', mode),
    
    // Docker/Container management
    checkDocker: () => ipcRenderer.invoke('check-docker'),
    checkContainer: () => ipcRenderer.invoke('check-container'),
    startContainer: () => ipcRenderer.invoke('start-container'),
    openShell: () => ipcRenderer.invoke('open-shell'),
    
    // Navigation from main process
    onNavigate: (callback) => {
      ipcRenderer.on('navigate', (event, url) => callback(url));
    },
    
    // Menu commands
    onGoBack: (callback) => {
      ipcRenderer.on('go-back', () => callback());
    },
    onGoForward: (callback) => {
      ipcRenderer.on('go-forward', () => callback());
    },
    onFocusLocation: (callback) => {
      ipcRenderer.on('focus-location', () => callback());
    },
    onToggleDevtools: (callback) => {
      ipcRenderer.on('toggle-devtools', () => callback());
    },
    
    // Status updates
    onStatus: (callback) => {
      ipcRenderer.on('status', (event, message) => callback(message));
    },
    
    // Window controls
    moveWindow: (x, y) => ipcRenderer.send('move-window', { x, y }),
    openWindow: (url) => ipcRenderer.invoke('ac-open-window', { url }),
    closeWindow: () => ipcRenderer.invoke('ac-close-window'),
    openExternalUrl: (url) => ipcRenderer.invoke('open-external-url', url),
    
    // Open DevTools docked at bottom (toggled via ~ command in prompt)
    openDevTools: () => ipcRenderer.send('open-devtools'),

    // App info (for desktop.mjs)
    getAppInfo: () => ipcRenderer.invoke('get-app-info'),

    // Platform info
    platform: process.platform,
  });

  contextBridge.exposeInMainWorld('electronAPI', {
    // Docker checks
    checkDocker: () => ipcRenderer.invoke('check-docker'),
    checkContainer: () => ipcRenderer.invoke('check-container'),
    startContainer: () => ipcRenderer.invoke('start-container'),
    
    // PTY management
    connectPty: () => ipcRenderer.invoke('connect-pty'),
    sendToPty: (data) => ipcRenderer.send('pty-input', data),
    resizePty: (cols, rows) => ipcRenderer.send('pty-resize', cols, rows),
    
    // PTY events
    onPtyData: (callback) => {
      ipcRenderer.on('pty-data', (event, data) => callback(data));
    },
    onPtyExit: (callback) => {
      ipcRenderer.on('pty-exit', (event, code) => callback(code));
    },
  });
} catch (e) {
  // contextBridge not available (shell window with nodeIntegration)
  // That's fine - shell.html uses require() directly
  console.log('[preload] contextBridge not available, using nodeIntegration mode');
}
