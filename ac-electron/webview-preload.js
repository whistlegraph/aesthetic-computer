/**
 * Aesthetic Computer - Webview Preload Script
 * 
 * This preload runs inside the webview (the AC content) and provides
 * a bridge to communicate with the parent Electron renderer (flip-view.html).
 * 
 * Uses ipcRenderer.sendToHost() which is specifically designed for webview → parent communication.
 */

const { ipcRenderer } = require('electron');

// Expose a minimal API for AC content to communicate with Electron
window.acElectron = {
  // Open a new AC Pane window
  openWindow: (options = {}) => {
    const { url, index = 0, total = 1 } = typeof options === 'string' ? { url: options } : options;
    console.log('[webview-preload] openWindow:', url, index, total);
    ipcRenderer.sendToHost('ac-open-window', { url, index, total });
  },
  
  // Close this window
  closeWindow: () => {
    console.log('[webview-preload] closeWindow');
    ipcRenderer.sendToHost('ac-close-window');
  },
  
  // Check if we're in Electron
  isElectron: true,

  // Platform info
  platform: process.platform,

  // USB flash support (Linux only)
  listBlockDevices: () => ipcRenderer.invoke('usb:list-devices'),
  flashImage: (opts) => ipcRenderer.invoke('usb:flash-image', opts),
};

// Forward USB flash progress events from main process to window
ipcRenderer.on('usb:flash-progress', (event, data) => {
  window.dispatchEvent(new CustomEvent('usb:flash-progress', { detail: data }));
});

// Also listen for messages from the content and forward them
// This catches postMessage calls from bios.mjs
window.addEventListener('message', (e) => {
  if (e.source !== window) return; // Only handle messages from this window
  
  if (e.data?.type === 'ac-open-window') {
    console.log('[webview-preload] Caught ac-open-window postMessage:', e.data);
    ipcRenderer.sendToHost('ac-open-window', {
      url: e.data.url,
      index: e.data.index || 0,
      total: e.data.total || 1,
    });
  } else if (e.data?.type === 'ac-close-window') {
    console.log('[webview-preload] Caught ac-close-window postMessage');
    ipcRenderer.sendToHost('ac-close-window');
  }
});

console.log('[webview-preload] AC Electron bridge loaded');
