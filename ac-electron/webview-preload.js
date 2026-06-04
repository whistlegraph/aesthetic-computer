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

  // Open an external URL in the system's default browser. bios.mjs routes
  // jump("out:…") links here so they leave the app instead of navigating the
  // webview in-app (window.open is popup-blocked across the worker boundary).
  openExternal: (url) => {
    console.log('[webview-preload] openExternal:', url);
    return ipcRenderer.invoke('open-external-url', url);
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

// 🎵 Forward dropped-file payload from Electron host into the AC page.
// bios.mjs listens for `ac-dropped-file` window messages and routes them
// to disk.mjs which exposes them as system.droppedFile + a dropped:file
// event for the running piece.
ipcRenderer.on('ac:dropped-file', (_event, payload) => {
  if (!payload || !payload.url) return;
  try {
    window.postMessage({
      type: 'ac-dropped-file',
      url: payload.url,
      name: payload.name,
      mime: payload.mime,
    }, '*');
  } catch (e) {
    console.warn('[webview-preload] failed to forward dropped-file:', e?.message || e);
  }
});

// 🎮 Gamepad host → guest bridge.
// Inside Electron <webview>, navigator.getGamepads() in the guest is often
// empty because the guest renderer doesn't reliably receive the user
// activation Chromium gates the Gamepad API behind. The host
// (flip-view.html) polls navigator.getGamepads() — which works there once
// the BrowserWindow has activation — and forwards a snapshot to us via
// the 'ac:gamepad-state' IPC channel. We patch navigator.getGamepads here
// so that lib/gamepad.mjs (which polls 8ms in the page's main world) sees
// the controller via the host. We also synthesize gamepadconnected /
// gamepaddisconnected events so listeners that wait for those see the pad.
(function () {
  let hostPads = [];
  const seen = new Map(); // index → last connected timestamp

  ipcRenderer.on('ac:gamepad-state', (_e, snapshot) => {
    hostPads = (snapshot || []).map((p) => {
      if (!p) return null;
      // Reshape to look like a real Gamepad to consumers (incl. lib/gamepad.mjs).
      return {
        index: p.index,
        id: p.id,
        connected: p.connected,
        timestamp: p.timestamp,
        mapping: p.mapping || 'standard',
        axes: p.axes || [],
        buttons: (p.buttons || []).map((b) => ({
          pressed: !!b.pressed,
          touched: !!b.touched,
          value: typeof b.value === 'number' ? b.value : (b.pressed ? 1 : 0),
        })),
      };
    });

    // Synthesize gamepadconnected on first sighting per index.
    for (const p of hostPads) {
      if (!p) continue;
      if (!seen.has(p.index)) {
        seen.set(p.index, true);
        try {
          // GamepadEvent constructor exists in modern Chromium.
          const evt = new GamepadEvent('gamepadconnected', { gamepad: p });
          window.dispatchEvent(evt);
        } catch (_) {
          const evt = new Event('gamepadconnected');
          try { evt.gamepad = p; } catch (_) {}
          window.dispatchEvent(evt);
        }
        console.log('[webview-preload] gamepad bridged from host:', p.id);
      }
    }
  });

  // Patch navigator.getGamepads so the guest sees the host's pads when its
  // own getGamepads is empty. If the guest ever does see a real controller
  // (e.g. user clicks the webview and presses a button), we defer to that.
  const native = navigator.getGamepads
    ? navigator.getGamepads.bind(navigator)
    : () => [];
  navigator.getGamepads = function () {
    let nativePads;
    try { nativePads = native(); } catch (_) { nativePads = []; }
    const hasNative = nativePads && Array.from(nativePads).some((g) => g);
    if (hasNative) return nativePads;
    return hostPads;
  };
})();

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
