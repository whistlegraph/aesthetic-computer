// preview-preload.js — makes a frameless slab preview window movable by
// ⌥Option-dragging anywhere on its body. Plain drags pass through to the
// page untouched — pieces like `video scrub` are drag instruments, so the
// window must never steal an unmodified drag. Hold Option (Alt) to grab
// the window itself; a plain tap still passes through as before.
//
// Wired only into `--preview` windows (see openPreviewWindow in main.js).
const { ipcRenderer, contextBridge } = require('electron');

// Give preview panes the same close API the main window has, so the
// prompt's `-` command closes THIS pane instead of falling back to a
// window.open("ac://close") that spawns a stray window.
try {
  contextBridge.exposeInMainWorld('acElectron', {
    closeWindow: () => ipcRenderer.send('preview-close'),
  });
} catch {}

// 🔐 acDESKTOP.login — boot.mjs hands a page's `hi` / `login` here. The app
// signs in through ac-login (system browser, ~/.ac-token) and returns the
// session; the page reboots with it as `session-aesthetic`.
try {
  contextBridge.exposeInMainWorld('acDESKTOP', {
    login: async (mode) => {
      const session = await ipcRenderer.invoke('ac:desktop-login', mode);
      if (!session) return null; // Cancelled or failed — stay put, signed out.
      const url = new URL(location.href);
      url.searchParams.set('session-aesthetic', session);
      location.replace(url.toString());
      return 'desktop';
    },
  });
} catch {}

const THRESHOLD = 4;            // px of travel before it counts as a drag
let down = false, dragging = false, startX = 0, startY = 0;

window.addEventListener('mousedown', (e) => {
  if (e.button !== 0 || !e.altKey) return;   // left button + Option only
  down = true; dragging = false;
  startX = e.screenX; startY = e.screenY;
  ipcRenderer.send('preview-drag-start', { sx: e.screenX, sy: e.screenY });
  e.preventDefault(); e.stopPropagation();   // the grab is ours, not the page's
}, true);

window.addEventListener('mousemove', (e) => {
  if (!down) return;
  if (!dragging &&
      Math.abs(e.screenX - startX) + Math.abs(e.screenY - startY) > THRESHOLD) {
    dragging = true;
  }
  if (dragging) {
    ipcRenderer.send('preview-drag-move', { sx: e.screenX, sy: e.screenY });
    e.preventDefault(); e.stopPropagation();
  }
}, true);

const end = () => {
  if (down && dragging) ipcRenderer.send('preview-drag-end');
  down = false; dragging = false;
};
window.addEventListener('mouseup', end, true);
window.addEventListener('blur', end, true);
