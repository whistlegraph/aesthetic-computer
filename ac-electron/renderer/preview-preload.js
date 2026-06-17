// preview-preload.js — makes a frameless slab preview window draggable by
// click-dragging anywhere on its body. A small movement threshold means a
// quick tap still passes through to the page (so click-to-regen etc. work);
// once you drag past the threshold the window follows the cursor instead.
//
// Wired only into `--preview` windows (see openPreviewWindow in main.js).
const { ipcRenderer } = require('electron');

const THRESHOLD = 4;            // px of travel before it counts as a drag
let down = false, dragging = false, startX = 0, startY = 0;

window.addEventListener('mousedown', (e) => {
  if (e.button !== 0) return;   // left button only
  down = true; dragging = false;
  startX = e.screenX; startY = e.screenY;
  ipcRenderer.send('preview-drag-start', { sx: e.screenX, sy: e.screenY });
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
