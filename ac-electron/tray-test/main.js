// Standalone live preview of the AC tray renderer — does NOT take the
// single-instance lock, so it runs alongside the packaged app. Shows a 2nd
// menu-bar icon driven by the real ../tray-renderer.js + ~/.ac-os/tray.json.
const { app, Tray, nativeImage } = require('electron');
const { TrayRenderer } = require('../tray-renderer');
app.dock?.hide?.();
let tray, renderer;
app.whenReady().then(() => {
  tray = new Tray(nativeImage.createEmpty());
  tray.setToolTip('AC tray preview');
  renderer = new TrayRenderer(tray);
  renderer.startWatching();
  console.log('[tray-preview] up — edit ~/.ac-os/tray.json to iterate live');
});
app.on('window-all-closed', () => { /* stay alive as a menubar daemon */ });
