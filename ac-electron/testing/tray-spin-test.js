// tray-spin-test.js — headless-ish check of the spin tray renderer WITHOUT
// launching the full AC app. Creates a Tray + TrayRenderer, forces spin mode,
// renders several phases, and dumps them to /tmp for visual inspection.
//   ac-electron $ ./node_modules/.bin/electron testing/tray-spin-test.js
const { app, Tray, nativeImage } = require('electron');
const fs = require('fs');
const path = require('path');
const os = require('os');

app.whenReady().then(async () => {
  const { TrayRenderer } = require('../tray-renderer');
  const tray = new Tray(nativeImage.createEmpty());
  const r = new TrayRenderer(tray);
  r.setState({ logoMode: 'spin' });
  await r.loadSpin();
  console.log('spinCount:', r.spinCount);
  const N = r.spinCount || 0;
  if (!N) { console.error('NO SPIN FRAMES LOADED'); app.exit(1); return; }
  const picks = [0, 0.15, 0.3, 0.5, 0.75];
  for (let i = 0; i < picks.length; i++) {
    r.phase = picks[i];
    const img = await r.render();
    if (!img || img.isEmpty()) { console.error('empty render at', picks[i]); continue; }
    const out = path.join(os.tmpdir(), `tray-spin-${i}.png`);
    fs.writeFileSync(out, img.toPNG());
    const s = img.getSize();
    console.log(`  phase ${picks[i]} → ${out} (${s.width}x${s.height})`);
  }
  // also render with a count badge to confirm compositing still works
  r.setState({ count: '3', showCount: true });
  r.phase = 0;
  const withCount = await r.render();
  if (withCount && !withCount.isEmpty()) {
    fs.writeFileSync(path.join(os.tmpdir(), 'tray-spin-count.png'), withCount.toPNG());
    console.log('  +count badge → /tmp/tray-spin-count.png');
  }
  tray.destroy();
  app.exit(0);
});
