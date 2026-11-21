import assert from 'node:assert/strict';
import { once } from 'node:events';
import { chromium } from 'playwright';
import { PNG } from 'pngjs';
import { createRaxServer } from '../server.mjs';

const app = createRaxServer({ guests: true });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const browser = await chromium.launch({ channel: 'chrome', headless: true, args: ['--enable-unsafe-webgpu'] });
try {
  const page = await browser.newPage();
  await page.route('**/shadow-fixture', route => route.fulfill({ contentType: 'text/html', body: '<canvas></canvas>' }));
  await page.goto(`http://127.0.0.1:${app.server.address().port}/shadow-fixture`);
  await page.evaluate(async () => {
    // An older installation may still have the retired blur setting.
    localStorage.setItem('rax:soft', '3');
    const { LensScene3D } = await import('./lens-host.mjs');
    const host = await LensScene3D.create(document.querySelector('canvas'), { shadows: true });
    host.resize(128, 96);
    host.beginFrame();
    const triangle = (a, b, c, z) => host.triangle(...a, z, ...b, z, ...c, z, 255, 255, 255, 128, 96);
    triangle([0, 0], [128, 0], [0, 96], 0);
    triangle([0, 96], [128, 0], [128, 96], 0);
    // This face lies behind the white ground so its footprint sets the stencil.
    triangle([5, 4], [121, 17], [79, 91], 1);
    window.shadowFrame = (aa, over) => {
      host.present({ clear: [1, 1, 1, 1], shadow: { first: 2, count: 1, shade: [0.25, 0.25, 0.25] },
        lens: { sx: 1, sy: 1, wide: 1, lens: 0.65, fovea: 0.5, aa, over, glaze: 0 } });
      return host.canvas.toDataURL();
    };
  });
  for (const [aa, over] of [[1, 1], [1, 2], [2, 1]]) {
    const url = await page.evaluate(([a, o]) => shadowFrame(a, o), [aa, over]);
    const { data } = PNG.sync.read(Buffer.from(url.split(',')[1], 'base64'));
    const levels = new Set();
    for (let i = 0; i < data.length; i += 4) levels.add(data[i]);
    assert.deepEqual([...levels].sort((a, b) => a - b), [64, 255], `shadow edge at aa=${aa}, over=${over}`);
  }
  console.log('WebGPU shadow edges contain only lit and shadowed pixels at normal resolution, full zoom sampling and 2× AA.');
} finally {
  await browser.close();
  await app.close();
}
