import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdir, writeFile } from 'node:fs/promises';
import { chromium } from 'playwright';
import { PNG } from 'pngjs';
import { createDuelServer } from '../server.mjs';

const app = createDuelServer({ prefix: '/dool' });
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}/dool/`;
const browser = await chromium.launch({ channel: 'chrome', headless: true });
const shots = new URL('../../tmp/duel/', import.meta.url);
await mkdir(shots, { recursive: true });
const errors = [],
  report = [];
const fixture = `<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1">
<style>html,body{margin:0;width:100%;height:100%;overflow:hidden}canvas{display:block;width:100%;height:100%;image-rendering:pixelated}</style>
<canvas id="game"></canvas><script type="importmap">{"imports":{"/system/public/aesthetic.computer/lib/pmove.mjs":"./pmove.mjs"}}</script>
<script type="module">
import {createView} from './view.mjs';import {spawn} from './world.mjs';
window.view=await createView(document.getElementById('game'));
window.state={me:spawn(0),players:[spawn(0),spawn(1)],scores:[3,7],status:'FIRST TO 10'};
state.joinURL='https://dool.example.com/?room=studio';
state.members=[{seat:0,handle:'Jeffrey',colors:null}];
state.connection={label:'12 MS',bars:4};
state.prompt={open:false,text:'',cursor:0};
state.players[1].z=3;
view.render(state);window.ready=true;
</script>`;
try {
  const images = [];
  for (const fallback of [false, true]) {
    const context = await browser.newContext({
      viewport: { width: 1280, height: 800 },
      deviceScaleFactor: 1,
    });
    if (fallback)
      await context.addInitScript(() =>
        Object.defineProperty(navigator, 'gpu', { value: undefined }),
      );
    const page = await context.newPage();
    page.on('pageerror', (e) => errors.push(e.message));
    page.on('console', (m) => {
      if (m.type() === 'error') errors.push(m.text());
    });
    await page.route('**/favicon.ico', (r) => r.fulfill({ status: 204 }));
    await page.route('**/dool/render-test', (r) =>
      r.fulfill({ contentType: 'text/html', body: fixture }),
    );
    await page.goto(base + 'render-test');
    await page.waitForFunction(() => window.ready);
    const backend = await page.evaluate(() => document.getElementById('game').dataset.engine);
    assert.equal(backend, fallback ? 'webgl2' : 'webgpu');
    images.push(
      PNG.sync.read(
        await page.screenshot({ path: new URL(`flat-${backend}-first.png`, shots).pathname }),
      ),
    );
    await page.evaluate(() => {
      state.third = true;
      view.render(state);
    });
    await page.screenshot({ path: new URL(`flat-${backend}-third.png`, shots).pathname });
    for (const [width, height] of [
      [390, 844],
      [1920, 1080],
      [3840, 2160],
      [1600, 700],
      [1280, 800],
    ]) {
      await page.setViewportSize({ width, height });
      await page.waitForFunction(
        ({ width, height }) => {
          const c = document.getElementById('game'),
            s = Math.max(1, Math.floor(Math.min(width / 480, height / 270)));
          return c.width === Math.ceil(width / s) && c.height === Math.ceil(height / s);
        },
        { width, height },
      );
      await page.evaluate(() => view.render(state));
      await page.screenshot({
        path: new URL(`flat-${backend}-${width}x${height}.png`, shots).pathname,
      });
    }
    const perf = await page.evaluate(async () => {
      state.third = false;
      const cpu = [],
        intervals = [];
      let last;
      for (let i = 0; i < 180; i++) {
        const now = await new Promise(requestAnimationFrame);
        view.render(state);
        if (i >= 30) {
          cpu.push(document.getElementById('game').doolStats.cpuMs);
          intervals.push(now - last);
        }
        last = now;
      }
      const quantile = (a, q) => a.sort((x, y) => x - y)[Math.floor((a.length - 1) * q)];
      return {
        ...document.getElementById('game').doolStats,
        cpuP50: quantile(cpu, 0.5),
        cpuP95: quantile(cpu, 0.95),
        callbackP50: quantile(intervals, 0.5),
      };
    });
    report.push(perf);
    // Combat flash and a camera close to cover must remain valid clipped geometry.
    await page.evaluate(() => {
      state.me.x = -4.6;
      state.me.z = -2.9;
      state.me.yaw = 90;
      view.effect({ type: 'blast', at: { x: 0, y: 1, z: 0 } });
      view.render(state);
    });
    await page.screenshot({ path: new URL(`flat-${backend}-near.png`, shots).pathname });
    await context.close();
  }
  let changed = 0;
  for (let i = 0; i < images[0].data.length; i += 4) {
    if ([0, 1, 2].some((c) => Math.abs(images[0].data[i + c] - images[1].data[i + c]) > 2))
      changed++;
  }
  const ratio = changed / (images[0].width * images[0].height);
  assert.ok(ratio < 0.005, `WebGPU/WebGL image difference: ${ratio}`);

  // Exercise depth independently of the game's projection: the later blue
  // triangle must not paint over the nearer red one, including after resize.
  const page = await browser.newPage({ viewport: { width: 128, height: 128 } });
  page.on('pageerror', (e) => errors.push(e.message));
  await page.route('**/dool/depth-test', (r) =>
    r.fulfill({
      contentType: 'text/html',
      body: `<style>body{margin:0}canvas{display:block}</style><canvas></canvas><script type="module">
  import {WebGPUOskiewarScene3D} from './engine/scene3d-webgpu.mjs';
  window.host=await WebGPUOskiewarScene3D.create(document.querySelector('canvas'));
  window.draw=size=>{host.resize(size,size);host.beginFrame();
    host.triangle(0,0,-1,128,0,-1,64,128,-1,255,0,0,128,128);
    host.triangle(0,0,1,128,0,1,64,128,1,0,0,255,128,128);host.present();};
  draw(64);window.ready=true;</script>`,
    }),
  );
  await page.goto(base + 'depth-test');
  await page.waitForFunction(() => window.ready);
  for (const size of [64, 128]) {
    await page.evaluate((size) => draw(size), size);
    const png = PNG.sync.read(await page.locator('canvas').screenshot());
    const at = ((size / 2) * png.width + size / 2) * 4;
    assert.deepEqual([...png.data.subarray(at, at + 4)], [255, 0, 0, 255]);
  }
  await page.evaluate(() => host.destroy());
  await page.close();
  assert.deepEqual(errors, []);
  console.log(JSON.stringify({ report, imageDifference: ratio }, null, 2));
  await writeFile(
    new URL('flat-performance.json', shots),
    JSON.stringify({ report, imageDifference: ratio }, null, 2),
  );
  console.log(
    'Passed: real WebGPU, shared WebGL fallback, depth occlusion, near clipping, both cameras and responsive rendering.',
  );
} finally {
  await browser.close();
  await app.close();
}
