import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdir, writeFile } from 'node:fs/promises';
import { chromium } from 'playwright';
import { PNG } from 'pngjs';
import { createRaxServer } from '../server.mjs';

const app = createRaxServer({ guests: true, prefix: '/rax' });
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}/rax/`;
const browser = await chromium.launch({ channel: 'chrome', headless: true });
const shots = new URL('../../tmp/rax/', import.meta.url);
await mkdir(shots, { recursive: true });
const errors = [],
  report = [];
const fixture = `<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1">
<style>html,body{margin:0;width:100%;height:100%;overflow:hidden}canvas{display:block;width:100%;height:100%;image-rendering:pixelated}</style>
<canvas id="game"></canvas><script type="importmap">{"imports":{"/system/public/aesthetic.computer/lib/pmove.mjs":"./pmove.mjs"}}</script>
<script type="module">
import {createView} from './view.mjs';import {spawn} from './world.mjs';
window.view=await createView(document.getElementById('game'));
window.state={shadows:false,me:spawn(0),players:[spawn(0),spawn(1)],scores:[3,7],status:'FIRST TO 10'};
state.joinURL='https://rax.example.com/?room=studio';
state.members=[{seat:0,handle:'Jeffrey',colors:null}];
state.connection={label:'12 MS',bars:4};
state.prompt={open:false,text:'',cursor:0};
state.players[1].z=3;
// Capture the same breathing and idle animation phase on both backends.
performance.now = () => 1000;
view.render(state);
delete performance.now;
window.ready=true;
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
    await page.route('**/rax/render-test?*', (r) =>
      r.fulfill({ contentType: 'text/html', body: fixture }),
    );
    await page.goto(base + 'render-test?lens=0&fovea=0');
    await page.waitForFunction(() => window.ready);
    // A continuous walk seen from adjoining 16 m blocks. The camera's
    // world-space position and sightline must agree on both sides.
    for (const third of [false, true]) {
      const seam = await page.evaluate(third => {
        const sample = x => {
          view.render({ ...state, third, me: { ...state.me, x, z: 0, yaw: 90, pitch: 0, vx: 10 } });
          return { eye: view.aimOrigin(), direction: view.aimForward() };
        };
        return [sample(8.24), sample(8.24 - 16)];
      }, third);
      const [before, after] = seam;
      assert.ok(Math.abs(before.eye.x - after.eye.x - 16) < 0.001);
      assert.ok(Math.abs(before.eye.y - after.eye.y) < 0.001);
      assert.ok(Math.abs(before.direction.y - after.direction.y) < 0.001, 'open edge preserves camera pitch');
    }
    await page.evaluate(() => view.render(state));
    const backend = await page.evaluate(() => document.getElementById('game').dataset.engine);
    assert.equal(backend, fallback ? 'webgl2' : 'webgpu');
    const orbitSizes = await page.evaluate(() => {
      const canvas = document.getElementById('game');
      const bounds = () => {
        const {left, top, width, height} = canvas.getBoundingClientRect();
        return [left, top, width, height, canvas.width, canvas.height];
      };
      view.grid({pixel: 2, left: 10, top: 4, width: 630, height: 390});
      view.render({bare: true, orbit: 1});
      const parked = bounds();
      view.render({bare: true, orbit: 0.9, reticle: true});
      const flying = bounds();
      view.grid(null);
      view.render(state);
      return {parked, flying};
    });
    assert.deepEqual(orbitSizes.parked, [0, 0, 1280, 800, 640, 400], 'the globe is drawn on the full Prompt pixel grid');
    assert.deepEqual(orbitSizes.flying, orbitSizes.parked, 'the globe and the flight share one pixel grid');
    // Observe the same fence from the same world position before and after
    // the local origin moves to the neighbouring block.
    const fencePixels = async (east) => {
      await page.evaluate(east => {
        performance.now = () => 1000;
        try {
          view.render({ ...state, bare: true, seam: { shown: true },
            park: { neighbours: east ? { west: true } : { east: true } },
            me: { ...state.me, x: east ? -10 : 6, z: 0, yaw: 90, pitch: 0 } });
        } finally { delete performance.now; }
      }, east);
      const { width, height, data } = PNG.sync.read(await page.screenshot());
      const pixels = [];
      // Sample the posts at eye height, clear of the ground and HUD.
      for (let x = 0; x < width; x++) {
        const i = 4 * (Math.floor(height / 2) * width + x);
        if (data[i] === 79 && data[i + 1] === 240 && data[i + 2] === 255) pixels.push(x);
      }
      return pixels;
    };
    const before = await fencePixels(false), after = await fencePixels(true);
    assert.ok(before.length > 10, 'fence visible from the approach');
    assert.deepEqual(after, before, 'fence stays on the shared edge');
    const paintTargetFrame = async (floorAim) => {
      await page.evaluate(floorAim => view.render({ ...state, input: 'touch', floorAim,
        me: { ...state.me, x: 1, z: 0, yaw: 0, pitch: -55 } }), floorAim);
      return page.screenshot({ clip: { x: 480, y: 400, width: 320, height: 200 } });
    };
    const floor = await paintTargetFrame(null);
    assert.ok(floor.equals(await paintTargetFrame({ i: 9, j: 9 })), 'aiming at a paint tile leaves the floor clear');
    for (const color of [0xffe066, 0xff3296]) {
      await page.evaluate(color => view.render({ ...state, flash: { color, at: performance.now() } }), color);
      const { width, height, data } = PNG.sync.read(await page.screenshot());
      for (const [x, y] of [[0, height >> 1], [width - 1, height >> 1], [width >> 1, 0], [width >> 1, height - 1]]) {
        const i = 4 * (y * width + x);
        assert.deepEqual([...data.subarray(i, i + 3)], [color >> 16, color >> 8 & 255, color & 255]);
      }
    }
    await page.evaluate(() => view.render(state));
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
            s = Math.max(1, Math.floor(Math.min(width / 640, height / 360)));
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
          cpu.push(document.getElementById('game').raxStats.cpuMs);
          intervals.push(now - last);
        }
        last = now;
      }
      const quantile = (a, q) => a.sort((x, y) => x - y)[Math.floor((a.length - 1) * q)];
      return {
        ...document.getElementById('game').raxStats,
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
  await page.route('**/rax/depth-test', (r) =>
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
