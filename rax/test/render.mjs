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
// 🐧 Linux Chrome offers WebGPU only when asked, and headless it gets SwiftShader (software)
// and loses its device: run it headful in a private display there, the real GPU's
// (on jastow: `xvfb-run -a -s "-screen 0 1920x1080x24" node rax/test/render.mjs`).
const linux = process.platform === 'linux';
const browser = await chromium.launch({ channel: 'chrome', headless: !linux,
  args: ['--enable-unsafe-webgpu', ...(linux ? ['--enable-gpu', '--ignore-gpu-blocklist', '--enable-features=Vulkan', '--use-vulkan=native', '--use-angle=vulkan'] : [])] });
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
  for (const fallback of [false, true]) { // (WebGPU, then the same page with none: WebGL 2, lens-host-gl.mjs)
    const context = await browser.newContext({
      viewport: { width: 1280, height: 800 },
      deviceScaleFactor: 1,
    });
    // Exact colours are checked below: the screens' glaze and the world's grade (lens-host.mjs) are left off.
    await context.addInitScript(() => { try { localStorage.setItem('rax:glaze', 'off'); } catch {} });
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
    const spin = await page.evaluate(async () => {
      let clock = 5400;
      performance.now = () => clock;
      try {
        const sample = (orbit, elapsed) => {
          clock += elapsed;
          view.render({bare: true, orbit, me: {...state.me, x: 0, z: -5, yaw: 60, pitch: 0}});
          const ray = view.aimForward();
          return Math.atan2(ray.x, ray.z) * 180 / Math.PI;
        };
        const turn = (a, b) => Math.abs(((a - b + 540) % 360) - 180);
        let previous = sample(1, 0), largest = 0;
        for (let i = 1; i <= 84; i++) {
          const heading = sample(1 - i / 84, 1000 / 60);
          largest = Math.max(largest, turn(heading, previous));
          previous = heading;
          if (i % 21 === 0) await new Promise(requestAnimationFrame);
        }
        const parked = sample(1, 16);
        const afterPause = sample(1, 800);
        const camera = {x: 9, y: 15, z: -8, yaw: 20, pitch: -24};
        view.render({bare: true, orbit: 0.001, camera, spectator: true});
        const eye = view.aimOrigin();
        return {largest, pause: turn(afterPause, parked), drone: Math.hypot(eye.x-camera.x, eye.y-camera.y, eye.z-camera.z)};
      } finally { delete performance.now; }
    });
    assert.ok(spin.largest < 5, 'heading eases throughout the flight');
    assert.ok(spin.pause < 1, 'a paused corner resumes without skipping around the globe');
    assert.ok(spin.drone < 0.01, 'the flight reaches a spectator’s camera');
    // A build placed in the room must appear even at the far end of the flight.
    const distantRoom = async built => {
      await page.evaluate(built => {
        performance.now = () => 1000;
        try {
          view.grid({pixel: 2, left: 0, top: 0, width: 640, height: 400});
          view.render({bare: true, orbit: 1, park: {plaza: true, neighbours: {}},
            skyline: {top: 0, dx: 0, dy: 0, dz: 0}, sunAt: 0.5,
            objects: built ? [{id: 'landmark', x: 5, z: -7, version: 1,
              geometry: {boxes: [{x: 0, y: 3, z: 0, w: 8, h: 6, d: 5, color: 0xff3296}]}}] : []});
        } finally { delete performance.now; }
      }, built);
      return PNG.sync.read(await page.screenshot({clip: {x: 1160, y: 680, width: 120, height: 120}})).data;
    };
    assert.ok(!(await distantRoom(false)).equals(await distantRoom(true)), 'a room build changes the pixels at the paper cursor');
    await page.evaluate(() => view.grid(null));
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
    // Both the white pad dot and the cyan brackets share a pixel boundary.
    await page.evaluate(() => view.render({ ...state, pointerSource: 'pad' }));
    const cursor = PNG.sync.read(await page.screenshot({ clip: { x: 600, y: 360, width: 80, height: 80 } }));
    function center(r, g, b, inset = 0) {
      const xs = [], ys = [];
      for (let y = inset; y < 80-inset; y++) for (let x = inset; x < 80-inset; x++) {
        const i = (y * 80 + x) * 4;
        if (cursor.data[i] === r && cursor.data[i+1] === g && cursor.data[i+2] === b) {
          xs.push(x); ys.push(y);
        }
      }
      assert.ok(xs.length);
      return [(Math.min(...xs) + Math.max(...xs)) / 2, (Math.min(...ys) + Math.max(...ys)) / 2];
    }
    assert.deepEqual(center(255, 255, 255, 32), center(0, 255, 255));
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
  // 🚫 With neither WebGPU nor WebGL 2 the page says so and stops.
  {
    const context = await browser.newContext({ viewport: { width: 640, height: 400 } });
    await context.addInitScript(() => {
      Object.defineProperty(navigator, 'gpu', { value: undefined });
      const get = HTMLCanvasElement.prototype.getContext;
      HTMLCanvasElement.prototype.getContext = function (kind, ...rest) { return kind === 'webgl2' ? null : get.call(this, kind, ...rest); };
    });
    const page = await context.newPage();
    await page.route('**/rax/render-test?*', (r) => r.fulfill({ contentType: 'text/html', body: fixture }));
    const refused = [];
    page.on('pageerror', (e) => refused.push(e.message));
    await page.goto(base + 'render-test?lens=0&fovea=0');
    await page.waitForFunction(() => document.querySelector('canvas')?.dataset.engine === 'none');
    assert.ok(refused.some((m) => /needs WebGPU or WebGL 2/.test(m)), 'the page says it needs WebGPU or WebGL 2');
    await page.screenshot({ path: new URL('flat-none.png', shots).pathname });
    await context.close();
  }
  // The flat picture, exact colours: the two cards must agree.
  const differing = (a, b, by) => {
    let changed = 0;
    for (let i = 0; i < a.data.length; i += 4)
      if ([0, 1, 2].some((c) => Math.abs(a.data[i + c] - b.data[i + c]) > by)) changed++;
    return changed / (a.width * a.height);
  };
  const ratio = differing(images[0], images[1], 2);
  assert.ok(ratio < 0.005, `WebGPU/WebGL 2 image difference: ${ratio}`);
  // 🔭 And the whole of it: the lens, the middle drawn larger, the sun's
  // shadows, the screens' glaze and the world's grade.
  // The clock is held so the glaze's grain and slip fall alike on both.
  const whole = [];
  for (const fallback of [false, true]) {
    const context = await browser.newContext({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 1 });
    if (fallback) await context.addInitScript(() => Object.defineProperty(navigator, 'gpu', { value: undefined }));
    const page = await context.newPage();
    page.on('pageerror', (e) => errors.push(e.message));
    page.on('console', (m) => { if (m.type() === 'error') errors.push(m.text()); });
    await page.route('**/favicon.ico', (r) => r.fulfill({ status: 204 }));
    await page.route('**/rax/render-test?*', (r) => r.fulfill({ contentType: 'text/html', body: fixture }));
    await page.goto(base + 'render-test?sun=0.42');
    await page.waitForFunction(() => window.ready);
    const backend = await page.evaluate(() => document.getElementById('game').dataset.engine);
    assert.equal(backend, fallback ? 'webgl2' : 'webgpu');
    for (const [name, change] of [['first', {}], ['third', { third: true }], ['down', { me: { pitch: -40, yaw: 30 } }]]) {
      await page.evaluate((change) => {
        performance.now = () => 1000;
        try {
          const next = { ...state, shadows: true, ...change, me: { ...state.me, ...(change.me || {}) } };
          for (let i = 0; i < 3; i++) view.render(next); // (the shadows' reach settles over a few frames)
        } finally { delete performance.now; }
      }, change);
      await page.waitForTimeout(50);
      whole.push([name, PNG.sync.read(await page.screenshot({ path: new URL(`whole-${backend}-${name}.png`, shots).pathname }))]);
    }
    await context.close();
  }
  const wholeDifference = {};
  for (let i = 0; i < whole.length / 2; i++) {
    const [name, gpu] = whole[i], [, gl] = whole[i + whole.length / 2];
    wholeDifference[name] = differing(gpu, gl, 6);
    assert.ok(wholeDifference[name] < 0.02, `WebGPU/WebGL 2 lensed image difference (${name}): ${wholeDifference[name]}`);
  }

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
  console.log(JSON.stringify({ report, imageDifference: ratio, wholeDifference }, null, 2));
  await writeFile(
    new URL('flat-performance.json', shots),
    JSON.stringify({ report, imageDifference: ratio, wholeDifference }, null, 2),
  );
  console.log(
    'Passed: real WebGPU, WebGL 2 beside it (flat and lensed), the refusal with neither, depth occlusion, near clipping, both cameras and responsive rendering.',
  );
} finally {
  await browser.close();
  await app.close();
}
