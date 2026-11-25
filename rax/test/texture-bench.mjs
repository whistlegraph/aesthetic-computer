// 🧊 The texture bench: spinning cubes wearing textures, measured. Each run
// stages some cubes in front of the eye and renders for a few seconds, and
// reports the frame's cost on the page (the render call), the frames a
// second the browser managed, and the textures' own traffic (steps, uploads,
// bytes, layers). A screenshot of each run goes beside the numbers.
//
//   node rax/test/texture-bench.mjs                 the whole sweep
//   node rax/test/texture-bench.mjs --quick         one run of each source
//   --faces '$roz,$bop'  --sizes 64,256  --cubes 1,12  --dpr 1,2  --seconds 4  --live (show the page)
//   --sources kidlisp.json   KidLisp sources from a file ({ code: source }), not AC's store
//
// It needs a GPU with WebGPU (real Chrome): run it on poorslice, not here.
import { once } from 'node:events';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';

const arg = (name, fallback) => {
  const i = process.argv.indexOf(`--${name}`);
  return i < 0 ? fallback : process.argv[i + 1];
};
const list = (name, fallback) => String(arg(name, fallback)).split(',').map((s) => s.trim()).filter(Boolean);
const quick = process.argv.includes('--quick');
const sourceFile = arg('sources', null) ? JSON.parse(await readFile(arg('sources'), 'utf8')) : null;
const seconds = Number(arg('seconds', quick ? 3 : 5));
const SOURCES = {
  kidlisp: ['$roz', '$bop', '$7zt', '$8an', '$oyo', '$4ba'],
  still: ['$roz:still', '$bop:still', '$7zt:still', '$8an:still', '$oyo:still', '$4ba:still'],
  procedural: ['fn:plasma', 'fn:checker', 'fn:rings', 'fn:plasma', 'fn:checker', 'fn:rings'],
  none: [],
};
const faces = arg('faces', null);
const sets = faces ? { given: faces.split(',') } : quick ? SOURCES : SOURCES;
const sizes = list('sizes', quick ? '128' : '32,64,128,256').map(Number);
const cubes = list('cubes', quick ? '6' : '1,6,24').map(Number);
const dprs = list('dpr', quick ? '1' : '1,2').map(Number);

const app = createRaxServer({ guests: true, prefix: '/rax' });
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}/rax/`;
const out = new URL('../../tmp/rax/texture-bench/', import.meta.url);
await mkdir(out, { recursive: true });

const fixture = `<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1">
<style>html,body{margin:0;width:100%;height:100%;overflow:hidden;background:#000}canvas{display:block;width:100%;height:100%}</style>
<canvas id="game"></canvas><script type="importmap">{"imports":{"/system/public/aesthetic.computer/lib/pmove.mjs":"./pmove.mjs"}}</script>
<script type="module">
import {createView} from './view.mjs';import {spawn} from './world.mjs';
import {texturedCube, registerProcedures} from './texture-demo.mjs';
const q = new URLSearchParams(location.search);
const view = await createView(document.getElementById('game'));
registerProcedures(view.textures);
const faces = (q.get('faces') || '').split(',').filter(Boolean), count = Number(q.get('cubes') || 1);
view.textures.config.size = Number(q.get('size') || 0);
view.textures.config.still = q.get('still') === '1';
const me = {...spawn(0), x: 0, z: -6, yaw: 0, pitch: 0};
const cube = faces.length ? texturedCube(faces, 1.2) : null;
const plain = texturedCube([], 1.2);
const state = {shadows: false, me, players: [me], members: [], sunAt: 0.5};
const placed = Array.from({length: count}, (_, i) => {
  const row = Math.floor(i / 6), col = i % 6;
  return {x: (col - 2.5) * 1.8, y: 1.4 + row * 1.6, z: -1 + row * 0.6};
});
window.run = async (ms) => {
  const frames = [], costs = [];
  let last = performance.now();
  const end = last + ms;
  while (performance.now() < end) {
    await new Promise(requestAnimationFrame);
    const t = performance.now();
    frames.push(t - last); last = t;
    view.stage(placed.map((p, i) => ({...(cube || plain), ...p, yaw: t / 900 + i, pitch: t / 1300 + i * 0.5})));
    const begun = performance.now();
    view.render(state);
    costs.push(performance.now() - begun);
  }
  const sorted = (a) => [...a].sort((x, y) => x - y), pct = (a, p) => sorted(a)[Math.min(a.length - 1, Math.floor(a.length * p))];
  const s = view.textures.stats();
  return {frames: frames.length, fps: frames.length / (ms / 1000), frameP50: pct(frames, .5), frameP95: pct(frames, .95),
    renderP50: pct(costs, .5), renderP95: pct(costs, .95), textures: s, sources: view.textures.sources()};
};
window.ready = true;
</script>`;

// 🐧 Linux Chrome offers WebGPU only when asked, and headless it gets SwiftShader (software)
// and loses its device: run it headful in a private display there, the real GPU's
// (on jastow: `xvfb-run -a -s "-screen 0 1920x1080x24" node rax/test/texture-bench.mjs`).
const linux = process.platform === 'linux';
const browser = await chromium.launch({ channel: 'chrome', headless: !process.argv.includes('--live') && !linux,
  args: ['--enable-unsafe-webgpu', '--disable-gpu-vsync', '--disable-frame-rate-limit', ...(linux ? ['--enable-gpu', '--ignore-gpu-blocklist', '--enable-features=Vulkan', '--use-vulkan=native', '--use-angle=vulkan'] : [])] });
const results = [];
try {
  for (const [set, faceList] of Object.entries(sets))
    for (const size of set === 'none' ? [0] : sizes)
      for (const count of cubes)
        for (const dpr of dprs) {
          const context = await browser.newContext({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: dpr });
          const page = await context.newPage();
          const errors = [];
          page.on('pageerror', (e) => { errors.push(e.message); console.log('page error:', e.message); });
          page.on('console', (m) => { if (m.type() === 'error') console.log('console:', m.text()); });
          await page.route('**/rax/texture-bench?*', (r) => r.fulfill({ contentType: 'text/html', body: fixture }));
          // --sources file.json ({ code: source }): KidLisp from a file, not AC's store (offline, or a box that cannot reach it).
          if (sourceFile) await page.route('**/rax/kidlisp-source?*', (r) => {
            const code = new URL(r.request().url()).searchParams.get('code');
            return sourceFile[code] ? r.fulfill({ contentType: 'application/json', body: JSON.stringify({ code, source: sourceFile[code] }) }) : r.fulfill({ status: 404 });
          });
          const q = new URLSearchParams({ faces: faceList.join(','), cubes: count, size, still: set === 'still' ? 1 : 0 });
          await page.goto(`${base}texture-bench?${q}`);
          await page.waitForFunction(() => window.ready, null, { timeout: 30000 });
          await page.evaluate(() => window.run(1500)); // (warm: sources load, the first layers go up)
          const r = await page.evaluate((ms) => window.run(ms), seconds * 1000);
          const name = `${set}-${size}px-${count}cubes-${dpr}x`;
          await page.screenshot({ path: new URL(`${name}.png`, out).pathname });
          const row = { set, size, cubes: count, dpr, ...r, errors };
          results.push(row);
          const t = r.textures;
          console.log(`${name.padEnd(34)} fps ${r.fps.toFixed(1).padStart(5)}  frame p95 ${r.frameP95.toFixed(1).padStart(5)} ms  render p50 ${r.renderP50.toFixed(2)} / p95 ${r.renderP95.toFixed(2)} ms  steps ${t.steps} (${(t.stepMs / Math.max(1, t.steps)).toFixed(2)} ms each)  up ${(t.bytes / 1e6).toFixed(1)} MB  layers ${t.layers}/${t.of}${t.failed.length ? `  FAILED ${t.failed.join('; ')}` : ''}${errors.length ? `  ERRORS ${errors.join('; ')}` : ''}`);
          await context.close();
        }
} finally {
  await browser.close();
  app.server.close();
  await writeFile(new URL('results.json', out), JSON.stringify(results, null, 2));
  console.log(`\nresults and screenshots: ${out.pathname}`);
}
process.exit(0);
