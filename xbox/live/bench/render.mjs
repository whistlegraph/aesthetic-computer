#!/usr/bin/env node
// Capture one deterministic triangle trace; replay it through both rasterizers.
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { createServer } from 'node:http';
import { resolve, dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { performance } from 'node:perf_hooks';
import { chromium } from 'playwright';
import { PNG } from 'pngjs';

const here = dirname(fileURLToPath(import.meta.url));
const live = resolve(here, '..');
const args = process.argv.slice(2);
const option = (name, fallback) => args.includes(name) ? args[args.indexOf(name) + 1] : fallback;
const output = resolve(option('--out', '/tmp/oskiewar-render-bench'));
const debugEnabled = args.includes('--debug');
const count = Number(option('--frames', '120'));
if (!Number.isInteger(count) || count < 30 || count > 600) throw new Error('--frames must be 30..600');
const source = await readFile(join(live, 'oskiewar.js'), 'utf8');
await mkdir(output, { recursive: true });
let now = 0, seed = 7341, commands = [], ignored = {}, clear = [0, 0, 0];
const seededMath = Object.create(Math);
seededMath.random = () => { seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed / 2 ** 32; };
const noOp = () => {};
const pads = [0, 1].map(() => ({ connected: true, down: [], leftX: 0, leftY: 0 }));
const names = ['runtime', 'gamepad', 'capabilities', 'telemetry', 'gameSignal', 'saveReplay',
  'publishLive', 'analytics', 'drum', 'wipe', 'box', 'line', 'triangle', 'triangle3d',
  'write', 'systemWrite', 'gameView', 'Math', 'Date'];
const omit = (name) => () => { ignored[name] = (ignored[name] || 0) + 1; };
const game = new Function(...names, `${source}\nreturn {boot,sim,paint,players,balls,
  setup(){shellMode='GAME';selecting=false;players[1].npc=false;players[1].bot=false;
    startFightAgainst('dummy',runtime().monotonicUs);players[0].x=1400;players[0].y=terrainFloorAt(1400);
    players[0].skateboard=true;players[0].skateVx=900;players[1].x=2100;players[1].y=terrainFloorAt(2100);
    for(const b of balls)b.active=false;debugHitboxes=${debugEnabled};}, error:()=>clientError};`)(
  () => ({ monotonicUs: now, unixMs: 1789401600000 + now / 1000, simCount: Math.floor(now / 16667), paintCount: 0 }),
  (index) => pads[index], () => ({ platform: 'web', inputFamily: 'keyboard', colorScheme: 'dark' }),
  noOp, noOp, noOp, noOp, noOp, noOp,
  (...ink) => { clear = ink.slice(0, 3); }, omit('box'), omit('line'), omit('triangle2d'),
  (...values) => commands.push(...values), omit('write'), omit('systemWrite'),
  () => ({ width: 1920, height: 1080 }), seededMath,
  class extends Date { static now() { return 1789401600000 + Math.floor(now / 1000); } });
game.boot(); game.setup();
for (let i = 0; i < 185; i++) { now += 16667; game.sim(); }
const trace = [], generation = [], sim = [], omitted = {};
for (let i = 0; i < 60; i++) {
  pads[0].down = i < 20 ? ['ArrowRight'] : i < 40 ? ['ArrowLeft'] : [];
  for (let step = 0; step < 3; step++) { now += 16667; const t = performance.now(); game.sim(); sim.push(performance.now() - t); }
  commands = []; ignored = {};
  const t = performance.now(); game.paint(); generation.push(performance.now() - t);
  if (game.error()) throw new Error(game.error());
  if (!commands.every(Number.isFinite)) throw new Error('Nonfinite captured vertex');
  if (commands.length % 12) throw new Error('Invalid captured triangle');
  for (const [key, value] of Object.entries(ignored)) omitted[key] = (omitted[key] || 0) + value;
  trace.push({ clear, triangles: commands });
}
const traceText = JSON.stringify(trace);
await writeFile(join(output, 'trace.json'), traceText);
const traceHash = createHash('sha256').update(traceText).digest('hex');
const sourceHash = createHash('sha256').update(source).digest('hex');
await writeFile(join(output, 'trace-meta.json'), JSON.stringify({ sourceHash, traceHash, debugEnabled, omitted }, null, 2)+'\n');
if (args.includes('--capture-only')) { console.log(traceHash); process.exit(0); }

const server = createServer(async (req, res) => {
  const pathname = new URL(req.url, 'http://localhost').pathname;
  const files = { '/': join(here, 'render.html'), '/render-browser.mjs': join(here, 'render-browser.mjs'), '/pack.mjs': join(here, 'pack.mjs'),
    '/scene3d-webgl.mjs': join(live, 'scene3d-webgl.mjs'), '/scene3d.mjs': join(live, 'scene3d.mjs'), '/trace.json': join(output, 'trace.json') };
  if (!files[pathname]) { res.writeHead(404); res.end(); return; }
  try { res.setHeader('Content-Type', pathname === '/' ? 'text/html' : pathname.endsWith('.json') ? 'application/json' : 'text/javascript'); res.end(await readFile(files[pathname])); }
  catch { res.writeHead(500); res.end(); }
});
await new Promise(r => server.listen(0, '127.0.0.1', r));
let browser;
try {
  const chrome = [process.env.PUPPETEER_EXECUTABLE_PATH,
    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
    '/usr/bin/google-chrome', '/usr/bin/chromium',
    process.env.PROGRAMFILES && join(process.env.PROGRAMFILES, 'Google/Chrome/Application/chrome.exe'),
  ].find(path => path && existsSync(path));
  browser = await chromium.launch({ headless: !args.includes('--headed'), executablePath: option('--chrome', chrome) });
  const page = await browser.newPage({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 1 });
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  await page.goto(`http://127.0.0.1:${server.address().port}/`);
  await page.waitForFunction(() => globalThis.bench);
  const hardware = await page.evaluate(() => bench.hardware());
  const idleBefore = await page.evaluate(count => bench.idle(count), count);
  const results = [];
  for (const size of [[1920, 1080], [1280, 720]]) {
    for (const order of [['canvas', 'webgl-stream', 'webgl-prepacked'], ['webgl-prepacked', 'webgl-stream', 'canvas']]) {
      for (const mode of order) {
        process.stdout.write(`${size.join('x')} ${mode}\n`);
        const result = await page.evaluate(({ mode, size, count }) => bench.run(mode, ...size, count), { mode, size, count });
        results.push(result);
        if (size[0] === 1920) await page.locator('#'+(mode === 'canvas' ? 'canvas' : 'gpu')).screenshot({ path: join(output, mode + '.png') });
      }
    }
  }
  const sorted = values => { values.sort((a,b)=>a-b); return { p50: values[Math.floor(values.length*.5)], p95: values[Math.floor(values.length*.95)] }; };
  const a = PNG.sync.read(await readFile(join(output, 'canvas.png')));
  const b = PNG.sync.read(await readFile(join(output, 'webgl-stream.png')));
  if (a.width !== b.width || a.height !== b.height) throw new Error('Screenshot dimensions differ');
  let difference = 0, changed = 0;
  for (let i = 0; i < a.data.length; i += 4) {
    let maximum = 0;
    for (let c = 0; c < 3; c++) {
      const delta = Math.abs(a.data[i+c] - b.data[i+c]);
      difference += delta; maximum = Math.max(maximum, delta);
    }
    if (maximum > 16) changed++;
  }
  const visualComparison = { width: a.width, height: a.height,
    meanAbsoluteChannelDifference: difference / (a.width * a.height * 3),
    fractionPixelsAbove16: changed / (a.width * a.height) };
  const report = { version: 1, sourceHash, traceHash, debugEnabled, traceFrames: trace.length,
    triangleRange: [Math.min(...trace.map(f=>f.triangles.length/12)), Math.max(...trace.map(f=>f.triangles.length/12))],
    omittedCommands: omitted, generationMs: sorted(generation), simulationMs: sorted(sim), hardware,
    scope: 'Identical recorded triangles only. Boxes, lines, text, simulation, and production composition are excluded from replay. Depth testing disabled for painter-order parity. Prepacked mode is a prepacked-trace lower bound, not a dynamic game implementation. rAF intervals are callback pacing, not proof of physical presentation.',
    visualComparison, idleBefore, idleAfter: await page.evaluate(count => bench.idle(count), count), errors, results };
  await writeFile(join(output, 'report.json'), JSON.stringify(report, null, 2)+'\n');
  if (errors.length) throw new Error(errors.join('\n'));
  console.log(`Report: ${join(output, 'report.json')}`);
} finally { await browser?.close(); await new Promise(r => server.close(r)); }
