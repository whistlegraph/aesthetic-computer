#!/usr/bin/env node
// A live script for the native bios shells (Xbox QuickJS, the Mac
// JavaScriptCore shell): the KidLisp bundle, a shim that draws a gpu-frame
// buffer with the host's wipe/box/line/triangle, the pieces embedded, and
// the boot/sim/paint/act lifecycle the shells call. PIECE-IL.md §10.
//
//   node kidlisp/tools/native-tv-script.mjs [draw|nodraw] [switchMs] [out.js] [density|auto] [piece.lisp ...]
// density: the piece's pixels per host pixel (0.5 = half resolution, drawn
// scaled up); auto starts at 0.5 and moves toward a 16 ms frame.
//   node xbox/tools/live.mjs hot-deploy kidlisp/build/kidlisp-tv.js     # the Xbox devkit
//
// Every 10th frame the script writes a `KIDLISP` telemetry line (piece,
// frame, ms, drawMs, op counts) and at boot a `KIDLISP_BENCH` line, so the
// host log says what the engine costs. The Mac shell reads it from
// live/oskiewar.js: copy the built app, replace that file, re-sign with the
// JIT entitlement (apple/oskiewar-mac/Oskiewar-macOS.entitlements), run with
// OSKIEWAR_PERF=1.
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { resolve, dirname, basename } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createRequire } from 'node:module';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const require = createRequire(resolve(root, 'system/package.json'));
const esbuild = require('esbuild');

const [mode = 'draw', switchArg = '20000', outArg = 'kidlisp/build/kidlisp-tv.js', densityArg = 'auto', ...pieceArgs] = process.argv.slice(2);
const pieceFiles = pieceArgs.length ? pieceArgs : ['kidlisp/examples/whistlegraph/shooter.lisp', 'kidlisp/examples/kernels/starfield.lisp', 'kidlisp/examples/whistlegraph/fia.lisp'];

const bundle = (await esbuild.build({
  entryPoints: [resolve(root, 'kidlisp/tools/native/entry.mjs')], bundle: true, write: false, format: 'iife', globalName: 'KidLispNative',
  platform: 'neutral', target: 'es2020', external: ['url', 'module', 'https', 'node:*'], logLevel: 'silent',
})).outputFiles[0].text;

const directives = (source) => (/^\s*;\s*@compile\b/m.test(source) ? '' : '; @compile\n') + (/^\s*;\s*@gpu\b/m.test(source) ? '' : '; @gpu\n') + source;
const pieces = Object.fromEntries(pieceFiles.map((file) => [basename(file, '.lisp'), directives(readFileSync(resolve(root, file), 'utf8'))]));

const script = `const buildVersion = 1;
globalThis.console = globalThis.console || { log() {}, warn() {}, error() {}, info() {}, debug() {} };
${bundle}
globalThis.performance = globalThis.performance || { now: () => Date.now() };
globalThis.KIDLISP_HOST_SOURCE = true;   // this script is itself host-built source: kernels may be emitted as JavaScript
const PIECES = ${JSON.stringify(pieces)};
const ORDER = ${JSON.stringify(Object.keys(pieces))};
const SWITCH_EVERY = ${Number(switchArg) || 20000};
const DRAW = ${mode === 'nodraw' ? 'false' : 'true'};
const DENSITY_AUTO = ${densityArg === 'auto' ? 'true' : 'false'};
let density = ${densityArg === 'auto' ? 0.5 : Number(densityArg) || 1};   // the piece's pixels per host pixel
const BUDGET_MS = 16;
const { KidLisp, compileProgram, readFrame } = KidLispNative;
let lisp = null, piece = null, current = -1, frames = 0, lastSwitch = 0, W = 390, H = 520, fpsCount = 0, fpsAt = 0, fps = 0;
let lastOps = 0, lastErr = "", lastDrawMs = 0, lastCounts = "";
const say = (event, detail) => { if (typeof telemetry === "function") telemetry(event, detail); };
function screen() { try { const c = capabilities(); if (c && c.width) { HW = c.width; HH = c.height; } } catch (_) {} W = Math.max(64, Math.round(HW * density)); H = Math.max(64, Math.round(HH * density)); }
let ink = [255, 255, 255, 255];
const api = {
  screen: { width: W, height: H }, params: [], colon: [], system: { fps: 60 }, paintCount: 0,
  needsPaint() {}, fps() {}, toggleHUD() {}, page() {}, unmask() {}, mask() {}, blend() {},
  inkrn: () => ink.slice(), clock: { time: () => new Date(), resync() {} }, sound: {},
  gpuFrame: { available: true, probe() {}, sent: false, send: (buffer) => drawFrame(buffer) },
  wipe() {}, line() {}, box() {}, circle() {}, oval() {}, tri() {}, shape() {}, write() {}, plot() {},
  ink(...a) { if (typeof a[0] === "number") ink = [a[0], a[1] ?? a[0], a[2] ?? a[0], a[3] ?? 255]; return api; },
};
// The hosts: wipe(r g b) · box(x y w h r g b) · line(x1 y1 x2 y2 width r g b) · triangle(x1 y1 x2 y2 x3 y3 r g b). No alpha, so faint ops are skipped.
const vis = (a) => a >= 48;
let S = 1;   // host pixels per piece pixel, 1/density
const L = (x1, y1, x2, y2, r, g, b) => line(x1 * S, y1 * S, x2 * S, y2 * S, Math.max(1, S), r, g, b);
const WINDING = 1;   // the GPU triangle path may cull one orientation; every fill is emitted with this sign
const T = (x1, y1, x2, y2, x3, y3, r, g, b) => { const area = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1); if (area * WINDING < 0) triangle(x1 * S, y1 * S, x3 * S, y3 * S, x2 * S, y2 * S, r, g, b); else triangle(x1 * S, y1 * S, x2 * S, y2 * S, x3 * S, y3 * S, r, g, b); };
const B = (x, y, w, h, r, g, b) => box(x * S, y * S, w * S, h * S, r, g, b);
const fan = (pts, r, g, b) => { for (let k = 1; k + 1 < pts.length / 2; k++) T(pts[0], pts[1], pts[k * 2], pts[k * 2 + 1], pts[k * 2 + 2], pts[k * 2 + 3], r, g, b); };
const ring = (pts, r, g, b) => { const n = pts.length / 2; for (let k = 0; k < n; k++) { const j = (k + 1) % n; L(pts[k * 2], pts[k * 2 + 1], pts[j * 2], pts[j * 2 + 1], r, g, b); } };
const visit = {
  clear: (r, g, b) => wipe(r, g, b),
  line: (x1, y1, x2, y2, th, r, g, b, a) => { if (vis(a)) L(x1, y1, x2, y2, r, g, b); },
  box: (x, y, w, h, fill, r, g, b, a) => { if (!vis(a)) return; if (fill) B(x, y, w, h, r, g, b); else ring([x, y, x + w, y, x + w, y + h, x, y + h], r, g, b); },
  oval: (cx, cy, rx, ry, fill, r, g, b, a) => { if (!vis(a)) return; const n = Math.max(8, Math.min(32, Math.round(Math.max(rx, ry)))); const pts = []; for (let k = 0; k < n; k++) { const t = (k / n) * Math.PI * 2; pts.push(cx + Math.cos(t) * rx, cy + Math.sin(t) * ry); } if (fill) fan(pts, r, g, b); else ring(pts, r, g, b); },
  tri: (x1, y1, x2, y2, x3, y3, fill, r, g, b, a) => { if (!vis(a)) return; if (fill) T(x1, y1, x2, y2, x3, y3, r, g, b); else ring([x1, y1, x2, y2, x3, y3], r, g, b); },
  shape: (pts, fill, r, g, b, a) => { if (!vis(a)) return; const p = Array.from(pts); if (fill) fan(p, r, g, b); else ring(p, r, g, b); },
};
const COUNTS = { clear: 0, line: 0, box: 0, oval: 0, tri: 0, shape: 0 };
const counting = { clear: () => COUNTS.clear++, line: () => COUNTS.line++, box: () => COUNTS.box++, oval: () => COUNTS.oval++, tri: () => COUNTS.tri++, shape: () => COUNTS.shape++ };
function drawFrame(buffer) {
  lastOps = buffer.length; for (const k in COUNTS) COUNTS[k] = 0; readFrame(buffer, counting); lastCounts = JSON.stringify(COUNTS);
  S = HW / W; const d0 = Date.now(); if (DRAW) readFrame(buffer, visit); lastDrawMs = Date.now() - d0;
}
function bench() {
  const t0 = Date.now(); let acc = 0; for (let i = 0; i < 10000000; i++) acc += Math.sin(i) * 0.5; const loopMs = Date.now() - t0;
  const t1 = Date.now(); for (let i = 0; i < 10000; i++) box(i % 100, 0, 1, 1, 0, 0, 0); const boxMs = Date.now() - t1;
  const t2 = Date.now(); for (let i = 0; i < 10000; i++) triangle(0, 0, 1, 0, 0, 1, 0, 0, 0); const triMs = Date.now() - t2;
  say("KIDLISP_BENCH", JSON.stringify({ loop1e7Ms: loopMs, box10kMs: boxMs, tri10kMs: triMs, acc: Math.round(acc) }));
}
function quietly(fn) { const log = console.log; console.log = () => {}; try { return fn(); } finally { console.log = log; } }
function load(index) {
  current = index; lisp = new KidLisp();
  quietly(() => { piece = lisp.module(PIECES[ORDER[index]], true); lisp.setAPI(api); piece.boot(api); });
  frames = 0; lastSwitch = Date.now();
}
function boot() { try { bench(); } catch (_) {} screen(); api.screen.width = W; api.screen.height = H; load(0); }
function sim() {}
function paint() {
  screen(); api.screen.width = W; api.screen.height = H;
  if (Date.now() - lastSwitch > SWITCH_EVERY) load((current + 1) % ORDER.length);
  api.paintCount = frames++;
  const t0 = Date.now();
  try { quietly(() => { piece.sim && piece.sim(api); piece.paint(api); }); }
  catch (e) { lastErr = String((e && e.message) || e).slice(0, 200); if (frames % 10 === 1) say("KIDLISP_ERR", lastErr); box(0, 0, W, 40, 120, 0, 0); return; }
  fpsCount++; if (Date.now() - fpsAt >= 1000) { fps = fpsCount; fpsCount = 0; fpsAt = Date.now(); }
  const ms = Date.now() - t0;
  if (DENSITY_AUTO) { if (ms > BUDGET_MS) density = Math.max(0.25, density * Math.max(0.7, Math.sqrt(BUDGET_MS / ms))); else if (ms < BUDGET_MS * 0.7) density = Math.min(1, density * 1.04); }
  if (frames % 10 === 1) say("KIDLISP", JSON.stringify({ piece: ORDER[current], frame: frames, ms, density: Math.round(density * 100) / 100, drawMs: lastDrawMs, ops: lastOps, counts: lastCounts, w: W, h: H, err: lastErr }));
  box(6, 6, Math.min(HW - 12, fps * 3), 4, 255, 255, 255); box(6, 12, Math.min(HW - 12, ms), 4, 255, 120, 60);   // fps and ms, as bars, in host pixels
}
function act() {}
`;
const out = resolve(root, outArg);
mkdirSync(dirname(out), { recursive: true });
writeFileSync(out, script);
console.log(JSON.stringify({ out: outArg, mode, switchMs: Number(switchArg) || 20000, pieces: Object.keys(pieces), kb: Math.round(script.length / 1024) }));
