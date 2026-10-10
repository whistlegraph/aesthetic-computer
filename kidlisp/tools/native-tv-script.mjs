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
import { KidLisp } from '../../system/public/aesthetic.computer/lib/kidlisp.mjs';
import { emitProgram } from '../../system/public/aesthetic.computer/lib/kidlisp-emit.mjs';
import { GpuFrame } from '../../system/public/aesthetic.computer/lib/gpu-frame.mjs';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const require = createRequire(resolve(root, 'system/package.json'));
const esbuild = require('esbuild');

const [mode = 'draw', switchArg = '20000', outArg = 'kidlisp/build/kidlisp-tv.js', densityArg = 'auto', ...pieceArgs] = process.argv.slice(2);
const pieceFiles = pieceArgs.length ? pieceArgs : ['kidlisp/examples/whistlegraph/shooter.lisp', 'kidlisp/examples/kernels/starfield.lisp', 'kidlisp/examples/whistlegraph/fia.lisp'];

const bundle = (await esbuild.build({
  entryPoints: [resolve(root, 'kidlisp/tools/native/entry.mjs')], bundle: true, write: false, format: 'iife', globalName: 'KidLispNative',
  platform: 'neutral', target: 'es2020', external: ['url', 'module', 'https', 'node:*'], logLevel: 'silent',
})).outputFiles[0].text;

// Each piece is emitted here, on the host, as one JavaScript function
// (kidlisp-emit.mjs): the shell binds it to the evaluator's runtime instead
// of compiling closures at load. The `; @compile` line is dropped so the
// evaluator does not also build the closure tree; `; @gpu` stays so the
// drawing records into the frame.
const directives = (source) => (/^\s*;\s*@gpu\b/m.test(source) ? '' : '; @gpu\n') + source.replace(/^\s*;\s*@compile\b.*$/mg, '');
const pieces = {}, emitted = {};
const quiet = console.log;
for (const file of pieceFiles) {
  const name = basename(file, '.lisp'), source = directives(readFileSync(resolve(root, file), 'utf8'));
  pieces[name] = source;
  console.log = () => {};
  try { const lisp = new KidLisp(); lisp.gpuFrame = new GpuFrame(); lisp.module(source, true); emitted[name] = emitProgram(lisp.ast, lisp); }
  finally { console.log = quiet; }
}

const script = `const buildVersion = 1;
globalThis.console = globalThis.console || { log() {}, warn() {}, error() {}, info() {}, debug() {} };
${bundle}
globalThis.performance = globalThis.performance || { now: () => Date.now() };
globalThis.KIDLISP_HOST_SOURCE = true;   // this script is itself host-built source: kernels may be emitted as JavaScript
const PIECES = ${JSON.stringify(pieces)};
const EMITTED = {
${Object.entries(emitted).map(([name, text]) => `  ${JSON.stringify(name)}: function (H) {\n${text}\n  },`).join('\n')}
};
const ORDER = ${JSON.stringify(Object.keys(pieces))};
const SWITCH_EVERY = ${Number(switchArg) || 20000};
const DRAW = ${mode === 'nodraw' ? 'false' : 'true'};
const DENSITY_AUTO = ${densityArg === 'auto' ? 'true' : 'false'};
let density = ${densityArg === 'auto' ? 0.5 : Number(densityArg) || 1};   // the piece's pixels per host pixel
const BUDGET_MS = 16;
const { KidLisp, programHelpers, readFrame } = KidLispNative;
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
const C = (v) => (v > 32000 ? 32000 : v < -32000 ? -32000 : v);   // the host refuses coordinates beyond ±32768, and the frame with them
const L = (x1, y1, x2, y2, r, g, b) => line(C(x1 * S), C(y1 * S), C(x2 * S), C(y2 * S), Math.max(1, S), r, g, b);
const WINDING = 1;   // the GPU triangle path may cull one orientation; every fill is emitted with this sign
const T = (x1, y1, x2, y2, x3, y3, r, g, b) => { x1 = C(x1 * S) / S; y1 = C(y1 * S) / S; x2 = C(x2 * S) / S; y2 = C(y2 * S) / S; x3 = C(x3 * S) / S; y3 = C(y3 * S) / S; const area = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1); if (area * WINDING < 0) triangle(x1 * S, y1 * S, x3 * S, y3 * S, x2 * S, y2 * S, r, g, b); else triangle(x1 * S, y1 * S, x2 * S, y2 * S, x3 * S, y3 * S, r, g, b); };
// The Xbox composites its box layer over the GPU triangles, so a filled box bigger than a dot is two triangles, in order with the rest.
const B = (x, y, w, h, r, g, b) => { if (Math.abs(w * h) <= 4) box(x * S, y * S, w * S, h * S, r, g, b); else { triangle(x * S, y * S, (x + w) * S, y * S, (x + w) * S, (y + h) * S, r, g, b); triangle(x * S, y * S, (x + w) * S, (y + h) * S, x * S, (y + h) * S, r, g, b); } };
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
// The 3D layer: the camera is built in host pixels; a placed mesh goes to the
// host's retained meshes where it has them (the Xbox: meshUpload/meshDraw
// with the 27-float camera) and through the projector otherwise, far to near,
// as triangle3d with depth when the host has it, else as plain triangles.
const { sceneCamera, placeMesh, projectMesh } = KidLispNative;
let camera27 = null; const handles = new Map(); let meshFaces = 0;
const hostMeshes = typeof meshUpload === "function" && typeof meshDraw === "function";
const host3d = typeof triangle3d === "function";
visit.camera = (x, y, z, yaw, pitch, fov, near) => { camera27 = sceneCamera(x * S, y * S, z * S, yaw, pitch, fov, HW, HH, camera27 || new Float32Array(27), (near || 1) * S); };
visit.place = (id, x, y, z, yaw, pitch, roll, scale, a) => {
  const mesh = lisp && lisp.gpuFrame && lisp.gpuFrame.meshes.get(id); if (!mesh || !camera27) return;
  if (hostMeshes) {
    // meshDraw has no pose of its own: a mesh is uploaded in world space, so
    // each distinct pose of a mesh is uploaded once and kept (static scenes
    // cost one upload; a moving mesh re-uploads when its pose changes).
    const key = id + ":" + (x * S) + "," + (y * S) + "," + (z * S) + "," + yaw + "," + pitch + "," + roll + "," + scale;
    let handle = handles.get(key);
    if (handle === undefined) {
      const world = placeMesh(mesh, x * S, y * S, z * S, yaw, pitch, roll, scale * S);
      handle = meshUpload(world.verts, world.faces); handles.set(key, handle);
      if (handles.size > 2048) { for (const [k, h] of handles) { if (typeof meshFree === "function") meshFree(h); handles.delete(k); if (handles.size <= 1024) break; } }
    }
    if (handle >= 0) meshFaces += meshDraw(handle, camera27, 1, 1, 1, 1, a / 255) | 0;
    return;
  }
  const world = placeMesh(mesh, x * S, y * S, z * S, yaw, pitch, roll, scale * S);
  meshFaces += projectMesh(camera27, world, (x1, y1, x2, y2, x3, y3, depth, r, g, b) => { if (host3d) triangle3d(C(x1), C(y1), depth, C(x2), C(y2), depth, C(x3), C(y3), depth, r, g, b); else triangle(C(x1), C(y1), C(x2), C(y2), C(x3), C(y3), r, g, b); }, a);
};
const COUNTS = { clear: 0, line: 0, box: 0, oval: 0, tri: 0, shape: 0, camera: 0, place: 0 };
const counting = { clear: () => COUNTS.clear++, line: () => COUNTS.line++, box: () => COUNTS.box++, oval: () => COUNTS.oval++, tri: () => COUNTS.tri++, shape: () => COUNTS.shape++, camera: () => COUNTS.camera++, place: () => COUNTS.place++ };
function drawFrame(buffer) {
  lastOps = buffer.length; for (const k in COUNTS) COUNTS[k] = 0; meshFaces = 0; readFrame(buffer, counting); lastCounts = JSON.stringify(COUNTS);
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
  quietly(() => { piece = lisp.module(PIECES[ORDER[index]], true); lisp.setAPI(api); piece.boot(api); lisp.compiled = EMITTED[ORDER[index]](programHelpers(lisp)); });
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
  if (frames % 10 === 1) say("KIDLISP", JSON.stringify({ piece: ORDER[current], frame: frames, ms, density: Math.round(density * 100) / 100, drawMs: lastDrawMs, ops: lastOps, counts: lastCounts, meshFaces, w: W, h: H, err: lastErr }));
  box(6, 6, Math.min(HW - 12, fps * 3), 4, 255, 255, 255); box(6, 12, Math.min(HW - 12, ms), 4, 255, 120, 60);   // fps and ms, as bars, in host pixels
}
function act() {}
`;
const out = resolve(root, outArg);
mkdirSync(dirname(out), { recursive: true });
writeFileSync(out, script);
console.log(JSON.stringify({ out: outArg, mode, switchMs: Number(switchArg) || 20000, pieces: Object.keys(pieces), kb: Math.round(script.length / 1024) }));
