#!/usr/bin/env node
// The emitted program against the closure compiler: same piece, same seeded
// random, N frames each, the recorded frame buffers compared number for
// number. Prints the first difference, and the time per frame of each.
//
//   node kidlisp/tools/check-emit.mjs kidlisp/examples/whistlegraph/fia.lisp [frames=20]
import { readFileSync } from 'node:fs';
import { KidLisp } from '../../system/public/aesthetic.computer/lib/kidlisp.mjs';
import { compileProgram } from '../../system/public/aesthetic.computer/lib/kidlisp-compile.mjs';
import { emitProgram, programHelpers } from '../../system/public/aesthetic.computer/lib/kidlisp-emit.mjs';
import { GpuFrame } from '../../system/public/aesthetic.computer/lib/gpu-frame.mjs';

const [file, framesArg = '20'] = process.argv.slice(2);
if (!file) { console.error('usage: check-emit.mjs piece.lisp [frames]'); process.exit(1); }
const frames = Number(framesArg) || 20;
globalThis.KIDLISP_HOST_SOURCE = true;
const source = readFileSync(file, 'utf8').replace(/^\s*;\s*@compile\b.*$/m, '').replace(/^\s*;\s*@gpu\b.*$/m, '');
const quiet = console.log;

function seeded() { let s = 12345; return () => { s = (s * 1664525 + 1013904223) >>> 0; return s / 4294967296; }; }
function api() {
  let ink = [255, 255, 255, 255];
  const a = { screen: { width: 390, height: 520 }, params: [], colon: [], system: { fps: 60 }, paintCount: 0,
    needsPaint() {}, fps() {}, toggleHUD() {}, page() {}, unmask() {}, mask() {}, blend() {}, inkrn: () => ink.slice(), clock: { time: () => new Date(0), resync() {} }, sound: {},
    gpuFrame: { available: true, probe() {}, send() {} },
    wipe() {}, line() {}, box() {}, circle() {}, oval() {}, tri() {}, shape() {}, write() {}, plot() {},
    ink(...v) { if (typeof v[0] === 'number') ink = [v[0], v[1] ?? v[0], v[2] ?? v[0], v[3] ?? 255]; return a; } };
  return a;
}
function run(mode) {
  const random = Math.random; Math.random = seeded();
  console.log = () => {};
  try {
    const lisp = new KidLisp();
    lisp.randomState = 12345;   // the evaluator seeds its own generator from the clock
    const a = api();
    const mod = lisp.module(source, true); lisp.setAPI(a); lisp.randomState = 12345; mod.boot(a);
    lisp.gpuFrame = new GpuFrame();
    if (mode === 'closures') lisp.compiled = compileProgram(lisp.ast, lisp);
    else { const text = emitProgram(lisp.ast, lisp); lisp.compiled = new Function('H', text)(programHelpers(lisp)); run.text = text; }
    const buffers = []; let ms = 0;
    for (let i = 0; i < frames; i++) {
      a.paintCount = i;
      const t0 = performance.now();
      mod.paint(a);
      ms += performance.now() - t0;
      buffers.push(lisp.gpuFrame.take());
      if (process.env.EMIT_DEBUG && i === 0) { const pool = lisp.pools.get(process.env.EMIT_DEBUG); if (pool) quiet(mode, 'pool', process.env.EMIT_DEBUG, 'count', pool.count, 'slot0', Array.from(pool.data.slice(0, pool.fields.length)).map((v) => +v.toFixed(4)).join(' '), 'globals', JSON.stringify(Object.fromEntries(Object.entries(lisp.globalDef).filter(([, v]) => typeof v === 'number')))); }
    }
    return { buffers, ms: ms / frames };
  } finally { console.log = quiet; Math.random = random; }
}
const [modeA = 'closures', modeB = 'emitted'] = (process.env.EMIT_MODES || 'closures,emitted').split(',');
const closures = run(modeA), emitted = run(modeB);
let same = true;
for (let i = 0; i < frames && same; i++) {
  const x = closures.buffers[i], y = emitted.buffers[i];
  if (x.length !== y.length) { same = false; console.log(`frame ${i}: ${x.length} floats from closures, ${y.length} emitted`); break; }
  for (let k = 0; k < x.length; k++) if (x[k] !== y[k] && !(Number.isNaN(x[k]) && Number.isNaN(y[k]))) { same = false; console.log(`frame ${i} float ${k}: closures ${x[k]} emitted ${y[k]}`); break; }
}
console.log(`${file}: ${same ? 'SAME' : 'DIFFERENT'} over ${frames} frames · closures ${closures.ms.toFixed(2)} ms/frame · emitted ${emitted.ms.toFixed(2)} ms/frame · ${((run.text || "").length / 1024).toFixed(0)} KB of source`);
if (process.env.EMIT_DUMP) console.log(run.text);
process.exit(same ? 0 : 1);
