import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createHash} from 'node:crypto';
import {KidLisp} from '../system/public/aesthetic.computer/lib/kidlisp.mjs';
import {compileProgram} from '../system/public/aesthetic.computer/lib/kidlisp-compile.mjs';

// The compiler against the interpreter on the same program, same seeded
// random, same recording host: the draw streams must match exactly.
function host() {
  const calls = [];
  const rec = name => (...a) => calls.push(name + ' ' + a.map(v => typeof v === 'number' ? v.toFixed(4) : typeof v === 'object' ? JSON.stringify(v) : String(v)).join(' '));
  const api = {screen: {width: 96, height: 64}, params: [], colon: [], system: {fps: 60}, paintCount: 0, needsPaint() {}, fps() {}, toggleHUD() {}, page() {}, unmask() {}, mask() {}, blend() {}, inkrn: () => [0, 0, 0, 255], clock: {time: () => new Date(0), resync() {}}, sound: {synth: () => ({update() {}, kill() {}})},
    wipe: rec('wipe'), ink: (...a) => { rec('ink')(...a); return api; }, line: rec('line'), box: rec('box'), circle: rec('circle'), oval: rec('oval'), tri: rec('tri'), shape: rec('shape'), write: rec('write'), plot: rec('plot')};
  return {api, calls};
}
function run(source, mode, frames = 3, events = []) {
  const h = host();
  const quiet = {log: console.log, warn: console.warn, error: console.error};
  const warnings = [];
  console.log = () => {}; console.warn = console.error = (...a) => warnings.push(a.join(' '));
  try {
    const lisp = new KidLisp();
    let seed = 7; lisp.seededRandom = () => { seed = (seed * 1664525 + 1013904223) >>> 0; return seed / 4294967296; };
    const mod = lisp.module(source, true); lisp.setAPI(h.api); mod.boot(h.api);
    if (mode === 'compiled') lisp.compiled = compileProgram(lisp.ast, lisp);
    for (let f = 0; f < frames; f++) { h.api.paintCount = f; for (const event of events) mod.act({api: h.api, event: {...event, is: t => String(event.name).indexOf(t) === 0}}); mod.sim?.(h.api); mod.paint(h.api); }
    return {calls: h.calls, warnings, compiled: lisp.compiled};
  } finally { Object.assign(console, quiet); }
}
const digest = calls => createHash('sha256').update(calls.join('\n')).digest('hex');
const same = (source, frames, events) => {
  const a = run(source, 'interpreted', frames, events), b = run(source, 'compiled', frames, events);
  assert.ok(b.compiled, 'compiled');
  assert.equal(b.calls.length, a.calls.length, 'same number of draw calls');
  assert.equal(digest(b.calls), digest(a.calls), 'same draw stream\n' + a.calls.slice(0, 6).join('\n') + '\n---\n' + b.calls.slice(0, 6).join('\n'));
  return b;
};

test('arithmetic, if/else, def/now, functions, repeat, pools and drawing compile to the same stream', () => {
  const b = same(`(def speed 2) (def tally 0)
(later tri2 aa bb (def cc (+ aa bb)) (if (> cc 5) (* cc 2) else (- 0 cc)))
(pool dots 8 x y vx)
(once (repeat 5 ii (spawn dots (x (* ii 10)) (y (random 40)) (vx (+ 1 ii)))))
(each dots (def x (+ x (* vx speed))) (if (> x 90) (kill) else (box x y 3 3)))
(rank dots y)
(each dots (ink (* x 2) y 100) (circle x y (hypot vx 2)))
(now tally (+ tally (alive dots)))
(repeat 4 kk (line kk (tri2 kk 3) (+ kk 5) (abs (- 0 kk)) 1))
(ink red) (write (pow 2 3) 1 2)
(box (clamp (atan2 1 2) 0 1) (floor 2.7) (sqrt 16) (exp 0))`, 6);
  assert.ok(b.calls.length > 40);
});

test('top-level behaviour forms are handed to the interpreter and see compiled globals', () => {
  const src = `(def hits 0)
(tap (now hits (+ hits 1)))
(once (now hits 10))
(0s (box hits 0 1 1))
(box hits 5 1 1)`;
  // The tap form registers during paint, so the first frame's touch finds no handler; the next two do.
  const b = same(src, 3, [{name: 'touch', x: 1, y: 1}]);
  assert.ok(b.calls.some(c => c.startsWith('box 12')), 'tap ran on the interpreter path and the compiled read saw it: ' + b.calls.filter(c => c.startsWith('box')).join(','));
});

test('a program the compiler cannot take falls back with a warning', () => {
  const lisp = new KidLisp();
  const warnings = []; const w = console.warn; console.warn = (...a) => warnings.push(a.join(' ')); console.log = () => {};
  try { lisp.module('; @compile\n(later ff aa (once (box aa 1 1 1)))\n(ff 3)', true); }
  finally { console.warn = w; }
  assert.equal(lisp.compiled, null);
  assert.ok(warnings.some(t => /fell back/.test(t)), warnings.join(' | '));
});

test('the two Whistlegraph pieces compile and draw the interpreter\'s stream', () => {
  for (const name of ['fia', 'shooter']) {
    const source = readFileSync(new URL(`../kidlisp/examples/whistlegraph/${name}.lisp`, import.meta.url), 'utf8');
    const b = run(source, 'compiled', 2);
    assert.ok(b.compiled, name + ' compiled');
    assert.ok(b.calls.length > 1000, name + ' drew ' + b.calls.length);
    assert.deepEqual(b.warnings.filter(t => !/Preview|synth/.test(t)), [], name + ' warnings');
  }
});
