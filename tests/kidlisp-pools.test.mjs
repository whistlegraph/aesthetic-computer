import test from 'node:test';
import assert from 'node:assert/strict';
import {KidLisp} from '../system/public/aesthetic.computer/lib/kidlisp.mjs';
import {KidLispExecution} from '../system/public/aesthetic.computer/lib/kidlisp-execution.mjs';

// A recording host: every drawing call lands in `calls`; sound hands back
// voice handles that record what they were told.
function host({sound = true} = {}) {
  const calls = [], voices = [];
  const api = {
    screen: {width: 64, height: 64}, params: [], colon: [], system: {fps: 60}, paintCount: 0, needsPaint() {}, fps() {}, toggleHUD() {}, page() {}, unmask() {}, mask() {}, blend() {}, inkrn: () => [0, 0, 0, 255], shape: (...a) => calls.push(['shape', ...a]), oval: (...a) => calls.push(['oval', ...a]), tri: (...a) => calls.push(['tri', ...a]), plot: (...a) => calls.push(['plot', ...a]),
    wipe: (...a) => calls.push(['wipe', ...a]), ink: (...a) => { calls.push(['ink', ...a]); return api; },
    box: (...a) => calls.push(['box', ...a]), line: (...a) => calls.push(['line', ...a]), circle: (...a) => calls.push(['circle', ...a]),
    write: (...a) => calls.push(['write', ...a]), clock: {time: () => new Date(0)},
    sound: sound ? {synth: opts => { const v = {opts, updates: [], killed: false, update: p => v.updates.push(p), kill: () => { v.killed = true; }}; voices.push(v); return v; }} : {},
  };
  return {api, calls, voices};
}
function piece(source, h) {
  const execution = new KidLispExecution({seed: 1, epochMs: 0, stepMs: 1000 / 60, maxSteps: 200000, maxDepth: 128});
  const lisp = new KidLisp({execution});
  const mod = lisp.module(source, true);
  lisp.setAPI(h.api);
  mod.boot?.(h.api);
  return {lisp, mod, execution, frame(n = 1, events = []) { for (let i = 0; i < n; i++) { execution.beginFrame(i); for (const event of events) mod.act({api: h.api, event: {...event, is: t => String(event.name || event.type).indexOf(t) === 0}}); mod.sim?.(h.api); mod.paint(h.api); } }};
}
const draws = (calls, name) => calls.filter(c => c[0] === name);

test('the new arithmetic forms evaluate as numbers', () => {
  const h = host();
  const p = piece('(box (abs -3) (sqrt 16) (hypot 3 4) (atan2 1 0))\n(box (exp 0) (pow 2 5) (sign -7) (clamp 9 0 5))', h);
  p.frame();
  assert.deepEqual(draws(h.calls, 'box')[0].slice(1, 5), [3, 4, 5, Math.atan2(1, 0)]);
  assert.deepEqual(draws(h.calls, 'box')[1].slice(1, 5), [1, 32, -1, 5]);
});

test('if takes an else branch and runs only one side', () => {
  const h = host();
  piece('(if (> 1 2) (box 1 1 1 1) (box 2 2 2 2) else (box 3 3 3 3))\n(if (< 1 2) (box 4 4 4 4) else (box 5 5 5 5) (box 6 6 6 6))', h).frame();
  assert.deepEqual(draws(h.calls, 'box').map(c => c[1]), [3, 4]);
});

test('a pool spawns into slots, each walks them with fields as locals, kill frees, full pools reuse the oldest', () => {
  const h = host();
  const p = piece(`(pool dots 3 x y vx)
(once (spawn dots (x 1) (y 10) (vx 2)) (spawn dots (x 5) (y 20) (vx 1)))
(each dots (def x (+ x vx)) (box x y 1 1) (if (> x 6) (kill)))`, h);
  p.frame();
  assert.deepEqual(draws(h.calls, 'box').map(c => [c[1], c[2]]), [[3, 10], [6, 20]], 'fields read and written back');
  assert.equal(p.lisp.pools.get('dots').count, 2);
  h.calls.length = 0; p.frame();
  assert.deepEqual(draws(h.calls, 'box').map(c => [c[1], c[2]]), [[5, 10], [7, 20]]);
  assert.equal(p.lisp.pools.get('dots').count, 1, 'the dot past 6 was killed after drawing');
  h.calls.length = 0; p.frame();
  assert.deepEqual(draws(h.calls, 'box').map(c => [c[1], c[2]]), [[7, 10]], 'only the live slot is visited');
  // Fill past capacity: the oldest slot is reused, count stays at capacity.
  const q = piece('(pool p 2 a)\n(spawn p (a frame))\n(each p (box a 0 1 1))', host());
  q.frame(5);
  assert.equal(q.lisp.pools.get('p').count, 2);
  assert.equal(q.lisp.pools.get('p').alive.reduce((n, v) => n + v, 0), 2);
});

test('each charges the work ledger by capacity', () => {
  const h = host();
  const p = piece('(pool p 50 a)\n(each p (box a 0 1 1))', h);
  const before = p.execution.state.steps; p.frame();
  assert.ok(p.execution.state.steps - before >= 50, 'capacity is charged even when empty');
});

test('hum starts a sustained voice once, tune updates it, hush and leave stop it', () => {
  const h = host();
  const p = piece('(hum lead sine 220 0.4 0.05 0.5)\n(tune lead (+ 200 frame) 0.3)', h);
  p.frame(3);
  assert.equal(h.voices.length, 1, 'one voice across three frames');
  assert.deepEqual([h.voices[0].opts.type, h.voices[0].opts.tone, h.voices[0].opts.duration], ['sine', 220, Infinity]);
  assert.equal(h.voices[0].updates.at(-1).tone, 202);
  p.mod.leave?.();
  assert.equal(h.voices[0].killed, true);
  const quiet = host({sound: false});
  piece('(hum lead sine 220)', quiet).frame();
  assert.equal(quiet.voices.length, 0, 'no synth, no voice, no error');
});

test('key and pad read the state the last events left', () => {
  const h = host();
  const p = piece('(if (key space) (box 1 1 1 1))\n(box (pad leftx) (pad a) 1 1)', h);
  p.frame(1, [{name: 'keyboard:down: ', key: ' '}, {name: 'gamepad:0:axis:0', axis: 0, value: -0.5}, {name: 'gamepad:0:button:0:push', button: 0, action: 'push'}]);
  assert.deepEqual(draws(h.calls, 'box').map(c => c.slice(1, 3)), [[1, 1], [-0.5, 1]]);
  h.calls.length = 0;
  p.frame(1, [{name: 'keyboard:up: ', key: ' '}, {name: 'gamepad:0:button:0:release', button: 0, action: 'release'}]);
  assert.deepEqual(draws(h.calls, 'box').map(c => c.slice(1, 3)), [[-0.5, 0]], 'space released, button released, axis held');
});
