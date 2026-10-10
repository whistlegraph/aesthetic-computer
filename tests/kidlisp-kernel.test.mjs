import test from 'node:test';
import assert from 'node:assert/strict';
import {KidLisp} from '../system/public/aesthetic.computer/lib/kidlisp.mjs';
import {compileProgram} from '../system/public/aesthetic.computer/lib/kidlisp-compile.mjs';
import {compileKernel, runKernel, instantiateKernelWasm, emitKernelWGSL, kernelOperationNames} from '../system/public/aesthetic.computer/lib/kidlisp-kernel.mjs';

const parse = source => new KidLisp().parse(source);
const PROJECT = `(kernel project (in hx hy) (uniform ssc zz cT sT cP sP foc hcx hcy) (out px py)
  (def xs (* hx ssc)) (def ys (* hy ssc))
  (def xr (+ (* xs cT) (* zz sT))) (def z0 (- (* zz cT) (* xs sT)))
  (def yr (- (* ys cP) (* z0 sP))) (def zr (+ (* ys sP) (* z0 cP)))
  (def ff (/ foc (- foc zr)))
  (set px (+ hcx (* xr ff))) (set py (if (> ff 0) (+ hcy (* yr ff)) else (- 0 (sqrt (abs yr))))))`;

test('a kernel body is the subset and nothing else', () => {
  const plan = compileKernel(parse(PROJECT)[0]);
  assert.deepEqual([plan.inputs, plan.uniforms, plan.outputs], [['hx', 'hy'], ['ssc', 'zz', 'cT', 'sT', 'cP', 'sP', 'foc', 'hcx', 'hcy'], ['px', 'py']]);
  assert.deepEqual(plan.locals, ['xs', 'ys', 'xr', 'z0', 'yr', 'zr', 'ff']);
  for (const bad of [
    '(kernel k (in a) (out b) (set b (box a 1 1 1)))',
    '(kernel k (in a) (out b) (set b (random 3)))',
    '(kernel k (in a) (out b) (set b (frame)))',
    '(kernel k (in a) (out b) (set b (myfn a)))',
    '(kernel k (in a) (out b) (set b (spawn p (x 1))))',
    '(kernel k (in a) (out b) (set c a))',
    '(kernel k (in a) (out b) (def a 2) (set b a))',
    '(kernel k (in a) (out b) (set b (if (> a 1) 1 2)))',
    '(kernel k (in a) (out b) (set b unknownName))',
    '(kernel k (in a) (set b a))',
  ]) assert.throws(() => compileKernel(parse(bad)[0]), /UNSUPPORTED_KERNEL|outside kernel-v1|not an input|already|no outputs|kernel if|not an output/, bad);
  assert.ok(kernelOperationNames().includes('atan2') && kernelOperationNames().includes('select'));
});

test('the JavaScript runner and the Wasm function give the same numbers, edge values included', () => {
  const plan = compileKernel(parse(PROJECT)[0]);
  const stride = 13, values = [0, -0, 1, -1, 2.5, -7.25, 1e-9, 1e9, 0.1];
  const rows = [];
  for (const hx of values) for (const hy of values) rows.push([hx, hy, 0.8, 3, 0.9, 0.43, 0.95, 0.31, 160, 195, 218, 0, 0]);
  const a = new Float64Array(rows.flat()), b = a.slice(), uniforms = rows[0].slice(2, 11);
  runKernel(plan, a, rows.length);
  const wasm = instantiateKernelWasm(plan); wasm.run(b, rows.length, uniforms);
  for (let r = 0; r < rows.length; r++) for (const i of [11, 12]) assert.ok(Object.is(a[r * stride + i], b[r * stride + i]), `row ${r} out ${i}: js ${a[r * stride + i]} wasm ${b[r * stride + i]}`);
  // every operation, two ways
  const names = kernelOperationNames().filter(n => !['select', 'mod', 'mul'].includes(n));
  for (const name of names) {
    const arity = {'+': 2, '-': 2, '*': 2, '/': 2, '%': 2, min: 2, max: 2, pow: 2, atan2: 2, hypot: 2, '>': 2, '<': 2, '=': 2, clamp: 3}[name] ?? 1;
    const ins = ['a', 'b', 'c'].slice(0, arity);
    const p = compileKernel(parse(`(kernel k (in ${ins.join(' ')}) (out o) (set o (${name} ${ins.join(' ')})))`)[0]);
    const w = instantiateKernelWasm(p);
    const probe = [[1.5, -2, 0.5], [-0, 0, 1], [3, 0, 2], [-4.5, 4.5, -1], [0.3, 0.7, 0.2]];
    for (const vals of probe) {
      const r1 = new Float64Array([...vals.slice(0, arity), 0]), r2 = r1.slice();
      runKernel(p, r1, 1); w.run(r2, 1, []);
      assert.ok(Object.is(r1[arity], r2[arity]) || (Number.isNaN(r1[arity]) && Number.isNaN(r2[arity])), `${name}(${vals.slice(0, arity)}) js ${r1[arity]} wasm ${r2[arity]}`);
    }
  }
});

test('WGSL comes out of the same plan', () => {
  const text = emitKernelWGSL(compileKernel(parse(PROJECT)[0]));
  assert.match(text, /@compute @workgroup_size\(64\)/);
  assert.match(text, /let hx = rows\[base \+ 0u\];/);
  assert.match(text, /let ssc = u\.v\[0u\];/);
  assert.match(text, /px = \(hcx \+ \(xr \* ff\)\);/);
  assert.match(text, /select\(/);
  assert.match(text, /rows\[base \+ 11u\] = px;/);
});

test('run applies a kernel over a pool, in the compiler and in the interpreter alike', () => {
  const source = `(pool pts 8 hx hy)
(pool out 8 px py)
(once (repeat 6 ii (spawn pts (hx (- ii 3)) (hy (* ii 2))) (spawn out (px 0) (py 0))))
(def ssc 0.8) (def zz 3) (def cT 0.9) (def sT 0.43) (def cP 0.95) (def sP 0.31) (def foc 160) (def hcx 195) (def hcy 218)
${PROJECT}
(run project pts out)
(each out (box px py 1 1))`;
  const streams = {};
  for (const mode of ['interpreted', 'compiled']) {
    const calls = [];
    const api = {screen: {width: 64, height: 64}, params: [], colon: [], system: {fps: 60}, paintCount: 0, needsPaint() {}, fps() {}, toggleHUD() {}, page() {}, unmask() {}, inkrn: () => [0, 0, 0, 255], clock: {time: () => new Date(0)}, sound: {},
      wipe() {}, ink() { return api; }, line() {}, box: (...a) => calls.push(a.slice(0, 2).map(v => +v.toFixed(5))), circle() {}, oval() {}, tri() {}, shape() {}, write() {}};
    console.log = () => {};
    const lisp = new KidLisp(); const mod = lisp.module(source, true); lisp.setAPI(api); mod.boot(api);
    if (mode === 'compiled') lisp.compiled = compileProgram(lisp.ast, lisp);
    mod.paint(api); mod.paint(api);
    streams[mode] = calls;
  }
  assert.equal(streams.compiled.length, 12);
  assert.deepEqual(streams.compiled, streams.interpreted);
  assert.notDeepEqual(streams.compiled[0], [0, 0], 'outputs were written');
});

test('the emitted JavaScript gives the stack runner\'s numbers, edge values included', async () => {
  const { instantiateKernelJS } = await import('../system/public/aesthetic.computer/lib/kidlisp-kernel.mjs');
  const plan = compileKernel(parse(PROJECT)[0]);
  const stride = 13, values = [0, -0, 1, -1, 2.5, -7.25, 1e-9, 1e9, 0.1];
  const rows = [];
  for (const hx of values) for (const hy of values) rows.push([hx, hy, 0.8, 3, 0.9, 0.43, 0.95, 0.31, 160, 195, 218, 0, 0]);
  const a = new Float64Array(rows.flat()), b = a.slice(), uniforms = rows[0].slice(2, 11);
  runKernel(plan, a, rows.length);
  instantiateKernelJS(plan).run(b, rows.length, uniforms);
  for (let r = 0; r < rows.length; r++) for (const i of [11, 12]) assert.ok(Object.is(a[r * stride + i], b[r * stride + i]), `row ${r} out ${i}: runner ${a[r * stride + i]} js ${b[r * stride + i]}`);
  const names = kernelOperationNames().filter(n => !['mod', 'mul'].includes(n));
  for (const name of names) {
    const arity = {'+': 2, '-': 2, '*': 2, '/': 2, '%': 2, min: 2, max: 2, pow: 2, atan2: 2, hypot: 2, '>': 2, '<': 2, '=': 2, clamp: 3, select: 3}[name] ?? 1;
    const ins = ['a', 'b', 'c'].slice(0, arity);
    const p = compileKernel(parse(`(kernel k (in ${ins.join(' ')}) (out o) (set o (${name} ${ins.join(' ')})))`)[0]);
    const js = instantiateKernelJS(p);
    for (const vals of [[1.5, -2, 0.5], [-0, 0, 1], [3, 0, 2], [-4.5, 4.5, -1], [0.3, 0.7, 0.2], [0, -0, -0]]) {
      const r1 = new Float64Array([...vals.slice(0, arity), 0]), r2 = r1.slice();
      runKernel(p, r1, 1); js.run(r2, 1, []);
      assert.ok(Object.is(r1[arity], r2[arity]) || (Number.isNaN(r1[arity]) && Number.isNaN(r2[arity])), `${name}(${vals.slice(0, arity)}) runner ${r1[arity]} js ${r2[arity]}`);
    }
  }
});
