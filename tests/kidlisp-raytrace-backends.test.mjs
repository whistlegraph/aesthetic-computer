import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { rayKernel, renderRayFrame } from "../kidlisp/benchmarks/raytrace.mjs";
import { createRayWasm } from "../kidlisp/benchmarks/raytrace-wasm.mjs";
import { emitRayPlan } from "../kidlisp/benchmarks/ray-plan.mjs";
import { compareRayGPU } from "../kidlisp/benchmarks/raytrace-gpu.mjs";
import { compileNumeric } from "../system/public/aesthetic.computer/lib/kidlisp-plan.mjs";
const read = name => readFileSync(new URL(`../kidlisp/benchmarks/${name}`, import.meta.url));
const source = read("ray-sphere.lisp").toString();
const bytes = read("raytrace-frame.wasm");

test("frame Wasm artifact matches source/plan, has fixed memory and zero imports", () => {
  const manifest = JSON.parse(read("raytrace-frame.wasm.json"));
  const hash = value => createHash("sha256").update(value).digest("hex");
  assert.equal(hash(bytes), manifest.sha256);
  assert.equal(hash(source), manifest.sourceSha256);
  assert.equal(hash(read("raytrace-frame.c")), manifest.hostSha256);
  assert.equal(hash(emitRayPlan(rayKernel(source).plan, "c")), manifest.kernelSha256);
  const module = new WebAssembly.Module(bytes);
  assert.deepEqual(WebAssembly.Module.imports(module), []);
  const { exports } = new WebAssembly.Instance(module);
  assert.equal(exports.memory.buffer.byteLength, manifest.memoryBytes);
  assert.throws(() => exports.memory.grow(1), RangeError);
  for (const [w,h,b,x] of [[513,1,2,0],[1,0,2,0],[1,1,4,0],[1,1,2,NaN],[1,1,2,99]]) assert.equal(exports.render(w,h,b,x),0);
});

test("whole-frame Wasm matches reference pixels and ray counts across frames and bounce limits", () => {
  const wasm = createRayWasm(bytes), js = rayKernel(source,"direct-js");
  for (const [width,height] of [[1,1],[65,37],[160,90],[320,180]]) {
    for (const [frame,bounces] of [[0,0],[12,1],[48,2],[97,3]]) {
      const controls = {width,height,frame,bounces};
      const expected = renderRayFrame(js,controls), actual = wasm.render(controls);
      assert.deepEqual(actual.rgba,expected.rgba,JSON.stringify(controls));
      assert.equal(actual.rays,expected.rays); assert.equal(actual.intersections,expected.intersections);
    }
  }
  assert.throws(() => wasm.render({width:Infinity}), /controls/);
  assert.throws(() => wasm.render({frame:-1}), /controls/);
});

test("ray shader lowering rejects unsupported arithmetic and preserves an inspectable plan", () => {
  const bindings = ["a","b","c","d","e","f","g"];
  for (const target of ["c","wgsl"]) {
    const plan = compileNumeric(["-",["*","a","b"],["+","c",1]],{bindings});
    const snapshot = JSON.stringify(plan);
    assert.match(emitRayPlan(plan,target),/rayKernel/);
    assert.equal(JSON.stringify(plan),snapshot);
    assert.throws(()=>emitRayPlan(compileNumeric(["/","a",2],{bindings}),target),/does not support/);
    assert.throws(()=>emitRayPlan(compileNumeric(Infinity,{bindings}),target),/finite/);
    assert.match(emitRayPlan(compileNumeric(1e25,{bindings}),target),/1e\+25;/);
    assert.match(emitRayPlan(compileNumeric(-0,{bindings}),target),/-0\.0;/);
    assert.throws(()=>emitRayPlan({...plan,code:Array(513).fill({op:"const",value:0})},target),/512/);
    assert.throws(()=>emitRayPlan({...plan,code:[{op:"input",slot:99}]},target),/slot/);
  }
});

test("GPU comparison tolerates small numeric error but rejects missing geometry and alpha", () => {
  const expected = renderRayFrame(rayKernel(source,"direct-js"),{width:65,height:37}).rgba;
  assert.equal(compareRayGPU(expected,expected).pass,true);
  const small = expected.slice(); small[0]++;
  assert.equal(compareRayGPU(expected,small).pass,true);
  const blank = new Uint8ClampedArray(expected.length); for(let i=3;i<blank.length;i+=4)blank[i]=255;
  assert.equal(compareRayGPU(expected,blank).pass,false);
  const alpha = expected.slice();alpha[3]=0;
  assert.equal(compareRayGPU(expected,alpha).pass,false);
});
