import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { rayKernel, renderRayFrame } from "../kidlisp/benchmarks/raytrace.mjs";
import { comparePixelFrames } from "../kidlisp/conformance/pixels.mjs";

const source = readFileSync(new URL("../kidlisp/benchmarks/ray-sphere.lisp", import.meta.url), "utf8");
test("ray-traced pixels match JS, reference KidLisp, numeric plans and Wasm", () => {
  for (const frame of [0, 12, 48]) {
    const controls = { width: 32, height: 18, frame };
    const expected = renderRayFrame(rayKernel(source, "direct-js"), controls);
    for (const backend of ["reference", "plan", "wasm"]) {
      const actual = renderRayFrame(rayKernel(source, backend), controls);
      assert.deepEqual(comparePixelFrames(expected.rgba, actual.rgba, 32, 18), { equal: true, changedPixels: 0, changedChannels: 0, first: null }, backend);
      assert.equal(expected.intersections, actual.intersections);
    }
  }
});
test("reflection work is measurable and changes the rendered result", () => {
  const kernel = rayKernel(source);
  const a = renderRayFrame(kernel, { width: 32, height: 18, bounces: 0 });
  const b = renderRayFrame(kernel, { width: 32, height: 18, bounces: 2 });
  assert.ok(b.rays > a.rays);
  assert.equal(comparePixelFrames(a.rgba, b.rgba, 32, 18).equal, false);
  assert.throws(() => renderRayFrame(kernel, { width: 10000 }), /controls/);
});
