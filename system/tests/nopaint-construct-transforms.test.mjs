import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import { recoveredConstructTransforms, flipTransform, invertTransform, scrollTransform } from "../public/aesthetic.computer/lib/nopaint-construct-transforms.mjs";

const base = Object.freeze({ kind: "base" });
const generate = (transform, seed = "transform") => transform.generate({ random: seededRandom(seed), width: 4, height: 2, base });
const pixels = new Uint8ClampedArray([
  1,2,3,255, 10,20,30,255, 40,50,60,255, 70,80,90,255,
  4,5,6,255, 11,21,31,255, 41,51,61,255, 71,81,91,255,
]);

test("all recovered transforms generate deterministic, explicit contracts", () => {
  assert.equal(recoveredConstructTransforms.length, 13);
  assert.equal(new Set(recoveredConstructTransforms.map(({ slug }) => slug)).size, 13);
  for (const transform of recoveredConstructTransforms) {
    assert.deepEqual(generate(transform), generate(transform));
    assert.equal(typeof transform.applyPixels, "function");
    assert.match(transform.source.file, /data\.json$/);
  }
});

test("Invert preserves alpha and inverts RGB", () => {
  const output = invertTransform.applyPixels(pixels, 4, 2, { inverted: true });
  assert.deepEqual([...output.slice(0, 4)], [254, 253, 252, 255]);
});

test("Flip performs exact horizontal and vertical replacements", () => {
  const horizontal = flipTransform.applyPixels(pixels, 4, 2, { vertically: false });
  assert.deepEqual([...horizontal.slice(0, 4)], [70, 80, 90, 255]);
  const vertical = flipTransform.applyPixels(pixels, 4, 2, { vertically: true });
  assert.deepEqual([...vertical.slice(0, 4)], [4, 5, 6, 255]);
});

test("Scroll wraps without dropping pixels", () => {
  const output = scrollTransform.applyPixels(pixels, 4, 2, { horizontal: true, offset: 1 });
  assert.deepEqual([...output.slice(4, 8)], [1, 2, 3, 255]);
  assert.equal(output.length, pixels.length);
});
