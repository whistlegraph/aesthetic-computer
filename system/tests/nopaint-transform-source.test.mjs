import assert from "node:assert/strict";
import test from "node:test";
import { cloneAcceptedPaintingPixels } from
  "../public/aesthetic.computer/disks/nopaint.mjs";

test("pixel transforms clone only disk.mjs's isolated accepted painting", () => {
  const accepted = new Uint8ClampedArray([
    1, 2, 3, 255,
    4, 5, 6, 255,
  ]);
  const interfacePixels = new Uint8ClampedArray(accepted.length).fill(220);
  const system = {
    painting: { width: 2, height: 1, pixels: accepted },
    nopaint: {
      buffer: { width: 2, height: 1, pixels: interfacePixels },
      piece: { composite: { width: 2, height: 1, pixels: interfacePixels } },
    },
  };

  const source = cloneAcceptedPaintingPixels(system, 2, 1);
  assert.deepEqual(source, accepted);
  assert.notDeepEqual(source, interfacePixels);
  source[0] = 99;
  assert.equal(accepted[0], 1, "transform input cannot mutate the accepted painting");
});

test("pixel transforms reject a mismatched painting instead of sampling another surface", () => {
  assert.throws(
    () => cloneAcceptedPaintingPixels({
      painting: { width: 1, height: 1, pixels: new Uint8ClampedArray(4) },
    }, 2, 1),
    /accepted substrate mismatch/,
  );
});
