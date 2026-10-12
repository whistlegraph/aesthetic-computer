// Translucent fills take a fast path in `lineh`; it must match `blend` exactly.
import assert from "node:assert/strict";
import test from "node:test";

globalThis.ImageData ??= class {
  constructor(width, height) {
    Object.assign(this, { width, height, data: new Uint8ClampedArray(width * height * 4) });
  }
};
const graph = await import("../system/public/aesthetic.computer/lib/graph.mjs");

// `blend` from graph.mjs, per pixel, as the reference.
function reference(dst, i, [r, g, b, a]) {
  if (dst[i + 3] < 255) {
    const s = a / 255, d = dst[i + 3] / 255, out = s + (1 - s) * d;
    [r, g, b].forEach((v, k) => { dst[i + k] = (v * s + dst[i + k] * (1 - s) * d) / (out + 1e-10); });
    dst[i + 3] = out * 255;
  } else {
    const alpha = a + 1, inv = 256 - alpha;
    [r, g, b].forEach((v, k) => { dst[i + k] = (alpha * v + inv * dst[i + k]) >> 8; });
  }
}

test("translucent box blends like blend over opaque and see-through pixels", () => {
  const width = 16, height = 4;
  const pixels = new Uint8ClampedArray(width * height * 4);
  for (let i = 0; i < pixels.length; i += 4) {
    pixels.set([(i * 7) % 256, (i * 13) % 256, (i * 29) % 256, i % 12 === 0 ? 90 : 255], i);
  }
  const expected = pixels.slice();
  for (let i = 0; i < expected.length; i += 4) reference(expected, i, [200, 40, 120, 77]);

  graph.setBuffer({ width, height, pixels });
  graph.color(200, 40, 120, 77);
  graph.box(0, 0, width, height);
  assert.deepEqual([...pixels], [...expected]);
});
