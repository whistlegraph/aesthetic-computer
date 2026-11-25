// fillBuffer paints into an existing buffer exactly as makeBuffer paints a
// new one, and puts the outer buffer and colour back — embedded kidlisp()
// regions reuse two buffers per region on the strength of this.
import assert from "node:assert/strict";
import test from "node:test";

globalThis.ImageData ??= class {
  constructor(width, height) {
    Object.assign(this, { width, height, data: new Uint8ClampedArray(width * height * 4) });
  }
};
const g = await import("../system/public/aesthetic.computer/lib/graph.mjs");

const fill = () => {
  g.color(255, 0, 0); g.box(2, 2, 10, 6);
  g.color(0, 200, 100, 128); g.box(5, 4, 12, 7);
  g.color(9, 9, 255); g.line(0, 11, 19, 0);
};
const api = () => ({ screen: { width: 20, height: 12 } });

test("a reused, cleared buffer paints exactly like a new one", () => {
  const fresh = g.makeBuffer(20, 12, fill, null, api());
  const reused = g.makeBuffer(20, 12, () => { g.color(1, 2, 3); g.box(0, 0, 20, 12); }, null, api());
  reused.pixels.fill(0);
  g.fillBuffer(reused, fill, null, api());
  assert.ok(fresh.pixels.some((v) => v), "the fill drew something");
  assert.deepEqual(Array.from(reused.pixels), Array.from(fresh.pixels));
});

test("the outer buffer and colour come back after a fill", () => {
  const outer = g.makeBuffer(8, 8);
  g.setBuffer(outer);
  g.color(7, 8, 9);
  g.fillBuffer(g.makeBuffer(20, 12), fill, null, api());
  assert.equal(g.getBuffer().pixels, outer.pixels);
  assert.deepEqual(Array.from(g.c.slice(0, 3)), [7, 8, 9]);
});
