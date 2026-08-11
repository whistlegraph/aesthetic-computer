import test from "node:test";
import assert from "node:assert/strict";
import {
  canvasFor,
  createNoPaintCanvas,
  seededStream,
  softMask,
} from "../public/aesthetic.computer/lib/nopaint-canvas.mjs";

const RED = Object.freeze([255, 0, 0]);
const BLUE = Object.freeze([0, 0, 255]);
const alphaAt = (canvas, x, y) => canvas.pixels[(y * canvas.width + x) * 4 + 3];
const rgbAt = (canvas, x, y) => {
  const at = (y * canvas.width + x) * 4;
  return [canvas.pixels[at], canvas.pixels[at + 1], canvas.pixels[at + 2]];
};

test("a canvas is a plain buffer paste can take", () => {
  const canvas = createNoPaintCanvas(8, 4);
  assert.equal(canvas.width, 8);
  assert.equal(canvas.height, 4);
  assert.equal(canvas.pixels.length, 8 * 4 * 4);
  assert.ok(canvas.pixels instanceof Uint8ClampedArray);
  assert.equal(canvas.painted(), 0, "it starts empty");
});

test("box, disc, and poly lay opaque paint and clip to the canvas", () => {
  const canvas = createNoPaintCanvas(16, 16);
  canvas.box(2, 2, 4, 4, RED);
  assert.equal(alphaAt(canvas, 3, 3), 255);
  assert.deepEqual(rgbAt(canvas, 3, 3), [255, 0, 0]);
  assert.equal(alphaAt(canvas, 8, 8), 0, "nothing outside the box");

  // Far outside the canvas — must clip rather than throw or wrap.
  canvas.box(-40, -40, 4, 4, RED);
  canvas.box(400, 400, 4, 4, RED);
  canvas.disc(-20, -20, 3, RED);
  canvas.poly([[-5, -5], [-1, -5], [-1, -1], [-5, -1]], RED);
  assert.equal(alphaAt(canvas, 0, 0), 0, "off-canvas work stays off");

  const filled = createNoPaintCanvas(16, 16);
  filled.disc(8, 8, 4, BLUE);
  assert.equal(alphaAt(filled, 8, 8), 255);
  assert.equal(alphaAt(filled, 8, 15), 0, "the disc has a radius");
});

test("erase takes paint away — Construct's blend mode 7", () => {
  const canvas = createNoPaintCanvas(16, 16);
  canvas.disc(8, 8, 6, RED);
  const before = canvas.painted();
  assert.ok(before > 0);
  canvas.erase(8, 8, 3);
  assert.equal(alphaAt(canvas, 8, 8), 0, "the bite is gone");
  assert.ok(canvas.painted() < before, "erasing removes rather than covers");
  assert.ok(canvas.painted() > 0, "and only where it bit");
});

test("soft ramps from its core to nothing at the rim, and inverts", () => {
  const canvas = createNoPaintCanvas(64, 64);
  canvas.soft(32, 32, 20, 5, RED);
  assert.equal(alphaAt(canvas, 32, 32), 255, "solid inside the hardness");
  assert.equal(alphaAt(canvas, 32, 32 - 25), 0, "nothing past the rim");
  let previous = 256;
  for (let offset = 5; offset <= 20; offset += 1) {
    const alpha = alphaAt(canvas, 32, 32 - offset);
    assert.ok(alpha <= previous, `alpha falls at ${offset}`);
    previous = alpha;
  }

  // Vignette closes in instead of opening up.
  const closing = createNoPaintCanvas(64, 64);
  closing.soft(32, 32, 20, 5, RED, { invert: true });
  assert.equal(alphaAt(closing, 32, 32), 0, "clear at the centre");
  assert.ok(alphaAt(closing, 32, 32 - 25) > 0, "and covered past the rim");
});

test("the strongest blend keeps a bloom one bloom", () => {
  const stacked = createNoPaintCanvas(32, 32);
  const kept = createNoPaintCanvas(32, 32);
  for (let pass = 0; pass < 6; pass += 1) {
    stacked.soft(16, 16, 10, 0, RED, { peak: 60 });
    kept.soft(16, 16, 10, 0, RED, { peak: 60, blend: "strongest" });
  }
  assert.ok(alphaAt(stacked, 16, 16) > 60, "source-over accumulates");
  assert.equal(alphaAt(kept, 16, 16), 60, "strongest does not");
});

test("a soft mask is Construct's falloff, reusable", () => {
  const mask = softMask(8, 2);
  assert.equal(mask.size, 16);
  assert.equal(mask.alpha[8 * 16 + 8], 255, "opaque at the centre");
  assert.equal(mask.alpha[0], 0, "empty at the corner");

  const canvas = createNoPaintCanvas(32, 32);
  canvas.stamp(mask, 16, 16, BLUE);
  assert.deepEqual(rgbAt(canvas, 16, 16), [0, 0, 255]);
  assert.equal(alphaAt(canvas, 16, 16), 255);
  // Stamping the same mask twice in a row must not shift it.
  const first = canvas.painted();
  canvas.stamp(mask, 16, 16, BLUE);
  assert.equal(canvas.painted(), first);
});

test("a canvas is held per score and advanced, not rebuilt", () => {
  const score = Object.freeze({ width: 16, height: 16 });
  let builds = 0;
  const build = () => { builds += 1; };
  const first = canvasFor(score, build);
  const again = canvasFor(score, build);
  assert.equal(builds, 1, "the canvas is built once for a score");
  assert.equal(first, again);
  assert.equal(first.canvas, again.canvas);
  assert.equal(first.placed, 0, "and carries how far the clock has reached");

  // A different score gets its own.
  const other = canvasFor(Object.freeze({ width: 16, height: 16 }), build);
  assert.equal(builds, 2);
  assert.notEqual(other.canvas, first.canvas);
});

test("a seeded stream replays from a score alone", () => {
  const a = seededStream(1234);
  const b = seededStream(1234);
  const drawn = Array.from({ length: 8 }, () => a());
  assert.deepEqual(drawn, Array.from({ length: 8 }, () => b()));
  assert.ok(drawn.every((value) => value >= 0 && value < 1));
  assert.notDeepEqual(drawn, Array.from({ length: 8 }, seededStream(5678)));
});
