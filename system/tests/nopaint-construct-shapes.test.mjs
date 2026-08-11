import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  SHAPE,
  ellipseProposal,
  triangleProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-shapes.mjs";
import {
  RAINBOW,
  rainbowProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-rainbow.mjs";

const base = Object.freeze({
  color: Object.freeze([20, 40, 60, 128]), x: 10, y: 20, w: 120, h: 80,
  drift: 4, thickness: 2, points: Object.freeze([]), phase: 0,
});
const make = (contract, seed) =>
  contract.generate({ random: seededRandom(seed), width: 256, height: 256, base });

test("Triangle and Ellipse share the recovered shadow and shake", () => {
  assert.deepEqual(SHAPE.shadow.channel, [0, 45]);
  assert.deepEqual(SHAPE.shadow.alpha, [10, 60]);
  assert.equal(SHAPE.shakeSeconds, 1);
  assert.deepEqual(SHAPE.jitter, [-1, 0, 1]);
  assert.equal(SHAPE.jitterCue, "common - jitter");
  assert.equal(SHAPE.minimumSize, 3, "Ellipse's width and height floor at three");
  for (const contract of [triangleProposal, ellipseProposal]) {
    for (let seed = 0; seed < 100; seed += 1) {
      const score = make(contract, `${contract.slug}:${seed}`);
      const [r, g, b, alpha] = score.shadow;
      for (const channel of [r, g, b]) {
        assert.ok(channel >= 0 && channel <= 45, `${channel} is inside 0..45`);
      }
      assert.ok(alpha >= 10 && alpha <= 60, `${alpha} is inside 10..60`);
    }
  }
});

test("both shapes are deterministic and draw a shadow under themselves", () => {
  for (const contract of [triangleProposal, ellipseProposal]) {
    const score = make(contract, contract.slug);
    assert.deepEqual(score, make(contract, contract.slug));
    const inks = [];
    const ink = (...color) => {
      inks.push(color.length === 1 ? color[0] : color);
      // Construct fills both passes, so Triangle uses `shape`, not `poly`.
      return { shape() {}, oval() {} };
    };
    contract.render({ ink }, score, 0);
    assert.deepEqual(inks[0], score.shadow, `${contract.slug} lays its shadow first`);
    assert.deepEqual(inks[1], score.color, `${contract.slug} draws over it`);
    assert.equal(SHAPE.shadowOffset, 1, "TriangleRender offsets the shadow by one");
  }
});

test("the shake moves every coordinate by one step a second", () => {
  const score = make(triangleProposal, "triangle:shake");
  const corners = (tick) => {
    const drawn = [];
    triangleProposal.render({
      ink: () => ({ shape: (points) => drawn.push(points) }),
    }, score, tick);
    return drawn[1]; // The shape itself, not its shadow.
  };
  const start = corners(0);
  const held = corners(59);
  assert.deepEqual(start, held, "nothing moves inside the first second");
  const shaken = corners(60);
  assert.notDeepEqual(shaken, start, "the first shake lands on the second");
  // choose(-1, 0, 1) — no coordinate may travel further than one step a shake.
  shaken.forEach(([x, y], index) => {
    assert.ok(Math.abs(x - start[index][0]) <= 1);
    assert.ok(Math.abs(y - start[index][1]) <= 1);
  });
  const after = corners(60 * 5);
  after.forEach(([x, y], index) => {
    assert.ok(Math.abs(x - start[index][0]) <= 5, "five shakes, at most five steps");
    assert.ok(Math.abs(y - start[index][1]) <= 5);
  });
});

test("Rainbow is a pixel transform, not a brush", () => {
  assert.equal(rainbowProposal.kind, "pixel-transform");
  assert.equal(typeof rainbowProposal.applyPixels, "function");
  assert.equal(rainbowProposal.render, undefined, "it draws nothing of its own");
  assert.equal(RAINBOW.shiftSeconds, .1);
  assert.deepEqual(RAINBOW.bounds, [-100, 100]);
  assert.equal(RAINBOW.cue, "rainbow - theme");
  assert.equal(RAINBOW.cueVolume, -5);
  assert.equal(RAINBOW.effect, "AdjustHSL");
});

test("Rainbow rotates hue while holding lightness and alpha", () => {
  const score = make(rainbowProposal, "rainbow");
  assert.deepEqual(score, make(rainbowProposal, "rainbow"));
  // Pure red, mid grey, and a transparent pixel.
  const pixels = new Uint8ClampedArray([255, 0, 0, 255, 128, 128, 128, 255, 9, 9, 9, 0]);
  const turned = rainbowProposal.applyPixels(pixels, 3, 1, { shift: 25 });

  assert.notDeepEqual([...turned.slice(0, 3)], [255, 0, 0], "red moves round the wheel");
  const before = Math.max(...pixels.slice(0, 3)) + Math.min(...pixels.slice(0, 3));
  const after = Math.max(...turned.slice(0, 3)) + Math.min(...turned.slice(0, 3));
  assert.ok(Math.abs(before - after) <= 2, "lightness is held");
  assert.equal(turned[3], 255, "alpha is untouched");

  assert.deepEqual([...turned.slice(4, 7)], [128, 128, 128], "grey has no hue to turn");
  assert.deepEqual([...turned.slice(8)], [9, 9, 9, 0], "transparent pixels are skipped");

  // A full turn is a round trip.
  const whole = rainbowProposal.applyPixels(pixels, 3, 1, { shift: 100 });
  whole.slice(0, 3).forEach((channel, index) => {
    assert.ok(Math.abs(channel - pixels[index]) <= 2, "100 is all the way round");
  });
});

test("No Paint 3 resolves each new slug to its own piece", async () => {
  const { COMPATIBLE_BRUSHES } = await import(
    "../public/aesthetic.computer/disks/nopaint.mjs");
  for (const slug of ["triangle", "ellipse", "rainbow", "grid-worm"]) {
    const piece = await import(`../public/aesthetic.computer/disks/${slug}.mjs`);
    assert.equal(piece.system, "nopaint", `${slug} is a nopaint piece`);
    assert.equal(piece.nopaintProposal.slug, slug);
    assert.equal(COMPATIBLE_BRUSHES.get(slug), piece.nopaintProposal,
      `${slug} resolves to its own module`);
  }
});
