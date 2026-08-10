import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  WAFER,
  permutation,
  waferProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-wafer.mjs";

const base = Object.freeze({
  color: Object.freeze([120, 90, 60, 128]), x: 10, y: 20, w: 120, h: 80,
  drift: 4, thickness: 2, points: Object.freeze([]), phase: 0,
});
const make = (seed = "wafer", width = 256, height = 256) =>
  waferProposal.generate({ random: seededRandom(seed), width, height, base });

const render = (score, tick) => {
  const pasted = [];
  waferProposal.render({ paste: (...args) => pasted.push(args) }, score, tick);
  assert.equal(pasted.length, 1, "one paste per frame");
  return pasted[0][0];
};
const solid = (layer) => layer.pixels.reduce(
  (total, value, index) => index % 4 === 3 && value > 0 ? total + 1 : total, 0);

test("Wafer keeps the constants read out of the expression table", () => {
  assert.deepEqual(WAFER.radii, [16, 16, 32, 32, 32, 32, 48, 48, 48, 64, 64, 96]);
  assert.equal(WAFER.positions, 12);
  assert.equal(WAFER.arc, 30);
  assert.equal(WAFER.arc * WAFER.positions, 360, "the bites go once around");
  assert.equal(WAFER.bitesPerVisit, 3);
  assert.deepEqual(WAFER.biteRadius, [.2, .5]);
  assert.equal(WAFER.biteAngleJitter, 5);
  assert.equal(WAFER.biteDistanceJitter, 4);
  assert.equal(WAFER.drawSeconds, .5);
  assert.equal(WAFER.bitesBeforeEnlarge, 13);
  assert.equal(WAFER.enlarge, 1.3);
  assert.deepEqual(WAFER.colorJitter, [-5, 0, 5]);
  assert.deepEqual(WAFER.cues.bites, [
    "wafer - nibble bite 1", "wafer - nibble bite 2", "wafer - nibble bite 3"]);
  assert.equal(WAFER.cues.appear, "wafer - nibble appear");
  assert.equal(WAFER.cues.enlarge, "wafer - enlarge");
});

test("the bite order is a permutation of all twelve positions", () => {
  for (let seed = 0; seed < 50; seed += 1) {
    const order = permutation(seededRandom(`wafer:${seed}`), WAFER.positions);
    assert.deepEqual([...order].sort((a, b) => a - b),
      Array.from({ length: WAFER.positions }, (_, index) => index),
      "every rim position is visited exactly once");
  }
  // Shuffled, not walked in order — at least one seed must reorder it.
  const ordered = Array.from({ length: WAFER.positions }, (_, index) => index);
  const shuffled = Array.from({ length: 20 }, (_, seed) =>
    permutation(seededRandom(`shuffle:${seed}`), WAFER.positions));
  assert.ok(shuffled.some((order) => order.join() !== ordered.join()));
});

test("Wafer generates deterministically inside the recovered ranges", () => {
  assert.deepEqual(make(), make());
  const radii = new Set();
  for (let seed = 0; seed < 200; seed += 1) {
    const score = make(`wafer:${seed}`);
    radii.add(score.sourceRadius);
    assert.ok(WAFER.radii.includes(score.sourceRadius));
    // Each channel is the proposal colour plus one of the recovered jitters.
    score.color.forEach((channel, index) => {
      const offsets = WAFER.colorJitter.map((jitter) =>
        Math.max(0, Math.min(255, base.color[index] + jitter)));
      assert.ok(offsets.includes(channel), `channel ${index} is jittered by ±5`);
    });
  }
  assert.deepEqual([...radii].sort((a, b) => a - b), [16, 32, 48, 64, 96]);
});

test("the biscuit appears already nibbled, then loses more as the clock runs", () => {
  const score = make("wafer:bites", 256, 256);
  const layer = render(score, 0);
  const start = solid(layer);
  assert.ok(start > 0, "the biscuit is drawn");

  // CircularBite runs twelve bites before anything is shown, so the disc is
  // never whole: it must be smaller than a plain circle of the same radius.
  const whole = Math.PI * score.radius * score.radius;
  assert.ok(start < whole, `${start} px is less than an unbitten ${Math.round(whole)}`);

  // Timer "Draw" is half a second; 30 frames is one bite.
  render(score, 30 * 6);
  const bitten = solid(layer);
  assert.ok(bitten < start, `bites remove pixels (${start} → ${bitten})`);
});

test("after thirteen bites the biscuit grows and is nibbled fresh", () => {
  const score = make("wafer:enlarge", 256, 256);
  const framesPerBite = 60 * WAFER.drawSeconds;
  const layer = render(score, 0);
  const beforeEnlarge = solid(layer);
  // One tick past the thirteenth bite is the enlarge.
  render(score, framesPerBite * (WAFER.bitesBeforeEnlarge + 1));
  const afterEnlarge = solid(layer);
  assert.ok(afterEnlarge > beforeEnlarge,
    `the biscuit grows by ${WAFER.enlarge} (${beforeEnlarge} → ${afterEnlarge})`);
});

test("the biscuit stops growing instead of swallowing the painting forever", () => {
  const score = make("wafer:cap", 128, 128);
  const layer = render(score, 0);
  render(score, 60 * 60 * 10);
  const settled = solid(layer);
  render(score, 60 * 60 * 60);
  assert.equal(solid(layer), settled, "growth is capped");
  assert.ok(settled <= layer.width * layer.height);
});

test("No Paint 3 resolves wafer to the standalone piece's contract", async () => {
  const { COMPATIBLE_BRUSHES } = await import(
    "../public/aesthetic.computer/disks/nopaint.mjs");
  const piece = await import("../public/aesthetic.computer/disks/wafer.mjs");
  assert.equal(piece.system, "nopaint");
  assert.equal(COMPATIBLE_BRUSHES.get("wafer"), piece.nopaintProposal);
  assert.equal(piece.nopaintProposal, waferProposal);
});
