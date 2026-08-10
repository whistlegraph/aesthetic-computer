import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  SOFTY,
  softyCueRate,
  softyProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-softy.mjs";

const base = Object.freeze({
  color: Object.freeze([20, 40, 60, 128]), x: 10, y: 20, w: 120, h: 80,
  drift: 4, thickness: 2, points: Object.freeze([]), phase: 0,
});
const make = (seed = "softy", width = 256, height = 256) =>
  softyProposal.generate({ random: seededRandom(seed), width, height, base });

test("Softy keeps the constants read out of the expression table", () => {
  // Weighted tier pick S:5 M:1 L:2, radius bands, and per-tier turn levels.
  assert.deepEqual(SOFTY.tiers.map((t) => [t.name, t.weight, t.radius, t.turn]), [
    ["S", 5, [6, 16], [1, 2]],
    ["M", 1, [16, 48], [2, 3]],
    ["L", 2, [48, 64], [4, 5, 6]],
  ]);
  assert.deepEqual(SOFTY.speeds, [.5, 1, 1.5]);
  assert.deepEqual(SOFTY.turnSpeeds, [16, 30, 45]);
  assert.deepEqual(SOFTY.hueDirections, [-1, 1]);
  assert.equal(SOFTY.colorSeconds, .1);
  assert.equal(SOFTY.moveSeconds, .1);
  assert.equal(SOFTY.turnSeconds, .2);
  assert.equal(SOFTY.reheadOdds, 10);
  assert.equal(SOFTY.minHardnessGap, 6);
  assert.equal(SOFTY.stepDivisor, 4);
  assert.equal(SOFTY.cue, "softy - landed");
  // The blurp pitches down as the brush grows: 1.8 - radius / 64.
  assert.equal(softyCueRate(64), 0.8);
  assert.equal(softyCueRate(6), 1.8 - 6 / 64);
});

test("Softy generates deterministically inside every recovered range", () => {
  assert.deepEqual(make(), make());
  const tiers = new Set();
  for (let seed = 0; seed < 300; seed += 1) {
    const score = make(`softy:${seed}`);
    const tier = SOFTY.tiers.find((entry) => entry.name === score.tier);
    tiers.add(score.tier);
    assert.ok(tier, `${score.tier} is a real tier`);
    assert.ok(score.radius >= tier.radius[0] && score.radius <= tier.radius[1],
      `radius ${score.radius} is inside ${tier.name}`);
    assert.ok(tier.turn.includes(score.turnLevel));
    assert.ok(SOFTY.speeds.includes(score.speed));
    assert.ok(SOFTY.turnSpeeds.includes(score.turnSpeed));
    assert.ok(SOFTY.hueDirections.includes(score.hueDirection));
    // hardness = round(random(0, radius - 6)), step = max(1, (r - h) / 4).
    assert.ok(score.hardness >= 0
      && score.hardness <= Math.max(0, score.radius - SOFTY.minHardnessGap));
    assert.equal(score.step,
      Math.max(1, (score.radius - score.hardness) / SOFTY.stepDivisor));
    assert.ok(score.startAngle >= 0 && score.startAngle < 360);
  }
  assert.deepEqual([...tiers].sort(), ["L", "M", "S"], "every tier is reachable");
});

test("the weighted picker favours S the way the original list did", () => {
  const counts = { S: 0, M: 0, L: 0 };
  for (let seed = 0; seed < 3000; seed += 1) counts[make(`tier:${seed}`).tier] += 1;
  // S:5 M:1 L:2 out of 8. Loose bounds — this asserts the ordering, not the RNG.
  assert.ok(counts.S > counts.L && counts.L > counts.M,
    `S ${counts.S} > L ${counts.L} > M ${counts.M}`);
});

test("Softy accumulates its stroke instead of redrawing it", () => {
  const score = make("softy:paint", 128, 128);
  const pasted = [];
  const render = (tick) => {
    pasted.length = 0;
    softyProposal.render({ paste: (...args) => pasted.push(args) }, score, tick);
    return pasted;
  };
  const [[layer]] = render(0);
  assert.equal(layer.width, 128);
  assert.equal(layer.height, 128);
  assert.equal(pasted.length, 1, "one paste per frame, whatever the stroke length");

  const painted = () => layer.pixels.reduce(
    (total, value, index) => index % 4 === 3 && value > 0 ? total + 1 : total, 0);
  const early = painted();
  assert.ok(early > 0, "the first stamp lands immediately");
  render(600);
  const later = painted();
  assert.ok(later > early, `the stroke grows (${early} → ${later})`);

  // The walk is capped, so a much later frame adds nothing more.
  render(60 * 60);
  const settled = painted();
  render(60 * 600);
  assert.equal(painted(), settled, "the stroke ends rather than running forever");
});

test("the soft falloff is opaque at the core and empty at the rim", () => {
  const score = Object.freeze({ ...make("softy:falloff", 128, 128), x: 64, y: 64 });
  const pasted = [];
  softyProposal.render({ paste: (...args) => pasted.push(args) }, score, 0);
  const [layer] = pasted[0];
  const alphaAt = (x, y) => layer.pixels[(y * layer.width + x) * 4 + 3];
  const radius = score.radius * score.scale;
  const hardness = score.hardness * score.scale;
  assert.equal(alphaAt(64, 64), 255, "the core is solid");
  if (hardness >= 1) assert.equal(alphaAt(64, Math.round(64 - hardness + 1)), 255);
  assert.equal(alphaAt(64, Math.round(64 - radius - 2)), 0, "nothing past the rim");
  // Alpha falls monotonically from the core out to the rim.
  let previous = 256;
  for (let offset = Math.ceil(hardness); offset < radius; offset += 1) {
    const alpha = alphaAt(64, Math.round(64 - offset));
    assert.ok(alpha <= previous, `alpha falls at ${offset} (${alpha} > ${previous})`);
    previous = alpha;
  }
});

test("No Paint 3 resolves softy to the standalone piece's contract", async () => {
  const { COMPATIBLE_BRUSHES } = await import(
    "../public/aesthetic.computer/disks/nopaint.mjs");
  const piece = await import("../public/aesthetic.computer/disks/softy.mjs");
  assert.equal(piece.system, "nopaint");
  assert.equal(COMPATIBLE_BRUSHES.get("softy"), piece.nopaintProposal);
  assert.equal(piece.nopaintProposal, softyProposal);
});
