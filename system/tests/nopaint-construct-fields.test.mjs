import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  AURA,
  VIGNETTE,
  auraProposal,
  vignetteProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-vignette.mjs";
import {
  BREATHE,
  breatheProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-breathe.mjs";

const base = Object.freeze({
  color: Object.freeze([20, 40, 60, 128]), x: 10, y: 20, w: 120, h: 80,
  drift: 4, thickness: 2, points: Object.freeze([]), phase: 0,
});
const make = (contract, seed, width = 256, height = 256) =>
  contract.generate({ random: seededRandom(seed), width, height, base });

test("Breathe keeps the recovered size list and timers", () => {
  assert.deepEqual(BREATHE.sizes, [64, 96, 128, 196, 256]);
  assert.equal(BREATHE.expandSeconds, .04);
  assert.equal(BREATHE.cycleSeconds, 6);
  assert.equal(BREATHE.effect, "Bulge");
  assert.equal(BREATHE.cue, "breathe - theme");
  // Volume is -5 - (5 - size / 256 * 5): a bigger breath is a louder one.
  assert.ok(BREATHE.cueVolume(256) > BREATHE.cueVolume(64));
  assert.equal(BREATHE.cueVolume(256), -5);
});

test("Breathe is a pixel transform that swells without inventing pixels", () => {
  assert.equal(breatheProposal.kind, "pixel-transform");
  assert.equal(breatheProposal.render, undefined);
  const score = make(breatheProposal, "breathe");
  assert.deepEqual(score, make(breatheProposal, "breathe"));
  assert.ok(BREATHE.sizes.includes(score.brush.parameters.size));

  // A 9×9 field with one marked pixel at the centre; the bulge must magnify it.
  const width = 9, height = 9;
  const pixels = new Uint8ClampedArray(width * height * 4);
  for (let i = 0; i < width * height; i += 1) {
    pixels[i * 4 + 3] = 255;
    pixels[i * 4] = 10;
  }
  const middle = (4 * width + 4) * 4;
  pixels[middle] = 250;
  const swelled = breatheProposal.applyPixels(pixels, width, height,
    { radius: 4, magnitude: .9, x: 4, y: 4 });
  const bright = [...swelled].filter((_, i) => i % 4 === 0).filter((v) => v === 250).length;
  assert.ok(bright > 1, `the centre pixel is magnified (${bright} pixels carry it)`);
  // Every value must have come from the source; a resample invents nothing.
  const sourceValues = new Set([...pixels].filter((_, i) => i % 4 === 0));
  [...swelled].filter((_, i) => i % 4 === 0).forEach((value) =>
    assert.ok(sourceValues.has(value), `${value} was resampled, not invented`));
});

test("Vignette keeps its own size list and both lightness ranges", () => {
  assert.deepEqual(VIGNETTE.sizes, [16, 32, 64, 96, 128]);
  assert.deepEqual(VIGNETTE.darkLightness, [.05, .2]);
  assert.deepEqual(VIGNETTE.lightLightness, [.7, .9]);
  assert.deepEqual(VIGNETTE.dark, [0, 1]);
  assert.deepEqual(VIGNETTE.drift, [-3, 3]);
  assert.equal(VIGNETTE.cue, "vignette - theme");

  const seen = { dark: 0, light: 0 };
  for (let seed = 0; seed < 200; seed += 1) {
    const score = make(vignetteProposal, `vignette:${seed}`);
    assert.ok(VIGNETTE.sizes.includes(score.size));
    assert.ok(VIGNETTE.drift.includes(score.drift));
    assert.ok(score.angle >= 0 && score.angle < 360);
    // hardness sits between half the radius and two short of it.
    assert.ok(score.hardness >= score.radius / 2 - 1 && score.hardness <= score.radius);
    seen[score.dark ? "dark" : "light"] += 1;
  }
  assert.ok(seen.dark > 0 && seen.light > 0, "choose(0, 1) reaches both");
});

test("Vignette and Aura lay a soft field that fades outward", () => {
  for (const contract of [vignetteProposal, auraProposal]) {
    const score = make(contract, `${contract.slug}:field`, 128, 128);
    assert.deepEqual(score, make(contract, `${contract.slug}:field`, 128, 128));
    const pasted = [];
    contract.render({ paste: (...args) => pasted.push(args) }, score, 0);
    assert.equal(pasted.length, 1, `${contract.slug} pastes one field`);
    const [layer, x, y] = pasted[0];
    assert.deepEqual([x, y], [0, 0]);
    assert.equal(layer.width, 128);
    const painted = layer.pixels.reduce(
      (total, value, index) => index % 4 === 3 && value > 0 ? total + 1 : total, 0);
    assert.ok(painted > 0, `${contract.slug} paints something`);
    const alphas = new Set();
    for (let index = 3; index < layer.pixels.length; index += 4) {
      if (layer.pixels[index] > 0) alphas.add(layer.pixels[index]);
    }
    assert.ok(alphas.size > 1, `${contract.slug} has a falloff, not a flat disc`);
  }
});

test("Aura keeps the emitter's recovered ranges", () => {
  assert.deepEqual(AURA.spray, [90, 110]);
  assert.deepEqual(AURA.rate, [2, 4]);
  assert.deepEqual(AURA.hue, [0, 100]);
  assert.deepEqual(AURA.saturation, [25, 120]);
  assert.deepEqual(AURA.lightness, [60, 180]);
  assert.equal(AURA.changeSeconds, 1);
  assert.deepEqual(AURA.changeHue, [-100, 100]);
  assert.equal(AURA.cue, "aura - theme");
  assert.deepEqual(AURA.cueVolume, [-100, -7.5]);
  for (let seed = 0; seed < 100; seed += 1) {
    const score = make(auraProposal, `aura:${seed}`);
    assert.ok(score.spray >= AURA.spray[0] && score.spray <= AURA.spray[1]);
    assert.ok(score.rate >= AURA.rate[0] && score.rate <= AURA.rate[1]);
    assert.ok(score.angle >= 0 && score.angle < 360);
  }
});

test("No Paint 3 resolves breathe, vignette, and aura to their own pieces", async () => {
  const { COMPATIBLE_BRUSHES } = await import(
    "../public/aesthetic.computer/disks/nopaint.mjs");
  for (const slug of ["breathe", "vignette", "aura"]) {
    const piece = await import(`../public/aesthetic.computer/disks/${slug}.mjs`);
    assert.equal(piece.system, "nopaint");
    assert.equal(piece.nopaintProposal.slug, slug);
    assert.equal(COMPATIBLE_BRUSHES.get(slug), piece.nopaintProposal);
  }
});
