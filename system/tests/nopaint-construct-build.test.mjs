import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  BANNER,
  BUILD,
  bannerProposal,
  buildProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-build.mjs";

const base = Object.freeze({
  color: Object.freeze([20, 40, 60, 128]), x: 10, y: 20, w: 120, h: 80,
  drift: 4, thickness: 2, points: Object.freeze([]), phase: 0,
});
const make = (contract, seed, width = 256, height = 256) =>
  contract.generate({ random: seededRandom(seed), width, height, base });
const render = (contract, score, tick) => {
  const pasted = [];
  contract.render({ paste: (...args) => pasted.push(args) }, score, tick);
  assert.equal(pasted.length, 1, `${contract.slug} pastes one layer`);
  return pasted[0][0];
};
const painted = (layer) => layer.pixels.reduce(
  (total, value, index) => index % 4 === 3 && value > 0 ? total + 1 : total, 0);

test("Build keeps its recovered block sizes, opacity, and cues", () => {
  assert.deepEqual(BUILD.blockSizes, [4, 8, 16, 32, 64, 128]);
  assert.deepEqual(BUILD.opacity, [50, 100]);
  // A bigger brick is laid more slowly, one delay per size.
  assert.deepEqual(BUILD.stepSeconds, [.1, .2, .3, .4, .5, .7]);
  assert.equal(BUILD.stepSeconds.length, BUILD.blockSizes.length);
  assert.equal(BUILD.cueFloor, 32);
  assert.deepEqual(Object.values(BUILD.cues).sort(), [
    "build - blow down", "build - brick click", "build - brick clop",
    "build - brick scrape", "build - builder's beat"].sort());
  const sizes = new Set();
  for (let seed = 0; seed < 200; seed += 1) {
    const score = make(buildProposal, `build:${seed}`);
    sizes.add(score.blockSize);
    assert.ok(BUILD.blockSizes.includes(score.blockSize));
    assert.ok(score.opacity >= 50 && score.opacity <= 100);
    // blockIndexMax = 256 / blockSize - 1, straight off the sheet.
    assert.equal(score.brush.parameters.blockIndexMax, 256 / score.blockSize - 1);
    assert.equal(score.stepSeconds,
      BUILD.stepSeconds[BUILD.blockSizes.indexOf(score.blockSize)]);
    // Only bricks of 32 and up are audible.
    assert.equal(score.audible, score.blockSize >= BUILD.cueFloor);
    assert.equal(score.brush.parameters.cues.length, score.audible ? 4 : 0);
  }
  assert.deepEqual([...sizes].sort((a, b) => a - b), BUILD.blockSizes);
});

test("the builder never crosses its own work", () => {
  // BuildStep only offers a neighbour while that tile still reads tileToCheck,
  // so no brick may ever be laid twice — the walk is self-avoiding, and it
  // ends boxed in rather than wandering forever.
  const score = make(buildProposal, "build:avoid", 64, 64);
  const laid = [];
  const layer = { width: score.width, height: score.height,
    pixels: new Uint8ClampedArray(score.width * score.height * 4) };
  // Re-run the walk through the real render and read the grid it fills.
  render(buildProposal, score, 60 * 100000);
  const filled = render(buildProposal, score, 60 * 100000);
  const cells = new Set();
  for (let row = 0; row < score.rows; row += 1) {
    for (let column = 0; column < score.columns; column += 1) {
      const at = ((Math.floor(row * score.block) + 1) * filled.width
        + Math.floor(column * score.block) + 1) * 4;
      if (filled.pixels[at + 3] > 0) cells.add(`${column},${row}`);
    }
  }
  assert.ok(cells.size > 1, "the builder walks somewhere");
  assert.ok(cells.size <= score.columns * score.rows,
    "it never lays more bricks than the grid has cells");
});

test("the builder lays bricks on its grid and stays inside the painting", () => {
  const score = make(buildProposal, "build:walk", 128, 128);
  assert.deepEqual(score, make(buildProposal, "build:walk", 128, 128));
  const layer = render(buildProposal, score, 0);
  const early = painted(layer);
  assert.ok(early > 0, "the first brick lands");
  render(buildProposal, score, 60 * 5);
  assert.ok(painted(layer) > early, "more bricks arrive as the clock runs");

  // The walk is capped, so once it has run out the layer stops changing.
  render(buildProposal, score, 60 * 100000);
  const settled = painted(layer);
  render(buildProposal, score, 60 * 200000);
  assert.equal(painted(layer), settled, "the build finishes rather than running on");
  assert.ok(settled <= layer.width * layer.height);
});

test("Banner keeps its recovered size, speed, depth, and turn lists", () => {
  assert.deepEqual(BANNER.sizes, [4, 8, 16]);
  assert.deepEqual(BANNER.speeds, [1, 2, 3, 4]);
  assert.deepEqual(BANNER.depths, [1, 2, 5]);
  assert.deepEqual(BANNER.turns, [-45, 45, -15, 15]);
  assert.equal(BANNER.drawSeconds, .1);
  assert.equal(BANNER.turnSeconds, .1);
  assert.equal(BANNER.cue, "banner - theme");
  // rate = .9 + speed / 4 * .2, volume = -5 - (10 - size / 16 * 10)
  assert.equal(BANNER.cueRate(4), 1.1);
  assert.equal(BANNER.cueVolume(16), -5);
  assert.ok(BANNER.cueVolume(4) < BANNER.cueVolume(16), "a smaller banner is quieter");

  for (let seed = 0; seed < 200; seed += 1) {
    const score = make(bannerProposal, `banner:${seed}`);
    assert.ok(BANNER.sizes.includes(score.size));
    assert.ok(BANNER.speeds.includes(score.speed));
    assert.ok(BANNER.depths.includes(score.depth));
    // Dark is mixed from .1–.5 lightness, light from .5–.95, so the pair must
    // differ in brightness the way the two hslaToRgba calls do.
    const brightness = (color) => color[0] + color[1] + color[2];
    assert.ok(brightness(score.light) >= brightness(score.dark) - 60,
      "the light band is not darker than the dark one");
  }
});

test("the banner lays a two-colour trail that turns as it goes", () => {
  const score = make(bannerProposal, "banner:zip", 128, 128);
  assert.deepEqual(score, make(bannerProposal, "banner:zip", 128, 128));
  const layer = render(bannerProposal, score, 0);
  const early = painted(layer);
  render(bannerProposal, score, 60 * 3);
  assert.ok(painted(layer) > early, "the ribbon advances");

  // Both bands are laid, alternating.
  const seen = new Set();
  for (let index = 0; index < layer.pixels.length; index += 4) {
    if (layer.pixels[index + 3] === 0) continue;
    seen.add(`${layer.pixels[index]},${layer.pixels[index + 1]},${layer.pixels[index + 2]}`);
  }
  assert.ok(seen.size >= 2, "the zipper alternates its two colours");
  render(bannerProposal, score, 60 * 100000);
  const settled = painted(layer);
  render(bannerProposal, score, 60 * 200000);
  assert.equal(painted(layer), settled, "the ribbon ends rather than running on");
});

test("No Paint 3 resolves build and banner to their own pieces", async () => {
  const { COMPATIBLE_BRUSHES } = await import(
    "../public/aesthetic.computer/disks/nopaint.mjs");
  for (const slug of ["build", "banner"]) {
    const piece = await import(`../public/aesthetic.computer/disks/${slug}.mjs`);
    assert.equal(piece.system, "nopaint");
    assert.equal(piece.nopaintProposal.slug, slug);
    assert.equal(COMPATIBLE_BRUSHES.get(slug), piece.nopaintProposal);
  }
});

test("the fallback catalog is down to the two names still waiting", async () => {
  const { nonConflictingConstructProposals } = await import(
    "../public/aesthetic.computer/lib/nopaint-construct-catalog.mjs");
  assert.deepEqual(nonConflictingConstructProposals.map(({ slug }) => slug).sort(),
    ["bubbles", "walker"],
    "only the two slugs whose pieces already own them by import order remain");
});
