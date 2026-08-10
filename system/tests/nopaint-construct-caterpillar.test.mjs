import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  CATERPILLAR,
  caterpillarProposal,
  caterpillarSprites,
} from "../public/aesthetic.computer/lib/nopaint-construct-caterpillar.mjs";

const project = JSON.parse(await readFile(
  new URL("../public/nopaint.art/data.json", import.meta.url), "utf8")).project;

const constructSprite = (objectName) => {
  const [animation] = project[3].find(([name]) => name === objectName)[7];
  return {
    fps: animation[1],
    frames: animation[7].map((frame) => ({
      sheet: `/nopaint.art/${frame[0]}`,
      x: frame[2], y: frame[3], w: frame[4], h: frame[5], ox: frame[7], oy: frame[8],
    })),
  };
};

const base = Object.freeze({
  color: Object.freeze([20, 40, 60, 128]), x: 10, y: 20, w: 120, h: 80,
  drift: 4, thickness: 2, points: Object.freeze([]), phase: 0,
});
const make = (seed = "caterpillar", width = 256, height = 256) =>
  caterpillarProposal.generate({ random: seededRandom(seed), width, height, base });

test("Caterpillar keeps the head, tailpiece, and lead crops exactly", () => {
  assert.deepEqual(caterpillarSprites.head, constructSprite("head"));
  assert.deepEqual(caterpillarSprites.tail, constructSprite("CaterpillarTailpiece"));
  assert.deepEqual(caterpillarSprites.lead, constructSprite("CaterpillarLead"));
});

test("Caterpillar keeps the constants read out of the expression table", () => {
  // ProcessNumericParameter(1, 3, 32) - 1, StartMargin, random(.2,.7),
  // Timer "Squirm" 1/35s, Timer "Turn" 1/15s, and the 256 painting.
  assert.deepEqual(CATERPILLAR.segments, [3, 32]);
  assert.equal(CATERPILLAR.startMargin, 9);
  assert.deepEqual(CATERPILLAR.saturation, [.2, .7]);
  assert.equal(CATERPILLAR.squirmHz, 35);
  assert.equal(CATERPILLAR.turnHz, 15);
  assert.equal(CATERPILLAR.spacing, 12);
  assert.equal(CATERPILLAR.canvas, 256);
  assert.equal(CATERPILLAR.rainbowLength, 6);
  assert.deepEqual(CATERPILLAR.cues,
    { rainbow: "caterpillar - rain bow road", walk: "caterpillar - trotting along" });
});

test("Caterpillar generates deterministically and within the recovered ranges", () => {
  assert.deepEqual(make(), make());
  for (let seed = 0; seed < 200; seed += 1) {
    const score = make(`caterpillar:${seed}`);
    const segments = score.length + 1;
    assert.ok(segments >= CATERPILLAR.segments[0] && segments <= CATERPILLAR.segments[1],
      `${segments} segments is inside 3..32`);
    assert.equal(score.colors.length, segments);
    assert.ok(score.side >= 0 && score.side < 4);
    assert.ok(score.saturation >= CATERPILLAR.saturation[0]
      && score.saturation <= CATERPILLAR.saturation[1]);
    assert.equal(score.rainbow, score.length === CATERPILLAR.rainbowLength);
    // Every entry starts one margin outside the painting.
    const outside = score.x < 0 || score.y < 0
      || score.x > CATERPILLAR.canvas || score.y > CATERPILLAR.canvas;
    assert.ok(outside, `entry ${score.x},${score.y} starts off the painting`);
  }
});

test("seven segments turn on the recovered rainbow road", () => {
  const rainbow = Array.from({ length: 400 }, (_, seed) => make(`caterpillar:${seed}`))
    .find((score) => score.rainbow);
  assert.ok(rainbow, "some seed asks for seven segments");
  assert.equal(rainbow.length + 1, 7);
  // The rainbow ramp walks hue; the ordinary one holds hue and drops lightness.
  const hues = new Set(rainbow.colors.map((color) => color.join(",")));
  assert.equal(hues.size, rainbow.colors.length, "every rainbow segment differs");
});

test("Caterpillar squirms one pixel per recovered step and trails its body", () => {
  const score = make();
  const drawn = (tick) => {
    const pasted = [];
    caterpillarProposal.render({
      nopaintAssets: new Map(caterpillarProposal.assets.map((path) => [path, { path }])),
      paste: (...args) => pasted.push(args),
      ink: () => ({ oval() {} }),
    }, score, tick);
    return pasted;
  };
  const early = drawn(0);
  assert.equal(early.length, score.length + 1, "one sprite per segment");
  assert.ok(early.every(([, x, y, transform]) =>
    Number.isFinite(x) && Number.isFinite(y) && transform.crop));

  // 60 ticks is 35 squirms, so the lead has moved and the body trails behind.
  const later = drawn(60);
  assert.notDeepEqual(later.at(-1).slice(1, 3), early.at(-1).slice(1, 3));
  const head = later.at(-1);
  const tail = later[0];
  assert.notDeepEqual(head.slice(1, 3), tail.slice(1, 3), "the tail lags the head");
});

test("No Paint 3 resolves caterpillar to the standalone piece's contract", async () => {
  const { COMPATIBLE_BRUSHES } = await import(
    "../public/aesthetic.computer/disks/nopaint.mjs");
  const piece = await import("../public/aesthetic.computer/disks/caterpillar.mjs");
  assert.equal(piece.system, "nopaint");
  assert.equal(COMPATIBLE_BRUSHES.get("caterpillar"), piece.nopaintProposal);
  assert.equal(piece.nopaintProposal, caterpillarProposal);
});
