import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  FRAME_CYCLE_TICKS,
  FRAME_START_INDEX,
  bubblesAnimations,
  bubblesProposal,
  frameFrames,
  frameProposal,
  walkerAnimations,
  walkerProposal,
} from "../public/aesthetic.computer/lib/nopaint-construct-sprites.mjs";

const root = new URL("../public/nopaint.art/data.json", import.meta.url);
const source = JSON.parse(await readFile(root, "utf8")).project;

function constructAnimations(objectName) {
  const object = source[3].find(([name]) => name === objectName);
  return Object.fromEntries(object[7].map((entry) => [entry[0], {
    fps: entry[1],
    frames: entry[7].map((frame) => ({
      sheet: `/nopaint.art/${frame[0]}`,
      x: frame[2], y: frame[3], w: frame[4], h: frame[5],
      ox: frame[7], oy: frame[8],
    })),
  }]));
}

function assertManifest(actual, expected) {
  assert.deepEqual(Object.keys(actual), Object.keys(expected));
  for (const name of Object.keys(expected)) {
    assert.equal(actual[name].fps, expected[name].fps, `${name} fps`);
    assert.equal(actual[name].frames.length, expected[name].frames.length, `${name} frame count`);
    actual[name].frames.forEach((frame, index) => {
      const original = expected[name].frames[index];
      assert.deepEqual(
        { sheet: frame.sheet, x: frame.x, y: frame.y, w: frame.w, h: frame.h },
        { sheet: original.sheet, x: original.x, y: original.y, w: original.w, h: original.h },
        `${name} frame ${index} crop`,
      );
      assert.ok(Math.abs(frame.ox - original.ox) < 0.000001, `${name} frame ${index} origin x`);
      assert.ok(Math.abs(frame.oy - original.oy) < 0.000001, `${name} frame ${index} origin y`);
    });
  }
}

test("Bubbles retains all Construct sprite crops, origins, and animation speeds", () => {
  assertManifest(bubblesAnimations, constructAnimations("Bubbles"));
});

test("WalkerElla retains all nine Construct sprite animations", () => {
  assertManifest(walkerAnimations, constructAnimations("WalkerElla"));
});

test("sprite-backed contracts are deterministic and render cropped source frames", () => {
  const base = Object.freeze({ color: Object.freeze([20, 40, 60, 128]), x: 10, y: 20,
    w: 120, h: 80, drift: 4, thickness: 2, points: Object.freeze([]), phase: 0 });
  for (const contract of [bubblesProposal, walkerProposal]) {
    const make = () => contract.generate({ random: seededRandom(contract.slug), width: 320, height: 240, base });
    const score = make();
    assert.deepEqual(score, make());
    const pasted = [];
    const assets = new Map(contract.assets.map((path) => [path, { path }]));
    contract.render({
      nopaintAssets: assets,
      paste: (...args) => pasted.push(args),
      ink: () => ({ oval() {}, box() {} }),
    }, score, 60);
    assert.ok(pasted.length > 0, `${contract.slug} uses its loaded sprite sheet`);
    assert.ok(pasted.every(([source, x, y, transform]) =>
      source?.path && Number.isFinite(x) && Number.isFinite(y) &&
      Number.isFinite(transform?.scale) && transform?.crop));
  }
});

test("Frame retains the eleven Construct borders in CycleFrame order", () => {
  const [original] = Object.values(constructAnimations("Frames"));
  assert.deepEqual(frameFrames, original.frames);
  assert.equal(original.fps, 0, "the borders are a still collection Frame cycles itself");
});

test("Frame advances one border per recovered one second cycle", () => {
  const make = () => frameProposal.generate({
    random: seededRandom("frame"), width: 320, height: 240,
    base: Object.freeze({ color: Object.freeze([20, 40, 60, 128]) }),
  });
  const score = make();
  assert.deepEqual(score, make());
  assert.equal(score.start, FRAME_START_INDEX, "Construct always opened on index 1");
  const cropAt = (tick) => {
    let call = null;
    frameProposal.render({
      nopaintAssets: new Map(frameProposal.assets.map((path) => [path, { path }])),
      paste: (...args) => { call = args; },
      ink: () => ({ box() {} }),
    }, score, tick);
    return call;
  };
  const [source, x, y, transform] = cropAt(0);
  assert.ok(source.path);
  assert.deepEqual([x, y], [0, 0], "the border covers the whole painting");
  assert.deepEqual([transform.width, transform.height], [320, 240]);
  assert.deepEqual(cropAt(0)[3].crop, cropAt(FRAME_CYCLE_TICKS - 1)[3].crop);
  assert.notDeepEqual(cropAt(0)[3].crop, cropAt(FRAME_CYCLE_TICKS)[3].crop);
  assert.deepEqual(cropAt(0)[3].crop,
    cropAt(FRAME_CYCLE_TICKS * frameFrames.length)[3].crop);
});

test("Bubbles, Walker, Dark Window, and Frame are standalone No Paint piece modules", async () => {
  for (const slug of ["bubbles", "walker", "dark-window", "frame"]) {
    const piece = await import(`../public/aesthetic.computer/disks/${slug}.mjs`);
    assert.equal(piece.system, "nopaint");
    assert.equal(piece.nopaintProposal.slug, slug);
    assert.equal(typeof piece.paint, "function");
    assert.equal(typeof piece.bake, "function");
    assert.equal(typeof piece.meta, "function");
  }
});

test("Line and Box publish the proposal contracts consumed by No Paint 3", async () => {
  const line = await import("../public/aesthetic.computer/disks/line.mjs");
  const box = await import("../public/aesthetic.computer/disks/box.mjs");
  assert.equal(line.nopaintProposal.slug, "line");
  assert.equal(box.nopaintProposal.slug, "rect");
  assert.equal(box.nopaintProposal.generate({
    base: { color: [1, 2, 3, 64], x: 2, y: 3, w: 4, h: 5, drift: 1 },
  }).brush.slug, "box");
});
