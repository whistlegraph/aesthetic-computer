import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import {
  STAMP_ANIMATION_COUNTS,
  STAMP_SHEETS,
  STAMP_USERS,
  stampAnimations,
  stampAnimationsFor,
} from "../public/aesthetic.computer/lib/nopaint-stamp-sprites.mjs";

const project = JSON.parse(await readFile(
  new URL("../public/nopaint.art/data.json", import.meta.url), "utf8")).project;

// The committed manifest is only trustworthy if it still equals what the
// Construct export says, so re-derive it here instead of restating constants.
const object = project[3].find(([name]) => name === "Stamp");
const sheet = project[6].find(([name]) => name === "Stamp");
const locals = Object.fromEntries(sheet[1]
  .filter((node) => node[0] === 1 && typeof node[1] === "string")
  .map((node) => [node[1], node[3]]));

const construct = Object.fromEntries(object[7].map((animation) => [animation[0], {
  fps: animation[1],
  loop: animation[2],
  frames: animation[7].map((frame) => ({
    sheet: `/nopaint.art/${frame[0]}`,
    x: frame[2], y: frame[3], w: frame[4], h: frame[5], ox: frame[7], oy: frame[8],
  })),
}]));

const base = Object.freeze({
  color: Object.freeze([20, 40, 60, 128]), x: 10, y: 20, w: 120, h: 80,
  drift: 4, thickness: 2, points: Object.freeze([]), phase: 0,
});

test("Stamp retains every Construct animation name, speed, crop, and origin", () => {
  assert.deepEqual(Object.keys(stampAnimations), Object.keys(construct));
  for (const [name, original] of Object.entries(construct)) {
    const recovered = stampAnimations[name];
    assert.equal(recovered.fps, original.fps, `${name} fps`);
    assert.equal(recovered.loop, original.loop, `${name} loop`);
    assert.deepEqual(recovered.frames, original.frames, `${name} frames`);
  }
  assert.equal(Object.values(stampAnimations)
    .reduce((total, { frames }) => total + frames.length, 0), 741);
});

test("Stamp keeps its event sheet's handle list and animation counts", () => {
  assert.deepEqual(STAMP_USERS, locals.users.split("|"));
  assert.deepEqual(STAMP_ANIMATION_COUNTS,
    locals.usersAnimationCount.split("|").map(Number));
  // Each declared count must correspond to a real "@handle-a-N" animation.
  STAMP_USERS.forEach((user, index) => {
    const { still, loops } = stampAnimationsFor(user);
    assert.ok(still, `${user} has a still collection`);
    assert.equal(loops.length, STAMP_ANIMATION_COUNTS[index], `${user} loop count`);
    assert.ok(loops.every((animation) => animation?.loop), `${user} loops are looping`);
  });
});

test("every manifest frame points at a sheet the contract preloads", () => {
  const sheets = new Set(STAMP_SHEETS);
  for (const [name, animation] of Object.entries(stampAnimations)) {
    for (const frame of animation.frames) {
      assert.ok(sheets.has(frame.sheet), `${name} uses ${frame.sheet}`);
    }
  }
});

test("Stamp's proposal is deterministic and pastes cropped source frames", async () => {
  const { brush, nopaintProposal } = await import(
    "../public/aesthetic.computer/disks/stamp.mjs");
  assert.equal(typeof brush, "function", "the standalone Stamp brush survives");
  assert.equal(nopaintProposal.slug, "stamp");
  assert.deepEqual(nopaintProposal.assets, STAMP_SHEETS);

  const make = () => nopaintProposal.generate({
    random: seededRandom("stamp"), width: 320, height: 240, base });
  const score = make();
  assert.deepEqual(score, make());
  assert.ok(Object.isFrozen(score));
  assert.ok(STAMP_USERS.includes(score.user));
  assert.ok(stampAnimations[score.animation], "the score names a real animation");

  const pasted = [];
  const assets = new Map(STAMP_SHEETS.map((path) => [path, { path }]));
  nopaintProposal.render({
    nopaintAssets: assets,
    paste: (...args) => pasted.push(args),
    ink: () => ({ box() {} }),
  }, score, 60);
  assert.equal(pasted.length, 1, "one recovered stamp per proposal");
  const [source, x, y, transform] = pasted[0];
  assert.ok(source.path && Number.isFinite(x) && Number.isFinite(y));
  assert.equal(transform.scale, score.scale);
  assert.ok(transform.crop, "the sheet is cropped to the chosen frame");
});

test("a looping handle advances through its own frames while a still holds", async () => {
  const { nopaintProposal } = await import(
    "../public/aesthetic.computer/disks/stamp.mjs");
  const assets = new Map(STAMP_SHEETS.map((path) => [path, { path }]));
  const cropAt = (score, frame) => {
    let crop = null;
    nopaintProposal.render({
      nopaintAssets: assets,
      paste: (...args) => { crop = args[3].crop; },
      ink: () => ({ box() {} }),
    }, score, frame);
    return crop;
  };
  // "@jeffrey-a-3" runs 6 frames at 10fps, so one cycle is 36 sixty-hz ticks.
  const looping = Object.freeze({ ...base, kind: "stamp", user: "jeffrey",
    animation: "@jeffrey-a-3", index: 0, x: 40, y: 40, scale: 2, angle: 0, mirrored: false });
  assert.deepEqual(cropAt(looping, 0), cropAt(looping, 36));
  assert.notDeepEqual(cropAt(looping, 0), cropAt(looping, 6));

  const still = Object.freeze({ ...looping, animation: "@jeffrey", index: 7 });
  assert.deepEqual(cropAt(still, 0), cropAt(still, 300));
});

test("No Paint 3 resolves the stamp slug to this exact module contract", async () => {
  const { COMPATIBLE_BRUSHES } = await import(
    "../public/aesthetic.computer/disks/nopaint.mjs");
  const { nopaintProposal } = await import(
    "../public/aesthetic.computer/disks/stamp.mjs");
  assert.equal(COMPATIBLE_BRUSHES.get("stamp"), nopaintProposal);
});
