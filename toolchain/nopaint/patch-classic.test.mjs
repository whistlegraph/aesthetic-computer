// The data.json patch type, run against the real build in memory. Nothing is
// written: each case applies operations to the served data.json's text and
// takes them away again.
//
//   node --test toolchain/nopaint/patch-classic.test.mjs

import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { json, at, PATCHES, build } from "./patch-classic.mjs";

const raw = readFileSync(new URL("../../system/public/nopaint.art/data.json", import.meta.url), "utf8");
const project = () => JSON.parse(raw).project;

test("the served build is the 2021 export plus the patch list", () => {
  for (const row of build()) assert.ok(row.current && row.manifest, `${row.file} has drifted`);
  assert.ok(PATCHES.length >= 2);
});

test("set finds a text by layout and UID and a variable by SID, and both come back", () => {
  const patch = json("data.json", "worked-example", [
    { set: at.instance("Splash", 398), path: [5, 0], from: "38 Brushes & ? Stamps", to: "39 Brushes & ? Stamps" },
    { set: at.variable("Core", 635115374915280), path: [3], from: 0.2, to: 0.3 },
  ]);
  const patched = patch.apply(raw);
  assert.notEqual(patched, raw);
  assert.equal(patched.split("39 Brushes & ? Stamps").length - 1, 1);
  assert.equal(at.variable("Core", 635115374915280)(JSON.parse(patched).project)[1], "IndexHangTime");
  assert.equal(at.variable("Core", 635115374915280)(JSON.parse(patched).project)[3], 0.3);
  assert.equal(patch.revert(patched), raw);
  assert.equal(patch.revert(raw), raw, "taking away a patch that is not applied changes nothing");
  assert.throws(() => patch.apply(patched), /already applied/);
});

test("set refuses a value that is not what the patch says is there", () => {
  const patch = json("data.json", "wrong", [{ set: at.variable("Core", 635115374915280), path: [3], from: 0.25, to: 0.3 }]);
  assert.throws(() => patch.apply(raw), /wrong, operation 1: expected 0.25, found 0.2/);
});

test("insert and remove a row, by position in a located list, and both come back", () => {
  const core = at.sheet("Core")(project())[1];
  const first = core[0];
  const added = [1, "BrushTestProbe", 0, 7, false, false, 111122223333444, false];
  const patch = json("data.json", "rows", [
    { insert: at.sheet("Core"), path: [1], row: added, index: 0 },
    { remove: at.sheet("Core"), path: [1], row: first },
  ]);
  const patched = patch.apply(raw);
  const after = at.sheet("Core")(JSON.parse(patched).project)[1];
  assert.deepEqual(after[0], added);
  assert.equal(after.length, core.length);
  assert.ok(!after.some((row) => JSON.stringify(row) === JSON.stringify(first)));
  // Taken away, the removed row goes to the end, not where it was: say so.
  const back = at.sheet("Core")(JSON.parse(patch.revert(patched)).project)[1];
  assert.deepEqual(back.at(-1), first);
  const exact = json("data.json", "rows-exact", [
    { insert: at.sheet("Core"), path: [1], row: added, index: 0 },
    { remove: at.sheet("Core"), path: [1], row: first, index: 1 },
  ]);
  assert.equal(exact.revert(exact.apply(raw)), raw, "with the index given, remove puts the row back in place");
  assert.throws(() => exact.apply(exact.apply(raw)), /already there/);
});

test("locators find what the source map says, and refuse what is not there or not single", () => {
  const p = project();
  assert.equal(at.objectType("Painting")(p)[11], 853010152845463);
  assert.equal(at.layout("Flip")(p)[4], "Flip");
  assert.equal(at.layer("Flip", "UI")(p)[14].length, 45);
  assert.equal(at.function("Core", "UploadPainting")(p)[4], 784578080760308);
  assert.equal(at.block("Core", 784578080760308)(p)[1][0], "UploadPainting");
  assert.equal(at.action("Core", 653194526514032)(p)[3], 653194526514032);
  assert.equal(at.project()(p)[0], "No Paint");
  assert.throws(() => at.layout("Nowhere")(p), /layout Nowhere: expected one, found 0/);
  assert.throws(() => at.variable("Core", 1)(p), /expected one, found 0/);
});
