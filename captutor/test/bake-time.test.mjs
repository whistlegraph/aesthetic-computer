import assert from "node:assert/strict";
import test from "node:test";

import { BAKE_TIME_PRESET, bakeTimeSpans, planBakeTime } from "../lib/bake-time.mjs";

test("bake-time pairs async boundaries by stable id", () => {
  const spans = bakeTimeSpans([
    { kind:"bake-time-start", id:"image", atSec:12, label:"Baking image", liveLeadSec:3 },
    { kind:"check", atSec:20 },
    { kind:"bake-time-end", id:"image", atSec:42 },
  ]);
  assert.deepEqual(spans, [{
    id:"image",
    label:"Baking image",
    startSec:12,
    endSec:42,
    liveLeadSec:3,
    resultLeadSec:BAKE_TIME_PRESET.resultLeadSec,
    minimumFoldSec:BAKE_TIME_PRESET.minimumFoldSec,
    transitionSec:BAKE_TIME_PRESET.transitionSec,
  }]);
});

test("bake-time folds long waits and remaps later beats", () => {
  const plan = planBakeTime({
    durationSec:100,
    spans:[
      { id:"image", startSec:10, endSec:40, liveLeadSec:4 },
      { id:"video", startSec:60, endSec:90, liveLeadSec:5 },
    ],
  });
  assert.equal(plan.preset, "bake-time-fold");
  assert.equal(plan.edits.length, 2);
  assert.equal(plan.edits[0].removedSec, 26);
  assert.equal(plan.edits[1].removedSec, 25);
  assert.equal(plan.outputDurationSec, 49);
  assert.equal(plan.mapTime(9), 9);
  assert.equal(plan.mapTime(25), 14);
  assert.equal(plan.mapTime(40), 14);
  assert.equal(plan.mapTime(60), 34);
  assert.equal(plan.mapTime(90), 39);
  assert.equal(plan.mapTime(100), 49);
  assert.deepEqual(plan.segments, [
    { startSec:0, endSec:14 },
    { startSec:40, endSec:65 },
    { startSec:90, endSec:100 },
  ]);
});

test("bake-time leaves short waits untouched", () => {
  const plan = planBakeTime({
    durationSec:20,
    spans:[{ id:"quick", startSec:5, endSec:12, liveLeadSec:4 }],
  });
  assert.equal(plan.edits.length, 0);
  assert.equal(plan.outputDurationSec, 20);
  assert.equal(plan.mapTime(18), 18);
  assert.deepEqual(plan.segments, [{ startSec:0, endSec:20 }]);
});
