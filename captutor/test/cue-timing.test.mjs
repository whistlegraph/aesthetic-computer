import test from "node:test";
import assert from "node:assert/strict";

import {
  createBeatCue, createBeatCuePlan, cueTimeMs, validateActionCuePlan,
} from "../lib/cue-timing.mjs";

const words = [
  { text:"Click", fromMs:100, toMs:300 },
  { text:"Publish.", fromMs:320, toMs:760 },
  { text:"The", fromMs:900, toMs:1030 },
  { text:"pill", fromMs:1050, toMs:1300 },
  { text:"flips", fromMs:1320, toMs:1600 },
  { text:"to", fromMs:1620, toMs:1710 },
  { text:"Live.", fromMs:1730, toMs:2040 },
];

test("cueTimeMs matches normalized phrases at either edge", () => {
  assert.equal(cueTimeMs(words, "Publish"), 320);
  assert.equal(cueTimeMs(words, "pill flips to Live", { anchor:"end" }), 2040);
});

test("createBeatCue subtracts movement lead time from the word cue", async () => {
  let clock = 10;
  const waits = [];
  const seen = [];
  const cue = createBeatCue({
    beat:{ words },
    startedAt:10,
    now:() => clock,
    sleep:async (ms) => { waits.push(ms); clock += ms / 1000; },
    onCue:(result) => seen.push(result),
  });
  await cue("Live", { leadMs:500 });
  assert.deepEqual(waits, [1230]);
  assert.equal(seen[0].targetMs, 1730);
});

test("missing cues fail before an unrelated action can run", () => {
  assert.throws(() => cueTimeMs(words, "Generate"), /was not found/);
});

test("required action cue plans fail before narration synthesis", () => {
  const screenplay = {
    slug:"new-tutorial",
    actionCuePolicy:"required",
    beats:[{ say:"Click Publish.", do:async () => {} }],
  };
  assert.throws(() => validateActionCuePlan(screenplay), /without a word cue/);
  screenplay.beats[0].cues = [{ phrase:"Generate" }];
  assert.throws(() => validateActionCuePlan(screenplay), /is not present/);
  screenplay.beats[0].cues = [{ phrase:"Click Publish" }];
  assert.equal(validateActionCuePlan(screenplay), screenplay);
});

test("required cue plans block early actions and unconsumed declarations", async () => {
  const called = [];
  const plan = createBeatCuePlan({
    beat:{ index:2, cues:["Publish", { phrase:"Live", leadMs:400 }] },
    cue:async (phrase, options) => called.push({ phrase, options }),
    required:true,
  });
  assert.throws(() => plan.assertActionReady("click"), /before nextCue/);
  await plan.next();
  plan.assertActionReady("click");
  assert.throws(() => plan.assertComplete(), /1 of 2/);
  await plan.next();
  plan.assertComplete();
  assert.deepEqual(called, [
    { phrase:"Publish", options:{} },
    { phrase:"Live", options:{ leadMs:400 } },
  ]);
});
