import test from "node:test";
import assert from "node:assert/strict";

import { tutorialLayoutScores } from "../lib/tutorial-layout.mjs";

const measured = (nodes) => ({
  eval: async () => ({
    viewport:{ width:1190, height:630 },
    safe:{ left:88, top:76, right:1118, bottom:480 },
    nodes,
  }),
});

test("ui-legibility-score accepts full labels in the safe region", async () => {
  const scores = await tutorialLayoutScores(measured([{
    selector:".image", truncated:[], layoutWidth:336,
    rect:{ left:468.5, top:104, right:737.5, bottom:452, width:269, height:348, cx:603, cy:278 },
  }]), [".image"]);
  assert.equal(scores["ui-legibility-score"].score, 100);
  assert.equal(scores["balanced-layout-score"].score, 100);
});

test("scores reject truncated labels and chat-zone collisions", async () => {
  const scores = await tutorialLayoutScores(measured([{
    selector:".image", truncated:["Nano Banan…"],
    rect:{ left:450, top:180, right:674, bottom:560, width:224, height:380, cx:562, cy:370 },
  }]), [".image"]);
  assert.ok(scores["ui-legibility-score"].score < scores["ui-legibility-score"].threshold);
  assert.ok(scores["balanced-layout-score"].score < scores["balanced-layout-score"].threshold);
});

test("balanced-layout-score rewards an aligned two-node teaching tableau", async () => {
  const scores = await tutorialLayoutScores(measured([
    {
      selector:".image", truncated:[],
      layoutWidth:336,
      rect:{ left:235, top:104, right:571, bottom:452, width:336, height:348, cx:403, cy:278 },
    },
    {
      selector:".video", truncated:[],
      layoutWidth:336,
      rect:{ left:635, top:104, right:971, bottom:452, width:336, height:348, cx:803, cy:278 },
    },
  ]), [".image", ".video"]);
  assert.equal(scores["ui-legibility-score"].score, 100);
  assert.equal(scores["balanced-layout-score"].score, 100);
});
