import assert from "node:assert/strict";
import test from "node:test";

import { planFuserRectPack, validateFuserLayoutPlan } from "../lib/fuser-layout.mjs";

test("Fuser rect pack is deterministic, centered, and non-overlapping", () => {
  const nodes = [
    { id:"c", type:"VideoNode", rect:{ x:620, y:420, width:240, height:180 } },
    { id:"a", type:"TextNode", rect:{ x:100, y:120, width:220, height:160 } },
    { id:"b", type:"ImageNode", rect:{ x:380, y:130, width:260, height:220 } },
  ];
  const plan = planFuserRectPack(nodes, { x:0, y:0, width:1280, height:720 });
  assert.equal(plan.rows, 1);
  assert.deepEqual(plan.placements.map((item) => item.nodeId), ["a", "b", "c"]);
  assert.deepEqual(validateFuserLayoutPlan(plan), { pass:true, issues:[] });
});

test("Fuser rect pack wraps tall collections into measured rows", () => {
  const nodes = Array.from({ length:6 }, (_, index) => ({
    id:`n${index}`, rect:{ x:index * 10, y:0, width:260, height:120 + index * 5 },
  }));
  const plan = planFuserRectPack(nodes, { x:0, y:0, width:1000, height:900 });
  assert.ok(plan.rows > 1);
  assert.equal(plan.placements.length, 6);
  assert.equal(validateFuserLayoutPlan(plan).pass, true);
});

