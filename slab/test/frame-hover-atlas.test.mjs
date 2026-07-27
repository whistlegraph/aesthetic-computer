import assert from "node:assert/strict";
import test from "node:test";

import { buildHoverProbes, changesNearPoint } from "../lib/frame-hover-atlas.mjs";

const env = {
  crop:{ x:100, y:80, w:900, h:620 },
  ax:{ elements:[{ role:"AXButton", title:"Share", cx:850, cy:130, actions:["AXPress"] }] },
  visual:[{ kind:"compact-control", cx:180, cy:240 }],
};

test("wanderer combines controls with draggable and resizable window surfaces", () => {
  const probes = buildHoverProbes(env, { mode:"wanderer", steps:20 });
  assert.ok(probes.some((probe) => probe.possibility === "button or pressable control"));
  assert.ok(probes.some((probe) => probe.possibility === "drag window"));
  assert.ok(probes.some((probe) => probe.possibility === "resize window diagonally"));
  assert.ok(probes.every((probe) => probe.x >= 100 && probe.x <= 1000));
});

test("wiggler makes a bounded no-click ring around the supplied point", () => {
  const probes = buildHoverProbes(env, { mode:"wiggler", x:500, y:300, radius:20, steps:9 });
  assert.equal(probes.length, 9);
  assert.deepEqual(probes[0], {
    x:500, y:300, kind:"probe-center",
    possibility:"hover boundary or cursor-shape change",
  });
});

test("quick diff scoring isolates changes near the moved pointer", () => {
  const score = changesNearPoint({ diff:[
    { r:[490, 290, 20, 20], cells:8 },
    { r:[900, 600, 20, 20], cells:40 },
  ] }, 500, 300, 80);
  assert.deepEqual(score, { count:1, cells:8 });
});
