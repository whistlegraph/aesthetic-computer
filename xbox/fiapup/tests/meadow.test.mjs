// The open meadow: far taps, chunk streaming by seed, the budget on the run,
// and the meadow's own taps.   node --test xbox/fiapup/tests/meadow.test.mjs

import { test } from "node:test";
import assert from "node:assert/strict";
import { load, countingHost } from "./load.mjs";

const tick = 1 / 60;
function fresh(width = 1920) {
  let us = 0;
  const { host, drawn } = countingHost({ runtime: () => ({ width, height: 1080, monotonicUs: (us += 16667) }) });
  const game = load(host);
  game.boot();
  game.fiapup.step();
  return { game, f: game.fiapup, drawn };
}
function frame(f, fingers = []) {
  globalThis.__fiapupScript = { down: [], touches: fingers };
  try { f.step(); } finally { globalThis.__fiapupScript = null; }
}
const idle = (f, seconds, each) => { for (let t = 0; t < seconds; t += tick) { frame(f); each?.(); } };
function tapScreen(f, x, y) { frame(f, [{ id: 7, x, y }]); frame(f, []); }

test("a tap far up the valley, near the top of the screen: the pup runs all the way", () => {
  const { f } = fresh(500);                 // a phone held upright
  idle(f, .2);
  // a point on the ground just under the horizon, straight ahead
  let y = 0;
  for (let k = 0; k < 1080; k += 8) {
    const g = f.ground(250, k);
    if (Math.hypot(g.x - f.world.pup.x, g.z - f.world.pup.z) < 1300 && f.tapTarget(250, k) === "grass") { y = k; break; }
  }
  const at = f.ground(250, y);
  const far = Math.hypot(at.x - f.world.pup.x, at.z - f.world.pup.z);
  assert.ok(far > 700, `a far tap: ${far.toFixed(0)} units away`);
  tapScreen(f, 250, y);
  assert.equal(f.world.pup.state, "go");
  const goal = { ...f.world.goSpot };
  idle(f, 14);
  const p = f.world.pup;
  assert.ok(Math.hypot(p.x - goal.x, p.z - goal.z) < 12, `arrived ${Math.hypot(p.x - goal.x, p.z - goal.z).toFixed(1)} away`);
  assert.ok(Math.abs(f.world.camera.x - p.x) < 200 && Math.abs(f.world.camera.z - p.z) < 250, "the camera came along");
});

test("chunks are the same for the same seed, and stream in and out as the pup moves", () => {
  const { f } = fresh();
  const a = f.chunkRecords(3, -5, 0), b = f.chunkRecords(3, -5, 0);
  assert.deepEqual(Array.from(a.records), Array.from(b.records), "same chunk, same records");
  assert.deepEqual(a.things, b.things);
  assert.notDeepEqual(Array.from(f.chunkRecords(4, -5, 0).records.slice(0, 40)), Array.from(a.records.slice(0, 40)));
  const keys = () => [...f.world.chunks.keys()].sort();
  const start = keys();
  assert.deepEqual(start, [...f.wantedChunks(f.world.pup.x, f.world.pup.z).keys()].sort());
  // walk the pup a long way east: the window follows it
  Object.assign(f.world.pup, { x: 3000, z: -200 });
  frame(f);
  assert.ok(f.world.chunksOwed > 0, "built a few at a time, not all in one frame");
  for (let i = 0; i < 40 && f.world.chunksOwed; i++) frame(f);
  const there = keys();
  assert.deepEqual(there, [...f.wantedChunks(3000, -200).keys()].sort());
  assert.ok(!there.some((k) => start.includes(k)), "the old ones went");
  // and back: the same chunks as before, built the same way
  Object.assign(f.world.pup, { x: 40, z: -40 });
  for (let i = 0; i < 40 && (i === 0 || f.world.chunksOwed); i++) frame(f);
  assert.deepEqual(keys(), start);
  const again = f.world.chunks.get(start[0]), made = f.chunkRecords(again.cx, again.cz, again.lod);
  assert.deepEqual(Array.from(again.records), Array.from(made.records));
});

test("a run across chunk boundaries keeps the budget", () => {
  const { game, f, drawn } = fresh(500);
  f.world.pup.energy = 1;
  // east and north, across several chunks, painting every frame
  const costs = [], numbers = [], faces = [];
  let crossings = 0, last = f.world.chunkBuilds;
  const stops = [[400, -300], [700, -900], [1300, -1300], [1500, -600]];
  for (const [x, z] of stops) {
    f.world.goSpot = { x, z };
    f.stageTap?.("none");
    // go there directly, as a tap would
    frame(f); f.world.pup.state = "go";
    for (let i = 0; i < 60 * 8 && f.world.pup.state === "go"; i++) {
      const t0 = performance.now();
      drawn.faces = 0;
      frame(f); game.paint();
      costs.push(performance.now() - t0);
      if (!f.stats.ops[18]) numbers.push(f.stats.numbers);
      faces.push(drawn.faces);
      if (f.world.chunkBuilds !== last) { crossings++; last = f.world.chunkBuilds; }
    }
  }
  const sorted = [...costs].sort((a, b) => a - b), median = sorted[sorted.length >> 1], worst = sorted.at(-1);
  assert.ok(crossings >= 6, `${crossings} chunk crossings`);
  assert.ok(Math.max(...numbers) < 1800, `steady program at most ${Math.max(...numbers)} numbers`);
  assert.ok(Math.max(...faces) < 14000, `at most ${Math.max(...faces)} faces`);
  assert.ok(median < 8, `median tick+paint ${median.toFixed(2)} ms`);
  console.log(`  ${crossings} crossings; tick+paint median ${median.toFixed(2)} ms, worst ${worst.toFixed(2)} ms; ` +
    `program up to ${Math.max(...numbers)} numbers; faces up to ${Math.max(...faces)}`);
});

test("tap the blanket from far off: the pup comes home", () => {
  const { f } = fresh();
  f.stage("camp", 12);
  const p = f.world.pup, h = f.home.camp;
  assert.ok(f.world.log.includes("go"));
  assert.ok(Math.hypot(p.x - h.x, p.z - h.z) < 40, `home: ${p.x.toFixed(0)},${p.z.toFixed(0)}`);
});

for (const [kind, state, check] of [
  ["flower", "smell", (w) => w.log.includes("smell") && w.heard.length > 3],
  ["butterfly", "chase", (w) => w.log.includes("chase")],
  ["stream", "splash", (w) => w.log.includes("splash") && w.pup.wet > 0],
  ["tuft", "roll", (w) => w.log.includes("roll")],
  ["sheep", "bark", (w) => w.log.includes("bark") && w.critters.some((k) => k.kind === "sheep" && k.hitAt != null)],
]) {
  test(`tap a ${kind}: the pup goes and does its thing (${state})`, () => {
    const { f } = fresh();
    const w = f.stage(kind, .05);
    assert.equal(w.pup.state, state, `${kind} → ${w.pup.state}`);
    idle(f, 7);
    assert.ok(check(f.world), `${kind}: ${f.world.log.join(">")}`);
  });
}
