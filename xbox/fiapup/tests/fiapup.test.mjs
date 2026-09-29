// fiapup: the pup's behaviour, the rig the game and the lisp share, and what
// a tick costs.   node --test xbox/fiapup/tests/

import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { load, countingHost, source } from "./load.mjs";
import { generate, embedded } from "../embed.mjs";
import { compile } from "../../live/object-lisp.mjs";

const tick = 1 / 60;
function fresh(extra) {
  const { host, drawn } = countingHost(extra);
  const game = load(host);
  game.boot();
  return { game, f: game.fiapup, drawn };
}
// Hold `down` for `seconds` of 60 Hz ticks.
function hold(f, seconds, down = [], every = null) {
  const script = { down };
  globalThis.__fiapupScript = script;
  try {
    for (let t = 0; t < seconds; t += tick) { f.step(); every?.(f.world); }
  } finally { globalThis.__fiapupScript = null; }
}
const tap = (f, button) => { hold(f, tick, [button]); hold(f, tick); };

test("the sealed block is what embed.mjs would write", () => {
  assert.equal(embedded(source), generate());
});

test("the game reaches only for the console's bindings", () => {
  const own = source.slice(source.indexOf("// </sealed>"));
  for (const web of ["document", "window.", "fetch(", "requestAnimationFrame", "localStorage", "import "])
    assert.ok(!own.includes(web), `fiapup.js uses ${web}`);
  // Booted with the Xbox binding set alone (load.mjs), it lives and draws.
  const { game, drawn } = fresh();
  for (let i = 0; i < 600; i++) { game.sim(); game.paint(); }
  assert.ok(drawn.faces > 600 * 300, `drew ${drawn.faces} faces in 600 paints`);
  // No title panel any more; the pad hint is the only type on screen.
  assert.ok(drawn.texts.some((t) => t.startsWith("A pet")), "the pad hint is drawn");
  assert.ok(!drawn.texts.includes("fiapup"), "no title panel");
});

test("it boots idle, and settles to sitting while it watches the hand", () => {
  const { f } = fresh();
  assert.equal(f.world.pup.state, "idle");
  hold(f, 2);
  assert.ok(f.world.pup.pose.pitch > .3, "sits after a moment");
});

test("fetch: a throw is chased, carried back and dropped at the hand", () => {
  const { f } = fresh();
  const w = f.world;
  Object.assign(w.hand, { x: 60, z: 110, holding: "ball" });
  w.ball.heldBy = "hand";
  const joy = w.pup.joy;
  let carried = false;
  tap(f, "A");
  assert.equal(w.pup.state, "fetch");
  hold(f, 8, [], (w) => { carried ||= w.ball.heldBy === "pup"; });
  assert.ok(carried, "the ball was in its mouth");
  assert.equal(w.pup.state, "idle");
  assert.equal(w.ball.heldBy, null);
  assert.ok(Math.hypot(w.ball.x - w.hand.x, w.ball.z - w.hand.z) < 70, "dropped by the hand");
  assert.ok(w.pup.joy > joy);
  assert.deepEqual(w.log.slice(0, 2), ["fetch", "idle"]);
});

test("petting: held A leans in, then it rolls over; letting go rolls it back", () => {
  const { f } = fresh();
  const w = f.world;
  Object.assign(w.pup, { x: 0, z: 60 });
  Object.assign(w.hand, { x: 6, z: 62 });
  hold(f, .5, ["A"]);
  assert.equal(w.pup.state, "petted");
  assert.equal(w.pup.pose.eyes, 0, "eyes shut");
  hold(f, 2, ["A"]);
  assert.equal(w.pup.state, "rollover");
  hold(f, 1.5, ["A"]);
  assert.ok(w.pup.pose.roll > 2.5, "belly up");
  hold(f, 2);
  assert.equal(w.pup.state, "idle");
});

test("a treat: held up it begs, let go it eats", () => {
  const { f } = fresh();
  const w = f.world;
  Object.assign(w.hand, { x: 30, z: 40 });
  hold(f, 3, ["X"]);
  assert.equal(w.pup.state, "beg");
  assert.ok(w.pup.pose.pitch > .8, "sits up");
  const joy = w.pup.joy;
  hold(f, 4);
  assert.ok(w.log.includes("eat"));
  assert.equal(w.treat.onFloor, false, "eaten");
  assert.ok(w.pup.joy > joy + .2);
});

test("tired, it naps in its bed until rested; a call wakes it and it comes", () => {
  const { f } = fresh();
  const w = f.world;
  w.pup.energy = .15;
  hold(f, 1);
  assert.equal(w.pup.state, "sleepy");
  hold(f, 8);
  assert.equal(w.pup.state, "nap");
  assert.ok(Math.hypot(w.pup.x - f.home.bed.x, w.pup.z - f.home.bed.z) < 20, "in the bed");
  const before = w.pup.energy;
  hold(f, 2);
  assert.ok(w.pup.energy > before, "resting");
  tap(f, "B");
  assert.equal(w.pup.state, "stretch");
  hold(f, 1.5);
  assert.equal(w.pup.state, "come");
});

test("play: a bow, then zoomies at a run, then a flop", () => {
  const { f } = fresh();
  const w = f.world;
  w.pup.energy = .9;
  tap(f, "Y");
  assert.equal(w.pup.state, "playbow");
  let top = 0;
  hold(f, 6, [], (w) => { top = Math.max(top, w.pup.speed); });
  assert.ok(w.log.includes("zoomies") && w.log.includes("flop"));
  assert.ok(top > 200, `ran at ${top.toFixed(0)}`);
  assert.ok(w.pup.energy < .9);
  hold(f, 2);
  assert.equal(w.pup.state, "idle");
});

test("tug: it takes the far end, pulls, and parades the rope when you let go", () => {
  const { f } = fresh();
  const w = f.world;
  Object.assign(w.hand, { x: 20, z: 60 });
  Object.assign(w.rope, { x: 20, z: 50 });
  tap(f, "A");
  assert.equal(w.hand.holding, "rope");
  hold(f, 3);
  assert.equal(w.pup.state, "tug");
  assert.equal(w.rope.heldBy, "both");
  tap(f, "A");
  assert.equal(w.pup.state, "parade");
  hold(f, 4);
  assert.equal(w.rope.heldBy, null, "dropped it");
  assert.equal(w.pup.state, "idle");
});

test("the same presses make the same afternoon", () => {
  const runs = [0, 1].map(() => {
    const { f } = fresh();
    hold(f, 3, ["ArrowUp"]); tap(f, "Y"); hold(f, 4); hold(f, 2, ["X"]); hold(f, 3);
    const p = f.world.pup;
    return JSON.stringify([p.state, p.x, p.z, p.heading, p.joy, p.energy, f.world.log]);
  });
  assert.equal(runs[0], runs[1]);
});

test("the JS rig puts the head where the lisp draws it", () => {
  const { f } = fresh();
  hold(f, 1.5, ["Y"]);                       // a bow: pitched, head up
  const puppy = compile(readFileSync(new URL("../objects/puppy-flat.lisp", import.meta.url), "utf8"), "puppy");
  const head = puppy.sketches.findIndex((s) => s.records[0] === 1 && s.records[15] === 9);
  assert.ok(head >= 0, "found the skull");
  let at = null;
  const view = new Float64Array(24);
  puppy({ owner: f.owner(f.world.pup) }, f.place(f.world.pup), {
    view, sketch: (index, m, o) => { if (index === head) at = [m[o], m[o + 1], m[o + 2]]; } });
  const rig = f.rig(f.world.pup).head;
  assert.ok(at, "the head part was drawn");
  assert.ok(Math.hypot(at[0] - rig.x, at[1] - rig.y, at[2] - rig.z) < 1e-6);
});

// ——— the budget ———
// The pup is a figure, not a prop, so it gets more than a prop's 60 numbers:
// at most 12 SKETCH ops (168 numbers) a tick, in every behaviour. The whole
// frame program, after the paint that carries a chunk's SHAPES, stays under
// 2000 numbers — about 30 chunk sketches, the ridges, the near trees, the pup, the props and
// the nearby critters — and 14000 host faces, and a tick plus a paint stays
// well inside a frame.
const moments = ["idle", "fetch", "pet", "beg", "nap", "zoomies", "tug"];

test("the pup sends at most 12 sketches a tick in every behaviour", () => {
  const puppy = compile(readFileSync(new URL("../objects/puppy-flat.lisp", import.meta.url), "utf8"), "puppy");
  const { f } = fresh();
  for (const name of moments) {
    f.stage(name, 3);
    let sketches = 0, other = 0;
    const count = () => { other++; };
    puppy({ owner: f.owner(f.world.pup) }, f.place(f.world.pup), { view: new Float64Array(24),
      sketch: () => { sketches++; }, ellipse: count, capsule: count, plate: count, disc: count, outline: count });
    assert.ok(sketches <= 12, `${name}: ${sketches} sketches`);
    assert.equal(other, 0, `${name}: nothing projected per tick`);
  }
});

test("a frame program stays small, and so does the work", () => {
  const { game, f, drawn } = fresh();
  game.paint();
  const first = f.stats.numbers;
  const costs = [];
  for (const name of moments) {
    f.stage(name, 3);
    drawn.faces = 0;
    const t0 = performance.now();
    for (let i = 0; i < 60; i++) { f.step(); game.paint(); }
    costs.push((performance.now() - t0) / 60);
    const faces = drawn.faces / 60;
    assert.ok(f.stats.numbers < 2000, `${name}: ${f.stats.numbers} numbers`);
    assert.ok(faces < 14000, `${name}: ${faces.toFixed(0)} faces`);
  }
  assert.ok(first > f.stats.numbers, "the first paint carries the SHAPES");
  const median = costs.sort((a, b) => a - b)[3];
  // Node on a loaded 8 GB Mac; the console's QuickJS is slower, so this is
  // a tripwire for a regression, not a console number.
  assert.ok(median < 8, `median tick+paint ${median.toFixed(2)} ms`);
  console.log(`  first paint ${first} numbers; steady ${f.stats.numbers}; tick+paint median ${median.toFixed(2)} ms`);
});
