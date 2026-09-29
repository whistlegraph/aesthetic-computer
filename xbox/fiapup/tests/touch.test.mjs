// fiapup under a finger: what strokes, flicks, taps and holds become.
//   node --test xbox/fiapup/tests/touch.test.mjs

import { test } from "node:test";
import assert from "node:assert/strict";
import { load, countingHost } from "./load.mjs";

const tick = 1 / 60;
function fresh(width = 1920) {
  let us = 0;
  const { host } = countingHost({ runtime: () => ({ width, height: 1080, monotonicUs: (us += 16667) }) });
  const game = load(host);
  game.boot();
  game.fiapup.step();                       // aims the camera
  return game.fiapup;
}
// One tick with these fingers down: [{ id, x, y }].
function frame(f, fingers = [], down = []) {
  globalThis.__fiapupScript = { down, touches: fingers };
  try { f.step(); } finally { globalThis.__fiapupScript = null; }
}
const idle = (f, seconds) => { for (let t = 0; t < seconds; t += tick) frame(f); };
const pupOnScreen = (f) => { const p = f.world.pup; return f.screenOf(p.x, 21 + p.pose.bob, p.z); };
// A finger stroking back and forth across the pup for `seconds`.
function stroke(f, seconds, id = 1, extra = () => []) {
  let t = 0;
  for (; t < seconds; t += tick) {
    const at = pupOnScreen(f);
    frame(f, [{ id, x: at.x + Math.sin(t * 9) * 40, y: at.y }, ...extra(t)]);
  }
}

test("a stroke across the pup is petting, and keeps on into a rollover", () => {
  const f = fresh();
  idle(f, .2);
  stroke(f, .6);
  assert.equal(f.world.pup.state, "petted");
  assert.ok(f.world.hand.petting);
  assert.ok(f.world.buzzes.includes("soft"), "a soft tap while petting");
  stroke(f, 1.8);
  assert.equal(f.world.pup.state, "rollover");
  idle(f, 2);
  assert.equal(f.world.pup.state, "idle");
});

test("a moving stroke counts for more than a resting finger", () => {
  const joyAfter = (moving) => {
    const f = fresh();
    idle(f, .2);
    const start = f.world.pup.joy;
    for (let t = 0; t < 1.2; t += tick) {
      const at = pupOnScreen(f);
      frame(f, [{ id: 1, x: at.x + (moving ? Math.sin(t * 9) * 40 : 0), y: at.y }]);
    }
    return f.world.pup.joy - start;
  };
  const still = joyAfter(false), moving = joyAfter(true);
  assert.ok(still > 0, "a resting finger still pets");
  assert.ok(moving > still * 1.3, `stroke ${moving.toFixed(3)} vs still ${still.toFixed(3)}`);
});

test("a flick throws the ball at the flick's velocity; a gentle lift sets it down", () => {
  const f = fresh();
  const b = f.world.ball, at = f.screenOf(b.x, 5, b.z);
  frame(f, [{ id: 1, x: at.x, y: at.y }]);
  assert.equal(f.world.hand.holding, null, "a finger on the ball is a tap until it moves");
  // Up the screen, fast: into the yard.
  // Where the finger is on the lawn, read against the camera each frame sees.
  const path = [];
  for (let i = 1; i <= 6; i++) {
    path.push(f.ground(at.x, at.y - i * 30));
    frame(f, [{ id: 1, x: at.x, y: at.y - i * 30 }]);
  }
  const before = f.world.ball.heldBy;
  const from = path[1], to = path[5];
  frame(f, []);
  assert.equal(before, "hand");
  assert.equal(f.world.ball.thrown, true);
  const b2 = f.world.ball;
  assert.ok(b2.vz < -140, `thrown into the yard: vz ${b2.vz.toFixed(0)}`);
  const off = Math.atan2(b2.vz, b2.vx) - Math.atan2(to.z - from.z, to.x - from.x);
  assert.ok(Math.abs(Math.atan2(Math.sin(off), Math.cos(off))) < .05, "along the flick, on the lawn");
  assert.ok(f.world.buzzes.includes("light"));
  idle(f, .1);
  assert.equal(f.world.pup.state, "fetch");

  // Faster flicks throw harder.
  const speedOf = (pixels) => {
    const g = fresh(), a = g.screenOf(g.world.ball.x, 5, g.world.ball.z);
    frame(g, [{ id: 1, x: a.x, y: a.y }]);
    for (let i = 1; i <= 6; i++) frame(g, [{ id: 1, x: a.x + i * pixels, y: a.y }]);
    frame(g, []);
    return Math.hypot(g.world.ball.vx, g.world.ball.vz);
  };
  assert.ok(speedOf(24) > speedOf(10) * 1.5);

  const g = fresh(), a = g.screenOf(g.world.ball.x, 5, g.world.ball.z);
  frame(g, [{ id: 1, x: a.x, y: a.y }]);
  for (let i = 1; i <= 30; i++) frame(g, [{ id: 1, x: a.x + i, y: a.y }]);
  for (let i = 0; i < 10; i++) frame(g, [{ id: 1, x: a.x + 30, y: a.y }]);
  frame(g, []);
  assert.equal(g.world.ball.thrown, false, "set down, not thrown");
  assert.equal(g.world.ball.heldBy, null);
});

// Tap once on the screen at a world point (or a screen point).
function tapAt(f, x, y, z) {
  const at = f.screenOf(x, y, z);
  frame(f, [{ id: 9, x: at.x, y: at.y }]);
  frame(f, []);
  return at;
}

test("tap the grass: the pup trots to that spot, with a marker, and settles", () => {
  const f = fresh();
  const spot = f.screenOf(-60, 0, -80);   // clear of the rope, the ball, the bed
  frame(f, [{ id: 1, x: spot.x, y: spot.y }]);
  const target = f.ground(spot.x, spot.y);     // the camera the lift is read against
  frame(f, []);
  assert.equal(f.world.pup.state, "go");
  assert.ok(Math.hypot(f.world.goSpot.x - target.x, f.world.goSpot.z - target.z) < 1);
  assert.ok(Math.hypot(target.x + 60, target.z + 80) < 8, "near where it was aimed");
  assert.ok(f.world.ripples.length > 0, "a ripple marks it");
  idle(f, 4);
  const p = f.world.pup;
  assert.ok(Math.hypot(p.x - target.x, p.z - target.z) < 12, `arrived: ${p.x.toFixed(0)},${p.z.toFixed(0)}`);
  assert.ok(f.world.log.includes("settle"));
});

test("the pup turns its head the moment a finger lands", () => {
  const f = fresh();
  idle(f, 1);
  const p = f.world.pup, before = p.pose.yaw;
  const at = f.screenOf(p.x - 150, 0, p.z + 100);
  frame(f, [{ id: 1, x: at.x, y: at.y }]);
  for (let i = 0; i < 5; i++) frame(f, [{ id: 1, x: at.x, y: at.y }]);
  assert.ok(Math.abs(p.pose.yaw - before) > .1, "head came round within 6 frames");
  assert.ok(p.pose.perk > .1, "ears up");
});

test("two taps on the grass start play", () => {
  const g = fresh();
  g.world.pup.energy = .9;
  const s2 = g.screenOf(60, 0, 100);
  frame(g, [{ id: 1, x: s2.x, y: s2.y }]); frame(g, []);
  idle(g, .1);
  frame(g, [{ id: 2, x: s2.x + 5, y: s2.y }]); frame(g, []);
  assert.equal(g.world.pup.state, "playbow");
});

test("tap the ball: it fetches it and brings it toward you", () => {
  const f = fresh();
  const b = f.world.ball;
  tapAt(f, b.x, 5, b.z);
  assert.equal(f.world.pup.state, "fetch");
  let carried = false;
  for (let t = 0; t < 8; t += tick) { frame(f); carried ||= b.heldBy === "pup"; }
  assert.ok(carried, "picked it up");
  assert.equal(b.heldBy, null, "and set it down");
  const edge = f.world.camera;
  assert.ok(b.z > 60, `brought to the near side: z ${b.z.toFixed(0)} (camera at ${edge.z.toFixed(0)})`);
});

test("tap the bowl: it goes and drinks", () => {
  const f = fresh();
  tapAt(f, 230, 5, -150);
  assert.equal(f.world.pup.state, "drink");
  let lapped = false;
  for (let t = 0; t < 6; t += tick) { frame(f); lapped ||= f.world.pup.phase === "lap"; }
  assert.ok(lapped);
  assert.ok(Math.hypot(f.world.pup.x - 230, f.world.pup.z + 150) < 40, "at the bowl");
});

test("tap the bed: it goes to bed and naps", () => {
  const f = fresh();
  tapAt(f, -230, 6, -140);
  assert.equal(f.world.pup.state, "sleepy");
  for (let t = 0; t < 7; t += tick) frame(f);
  assert.equal(f.world.pup.state, "nap");
  assert.ok(Math.hypot(f.world.pup.x + 230, f.world.pup.z + 140) < 20);
});

test("tap the rope: it snatches it up, shakes it, and parades it", () => {
  const f = fresh();
  const r = f.world.rope;
  tapAt(f, r.x + Math.cos(r.angle) * 15, 3.4, r.z + Math.sin(r.angle) * 15);
  assert.equal(f.world.pup.state, "shake");
  let shook = false;
  for (let t = 0; t < 5; t += tick) { frame(f); shook ||= f.world.pup.phase === "shake" && r.heldBy === "pup"; }
  assert.ok(shook);
  assert.ok(f.world.log.includes("parade"));
});

test("tap the pup: a happy wiggle", () => {
  const f = fresh();
  idle(f, .2);
  const p = f.world.pup;
  tapAt(f, p.x, 21, p.z);
  assert.equal(p.state, "wiggle");
  assert.ok(f.world.heard.length > 0, "a yip");
  idle(f, 1.2);
  assert.equal(p.state, "idle");
});

test("a tap redirects: mid-nap it wakes and comes; mid-fetch it goes where you tap", () => {
  const f = fresh();
  f.world.pup.energy = .15;
  idle(f, 9);
  assert.equal(f.world.pup.state, "nap");
  tapAt(f, 0, 0, 60);
  assert.equal(f.world.pup.state, "stretch");
  idle(f, 5);
  const p = f.world.pup;
  assert.ok(f.world.log.includes("go"));
  assert.ok(Math.hypot(p.x, p.z - 60) < 14, `came to the tap: ${p.x.toFixed(0)},${p.z.toFixed(0)}`);

  const g = fresh();
  tapAt(g, g.world.ball.x, 5, g.world.ball.z);
  for (let t = 0; t < 8 && g.world.ball.heldBy !== "pup"; t += tick) frame(g);
  assert.equal(g.world.ball.heldBy, "pup");
  tapAt(g, -150, 0, 40);
  assert.equal(g.world.pup.state, "go");
  idle(g, 5);
  assert.equal(g.world.ball.heldBy, null, "dropped it at the spot");
  assert.ok(Math.hypot(g.world.ball.x + 150, g.world.ball.z - 40) < 45);
});

test("holding still on the grass shows a treat; letting go drops it", () => {
  const f = fresh();
  const spot = f.screenOf(40, 0, 60);
  for (let t = 0; t < .5; t += tick) frame(f, [{ id: 1, x: spot.x, y: spot.y }]);
  assert.equal(f.world.hand.holding, "treat");
  for (let t = 0; t < 3; t += tick) frame(f, [{ id: 1, x: spot.x, y: spot.y }]);
  assert.equal(f.world.pup.state, "beg");
  frame(f, []);
  assert.equal(f.world.treat.onFloor, true);
  idle(f, 3);
  assert.ok(f.world.log.includes("eat"));
  assert.equal(f.world.treat.onFloor, false, "eaten");
});

test("dragging the rope starts a tug", () => {
  const f = fresh();
  const r = f.world.rope;
  Object.assign(r, { x: 0, z: 60 });
  Object.assign(f.world.pup, { x: 20, z: -20 });
  idle(f, .1);
  const at = f.screenOf(r.x + Math.cos(r.angle) * 15, 3.4, r.z + Math.sin(r.angle) * 15);
  frame(f, [{ id: 1, x: at.x, y: at.y }]);
  for (let t = 0; t < 3; t += tick) frame(f, [{ id: 1, x: at.x, y: at.y + 20 + t * 20 }]);
  assert.equal(f.world.hand.holding, "rope");
  assert.equal(f.world.pup.state, "tug");
  assert.equal(r.heldBy, "both");
  frame(f, []);
  assert.equal(f.world.pup.state, "parade");
});

test("a second finger doesn't break a stroke", () => {
  const f = fresh();
  idle(f, .2);
  const far = f.screenOf(200, 0, 120);
  stroke(f, 1.2, 1, (t) => (t > .4 && t < .5 ? [{ id: 2, x: far.x, y: far.y }] : []));
  assert.equal(f.world.pup.state, "petted");
  assert.ok(f.world.hand.petting);
});

test("no play button: a double tap plays; a pad press takes the game back from touch", () => {
  const f = fresh();
  f.world.pup.energy = .9;
  assert.equal(f.playButton, undefined);
  assert.equal(f.world.touch.mode, false);
  f.stageTap("play");
  assert.equal(f.world.pup.state, "playbow");
  frame(f, [], ["ArrowLeft"]);
  assert.equal(f.world.touch.mode, false);
});

test("portrait frames the pup bigger across the screen than landscape", () => {
  const share = (width) => {
    const f = fresh(width);
    const p = f.world.pup, c = Math.cos(p.heading), s = Math.sin(p.heading);
    const nose = f.screenOf(p.x + c * 26, 25, p.z + s * 26), tail = f.screenOf(p.x - c * 20, 25, p.z - s * 20);
    return Math.hypot(nose.x - tail.x, nose.y - tail.y) / width;
  };
  const portrait = share(500), landscape = share(1920);
  assert.ok(portrait > landscape * 2, `portrait ${portrait.toFixed(2)} of the width, landscape ${landscape.toFixed(2)}`);
  assert.ok(portrait > .15 && portrait < .7, `portrait share ${portrait.toFixed(2)}`);
});
