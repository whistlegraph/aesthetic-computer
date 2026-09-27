import test from "node:test";
import assert from "node:assert/strict";
import { LairkManager } from "./lairk-manager.mjs";
import {
  lairkSpawn,
  lairkStep,
  packInput,
  LAIRK_HALF,
  LAIRK_FALL_Y,
  BTN,
} from "../system/public/aesthetic.computer/lib/lairk-world.mjs";

function setup({ roster = ["jeffrey", "prutti"], tokens = { good: "Jeffrey", other: "stranger" } } = {}) {
  let clock = 1000;
  const saved = [];
  const sent = [];
  const lm = new LairkManager({
    verify: async (token) => (tokens[token] ? { handle: tokens[token] } : null),
    fetchRoster: async () => roster,
    save: (handle, pos) => saved.push([handle, pos]),
    now: () => clock,
  });
  lm.setSendFunction((id, type, content) => sent.push({ id, type, content }));
  return { lm, sent, saved, tick: (ms) => (clock += ms) };
}

const last = (sent, id, type) => sent.filter((m) => m.id === id && m.type === type).at(-1);
const forward = (t, extra = {}) => packInput(t, { fwd: 1, right: 0, yaw: 0, pitch: 0, buttons: 0, ...extra });

test("a verified handle on the roster may walk", async () => {
  const { lm, sent } = setup();
  await lm.auth("a", "good");
  const ok = last(sent, "a", "lairk:auth:ok").content;
  assert.equal(ok.handle, "jeffrey");
  assert.ok(Number.isFinite(ok.state.x));
});

test("a handle nobody mentioned may not; nor a forged token", async () => {
  const { lm, sent } = setup();
  await lm.auth("a", "other");
  assert.equal(last(sent, "a", "lairk:auth:no").content.reason, "mention");
  await lm.auth("b", "forged");
  assert.equal(last(sent, "b", "lairk:auth:no").content.reason, "login");
});

test("the server runs inputs through the same step the client predicts with", async () => {
  const { lm, tick } = setup();
  await lm.auth("a", "good");
  tick(1000);
  const inputs = [1, 2, 3, 4, 5].map((t) => forward(t));
  lm.input("a", { inputs });

  let predicted = lairkSpawn(0, 6, 180);
  for (let t = 1; t <= 5; t++) predicted = lairkStep(predicted, { fwd: 1, right: 0, yaw: 0, pitch: 0, buttons: 0 }, { x: 0, z: 6 });
  const w = lm.walkers.get("a");
  assert.equal(w.lastTick, 5);
  assert.equal(w.state.z, predicted.z); // Bit-identical, same code on both sides.
});

test("redundant inputs are run once", async () => {
  const { lm, tick } = setup();
  await lm.auth("a", "good");
  tick(1000);
  lm.input("a", { inputs: [forward(1), forward(2)] });
  const z = lm.walkers.get("a").state.z;
  lm.input("a", { inputs: [forward(1), forward(2)] });
  assert.equal(lm.walkers.get("a").state.z, z);
});

test("watchers can't move", async () => {
  const { lm } = setup();
  lm.hello("w");
  lm.input("w", { inputs: [forward(1)] });
  assert.equal(lm.walkers.size, 0);
});

test("running faster than time allows is cut off", async () => {
  const { lm } = setup();
  await lm.auth("a", "good"); // No time has passed.
  const flood = Array.from({ length: 500 }, (_, i) => forward(i + 1));
  lm.input("a", { inputs: flood });
  assert.ok(lm.walkers.get("a").lastTick < 60, `ran ${lm.walkers.get("a").lastTick} ticks`);
});

test("snapshots carry everyone, and your ack", async () => {
  const { lm, sent, tick } = setup();
  lm.hello("w");
  await lm.auth("a", "good");
  tick(1000);
  lm.input("a", { inputs: [forward(1), forward(2), forward(3)] });
  tick(60);
  lm.tick();
  assert.equal(last(sent, "a", "lairk:snap").content.ack, 3);
  const snap = last(sent, "w", "lairk:snap").content;
  assert.equal(snap.ack, undefined);
  assert.equal(snap.players[0].h, "jeffrey");
});

test("the tower is solid and the edge drops you home", () => {
  const home = { x: 0, z: 6 };
  let s = lairkSpawn(0, 6, 180); // Facing the tower (-z).
  for (let i = 0; i < 240; i++) s = lairkStep(s, { fwd: 1, yaw: 180 }, home);
  assert.ok(s.z > 1.3, `walked into the tower to z=${s.z}`);

  s = lairkSpawn(0, LAIRK_HALF - 1, 0); // Facing the edge (+z).
  let fell = false;
  for (let i = 0; i < 600; i++) {
    s = lairkStep(s, { fwd: 1, yaw: 0 }, home);
    if (s.y < 0) fell = true;
  }
  assert.ok(fell, "never left the platform");
  assert.ok(s.y > LAIRK_FALL_Y, "never came back");
});

test("jump leaves the ground and lands", () => {
  let s = lairkSpawn(0, 6, 0);
  s = lairkStep(s, { buttons: BTN.JUMP }, null);
  assert.equal(s.onGround, false);
  for (let i = 0; i < 120; i++) s = lairkStep(s, {}, null);
  assert.equal(s.onGround, true);
});

test("leaving keeps where they stood", async () => {
  const { lm, sent, saved, tick } = setup();
  lm.hello("w");
  await lm.auth("a", "good");
  tick(1000);
  lm.input("a", { inputs: [forward(1), forward(2)] });
  lm.leave("a");
  assert.equal(last(sent, "w", "lairk:still").content.handle, "jeffrey");
  assert.equal(saved.at(-1)[0], "jeffrey");
  lm.hello("late");
  assert.ok(last(sent, "late", "lairk:state").content.positions.jeffrey);
});
