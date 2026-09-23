import test from "node:test";
import assert from "node:assert/strict";
import { LairkManager, keepOnGround, LAIRK_BOUNDS } from "./lairk-manager.mjs";

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

test("a verified handle on the roster may walk", async () => {
  const { lm, sent } = setup();
  await lm.auth("a", "good");
  assert.deepEqual(last(sent, "a", "lairk:auth:ok").content, { handle: "jeffrey", at: null });
});

test("a handle nobody mentioned may not", async () => {
  const { lm, sent } = setup();
  await lm.auth("a", "other");
  assert.equal(last(sent, "a", "lairk:auth:no").content.reason, "mention");
});

test("no token, no walking", async () => {
  const { lm, sent } = setup();
  await lm.auth("a", "forged");
  assert.equal(last(sent, "a", "lairk:auth:no").content.reason, "login");
});

test("watchers can't move; moves reach other watchers only", async () => {
  const { lm, sent, tick } = setup();
  lm.hello("w");
  lm.move("w", { x: 5, z: 5 });
  assert.equal(sent.filter((m) => m.type === "lairk:pos").length, 0);

  await lm.auth("a", "good");
  tick(100);
  lm.move("a", { x: 5, z: 5, facing: 1 });
  assert.deepEqual(last(sent, "w", "lairk:pos").content, { handle: "jeffrey", x: 5, z: 5, facing: 1 });
  assert.equal(last(sent, "a", "lairk:pos"), undefined);
});

test("a teleport is clamped to a run", async () => {
  const { lm, tick } = setup();
  await lm.auth("a", "good");
  lm.move("a", { x: 5, z: 5 });
  tick(100);
  lm.move("a", { x: 20, z: 5 });
  const p = lm.positions.get("jeffrey");
  assert.ok(p.x < 7, `moved to ${p.x}`);
});

test("the ground has an edge and the tower is solid", () => {
  const far = keepOnGround(100, 0);
  assert.equal(far.x, LAIRK_BOUNDS);
  const inside = keepOnGround(0.2, 0.1);
  assert.ok(Math.abs(inside.x) >= 1.6 || Math.abs(inside.z) >= 1.6);
});

test("positions are remembered and handed to new watchers", async () => {
  const { lm, sent, saved, tick } = setup();
  await lm.auth("a", "good");
  tick(100);
  lm.move("a", { x: 3, z: 4 });
  lm.leave("a");
  lm.hello("late");
  assert.deepEqual(last(sent, "late", "lairk:state").content.positions.jeffrey, { x: 3, z: 4, facing: 0 });
  assert.equal(saved.at(-1)[0], "jeffrey");
});

test("leaving says still to the others", async () => {
  const { lm, sent } = setup();
  lm.hello("w");
  await lm.auth("a", "good");
  lm.leave("a");
  assert.deepEqual(last(sent, "w", "lairk:still").content, { handle: "jeffrey" });
});
