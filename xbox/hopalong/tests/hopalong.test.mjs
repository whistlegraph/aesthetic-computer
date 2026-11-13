import assert from "node:assert/strict";
import test from "node:test";
import { bindings, load, quietHost, source } from "./load.mjs";

test("nothing touches the host before boot", () => {
  const thrower = Object.fromEntries(bindings.map((name) => [name, () => { throw new Error(name + " at top level"); }]));
  assert.doesNotThrow(() => load(thrower));
  assert.ok(Buffer.byteLength(source) < 2 * 1024 * 1024, "under the host's 2 MiB live limit");
});

test("the title plays itself inside the frame budget", () => {
  const { host, frame } = quietHost();
  const game = load(host);
  game.boot();
  let most = 0;
  for (let i = 0; i < 60 * 40; i++) {
    game.sim();
    if (i % 3) continue;
    game.paint();
    most = Math.max(most, frame.count);
    for (let k = 0; k < frame.count * 12; k++) {
      const v = frame.faces[k];
      assert.ok(Number.isFinite(v) && Math.abs(v) <= 32768, `coordinate ${v} at tick ${i}`);
    }
  }
  assert.equal(game.hopalong.state.mode, "title");
  assert.ok(most > 200 && most < 5000, `${most} triangles at most`);
});

for (const level of [0, 1, 2]) test(`the autopilot finishes stage ${level + 1}`, () => {
  const { host } = quietHost();
  const game = load(host);
  game.boot();
  game.hopalong.start(level, true);
  let state = game.hopalong.state, far = 0;
  for (let i = 0; i < 60 * 200 && state.mode === "play"; i++) {
    game.sim();
    if (i % 20 === 0) game.paint();
    state = game.hopalong.state;
    if (state.level === level) far = Math.max(far, state.x);
  }
  assert.equal(state.mode, "clear", `stopped at column ${far.toFixed(1)} of ${state.goal} in ${state.mode}`);
  assert.equal(state.deaths, 0);
});

test("a pad walks, jumps and pauses", () => {
  let down = [];
  const { host } = quietHost();
  const game = load({ ...host, gamepad: () => ({ down, leftX: 0, leftY: 0 }) });
  game.boot();
  for (let i = 0; i < 40; i++) game.sim();
  down = ["A"]; game.sim(); down = [];
  assert.equal(game.hopalong.state.mode, "intro");
  for (let i = 0; i < 120; i++) game.sim();
  assert.equal(game.hopalong.state.mode, "play");
  const x0 = game.hopalong.state.x;
  down = ["ArrowRight"];
  for (let i = 0; i < 30; i++) game.sim();
  assert.ok(game.hopalong.state.x > x0 + 1, "walked right");
  down = ["ArrowRight", "A"];
  for (let i = 0; i < 12; i++) game.sim();
  assert.ok(game.hopalong.state.y > 4 && !game.hopalong.state.ground, "left the ground");
  down = ["Menu"]; game.sim(); down = [];
  assert.equal(game.hopalong.state.mode, "pause");
});
