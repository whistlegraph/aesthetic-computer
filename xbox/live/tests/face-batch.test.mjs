// A host that says `batchFaces` (the Mac) receives the same scene lane as
// one that takes a call per face: the same faces, boxes, lines and
// translucent faces, in the same order, in a couple of crossings a paint.
import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");

function lane(batchFaces) {
  let now = 0, crossings = 0;
  const events = [];
  const noOp = () => {};
  const face = (...v) => events.push(["face", ...v.slice(0, 9).map(Math.fround), ...[0, 1, 2].map(k => Math.fround(v[9 + k] ?? 255))]);
  const fight = new Function("runtime", "gamepad", "capabilities", "telemetry", "gameSignal",
    "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle",
    "triangle3d", "triangles3d", "triangleAlpha", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, sim, paint, enterGame: () => enterGame(runtime().monotonicUs),
      startFight: () => { shellMode = "GAME"; selecting = false; players[1].npc = false; players[1].bot = false; applyRoster(players[1], 2); startReplay(runtime().monotonicUs); resetRound(runtime().monotonicUs, true); } };`)(
    () => ({ monotonicUs: now, unixMs: 1785870000000, simCount: 0, paintCount: 0 }),
    () => ({ connected: true, down: [], leftX: 0, leftY: 0 }),
    () => ({ platform: "macos", inputFamily: "keyboard", batchFaces }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp,
    (...v) => events.push(["wipe", ...v]), (...v) => events.push(["box", ...v]),
    (...v) => events.push(["line", ...v]), (...v) => events.push(["triangle", ...v]), face,
    (buffer, count = buffer.length / 12) => {
      crossings++;
      for (let i = 0; i < count; i++) face(...buffer.subarray(i * 12, i * 12 + 12));
    },
    (...v) => events.push(["alpha", ...v]), noOp, noOp, () => ({ width: 1920, height: 1080 }));
  globalThis.__oskiewarOpponent = "dummy";
  fight.boot(); fight.enterGame(); fight.startFight();
  delete globalThis.__oskiewarOpponent;
  for (let i = 0; i < 120; i++) { now += 16667; fight.sim(); }
  events.length = 0; crossings = 0;
  fight.paint();
  return { events, crossings };
}

test("batched faces reach the host in the per-call order, in a few crossings", () => {
  const perCall = lane(false), batched = lane(true);
  assert.ok(perCall.events.filter(e => e[0] === "face").length > 500);
  assert.ok(batched.crossings > 0 && batched.crossings < 20 && batched.crossings > perCall.crossings, `${batched.crossings} crossings`);
  assert.deepEqual(batched.events, perCall.events);
});
