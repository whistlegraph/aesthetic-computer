import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../hello.js", import.meta.url), "utf8");

function createFight() {
  let now = 0;
  const signals = [];
  const pads = [0, 1].map(() => ({ connected: true, down: [], leftX: 0, leftY: 0 }));
  const noOp = () => {};
  const fight = new Function(
    "runtime", "gamepad", "telemetry", "gameSignal", "drum", "wipe", "box", "line", "write", "systemWrite",
    `${source}\nreturn { boot, sim, players, runnerWorldGeometry, runnerDistanceToPoint, disableBot: () => { botEnabled = false; }, cameraState: () => ({ cameraWidth, cameraCenterY }), roundState: () => ({ roundResult, roundElapsedUs, matchOver }) };`
  )(
    () => ({ monotonicUs: now }),
    (index = 0) => ({ ...pads[index], down: pads[index].down.slice() }),
    noOp, (...signal) => signals.push(signal), noOp, noOp, noOp, noOp, noOp, noOp
  );
  fight.boot();
  fight.disableBot();

  const tick = (elapsedUs = 16667) => {
    now += elapsedUs;
    fight.sim();
  };
  const tap = (pad, button, gapUs = 70000) => {
    pads[pad].down = [button];
    tick();
    pads[pad].down = [];
    tick();
    tick(gapUs);
  };
  return { fight, pads, signals, tick, tap, now: () => now };
}

test("melee and movement edges emit bounded Ableton signals", () => {
  const { signals, tap } = createFight();
  tap(0, "A");
  assert.ok(signals.some(([event, player]) => event === "kick" && player === 0));
  tap(0, "B");
  assert.ok(signals.some(([event, player]) => event === "punch" && player === 0));
  tap(0, "ArrowRight");
  assert.ok(signals.some(([event, player, horizontal]) =>
    event === "move" && player === 0 && horizontal === 1));
});

test("double-tap directions trigger dash, ultra-jump, and fast-drop", () => {
  const { fight, tap } = createFight();
  tap(0, "ArrowRight");
  tap(0, "ArrowRight");
  assert.equal(fight.players[0].lastButton, "DASH RIGHT");
  assert.ok(fight.players[0].vx > 2000);

  tap(0, "ArrowUp");
  tap(0, "ArrowUp");
  assert.equal(fight.players[0].lastButton, "ULTRA JUMP");
  assert.ok(fight.players[0].vy < -1100);

  tap(0, "ArrowDown");
  tap(0, "ArrowDown");
  assert.equal(fight.players[0].lastButton, "DASH DOWN");
  assert.ok(fight.players[0].vy > 1200);
});

test("holding one direction never becomes a double-tap dash", () => {
  const { fight, pads, tick } = createFight();
  pads[0].down = ["ArrowLeft"];
  for (let frame = 0; frame < 30; frame++) tick(16667);
  assert.notEqual(fight.players[0].lastButton, "DASH LEFT");
  assert.equal(fight.players[0].dashUntil, 0);
});

test("player lands on the center platform", () => {
  const { fight, tick } = createFight();
  const player = fight.players[0];
  player.x = 6000;
  player.y = 10200;
  player.vy = 300;
  player.grounded = false;
  for (let step = 0; step < 10 && !player.grounded; step++) tick(40000);
  assert.equal(player.y, 10400);
  assert.equal(player.grounded, true);
});

test("hit detection follows the animated runner geometry", () => {
  const { fight } = createFight();
  const player = fight.players[0];
  const resting = fight.runnerWorldGeometry(player, 0);
  const breathing = fight.runnerWorldGeometry(player, .5);
  assert.notEqual(resting.head.y, breathing.head.y);
  assert.equal(fight.runnerDistanceToPoint(
    player, 0, resting.head.x, resting.head.y, resting.head.z), 0);
  assert.ok(fight.runnerDistanceToPoint(
    player, 0, resting.head.x + 140, resting.head.y, resting.head.z) > 60);
});

test("round clock can end in a tie and resets", () => {
  const { fight, tick } = createFight();
  for (let frame = 0; frame < 750; frame++) tick(40000);
  assert.equal(fight.roundState().roundResult, "TIE");
  tick(3000001);
  assert.equal(fight.roundState().roundResult, "");
  assert.equal(fight.players[0].score, 0);
  assert.equal(fight.players[1].score, 0);
});

test("first to five round wins takes the match", () => {
  const { fight, tick } = createFight();
  for (let round = 1; round <= 5; round++) {
    fight.players[0].score = 1;
    for (let frame = 0; frame < 750; frame++) tick(40000);
    assert.equal(fight.players[0].roundWins, round);
    assert.equal(fight.roundState().matchOver, round === 5);
    assert.equal(fight.roundState().roundResult,
      round === 5 ? "JEFFREY WINS MATCH" : "JEFFREY WINS ROUND");
    tick(round === 5 ? 5000001 : 3000001);
  }
  assert.equal(fight.players[0].roundWins, 0);
  assert.equal(fight.roundState().roundResult, "");
});
