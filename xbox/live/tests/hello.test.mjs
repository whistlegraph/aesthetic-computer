import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../hello.js", import.meta.url), "utf8");

function createFight(startImmediately = true) {
  let now = 0;
  const signals = [];
  const replays = [];
  const triangles = [];
  const pads = [0, 1].map(() => ({ connected: true, down: [], leftX: 0, leftY: 0 }));
  const noOp = () => {};
  const drawTriangle = (...values) => {
    for (const value of values.slice(0, 6))
      assert.ok(Number.isFinite(value) && Math.abs(value) <= 32768);
    triangles.push(values);
  };
  const fight = new Function(
    "runtime", "gamepad", "telemetry", "gameSignal", "saveReplay", "drum", "wipe", "box", "line", "triangle", "write", "systemWrite",
    `${source}\nreturn { boot, sim, paint, players, ball, runnerWorldGeometry, runnerDistanceToPoint, disableBall: () => { ballEnabled = false; ball.active = false; }, enableBall: () => { ballEnabled = true; ball.active = true; ball.serveAt = 0; ball.safeUntil = 0; ball.safePlayers = 0; }, setWind: (value) => { windAcceleration = value; }, windState: () => ({ direction: windDirection, mph: windMph }), nextRound: () => resetRound(runtime().monotonicUs, false), wackBall: () => { players[0].attackKind = "KICK"; returnBall(players[0], runtime().monotonicUs, false); }, crossWackBall: (contact = 1) => crossWackBall(players.map((player) => ({ player, contact })), runtime().monotonicUs), startFight: () => { selecting = false; startReplay(runtime().monotonicUs); resetRound(runtime().monotonicUs, true); }, selectionState: () => ({ selecting, ready: selectionReady.slice() }), cameraState: () => ({ cameraWidth, cameraCenter, cameraCenterY }), roundState: () => ({ roundResult, roundElapsedUs, matchOver }), instantReplayState: () => instantReplay ? { active: true, paused: instantReplay.paused, cursor: instantReplay.cursor, frames: instantReplay.frames.length } : { active: false }, replayFrameCount: () => roundReplayFrames.length };`
  )(
    () => ({ monotonicUs: now, unixMs: 1785870000000 + Math.floor(now / 1000) }),
    (index = 0) => ({ ...pads[index], down: pads[index].down.slice() }),
    noOp, (...signal) => signals.push(signal), (payload) => replays.push(payload),
    noOp, noOp, noOp, noOp,
    drawTriangle, noOp, noOp
  );
  fight.boot();

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
  fight.disableBall();
  if (startImmediately) {
    fight.startFight();
    tick(3000001);
  }
  return { fight, pads, signals, replays, triangles, tick, tap, now: () => now };
}

test("character select offers the four AC fighters and waits for both pads", () => {
  const { fight, pads, tick } = createFight(false);
  pads[0].down = ["ArrowRight"];
  tick();
  pads[0].down = [];
  tick();
  assert.equal(fight.players[0].name, "@FIFI");
  pads[0].down = ["A"];
  tick();
  assert.equal(fight.selectionState().selecting, true);
  assert.equal(fight.players[1].name, "@OSKIE");
  assert.equal(fight.players[1].npc, false);
  pads[0].down = [];
  pads[1].down = ["A"];
  tick();
  assert.equal(fight.selectionState().selecting, false);
});

test("P1 X toggles P2 between controller and dummy on character select", () => {
  const { fight, pads, tick } = createFight(false);
  pads[0].down = ["X"];
  tick();
  assert.equal(fight.players[1].name, "DUMMY");
  assert.equal(fight.players[1].npc, true);
  assert.equal(fight.selectionState().ready[1], true);
  pads[0].down = [];
  tick();
  pads[0].down = ["X"];
  tick();
  assert.equal(fight.players[1].name, "@OSKIE");
  assert.equal(fight.players[1].npc, false);
  assert.equal(fight.selectionState().ready[1], false);
});

test("perspective intro never submits invalid ground triangles", () => {
  const { fight, pads, triangles, tick } = createFight(false);
  pads[0].down = ["A"];
  tick();
  fight.paint();
  tick(500000);
  fight.paint();
  assert.equal(triangles.length % 144, 0);
});

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
  const { fight, tap, tick } = createFight();
  tap(0, "ArrowRight");
  tap(0, "ArrowRight");
  assert.equal(fight.players[0].lastButton, "DASH RIGHT");
  assert.ok(fight.players[0].vx > 2000);
  tick(40000);
  assert.ok(Math.abs(fight.players[0].vx) < 100);

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

test("opposite input and wall contact cancel dash lock immediately", () => {
  const { fight, pads, tap, tick } = createFight();
  tap(0, "ArrowRight");
  tap(0, "ArrowRight");
  pads[0].down = ["ArrowLeft"];
  tick();
  assert.equal(fight.players[0].dashUntil, 0);
  assert.ok(fight.players[0].vx < 0);

  fight.players[0].x = 12000;
  fight.players[0].dashUntil = Number.MAX_SAFE_INTEGER;
  fight.players[0].dashVx = 2400;
  pads[0].down = ["ArrowRight"];
  tick();
  assert.equal(fight.players[0].dashUntil, 0);
});

test("a direction held across round reset waits for a real release", () => {
  const { fight, pads, tick } = createFight();
  pads[0].down = ["ArrowRight"];
  tick();
  fight.nextRound();
  tick(3000001);
  assert.ok(fight.players[0].vx < 500);
  pads[0].down = [];
  tick();
  pads[0].down = ["ArrowRight"];
  tick();
  assert.ok(fight.players[0].vx > 1000);
});

test("X shield blocks melee geometry", () => {
  const { fight, pads, signals, tick } = createFight();
  fight.players[1].npc = false;
  fight.players[0].x = 5000;
  fight.players[1].x = 5100;
  pads[0].down = ["A"];
  pads[1].down = ["X"];
  for (let frame = 0; frame < 5; frame++) tick(16667);
  assert.equal(fight.players[1].alive, true);
  assert.equal(fight.players[0].score, 0);
  assert.ok(signals.some(([event, player]) => event === "block" && player === 1));
});

test("P2 horizontal control remains active while shielding", () => {
  const { fight, pads, tick } = createFight();
  pads[1].down = ["X", "ArrowLeft"];
  tick();
  assert.ok(fight.players[1].vx < -1000);
  pads[1].down = ["X", "ArrowRight"];
  tick();
  assert.ok(fight.players[1].vx > 1000);
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

test("a grounded ball ignores wind", () => {
  const { fight, signals, tick } = createFight();
  fight.enableBall();
  fight.setWind(1200);
  fight.ball.x = 6000;
  fight.ball.y = 12000 - fight.ball.radius;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  tick(16667);
  assert.equal(fight.ball.vx, 0);
  assert.equal(signals.some(([event]) => event === "boot"), false);
});

test("each round serves the ball in the air above the center platform", () => {
  const { fight } = createFight();
  assert.equal(fight.ball.x, 6000);
  assert.ok(fight.ball.y < 10400 - fight.ball.radius);
  assert.equal(fight.ball.vy, 0);
});

test("wind rerolls and reverses direction every round", () => {
  const { fight } = createFight();
  const first = fight.windState();
  fight.nextRound();
  const second = fight.windState();
  assert.equal(second.direction, -first.direction);
  assert.ok(second.mph >= 4 && second.mph <= 24);
});

test("running into a grounded ball boots it instead of killing the player", () => {
  const { fight, signals, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  fight.ball.x = player.x;
  fight.ball.y = 12000 - fight.ball.radius;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  player.vx = 900;
  tick(16667);
  assert.equal(player.alive, true);
  assert.equal(player.lastButton, "BOOT");
  assert.ok(fight.ball.vx > 0);
  assert.ok(signals.some(([event, pad]) => event === "boot" && pad === 0));
});

test("melee returns label and signal the ball as WACK", () => {
  const { fight, signals } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  fight.ball.x = player.x + 185;
  fight.ball.y = player.y - 55;
  fight.ball.vx = -500;
  fight.ball.vy = -220;
  fight.wackBall();
  assert.equal(player.lastButton, "WACK");
  assert.ok(Math.abs(fight.ball.vx) >= 1300);
  assert.ok(signals.some(([event, pad]) => event === "wack" && pad === 0));
});

test("a close simultaneous punch and kick produce a stronger CROSS WACK", () => {
  const { fight, signals } = createFight();
  fight.enableBall();
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  fight.players[0].x = 5800;
  fight.players[1].x = 6200;
  fight.players[0].facing = 1;
  fight.players[1].facing = -1;
  fight.crossWackBall(1);
  assert.equal(fight.players[0].lastButton, "CROSS WACK");
  assert.equal(fight.players[1].lastButton, "CROSS WACK");
  assert.ok(Math.abs(fight.ball.vx) > 5000);
  assert.ok(signals.some(([event, pad]) => event === "crosswack" && pad === -1));
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

test("round result offers an instant replay with pause, scrub, and exit", () => {
  const { fight, pads, tick } = createFight();
  for (let frame = 0; frame < 220; frame++) tick(33334);
  assert.ok(fight.replayFrameCount() > 100);
  fight.players[0].score = 1;
  for (let frame = 0; frame < 680; frame++) tick(33334);
  assert.match(fight.roundState().roundResult, /WINS ROUND/);
  pads[0].down = ["Y"];
  tick();
  assert.equal(fight.instantReplayState().active, true);
  pads[0].down = [];
  tick();
  pads[0].down = ["A"];
  tick();
  assert.equal(fight.instantReplayState().paused, true);
  const beforeScrub = fight.instantReplayState().cursor;
  pads[0].down = [];
  tick();
  pads[0].down = ["ArrowRight"];
  tick();
  assert.ok(fight.instantReplayState().cursor > beforeScrub);
  pads[0].down = [];
  tick();
  pads[0].down = ["B"];
  tick();
  assert.equal(fight.instantReplayState().active, false);
});

test("first to five round wins takes the match", () => {
  const { fight, replays, tick } = createFight();
  for (let round = 1; round <= 5; round++) {
    fight.players[0].score = 1;
    for (let frame = 0; frame < 750; frame++) tick(40000);
    assert.equal(fight.players[0].roundWins, round);
    assert.equal(fight.roundState().matchOver, round === 5);
    assert.equal(fight.roundState().roundResult,
      round === 5 ? "@JEFFREY WINS MATCH" : "@JEFFREY WINS ROUND");
    tick(round === 5 ? 5000001 : 3000001);
    if (round < 5) tick(3000001);
  }
  assert.equal(fight.players[0].roundWins, 0);
  assert.equal(fight.roundState().roundResult, "");
  assert.equal(replays.length, 1);
  const demo = JSON.parse(replays[0]);
  assert.equal(demo.format, "ac.oskiedemo");
  assert.equal(demo.version, 1);
  assert.equal(demo.winner, "@JEFFREY");
  assert.match(demo.matchName,
    /^(?:[bdfgklmnprstvz][aeiou]){3}-(?:[bdfgklmnprstvz][aeiou]){3}-(?:[bdfgklmnprstvz][aeiou]){3}$/);
  assert.equal(demo.matchId, "ow-" + demo.matchName);
  assert.ok(demo.commands.length > 0);
  assert.ok(demo.checkpoints.length > 0);
});
