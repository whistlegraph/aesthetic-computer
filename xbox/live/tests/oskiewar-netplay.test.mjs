// oskiewar rollback netplay, 26.09.09
// Two seats of the same fight in one process, joined by a fake wire with
// delay and loss. The contract under test: fed the same pads, both seats
// hash the same state frame after frame, however late the packets land.
import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");

// A seat: the piece loaded headless, with a clock the test owns, a pad the
// test presses, and every host hook counted rather than performed.
function createSeat({ viewport = { width: 1920, height: 1080 },
  roundBridge = null, netSend = null } = {}) {
  let now = 5000000;
  const pad = { connected: true, down: [], leftX: 0, leftY: 0 };
  const counts = { drums: 0, signals: 0, telemetry: [], analytics: 0, published: 0 };
  const noOp = () => {};
  const fight = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal", "saveReplay",
    "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle",
    "triangle3d", "triangles3d", "write", "systemWrite", "gameView",
    `${source}
     return { boot, sim, paint,
       netplayBegin: (deal, seat, send) => netBegin(deal, seat, send),
       netplayDeal: () => netMakeDeal(),
       netplayEnd: (reason) => netEnd(reason),
       netplayInbox: (packet) => netInbox.push(packet),
       netplayPreSession: (packet) => netHandlePreSession(packet),
       netplayHello: (hello) => { netPeerHello = hello; },
       netplayState: () => netSession ? { seat: netSession.seat,
         frame: netSession.frame, confirmed: netSession.confirmed,
         remoteFrame: netSession.remoteFrame, stats: { ...netSession.stats },
         snapshots: netSession.snapshots.size } : null,
       netplayHash: () => netStateHash(),
       netplaySnapshot: () => netSnapshot(),
       netplayRestore: (saved) => netRestore(saved),
       netplayScalarNames: () => Object.keys(netSimScalars()),
       fighters: () => players.map((player) => ({ name: player.name,
         x: player.x, y: player.y, alive: player.alive, score: player.score,
         roundWins: player.roundWins, remote: player.remote })),
       roundState: () => ({ roundResult, roundElapsedUs, matchOver,
         shellMode, gameplayStarted, fightOpponent, gameMode }),
       startVersus: () => startVersusFight(runtime().monotonicUs, true),
       clock: () => runtime().monotonicUs };`
  )(
    () => ({ monotonicUs: now, unixMs: 1785870000000 + Math.floor(now / 1000),
      simCount: Math.floor(now / 16667), paintCount: 0, renderAlpha: 0 }),
    (index = 0) => index === 0 ? { ...pad, down: pad.down.slice() }
      : { connected: false, down: [], leftX: 0, leftY: 0 },
    () => ({ platform: "web", inputFamily: "keyboard" }),
    (event, detail) => counts.telemetry.push([event, detail]),
    () => { counts.signals++; }, () => Promise.resolve(true),
    () => { counts.published++; }, () => { counts.analytics++; },
    () => { counts.drums++; }, noOp, noOp, noOp, noOp, undefined, undefined,
    noOp, noOp, () => viewport,
  );
  globalThis.__oskiewarVersusCapable = true;
  globalThis.__oskiewarNetSend = netSend || (() => false);
  globalThis.__oskiewarNetInbox = [];
  globalThis.__oskiewarRoundBridge = roundBridge;
  fight.boot();
  globalThis.__oskiewarRoundBridge = null;
  return {
    fight, pad, counts,
    press: (...buttons) => { pad.down = buttons; },
    tick: (elapsedUs = 16667) => { now += elapsedUs; fight.sim(); },
    time: () => now,
    setTime: (value) => { now = value; },
  };
}

// The wire between two seats: every packet takes `delay` ticks, a share of
// them never arrive, and the order is kept (the relay is TCP underneath).
function createWire(a, b, { delay = 3, loss = 0, jitter = 0, seed = 7 } = {}) {
  let tick = 0;
  let rng = seed >>> 0;
  const random = () => {
    rng = (rng + 0x6d2b79f5) >>> 0;
    let value = rng;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
  const queues = [[], []];
  const sent = [0, 0], dropped = [0, 0];
  const sender = (from) => (packet) => {
    sent[from]++;
    if (loss && random() < loss) { dropped[from]++; return true; }
    const extra = jitter ? Math.floor(random() * (jitter + 1)) : 0;
    queues[from].push({ at: tick + delay + extra, packet: structuredClone(packet) });
    return true;
  };
  const seats = [a, b];
  return {
    sendFrom: [sender(0), sender(1)],
    step() {
      tick++;
      for (let from = 0; from < 2; from++) {
        const queue = queues[from];
        queue.sort((left, right) => left.at - right.at);
        while (queue.length && queue[0].at <= tick)
          seats[1 - from].fight.netplayInbox(queue.shift().packet);
      }
    },
    stats: () => ({ sent, dropped, inFlight: queues.map((queue) => queue.length) }),
  };
}

// A scripted hand: deterministic, busy, and different per seat, so the two
// fighters actually meet and hit each other.
function choreography(seat, frame) {
  const beat = frame % 120;
  if (seat === 0) {
    if (beat < 40) return ["ArrowRight"];
    if (beat < 44) return ["A"];
    if (beat < 60) return ["ArrowRight", "ArrowUp"];
    if (beat < 64) return ["B"];
    if (beat < 90) return ["ArrowLeft"];
    if (beat < 94) return ["ArrowDown", "A"];
    return [];
  }
  if (beat < 30) return ["ArrowLeft"];
  if (beat < 34) return ["X"];
  if (beat < 50) return ["ArrowUp"];
  if (beat < 70) return ["ArrowLeft", "B"];
  if (beat < 100) return ["ArrowRight"];
  if (beat < 104) return ["Y"];
  return [];
}

function beginPair(host, guest, wire) {
  host.fight.netplayHello({ name: "@RIVAL", colors: [[10, 200, 30]] });
  const deal = host.fight.netplayDeal();
  host.fight.netplayBegin(deal, 0, wire.sendFrom[0]);
  guest.fight.netplayBegin(structuredClone(deal), 1, wire.sendFrom[1]);
  return deal;
}

// Drive both seats for `frames` ticks, then keep ticking with idle pads
// until both have confirmed every frame the other simulated.
function run(host, guest, wire, frames, { settle = 40 } = {}) {
  for (let frame = 0; frame < frames; frame++) {
    const hostState = host.fight.netplayState();
    const guestState = guest.fight.netplayState();
    host.press(...choreography(0, hostState.frame));
    guest.press(...choreography(1, guestState.frame));
    host.tick();
    guest.tick();
    wire.step();
  }
  host.press();
  guest.press();
  for (let frame = 0; frame < settle; frame++) {
    host.tick();
    guest.tick();
    wire.step();
  }
}

// Bring the seat that is behind level with the one ahead, ticking only it
// (and the wire) so both stand on the same frame for a hash comparison.
function equalize(host, guest, wire, limit = 200) {
  for (let step = 0; step < limit; step++) {
    const a = host.fight.netplayState().frame;
    const b = guest.fight.netplayState().frame;
    if (a === b) return true;
    (a < b ? host : guest).tick();
    wire.step();
  }
  return false;
}

test("a snapshot restores the sim exactly: replaying the same pads reproduces the same state", () => {
  const seat = createSeat();
  seat.fight.startVersus();
  for (let frame = 0; frame < 90; frame++) {
    seat.press(...choreography(0, frame));
    seat.tick();
  }
  const saved = seat.fight.netplaySnapshot();
  const savedTime = seat.time();
  const savedHash = seat.fight.netplayHash();
  for (let frame = 90; frame < 150; frame++) {
    seat.press(...choreography(1, frame));
    seat.tick();
  }
  const ahead = seat.fight.netplayHash();
  assert.notEqual(ahead, savedHash, "the fight moved on");
  seat.fight.netplayRestore(saved);
  seat.setTime(savedTime);
  assert.equal(seat.fight.netplayHash(), savedHash, "restore lands on the saved state");
  for (let frame = 90; frame < 150; frame++) {
    seat.press(...choreography(1, frame));
    seat.tick();
  }
  assert.equal(seat.fight.netplayHash(), ahead, "the replayed frames land where the originals did");
});

test("two seats fed the same pads over a laggy wire stay one fight", () => {
  const host = createSeat();
  const guest = createSeat({ viewport: { width: 390, height: 844 } });
  const wire = createWire(host, guest, { delay: 4, jitter: 3, loss: .1 });
  beginPair(host, guest, wire);
  assert.equal(host.fight.netplayHash(), guest.fight.netplayHash(),
    "frame zero is the same state on both seats");
  run(host, guest, wire, 600);
  assert.ok(equalize(host, guest, wire), "both seats can be brought level");
  const hostState = host.fight.netplayState();
  const guestState = guest.fight.netplayState();
  assert.equal(hostState.frame, guestState.frame, "both seats stand on the same frame");
  assert.equal(host.fight.netplayHash(), guest.fight.netplayHash(),
    "and on the same state");
  assert.ok(hostState.stats.rollbacks > 0, "the host had to roll back at least once");
  assert.ok(guestState.stats.rollbacks > 0, "so did the guest");
  assert.equal(hostState.stats.desyncs, 0);
  assert.equal(guestState.stats.desyncs, 0);
  assert.ok(hostState.stats.maxRollback <= 8, "rollbacks stay inside the window");
  const names = host.fight.fighters().map((fighter) => fighter.name);
  assert.deepEqual(names, guest.fight.fighters().map((fighter) => fighter.name));
  assert.equal(names[1], "@RIVAL");
});

test("a seat that stops hearing the rival waits instead of guessing forever", () => {
  const host = createSeat();
  const guest = createSeat();
  const wire = createWire(host, guest, { delay: 2 });
  beginPair(host, guest, wire);
  run(host, guest, wire, 60, { settle: 0 });
  // The guest goes quiet: the host may run ahead only as far as the window.
  const frozen = host.fight.netplayState().frame;
  const remoteFrame = host.fight.netplayState().remoteFrame;
  for (let frame = 0; frame < 30; frame++) { host.press("ArrowRight"); host.tick(); }
  const later = host.fight.netplayState();
  assert.ok(later.frame <= later.remoteFrame + 8 + 1,
    `host held at ${later.frame} vs rival ${later.remoteFrame}`);
  assert.ok(later.remoteFrame >= remoteFrame);
  assert.ok(later.stats.waits + later.stats.stalls > 0, "the host held for the rival");
  assert.ok(later.frame >= frozen, "the host never rewinds its own frame count");
});

test("the sim state list is not empty and covers the round clock", () => {
  const seat = createSeat();
  const names = seat.fight.netplayScalarNames();
  for (const needed of ["roundResult", "roundElapsedUs", "roundStartedAt",
      "roundOverAt", "matchOver", "lastSimAt", "gameMode", "fightOpponent"])
    assert.ok(names.includes(needed), `${needed} is snapshotted`);
});

test("rounds roll over inside the fight and both seats agree on the score", () => {
  const host = createSeat();
  const guest = createSeat();
  const wire = createWire(host, guest, { delay: 3, jitter: 2, loss: .05, seed: 11 });
  beginPair(host, guest, wire);
  // Long enough for a knockout, the result card, and the next round's bell.
  run(host, guest, wire, 2400, { settle: 60 });
  assert.ok(equalize(host, guest, wire));
  assert.equal(host.fight.netplayHash(), guest.fight.netplayHash());
  const hostState = host.fight.netplayState();
  assert.equal(hostState.stats.desyncs, 0);
  assert.equal(guest.fight.netplayState().stats.desyncs, 0);
  const scores = host.fight.fighters().map((fighter) => fighter.roundWins + fighter.score);
  assert.ok(scores.some((value) => value > 0), `somebody scored: ${JSON.stringify(scores)}`);
  assert.deepEqual(scores, guest.fight.fighters().map((fighter) => fighter.roundWins + fighter.score));
  const frames = hostState.frame;
  const perFrame = (value) => (value / frames).toFixed(3);
  console.log(`netplay cost over ${frames} frames: snapshot ${perFrame(hostState.stats.snapshotMs)} ms/frame, ` +
    `resim ${perFrame(hostState.stats.resimMs)} ms/frame, rollbacks ${hostState.stats.rollbacks} ` +
    `(max depth ${hostState.stats.maxRollback}), stalls ${hostState.stats.stalls}, waits ${hostState.stats.waits}`);
});

test("a seat that presses Menu leaves, and the rival hears it at once", () => {
  const host = createSeat();
  const guest = createSeat();
  const wire = createWire(host, guest, { delay: 2 });
  beginPair(host, guest, wire);
  run(host, guest, wire, 120, { settle: 0 });
  guest.press("Menu");
  guest.tick();
  assert.equal(guest.fight.netplayState(), null, "the guest's session is over");
  guest.press();
  for (let frame = 0; frame < 6; frame++) { host.tick(); guest.tick(); wire.step(); }
  assert.equal(host.fight.netplayState(), null, "the host heard the goodbye");
  assert.equal(host.fight.roundState().fightOpponent, "versus-lobby", "the host waits alone again");
  // The host's clock kept going forward through the hand-back.
  const before = host.fight.clock();
  host.tick();
  assert.ok(host.fight.clock() > before);
});

test("frame zero and the deal are the same on both seats whatever the viewport", () => {
  const host = createSeat({ viewport: { width: 2880, height: 1620 } });
  const guest = createSeat({ viewport: { width: 375, height: 667 } });
  const wire = createWire(host, guest, { delay: 1 });
  const deal = beginPair(host, guest, wire);
  assert.equal(deal.origin % 16667, 0, "the origin sits on a tick");
  assert.equal(host.fight.netplayHash(), guest.fight.netplayHash());
  assert.deepEqual(host.fight.fighters().map((fighter) => fighter.name),
    guest.fight.fighters().map((fighter) => fighter.name));
  assert.equal(host.fight.roundState().gameplayStarted, true);
  assert.equal(guest.fight.roundState().gameplayStarted, true, "the guest's HUD lane is open");
});

// The opening as it actually happens: the challenger's bridge says hello on
// the net channel, the host answers with a deal, and both fall into the same
// fight. A rival who never says hello gets the streamed fight it always had.
test("a hello turns the streamed lane into a rollback fight, and silence leaves it alone", () => {
  const sent = [];
  const host = createSeat({ netSend: (room, content) => { sent.push([room, content]); return true; } });
  assert.equal(host.fight.roundState().fightOpponent, "versus-lobby", "the host waits in its room");
  // An ordinary rival: a pad arrives, no hello. The streamed fight starts.
  globalThis.__oskiewarRemotePad = { at: Date.now(), down: [], leftX: 0, leftY: 0, name: "@OLD" };
  host.tick();
  assert.equal(host.fight.netplayState(), null, "no rollback session without an offer");
  assert.equal(host.fight.roundState().fightOpponent, "versus", "the old lane still works");
  globalThis.__oskiewarRemotePad = null;

  // A rollback-capable rival. Its hello arrives on the shell's inbox and the
  // host answers with a deal on the same channel.
  const rollback = createSeat({ netSend: (room, content) => { sent.push([room, content]); return true; } });
  sent.length = 0;
  globalThis.__oskiewarNetInbox = [{ t: "hello", v: 1, name: "@NEWRIVAL", colors: [[9, 9, 9]] }];
  globalThis.__oskiewarNetSend = (room, content) => { sent.push([room, content]); return true; };
  rollback.tick();
  const state = rollback.fight.netplayState();
  assert.ok(state, "the host opened a rollback session");
  assert.equal(state.seat, 0);
  const deal = sent.find(([, content]) => content.t === "start");
  assert.ok(deal, "and mailed the deal");
  assert.equal(deal[1].fighters[1].name, "@NEWRIVAL");
  assert.match(deal[0], /^ow-/, "addressed to the room");
  assert.equal(rollback.fight.roundState().gameplayStarted, true);
  globalThis.__oskiewarNetInbox = [];
});

test("the challenger takes a deal from its bridge and stops watching the stream", () => {
  const outbound = [];
  const bridge = { name: "sezzi7", seat: "challenger", live: true,
    start(listener) { bridge.listener = listener; return () => {}; },
    sendNet(content) { outbound.push(content); return true; },
    sendInput() { return true; } };
  const guest = createSeat({ roundBridge: bridge });
  assert.ok(bridge.listener, "the bridge is running");
  // Seated and watching: the challenger announces itself.
  guest.tick();
  assert.ok(outbound.some((packet) => packet.t === "hello"), "it says hello");
  const deal = { t: "start", v: 1, origin: 16667000, delay: 2, ballType: "soccer",
    fighters: [{ name: "@HOST", rosterIndex: 0, color: [190, 42, 58], handleColors: [] },
      { name: "@ME", rosterIndex: -1, color: [38, 82, 176], handleColors: [] }] };
  bridge.listener({ type: "net", content: deal, roundName: "sezzi7", live: true });
  const state = guest.fight.netplayState();
  assert.ok(state, "the deal opened a session");
  assert.equal(state.seat, 1, "in the second chair");
  assert.deepEqual(guest.fight.fighters().map((fighter) => fighter.name), ["@HOST", "@ME"]);
  // A state frame from the old lane is ignored now: this seat simulates.
  const before = guest.fight.fighters()[0].x;
  bridge.listener({ type: "state", roundName: "sezzi7", live: true, content: {
    format: "ac.oskiewar.live", version: 1, seq: 9, at: 1, phase: "fight",
    fighters: [{ name: "@HOST", color: [1, 2, 3], x: -99999, y: 0, z: 0, facing: 1,
      alive: true, grounded: true, ducking: false, blocking: false, score: 0,
      roundWins: 0, attack: "", removedParts: [] },
      { name: "@ME", color: [1, 2, 3], x: 99999, y: 0, z: 0, facing: -1,
        alive: true, grounded: true, ducking: false, blocking: false, score: 0,
        roundWins: 0, attack: "", removedParts: [] }],
    ball: { active: false, x: 0, y: 0, z: 0, radius: 10 },
    camera: { x: 0, y: 0, width: 800 }, round: { remainingMs: 0, result: "" } } });
  assert.equal(guest.fight.fighters()[0].x, before, "the stream no longer moves this fight");
  guest.press("ArrowRight");
  for (let frame = 0; frame < 30; frame++) guest.tick();
  assert.ok(outbound.some((packet) => packet.t === "i"), "its pads go up the net channel");
});
