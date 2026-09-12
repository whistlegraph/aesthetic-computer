import test from "node:test";
import assert from "node:assert/strict";
import { EventEmitter } from "node:events";
import { canonicalMatchId, validateOskiewarLiveState,
  OskiewarLiveManager, OSKIEWAR_LIVE_LIMITS } from "./oskiewar-live-manager.mjs";

class FakeSocket extends EventEmitter {
  constructor() { super(); this.readyState = 1; this.sent = []; this.closed = null; }
  send(value) { this.sent.push(JSON.parse(value)); }
  close(code, reason) { this.readyState = 3; this.closed = { code, reason }; this.emit("close"); }
}

const state = (seq = 1) => ({
  format: "ac.oskiewar.live", version: 1, seq, at: 1000 + seq, phase: "fight",
  fighters: [
    { name: "@JEFFREY", color: [190, 42, 58], x: 2000, y: 12000, z: 0,
      facing: 1, alive: true, grounded: true, ducking: false, blocking: false,
      score: 2, roundWins: 1, attack: "" },
    { name: "@OSKIE", color: [38, 82, 176], x: 10000, y: 12000, z: 0,
      facing: -1, alive: true, grounded: true, ducking: false, blocking: true,
      score: 1, roundWins: 0, attack: "KICK" },
  ],
  ball: { active: true, x: 6000, y: 11945, z: 0, radius: 55 },
  camera: { x: 6000, y: 9140, width: 12000 },
  round: { remainingMs: 24000, result: "" },
});

test("canonical match IDs accept the QR name or ow-prefixed ID", () => {
  const name = "bafegu-dorimi-kunapo";
  assert.equal(canonicalMatchId(name), `ow-${name}`);
  assert.equal(canonicalMatchId(`ow-${name}`), `ow-${name}`);
  assert.equal(canonicalMatchId("sezzi7"), "ow-sezzi7");
  assert.equal(canonicalMatchId("ow-shuppy652"), "ow-shuppy652");
  assert.equal(canonicalMatchId("short-name"), null);
});

test("live state validation rejects unbounded or malformed values", () => {
  assert.equal(validateOskiewarLiveState(state()), null);
  assert.equal(validateOskiewarLiveState({ ...state(),
    seriesId: "ow-zavoki-bemuru-ditale",
    roundId: "ow-bafegu-dorimi-kunapo",
    previousRoundId: "ow-dorimi-kunapo-lafegu",
    nextRoundId: "ow-fagori-buneta-kovisu",
    wind: { direction: -1, mph: 18 } }), null);
  assert.equal(validateOskiewarLiveState({ ...state(), fighters: [] }), "Invalid fighters");
  assert.equal(validateOskiewarLiveState({ ...state(), nextRoundId: "bad" }),
    "Invalid next round ID");
  assert.equal(validateOskiewarLiveState({ ...state(), wind: { direction: 0, mph: 8 } }),
    "Invalid wind");
  assert.equal(validateOskiewarLiveState({ ...state(), replayUrl: "https://bad" }),
    "Invalid replay URL");
});

test("one publisher fans state only to viewers in its match", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), viewer = new FakeSocket(), other = new FakeSocket();
  manager.handleConnection(host, { url: "/oskiewar-live?match=bafegu-dorimi-kunapo&role=publisher" });
  manager.handleConnection(viewer, { url: "/oskiewar-live?match=bafegu-dorimi-kunapo" });
  manager.handleConnection(other, { url: "/oskiewar-live?match=lafegu-dorimi-kunapo" });
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state", content: state() })));
  assert.equal(viewer.sent.at(-1).type, "oskiewar:state");
  assert.equal(viewer.sent.at(-1).content.seq, 1);
  assert.notEqual(other.sent.at(-1)?.type, "oskiewar:state");
});

test("a fixed match broadcast fans out and caches state for late spectators", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket();
  const viewers = Array.from({ length: 12 }, () => new FakeSocket());
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  for (const viewer of viewers) manager.handleConnection(viewer, { url });
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({
    type: "oskiewar:state", content: state(),
  })));
  assert.ok(viewers.every((viewer) =>
    viewer.sent.at(-1)?.type === "oskiewar:state"));
  const late = new FakeSocket();
  manager.handleConnection(late, { url });
  assert.equal(late.sent.at(-1)?.type, "oskiewar:state");
  assert.equal(late.sent.at(-1)?.content.seq, 1);
});

// Coalescing is allowed to cost latency. It is not allowed to cost
// information — and for a long time it cost both: the rate gate ran before
// the store, so a burst of two frames in one instant (a host catch-up tick,
// or a link stall batching them) delivered the OLDER one and lost the newer
// for good. Against the real relay that was 25% of frames on a wired link
// and 57% on a busy one. The newest frame now always survives the gate.
test("over-rate publisher frames coalesce to the newest, and duplicates drop", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), viewer = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(viewer, { url });
  const publish = (seq) => host.emit("message",
    Buffer.from(JSON.stringify({ type: "oskiewar:state", content: state(seq) })));
  now += 30;
  publish(1);
  const count = viewer.sent.length;
  assert.equal(viewer.sent.at(-1).content.seq, 1);
  // Three more inside the 25 ms floor: nothing goes out yet, and what is held
  // is the last of them, not the first.
  now += 1;
  publish(2);
  now += 1;
  publish(3);
  now += 1;
  publish(4);
  assert.equal(viewer.sent.length, count, "the burst is coalesced, not fanned out");
  const room = manager.rooms.get("ow-bafegu-dorimi-kunapo");
  assert.equal(room.state.seq, 4, "and the newest is what is held");
  // A frame the room has already passed is a duplicate whenever it arrives.
  now += 30;
  publish(2);
  assert.equal(room.state.seq, 4);
  // The trailing flush hands the room whatever is newest when it fires.
  manager.flush(room);
  assert.equal(viewer.sent.length, count + 1);
  assert.equal(viewer.sent.at(-1).content.seq, 4);
  // And it fires once for that frame, however often it is asked.
  manager.flush(room);
  assert.equal(viewer.sent.length, count + 1);
});

test("the seat hears its own fighter before the grandstand does", () => {
  const manager = new OskiewarLiveManager({ now: () => 1000 });
  const host = new FakeSocket(), rival = new FakeSocket(), fan = new FakeSocket();
  const url = "/oskiewar-live?match=sezzi7";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(fan, { url });
  manager.handleConnection(rival, { url: `${url}&role=challenger` });
  const order = [];
  for (const [name, socket] of [["fan", fan], ["rival", rival]])
    socket.send = ((inner) => (data) => { order.push(name); inner.call(socket, data); })(socket.send);
  const room = manager.rooms.get("ow-sezzi7");
  room.state = state(9);
  manager.flush(room);
  assert.deepEqual(order, ["rival", "fan"]);
});

test("a second live publisher cannot take over a match", () => {
  const manager = new OskiewarLiveManager();
  const first = new FakeSocket(), second = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo&role=publisher";
  manager.handleConnection(first, { url });
  manager.handleConnection(second, { url });
  assert.equal(second.closed?.code, 4409);
});

test("a successor publisher counts from zero — no corpse seq gate, no stale frame", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const first = new FakeSocket(), viewer = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(first, { url: `${url}&role=publisher` });
  manager.handleConnection(viewer, { url });
  now += 30;
  first.emit("message", Buffer.from(JSON.stringify(
    { type: "oskiewar:state", content: state(500) })));
  assert.equal(viewer.sent.at(-1).content.seq, 500);
  first.close();
  const second = new FakeSocket();
  manager.handleConnection(second, { url: `${url}&role=publisher` });
  const late = new FakeSocket();
  manager.handleConnection(late, { url });
  assert.ok(!late.sent.some((m) => m.type === "oskiewar:state"),
    "a mid-handoff spectator must not be handed the dead host's frame");
  now += 30;
  second.emit("message", Buffer.from(JSON.stringify(
    { type: "oskiewar:state", content: state(1) })));
  assert.equal(viewer.sent.at(-1).content.seq, 1,
    "the claimed room must fan the new host's first frame, not mute it");
  assert.equal(late.sent.at(-1).content.seq, 1);
});

test("a challenger is seated, watches state, and their presses reach the publisher", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), rival = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(rival, { url: `${url}&role=challenger` });
  assert.ok(rival.sent.some((message) => message.type === "oskiewar:seat" &&
    message.content.seat === "challenger"));
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state", content: state() })));
  assert.equal(rival.sent.at(-1).type, "oskiewar:state");
  now += 30;
  rival.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:input",
    content: { seq: 1, down: ["ArrowLeft", "A"], leftX: -0.4, leftY: 0,
      name: "@FRIEND", colors: [[10, 20, 30]] } })));
  const input = host.sent.at(-1);
  assert.equal(input.type, "oskiewar:input");
  assert.deepEqual(input.content.down, ["ArrowLeft", "A"]);
  assert.equal(input.content.name, "@FRIEND");
});

test("the second chair holds one challenger and frees on close", () => {
  const manager = new OskiewarLiveManager();
  const host = new FakeSocket(), first = new FakeSocket(), second = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(first, { url: `${url}&role=challenger` });
  manager.handleConnection(second, { url: `${url}&role=challenger` });
  assert.equal(second.closed?.code, 4409);
  first.close(1000, "leaving");
  const third = new FakeSocket();
  manager.handleConnection(third, { url: `${url}&role=challenger` });
  assert.ok(third.sent.some((message) => message.type === "oskiewar:seat"));
});

// A host that leaves takes its last frame with it. Measured on ow-regga890:
// the room still answered `hasState: true` with a seq 2857 frame of a `fight`
// between NOBODY and NOBODY long after the host had gone, and it was the first
// thing handed to anybody who opened that address — who then read a frozen
// round as a fight they could take a seat in.
test("a departed publisher's last frame does not outlive it", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state",
    content: state(2857) })));
  const early = new FakeSocket();
  manager.handleConnection(early, { url });
  assert.equal(early.sent.at(-1).content.seq, 2857);

  host.close(1006, "tab killed");
  const late = new FakeSocket();
  manager.handleConnection(late, { url });
  const status = late.sent.find((message) => message.type === "oskiewar:status");
  assert.equal(status.content.live, false);
  assert.equal(status.content.hasState, false);
  assert.ok(!late.sent.some((message) => message.type === "oskiewar:state"));
});

// A chair is held by whoever is still speaking from it. A killed tab sends no
// close frame, so its socket stays open until the shared ping sweep reaps it
// two misses later — and for that half minute the person who just refreshed
// was told the chair was taken and demoted to the grandstand, unable to rejoin
// as themselves because the relay was holding their own dead socket against
// them.
test("a silent chair is reclaimed, a playing one is not", () => {
  let now = 100000;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), first = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(first, { url: `${url}&role=challenger` });

  // Still playing: a second arrival is refused and stays to watch.
  now += 2000;
  first.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:input",
    content: { seq: 1, down: ["A"], leftX: 0, leftY: 0 } })));
  const rival = new FakeSocket();
  manager.handleConnection(rival, { url: `${url}&role=challenger` });
  assert.equal(rival.closed?.code, 4409);
  assert.equal(first.readyState, 1);

  // Gone quiet past the ghost window: the same person comes back and the seat
  // is theirs, and the socket that was squatting on it is closed.
  now += OSKIEWAR_LIVE_LIMITS.CHALLENGER_GHOST_MS + 1;
  const rejoin = new FakeSocket();
  manager.handleConnection(rejoin, { url: `${url}&role=challenger` });
  assert.ok(rejoin.sent.some((message) => message.type === "oskiewar:seat"));
  assert.equal(rejoin.closed, null);
  assert.equal(first.closed?.code, 4410);

  // And the evicted socket's own close must not clear the seat it no longer
  // holds — the newcomer's pads still reach the host.
  first.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:input",
    content: { seq: 9, down: ["B"], leftX: 0, leftY: 0 } })));
  now += 100;
  rejoin.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:input",
    content: { seq: 2, down: ["X"], leftX: 0, leftY: 0 } })));
  const pads = host.sent.filter((message) => message.type === "oskiewar:input");
  assert.deepEqual(pads.at(-1).content.down, ["X"]);
});

// The bare front door asks for one open room. Only a live host with an empty
// second chair and an untimed round qualifies — the timed rounds are the
// recorded broadcast farm, television rather than an open chair.
test("the matchmaker offers one live untimed room with an empty chair", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  assert.equal(manager.openRoom(), null);
  const farm = new FakeSocket();
  manager.handleConnection(farm, { url: "/oskiewar-live?match=lafegu-dorimi-kunapo&role=publisher" });
  now += 30;
  farm.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state",
    content: { ...state(), round: { remainingMs: 24000, timed: true, result: "" } } })));
  assert.equal(manager.openRoom(), null);
  const host = new FakeSocket();
  manager.handleConnection(host, { url: "/oskiewar-live?match=bafegu-dorimi-kunapo&role=publisher" });
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state",
    content: { ...state(), round: { remainingMs: 0, timed: false, result: "" } } })));
  assert.deepEqual(manager.openRoom(),
    { matchId: "ow-bafegu-dorimi-kunapo", room: "bafegu-dorimi-kunapo" });
  // A seated challenger closes the door; an emptied chair reopens it —
  // until the host's frames go stale.
  const rival = new FakeSocket();
  manager.handleConnection(rival, { url: "/oskiewar-live?match=bafegu-dorimi-kunapo&role=challenger" });
  assert.equal(manager.openRoom(), null);
  rival.close(1000, "leaving");
  assert.equal(manager.openRoom()?.room, "bafegu-dorimi-kunapo");
  now += 10001;
  assert.equal(manager.openRoom(), null);
});

test("bent or over-rate challenger input is dropped in silence", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), rival = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(rival, { url: `${url}&role=challenger` });
  const count = host.sent.length;
  const press = (content) => rival.emit("message",
    Buffer.from(JSON.stringify({ type: "oskiewar:input", content })));
  now += 30;
  press({ seq: 1, down: ["<script>"], leftX: 0, leftY: 0 });
  press({ seq: 2, down: ["A"], leftX: 99, leftY: 0 });
  press({ seq: "3", down: ["A"], leftX: 0, leftY: 0 });
  press({ seq: 4, down: ["A"], leftX: 0, leftY: 0, name: "not a name!" });
  assert.equal(host.sent.length, count);
  press({ seq: 5, down: ["A"], leftX: 0, leftY: 0 });
  assert.equal(host.sent.length, count + 1);
  // The 15 ms floor paces a stick, which moves continuously — so the same
  // buttons arriving again inside it are dropped, leftX and all.
  press({ seq: 6, down: ["A"], leftX: .5, leftY: 0 });
  assert.equal(host.sent.length, count + 1);
  // A button edge is not a stick. The client marks a frame sent the moment
  // the socket takes it, so a press swallowed here was invisible to both ends
  // until the next change or the idle heartbeat a quarter of a second later.
  press({ seq: 7, down: ["B"], leftX: 0, leftY: 0 });
  assert.equal(host.sent.length, count + 2, "a press goes through at once");
  assert.deepEqual(host.sent.at(-1).content.down, ["B"]);
  press({ seq: 8, down: [], leftX: 0, leftY: 0 });
  assert.equal(host.sent.length, count + 3, "and so does the release");
  press({ seq: 9, down: [], leftX: 0, leftY: 0 });
  assert.equal(host.sent.length, count + 3, "holding nothing is still a hold");
  now += 30;
  press({ seq: 10, down: [], leftX: 0, leftY: 0 });
  assert.equal(host.sent.length, count + 4);
});

test("live rooms emit minimized server milestones once", () => {
  let now = 100;
  const captured = [];
  const manager = new OskiewarLiveManager({
    now: () => now,
    analytics: {
      capture(action, properties) {
        captured.push([action, properties]);
      },
    },
  });
  const host = new FakeSocket(), viewer = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, {
    url: `${url}&role=publisher&surface=xbox`,
  });
  manager.handleConnection(viewer, { url: `${url}&surface=web` });
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({
    type: "oskiewar:state",
    content: state(1),
  })));
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({
    type: "oskiewar:state",
    content: state(2),
  })));

  assert.deepEqual(captured, [
    ["spectator_joined", {
      source_system: "session-server",
      surface: "web",
      viewer_state: "live",
    }],
    ["live_started", {
      source_system: "session-server",
      surface: "xbox",
      phase: "fight",
    }],
  ]);
  assert.ok(captured.every(([, properties]) =>
    !Object.hasOwn(properties, "matchId")));
});

// @jeffrey: "if we are in debug mode and reading telemetry on a device can we
// show a little agent icon to show our linked in connection". The game can only
// draw that mark if the relay tells it a machine — rather than one more phone
// that scanned the round QR — is on the wire.
test("an agent link is counted apart from the phone grandstand", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), viewer = new FakeSocket();
  const agent = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher&surface=web` });
  manager.handleConnection(viewer, { url });
  assert.deepEqual(host.sent.at(-1),
    { type: "oskiewar:viewers", content: { count: 1, agents: 0 } });
  manager.handleConnection(agent, { url: `${url}&role=agent` });
  assert.deepEqual(host.sent.at(-1),
    { type: "oskiewar:viewers", content: { count: 1, agents: 1 } });
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state",
    content: { ...state(), perf: { fps: 58, frameMs: 17.2 } } })));
  // An agent reads the frame numbers out of the same payload a phone gets, so
  // the split is in the counting only, never in the fan-out.
  assert.equal(agent.sent.at(-1).type, "oskiewar:state");
  assert.deepEqual(agent.sent.at(-1).content.perf, { fps: 58, frameMs: 17.2 });
  agent.close(1000, "done");
  assert.deepEqual(host.sent.at(-1),
    { type: "oskiewar:viewers", content: { count: 1, agents: 0 } });
});

// @jeffrey plays in Edge on an Xbox, where "refresh the page" means driving
// an address bar with a controller — so an attached agent may ask the game to
// reload itself. The relay forwards the bare instruction to the publisher,
// rate-limited, and nothing a spectator sends goes anywhere at all.
test("an agent can nudge the publisher to reload, gently and only agents", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), viewer = new FakeSocket();
  const agent = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher&surface=web` });
  manager.handleConnection(viewer, { url });
  manager.handleConnection(agent, { url: `${url}&role=agent` });
  const reload = JSON.stringify({ type: "oskiewar:reload" });
  agent.emit("message", Buffer.from(reload));
  assert.deepEqual(host.sent.at(-1), { type: "oskiewar:reload", content: {} });
  // A second nudge inside the five-second window is swallowed.
  const delivered = host.sent.length;
  now += 3000;
  agent.emit("message", Buffer.from(reload));
  assert.equal(host.sent.length, delivered);
  now += 5001;
  agent.emit("message", Buffer.from(reload));
  assert.deepEqual(host.sent.at(-1), { type: "oskiewar:reload", content: {} });
  // Anything else an agent says — and anything a viewer says — is dropped.
  const settled = host.sent.length;
  agent.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state" })));
  viewer.emit("message", Buffer.from(reload));
  assert.equal(host.sent.length, settled);
});

// The render experiment lane: an agent flips one layer of the picture off,
// reads the fps out of the telemetry, and restores it — so what each layer
// costs is measured on the machine that is struggling. The relay forwards
// only a closed dictionary shape and no faster than four a second.
test("an agent can pass render experiment flags, shaped and paced", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), agent = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher&surface=web` });
  manager.handleConnection(agent, { url: `${url}&role=agent` });
  const flags = (content) => agent.emit("message",
    Buffer.from(JSON.stringify({ type: "oskiewar:flags", content })));
  flags({ grass: false, bands: 1 });
  assert.deepEqual(host.sent.at(-1),
    { type: "oskiewar:flags", content: { grass: false, bands: 1 } });
  // Paced: a second batch inside 250ms is swallowed, after it flows.
  const delivered = host.sent.length;
  now += 100;
  flags({ sky: false });
  assert.equal(host.sent.length, delivered);
  now += 251;
  flags({ sky: false });
  assert.deepEqual(host.sent.at(-1),
    { type: "oskiewar:flags", content: { sky: false } });
  // Shaped: bad names, huge numbers, strings and empty batches all die here.
  const settled = host.sent.length;
  now += 1000;
  flags({ "Bad-Name": true });
  flags({ grass: "off" });
  flags({ bands: 1e9 });
  flags({});
  assert.equal(host.sent.length, settled);
});

// A room packed with spectators must never lock a maintainer out of the
// telemetry, and a stuck agent must never eat the spectator allowance.
test("agents hold their own small allowance beside the 64 viewer seats", () => {
  const manager = new OskiewarLiveManager();
  const host = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  const agents = Array.from({ length: OSKIEWAR_LIVE_LIMITS.MAX_AGENTS },
    () => new FakeSocket());
  for (const agent of agents)
    manager.handleConnection(agent, { url: `${url}&role=agent` });
  assert.ok(agents.every((agent) => agent.closed === null));
  const extra = new FakeSocket();
  manager.handleConnection(extra, { url: `${url}&role=agent` });
  assert.equal(extra.closed?.code, 4429);
  // The grandstand is untouched by a full agent bench.
  const viewer = new FakeSocket();
  manager.handleConnection(viewer, { url });
  assert.deepEqual(host.sent.at(-1).content,
    { count: 1, agents: OSKIEWAR_LIVE_LIMITS.MAX_AGENTS });
});

// A phone that scanned the QR sends no role at all, and a client built against
// a later vocabulary must not be turned away at the door.
test("an unrecognized role still watches as an ordinary spectator", () => {
  const manager = new OskiewarLiveManager();
  const host = new FakeSocket(), stranger = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(stranger, { url: `${url}&role=telemetry-probe` });
  assert.equal(stranger.closed, null);
  assert.deepEqual(host.sent.at(-1).content, { count: 1, agents: 0 });
});

// The frame numbers are the reason an agent connects at all, and they land on a
// public feed, so the block is a closed list of bounded numbers.
test("published frame timing is bounded and closed to unknown keys", () => {
  assert.equal(validateOskiewarLiveState({ ...state(),
    perf: { fps: 59, frameMs: 16.72, renderMs: 3.41, hz: 60 } }), null);
  assert.equal(validateOskiewarLiveState({ ...state(), perf: { fps: 59 } }), null);
  assert.equal(validateOskiewarLiveState({ ...state(),
    perf: { fps: 59, handle: "@jeffrey" } }), "Invalid performance");
  assert.equal(validateOskiewarLiveState({ ...state(), perf: { fps: 1000000 } }),
    "Invalid performance");
  assert.equal(validateOskiewarLiveState({ ...state(), perf: [59] }),
    "Invalid performance");
});

test("projectiles and impacts ride out bounded, and a full frame still fits", () => {
  const shot = [820, -140, 0, 750, -140, 4200, 0, 1, 4];
  const lob = [910, -80, 0, 1950, -400, 0, 1, 0, 1150];
  assert.equal(validateOskiewarLiveState({ ...state(), shots: [], lobs: [] }), null);
  assert.equal(validateOskiewarLiveState({ ...state(),
    shots: Array.from({ length: 24 }, () => shot),
    lobs: Array.from({ length: 12 }, () => lob) }), null);
  // The caps are the game's own, and a row is an exact shape: a watcher
  // positions render objects off these numbers without re-checking them.
  assert.equal(validateOskiewarLiveState({ ...state(),
    shots: Array.from({ length: 25 }, () => shot) }), "Invalid shots");
  assert.equal(validateOskiewarLiveState({ ...state(),
    shots: [shot.slice(0, 8)] }), "Invalid shots");
  assert.equal(validateOskiewarLiveState({ ...state(),
    shots: [[...shot.slice(0, 8), 8]] }), "Invalid shots");
  assert.equal(validateOskiewarLiveState({ ...state(),
    shots: [[...shot.slice(0, 7), 2, 4]] }), "Invalid shots");
  assert.equal(validateOskiewarLiveState({ ...state(),
    shots: [[NaN, ...shot.slice(1)]] }), "Invalid shots");
  assert.equal(validateOskiewarLiveState({ ...state(),
    lobs: Array.from({ length: 13 }, () => lob) }), "Invalid lobs");
  assert.equal(validateOskiewarLiveState({ ...state(),
    lobs: [[...lob.slice(0, 7), -1, 1150]] }), "Invalid lobs");
  // The host's impact track, whose ids are what let a watcher tell a repeated
  // mark from a new one — so an id has to be a real ascending integer, not
  // any finite number.
  const mark = [9999, 5200, 11940, 0, 550, 520, 1];
  assert.equal(validateOskiewarLiveState({ ...state(),
    impacts: Array.from({ length: 12 }, () => mark) }), null);
  assert.equal(validateOskiewarLiveState({ ...state(),
    impacts: Array.from({ length: 13 }, () => mark) }), "Invalid impacts");
  assert.equal(validateOskiewarLiveState({ ...state(),
    impacts: [[1.5, ...mark.slice(1)]] }), "Invalid impacts");
  assert.equal(validateOskiewarLiveState({ ...state(),
    impacts: [[...mark.slice(0, 6), 4]] }), "Invalid impacts");
  assert.equal(validateOskiewarLiveState({ ...state(),
    impacts: [mark.slice(0, 6)] }), "Invalid impacts");
  // A full sky is the frame the relay has to carry, so it is measured here
  // rather than discovered in play: the size cap turns a busy fight away
  // silently, and a watcher would see the projectiles stop, not an error.
  const full = JSON.stringify({ ...state(),
    shots: Array.from({ length: 24 }, () => shot),
    lobs: Array.from({ length: 12 }, () => lob),
    impacts: Array.from({ length: 12 }, () => mark) });
  assert.ok(Buffer.byteLength(full) < 8192,
    `a full sky is ${Buffer.byteLength(full)} bytes`);
});

test("the rollback lane's packets pass seat to seat and never reach the grandstand", () => {
  const manager = new OskiewarLiveManager({ now: () => 1000 });
  const host = new FakeSocket(), rival = new FakeSocket(), fan = new FakeSocket();
  const url = "/oskiewar-live?match=sezzi7";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(rival, { url: `${url}&role=challenger` });
  manager.handleConnection(fan, { url });
  host.sent.length = rival.sent.length = fan.sent.length = 0;
  const packet = (content) => Buffer.from(JSON.stringify({ type: "oskiewar:net", content }));
  rival.emit("message", packet({ t: "i", f: 12, m: [3, 3, 0], a: 9, s: 14 }));
  assert.deepEqual(host.sent, [{ type: "oskiewar:net",
    content: { t: "i", f: 12, m: [3, 3, 0], a: 9, s: 14 } }]);
  host.emit("message", packet({ t: "start", origin: 0, delay: 2 }));
  assert.deepEqual(rival.sent, [{ type: "oskiewar:net",
    content: { t: "start", origin: 0, delay: 2 } }]);
  assert.equal(fan.sent.length, 0, "spectators never hear the seats talk");
  // The grandstand has no voice on this channel, and neither seat may send
  // junk or a payload past the cap.
  fan.emit("message", packet({ t: "i" }));
  rival.emit("message", packet("bent"));
  rival.emit("message", packet({ pad: "x".repeat(OSKIEWAR_LIVE_LIMITS.MAX_NET_BYTES) }));
  assert.equal(host.sent.length, 1);
  assert.equal(rival.sent.length, 1);
  // Pads still ride the same socket under their own tighter cap.
  rival.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:input",
    content: { seq: 1, down: ["A"], leftX: 0, leftY: 0 } })));
  assert.equal(host.sent.length, 2);
  assert.equal(host.sent[1].type, "oskiewar:input");
});
