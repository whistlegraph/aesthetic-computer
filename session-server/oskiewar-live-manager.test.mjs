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

test("duplicate and over-rate publisher frames are dropped", () => {
  let now = 100;
  const manager = new OskiewarLiveManager({ now: () => now });
  const host = new FakeSocket(), viewer = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo";
  manager.handleConnection(host, { url: `${url}&role=publisher` });
  manager.handleConnection(viewer, { url });
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state", content: state(1) })));
  const count = viewer.sent.length;
  now += 1;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state", content: state(2) })));
  now += 30;
  host.emit("message", Buffer.from(JSON.stringify({ type: "oskiewar:state", content: state(1) })));
  assert.equal(viewer.sent.length, count);
});

test("a second live publisher cannot take over a match", () => {
  const manager = new OskiewarLiveManager();
  const first = new FakeSocket(), second = new FakeSocket();
  const url = "/oskiewar-live?match=bafegu-dorimi-kunapo&role=publisher";
  manager.handleConnection(first, { url });
  manager.handleConnection(second, { url });
  assert.equal(second.closed?.code, 4409);
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
