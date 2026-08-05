import test from "node:test";
import assert from "node:assert/strict";
import { EventEmitter } from "node:events";
import { canonicalMatchId, validateOskiewarLiveState,
  OskiewarLiveManager } from "./oskiewar-live-manager.mjs";

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
  assert.equal(canonicalMatchId("short-name"), null);
});

test("live state validation rejects unbounded or malformed values", () => {
  assert.equal(validateOskiewarLiveState(state()), null);
  assert.equal(validateOskiewarLiveState({ ...state(), fighters: [] }), "Invalid fighters");
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
