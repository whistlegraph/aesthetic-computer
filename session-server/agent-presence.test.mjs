import test from "node:test";
import assert from "node:assert/strict";
import { EventEmitter } from "node:events";
import {
  AgentPresenceManager,
  canonicalRoom,
  AGENT_PRESENCE_LIMITS,
} from "./agent-presence.mjs";

class FakeSocket extends EventEmitter {
  constructor() { super(); this.readyState = 1; this.sent = []; this.closed = null; }
  send(value) { this.sent.push(JSON.parse(value)); }
  close(code, reason) { this.readyState = 3; this.closed = { code, reason }; this.emit("close"); }
}

const req = (query) => ({ url: `/agent-presence?${query}` });
const last = (ws) => ws.sent[ws.sent.length - 1];

test("room names accept handles with or without the @", () => {
  assert.equal(canonicalRoom("@jeffrey"), "jeffrey");
  assert.equal(canonicalRoom("JEFFREY"), "jeffrey");
  assert.equal(canonicalRoom("laer-klokken_2"), "laer-klokken_2");
  assert.equal(canonicalRoom(""), null);
  assert.equal(canonicalRoom("no spaces"), null);
  assert.equal(canonicalRoom("x".repeat(33)), null);
});

test("wrong path is not claimed; bad room is rejected", () => {
  const manager = new AgentPresenceManager();
  assert.equal(manager.handleConnection(new FakeSocket(), { url: "/other" }), false);
  const ws = new FakeSocket();
  assert.equal(manager.handleConnection(ws, req("room=bad room")), true);
  assert.equal(ws.closed.code, 4400);
});

test("surface hears the agent arrive and leave", () => {
  const manager = new AgentPresenceManager();
  const surface = new FakeSocket();
  manager.handleConnection(surface, req("room=@jeffrey&role=surface"));
  assert.deepEqual(last(surface).content.agents, []);
  assert.equal(last(surface).content.surfaces, 1);

  const agent = new FakeSocket();
  manager.handleConnection(agent, req("room=jeffrey&role=agent&label=claude"));
  assert.deepEqual(last(surface).content.agents, ["claude"]);
  assert.deepEqual(last(agent).content.agents, ["claude"]);

  agent.close();
  assert.deepEqual(last(surface).content.agents, []);
});

test("agent can rename itself with hello, rate-limited", () => {
  let time = 1000;
  const manager = new AgentPresenceManager({ now: () => time });
  const surface = new FakeSocket();
  const agent = new FakeSocket();
  manager.handleConnection(surface, req("room=jeffrey&role=surface"));
  manager.handleConnection(agent, req("room=jeffrey&role=agent"));
  assert.deepEqual(last(surface).content.agents, ["agent"]);

  agent.emit("message", JSON.stringify({
    type: "agent-presence:hello", content: { label: "claude" } }));
  assert.deepEqual(last(surface).content.agents, ["claude"]);

  agent.emit("message", JSON.stringify({
    type: "agent-presence:hello", content: { label: "too-soon" } }));
  assert.deepEqual(last(surface).content.agents, ["claude"]);

  time += 2000;
  agent.emit("message", JSON.stringify({
    type: "agent-presence:hello", content: { label: "fable" } }));
  assert.deepEqual(last(surface).content.agents, ["fable"]);
});

test("agent capacity is enforced apart from surfaces", () => {
  const manager = new AgentPresenceManager();
  for (let i = 0; i < AGENT_PRESENCE_LIMITS.MAX_AGENTS; i += 1) {
    const ws = new FakeSocket();
    manager.handleConnection(ws, req("room=jeffrey&role=agent"));
    assert.equal(ws.closed, null);
  }
  const extra = new FakeSocket();
  manager.handleConnection(extra, req("room=jeffrey&role=agent"));
  assert.equal(extra.closed.code, 4429);
  const surface = new FakeSocket();
  manager.handleConnection(surface, req("room=jeffrey&role=surface"));
  assert.equal(surface.closed, null);
});

test("empty rooms prune after the TTL", () => {
  let time = 1000;
  const manager = new AgentPresenceManager({ now: () => time });
  const ws = new FakeSocket();
  manager.handleConnection(ws, req("room=jeffrey&role=surface"));
  ws.close();
  assert.equal(manager.rooms.size, 1);
  time += AGENT_PRESENCE_LIMITS.ROOM_TTL_MS + 1;
  manager.prune();
  assert.equal(manager.rooms.size, 0);
});
