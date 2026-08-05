import test from "node:test";
import assert from "node:assert/strict";
import { RoundRoom, roundNameFromPath } from "../round-room.mjs";

class Socket {
  constructor(url) { this.url = url; this.listeners = new Map(); Socket.all.push(this); }
  addEventListener(type, listener) { this.listeners.set(type, listener); }
  send() {}
  close() { this.listeners.get("close")?.(); }
  event(type, data) { this.listeners.get(type)?.({ data }); }
}
Socket.all = [];

test("raw paths recognize only pronounceable round IDs", () => {
  assert.equal(roundNameFromPath("/sezzi7"), "sezzi7");
  assert.equal(roundNameFromPath("/shuppy652"), "shuppy652");
  assert.equal(roundNameFromPath("/bafegu-dorimi-kunapo"),
    "bafegu-dorimi-kunapo");
  assert.equal(roundNameFromPath("/bafegu-dorimi-kunapo/"),
    "bafegu-dorimi-kunapo");
  assert.equal(roundNameFromPath("/watch/bafegu-dorimi-kunapo"), "");
});

test("a live room changes its URL and socket when the next round arrives", async () => {
  const paths = [], messages = [], analytics = [];
  const room = new RoundRoom("sezzi7", {
    WebSocketImpl: Socket,
    fetchImpl: async () => ({ ok: false }),
    historyImpl: { replaceState(_state, _title, path) { paths.push(path); } },
    analytics: (action, properties) => analytics.push([action, properties]),
  });
  room.start((message) => messages.push(message));
  const first = Socket.all.at(-1);
  first.event("message", JSON.stringify({ type: "oskiewar:state", content: {
    nextRoundId: "ow-shuppy652",
  } }));
  assert.deepEqual(paths, ["/shuppy652"]);
  assert.match(Socket.all.at(-1).url, /match=ow-shuppy652&surface=web$/);
  assert.equal(messages.at(-1).type, "round");
  assert.deepEqual(analytics, [["round_followed", {
    source_system: "browser",
    surface: "web",
  }]]);
  room.stop();
});

test("a stored round is delivered to the shared game client as a demo", async () => {
  const replay = { format: "ac.oskiedemo", roundId: "ow-bafegu-dorimi-kunapo" };
  const messages = [], analytics = [];
  const room = new RoundRoom("bafegu-dorimi-kunapo", {
    WebSocketImpl: Socket,
    fetchImpl: async function () {
      assert.equal(this, globalThis);
      return { ok: true, async json() { return { replay }; } };
    },
    analytics: (action, properties) => analytics.push([action, properties]),
  });
  room.start((message) => messages.push(message));
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.equal(messages.find((message) => message.type === "demo")?.content, replay);
  assert.deepEqual(analytics, [["replay_viewed", {
    source_system: "browser",
    surface: "web",
  }]]);
  room.stop();
});

test("the first live state emits one identifier-free view milestone", () => {
  const analytics = [];
  const room = new RoundRoom("bafegu-dorimi-kunapo", {
    WebSocketImpl: Socket,
    fetchImpl: null,
    analytics: (action, properties) => analytics.push([action, properties]),
  });
  room.start(() => {});
  const socket = Socket.all.at(-1);
  for (let seq = 1; seq <= 2; seq++) {
    socket.event("message", JSON.stringify({
      type: "oskiewar:state",
      content: { seq },
    }));
  }
  assert.deepEqual(analytics, [["live_viewed", {
    source_system: "browser",
    surface: "web",
  }]]);
  room.stop();
});
