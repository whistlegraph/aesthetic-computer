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

// A versus room has no stored replay and 404s forever, so the 1800 ms retry
// loop held the bridge's only timer slot almost all the time. A socket closing
// inside that window asked to reconnect and was dropped in silence — which is
// how a third visitor, denied the chair and closed with 4409, wedged on "this
// match already has a challenger" and never joined the grandstand at all.
test("a denied chair still reaches the grandstand while the replay retry runs", async () => {
  const messages = [];
  let replayTries = 0;
  const room = new RoundRoom("sezzi7", {
    WebSocketImpl: Socket,
    fetchImpl: async () => { replayTries++; return { ok: false }; },
    role: "challenger",
    analytics: () => {},
  });
  room.start((message) => messages.push(message));
  const denied = Socket.all.at(-1);
  assert.match(denied.url, /role=challenger$/, "it asks for the chair first");
  // Let the replay retry take its slot, the way it does on a live versus room.
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.ok(replayTries > 0);
  assert.ok(room.timers.replay, "the replay retry is armed and holding a slot");
  // The relay turns the chair down and closes.
  denied.event("message", JSON.stringify({ type: "oskiewar:error",
    content: { message: "This match already has a challenger" } }));
  assert.equal(messages.at(-1).content.label,
    "This match already has a challenger");
  const sockets = Socket.all.length;
  denied.close();
  assert.ok(room.timers.reconnect,
    "the reconnect gets its own slot rather than being dropped");
  await new Promise((resolve) => setTimeout(resolve, 1300));
  assert.equal(Socket.all.length, sockets + 1, "and it actually reopens");
  assert.doesNotMatch(Socket.all.at(-1).url, /role=challenger/,
    "as one more face in the grandstand");
  room.stop();
  assert.equal(room.timers.reconnect, null);
  assert.equal(room.timers.replay, null);
});
