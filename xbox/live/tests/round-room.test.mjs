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

test("empty requires both a host status and a successful replay lookup", async () => {
  for (const scenario of ["empty", "live", "saved", "error"]) {
    const room = new RoundRoom("simma356", {
      WebSocketImpl: Socket,
      fetchImpl: async () => scenario === "error" ? { ok: false, status: 503 }
        : { ok: true, json: async () => ({ replay: scenario === "saved" ? {} : null }) },
    });
    room.start(() => {});
    await new Promise(resolve => setTimeout(resolve, 0));
    assert.equal(room.empty, false, "no host status yet");
    Socket.all.at(-1).event("message", JSON.stringify({
      type: "oskiewar:status", content: { live: scenario === "live" },
    }));
    assert.equal(room.empty, scenario === "empty", scenario);
    room.stop();
  }
});

// The relay keeps a line for the chair. The bridge stands in it, names
// itself, follows its place, takes the chair on the socket it holds, and
// walks to the back of the line when its turn ends — never racing a retake.
test("a seat-seeking bridge stands in the relay's line and keeps its place", async () => {
  class LineSocket extends Socket {
    constructor(url) { super(url); this.readyState = 1; this.out = []; }
    send(text) { this.out.push(JSON.parse(text)); }
  }
  const messages = [];
  const room = new RoundRoom("sezzi7", { WebSocketImpl: LineSocket,
    fetchImpl: async () => ({ ok: false, status: 404 }), role: "challenger" });
  room.start((message) => messages.push(message));
  const socket = LineSocket.all.at(-1);
  assert.match(socket.url, /&line=1&role=challenger$/);
  room.line(true, "GUEST-BAZKO");
  socket.event("open");
  assert.deepEqual(socket.out.at(-1), { type: "oskiewar:line",
    content: { join: true, name: "GUEST-BAZKO" } });
  const sent = socket.out.length;
  room.line(true, "GUEST-BAZKO");
  assert.equal(socket.out.length, sent, "an unchanged wish stays off the wire");
  const order = { host: "@JEFFREY", challenger: "@SAT", line: ["GUEST-BAZKO"] };
  socket.event("message", JSON.stringify({ type: "oskiewar:status",
    content: { live: true, challenger: true, order, place: 1 } }));
  assert.deepEqual(room.order, order);
  assert.equal(room.place, 1);
  // The chair frees: a line-keeping relay seats from the line, so the bridge
  // does not reconnect to grab it.
  const sockets = LineSocket.all.length;
  socket.event("message", JSON.stringify({ type: "oskiewar:status",
    content: { live: true, challenger: false, order: { ...order, challenger: "" }, place: 1 } }));
  assert.equal(LineSocket.all.length, sockets);
  socket.event("message", JSON.stringify({ type: "oskiewar:seat",
    content: { seat: "challenger", from: "line" } }));
  assert.equal(room.seat, "challenger");
  assert.ok(room.sendInput({ seq: 1, down: [], leftX: 0, leftY: 0 }));
  // The turn ends: stood down in place, and straight back into the line.
  socket.event("message", JSON.stringify({ type: "oskiewar:seat",
    content: { seat: "", turn: "over" } }));
  assert.equal(room.seat, "");
  assert.deepEqual(messages.at(-1).content, { seat: "", turn: "over" });
  assert.deepEqual(socket.out.at(-1), { type: "oskiewar:line",
    content: { join: true, name: "GUEST-BAZKO" } });
  room.line(false);
  assert.deepEqual(socket.out.at(-1).content, { join: false, name: "GUEST-BAZKO" });
  room.stop();
});
