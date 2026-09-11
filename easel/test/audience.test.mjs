import assert from "node:assert/strict";
import test from "node:test";
import { Audience } from "../src/audience.mjs";

// A WebSocket's shape, without a network. Nothing here is timed — the test
// drives every transition by hand.
class FakeSocket {
  constructor() {
    this.readyState = 0;
    this.sent = [];
    this.closed = false;
  }
  open() {
    this.readyState = 1;
    this.onopen?.();
  }
  send(text) {
    this.sent.push(JSON.parse(text));
  }
  deliver(message) {
    this.onmessage?.({ data: JSON.stringify(message) });
  }
  close() {
    this.closed = true;
    this.readyState = 3;
    this.onclose?.();
  }
}

function audience(options = {}) {
  const sockets = [];
  const room = new Audience({
    channel: "jeffrey/balozo",
    socket: () => {
      const socket = new FakeSocket();
      sockets.push(socket);
      return socket;
    },
    ...options,
  });
  return { room, sockets };
}

test("it asks the server for the channel it is watching", () => {
  const { room, sockets } = audience();
  room.start();
  sockets[0].open();
  assert.deepEqual(sockets[0].sent[0], {
    type: "code-channel:info",
    content: "jeffrey/balozo",
  });
  room.close();
});

// The distinction the whole module exists to protect: no answer is not zero
// viewers, and the interface must be able to tell those apart.
test("nobody-has-answered is not the same as nobody-is-here", () => {
  const { room, sockets } = audience();
  room.start();
  sockets[0].open();
  assert.equal(room.here, null, "an unanswered query reports nothing");
  sockets[0].deliver({
    type: "code-channel:info",
    content: { channel: "jeffrey/balozo", viewers: 0 },
  });
  assert.equal(room.here, 0, "an answer of zero is a real zero");
  room.close();
});

test("it keeps the high-water mark and counts the rises", () => {
  const { room, sockets } = audience();
  const seen = [];
  room.on("change", (report) => seen.push(report.here));
  room.start();
  sockets[0].open();
  const say = (viewers) =>
    sockets[0].deliver({
      type: "code-channel:info",
      content: { channel: "jeffrey/balozo", viewers },
    });
  say(2);
  say(5);
  say(1);
  assert.equal(room.here, 1);
  assert.equal(room.peak, 5, "the peak survives people leaving");
  assert.equal(room.arrivals, 3, "only the rise from 2 to 5 counts as arrivals");
  assert.deepEqual(seen, [2, 5, 1], "a change is emitted per change, not per poll");
  room.close();
});

test("an unchanged count does not churn the interface", () => {
  const { room, sockets } = audience();
  let changes = 0;
  room.on("change", () => (changes += 1));
  room.start();
  sockets[0].open();
  for (let i = 0; i < 3; i += 1) {
    sockets[0].deliver({
      type: "code-channel:info",
      content: { channel: "jeffrey/balozo", viewers: 4 },
    });
  }
  assert.equal(changes, 1, "three identical replies are one piece of news");
  room.close();
});

test("a reply for a channel we left is ignored", () => {
  const { room, sockets } = audience();
  room.start();
  sockets[0].open();
  room.watch("jeffrey/murafi");
  sockets[0].deliver({
    type: "code-channel:info",
    content: { channel: "jeffrey/balozo", viewers: 9 },
  });
  assert.equal(room.here, null, "the old channel's count is not the new one's");
  room.close();
});

test("retargeting resets the counts, because they belonged to the old piece", () => {
  const { room, sockets } = audience();
  room.start();
  sockets[0].open();
  sockets[0].deliver({
    type: "code-channel:info",
    content: { channel: "jeffrey/balozo", viewers: 6 },
  });
  assert.equal(room.peak, 6);
  room.watch("jeffrey/murafi");
  assert.equal(room.here, null);
  assert.equal(room.peak, 0);
  assert.equal(room.arrivals, 0);
  room.close();
});

// A count from a connection that has since dropped is not current, and showing
// it would be the interface claiming to know something it no longer does.
test("a dropped connection stops claiming to know who is here", () => {
  const { room, sockets } = audience();
  room.start();
  sockets[0].open();
  sockets[0].deliver({
    type: "code-channel:info",
    content: { channel: "jeffrey/balozo", viewers: 3 },
  });
  assert.equal(room.here, 3);
  sockets[0].close();
  assert.equal(room.here, null);
  assert.equal(room.peak, 3, "but what was seen was still seen");
  room.close();
});

test("the greeting carries how many people are on Aesthetic Computer at all", () => {
  const { room, sockets } = audience();
  room.start();
  sockets[0].open();
  sockets[0].deliver({
    type: "connected",
    content: JSON.stringify({ ip: "127.0.0.1", playerCount: 10 }),
  });
  assert.equal(room.online, 10);
  assert.equal(room.here, null, "which is a different fact from this piece's audience");
  room.close();
});

test("a session with no channel yet asks nothing", () => {
  const { room, sockets } = audience({ channel: "" });
  room.start();
  sockets[0].open();
  assert.equal(sockets[0].sent.length, 0, "there is nothing to ask about yet");
  assert.equal(room.watching, false);
  room.close();
});

test("garbage on the wire is ignored rather than fatal", () => {
  const { room, sockets } = audience();
  room.start();
  sockets[0].open();
  sockets[0].onmessage({ data: "not json" });
  sockets[0].deliver({ type: "code-channel:info", content: null });
  sockets[0].deliver({ type: "something-else", content: { viewers: 4 } });
  assert.equal(room.here, null);
  room.close();
});
