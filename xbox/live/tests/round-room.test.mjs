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
  assert.equal(roundNameFromPath("/bafegu-dorimi-kunapo"),
    "bafegu-dorimi-kunapo");
  assert.equal(roundNameFromPath("/bafegu-dorimi-kunapo/"),
    "bafegu-dorimi-kunapo");
  assert.equal(roundNameFromPath("/watch/bafegu-dorimi-kunapo"), "");
});

test("a live room changes its URL and socket when the next round arrives", async () => {
  const paths = [], messages = [];
  const room = new RoundRoom("bafegu-dorimi-kunapo", {
    WebSocketImpl: Socket,
    fetchImpl: async () => ({ ok: false }),
    historyImpl: { replaceState(_state, _title, path) { paths.push(path); } },
  });
  room.start((message) => messages.push(message));
  const first = Socket.all.at(-1);
  first.event("message", JSON.stringify({ type: "oskiewar:state", content: {
    nextRoundId: "ow-fagori-buneta-kovisu",
  } }));
  assert.deepEqual(paths, ["/fagori-buneta-kovisu"]);
  assert.match(Socket.all.at(-1).url, /match=ow-fagori-buneta-kovisu$/);
  assert.equal(messages.at(-1).type, "round");
  room.stop();
});

test("a stored round is delivered to the shared game client as a demo", async () => {
  const replay = { format: "ac.oskiedemo", roundId: "ow-bafegu-dorimi-kunapo" };
  const messages = [];
  const room = new RoundRoom("bafegu-dorimi-kunapo", {
    WebSocketImpl: Socket,
    fetchImpl: async () => ({ ok: true, async json() { return { replay }; } }),
  });
  room.start((message) => messages.push(message));
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.equal(messages.find((message) => message.type === "demo")?.content, replay);
  room.stop();
});
