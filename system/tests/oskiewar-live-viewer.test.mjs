import test from "node:test";
import assert from "node:assert/strict";

const sockets = [];
class FakeWebSocket {
  constructor(url) { this.url = url; sockets.push(this); }
  close() { this.onclose?.(); }
}
globalThis.location = { hostname: "aesthetic.computer" };
globalThis.WebSocket = FakeWebSocket;
globalThis.fetch = async () => ({ ok: false, async json() { return {}; } });
let replacedPath = "";
globalThis.history = { replaceState(_state, _title, path) { replacedPath = path; } };

const viewer = await import("../public/aesthetic.computer/disks/oskiewar.mjs");

const state = {
  format: "ac.oskiewar.live", version: 1, seq: 1, at: Date.now(), phase: "fight",
  fighters: [
    { name: "@JEFFREY", color: [190, 42, 58], x: 2000, y: 12000, z: 0,
      facing: 1, alive: true, grounded: true, ducking: false, blocking: false,
      score: 2, roundWins: 1, attack: "" },
    { name: "@OSKIE", color: [38, 82, 176], x: 10000, y: 12000, z: 0,
      facing: -1, alive: true, grounded: true, ducking: false, blocking: false,
      score: 1, roundWins: 0, attack: "KICK" },
  ],
  ball: { active: true, x: 6000, y: 11945, z: 0, radius: 55 },
  camera: { x: 6000, y: 9125, width: 12000 },
  round: { remainingMs: 24000, result: "" },
};

test("colon URL joins the match room and paints both contained fighters", () => {
  const draws = [];
  viewer.boot({ colon: ["bafegu-dorimi-kunapo"], wipe() {} });
  const socket = sockets.at(-1);
  assert.equal(socket.url,
    "wss://session-server.aesthetic.computer/oskiewar-live?match=ow-bafegu-dorimi-kunapo");
  socket.onopen();
  socket.onmessage({ data: JSON.stringify({ type: "oskiewar:state", content: state }) });
  viewer.sim();
  const chain = {
    write(text, position) { draws.push(["text", text, position.x, position.y]); return chain; },
    box(x, y, width, height) { draws.push(["box", x, y, width, height]); return chain; },
    line(x1, y1, x2, y2) { draws.push(["line", x1, y1, x2, y2]); return chain; },
    circle(x, y, radius) { draws.push(["circle", x, y, radius]); return chain; },
  };
  viewer.paint({ wipe() {}, ink() { return chain; },
    line(x1, y1, x2, y2) { draws.push(["line", x1, y1, x2, y2]); },
    circle(x, y, radius) { draws.push(["circle", x, y, radius]); },
    screen: { width: 256, height: 480 } });
  const geometry = draws.filter(([type]) => type === "line" || type === "circle");
  assert.ok(geometry.length >= 10);
  assert.ok(geometry.every((entry) => entry[0] === "line"
    ? entry[1] >= -1 && entry[1] <= 257 && entry[3] >= -1 && entry[3] <= 257
    : entry[1] >= -1 && entry[1] <= 257));
  viewer.leave();
});

test("a round transition swaps the raw URL and joins the next live room", () => {
  viewer.boot({ colon: ["bafegu-dorimi-kunapo"], wipe() {} });
  const socket = sockets.at(-1);
  socket.onmessage({ data: JSON.stringify({ type: "oskiewar:state",
    content: { ...state, nextRoundId: "ow-fagori-buneta-kovisu" } }) });
  assert.equal(replacedPath, "/fagori-buneta-kovisu");
  assert.match(sockets.at(-1).url, /match=ow-fagori-buneta-kovisu$/);
  viewer.leave();
});

test("a stored round demo interpolates its recorded checkpoints", () => {
  const row = (tick, x) => {
    const values = Array(26).fill(0);
    values[0] = tick;
    values[1] = x;
    values[2] = 12000;
    values[6] = 3;
    values[9] = 10000 - x;
    values[10] = 12000;
    values[14] = 3;
    values[17] = 6000;
    values[18] = 11945;
    values[22] = 1;
    values[23] = 6000;
    values[24] = 9140;
    values[25] = 12000;
    return values;
  };
  const frame = viewer.demoSnapshot({ tickRate: 60, durationTicks: 60,
    roundIndex: 0, rounds: [[0, 1, 12, 2]], fighters: ["@JEFFREY", "@OSKIE"],
    checkpoints: [row(0, 2000), row(60, 4000)], events: [] }, 500);
  assert.equal(frame.phase, "replay");
  assert.equal(frame.fighters[0].x, 3000);
  assert.equal(frame.ball.active, true);
});
