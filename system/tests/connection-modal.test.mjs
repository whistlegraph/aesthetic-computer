import test from "node:test";
import assert from "node:assert/strict";
import { createConnectionModal } from "../public/aesthetic.computer/lib/connection-modal.mjs";
import { watchConnection } from "../public/aesthetic.computer/lib/connection-status.mjs";

function fixture() {
  let clock = 1000;
  const modal = createConnectionModal(() => clock);
  return { modal, tick: ms => { clock += ms; } };
}

test("a lost connection never takes an event from the piece", () => {
  const f = fixture();
  f.modal.update("offline");
  assert.equal(f.modal.visible, true);
  assert.equal(f.modal.act({ event: { is: () => true } }), false);
});

test("the notice types itself out, counts retries, and says goodbye before it leaves", () => {
  const f = fixture();
  f.modal.update("offline");
  assert.deepEqual(f.modal.lines(), []);
  f.tick(16 * 4);
  assert.deepEqual(f.modal.lines().map(l => l.text), ["conn"]);
  f.tick(5000);
  assert.deepEqual(f.modal.lines().map(l => l.text), ["connection lost", "waiting to retry"]);
  f.modal.update("checking");
  assert.equal(f.modal.lines()[1].text, "retrying");
  f.modal.update("offline");
  assert.equal(f.modal.lines()[1].text, "no answer, retry 2 soon");
  f.modal.update("checking"); f.modal.update("online");
  assert.equal(f.modal.visible, true);
  assert.equal(f.modal.lines()[1].text, "back online");
  f.tick(2000);
  assert.equal(f.modal.visible, false);
});

test("a later outage starts its own notice from the first letter", () => {
  const f = fixture();
  f.modal.update("offline"); f.tick(5000); f.modal.update("online"); f.tick(5000);
  f.modal.update("offline");
  assert.equal(f.modal.visible, true);
  assert.deepEqual(f.modal.lines(), []);
});

test("the notice stays in the corner and paints nothing once it has gone", () => {
  const f = fixture(), boxes = [];
  const $ = { ink: () => ({ box: (...b) => boxes.push(b), write() {} }) };
  f.modal.paint($);
  assert.equal(boxes.length, 0);
  f.modal.update("offline"); f.tick(5000);
  f.modal.paint($);
  assert.ok(boxes.length >= 2);
  for (const [x, y, w, h] of boxes) assert.ok(x >= 0 && y >= 0 && x + w <= 152 && y + h <= 100);
});

test("the modal restores an accumulating drawing and never copies it into another piece", () => {
  const f = fixture(); f.modal.update("offline");
  const screen = { width: 1, height: 1, pixels: new Uint8ClampedArray([70, 50, 100, 255]) };
  f.modal.capture(screen, "painting");
  screen.pixels.fill(0);
  assert.equal(f.modal.restore(screen, "painting"), true);
  assert.deepEqual([...screen.pixels], [70, 50, 100, 255]);
  assert.equal(f.modal.restore(screen, "painting"), false);
  f.modal.capture(screen, "painting");
  screen.pixels.fill(1);
  assert.equal(f.modal.restore(screen, "notepat"), false);
  assert.deepEqual([...screen.pixels], [1, 1, 1, 1]);
});

test("a lost host is asked again without anyone pressing a button", async () => {
  const events = {}, states = [], timers = [];
  const win = { navigator: { onLine: true }, clearTimeout() {},
    setTimeout: (fn, ms) => timers.push({ fn, ms }),
    addEventListener: (name, fn) => events[name] = fn,
    fetch: async () => { throw new Error("unreachable"); },
  };
  watchConnection(win, m => states.push(m.content.status));
  events.offline();
  const waits = [];
  for (let i = 0; i < 5; i++) {
    const retry = timers.filter(t => t.ms !== 5000).at(-1); // 5000 is the probe's own timeout.
    waits.push(retry.ms);
    await retry.fn();
  }
  assert.deepEqual(waits, [2000, 4000, 8000, 15000, 15000]);
  assert.deepEqual(states.slice(0, 5), ["offline", "checking", "offline", "checking", "offline"]);
});

test("a stale successful probe cannot hide a later disconnect", async () => {
  const events = {}, states = [], requests = [];
  const win = { navigator: { onLine: true }, setTimeout: () => 0, clearTimeout() {},
    addEventListener: (name, fn) => events[name] = fn,
    fetch: (url, options) => new Promise(resolve => requests.push({ resolve, options })),
  };
  const monitor = watchConnection(win, m => states.push(m.content.status));
  const pending = monitor.retry();
  events.offline();
  requests[0].resolve({ ok: true }); await pending;
  assert.deepEqual(states, ["checking", "offline"]);
  assert.equal(requests[0].options.method, "HEAD");
  assert.equal(requests[0].options.cache, "no-store");
  const recovery = events.online();
  requests[1].resolve({ ok: true }); await recovery;
  assert.equal(states.at(-1), "online");
});

test("native disconnects and failed retries reach disk without replacing the document", async () => {
  const events = {}, states = [];
  const win = { acNativeOnline: false, navigator: { onLine: true }, setTimeout: () => 0, clearTimeout() {},
    addEventListener: (name, fn) => events[name] = fn,
    fetch: async () => { throw new Error("offline"); },
  };
  const monitor = watchConnection(win, m => states.push(m.content.status));
  await monitor.retry();
  assert.deepEqual(states, ["offline", "checking", "offline"]);
});

test("the keyboard is left alone while the connection is down", () => {
  const events = {};
  const win = { navigator: { onLine: true }, setTimeout: () => 0, clearTimeout() {},
    addEventListener: (name, fn) => events[name] = fn,
  };
  const monitor = watchConnection(win, () => {});
  events.offline();
  monitor.setModal(true);
  assert.equal(events.keydown, undefined);
});
