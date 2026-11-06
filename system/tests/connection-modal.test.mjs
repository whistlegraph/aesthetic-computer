import test from "node:test";
import assert from "node:assert/strict";
import { createConnectionModal, connectionLayout } from "../public/aesthetic.computer/lib/connection-modal.mjs";
import { watchConnection } from "../public/aesthetic.computer/lib/connection-status.mjs";

function fixture() {
  const modal = createConnectionModal(), sent = [];
  const $ = { screen: { width: 320, height: 180 }, needsPaint() {}, send: message => sent.push(message) };
  const event = (name, extra = {}) => modal.act({ ...$, event: { ...extra, is: n => n === name } });
  return { modal, $, sent, event };
}

test("disconnect preserves the piece, dismisses without reloading, and reopens only for a new outage", () => {
  const f = fixture();
  assert.equal(f.event("keyboard:down:enter"), false);
  f.modal.update("offline");
  assert.equal(f.modal.visible, true);
  assert.equal(f.event("keyboard:down:escape"), true);
  assert.equal(f.modal.visible, false);
  f.modal.update("offline");
  assert.equal(f.modal.visible, false);
  assert.deepEqual(f.sent, []);
  f.modal.update("online");
  f.modal.update("offline");
  assert.equal(f.modal.visible, true);
});

test("retry cannot submit twice, failure allows another attempt, and recovery closes the modal", () => {
  const f = fixture(); f.modal.update("offline");
  f.event("keyboard:down:enter"); f.event("keyboard:down:enter");
  assert.deepEqual(f.sent, [{ type: "connection:retry" }]);
  f.modal.update("offline"); f.event("keyboard:down:enter");
  assert.equal(f.sent.length, 2);
  f.modal.update("online"); assert.equal(f.modal.visible, false);
});

test("pointer releases outside their button do nothing; Tab and Enter can keep playing", () => {
  const f = fixture(); f.modal.update("offline");
  const b = connectionLayout(320, 180).buttons[0];
  f.event("touch", { x: b.x + 1, y: b.y + 1 });
  f.event("lift", { x: 0, y: 0 });
  assert.equal(f.sent.length, 0);
  f.event("keyboard:down:tab"); f.event("keyboard:down:enter");
  assert.equal(f.modal.visible, false);
  assert.equal(f.sent.length, 0);
});

test("connection controls fit phone, landscape, and short Mac windows", () => {
  for (const [w, h] of [[152, 100], [187, 212], [320, 120], [700, 248]]) {
    const layout = connectionLayout(w, h);
    assert.ok(layout.y + layout.h <= h);
    for (const b of layout.buttons) {
      assert.ok(b.x >= 0 && b.x + b.w <= w);
      assert.ok(b.y >= 0 && b.y + b.h <= h);
    }
  }
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

test("a stale successful probe cannot hide a later disconnect", async () => {
  const events = {}, states = [], requests = [];
  const win = { navigator: { onLine: true }, setTimeout, clearTimeout,
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
  const win = { acNativeOnline: false, navigator: { onLine: true }, setTimeout, clearTimeout,
    addEventListener: (name, fn) => events[name] = fn,
    fetch: async () => { throw new Error("offline"); },
  };
  const monitor = watchConnection(win, m => states.push(m.content.status));
  await monitor.retry();
  assert.deepEqual(states, ["offline", "checking", "offline"]);
});

test("modal keys bypass prompt text entry while native command shortcuts remain available", () => {
  const events = {}, sent = [];
  const win = { navigator: { onLine: true }, setTimeout, clearTimeout,
    addEventListener: (name, fn, capture) => { events[name] = fn; if (name === "keydown") assert.equal(capture, true); },
  };
  const monitor = watchConnection(win, message => sent.push(message));
  let prevented = 0, stopped = 0;
  const e = { key: "Enter", preventDefault: () => prevented++, stopImmediatePropagation: () => stopped++ };
  events.keydown(e); assert.equal(sent.length, 0);
  monitor.setModal(true); events.keydown(e);
  assert.deepEqual(sent, [{ type: "connection:key", content: { key: "enter" } }]);
  assert.equal(prevented, 1); assert.equal(stopped, 1);
  events.keydown({ ...e, metaKey: true }); assert.equal(sent.length, 1);
  monitor.setModal(false); events.keydown(e); assert.equal(sent.length, 1);
});
