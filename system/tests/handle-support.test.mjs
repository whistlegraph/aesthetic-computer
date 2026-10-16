import test from "node:test";
import assert from "node:assert/strict";
import { boot, paint, act, leave } from "../public/aesthetic.computer/disks/handle.mjs";

test("handle previews survive checkout, paid saves work, and cancellation closes editing", async t => {
  const palette = [{ r: 1, g: 2, b: 3 }, { r: 4, g: 5, b: 6 }];
  t.mock.method(globalThis, "fetch", async () => ({ json: async () => ({ colors: palette }) }));
  let paid = false, unavailable = false, revoked = false, persisted, destination, deleted = 0;
  const posts = [], labels = [];
  const store = { handle: "x", retrieve: async () => store["handle:color-preview"],
    persist: key => { persisted = structuredClone(store[key]); }, delete: async key => { delete store[key]; deleted++; } };
  const net = { userRequest: async (method, path, body) => {
    if (method === "GET") return unavailable ? { status: 503 } : { status: 200, monthlySupporter: paid };
    posts.push(body); return revoked ? { status: 403, code: "monthly_support_required" } : { status: 200 };
  } };
  class TextButton {
    constructor(text) { this.text = text; }
    reposition() {}
    paint() { labels.push(this.text); }
    act(event, callback) { if (event.button === this.text) callback(); }
  }
  const pen = { write() { return pen; }, box() { return pen; }, line() { return pen; } };
  const api = { store, net, help: { repeat() {} }, sound: { synth() {} },
    jump: url => { destination = url; }, screen: { width: 400, height: 600 },
    ink: () => pen, wipe() {}, write() {}, ui: { TextButton }, query: { source: "signup" } };
  const settle = () => new Promise(resolve => setImmediate(resolve));
  const render = () => { labels.length = 0; paint(api); };
  const click = button => act({ ...api, event: { button, is: () => false, x: -1, y: -1 } });
  try {
    await boot(api); await settle(); render();
    assert.ok(labels.includes("Support monthly"));
    click("Support monthly");
    assert.equal(posts.length, 0);
    assert.deepEqual(persisted.colors, palette);
    assert.equal(destination, "https://aesthetic.computer/support.html?source=signup");

    store["handle:color-preview"].colors[0] = { r: 100, g: 101, b: 102 };
    paid = true; leave(); await boot(api); await settle(); render();
    assert.ok(labels.includes("Save")); click("Save"); await settle();
    assert.deepEqual(posts[0].colors[0], { r: 100, g: 101, b: 102 });
    assert.equal(deleted, 1);

    leave(); await boot(api); await settle(); render();
    revoked = true; click("Save"); await settle(); render();
    assert.ok(labels.includes("Support monthly"));

    unavailable = true; leave(); await boot(api); await settle(); render();
    assert.ok(labels.includes("Check support"));
    click("Check support"); await settle();
    assert.equal(posts.length, 2);
  } finally { leave(); }
});
