import test from "node:test";
import assert from "node:assert/strict";
import { paintingFrame } from "../public/aesthetic.computer/lib/painting-playback.mjs";
import { createNoPaintRecording } from "../public/aesthetic.computer/lib/nopaint-recording.mjs";
import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer } from
  "../public/aesthetic.computer/lib/nopaint-pieces.mjs";
import { encodePaintingState } from "../public/aesthetic.computer/lib/painting-state.mjs";
import { paste, setBuffer } from "../public/aesthetic.computer/lib/graph.mjs";
import * as viewer from "../public/aesthetic.computer/disks/wip.mjs";

function sample() {
  let piece = createNoPaintPiece({ seed: "playback", width: 2, height: 2,
    pixels: new Uint8ClampedArray(16).fill(255) });
  for (const color of [60, 180]) {
    const pixels = new Uint8ClampedArray(16).fill(color);
    piece = appendNoPaintLayer(piece, createNoPaintProposalLayer({ piece,
      proposal: { kind: "test" }, proposalNumber: piece.layers.length, pixels, pixelMode: "composite" }), pixels);
  }
  return piece;
}

test("seeking reconstructs cropped translucent layers and size changes without mutating saved pixels", () => {
  let piece = sample();
  const overlay = new Uint8ClampedArray(16);
  overlay.set([255, 0, 0, 128], 12);
  piece = appendNoPaintLayer(piece, createNoPaintProposalLayer({ piece,
    proposal: { kind: "overlay" }, proposalNumber: 3, pixels: overlay }), piece.composite.pixels);
  const resized = createNoPaintPiece({ seed: "resized", width: 1, height: 1,
    pixels: new Uint8ClampedArray([1, 2, 3, 255]) });
  piece.layers.push(resized.layers[0]);
  const original = structuredClone(piece);
  const screen = { width: 1, height: 1, pixels: new Uint8ClampedArray(4) };
  let target;
  const api = { screen, paste, flatten() {}, num: { timestamp: () => "test" },
    page(buffer) { target = buffer; setBuffer(buffer); return { paste }; } };
  const recording = createNoPaintRecording(api, piece);
  for (const i of [3, 0, 4, 2, 1, 3]) {
    assert.deepEqual(paintingFrame(api, piece, i), recording[i].painting);
    assert.equal(target, screen, "drawing target remains the display");
  }
  assert.deepEqual(piece, original);
});

async function fixture(t) {
  let source = sample(), now = 0, refresh, encoded = await encodePaintingState(source);
  let metadata = { revision: 1, status: "wip", canEdit: false };
  let ready;
  const painted = [], labels = [], jumps = [];
  class TextButton {
    constructor(text) { this.text = text; this.btn = { box: {} }; }
    reposition(_position, text) { if (text) this.text = text; }
    paint() { labels.push(this.text); }
    act(event, push) { if (!this.disabled && event.button === this.text) push(); }
  }
  const api = { params: ["test"], ui: { TextButton }, screen: { width: 195, height: 422 },
    store: { retrieve: async () => ({}) }, hud: { label() {} },
    net: { getToken: async () => null, rewrite() {} }, jump: url => jumps.push(url),
    wipe() {}, paste: frame => painted.push(frame),
    ink() { return { write(text) { labels.push(text); } }; }, needsPaint() { ready?.(); } };
  t.mock.method(Date, "now", () => now);
  t.mock.method(globalThis, "setTimeout", (callback, delay) => {
    assert.equal(delay, 2000); refresh = callback; return 1;
  });
  t.mock.method(globalThis, "clearTimeout", () => {});
  t.mock.method(globalThis, "fetch", async (_url, options) => {
    const request = JSON.parse(options.body);
    return { ok: true, json: async () => ({ ...metadata, ...(request.state ? { state: encoded } : {}) }) };
  });
  const wait = () => new Promise(resolve => { ready = resolve; });
  const loaded = wait();
  viewer.boot(api);
  t.after(() => viewer.leave());
  await loaded;
  const render = () => { painted.length = labels.length = 0; viewer.paint(api); return painted.at(-1); };
  return { labels, jumps, source, render,
    advance(ms) { now += ms; viewer.sim(api); return render(); },
    key(name) { viewer.act({ ...api, event: { is: value => value === `keyboard:down:${name}` } }); return render(); },
    click(button) { viewer.act({ ...api, event: { button, is: () => false } }); return render(); },
    async update(nextSource, changes = {}) {
      source = nextSource; encoded = await encodePaintingState(source);
      metadata = { ...metadata, revision: metadata.revision + 1, ...changes };
      const loaded = wait(); await refresh(); await loaded; return render();
    },
  };
}

test("WIP plays saved steps, loops, pauses, seeks, and preserves a paused step during updates", async t => {
  const f = await fixture(t);
  assert.equal(f.render().pixels[0], 255, "starts with the initial canvas");
  assert.equal(f.advance(1499).pixels[0], 255);
  assert.equal(f.advance(1).pixels[0], 60);
  assert.equal(f.advance(1500).pixels[0], 180);
  assert.equal(f.advance(2499).pixels[0], 180, "holds the latest step longer");
  assert.equal(f.advance(1).pixels[0], 255, "loops to the start");
  f.click("Pause");
  assert.equal(f.advance(10000).pixels[0], 255);
  assert.equal(f.key("arrowright").pixels[0], 60);
  await f.update(f.source, { canEdit: true });
  assert.equal(f.render().pixels[0], 60, "polling does not reset the chosen step");
  assert.ok(f.labels.includes("Play"));
  f.click("Edit");
  assert.deepEqual(f.jumps, ["nopaint~resume~test"]);
  assert.equal(f.key("arrowright").pixels[0], 180);
  assert.equal(f.key("space").pixels[0], 255, "playing from the end restarts");
  f.click("Pause");
  assert.equal(f.click(">").pixels[0], 60);
  const shortened = { ...f.source, layers: [f.source.layers[0]], composite: f.source.layers[0].pixels };
  await f.update(shortened);
  assert.equal(f.render().pixels[0], 255, "undo to one layer clamps the selected step");
  assert.ok(!f.labels.includes("Play"), "a single frame has no playback controls");
});
