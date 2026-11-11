import test from 'node:test';
import assert from 'node:assert/strict';

async function reel(t, count = 20, deferPosters = false) {
  let now = 0;
  t.mock.method(performance, 'now', () => now);
  const posters = [];
  let releasePosters;
  const posterGate = new Promise(resolve => { releasePosters = resolve; });
  t.mock.method(globalThis, 'fetch', async url => String(url).startsWith('/api/mime')
    ? {ok: true, json: async () => ({hasMore: false, recent: Array.from({length: count}, (_, i) => ({
      code: `kidlisp_${i}`, media: {kind: 'kidlisp', code: `c${i}`},
    }))})}
    : {ok: true, blob: async () => ({})});
  t.mock.method(globalThis, 'createImageBitmap', async () => {
    if (deferPosters) await posterGate;
    const bitmap = {width: 32, height: 100, closed: false, close() { this.closed = true; }};
    posters.push(bitmap);
    return bitmap;
  });
  const m = await import(`../system/public/aesthetic.computer/lib/mime-reel.mjs?test=${Math.random()}`);
  const jumps = [], warmed = [];
  m.start({send() {}, needsPaint() {}, preload: async address => warmed.push(address),
    jump(address, ready) { jumps.push(address); ready(); }});
  const flush = () => new Promise(resolve => setImmediate(resolve));
  await flush();
  const writes = [];
  const overlays = {};
  const $ = {screen: {width: 32, height: 100, pixels: new Uint8ClampedArray(32 * 100 * 4)},
    ink() { return this; }, write(text, position) { writes.push({text, ...position}); return this; }};
  const loaded = (finish = true) => { m.loadStarted(); m.hold(true, $.screen); if (finish) { now += 200; if (m.busy()) m.draw($, overlays); m.hold(true, $.screen); } };
  m.draw($); loaded(); writes.length = 0;
  const step = (ms = 16) => { now += ms; if (m.busy()) m.draw($, overlays); };
  const settle = () => { for (let i = 0; i < 60; i++) step(); };
  return {m, jumps, warmed, posters, loaded, step, settle, flush, $, overlays, writes, releasePosters};
}

// Node may not expose this browser API.
if (!globalThis.createImageBitmap) globalThis.createImageBitmap = async () => {};

test('neighbors are warmed before the next swipe and decoded posters stay bounded', async t => {
  const r = await reel(t);
  assert.deepEqual(r.warmed, ['$c1', '$c2']);
  for (let i = 0; i < 12; i++) {
    r.m.key({name: 'keyboard:down', key: 'PageDown'});
    r.settle(); r.loaded(); await r.flush();
  }
  assert.equal(r.jumps.length, 13);
  assert.ok(r.posters.filter(p => !p.closed).length <= 6);
  assert.ok(r.posters.some(p => p.closed));
  assert.equal(new Set(r.warmed).size, r.warmed.length);
});

test('trackpad momentum settles one card and a fresh gesture advances again', async t => {
  const r = await reel(t);
  r.m.scroll({y: 60});
  for (let i = 0; i < 60; i++) {
    r.step();
    r.m.scroll({y: 8});
    if (r.jumps.length === 2) r.loaded(false);
  }
  assert.deepEqual(r.jumps, ['$c0', '$c1']);
  assert.equal(r.m.busy(), false);
  r.step(160);
  r.m.scroll({y: 60}); r.settle();
  assert.deepEqual(r.jumps, ['$c0', '$c1', '$c2']);
});

test('a short drag cancels and a subsequent flick advances', async t => {
  const r = await reel(t);
  const pen = (name, y) => r.m.pen({name, x: 10, y, isPrimary: true}, () => {});
  pen('touch', 80); r.step(100); pen('draw', 60); r.step(200); pen('lift', 60);
  r.settle();
  assert.equal(r.jumps.length, 1);
  pen('touch', 80); r.step(16); pen('draw', 60); r.step(16); pen('lift', 60);
  r.settle();
  assert.deepEqual(r.jumps, ['$c0', '$c1']);
});


test('the HUD travels with its card, without stationary overlays or duplicate captions', async t => {
  const r = await reel(t);
  const rgba = [230, 90, 40, 255];
  r.$.screen.pixels.fill(0);
  const img = {width: 1, height: 1, pixels: new Uint8ClampedArray(rgba)};
  Object.assign(r.overlays, {label: {x: 2, y: 40, img}, qrOverlay: {x: 24, y: 90, img}, loading: true});
  r.m.pen({name: 'touch', x: 10, y: 80, isPrimary: true}, () => {});
  r.m.pen({name: 'draw', x: 10, y: 60, isPrimary: true}, () => {});
  r.step();
  const pixel = (x, y) => Array.from(r.$.screen.pixels.slice((y * 32 + x) * 4, (y * 32 + x + 1) * 4));
  assert.deepEqual(pixel(2, 20), rgba);
  assert.deepEqual(pixel(24, 70), rgba);
  assert.deepEqual(pixel(2, 40), [0, 0, 0, 0]);
  assert.deepEqual(r.overlays, {loading: true});
  assert.deepEqual(Array.from(img.pixels), rgba);
  assert.deepEqual(r.writes, [], 'neither captured HUD nor offscreen neighbor needs an extra caption');
  r.step(200);
  r.m.pen({name: 'lift', x: 10, y: 60, isPrimary: true}, () => {});
  r.settle();
  assert.equal(r.m.hold(true, r.$.screen), false);
  assert.ok(r.$.screen.pixels.every(value => value === 0), 'cancel restores piece pixels without baked-in HUD');
});

test('a poster arriving during a swipe cannot replace its preview at the loading handoff', async t => {
  const original = globalThis.OffscreenCanvas;
  globalThis.OffscreenCanvas = class {
    constructor(w, h) { this.w = w; this.h = h; }
    getContext() { return {drawImage() {}, getImageData: () => ({data: new Uint8ClampedArray(this.w * this.h * 4).fill(200)})}; }
  };
  t.after(() => { if (original) globalThis.OffscreenCanvas = original; else delete globalThis.OffscreenCanvas; });
  const r = await reel(t, 3, true);
  r.m.pen({name: 'touch', x: 10, y: 80, isPrimary: true}, () => {});
  r.m.pen({name: 'draw', x: 10, y: 20, isPrimary: true}, () => {});
  r.step();
  const pixel = () => Array.from(r.$.screen.pixels.slice(-4));
  assert.deepEqual(pixel(), [23, 21, 38, 255]);
  r.releasePosters(); await r.flush();
  assert.ok(r.posters.length > 0);
  r.step();
  assert.deepEqual(pixel(), [23, 21, 38, 255]);
  r.m.pen({name: 'lift', x: 10, y: 20, isPrimary: true}, () => {});
  r.settle();
  assert.deepEqual(r.jumps, ['$c0', '$c1']);
  assert.deepEqual(pixel(), [23, 21, 38, 255]);
});


test('the preview blends into the live frame and its HUD, then restores clean piece pixels', async t => {
  const r = await reel(t);
  r.m.key({name: 'keyboard:down', key: 'PageDown'}); r.settle();
  r.m.loadStarted();
  assert.equal(r.m.hold(true, r.$.screen), false);
  r.$.screen.pixels.fill(100);
  const img = {width: 1, height: 1, pixels: new Uint8ClampedArray([240, 240, 240, 255])};
  r.overlays.label = {x: 0, y: 0, img};
  r.step(0);
  assert.deepEqual(Array.from(r.$.screen.pixels.slice(0, 4)), [23, 21, 38, 255]);
  assert.equal(r.overlays.label, undefined);
  r.m.hold(true, r.$.screen);
  assert.equal(r.$.screen.pixels[0], 100);
  r.overlays.label = {x: 0, y: 0, img};
  r.step(90);
  assert.ok(r.$.screen.pixels[0] > 23 && r.$.screen.pixels[0] < 240);
  r.m.hold(true, r.$.screen);
  r.overlays.label = {x: 0, y: 0, img};
  r.step(90);
  assert.deepEqual(Array.from(r.$.screen.pixels.slice(0, 4)), [240, 240, 240, 255]);
  assert.equal(r.m.busy(), false);
  r.m.hold(true, r.$.screen);
  assert.ok(r.$.screen.pixels.every(value => value === 100));
});

test('a fresh flick inside the momentum tail advances without waiting for it to die', async t => {
  const r = await reel(t);
  r.m.scroll({y: 60});
  for (let i = 0; i < 20; i++) {
    r.step();
    r.m.scroll({y: Math.max(1, 30 - i * 2)}); // a fading tail
    if (r.jumps.length === 2) r.loaded(false);
  }
  assert.deepEqual(r.jumps, ['$c0', '$c1']);
  // No rest: the fingers come down again while the tail is still running.
  r.step(); r.m.scroll({y: 60});
  for (let i = 0; i < 60; i++) {
    r.step();
    r.m.scroll({y: Math.max(1, 30 - i * 2)});
    if (r.jumps.length === 3) r.loaded(false);
  }
  assert.deepEqual(r.jumps, ['$c0', '$c1', '$c2']);
});
