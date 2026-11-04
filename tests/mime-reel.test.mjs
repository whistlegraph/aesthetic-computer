import test from 'node:test';
import assert from 'node:assert/strict';

async function reel(t, count = 20) {
  let now = 0;
  t.mock.method(performance, 'now', () => now);
  const posters = [];
  t.mock.method(globalThis, 'fetch', async url => String(url).startsWith('/api/mime')
    ? {ok: true, json: async () => ({hasMore: false, recent: Array.from({length: count}, (_, i) => ({
      code: `kidlisp_${i}`, media: {kind: 'kidlisp', code: `c${i}`},
    }))})}
    : {ok: true, blob: async () => ({})});
  t.mock.method(globalThis, 'createImageBitmap', async () => {
    const bitmap = {closed: false, close() { this.closed = true; }};
    posters.push(bitmap);
    return bitmap;
  });
  const m = await import(`../system/public/aesthetic.computer/lib/mime-reel.mjs?test=${Math.random()}`);
  const jumps = [], warmed = [];
  m.start({send() {}, needsPaint() {}, preload: async address => warmed.push(address),
    jump(address, ready) { jumps.push(address); ready(); }});
  const flush = () => new Promise(resolve => setImmediate(resolve));
  await flush();
  const $ = {screen: {width: 32, height: 100, pixels: new Uint8ClampedArray(32 * 100 * 4)},
    ink() { return this; }, write() { return this; }};
  const loaded = () => { m.loadStarted(); m.hold(true); };
  m.draw($); loaded();
  const step = (ms = 16) => { now += ms; if (m.busy()) m.draw($); };
  const settle = () => { for (let i = 0; i < 60; i++) step(); };
  return {m, jumps, warmed, posters, loaded, step, settle, flush};
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
    if (r.jumps.length === 2) r.loaded();
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
