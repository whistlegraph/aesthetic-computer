import test from 'node:test';
import assert from 'node:assert/strict';
import { createTextures, keyOf, nameOf, parseName, tierOf } from '../textures.mjs';

const flush = () => new Promise((r) => setTimeout(r, 0));
function rig(over = {}) {
  const sent = [];
  let clock = 0;
  const host = { texture: (layer, p) => (sent.push({ layer, w: p.width, h: p.height }), p.width * p.height * 4) };
  const steps = new Map();
  const loaders = {
    kidlisp: async (code, { width, height }) => ({ width, height, step: () => (steps.set(code, (steps.get(code) || 0) + 1), new Uint8ClampedArray(width * height * 4)) }),
    painting: async (code, size) => ({ width: size, height: size, pixels: new Uint8ClampedArray(size * size * 4) }),
    image: async () => ({ width: 8, height: 8, pixels: new Uint8ClampedArray(256) }),
  };
  const t = createTextures({ host, loaders, now: () => clock, ...over });
  return { t, sent, steps, advance: (ms) => (clock += ms), at: () => clock };
}

test('names say what makes them', () => {
  assert.deepEqual(parseName('#abc'), { kind: 'painting', ref: 'abc', still: true, size: 0 });
  assert.deepEqual(parseName('$xyz'), { kind: 'kidlisp', ref: 'xyz', still: false, size: 0 });
  assert.deepEqual(parseName('$xyz:still:64'), { kind: 'kidlisp', ref: 'xyz', still: true, size: 64 });
  assert.equal(parseName('url:https://a.b/c.png').ref, 'https://a.b/c.png');
  assert.equal(keyOf('$xyz'), keyOf('$xyz'));
  assert.equal(nameOf(keyOf('#abc')), '#abc');
  assert.equal(tierOf(1).size, 256);
  assert.equal(tierOf(100).size, 16);
});

test('asked for, a painting loads and is sent once to a layer', async () => {
  const { t, sent } = rig();
  const k = keyOf('#paint');
  assert.equal(t.resolve(k), -1);
  await flush();
  t.tick(0);
  assert.equal(sent.length, 1);
  assert.ok(t.resolve(k) >= 0);
  t.tick(16); t.tick(32);
  assert.equal(sent.length, 1, 'a still picture is not sent again');
});

test('a running piece steps at its tier\'s rate; a near one is bigger and faster', async () => {
  const { t, steps, advance, at } = rig();
  const near = keyOf('$near'), far = keyOf('$far');
  t.resolve(near); t.resolve(far); t.hint(near, 1); t.hint(far, 60);
  await flush();
  for (let i = 0; i < 60; i++) { t.resolve(near); t.resolve(far); t.hint(near, 1); t.hint(far, 60); t.tick(at()); advance(1000 / 60); }
  await flush();
  for (let i = 0; i < 60; i++) { t.resolve(near); t.resolve(far); t.hint(near, 1); t.hint(far, 60); t.tick(at()); advance(1000 / 60); }
  const n = steps.get('near'), f = steps.get('far');
  assert.ok(n > 20 && n <= 31, `near stepped ${n} in a second (30 Hz)`);
  assert.ok(f >= 1 && f <= 3, `far stepped ${f} (2 Hz)`);
});

test(':still runs a moment, then holds and costs nothing', async () => {
  const { t, steps, advance, at } = rig();
  const k = keyOf('$held:still');
  t.resolve(k);
  await flush();
  for (let i = 0; i < 100; i++) { t.resolve(k); t.tick(at()); advance(16); }
  assert.equal(steps.get('held'), 30);
});

test('more textures than layers: the least lately used gives its layer up', async () => {
  const { t } = rig({ layers: 2 });
  const [a, b, c] = ['#a', '#b', '#c'].map(keyOf);
  t.resolve(a); t.resolve(b);
  await flush();
  t.tick(0);
  t.resolve(b); t.resolve(c); // a is not drawn this frame
  await flush();
  t.tick(16);
  assert.equal(t.resolve(a), -1);
  assert.ok(t.resolve(b) >= 0 && t.resolve(c) >= 0);
  assert.equal(t.stats().evictions, 1);
});

test('a failed source is reported, not thrown', async () => {
  const { t } = rig();
  t.resolve(keyOf('fn:nothing'));
  await flush();
  t.tick(0);
  assert.match(t.stats().failed[0], /nothing registered/);
});
