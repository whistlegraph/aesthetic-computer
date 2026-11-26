import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createKiosks, behind } from '../kiosks.mjs';

test('what is done to a panel is kept in order, and a slide is not', () => {
  const k = createKiosks({ later() {} });
  k.record('lobby', 'yard-2', 'touch', 10, 12, 0);
  k.record('lobby', 'yard-2', 'draw', 11, 12, 0);
  k.record('lobby', 'yard-2', 'lift', 10, 12, 0);
  assert.deepEqual(k.of('lobby')['yard-2'], { total: 2, events: [['touch', 10, 12, 0], ['lift', 10, 12, 0]] });
  assert.deepEqual(k.of('floor-00000000'), {});
});

test('a page does only what it has not done', () => {
  const kept = { total: 6, events: [['touch', 1, 1, 0], ['lift', 1, 1, 0], ['touch', 2, 2, 1], ['lift', 2, 2, 1]] };
  assert.deepEqual(behind(6, kept), []);
  assert.deepEqual(behind(4, kept), kept.events.slice(-2));
  assert.equal(behind(0, kept).length, 4, 'further behind than was kept: all that is kept');
  assert.deepEqual(behind(0, undefined), []);
});

test('only the latest are kept, and the count goes on', () => {
  const k = createKiosks({ limit: 4, later() {} });
  for (let n = 0; n < 10; n++) k.record('lobby', 'p', n % 2 ? 'lift' : 'touch', n, 0, 0);
  const kept = k.of('lobby').p;
  assert.equal(kept.total, 10);
  assert.deepEqual(kept.events.map((e) => e[1]), [6, 7, 8, 9]);
});

test('it survives a restart', async () => {
  const directory = await mkdtemp(join(tmpdir(), 'rax-kiosks-'));
  const a = createKiosks({ directory, later() {} });
  a.record('lobby', 'yard-3', 'touch', 5, 6, 1);
  a.record('lobby', 'yard-3', 'lift', 5, 6, 1);
  await a.keep();
  const b = createKiosks({ directory, later() {} });
  assert.deepEqual(b.of('lobby')['yard-3'], { total: 2, events: [['touch', 5, 6, 1], ['lift', 5, 6, 1]] });
});

test('a piece begun again forgets what was done to it and counts its beginnings', () => {
  const k = createKiosks({ later() {} });
  k.record('lobby', 'yard-2', 'touch', 1, 1, 0);
  k.record('lobby', 'yard-2', 'lift', 1, 1, 0);
  k.reset('lobby', 'yard-2');
  assert.deepEqual(k.of('lobby')['yard-2'], { total: 0, events: [], epoch: 1 });
  k.record('lobby', 'yard-2', 'touch', 3, 3, 1);
  k.reset('lobby', 'yard-2');
  assert.equal(k.of('lobby')['yard-2'].epoch, 2);
  k.reset('lobby', 'yard-9');
  assert.equal(k.of('lobby')['yard-9'].epoch, 1, 'one never touched can be begun again too');
});

test('the server restores Notepat settings across restarts and starts with all keys released', async t => {
  const { createCommunalNotepat } = await import('../communal-notepat.mjs');
  const { rm } = await import('node:fs/promises');
  const { setTimeout: delay } = await import('node:timers/promises');
  const directory = await mkdtemp(join(tmpdir(), 'rax-instrument-'));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const ws = {};
  function open() {
    const kiosks = createKiosks({ directory, later() {} });
    const messages = [];
    const device = createCommunalNotepat({ kiosks, send: (_, m) => messages.push(m),
      sendText: (_, text) => messages.push(JSON.parse(text)) });
    device.watch(ws);
    return { device, messages, async close() { device.leave(ws); await device.close(); await kiosks.keep(); } };
  }
  async function waitFor(check) {
    for (let i = 0; i < 200; i++) { if (check()) return; await delay(10); }
    assert.fail('timed out waiting for Notepat');
  }
  const a = open();
  try {
    await waitFor(() => a.messages.some(m => m.type === 'notepat-frame'));
    a.device.key(ws, { way: 'down', key: '7', code: 'Digit7' });
    a.device.key(ws, { way: 'up', key: '7', code: 'Digit7' });
    a.device.key(ws, { way: 'down', key: 'a', code: 'KeyA' });
    await waitFor(() => a.messages.some(m => m.voices?.[0]?.program === 6));
  } finally { await a.close(); }
  const b = open();
  try {
    await waitFor(() => b.messages.some(m => m.type === 'notepat-frame'));
    assert.ok(b.messages.every(m => !m.voices?.length));
    b.device.key(ws, { way: 'down', key: 's', code: 'KeyS' });
    await waitFor(() => b.messages.some(m => m.voices?.[0]?.program === 6));
  } finally { await b.close(); }
});
