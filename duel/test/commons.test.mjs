// The plaza belongs to the world: a visitor cannot repaint it, take things
// out of it or clear it; the keeper can (2026-11-16, after a visitor emptied
// the plaza on the tower's first evening).
import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createDuelServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';

async function stage(handle) {
  const dir = await mkdtemp(join(tmpdir(), 'dool-commons-'));
  const app = createDuelServer({ stateDir: dir, verifyIdentity: async () => ({ handle, sub: `acct-${handle}` }),
    listHandles: async () => ['jeffrey', 'neighbor-ada'] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?room=lobby&protocol=${PROTOCOL}&account=1`);
  ws.got = [];
  ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type !== 'snapshot') ws.got.push(d); });
  await once(ws, 'open');
  ws.send(JSON.stringify({ type: 'identity' }));
  ws.send(JSON.stringify({ type: 'account', accessToken: 'valid-test-token' }));
  for (let i = 0; i < 300 && !ws.got.some((d) => d.type === 'home'); i++) await delay(10);
  return { app, ws };
}

test('a visitor cannot paint, remove from or clear the plaza', async () => {
  const { app, ws } = await stage('neighbor-ada');
  try {
    await app.store.paintTile('lobby', 4, 4, 3);
    for (const message of [{ type: 'paint', i: 4, j: 4, c: 0 }, { type: 'paint', i: 5, j: 5, c: 2 },
      { type: 'clear-room' }, { type: 'remove', objectId: 'anything' }]) {
      ws.got.length = 0;
      ws.send(JSON.stringify(message));
      for (let i = 0; i < 200 && !ws.got.some((d) => d.type === 'build-result'); i++) await delay(10);
      assert.match(ws.got.find((d) => d.type === 'build-result')?.error || '', /plaza/, message.type);
    }
    assert.deepEqual(app.store.tiles('lobby'), { '4,4': 3 });
  } finally { ws.close(); await app.close(); }
});

test('the keeper arranges the plaza', async () => {
  const { app, ws } = await stage('jeffrey');
  try {
    ws.send(JSON.stringify({ type: 'paint', i: 4, j: 4, c: 3 }));
    for (let i = 0; i < 200 && !app.store.tiles('lobby')['4,4']; i++) await delay(10);
    assert.deepEqual(app.store.tiles('lobby'), { '4,4': 3 });
  } finally { ws.close(); await app.close(); }
});
