import assert from 'node:assert/strict';
import { once } from 'node:events';
import test from 'node:test';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { createDuelServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';
import { LOTS } from '../geography.mjs';

// A residential block tells whoever joins where its lots are and who settled
// them; the lobby has no lots and says nothing.
test('a block sends its lots on join, the lobby does not', async () => {
  const app = createDuelServer({ resolveHandleColors: async () => null });
  await app.ready;
  const join = async (room) => {
    const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?protocol=${PROTOCOL}&room=${room}`);
    const got = [];
    ws.on('message', (raw) => got.push(JSON.parse(raw)));
    await once(ws, 'open');
    await delay(150);
    ws.close();
    return got.filter((m) => m.type === 'lots');
  };
  try {
    await app.geography.change((g) => g.settle('jeffrey'));
    app.server.listen(0, '127.0.0.1');
    await once(app.server, 'listening');
    const [lots] = await join('e1n0');
    assert.equal(lots.room, 'e1n0');
    assert.equal(lots.rects.length, LOTS);
    assert.deepEqual(lots.homes.map(({lot, handle}) => ({lot, handle})), [{ lot: 0, handle: 'jeffrey' }]);
    assert.equal(lots.homes[0].pod, app.geography.geography.home('jeffrey').pod);
    assert.equal(lots.homes[0].profile.handle, 'jeffrey');
    assert.deepEqual(await join('e1n1'), [], 'unopened blocks are unavailable');
    assert.deepEqual(await join('lobby'), []);
  } finally {
    await app.close();
  }
});

// The world stream: a block and every open block beside it, each where it
// stands on the lattice, the plaza among them.
test('/world answers a block with its open neighbours in place', async () => {
  const app = createDuelServer({ resolveHandleColors: async () => null });
  await app.ready;
  try {
    for (const h of ['a', 'b', 'c', 'd', 'e', 'f', 'g']) await app.geography.change((g) => g.settle(h));
    app.server.listen(0, '127.0.0.1');
    await once(app.server, 'listening');
    const ask = async (room) =>
      (await fetch(`http://127.0.0.1:${app.server.address().port}/world?room=${room}`)).json();
    const world = await ask('e1n0');
    const at = Object.fromEntries(world.blocks.map((b) => [b.id, b]));
    assert.deepEqual([at.e1n0.di, at.e1n0.dj], [0, 0]);
    assert.deepEqual([at.lobby.di, at.lobby.dj, at.lobby.plaza], [-1, 0, true]);
    assert.deepEqual([at.e1n1.di, at.e1n1.dj], [0, 1]);
    assert.equal(at.e1n0.lots.homes.length, 4);
    assert.equal(at.lobby.lots, null);
    assert.equal(at.e1n0.neighbours.west, 'lobby');
    assert.ok(world.blocks.every((b) => Array.isArray(b.objects) && typeof b.tiles === 'object'));
    assert.deepEqual((await ask('lobby')).blocks.find((b) => b.id === 'lobby').neighbours.east, 'e1n0');
    assert.deepEqual((await ask('nowhere')).blocks, []);
  } finally {
    await app.close();
  }
});
