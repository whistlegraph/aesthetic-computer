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
