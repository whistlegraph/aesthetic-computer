import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { createDuelServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';

// A page that says it is signed in is never seated or named as a guest
// while its handle is still being verified, however slow that is.
test('a signed-in page waits for its handle: no guest seat or name first', async () => {
  let release;
  const verified = new Promise((r) => (release = r));
  const app = createDuelServer({
    verifyIdentity: async (token) => {
      await verified; // A slow network.
      if (token === 'valid-test-token') return { handle: 'test_handle' };
      throw Error('not authenticated');
    },
  });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `ws://127.0.0.1:${app.server.address().port}/play?room=hold&protocol=${PROTOCOL}`;
  const wait = async (f) => {
    for (let i = 0; i < 100; i++) {
      if (f()) return;
      await delay(10);
    }
    throw Error('timeout');
  };
  const open = async (url) => {
    const ws = new WebSocket(url), messages = [];
    ws.on('message', (raw) => messages.push(JSON.parse(raw)));
    await once(ws, 'open');
    await wait(() => messages.some((m) => m.type === 'welcome'));
    return { ws, messages };
  };
  const a = await open(`${base}&account=1`), b = await open(`${base}&account=1`);
  try {
    a.ws.send(JSON.stringify({ type: 'identity' }));
    a.ws.send(JSON.stringify({ type: 'account', accessToken: 'valid-test-token' }));
    await delay(150);
    const named = (m) => m.type === 'presence' && (m.self || m.members.length);
    assert.equal(a.messages.find((m) => m.type === 'welcome').seat, -1);
    assert.equal(a.messages.some(named), false, 'nothing is shown of a member still signing in');
    assert.equal(a.messages.some((m) => m.type === 'role'), false);
    release();
    await wait(() => a.messages.some((m) => m.type === 'role' && m.seat >= 0));
    const first = a.messages.find(named);
    assert.equal(first.self.handle, 'test_handle', 'the first name seen is the handle');
    assert.equal(first.self.name, null);
    // Nobody signed in after all: seated as a guest at once, not after the hold.
    b.ws.send(JSON.stringify({ type: 'identity' }));
    b.ws.send(JSON.stringify({ type: 'account-none' }));
    await wait(() => b.messages.some((m) => m.type === 'role' && m.seat >= 0));
  } finally {
    a.ws.terminate();
    b.ws.terminate();
    await app.close();
  }
});
