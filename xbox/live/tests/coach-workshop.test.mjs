import test from 'node:test';
import assert from 'node:assert/strict';
import { WebSocketServer } from 'ws';

test('coach waits for a reset acknowledgement before following the next round', async () => {
  const server = new WebSocketServer({ port: 0, host: '127.0.0.1' });
  await new Promise(resolve => server.once('listening', resolve));
  const previous = process.env.OSKIEWAR_RELAY;
  process.env.OSKIEWAR_RELAY = `ws://127.0.0.1:${server.address().port}`;
  const { handleMessage } = await import('../coach.mjs?workshop-test');
  if (previous === undefined) delete process.env.OSKIEWAR_RELAY;
  else process.env.OSKIEWAR_RELAY = previous;
  const rooms = [];
  server.on('connection', (socket, request) => {
    rooms.push(new URL(request.url, 'http://local').searchParams.get('match'));
    socket.send(JSON.stringify({ type: 'oskiewar:status', content: { live: true } }));
    socket.on('message', data => {
      const message = JSON.parse(data);
      if (message.type !== 'oskiewar:workshop') return;
      socket.send(JSON.stringify({ type: 'oskiewar:state', content: {
        fighters: [], sessionId: 'ow-start1', nextRoundId: 'ow-next2' } }));
      setTimeout(() => socket.send(JSON.stringify({ type: 'oskiewar:workshop-result',
        content: { id: message.content.id, ok: true, result: { revision: 2 } } })), 20);
    });
  });
  const call = (name, args) => handleMessage({ id: 1, method: 'tools/call',
    params: { name, arguments: args } });
  try {
    await call('coach_in', { room: 'start1' });
    const response = await call('coach_workshop', { op: 'reset-round', revision: 1 });
    assert.equal(response.result.isError, undefined);
    assert.equal(JSON.parse(response.result.content[0].text).revision, 2);
    for (let i = 0; i < 20 && rooms.length < 2; i++) await new Promise(resolve => setTimeout(resolve, 10));
    assert.deepEqual(rooms, ['ow-start1', 'ow-next2']);
  } finally {
    await call('coach_out');
    for (const socket of server.clients) socket.terminate();
    await new Promise(resolve => server.close(resolve));
  }
});
