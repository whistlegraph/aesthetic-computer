import test from 'node:test';
import assert from 'node:assert/strict';
import net from 'node:net';
import { mkdtemp, rm, readdir } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { localFrame } from '../lib/frame-local.mjs';

async function fixture(t, handle) {
  const dir = await mkdtemp(join(tmpdir(), 'frame-ipc-'));
  const clients = new Set();
  const server = net.createServer(socket => {
    clients.add(socket); socket.on('close', () => clients.delete(socket));
    let data = '';
    socket.on('data', chunk => {
      data += chunk;
      if (data.endsWith('\n')) handle(socket, JSON.parse(data));
    });
  });
  await new Promise(resolve => server.listen(join(dir, 'frame-native.sock'), resolve));
  t.after(async () => {
    for (const socket of clients) socket.destroy();
    await new Promise(resolve => server.close(resolve));
    await rm(dir, { recursive: true, force: true });
  });
  return dir;
}
function packet(id, frame, jpg = Buffer.alloc(0)) {
  const json = Buffer.from(JSON.stringify({ id, frame })), header = Buffer.alloc(12);
  header.write('ACF2'); header.writeUInt32BE(json.length, 4); header.writeUInt32BE(jpg.length, 8);
  return Buffer.concat([header, json, jpg]);
}

test('native socket accepts fragmented correlated output without sidecars', async t => {
  const jpg = Buffer.from([255, 216, 0, 10, 255, 217]);
  const dir = await fixture(t, (socket, request) => {
    assert.equal(request.mode, 'native-click=fixture');
    const data = packet(request.id, { capture: 'ok' }, jpg);
    socket.write(data.subarray(0, 3));
    setImmediate(() => socket.end(data.subarray(3)));
  });
  const result = await localFrame(dir, 'native-click=fixture');
  assert.equal(JSON.parse(result.json).capture, 'ok'); assert.deepEqual(result.jpg, jpg);
  assert.deepEqual(await readdir(dir), ['frame-native.sock']);
});
for (const mode of ['disconnect', 'timeout', 'wrong-id', 'oversize', 'truncated']) {
  test(`native socket ${mode} never replays input through files`, async t => {
    let requests = 0;
    const dir = await fixture(t, (socket, request) => {
      requests++;
      if (mode === 'disconnect') socket.destroy();
      if (mode === 'wrong-id') socket.end(packet('wrong', { capture: 'verified' }));
      if (mode === 'truncated') socket.end(packet(request.id, {}).subarray(0, 15));
      if (mode === 'oversize') { const h = Buffer.alloc(12); h.write('ACF2'); h.writeUInt32BE(30e6, 4); socket.end(h); }
    });
    await assert.rejects(localFrame(dir, 'native-click=fixture', { timeoutMs: 60 }), /outcome unknown.*not retried/);
    assert.equal(requests, 1);
    assert.deepEqual(await readdir(dir), ['frame-native.sock']);
  });
}
