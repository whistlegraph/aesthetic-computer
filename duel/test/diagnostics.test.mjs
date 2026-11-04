import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { once } from 'node:events';
import { WebSocket } from 'ws';
import { createDiagnostics } from '../diagnostics.mjs';
import { ClientDiagnostics } from '../diagnostics-client.mjs';
import { createDuelServer } from '../server.mjs';
import { connectBuilder } from '../aesel.mjs';
import { PROTOCOL } from '../world.mjs';
const delay = (ms) => new Promise((r) => setTimeout(r, ms));
async function wait(fn) {
  const end = Date.now() + 5000;
  while (!fn()) { if (Date.now() > end) throw Error('Timed out'); await delay(10); }
}

test('diagnostics are opt-in, allowlisted, bounded and fail open', () => {
  const rows = [], id = randomUUID();
  createDiagnostics({ enabled: false, write: (r) => rows.push(r) }).emit('client.sample', { id });
  assert.equal(rows.length, 0);
  const d = createDiagnostics({ enabled: true, write: (r) => rows.push(r), now: () => 1000 });
  d.emit('client.sample', { id, session: 'secret', prompt: 'secret', owner: 'secret', code: '../secret',
    backend: 'secret', frameWorkMaxMs: Infinity, frames: 100, buildIds: [id, 'secret'] });
  assert.equal(JSON.stringify(rows).includes('secret'), false);
  assert.equal(rows[0].frames, 100);
  assert.deepEqual(rows[0].buildIds, [id]);
  for (let i = 0; i < 1000; i++) d.emit('server.sample');
  assert.equal(rows.length, 120);
  assert.doesNotThrow(() => createDiagnostics({ enabled: true, write() { throw Error('sink down'); } }).emit('server.sample'));
});

test('visible sampling records recovered stalls, resets on hiding, and sends at most every five seconds', () => {
  let now = 0;
  const rows = [], d = new ClientDiagnostics((r) => rows.push(r), () => now), id = randomUUID();
  d.frame({ visible: true });
  assert.equal(rows.length, 0);
  d.enabled = true;
  d.frame({ visible: true, buildIds: [id], workMs: 12 });
  now = 1200;
  d.longTask(900);
  d.frame({ visible: true, workMs: 900 });
  now = 5001;
  d.frame({ visible: true, workMs: 5 });
  assert.equal(rows.length, 1);
  assert.equal(rows[0].longTaskMaxMs, 900);
  assert.equal(rows[0].frameWorkMaxMs, 900);
  assert.deepEqual(rows[0].buildIds, [id]);
  now = 5100;
  d.frame({ visible: false });
  now = 100000;
  d.frame({ visible: false });
  d.frame({ visible: true });
  assert.equal(rows.length, 1);
});

test('two players share build IDs, rejection/cancellation survive, and hostile samples cannot leak content', async (t) => {
  const rows = [], token = 'd'.repeat(64);
  const app = createDuelServer({ builderToken: token, diagnosticsEnabled: true, diagnosticWrite: (r) => rows.push(r) });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const url = `ws://127.0.0.1:${app.server.address().port}`;
  const sockets = [];
  let release;
  const worker = connectBuilder({ url: url + '/builder', token, generate: async (_job, _signal, { onProgress }) => {
    onProgress({ phase: 'generating', code: 'PRIVATE SOURCE' });
    await new Promise((r) => { release = r; });
    return { name: 'box', boxes: [{ x: 0, y: 1, z: 0, w: 1, h: 2, d: 1 }] };
  } });
  t.after(async () => { release?.(); worker.close(); sockets.forEach((s) => s.ws.terminate()); await app.close(); });
  await wait(() => app.builders.workers.size === 1);
  for (let i = 0; i < 2; i++) {
    const ws = new WebSocket(url + `/play?room=private-room&protocol=${PROTOCOL}`), messages = [];
    ws.on('message', (b) => messages.push(JSON.parse(b)));
    sockets.push({ ws, messages });
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity' }));
    await wait(() => messages.some((m) => m.type === 'identity'));
  }
  const [a, b] = sockets;
  a.ws.send(JSON.stringify({ type: 'prompt', prompt: 'PRIVATE PROMPT' }));
  await wait(() => release && rows.some((r) => r.event === 'build.accepted'));
  const id = rows.find((r) => r.event === 'build.accepted').id;
  await wait(() => b.messages.some((m) => m.jobs?.some((j) => j.id === id)));
  b.ws.send(JSON.stringify({ type: 'diagnostic', frames: 300, frameGapMaxMs: 750, buildIds: [id, randomUUID()],
    prompt: 'SECRET', session: 'spoofed', backend: 'webgpu', error: 'SECRET' }));
  await wait(() => rows.some((r) => r.event === 'client.sample'));
  const sample = rows.find((r) => r.event === 'client.sample');
  assert.deepEqual(sample.buildIds, [id]);
  assert.notEqual(sample.session, rows.find((r) => r.event === 'build.accepted').session);
  release();
  await wait(() => rows.some((r) => r.event === 'build.finished' && r.outcome === 'committed'));
  b.ws.send(JSON.stringify({ type: 'prompt', prompt: 'PRIVATE EDIT', objectId: app.store.objects('private-room')[0].id, baseVersion: 0 }));
  await wait(() => rows.some((r) => r.event === 'build.rejected'));
  assert.equal(rows.find((r) => r.event === 'build.rejected').reason, 'ownership');
  a.ws.send(JSON.stringify({ type: 'prompt', prompt: 'PRIVATE CANCEL' }));
  await wait(() => app.builders.jobs.size === 1);
  a.ws.send(JSON.stringify({ type: 'cancel-build' }));
  await wait(() => rows.some((r) => r.outcome === 'cancelled'));
  assert.doesNotMatch(JSON.stringify(rows), /PRIVATE|SECRET|private-room|spoofed/);
  assert.equal(app.store.objects('private-room').length, 1);
});
