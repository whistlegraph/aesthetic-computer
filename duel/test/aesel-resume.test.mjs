import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { WebSocket } from 'ws';
import { createDuelServer } from '../server.mjs';
import { connectBuilder } from '../aesel.mjs';
import { PROTOCOL } from '../world.mjs';
import { parseSource } from '../object-language.mjs';

const token = 'b'.repeat(40);
const SOURCE = '; Slow cube\n(ink red) (slab -1 0 -1 1 2 1)';

async function serve(stateDir, port = 0) {
  const app = createDuelServer({ builderToken: token, stateDir, verifyIdentity: async () => { throw Error('guest'); } });
  await app.ready;
  app.server.listen(port, '127.0.0.1');
  await once(app.server, 'listening');
  return app;
}

function player(port, ownerToken) {
  const ws = new WebSocket(`ws://127.0.0.1:${port}/play?room=resume&protocol=${PROTOCOL}`);
  const seen = [];
  ws.on('message', (raw) => {
    const data = JSON.parse(raw);
    seen.push(data);
    if (data.type === 'welcome') ws.send(JSON.stringify({ type: 'identity', token: ownerToken }));
  });
  const wait = async (pred, ms = 8000) => {
    const end = Date.now() + ms;
    while (Date.now() < end) {
      const hit = seen.find(pred);
      if (hit) return hit;
      await new Promise((r) => setTimeout(r, 25));
    }
    throw Error('timed out waiting; saw ' + JSON.stringify(seen.filter((d) => d.type !== 'snapshot').map((d) => d.type === 'objects' ? 'objects:' + d.jobs.length + (d.builder ? '+b' : '') : d.type + (d.error ? ':' + d.error : '')).slice(-12)));
  };
  return { ws, seen, wait };
}

test('a build survives a server restart and still reaches its player; several run at once', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'dool-resume-'));
  let app = await serve(dir);
  const port = app.server.address().port;
  let release;
  const gate = new Promise((r) => (release = r));
  let started = 0;
  const builder = connectBuilder({
    url: `ws://127.0.0.1:${port}/builder`,
    token,
    generate: async (job, signal, { onProgress }) => {
      started++;
      onProgress({ phase: 'generating', code: '(ink red)', geometry: null });
      await gate;
      return parseSource(SOURCE);
    },
  });
  try {
    const a = player(port, null);
    const identity = await a.wait((d) => d.type === 'identity');
    await a.wait((d) => d.type === 'objects' && d.builder);
    for (const prompt of ['a slow cube', 'another', 'a third'])
      a.ws.send(JSON.stringify({ type: 'prompt', prompt }));
    await a.wait((d) => d.type === 'objects' && d.jobs.length === 3);
    assert.equal(started, 3, 'three builds at once for one player');

    // The server goes away mid-build and comes back on the same port.
    a.ws.close();
    await app.close();
    app = await serve(dir, port);
    const b = player(port, identity.token);
    await b.wait((d) => d.type === 'objects' && d.jobs.length === 3, 10000);
    release();
    const result = await b.wait((d) => d.type === 'build-result' && !d.error, 10000);
    assert.ok(result.objectId, 'the resumed build landed for the same player');
    b.ws.close();
  } finally {
    builder.close();
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
