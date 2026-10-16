import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
import { createGuestbookServer } from './server.mjs';

async function fixture(t, options = {}) {
  const dir = await mkdtemp(join(tmpdir(), 'reset-guestbook-'));
  const dataFile = join(dir, 'book.json');
  let server;
  async function start() {
    server = await createGuestbookServer({ dataFile, ...options });
    await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
    return `http://127.0.0.1:${server.address().port}`;
  }
  let origin = await start();
  const close = () => new Promise(resolve => server.close(resolve));
  t.after(async () => { await close(); await rm(dir, { recursive: true, force: true }); });
  return {
    dataFile,
    get: (path = '/reset/api/guestbook') => fetch(origin + path),
    post: (body, headers = {}) => fetch(origin + '/reset/api/guestbook', { method: 'POST', headers: { 'Content-Type': 'application/json', Origin: origin, ...headers }, body: JSON.stringify(body) }),
    restart: async () => { await close(); origin = await start(); },
  };
}
const entry = (overrides = {}) => ({ id: randomUUID(), name: 'Test ghost', message: 'Goodnight, old computer. 👻', attending: true, website: '', ...overrides });

test('signatures are shared, survive restart, and retries do not duplicate them', async t => {
  const app = await fixture(t);
  const input = entry();
  assert.equal((await app.post(input)).status, 201);
  assert.equal((await app.post(input)).status, 200);
  await app.restart();
  const { entries } = await (await app.get()).json();
  assert.equal(entries.length, 1);
  assert.equal(entries[0].message, input.message);
  assert.equal(entries[0].attending, true);
  assert.ok(entries[0].createdAt);
  assert.deepEqual(Object.keys(entries[0]).sort(), ['attending', 'createdAt', 'id', 'message', 'name']);
});

test('rejects malformed, oversized, cross-origin, and honeypot submissions', async t => {
  const app = await fixture(t);
  for (const invalid of [{ name: '  ' }, { name: 'a'.repeat(61) }, { message: 'a'.repeat(601) }, { attending: 'yes' }, { website: 'spam' }, { id: 'bad' }]) {
    assert.equal((await app.post(entry(invalid))).status, 400);
  }
  assert.equal((await app.post(entry(), { Origin: 'https://other.example' })).status, 403);
  assert.equal((await app.post(entry(), { 'Content-Type': 'text/plain' })).status, 415);
  assert.equal((await app.post(entry({ message: 'x'.repeat(9000) }))).status, 413);
  assert.equal((await app.get()).status, 200);
  assert.deepEqual((await (await app.get()).json()).entries, []);
});

test('concurrent writes preserve each signature and rate limits cannot be evaded with forwarding headers', async t => {
  const app = await fixture(t, { rateLimit: 3 });
  const responses = await Promise.all(Array.from({ length: 3 }, (_, i) => app.post(entry({ name: `Ghost ${i}` }))));
  assert.deepEqual(responses.map(r => r.status), [201, 201, 201]);
  assert.equal(JSON.parse(await readFile(app.dataFile)).length, 3);
  const blocked = await app.post(entry(), { 'X-Real-IP': '203.0.113.7' });
  assert.equal(blocked.status, 429);
  assert.ok(Number(blocked.headers.get('retry-after')) > 0);
});

test('serves local assets but keeps data and source private', async t => {
  const app = await fixture(t);
  for (const path of ['/reset/', '/reset/guestbook.js', '/reset/assets/RIP.gif', '/reset/funeral2.html', '/reset/reset-in-peace.ics']) {
    assert.equal((await app.get(path)).status, 200, path);
  }
  for (const path of ['/server.mjs', '/data/guestbook.json', '/upstream/index.html', '/reset/%2e%2e%2f%2e%2e%2fserver.mjs']) {
    assert.equal((await app.get(path)).status, 404, path);
  }
});

test('a corrupt book is never silently replaced', async t => {
  const dir = await mkdtemp(join(tmpdir(), 'reset-corrupt-'));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const dataFile = join(dir, 'book.json');
  await writeFile(dataFile, 'broken book');
  await assert.rejects(createGuestbookServer({ dataFile }));
  assert.equal(await readFile(dataFile, 'utf8'), 'broken book');
});
