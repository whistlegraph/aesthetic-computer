import test, { mock } from 'node:test';
import assert from 'node:assert/strict';
const records = [];
let logins = 0, projectionId = '0123456789abcdef01234567', reads = 0;
mock.module('@atproto/api', { exports: { AtpAgent: class {
  async login() { logins++; }
  com = { atproto: { repo: { createRecord: async ({ record }) => {
    records.push(record); return { uri: 'at://test/computer.aesthetic.kidlisp/record' };
  } } } };
} } });
mock.module('../backend/shell.mjs', { exports: { shell: { log() {}, warn() {}, error() {} } } });
mock.module('../backend/thumbnail.mjs', { exports: { getThumbnailFromSlug() {} } });
const { createMediaRecord, MediaTypes } = await import('../backend/media-atproto.mjs');
const db = { db: { collection: (name) => ({ findOne: async (query, options) => {
  if (name === 'users') return { atproto: { did: 'did:test', password: 'test-only' } };
  assert.equal(name, 'kidlisp');
  assert.deepEqual(query, { code: 'abc' });
  assert.deepEqual(options, { projection: { _id: 1 } });
  reads++;
  return projectionId ? { _id: projectionId } : null;
} }) } };
const item = { code: 'abc', source: '(slab 0 0 0 1 1 1)', when: new Date('2026-01-01') };
test('Datomic publications use the existing projection identity; Mongo keeps its identity', async () => {
  const result = await createMediaRecord(db, MediaTypes.KIDLISP, item);
  assert.equal(result.rkey, 'record');
  assert.equal(records[0].ref, projectionId);
  assert.equal(records[0].source, item.source);
  assert.equal(reads, 1);
  await createMediaRecord(db, MediaTypes.KIDLISP, { ...item, _id: 'abcdef0123456789abcdef01' });
  assert.equal(records[1].ref, 'abcdef0123456789abcdef01');
  assert.equal(reads, 1);
});
test('missing projection is reported without publishing an invalid record', async () => {
  projectionId = null;
  const before = logins;
  const result = await createMediaRecord(db, MediaTypes.KIDLISP, item);
  assert.equal(result.error, 'KidLisp projection missing identity');
  assert.equal(logins, before);
});
