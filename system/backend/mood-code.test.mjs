import test from 'node:test';
import assert from 'node:assert/strict';
import { ObjectId } from 'mongodb';
import { ensureMoodCode } from './mood-code.mjs';
import { feedOnlyOp } from './mime-feed-engine.mjs';
import { createHandler } from '../netlify/functions/mime.mjs';

function fixture() {
  const rows = new Map();
  let collisions = 0;
  const codes = {
    createIndex: async () => {},
    findOne: async q => q._id ? rows.get(q._id) : [...rows.values()].find(r => r.code === q.code),
    insertOne: async row => {
      if (collisions-- > 0 || rows.has(row._id) || [...rows.values()].some(r => r.code === row.code)) throw Object.assign(new Error('duplicate'), { code: 11000 });
      rows.set(row._id, row);
    },
  };
  const mood = { _id: new ObjectId(), user: 'person', mood: 'feeling good', when: new Date() };
  const replies = [{ code: 'reply', parent: 'mood_' + mood._id, text: 'hello', when: new Date() }];
  const cursor = rows => ({ sort() { return this; }, toArray: async () => rows });
  const db = { collection(name) {
    if (name === 'mood-codes') return codes;
    if (name === 'moods') return { findOne: async q => String(q._id) === String(mood._id) && !mood.deleted ? mood : null };
    if (name === '@handles') return { find: () => cursor([{ _id: 'person', handle: 'tester' }]) };
    if (name === 'mime-media-threads') return { findOne: async () => ({ replies: 1 }) };
    if (name === 'mimechan') return { find: q => cursor(replies.filter(r => r.parent === q.parent)), findOne: async () => null };
    if (name === 'mime-engagement') return { aggregate: () => cursor([]) };
    throw Error(name);
  } };
  return { db, mood, rows, collide: n => { collisions = n; } };
}

test('concurrent mood reads persist one short identity, including collision retries', async () => {
  const f = fixture(); f.collide(2);
  const codes = await Promise.all(Array.from({ length: 8 }, () => ensureMoodCode(f.db, f.mood._id)));
  assert.equal(new Set(codes).size, 1);
  assert.match(codes[0], /^[a-z2-9]{3}$/);
  assert.equal(f.rows.size, 1);
  f.mood.mood = 'new words';
  assert.equal(await ensureMoodCode(f.db, f.mood._id), codes[0]);
});

test('short and legacy mood links resolve the same thread; deleted moods stay unavailable', async () => {
  const f = fixture();
  const code = await ensureMoodCode(f.db, f.mood._id);
  const old = 'mood_' + f.mood._id;
  const op = await feedOnlyOp(f.db, 'mood~' + code);
  assert.equal(op.code, old);
  assert.equal(op.media.code, code);
  assert.deepEqual(await feedOnlyOp(f.db, old), op);
  const handler = createHandler(async () => ({ db: f.db }));
  const response = await handler({ httpMethod: 'GET', queryStringParameters: { thread: 'mood~' + code } });
  assert.equal(response.statusCode, 200, response.body);
  assert.equal(JSON.parse(response.body).replies[0].code, 'reply');
  f.mood.deleted = true;
  assert.equal(await feedOnlyOp(f.db, 'mood~' + code), null);
  assert.equal(await feedOnlyOp(f.db, 'mood~missing'), null);
});
