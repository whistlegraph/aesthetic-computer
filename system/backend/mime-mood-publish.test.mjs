import test from 'node:test';
import assert from 'node:assert/strict';
import { ObjectId } from 'mongodb';
import { createHandler } from '../netlify/functions/mood.mjs';

function fixture() {
  const rows = [], codes = new Map(), effects = [];
  const db = { collection(name) {
    if (name === 'moods') return {
      createIndex: async () => {},
      findOne: async () => rows.findLast(row => !row.deleted),
      insertOne: async row => { row._id = new ObjectId(); rows.push(row); return { insertedId: row._id }; },
    };
    if (name === 'mood-codes') return {
      createIndex: async () => {},
      findOne: async q => q._id ? codes.get(q._id) : [...codes.values()].find(row => row.code === q.code),
      insertOne: async row => { codes.set(row._id, row); },
    };
    throw new Error(name);
  } };
  const handler = createHandler({
    authorize: async headers => headers.authorization ? { sub: 'fixture-user' } : null,
    getHandleOrEmail: async () => '@fixture', connect: async () => ({ db, disconnect: async () => {} }),
    createMoodOnAtproto: async () => { effects.push('federate'); return {}; },
    shouldMirror: async () => false,
    broadcastToTopic: async () => { effects.push('push'); },
    publishProfileEvent: async () => { effects.push('profile'); },
  });
  const post = (body, signedIn = true) => handler({ httpMethod: 'POST',
    headers: signedIn ? { authorization: 'Bearer fixture' } : {}, body: JSON.stringify(body) });
  return { rows, effects, post };
}

test('posting returns the saved mood card; a retry keeps its identity and does not republish', async () => {
  const f = fixture();
  const response = await f.post({ mood: '  A little room for a mood.  ' });
  assert.equal(response.statusCode, 200, response.body);
  const first = JSON.parse(response.body);
  assert.equal(first.mood, 'A little room for a mood.');
  assert.equal(first.post.text, first.mood);
  assert.equal(first.post.name, '@fixture');
  assert.equal(first.post.board, 'text/plain');
  assert.equal(first.post.code, 'mood_' + f.rows[0]._id);
  assert.equal(first.post.media.kind, 'mood');
  assert.match(first.post.media.code, /^[a-z2-9]{3}$/);
  assert.deepEqual(JSON.parse((await f.post({ mood: first.mood })).body), first);
  assert.equal(f.rows.length, 1);
  assert.deepEqual(f.effects.sort(), ['federate', 'profile', 'push']);
});

test('sign-in and nonempty text are required; a removed mood is not reused', async () => {
  const f = fixture();
  assert.equal((await f.post({ mood: 'Hi' }, false)).statusCode, 401);
  for (const mood of ['', '   ', 42, {}]) assert.equal((await f.post({ mood })).statusCode, 400);
  assert.equal(f.rows.length, 0);
  const before = JSON.parse((await f.post({ mood: 'Hi' })).body);
  f.rows[0].deleted = true;
  const after = JSON.parse((await f.post({ mood: 'Hi' })).body);
  assert.notEqual(after.post.code, before.post.code);
  assert.equal(f.rows.length, 2);
});
