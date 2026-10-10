import test from 'node:test';
import assert from 'node:assert/strict';
import {mongoScreenStore, validScreenCode, MAX_SOURCE} from './whistlegraph-screens.mjs';

// The slice of Mongo the screen store uses: insert with a unique _id,
// findOne/findOneAndUpdate/updateOne with equality filters, $set and $inc.
function collection() {
  const rows = new Map();
  const matches = (row, query) => Object.entries(query).every(([key, value]) => row[key] === value);
  const apply = (row, update) => {
    for (const [key, value] of Object.entries(update.$set || {})) row[key] = value;
    for (const [key, value] of Object.entries(update.$inc || {})) row[key] = (row[key] || 0) + value;
  };
  const find = query => [...rows.values()].find(row => matches(row, query));
  return {
    createIndex: async () => {},
    insertOne: async row => { if (rows.has(row._id)) throw Object.assign(Error('dup'), {code: 11000}); rows.set(row._id, {...row}); },
    findOne: async query => { const row = find(query); return row ? {...row} : null; },
    findOneAndUpdate: async (query, update) => { const row = find(query); if (!row) return null; apply(row, update); return {...row}; },
    updateOne: async (query, update) => { const row = find(query); if (!row) return {matchedCount: 0}; apply(row, update); return {matchedCount: 1}; },
    find: query => ({sort: () => ({limit: () => ({toArray: async () => [...rows.values()].filter(row => matches(row, query))})})}),
    size: () => rows.size,
  };
}

test('screen codes are four readable letters', () => {
  assert.ok(validScreenCode('ABCD')); assert.ok(validScreenCode('ZZZZ'));
  assert.ok(!validScreenCode('ABCI'), 'no I'); assert.ok(!validScreenCode('ABCO'), 'no O');
  assert.ok(!validScreenCode('abcd')); assert.ok(!validScreenCode('ABCDE')); assert.ok(!validScreenCode(''));
});

test('a screen is made, paired, pushed to, and unpaired', async () => {
  const store = mongoScreenStore(collection());
  const {code, secret} = await store.create();
  assert.ok(validScreenCode(code)); assert.match(secret, /^[a-f0-9-]{36}$/);
  let state = await store.poll(code, secret);
  assert.deepEqual({paired: state.paired, revision: state.revision, source: state.source}, {paired: false, revision: 0, source: ''}, 'unpaired, nothing to show');
  assert.equal(await store.poll(code, 'wrong-secret'), null, 'the secret gates the screen');
  assert.equal(await store.poll('QQQQ', secret), null);
  assert.equal(await store.push(code, 'user1', 'x'), false, 'no pushing before pairing');
  assert.ok(await store.pair(code, 'user1', 'jeffrey'));
  state = await store.poll(code, secret, 0);
  assert.equal(state.paired, true); assert.equal(state.name, 'jeffrey'); assert.equal(state.source, undefined, 'nothing new since revision 0');
  assert.ok(await store.push(code, 'user1', 'export function paint({wipe}){wipe("red")}'));
  assert.equal(await store.push(code, 'user2', 'nope'), false, 'another handle cannot push');
  state = await store.poll(code, secret, 0);
  assert.equal(state.revision, 1); assert.match(state.source, /red/); assert.match(state.sourceHash, /^[a-f0-9]{64}$/);
  const pushedHash = state.sourceHash;
  assert.equal((await store.poll(code, secret, 1)).source, undefined, 'already seen');
  assert.ok(await store.status(code, 'user1', {code: 'wgDuram', phase: 'Drawing…', busy: true}));
  state = await store.poll(code, secret, 1);
  assert.deepEqual({code: state.status.code, phase: state.status.phase, busy: state.status.busy}, {code: 'wgDuram', phase: 'Drawing…', busy: true});
  assert.equal((await store.read(code, 'user1')).sourceHash, pushedHash); assert.equal(await store.read(code, 'user2'), null);
  assert.equal((await store.mine('user1'))[0].code, code);
  assert.equal(await store.push(code, 'user1', 'y'.repeat(MAX_SOURCE + 1)), false, 'too large');
  assert.ok(await store.unpair(code, 'user1'));
  state = await store.poll(code, secret, 1);
  assert.equal(state.paired, false); assert.equal(state.revision, 2); assert.equal(state.source, '', 'the screen shows its code again');
  assert.equal(await store.push(code, 'user1', 'x'), false, 'unpaired');
});
