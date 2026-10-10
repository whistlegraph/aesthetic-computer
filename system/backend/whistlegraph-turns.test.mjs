import test from 'node:test';
import assert from 'node:assert/strict';
import {mongoTurnQueue, validateTurnRequest, publicTurn, LEASE_MS, MAX_QUEUED_PER_OWNER} from './whistlegraph-turns.mjs';

// The slice of Mongo a queue needs, including findOneAndUpdate with sort, $nin, $lt/$gte and distinct.
function collection() {
  const rows = [];
  const get = (row, key) => key.split('.').reduce((o, k) => o?.[k], row);
  const matches = (row, query) => Object.entries(query).every(([key, value]) => {
    const actual = get(row, key);
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      if ('$in' in value && !value.$in.includes(actual)) return false;
      if ('$nin' in value && value.$nin.includes(actual)) return false;
      if ('$lt' in value && !(actual != null && actual < value.$lt)) return false;
      if ('$gte' in value && !(actual != null && actual >= value.$gte)) return false;
      return true;
    }
    return actual === value;
  });
  const apply = (row, update) => { Object.assign(row, update.$set || {}); for (const [k, n] of Object.entries(update.$inc || {})) row[k] = (row[k] || 0) + n; };
  const sorted = (list, sort) => { if (!sort) return list; const [[key, dir]] = Object.entries(sort); return list.slice().sort((a, b) => (get(a, key) > get(b, key) ? 1 : get(a, key) < get(b, key) ? -1 : 0) * dir); };
  return {
    rows,
    createIndex: async () => {},
    insertOne: async row => { if (rows.some(r => r.requestID === row.requestID)) throw Object.assign(Error('dup'), {code: 11000}); rows.push({...row}); },
    findOne: async q => rows.find(r => matches(r, q)) || null,
    countDocuments: async q => rows.filter(r => matches(r, q)).length,
    distinct: async (key, q) => [...new Set(rows.filter(r => matches(r, q)).map(r => get(r, key)))],
    updateOne: async (q, update) => { const row = rows.find(r => matches(r, q)); if (!row) return {modifiedCount: 0}; apply(row, update); return {modifiedCount: 1}; },
    findOneAndUpdate: async (q, update, {sort} = {}) => { const row = sorted(rows.filter(r => matches(r, q)), sort)[0]; if (!row) return null; apply(row, update); return {...row}; },
    find: (q) => { let out = rows.filter(r => matches(r, q)); const chain = {sort(s) { out = sorted(out, s); return chain; }, limit(n) { out = out.slice(0, n); return chain; }, toArray: async () => out.map(r => ({...r}))}; return chain; },
  };
}
const hash = 'a'.repeat(64);
const thread = (id, code) => ({_id: id, code});
const good = {code: 'wgFeeda', text: 'make it spin', baseVersion: 2, baseHash: hash};

test('a turn request is checked before it is queued', () => {
  assert.deepEqual(validateTurnRequest(good), {code: 'wgFeeda', text: 'make it spin', baseVersion: 2, baseHash: hash});
  assert.throws(() => validateTurnRequest({...good, code: 'nope'}), /thread code/);
  assert.throws(() => validateTurnRequest({...good, text: ''}), /words or add chalk/);
  assert.throws(() => validateTurnRequest({...good, text: 'x'.repeat(20_001)}), /at most/);
  assert.throws(() => validateTurnRequest({...good, baseHash: 'short'}), /sha256/);
  assert.throws(() => validateTurnRequest({...good, baseVersion: -1}), /baseVersion/);
  assert.throws(() => validateTurnRequest({...good, model: 'bad model!'}), /model/);
  assert.throws(() => validateTurnRequest({...good, drawing: {schema: 'nope'}}), /drawing/);
  const chalk = validateTurnRequest({...good, text: '', drawing: {schema: 'whistlegraph-drawing/v1', id: '0'.repeat(8) + '-0000-4000-8000-' + '0'.repeat(12), revision: 1, aspect: 1.33, strokes: [[[1, 2, 0]]]}});
  assert.equal(chalk.text, ''); assert.ok(chalk.drawing);
});

test('claims go oldest first, one running turn per thread, and a quiet lease is reclaimable', async () => {
  let t = 0; const clock = () => new Date(1_700_000_000_000 + t);
  const c = collection(), q = mongoTurnQueue(c, {now: clock});
  const a = await q.enqueue('alice', thread('t1', 'wgFeeda'), validateTurnRequest(good)); t += 1000;
  const b = await q.enqueue('alice', thread('t1', 'wgFeeda'), validateTurnRequest({...good, text: 'then bounce'})); t += 1000;
  const other = await q.enqueue('bob', thread('t2', 'wgBobbo'), validateTurnRequest({...good, code: 'wgBobbo', text: 'a moon'})); t += 1000;
  assert.equal(publicTurn(a).owner, undefined, 'the subject never leaves the server');
  assert.equal((await q.claim('w1'))._id, a._id, 'oldest first');
  assert.equal((await q.claim('w2'))._id, other._id, 'the same thread is skipped while a turn runs on it, so bob goes next');
  assert.equal(await q.claim('w3'), null, 'alice\'s second turn waits for her first');
  assert.ok(await q.heartbeat(a._id, 'w1', 'export function paint(){}'));
  assert.equal(!(await q.heartbeat(a._id, 'w9')), true, 'only the lease holder heartbeats');
  t += LEASE_MS + 1;
  assert.equal((await q.claim('w3'))._id, a._id, 'w1 went quiet; w3 takes the job over');
  assert.equal(c.rows.find(r => r._id === a._id).attempts, 2);
  assert.equal(c.rows.find(r => r._id === a._id).checkpoint, 'export function paint(){}', 'the checkpoint survives the handover');
  assert.equal(await q.complete(a._id, 'w1', {versionID: 3}), false, 'the late worker cannot finish it');
  assert.ok(await q.complete(a._id, 'w3', {versionID: 3}));
  assert.equal((await q.claim('w1'))._id, other._id, 'bob\'s lease also lapsed; a lapsed job is taken over before new work');
  assert.equal((await q.claim('w1'))._id, b._id, 'with alice\'s first done, her second runs');
  assert.ok(await q.fail(b._id, 'w1', 'Nothing painted', {draft: 'x'}));
  const depth = await q.depth(); assert.deepEqual(depth, {queued: 0, running: 1}, 'bob\'s reclaimed turn is still running');
  assert.equal((await q.listOpen('alice', 't1')).length, 0);
  assert.equal((await q.recent('alice')).length, 2);
});

test('the same request is one row, and a person cannot stack more than the cap', async () => {
  const c = collection(), q = mongoTurnQueue(c);
  const id = '12345678-1234-4123-8123-123456789abc';
  const first = await q.enqueue('alice', thread('t1', 'wgFeeda'), validateTurnRequest({...good, requestID: id}));
  const again = await q.enqueue('alice', thread('t1', 'wgFeeda'), validateTurnRequest({...good, requestID: id}));
  assert.equal(again._id, first._id);
  await assert.rejects(q.enqueue('bob', thread('t2', 'wgBobbo'), validateTurnRequest({...good, code: 'wgBobbo', requestID: id})), {statusCode: 404});
  for (let i = 1; i < MAX_QUEUED_PER_OWNER; i++) await q.enqueue('alice', thread('t1', 'wgFeeda'), validateTurnRequest({...good, text: 'more ' + i}));
  await assert.rejects(q.enqueue('alice', thread('t1', 'wgFeeda'), validateTurnRequest({...good, text: 'one too many'})), {statusCode: 429});
  assert.equal(await q.cancel('bob', first._id), false, 'only the owner cancels');
  assert.ok(await q.cancel('alice', first._id));
  assert.equal(await q.cancel('alice', first._id), false, 'a cancelled turn is not waiting');
  await q.enqueue('alice', thread('t1', 'wgFeeda'), validateTurnRequest({...good, text: 'room again'}));
});
