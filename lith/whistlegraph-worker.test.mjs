import test from 'node:test';
import assert from 'node:assert/strict';
import {mongoWhistlegraphStore, sourceHash} from '../system/backend/whistlegraph.mjs';
import {commitVersion, runTurn} from './whistlegraph-worker.mjs';

function collection() {
  const rows = [];
  const matches = (row, query) => Object.entries(query).every(([k, v]) => row[k] === v);
  return {
    rows, createIndex: async () => {},
    insertOne: async row => { rows.push({...row}); },
    findOne: async q => { const r = rows.find(r => matches(r, q)); return r ? structuredClone(r) : null; },
    updateOne: async (q, update) => { const row = rows.find(r => matches(r, q)); if (!row) return {modifiedCount: 0}; Object.assign(row, update.$set || {}); for (const [k, n] of Object.entries(update.$inc || {})) row[k] = (row[k] || 0) + n; return {modifiedCount: 1}; },
  };
}
const ledger = (head, source) => ({format: 1, head, versions: Array.from({length: head + 1}, (_, id) => ({id, parent: id ? id - 1 : null, source: id === head ? source : 'export function paint({wipe}){wipe(' + id + ');}', request: id ? 'req ' + id : null, createdAt: '2026-10-10T00:00:0' + id + 'Z', layers: 0}))});

test('a worker commit appends the next version and loses the race if the phone moved first', async () => {
  const c = collection(), store = mongoWhistlegraphStore(c, {name: () => 'wgWorka'});
  const row = await store.open('alice', '11111111-1111-4111-8111-111111111111');
  const base = 'export function paint({wipe}){wipe("blue");}';
  await store.save('alice', row._id, 0, ledger(2, base));
  const thread = await store.read('alice', 'wgWorka');
  const job = {owner: 'alice', code: 'wgWorka', request: {text: 'make it red'}, baseVersion: 2, baseHash: sourceHash(base)};
  const result = await commitVersion(store, job, thread, 'export function paint({wipe}){wipe("red");}', ['api-x']);
  assert.equal(result.versionID, 3); assert.equal(result.acceptance, 'unreviewed'); assert.deepEqual(result.notes, ['api-x']);
  const after = await store.read('alice', 'wgWorka');
  assert.equal(after.ledger.head, 3); assert.equal(after.ledger.versions[3].request, 'make it red'); assert.equal(after.ledger.versions[3].parent, 2);
  await assert.rejects(commitVersion(store, job, thread, 'export function paint({wipe}){wipe("green");}', []), {code: 'moved'}, 'a stale revision cannot fork the thread');
});

test('a turn refuses to run on a piece that moved past its base', async () => {
  const thread = {_id: 't', revision: 3, ledger: ledger(2, 'export function paint({wipe}){wipe(1);}')};
  await assert.rejects(runTurn({job: {baseVersion: 1, baseHash: 'x', request: {text: 'x'}}, thread}), {code: 'moved'});
});
