import test from 'node:test';
import assert from 'node:assert/strict';
import {ledgerMark, ledgerText} from '../../../aesel/src/whistlegraph-thread.mjs';
import {compactLedgerCopies} from '../Resources/Web/legacy-storage.mjs';

const ledger = (n, extra = '') => ({format: 1, head: n, versions: Array.from({length: n + 1}, (_, id) => ({id, parent: id ? id - 1 : null, source: 'x'.repeat(2000) + extra + id, request: id ? 'r' + id : null, createdAt: '', layers: 0}))});
const memory = () => { const m = new Map(); return {get length() { return m.size; }, key: i => [...m.keys()][i] ?? null, getItem: k => m.get(k) ?? null, setItem: (k, v) => m.set(k, String(v)), removeItem: k => m.delete(k), clear: () => m.clear()}; };

test('a ledger mark is short, stable, and tells ledgers apart', () => {
  const a = ledgerMark(ledger(3)), b = ledgerMark(ledger(3)), c = ledgerMark(ledger(3, 'y')), d = ledgerMark(ledger(4));
  assert.equal(a, b);
  assert.notEqual(a, c); assert.notEqual(a, d);
  assert.ok(a.startsWith('mark:') && a.length < 40, a);
  assert.equal(ledgerMark(ledgerText(ledger(3))), a, 'marking the text equals marking the ledger');
  assert.equal(ledgerMark(a), a, 'a mark marks to itself');
});

test('compaction turns whole cloud-ledger copies into marks, live and parked', () => {
  const storage = memory();
  const open = ledger(5), parked = ledger(4);
  storage.setItem('whistlegraph-source-cloud-ledger', ledgerText(open));
  storage.setItem('whistlegraph-source-versions', JSON.stringify(open));
  storage.setItem('whistlegraph-archive-abc', JSON.stringify({identity: {id: 'abc', code: 'wgTest'}, ledger: parked, source: 'x', extras: {'-cloud-ledger': ledgerText(parked), '-cloud-revision': '5'}}));
  storage.setItem('whistlegraph-archive-def', JSON.stringify({identity: {id: 'def'}, ledger: parked, source: 'x', extras: {}}));
  storage.setItem('unrelated', 'keep');
  const before = [...Array(storage.length)].reduce((n, _, i) => n + storage.getItem(storage.key(i)).length, 0);
  const freed = compactLedgerCopies(storage, ledgerMark);
  const after = [...Array(storage.length)].reduce((n, _, i) => n + storage.getItem(storage.key(i)).length, 0);
  assert.equal(storage.getItem('whistlegraph-source-cloud-ledger'), ledgerMark(open));
  assert.equal(JSON.parse(storage.getItem('whistlegraph-archive-abc')).extras['-cloud-ledger'], ledgerMark(parked));
  assert.equal(JSON.parse(storage.getItem('whistlegraph-archive-abc')).extras['-cloud-revision'], '5');
  assert.equal(storage.getItem('unrelated'), 'keep');
  assert.equal(before - after, freed);
  assert.ok(freed > 20000, 'freed ' + freed);
  assert.equal(compactLedgerCopies(storage, ledgerMark), 0, 'a second pass frees nothing');
});
