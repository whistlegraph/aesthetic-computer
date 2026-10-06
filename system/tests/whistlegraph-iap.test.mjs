import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { BUNDLE_ID, PRODUCT_ID, purchaseGrant, purchaseState, makeVerifiers, whistlegraphPurchases } from '../backend/whistlegraph-iap.mjs';
import { createHandler, handler as liveHandler } from '../netlify/functions/whistlegraph-iap.mjs';

const copy = value => value == null ? value : structuredClone(value);
const duplicate = () => Object.assign(new Error('duplicate'), { code: 11000 });
const get = (doc, path) => path.split('.').reduce((value, key) => value?.[key], doc);
function set(doc, path, value) {
  const keys = path.split('.'), last = keys.pop();
  for (const key of keys) doc = doc[key] ??= {};
  doc[last] = copy(value);
}
function matches(doc, query) {
  return Object.entries(query).every(([key, expected]) => {
    if (key === '$or') return expected.some(q => matches(doc, q));
    const actual = get(doc, key);
    if (Array.isArray(expected)) return JSON.stringify(actual) === JSON.stringify(expected);
    if (expected && typeof expected === 'object') return Object.entries(expected).every(([op, value]) => {
      if (op === '$exists') return (actual !== undefined) === value;
      if (op === '$lt') return actual !== undefined && actual < value;
      if (op === '$lte') return actual !== undefined && actual <= value;
      throw Error(`Unexpected query operator: ${op}`);
    });
    return actual === expected;
  });
}
// Evaluate the small aggregation subset used by the production update, rather
// than replacing that update with a test-only refund algorithm.
function evaluate(expr, doc) {
  if (expr === '$$NOW') return new Date();
  if (typeof expr === 'string' && expr.startsWith('$')) return get(doc, expr.slice(1));
  if (Array.isArray(expr)) return expr.map(value => evaluate(value, doc));
  if (!expr || typeof expr !== 'object') return expr;
  const [[op, raw]] = Object.entries(expr);
  if (op === '$literal') return raw;
  const values = evaluate(raw, doc);
  if (op === '$ifNull') return values[0] ?? values[1];
  if (op === '$in') return values[1].includes(values[0]);
  if (op === '$cond') return values[0] ? values[1] : values[2];
  if (op === '$add') return values.reduce((a, b) => a + b, 0);
  if (op === '$subtract') return values[0] - values[1];
  if (op === '$setUnion') return [...new Set(values.flat())];
  throw Error(`Unexpected aggregation expression: ${op}`);
}
function collection() {
  const rows = new Map();
  return {
    rows,
    async insertOne(doc) { if (rows.has(doc._id)) throw duplicate(); rows.set(doc._id, copy(doc)); },
    async findOne(query) { return copy([...rows.values()].find(doc => matches(doc, query))); },
    find(query) {
      let found = [...rows.values()].filter(doc => matches(doc, query));
      return { sort() { return this; }, limit(limit) { found = found.slice(0, limit); return this; }, async toArray() { return copy(found); } };
    },
    async deleteOne(query) { for (const [id, doc] of rows) if (matches(doc, query)) { rows.delete(id); return { deletedCount: 1 }; } return { deletedCount: 0 }; },
    async deleteMany(query) { for (const [id, doc] of rows) if (matches(doc, query)) rows.delete(id); },
    async updateOne(query, update, options = {}) {
      let doc = rows.get(query._id);
      if (!doc && options.upsert) { doc = { _id: query._id, ...copy(update.$setOnInsert) }; rows.set(doc._id, doc); }
      if (!doc || !matches(doc, query)) return { modifiedCount: 0, matchedCount: 0 };
      for (const stage of Array.isArray(update) ? update : [update]) {
        const before = copy(doc);
        for (const [key, value] of Object.entries(stage.$set || {})) set(doc, key, Array.isArray(update) ? evaluate(value, before) : value);
        for (const key of typeof stage.$unset === 'string' ? [stage.$unset] : Object.keys(stage.$unset || {})) delete doc[key];
      }
      return { modifiedCount: 1, matchedCount: 1 };
    },
  };
}
function walletCollection() {
  const base = collection();
  return { ...base, failRefund: false,
    async updateOne(filter, update, options) {
      if (Array.isArray(update)) {
        if (this.failRefund) { this.failRefund = false; throw Error('wallet temporarily offline'); }
      }
      return base.updateOne(filter, update, options);
    },
  };
}
function fixture({ sandboxUsers = [] } = {}) {
  const accounts = collection(), purchases = collection(), notifications = collection(), wallets = walletCollection(), deletions = collection(), tombstones = collection();
  const payloads = new Map();
  const verifier = environment => ({ environment,
    async verifyAndDecodeTransaction(jws) { const p = payloads.get(jws); if (!p || p.environment !== environment) throw Error('signature or environment'); return copy(p); },
    async verifyAndDecodeNotification(jws) { const p = payloads.get(jws); if (!p || p.data?.environment !== environment) throw Error('signature or environment'); return copy(p); },
  });
  const service = whistlegraphPurchases({ accounts, purchases, notifications, wallets, deletions, tombstones, verifiers: ['Production', 'Sandbox'].map(verifier), sandboxUsers: new Set(sandboxUsers) });
  const handle = createHandler({ service, authorize: async headers => { if (!headers.authorization) throw Error('no auth'); return { sub: headers.authorization }; } });
  const post = (body, user = 'alice') => handle({ httpMethod: 'POST', headers: user ? { authorization: user } : {}, body: JSON.stringify(body) });
  async function tx(name = 'purchase', user = 'alice', changes = {}) {
    const account = await service.account(user);
    const value = { transactionId: '2000000001', bundleId: BUNDLE_ID, productId: PRODUCT_ID, type: 'Consumable', quantity: 1,
      environment: 'Production', appAccountToken: account.appAccountToken, signedDate: 1000, ...changes };
    payloads.set(name, value); return value;
  }
  let signedDate = 2000;
  function note(name, transaction, environment = 'Production', changes = {}) {
    payloads.set(name, { notificationType: 'REFUND', notificationUUID: randomUUID(), signedDate: signedDate++,
      data: { bundleId: BUNDLE_ID, environment, signedTransactionInfo: transaction }, ...changes });
  }
  return { accounts, purchases, notifications, wallets, deletions, tombstones, payloads, service, post, tx, note };
}

test('purchase account UUID is stable under retries and unique to the signed-in account', async () => {
  const f = fixture();
  const accounts = await Promise.all(Array.from({ length: 20 }, () => f.service.account('alice')));
  assert.equal(new Set(accounts.map(x => x.appAccountToken)).size, 1);
  assert.notEqual((await f.service.account('bob')).appAccountToken, accounts[0].appAccountToken);
  assert.equal((await f.post({ action: 'account' }, null)).statusCode, 401);
});

test('one verified purchase adds one pack across concurrent retries', async () => {
  const f = fixture(); await f.tx();
  const responses = await Promise.all(Array.from({ length: 20 }, () => f.post({ action: 'redeem', jws: 'purchase' })));
  assert.ok(responses.every(r => r.statusCode === 200));
  assert.equal(responses.filter(r => JSON.parse(r.body).credited).length, 1);
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
  assert.equal(f.purchases.rows.size, 1);
});

test('a signed transaction cannot be redeemed by a different AC account', async () => {
  const f = fixture(); await f.tx();
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' }, 'bob')).statusCode, 403);
  assert.equal(f.wallets.rows.size, 0);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 200);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' }, 'bob')).statusCode, 403);
  assert.equal(f.wallets.rows.size, 1);
});

test('transaction claims remain globally bound even if a different signed payload reuses its ID', async () => {
  const f = fixture(); await f.tx(); await f.service.redeem('alice', 'purchase');
  await f.tx('other-owner', 'bob');
  await assert.rejects(f.service.redeem('bob', 'other-owner'), /another purchase/);
  assert.equal(f.wallets.rows.has('bob'), false);
});

test('rejects invalid app, product, type, quantity, token, transaction ID and environment', async () => {
  const f = fixture(); const transaction = await f.tx();
  for (const patch of [{ bundleId: 'wrong' }, { productId: 'wrong' }, { type: 'Auto-Renewable Subscription' },
    { quantity: undefined }, { quantity: 0 }, { quantity: 2 }, { transactionId: '../bad' },
    { appAccountToken: undefined }, { appAccountToken: 'not-a-uuid' }, { environment: 'Xcode' }]) {
    assert.throws(() => purchaseGrant({ ...transaction, ...patch }, 'Production'));
  }
  assert.equal((await f.post({ action: 'redeem', jws: 'forged' })).statusCode, 401);
  assert.equal(f.wallets.rows.size, 0);
});

test('sandbox money is restricted to explicitly allowed test accounts', async () => {
  const f = fixture({ sandboxUsers: ['reviewer'] });
  await f.tx('alice-sandbox', 'alice', { environment: 'Sandbox' });
  assert.equal((await f.post({ action: 'redeem', jws: 'alice-sandbox' })).statusCode, 403);
  await f.tx('review-sandbox', 'reviewer', { environment: 'Sandbox' });
  assert.equal((await f.post({ action: 'redeem', jws: 'review-sandbox' }, 'reviewer')).statusCode, 200);
  assert.equal(f.wallets.rows.has('alice'), false);
  assert.equal(f.wallets.rows.get('reviewer').balance, 1_000_000);
});

test('refunds before redemption cannot be bypassed with the original valid transaction', async () => {
  const f = fixture(); await f.tx(); f.note('refund', 'purchase');
  assert.equal((await f.post({ signedPayload: 'refund' }, null)).statusCode, 200);
  assert.equal(f.wallets.rows.get('alice').balance, 0);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 409);
  assert.equal(f.wallets.rows.get('alice').balance, 0);
});

test('a refund is applied once and spent credits become debt', async () => {
  const f = fixture(); await f.tx(); await f.service.redeem('alice', 'purchase');
  f.wallets.rows.get('alice').balance = 600_000; f.note('refund', 'purchase');
  await Promise.all(Array.from({ length: 5 }, () => f.service.notification('refund')));
  assert.equal(f.wallets.rows.get('alice').balance, -400_000);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 409);
  assert.equal(f.wallets.rows.get('alice').balance, -400_000);
});

test('wallet failure after refund claim is recoverable and stale redemption cannot mint credits', async () => {
  const f = fixture(); await f.tx(); await f.service.redeem('alice', 'purchase'); f.note('refund', 'purchase');
  f.wallets.failRefund = true;
  assert.equal((await f.post({ signedPayload: 'refund' }, null)).statusCode, 503);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 409);
  assert.equal(f.wallets.rows.get('alice').balance, 0);
  assert.equal((await f.post({ signedPayload: 'refund' }, null)).statusCode, 200);
  assert.equal(f.wallets.rows.get('alice').balance, 0);
});

test('a crash after the wallet grant but before acknowledgment does not double-credit', async () => {
  const f = fixture(); await f.tx();
  const update = f.purchases.updateOne.bind(f.purchases); let failOnce = true;
  f.purchases.updateOne = async (q, u) => { if (u.$set.deliveredAt && failOnce) { failOnce = false; throw Error('write interrupted'); } return update(q, u); };
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 503);
  const retry = await f.post({ action: 'redeem', jws: 'purchase' });
  assert.equal(retry.statusCode, 200); assert.equal(JSON.parse(retry.body).credited, false);
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
});

test('refund reversals restore exactly the revoked credits and duplicate delivery cannot double-credit', async () => {
  const f = fixture(); await f.tx(); await f.service.redeem('alice', 'purchase');
  f.note('refund', 'purchase'); await f.service.notification('refund');
  f.note('reversed', 'purchase', 'Production', { notificationType: 'REFUND_REVERSED' });
  await f.service.notification('reversed'); await f.service.notification('reversed');
  assert.equal(f.notifications.rows.size, 2);
  assert.ok([...f.notifications.rows.values()].every(row => row.status === 'applied' && !row.signedPayload));
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
  await f.service.notification('refund');
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000, 'older refund cannot undo reversal');
  assert.equal((await f.service.redeem('alice', 'purchase')).credited, false);
});

test('notification signature, app and environment must all match', async () => {
  const f = fixture(); await f.tx('sandbox', 'alice', { environment: 'Sandbox' });
  assert.equal((await f.post({ signedPayload: 'forged' }, null)).statusCode, 401);
  f.note('mixed', 'sandbox');
  assert.equal((await f.post({ signedPayload: 'mixed' }, null)).statusCode, 401);
  f.note('wrong-app', 'sandbox', 'Sandbox', { data: { bundleId: 'wrong', environment: 'Sandbox', signedTransactionInfo: 'sandbox' } });
  assert.equal((await f.post({ signedPayload: 'wrong-app' }, null)).statusCode, 400);
  assert.equal(f.wallets.rows.size, 0);
});

test('partial refunds use Apple milliunits and a reversal restores only the revoked amount', async () => {
  const f = fixture(); await f.tx(); await f.service.redeem('alice', 'purchase');
  f.wallets.rows.get('alice').balance = 600_000; // 400k already spent.
  await f.tx('partial', 'alice', { revocationDate: 1900, revocationType: 'REFUND_PRORATED', revocationPercentage: 25_000 });
  f.note('refund-quarter', 'partial'); await f.service.notification('refund-quarter');
  assert.equal(f.wallets.rows.get('alice').balance, 350_000);
  f.note('reversal', 'purchase', 'Production', { notificationType: 'REFUND_REVERSED' });
  await f.service.notification('reversal');
  assert.equal(f.wallets.rows.get('alice').balance, 600_000);
  assert.equal(f.wallets.rows.get('alice').grants.length, 1);
  for (const percent of [-1, 100_001, 1.5, '25', undefined]) {
    assert.throws(() => purchaseState({ signedDate: 1, revocationDate: 1, revocationType: 'REFUND_PRORATED', revocationPercentage: percent }));
  }
  assert.throws(() => purchaseState({ signedDate: undefined }));
});

test('all event orders converge to the newest signed state, including a second refund after a reversal', async () => {
  const permutations = values => values.length ? values.flatMap((v, i) => permutations(values.filter((_, j) => i !== j)).map(rest => [v, ...rest])) : [[]];
  for (const order of permutations(['first', 'reversal', 'last'])) {
    const f = fixture(); await f.tx();
    f.note('first', 'purchase', 'Production', { signedDate: 2000 });
    f.note('reversal', 'purchase', 'Production', { signedDate: 3000, notificationType: 'REFUND_REVERSED' });
    f.note('last', 'purchase', 'Production', { signedDate: 4000 });
    for (const event of order) await f.service.notification(event);
    assert.equal(f.wallets.rows.get('alice').balance, 0, order.join(','));
    assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 409);
    assert.equal(f.wallets.rows.get('alice').balance, 0);
  }
});

test('a reversal arriving first does not lose credit when the old refund eventually arrives', async () => {
  const f = fixture(); await f.tx();
  f.note('old-refund', 'purchase', 'Production', { signedDate: 2000 });
  f.note('reversed', 'purchase', 'Production', { signedDate: 3000, notificationType: 'REFUND_REVERSED' });
  await Promise.all(Array.from({ length: 20 }, (_, i) => f.service.notification(i % 2 ? 'old-refund' : 'reversed')));
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
  assert.equal(f.wallets.rows.get('alice').grants.length, 1);
});

test('a failed reversal remains retryable and a client retry repairs the intended balance', async () => {
  const f = fixture(); await f.tx(); f.note('refund', 'purchase'); await f.service.notification('refund');
  f.note('reversed', 'purchase', 'Production', { notificationType: 'REFUND_REVERSED' });
  f.wallets.failRefund = true;
  assert.equal((await f.post({ signedPayload: 'reversed' }, null)).statusCode, 503);
  assert.equal(f.wallets.rows.get('alice').balance, 0);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 200);
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
  await f.service.notification('reversed');
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
});

test('notifications cannot bypass the sandbox account allowlist', async () => {
  const f = fixture(); await f.tx('sandbox', 'alice', { environment: 'Sandbox' });
  f.note('refund', 'sandbox', 'Sandbox');
  f.note('reversed', 'sandbox', 'Sandbox', { notificationType: 'REFUND_REVERSED' });
  assert.equal((await f.post({ signedPayload: 'refund' }, null)).statusCode, 403);
  assert.equal((await f.post({ signedPayload: 'reversed' }, null)).statusCode, 403);
  assert.equal(f.wallets.rows.size, 0);
});

test('deleted claims acknowledge Apple without recreating wallets or rebinding the purchase', async () => {
  const f = fixture(); const tx = await f.tx(); await f.service.redeem('alice', 'purchase');
  const row = [...f.purchases.rows.values()][0]; row.deletedAt = new Date(); delete row.user; delete row.appAccountToken;
  f.accounts.rows.clear(); f.wallets.rows.clear();
  f.note('reversed', 'purchase', 'Production', { notificationType: 'REFUND_REVERSED' });
  assert.equal((await f.post({ signedPayload: 'reversed' }, null)).statusCode, 200);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 410);
  assert.equal(f.accounts.rows.size, 0); assert.equal(f.wallets.rows.size, 0); assert.equal(f.notifications.rows.size, 0);
  assert.equal(row.transactionId, tx.transactionId);
});

test('durable deletion locks reject account creation and redemption; cancelled deletion can reconcile queued refund', async () => {
  const f = fixture(); await f.tx(); await f.service.redeem('alice', 'purchase');
  f.deletions.rows.set('alice', { _id: 'alice', state: 'scheduled' });
  assert.equal((await f.post({ action: 'account' })).statusCode, 409);
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 409);
  f.note('refund', 'purchase'); assert.equal((await f.post({ signedPayload: 'refund' }, null)).statusCode, 409);
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
  f.deletions.rows.clear();
  assert.equal((await f.post({ action: 'redeem', jws: 'purchase' })).statusCode, 409);
  assert.equal(f.wallets.rows.get('alice').balance, 0);
  const hash = createHash('sha256').update('alice').digest('hex');
  f.tombstones.rows.set(hash, { _id: hash, completedAt: new Date() });
  assert.equal((await f.post({ action: 'account' })).statusCode, 409);
});

test('the operational runner repairs refunds after failures without waiting for another client or Apple request', async () => {
  const f = fixture(); await f.tx(); await f.service.redeem('alice', 'purchase');
  f.note('refund', 'purchase'); f.wallets.failRefund = true;
  assert.equal((await f.post({ signedPayload: 'refund' }, null)).statusCode, 503);
  assert.deepEqual(await f.service.reconcilePending(), { applied: 1, pending: 0 });
  assert.equal(f.wallets.rows.get('alice').balance, 0);
  f.note('reversed', 'purchase', 'Production', { notificationType: 'REFUND_REVERSED' });
  f.deletions.rows.set('alice', { _id: 'alice', state: 'scheduled' });
  await f.post({ signedPayload: 'reversed' }, null);
  assert.deepEqual(await f.service.reconcilePending(), { applied: 0, pending: 1 });
  f.deletions.rows.clear();
  for (const row of f.notifications.rows.values()) row.nextAttemptAt = new Date(0);
  assert.deepEqual(await f.service.reconcilePending(), { applied: 1, pending: 0 });
  assert.equal(f.wallets.rows.get('alice').balance, 1_000_000);
  assert.deepEqual(await f.service.reconcilePending(), { applied: 0, pending: 0 });
});

test('pausing new sales preserves paid delivery and Apple refund handling', async () => {
  const f = fixture(); await f.tx(); f.note('refund', 'purchase');
  const handler = createHandler({ service: f.service, authorize: async () => ({ sub: 'alice' }), salesEnabled: false });
  const request = body => handler({ httpMethod: 'POST', headers: {}, body: JSON.stringify(body) });
  assert.equal((await request({ action: 'account' })).statusCode, 503);
  assert.equal((await request({ action: 'redeem', jws: 'purchase' })).statusCode, 200);
  assert.equal((await request({ signedPayload: 'refund' })).statusCode, 200);
  assert.equal(f.wallets.rows.get('alice').balance, 0);
});

test('in-flight inserts crossing irreversible deletion cannot recreate raw account data', async () => {
  function purge(f) {
    f.accounts.rows.clear(); f.wallets.rows.clear(); f.notifications.rows.clear();
    for (const row of f.purchases.rows.values()) { row.deletedAt = new Date(); delete row.user; delete row.appAccountToken; }
    const hash = createHash('sha256').update('alice').digest('hex');
    f.tombstones.rows.set(hash, { _id: hash, completedAt: new Date() });
  }
  for (const stage of ['account', 'claim', 'wallet', 'notification']) {
    const f = fixture();
    if (stage !== 'account') await f.tx();
    const target = stage === 'account' ? f.accounts : stage === 'claim' ? f.purchases : stage === 'wallet' ? f.wallets : f.notifications;
    const method = stage === 'wallet' ? 'updateOne' : 'insertOne', original = target[method].bind(target);
    let once = true;
    target[method] = async (...args) => {
      if (once && (stage !== 'wallet' || args[1].$setOnInsert)) { once = false; purge(f); }
      return original(...args);
    };
    const body = stage === 'account' ? { action: 'account' } : { action: 'redeem', jws: 'purchase' };
    if (stage === 'notification') { f.note('refund', 'purchase'); delete body.action; delete body.jws; body.signedPayload = 'refund'; }
    const response = await f.post(body);
    assert.equal(response.statusCode, stage === 'account' ? 409 : stage === 'notification' ? 200 : 410, stage);
    assert.equal(f.accounts.rows.size, 0, stage); assert.equal(f.wallets.rows.size, 0, stage); assert.equal(f.notifications.rows.size, 0, stage);
    for (const row of f.purchases.rows.values()) {
      assert.ok(row.deletedAt, stage); assert.equal(row.user, undefined, stage); assert.equal(row.appAccountToken, undefined, stage);
    }
  }
});

test('the actual Apple verifier rejects an unsigned payload before delivery', async () => {
  const accounts = collection(), purchases = collection(), wallets = walletCollection();
  const service = whistlegraphPurchases({ accounts, purchases, wallets,
    verifiers: makeVerifiers({ appAppleId: 1, online: false }) }); // Fixture app ID; no network.
  const unsigned = Buffer.from(JSON.stringify({ alg: 'none' })).toString('base64url') + '.' +
    Buffer.from(JSON.stringify({ bundleId: BUNDLE_ID, productId: PRODUCT_ID })).toString('base64url') + '.';
  await assert.rejects(service.redeem('alice', unsigned), /Apple could not verify/);
  assert.equal(purchases.rows.size, 0);
  assert.equal(wallets.rows.size, 0);
});

test('unconfigured production verifier and malformed requests fail closed', async () => {
  assert.throws(() => makeVerifiers({ appAppleId: undefined }), /not configured/);
  const f = fixture();
  for (const body of [null, [], 12, 'text']) assert.equal((await f.post(body)).statusCode, 400);
  assert.equal((await f.post({ action: 'redeem', jws: 'a'.repeat(70_000) })).statusCode, 413);
  const saved = process.env.WHISTLEGRAPH_IAP_ENABLED; delete process.env.WHISTLEGRAPH_IAP_ENABLED;
  try { assert.equal((await liveHandler({ httpMethod: 'POST', body: '{}' })).statusCode, 503); }
  finally { if (saved === undefined) delete process.env.WHISTLEGRAPH_IAP_ENABLED; else process.env.WHISTLEGRAPH_IAP_ENABLED = saved; }
});
