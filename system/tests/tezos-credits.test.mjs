import test from 'node:test';
import assert from 'node:assert/strict';
import { randomBytes } from 'node:crypto';
import { InMemorySigner } from '@taquito/signer';
import { b58cencode, prefix } from '@taquito/utils';
import { tezosPayments, quoteAmount, displayPrice, ownershipPayload, verifyOwnership, matchingPayment, TEZOS_TREASURY } from '../backend/tezos-credits.mjs';
import { createHandler } from '../netlify/functions/easel-tezos.mjs';

const time = new Date('2026-10-05T18:00:00Z');
const head = { chain:'mainnet', chainId:'NetXdQprcVkpaWU', synced:true, level:103, quoteLevel:103, timestamp:time.toISOString(), quoteUsd:0.4 };
const hash = 'o' + 'a'.repeat(50);
const sender = 'tz1inPpZMzFUv5mkmqDMEC8sYxmEq53vxRhw';
function collection() {
  const docs = new Map();
  const matches = (doc, filter) => Object.entries(filter).every(([key, value]) => {
    if (value && typeof value === 'object' && !(value instanceof Date)) {
      if ('$exists' in value) return (doc[key] !== undefined) === value.$exists;
      if ('$gt' in value) return doc[key] > value.$gt;
      if ('$ne' in value) return !Array.isArray(doc[key]) || !doc[key].includes(value.$ne);
    }
    return doc[key] === value;
  });
  return { docs,
    async findOne(filter) { const value = [...docs.values()].find(doc => matches(doc, filter)); return value ? structuredClone(value) : null; },
    async countDocuments(filter) { return [...docs.values()].filter(doc => matches(doc, filter)).length; },
    async insertOne(doc) { if (docs.has(doc._id)) throw Object.assign(new Error('duplicate'), { code:11000 }); docs.set(doc._id, structuredClone(doc)); },
    async updateOne(filter, update, opts = {}) {
      let doc = [...docs.values()].find(doc => matches(doc, filter));
      if (!doc && opts.upsert) { doc = { _id:filter._id, ...structuredClone(update.$setOnInsert) }; docs.set(doc._id, doc); }
      if (!doc) return { modifiedCount:0 };
      Object.assign(doc, structuredClone(update.$set || {}));
      for (const [key, val] of Object.entries(update.$inc || {})) doc[key] = (doc[key] || 0) + val;
      for (const [key, val] of Object.entries(update.$addToSet || {})) if (!doc[key].includes(val)) doc[key].push(val);
      return { modifiedCount:1 };
    },
  };
}
async function fixture() {
  const intents = collection(), claims = collection(), wallets = collection();
  let clock = time;
  const state = { head:{ ...head }, txs:[] };
  const p = tezosPayments({ intents, claims, wallets, now:() => clock, verify:(_i, proof) => proof.signature === 'valid',
    chain:async path => path === '/head' ? state.head : state.txs });
  const created = await p.create('user', '@test');
  const secret = new URL(created.checkoutURL).hash.slice(1);
  const quoted = await p.quote(secret, { address:sender, signature:'valid' });
  const intent = [...intents.docs.values()][0];
  const tx = { id:1234, hash, status:'applied', sender:{ address:sender }, target:{ address:TEZOS_TREASURY },
    amount:Number(quoted.amountMutez), timestamp:new Date(+time + 1000).toISOString(), level:100 };
  return { p, state, intents, claims, wallets, secret, intent, quoted, tx, advance:ms => { clock = new Date(+time + ms); } };
}

test('quotes use the existing $5 pack and reject stale or wrong-network prices', () => {
  assert.equal(quoteAmount(head, time), '12500000');
  for (const patch of [{ quoteUsd:0 }, { synced:false }, { chain:'ghostnet' }, { chainId:'other' }, { timestamp:'bad' }, { quoteLevel:0 }])
    assert.throws(() => quoteAmount({ ...head, ...patch }, time), /price|quote/);
  assert.throws(() => quoteAmount(head, new Date(+time + 3600_000)), /fresh/);
});

test('display conversions expose a dated, expiring rate without requiring a checkout', async () => {
  const price = () => displayPrice({ chain:async () => head, now:time });
  const response = await createHandler({ price })({ httpMethod:'POST', body:JSON.stringify({ action:'price' }) });
  assert.equal(response.statusCode, 200);
  const rate = JSON.parse(response.body);
  assert.equal(rate.usdPerTez, 0.4);
  assert.equal(rate.braincellsPerUSD, 200_000);
  assert.equal(rate.asOf, head.timestamp);
  assert.equal(Date.parse(rate.expiresAt) - +time, 5 * 60_000);
  await assert.rejects(displayPrice({ chain:async () => head, now:new Date(+time + 3600_000) }), /fresh/);
});

test('wallet proof is a real signed message bound to this checkout and AC account', async () => {
  const signer = new InMemorySigner(b58cencode(randomBytes(32), prefix.edsk2));
  const intent = { _id:'checkout', handle:'test', nonce:'random' };
  const signature = (await signer.sign(ownershipPayload(intent))).prefixSig;
  const proof = { address:await signer.publicKeyHash(), publicKey:await signer.publicKey(), signature };
  assert.equal(verifyOwnership(intent, proof), true);
  assert.equal(verifyOwnership({ ...intent, _id:'other' }, proof), false);
  assert.equal(verifyOwnership({ ...intent, handle:'someone-else' }, proof), false);
  assert.equal(verifyOwnership(intent, { ...proof, address:sender }), false);
});

test('only an applied, matching, sufficiently confirmed payment is credited once', async () => {
  const f = await fixture();
  assert.equal((await f.p.confirm(f.secret, hash)).status, 'confirming');
  f.state.txs = [f.tx]; f.state.head.level = 101;
  assert.equal((await f.p.confirm(f.secret, hash)).status, 'confirming');
  assert.equal(f.wallets.docs.size, 0);
  f.state.head.level = 102;
  await Promise.all([f.p.confirm(f.secret, hash), f.p.confirm(f.secret, hash)]);
  await f.p.confirm(f.secret, hash);
  assert.equal(f.wallets.docs.get('user').balance, 1_000_000);
  assert.equal(f.claims.docs.size, 1);
  assert.equal((await f.p.status(f.secret)).status, 'credited');
});

test('wrong amount, sender, recipient, status, dates and internal calls never grant credit', async () => {
  const f = await fixture();
  for (const patch of [{ amount:1 }, { sender:{ address:'other' } }, { target:{ address:'other' } }, { status:'backtracked' },
    { timestamp:'2026-10-05T17:59:59Z' }, { timestamp:'2026-10-05T18:16:00Z' }, { initiator:{ address:'other' } }, { nonce:0 }, { parameter:{} }]) {
    assert.equal(matchingPayment(f.intent, [{ ...f.tx, ...patch }], head), null);
    f.state.txs = [{ ...f.tx, ...patch }];
    await assert.rejects(f.p.confirm(f.secret, hash), /does not match/);
  }
  assert.equal(f.wallets.docs.size, 0);
});

test('expired quotes cannot be paid again, but an on-time payment can settle after expiry', async () => {
  const f = await fixture(); f.advance(20 * 60_000);
  await assert.rejects(f.p.quote(f.secret, { address:sender }), /expired/);
  f.state.txs = [f.tx];
  assert.equal((await f.p.confirm(f.secret, hash)).status, 'credited');
});

test('a payment cannot be claimed by another checkout or account', async () => {
  const f = await fixture(); f.state.txs = [f.tx];
  const other = await f.p.create('someone-else', '@other');
  const secret = new URL(other.checkoutURL).hash.slice(1);
  await f.p.quote(secret, { address:sender, signature:'valid' });
  await f.p.confirm(f.secret, hash);
  await assert.rejects(f.p.confirm(secret, hash), /another checkout/);
  assert.equal(f.wallets.docs.get('user').balance, 1_000_000);
  assert.equal(f.wallets.docs.has('someone-else'), false);
});

test('a crash after granting is recoverable and never grants twice', async () => {
  const f = await fixture(); f.state.txs = [f.tx];
  const update = f.intents.updateOne;
  f.intents.updateOne = async (filter, change, opts) => {
    if (change.$set?.status === 'credited') throw new Error('simulated crash');
    return update(filter, change, opts);
  };
  await assert.rejects(f.p.confirm(f.secret, hash), /simulated crash/);
  assert.equal(f.wallets.docs.get('user').balance, 1_000_000);
  f.intents.updateOne = update;
  assert.equal((await f.p.confirm(f.secret, hash)).status, 'credited');
  assert.equal(f.wallets.docs.get('user').balance, 1_000_000);
});

test('iOS return recovers a matching transfer without needing the wallet callback hash', async () => {
  const f = await fixture();
  assert.equal((await f.p.confirm(f.secret)).status, 'quoted');
  f.state.txs = [f.tx];
  assert.equal((await f.p.confirm(f.secret)).status, 'credited');
});

test('checkout creation requires AC sign-in; confirmation trusts the chain rather than client amounts', async () => {
  const f = await fixture();
  const h = createHandler({ payments:f.p, authorize:async headers => headers.authorization === 'Bearer auth' ? { sub:'user' } : null, handleFor:async () => '@test' });
  const post = (body, bearer = '') => ({ httpMethod:'POST', headers:{ authorization:`Bearer ${bearer}` }, body:JSON.stringify(body) });
  assert.equal((await h(post({ action:'create' }))).statusCode, 401);
  assert.equal((await h(post({ action:'create' }, 'auth'))).statusCode, 200);
  assert.equal((await h(post({ action:'status' }, 'invented'))).statusCode, 401);
  const result = await h(post({ action:'confirm', operationHash:hash, credits:99_000_000, paid:true }, f.secret));
  assert.equal(JSON.parse(result.body).status, 'confirming');
  assert.equal(f.wallets.docs.size, 0);
  const off = createHandler({ payments:f.p, enabled:false });
  assert.equal((await off(post({ action:'create' }))).statusCode, 503);
});
