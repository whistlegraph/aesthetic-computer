import test from 'node:test';
import assert from 'node:assert/strict';
import { extractFields } from '../src/clairic.mjs';
import { createClairicHandler, clairicBudget, extractionRequest } from '../../system/backend/clairic.mjs';

const fields = { total: { type: 'number', decimals: 2, unit: 'USD' }, invoice: { type: 'text' } };
const answers = { total: { value: 310.5, confidence: 0.98 }, invoice: { value: 'INV-204', span: [8, 15] } };
const creds = { accountId: 'acct', token: 'secret' };
const post = (body, headers = { authorization: 'Bearer t' }) =>
  ({ httpMethod: 'POST', headers, body: typeof body === 'string' ? body : JSON.stringify(body) });

test('client posts to Workers AI and unwraps the REST envelope', async () => {
  let seen;
  const result = await extractFields({ state: 'Invoice INV-204, total $310.50', fields, referenceDate: '2026-11-17' }, {
    ...creds, fetchImpl: async (url, init) => { seen = { url, init }; return { ok: true, json: async () => ({ success: true, result: { answers, usage: { input_tokens: 40 } } }) }; },
  });
  assert.equal(seen.url, 'https://api.cloudflare.com/client/v4/accounts/acct/ai/run/@cf/cloudflare/clairic');
  assert.equal(seen.init.headers.Authorization, 'Bearer secret');
  assert.deepEqual(JSON.parse(seen.init.body), { model: 'clairic', state: 'Invoice INV-204, total $310.50', fields, reference_date: '2026-11-17' });
  assert.equal(result.answers.total.value, 310.5);
});

test('client refuses bad input, provider errors and missing answers', async () => {
  const never = async () => assert.fail('no request expected');
  await assert.rejects(extractFields({ state: 'x', fields }, { accountId: '', token: '', fetchImpl: never }), /CLOUDFLARE/);
  await assert.rejects(extractFields({ state: 'x', fields: { a: { type: 'choice' } } }, { ...creds, fetchImpl: never }), /typed fields/);
  await assert.rejects(extractFields({ state: '', fields }, { ...creds, fetchImpl: never }), /state text or images/);
  await assert.rejects(extractFields({ state: 'x', fields }, { ...creds, fetchImpl: async () => ({ ok: false, status: 429 }) }), /HTTP 429/);
  await assert.rejects(extractFields({ state: 'x', fields }, { ...creds,
    fetchImpl: async () => ({ ok: true, json: async () => ({ result: { answers: { total: {} } } }) }) }), /Missing answer: invoice/);
});

test('request shape is bounded before anything is spent', () => {
  assert.deepEqual(extractionRequest({ state: 'a', fields }), { state: 'a', fields, referenceDate: undefined });
  for (const bad of [
    { state: '', fields }, { state: 'a'.repeat(40_001), fields }, { state: 'a', fields: {} },
    { state: 'a', fields: { 'bad name': { type: 'text' } } }, { state: 'a', fields: { a: { type: 'choice' } } },
    { state: 'a', fields: { a: { type: 'text', instructions: 'x'.repeat(700) } } },
    { state: 'a', fields, referenceDate: 'tomorrow' }, { state: 'a', fields, images: ['x'] },
  ]) assert.throws(() => extractionRequest(bad));
});

test('handler needs a handle, spends budget, and never leaks provider errors', async () => {
  let spent = 0, calls = 0;
  const make = (over = {}) => createClairicHandler({ authenticate: async () => 'auth0|1',
    budget: { consume: async () => { spent++; return true; } },
    extract: async () => { calls++; return { answers, usage: { input_tokens: 40 }, model: 'clairic' }; }, ...over });
  const body = { state: 'Invoice INV-204, total $310.50', fields };

  assert.equal((await make()(post(body, {}))).statusCode, 401);
  assert.equal((await make({ authenticate: async () => null })(post(body))).statusCode, 401);
  assert.equal((await make()(post('{nope'))).statusCode, 400);
  assert.equal((await make()({ httpMethod: 'GET', headers: {} })).statusCode, 405);
  assert.equal(spent, 0);

  const ok = await make()(post(body));
  assert.equal(ok.statusCode, 200);
  assert.deepEqual(JSON.parse(ok.body).answers, answers);
  assert.equal(spent, 1); assert.equal(calls, 1);

  assert.equal((await make({ budget: { consume: async () => false } })(post(body))).statusCode, 429);
  const down = await make({ extract: async () => { throw new Error('token secret leaked'); } })(post(body));
  assert.equal(down.statusCode, 503); assert.doesNotMatch(down.body, /secret/);
});

test('budget stops at the per-minute limit', async () => {
  const rows = new Map();
  const collection = {
    async updateOne({ _id }, { $setOnInsert }) { if (!rows.has(_id)) rows.set(_id, { ...$setOnInsert }); },
    async findOneAndUpdate({ _id, count }) { const row = rows.get(_id); if (!row || row.count >= count.$lt) return null; row.count++; return row; },
  };
  const budget = clairicBudget(collection, { perMinute: 2, perDay: 10, globalPerDay: 10 });
  assert.equal(await budget.consume('u', 0), true);
  assert.equal(await budget.consume('u', 1), true);
  assert.equal(await budget.consume('u', 2), false);
  assert.equal(await budget.consume('other', 3), true);
});
