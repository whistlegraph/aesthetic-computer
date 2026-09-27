import test from 'node:test';
import assert from 'node:assert/strict';
import { COLLECTION, createLeaderboardHandler, validateResult, canonicalResult, standingsPipeline } from '../oskiewar-leaderboard.mjs';

const accounts = [{ _id: 'auth0|a', handle: 'alice' }, { _id: 'auth0|b', handle: 'bob' }];
const result = { matchId: 'ow-befaru-nitova-gopasu', seat: 0, handles: ['alice', 'bob'], roundWins: [5, 2], winner: 0 };
const get = (object, key) => key.split('.').reduce((o, k) => o?.[k], object);
function matches(doc, query) {
  return Object.entries(query).every(([key, value]) => value && typeof value === 'object' && '$exists' in value
    ? (get(doc, key) !== undefined) === value.$exists : get(doc, key) === value);
}
function fixture() {
  const docs = new Map();
  const ledger = {
    async insertOne(doc) { if (docs.has(doc._id)) throw Object.assign(Error('Duplicate'), { code: 11000 }); docs.set(doc._id, structuredClone(doc)); },
    async findOne(query) { return structuredClone([...docs.values()].find(d => matches(d, query))); },
    async updateOne(query, change) {
      const doc = [...docs.values()].find(d => matches(d, query));
      if (!doc) return { matchedCount: 0 };
      for (const [path, value] of Object.entries(change.$set || {})) {
        const keys = path.split('.'); let object = doc;
        for (const key of keys.slice(0, -1)) object = object[key] ||= {};
        object[keys.at(-1)] = structuredClone(value);
      }
      for (const key of Object.keys(change.$unset || {})) delete doc[key];
      return { matchedCount: 1 };
    },
    async createIndex() {},
    aggregate(pipeline) {
      assert.ok(pipeline[0].$match.confirmedAt.$exists, 'unconfirmed rows excluded');
      return { toArray: async () => [{ top: [], players: [] }] };
    },
  };
  const handleCollection = {
    async findOne(query) { return accounts.find(a => matches(a, query)); },
    find() { return { toArray: async () => [] }; },
  };
  const handler = createLeaderboardHandler({
    authorize: async headers => {
      const account = accounts.find(a => headers.authorization === `Bearer ${a._id}`);
      return account && { sub: account._id };
    },
    connect: async () => ({ db: { collection: name => name === COLLECTION ? ledger : handleCollection }, disconnect: async () => {} }),
    respond: (statusCode, body) => ({ statusCode, body }),
    now: () => new Date('2026-09-24T12:00:00Z'),
  });
  return { handler, docs, ledger };
}
const post = (body = result, sub = 'auth0|a') => ({ httpMethod: 'POST', headers: { authorization: `Bearer ${sub}` }, body: JSON.stringify(body) });
test('two distinct independently authenticated reports record exactly one match', async () => {
  const { handler, docs } = fixture();
  assert.equal((await handler(post())).statusCode, 202);
  let doc = docs.get(result.matchId);
  assert.equal(doc.confirmedAt, undefined);
  assert.ok(doc.expiresAt);
  assert.equal((await handler(post({ ...result, seat: 1 }, 'auth0|b'))).body.status, 'recorded');
  doc = docs.get(result.matchId);
  assert.ok(doc.confirmedAt);
  assert.equal(doc.expiresAt, undefined);
  assert.deepEqual(doc.players.map(p => p.subject), ['auth0|a', 'auth0|b']);
  assert.equal(JSON.stringify(doc).includes('Bearer'), false);
});

test('anonymous, wrong-seat and client-supplied identity cannot record', async () => {
  const { handler, docs } = fixture();
  assert.equal((await handler(post(result, 'forged'))).statusCode, 401);
  assert.equal((await handler(post({ ...result, seat: 1, subject: 'auth0|b' }))).statusCode, 403);
  assert.equal(docs.size, 0);
});

test('same account cannot occupy both seats, missing handles and incomplete rounds fail', async () => {
  const { handler, docs } = fixture();
  for (const invalid of [
    { handles: ['alice', '@ALICE'] }, { handles: ['alice', 'missing'] },
    { roundWins: [1, 0] }, { roundWins: [5, 5] }, { winner: 1 },
    { roundWins: [5, 2.5] }, { seat: 2 }, { handles: [{ $ne: null }, 'bob'] },
  ]) assert.equal((await handler(post({ ...result, ...invalid }))).statusCode, 400);
  assert.equal(docs.size, 0);
});

test('conflicting result never confirms, matching retry can complete', async () => {
  const { handler, docs } = fixture();
  await handler(post());
  assert.equal((await handler(post({ ...result, seat: 1, roundWins: [5, 3] }, 'auth0|b'))).statusCode, 409);
  assert.equal(docs.get(result.matchId).confirmedAt, undefined);
  assert.equal((await handler(post({ ...result, seat: 1 }, 'auth0|b'))).statusCode, 200);
});

test('concurrent reports and retries are idempotent; completed result is immutable', async () => {
  const { handler, docs } = fixture();
  const submissions = Array.from({ length: 30 }, (_, i) => handler(post({ ...result, seat: i % 2 }, i % 2 ? 'auth0|b' : 'auth0|a')));
  await Promise.all(submissions);
  assert.equal(docs.size, 1);
  const before = structuredClone(docs.get(result.matchId));
  assert.equal((await handler(post())).statusCode, 200);
  assert.deepEqual(docs.get(result.matchId), before);
  assert.equal((await handler(post({ ...result, roundWins: [1, 5], winner: 1 }))).statusCode, 409);
  assert.deepEqual(docs.get(result.matchId), before);
});

test('repeated single-account reports do not confirm', async () => {
  const { handler, docs } = fixture();
  await Promise.all(Array.from({ length: 10 }, () => handler(post())));
  assert.equal(docs.get(result.matchId).confirmedAt, undefined);
  assert.deepEqual(Object.keys(docs.get(result.matchId).reports), ['0']);
});

test('canonical digest ignores body identities, spelling and unrelated client fields', () => {
  const expected = canonicalResult(result, accounts).digest;
  assert.equal(canonicalResult({ ...result, handles: ['@ALICE', '@BOB'], token: 'fake', subject: 'fake' }, accounts).digest, expected);
  assert.notEqual(canonicalResult({ ...result, roundWins: [5, 3] }, accounts).digest, expected);
  assert.notEqual(canonicalResult(result, accounts.toReversed()).digest, expected);
});

test('strict bounded input, CORS and public read behavior', async () => {
  const { handler } = fixture();
  assert.equal((await handler({ ...post(), body: 'x'.repeat(4097) })).statusCode, 413);
  assert.equal((await handler({ ...post(), body: '{' })).statusCode, 400);
  assert.equal((await handler({ httpMethod: 'OPTIONS' })).statusCode, 204);
  assert.equal((await handler({ httpMethod: 'DELETE' })).statusCode, 405);
  assert.equal((await handler({ httpMethod: 'GET', queryStringParameters: { handles: 'a,b,c' } })).statusCode, 400);
  const response = await handler({ httpMethod: 'GET', queryStringParameters: { handles: '@ALICE,bob' } });
  assert.equal(response.statusCode, 200);
  assert.deepEqual(response.body.players.map(p => p.handle), ['alice', 'bob']);
  assert.equal(JSON.stringify(response.body).includes('auth0|'), false);
});

test('standings count only confirmed ledger matches and resolve current handles', () => {
  const pipeline = standingsPipeline();
  assert.deepEqual(pipeline[0], { $match: { confirmedAt: { $exists: true } } });
  assert.equal(pipeline[2].$group._id, '$players.subject');
  assert.equal(pipeline[3].$lookup.from, '@handles');
  assert.equal(validateResult(result), null);
});

test('public standings expose saved per-character colors even before the first ranked match',async()=>{
  const {readStandings}=await import('../oskiewar-leaderboard.mjs');
  const colors=Array.from({length:6},(_,i)=>({r:i,g:80,b:150,private:'discard'}));
  const db={collection:name=>name==='@handles'
    ? {find:()=>({toArray:async()=>[{_id:'private-sub',handle:'alice',colors}]})}
    : {aggregate:()=>({toArray:async()=>[{top:[{_id:'private-sub',handle:'alice',colors}],players:[]}]})}};
  const result=await readStandings(db,['alice']);
  for(const row of [result.top[0],result.players[0]]) {
    assert.deepEqual(row.colors,colors.map(({r,g,b})=>({r,g,b})));
    assert.equal(row._id,undefined);
  }
  assert.equal(result.players[0].matchesPlayed,0);
  assert.ok(!JSON.stringify(result).includes('private'));
});
