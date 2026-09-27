// Optional real-Mongo verification. Use an isolated Mongo server, never production:
// OSKIEWAR_TEST_MONGO=mongodb://127.0.0.1:27029 node --test system/backend/tests/oskiewar-leaderboard-mongo.test.mjs
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { COLLECTION, createLeaderboardHandler } from '../oskiewar-leaderboard.mjs';

test('real Mongo: concurrent consensus, aggregate ranking, current handles and pair stats', { skip: !process.env.OSKIEWAR_TEST_MONGO }, async () => {
  const uri = process.env.OSKIEWAR_TEST_MONGO;
  assert.match(uri, /^mongodb:\/\/127\.0\.0\.1:\d+\/?$/, 'Only an explicit isolated loopback test server is allowed');
  const { MongoClient } = await import('mongodb');
  const client = await new MongoClient(uri).connect();
  const db = client.db(`oskiewar_test_${randomUUID().replaceAll('-', '')}`);
  const users = [{ _id: 'auth0|test-a', handle: 'alice' }, { _id: 'auth0|test-b', handle: 'bob' }];
  try {
    await db.collection('@handles').insertMany(users);
    // Unauthenticated historical replays cannot become standings.
    await db.collection('oskiewar-replays').insertOne({ fighters: ['alice', 'bob'], finalRoundWins: [999, 0] });
    const handler = createLeaderboardHandler({
      authorize: async ({ authorization }) => {
        const user = users.find(u => authorization === `Bearer ${u._id}`);
        return user && { sub: user._id };
      },
      connect: async () => ({ db, disconnect: async () => {} }),
      respond: (statusCode, body) => ({ statusCode, body }),
    });
    const body = { matchId: 'ow-real-mongo-first', handles: ['alice', 'bob'], roundWins: [5, 2], winner: 0 };
    const send = (seat, extra = {}) => handler({ httpMethod: 'POST', headers: { authorization: `Bearer ${users[seat]._id}` },
      body: JSON.stringify({ ...body, seat, ...extra }) });
    const replies = await Promise.all(Array.from({ length: 30 }, (_, i) => send(i % 2)));
    assert.ok(replies.every(r => [200, 202].includes(r.statusCode)));
    assert.equal(await db.collection(COLLECTION).countDocuments({ confirmedAt: { $exists: true } }), 1);
    assert.equal((await send(1, { roundWins: [5, 3] })).statusCode, 409);
    const second = { matchId: 'ow-real-mongo-second', winner: 1, roundWins: [4, 5] };
    await send(1, second);
    await send(0, second);
    await send(0, { matchId: 'ow-real-mongo-pending' });
    const read = () => handler({ httpMethod: 'GET', queryStringParameters: { handles: 'alice,bob' } });
    const response = await read();
    assert.equal(response.statusCode, 200);
    assert.deepEqual(response.body.top.map(r => r.handle), ['alice', 'bob']);
    assert.deepEqual(response.body.players.map(r => [r.matchesPlayed, r.matchesWon, r.roundsWon, r.roundsLost]), [[2, 1, 9, 7], [2, 1, 7, 9]]);
    assert.equal(response.body.pair.matchesPlayed, 2);
    assert.equal(JSON.stringify(response.body).includes('auth0|'), false);
    assert.equal(JSON.stringify(response.body).includes('_id'), false);
    await db.collection('@handles').updateOne({ _id: users[0]._id }, { $set: { handle: 'alicia' } });
    const renamed = await handler({ httpMethod: 'GET', queryStringParameters: { handles: 'alicia,bob' } });
    assert.equal(renamed.body.players[0].handle, 'alicia');
    assert.equal(renamed.body.players[0].matchesWon, 1);
    assert.equal(renamed.body.pair.matchesPlayed, 2);
    const indexes = await db.collection(COLLECTION).indexes();
    assert.ok(indexes.some(i => i.expireAfterSeconds === 0 && i.key.expiresAt === 1));
    assert.equal(await db.collection(COLLECTION).countDocuments({ confirmedAt: { $exists: true }, expiresAt: { $exists: true } }), 0);
  } finally {
    await db.dropDatabase();
    await client.close();
  }
});
