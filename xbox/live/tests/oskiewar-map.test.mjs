import test from 'node:test';
import assert from 'node:assert/strict';
import { validateMap } from '../oskiewar-map.mjs';
import { createMapsHandler } from '../../../system/netlify/functions/oskiewar-maps.mjs';
const map = () => ({ format: 'ac.oskiewar.map', version: 1, name: 'Moon yard',
  features: [{ from: 0, to: 40, kind: 'flat' }], decks: [{ col: 12, cols: 3, row: 4 }],
  spawns: [16, 23], pickups: [{ kind: 'HANDGUN', col: 20, amount: 6 }], skateboard: true });
test('map validation rejects gaps, nonfinite geometry, oversized lists and unknown items', () => {
  assert.equal(validateMap(map()).name, 'Moon yard');
  for (const change of [
    { features: [{ from: 1, to: 40, kind: 'flat' }] },
    { features: [{ from: 0, to: 39, kind: 'flat' }] },
    { spawns: [NaN, 2] }, { decks: [{ col: 39, cols: 5, row: 2 }] },
    { pickups: [{ kind: 'CODE', col: 1, amount: 1 }] },
    { pickups: Array(33).fill(map().pickups[0]) },
  ]) assert.throws(() => validateMap({ ...map(), ...change }));
  assert.equal(validateMap({ ...map(), script: 'bad' }).script, undefined);
});
test('map drafts are private; published versions are immutable, named and playable', async () => {
  const rows = new Map();
  const collection = {
    async updateOne({ _id }, update) {
      if (!rows.has(_id)) rows.set(_id, { _id, ...update.$setOnInsert });
    },
    async findOne(filter) {
      const row = rows.get(filter._id);
      return row && filter.$or.some(p => Object.entries(p).every(([k, v]) => row[k] === v)) ? row : null;
    },
  };
  const handler = createMapsHandler({
    authorize: async headers => headers.authorization ? { sub: headers.authorization } : null,
    connect: async () => ({ db: { collection: () => collection }, disconnect: async () => {} }),
  });
  const post = (owner, publish, value = map()) => handler({ httpMethod: 'POST',
    headers: { authorization: owner }, body: JSON.stringify({ map: value, publish }) });
  const get = (id, owner) => handler({ httpMethod: 'GET',
    headers: { authorization: owner }, queryStringParameters: { id } });
  assert.equal((await post(null, true)).statusCode, 401);
  const draft = JSON.parse((await post('alice', false)).body);
  assert.equal((await get(draft.id)).statusCode, 404);
  assert.equal((await get(draft.id, 'bob')).statusCode, 404);
  assert.equal((await get(draft.id, 'alice')).statusCode, 200);
  const published = JSON.parse((await post('alice', true)).body);
  assert.match(published.url, /\/workshop\?map=[a-f0-9]{64}$/);
  assert.equal((await get(published.id)).statusCode, 200);
  assert.equal(JSON.parse((await post('alice', true)).body).id, published.id);
  const changed = JSON.parse((await post('alice', true, { ...map(), name: 'Moon yard 2' })).body);
  assert.notEqual(changed.id, published.id);
  assert.equal(JSON.parse((await get(published.id)).body).map.name, 'Moon yard');
  assert.notEqual(JSON.parse((await post('bob', true)).body).id, published.id);
});
