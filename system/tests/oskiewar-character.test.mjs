import assert from 'node:assert/strict';
import test, { mock } from 'node:test';
import { defaultCharacter, validateCharacter, characterEditorUrl } from '../public/aesthetic.computer/lib/oskiewar-character.mjs';

const rows = new Map();
let authorized = { sub: 'email|otp' }, current = { sub: 'auth0|owner', handle: 'tester' };
let identityCalls = [], writes = 0;
let locked = false;
const handles = [{ _id: 'auth0|owner', handle: 'tester' }, { _id: 'auth0|other', handle: 'other' }];
mock.module('../backend/authorization.mjs', { exports: { authorize: async () => authorized } });
mock.module('../backend/account-lock.mjs', { exports: { accountLocked: async () => locked } });
mock.module('../backend/oskiewar-identity.mjs', { exports: { fighterIdentity: async sub => {
  identityCalls.push(sub); return current;
} } });
mock.module('../backend/database.mjs', { exports: { connect: async () => ({
  disconnect: async () => {}, db: { collection: name => name === '@handles' ? {
    findOne: async query => handles.find(row => query.handle.$regex.test(row.handle)),
  } : {
    findOne: async query => rows.get(query._id),
    insertOne: async record => {
      if (rows.has(record._id)) throw Object.assign(Error('duplicate'), { code: 11000 });
      rows.set(record._id, structuredClone(record)); writes++;
    },
    findOneAndUpdate: async (query, update) => {
      const row = rows.get(query._id);
      if (!row || row.revision !== query.revision) return null;
      Object.assign(row, structuredClone(update.$set)); row.revision++; writes++;
      return structuredClone(row);
    },
  } },
}) } });
const { handler } = await import('../netlify/functions/oskiewar-character.mjs');
const call = async (method, body, query) => {
  const result = await handler({ httpMethod: method, headers: { authorization: 'Bearer test' },
    body: body && JSON.stringify(body), queryStringParameters: query });
  return { status: result.statusCode, headers: result.headers, body: result.body ? JSON.parse(result.body) : null };
};

test('appearance accepts only bounded colors and facial enums; QR contains only the handle', () => {
  const value = defaultCharacter();
  assert.deepEqual(validateCharacter(value), value);
  const clean = validateCharacter(value); clean.colors.head[0] = 1;
  assert.notEqual(value.colors.head[0], 1);
  for (const bad of [NaN, 256, -1, 0.5, '200']) {
    const candidate = defaultCharacter(); candidate.colors.leftArm[0] = bad;
    assert.throws(() => validateCharacter(candidate));
  }
  assert.throws(() => validateCharacter({ ...value, token: 'private' }));
  assert.throws(() => validateCharacter({ ...value, face: { ...value.face, eyes: 'script' } }));
  assert.equal(characterEditorUrl('@FIFI'), 'https://aesthetic.computer/oskiewarchar:@fifi');
  assert.equal(characterEditorUrl('../private?token=a'), '');
});

test('public reads contain only appearance and version, and no profile means preserve existing look', async () => {
  authorized = null; identityCalls = [];
  const result = await call('GET', null, { handle: 'tester' });
  assert.equal(result.status, 200);
  assert.deepEqual(result.body, { handle: 'tester', revision: 0, appearance: null });
  assert.equal(result.headers['Cache-Control'], 'no-store');
  assert.deepEqual(identityCalls, []);
  assert.equal((await call('GET', null, { handle: 'missing' })).status, 404);
  assert.equal((await call('GET')).status, 401);
  assert.equal((await call('POST', { handle: 'tester', baseRevision: 0, appearance: defaultCharacter() })).status, 401);
});

test('OTP alias edits its resolved owner only; concurrent and stale editors cannot overwrite', async () => {
  authorized = { sub: 'email|otp' }; writes = 0;
  const appearance = defaultCharacter(); appearance.colors.leftArm = [255, 100, 150];
  assert.equal((await call('POST', { handle: 'other', baseRevision: 0, appearance })).status, 403);
  assert.equal(writes, 0);
  const first = await call('POST', { handle: 'tester', baseRevision: 0, appearance });
  assert.equal(first.status, 200); assert.equal(first.body.revision, 1);
  assert.ok(identityCalls.includes('email|otp'));
  assert.ok(rows.has('auth0|owner')); assert.ok(!rows.has('email|otp'));
  assert.deepEqual(Object.keys(first.body).sort(), ['appearance', 'handle', 'revision']);
  const duplicate = await call('POST', { handle: 'tester', baseRevision: 0, appearance: defaultCharacter() });
  assert.equal(duplicate.status, 409); assert.equal(writes, 1);
  const updated = await call('POST', { handle: 'tester', baseRevision: 1, appearance: defaultCharacter() });
  assert.equal(updated.status, 200); assert.equal(updated.body.revision, 2);
  assert.equal((await call('POST', { handle: 'tester', baseRevision: 1, appearance })).status, 409);
  assert.equal(writes, 2);
  assert.deepEqual((await call('GET')).body, updated.body);
  assert.deepEqual((await call('GET', null, { handle: 'tester' })).body, updated.body);
});

test('malformed saves leave stored appearance intact', async () => {
  const before = structuredClone(rows.get('auth0|owner'));
  assert.equal((await call('POST', { handle: 'tester', baseRevision: 2, appearance: { colors: {} } })).status, 400);
  assert.equal((await call('POST', { handle: 'tester', appearance: defaultCharacter() })).status, 400);
  const oversized = await handler({ httpMethod: 'POST', headers: {}, body: 'x'.repeat(8193) });
  assert.equal(oversized.statusCode, 400);
  assert.deepEqual(rows.get('auth0|owner'), before);
});

test('frequent public-look reads reuse verified identity, but writes and owner locks remain fresh', async () => {
  const before=identityCalls.length;
  await call('GET'); await call('GET');
  assert.equal(identityCalls.length,before);
  locked=true;
  assert.equal((await call('GET')).status,401);
  locked=false;
  await call('POST',{handle:'tester',baseRevision:2,appearance:defaultCharacter()});
  assert.equal(identityCalls.length,before+1);
});
