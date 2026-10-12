import test from 'node:test';
import assert from 'node:assert/strict';
import {AccountConnection} from '../Resources/Web/account-connection.mjs';
import {handleCharacterColors} from '../../../aesel/src/handle-colors.mjs';

test('signed out, verified, missing handle, failure, and retry all finish checking', async () => {
  const states = [];
  let result = {sub: 'test', handle: 'maker'}, failure;
  const connection = new AccountConnection({
    verify: async () => { if (failure) throw failure; return result; },
    changed: state => states.push(state),
  });
  assert.equal(await connection.connect(''), null);
  assert.equal(states.at(-1).status, 'signedOut');
  assert.equal((await connection.connect('test-token')).handle, 'maker');
  assert.deepEqual(states.slice(-2).map(s => s.status), ['checking', 'ready']);
  result = {sub: 'test', handle: ''};
  await connection.connect('test-token');
  assert.equal(states.at(-1).status, 'needsHandle');
  failure = Object.assign(Error('private network details'), {code: 'offline'});
  await connection.connect('test-token');
  assert.equal(states.at(-1).status, 'failed');
  assert.match(states.at(-1).notice, /connection and retry/);
  assert.ok(!JSON.stringify(states).includes('private network details'));
  failure = null; result.handle = 'maker';
  await connection.connect('test-token');
  assert.equal(states.at(-1).status, 'ready', 'the same token can be retried');
  await connection.connect('', 'Sign in again to renew your AC session.');
  assert.equal(states.at(-1).status, 'failed', 'native renewal failures stay visible');
});

test('late verification cannot undo sign-out or replace a newer retry', async () => {
  const pending = [], states = [];
  const connection = new AccountConnection({verify: () => new Promise((resolve, reject) => pending.push({resolve,reject})), changed: s => states.push(s)});
  const old = connection.connect('old');
  await connection.connect('');
  pending[0].resolve({sub:'old',handle:'old'});
  assert.equal(await old, null);
  assert.equal(states.at(-1).status, 'signedOut');
  const first = connection.connect('same'), retry = connection.connect('same');
  pending[2].resolve({sub:'new',handle:'new'});
  assert.equal((await retry).handle, 'new');
  pending[1].reject(Error('late failure'));
  assert.equal(await first, null);
  assert.equal(states.at(-1).status, 'ready');
});

test('saved palettes update the current handle without surviving sign-out or failed fetches', async () => {
  const pending = [], palettes = [], states = [];
  const connection = new AccountConnection({
    verify: async token => ({sub: token, handle: token}), changed: state => states.push(state),
    loadColors: () => new Promise((resolve, reject) => pending.push({resolve, reject})),
    initialColors: handleCharacterColors, paletteChanged: palette => palettes.push(palette),
  });
  const old = await connection.connect('old');
  assert.equal(old.colors.length, 4, 'the @ character gets a fallback color too');
  await connection.connect('');
  pending[0].resolve([[1,2,3]]);
  await Promise.resolve();
  assert.deepEqual(palettes, [], 'an old palette cannot paint a signed-out account');
  const current = await connection.connect('new');
  const colors = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]];
  pending[1].resolve(colors);
  await Promise.resolve();
  assert.deepEqual(current.colors, colors);
  assert.deepEqual(palettes, [{handle:'new', colors}]);
  const fallback = await connection.connect('retry');
  pending[2].reject(Error('palette unavailable'));
  await Promise.resolve(); await Promise.resolve();
  assert.equal(fallback.colors.length, 6);
  assert.equal(states.at(-1).status, 'ready', 'palette errors do not turn into sign-in errors');
});
