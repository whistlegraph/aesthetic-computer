import test from 'node:test';
import assert from 'node:assert/strict';
import {AccountConnection} from '../Resources/Web/account-connection.mjs';

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
