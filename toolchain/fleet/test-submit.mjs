import assert from 'node:assert/strict';
import { test } from 'node:test';
import { isLocalCandidate } from './submit.mjs';

test('remote-only excludes hostname aliases, loopback, and the local tailnet IP', () => {
  const interfaces = { utun: [{ address: '100.64.0.2' }] };
  for (const candidate of [
    { name: 'neo', ip: 'some-alias' },
    { name: 'alias', ip: '100.64.0.2' },
    { name: 'alias', ip: '127.0.0.1' },
    { name: 'alias', ip: '::1' },
    { name: 'alias', ip: 'localhost' },
  ]) assert.equal(isLocalCandidate(candidate, 'Neo.local', interfaces), true);
  assert.equal(isLocalCandidate({ name: 'chicken', ip: '100.64.0.3' }, 'Neo.local', interfaces), false);
});
