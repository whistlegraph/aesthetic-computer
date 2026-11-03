import test from 'node:test';
import assert from 'node:assert/strict';
import { fitRoom } from '../object-store.mjs';
import { WORLD } from '../world.mjs';

test('builds from a larger room scale inside the walls, keeping their layout', () => {
  const list = [{ x: 30, z: -12 }, { x: -6, z: 60 }, { x: 0, z: 0 }];
  assert.ok(fitRoom(list));
  const limit = WORLD - 1.5;
  for (const o of list) {
    assert.ok(Math.abs(o.x) <= limit && Math.abs(o.z) <= limit);
  }
  assert.equal(list[2].x, 0);
  assert.ok(Math.abs(list[0].x / list[0].z - 30 / -12) < 0.05, 'ratios survive');
  assert.equal(fitRoom([{ x: 1, z: -2 }]), false, 'a room already inside is left alone');
});
