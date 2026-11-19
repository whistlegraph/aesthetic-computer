import test from 'node:test';
import assert from 'node:assert/strict';
import { WORLD, TOWER_HALF, MAX_HALF, WALLS, HZ, halfOf, wallsOf, move, spawn, spawnPoint, wallDistance } from '../world.mjs';
import { reconcile } from '../net.mjs';
import { Match } from '../match.mjs';
import { aimedSquare } from '../builders.mjs';

const run = (half, seconds = 8) => {
  let p = { ...spawn(0, 1, half), x: 0, z: 0, yaw: 90 };
  for (let i = 0; i < seconds * HZ; i++) p = move(p, { fwd: 1, right: 0, yaw: 90, pitch: 0, buttons: 0 }, [], 0, [], false, null, half);
  return p;
};

test('a room says its own size: the tower is one size, every other room the studio', () => {
  assert.equal(halfOf('lobby'), TOWER_HALF);
  assert.equal(halfOf('floor-0a1b2c3d'), TOWER_HALF);
  for (const room of ['studio', 'park-3-1', 'botbop', undefined]) assert.equal(halfOf(room), WORLD);
  assert.ok(MAX_HALF >= WORLD && MAX_HALF >= TOWER_HALF);
  assert.equal(wallsOf(WORLD), WALLS, 'the studio walls are the walls everything used before');
  assert.equal(wallsOf(24), wallsOf(24), 'one list for each size');
});

test('a body walks to the wall of the room it is in, whatever the size', () => {
  const far = Math.abs(run(24, 12).x);
  assert.ok(Math.abs(far - 23.6) < 1e-6, `stopped at ${far} in a 48 m room`);
  assert.ok(Math.abs(Math.abs(run(WORLD).x) - (WORLD - 0.4)) < 1e-6, 'and where it always did in the studio');
  assert.ok(Math.abs(Math.abs(run(undefined).x) - (WORLD - 0.4)) < 1e-6, 'no size given is the studio');
});

test('rays, spawns, prediction and the aimed square all take the room size', () => {
  const eye = { x: 0, y: 1.7, z: 0 }, east = { x: 1, y: 0, z: 0 };
  assert.ok(Math.abs(wallDistance(eye, east, 100, [], 0, 24) - 24) < 1e-6);
  assert.ok(Math.abs(wallDistance(eye, east, 100) - WORLD) < 1e-6);
  assert.ok(Math.hypot(spawnPoint(0, 24).x, spawnPoint(0, 24).z) > Math.hypot(spawnPoint(0).x, spawnPoint(0).z));
  const start = { ...spawn(0, 1, 24), x: 20, z: 0, yaw: 90 };
  const pending = Array.from({ length: HZ }, (_, i) => ({ seq: i + 1, fwd: 1, right: 0, yaw: 90, pitch: 0, buttons: 0 }));
  const big = reconcile(start, pending, 0, [], 0, [], false, null, 24).player;
  assert.ok(Math.abs(big.x) > 20.5, 'prediction in a big room is not pulled back to the studio wall');
  const looking = { x: 15, y: 1.7, z: 0, yaw: 0, pitch: -40 };
  assert.equal(aimedSquare(looking), null, 'past the studio');
  assert.ok(aimedSquare(looking, 24), 'inside a 48 m room');
  const match = new Match();
  assert.equal(match.half, WORLD);
});
