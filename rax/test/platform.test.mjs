import test from 'node:test';
import assert from 'node:assert/strict';
import { move, spawn, slabOf, WORLD, CFG, FALL_Y, PLATFORM_DEPTH } from '../world.mjs';
import { Match } from '../match.mjs';

const walk = (p, cmd, fixed, n) => { for (let i = 0; i < n; i++) p = move(p, cmd, [], 0, fixed); return p; };

test('a platform is a volume: a body stands on its top, walks off its edge and falls, and its side is solid from below', () => {
  const slab = slabOf(WORLD), fixed = [slab];
  assert.equal(slab.yMax, 0); assert.equal(slab.yMin, -PLATFORM_DEPTH);
  let p = walk({ ...spawn(0), x: 0, z: 0, yaw: 0 }, { yaw: 0, pitch: 0, fwd: 0, right: 0, buttons: 0 }, fixed, 30);
  assert.ok(p.onGround && Math.abs(p.y - CFG.eyeHeight) < 1e-6, 'stands on the top');
  p = walk(p, { yaw: 0, pitch: 0, fwd: 1, right: 0, buttons: 0 }, fixed, 400);
  assert.ok(Math.hypot(p.x, p.z) > WORLD, 'past the edge, nothing held it');
  assert.ok(p.y < FALL_Y, 'and it fell');
  // In a walled room the same walk stops at the wall, on the ground.
  const held = walk({ ...spawn(0), x: 0, z: 0, yaw: 0 }, { yaw: 0, pitch: 0, fwd: 1, right: 0, buttons: 0 }, [], 400);
  assert.ok(Math.max(Math.abs(held.x), Math.abs(held.z)) <= WORLD && held.onGround);
  // Beside the block and below its top, a push inward is stopped by its side.
  let low = { ...spawn(0), x: WORLD + 1, z: 0, y: CFG.eyeHeight - 1, vy: 0, onGround: false, yaw: -90 };
  for (const yaw of [90, -90]) { const q = move({ ...low, yaw }, { yaw, pitch: 0, fwd: 1, right: 0, buttons: 0 }, [], 0, fixed); assert.ok(q.x >= WORLD, 'not inside the block'); }
});

test('a body fallen off a platform is stood back on it by the match', () => {
  const m = new Match();
  m.setWalls([slabOf(WORLD)]);
  const seat = m.join();
  const s = m.seats[seat];
  s.player = { ...s.player, x: WORLD + 3, y: FALL_Y - 1, onGround: false };
  m.step();
  assert.ok(m.seats[seat].player.y > 0, 'back on top');
});
