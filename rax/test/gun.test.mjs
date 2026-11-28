import test from 'node:test';
import assert from 'node:assert/strict';
import { Match, CLIP } from '../match.mjs';
import { HZ, WORLD, slabOf, muzzleOf, muzzleAt, surfaceHit, spawn } from '../world.mjs';
import { rackFor } from '../armory.mjs';

// One body alone in a room (waiting), holding a gun: house or its own.
const armed = (house = true) => {
  const m = new Match(), seat = m.join();
  const [gun] = rackFor('lobby');
  const thing = house ? gun : { ...gun, id: 'made-gun', house: false };
  m.setObjects([thing]);
  Object.assign(m.seats[seat].player, { x: thing.x, z: thing.z, yaw: 0, pitch: 0 });
  for (let k = 0; k < HZ / 4; k++) m.step();
  assert.ok(m.held(seat), 'picked up');
  return { m, seat, it: m.held(seat) };
};
let seq = 0;
const press = (m, seat, extra = {}) => { m.input(seat, [{ seq: ++seq, fwd: 0, right: 0, yaw: 0, pitch: 0, use: true, ...extra }]); m.step(); };

test('a round leaves the barrel\'s end: forward of the hand by the gun\'s own length, along the aim', () => {
  const { m, seat, it } = armed();
  const muzzle = muzzleOf(rackFor('lobby')[0].geometry);
  assert.ok(muzzle && muzzle.z > 0.05, 'the pistol has a barrel that reaches forward');
  assert.deepEqual(it.muzzle, muzzle);
  const p = { ...spawn(0), x: 0, z: 0, yaw: 0, pitch: 0 }, hand = muzzleAt(p), end = muzzleAt(p, muzzle);
  assert.ok(Math.abs(end.z - hand.z - muzzle.z) < 1e-9 && Math.abs(end.y - hand.y - muzzle.y) < 1e-9 && Math.abs(end.x - hand.x) < 1e-9);
  // Aimed straight up, forward is up.
  const up = muzzleAt({ ...p, pitch: 85 }, muzzle), upHand = muzzleAt({ ...p, pitch: 85 });
  assert.ok(up.y - upHand.y > muzzle.z * 0.9);
  press(m, seat);
  const shot = m.events.find((e) => e.type === 'shot');
  const q = m.seats[seat].player, want = muzzleAt(q, muzzle);
  assert.ok(Math.hypot(shot.at.x - want.x, shot.at.y - want.y, shot.at.z - want.z) < 0.5, 'fired from there');
});

test('a gun holds a clip: the last round fired it is dropped, breaks on the ground, and a made gun is taken out of the room', () => {
  for (const house of [true, false]) {
    const { m, seat, it } = armed(house);
    assert.equal(it.ammo, CLIP);
    Object.assign(m.seats[seat].player, { x: 0, z: 0 });
    // (Fired at the sky: a round come back off a wall would end the body, and a body that falls drops its gun.)
    for (let k = 0; k < CLIP * HZ && it.holder === seat; k++) press(m, seat, { pitch: 80 });
    assert.ok(m.seats[seat].player.alive);
    assert.equal(it.holder, -1, 'out of the hand');
    assert.ok(it.spent || it.awayUntil, 'empty');
    Object.assign(m.seats[seat].player, { x: -7, z: -7 }); // (stand clear: nothing empty is picked up anyway)
    for (let k = 0; k < HZ * 2; k++) m.step();
    assert.ok(it.awayUntil > 0 && !it.spent, 'broke where it fell');
    if (house) {
      assert.deepEqual(m.broken, []);
      for (let k = 0; k < HZ * 4; k++) m.step();
      assert.equal(it.ammo, CLIP);
      assert.ok(it.y === 0 && Math.hypot(it.x - it.home.x, it.z - it.home.z) < 0.05, 'back on the rack, loaded');
    } else assert.deepEqual(m.broken, ['made-gun'], 'the server is told to take it out');
  }
});

test('alone in a room, a body\'s own round come back off a wall ends it: it gibs and is stood up again', () => {
  const m = new Match(), seat = m.join();
  assert.equal(m.phase, 'waiting');
  const p = m.seats[seat].player;
  Object.assign(p, { x: 0, z: 0 });
  // A round of its own, past its safe moment, flying at it.
  m.bullets.push({ x: p.x, y: p.y - 0.6, z: p.z - 2, vx: 0, vy: 0, vz: 32, owner: seat, born: m.tick, safeUntil: m.tick, bounces: 1, id: 999 });
  const life = p.life;
  for (let k = 0; k < 10 && m.seats[seat].player.alive; k++) m.step();
  assert.equal(m.seats[seat].player.alive, false);
  assert.ok(m.events.some((e) => e.type === 'gib' && e.seat === seat));
  for (let k = 0; k < HZ + 5; k++) m.step();
  assert.ok(m.seats[seat].player.alive && m.seats[seat].player.life === life + 1, 'stood up again');
});

test('off a platform a round meets no wall and no floor: only the block', () => {
  const slab = slabOf(WORLD), out = { x: 1, y: 0, z: 0 }, down = { x: 0, y: -1, z: 0 };
  assert.ok(surfaceHit({ x: 0, y: 1, z: 0 }, out, 100), 'a walled room stops it');
  assert.equal(surfaceHit({ x: 0, y: 1, z: 0 }, out, 100, [], 0, WORLD, null, slab), null);
  assert.equal(surfaceHit({ x: WORLD + 3, y: 5, z: 0 }, down, 100, [], 0, WORLD, null, slab), null, 'no floor beside the block');
  const top = surfaceHit({ x: 0, y: 5, z: 0 }, down, 100, [], 0, WORLD, null, slab);
  assert.ok(top && Math.abs(top.t - 5) < 1e-9 && top.normal.y === 1, 'the block\'s top is floor');
  const side = surfaceHit({ x: WORLD + 3, y: -1, z: 0 }, { x: -1, y: 0, z: 0 }, 100, [], 0, WORLD, null, slab);
  assert.ok(side && side.normal.x === 1, 'and its side is a wall');
});
