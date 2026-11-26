import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { ObjectStore } from '../object-store.mjs';
import { Match } from '../match.mjs';
import { HZ } from '../world.mjs';
import { rackFor, RACK_RETURN_SECONDS } from '../armory.mjs';

// 🔫 The rack beside the dispenser: three house guns in the lobby only,
// counted apart from what people built, and back in their slots when left.
test('the lobby lists three house guns beside the dispenser; other rooms none', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-armory-'));
  try {
    const s = new ObjectStore(dir);
    await s.load();
    const guns = s.objects('lobby').filter((o) => o.house);
    assert.equal(guns.length, 3);
    assert.ok(guns.every((g) => g.item === 'shoot' && g.geometry?.name === 'Pistol'));
    assert.equal(s.stored('lobby').length, 0, 'not counted as built things');
    assert.equal(s.objects('e1n1').filter((o) => o.house).length, 0);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test('a house gun picked up, carried off and dropped goes back to its slot', () => {
  const m = new Match();
  m.join(); m.join(); m.phase = 'playing';
  const [gun] = rackFor('lobby');
  m.setObjects(rackFor('lobby'));
  Object.assign(m.seats[0].player, { x: gun.x, z: gun.z, yaw: 0, pitch: 0 });
  Object.assign(m.seats[1].player, { x: -8, z: 8 });
  for (let k = 0; k < HZ / 4; k++) m.step();
  const held = m.held(0);
  assert.ok(held?.house, 'picked up');
  // Walk off and drop it.
  Object.assign(m.seats[0].player, { x: gun.x - 5, z: gun.z + 5 });
  m.step();
  m.input(0, [{ seq: 1, fwd: 0, right: 0, yaw: 0, pitch: 0, drop: true }]);
  m.step();
  assert.equal(held.holder, -1);
  for (let k = 0; k < HZ * 2; k++) m.step();
  assert.ok(Math.hypot(held.x - held.home.x, held.z - held.home.z) > 1, 'lying where it was dropped');
  // Walk well away so it is not picked up again, then wait it out.
  Object.assign(m.seats[0].player, { x: -8, z: -8 });
  for (let k = 0; k < (RACK_RETURN_SECONDS + 1) * HZ; k++) m.step();
  assert.ok(Math.hypot(held.x - held.home.x, held.z - held.home.z) < 0.05, 'back in its slot');
});

// 🎒 Carried over a seam: listed where it is now, not at home; home again when uncarried.
test('a house gun carried over a seam is listed in the new room and not on the rack, until it goes home', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-armory-'));
  try {
    const s = new ObjectStore(dir);
    await s.load();
    const moved = s.carry('house-gun-1', 'e1n0', { x: -7.5, z: 0 });
    assert.deepEqual(moved, { origin: 'lobby', room: 'e1n0', from: 'lobby' });
    assert.ok(!s.objects('lobby').some((o) => o.id === 'house-gun-1'), 'off the rack');
    const there = s.objects('e1n0').find((o) => o.id === 'house-gun-1');
    assert.ok(there?.carried && there.item === 'shoot' && there.x === -7.5);
    // On again, one more room over: still from the lobby.
    assert.equal(s.carry('house-gun-1', 'e2n0', { x: -7.5, z: 1 }).from, 'e1n0');
    assert.ok(!s.objects('e1n0').some((o) => o.id === 'house-gun-1'));
    assert.equal(s.uncarry('house-gun-1').origin, 'lobby');
    assert.ok(s.objects('lobby').some((o) => o.id === 'house-gun-1' && !o.carried), 'back on the rack');
    assert.ok(!s.objects('e2n0').some((o) => o.id === 'house-gun-1'));
    // Carried back into its own room: simply home.
    s.carry('house-gun-2', 'e1n0', { x: 0, z: 0 });
    s.carry('house-gun-2', 'lobby', { x: 0, z: 0 });
    assert.equal(s.carried.size, 0);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test('a carried-in thing left lying asks to go home', () => {
  const m = new Match();
  m.join(); m.join(); m.phase = 'playing';
  const [gun] = rackFor('lobby');
  m.setObjects([{ ...gun, x: 0, z: 0, house: false, carried: true }]);
  Object.assign(m.seats[0].player, { x: -8, z: -8 });
  Object.assign(m.seats[1].player, { x: 8, z: 8 });
  for (let k = 0; k < (RACK_RETURN_SECONDS + 1) * HZ; k++) m.step();
  assert.deepEqual(m.goingHome, [gun.id]);
});

// 🧟 The east room: two scarecrows and two zombies, all targets that take hits.
test('the east room stands scarecrows and zombies as targets', () => {
  const list = rackFor('east').filter((o) => o.target);
  assert.deepEqual(list.map((o) => o.geometry.name).sort(), ['Scarecrow', 'Scarecrow', 'Zombie', 'Zombie']);
  const m = new Match();
  m.setObjects(rackFor('east'));
  assert.equal(m.targets.length, 4);
});
