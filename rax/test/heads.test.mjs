import test from 'node:test';
import assert from 'node:assert/strict';
import { Match, HEAD } from '../match.mjs';
import { HZ } from '../world.mjs';
import { rackFor } from '../armory.mjs';

const stage = () => {
  const m = new Match(), seat = m.join();
  m.waves = true; m.random = () => 0.25;
  m.setObjects(rackFor('origin'));
  Object.assign(m.seats[seat].player, { x: 0, z: 0 }); // (on the gun)
  return { m, seat };
};

test('the stage has one gun in its middle, and no head comes until someone takes it up', () => {
  const guns = rackFor('origin');
  assert.equal(guns.length, 1); assert.deepEqual([guns[0].x, guns[0].z, guns[0].item], [0, 0, 'shoot']);
  const { m, seat } = stage();
  Object.assign(m.seats[seat].player, { x: 5, z: 5 });
  for (let k = 0; k < HZ * 3; k++) m.step();
  assert.equal(m.heads.length, 0); assert.equal(m.wave, 0);
  Object.assign(m.seats[seat].player, { x: 0, z: 0 });
  for (let k = 0; k < HZ; k++) m.step();
  assert.ok(m.held(seat), 'the gun is in hand');
  assert.equal(m.wave, 1); assert.ok(m.heads.length >= 1);
  assert.ok(Math.hypot(m.heads[0].x, m.heads[0].z) > m.half, 'it comes from outside the room');
  assert.ok(m.snapshot().heads.length >= 1 && m.snapshot().wave === 1);
});

test('a head makes for the body, a round ends it, and the next wave is one more', () => {
  const { m, seat } = stage();
  for (let k = 0; k < HZ; k++) m.step();
  const first = Math.hypot(m.heads[0].x, m.heads[0].z);
  for (let k = 0; k < HZ; k++) m.step();
  assert.ok(Math.hypot(m.heads[0].x, m.heads[0].z) < first, 'nearer');
  const wave = m.wave, owed = HEAD.first + wave;
  let killed = 0;
  for (let k = 0; k < HZ * 60 && m.wave === wave; k++) {
    // (A round put right on each head, as a good shot is.)
    for (const h of m.heads) if (!m.bullets.some((b) => b.at === h.id)) { m.bullets.push({ at: h.id, x: h.x, y: h.y, z: h.z - 0.6, vx: 0, vy: 0, vz: 32, owner: seat, born: m.tick, safeUntil: m.tick + HZ, bounces: 0, id: 5000 + k }); killed++; }
    m.step();
  }
  assert.ok(m.seats[seat].player.alive, 'kept alive by shooting');
  assert.equal(m.wave, wave + 1);
  assert.ok(killed >= owed);
  assert.ok(m.events.some((e) => e.type === 'wave'));
});

test('a head that reaches a body ends it: the body comes apart, the wave is over, and it stands again a few seconds on', () => {
  const { m, seat } = stage();
  for (let k = 0; k < HZ; k++) m.step();
  let gibbed = false;
  for (let k = 0; k < HZ * 40 && m.seats[seat].player.alive; k++) { m.step(); gibbed ||= m.events.some((e) => e.type === 'gib' && e.seat === seat && e.kind === 'head'); }
  assert.equal(m.seats[seat].player.alive, false);
  assert.ok(gibbed);
  assert.deepEqual(m.deaths.at(-1), { seat, attacker: -1 });
  assert.equal(m.heads.length, 0); assert.equal(m.wave, 0);
  for (let k = 0; k < HZ * (HEAD.down + 0.5); k++) m.step();
  assert.ok(m.seats[seat].player.alive, 'stood up again');
});
