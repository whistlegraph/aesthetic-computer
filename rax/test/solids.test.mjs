import test from 'node:test';
import assert from 'node:assert/strict';
import { crossings, describeCrossing, placed } from '../solids.mjs';
import { carSolids, carScreenSolids, CAR_SCREEN_Z, CAR, CAR_WALL, CAR_HEIGHT } from '../elevator.mjs';
import { promenade, PROMENADE, GROUNDS, groundsOf, cellOf } from '../park.mjs';

const box = (x, y, z, w, h, d, color = 'a') => ({ x, y, z, w, h, d, color });

test('solids that only touch do not cross; solids that share volume do', () => {
  assert.deepEqual(crossings([box(0, 0, 0, 1, 1, 1), box(1, 0, 0, 1, 1, 1)]), []);
  const [found] = crossings([box(0, 0, 0, 1, 1, 1), box(0.9, 0, 0, 1, 1, 1)]);
  assert.deepEqual(found.overlap.map((n) => +n.toFixed(3)), [0.1, 1, 1]);
});

test('a crossing flickers when two faces in one plane differ in colour', () => {
  // The old car sign: as deep as the corner post and run through it.
  const sign = box(0, 2.6, 4.5, 3.4, 0.8, 0.2, 'sign'), post = box(1.6, 1.5, 4.5, 0.2, 3, 0.2, 'frame');
  const [found] = crossings([sign, post]);
  assert.equal(found.flickers, true);
  assert.match(describeCrossing(found), /crosses .* and flickers$/);
  // The same joint in one colour, or sunk clear of every face, does not show.
  assert.equal(crossings([{ ...sign, color: 'frame' }, post])[0].flickers, false);
  assert.equal(crossings([box(0, 0, 0, 2, 2, 2, 'a'), box(0, 0, 0, 1, 1, 1, 'b')])[0].flickers, false);
});

test('the elevator car is built of solids that never cross', () => {
  const car = [...carSolids(), ...carScreenSolids().map((s) => placed(s, 0, 0, CAR_SCREEN_Z, true))];
  assert.deepEqual(crossings(car).map(describeCrossing), []);
});

test('the car keeps its shape: four posts, a roof over all of it, a sign between the front posts', () => {
  const car = carSolids(), o = CAR + CAR_WALL;
  assert.equal(car.filter((s) => s.name === 'post').length, 4);
  const roof = car.find((s) => s.name === 'roof'), sign = car.find((s) => s.name === 'sign');
  assert.deepEqual([roof.w, roof.d, roof.y - roof.h / 2], [2 * o, 2 * o, CAR_HEIGHT]);
  assert.equal(sign.w, 2 * CAR);
  assert.ok(sign.y + sign.h / 2 <= CAR_HEIGHT);
});

test('the promenade is two blocks of pool, walks and rose beds, with nothing crossing', () => {
  assert.equal(promenade({ i: 1, j: -1 }), null, 'only the cells south of the door');
  let water = 0, roses = 0;
  for (const j of [-1, -2]) {
    const laid = promenade({ i: 0, j });
    assert.deepEqual(crossings(laid.solids).map(describeCrossing), []);
    for (const s of laid.solids) {
      assert.ok(Math.abs(s.x) + s.w / 2 <= 8 && Math.abs(s.z) + s.d / 2 <= 8, `${s.name} stays in its block`);
      assert.ok(s.y - s.h / 2 >= 0, `${s.name} stands on the ground`);
      if (s.name === 'rose') roses++;
    }
    // Pavings lie side by side, never one on another (they would flicker).
    for (const [n, a] of laid.flats.entries())
      for (const b of laid.flats.slice(n + 1)) {
        const over = Math.min(a.x1, b.x1) - Math.max(a.x0, b.x0) > 1e-6 && Math.min(a.z1, b.z1) - Math.max(a.z0, b.z0) > 1e-6;
        assert.ok(!over || a.ink === 'water' || b.ink === 'water', `${a.ink} lies on ${b.ink}`);
      }
    for (const f of laid.flats) {
      assert.ok(f.x0 >= -8 && f.x1 <= 8 && f.z0 >= -8 && f.z1 <= 8 && f.z1 > f.z0);
      if (f.ink === 'water') water += f.z1 - f.z0;
    }
  }
  assert.equal(+water.toFixed(3), PROMENADE.pool.north - PROMENADE.pool.south, 'the pool runs unbroken across both blocks');
  assert.ok(roses >= 40);
});

test('rooms know whether they are the tower\'s grounds', () => {
  assert.deepEqual(groundsOf('e0s1'), { i: 0, j: -1 });
  assert.deepEqual(groundsOf('w1n0'), { i: -1, j: 0 });
  for (const id of ['lobby', 'e1n0', 'e1n1', 'pod-abc', 'floor-12345678', 'studio']) assert.equal(groundsOf(id), null, id);
  assert.deepEqual(cellOf('lobby'), { i: 0, j: 0 });
  assert.equal(GROUNDS.length, 46); // three blocks out on every side, less the plaza and the two settled blocks
  assert.deepEqual(groundsOf('w3s3'), { i: -3, j: -3 });
  assert.equal(groundsOf('e4n0'), null, 'past the yard');
});
