import test from 'node:test';
import assert from 'node:assert/strict';
import { crossings, describeCrossing, placed } from '../solids.mjs';
import { carSolids, carScreenSolids, CAR_SCREEN_Z, CAR, CAR_WALL, CAR_HEIGHT } from '../elevator.mjs';

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
