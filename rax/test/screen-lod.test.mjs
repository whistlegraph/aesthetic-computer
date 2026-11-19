import test from 'node:test';
import assert from 'node:assert/strict';
import { SCREEN_TIERS, screenDetail, screenSeen, screenRows, carryPixels } from '../screen-lod.mjs';
import { clearOfCar, inCar, CAR, CAR_Z, CAR_WALL } from '../elevator.mjs';

test('a screen sharpens and speeds up as the eye walks up to it', () => {
  const walk = [60, 30, 15, 6, 2].map((d) => screenDetail(d));
  for (let i = 1; i < walk.length; i++) {
    assert.ok(walk[i].cols > walk[i - 1].cols, 'finer grid nearer');
    assert.ok(walk[i].hz >= walk[i - 1].hz, 'never slower nearer');
  }
  assert.deepEqual(walk[0], { cols: 16, hz: 2 });
  assert.deepEqual(walk.at(-1), { cols: 192, hz: 20 });
  for (const t of SCREEN_TIERS) assert.equal(screenRows(t.cols) * 16, t.cols * 9, 'every grid is 16:9');
});

test('a grid is held a little past its edge, so a boundary does not flicker', () => {
  assert.equal(screenDetail(10.5).cols, 64);
  assert.equal(screenDetail(10.5, { cols: 128 }).cols, 128, 'walking away keeps the finer grid');
  assert.equal(screenDetail(13, { cols: 128 }).cols, 64, 'and lets it go further out');
  assert.equal(screenDetail(9.5, { cols: 64 }).cols, 128, 'walking up sharpens at the edge itself');
});

test('a screen out of sight computes nothing and keeps the grid it has', () => {
  assert.deepEqual(screenDetail(2, { cols: 64, seen: false }), { cols: 64, hz: 0 });
  assert.deepEqual(screenDetail(30, { seen: false }), { cols: 16, hz: 0 });
  const eye = { x: 0, z: 0 }, north = { x: 0, z: 1 };
  assert.ok(screenSeen(eye, north, { x: 0, z: 12 }));
  assert.ok(!screenSeen(eye, north, { x: 0, z: -12 }), 'behind the eye');
  assert.ok(screenSeen(eye, north, { x: 0, z: -2 }), 'a screen at your back within reach still counts');
});

test('a coarse picture carries into a finer grid', () => {
  const from = new Uint8ClampedArray([255, 0, 0, 255, 0, 0, 255, 255]); // red, blue: 2x1
  const to = carryPixels(from, 2, 1, new Uint8ClampedArray(4 * 4 * 2), 4, 2);
  for (const j of [0, 1]) {
    assert.deepEqual([...to.slice((j * 4 + 1) * 4, (j * 4 + 1) * 4 + 3)], [255, 0, 0]);
    assert.deepEqual([...to.slice((j * 4 + 2) * 4, (j * 4 + 2) * 4 + 3)], [0, 0, 255]);
  }
});

test('no screen stands on the car, in its shaft or across its door', () => {
  const reach = CAR + CAR_WALL + 2; // half a screen's width
  for (let x = -7; x <= 7; x += 0.5)
    for (let z = -7; z <= 7; z += 0.5) {
      const at = clearOfCar(x, z);
      assert.ok(!inCar({ x: at.x, z: at.z, y: 0 }), `(${x}, ${z}) lands in the car`);
      const beside = Math.abs(at.x) >= reach, before = at.z <= CAR_Z - CAR - CAR_WALL - 2;
      assert.ok(beside || before, `(${x}, ${z}) → (${at.x}, ${at.z}) overlaps the car`);
    }
  assert.deepEqual(clearOfCar(-5, -3), { x: -5, z: -3 }, 'open floor is left alone');
  assert.equal(clearOfCar(0, CAR_Z).z < CAR_Z - CAR - 2, true, 'from inside the car, back from its door');
});
