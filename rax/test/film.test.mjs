import test from 'node:test';
import assert from 'node:assert/strict';
import { pose, aim, turn } from '../film/camera.mjs';
import { studio, director } from '../film/shoot.mjs';
import { inCar } from '../elevator.mjs';

const near = (a, b, e = 1e-6) => assert.ok(Math.abs(a - b) < e, `${a} ≉ ${b}`);

test('the camera aims the way the world measures bearings', () => {
  near(aim({ x: 0, y: 0, z: 0 }, { x: 0, y: 0, z: 5 }).yaw, 0);
  near(aim({ x: 0, y: 0, z: 0 }, { x: 5, y: 0, z: 0 }).yaw, 90);
  near(aim({ x: 0, y: 0, z: 0 }, { x: 0, y: 5, z: 5 }).pitch, 45);
  assert.equal(turn(350, 10), 20);
  assert.equal(turn(10, 350), -20);
});

test('shots: still, move, follow, orbit', () => {
  const who = { x: 2, y: 1.7, z: 3, yaw: 0 }, find = (h) => (h === 'jeffrey' ? who : null);
  const still = pose({ kind: 'still', at: [0, 2, -4], look: 'jeffrey' }, 0, find);
  assert.deepEqual([still.x, still.y, still.z, still.zoom], [0, 2, -4, 1]);
  near(still.yaw, aim(still, who).yaw);
  const move = { kind: 'move', from: [0, 1, 0], to: [10, 1, 0], seconds: 2, ease: 'linear', yaw: 90 };
  near(pose(move, 1, find).x, 5);
  near(pose(move, 9, find).x, 10); // holds its end
  const follow = pose({ kind: 'follow', who: 'jeffrey', back: 4, up: 1 }, 0, find);
  near(follow.z, -1); near(follow.x, 2); near(follow.y, 2.7); near(follow.yaw, 0);
  const orbit = { kind: 'orbit', around: 'jeffrey', radius: 5, height: 3, period: 20 };
  near(Math.hypot(pose(orbit, 7, find).x - 2, pose(orbit, 7, find).z - 3), 5);
  // An absent actor leaves the camera where it was.
  const kept = { x: 1, y: 1, z: 1, yaw: 0, pitch: 0, zoom: 1 };
  assert.equal(pose({ kind: 'follow', who: 'nobody' }, 0, find, kept), kept);
  // A smoothed shot closes part of the gap each frame.
  const eased = pose({ kind: 'still', at: [10, 1, 1], yaw: 0, smooth: 6 }, 0, find, kept, 1 / 60);
  assert.ok(eased.x > 1 && eased.x < 10);
});

test('an actor walks into the tower and rides to its floor', async () => {
  const film = { name: 'test', offline: true, handles: ['jeffrey', 'oskie'] };
  const set = await studio(film);
  const d = director(film, set);
  try {
    await set.app.tower.change((t) => { t.arrive('jeffrey', 'film-jeffrey'); return t.take('jeffrey', 89); });
    const jeffrey = await d.cast('jeffrey', { at: [-4, -6], facing: 40 });
    assert.ok(Math.abs(jeffrey.me.x + 4) < 0.5 && Math.abs(jeffrey.me.z + 6) < 0.5, 'stands on the mark');
    const began = Date.now();
    await jeffrey.walkTo(0, -1, { pace: 0.5 });
    const strolled = (Date.now() - began) / 1000;
    assert.ok(strolled > 0.6, `a walk, not a dash (${strolled}s)`);
    await jeffrey.walkTo(0, 6, { pace: 0.5 });
    assert.ok(inCar(jeffrey.me));
    const ride = await jeffrey.ride('home');
    assert.deepEqual(ride, { from: 0, to: 89, handle: 'jeffrey' });
    assert.equal(jeffrey.tower.here.number, 89);
    assert.ok(inCar(jeffrey.me), 'arrives in the car');
    await jeffrey.walkTo(0, 2, { pace: 0.6 });
    assert.ok(!inCar(jeffrey.me), 'and can step out');
  } finally {
    for (const a of d.actors) a.leave();
    await set.close();
  }
});
