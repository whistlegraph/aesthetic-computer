// 🪐 The planets hung over home: a bike is flown to one, rests on it, its
// rider gets off and walks its top, and past its edge falls home again.
import test from 'node:test';
import assert from 'node:assert/strict';
import { move, spawn, CFG, HZ } from '../world.mjs';
import { BIKE } from '../bike.mjs';
import { HUNG, CAP, hungFor, topOf } from '../orbits.mjs';
import { SPAN, LAP } from '../park.mjs';

const cmd = (c = {}) => ({ fwd: 0, right: 0, yaw: 0, pitch: 0, buttons: 0, ...c });
const run = (p, c, ticks, fixed, open = ['east', 'west', 'north', 'south']) => { for (let i = 0; i < ticks; i++) p = move(p, c, [], 0, fixed, false, open); return p; };
const far = (p, b) => Math.hypot(p.x - b.x, p.y - b.y, p.z - b.z);

test('each planet is small, near the tower, clear of the land, and whole inside one block', () => {
  for (const b of HUNG) {
    assert.ok(b.r <= 8 && Math.hypot(b.x, b.z) < 60, `${b.id} is a short flight off`);
    assert.ok(b.y - b.r > 12 && b.y + b.r + 3 < BIKE.top, 'over the land and under where a bike stops');
    assert.ok(b.x % SPAN === 0 && b.z % SPAN === 0);
    const own = hungFor({ i: b.x / SPAN, j: b.z / SPAN });
    assert.deepEqual([own[0].x, own[0].z], [0, 0], 'over the middle of its block');
  }
});

test('a room has the planets in its own metres, the lap round, and only the near ones', () => {
  const [pinch] = HUNG;
  const beside = hungFor({ i: pinch.x / SPAN + 1, j: pinch.z / SPAN }).find((b) => b.id === pinch.id);
  assert.equal(beside.x, -SPAN);
  const round = hungFor({ i: pinch.x / SPAN + 1 - LAP, j: pinch.z / SPAN }).find((b) => b.id === pinch.id);
  assert.equal(round.x, -SPAN, 'one lap round is the same place');
  assert.equal(hungFor({ i: 20, j: 20 }).length, 0);
  assert.equal(hungFor(null).length, 0);
  assert.equal(hungFor({ i: 0, j: 0 }, 19, Infinity)[0].y, pinch.y - 19, 'from a floor that high');
});

test('a bike pointed at a planet from the plaza is flown there, and rests on its top', () => {
  const [pinch] = hungFor({ i: 0, j: 0 }, 0, Infinity);
  let p = { ...spawn(0), x: 0, y: CFG.eyeHeight, z: -5, onGround: true };
  p = move(p, cmd({ bike: true }), [], 0, [pinch], false, []);
  // (One big room for the sum: the page and the server hand the body from block to block on the way.)
  const aim = () => { const dx = pinch.x - p.x, dy = pinch.y + pinch.r + 2 - p.y, dz = pinch.z - p.z; return { yaw: (Math.atan2(dx, dz) * 180) / Math.PI, pitch: (Math.atan2(dy, Math.hypot(dx, dz)) * 180) / Math.PI }; };
  let ticks = 0;
  while (Math.hypot(p.x - pinch.x, p.z - pinch.z) > 1 && ticks++ < HZ * 20) p = move(p, cmd({ fwd: 1, ...aim() }), [], 0, [pinch], false, null, 64);
  assert.ok(ticks < HZ * 8, `there in ${(ticks / HZ).toFixed(1)} s, only by looking and going`);
  for (let i = 0; i < HZ * 3; i++) p = move(p, cmd({ duck: true }), [], 0, [pinch], false, null, 64);
  const rest = p;
  for (let i = 0; i < HZ; i++) p = move(p, cmd({ duck: true }), [], 0, [pinch], false, null, 64);
  assert.ok(Math.hypot(p.x - rest.x, p.z - rest.z) < 0.3, 'at rest there: it does not slide off the round of it');
  const over = p.y - topOf(pinch, p.x, p.z);
  assert.ok(Math.abs(over - (BIKE.hover + BIKE.seat + CFG.eyeHeight)) < 0.05, `hovering on its top (${over.toFixed(2)} over it)`);
});

test('a rider gets off, stands and walks the top, and past the edge falls to the land', () => {
  const fixed = hungFor({ i: HUNG[0].x / SPAN, j: HUNG[0].z / SPAN }), b = fixed[0];
  let p = { ...spawn(0), x: 0, y: b.y + b.r + 2.7, z: 0, vx: 0, vy: 0, vz: 0, bike: true, onGround: false };
  p = run(move(p, cmd({ bike: true }), [], 0, fixed), cmd(), HZ, fixed);
  assert.equal(p.onGround, true);
  assert.ok(Math.abs(p.y - CFG.eyeHeight - (b.y + b.r)) < 1e-6, 'standing on its very top');
  // Out toward the edge the ground falls away under the feet, and the feet keep to it.
  for (let i = 0; i < 20; i++) {
    p = move(p, cmd({ fwd: 1, yaw: 90 }), [], 0, fixed);
    const top = topOf(b, p.x, p.z);
    if (top === null) break;
    assert.ok(p.onGround && Math.abs(p.y - CFG.eyeHeight - top) < 1e-6, `on the ground at x ${p.x.toFixed(2)}`);
  }
  assert.ok(p.x > 2, 'walked out from the middle');
  // A jump on it comes back down on it.
  let q = run({ ...p, x: 0, z: 0, y: b.y + b.r + CFG.eyeHeight, vx: 0, vz: 0, onGround: true }, cmd({ buttons: 1 }), 1, fixed);
  assert.ok(q.vy > 0 && !q.onGround);
  q = run(q, cmd(), HZ * 2, fixed);
  assert.ok(q.onGround && Math.abs(q.y - CFG.eyeHeight - (b.y + b.r)) < 1e-6);
  // Walked on past the edge: down to the land, never through the planet.
  let nearest = Infinity;
  for (let i = 0; i < HZ * 6 && p.y > CFG.eyeHeight + 0.01; i++) {
    p = move(p, cmd({ fwd: 1, yaw: 90 }), [], 0, fixed, false, ['east', 'west', 'north', 'south']);
    nearest = Math.min(nearest, far({ ...p, y: p.y - 0.8 }, b));
  }
  assert.ok(Math.abs(p.y - CFG.eyeHeight) < 0.02 && p.onGround, 'on the land under it');
  assert.ok(nearest > b.r - 0.01, 'and not through it');
  assert.equal(topOf(b, b.r * CAP + 0.1, 0), null, 'past the cap is no ground');
});

test('a bike never gets inside a planet, from any side', () => {
  const fixed = hungFor({ i: HUNG[1].x / SPAN, j: HUNG[1].z / SPAN }), b = fixed[0];
  for (const [yaw, pitch, from] of [[90, 0, [-14, b.y + 1.3, 0.4]], [0, 60, [0.3, b.y - 14, -6]], [200, -50, [2, b.y + 15, 5]], [270, 5, [14, b.y + 3, -0.2]]]) {
    let p = { ...spawn(0), x: from[0], y: from[1], z: from[2], vx: 0, vy: 0, vz: 0, bike: true, onGround: false }, nearest = Infinity;
    for (let i = 0; i < HZ * 5; i++) { p = move(p, cmd({ fwd: 1, yaw, pitch }), [], 0, fixed, false, ['east', 'west', 'north', 'south']); nearest = Math.min(nearest, far({ ...p, y: p.y - 1.3 }, b)); }
    assert.ok(nearest > b.r + 0.5, `kept out, flown at it ${yaw}/${pitch} (${nearest.toFixed(2)})`); // (its seat clear of the ball: over the top it hovers on it, at the side it is held off)
  }
});
