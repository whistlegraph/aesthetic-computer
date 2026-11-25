import test from 'node:test';
import assert from 'node:assert/strict';
import { Gore, anglesOf, pose } from '../gore.mjs';

// The rotation project.mjs draw applies (yaw · pitch · roll), for checking anglesOf.
function rotate(p, yaw, pitch, roll) {
  const rx = p.x * Math.cos(roll) - p.y * Math.sin(roll), ry = p.x * Math.sin(roll) + p.y * Math.cos(roll);
  const py = ry * Math.cos(pitch) - p.z * Math.sin(pitch), pz = ry * Math.sin(pitch) + p.z * Math.cos(pitch);
  return { x: rx * Math.cos(yaw) + pz * Math.sin(yaw), y: py, z: -rx * Math.sin(yaw) + pz * Math.cos(yaw) };
}
const near = (a, b, e = 1e-6) => Math.abs(a.x - b.x) < e && Math.abs(a.y - b.y) < e && Math.abs(a.z - b.z) < e;

test('anglesOf gives the draw angles that carry a frame\'s axes where they belong', () => {
  for (let k = 0; k < 50; k++) {
    const [yaw, pitch, roll] = [Math.random() * 6 - 3, Math.random() * 3 - 1.5, Math.random() * 6 - 3];
    const X = rotate({ x: 1, y: 0, z: 0 }, yaw, pitch, roll), Y = rotate({ x: 0, y: 1, z: 0 }, yaw, pitch, roll), Z = rotate({ x: 0, y: 0, z: 1 }, yaw, pitch, roll);
    const a = anglesOf(X, Y, Z);
    assert.ok(near(rotate({ x: 1, y: 0, z: 0 }, a.yaw, a.pitch, a.roll), X) && near(rotate({ x: 0, y: 1, z: 0 }, a.yaw, a.pitch, a.roll), Y));
  }
});

const body = { x: 0, base: 0, z: 0, yaw: 0, pitch: 0, vx: 0, vy: 0, vz: 0 };
const run = (gore, seconds) => { for (let t = 0; t < seconds; t += 1 / 60) gore.step(1 / 60, [], 8); };
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z);

test('a body a round ends falls as a ragdoll, lies on the floor and keeps its bones', () => {
  const gore = new Gore();
  const doll = gore.gib({ seat: 0, kind: 'shot', zone: 'torso', dir: { x: 0, y: 0, z: 1 }, hit: { x: 0, y: 1, z: 0 } }, body);
  const rest = dist(pose(body).shoulderL, pose(body).handL);
  run(gore, 3);
  const ys = Object.values(doll.points).map((p) => p.at.y);
  assert.ok(ys.every(Number.isFinite), 'no NaN');
  assert.ok(Math.max(...ys) < 0.7, `lying down (highest joint ${Math.max(...ys).toFixed(2)} m)`);
  assert.ok(Math.min(...ys) >= 0.04, 'not through the floor');
  assert.ok(Math.abs(dist(doll.points.shoulderL.at, doll.points.handL.at) - rest) < 0.05, 'the arm kept its length');
  assert.ok(doll.points.pelvis.at.z > 0.1, 'pushed the way the round went');
  const parts = gore.draws().filter((d) => d.seat === 0).map((d) => d.part);
  assert.deepEqual(parts.sort(), ['hand', 'hand', 'head', 'limb', 'limb', 'limb', 'limb', 'torso']);
});

test('a head shot takes the head off: it flies, the neck spurts, and blood lands', () => {
  const gore = new Gore();
  const doll = gore.gib({ seat: 1, kind: 'shot', zone: 'head', dir: { x: 1, y: 0, z: 0 }, hit: { x: 0, y: 1.6, z: 0 } }, body);
  assert.ok(doll.gone.has('crown'));
  assert.equal(gore.gibs.filter((g) => g.part === 'gib-head').length, 1);
  assert.equal(gore.spurts.length, 1);
  run(gore, 1.5);
  assert.ok(!gore.draws().some((d) => d.part === 'head' && d.seat === 1), 'the body has no head');
  assert.ok(gore.splats.length > 0, 'blood on the floor');
  assert.ok(gore.gibs[0].at.x > 1, 'the head went the way of the round');
});

test('a blast takes a body all apart into pieces and meat, which come to rest', () => {
  const gore = new Gore();
  assert.equal(gore.gib({ seat: 2, kind: 'blast', dir: { x: 0, y: 0.3, z: 1 }, hit: { x: 0, y: 0.5, z: -0.5 } }, body), null);
  assert.equal(gore.dolls.length, 0);
  const kinds = gore.gibs.map((g) => g.part);
  assert.equal(kinds.filter((k) => k === 'gib-limb').length, 4);
  assert.equal(kinds.filter((k) => k === 'gib-meat').length, 9);
  run(gore, 6);
  assert.ok(gore.gibs.every((g) => Number.isFinite(g.at.x) && g.at.y >= -0.1));
  assert.ok(gore.gibs.filter((g) => g.resting).length > gore.gibs.length / 2, 'most have come to rest');
  assert.ok(gore.splats.length > 4, 'the room is stained');
});
