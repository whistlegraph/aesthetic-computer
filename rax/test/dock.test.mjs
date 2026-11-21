// 🖥️ Going in to a screen: where the eye stands, how far the lens closes, and the mouse kept on the face.
import test from 'node:test';
import assert from 'node:assert/strict';
import { dockPose, faceStandoff, boxStandoff, glide, smooth, cursorStep, rates } from '../dock.mjs';
import { panelHit } from '../panel.mjs';
import { direction } from '../world.mjs';

const scale = { sx: 0.6, sy: 1.1 }; // a wide picture

test('the eye stands square in front of the face, at any turn, and its middle is the middle of the picture', () => {
  for (const yaw of [0, 90, 180, 270, 37]) {
    const face = { x: 3, y: 1.5, z: -2, yaw, w: 1, h: 1.5, cols: 64, rows: 96 };
    const pose = dockPose(face, scale), hit = panelHit(face, pose, direction(pose.yaw, pose.pitch));
    assert.ok(hit, `seen from the front at ${yaw}`);
    assert.ok(Math.abs(hit.t - pose.back) < 1e-9 && Math.abs(hit.x - 32) < 1e-6 && Math.abs(hit.y - 48) < 1e-6, `centred at ${yaw}`);
  }
});

test('the face fills the picture and no more: the tighter of width and height fits, with the margin left', () => {
  for (const [w, h] of [[1, 1.5], [4, 2.25], [6, 1]]) {
    const pose = dockPose({ x: 0, y: 2, z: 0, yaw: 0, w, h }, scale, { margin: 0.1 });
    const across = (w / 2 / pose.back) * scale.sx * pose.zoom, up = (h / 2 / pose.back) * scale.sy * pose.zoom;
    assert.ok(Math.abs(Math.max(across, up) - 0.9) < 1e-9, `${w} by ${h} fills to the margin`);
    assert.ok(Math.min(across, up) <= 0.9);
  }
});

test('it stands no further back than there is air, and not at all where there is none', () => {
  const face = { x: 0, y: 1.5, z: 0, yaw: 0, w: 4, h: 2.25 };
  assert.ok(Math.abs(dockPose(face, scale, { clear: 2 }).back - 1.65) < 1e-9);
  assert.equal(dockPose(face, scale, { clear: 0.6 }), null);
  assert.equal(faceStandoff(face, 1), null);
});

test('a drone looks a face over from its front, and a build from its own side, clear of it', () => {
  const at = faceStandoff({ x: 0, y: 1.5, z: 5, yaw: 180, w: 1, h: 1.5 });
  assert.ok(at.z > 5 + 2 && Math.abs(at.x) < 1e-9, 'a face turned north is seen from the north');
  const box = { xMin: -2, xMax: 2, yMin: 0, yMax: 3, zMin: -1, zMax: 1 };
  const spot = boxStandoff(box, { x: 30, y: 9, z: 0 });
  assert.ok(spot.x > 2 + 1 && Math.abs(spot.z) < 1e-9 && spot.y > 1.5, 'east of it, where the drone was, outside it');
  assert.equal(boxStandoff(box, { x: 30, y: 9, z: 0 }, 2.5), null, 'no room: it stays where it is');
  assert.ok(Number.isFinite(boxStandoff(box, { x: 0, y: 9, z: 0 }).z), 'from right overhead it still has somewhere to go');
});

test('a glide turns the short way and ends where it was going', () => {
  const a = { x: 0, y: 1, z: 0, yaw: 350, pitch: -10, zoom: 1 }, b = { x: 4, y: 2, z: 8, yaw: 10, pitch: 0, zoom: 4 };
  const half = glide(a, b, 0.5), end = glide(a, b, smooth(2));
  assert.ok(Math.abs(half.yaw) < 1e-9 || Math.abs(half.yaw - 360) < 1e-9, 'through north, not round by the south');
  assert.ok(Math.abs(half.zoom - 2) < 1e-9, 'the lens closes by ratio');
  assert.deepEqual([end.x, end.y, end.z, end.yaw, end.pitch, end.zoom], [4, 2, 8, 10, 0, 4]);
});

test('the mouse stops at the face\'s edges', () => {
  const face = { cols: 64, rows: 96 };
  assert.deepEqual(cursorStep({ x: 32, y: 48 }, face, 5, -8), { x: 37, y: 40 });
  const far = cursorStep({ x: 32, y: 48 }, face, 500, -500);
  assert.ok(far.x < 64 && far.x > 63.9 && far.y === 0);
});

test('nothing changing is no rate at all, and each way of changing is its own', () => {
  const p = { x: 1, y: 2, z: 3, yaw: 90, pitch: 5, zoom: 2 };
  assert.deepEqual(rates(p, { ...p }, 1 / 60), { lens: 0, travel: 0, turn: 0 });
  const r = rates(p, { ...p, zoom: 2.02 }, 1 / 60);
  assert.ok(r.lens > 0 && r.travel === 0 && r.turn === 0);
  assert.ok(rates(p, { ...p, x: 1.05 }, 1 / 60).travel > 0.3);
  assert.ok(rates({ ...p, yaw: 359 }, { ...p, yaw: 1 }, 1 / 60).turn === 1, 'two degrees across north, not 358');
});
