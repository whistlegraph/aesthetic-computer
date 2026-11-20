import assert from 'node:assert/strict';
import test from 'node:test';
import { PLANET, bend, sight, hidden, depthRange, depthAt, widest, ballOf, landTiles, NEAR, FAR, SPLIT, SHARE } from '../planet.mjs';
import { Projector, layerBias } from '../project.mjs';
import { moveFreecam, FLOOR, DEEP } from '../freecam.mjs';
import { OskiewarScene3D } from '../../xbox/live/scene3d.mjs';

let seed = 11;
const rnd = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296);

test('the ball puts the land a radius from its middle, keeps its bearing, and stands things upright on it', () => {
  for (const R of [25, 140, 900]) for (const eye of [1.7, 300, -40]) {
    const centre = [0, -R - eye, 0];
    for (let n = 0; n < 200; n++) {
      const dx = (rnd() - 0.5) * 5 * R, dz = (rnd() - 0.5) * 5 * R, y = rnd() * 30;
      const [x, h, z] = bend(dx, 0, dz, R, eye), [tx, th, tz] = bend(dx, y, dz, R, eye);
      assert.ok(Math.abs(Math.hypot(x - centre[0], h - centre[1], z - centre[2]) - R) < 1e-6, 'the ground lies on the ball');
      assert.ok(Math.abs(Math.hypot(tx - centre[0], th - centre[1], tz - centre[2]) - (R + y)) < 1e-6, 'and a height is that far over it');
      assert.ok(Math.abs(x * dz - z * dx) < 1e-6 * R * R && x * dx + z * dz >= -1e-9, 'on the bearing it had');
    }
    assert.deepEqual(bend(0, 0, 0, R, eye).map((v) => Math.round(v * 1e6) / 1e6), [0, -eye, 0], 'the ground under the eye is straight under it');
  }
});

test('nothing that can be seen is ever called hidden', () => {
  // The line from the eye to a point clears the ball when it comes no nearer the middle than the radius.
  const clear = (R, eye, p) => {
    const e = [0, 0, 0], c = [0, -R - eye, 0], d = p.map((v, i) => v - e[i]), len = Math.hypot(...d);
    const t = Math.max(0, Math.min(1, -(d[0] * (e[0] - c[0]) + d[1] * (e[1] - c[1]) + d[2] * (e[2] - c[2])) / (len * len)));
    return Math.hypot(e[0] + d[0] * t - c[0], e[1] + d[1] * t - c[1], e[2] + d[2] * t - c[2]) >= R - 1e-7;
  };
  let seen = 0, kept = 0, dropped = 0;
  for (let n = 0; n < 6000; n++) {
    const R = [25, 140, 900][n % 3], eye = 0.3 + rnd() * rnd() * 400;
    const d = rnd() * 3.5 * R, y = rnd() * rnd() * 60, r = 0.2 + rnd() * 12, turn = rnd() * 7;
    const gone = hidden(R, eye, d, y, r);
    if (gone) dropped++;
    for (let k = 0; k < 24; k++) {
      // A point of the thing, in the flat world: within r of its middle, not under the land.
      const a = rnd() * 7, b = rnd() * r, up = Math.max(0, y + (rnd() - 0.5) * 2 * Math.sqrt(Math.max(0, r * r - b * b)));
      const dx = Math.cos(turn) * d + Math.cos(a) * b, dz = Math.sin(turn) * d + Math.sin(a) * b;
      if (Math.hypot(dx, dz) >= Math.PI * R) continue; // past the far side: gone by the ball's own rule
      if (clear(R, eye, bend(dx, up, dz, R, eye))) { seen++; assert.equal(gone, false, `R ${R} eye ${eye} d ${d} y ${y} r ${r}`); }
    }
    if (!gone) kept++;
  }
  assert.ok(seen > 1000 && dropped > 1000 && kept > 1000, 'the trial saw all three kinds');
  assert.equal(hidden(140, -20, 300, 0, 5), false, 'inside the ball nothing is over a horizon');
  assert.equal(hidden(140, -20, 500, 0, 5), true, 'but what is past the far side is still gone');
  assert.ok(sight(900, 1.7) > 55 && sight(900, 1.7) < 56, 'the horizon from head height on a ball of 900 m is 55 m off');
});

test('depth runs one way from near to far and stays in the card\'s range, in one part or two', () => {
  for (const clear of [0, 5, 12, 13, 80, 400, 4000, 50000, Infinity]) {
    const range = depthRange(clear);
    let last = -1;
    for (let z = NEAR; z < FAR; z *= 1.07) {
      const d = depthAt(range, z);
      assert.ok(d >= -1e-12 && d <= 1, `inside 0..1 at ${z} for ${clear}`);
      assert.ok(d >= last, `never back toward the eye at ${z} for ${clear}`);
      last = d;
    }
    assert.ok(depthAt(range, FAR) <= 1 + 1e-12);
  }
  assert.equal(depthRange(11).split, Infinity, 'with something within twelve metres depth is all one part');
  const high = depthRange(1200);
  assert.equal(high.open, 300);
  assert.ok(depthAt(high, SPLIT) <= SHARE + 1e-12 && depthAt(high, 300) >= SHARE - 1e-12, 'the eye\'s own few metres keep a thin share in front');
  // 24 bits over the half of the range the card uses: how far apart two things must be to be told apart.
  const step = 2 ** -23, told = (range, z) => { let lo = 0, hi = z; for (let n = 0; n < 60; n++) { const mid = (lo + hi) / 2; if (depthAt(range, z + mid) - depthAt(range, z) > step) hi = mid; else lo = mid; } return hi; };
  assert.ok(told(depthRange(0), 1000) > 1, 'near plane at 5 cm: over a metre is lost a kilometre off');
  assert.ok(told(high, 1000) < 0.001, 'cut in two from a height: under a millimetre');
});

test('a thin layer on the land is written nearer by its height, the land under it farther, and nothing from 4 cm up', () => {
  assert.equal(layerBias(0), 0);
  for (let h = -0.03; h < 0.02; h += 0.001) assert.ok(layerBias(h + 0.001) > layerBias(h), 'higher is nearer');
  assert.ok(layerBias(-0.02) < 0 && layerBias(0.006) > 0);
  assert.equal(layerBias(0.05), 0);
  assert.equal(layerBias(2), 0);
  assert.ok(Math.abs(layerBias(0.02) - layerBias(0.0200001)) < 1e-4 && layerBias(0.04) === 0, 'and it eases away with no step');
  // A path 6 mm over the grass, a kilometre off from a head on the ground: told apart, where bare depth cannot.
  const range = depthRange(0), z = 1000;
  assert.ok(depthAt(range, z) - depthAt(range, z * (1 - layerBias(0.006))) > 2 ** -23);
});

test('the widest angle in the picture: a flat picture\'s own, wider under the lens', () => {
  assert.ok(Math.abs(widest(1, 0) - Math.PI / 4) < 1e-6);
  assert.ok(widest(1.7, 0.65) > widest(1.7, 0));
  assert.ok(widest(1.7, 0.65) < Math.PI / 2);
  assert.equal(widest(2.5, 1), Math.PI / 2, 'all the lens takes in everything ahead at this size');
  for (let r = 0.2; r < 3; r += 0.2) assert.ok(widest(r + 0.2, 0.65) >= widest(r, 0.65));
});

test('the far land\'s tiles cover everything outside the hole once, nothing in it, and leave no edge to crack', () => {
  for (const cut of [1, 3]) {
    const tiles = landTiles(47, 9000, cut);
    const on = (x, z) => tiles.filter((t) => x > t.x0 && x < t.x1 && z > t.z0 && z < t.z1).length;
    for (let n = 0; n < 3000; n++) {
      const x = (rnd() - 0.5) * 16000, z = (rnd() - 0.5) * 16000;
      assert.equal(on(x, z), Math.max(Math.abs(x), Math.abs(z)) > 23.5 ? 1 : 0, `${x} ${z}`);
    }
    for (let n = 0; n < 300; n++) assert.equal(on((rnd() - 0.5) * 46.9, (rnd() - 0.5) * 46.9), 0);
    // Every stretch of edge between two points is another tile's stretch too, run the other way
    // (but round the hole and the outer rim): so the two are always halved at the same places.
    const key = (a, b) => `${a[0].toFixed(3)} ${a[1].toFixed(3)} ${b[0].toFixed(3)} ${b[1].toFixed(3)}`, edges = new Set();
    for (const t of tiles) t.points.forEach((p, k) => edges.add(key(p, t.points[(k + 1) % t.points.length])));
    const rim = Math.max(...tiles.map((t) => t.x1));
    let lone = 0;
    for (const t of tiles) t.points.forEach((p, k) => {
      const q = t.points[(k + 1) % t.points.length], out = (v) => Math.abs(Math.abs(v) - rim) < 1e-6, hole = (a) => Math.max(Math.abs(a[0]), Math.abs(a[1])) < 23.5 + 1e-6;
      if ((out(p[0]) && out(q[0])) || (out(p[1]) && out(q[1])) || (hole(p) && hole(q))) return;
      if (!edges.has(key(q, p))) lone++;
    });
    assert.equal(lone, 0, `cut ${cut}`);
    if (cut === 3) assert.ok(tiles.some((t) => t.odd) && tiles.some((t) => !t.odd));
  }
});

test('a model\'s own ball holds all its corners', () => {
  const v = new Float32Array(18 * 20);
  for (let i = 0; i < v.length; i++) v[i] = (rnd() - 0.5) * 40;
  const b = ballOf(v);
  for (let i = 0; i < v.length; i += 6) assert.ok(Math.hypot(v[i] - b.x, v[i + 1] - b.y, v[i + 2] - b.z) <= b.r + 1e-4);
  assert.equal(ballOf(v), b, 'and is worked out once');
});

// A dozen triangles standing at a place: enough for the projector to ask whether any of it shows.
const thing = (size = 1) => {
  const t = [];
  for (let n = 0; n < 12; n++) t.push(-size, n * 0.1, 0, 1, 0, 0, size, n * 0.1, 0, 1, 0, 0, 0, n * 0.1 + size, 0, 1, 0, 0);
  return new Float32Array(t);
};
function eyeAt(eye, aim, R = 140) {
  const scene = new OskiewarScene3D({ maxTriangles: 4096 }), p = new Projector(scene);
  p.camera(eye, aim, 16 / 9);
  p.lens = 0.65; p.planet = R; p.fine = 0.16;
  p.frame();
  return { scene, p };
}

test('what stands over the horizon or behind the eye is not drawn; what shows over the horizon is', () => {
  const { scene, p } = eyeAt({ x: 0, y: 1.7, z: 0 }, { x: 0, y: 1.7, z: 10 });
  p.draw(thing(), 0, 0, 10);
  const near = scene.triangleCount;
  assert.ok(near >= 12, 'ten metres ahead: drawn');
  p.draw(thing(), 0, 0, 60);
  assert.equal(scene.triangleCount, near, 'sixty metres off on a ball of 140 m, two metres tall: under the horizon');
  p.draw(thing(), 0, 60, 60);
  assert.ok(scene.triangleCount > near, 'the same place sixty metres up: over it');
  const before = scene.triangleCount;
  p.draw(thing(), 0, 0, -10);
  assert.equal(scene.triangleCount, before, 'behind the eye: not drawn');
  assert.equal(p.unseen(thing(), 0, 0, 10, 0, 0, 1, 0), false);
  assert.equal(p.unseen(thing(), 0, 0, 60, 0, 0, 1, 0), true);
});

test('a great square of land that reaches back to the eye is kept though its corners are all over the horizon', () => {
  const { scene, p } = eyeAt({ x: 0, y: 1.7, z: 0 }, { x: 0, y: 0, z: 6 });
  const square = new Float32Array([-500, 0, -500, 0, 1, 0, -500, 0, 500, 0, 1, 0, 500, 0, 500, 0, 1, 0]);
  p.halvings = 13;
  p.draw(square);
  assert.ok(scene.triangleCount > 0);
  assert.ok([...scene.frameVertices()].every(Number.isFinite));
});

test('from inside the ball the land overhead and all round is drawn, behind the eye in the flat world or not', () => {
  const { scene, p } = eyeAt({ x: 0, y: -60, z: 0 }, { x: 0, y: -160, z: 40 }); // looking down and ahead
  // Land 300 m ahead in the flat world: far round the ball, below the eye.
  const patch = new Float32Array([-20, 0, 280, 0, 1, 0, -20, 0, 320, 0, 1, 0, 20, 0, 320, 0, 1, 0]);
  p.draw(patch);
  assert.ok(scene.triangleCount > 0, 'the far side of the dome, under the eye, shows when it looks down');
  assert.ok([...scene.frameVertices()].every(Number.isFinite));
  for (let i = 2; i < scene.frameVertices().length; i += 6) assert.ok(scene.vertices[i] >= 0 && scene.vertices[i] <= 1);
  assert.equal(p.unseen(thing(), 0, 0.5, 300, 0, 0, 1, 0), true, 'what stands wholly over the land is behind it from in here');
  assert.equal(p.unseen(thing(), 0, 0, 300, 0, 0, 1, 0), false, 'what lies on the land may show through from under');
});

test('depth is shared out by how near the nearest thing of any size was last frame', () => {
  const scene = new OskiewarScene3D({ maxTriangles: 4096 }), p = new Projector(scene);
  const frame = (eye) => { scene.beginFrame(); p.camera(eye, { x: 0, y: 0, z: 0.01 }, 16 / 9); p.lens = 0.65; p.planet = 900; p.fine = 0.16; p.frame(); p.draw(new Float32Array([-200, 0, -200, 0, 1, 0, -200, 0, 200, 0, 1, 0, 200, 0, 200, 0, 1, 0])); };
  frame({ x: 0, y: 400, z: 0 });
  assert.equal(p.range.split, Infinity, 'the first frame knows of nothing: one part');
  frame({ x: 0, y: 400, z: 0 });
  assert.equal(p.range.split, SPLIT, 'four hundred metres over the land: two');
  assert.ok(p.range.open > 50 && p.range.open <= 100);
  frame({ x: 0, y: 6, z: 0 });
  frame({ x: 0, y: 6, z: 0 });
  assert.equal(p.range.split, Infinity, 'down at six metres: one again');
});

test('a drone flies straight through the land into the planet, is held by a floor of the tower, and stopped over the far side\'s inside', () => {
  const down = { fwd: 0, right: 0, up: -1 };
  let p = { x: 0, y: 3, z: 0, yaw: 0, pitch: 0 }, rested = 0;
  for (let n = 0; n < 240 && p.y >= 0; n++) { const y = p.y; p = moveFreecam(p, down, 1 / 60); if (p.y === y) rested++; }
  assert.ok(p.y < 0, 'it goes through');
  assert.equal(rested, 0, 'without resting on the way');
  let dive = { x: 0, y: 2, z: 0, yaw: 0, pitch: -40 };
  for (let n = 0; n < 240; n++) dive = moveFreecam(dive, { fwd: 1, right: 0, up: 0 }, 1 / 60);
  assert.ok(dive.y < -1, 'flown at, the land lets it in too');
  let floor = { x: 0, y: 3, z: 0, yaw: 0, pitch: 0 };
  for (let n = 0; n < 600; n++) floor = moveFreecam(floor, down, 1 / 60, null, false);
  assert.equal(floor.y, FLOOR, 'a floor of the tower is only a floor');
  let deep = { x: 0, y: DEEP + 1, z: 0, yaw: 0, pitch: 0 };
  for (let n = 0; n < 600; n++) deep = moveFreecam(deep, { ...down, boost: true }, 1 / 60);
  assert.equal(deep.y, DEEP);
  assert.equal(DEEP, -(2 * PLANET - 4));
  let up = { x: 0, y: -2, z: 0, yaw: 0, pitch: 0 };
  for (let n = 0; n < 240; n++) up = moveFreecam(up, { fwd: 0, right: 0, up: 1 }, 1 / 60);
  assert.ok(up.y > 1, 'coming back up it meets nothing');
});
