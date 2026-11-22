import test from 'node:test';
import assert from 'node:assert/strict';
import { shadowVolume, VOLUME_TRIANGLES, trianglesIn, reach, shadeFor } from '../shadow.mjs';
import { skyAt } from '../sky.mjs';

const unit = (x, y, z) => { const n = Math.hypot(x, y, z); return { x: x / n, y: y / n, z: z / n }; };
function volume(box, light, { yaw = 0, scale = 1, at = [0, 0, 0], near = 0.02, far = 30 } = {}) {
  const out = new Float32Array(VOLUME_TRIANGLES * 18);
  const end = shadowVolume(out, 0, box, ...at, yaw, scale, light, near, far);
  const tris = [];
  for (let i = 0; i < end; i += 18) tris.push([0, 6, 12].map((k) => [out[i + k], out[i + k + 1], out[i + k + 2]]));
  return tris;
}
// Six times the solid's signed volume: positive when every face looks outward.
const signed = (tris) => tris.reduce((sum, [a, b, c]) =>
  sum + a[0] * (b[1] * c[2] - b[2] * c[1]) - a[1] * (b[0] * c[2] - b[2] * c[0]) + a[2] * (b[0] * c[1] - b[1] * c[0]), 0) / 6;
// A closed skin: every edge one triangle walks, another walks back.
function open(tris) {
  const key = (p) => p.map((n) => n.toFixed(3)).join(','), edges = new Map();
  for (const t of tris) for (let k = 0; k < 3; k++) {
    const a = key(t[k]), b = key(t[(k + 1) % 3]);
    edges.set(`${a}>${b}`, (edges.get(`${a}>${b}`) || 0) + 1);
  }
  let unmatched = 0;
  for (const [edge, count] of edges) {
    const [a, b] = edge.split('>');
    if ((edges.get(`${b}>${a}`) || 0) !== count) unmatched++;
  }
  return unmatched;
}

test('a box\'s shadow is one closed solid, faces outward, whatever the sun and however it is turned', () => {
  const box = { x: 0.4, y: 1.5, z: -0.3, w: 1.2, h: 3, d: 0.8 };
  const lights = [unit(0, -1, 0), unit(1, -1, 0), unit(-0.3, -0.6, 0.7), unit(0.9, -0.1, -0.4), unit(-1, -0.4, -1)];
  for (const light of lights)
    for (const yaw of [0, 0.7, Math.PI / 2, 2.9, -1.2])
      for (const scale of [1, 0.5]) {
        const tris = volume(box, light, { yaw, scale, at: [3, 0.5, -2], far: 20 });
        assert.equal(open(tris), 0, 'closed');
        assert.ok(tris.length <= VOLUME_TRIANGLES);
        // Swept from 0.02 to 20 m along the light: the box's shadow area times that run.
        assert.ok(signed(tris) > 0, 'faces outward');
      }
});

test('a box written with a negative size casts the same shadow, not one inside out', () => {
  const light = unit(0.4, -0.8, 0.3);
  const plain = volume({ x: 1, y: 1, z: 2, w: 2, h: 2, d: 3 }, light);
  for (const box of [{ x: 1, y: 1, z: 2, w: -2, h: 2, d: 3 }, { x: 1, y: 1, z: 2, w: 2, h: 2, d: -3 }, { x: 1, y: 1, z: 2, w: -2, h: -2, d: -3 }]) {
    const tris = volume(box, light);
    assert.equal(open(tris), 0);
    assert.ok(Math.abs(signed(tris) - signed(plain)) < 1e-3);
  }
});

test('the volume starts just behind the lit skin and runs the asked length along the light', () => {
  const light = unit(0, -1, 0), tris = volume({ x: 0, y: 1, z: 0, w: 2, h: 2, d: 2 }, light, { near: 0.02, far: 10 });
  const ys = tris.flat().map((p) => p[1]);
  assert.ok(Math.abs(Math.max(...ys) - (2 - 0.02)) < 1e-6, 'its top sits 2 cm under the box\'s lit top');
  assert.ok(Math.abs(Math.min(...ys) - (0 - 10)) < 1e-6);
  assert.ok(Math.abs(signed(tris) - 2 * 2 * (2 + 10 - 0.02)) < 1e-3, 'a straight prism under a sun overhead');
  assert.equal(trianglesIn(tris.length * 18), tris.length);
});

test('shadows reach the ground and no further than they must; a low sun casts none', () => {
  const noon = skyAt(0.5).sun, light = { x: -noon.x, y: -noon.y, z: -noon.z };
  assert.ok(reach(3, light) < 7 && reach(3, light) > 5);
  assert.equal(reach(3, { x: 1, y: -0.001, z: 0 }), 70, 'held, however low the sun');
  assert.equal(shadeFor(0.02), null);
  assert.equal(shadeFor(-0.5), null);
  assert.deepEqual(shadeFor(0.9), [0.8, 0.82, 0.88]);
  assert.ok(shadeFor(0.15)[0] > 0.8 && shadeFor(0.15)[0] < 1, 'fading in after sunrise');
});
