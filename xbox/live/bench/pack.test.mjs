import test from 'node:test';
import assert from 'node:assert/strict';
import { pack } from './pack.mjs';
import { OskiewarScene3D } from '../scene3d.mjs';

test('stream pack preserves the existing GPU vertex contract', () => {
  const triangles = [
    -120, 20, -3, 960, 540, 0, 2040, 1100, 3, 255, 80, 120,
    4, 9, -1.4, 8, 10, -.22, 12, 13, 1.4, 0, 128, 255,
  ];
  const expected = new OskiewarScene3D({ maxTriangles: 2 });
  for (let i = 0; i < triangles.length; i += 12)
    expected.triangle(...triangles.slice(i, i + 12));
  const actual = new Float32Array(36);
  pack({ triangles }, actual);
  assert.deepEqual(actual, expected.frameVertices());
  const prior = actual.slice();
  pack({ triangles: triangles.slice(12) }, actual);
  assert.deepEqual(actual.slice(0, 18), prior.slice(18));
});

test('packing refuses partial triangles and undersized buffers', () => {
  assert.throws(() => pack({ triangles: [1] }, new Float32Array(18)), RangeError);
  assert.throws(() => pack({ triangles: Array(12).fill(0) }, new Float32Array(17)), RangeError);
});
