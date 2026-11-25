// 🎨 Corners keep their own colours, and a skinned triangle its place on a
// texture, through the projector's clipping and halving (project.mjs).
import assert from 'node:assert/strict';
import test from 'node:test';
import { Projector } from '../project.mjs';
import { setSkin, SKIN_FLOATS } from '../skin.mjs';
import { OskiewarScene3D } from '../../xbox/live/scene3d.mjs';

const corners = (points, colors) => new Float32Array(points.flatMap((p, i) => [...p, ...colors[i]]));
function setup(maxTriangles = 4096) {
  const scene = new OskiewarScene3D({ maxTriangles });
  scene.uvs = new Float32Array(maxTriangles * 12);
  const p = new Projector(scene);
  p.camera({ x: 0, y: 0, z: 0 }, { x: 0, y: 0, z: 1 }, 1);
  return { scene, p };
}
const RGB = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];

test('each corner keeps its own colour (no longer the first corner\'s for all three)', () => {
  const { scene, p } = setup();
  p.draw(corners([[-1, 0, 2], [0, 1, 2], [0, 0, 2]], RGB));
  assert.equal(scene.triangleCount, 1);
  const v = scene.vertices;
  assert.deepEqual([...v.subarray(3, 6)], [1, 0, 0]);
  assert.deepEqual([...v.subarray(9, 12)], [0, 1, 0]);
  assert.deepEqual([...v.subarray(15, 18)], [0, 0, 1]);
});

test('a corner made at the near plane is the blend of the two it lies between', () => {
  const { scene, p } = setup();
  p.draw(corners([[-0.01, 0, -1], [0.01, 0.01, 1], [0.01, -0.01, 1]], RGB));
  assert.ok(scene.triangleCount >= 1);
  const v = scene.frameVertices();
  for (let i = 3; i < v.length; i += 6) {
    const sum = v[i] + v[i + 1] + v[i + 2];
    assert.ok(Math.abs(sum - 1) < 1e-5, `a blend of the corners: ${[...v.subarray(i, i + 3)]}`);
  }
});

test('halved shapes blend their colours along the cut', () => {
  const { scene, p } = setup();
  p.halvings = 4; p.lens = 0.5; p.fine = 0.05; // (halving on, edges short)
  p.draw(corners([[-3, -2, 2], [3, -2, 2], [0, 3, 2]], RGB));
  assert.ok(scene.triangleCount > 1, `halved into ${scene.triangleCount}`);
  const v = scene.frameVertices();
  let blended = 0;
  for (let i = 3; i < v.length; i += 6) {
    assert.ok(Math.abs(v[i] + v[i + 1] + v[i + 2] - 1) < 1e-5);
    if (v[i] > 0.01 && v[i + 1] > 0.01) blended++;
  }
  assert.ok(blended > 0);
});

test('a skinned triangle is flagged and carries u/z, v/z, 1/z and its layer', () => {
  const { scene, p } = setup();
  p.textures = (key) => (key === 7 ? 3 : -1);
  const face = corners([[-1, 0, 2], [0, 1, 4], [0, 0, 2]], [[1, 1, 1], [1, 1, 1], [1, 1, 1]]);
  const skin = new Float32Array(SKIN_FLOATS);
  skin.set([7, 0, 0, 1, 0, 1, 1]);
  setSkin(face, skin);
  p.draw(face);
  assert.equal(scene.triangleCount, 1);
  const v = scene.vertices, uv = scene.uvs;
  for (let c = 0; c < 3; c++) assert.ok(v[c * 6 + 5] > 8.5, 'blue lifted by 8: textured');
  for (let c = 0; c < 3; c++) assert.equal(uv[c * 4 + 3], 3, 'the layer');
  const back = (c) => [uv[c * 4] / uv[c * 4 + 2], uv[c * 4 + 1] / uv[c * 4 + 2]];
  for (const [c, want] of [[0, [0, 0]], [1, [1, 0]], [2, [1, 1]]]) {
    const got = back(c);
    assert.ok(Math.abs(got[0] - want[0]) < 1e-5 && Math.abs(got[1] - want[1]) < 1e-5, `corner ${c}: ${got}`);
  }
  assert.ok(uv[2] > uv[6], 'the far corner has the smaller 1/z');
});

test('a texture that is not ready draws its tint, unflagged', () => {
  const { scene, p } = setup();
  p.textures = () => -1;
  const face = corners([[-1, 0, 2], [0, 1, 2], [0, 0, 2]], RGB);
  setSkin(face, new Float32Array([7, 0, 0, 1, 0, 1, 1]));
  p.draw(face);
  assert.ok(scene.vertices[5] < 1.5);
});
