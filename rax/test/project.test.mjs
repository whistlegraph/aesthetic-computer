import assert from 'node:assert/strict';
import test from 'node:test';
import { Projector } from '../project.mjs';
import { OskiewarScene3D } from '../../xbox/live/scene3d.mjs';

const triangle = (a, b, c) => new Float32Array([a, b, c].flatMap((p) => [...p, 1, 0, 0]));
function setup(maxTriangles = 16) {
  const scene = new OskiewarScene3D({ maxTriangles }),
    p = new Projector(scene);
  p.camera({ x: 0, y: 0, z: 0 }, { x: 0, y: 0, z: 1 }, 1);
  return { scene, p };
}
test('projection keeps rightward movement on the camera right and preserves depth', () => {
  const { scene, p } = setup();
  p.draw(triangle([-1, 0, 2], [0, 1, 2], [0, 0, 2]));
  assert.equal(scene.triangleCount, 1);
  assert.equal(scene.vertices[0], 0.5);
  const near = scene.vertices[2];
  p.draw(triangle([-1, 0, 4], [0, 1, 4], [0, 0, 4]));
  assert.ok(scene.vertices[20] > near);
  assert.ok(near > 0 && near < 1);
});
test('near-plane crossings clip to finite triangles instead of exploding across the screen', () => {
  const { scene, p } = setup();
  p.draw(triangle([-0.01, 0, -1], [0.01, 0.01, 1], [0.01, -0.01, 1]));
  assert.equal(scene.triangleCount, 2);
  assert.ok([...scene.frameVertices()].every(Number.isFinite));
  for (let i = 2; i < scene.frameVertices().length; i += 6)
    assert.ok(scene.vertices[i] >= -1e-7 && scene.vertices[i] <= 1);
});
test('geometry behind the camera or wholly outside the viewport does not consume the stream', () => {
  const { scene, p } = setup();
  p.draw(triangle([-1, 0, -2], [0, 1, -2], [0, 0, -2]));
  p.draw(triangle([10, 0, 2], [12, 1, 2], [11, 0, 2]));
  assert.equal(scene.triangleCount, 0);
});
test('model placement and camera position agree, and overflow is explicit', () => {
  const { scene, p } = setup(1),
    face = triangle([-0.1, 0, 0], [0.1, 0.1, 0], [0.1, 0, 0]);
  p.draw(face, 0, 0, 2);
  assert.equal(scene.triangleCount, 1);
  // Overflow drops geometry and flags it; it never throws (a throw froze the frame loop).
  p.draw(face, 0, 0, 2);
  assert.equal(p.overflow, true);
  scene.beginFrame();
  p.camera({ x: 5, y: 0, z: 0 }, { x: 5, y: 0, z: 1 }, 1);
  p.draw(face, 5, 0, 2);
  assert.ok(Math.abs(scene.vertices[0] - 0.05) < 1e-7);
});

test('a face between the eye and the body it follows is looked through; faces beside and beyond stay', () => {
  const { scene, p } = setup();
  const wall = (z, x = 0) => triangle([x - 1, -1, z], [x + 1, -1, z], [x, 2, z]);
  p.look([{ x: 0, y: 0, z: 4 }]); // the body stands four metres ahead
  p.draw(wall(2));
  assert.equal(scene.triangleCount, 0, 'a wall across the sight line is not drawn');
  p.draw(wall(2, 3));
  assert.equal(scene.triangleCount, 1, 'a wall off to the side is');
  p.draw(wall(6));
  assert.equal(scene.triangleCount, 2, 'and so is one behind the body');
  p.draw(wall(3.8));
  assert.equal(scene.triangleCount, 3, 'the last 0.45 m is the body itself: never cut');
  p.look();
  p.draw(wall(2));
  assert.equal(scene.triangleCount, 4, 'with nothing to look for, everything is drawn');
  p.look([{ x: 0, y: 0, z: 0.2 }]);
  assert.equal(p.cut, null, 'a body closer than the margin asks for nothing');
});
