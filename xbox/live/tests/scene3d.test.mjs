import assert from "node:assert/strict";
import test from "node:test";
import {
  clipDepth,
  clipPoint,
  OskiewarScene3D,
  OSKIEWAR_TRIANGLE_FLOATS,
} from "../scene3d.mjs";

test("scene depth matches the shipped Xbox D3D mapping", () => {
  assert.equal(clipDepth(-1.5), 0);
  assert.equal(clipDepth(0), .5);
  assert.equal(clipDepth(1.5), 1);
  assert.equal(clipDepth(99), 1);
});
test("logical coordinates become GPU clip coordinates", () => {
  assert.deepEqual(clipPoint(0, 0, 0, 1920, 1080), [-1, 1, .5]);
  assert.deepEqual(clipPoint(960, 540, 0, 1920, 1080), [0, 0, .5]);
  assert.deepEqual(clipPoint(1920, 1080, 0, 1920, 1080), [1, -1, .5]);
});

test("scene batches depth-bearing colored triangles without allocation churn", () => {
  const scene = new OskiewarScene3D({ maxTriangles: 2 });
  scene.beginFrame();
  assert.equal(scene.triangle(0, 0, -1, 10, 0, 0, 0, 10, 1,
    255, 128, 0, 20, 10), true);
  assert.equal(scene.triangleCount, 1);
  assert.equal(scene.frameVertices().length, OSKIEWAR_TRIANGLE_FLOATS);
  assert.equal(scene.frameVertices()[3], 1);
  assert.ok(Math.abs(scene.frameVertices()[4] - 128 / 255) < 1e-6);
  assert.equal(scene.frameVertices()[5], 0);
  scene.beginFrame();
  assert.equal(scene.triangleCount, 0);
  assert.equal(scene.frameVertices().length, 0);
});
