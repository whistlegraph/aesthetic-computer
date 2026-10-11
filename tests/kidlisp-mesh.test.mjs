import test from 'node:test';
import assert from 'node:assert/strict';
import { buildMesh, sceneCamera, placeMesh, projectMesh } from '../system/public/aesthetic.computer/lib/kidlisp-mesh.mjs';
import { KidLisp } from '../system/public/aesthetic.computer/lib/kidlisp.mjs';
import { compileProgram } from '../system/public/aesthetic.computer/lib/kidlisp-compile.mjs';
import { emitProgram, programHelpers } from '../system/public/aesthetic.computer/lib/kidlisp-emit.mjs';
import { GpuFrame, readFrame, OP } from '../system/public/aesthetic.computer/lib/gpu-frame.mjs';

test('a cube is eight vertices and six outward quads', () => {
  const m = buildMesh([['cube', 2, 4, 6, 200, 100, 50]]);
  assert.equal(m.verts.length, 24);
  assert.equal(m.faces.length, 60);
  for (let f = 0; f < 60; f += 10) {
    const [nx, ny, nz] = [m.faces[f + 7], m.faces[f + 8], m.faces[f + 9]];
    assert.ok(Math.abs(Math.hypot(nx, ny, nz) - 1) < 1e-6, 'unit normal');
    // the normal points away from the centre
    const a = m.faces[f] * 3; assert.ok(m.verts[a] * nx + m.verts[a + 1] * ny + m.verts[a + 2] * nz > 0, 'outward');
    assert.deepEqual([m.faces[f + 4], m.faces[f + 5], m.faces[f + 6]], [200, 100, 50]);
  }
  assert.throws(() => buildMesh([['cube', 1, 'two', 3, 0, 0, 0]]), /numbers only/);
});

test('a cube in front of the camera projects inside the viewport, lit, far faces first', () => {
  const cam = sceneCamera(0, 0, 0, 0, 0, 60, 400, 300);
  const world = placeMesh(buildMesh([['cube', 10, 10, 10, 255, 255, 255]]), 0, 0, 50);
  const tris = [];
  const faces = projectMesh(cam, world, (...t) => tris.push(t));
  assert.equal(faces, 1, 'only the facing side of a cube seen head-on survives culling');
  for (const t of tris) { for (const i of [0, 2, 4]) assert.ok(t[i] > 150 && t[i] < 250, 'x inside'); for (const i of [1, 3, 5]) assert.ok(t[i] > 100 && t[i] < 200, 'y inside'); assert.ok(t[7] <= 255 && t[7] >= Math.floor(255 * 0.34), 'lit colour: at least the ambient share'); }
  // a second cube behind the first is emitted before it
  const far = placeMesh(buildMesh([['cube', 10, 10, 10, 255, 0, 0]]), 0, 0, 90);
  const order = [];
  projectMesh(cam, { verts: new Float32Array([...far.verts, ...world.verts]), faces: new Float32Array([...far.faces, ...Array.from(world.faces).map((v, i) => (i % 10 < 4 ? v + far.verts.length / 3 : v))]) }, (...t) => order.push(t[8]));
  assert.equal(order.length, 4, 'two facing sides, two triangles each');
  assert.deepEqual(order.slice(0, 2), [0, 0], 'the far red cube is emitted first');
  assert.ok(order[2] > 0 && order[3] > 0, 'then the near white one');
});

test('mesh, camera and place record into the frame the same way compiled, emitted and interpreted', () => {
  const source = `(mesh cube1 (cube 20 20 20 255 128 0))
(camera 0 0 0 0 0 60)
(place cube1 (sin frame) 0 100 0.5)
(place cube1 30 0 120)`;
  const streams = {};
  for (const mode of ['interpreted', 'compiled', 'emitted']) {
    const calls = [];
    const api = { screen: { width: 200, height: 100 }, params: [], colon: [], system: { fps: 60 }, paintCount: 3, needsPaint() {}, fps() {}, toggleHUD() {}, page() {}, unmask() {}, inkrn: () => [255, 255, 255, 255], clock: { time: () => new Date(0) }, sound: {},
      gpuFrame: { available: true, probe() {}, send() {} }, wipe() {}, ink() { return api; }, line() {}, box() {}, circle() {}, oval() {}, tri: (...a) => calls.push(a.map((v) => +v.toFixed(3))), shape() {}, write() {} };
    console.log = () => {};
    globalThis.KIDLISP_HOST_SOURCE = true;
    const lisp = new KidLisp(); const mod = lisp.module(source, true); lisp.setAPI(api); mod.boot(api);
    lisp.gpuFrame = new GpuFrame();
    if (mode === 'compiled') lisp.compiled = compileProgram(lisp.ast, lisp);
    if (mode === 'emitted') lisp.compiled = new Function('H', emitProgram(lisp.ast, lisp))(programHelpers(lisp));
    mod.paint(api);
    if (mode === 'interpreted') { streams[mode] = { cpuTriangles: calls.length }; continue; }
    const ops = []; readFrame(lisp.gpuFrame.take(), { camera: (...a) => ops.push(['camera', ...a]), place: (...a) => ops.push(['place', ...a]) });
    streams[mode] = ops;
    assert.equal(lisp.gpuFrame.meshes.size, 1, 'the mesh rode along once');
  }
  assert.deepEqual(streams.compiled, streams.emitted);
  assert.equal(streams.compiled.length, 3);
  assert.equal(streams.compiled[0][0], 'camera');
  assert.equal(streams.compiled[1][1], 1, 'mesh id');
  assert.ok(streams.interpreted.cpuTriangles > 0, 'the interpreter drew the cubes with tri');
});
