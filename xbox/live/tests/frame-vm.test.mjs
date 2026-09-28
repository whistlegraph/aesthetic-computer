import assert from "node:assert/strict";
import test from "node:test";
import { createFrameVm, FRAME_VIEW, FRAME_FACE, FRAME_DISC, FRAME_CAPSULE,
  FRAME_TEXT, FRAME_BOX, FRAME_LINE, FRAME_WIPE, FRAME_CAMERA, FRAME_ASSET,
  FRAME_MODEL } from "../frame-vm.mjs";

// A recording host: every call the interpreter makes, by name.
function host() {
  const calls = [];
  const record = (name) => (...args) => calls.push([name, ...args]);
  return { calls, triangle3d: record("face"), triangle: record("flat"),
    box: record("box"), line: record("line"), wipe: record("wipe"),
    write: record("write"), systemWrite: record("system"),
    comicWrite: record("comic") };
}
const program = (...ops) => {
  const values = ops.flat();
  return [new Float32Array(values), values.length];
};

test("every op reaches the host it names, in program order", () => {
  const h = host();
  const vm = createFrameVm(h);
  const strings = ["hello"];
  vm.run(...program(
    [FRAME_WIPE, 1, 2, 3],
    [FRAME_BOX, 10, 20, 30, 40, 5, 6, 7, 255],
    [FRAME_BOX, 10, 20, 30, 40, 5, 6, 7, 128],
    [FRAME_LINE, 0, 0, 10, 10, 2, 9, 9, 9],
    [FRAME_FACE, 0, 0, -1, 10, 0, -1, 0, 10, -1, 1, 2, 3],
    [FRAME_TEXT, 0, 5, 6, 20, 1, 2, 3, 0],
    [FRAME_TEXT, 3, 5, 6, 20, 1, 2, 3, 0]), strings);
  assert.deepEqual(h.calls.map((c) => c[0]),
    ["wipe", "box", "box", "line", "face", "comic", "write"]);
  assert.deepEqual(h.calls[1], ["box", 10, 20, 30, 40, 5, 6, 7]);
  assert.deepEqual(h.calls[2], ["box", 10, 20, 30, 40, 5, 6, 7, 128]);
  assert.deepEqual(h.calls[5], ["comic", "hello", 5, 6, 20, 1, 2, 3]);
});

test("a disc is fanned here, a capsule too, and a face keeps its own depths", () => {
  const h = host();
  const vm = createFrameVm(h);
  vm.run(...program([FRAME_DISC, 100, 100, -1.2, 10, 1, 2, 3]), []);
  const discFaces = h.calls.length;
  assert.ok(discFaces >= 6, `a disc of radius 10 fans into faces, got ${discFaces}`);
  const depth = Math.fround(-1.2);
  assert.ok(h.calls.every((c) => c[3] === depth && c[6] === depth && c[9] === depth),
    "every fan face sits at the disc's depth");
  h.calls.length = 0;
  vm.run(...program([FRAME_CAPSULE, 0, 0, 50, 0, -1.1, 8, 1, 2, 3]), []);
  assert.ok(h.calls.length > 2, "a capsule is a body and two end fans");
  h.calls.length = 0;
  vm.run(...program([FRAME_FACE, 0, 0, -1, 10, 0, -.5, 0, 10, 0, 1, 2, 3]), []);
  assert.deepEqual(h.calls[0], ["face", 0, 0, -1, 10, 0, -.5, 0, 10, 0, 1, 2, 3]);
});

test("inside a view, faces are cut to its rectangle and the stage clears it", () => {
  const h = host();
  const vm = createFrameVm(h);
  vm.run(...program(
    [FRAME_VIEW, 100, 100, 50, 50],
    [FRAME_FACE, 0, 0, -1, 10, 0, -1, 0, 10, -1, 1, 2, 3],       // wholly outside
    [FRAME_FACE, 110, 110, -1, 120, 110, -1, 110, 120, -1, 1, 2, 3], // wholly inside
    [FRAME_FACE, 90, 110, -1, 130, 110, -1, 110, 140, -1, 1, 2, 3],  // crossing
    [FRAME_VIEW, NaN, 0, 0, 0],
    [FRAME_FACE, 0, 0, -1, 10, 0, -1, 0, 10, -1, 1, 2, 3]), []);
  assert.equal(h.calls[0][1], 110, "the outside face was dropped, the inside one drawn whole");
  for (const call of h.calls) {
    if (call === h.calls[h.calls.length - 1]) break;
    for (let i = 1; i <= 9; i += 3) {
      assert.ok(call[i] >= 100 && call[i] <= 150, `x ${call[i]} inside the view`);
      assert.ok(call[i + 1] >= 100 && call[i + 1] <= 150, `y ${call[i + 1]} inside the view`);
    }
  }
  assert.deepEqual(h.calls[h.calls.length - 1].slice(0, 3), ["face", 0, 0],
    "after the view closes the stage is whole again");
});

test("without a depth host the face falls to the flat triangle", () => {
  const h = host();
  delete h.triangle3d;
  const vm = createFrameVm(h);
  vm.run(...program([FRAME_FACE, 0, 0, -1, 10, 0, -1, 0, 10, -1, 1, 2, 3]), []);
  assert.deepEqual(h.calls[0], ["flat", 0, 0, 10, 0, 0, 10, 1, 2, 3]);
});

test("decode gives back what was written, and an unknown op is an error", () => {
  const vm = createFrameVm(host());
  const ops = vm.decode(...program([FRAME_WIPE, 1, 2, 3], [FRAME_TEXT, 2, 1, 2, 3, 4, 5, 6, 0]), ["x"]);
  assert.deepEqual(ops, [{ op: FRAME_WIPE, args: [1, 2, 3] },
    { op: FRAME_TEXT, args: [2, 1, 2, 3, 4, 5, 6, "x"] }]);
  assert.throws(() => vm.run(...program([99, 1, 2]), []), /unknown op 99/);
});

// MODEL: a baked mesh placed by a matrix, at the level its size calls for.
// A flat camera makes screen x, y the world's, so the numbers read directly.
test("a MODEL places a mesh, mirrors it face-out, lights by its normal, and picks a level", () => {
  const camera = [FRAME_CAMERA, 0, 0, -100, 1, 0, 0, 0, -1, 0, 0, 0, 1,
    0, 0, 1, 1000, 0, 2.8 / 16000, -1.4, 8, -1e4, 1e4, -1e4, 1e4];
  // Two triangles (the fourth id repeats the third): one lit and facing +z,
  // one glowing (a zero normal).
  const mesh = [FRAME_ASSET, 3, 3, 2, 0, 0, 0, 10, 0, 0, 0, 10, 0,
    0, 1, 2, 2, 200, 100, 50, 0, 0, 1,
    0, 1, 2, 2, 255, 255, 255, 0, 0, 0];
  // Mirrored across x and moved to (100, 50). The light shines along +z, so
  // the mirrored face, still facing +z, is turned away from it: .72.
  const model = (radius) => [FRAME_MODEL, radius, 1, 2, 3, 100, 50, 0,
    -1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1];
  const h = host();
  createFrameVm(h).run(...program(camera, mesh, model(10)), []);
  const faces = h.calls.filter((c) => c[0] === "face")
    .map((c) => [c[1], c[2], c[4], c[5], c[7], c[8], c[10], c[11], c[12]]);
  assert.deepEqual(faces, [
    [100, 50, 100, 60, 90, 50, 144, 72, 36],
    [100, 50, 100, 60, 90, 50, 255, 255, 255]]);
  // 10 px of radius is the far level, handle 3. At 30 px it is the middle,
  // handle 2, which this host was never given: nothing is drawn.
  const middle = host();
  createFrameVm(middle).run(...program(camera, mesh, model(30)), []);
  assert.equal(middle.calls.length, 0);
});
