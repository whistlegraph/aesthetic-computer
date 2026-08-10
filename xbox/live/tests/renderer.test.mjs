// The rasterization front end, on its own bench.
//
// `oskiewar.test.mjs` drives whole frames; this file drives the camera and the
// clipper directly, because the artifacts they produce are geometric and a
// frame is too coarse a thing to see them in. Everything here loads the same
// source through the same sandbox, but only reaches for the projection and
// clipping exports — no round, no players, no HUD.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");

// One face per emitted triangle, with the vertices as the host receives them.
function createRenderer({ width = 1920, height = 1080 } = {}) {
  let now = 0;
  const faces = [];
  const lines = [];
  const noOp = () => {};
  // The host's own limit. Anything wider is a coordinate the console cannot
  // hold, so a face that asks for one is the artifact, not a symptom of it.
  const hostLimit = 32768;
  const triangle3d = (...values) => {
    for (const value of values.slice(0, 9))
      assert.ok(Number.isFinite(value) && Math.abs(value) <= hostLimit,
        `host rejected vertex ${value}`);
    faces.push({ vertices: [[values[0], values[1], values[2]],
      [values[3], values[4], values[5]], [values[6], values[7], values[8]]],
      color: values.slice(9, 12) });
  };
  const api = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal",
    "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line",
    "triangle", "triangle3d", "triangles3d", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, cameraDoll, projectPoint, worldQuad, worldTriangle,
       worldLine, worldCapsule, worldSegment, clipViewNear, clipScreenBand,
       cameraNear, guardBand, terrainSpan, stageGeometry: () => ({ floorY,
         ceilingY, worldLeft, worldRight, worldNear, worldFar, platformY,
         platformLeft, platformRight }),
       setTriangleDepth: (value) => { triangleDepth = value; },
       viewport: () => ({ width: viewWidth(), height: viewHeight }) };`
  )(
    () => ({ monotonicUs: now, unixMs: 1785870000000, simCount: 0,
      paintCount: 0, clientErrorReportStatus: "" }),
    () => ({ connected: false, down: [], leftX: 0, leftY: 0 }),
    () => ({ platform: "web", inputFamily: "keyboard" }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp,
    noOp, (...values) => lines.push(values), noOp, triangle3d, undefined,
    noOp, noOp, () => ({ width, height })
  );
  api.boot();
  // `boot` does not run a frame, so the viewport is still at its declared
  // default until something asks the camera to prepare against it.
  api.cameraDoll.dirty = true;
  return { ...api, faces, lines, hostLimit,
    take: () => faces.splice(0), advance: (us) => { now += us; } };
}

// Park the dolly somewhere exact. The game's own camera is inertial and eased,
// which is the wrong instrument for asking "what happens at this distance".
function place(renderer, { position, target, perspective = 1, fov = 55,
  width = 1200, roll = 0 }) {
  const doll = renderer.cameraDoll;
  doll.position = { ...position };
  doll.target = { ...target };
  doll.perspective = perspective;
  doll.fov = fov;
  doll.width = width;
  doll.roll = roll;
  doll.dirty = true;
}

test("camera space keeps the honest depth and only the projection pins it", () => {
  const renderer = createRenderer();
  place(renderer, { position: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 1 } });
  const behind = renderer.cameraDoll.toView({ x: 0, y: 0, z: -500 });
  assert.ok(behind.z < 0, "a point behind the lens reports a negative depth");
  const projected = renderer.cameraDoll.projectView(behind);
  assert.ok(Number.isFinite(projected.x) && Number.isFinite(projected.y));
  // The scene's depth band stops short of the overlay band at -1.4, which is
  // the contract the HUD layers above it depend on.
  assert.ok(projected.z > -1.4);
  assert.match(source, /const depth = Math\.max\(cameraNear, view\.z\)/);
});

test("a face wholly behind the camera is dropped, not pinned", () => {
  const renderer = createRenderer();
  place(renderer, { position: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 1 } });
  renderer.worldTriangle({ x: -400, y: -400, z: -900 },
    { x: 400, y: -400, z: -900 }, { x: 0, y: 400, z: -900 }, [200, 40, 40]);
  assert.equal(renderer.take().length, 0);
});

test("a face straddling the near plane is cut at it, not sheared across it", () => {
  const renderer = createRenderer();
  place(renderer, { position: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 1 } });
  // Two corners in front, one well behind: the classic straddle.
  const near = renderer.clipViewNear([
    { x: -300, y: 0, z: 900 }, { x: 300, y: 0, z: 900 },
    { x: 0, y: 200, z: -600 }]);
  assert.equal(near.length, 4, "a straddling triangle clips to a quad");
  assert.ok(near.every((vertex) => vertex.z >= renderer.cameraNear - 1e-9),
    "nothing survives the near clip in front of the plane");
  // The cut vertices sit exactly on the plane rather than at some pinned depth.
  const cuts = near.filter((vertex) => Math.abs(vertex.z - renderer.cameraNear) < 1e-9);
  assert.equal(cuts.length, 2);
});

test("a face entirely in front is passed through untouched", () => {
  const renderer = createRenderer();
  place(renderer, { position: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 1 } });
  const corners = [{ x: -300, y: 0, z: 900 }, { x: 300, y: 0, z: 900 },
    { x: 0, y: 200, z: 1200 }];
  assert.deepEqual(renderer.clipViewNear(corners), corners);
});

test("the guard band bounds a near-plane vertex instead of dropping its face", () => {
  const renderer = createRenderer();
  const { width, height } = renderer.viewport();
  // A vertex on the near plane projects by focal/near — about twenty-three
  // times at this lens — so an offset of a few hundred lands tens of thousands
  // of pixels out. Before the band, that face was thrown away and the ground
  // it belonged to had a hole in it.
  const wild = [{ x: -60000, y: -400, z: -1 }, { x: 60000, y: -400, z: -1 },
    { x: 0, y: 40000, z: 1 }];
  const banded = renderer.clipScreenBand(wild);
  assert.ok(banded.length >= 3, "the face survives the band rather than vanishing");
  for (const vertex of banded) {
    assert.ok(vertex.x >= -width - 1 && vertex.x <= width * 2 + 1,
      `x ${vertex.x} escaped the band`);
    assert.ok(vertex.y >= -height - 1 && vertex.y <= height * 2 + 1,
      `y ${vertex.y} escaped the band`);
  }
});

// This is the reported artifact, in the smallest form that shows it. The
// arena's ground quad runs from `worldNear` to `worldFar`, and in ordinary
// play the dolly sits between them — so the near edge of the floor is behind
// the camera every single frame.
test("the ground the dolly stands inside of stays inside the host's limits", () => {
  const renderer = createRenderer();
  const stage = renderer.stageGeometry();
  assert.ok(stage.worldNear < 0 && stage.worldFar > 0,
    "the arena has depth on both sides of the stage plane");
  for (const perspective of [0, .1, .5, .82, 1]) {
    for (const dolly of [-300, -900, -1500, -2400, -4000]) {
      place(renderer, { position: { x: 6000, y: 11400, z: dolly },
        target: { x: 6000, y: 11400, z: 0 }, perspective, width: 1200 });
      renderer.worldQuad(
        { x: 3000, y: stage.floorY, z: stage.worldNear },
        { x: 9000, y: stage.floorY, z: stage.worldNear },
        { x: 9000, y: stage.floorY, z: stage.worldFar },
        { x: 3000, y: stage.floorY, z: stage.worldFar }, [140, 150, 140]);
      const faces = renderer.take();
      // The host asserts the coordinate limit itself; reaching here means every
      // vertex cleared it. What is left to check is that the floor is actually
      // drawn — dropping it was the other half of the artifact.
      assert.ok(faces.length > 0,
        `the floor vanished at perspective ${perspective}, dolly ${dolly}`);
    }
  }
});

// The half of this that a single camera cannot tell you. Clipping the floor
// leaves a straight cut at constant depth, and where that cut lands on screen
// depends on the lens and on how far the camera is above the floor — so it has
// to be swept, not sampled. A cut inside the frame is a band of sky under the
// fighters' feet.
test("a floor crossing the camera plane keeps covering the lower screen", () => {
  const renderer = createRenderer();
  const stage = renderer.stageGeometry();
  const { height } = renderer.viewport();
  const lowestFloorEdge = (camera) => {
    place(renderer, camera);
    renderer.worldQuad(
      { x: 3000, y: stage.floorY, z: stage.worldNear },
      { x: 9000, y: stage.floorY, z: stage.worldNear },
      { x: 9000, y: stage.floorY, z: stage.worldFar },
      { x: 3000, y: stage.floorY, z: stage.worldFar }, [140, 150, 140]);
    const faces = renderer.take();
    if (!faces.length) return null;
    // The lowest point of the floor's silhouette anywhere across the frame,
    // not just at its corners — the cut can be slanted.
    return Math.max(...faces.flatMap((face) =>
      face.vertices.map(([, y]) => y)));
  };
  // How high the eye sits above the floor, in the range the game actually
  // uses: fighters stand 180 tall and the dolly frames their heads.
  for (const eye of [90, 145, 260, 420, 700]) {
    for (const perspective of [.1, .5, .82, 1]) {
      for (const dolly of [-260, -700, -1400]) {
        const camera = {
          position: { x: 6000, y: stage.floorY - eye, z: dolly },
          target: { x: 6000, y: stage.floorY - eye, z: 0 },
          perspective, width: 900 };
        const lowest = lowestFloorEdge(camera);
        assert.ok(lowest !== null,
          `floor vanished at eye ${eye}, perspective ${perspective}`);
        assert.ok(lowest >= height,
          `floor stopped at y ${Math.round(lowest)} (frame is ${height}) ` +
          `with eye ${eye}, perspective ${perspective}, dolly ${dolly}`);
      }
    }
  }
});

test("world segments are trimmed at the plane rather than raked across it", () => {
  const renderer = createRenderer();
  place(renderer, { position: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 1 } });
  assert.equal(renderer.worldSegment(0, 0, -900, 0, 0, -400), null,
    "a segment wholly behind the camera draws nothing");
  const crossing = renderer.worldSegment(200, 0, -600, 200, 0, 1400);
  assert.ok(crossing, "a crossing segment still draws its visible half");
  for (const end of [crossing.from, crossing.to]) {
    assert.ok(Number.isFinite(end.x) && Number.isFinite(end.y));
    assert.ok(Math.abs(end.x) <= renderer.hostLimit);
  }
  // Pinning used to leave both ends at the same wildly magnified offset; the
  // trimmed segment keeps its own direction instead.
  assert.ok(Math.abs(crossing.to.x - crossing.from.x) > 0 ||
    Math.abs(crossing.to.y - crossing.from.y) > 0);
});

test("clipping does not disturb a scene the camera is entirely outside of", () => {
  const renderer = createRenderer();
  const stage = renderer.stageGeometry();
  place(renderer, { position: { x: 6000, y: 11400, z: -6000 },
    target: { x: 6000, y: 11400, z: 0 }, perspective: .82, width: 1200 });
  renderer.worldQuad(
    { x: 5400, y: stage.floorY, z: -200 },
    { x: 6600, y: stage.floorY, z: -200 },
    { x: 6600, y: stage.floorY, z: 200 },
    { x: 5400, y: stage.floorY, z: 200 }, [140, 150, 140]);
  const faces = renderer.take();
  // Nothing crosses either plane, so the quad arrives as its two plain
  // triangles and the clipper has cost it nothing.
  assert.equal(faces.length, 2);
});
