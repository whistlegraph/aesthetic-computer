// Conformance: a world face drawn immediately by the game and the same face
// written into a frame program and run by the web interpreter must reach the
// host as the same triangles. The game's immediate path is the one the
// console and the harness still use, so this is what keeps the two readers
// of the format honest while projection moves out of the game.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import { createFrameVm } from "../frame-vm.mjs";

const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");

function createGame({ buffered, onProgram = null }) {
  const faces = [];
  const noOp = () => {};
  const record = (...v) => faces.push(v.slice(0, 12));
  const vm = createFrameVm({ triangle3d: record, box: noOp, line: noOp,
    wipe: noOp, write: noOp, systemWrite: noOp, comicWrite: noOp });
  const frame = buffered ? (program, length, strings) => {
    onProgram?.(vm.decode(program, length, strings));
    vm.run(program, length, strings);
  } : undefined;
  const api = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal",
    "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line",
    "triangle", "triangle3d", "triangles3d", "frame", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, cameraDoll, worldQuad, worldTriangle,
       stage: () => ({ floorY, worldNear, worldFar }),
       begin: beginFrameProgram, end: endFrameProgram,
       insetView: (rect, draw) => withRenderView(cameraDoll, rect, draw) };`
  )(
    () => ({ monotonicUs: 0, unixMs: 1785870000000, simCount: 0, paintCount: 0,
      clientErrorReportStatus: "" }),
    () => ({ connected: false, down: [], leftX: 0, leftY: 0 }),
    () => ({ platform: "web", inputFamily: "keyboard" }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp, noOp, noOp,
    noOp, record, undefined, frame, noOp, noOp, () => ({ width: 1920, height: 1080 })
  );
  api.boot();
  api.cameraDoll.dirty = true;
  const draw = (fn) => {
    if (buffered) { api.begin(); try { fn(api); } finally { api.end(); } }
    else fn(api);
    return faces.splice(0);
  };
  return { ...api, draw };
}

function place(game, { position, target, perspective = 1, fov = 55, width = 1200, roll = 0 }) {
  const doll = game.cameraDoll;
  doll.position = { ...position };
  doll.target = { ...target };
  doll.perspective = perspective;
  doll.fov = fov;
  doll.width = width;
  doll.roll = roll;
  doll.dirty = true;
}

// The program is Float32, the immediate path Float64. Near the plane the lens
// magnifies by focal/near (~230x), so the tolerance grows with the value.
function sameFaces(immediate, buffered, label) {
  assert.equal(buffered.length, immediate.length, `${label}: face count`);
  for (let i = 0; i < immediate.length; i++)
    for (let k = 0; k < 12; k++) {
      const a = immediate[i][k], b = buffered[i][k];
      const tolerance = k >= 9 ? .6 : .05 + Math.abs(a) * 2e-4;
      assert.ok(Math.abs(a - b) <= tolerance,
        `${label}: face ${i} value ${k} immediate ${a} program ${b}`);
    }
}

const cameras = [];
for (const perspective of [0, .1, .5, .82, 1])
  for (const dolly of [-300, -900, -2400])
    for (const eye of [90, 300, 700])
      cameras.push({ perspective, dolly, eye });

test("world quads straddling the near plane match through the interpreter", () => {
  const immediate = createGame({ buffered: false });
  const program = createGame({ buffered: true });
  const stage = immediate.stage();
  let total = 0;
  for (const { perspective, dolly, eye } of cameras) {
    const camera = { position: { x: 6000, y: stage.floorY - eye, z: dolly },
      target: { x: 6000, y: stage.floorY - eye, z: 0 }, perspective, width: 900 };
    const scene = (game) => {
      game.worldQuad(
        { x: 3000, y: stage.floorY, z: Math.min(stage.worldNear, dolly - 600) },
        { x: 9000, y: stage.floorY, z: Math.min(stage.worldNear, dolly - 600) },
        { x: 9000, y: stage.floorY, z: stage.worldFar },
        { x: 3000, y: stage.floorY, z: stage.worldFar }, [140, 150, 140]);
      game.worldQuad(
        { x: 5800, y: stage.floorY - 400, z: 0 }, { x: 6200, y: stage.floorY - 400, z: 0 },
        { x: 6200, y: stage.floorY, z: 0 }, { x: 5800, y: stage.floorY, z: 0 }, [200, 80, 60]);
    };
    place(immediate, camera);
    place(program, camera);
    const a = immediate.draw(scene), b = program.draw(scene);
    total += a.length;
    sameFaces(a, b, `perspective ${perspective} dolly ${dolly} eye ${eye}`);
  }
  assert.ok(total > 0, "the sweep drew something");
});

test("a face wholly behind the camera draws nothing on either path", () => {
  const immediate = createGame({ buffered: false });
  const program = createGame({ buffered: true });
  const camera = { position: { x: 0, y: 0, z: 0 }, target: { x: 0, y: 0, z: 1 } };
  const scene = (game) => game.worldTriangle({ x: 0, y: 0, z: -900 },
    { x: 100, y: 0, z: -900 }, { x: 0, y: 100, z: -400 }, [1, 2, 3]);
  place(immediate, camera);
  place(program, camera);
  assert.equal(immediate.draw(scene).length, 0);
  assert.equal(program.draw(scene).length, 0);
});

test("inside a second view the band is its rectangle on both paths", () => {
  const immediate = createGame({ buffered: false });
  const program = createGame({ buffered: true });
  const stage = immediate.stage();
  const camera = { position: { x: 6000, y: stage.floorY - 300, z: -900 },
    target: { x: 6000, y: stage.floorY - 300, z: 0 }, perspective: .82, width: 900 };
  const rect = { x: 1400, y: 30, w: 320, h: 180 };
  const scene = (game) => game.insetView(rect, () => game.worldQuad(
    { x: 3000, y: stage.floorY, z: -1500 }, { x: 9000, y: stage.floorY, z: -1500 },
    { x: 9000, y: stage.floorY, z: stage.worldFar }, { x: 3000, y: stage.floorY, z: stage.worldFar },
    [140, 150, 140]));
  place(immediate, camera);
  place(program, camera);
  const a = immediate.draw(scene), b = program.draw(scene);
  sameFaces(a, b, "inset");
  for (const face of b)
    for (let v = 0; v < 9; v += 3) {
      assert.ok(face[v] >= rect.x - .01 && face[v] <= rect.x + rect.w + .01, `x ${face[v]} in rect`);
      assert.ok(face[v + 1] >= rect.y - .01 && face[v + 1] <= rect.y + rect.h + .01, `y ${face[v + 1]} in rect`);
    }
});

test("a frame carries the camera once, not once per face", () => {
  const decoded = [];
  const game = createGame({ buffered: true,
    onProgram: (ops) => decoded.push(...ops) });
  const stage = game.stage();
  place(game, { position: { x: 6000, y: stage.floorY - 300, z: -900 },
    target: { x: 6000, y: stage.floorY - 300, z: 0 }, perspective: .5, width: 900 });
  game.draw((g) => {
    for (let i = 0; i < 5; i++)
      g.worldQuad({ x: 5000 + i * 100, y: stage.floorY, z: -100 },
        { x: 5080 + i * 100, y: stage.floorY, z: -100 },
        { x: 5080 + i * 100, y: stage.floorY, z: 100 },
        { x: 5000 + i * 100, y: stage.floorY, z: 100 }, [10, 20, 30]);
  });
  const kinds = decoded.map((entry) => entry.op);
  assert.equal(kinds.filter((op) => op === 9).length, 1, "one CAMERA");
  assert.equal(kinds.filter((op) => op === 10).length, 10, "two WORLD faces per quad");
  assert.equal(kinds[0], 9, "the camera comes before the faces it projects");
});

test("a host with only a flat triangle still takes a buffered program", () => {
  // The Canvas2D fallback: no triangle3d, a frame host. Every face reaches
  // the flat triangle; nothing throws.
  const flat = [];
  const noOp = () => {};
  const vm = createFrameVm({ triangle: (...v) => flat.push(v), box: noOp, line: noOp,
    wipe: noOp, write: noOp, systemWrite: noOp, comicWrite: noOp });
  const api = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal",
    "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line",
    "triangle", "triangle3d", "triangles3d", "frame", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, cameraDoll, worldQuad, screenRect: (x, y, w, h) => screenRect(x, y, w, h, [1, 2, 3]),
       stage: () => ({ floorY, worldFar }), begin: beginFrameProgram, end: endFrameProgram };`
  )(
    () => ({ monotonicUs: 0, unixMs: 1785870000000, simCount: 0, paintCount: 0,
      clientErrorReportStatus: "" }),
    () => ({ connected: false, down: [], leftX: 0, leftY: 0 }),
    () => ({ platform: "web", inputFamily: "keyboard" }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp, noOp, noOp,
    (...v) => flat.push(v), undefined, undefined,
    (program, length, strings) => vm.run(program, length, strings),
    noOp, noOp, () => ({ width: 1920, height: 1080 }));
  api.boot();
  const stage = api.stage();
  place({ cameraDoll: api.cameraDoll }, { position: { x: 6000, y: stage.floorY - 300, z: -900 },
    target: { x: 6000, y: stage.floorY - 300, z: 0 }, perspective: .5, width: 900 });
  api.begin();
  try {
    api.screenRect(10, 10, 100, 50);
    api.worldQuad({ x: 5800, y: stage.floorY, z: -100 }, { x: 6200, y: stage.floorY, z: -100 },
      { x: 6200, y: stage.floorY, z: 100 }, { x: 5800, y: stage.floorY, z: 100 }, [9, 9, 9]);
  } finally { api.end(); }
  assert.ok(flat.length >= 4, `screen rect and world quad reached the flat host (${flat.length})`);
});

