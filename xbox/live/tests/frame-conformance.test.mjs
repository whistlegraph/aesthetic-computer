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
    `${source}\nreturn { boot, cameraDoll, worldQuad, worldTriangle, drawMonowheel, players, drawRunner,
       drawSpotShadow, captureQuadMesh, drawQuadMesh,
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

const onScreen = (face) => {
  for (let v = 0; v < 9; v += 3)
    if (face[v] >= 0 && face[v] <= 1920 && face[v + 1] >= 0 && face[v + 1] <= 1080) return true;
  return false;
};

test("a shadow lies flat behind its caster on both paths", () => {
  const immediate = createGame({ buffered: false });
  const program = createGame({ buffered: true });
  const stage = immediate.stage();
  const camera = { position: { x: 6000, y: stage.floorY - 300, z: -900 },
    target: { x: 6000, y: stage.floorY - 300, z: 0 }, perspective: .82, width: 900 };
  const scene = (game) => game.drawSpotShadow(6000, stage.floorY - 40, 0, 60, [20, 20, 30]);
  place(immediate, camera);
  place(program, camera);
  const a = immediate.draw(scene), b = program.draw(scene);
  assert.ok(a.length > 0, "the shadow draws");
  sameFaces(a, b, "shadow");
  const depth = a[0][2];
  assert.ok(a.every((face) => face[2] === depth && face[5] === depth && face[8] === depth),
    "one flat depth across the whole ellipse");
});

test("a retained mesh drawn by handle matches the game's own mesh path", () => {
  const immediate = createGame({ buffered: false });
  const program = createGame({ buffered: true });
  const stage = immediate.stage();
  const build = (game) => game.captureQuadMesh(() => {
    for (let i = 0; i < 6; i++)
      game.worldQuad({ x: 5400 + i * 200, y: stage.floorY - 300, z: -120 },
        { x: 5560 + i * 200, y: stage.floorY - 300, z: -120 },
        { x: 5560 + i * 200, y: stage.floorY, z: -120 },
        { x: 5400 + i * 200, y: stage.floorY, z: -120 }, [90 + i * 20, 120, 160]);
  });
  const meshA = build(immediate), meshB = build(program);
  for (const { perspective, dolly, eye } of cameras.filter((_, i) => i % 3 === 0)) {
    const camera = { position: { x: 6000, y: stage.floorY - eye, z: dolly },
      target: { x: 6000, y: stage.floorY - eye, z: 0 }, perspective, width: 900 };
    place(immediate, camera);
    place(program, camera);
    // Twice: the first program carries the ASSET, the second only the handle.
    for (const pass of [1, 2]) {
      const a = immediate.draw((g) => g.drawQuadMesh(meshA)).filter(onScreen);
      const b = program.draw((g) => g.drawQuadMesh(meshB)).filter(onScreen);
      sameFaces(a, b, `mesh pass ${pass} perspective ${perspective} dolly ${dolly} eye ${eye}`);
    }
  }
});

test("a mesh crosses the boundary once, then by handle", () => {
  const programs = [];
  const game = createGame({ buffered: true, onProgram: (ops) => programs.push(ops) });
  const stage = game.stage();
  place(game, { position: { x: 6000, y: stage.floorY - 300, z: -900 },
    target: { x: 6000, y: stage.floorY - 300, z: 0 }, perspective: .5, width: 900 });
  const mesh = game.captureQuadMesh(() => game.worldQuad(
    { x: 5900, y: stage.floorY - 200, z: 0 }, { x: 6100, y: stage.floorY - 200, z: 0 },
    { x: 6100, y: stage.floorY, z: 0 }, { x: 5900, y: stage.floorY, z: 0 }, [200, 100, 50]));
  game.draw((g) => g.drawQuadMesh(mesh));
  game.draw((g) => g.drawQuadMesh(mesh));
  const kinds = programs.map((ops) => ops.map((o) => o.op));
  assert.ok(kinds[0].includes(12) && kinds[0].includes(13), "first frame: ASSET and MESH");
  assert.ok(!kinds[1].includes(12) && kinds[1].includes(13), "second frame: MESH only");
});


// The monowheel is a flat object (objects/monowheel-flat.lisp). Buffered, it
// goes up as SHAPES once and is three SKETCH ops a frame, drawn by the
// interpreter; immediate, the game projects the same shapes and fills them
// itself. Both must cover the same screen.
test("the flat monowheel draws the same whether buffered or immediate", () => {
  const programs = [];
  const immediate = createGame({ buffered: false });
  const program = createGame({ buffered: true, onProgram: (ops) => programs.push(ops) });
  const stage = immediate.stage();
  const camera = { position: { x: 6000, y: stage.floorY - 160, z: -420 },
    target: { x: 6000, y: stage.floorY - 30, z: 0 }, perspective: .82, width: 600 };
  const wheel = { x: 6000, y: stage.floorY, z: 0, skatePitch: 0, facing: 1 };
  place(immediate, camera);
  place(program, camera);
  const a = immediate.draw((g) => g.drawMonowheel(wheel));
  const b = program.draw((g) => g.drawMonowheel(wheel));
  const box = (faces) => {
    const out = [Infinity, Infinity, -Infinity, -Infinity];
    for (const f of faces) for (let v = 0; v < 9; v += 3) {
      out[0] = Math.min(out[0], f[v]); out[1] = Math.min(out[1], f[v + 1]);
      out[2] = Math.max(out[2], f[v]); out[3] = Math.max(out[3], f[v + 1]);
    }
    return out;
  };
  assert.ok(a.length > 20 && b.length > 20, `both draw: immediate ${a.length}, buffered ${b.length}`);
  box(a).forEach((x, i) => assert.ok(Math.abs(x - box(b)[i]) < 4, `edge ${i}: immediate ${x} buffered ${box(b)[i]}`));
  const ops = programs[0].map((o) => o.op);
  assert.equal(ops.filter((op) => op === 18).length, 3, "three sketches go up once");
  assert.equal(ops.filter((op) => op === 19).length, 3, "three SKETCH ops a frame");
  program.draw((g) => g.drawMonowheel(wheel));
  assert.equal(programs[1].filter((o) => o.op === 18).length, 0, "and not again");
});

// Flat figures (behind globalThis.oskiewarFlatFigures): a fighter is one
// FIGURE op, and the host draws fewer triangles for it than for the figure
// the game draws today, at every distance. Immediate, the game draws the same
// figure itself.
test("a flat figure is one op, fewer triangles than today's, and draws the same immediate", (t) => {
  const pose = (game, dist) => {
    const p = game.players[0];
    p.parkEntrance = null;
    place(game, { position: { x: p.x, y: p.y - 120, z: p.z - dist }, target: { x: p.x, y: p.y - 100, z: p.z }, width: 500 });
    return p;
  };
  const rows = [];
  try {
    for (const dist of [260, 700, 1500]) {
      const programs = [];
      globalThis.oskiewarFlatFigures = false;
      const today = createGame({ buffered: true, onProgram: (ops) => programs.push(ops) });
      const was = today.draw((g) => g.drawRunner(pose(g, dist), 1));
      const sent = programs.at(-1).filter((o) => o.op !== 9).reduce((n, o) => n + 1 + o.args.length, 0);
      globalThis.oskiewarFlatFigures = true;
      const flatPrograms = [];
      const flat = createGame({ buffered: true, onProgram: (ops) => flatPrograms.push(ops) });
      flat.draw((g) => g.drawRunner(pose(g, dist), 1));
      const now = flat.draw((g) => g.drawRunner(pose(g, dist), 1));
      const ops = flatPrograms.at(-1).filter((o) => o.op !== 9);
      rows.push(`${dist}: ${sent} numbers ${was.length} triangles → ${ops.reduce((n, o) => n + 1 + o.args.length, 0)} numbers ${now.length} triangles`);
      assert.deepEqual(ops.map((o) => o.op), [20], "one FIGURE a tick after the first");
      assert.ok(now.length < was.length, `at ${dist}: flat ${now.length}, today ${was.length}`);
      if (dist === 260) {
        const immediate = createGame({ buffered: false });
        const drawn = immediate.draw((g) => g.drawRunner(pose(g, dist), 1));
        const box = (faces) => faces.reduce((b, f) => [Math.min(b[0], f[0], f[3], f[6]), Math.min(b[1], f[1], f[4], f[7]),
          Math.max(b[2], f[0], f[3], f[6]), Math.max(b[3], f[1], f[4], f[7])], [Infinity, Infinity, -Infinity, -Infinity]);
        box(now).forEach((x, i) => assert.ok(Math.abs(x - box(drawn)[i]) < 4, `edge ${i}: buffered ${x}, immediate ${box(drawn)[i]}`));
      }
    }
  } finally { delete globalThis.oskiewarFlatFigures; }
  t.diagnostic(`a figure, today → flat: ${rows.join("; ")}`);
});

// The flat figure reads the live pose: the joints it hangs shapes on are the
// ones the rig draws from, frame by frame, so a strike moves them.
test("a flat figure's joints follow a strike, not a rest pose", () => {
  let clock = 1e6;
  const noOp = () => {};
  const api = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal",
    "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line",
    "triangle", "triangle3d", "triangles3d", "frame", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, players, figureJoints, runnerWorldGeometry, startMelee };`
  )(
    () => ({ monotonicUs: clock, unixMs: 1785870000000, simCount: 0, paintCount: 0, clientErrorReportStatus: "" }),
    () => ({ connected: false, down: [], leftX: 0, leftY: 0 }),
    () => ({ platform: "web", inputFamily: "keyboard" }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp, noOp, noOp,
    noOp, noOp, undefined, undefined, noOp, noOp, () => ({ width: 1920, height: 1080 }));
  api.boot();
  const p = api.players[0];
  const joint = (J, i) => [J[i * 3], J[i * 3 + 1], J[i * 3 + 2]];
  const at = (t) => api.figureJoints(p, api.runnerWorldGeometry(p, t)).slice();
  const rest = at(1);
  for (const [kind, ends, role] of [["KICK", [14, 15], "shin"], ["PUNCH", [8, 9], "forearm"]]) {
    api.startMelee(p, kind, clock);
    clock += 90000;
    const world = api.runnerWorldGeometry(p, 1.09), strike = api.figureJoints(p, world).slice();
    // The same ends the rig's own segments have.
    for (const [index, side] of [[ends[0], "left"], [ends[1], "right"]]) {
      const bone = world.segments.find((s) => s.part === `${side}-${role === "shin" ? "leg" : "arm"}` && s.role.endsWith(role));
      assert.deepEqual(joint(strike, index), [bone.x2, bone.y2, bone.z2], `${kind}: the flat ${side} joint is the rig's`);
    }
    const moved = Math.max(...ends.map((j) => Math.hypot(...joint(strike, j).map((v, k) => v - joint(rest, j)[k]))));
    assert.ok(moved > 10, `${kind}: a hand or foot moved ${moved.toFixed(1)} from rest`);
    clock += 1e6;
  }
});
