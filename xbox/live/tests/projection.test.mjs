// One projection for every world object, at any stage shape. The Mac stage
// follows its window (2880×800 on a wide slab tile), so nothing may assume a
// 1920×1080 stage or hold a camera from earlier in the frame.
import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");
const macShell = await readFile(new URL("../../macos-native/main.swift", import.meta.url), "utf8");
const metalScene = await readFile(new URL("../../../apple/oskiewar/Sources/MetalSceneView.swift", import.meta.url), "utf8");

function seat(view) {
  let now = 0;
  const faces = [], alpha = [];
  const noOp = () => {};
  const pad = { connected: true, down: [], leftX: 0, leftY: 0 };
  const fight = new Function("runtime", "gamepad", "capabilities", "telemetry", "gameSignal",
    "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle",
    "triangle3d", "triangleAlpha", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, sim, paint, projectPoint, mainNativeCamera,
      enterGame: () => enterGame(runtime().monotonicUs),
      startFight: () => { shellMode = "GAME"; selecting = false; players[1].npc = false; players[1].bot = false; applyRoster(players[1], 2); startReplay(runtime().monotonicUs); resetRound(runtime().monotonicUs, true); },
      stage: () => ({ width: viewWidth(), height: viewHeight, map: currentMapId }),
      roof: () => ({ y: dojoRoofY, left: worldLeft, right: worldRight, near: worldNear, far: worldFar }),
      drawDojoRoof, worldEntities, players, netSnapshot, doll: () => cameraDoll };`)(
    () => ({ monotonicUs: now, unixMs: 1785870000000, simCount: 0, paintCount: 0 }),
    () => ({ ...pad, down: [] }), () => ({ platform: "macos", inputFamily: "keyboard" }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp, noOp, noOp, noOp,
    (...v) => faces.push(v), (...v) => alpha.push(v), noOp, noOp, () => view);
  globalThis.__oskiewarOpponent = "dummy";
  fight.boot(); fight.enterGame(); fight.startFight();
  delete globalThis.__oskiewarOpponent;
  for (let i = 0; i < 30; i++) { now += 16667; fight.sim(); }
  return { fight, faces, alpha, paint() { faces.length = alpha.length = 0; fight.paint(); } };
}

for (const view of [{ width: 1920, height: 1080 }, { width: 2880, height: 800 }]) {
  test(`a ${view.width}×${view.height} stage is the stage every projection uses`, () => {
    const s = seat(view);
    s.paint();
    assert.deepEqual(s.fight.stage().width, view.width);
    assert.deepEqual(s.fight.stage().height, view.height);
    const m = s.fight.mainNativeCamera(), v = s.fight.doll().view;
    for (const [i, value] of [[12, v.centerX], [13, v.centerY], [14, v.orthoScale], [15, v.focal]])
      assert.equal(m[i], Math.fround(value));
    assert.deepEqual([m[18], m[19], m[20], m[21]], [0, 0, view.width, view.height]);
  });

  test(`at ${view.width}×${view.height} the translucent ceiling glass lands under the camera's roof`, () => {
    const s = seat(view);
    assert.equal(s.fight.stage().map, "dojo");
    s.paint();
    // Look up at the roof from under its middle, as a jump into it does.
    const r = s.fight.roof(), d = s.fight.doll(), cx = (r.left + r.right) / 2, cz = (r.near + r.far) / 2;
    d.target = { x: cx, y: r.y, z: cz }; d.position = { x: cx, y: r.y + 900, z: cz - 2400 }; d.prepare();
    s.alpha.length = 0; s.fight.drawDojoRoof();
    assert.ok(s.alpha.length > 0, "the roof panes are drawn translucent");
    const corners = [[r.left, r.near], [r.right, r.near], [r.right, r.far], [r.left, r.far]]
      .map(([x, z]) => s.fight.projectPoint(x, r.y, z));
    const minX = Math.min(...corners.map(p => p.x)), maxX = Math.max(...corners.map(p => p.x));
    const minY = Math.min(...corners.map(p => p.y)), maxY = Math.max(...corners.map(p => p.y));
    for (const t of s.alpha) for (let k = 0; k < 9; k += 3) {
      assert.ok(t[k] >= minX - 1 && t[k] <= maxX + 1, `x ${t[k]} outside ${minX}..${maxX}`);
      assert.ok(t[k + 1] >= minY - 1 && t[k + 1] <= maxY + 1, `y ${t[k + 1]} outside ${minY}..${maxY}`);
    }
  });
}

test("a native camera taken mid-frame follows the lens, never an earlier copy", () => {
  const s = seat({ width: 2880, height: 800 });
  s.paint();
  const doll = s.fight.doll();
  const before = s.fight.mainNativeCamera()[0];
  doll.position.x += 100; doll.prepare();
  assert.ok(Math.abs(s.fight.mainNativeCamera()[0] - before - 100) < .01);
});

test("the Mac maps translucent faces with the live stage, not a 1920×1080 constant", () => {
  assert.ok(!/CGSize\(width: 1920, height: 1080\)/.test(macShell), "main.swift names a fixed stage");
  assert.ok(!/func triangle\(_ values: \[Double\], logicalSize/.test(metalScene), "triangle takes a stage per call");
});

// The native lens (27 floats) and projectPoint are the same projection.
function nativeProject(m, x, y, z) {
  const dx = x - m[0], dy = y - m[1], dz = z - m[2];
  const vx = dx * m[3] + dy * m[4] + dz * m[5], vy = dx * m[6] + dy * m[7] + dz * m[8];
  const vz = dx * m[9] + dy * m[10] + dz * m[11];
  const k = m[14] + (m[15] / Math.max(m[17], vz) - m[14]) * m[16];
  return { x: m[12] + vx * k, y: m[13] - vy * k };
}

for (const view of [{ width: 1920, height: 1080 }, { width: 2880, height: 800 }]) {
  test(`at ${view.width}×${view.height} every world entity projects the same through either lens`, () => {
    const s = seat(view);
    s.paint();
    const m = s.fight.mainNativeCamera(), entities = s.fight.worldEntities();
    assert.ok(entities.some(e => e.type === "player") && entities.some(e => e.type === "gun-pickup"));
    for (const e of entities) {
      const a = s.fight.projectPoint(e.x, e.y, e.z ?? 0), b = nativeProject(m, e.x, e.y, e.z ?? 0);
      if (a.behind) continue;
      assert.ok(Math.abs(a.x - b.x) < .5 && Math.abs(a.y - b.y) < .5, `${e.type} ${e.id}: ${a.x},${a.y} vs ${b.x},${b.y}`);
    }
  });
}

test("world entities carry a renderer id and a type, and the snapshot never sees them", () => {
  const s = seat({ width: 1920, height: 1080 });
  const keys = Object.keys(s.fight.players[0]).join();
  const before = JSON.stringify(s.fight.netSnapshot());
  s.paint();
  const first = s.fight.worldEntities();
  s.paint();
  const second = s.fight.worldEntities();
  assert.deepEqual(second.map(e => [e.id, e.type]), first.map(e => [e.id, e.type]));
  assert.equal(new Set(first.map(e => e.id)).size, first.length);
  assert.equal(Object.keys(s.fight.players[0]).join(), keys);
  assert.equal(JSON.stringify(s.fight.netSnapshot()), before);
});
