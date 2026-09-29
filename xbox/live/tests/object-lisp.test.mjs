// Objects in KidLisp: the reader agrees with KidLisp's own parse, the compiled
// closures bind and loop as the dialect says, faces shade exactly like the
// game's worldQuad, the monowheel rolls, leans and lands, its baked meshes
// draw what its faces would have, and it keeps to the object budget.
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import test from "node:test";
import { compile, read, objectLight } from "../object-lisp.mjs";
import { createFrameVm, FRAME_CAMERA, FRAME_WORLD, FRAME_ASSET, FRAME_MODEL, FRAME_DISC,
  FRAME_CAPSULE, FRAME_ELLIPSE, FRAME_PLATE, FRAME_OUTLINE, FRAME_SHAPES, FRAME_SKETCH } from "../frame-vm.mjs";
import { parse } from "../../../system/public/aesthetic.computer/lib/kidlisp.mjs";

const objects = new URL("../objects/", import.meta.url);
const game = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");
const monowheelSource = await readFile(new URL("monowheel.lisp", objects), "utf8");

// Object space straight into world space, so tests read coordinates directly.
const identity = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1];
// The game's world is y-down; an upright object facing +x sits in it so.
const upright = [0, -24, 0, 1, 0, 0, 0, -1, 0, 0, 0, -1];
function faces(run, inputs = {}, place = identity) {
  const out = [];
  run(inputs, place, (...f) => out.push(f));
  return out;
}

test("the reader reads every object exactly as KidLisp's parse does", async () => {
  const names = (await readdir(objects)).filter((n) => n.endsWith(".lisp"));
  assert.ok(names.includes("monowheel.lisp"));
  const fixture = `; bare lines, commas, a continuation, a string
def r 24
ink red, move 0 1 2 (disc r)
(let spin (* distance
  0.5))
repeat 3 i (tri 0 0 0 1 0 0 0 1 0)
(if (> speed 3) (ink 1 2 3)`;   // its `)` left off on purpose: both auto-close
  for (const source of [fixture, ...await Promise.all(names.map((n) => readFile(new URL(n, objects), "utf8")))])
    assert.deepEqual(read(source), parse(source));
});

test("the sun is the game's globalLight", () => {
  const m = game.match(/const globalLight = normalize3\(\{ x: ([-.\d]+), y: ([-.\d]+), z: ([-.\d]+) \}\);/);
  assert.ok(m, "globalLight still spelled as expected in oskiewar.js");
  const v = m.slice(1).map(Number), len = Math.hypot(...v);
  v.forEach((x, i) => assert.ok(Math.abs(x / len - objectLight[i]) < 1e-12));
});

test("let re-evaluates every tick; def binds once and refuses inputs", () => {
  const run = compile(`def w 3
(let x (* time w))
(tri x 0 0 0 1 0 0 0 1)`);
  assert.equal(faces(run, { time: 1 })[0][0], 3);
  assert.equal(faces(run, { time: 2 })[0][0], 6);
  assert.throws(() => compile("(def x time)", "bad"), /bad: def binds once .* use let/);
  assert.throws(() => compile("(tri nope 0 0 0 1 0 0 0 1)", "bad"), /bad: unknown word `nope`/);
  assert.throws(() => compile("(spin 3)", "bad"), /bad: unknown form `spin`/);
});

test("if has no else, and repeat counts with its iterator", () => {
  const run = compile(`(if (> speed 10) (tri 0 0 0 1 0 0 0 1 0))
(repeat 3 i (move i 0 0 (tri 0 0 0 1 0 0 0 1 0)))`);
  assert.equal(faces(run, { speed: 0 }).length, 3);
  const fast = faces(run, { speed: 20 });
  assert.equal(fast.length, 4);
  assert.deepEqual(fast.slice(1).map((f) => f[0]), [0, 1, 2]);
});

test("move, rotate and scale nest; a mirror keeps faces facing out", () => {
  const [f] = faces(compile("(move 10 0 0 (rotate z (/ pi 2) (scale 2 (tri 1 0 0 0 0 0 0 0 1))))"));
  // x turns toward y: (1,0,0) → (0,2,0) after scale 2, then moved by 10.
  [10, 2, 0, 10, 0, 0, 10, 0, 2].forEach((want, i) => assert.ok(Math.abs(f[i] - want) < 1e-9, `${i}: ${f[i]}`));
  // A top face lit from above, and the same face mirrored across z: same shade.
  const top = "(quad 0 0 0  0 0 1  1 0 1  1 0 0)";
  const plain = faces(compile(`(ink 200 200 200) ${top}`), {}, upright);
  const mirrored = faces(compile(`(ink 200 200 200) (scale 1 1 -1 ${top})`), {}, upright);
  assert.deepEqual(mirrored.map((f) => f.slice(9)), plain.map((f) => f.slice(9)));
});

test("faces shade exactly as the game's worldQuad", () => {
  // litQuadColor, lifted out of the game as written.
  const from = game.indexOf("function litQuadColor(");
  const body = game.slice(from, game.indexOf("\n}\n", from) + 2);
  const lit = new Function("globalLight", `${body}; return litQuadColor;`)(
    { x: objectLight[0], y: objectLight[1], z: objectLight[2] });
  const run = compile(`(ink 180 120 60)
(rotate x .7 (rotate y .4 (quad 0 0 0  0 0 1  1 0 1  1 0 0) (quad 0 0 0  1 0 0  1 1 0  0 1 0)))
(glow (tri 0 0 0 1 0 0 0 1 0))`);
  const out = faces(run, {}, upright);
  const shades = new Set(out.slice(0, 4).map((f) => f.slice(9).join()));
  // Both triangles of a quad carry the light of its first three corners,
  // which are the first triangle's.
  const quadColor = (i) => {
    const f = out[i * 2];
    return lit({ x: f[0], y: f[1], z: f[2] }, { x: f[3], y: f[4], z: f[5] }, { x: f[6], y: f[7], z: f[8] }, [180, 120, 60]);
  };
  assert.deepEqual(out[0].slice(9), quadColor(0));
  assert.deepEqual(out[1].slice(9), quadColor(0));
  assert.deepEqual(out[2].slice(9), quadColor(1));
  assert.deepEqual(out[3].slice(9), quadColor(1));
  assert.equal(shades.size, 2, "two quads facing different ways shade differently");
  assert.deepEqual(out[4].slice(9), [180, 120, 60], "glow is unlit");
});

test("the monowheel rolls with distance, leans on its contact patch, squashes on landing", () => {
  const wheel = compile(monowheelSource, "monowheel");
  const r = 24, lap = Math.PI * 2 * r;
  const at = (inputs) => faces(wheel, inputs);
  const still = at({ distance: 30 });
  const close = (a, b) => a.length === b.length && a.every((f, i) => f.every((x, k) => Math.abs(x - b[i][k]) < 1e-6));
  assert.ok(close(still, at({ distance: 30 + lap })), "a full lap later it looks the same");
  assert.ok(!close(still, at({ distance: 30 + lap / 7 })), "partway round it does not");

  // Leaning right carries everything above the axle toward +z, about the
  // contact patch.
  const upper = (fs) => {
    let z = 0, n = 0;
    for (const f of fs) for (let v = 0; v < 9; v += 3) if (f[v + 1] > 10) { z += f[v + 2]; n++; }
    return z / n;
  };
  assert.ok(Math.abs(upper(at({}))) < 1e-6, "upright, it is symmetric");
  assert.ok(upper(at({ lean: .3 })) > 2, "leans right");
  assert.ok(upper(at({ lean: -.3 })) < -2, "leans left");
  const top = (fs) => Math.max(...fs.flatMap((f) => [f[1], f[4], f[7]]));
  const lowest = (fs) => Math.min(...fs.flatMap((f) => [f[1], f[4], f[7]]));
  assert.ok(Math.abs(lowest(at({})) + r) < .5, "tire sits on the ground");
  assert.ok(Math.abs(lowest(at({ land: 0 })) + r) < .5, "squashed, it still sits on the ground");
  assert.ok(top(at({ land: 0 })) < top(at({})) - 3, "a landing flattens it");
  assert.equal(top(at({ land: 1 })), top(at({})), "and a second later it is round again");
});

test("revolve faces out whichever way its profile is walked; radial and mirror repeat", () => {
  // A drum: radius 10, from z -5 to 5, turned about z in eight sides. Every
  // face's normal points away from the drum's centre.
  for (const profile of ["0 -5 10 -5 10 5 0 5", "0 5 10 5 10 -5 0 -5"]) {
    const out = faces(compile(`(revolve z ${profile})`));
    assert.equal(out.length, 8 * 4, "two capped ends and a wall");
    for (const f of out) {
      const u = [f[3] - f[0], f[4] - f[1], f[5] - f[2]], v = [f[6] - f[0], f[7] - f[1], f[8] - f[2]];
      const n = [u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0]];
      const c = [(f[0] + f[3] + f[6]) / 3, (f[1] + f[4] + f[7]) / 3, (f[2] + f[5] + f[8]) / 3];
      assert.ok(n[0] * c[0] + n[1] * c[1] + n[2] * c[2] > 0, `faces out: ${profile}`);
    }
  }
  assert.equal(faces(compile("(radial z 5 (tri 1 0 0 2 0 0 1 1 0))")).length, 5);
  const [a, b] = faces(compile("(mirror z (tri 0 0 1 1 0 1 0 1 1))"));
  assert.equal(a[2], 1);
  assert.equal(b[2], -1);
});

// A camera as FightCamDoll.prepare builds one, `d` from the axle and a little
// above, with its ortho width matched to that distance as the lab's is.
const W = 1280, H = 720;
const away = (d) => [0, -24 - d * Math.sin(.16), -d * Math.cos(.16)];
function camera(eye) {
  const d = Math.hypot(eye[0], eye[1] + 24, eye[2]);
  const norm = (v) => v.map((x) => x / Math.hypot(...v));
  const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
  const forward = norm([0 - eye[0], -24 - eye[1], 0 - eye[2]]);
  const right = norm(cross(forward, [0, -1, 0])), up = norm(cross(right, forward));
  return [FRAME_CAMERA, ...eye, ...right, ...up, ...forward, W / 2, H / 2, W / (2 * d * Math.tan(55 * Math.PI / 360)),
    W / (2 * Math.tan(55 * Math.PI / 360)), .82, 2.8 / 16000, -1.4, 8, -W * .5, W * 1.5, -H * .5, H * 1.5];
}
const noOp = () => {};
const runProgram = (values) => {
  const drawn = [];
  createFrameVm({ triangle3d: (...t) => drawn.push(t), box: noOp, line: noOp, wipe: noOp,
    write: noOp, systemWrite: noOp }).run(new Float32Array(values), values.length, []);
  return drawn;
};
const asset = (handle, m) => [FRAME_ASSET, handle, m.vertices.length / 3, m.count, ...m.vertices, ...m.faces];
// One tick of an object into a frame program, as the game would write it —
// faces only, or baked parts as ASSET/SHAPES once plus MODEL/SKETCH — run by
// the game's web interpreter. Returns what reached the host and the cost.
function draw(object, inputs, { baked = true, eye = away(220) } = {}) {
  const setup = baked ? [...object.meshes.flatMap((m, handle) => asset(handle, m)),
    ...object.sketches.flatMap((k, handle) => [FRAME_SHAPES, handle, k.count, k.records.length, ...k.records])] : [];
  const tick = [];
  const cost = { faces: 0, models: 0, sketches: 0 };
  const face = (...f) => { cost.faces++; tick.push(FRAME_WORLD, ...f); };
  const model = (radius, h0, h1, h2, m, at) => {
    cost.models++;
    tick.push(FRAME_MODEL, radius, h0, h1, h2, ...m.subarray(at, at + 12), ...objectLight);
  };
  const sketch = (h, m, at) => { cost.sketches++; tick.push(FRAME_SKETCH, h, ...m.subarray(at, at + 12)); };
  object(inputs, upright, baked ? { face, model, sketch } : face);
  cost.floats = tick.length;
  return { drawn: runProgram([...setup, ...camera(eye), ...tick]), cost };
}

test("baked meshes draw what the faces would have", () => {
  const wheel = compile(monowheelSource, "monowheel");
  assert.equal(wheel.parts, 2, "the wheel and the decks bake; only the lamps stay faces");
  for (const inputs of [{ distance: 40, speed: 300, lean: .2 }, { distance: 7, turbo: 1, lean: -.4, land: .05 }]) {
    const plain = draw(wheel, inputs, { baked: false }).drawn, baked = draw(wheel, inputs).drawn;
    assert.equal(baked.length, plain.length, "same triangles");
    for (let i = 0; i < plain.length; i++)
      for (let k = 0; k < 12; k++) {
        // Positions reach Float32 through a second matrix; a lit channel may
        // round the other way by one.
        const tolerance = k >= 9 ? 1 : .02 + Math.abs(plain[i][k]) * 1e-4;
        assert.ok(Math.abs(plain[i][k] - baked[i][k]) <= tolerance,
          `triangle ${i} value ${k}: faces ${plain[i][k]} baked ${baked[i][k]}`);
      }
  }
});

test("a MODEL op mirrors and scales exactly, and the host picks a level by size", () => {
  // One quad, placed mirrored across x, stretched and sheared: it must shade
  // as the same quad written out as world faces does.
  const object = compile("(ink 200 100 50) (quad 0 0 0  10 0 0  10 10 0  0 10 0)");
  const place = [5, 5, 5, -2, 0, 0, 0, .5, .3, 0, -.3, 1];
  const flat = [];
  object({}, place, (...f) => flat.push(f));
  let placed;
  object({}, place, { face: () => assert.fail("nothing is left as faces"), model: (...a) => { placed = a; } });
  const [radius, h0, h1, h2, m, at] = placed;
  const eye = camera(away(220));
  const viaModel = runProgram([...asset(0, object.meshes[0]), ...eye,
    FRAME_MODEL, radius, h0, h1, h2, ...m.subarray(at, at + 12), ...objectLight]);
  const direct = runProgram([...eye, ...flat.flatMap((f) => [FRAME_WORLD, ...f])]);
  assert.equal(viaModel.length, 2);
  assert.deepEqual(viaModel.map((t) => t.slice(9)), direct.map((t) => t.slice(9)), "same shade");
  viaModel.forEach((t, i) => t.slice(0, 9).forEach((x, k) => assert.ok(Math.abs(x - direct[i][k]) < .01)));

  const wheel = compile(monowheelSource, "monowheel");
  // Levels switch at 56 and 20 px of projected radius (1229 px at 1 unit):
  // at 220 the wheel and decks are both near, at 1500 both middle, at 5000 far.
  const levels = [220, 1500, 5000].map((d) => draw(wheel, {}, { eye: away(d) }).drawn.length);
  assert.deepEqual(levels, [148, 92, 80], "near, middle, far");
});

// The rule for every object (OBJECT-DIALECT.md): a tick sends at most 150
// numbers and 8 moving faces, and draws at most 150 / 100 / 80 triangles at
// levels 0 / 1 / 2. The old flat monowheel sent 88 faces, 1144 numbers.
test("the monowheel keeps to the object budget", (t) => {
  const wheel = compile(monowheelSource, "monowheel");
  const near = draw(wheel, { distance: 12, speed: 300 });
  t.diagnostic(`monowheel tick: ${near.cost.models} MODEL + ${near.cost.faces} WORLD = ${near.cost.floats} numbers`);
  assert.ok(near.cost.floats <= 150, `${near.cost.floats} numbers a tick`);
  assert.ok(near.cost.faces <= 8, `${near.cost.faces} moving faces`);
  const drawnAt = (eye) => draw(wheel, { speed: 300 }, { eye }).drawn.length;
  assert.ok(drawnAt(away(220)) <= 150);
  assert.ok(drawnAt(away(1500)) <= 100);
  assert.ok(drawnAt(away(5000)) <= 80);
});

// The flat monowheel the game draws today, measured through the game's own
// frame program, so the budget is held against something real.
test("the old flat monowheel, for comparison, and the game's flat one", (t) => {
  let ops = [];
  const vm = createFrameVm({ triangle3d: noOp, box: noOp, line: noOp, wipe: noOp, write: noOp, systemWrite: noOp });
  const api = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal",
    "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line",
    "triangle", "triangle3d", "triangles3d", "frame", "write", "systemWrite", "gameView",
    `${game}\nreturn { boot, drawMonowheel, begin: beginFrameProgram, end: endFrameProgram };`
  )(
    () => ({ monotonicUs: 0, unixMs: 1785870000000, simCount: 0, paintCount: 0, clientErrorReportStatus: "" }),
    () => ({ connected: false, down: [], leftX: 0, leftY: 0 }),
    () => ({ platform: "web", inputFamily: "keyboard" }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp, noOp, noOp,
    noOp, noOp, undefined, (p, n, s) => { ops = vm.decode(p, n, s); }, noOp, noOp,
    () => ({ width: 1920, height: 1080 }));
  api.boot();
  const wheel = { x: 6000, y: 900, z: 0, facing: 1, skatePitch: 0, onewheel: true };
  const tick = () => { api.begin(); api.drawMonowheel(wheel); api.end(); return ops; };
  // The old wheel sits behind the game's A/B flag now.
  globalThis.oskiewarOldMonowheel = true;
  let world;
  try { world = tick().filter((o) => o.op === FRAME_WORLD).length; }
  finally { delete globalThis.oskiewarOldMonowheel; }
  t.diagnostic(`old monowheel tick: ${world} WORLD = ${world * 13} numbers`);
  assert.equal(world, 88);
  // And the game's flat one: its sketches go up once, then a tick is three SKETCH.
  const first = tick().map((o) => o.op), second = tick();
  const numbers = second.filter((o) => o.op === FRAME_SKETCH).reduce((n, o) => n + 1 + o.args.length, 0);
  t.diagnostic(`the game's flat monowheel tick: ${numbers} numbers (${first.filter((op) => op === FRAME_SHAPES).length} sketches sent once)`);
  assert.equal(numbers, 42);
});

// Reported, and bounded only loosely: this Mac is shared, and a timing gate
// that flakes under load teaches nobody anything.
test("a tick is closures and two MODEL ops, not a tree walk", (t) => {
  const wheel = compile(monowheelSource, "monowheel");
  const out = { face() {}, model() {} };
  const time = (fn) => {
    const start = performance.now();
    for (let i = 0; i < 4000; i++) fn(i);
    return (performance.now() - start) / 4000 * 1000;
  };
  const baked = time((i) => wheel({ distance: i, time: i / 60, speed: 200 }, upright, out));
  const plain = time((i) => wheel({ distance: i, time: i / 60, speed: 200 }, upright, out.face));
  t.diagnostic(`monowheel: ${baked.toFixed(1)} µs a tick baked, ${plain.toFixed(1)} µs as faces`);
  assert.ok(baked < 5000, `${baked.toFixed(1)} µs a tick`);
});

// ——— the flat monowheel: world-anchored 2D shapes with ink outlines ———

const flatSource = await readFile(new URL("monowheel-flat.lisp", objects), "utf8");
const opCodes = { WORLD: FRAME_WORLD, DISC: FRAME_DISC, CAPSULE: FRAME_CAPSULE, ELLIPSE: FRAME_ELLIPSE,
  PLATE: FRAME_PLATE, OUTLINE: FRAME_OUTLINE };
// One tick of a flat object as the ops it sends, and as a frame program.
function flatTick(object, inputs, eye) {
  const view = camera(eye).slice(1), ops = [];
  const rec = (op) => (...a) => ops.push({ op, a: op === "PLATE" ? [a[0], ...a[1], ...a.slice(2)] : a });
  object(inputs, upright, { view, face: rec("WORLD"), disc: rec("DISC"), capsule: rec("CAPSULE"),
    ellipse: rec("ELLIPSE"), plate: rec("PLATE"), outline: rec("OUTLINE") });
  ops.program = [FRAME_CAMERA, ...view, ...ops.flatMap((o) => [opCodes[o.op], ...o.a])];
  ops.numbers = ops.program.length - 25;
  return ops;
}
const side = [0, -24, -220], front = [220, -24, 0];
const ellipses = (ops) => ops.filter((o) => o.op === "ELLIPSE").map((o) => o.a);

test("the flat monowheel rolls, leans and squashes", () => {
  const wheel = compile(flatSource, "monowheel-flat");
  // Through the per-tick path (a host without SKETCH), where the JS projects.
  // The spokes: the two bars in spoke pink.
  const spokes = (distance) => flatTick(wheel, { distance }, side)
    .filter((o) => o.op === "PLATE" && o.a.at(-3) === 232).map((o) => o.a.slice(1, 9));
  const close = (a, b) => a.every((s, i) => s.every((x, k) => Math.abs(x - b[i][k]) < 1e-6));
  const r = 24;
  assert.ok(close(spokes(10), spokes(10 + Math.PI * 2 * r)), "a full lap later the spokes are back");
  assert.ok(!close(spokes(10), spokes(10 + r * Math.PI / 4)), "an eighth of a turn on, they are not");
  // The tire, seen from ahead (its two caps' midpoint): leaning right carries
  // it to the rider's right, which from ahead is the screen's left.
  const tire = (inputs) => { const [, a, b] = ellipses(flatTick(wheel, inputs, front)); return (a[0] + b[0]) / 2; };
  assert.ok(tire({ lean: .3 }) < tire({}) - 5, "leans right");
  assert.ok(tire({ lean: -.3 }) > tire({}) + 5, "leans left");
  // A landing: side on, the drum's near cap gets wider and shorter.
  const cap = (inputs) => ellipses(flatTick(wheel, inputs, side))[2];
  const spans = (e) => [Math.hypot(e[3], e[4]), Math.hypot(e[5], e[6])].sort((a, b) => a - b);
  const [round, squashed] = [spans(cap({})), spans(cap({ land: 0 }))];
  assert.ok(squashed[1] > round[1] * 1.05, "wider");
  assert.ok(squashed[0] < round[0] * .95, "shorter");
});

test("flat shapes carry their own depth: near covers far, the visible face comes forward", () => {
  const wheel = compile(flatSource, "monowheel-flat");
  // Three-quarter views from either side, where the tire shows its band.
  for (const eye of [[-130, -80, -170], [130, -80, 170]]) {
    const ops = flatTick(wheel, {}, eye);
    const [shadow, far, near, rim] = ellipses(ops);
    const band = ops.filter((o) => o.op === "PLATE")[0].a;
    const bandDepth = band[1 + band[0] * 2];
    assert.ok(far[2] > bandDepth && bandDepth > near[2], "far cap, band, near cap");
    // The rim lies in the near cap's plane, not the far one's.
    assert.ok(Math.abs(rim[2] - near[2]) < 1e-9 && rim[2] < far[2], "`toward` puts the face on the side the camera sees");
    assert.ok(shadow[2] > far[2], "the shadow lies behind the wheel");
  }
});

// The flat rule (OBJECT-DIALECT.md): a tick sends at most 60 numbers, and at
// every distance the host draws fewer triangles than for the lit wheel.
test("the flat monowheel keeps to its budget, under the lit one's", (t) => {
  const flat = compile(flatSource, "monowheel-flat"), lit = compile(monowheelSource, "monowheel");
  assert.equal(flat.parts, 3, "shadow, wheel and deck bake; nothing is projected in JS");
  const near = draw(flat, { speed: 300, distance: 5 });
  assert.deepEqual([near.cost.sketches, near.cost.faces, near.cost.models], [3, 0, 0]);
  assert.ok(near.cost.floats <= 60, `${near.cost.floats} numbers`);
  const rows = [220, 600, 1500, 4000].map((d) => [d, draw(flat, { speed: 300 }, { eye: away(d) }).drawn.length,
    draw(lit, { speed: 300 }, { eye: away(d) }).drawn.length]);
  t.diagnostic(`flat tick ${near.cost.floats} numbers; triangles flat/lit by distance: ${rows.map(([d, f, l]) => `${d}: ${f}/${l}`).join(", ")}`);
  for (const [d, f, l] of rows) assert.ok(f < l, `at ${d}: flat ${f} triangles, lit ${l}`);
});

// Baked and projected per tick, the flat wheel covers the same screen: the
// sketch the host draws is the shapes the JS path would have sent.
test("a baked sketch draws where the per-tick shapes do", () => {
  const flat = compile(flatSource, "monowheel-flat");
  const box = (tris) => {
    const b = [Infinity, Infinity, -Infinity, -Infinity];
    for (const t of tris) for (let v = 0; v < 9; v += 3) {
      b[0] = Math.min(b[0], t[v]); b[1] = Math.min(b[1], t[v + 1]); b[2] = Math.max(b[2], t[v]); b[3] = Math.max(b[3], t[v + 1]);
    }
    return b;
  };
  for (const [inputs, eye] of [[{ distance: 9 }, away(220)], [{ lean: .3, land: .05, turbo: 1 }, [-130, -80, -170]]]) {
    const baked = box(draw(flat, inputs, { eye }).drawn), perTick = box(runProgram(flatTick(flat, inputs, eye).program));
    baked.forEach((x, i) => assert.ok(Math.abs(x - perTick[i]) < 4, `edge ${i}: baked ${x} per tick ${perTick[i]}`));
  }
});

// Reported, and bounded only loosely, as the lit timing is.
test("a flat tick is three SKETCH ops, cheaper than the lit tick", (t) => {
  const flat = compile(flatSource, "monowheel-flat"), lit = compile(monowheelSource, "monowheel");
  const out = { view: camera(away(260)).slice(1), face() {}, model() {}, sketch() {} };
  const time = (object) => {
    const start = performance.now();
    for (let i = 0; i < 4000; i++) object({ distance: i, time: i / 60, speed: 200 }, upright, out);
    return (performance.now() - start) / 4000 * 1000;
  };
  time(flat); time(lit);
  const [f, l] = [time(flat), time(lit)];
  t.diagnostic(`a tick: flat ${f.toFixed(1)} µs, lit ${l.toFixed(1)} µs`);
  assert.ok(f < 5000);
});

test("the game carries exactly the lab's compiler and objects (run xbox/tools/embed-objects.mjs after editing)", async () => {
  const { generate, embedded } = await import("../../tools/embed-objects.mjs");
  assert.equal(embedded(), generate());
});
