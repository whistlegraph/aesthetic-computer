// Objects in KidLisp: the reader agrees with KidLisp's own parse, the compiled
// closures bind and loop as the dialect says, faces shade exactly like the
// game's worldQuad, and the monowheel rolls, leans and lands. The last test
// runs the monowheel through the web interpreter the game itself uses.
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import test from "node:test";
import { compile, read, objectLight } from "../object-lisp.mjs";
import { createFrameVm, FRAME_CAMERA, FRAME_WORLD } from "../frame-vm.mjs";
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
  assert.ok(still.length > 150 && still.length <= 300, `face budget: ${still.length}`);
  const close = (a, b) => a.every((f, i) => f.every((x, k) => Math.abs(x - b[i][k]) < 1e-6));
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

test("the monowheel draws through the game's web interpreter", () => {
  const wheel = compile(monowheelSource, "monowheel");
  const W = 1280, H = 720, program = new Float32Array(8192);
  // A camera 220 units off the object's right side, a little above, looking
  // at the axle — built as FightCamDoll.prepare builds one.
  const norm = (v) => v.map((x) => x / Math.hypot(...v));
  const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
  const eye = [0, -60, -220], forward = norm([0, -24 - eye[1], -eye[2]]);
  const right = norm(cross(forward, [0, -1, 0])), up = norm(cross(right, forward));
  program.set([FRAME_CAMERA, ...eye, ...right, ...up, ...forward,
    W / 2, H / 2, W / 230, W / (2 * Math.tan(55 * Math.PI / 360)), .82, 2.8 / 16000, -1.4, 8,
    -W * .5, W * 1.5, -H * .5, H * 1.5]);
  let length = 25;
  wheel({ distance: 40, speed: 300, lean: .2 }, upright, (...f) => {
    program.set([FRAME_WORLD, ...f], length);
    length += 13;
  });
  const drawn = [];
  const noOp = () => {};
  const vm = createFrameVm({ triangle3d: (...t) => drawn.push(t), box: noOp, line: noOp,
    wipe: noOp, write: noOp, systemWrite: noOp });
  vm.run(program, length, []);
  assert.ok(drawn.length >= (length - 25) / 13, "every face reaches the host");
  for (const t of drawn) {
    assert.ok(t.every(Number.isFinite));
    for (let v = 0; v < 9; v += 3) {
      assert.ok(t[v] > 0 && t[v] < W, `on screen x ${t[v]}`);
      assert.ok(t[v + 1] > 0 && t[v + 1] < H, `on screen y ${t[v + 1]}`);
    }
  }
});

// Reported, and bounded only loosely: this Mac is shared, and a timing gate
// that flakes under load teaches nobody anything.
test("a tick is closures, not a tree walk", (t) => {
  const wheel = compile(monowheelSource, "monowheel");
  const start = performance.now();
  for (let i = 0; i < 2000; i++) wheel({ distance: i, time: i / 60, speed: 200 }, upright, () => {});
  const us = (performance.now() - start) / 2000 * 1000;
  t.diagnostic(`monowheel: ${us.toFixed(1)} µs a tick`);
  assert.ok(us < 5000, `${us.toFixed(1)} µs a tick`);
});
