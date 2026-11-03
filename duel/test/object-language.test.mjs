import test from 'node:test';
import assert from 'node:assert/strict';
import {
  parseSource,
  sourceFrame,
  OBJECT_FORMAT,
} from '../object-language.mjs';
import { geometry, objectBoxes } from '../objects.mjs';
import { partialGeometry } from '../build-progress.mjs';
import { move, spawn } from '../world.mjs';

const butterfly = `; Butterfly
(ink pink)
(move (sin time) 2 0
 (rotate z (* .6 (sin (* time 12))) (slab 0 0 -.3 1 .02 .3))
 (rotate z (* -.6 (sin (* time 12))) (slab -1 0 -.3 0 .02 .3))
 (ink black) (orb 0 0 0 .12 6))`;

test('shared compiler retains shapes, bounds their animation and preserves thin wings', () => {
  const g = parseSource(butterfly);
  assert.equal(g.format, OBJECT_FORMAT);
  assert.equal(g.animated, true);
  assert.equal(g.boxes.length, 3);
  const a = Array.from(sourceFrame(g, 0).vertices),
    b = Array.from(sourceFrame(g, 0.15).vertices);
  assert.notDeepEqual(a, b);
  assert.deepEqual(Array.from(sourceFrame(g, 0).vertices), a);
  assert.ok(a.length / 18 <= 512);
  const objects = [{ id: 'a', x: 3, z: 4, geometry: g }];
  assert.notDeepEqual(objectBoxes(objects, 0), objectBoxes(objects, 0.15));
  for (let time = 0; time < 10; time += 0.037) {
    const { vertices } = sourceFrame(g, time);
    for (let i = 0; i < vertices.length; i += 6) {
      assert.ok(vertices[i] >= -6 && vertices[i] <= 6);
      assert.ok(vertices[i + 1] >= 0 && vertices[i + 1] <= 10);
      assert.ok(vertices[i + 2] >= -6 && vertices[i + 2] <= 6);
      assert.ok(
        g.boxes.some(
          (b) =>
            Math.abs(vertices[i] - b.x) <= b.w / 2 + 1e-6 &&
            Math.abs(vertices[i + 1] - b.y) <= b.h / 2 + 1e-6 &&
            Math.abs(vertices[i + 2] - b.z) <= b.d / 2 + 1e-6,
        ),
      );
    }
  }
  assert.deepEqual(
    geometry({ ...g, boxes: [], animated: false }),
    g,
    'derive metadata from source, not client claims',
  );
});

test('repeated slabs retain separate stair collisions', () => {
  const g = parseSource(
    '; Stairs\n(ink gray) (repeat 6 i (move 0 0 (* i .5) (scale 1 (+ 1 i) 1 (slab -1 0 0 1 .25 .5))))',
  );
  assert.equal(g.animated, false);
  assert.equal(sourceFrame(g).boxes.length, 6);
  assert.deepEqual(
    sourceFrame(g).boxes.map((b) => b.h),
    [0.25, 0.5, 0.75, 1, 1.25, 1.5],
  );
  let player = { ...spawn(0), x: 0, z: -1 };
  let highest = player.y;
  for (let i = 0; i < 75; i++) {
    player = move(
      player,
      { fwd: 1, right: 0, yaw: 0, pitch: 0, buttons: 0 },
      [{ x: 0, z: 0, geometry: g }],
      i / 60,
    );
    highest = Math.max(highest, player.y);
  }
  assert.ok(highest > 3, 'player climbs compiled stairs');
});

test('admission rejects excessive work and unbounded motion before compiling', () => {
  for (const source of [
    '(repeat 100000 i (orb 0 1 0 .5))',
    '(repeat 16 i (repeat 16 j (orb 0 1 0 .5)))',
    '(move time 1 0 (orb 0 0 0 .1))',
    '(orb 0 1 0 (/ 1 0))',
    '(eval "arbitrary code")',
    '('.repeat(14) + 'orb 0 1 0 .1' + ')'.repeat(14),
    '(let a (sin time))' +
      Array.from(
        { length: 13 },
        (_, i) =>
          `(let a${i} (+ ${i ? 'a' + (i - 1) : 'a'} ${i ? 'a' + (i - 1) : 'a'}))`,
      ).join('') +
      '(move a12 1 0 (orb 0 0 0 .1))',
  ])
    assert.throws(() => parseSource(source), source);
});

test('a program that leaves its volume is fitted, not refused', () => {
  const g = parseSource('(move 0 1 0 (rotate z time (slab 0 0 0 3 .1 1)))');
  for (let t = 0; t < 7; t += 0.1)
    for (let v = sourceFrame(g, t).vertices, i = 1; i < v.length; i += 6)
      assert.ok(v[i] >= -0.001, 'lifted clear of the floor');
  const big = parseSource('; Giant\n(slab -20 0 -1 20 30 1)');
  assert.ok(big.boxes.every((b) => b.w <= 12 && b.y + b.h / 2 <= 10));
});

test('progress closes only complete forms through bounded admission', () => {
  const p = partialGeometry(
    '; Butterfly\n(ink pink)\n(move (sin time) 2 0 (slab 0 0 -.3 1 .02 .3)\n (rotate z',
  );
  assert.equal(p.boxes.length, 1);
  assert.equal(p.animated, true);
  assert.equal(partialGeometry('(repeat 999999 i (slab 0 0 0 1 1 1)'), null);
  assert.equal(partialGeometry('(slab 0 0'), null);
});

test('legacy versions still render and accept thin parts', () => {
  const g = geometry({
    name: 'Wing',
    boxes: [{ x: 0, y: 1, z: 0, w: 1, h: 0.02, d: 0.5 }],
  });
  assert.equal(g.boxes[0].h, 0.02);
  assert.equal(objectBoxes([{ x: 0, z: 0, geometry: g }]).length, 1);
});

test('the AC object viewer ships the same compiler as the game', async () => {
  const { readFile } = await import('node:fs/promises');
  const at = (p) => readFile(new URL(p, import.meta.url), 'utf8');
  assert.equal(
    await at('../../system/public/aesthetic.computer/lib/object/object-language.mjs'),
    await at('../object-language.mjs'),
    'copy duel/object-language.mjs into system/public/aesthetic.computer/lib/object/',
  );
  assert.equal(
    await at('../../system/public/aesthetic.computer/lib/object/object-compiler.mjs'),
    await at('../../xbox/live/object-lisp.mjs'),
    'copy xbox/live/object-lisp.mjs into system/public/aesthetic.computer/lib/object/object-compiler.mjs',
  );
});

test('objects can carry oscillators that follow time', async () => {
  const { sourceTones } = await import('../object-language.mjs');
  const g = parseSource('; Hum\n(orb 0 1 0 .5) (move 0 2 0 (tone (+ 220 (* 40 (sin time))) triangle .4))');
  const [t] = sourceTones(g);
  assert.equal(t.wave, 'triangle');
  assert.equal(t.volume, 0.4);
  assert.deepEqual(t.at, [0, 2, 0]);
  assert.equal(Math.round(t.frequency(Math.PI / 2)), 260);
  assert.throws(() => parseSource('(orb 0 1 0 .5) (tone 5000)'), /20..4000/);
  assert.throws(() => parseSource('(orb 0 1 0 .5) (tone 220 kazoo)'), /wave/);
});
