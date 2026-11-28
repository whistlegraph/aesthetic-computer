// 🧊 The lattice's arithmetic (lattice.mjs, GEOMETRY.md): names, the six
// faces, levels and metres.
import test from 'node:test';
import assert from 'node:assert/strict';
import { placeFor, HOME_ROOM } from '../world.mjs';
import { EDGE, PER, ORIGIN, cube, same, isCube, word, full, fromWord, key, fromKey, through, neighbours, OPPOSITE, FACES, outside, seat, inside, insides, edge, middle, at, gone, carried } from '../lattice.mjs';

test('a cube is written and read back, as a word and as a room key', () => {
  for (const c of [ORIGIN, cube(1, 0, -2), cube(-40, 7, 999999), cube(3, -3, 0, 2), cube(0, 0, 0, -5)]) {
    assert.ok(same(fromWord(word(c)), c), word(c));
    assert.ok(same(fromWord(full(c)), c), full(c));
    assert.ok(same(fromKey(key(c)), c), key(c));
    assert.match(key(c), /^[a-z0-9-]{1,32}$/, 'a key is a room key');
  }
  assert.equal(word(ORIGIN), '0,0,0');
  assert.equal(word(cube(1, 0, -2, 2)), '2:1,0,-2');
  assert.equal(full(ORIGIN), '0:0,0,0');
  assert.equal(key(cube(1, 0, -2)), 'c0x1y0zm2');
  assert.ok(same(fromWord(' 1 , 0 , -2 '), cube(1, 0, -2)), 'spaces are let by');
  for (const not of ['', '1,2', '1,2,3,4', 'a,b,c', '1.5,0,0', '0:0:0,0,0', '1,0,9999999', 'studio']) assert.equal(fromWord(not), null, not);
  for (const not of ['', 'studio', 'c0x0y0', 'c0xm0y0z0', 'c0x01y0z0', 'c0x1y0z-2', 'e1n0']) assert.equal(fromKey(not), null, not);
  assert.equal(isCube(cube(0.5, 0, 0)), false);
});

test('each of the six faces leads to one cube, and back', () => {
  const c = cube(2, -1, 5, 1);
  const round = neighbours(c);
  assert.deepEqual(Object.keys(round).sort(), Object.keys(FACES).sort());
  assert.ok(same(round.east, cube(3, -1, 5, 1)));
  assert.ok(same(round.up, cube(2, 0, 5, 1)));
  assert.ok(same(round.south, cube(2, -1, 4, 1)));
  for (const face of Object.keys(FACES)) assert.ok(same(through(through(c, face), OPPOSITE[face]), c), face);
  assert.equal(new Set(Object.values(round).map(key)).size, 6, 'six different cubes');
  assert.equal(through(c, 'sideways'), null);
});

test('a cube is in one cube of the next level, and holds PER cubed of the one under', () => {
  const c = cube(5, -1, 0);
  assert.ok(same(outside(c), cube(1, -1, 0, 1)), 'floored, so minus one is in minus one');
  assert.deepEqual(seat(c), { i: 1, j: 3, k: 0 });
  const held = [...insides(cube(1, -1, 0, 1))];
  assert.equal(held.length, PER ** 3);
  assert.equal(new Set(held.map(key)).size, PER ** 3);
  assert.ok(held.some((d) => same(d, c)), 'and it is one of them');
  for (const d of held) assert.ok(same(outside(d), cube(1, -1, 0, 1)));
  // In and out again, at every seat.
  for (const d of insides(ORIGIN)) { const s = seat(d); assert.ok(same(inside(outside(d), s.i, s.j, s.k), d)); }
  assert.equal(inside(c, PER, 0, 0), null);
  assert.equal(inside(c, -1, 0, 0), null);
});

test('metres: a level is PER times the one under, and a point is in one cube of each', () => {
  assert.equal(edge(0), EDGE);
  assert.equal(edge(1), EDGE * PER);
  assert.equal(edge(-1), EDGE / PER);
  assert.deepEqual(middle(ORIGIN), { x: 0, y: 0, z: 0 });
  assert.deepEqual(middle(cube(1, 0, -1)), { x: EDGE, y: 0, z: -EDGE });
  // The level-1 cube 0,0,0 holds the level-0 cubes 0 … PER - 1: its middle is the middle of those.
  const big = cube(0, 0, 0, 1), mid = middle(big);
  const mean = [...insides(big)].map(middle).reduce((a, m) => ({ x: a.x + m.x / PER ** 3, y: a.y + m.y / PER ** 3, z: a.z + m.z / PER ** 3 }), { x: 0, y: 0, z: 0 });
  assert.deepEqual(mid, mean);
  for (const c of [ORIGIN, cube(3, -2, 1), cube(-1, 0, 0, 1), cube(2, 2, 2, -1)]) assert.ok(same(at(middle(c), c.level), c), full(c));
  assert.ok(same(at({ x: EDGE / 2 - 0.01, y: 0, z: 0 }), ORIGIN), 'just inside the east face');
  assert.ok(same(at({ x: EDGE / 2 + 0.01, y: 0, z: 0 }), cube(1, 0, 0)), 'just through it');
  assert.ok(same(at({ x: 0, y: -EDGE / 2 - 0.01, z: 0 }), cube(0, -1, 0)), 'fallen through the floor');
  // A point's cube at one level is in its cube at the next.
  const p = { x: 123.4, y: -77, z: 9 };
  assert.ok(same(outside(at(p, 0)), at(p, 1)));
});

test('a point that has left a cube has gone through a face, and is carried into the next cube\'s own metres', () => {
  const half = EDGE / 2;
  assert.equal(gone({ x: 0, y: 0, z: 0 }), null);
  assert.equal(gone({ x: half + 0.2, y: 0, z: 0 }), 'east');
  assert.equal(gone({ x: 0, y: -half - 3, z: 0 }), 'down');
  assert.equal(gone({ x: half + 0.2, y: 0, z: -half - 1 }), 'south', 'the face it is furthest through');
  const there = carried({ x: half + 0.2, y: 1, z: 2 }, 'east');
  assert.ok(Math.abs(there.x - (-half + 0.2)) < 1e-9 && there.y === 1 && there.z === 2, 'just inside the west face of the next');
  assert.equal(gone(there), null);
  assert.equal(carried({ x: 0, y: 0, z: 0 }, 'sideways'), null);
});

test('until cubes are rooms of their own, every cube asked for is home; a named room is still itself', () => {
  for (const c of [ORIGIN, cube(1, 0, -2), cube(0, -1, 0), cube(0, 0, 0, 2)]) assert.equal(placeFor(key(c), true), HOME_ROOM, full(c));
  assert.equal(placeFor('studio', true), 'studio');
  assert.equal(placeFor('plaza', true), 'lobby');
});
