import test from 'node:test';
import assert from 'node:assert/strict';
import { move, spawn, CFG } from '../world.mjs';
import { command } from '../net.mjs';
import { BIKE, rideable } from '../bike.mjs';
import { POOL } from '../basement.mjs';
import { lobbyWalls } from '../tower.mjs';
import { atriumRoof } from '../elevator.mjs';

const cmd = (over = {}) => ({ seq: 1, fwd: 0, right: 0, yaw: 0, pitch: 0, buttons: 0, ...over });
const run = (p, c, n, ...rest) => { for (let i = 0; i < n; i++) p = move(p, c, ...rest); return p; };
const settled = () => run(spawn(0), cmd(), 30);

test('the wire carries B, and nothing else turns it on', () => {
  assert.equal(command(cmd({ bike: true })).bike, true);
  assert.equal(command(cmd({ bike: 'yes' })).bike, false);
});

test('B gets on; the bike hovers, then goes where the eye looks', () => {
  let p = move(settled(), cmd({ bike: true }));
  assert.equal(p.bike, true);
  p = run(p, cmd(), 60);
  const hover = BIKE.hover + BIKE.seat + CFG.eyeHeight;
  assert.ok(Math.abs(p.y - hover) < 0.05, `hovers at ${p.y}`);
  const start = p;
  p = run(p, cmd({ fwd: 1, pitch: 30 }), 120); // two seconds, looking up and ahead
  assert.ok(p.y > start.y + 5, `climbed to ${p.y}`);
  assert.equal(p.bike, true);
  assert.equal(p.onGround, false);
});

test('space lifts, shift lets it sink, never under its hover', () => {
  let p = move(settled(), cmd({ bike: true }));
  p = run(p, cmd({ buttons: 1 }), 120);
  const high = p.y;
  assert.ok(high > 8, `lifted to ${high}`);
  p = run(p, cmd({ duck: true }), 600);
  assert.ok(Math.abs(p.y - (BIKE.hover + BIKE.seat + CFG.eyeHeight)) < 0.05);
});

test('B again in the air: off the bike, the body falls to the ground', () => {
  let p = move(settled(), cmd({ bike: true }));
  p = run(p, cmd({ buttons: 1 }), 90);
  p = move(p, cmd({ bike: true }));
  assert.equal(p.bike, false);
  p = run(p, cmd(), 240);
  assert.equal(p.onGround, true);
  assert.ok(Math.abs(p.y - CFG.eyeHeight) < 0.01);
});

test('no bikes in a side-on arena, the inner sea, or under a low roof', () => {
  const at = { x: 0, y: CFG.eyeHeight, z: 0 };
  assert.equal(rideable([], true, at), false);
  assert.equal(rideable([POOL], false, at), false);
  const low = { xMin: -4, xMax: 4, zMin: -4, zMax: 4, yMin: 3, yMax: 3.2, ceiling: true };
  assert.equal(rideable([low], false, at), false, 'a cellar\'s ceiling over the head');
  assert.equal(rideable([{ ...low, yMin: 12, yMax: 12.3 }], false, at), true, 'a twelve-metre hall is open enough');
  assert.equal(rideable([], false, at), true);
  assert.equal(move(settled(), cmd({ bike: true }), [], 0, [], true).bike || false, false);
});

test('the plaza: everyone starts there, and a bike flies there, held under the tower and out of the glass', () => {
  const walls = [...lobbyWalls(), atriumRoof()];
  const courtyard = { ...spawn(0), x: 0, y: CFG.eyeHeight, z: -5, onGround: true };
  assert.equal(rideable(walls, false, courtyard), true);
  let p = move(courtyard, cmd({ bike: true }), [], 0, walls);
  assert.equal(p.bike, true);
  const up = run(p, cmd({ buttons: 1 }), 600, [], 0, walls);
  assert.ok(up.y < atriumRoof().yMin, `held under the first floor (eyes at ${up.y.toFixed(2)})`);
  assert.equal(up.bike, true, 'still on the bike up against the roof (it does not throw the rider off)');
  // At the hall's glass front, flying north low (between the rails): stopped, not through.
  const front = run(p, cmd({ fwd: 1, yaw: 30 }), 240, [], 0, walls);
  assert.ok(front.z < 1.2 || Math.abs(front.x) < 1.6, `stopped at the glass unless at the door, at ${front.x.toFixed(2)}, ${front.z.toFixed(2)}`);
});

test('a bike drifted in close to a wall slides along it and gets away, never stuck', () => {
  const wall = { type: 'box', xMin: -8, xMax: 8, yMin: 0, yMax: 12, zMin: 1.1, zMax: 1.3 };
  // In the margin already (0.3 m off the glass), at height.
  let p = { ...spawn(0), x: 0, y: 6, z: 0.8, vx: 0, vy: 0, vz: 0, bike: true, onGround: false };
  const along = run(p, cmd({ fwd: 1, yaw: 270 }), 60, [], 0, [wall]); // west, along the glass
  assert.ok(along.x < -3, `slid along it to x ${along.x.toFixed(2)}`);
  assert.ok(along.z < 1.1 - 0.29, 'never through it');
  const away = run(p, cmd({ fwd: 1, yaw: 180 }), 60, [], 0, [wall]); // south, away from it
  assert.ok(away.z < -2, `got away to z ${away.z.toFixed(2)}`);
  const into = run(p, cmd({ fwd: 1, yaw: 0 }), 60, [], 0, [wall]); // north, into it
  assert.ok(into.z <= 0.8 + 1e-6, 'no further in');
});
