import test from 'node:test';
import assert from 'node:assert/strict';
import { move, spawn, slabOf, WORLD, CFG, FALL_Y, PLATFORM_DEPTH } from '../world.mjs';
import { Match } from '../match.mjs';

const walk = (p, cmd, fixed, n) => { for (let i = 0; i < n; i++) p = move(p, cmd, [], 0, fixed); return p; };

test('a platform is a volume: a body stands on its top, walks off its edge and falls, and its side is solid from below', () => {
  const slab = slabOf(WORLD), fixed = [slab];
  assert.equal(slab.yMax, 0); assert.equal(slab.yMin, -PLATFORM_DEPTH);
  let p = walk({ ...spawn(0), x: 0, z: 0, yaw: 0 }, { yaw: 0, pitch: 0, fwd: 0, right: 0, buttons: 0 }, fixed, 30);
  assert.ok(p.onGround && Math.abs(p.y - CFG.eyeHeight) < 1e-6, 'stands on the top');
  p = walk(p, { yaw: 0, pitch: 0, fwd: 1, right: 0, buttons: 0 }, fixed, 400);
  assert.ok(Math.hypot(p.x, p.z) > WORLD, 'past the edge, nothing held it');
  assert.ok(p.y < FALL_Y, 'and it fell');
  // In a walled room the same walk stops at the wall, on the ground.
  const held = walk({ ...spawn(0), x: 0, z: 0, yaw: 0 }, { yaw: 0, pitch: 0, fwd: 1, right: 0, buttons: 0 }, [], 400);
  assert.ok(Math.max(Math.abs(held.x), Math.abs(held.z)) <= WORLD && held.onGround);
  // Beside the block and below its top, a push inward is stopped by its side.
  let low = { ...spawn(0), x: WORLD + 1, z: 0, y: CFG.eyeHeight - 1, vy: 0, onGround: false, yaw: -90 };
  for (const yaw of [90, -90]) { const q = move({ ...low, yaw }, { yaw, pitch: 0, fwd: 1, right: 0, buttons: 0 }, [], 0, fixed); assert.ok(q.x >= WORLD, 'not inside the block'); }
});

test('a body fallen off the roof hits the street, comes apart there, and stands on the roof again', () => {
  const m = new Match();
  m.setWalls([slabOf(WORLD)]);
  const seat = m.join();
  const s = m.seats[seat];
  s.player = { ...s.player, x: WORLD + 3, y: FALL_Y - 1, onGround: false };
  m.step();
  assert.equal(m.seats[seat].player.alive, false);
  assert.ok(m.events.some((e) => e.type === 'gib' && e.seat === seat));
  assert.deepEqual(m.deaths, [{ seat, attacker: -1, fell: true }]);
  for (let k = 0; k < 60 * 3; k++) m.step();
  assert.ok(m.seats[seat].player.alive && m.seats[seat].player.y > 0, 'back on top');
  assert.equal(m.deaths.length, 1, 'one death for one fall');
});

import { createPack } from '../hud.mjs';
test('the small lines beside the face cam pack in a region of their own: three to a column, the rest in the next, none on another', () => {
  const side = createPack(4096, 28, { safeX: 0, safeY: 0, gap: 2, reach: 1 });
  const widths = [60, 40, 12, 110, 30];
  const got = widths.map((w, i) => side.reserve(`line-${i}`, 'tl', w, 8));
  assert.deepEqual(side.overlaps(), []);
  assert.deepEqual(got.slice(0, 3).map((r) => [r.x, r.y]), [[0, 0], [0, 10], [0, 20]]);
  for (const r of got) assert.ok(r.y >= 0 && r.y + r.h <= 28, 'inside the cam\'s height');
  assert.ok(got[3].x > 60 && got[3].y === 0, 'the fourth starts the next column, clear of the first');
  // The screen's own packing still keeps to its half.
  const screen = createPack(400, 300), tall = Array.from({ length: 5 }, (_, i) => screen.reserve(`t${i}`, 'tr', 50, 60));
  assert.ok(tall.every((r) => r.y + r.h <= 300 * 0.65));
});

import { roofBoxes, ESCAPE_DROP, ESCAPE_WIDE } from '../world.mjs';
test('the fire escape: stepped off the roof a body lands on the landing and climbs the steps back; run off, it clears the rail and falls', () => {
  const fixed = roofBoxes(WORLD), go = (p, cmd, n) => { for (let i = 0; i < n; i++) p = move(p, cmd, [], 0, fixed); return p; };
  const east = { yaw: 90, pitch: 0, right: 0, buttons: 0 };
  // Stepped off, slowly, a little north of the steps.
  let p = go({ ...spawn(0), x: WORLD - 0.5, z: -6, yaw: 90 }, { ...east, fwd: 0.25 }, 240);
  assert.ok(p.onGround && Math.abs(p.y - CFG.eyeHeight + ESCAPE_DROP) < 0.01, `on the landing: y ${p.y}`);
  assert.ok(p.x > WORLD && p.x < WORLD + ESCAPE_WIDE, 'held by the rail');
  // Along the landing to the foot of the steps and up them: back level with the roof.
  p = { ...p, x: WORLD + 0.65, yaw: 0 };
  for (let i = 0; i < 400 && p.z < 4; i++) p = move(p, { yaw: 0, pitch: 0, right: 0, buttons: 0, fwd: 0.6 }, [], 0, fixed);
  assert.ok(Math.abs(p.y - CFG.eyeHeight) < 0.05, `at the top of the steps, level with the roof: y ${p.y} at z ${p.z}`);
  // …and off the top step on to the roof itself.
  p = go({ ...p, yaw: 270 }, { yaw: 270, pitch: 0, right: 0, buttons: 0, fwd: 0.6 }, 60);
  assert.ok(p.x < WORLD && p.onGround && Math.abs(p.y - CFG.eyeHeight) < 0.01, `back on the roof: x ${p.x} y ${p.y}`);
  // Run off: over the rail and down.
  p = go({ ...spawn(0), x: 0, z: -6, yaw: 90 }, { ...east, fwd: 1 }, 600);
  assert.ok(p.y < FALL_Y, 'the street');
});

import { inOffice, OFFICE_DOOR } from '../world.mjs';
test('the office under the roof: from the landing a body walks in at the door and stands on its floor under the roof; beside the door the wall stops it', () => {
  const fixed = roofBoxes(WORLD), north = { yaw: 0, pitch: 0, right: 0, buttons: 0, fwd: 0.6 };
  const on = { ...spawn(0), y: CFG.eyeHeight - ESCAPE_DROP, onGround: true, yaw: 0, z: -WORLD - 1.2 };
  let p = { ...on, x: 0 };
  for (let i = 0; i < 240; i++) p = move(p, north, [], 0, fixed);
  assert.ok(inOffice(p, WORLD), `inside: z ${p.z}`);
  assert.ok(p.onGround && Math.abs(p.y - (CFG.eyeHeight - ESCAPE_DROP)) < 0.01, `on the office floor: y ${p.y}`);
  assert.ok(p.z < WORLD, 'and stopped by the far wall');
  let q = { ...on, x: OFFICE_DOOR + 2 };
  for (let i = 0; i < 240; i++) q = move(q, north, [], 0, fixed);
  assert.ok(q.z < -WORLD + 0.2, `the wall beside the door holds: z ${q.z}`);
  // On the roof nothing of the office is felt.
  let r = { ...spawn(0), x: 0, z: -7, yaw: 0 };
  for (let i = 0; i < 120; i++) r = move(r, north, [], 0, fixed);
  assert.ok(r.z > -5 && Math.abs(r.y - CFG.eyeHeight) < 0.01);
});
