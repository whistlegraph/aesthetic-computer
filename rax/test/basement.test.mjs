import test from 'node:test';
import assert from 'node:assert/strict';
import { BASEMENT, DEPTH, STAIR, cellarWalls, ceiling, stairPassage, treads } from '../basement.mjs';
import { move, spawn } from '../world.mjs';
import { altitude, floorAt } from '../elevator.mjs';

test('a walker descends from the plaza, crosses the cellar and can climb back without jumping', () => {
  const walls = [...cellarWalls, ...treads, ...ceiling];
  let body = { ...spawn(0), x: (STAIR.left + STAIR.right) / 2, z: STAIR.lip + 0.1 };
  const entry = stairPassage('lobby', body);
  assert.equal(entry.room, BASEMENT);
  assert.ok(Math.abs(entry.y + altitude(-1) - body.y) < 1e-9);
  body.y = entry.y;
  const walk = fwd => { body = move(body, { fwd, yaw: 0, pitch: 0, buttons: 0 }, [], 0, walls); };
  for (let i = 0; i < 55; i++) walk(1);
  assert.equal(body.y, 1.7);
  assert.ok(body.z > STAIR.end && body.onGround);
  let exit;
  for (let i = 0; i < 65 && !exit; i++) {
    walk(-1);
    exit = stairPassage(BASEMENT, body);
  }
  assert.equal(exit?.room, 'lobby');
  assert.equal(exit.y, body.y - DEPTH);
  assert.equal(floorAt(altitude(-1)), -1);
});

test('walking elsewhere in the hall and jumping below the ceiling stay inside the room', () => {
  const walls = [...cellarWalls, ...treads, ...ceiling];
  let body = spawn(0), highest = 0;
  assert.equal(stairPassage('lobby', body), null);
  for (let i = 0; i < 60; i++) {
    body = move(body, { fwd: 0, yaw: 0, buttons: Number(i === 0) }, [], 0, walls);
    highest = Math.max(highest, body.y);
  }
  assert.ok(highest > 2.5 && highest < DEPTH - 0.15);
  assert.equal(body.y, 1.7);
});
