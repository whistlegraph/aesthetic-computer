import test from 'node:test';
import assert from 'node:assert/strict';
import { CFG, spawn, move } from '../world.mjs';
import { carWalls, lobbyWalls, roofWalls, storeyWindows, towerSides, SLAB } from '../elevator.mjs';

function advance(p, yaw, frames, walls, jump = false) {
  for (let i = 0; i < frames; i++)
    p = move(p, { yaw, pitch: 0, fwd: 1, right: 0, buttons: jump && i === 0 ? 1 : 0 }, [], i/60, walls, false, towerSides);
  return p;
}

test('every open tower face and roof edge holds a walker and clears a jumper', () => {
  for (const frame of [storeyWindows(), roofWalls()]) {
    const walls = [...carWalls(), ...frame];
    for (const [x,z,yaw] of [[6,2,90], [-6,2,-90], [2,-6,180], [3,6,0], [-3,6,0]]) {
      const p = advance({...spawn(0), x, z}, yaw, 90, walls);
      assert.ok(Math.max(Math.abs(p.x), Math.abs(p.z)) < SLAB);
      const jump = advance(p, yaw, 27, walls, true);
      assert.ok(Math.max(Math.abs(jump.x), Math.abs(jump.z)) > SLAB + 0.4, JSON.stringify({x,z,jump}));
      assert.ok(jump.y > CFG.eyeHeight);
    }
  }
});

test('open lobby windows admit a jump and the elevator glazing holds one', () => {
  const walls = [...carWalls(), ...lobbyWalls()];
  for (const [x,z,yaw] of [[4.85,3,180],[-4.85,3,180],[6,4.6,90],[-6,4.6,-90]]) {
    let p = advance({...spawn(0), x, z}, yaw, 60, walls);
    p = advance(p, yaw, 36, walls, true);
    assert.ok(yaw===180 ? p.z<0.8 : Math.abs(p.x)>8.3, JSON.stringify({x,z,p}));
  }
  const inside = advance({...spawn(0), x:0.6, z:6.1}, 0, 36, walls, true);
  assert.ok(inside.z < 7.6);
});
