import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { Geography, BLOCK } from '../geography.mjs';
import { Tower, floorRoom, carWalls, lobbyWalls, altitude, ATRIUM, STOREY, SLAB, LEG } from '../tower.mjs';
import { WORLD } from '../world.mjs';
import { worldMap, towerShapes, place, MAP_VERSION } from '../map.mjs';
import { createRaxServer } from '../server.mjs';

const world = () => {
  const geography = new Geography(), tower = new Tower();
  for (const h of ['jeffrey', 'oskie', 'prutti']) geography.settle(h);
  tower.admit(['jeffrey', 'oskie', 'prutti', 'max']);
  return { geography, tower };
};
const holds = (s, w) => s.k === 'box' && s.x0 <= w.xMin + 1e-6 && s.x1 >= w.xMax - 1e-6
  && s.z0 <= w.zMin + 1e-6 && s.z1 >= w.zMax - 1e-6 && s.y0 <= w.yMin + 1e-6 && s.y1 >= w.yMax - 1e-6;

test('every solid the server walks people against is on the map', () => {
  const { geography, tower } = world();
  const map = worldMap({ geography, tower });
  assert.equal(map.version, MAP_VERSION);
  for (const w of [...lobbyWalls(), ...carWalls()])
    assert.ok(map.structure.some((s) => holds(s, w)), `missing ${JSON.stringify(w)}`);
});

test('the tower is as tall as its highest floor, a slab a storey', () => {
  const { geography, tower } = world();
  tower.arrive('max');
  tower.take('max', 89);
  const map = worldMap({ geography, tower });
  const floors = map.structure.find((s) => s.k === 'stack' && s.role === 'floor');
  assert.deepEqual([floors.y, floors.step, floors.count], [ATRIUM, STOREY, 90]); // 89 floors and the roof
  assert.equal(floors.x1, SLAB);
  assert.equal(map.tower.height, altitude(90));
  assert.deepEqual(map.tower, { floors: 4, top: 89, atrium: ATRIUM, storey: STOREY, height: altitude(90) });
  const legs = map.structure.filter((s) => s.role === 'leg');
  assert.equal(legs.length, 4);
  assert.ok(legs.every((s) => s.y1 === ATRIUM && Math.abs(Math.abs(s.x0 + s.x1) / 2 - LEG.at) < 1e-6));
  // The frame stands outside the room every floor is.
  assert.ok(LEG.at > WORLD && SLAB > LEG.at);
  assert.deepEqual(towerShapes(0), []); // no handles yet, no tower
});

test('the lattice: the plaza, the open blocks and the blocks that open next; no lots or houses', () => {
  const { geography, tower } = world();
  const map = worldMap({ geography, tower });
  const count = (role) => map.structure.filter((s) => s.role === role).length;
  const census = geography.census();
  assert.equal(count('plaza'), 1);
  assert.equal(count('block'), census.opened);
  assert.deepEqual([count('home'), count('lot'), count('house')], [0, 0, 0]);
  assert.equal(count('next'), 6);
  assert.equal(map.extent.x1 - map.extent.x0 >= BLOCK, true);
  assert.equal(map.census.settled, 3);
});

test('what stands in a block is drawn where it stands', () => {
  const { geography, tower } = world();
  const map = worldMap({ geography, tower, contents: (id) => (id === 'e1n0'
    ? { tiles: { '9,9': 2 }, objects: [{ x: 1, z: -2, code: 'cow' }, { x: 0, z: 0, item: 'held' }], screens: [{ x: 3, z: 4, yaw: 1.5, code: 'roz' }] }
    : {}) });
  const paint = map.content.find((s) => s.role === 'paint');
  assert.deepEqual([paint.x0, paint.z0, paint.x1, paint.z1, paint.color], [BLOCK, 0, BLOCK + 2, 2, 0xe0b23a]);
  const builds = map.content.filter((s) => s.role === 'build');
  assert.equal(builds.length, 1); // the carried one does not stand
  assert.equal(builds[0].label, '$cow');
  assert.equal((builds[0].x0 + builds[0].x1) / 2, BLOCK + 1);
  const screen = map.content.find((s) => s.role === 'screen');
  assert.deepEqual([screen.x, screen.z, screen.yaw], [BLOCK + 3, 4, 1.5]);
});

test('people stand where they are: the plaza, a block, a floor at its height', () => {
  const { geography, tower } = world();
  const f = tower.forHandle('oskie');
  const at = (room, p) => place(room, p, { geography, tower });
  assert.deepEqual(at('lobby', { x: 1, y: 0, z: 2 }), { x: 1, y: 0, z: 2, floor: 0 });
  assert.deepEqual(at('e1n0', { x: -3, y: 0, z: 2 }), { x: BLOCK - 3, y: 0, z: 2, floor: 0 });
  assert.deepEqual(at(floorRoom(f.id), { x: 1, y: 0.5, z: 2 }), { x: 1, y: altitude(f.number) + 0.5, z: 2, floor: f.number });
  assert.equal(at(geography.home('jeffrey').pod, { x: 0, y: 0, z: 0 }), null); // a house is no longer a place
  assert.equal(at('botbop', { x: 0, y: 0, z: 0 }), null);
  const map = worldMap({ geography, tower, seated: [
    { handle: 'Oskie', room: floorRoom(f.id), x: 0, y: 0, z: 0 }, { handle: 'x', room: 'studio', x: 0, y: 0, z: 0 }] });
  assert.deepEqual(map.people, [{ handle: 'oskie', x: 0, y: altitude(f.number), z: 0, floor: f.number }]);
});

test('GET /map serves the drawing', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-map-srv-'));
  const app = createRaxServer({ stateDir: dir, listHandles: async () => ['jeffrey', 'oskie'] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  try {
    // Floors are handed out just after the server is ready.
    let r, map;
    for (let i = 0; i < 100; i++) {
      r = await fetch(`http://127.0.0.1:${app.server.address().port}/map`);
      map = await r.clone().json();
      if (map.tower.top) break;
      await delay(10);
    }
    assert.equal(r.status, 200);
    assert.equal(r.headers.get('access-control-allow-origin'), '*');
    assert.equal(map.version, MAP_VERSION);
    assert.equal(map.tower.top, 2);
    assert.ok(map.structure.some((s) => s.role === 'plaza'));
    assert.ok(map.structure.some((s) => s.role === 'floor' && s.count === 3));
    assert.deepEqual(map.people, []);
  } finally {
    await app.close();
  }
});

test('with the houses retired the map has no lots, homes or houses: only the plaza, the open blocks and the tower', () => {
  const { geography, tower } = world();
  const gone = geography.retire();
  assert.equal(gone.length, 3);
  assert.ok(gone.every((p) => p.id.startsWith('pod-') && p.handle));
  const map = worldMap({ geography, tower });
  const count = (role) => map.structure.filter((s) => s.role === role).length;
  assert.deepEqual([count('home'), count('lot'), count('house')], [0, 0, 0]);
  assert.equal(count('block'), geography.census().opened);
  assert.equal(geography.retire().length, 0);
});
