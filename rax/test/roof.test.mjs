import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { Tower, carTo, floorRoom, GROUND, ROOF, aloft, roofNumber, roofOf, altitude, floorAt, listFloors, rideSeconds, carSolids, carWalls, roofSolids, roofWalls, standsOn, inCar,
  PARAPET, CAR, CAR_WALL, CAR_Z, SLAB, ATRIUM, STOREY, SHORT } from '../tower.mjs';
import { crossings, describeCrossing } from '../solids.mjs';
import { Geography } from '../geography.mjs';
import { place } from '../map.mjs';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL, CFG, move, spawn, halfOf } from '../world.mjs';

const tower = () => {
  const t = new Tower();
  t.admit(['jeffrey', 'oskie', 'prutti', 'max']);
  t.arrive('max');
  t.take('max', 89);
  listFloors(t.listed()); // (as the ledger does on every change)
  return t;
};

test('the roof is a room over the highest floor, and its number is its height', () => {
  const t = tower();
  assert.ok(aloft(ROOF) && aloft('floor-0123abcd') && !aloft(GROUND) && !aloft('e1n0'));
  assert.equal(halfOf(ROOF), halfOf(GROUND), 'the same 16 m footprint');
  assert.equal(roofNumber(0), 0, 'no floors, no roof');
  assert.equal(roofNumber(t.top), 90);
  assert.equal(altitude(roofNumber(t.top)), roofOf(t.top), 'a ride to the roof is counted like any other');
  assert.equal(roofOf(t.top) - altitude(t.top), STOREY, 'one storey over the highest floor');
  if (SHORT) assert.equal(roofOf(t.top), 33);
  assert.equal(floorAt(roofOf(t.top) + 0.5, [roofNumber(t.top), 0]), 90, 'the car arriving reads the roof');
  for (const listed of [[1, 2], [5, 9], [4000], [1, 4000]]) {
    listFloors(listed);
    const top = listed.at(-1);
    assert.equal(altitude(roofNumber(top)), roofOf(top), `listed ${listed}`);
  }
  listFloors([]);
});

test('the car goes where it is asked: top is the roof, and the roof is one up from the highest floor', () => {
  const t = tower(), room = (h) => floorRoom(t.forHandle(h).id), roof = { room: ROOF, number: 90, handle: null };
  for (const word of ['top', 'roof', 'the top', 'the roof', 'penthouse', ' TOP '])
    assert.deepEqual(carTo(t, GROUND, word), { ...roof, from: 0 }, word);
  assert.deepEqual(carTo(t, room('max'), 'up'), { ...roof, from: 89 }, 'up from the highest floor');
  assert.deepEqual(carTo(t, ROOF, 'down'), { room: room('max'), number: 89, handle: 'max', from: 90 }, 'down from the roof');
  assert.deepEqual(carTo(t, ROOF, 'up'), { error: 'this is the top' });
  assert.deepEqual(carTo(t, ROOF, 'top'), { error: 'you are here' });
  assert.deepEqual(carTo(t, ROOF, 'roof'), { error: 'you are here' });
  assert.deepEqual(carTo(t, ROOF, '2'), { room: room('oskie'), number: 2, handle: 'oskie', from: 90 }, 'a number from the roof');
  assert.deepEqual(carTo(t, ROOF, '@jeffrey'), { room: room('jeffrey'), number: 1, handle: 'jeffrey', from: 90 }, 'a handle from the roof');
  assert.deepEqual(carTo(t, ROOF, 'g'), { room: GROUND, number: 0, handle: null, from: 90 });
  assert.deepEqual(carTo(t, ROOF, 'home', 'oskie'), { room: room('oskie'), number: 2, handle: 'oskie', from: 90 });
  assert.deepEqual(carTo(t, ROOF, 'home'), { error: 'sign in for a floor' });
  // What it did before the roof, it still does.
  assert.deepEqual(carTo(t, GROUND, 'up'), { room: room('jeffrey'), number: 1, handle: 'jeffrey', from: 0 });
  assert.deepEqual(carTo(t, room('jeffrey'), 'down'), { room: GROUND, number: 0, handle: null, from: 1 });
  assert.deepEqual(carTo(t, room('prutti'), 'up'), { room: room('max'), number: 89, handle: 'max', from: 3 }, 'up skips the gaps');
  assert.deepEqual(carTo(t, GROUND, 'down'), { room: 'basement', number: -1, handle: null, from: 0 });
  assert.deepEqual(carTo(t, GROUND, 'lobby'), { error: 'you are here' });
  assert.deepEqual(carTo(t, GROUND, 'nobody'), { error: 'no floor nobody' });
  assert.deepEqual(carTo(t, GROUND, 'my floor', 'prutti'), { room: room('prutti'), number: 3, handle: 'prutti', from: 0 });
  assert.deepEqual(carTo(t, GROUND, '90'), { error: 'no floor 90' }, 'the roof is not a floor to be asked for by number');
  // No floors yet: no roof to ride to.
  assert.deepEqual(carTo(new Tower(), GROUND, 'top'), { error: 'no floor top' });
  assert.deepEqual(carTo(new Tower(), GROUND, 'up'), { error: 'this is the top' });
  // The ride's length is the difference of the two heights.
  const up = carTo(t, GROUND, 'top');
  assert.equal(altitude(up.number) - altitude(up.from), roofOf(t.top));
  assert.ok(rideSeconds(roofOf(t.top)) > rideSeconds(altitude(1)));
  listFloors([]);
});

test('the parapet shares no volume with the car, and stands round the whole edge but the shaft', () => {
  const found = crossings([...carSolids(), ...roofSolids()]);
  assert.deepEqual(found.map(describeCrossing), []);
  const walls = roofWalls(), o = CAR + CAR_WALL;
  assert.ok(walls.every((w) => w.yMin === 0 && w.yMax === PARAPET.height));
  assert.ok(walls.every((w) => Math.max(Math.abs(w.xMin), Math.abs(w.xMax), Math.abs(w.zMin), Math.abs(w.zMax)) <= SLAB), 'on the slab');
  // Every point of the slab's rim is behind a wall, but for the car's own width on the north side.
  const rim = PARAPET.at, covered = (x, z) => walls.some((w) => x >= w.xMin - 1e-9 && x <= w.xMax + 1e-9 && z >= w.zMin - 1e-9 && z <= w.zMax + 1e-9);
  for (let s = -rim; s <= rim; s += 0.05) {
    assert.ok(covered(s, -rim), `south at ${s}`);
    assert.ok(covered(-rim, s), `west at ${s}`);
    assert.ok(covered(rim, s), `east at ${s}`);
    assert.equal(covered(s, rim), Math.abs(s) >= o - 1e-9, `north at ${s}`);
  }
  // Nothing of it stands in the car, in its door, or on the walk to it.
  for (const w of walls) assert.ok(w.xMin >= o - 1e-9 || w.xMax <= -o + 1e-9 || w.zMax <= CAR_Z - o || w.zMin >= CAR_Z + o, JSON.stringify(w));
});

test('nobody walks off the roof, and the car is walked into from it', () => {
  const walls = [...carWalls(), ...roofWalls()], half = halfOf(ROOF);
  for (let yaw = 0; yaw < 360; yaw += 15) {
    let p = { ...spawn(0, 1, half), x: 0, z: -2, yaw, pitch: 0 };
    for (let i = 0; i < 600; i++) p = move(p, { fwd: 1, right: 0, yaw, pitch: 0, buttons: 0 }, [], i / 60, walls, false, [], half);
    assert.ok(Math.abs(p.x) < PARAPET.at - PARAPET.wide / 2 && Math.abs(p.z) < PARAPET.at - PARAPET.wide / 2, `yaw ${yaw}: at ${p.x.toFixed(2)}, ${p.z.toFixed(2)}`);
    assert.ok(Math.abs(p.y - CFG.eyeHeight) < 0.01, 'on the deck');
  }
  let p = { ...spawn(0, 1, half), x: 0, z: -5, yaw: 0, pitch: 0 };
  for (let i = 0; i < 300; i++) p = move(p, { fwd: 1, right: 0, yaw: 0, pitch: 0, buttons: 0 }, [], i / 60, walls, false, [], half);
  assert.ok(inCar(p), `walked in to ${p.x.toFixed(2)}, ${p.z.toFixed(2)}`);
});

test('nobody stands on air: whoever is seen on the tower has a slab under them, or the car', () => {
  listFloors([1, 4000]);
  // (The one seen in production, 2026-11-20: on floor 4, a number that shares the second storey, standing in the car.)
  const four = altitude(4);
  assert.equal(four, SHORT ? 19 : altitude(4));
  assert.deepEqual(standsOn([{ x: 0.4, y: four, z: CAR_Z }]), { slabs: [SHORT ? 2 : 4], cars: [SHORT ? 2 : 4] }, 'in the shaft: the car is their floor');
  assert.deepEqual(standsOn([{ x: -5, y: four, z: -3 }]), { slabs: [SHORT ? 2 : 4], cars: [] }, 'on a shared storey as on a listed one');
  assert.deepEqual(standsOn([{ x: 3, y: altitude(1) + 1.2, z: 2 }]).slabs, [1], 'a jump is not the next storey');
  assert.deepEqual(standsOn([{ x: 3, y: altitude(1) - 0.004, z: 2 }]).slabs, [1], 'a height told rounded is still its storey');
  // On the ground there is no slab to draw; in the car there, the car.
  assert.deepEqual(standsOn([{ x: 2, y: 0, z: -4 }]), { slabs: [], cars: [] });
  assert.deepEqual(standsOn([{ x: 0, y: 0, z: CAR_Z + 1 }]), { slabs: [], cars: [0] });
  // The roof is the storey over the last.
  assert.deepEqual(standsOn([{ x: 0, y: roofOf(4000), z: 0 }, { x: 1, y: roofOf(4000), z: CAR_Z }]), { slabs: [SHORT ? 4 : 4001], cars: [SHORT ? 4 : 4001] });
  // Off the tower (a block away, or in the air beside it) is no storey's business.
  assert.deepEqual(standsOn([{ x: 16, y: 0, z: 0 }, { x: SLAB + 0.5, y: 30, z: 0 }, null]), { slabs: [], cars: [] });
  // Many at once: each storey once, lowest first.
  assert.deepEqual(standsOn([{ x: 0, y: 26, z: 0 }, { x: 1, y: ATRIUM, z: 1 }, { x: 2, y: 26, z: 2 }, { x: 0, y: 19, z: CAR_Z }]),
    { slabs: [1, 2, 3], cars: [2] });
  listFloors([]);
});

test('someone on the roof is on the map at the roof\'s height', () => {
  const t = tower(), geography = new Geography();
  assert.deepEqual(place(ROOF, { x: 2, y: CFG.eyeHeight, z: -3 }, { geography, tower: t }), { x: 2, y: roofOf(89) + CFG.eyeHeight, z: -3, floor: 90, roof: true });
  assert.equal(place(ROOF, { x: 0, y: 0, z: 0 }, { geography, tower: new Tower() }), null, 'no tower, no roof');
  listFloors([]);
});

test('the elevator rides to the roof and back; the roof is kept as it is', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-roof-'));
  const app = createRaxServer({ stateDir: dir, verifyIdentity: async () => ({ handle: 'rider', sub: 'acct-rider' }),
    listHandles: async () => ['jeffrey', 'oskie', 'rider'] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const port = app.server.address().port;
  const open = async (room, extra = '') => {
    const ws = new WebSocket(`ws://127.0.0.1:${port}/play?room=${room}&protocol=${PROTOCOL}&account=1&stay=1${extra}`);
    ws.got = [];
    ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type !== 'snapshot') ws.got.push(d); });
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity' }));
    ws.send(JSON.stringify({ type: 'account', accessToken: 'valid-test-token' }));
    return ws;
  };
  const until = async (ws, f) => {
    for (let i = 0; i < 300; i++) {
      const k = ws.got.findIndex(f);
      if (k >= 0) return ws.got.splice(k, 1)[0];
      await delay(10);
    }
    throw Error('timeout: ' + JSON.stringify(ws.got.map((d) => d.type)));
  };
  const intoCar = (room) => {
    const r = app.rooms.get(room), seat = [...r.sessions.values()].find((m) => m.handle === 'rider').seat;
    Object.assign(r.match.seats[seat].player, { x: 0.5, z: CAR_Z });
  };
  try {
    // Before the tower has a floor there is no roof to stand on. (This tower has three by now.)
    const ws = await open('lobby');
    await until(ws, (d) => d.type === 'tower' && d.yours);
    await until(ws, (d) => d.type === 'identity'); // Elevator commands require the visitor's owner token.
    const top = app.tower.tower.top, roof = roofOf(top);
    assert.equal(top, 3);
    intoCar('lobby');
    ws.send(JSON.stringify({ type: 'elevator', to: 'top' }));
    const trip = await until(ws, (d) => d.type === 'travel');
    assert.equal(trip.room, ROOF);
    assert.equal(trip.here, true);
    assert.deepEqual(trip.ride, { from: 0, to: 4, handle: null });
    assert.deepEqual(trip.at, { x: 0.5, z: CAR_Z }, 'the rider stays where they stand in the car');
    const here = await until(ws, (d) => d.type === 'tower' && d.here?.room === ROOF);
    assert.deepEqual(here.here, { room: ROOF, number: 4, roof: true });
    assert.equal(here.owner, false);
    await until(ws, (d) => d.type === 'arrival');
    assert.ok(app.rooms.get(ROOF).match.walls.length >= carWalls().length + roofWalls().length, 'the car and the parapet are solid there');
    // On the map they stand at the roof's height.
    const people = await (await fetch(`http://127.0.0.1:${port}/people`)).json();
    assert.deepEqual(people.people.map((p) => [p.handle, p.y, p.floor, p.roof]), [['rider', roof + CFG.eyeHeight, 4, true]]); // (a body's y is its eye)
    // It is nobody's floor: nothing is built or painted there.
    ws.send(JSON.stringify({ type: 'paint', i: 1, j: 1, c: 2 }));
    assert.match((await until(ws, (d) => d.type === 'build-result')).error, /The roof is kept as it is/);
    ws.send(JSON.stringify({ type: 'prompt', text: 'a chair' }));
    assert.match((await until(ws, (d) => d.type === 'build-result')).error, /The roof is kept as it is/);
    // Up from the roof there is nothing; top is here.
    ws.send(JSON.stringify({ type: 'elevator', to: 'up' }));
    assert.match((await until(ws, (d) => d.type === 'elevator-result')).error, /this is the top/);
    ws.send(JSON.stringify({ type: 'elevator', to: 'roof' }));
    assert.match((await until(ws, (d) => d.type === 'elevator-result')).error, /you are here/);
    // Down from the roof is the highest floor; up from there is the roof again.
    ws.send(JSON.stringify({ type: 'elevator', to: 'down' }));
    const down = await until(ws, (d) => d.type === 'travel');
    assert.equal(down.room, floorRoom(app.tower.tower.at(3).id));
    assert.equal(down.ride.from, 4);
    assert.equal(down.ride.to, 3);
    await until(ws, (d) => d.type === 'arrival');
    ws.send(JSON.stringify({ type: 'elevator', to: 'up' }));
    assert.equal((await until(ws, (d) => d.type === 'travel')).room, ROOF);
    await until(ws, (d) => d.type === 'arrival');
    ws.close();
    // 🌍 Coming back, the handle is told it last stood on the roof.
    await delay(150);
    const back = await open('lobby');
    assert.equal((await until(back, (d) => d.type === 'whereabouts')).room, ROOF);
    back.close();
    // From the roof the plaza's blocks are asked for, and the roof is among the floors near its own height.
    const world = await (await fetch(`http://127.0.0.1:${port}/world?room=lobby&y=${roof}`)).json();
    assert.deepEqual(world.floors.find((f) => f.id === ROOF), { id: ROOF, number: 4, y: roof, objects: [], tiles: {} });
  } finally {
    await app.close();
  }
});

test('with no floors in the tower the roof is not a place', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-roof-none-'));
  const app = createRaxServer({ stateDir: dir, listHandles: async () => [] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  try {
    const bad = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?room=roof&protocol=${PROTOCOL}`);
    const [code] = await once(bad, 'close');
    assert.equal(code, 1008);
  } finally {
    await app.close();
  }
});
