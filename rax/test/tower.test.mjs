import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { Tower, TowerStore, floorRoom, isFloorRoom, carWalls, lobbyWalls, LOBBY_FRONT, inCar, CAR, CAR_Z, altitude, floorAt, SHORT, listFloors, storeyOf, roofOf, floorsNear } from '../tower.mjs';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL, move, spawn } from '../world.mjs';

test('every handle gets a floor, in the order given, and keeps it', () => {
  const t = new Tower();
  assert.equal(t.admit(['@Oskie', 'jeffrey', 'not a handle', 'prutti']), 3);
  assert.deepEqual(t.directory(), [[1, 'oskie', 0], [2, 'jeffrey', 0], [3, 'prutti', 0]]);
  assert.equal(t.admit(['jeffrey', 'max']), 1);
  assert.equal(t.forHandle('max').number, 4);
  assert.equal(t.count, 4);
  assert.ok(isFloorRoom(floorRoom(t.forHandle('max').id)));
  assert.equal(t.forRoom(floorRoom(t.forHandle('max').id)).handle, 'max');
});

test('a floor is found by number or by handle', () => {
  const t = new Tower();
  t.admit(['jeffrey', '89', 'oskie']);
  assert.equal(t.resolve('1').handle, 'jeffrey');
  assert.equal(t.resolve('#3').handle, 'oskie');
  assert.equal(t.resolve('@Oskie').number, 3);
  assert.equal(t.resolve('oskie').number, 3);
  assert.equal(t.resolve('@89').number, 2); // a handle of digits, by its @
  assert.equal(t.resolve('nobody'), null);
});

test('a number is taken like a name: free, traded with the absent, refused when held', () => {
  const t = new Tower();
  t.admit(['a', 'b', 'c']);
  const id = t.forHandle('c').id;
  t.arrive('c', 'acct-c');
  assert.equal(t.take('c', 89).number, 89); // free
  assert.equal(t.forHandle('c').id, id); // the room goes with the handle
  assert.equal(t.at(3), null);
  assert.equal(t.top, 89);
  assert.equal(t.count, 3);
  assert.equal(t.take('c', 1).number, 1); // a was only pencilled in: they trade
  assert.equal(t.forHandle('a').number, 89);
  t.arrive('b');
  assert.throws(() => t.take('c', 2), /@b's/); // b has arrived: it is b's
  assert.throws(() => t.take('c', 0), /Floors run/);
  assert.throws(() => t.take('c', 10000), /Floors run/);
  assert.throws(() => t.take('nobody', 5), /Sign in/);
  assert.equal(t.lowestFree(), 3);
  t.admit(['d']);
  assert.equal(t.forHandle('d').number, 3); // new handles fill the gaps
});

test('up and down skip the gaps', () => {
  const t = new Tower();
  t.admit(['a', 'b']);
  t.take('b', 40);
  assert.equal(t.step(0, 1).handle, 'a');
  assert.equal(t.step(1, 1).handle, 'b');
  assert.equal(t.step(40, 1), null);
  assert.equal(t.step(40, -1).handle, 'a');
  assert.equal(t.step(1, -1), null);
});

test('a renamed account keeps its floor; only the owner arranges it', () => {
  const t = new Tower();
  t.admit(['old', 'new']);
  const f = t.arrive('old', 'acct');
  assert.ok(t.owns(f, { handle: 'old', accountId: 'acct' }));
  assert.ok(!t.owns(f, { handle: 'old', accountId: 'other' }));
  assert.ok(!t.owns(f, { handle: null }));
  const g = t.arrive('new', 'acct');
  assert.equal(g.id, f.id);
  assert.equal(g.number, 1);
  assert.equal(t.forHandle('old'), null);
  assert.equal(t.count, 1);
});

test('the ledger survives a restart', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-tower-'));
  const a = new TowerStore(dir);
  await a.load();
  await a.change((t) => t.admit(['jeffrey', 'oskie']));
  await a.change((t) => { t.arrive('oskie'); return t.take('oskie', 89); });
  const b = new TowerStore(dir);
  await b.load();
  assert.equal(b.tower.forHandle('oskie').number, 89);
  assert.equal(b.tower.forHandle('jeffrey').number, 1);
});

test('the car is walked into through its door and nowhere else', () => {
  const walls = carWalls();
  let p = { ...spawn(0), x: 0, z: 0, yaw: 0, pitch: 0 };
  for (let i = 0; i < 240; i++) p = move(p, { fwd: 1, right: 0, yaw: 0, pitch: 0 }, [], i / 60, walls);
  assert.ok(inCar(p), `walked in to ${p.x},${p.z}`);
  assert.ok(p.z < CAR_Z + CAR, 'stopped by the back wall');
  let q = { ...spawn(0), x: -6, z: CAR_Z, yaw: 0, pitch: 0 };
  for (let i = 0; i < 240; i++) q = move(q, { fwd: 0, right: 1, yaw: 0, pitch: 0 }, [], i / 60, walls);
  assert.ok(!inCar(q), `stopped outside at ${q.x},${q.z}`);
});

test('the lobby is entered by its door', () => {
  const walls = [...carWalls(), ...lobbyWalls()];
  const walk = (x) => {
    let p = { ...spawn(0), x, z: -3, yaw: 0, pitch: 0 };
    for (let i = 0; i < 120; i++) p = move(p, { fwd: 1, right: 0, yaw: 0, pitch: 0 }, [], i / 60, walls);
    return p;
  };
  assert.ok(walk(0).z > LOBBY_FRONT + 1, 'through the entrance');
  assert.ok(walk(5).z < LOBBY_FRONT, 'not through the glass');
});

// (2026-11-19: the tower is short for now, so a floor's height is its storey's,
// not its number's. The full tower's heights are still checked, for the day
// SHORT is put back to false.)
test('a floor is a height', () => {
  assert.equal(altitude(0), 0);
  if (!SHORT) {
    assert.equal(altitude(1), 12);
    assert.equal(altitude(89), 628);
    assert.deepEqual([0, 11.9, 12, 18.9, 19, 628, 634.9].map((y) => floorAt(y)), [0, 0, 1, 1, 2, 89, 89]);
    return;
  }
  // @jeffrey on 1 and the highest floor, 4000, have a storey each; every
  // number between shares the one between them; the roof lies on the top.
  listFloors([4000, 1]);
  assert.deepEqual([1, 2, 89, 3999, 4000].map(storeyOf), [1, 2, 2, 2, 3]);
  assert.deepEqual([1, 89, 4000].map(altitude), [12, 19, 26]);
  assert.equal(roofOf(4000), 33);
  assert.deepEqual([0, 11.9, 12, 18.9, 19, 26, 32.9].map((y) => floorAt(y)), [0, 0, 1, 1, 2, 4000, 4000]);
  assert.equal(floorAt(20, [89, 0]), 89, 'a ride to 89 passes 89, not the lowest number of its storey');
  assert.deepEqual(floorsNear(13), [1, 4000], 'only floors with a storey of their own are drawn from outside');
  // A listed floor that is not the lowest: the numbers under it share a storey too.
  listFloors([5, 9]);
  assert.deepEqual([1, 4, 5, 6, 8, 9].map(storeyOf), [1, 1, 2, 3, 3, 4]);
  // Two listed floors side by side have no storey between them.
  listFloors([1, 2]);
  assert.deepEqual([1, 2].map(altitude), [12, 19]);
  assert.equal(roofOf(2), 26);
  listFloors([]);
});

test('the ledger lists @jeffrey\'s floor and the highest', () => {
  const t = new Tower();
  t.admit(['oskie', 'jeffrey', 'prutti']);
  assert.deepEqual(t.listed(), [2, 3]);
  t.arrive('prutti');
  t.take('prutti', 300);
  assert.deepEqual(t.listed(), [2, 300]);
  assert.equal(t.count, 3, 'every handle keeps its floor');
});

test('the elevator rides from the ground to a floor, and only its owner builds there', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-tower-srv-'));
  const app = createRaxServer({ stateDir: dir, verifyIdentity: async () => ({ handle: 'rider', sub: 'acct-rider' }),
    listHandles: async () => ['jeffrey', 'oskie', 'rider'] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const port = app.server.address().port;
  const open = async (room, extra = '', token) => {
    const ws = new WebSocket(`ws://127.0.0.1:${port}/play?room=${room}&protocol=${PROTOCOL}&account=1${extra}`);
    ws.got = [];
    ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type !== 'snapshot') ws.got.push(d); });
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity', token }));
    ws.send(JSON.stringify({ type: 'account', accessToken: 'valid-test-token' }));
    return ws;
  };
  const until = async (ws, f) => {
    for (let i = 0; i < 300; i++) {
      const hit = ws.got.find(f);
      if (hit) return hit;
      await delay(10);
    }
    throw Error('timeout: ' + JSON.stringify(ws.got.map((d) => d.type)));
  };
  try {
    const ws = await open('lobby');
    const home = await until(ws, (d) => d.type === 'home');
    assert.equal(home.floor, 3);
    assert.equal((await until(ws, (d) => d.type === 'whereabouts')).room, 'lobby'); // a first visit starts on the plaza
    const told = await until(ws, (d) => d.type === 'tower' && d.yours);
    assert.equal(told.count, 3);
    assert.equal(told.here.number, 0);
    const dir = await (await fetch(`http://127.0.0.1:${port}/tower`)).json();
    assert.deepEqual(dir.floors, [[1, 'jeffrey', 0], [2, 'oskie', 0], [3, 'rider', 1]]);
    assert.equal((await (await fetch(`http://127.0.0.1:${port}/tower?q=@oskie`)).json()).number, 2);
    // Outside the car the elevator stays put and says why.
    ws.send(JSON.stringify({ type: 'elevator', to: '1' }));
    assert.match((await until(ws, (d) => d.type === 'elevator-result')).error, /step into the car/);
    assert.ok(!ws.got.some((d) => d.type === 'travel'));
    ws.got = ws.got.filter((d) => d.type !== 'elevator-result');
    // Step into the car and ask for floor 1.
    const room = app.rooms.get('lobby'), seat = [...room.sessions.values()][0].seat;
    Object.assign(room.match.seats[seat].player, { x: 0, z: CAR_Z });
    ws.send(JSON.stringify({ type: 'elevator', to: 'nobody' }));
    assert.match((await until(ws, (d) => d.type === 'elevator-result')).error, /no floor/);
    ws.send(JSON.stringify({ type: 'elevator', to: '@jeffrey' }));
    const { token } = await until(ws, (d) => d.type === 'identity');
    const trip = await until(ws, (d) => d.type === 'travel');
    assert.deepEqual(trip.ride, { from: 0, to: 1, handle: 'jeffrey' });
    ws.close();
    const up = await open(trip.room, `&passage=${trip.passage}`, token);
    const here = await until(up, (d) => d.type === 'tower' && d.yours);
    assert.equal(here.here.handle, 'jeffrey');
    assert.equal(here.owner, false);
    await until(up, (d) => d.type === 'arrival');
    // A visitor cannot paint @jeffrey's floor.
    up.send(JSON.stringify({ type: 'paint', i: 1, j: 1, c: 2 }));
    assert.match((await until(up, (d) => d.type === 'build-result')).error, /@jeffrey/);
    // Take a number: 89 is free; 2 is pencilled in for oskie, so they trade.
    up.send(JSON.stringify({ type: 'take-floor', number: 89 }));
    assert.match((await until(up, (d) => d.type === 'elevator-result' && d.note)).note, /89/);
    assert.equal(app.tower.tower.forHandle('rider').number, 89);
    assert.equal((await until(up, (d) => d.type === 'home' && d.floor === 89)).room, home.room);
    // Up from 1 is the next occupied floor.
    const seat2 = [...app.rooms.get(trip.room).sessions.values()][0].seat;
    Object.assign(app.rooms.get(trip.room).match.seats[seat2].player, { x: 0, z: CAR_Z });
    up.send(JSON.stringify({ type: 'elevator', to: 'up' }));
    assert.equal((await until(up, (d) => d.type === 'travel')).ride.to, 2);
    up.close();
    // 🌍 One world: coming back, the server says where the handle last stood.
    await delay(150);
    const back = await open('lobby', '', token);
    assert.equal((await until(back, (d) => d.type === 'whereabouts')).room, trip.room);
    back.close();
    // Looking in on the wrong room and being sent on is not standing there:
    // the rest stays put (it once moved, and the two rooms traded the visitor forever).
    await delay(150);
    const again = await open('lobby', '', token);
    assert.equal((await until(again, (d) => d.type === 'whereabouts')).room, trip.room);
    again.close();
    // A floor that does not exist is not a place.
    const bad = new WebSocket(`ws://127.0.0.1:${port}/play?room=floor-00000000&protocol=${PROTOCOL}`);
    const [code] = await once(bad, 'close');
    assert.equal(code, 1008);
  } finally {
    await app.close();
  }
});

test('a rider walks about in the running car and cannot step out until its door opens', async () => {
  const { move, spawn, HZ } = await import('../world.mjs');
  const { carWalls: walls, inCar: inside, CAR_Z: z, shutTicks } = await import('../elevator.mjs');
  const south = { yaw: 180, pitch: 0, fwd: 1, right: 0, buttons: 0 };
  let p = { ...spawn(0), x: 0.8, z: z + 0.8, shut: shutTicks(2, HZ) };
  const from = { x: p.x, z: p.z };
  while (p.shut > 0) { p = move(p, south, [], 0, walls()); assert.ok(inside(p), `out of the car at ${p.x.toFixed(2)}, ${p.z.toFixed(2)} with the door shut`); }
  assert.ok(Math.hypot(p.x - from.x, p.z - from.z) > 1, 'the rider did not move in the car');
  for (let i = 0; i < HZ; i++) p = move(p, south, [], 0, walls());
  assert.ok(!inside(p), 'the door did not open after the ride');
});
