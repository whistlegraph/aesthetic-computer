import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { Tower, TowerStore, floorRoom, isFloorRoom, carWalls, lobbyWalls, LOBBY_FRONT, inCar, CAR, CAR_Z, altitude, floorAt } from '../tower.mjs';
import { createDuelServer } from '../server.mjs';
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
  const dir = await mkdtemp(join(tmpdir(), 'dool-tower-'));
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

test('a floor is a height', () => {
  assert.equal(altitude(0), 0);
  assert.equal(altitude(1), 12);
  assert.equal(altitude(89), 628);
  assert.deepEqual([0, 11.9, 12, 18.9, 19, 628, 634.9].map(floorAt), [0, 0, 1, 1, 2, 89, 89]);
});

test('the elevator rides from the ground to a floor, and only its owner builds there', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'dool-tower-srv-'));
  const app = createDuelServer({ stateDir: dir, verifyIdentity: async () => ({ handle: 'rider', sub: 'acct-rider' }),
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
    // A floor that does not exist is not a place.
    const bad = new WebSocket(`ws://127.0.0.1:${port}/play?room=floor-00000000&protocol=${PROTOCOL}`);
    const [code] = await once(bad, 'close');
    assert.equal(code, 1008);
  } finally {
    await app.close();
  }
});
