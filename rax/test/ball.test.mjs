// ⚽ The ball over a seam: one football, no room's own. These cases roll it
// over a block's line and look for a jump — in where it is, in how fast it
// goes — from the world's side and from the two rooms that watch it.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createBall, createDispenser, MAX_BALLS, BALL, ballAt, carry, liftOf } from '../ball.mjs';
import { Actor } from '../film/actor.mjs';
import { CAR, CAR_Z, altitude } from '../elevator.mjs';
import { createRaxServer } from '../server.mjs';
import { Match } from '../match.mjs';
import { CFG, DT, PROTOCOL } from '../world.mjs';
import { SPAN } from '../park.mjs';

// Two blocks side by side, a and b to its east; nothing else is open.
const pair = (bodies = () => []) => createBall({
  neighbours: (room) => (room === 'a' ? { east: 'b' } : { west: 'a' }),
  bodies, start: { room: 'a', x: 6, z: 0 },
});
const world = (b) => b.x + (b.room === 'b' ? SPAN : 0);

test('the dispenser releases once per approach and keeps active balls in play', () => {
  const p = { seat: 1, x: BALL.x + 1, y: CFG.eyeHeight, z: BALL.z };
  let bodies = [p];
  const dispenser = createDispenser({ neighbours: () => ({}), bodies: () => bodies });
  dispenser.step();
  assert.ok(dispenser.balls[0].ball.vx < 0);
  for (let i = 0; i < 10; i++) dispenser.step();
  assert.equal(dispenser.balls.length, 1);
  for (let i = 0; i < MAX_BALLS; i++) {
    bodies = [];
    dispenser.step();
    bodies = [p];
    dispenser.step();
    assert.equal(dispenser.balls.length, Math.min(MAX_BALLS, i + 2));
  }
  assert.ok(dispenser.balls[0].ball.x < BALL.x - 1.5);
});

test('rolled over a seam the ball keeps its place in the world and its speed', () => {
  const { ball, step } = pair();
  Object.assign(ball, { vx: 9, vy: 3, vz: 1 });
  let last = world(ball), lastV = ball.vx, crossedAt = null;
  for (let i = 0; i < 90; i++) {
    const room = ball.room;
    step();
    const now = world(ball);
    assert.ok(now >= last && now - last < 9 * DT * 1.01 + 1e-9, `no jump at tick ${i}: ${last} → ${now}`);
    if (ball.room !== room) { crossedAt = i; assert.ok(Math.abs(ball.vx - lastV) < 0.2, 'the same speed on the far side'); }
    last = now; lastV = ball.vx;
  }
  assert.equal(ball.room, 'b');
  assert.ok(crossedAt !== null && ball.crossings === 1);
});

test('where no block is open the ball comes back off the line', () => {
  const { ball, step } = pair();
  Object.assign(ball, { room: 'b', x: 6, vx: 9 }); // b has nothing to its east
  for (let i = 0; i < 120; i++) step();
  assert.equal(ball.room, 'b');
  assert.ok(ball.x <= SPAN / 2 && ball.vx <= 0);
});

test('a runner kicks it ahead of them, and one standing in the next block stops it at the line', () => {
  const runner = { x: 5, y: CFG.eyeHeight, z: 0, vx: 7, vz: 0 };
  const keeper = { x: -7.6, y: CFG.eyeHeight, z: 0, vx: 0, vz: 0 }; // in b, just over the seam
  const { ball, step } = pair((room) => (room === 'a' ? [runner] : [keeper]));
  let fastest = 0;
  for (let i = 0; i < 20; i++) { runner.x += runner.vx * DT; step(); fastest = Math.max(fastest, ball.vx); }
  assert.ok(fastest > 7, 'it leaves faster than the foot');
  runner.vx = 0;
  for (let i = 0; i < 240; i++) step();
  // The keeper stands at world x 8.4: the ball met them from a's side of the seam.
  assert.ok(world(ball) < 8.4 - 0.4, `turned back by a body in the next room (${world(ball)})`);
});

test('between words from the server the page carries the ball on', () => {
  const b = { x: 0, y: BALL.r, z: 0, vx: 4, vy: 0, vz: -2 };
  assert.deepEqual(ballAt(b, 0.5), { x: 2, y: BALL.r, z: -1 });
  assert.ok(ballAt({ ...b, y: 2, vy: 0 }, 0.2).y < 2);
});

test('two rooms watch one ball roll over their seam, each in its own metres', async () => {
  const app = createRaxServer({ guests: true, stateDir: await mkdtemp(join(tmpdir(), 'rax-ball-')) });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `ws://127.0.0.1:${app.server.address().port}/play`;
  const east = app.geography.geography.neighbours('lobby')?.east;
  assert.ok(east, 'a block is open east of the plaza');
  const watch = async (room) => {
    const ws = new WebSocket(`${base}?${new URLSearchParams({ room, protocol: PROTOCOL })}`), seen = [];
    ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type === 'ball') seen.push(d); });
    await once(ws, 'open');
    return { ws, seen };
  };
  const a = await watch('lobby'), b = await watch(east);
  try {
    Object.assign(app.ball.ball, { room: 'lobby', x: 6, y: BALL.r, z: -6, vx: 8, vy: 0, vz: 0 });
    a.seen.length = b.seen.length = 0; // from where it is put down, not where it lay
    await delay(20);
    a.seen.length = b.seen.length = 0;
    for (let i = 0; i < 200 && app.ball.ball.room !== east; i++) await delay(10);
    await delay(100);
    assert.equal(app.ball.ball.room, east, 'the ball is in the next block');
    assert.ok(a.seen.length > 10 && b.seen.length > 10, 'both rooms were told');
    // The same ball at the same moment is one block apart in the two rooms' metres.
    const last = a.seen.at(-1), same = b.seen.at(-1);
    assert.ok(Math.abs(last.x - same.x - SPAN) < 0.5, `${last.x} in the plaza is ${same.x} next door`);
    // Neither room saw it jump: each word is a step on from the one before.
    for (const { seen } of [a, b])
      for (let i = 1; i < seen.length; i++)
        assert.ok(Math.abs(seen[i].x - seen[i - 1].x) < 1, `a jump of ${seen[i].x - seen[i - 1].x} m`);
    assert.ok(a.seen.some((s) => s.x > SPAN / 2), 'the plaza saw it past its own line');
    assert.ok(b.seen.some((s) => s.x < -SPAN / 2), 'next door saw it coming before it arrived');
  } finally {
    a.ws.close(); b.ws.close();
    await app.close();
  }
});

test('the ball lies where it was left when the server comes back', async () => {
  const stateDir = await mkdtemp(join(tmpdir(), 'rax-ball-kept-'));
  const first = createRaxServer({ guests: true, stateDir });
  await first.ready;
  const east = first.geography.geography.neighbours('lobby')?.east;
  Object.assign(first.ball.ball, { room: east, x: -2.5, y: BALL.r, z: 4, vx: 0, vy: 0, vz: 0 });
  await first.close();
  const second = createRaxServer({ guests: true, stateDir });
  await second.ready;
  try {
    const { room, x, z } = second.ball.ball;
    assert.deepEqual({ room, x, z }, { room: east, x: -2.5, z: 4 });
  } finally { await second.close(); }
});

test('off the edge of a floor of the tower the ball falls to the plaza below, and on from there', () => {
  const { ball, step } = createBall({
    neighbours: (room) => (room === 'lobby' ? { east: 'e1n0' } : room === 'e1n0' ? { west: 'lobby' } : null),
    below: (room) => (room === 'floor-x' ? { room: 'lobby', dy: 40 } : null),
    start: { room: 'floor-x', x: 6, z: 0 },
  });
  ball.vx = 8;
  let top = 0, fell = false, landed = null;
  const seen = new Set();
  for (let i = 0; i < 150; i++) {
    const was = ball.room, y = ball.y;
    step();
    if (was === 'floor-x' && ball.room !== was) { fell = true; assert.ok(Math.abs(ball.y - (y + 40)) < 0.5, 'the same height in the world'); }
    top = Math.max(top, ball.y);
    if (ball.room === 'e1n0' && landed === null && ball.y === BALL.r) landed = ball.room;
    seen.add(ball.room);
  }
  assert.ok(fell && top > 39);
  assert.deepEqual([...seen], ['floor-x', 'lobby', 'e1n0'].filter((r) => seen.has(r)), 'floor, then the plaza\'s air, then the next block');
  assert.equal(landed, 'e1n0', 'it came down past the plaza\'s line, in the next block');
});

test('what is in the air is put by and taken up: rounds, rockets and a thrown thing', () => {
  const a = new Match();
  a.setObjects([{ id: 'rock', item: 'throw', x: 1, z: 1 }]);
  a.tick = 500;
  a.bullets.push({ x: 1, y: 1.2, z: 2, vx: 30, vy: 0, vz: 4, owner: 0, born: 480, safeUntil: 495, bounces: 3, id: 7 });
  a.rockets.push({ x: -2, y: 1.5, z: 0, dir: { x: 0, y: 0, z: 1 }, owner: 1, born: 470, id: 8 });
  Object.assign(a.items.get('rock'), { x: 4, y: 2, z: -3, vx: 5, vy: 1, vz: 0, fuseAt: 560 });
  const kept = JSON.parse(JSON.stringify(a.flight()));
  const b = new Match();
  b.setObjects([{ id: 'rock', item: 'throw', x: 1, z: 1 }]);
  b.tick = 20;
  b.resume(kept);
  assert.deepEqual({ ...b.bullets[0], id: 0 }, { x: 1, y: 1.2, z: 2, vx: 30, vy: 0, vz: 4, owner: -1, born: 20, safeUntil: 0, bounces: 3, id: 0 });
  assert.equal(b.tick - b.rockets[0].born, 30, 'a rocket is as old as it was');
  const rock = b.items.get('rock');
  assert.deepEqual([rock.x, rock.y, rock.z, rock.vx, rock.fuseAt - b.tick], [4, 2, -3, 5, 60]);
  assert.equal(new Match().flight(), null, 'nothing in the air is nothing to keep');
});

test('the dispenser keeps an old single-ball save and restores the additional balls it dispenses', async t => {
  const { readFile, writeFile, rm } = await import('node:fs/promises');
  const stateDir = await mkdtemp(join(tmpdir(), 'rax-dispenser-'));
  t.after(() => rm(stateDir, { recursive: true, force: true }));
  const file = join(stateDir, 'ball.json');
  const old = { room: 'lobby', x: -2, y: BALL.r, z: -3, vx: 0, vy: 0, vz: 0 };
  await writeFile(file, JSON.stringify(old));
  const first = createRaxServer({ guests: true, stateDir, listHandles: async () => [] });
  await first.ready;
  let ws;
  try {
    first.server.listen(0, '127.0.0.1');
    await once(first.server, 'listening');
    ws = new WebSocket(`ws://127.0.0.1:${first.server.address().port}/play?room=lobby&protocol=${PROTOCOL}`);
    const packets = [];
    ws.on('message', raw => { const m = JSON.parse(raw); if (m.type === 'ball') packets.push(m); });
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity' }));
    for (let i = 0; i < 200 && !first.rooms.get('lobby')?.members.some(Boolean); i++) await delay(10);
    const seat = first.rooms.get('lobby').match.seats.find(Boolean);
    Object.assign(seat.player, { x: BALL.x + 1, z: BALL.z, y: CFG.eyeHeight });
    for (let i = 0; i < 200 && !packets.some(m => m.others?.length); i++) await delay(10);
    const m = packets.find(m => m.others?.length);
    assert.ok(m);
    assert.equal(m.x, old.x);
    assert.equal(m.others[0].id, 1);
    assert.ok(m.others[0].vx < 0);
  } finally { ws?.close(); await first.close(); }
  const saved = JSON.parse(await readFile(file));
  const second = createRaxServer({ guests: true, stateDir, listHandles: async () => [] });
  await second.ready;
  await second.close();
  assert.deepEqual(JSON.parse(await readFile(file)), saved);
});

test('a ball on another floor is drawn on that floor: it does not fall from it, nor come up through this one', () => {
  // Seen from the plaza, at rest on a floor 19 m up.
  const above = { x: 1, y: 19 + BALL.r, z: 2, vx: 0, vy: 0, vz: 0, g: 19 };
  assert.equal(ballAt(above, 0.5).y, 19 + BALL.r, 'it lies where it lay');
  // Seen from that floor, at rest on the plaza.
  const under = { x: 1, y: -19 + BALL.r, z: 2, vx: 0, vy: 0, vz: 0, g: -19 };
  assert.equal(ballAt(under, 0.5).y, -19 + BALL.r, 'it stays under the slab');
  // Dropped on that floor it comes down on that floor.
  assert.equal(ballAt({ ...above, y: 19.5 }, 1).y, 19 + BALL.r);
});

test('a ball in the car goes up with it, a little a tick, and the door is shut on it', () => {
  const { ball, step } = createBall({ neighbours: () => null, start: { room: 'floor-x', x: 0, z: CAR_Z } });
  carry(ball, -19, 4); // the car left the plaza for a floor 19 m up
  assert.equal(liftOf(ball), -19, 'it starts where the car started');
  ball.vz = -6; // kicked at the door
  let last = liftOf(ball), ticks = 0;
  while (ball.ride && ticks < 1000) {
    assert.equal(step(), true, 'somewhere new every tick of the ride');
    const now = liftOf(ball);
    assert.ok(now >= last && now - last < 0.3, `no jump at tick ${ticks}: ${last} → ${now}`);
    assert.ok(ball.z > CAR_Z - CAR, 'kept in by the door');
    last = now; ticks++;
  }
  assert.ok(ticks >= 240 && ticks < 300, `the ride and a moment after the bell (${ticks} ticks)`);
  assert.equal(liftOf(ball), 0);
  ball.vz = -6;
  for (let i = 0; i < 60; i++) step();
  assert.ok(ball.z < CAR_Z - CAR, 'and out of the car when it has arrived');
});

test('the dispenser calls back no ball that someone is with', () => {
  const p = { seat: 1, x: BALL.x + 1, y: CFG.eyeHeight, z: BALL.z };
  let lobby = [p];
  const friend = { seat: 2, x: 5, y: CFG.eyeHeight, z: 5 };
  const dispenser = createDispenser({ neighbours: () => ({}), bodies: (room) => (room === 'lobby' ? lobby : room === 'floor-x' ? [friend] : []) });
  const approach = () => { lobby = []; dispenser.step(); lobby = [p]; dispenser.step(); };
  while (dispenser.balls.length < MAX_BALLS) approach();
  // Every ball is taken off somewhere and left: one beside a friend up the tower, the rest out in the park.
  dispenser.balls.forEach(({ ball }, i) => Object.assign(ball, { room: i ? 'e1n0' : 'floor-x', x: i - 6, y: BALL.r, z: 0, vx: 0, vy: 0, vz: 0 }));
  for (let i = 0; i < MAX_BALLS * 2; i++) approach();
  assert.deepEqual({ room: dispenser.balls[0].ball.room, x: dispenser.balls[0].ball.x }, { room: 'floor-x', x: -6 }, 'the one upstairs lies where it was left');
  assert.ok(dispenser.balls.some(({ ball }) => ball.room === 'lobby'), 'the park\'s were called back');
});

test('a ball in the car rides up with its rider: the plaza sees it rise, the rider keeps it at their feet', async () => {
  const handles = ['jeffrey'];
  const app = createRaxServer({ guests: true, stateDir: await mkdtemp(join(tmpdir(), 'rax-ball-ride-')),
    listHandles: async () => handles, resolveHandleColors: async () => null,
    verifyIdentity: async (token) => { if (token.startsWith('guest')) throw Error('not signed in'); return { handle: token, sub: `acct-${token}` }; } });
  await app.ready;
  await app.tower.change((t) => t.admit(handles));
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const port = app.server.address().port;
  const rider = new Actor({ base: `http://127.0.0.1:${port}`, handle: 'jeffrey' });
  const below = new WebSocket(`ws://127.0.0.1:${port}/play?${new URLSearchParams({ room: 'lobby', protocol: PROTOCOL })}`);
  const fromPlaza = [], fromCar = [];
  try {
    await once(below, 'open');
    await rider.enter('lobby');
    await rider.walkTo(0, -1.2, { pace: 1 });
    await rider.walkTo(0, 3, { pace: 1 });
    await rider.walkTo(0.6, CAR_Z, { pace: 0.6, within: 0.05 });
    const b = app.ball.ball;
    Object.assign(b, { room: 'lobby', x: -0.7, y: BALL.r, z: CAR_Z + 0.3, vx: 0, vy: 0, vz: 0 });
    await delay(50);
    below.on('message', (raw) => { const d = JSON.parse(raw); if (d.type === 'ball') fromPlaza.push(d); });
    const floor = app.tower.tower.forHandle('jeffrey'), up = altitude(floor.number);
    await rider.ride('home'); // (a scripted rider comes to the floor on a new socket)
    rider.ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type === 'ball') fromCar.push(d); });
    assert.equal(b.room, `floor-${floor.id}`, 'the ball is on its way to the floor');
    for (let i = 0; i < 1000 && b.ride; i++) await delay(10);
    await delay(100);
    assert.ok(up > 5 && !b.ride);
    // From the plaza: up from the car's floor to the floor above, never more than a step at a time.
    assert.ok(fromPlaza.length > 100, `told all the way up (${fromPlaza.length} words)`);
    for (let i = 1; i < fromPlaza.length; i++)
      assert.ok(Math.abs(fromPlaza[i].y - fromPlaza[i - 1].y) < 0.6, `a jump of ${fromPlaza[i].y - fromPlaza[i - 1].y} m seen from the plaza`);
    assert.ok(fromPlaza[0].y < 1 && Math.abs(fromPlaza.at(-1).y - (up + BALL.r)) < 0.01, `${fromPlaza[0].y} → ${fromPlaza.at(-1).y}`);
    // From the car: it lies on the car's floor the whole way (`lift` is the car's own height, which the page counts for itself).
    const riding = fromCar.filter((d) => d.lift !== undefined);
    assert.ok(riding.length > 100, `the rider was told all the way (${riding.length} words)`);
    for (const d of riding) assert.ok(Math.abs(d.y - d.lift - BALL.r) < 0.01 && Math.abs((d.g || 0) - d.lift) < 0.01, JSON.stringify(d));
    assert.ok(riding[0].lift < -5, `still well under the floor when first told (${riding[0].lift})`);
    assert.ok(Math.abs(fromCar.at(-1).y - BALL.r) < 0.01 && fromCar.at(-1).g === undefined, 'and on the floor when the car is there');
  } finally {
    below.close(); rider.leave();
    await app.close();
  }
});
