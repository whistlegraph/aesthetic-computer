// 🧱 Spell: build-on-floor. On your own floor of the tower you aim at the
// floor, open the build prompt and ask for a cube. Each test says what a
// visitor ought to get; a failing one is a defect in rax, not in the test.
//
// A private rax, the real socket, a scripted @jeffrey (film/actor.mjs) and a
// stand-in for Aesel that answers every prompt with a 2 m cube.
import test, { before, after } from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { createRaxServer } from '../server.mjs';
import { connectBuilder } from '../aesel.mjs';
import { Actor } from '../film/actor.mjs';
import { WORLD, direction, wallDistance } from '../world.mjs';
import { CAR_Z, inCar } from '../elevator.mjs';

const CUBE = { name: 'Cube', boxes: [{ x: 0, y: 1, z: 0, w: 2, h: 2, d: 2, color: 0xff593b }] };

// The floor square the page highlights under the crosshair, as client.mjs
// works it out each frame (the "floor square under the crosshair" block): the
// ray from the eyes meets y = 0 before any wall, and the square is 2 m wide on
// a grid that starts at -18. Returns its indices and its centre in metres.
function highlighted(eye, yaw, pitch) {
  const dir = direction(yaw, pitch);
  if (!(dir.y < -0.02)) return null;
  const nearest = wallDistance(eye, dir, 60), t = -eye.y / dir.y;
  if (!(t > 0 && t < Math.min(60, nearest + 0.05))) return null;
  const i = Math.floor((eye.x + dir.x * t + 18) / 2), j = Math.floor((eye.z + dir.z * t + 18) / 2);
  return i >= 0 && i < 18 && j >= 0 && j < 18 ? { i, j, x: -18 + i * 2 + 1, z: -18 + j * 2 + 1 } : null;
}

let app, worker, base, jeffrey, floor;
const body = (a) => app.rooms.get(a.room).match.seats[a.seat].player;
// The director's mark: stand here, face there, look down this much.
async function mark(a, x, z, yaw, pitch = 0) {
  Object.assign(body(a), { x, z, vx: 0, vz: 0 });
  a.control.yaw = yaw;
  a.control.pitch = pitch;
  await delay(150);
}
// Type a build prompt as the page sends it; resolves with the new object.
async function build(a, prompt = 'a cube') {
  const had = new Set(app.store.objects(a.room).map((o) => o.id));
  const answer = a.until((d) => d.type === 'build-accepted' || (d.type === 'build-result' && d.error), 5000, 'the build to be accepted');
  a.send({ type: 'prompt', prompt });
  const accepted = await answer;
  if (accepted.error) throw Error(accepted.error);
  const job = app.builders.jobs.get(accepted.id), footprint = { x: job.x, z: job.z };
  const done = await a.until((d) => d.type === 'build-result' && d.id === accepted.id, 8000, 'the build to finish');
  if (done.error) throw Error(done.error);
  const made = app.store.objects(a.room).find((o) => !had.has(o.id));
  return { x: made.x, z: made.z, id: made.id, footprint };
}
// Take everything out of a room the way the page does (the remove message).
async function clear(a) {
  for (const o of app.store.objects(a.room)) a.send({ type: 'remove', objectId: o.id });
  for (let i = 0; i < 150 && (app.store.objects(a.room).length || app.rooms.get(a.room).match.objects.length); i++) await delay(20);
  assert.equal(app.store.objects(a.room).length, 0, 'the room did not empty');
}
async function rideFromLobby(a, to) {
  await a.enter('lobby');
  await mark(a, 0, CAR_Z, 0);
  await a.ride(to);
}
const where = (o) => `${o.x}, ${o.z}`;

before(async () => {
  const token = 'b'.repeat(64);
  app = createRaxServer({
    stateDir: await mkdtemp(join(tmpdir(), 'rax-spell-build-')),
    builderToken: token,
    listHandles: async () => ['jeffrey', 'neighbor.keeper'],
    keeper: 'neighbor.keeper', // the plaza is arranged only by its keeper
    verifyIdentity: async (handle) => ({ handle, sub: `acct-${handle}` }),
    resolveHandleColors: async () => null,
  });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  base = `http://127.0.0.1:${app.server.address().port}`;
  worker = connectBuilder({ url: base.replace('http', 'ws') + '/builder', token, name: 'stand-in',
    generate: async () => { await delay(120); return CUBE; } });
  for (let i = 0; i < 200 && app.builders.workers.size < 1; i++) await delay(10);
  jeffrey = new Actor({ base, handle: 'jeffrey' });
  await rideFromLobby(jeffrey, 'home');
  floor = jeffrey.room;
  // These tests place the builder around the floor. The arrival ride must finish first.
  const end = Date.now() + 12000;
  while (jeffrey.me.shut > 0 && Date.now() < end) await delay(20);
  assert.ok(!(jeffrey.me.shut > 0), 'the elevator opened on the builder’s floor');
});
after(async () => {
  jeffrey?.leave();
  worker?.close();
  await app?.close();
});

test('the owner rides home and a cube asked for there stands on their floor, inside the glass', async () => {
  assert.equal(jeffrey.tower.owner, true);
  assert.equal(jeffrey.tower.here.handle, 'jeffrey');
  await mark(jeffrey, 1, 2.2, 180, -30);
  const cube = await build(jeffrey);
  assert.ok(Math.abs(cube.x) + 1 <= WORLD && Math.abs(cube.z) + 1 <= WORLD, `the cube is off the slab at ${where(cube)}`);
  assert.equal(app.store.objects('lobby').length, 0, 'nothing fell to the plaza');
  await clear(jeffrey);
});

test('the crosshair only ever highlights a square of the slab, never one past the glass or on the ground below', () => {
  for (let x = -7.5; x <= 7.5; x += 2.5)
    for (let z = -7.5; z <= 7.5; z += 2.5)
      for (let yaw = 0; yaw < 360; yaw += 15)
        for (const pitch of [-85, -60, -40, -25, -12, -6, -3, -1.5]) {
          const s = highlighted({ x, y: 1.7, z }, yaw, pitch);
          if (s) assert.ok(s.i >= 5 && s.i <= 12 && s.j >= 5 && s.j <= 12, `from ${x}, ${z} yaw ${yaw} pitch ${pitch} square ${s.i}, ${s.j} is off the slab`);
        }
});

test('a new build lands on the floor square the crosshair highlights', async () => {
  await clear(jeffrey);
  await mark(jeffrey, 1, 2.2, 180, -30); // just out of the car, looking at the floor three metres ahead
  const aim = highlighted(jeffrey.me, jeffrey.control.yaw, jeffrey.control.pitch);
  assert.deepEqual([aim.x, aim.z], [1, -1], 'the square under the crosshair');
  const cube = await build(jeffrey);
  await clear(jeffrey);
  assert.ok(Math.abs(cube.x - aim.x) <= 1 && Math.abs(cube.z - aim.z) <= 1,
    `aimed at the square centred ${where(aim)}; the cube stands at ${where(cube)} (its first footprint at ${where(cube.footprint)})`);
});

test('aiming at a different square moves where the build lands', async () => {
  await clear(jeffrey);
  const spots = [];
  for (const pitch of [-30, -14, 20]) { // a near square, a far square, the sky
    await mark(jeffrey, 1, 2.2, 180, pitch);
    const aim = highlighted(jeffrey.me, jeffrey.control.yaw, jeffrey.control.pitch);
    const cube = await build(jeffrey);
    await clear(jeffrey);
    spots.push({ pitch, aim: aim ? where(aim) : 'no square', cube: where(cube) });
  }
  assert.notEqual(spots[0].aim, spots[1].aim, 'the two aims are different squares');
  assert.notEqual(spots[0].cube, spots[1].cube, `every aim put the cube in one place: ${JSON.stringify(spots)}`);
});

test('a build aimed at the floor before the south glass lands on or beside that square, ahead of the builder', async () => {
  await clear(jeffrey);
  await mark(jeffrey, 1, -4.8, 180, -40); // three metres from the glass, looking at the floor two metres ahead
  const aim = highlighted(jeffrey.me, jeffrey.control.yaw, jeffrey.control.pitch);
  const cube = await build(jeffrey);
  await clear(jeffrey);
  assert.ok(Math.hypot(cube.x - aim.x, cube.z - aim.z) <= 2,
    `standing at 1, -4.8 facing south, aimed at the square centred ${where(aim)}; the cube stands at ${where(cube)}`);
});

test('a build asked for while facing the elevator leaves the car door open', async () => {
  await clear(jeffrey);
  await mark(jeffrey, 0, 0, 0, -30); // mid-floor, facing the car
  const cube = await build(jeffrey);
  let reached = null;
  try {
    await jeffrey.walkTo(0, CAR_Z, { pace: 0.6, patience: 4 });
    reached = true;
  } catch { reached = false; }
  const stopped = { ...jeffrey.at };
  await clear(jeffrey);
  assert.ok(reached && inCar(stopped), `the cube stands at ${where(cube)}; walking to the car stopped at ${stopped.x.toFixed(2)}, ${stopped.z.toFixed(2)}`);
});

test('a visitor riding up can step out of the car after the owner builds facing it', async () => {
  await clear(jeffrey);
  await mark(jeffrey, 0, 0, 0, -30);
  const cube = await build(jeffrey);
  const visitor = new Actor({ base, handle: 'neighbor.keeper' });
  let reached = null, stopped = null;
  try {
    await rideFromLobby(visitor, '@jeffrey');
    assert.equal(visitor.room, floor);
    try { await visitor.walkTo(0, 0, { pace: 0.6, patience: 9 }); reached = true; } catch { reached = false; } // (the door is shut while the car runs: the walk waits at it)
    stopped = { ...visitor.at };
  } finally {
    visitor.leave();
    await clear(jeffrey);
  }
  assert.ok(reached, `the cube stands at ${where(cube)}; the visitor left the car as far as ${stopped.x.toFixed(2)}, ${stopped.z.toFixed(2)}`);
});

test('the owner can take a misplaced build away and ask again', async () => {
  await clear(jeffrey);
  await mark(jeffrey, 1, 2.2, 180, -30);
  const cube = await build(jeffrey);
  jeffrey.send({ type: 'remove', objectId: cube.id });
  for (let i = 0; i < 150 && app.store.objects(floor).length; i++) await delay(20);
  assert.equal(app.store.objects(floor).length, 0);
  const again = await build(jeffrey);
  assert.ok(again.id && again.id !== cube.id);
  await clear(jeffrey);
});

test('on the plaza a build lands on the highlighted square too', async () => {
  const walker = new Actor({ base, handle: 'neighbor.keeper' });
  let aim, cube;
  try {
    await walker.enter('lobby');
    await mark(walker, -5, -7, 90, -30); // clear ground opposite the courtyard dispenser
    aim = highlighted(walker.me, walker.control.yaw, walker.control.pitch);
    cube = await build(walker);
    await clear(walker);
  } finally { walker.leave(); }
  assert.ok(Math.abs(cube.x - aim.x) <= 1 && Math.abs(cube.z - aim.z) <= 1,
    `aimed at the square centred ${where(aim)}; the cube stands at ${where(cube)}`);
});

test('a build asked for from the courtyard leaves the lobby entrance open', async () => {
  const walker = new Actor({ base, handle: 'neighbor.keeper' });
  let cube, reached = null, stopped;
  try {
    await walker.enter('lobby');
    await mark(walker, 0, -5, 0, -30); // where a first visit starts, facing the door
    cube = await build(walker);
    try { await walker.walkTo(0, 3, { pace: 0.6, patience: 4 }); reached = true; } catch { reached = false; }
    stopped = { ...walker.at };
    await clear(walker);
  } finally { walker.leave(); }
  assert.ok(reached, `the cube stands at ${where(cube)}; walking in stopped at ${stopped.x.toFixed(2)}, ${stopped.z.toFixed(2)}`);
});
