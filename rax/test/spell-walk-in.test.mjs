// 🚶 Spell "walk-in": no words. Walk north from where rax sets you down:
// courtyard, entrance, lobby, car.
//
// Every case runs a private rax in this process and drives it over the real
// socket with scripted players (film/actor.mjs), the way a browser does. Each
// test asserts what a visitor ought to get, so a failing test is a finding.
// The cast is @jeffrey and invented neighbours; nothing here touches production.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile, readFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createRaxServer } from '../server.mjs';
import { Actor } from '../film/actor.mjs';
import { objectBoxes } from '../objects.mjs';
import { inCar, CAR, CAR_Z, LOBBY_FRONT, DOOR } from '../elevator.mjs';

const BUILDER_TOKEN = 'spell-walk-in-builder-token-0123456789abcdef';
const cube = (name, w = 1.6, h = 1.6, d = 1.6) => ({ name, boxes: [{ x: 0, y: h / 2, z: 0, w, h, d, color: 0xd66fae }] });
// A build as the store kept it before the tower: someone long gone made it.
const old = (id, x, z, geometry, code = null) => ({
  id, owner: 'someone-long-gone', x, z, item: null, effect: null,
  versions: [{ version: 0, prompt: geometry.name, at: 1, geometry, ...(code ? { code } : {}) }],
});

// A private plaza with the one-time ground clearing already recorded.
// Seeded builds must still be standing when the visitor arrives.
async function rax(seed = null) {
  const stateDir = await mkdtemp(join(tmpdir(), 'rax-spell-walk-in-'));
  if (seed) await writeFile(join(stateDir, 'objects.json'), JSON.stringify({ owners: {}, rooms: {}, marks: { groundCleared: 1 }, ...seed }));
  const app = createRaxServer({ openBuild: false, guests: true,
    stateDir,
    builderToken: BUILDER_TOKEN,
    // The cast signs in by name; a token starting "guest" is nobody's.
    verifyIdentity: async (token) => {
      if (token.startsWith('guest')) throw Error('not signed in');
      return { handle: token, sub: `acct-${token}` };
    },
    resolveHandleColors: async () => null,
    listHandles: async () => ['jeffrey', 'neighbor_ada', 'neighbor_bo'],
  });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `http://127.0.0.1:${app.server.address().port}`;
  const cast = [];
  let worker = null;
  return {
    app, base, stateDir,
    async enter(handle, room = 'lobby') {
      const a = new Actor({ base, handle });
      cast.push(a);
      await a.enter(room);
      return a;
    },
    // Aesel, stood in for: every prompt comes back as `shape` at once.
    async builder(shape) {
      worker = new WebSocket(`${base.replace('http', 'ws')}/builder`, { headers: { authorization: `Bearer ${BUILDER_TOKEN}` } });
      worker.on('message', (raw) => {
        const d = JSON.parse(raw);
        if (d.type === 'build') worker.send(JSON.stringify({ type: 'built', id: d.id, geometry: shape }));
      });
      await once(worker, 'open');
      worker.send(JSON.stringify({ type: 'hello', name: 'aesel' }));
      await delay(50);
    },
    disk: async () => JSON.parse(await readFile(join(stateDir, 'objects.json'), 'utf8')),
    async close() {
      for (const a of cast) a.leave();
      worker?.close();
      await app.close();
    },
  };
}

const spot = (a) => `${a.me.x.toFixed(2)}, ${a.me.z.toFixed(2)}`;
// Push the stick north for a while, as someone who has just arrived does.
// `jumping` hops the whole way (someone trying to get over what is in the way).
async function north(a, seconds = 3, { jumping = false } = {}) {
  a.control.yaw = 0;
  a.control.fwd = 1;
  const end = Date.now() + seconds * 1000;
  while (Date.now() < end) {
    if (jumping) a.jump();
    await delay(100);
  }
  a.control.fwd = 0;
  await delay(150);
}
// The solid boxes of what stands in the plaza.
const standing = (app) => objectBoxes(app.store.stored('lobby').filter((o) => !o.item), 0);
const inDoorway = (b) => b.xMax > -DOOR && b.xMin < DOOR && b.zMax > LOBBY_FRONT - 0.5 && b.zMin < LOBBY_FRONT + 0.5;
const inCarSpace = (b) => b.xMax > -CAR && b.xMin < CAR && b.zMax > CAR_Z - CAR && b.zMin < CAR_Z + CAR;
// Type a build at the prompt and wait for it to stand.
async function build(a, text) {
  for (let i = 0; i < 200 && !a.owner; i++) await delay(10); // the socket's identity has arrived
  const done = a.until((d) => d.type === 'build-result', 6000, 'the build');
  a.send({ type: 'prompt', prompt: text });
  const r = await done;
  assert.equal(r.error, undefined, `the build was refused: ${r.error}`);
  await delay(80);
  return r.objectId;
}

test('on an empty plaza, walking north from where rax sets you down ends in the elevator car', async () => {
  const d = await rax();
  try {
    const a = await d.enter('jeffrey');
    assert.equal(spot(a), '0.00, -5.00', 'the first visitor is set down in the courtyard, south of the door');
    await north(a, 3);
    assert.ok(inCar(a.me), `walked north to ${spot(a)}, which is not in the car`);
  } finally { await d.close(); }
});

test('a build left in the plaza before the tower does not stop a new visitor walking north to the car', async () => {
  // One 1.6 m build at (0, 1): six metres ahead of the first spawn, which is
  // where every prompt typed on arrival has always put its build.
  const d = await rax({ rooms: { lobby: [old('old-statue', 0, 1, cube('old statue'))] } });
  try {
    const a = await d.enter('jeffrey');
    await north(a, 3);
    assert.ok(inCar(a.me), `stopped at ${spot(a)} by a build standing in the entrance (the lobby front is at z ${LOBBY_FRONT})`);
  } finally { await d.close(); }
});

test('a tall build left across the entrance still leaves a way in, on foot or by jumping', async () => {
  // 3.2 m wide and 4 m tall: one object, well inside the 12 x 6 x 12 a build may be.
  const d = await rax({ rooms: { lobby: [old('old-gate', 0, 1, cube('old gate', 3.2, 4, 1))] } });
  try {
    const a = await d.enter('jeffrey');
    await north(a, 4, { jumping: true });
    assert.ok(a.me.z > LOBBY_FRONT + 0.5, `never got past the entrance: stopped at ${spot(a)}`);
  } finally { await d.close(); }
});

test('a build left where the car now stands does not keep a visitor out of the car', async () => {
  const d = await rax({ rooms: { lobby: [old('old-tree', 0, 6, cube('old tree', 2.6, 3, 2.6))] } });
  try {
    const a = await d.enter('jeffrey');
    await north(a, 4, { jumping: true });
    // The last requested jump may still be airborne. Check what it lands on.
    const landingDeadline = Date.now() + 2500;
    while (!a.me.onGround && Date.now() < landingDeadline) await delay(20);
    assert.ok(a.me.onGround && inCar(a.me) && a.me.y < 2.3, `ended at ${spot(a)} (eyes at ${a.me.y.toFixed(2)} m), not standing on the car's floor`);
  } finally { await d.close(); }
});

test('a build typed on arrival today is not placed in the tower entrance', async () => {
  const d = await rax();
  try {
    await d.builder(cube('statue'));
    const a = await d.enter('jeffrey');
    await build(a, 'a statue');
    const at = d.app.store.stored('lobby')[0];
    assert.ok(!standing(d.app).some(inDoorway), `the server stood it at (${at.x}, ${at.z}), in the entrance`);
  } finally { await d.close(); }
});

test('a build cannot be placed inside the elevator car', async () => {
  const d = await rax();
  try {
    await d.builder(cube('statue'));
    const a = await d.enter('jeffrey');
    await a.walkTo(0, 0, { pace: 0.6, within: 0.1 });
    await a.face(0, 0.2);
    await delay(150);
    await build(a, 'a statue');
    const at = d.app.store.stored('lobby')[0];
    assert.ok(!standing(d.app).some(inCarSpace), `the server stood it at (${at.x}, ${at.z}), inside the car`);
  } finally { await d.close(); }
});

test('nobody is set down inside the elevator car on arriving in the plaza', async () => {
  const d = await rax();
  try {
    const first = await d.enter('jeffrey'), second = await d.enter('neighbor_ada');
    assert.ok(!inCar(first.me), `the first visitor arrived in the car at ${spot(first)}`);
    assert.ok(!inCar(second.me), `the second visitor arrived in the car at ${spot(second)}, facing ${second.me.yaw}`);
  } finally { await d.close(); }
});

test('a passer-by cannot delete a build from the plaza; on a floor only its owner can', async () => {
  const d = await rax();
  try {
    await d.builder(cube('statue'));
    const jeffrey = await d.enter('jeffrey');
    const id = await build(jeffrey, 'a statue');
    assert.equal(d.app.store.stored('lobby').length, 1);
    const guest = await d.enter('guest-passer-by');
    assert.equal(guest.tower?.yours ?? null, null, 'the passer-by has no handle');
    const no = guest.until((m) => m.type === 'build-result' && m.error, 3000, 'a refusal');
    guest.send({ type: 'remove', objectId: id });
    assert.match((await no).error, /plaza/);
    assert.equal(d.app.store.stored('lobby').length, 1, 'the guest\'s remove took @jeffrey\'s build out of the plaza');
    // The same message on a floor of the tower is refused.
    const room = `floor-${d.app.tower.tower.forHandle('jeffrey').id}`;
    await d.app.store.commit({ room, objectId: 'on-his-floor', owner: 'x', baseVersion: -1, x: 0, z: 0, prompt: 'lamp' }, cube('lamp'));
    const ada = await d.enter('neighbor_ada', room);
    const refused = ada.until((m) => m.type === 'build-result', 3000, 'a refusal');
    ada.send({ type: 'remove', objectId: 'on-his-floor' });
    assert.match((await refused).error, /Only @jeffrey/);
    assert.equal(d.app.store.stored(room).length, 1);
  } finally { await d.close(); }
});

test('a build the keeper takes out of the plaza is still in the store to put back', async () => {
  const d = await rax({ rooms: { lobby: [old('old-statue', 0, 1, cube('old statue'))] } });
  try {
    const keeper = await d.enter('jeffrey');
    keeper.send({ type: 'remove', objectId: 'old-statue' });
    await delay(200);
    assert.equal(d.app.store.stored('lobby').length, 0);
    assert.equal(d.app.store.removed('lobby')[0]?.removedBy, 'jeffrey');
    const kept = JSON.stringify(await d.disk());
    assert.ok(kept.includes('old statue'), 'objects.json holds no trace of the deleted build: no geometry, no position, no owner');
  } finally { await d.close(); }
});

test('one message from a guest does not wipe the plaza\'s builds, paint and screens', async () => {
  const d = await rax({
    rooms: { lobby: [old('old-statue', -4, -6, cube('old statue')), old('old-tree', 5, 6, cube('old tree'))] },
    tiles: { lobby: { '8,8': 1, '9,8': 2, '8,9': 3 } },
    screens: { lobby: [{ id: 'old-screen', owner: 'someone-long-gone', code: 'roz', x: -5, z: 6, yaw: 0, at: 1 }] },
  });
  try {
    const guest = await d.enter('guest-passer-by');
    guest.send({ type: 'clear-room' }); // no menu sends this any more; the socket still takes it
    await delay(200);
    const left = { builds: d.app.store.stored('lobby').length, paint: Object.keys(d.app.store.tiles('lobby')).length, screens: d.app.store.screens('lobby').filter((s) => s.owner).length }; // (the lobby's own three are not anyone's)
    assert.deepEqual(left, { builds: 2, paint: 3, screens: 1 });
  } finally { await d.close(); }
});
