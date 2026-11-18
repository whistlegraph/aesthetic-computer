// 🛗 Spell: ride-home. In the car a visitor types `home`, or the bare number
// of their own floor, and the car takes them up.
//
// Each case is a private dool over the real socket, with scripted players
// (film/actor.mjs) who walk the newcomer's path on foot: courtyard, lobby
// door, car. A case asserts what a visitor ought to get, so the ones that
// fail are the findings. The cast is @jeffrey and invented neighbours.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { createDuelServer } from '../server.mjs';
import { Actor } from '../film/actor.mjs';
import { Projector } from '../project.mjs';
import { CFG, direction, along } from '../world.mjs';
import { CAR_SCREEN_Z, inCar, CAR, CAR_Z } from '../elevator.mjs';

// Ten neighbours came before @jeffrey, so his floor is 11.
const HANDLES = [];
for (let i = 1; i <= 10; i++) HANDLES.push(`neighbor${i.toString(36)}`);
HANDLES.push('jeffrey');

async function dool() {
  const app = createDuelServer({ guests: true,
    stateDir: await mkdtemp(join(tmpdir(), 'dool-spell-ride-home-')),
    listHandles: async () => HANDLES,
    resolveHandleColors: async () => null,
    // A token is the handle it plays; a token that starts `guest` is nobody.
    verifyIdentity: async (token) => {
      if (token.startsWith('guest')) throw Error('not signed in');
      return { handle: token, sub: `acct-${token}` };
    },
  });
  await app.ready;
  await app.tower.change((t) => t.admit(HANDLES));
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `http://127.0.0.1:${app.server.address().port}`, cast = [];
  return {
    app,
    async join(handle, room = 'lobby') {
      const actor = new Actor({ base, handle });
      cast.push(actor);
      await actor.enter(room);
      return actor;
    },
    async close() { for (const a of cast) a.leave(); await app.close(); },
  };
}
// What the elevator (or anything else) says back to one message, within `ms`.
async function ask(actor, message, ms = 500) {
  const heard = [], listen = (raw) => { const d = JSON.parse(raw); if (d.type !== 'snapshot') heard.push(d); };
  actor.ws.on('message', listen);
  actor.send(message);
  await delay(ms);
  actor.ws.off('message', listen);
  return heard.filter((d) => ['travel', 'elevator-result', 'build-result', 'said'].includes(d.type));
}
// The newcomer's walk: up the courtyard, through the lobby door, to `z` on the car's line.
async function walkToCar(actor, z = CAR_Z) {
  await actor.walkTo(0, -1.2, { pace: 1 });
  await actor.walkTo(0, 3, { pace: 1 });
  await actor.walkTo(0, z, { pace: 0.6, within: 0.03 });
}
const towerTold = async (actor, test) => {
  for (let i = 0; i < 300 && !(actor.tower && test(actor.tower)); i++) await delay(10);
  return actor.tower;
};

test('home, typed in the car on the plaza, rides a first-time visitor up to their own floor', async () => {
  const world = await dool();
  try {
    const jeffrey = await world.join('jeffrey');
    const told = await towerTold(jeffrey, (t) => t.yours);
    assert.equal(told.yours.number, 11, 'the lobby board reads: your floor: 11');
    assert.equal(told.here.number, 0);
    await walkToCar(jeffrey);
    assert.ok(inCar(jeffrey.me), 'walked into the car on foot');
    const trip = await jeffrey.ride('home');
    assert.deepEqual(trip, { from: 0, to: 11, handle: 'jeffrey' });
    assert.equal(jeffrey.room, told.yours.room);
    const up = await towerTold(jeffrey, (t) => t.here?.number);
    assert.equal(up.here.number, 11);
    assert.equal(up.owner, true, 'it is his floor to arrange');
    assert.ok(inCar(jeffrey.me), 'he arrives standing in the car');
  } finally { await world.close(); }
});

test('the bare number of your own floor, 11, is the same ride as home', async () => {
  const world = await dool();
  try {
    const jeffrey = await world.join('jeffrey');
    const { yours } = await towerTold(jeffrey, (t) => t.yours);
    await walkToCar(jeffrey);
    // Every spelling the car's line can send for the same floor.
    for (const typed of ['11', '#11', '011', ' 11 ', 'HOME', '@jeffrey', 'jeffrey']) {
      const [answer] = await ask(jeffrey, { type: 'elevator', to: typed }, 250);
      assert.equal(answer?.type, 'travel', `"${typed}" should ride, and got ${JSON.stringify(answer)}`);
      assert.equal(answer.room, yours.room, `"${typed}" goes to floor 11`);
      assert.equal(answer.ride.to, 11);
    }
    assert.equal((await jeffrey.ride(11)).to, 11);
    assert.equal(jeffrey.room, yours.room);
  } finally { await world.close(); }
});

test('typed on the floor you already stand on, home and your number both say: you are here', async () => {
  const world = await dool();
  try {
    const jeffrey = await world.join('jeffrey');
    await towerTold(jeffrey, (t) => t.yours);
    await walkToCar(jeffrey);
    await jeffrey.ride('home');
    for (const typed of ['home', '11']) {
      const [answer] = await ask(jeffrey, { type: 'elevator', to: typed }, 250);
      assert.equal(answer?.type, 'elevator-result');
      assert.equal(answer.error, 'you are here');
    }
  } finally { await world.close(); }
});

test('a guest who types home is told to sign in for a floor, and can still ride to a number', async () => {
  const world = await dool();
  try {
    const guest = await world.join('guest-neighbor');
    await walkToCar(guest);
    const [refused] = await ask(guest, { type: 'elevator', to: 'home' }, 250);
    assert.equal(refused?.error, 'sign in for a floor');
    const [ride] = await ask(guest, { type: 'elevator', to: '11' }, 250);
    assert.equal(ride?.type, 'travel');
    assert.equal(ride.ride.handle, 'jeffrey');
  } finally { await world.close(); }
});

test('asked from one step outside the car door, the elevator says why it is staying put', async () => {
  const world = await dool();
  try {
    const jeffrey = await world.join('jeffrey');
    await towerTold(jeffrey, (t) => t.yours);
    // In the doorway, looking in: 0.1 m short of the car's floor (it starts at z = 4.6).
    await walkToCar(jeffrey, CAR_Z - CAR - 0.1);
    assert.ok(!inCar(jeffrey.me), `standing at z ${jeffrey.me.z.toFixed(2)}, outside the car`);
    const answers = await ask(jeffrey, { type: 'elevator', to: 'home' });
    assert.ok(!answers.some((d) => d.type === 'travel'), 'the car is right not to leave without him');
    assert.ok(answers.some((d) => d.type === 'elevator-result' && (d.error || d.note)),
      'the elevator said nothing at all: no ride and no reason (server.mjs:915 returns without a word)');
  } finally { await world.close(); }
});

// The client's own numbers, read from its source so this case follows the
// code: where Enter turns the rider (client.mjs), where the car's screen
// hangs and where the typed line sits on it (view.mjs).
async function carLine() {
  const here = new URL('../', import.meta.url);
  const client = await readFile(new URL('client.mjs', here), 'utf8'), view = await readFile(new URL('view.mjs', here), 'utf8');
  const turned = /Turn to the screen over the door[^\n]*\n\s*yaw = ([\d.]+);\s*\n\s*pitch = ([\d.]+);/.exec(client);
  const hung = /carScreenFor\(tower\.screen\), 0, tower\.lift \|\| 0, CAR_SCREEN_Z, Math\.PI\)/.test(view); // elevator.mjs says how deep
  const typed = /line\(screen\.handle, [\d.]+, ([\d.]+), [\d.]+, ([\d.]+),/.exec(view);
  assert.ok(turned && hung && typed, 'the client changed how the car line is shown: read it again and update this case');
  return {
    yaw: Number(turned[1]), pitch: Number(turned[2]),
    z: CAR_SCREEN_Z,
    // The typed line's middle: font_1 is ten rows of `unit` under its top.
    y: Number(typed[1]) - 5 * Number(typed[2]),
    // Only the screen shows the line: the top line is closed while it is the elevator's.
    onlyOnScreen: /open: chat\.open && !liftLine,/.test(client),
    onTopLine: /^\s*open: chat\.open,$/m.test(client),
  };
}
test('Enter in the car shows the rider the line they are typing, wherever in the car they stand', async () => {
  const line = await carLine();
  const blind = [];
  // Every depth a body can stand at, door to back wall, on a 16:9 screen.
  const back = CAR_Z + CAR - CFG.playerRadius;
  for (let z = CAR_Z - CAR + 0.01; z <= back; z += 0.01) {
    const eye = { x: 0, y: CFG.eyeHeight, z }, lens = new Projector({});
    lens.camera(eye, along(eye, direction(line.yaw, line.pitch), 1), 16 / 9);
    const at = lens.project(0, line.y, line.z);
    if (!at || Math.abs(at.x) > 1 || Math.abs(at.y) > 1) blind.push(z);
  }
  const depth = back - (CAR_Z - CAR);
  // The line is also drawn on the top row of the page, whatever the view.
  if (line.onTopLine) return;
  assert.equal(blind.length, 0,
    `Enter turns the rider to yaw ${line.yaw}, pitch ${line.pitch}; from z ${blind[0]?.toFixed(2)} to ${blind.at(-1)?.toFixed(2)} ` +
    `(the first ${(blind.length / 100).toFixed(2)} m inside the door, ${Math.round((blind.length / 100 / depth) * 100)}% of the car's depth) ` +
    `the typed line at y ${line.y.toFixed(2)}, z ${line.z.toFixed(2)} is outside the frame` +
    (line.onlyOnScreen ? ', and the client draws it nowhere else (client.mjs:2249 closes the top line; view.mjs:1747 then drops the key hint)' : ''));
});
