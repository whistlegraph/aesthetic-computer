import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp, readFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { kioskPlan, kioskSigns, kioskBox, kioskFront, SONGS } from '../kiosks-plan.mjs';
import { carWalls, lobbyWalls, clearOfCar, LEG, ATRIUM, LOBBY_FRONT, DOOR, CAR, CAR_WALL, CAR_Z } from '../elevator.mjs';
import { PANEL_REACH, panelHit } from '../panel.mjs';
import { PROTOCOL, TOWER_HALF } from '../world.mjs';
import { createRaxServer } from '../server.mjs';

// Everything solid on the plaza: the lobby's glass and the planters, the car, the tower's legs.
const legs = [-LEG.at, LEG.at].flatMap((x) => [-LEG.at, LEG.at].map((z) =>
  ({ type: 'box', xMin: x - LEG.wide / 2, xMax: x + LEG.wide / 2, yMin: 0, yMax: ATRIUM, zMin: z - LEG.wide / 2, zMax: z + LEG.wide / 2 })));
const solids = [...lobbyWalls(), ...carWalls(), ...legs];
const shares = (a, b) => Math.min(a.xMax, b.xMax) - Math.max(a.xMin, b.xMin) > 1e-6 && Math.min(a.yMax, b.yMax) - Math.max(a.yMin, b.yMin) > 1e-6
  && Math.min(a.zMax, b.zMax) - Math.max(a.zMin, b.zMin) > 1e-6;
// A body (0.4 m about its middle, as the server walks it) standing at a point.
const body = ({ x, z }) => ({ xMin: x - 0.4, xMax: x + 0.4, yMin: 0, yMax: 1.8, zMin: z - 0.4, zMax: z + 0.4 });
const plan = kioskPlan('lobby');

test('the plaza\'s kiosks keep their ids and their pieces', () => {
  assert.deepEqual(plan.map((k) => [k.id, k.piece]), [
    ['yard-0', 'bleep:sine 4x4'], ['yard-1', 'bleep:sine 4x4'],
    ['yard-2', 'song twinkle'], ['yard-3', 'song row'], ['yard-4', 'song weasel'], ['yard-5', 'song kid'], ['yard-6', 'song mmm'],
    ['yard-notepat', 'notepat:sine'],
  ]);
  assert.deepEqual(SONGS.map((name) => `song ${name}`), plan.filter((k) => k.piece.startsWith('song ')).map((k) => k.piece));
  assert.equal(new Set(plan.map((k) => k.id)).size, plan.length);
  for (const k of plan) {
    assert.match(k.id, /^[a-z0-9-]{1,24}$/);
    for (const n of ['x', 'y', 'z', 'yaw', 'w', 'h', 'cols', 'rows']) assert.ok(Number.isFinite(k[n]), `${k.id} ${n}`);
    assert.equal(typeof k.sign, 'boolean');
    assert.ok([0, 90, 180, 270].includes(k.yaw));
  }
  assert.equal(kioskPlan('lobby'), plan, 'one plan, worked out once');
  // Only the plaza has any, so far.
  for (const room of ['roof', 'floor-0123abcd', 'e1n0', 'botbop', '']) assert.deepEqual([...kioskPlan(room)], [], room);
});

test('the bleeps and the board stand where they stood', () => {
  const at = Object.fromEntries(plan.map((k) => [k.id, [k.x, k.y, k.z, k.yaw, k.w, k.h, k.cols, k.rows]]));
  assert.deepEqual(at['yard-0'], [-4.5, 1.35, 0.95, 0, 1.2, 1.2, 64, 64]);
  assert.deepEqual(at['yard-1'], [-2.9, 1.35, 0.95, 0, 1.2, 1.2, 64, 64]);
  assert.deepEqual(at['yard-notepat'], [-4.9, 1.6, -7.6, 180, 4.8, 2.4, 320, 160]);
});

test('the song hall: one row inside the lobby, east of the door, facing in, under its sign', () => {
  const songs = plan.filter((k) => k.piece.startsWith('song '));
  assert.equal(songs.length, 5);
  const end = TOWER_HALF - 0.1; // the east end's inner face
  for (const k of songs) {
    assert.equal(k.yaw, 180, 'facing north, into the hall');
    assert.equal(k.z, songs[0].z);
    assert.ok(k.z > LOBBY_FRONT + 0.1 && k.z < LOBBY_FRONT + 0.5, 'against the back of the front glass');
    assert.ok(k.x - k.w / 2 > DOOR && k.x + k.w / 2 < end, `${k.id} between the door and the hall's end`);
    assert.ok(k.y - k.h / 2 > 0.6 && k.y + k.h / 2 < 2.1, 'at a hand\'s height');
  }
  // Read from the left by whoever faces them: the first is nearest the door. Evenly spaced.
  const xs = songs.map((k) => k.x), gaps = xs.slice(1).map((x, i) => x - xs[i]);
  assert.ok(gaps.every((g) => g > songs[0].w && Math.abs(g - gaps[0]) < 1e-6), `spaced ${gaps}`);
  const [sign] = kioskSigns('lobby');
  assert.equal(sign.text, 'songs');
  assert.ok(Math.abs(sign.x - (xs[0] + xs.at(-1)) / 2) < 1e-6 && sign.z === songs[0].z && sign.yaw === 180, 'over the middle of the row');
  assert.ok(sign.y - sign.h / 2 > songs[0].y + songs[0].h / 2 + 0.27, 'clear of the songs\' own signs');
  assert.ok(!plan.some((k) => k.id === sign.id));
});

test('no kiosk is in a solid, and no two share space', () => {
  const boxes = plan.map((k) => ({ k, box: kioskBox(k) }));
  for (const { k, box } of boxes) {
    for (const s of solids) assert.ok(!shares(box, s), `${k.id} is in ${JSON.stringify(s)}`);
    assert.ok(Math.max(Math.abs(box.xMin), Math.abs(box.xMax), Math.abs(box.zMin), Math.abs(box.zMax)) <= TOWER_HALF, `${k.id} is on the plaza`);
    assert.ok(box.yMin > 0, `${k.id} is off the ground`);
  }
  for (let i = 0; i < boxes.length; i++)
    for (let j = i + 1; j < boxes.length; j++)
      assert.ok(!shares(boxes[i].box, boxes[j].box), `${boxes[i].k.id} and ${boxes[j].k.id}`);
  // The signs too, with the kiosks and with each other.
  const signs = [...plan.filter((k) => k.sign).map((k) => ({ ...k, id: `${k.id}-sign`, y: k.y + k.h / 2 + 0.17, h: 0.2 })), ...kioskSigns('lobby')].map((k) => ({ k, box: kioskBox(k, 0.06) }));
  for (const a of signs) {
    for (const s of solids) assert.ok(!shares(a.box, s), `${a.k.id} is in a solid`);
    for (const b of [...boxes, ...signs]) if (a !== b) assert.ok(!shares(a.box, b.box), `${a.k.id} and ${b.k.id}`);
  }
});

test('every kiosk is worked from open ground before its face, to both its edges', () => {
  for (const k of plan) {
    // Standing a step back from its middle and from either side of it: on the plaza, in no solid, and the aim meets its face from there.
    for (const side of [-0.3, 0, 0.3]) {
      const front = kioskFront(k, 1.2), along = k.yaw % 180 ? { x: 0, z: side * k.w } : { x: side * k.w, z: 0 };
      const stand = { x: front.x + along.x, z: front.z + along.z };
      assert.ok(Math.abs(stand.x) <= TOWER_HALF - 0.4 && Math.abs(stand.z) <= TOWER_HALF - 0.4, `${k.id}: ${JSON.stringify(stand)} is off the plaza`);
      for (const s of solids) assert.ok(!shares(body(stand), s), `${k.id}: standing at ${JSON.stringify(stand)} is in ${JSON.stringify(s)}`);
      const eye = { x: stand.x, y: 1.7, z: stand.z }, to = { x: k.x + along.x - eye.x, y: k.y - eye.y, z: k.z + along.z - eye.z }, far = Math.hypot(to.x, to.y, to.z);
      const hit = panelHit(k, eye, { x: to.x / far, y: to.y / far, z: to.z / far }, PANEL_REACH);
      assert.ok(hit, `${k.id}: its face is not met from ${JSON.stringify(stand)}`);
    }
    // From before its middle the aim reaches its two edges.
    const mid = kioskFront(k, 1.2);
    for (const side of [-0.48, 0.48]) {
      const edge = k.yaw % 180 ? { x: k.x, z: k.z + side * k.w } : { x: k.x + side * k.w, z: k.z };
      const to = { x: edge.x - mid.x, y: k.y - 1.7, z: edge.z - mid.z }, far = Math.hypot(to.x, to.y, to.z);
      assert.ok(panelHit(k, { x: mid.x, y: 1.7, z: mid.z }, { x: to.x / far, y: to.y / far, z: to.z / far }, PANEL_REACH), `${k.id}: an edge is out of reach`);
    }
    // From behind it is not worked.
    const back = kioskFront(k, -1.2);
    assert.equal(panelHit(k, { x: back.x, y: 1.7, z: back.z }, { x: (k.x - back.x) / 1.2, y: 0, z: (k.z - back.z) / 1.2 }, PANEL_REACH), null, `${k.id} from behind`);
  }
});

test('the hall leaves the door, the walk to the car, the car and the lobby\'s screens clear', () => {
  const keep = (xMin, xMax, zMin, zMax) => ({ xMin, xMax, yMin: 0, yMax: ATRIUM, zMin, zMax });
  const o = CAR + CAR_WALL;
  const clearings = {
    'the door': keep(-DOOR, DOOR, LOBBY_FRONT - 2, LOBBY_FRONT + 2),
    'the walk to the car': keep(-1.3, 1.3, -TOWER_HALF, CAR_Z - CAR),
    'the car': keep(-o, o, CAR_Z - o, CAR_Z + o),
    // The lobby's three screens (server.mjs `standing`), each 4 m wide, and the 3 m before each from which it is read.
    'the east screen': keep(4.2, 7.9, 2.6, 6.6),
    'the west screen': keep(-7.9, -4.2, 2.6, 6.6),
    'the north screen': keep(2.9, 6.9, 4.7, 7.9),
  };
  for (const k of [...plan, ...kioskSigns('lobby')])
    for (const [name, c] of Object.entries(clearings)) assert.ok(!shares(kioskBox(k), c), `${k.id} is in ${name}`);
  // And none stands where a screen would be stood aside for the car.
  for (const k of plan) assert.deepEqual(clearOfCar(k.x, k.z), { x: k.x, z: k.z }, k.id);
});

test('the server takes a press only on a kiosk that stands in the room', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-kiosks-plan-'));
  const app = createRaxServer({ stateDir: dir, guests: true, listHandles: async () => [] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const open = async (room) => {
    const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?room=${room}&protocol=${PROTOCOL}`);
    ws.got = [];
    ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type === 'panel' || d.type === 'welcome') ws.got.push(d); });
    await once(ws, 'open');
    for (let i = 0; i < 200 && !ws.got.some((d) => d.type === 'welcome' && d.seat >= 0); i++) await delay(10);
    return ws;
  };
  try {
    const a = await open('lobby'), b = await open('lobby'), c = await open('studio'), d = await open('studio');
    const press = (ws, id) => ws.send(JSON.stringify({ type: 'panel', id, name: 'touch', x: 3, y: 4 }));
    press(a, 'yard-99'); // no such kiosk
    press(a, 'car-pad'); // a panel, but the page's own: not a kiosk
    press(a, 'yard-songs-sign'); // a sign is not pressed
    press(a, 'yard-2');
    press(a, 'yard-notepat');
    press(c, 'yard-2'); // a kiosk, but not in this room
    await delay(150);
    assert.deepEqual(b.got.filter((m) => m.type === 'panel').map((m) => m.id), ['yard-2', 'yard-notepat']);
    assert.deepEqual(d.got.filter((m) => m.type === 'panel'), []);
    for (const ws of [a, b, c, d]) ws.close();
    await delay(50);
  } finally {
    await app.close();
  }
  const kept = JSON.parse(await readFile(join(dir, 'kiosks.json'), 'utf8'));
  assert.deepEqual(Object.keys(kept), ['lobby']);
  assert.deepEqual(Object.keys(kept.lobby).sort(), ['yard-2', 'yard-notepat']);
});
