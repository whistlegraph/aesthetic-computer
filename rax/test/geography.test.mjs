import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import {
  BLOCK, LANE, LOTS, LOBBY, blockAt, blockIndex, spiralAt, spiralIndex, isGrounds, blockId, parseBlock, toWorld, fromWorld,
  lot, lotAt, blocksFor, span, Geography, GeographyStore,
} from '../geography.mjs';
import { WORLD, PROTOCOL } from '../world.mjs';
import { GROUNDS } from '../park.mjs';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { createRaxServer } from '../server.mjs';

test('a block is the room the server already runs', () => {
  assert.equal(BLOCK, WORLD * 2);
});

test('the spiral gives every cell one place, and neighbours in order touch', () => {
  const seen = new Set();
  let last = spiralAt(0);
  for (let n = 0; n < 2000; n++) {
    const b = spiralAt(n);
    assert.equal(spiralIndex(b), n);
    const id = blockId(b);
    assert.ok(!seen.has(id), `cell ${n} repeats ${id}`);
    seen.add(id);
    assert.deepEqual(parseBlock(id), b);
    assert.match(id, /^[a-z0-9-]{1,32}$/);
    if (n > 0) assert.equal(Math.abs(b.i - last.i) + Math.abs(b.j - last.j), 1, `cell ${n} touches ${n - 1}`);
    last = b;
  }
  assert.equal(blockId(spiralAt(0)), LOBBY);
  assert.equal(parseBlock('e0n0'), null, 'the lobby has one spelling');
  assert.equal(parseBlock('studio'), null);
});

test('housing blocks keep one place each and are never laid on the tower\'s grounds', () => {
  const seen = new Set();
  for (let n = 0; n < 2000; n++) {
    const b = blockAt(n);
    assert.equal(blockIndex(b), n);
    assert.ok(n === 0 || !isGrounds(b), `block ${n} is on the grounds`);
    assert.ok(!seen.has(blockId(b)), `block ${n} repeats`);
    seen.add(blockId(b));
  }
  // The two blocks settled before the grounds were laid out stay where they were.
  assert.deepEqual([blockId(blockAt(1)), blockId(blockAt(2))], ['e1n0', 'e1n1']);
  for (const [i, j] of GROUNDS) {
    assert.equal(isGrounds({ i, j }), true);
    assert.equal(blockIndex({ i, j }), 0, 'the grounds are open like the plaza');
  }
  assert.equal(isGrounds({ i: 0, j: 0 }), false);
});

test('the grounds open off the plaza on every side but the tower\'s back, two blocks south', () => {
  const g = new Geography({ opened: 2 });
  assert.deepEqual(g.neighbours(LOBBY), { east: 'e1n0', west: 'w1n0', south: 'e0s1' });
  assert.equal(g.neighbours('e0n1').south, undefined, 'no way in through the back of the tower');
  assert.equal(g.neighbours('e0s1').south, 'e0s2');
  assert.equal(g.neighbours('e0s2').south, undefined, 'the promenade ends at the terrace');
  assert.equal(g.neighbours('e1s1').east, undefined, 'housing beyond the grounds opens in its turn');
});

test('world metres and block-local metres convert both ways', () => {
  const block = { i: 3, j: -2 };
  const world = toWorld(block, { x: -7.5, z: 4 });
  assert.deepEqual(world, { x: 40.5, z: -28 });
  assert.deepEqual(fromWorld(world), { block, local: { x: -7.5, z: 4 } });
});

test('four lots fill a block around its two lanes without overlapping', () => {
  let area = 0;
  for (let k = 0; k < LOTS; k++) {
    const r = lot(k);
    area += (r.xMax - r.xMin) * (r.zMax - r.zMin);
    assert.ok(Math.abs(r.xMin) >= LANE / 2 || Math.abs(r.xMax) >= LANE / 2);
    const mid = { x: (r.xMin + r.xMax) / 2, z: (r.zMin + r.zMax) / 2 };
    assert.equal(lotAt(mid), k);
  }
  assert.equal(area, (BLOCK - LANE) ** 2, 'everything but the lanes is a lot');
  assert.equal(lotAt({ x: 0, z: 5 }), -1, 'the north-south lane is nobody’s');
  assert.equal(lotAt({ x: 5, z: 0 }), -1, 'the east-west lane is nobody’s');
});

test('a handle has one home; the next block opens when the open lots are three quarters full', () => {
  const g = new Geography();
  assert.equal(g.opened, 1);
  for (let n = 0; n < 2; n++) g.settle(`@p${n}`);
  assert.equal(g.opened, 1);
  const third = g.settle('p5');
  assert.equal(third.opened, 2, 'three of four opens block two');
  assert.deepEqual(g.settle('@P5'), { ...g.home('p5'), moved: false, opened: 2 }, 'settling again changes nothing');
  assert.equal(Object.keys(g.homes).length, 3);
  // Any free lot in an open block may be chosen; a taken one may not.
  assert.throws(() => g.settle('q', { block: 1, lot: 0 }), /taken/);
  assert.throws(() => g.settle('q', { block: 3, lot: 0 }), /not open/);
  const far = g.settle('q', { block: 2, lot: 3 });
  assert.equal(far.block, 2);
  // Moving frees the old lot and keeps the founding date.
  const moved = g.settle('p0', { block: 2, lot: 0 }, 99);
  assert.ok(moved.moved);
  assert.equal(moved.at, g.home('p0').at);
  assert.ok(!g.taken(1, 0));
});

test('the settled world stays dense as it grows', () => {
  const g = new Geography();
  for (let n = 0; n < 3076; n++) g.settle(`h${n}`);
  const c = g.census();
  assert.equal(c.settled, 3076);
  assert.ok(c.settled / c.lots >= 0.74, `fill ${c.settled / c.lots}`);
  assert.equal(c.opened, blocksFor(3076));
  assert.ok(c.span.seconds <= 55, `crossing takes ${c.span.seconds}s`); // four house-sized lots to a block
  assert.equal(span(1).metres, 48);
});

test('an avatar rests where it stood, in a real block', () => {
  const g = new Geography();
  assert.equal(g.rest('@jeffrey', { room: 'studio', x: 0, z: 0 }), null);
  g.rest('@jeffrey', { room: 'e1n0', x: 3.14159, z: -99, yaw: 91.4 }, 5);
  assert.deepEqual(g.whereabouts('jeffrey'), { room: 'e1n0', x: 3.14, z: -8, yaw: 91, at: 5 });
  assert.deepEqual(g.sleepers('e1n0').map((s) => s.handle), ['jeffrey']);
  assert.deepEqual(g.sleepers(LOBBY), []);
});

test('the ledger survives a restart', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-geo-'));
  const a = new GeographyStore(dir);
  await a.load();
  await a.change((g) => g.settle('@jeffrey'));
  await a.change((g) => g.rest('jeffrey', { room: LOBBY, x: 1, z: 2 }));
  const b = new GeographyStore(dir);
  await b.load();
  assert.deepEqual(b.geography.home('jeffrey').lot, 0);
  assert.equal(b.geography.whereabouts('jeffrey').room, LOBBY);
  assert.ok(JSON.parse(await readFile(join(dir, 'geography.json'), 'utf8')).opened >= 1);
});

test('the server rests a handle on leaving and serves the map; its home is a floor, not a lot', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-geo-srv-'));
  const app = createRaxServer({ stateDir: dir, verifyIdentity: async () => ({ handle: 'mapper' }), listHandles: async () => [] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const port = app.server.address().port;
  const map = async () => {
    const res = await fetch(`http://127.0.0.1:${port}/geography`);
    assert.equal(res.headers.get('access-control-allow-origin'), '*');
    return res.json();
  };
  const wait = async (f) => {
    for (let i = 0; i < 200; i++) {
      if (await f()) return;
      await delay(10);
    }
    throw Error('timeout');
  };
  try {
    const empty = await map();
    assert.equal(empty.settled, 0);
    assert.equal(empty.lotRects.length, LOTS);
    assert.equal(empty.block, BLOCK);
    const ws = new WebSocket(`ws://127.0.0.1:${port}/play?room=lobby&protocol=${PROTOCOL}&account=1`);
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity' }));
    ws.send(JSON.stringify({ type: 'account', accessToken: 'valid-test-token' }));
    await wait(async () => (await map()).awake.some((a) => a.handle === 'mapper' && a.room === LOBBY));
    // 🏢 Homes are floors of the tower now: arriving settles no lot.
    await wait(async () => (await (await fetch(`http://127.0.0.1:${port}/tower`)).json()).count === 1);
    assert.equal((await map()).settled, 0);
    ws.close();
    await wait(async () => (await map()).resting.some((r) => r.handle === 'mapper' && r.room === LOBBY));
    assert.deepEqual((await map()).awake, []);
  } finally {
    await app.close();
  }
});
