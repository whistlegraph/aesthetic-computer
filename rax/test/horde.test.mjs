import test from 'node:test';
import assert from 'node:assert/strict';
import { createHorde, ZOMBIE } from '../horde.mjs';
import { Match } from '../match.mjs';
import { SPAN } from '../park.mjs';
import { HZ } from '../world.mjs';

// 🧟 Zombies walk the lap on the world's clock, cross from block to block,
// and a round that meets one gibs it.
const seeded = (s = 7) => () => ((s = (s * 16807) % 2147483647) - 1) / 2147483646;

test('a zombie walks on into the next block over a line, and turns back where none is open', () => {
  const ring = { east: 'b', west: null, north: null, south: null };
  const h = createHorde({ neighbours: (k) => (k === 'a' ? ring : { west: 'a' }), start: ['a'], count: 1, random: seeded() });
  const z = h.zombies[0];
  z.x = SPAN / 2 - 0.01; z.yaw = 90; z.turn = 1e6;
  h.step(); h.step();
  assert.equal(z.room, 'b');
  assert.ok(z.x < -SPAN / 2 + 1, `came in at the far side (${z.x.toFixed(2)})`);
  z.room = 'a'; z.x = -SPAN / 2 + 0.01; z.yaw = 270; z.turn = 1e6;
  for (let k = 0; k < 4; k++) h.step();
  assert.equal(z.room, 'a');
  assert.ok(z.x >= -SPAN / 2 - 1e-6, 'stopped at the closed edge');
});

test('over time they wander through the lattice and stay inside it', () => {
  const near = (k) => ({ east: k, west: k, north: k, south: k });
  const h = createHorde({ neighbours: near, start: ['a'], count: 6, random: seeded(3) });
  for (let k = 0; k < 6000; k++) h.step();
  for (const z of h.zombies) assert.ok(Math.abs(z.x) <= SPAN / 2 + 1 && Math.abs(z.z) <= SPAN / 2 + 1);
  assert.equal(h.zombies.length, 6);
});

test('a killed zombie is let loose again after a few seconds', () => {
  const h = createHorde({ neighbours: () => null, start: ['a'], count: 2, random: seeded() });
  assert.ok(h.kill(h.zombies[0].id));
  assert.equal(h.zombies.length, 1);
  for (let k = 0; k < Math.ceil(ZOMBIE.respawn * HZ) + 5; k++) h.step();
  assert.equal(h.zombies.length, 2);
});

test('a round through a zombie gibs it, and the match reports who to take off the horde', () => {
  const m = new Match();
  const h = createHorde({ neighbours: () => null, start: ['a'], count: 1, random: seeded() });
  const z = h.zombies[0];
  z.x = 0; z.z = 3;
  m.zombies = h.inRoom('a');
  m.bullets.push({ x: 0, y: 1.5, z: 0, vx: 0, vy: 0, vz: 32, owner: -1, born: 0, safeUntil: 0, bounces: 0, id: 1 });
  for (let k = 0; k < 12; k++) m.stepBullets();
  const gib = m.events.find((e) => e.type === 'gib');
  assert.ok(gib && gib.zombie, 'a gib event');
  assert.deepEqual(m.zombieKills, [z.id]);
  assert.equal(m.zombies.length, 0);
  assert.ok(Math.abs(gib.at.z - 3) < 0.6, 'at the zombie');
});

test('a round that misses a zombie leaves it standing', () => {
  const m = new Match();
  const h = createHorde({ neighbours: () => null, start: ['a'], count: 1, random: seeded() });
  h.zombies[0].x = 4; h.zombies[0].z = 3;
  m.zombies = h.inRoom('a');
  m.bullets.push({ x: 0, y: 1.5, z: 0, vx: 0, vy: 0, vz: 32, owner: -1, born: 0, safeUntil: 0, bounces: 0, id: 1 });
  for (let k = 0; k < 12; k++) m.stepBullets();
  assert.equal(m.zombieKills.length, 0);
});

test('on a real server: a walker reaches the page in beyond, and a round through it gibs it', async (t) => {
  const { once } = await import('node:events');
  const { mkdtemp, rm } = await import('node:fs/promises');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const { setTimeout: delay } = await import('node:timers/promises');
  const { default: WebSocket } = await import('ws');
  const { createRaxServer } = await import('../server.mjs');
  const { PROTOCOL } = await import('../world.mjs');
  const dir = await mkdtemp(join(tmpdir(), 'rax-horde-'));
  const app = createRaxServer({ stateDir: dir, listHandles: async () => ['walker'], verifyIdentity: async () => ({ handle: 'walker', sub: 'acct-walker' }) });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  t.after(async () => { await app.close(); await rm(dir, { recursive: true, force: true }); });
  const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?` + new URLSearchParams({ room: 'lobby', protocol: PROTOCOL, account: '1', stay: '1' }));
  const seen = [];
  ws.on('message', (raw) => seen.push(JSON.parse(raw)));
  await once(ws, 'open');
  t.after(() => ws.close());
  const wait = async (f, what) => { for (const end = Date.now() + 5000; Date.now() < end; await delay(20)) if (f()) return; assert.fail(`waited too long for ${what}`); };
  assert.ok(app.horde.zombies.length >= 1, 'the server has a horde');
  await wait(() => seen.some((m) => m.type === 'beyond' && m.people?.some((p) => String(p[0]).startsWith('z'))), 'a zombie in beyond');
  const row = seen.filter((m) => m.type === 'beyond').flatMap((m) => m.people).find((p) => String(p[0]).startsWith('z'));
  assert.equal(row.length, 12);
  // Stand one in the lobby, in front of a round, and fire.
  const z = app.horde.zombies[0], before = app.horde.zombies.length;
  Object.assign(z, { room: 'lobby', x: 2, z: 3, turn: 1e6 });
  const match = app.rooms.get('lobby').match;
  match.bullets.push({ x: 2, y: 1.5, z: 0, vx: 0, vy: 0, vz: 32, owner: -1, born: match.tick, safeUntil: 0, bounces: 0, id: 999 });
  await wait(() => seen.some((m) => m.events?.some((e) => e.type === 'gib' && e.zombie)) || seen.some((m) => JSON.stringify(m).includes('"zombie":true')), 'the gib reaching the page');
  assert.ok(!app.horde.zombies.includes(z), 'taken off the horde');
  assert.ok(app.horde.zombies.length <= before, 'one fewer until it is let loose again');
});
