// 🏠 One room (world.mjs ONE_ROOM): everyone lands in the plaza, no way leads
// out of it, everyone builds there, and what a visitor builds is a conjuring
// that fades (2026-11-27).
import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createRaxServer } from '../server.mjs';
import { ObjectStore } from '../object-store.mjs';
import { PROTOCOL, placeFor, playURL, HOME_ROOM } from '../world.mjs';

async function stage(handle, { room = 'lobby', conjureMs = 300 } = {}) {
  const dir = await mkdtemp(join(tmpdir(), 'rax-one-room-'));
  const app = createRaxServer({ oneRoom: true, conjureMs, openBuild: false, stateDir: dir, verifyIdentity: async () => ({ handle, sub: `acct-${handle}` }),
    listHandles: async () => ['jeffrey', 'ada'], roomHosts: { e1n0: 'elsewhere.example' } });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?room=${room}&protocol=${PROTOCOL}&account=1&stay=1`);
  ws.got = [];
  ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type !== 'snapshot') ws.got.push(d); });
  await once(ws, 'open');
  ws.send(JSON.stringify({ type: 'identity' }));
  ws.send(JSON.stringify({ type: 'account', accessToken: 'valid-test-token' }));
  for (let i = 0; i < 300 && !ws.got.some((d) => d.type === 'whereabouts' || d.type === 'home'); i++) await delay(10);
  return { app, ws };
}
const until = async (what, ms = 3000) => { for (let i = 0; i < ms / 10; i++) { if (await what()) return true; await delay(10); } return false; };

test('any place in the world is the one room; a game or a named room is itself', () => {
  for (const place of ['lobby', 'basement', 'roof', 'east', 'e1n0', 'w2s3', 'floor-1a2b3c4d', 'pod-abc']) assert.equal(placeFor(place, true), HOME_ROOM, place);
  for (const other of ['studio', 'botbop', 'botbop-ada']) assert.equal(placeFor(other, true), other);
  assert.equal(placeFor('e1n0', false), 'e1n0', 'with the world open a block is a block');
  const page = { hostname: 'aesthetic.computer', protocol: 'https:' };
  assert.equal(playURL('e1n0', 'https://aesthetic.computer/rax/', page, true).host, 'aesthetic.computer', 'no room is played on another box');
  assert.equal(playURL('e1n0', 'https://aesthetic.computer/rax/', page, false).host, 'nj.rax.ac');
});

test('whatever place is asked for, the body lands on the plaza, walled on all four sides', async () => {
  for (const room of ['e1n0', 'basement', 'roof']) {
    const { app, ws } = await stage('ada', { room });
    try {
      const park = ws.got.find((d) => d.type === 'park');
      assert.deepEqual(park.neighbours, {}, `${room}: no side leads on`);
      assert.equal(park.plaza, true, `${room}: the plaza`);
      assert.equal(park.one, true);
      assert.equal(ws.got.find((d) => d.type === 'welcome').room, HOME_ROOM, `${room}: the room it is in`);
      const where = ws.got.find((d) => d.type === 'whereabouts');
      if (where) assert.equal(where.room, HOME_ROOM);
    } finally { ws.close(); await app.close(); }
  }
});

test('no way leads out: an edge, the car, the stair', async () => {
  const { app, ws } = await stage('ada');
  try {
    ws.got.length = 0;
    for (const edge of ['east', 'west', 'south', 'north']) ws.send(JSON.stringify({ type: 'walk', edge }));
    ws.send(JSON.stringify({ type: 'elevator', to: 'top' }));
    ws.send(JSON.stringify({ type: 'elevator', to: 'b1' }));
    ws.send(JSON.stringify({ type: 'take-floor', number: 3 }));
    await delay(400);
    assert.ok(!ws.got.some((d) => ['travel', 'relocated', 'ride', 'passage'].includes(d.type)), `nothing moved the body: ${ws.got.map((d) => d.type).join(' ')}`);
    assert.ok(!ws.got.some((d) => d.type === 'park' && Object.keys(d.neighbours || {}).length), 'still walled');
  } finally { ws.close(); await app.close(); }
});

test('a visitor paints and calls a screen in, and both fade; the square is as it was', async () => {
  const { app, ws } = await stage('ada');
  try {
    await app.store.paintTile('lobby', 4, 4, 3); // the keeper's, from before
    ws.send(JSON.stringify({ type: 'paint', i: 4, j: 4, c: 5 }));
    ws.send(JSON.stringify({ type: 'paint', i: 6, j: 6, c: 2 }));
    ws.send(JSON.stringify({ type: 'screen', code: 'roz', x: 2, z: 2, yaw: 0 }));
    assert.ok(await until(() => app.store.tiles('lobby')['4,4'] === 5 && app.store.tiles('lobby')['6,6'] === 2), 'painted');
    assert.ok(await until(() => app.store.screens('lobby').some((s) => s.code === 'roz' && s.fades)), 'the screen stands, for a while');
    assert.ok(await until(() => app.store.tiles('lobby')['4,4'] === 3 && !app.store.tiles('lobby')['6,6']), 'the squares are as they were');
    assert.ok(await until(() => !app.store.screens('lobby').some((s) => s.fades)), 'the screen is gone');
  } finally { ws.close(); await app.close(); }
});

test('a visitor takes nothing out and clears nothing; the keeper\'s work stays', async () => {
  const visitor = await stage('ada');
  try {
    for (const message of [{ type: 'clear-room' }, { type: 'remove', objectId: 'anything' }]) {
      visitor.ws.got.length = 0;
      visitor.ws.send(JSON.stringify(message));
      assert.ok(await until(() => visitor.ws.got.some((d) => d.type === 'build-result')), message.type);
      assert.match(visitor.ws.got.find((d) => d.type === 'build-result').error, /keeper/, message.type);
    }
  } finally { visitor.ws.close(); await visitor.app.close(); }
  const { app, ws } = await stage('jeffrey');
  try {
    ws.send(JSON.stringify({ type: 'paint', i: 4, j: 4, c: 3 }));
    assert.ok(await until(() => app.store.tiles('lobby')['4,4'] === 3));
    await delay(700); // (more than two conjurings long)
    assert.deepEqual(app.store.tiles('lobby'), { '4,4': 3 }, 'the keeper\'s square stays');
  } finally { ws.close(); await app.close(); }
});

test('a build that fades is taken out when its time is up, and one that does not is left', async () => {
  const store = new ObjectStore(await mkdtemp(join(tmpdir(), 'rax-fade-')));
  await store.load();
  const shape = { name: 'box', boxes: [{ x: 0, y: 0.5, z: 0, w: 1, h: 1, d: 1, color: 0xff3296 }] };
  const job = (objectId, fades) => ({ objectId, owner: 'someone', room: 'lobby', prompt: 'a box', baseVersion: -1, x: 2, z: 3, fades });
  await store.commit(job('stays', 0), shape);
  const conjured = await store.commit(job('fades', 1000), shape);
  assert.ok(conjured.fades > Date.now());
  assert.deepEqual(await store.fade('lobby', Date.now()), { gone: [], changed: false }, 'not yet');
  const swept = await store.fade('lobby', Date.now() + 2000);
  assert.deepEqual(swept.gone.map((g) => g.id), ['fades']);
  assert.deepEqual(store.objects('lobby').map((o) => o.id).filter((id) => ['stays', 'fades'].includes(id)), ['stays']);
});
