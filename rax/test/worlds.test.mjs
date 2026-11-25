// 🌐 One world on two hosts: home keeps the plaza, a satellite keeps the
// block east of it. A walk over the line is handed from one to the other
// with a passage the arriving host fetches from the one that gave it.
import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';

const wait = async (f, what = 'it', ms = 4000) => {
  for (const end = Date.now() + ms; Date.now() < end; await delay(10)) if (f()) return;
  assert.fail(`waited too long for ${what}`);
};

async function twoHosts(t) {
  const ports = {};
  const fetchPassage = async (host, receipt) => {
    const r = await fetch(`http://127.0.0.1:${ports[host]}/passage?receipt=${encodeURIComponent(receipt)}`);
    return r.ok ? r.json() : null;
  };
  const accounts = { 'walker-token': { handle: 'walker', sub: 'acct-walker' }, 'other-token': { handle: 'other', sub: 'acct-other' } };
  const common = { listHandles: async () => ['walker', 'other'], verifyIdentity: async (token) => { if (accounts[token]) return accounts[token]; throw Error('no'); }, fetchPassage };
  const dirs = [await mkdtemp(join(tmpdir(), 'rax-home-')), await mkdtemp(join(tmpdir(), 'rax-sat-'))];
  const home = createRaxServer({ ...common, stateDir: dirs[0], roomHosts: { e1n0: 'sat.test' }, selfHost: 'home.test', homeHost: 'home.test' });
  const sat = createRaxServer({ ...common, stateDir: dirs[1], onlyRooms: ['e1n0'], selfHost: 'sat.test', homeHost: 'home.test' });
  for (const [name, app] of [['home.test', home], ['sat.test', sat]]) {
    await app.ready;
    app.server.listen(0, '127.0.0.1');
    await once(app.server, 'listening');
    ports[name] = app.server.address().port;
  }
  t.after(async () => { await home.close(); await sat.close(); for (const d of dirs) await rm(d, { recursive: true, force: true }); });
  const connect = async (host, room, token, extra = {}) => {
    const ws = new WebSocket(`ws://127.0.0.1:${ports[host]}/play?` + new URLSearchParams({ room, protocol: PROTOCOL, account: '1', ...extra })), messages = [];
    ws.on('message', (raw) => messages.push(JSON.parse(raw)));
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity' }));
    ws.send(JSON.stringify({ type: 'account', accessToken: token }));
    t.after(() => ws.close());
    return { ws, messages, say: (m) => ws.send(JSON.stringify(m)) };
  };
  const bodyOf = (app, room, handle) => {
    const r = app.rooms.get(room), m = r && [...r.sessions.values()].find((s) => s.seat >= 0 && s.handle === handle);
    return m ? r.match.seats[m.seat].player : null;
  };
  return { home, sat, ports, connect, bodyOf };
}

test('a walk from the plaza into a block kept on another host, and back', async (t) => {
  const { home, sat, ports, connect, bodyOf } = await twoHosts(t);
  const a = await connect('home.test', 'lobby', 'walker-token', { stay: '1' });
  await wait(() => bodyOf(home, 'lobby', 'walker'), 'a body at home');
  await delay(300);
  Object.assign(bodyOf(home, 'lobby', 'walker'), { x: 8.3, z: 0.4, y: 1.7, yaw: 90, vx: 6 });
  a.say({ type: 'walk', edge: 'east', in: 'lobby' });
  await wait(() => a.messages.some((m) => m.type === 'travel'), 'the travel out');
  const out = a.messages.find((m) => m.type === 'travel');
  assert.equal(out.room, 'e1n0');
  assert.notEqual(out.here, true, 'not moved on the same socket: the room is kept elsewhere');
  assert.match(out.passage, /^home\.test\|/, 'the receipt names the host that gave it');
  assert.equal(home.rooms.has('e1n0'), false, 'home never opened the block');
  // The page opens the block where it is kept, with the passage.
  const b = await connect('sat.test', 'e1n0', 'walker-token', { stay: '1', passage: out.passage });
  await wait(() => { const p = bodyOf(sat, 'e1n0', 'walker'); return p && p.x < -6; }, 'the body over the line on the satellite');
  assert.ok(Math.abs(bodyOf(sat, 'e1n0', 'walker').x + 7.7) < 0.6, `set down just over the line (x ${bodyOf(sat, 'e1n0', 'walker').x.toFixed(2)})`);
  assert.equal((await fetch(`http://127.0.0.1:${ports['home.test']}/passage?receipt=${encodeURIComponent(out.passage)}`)).status, 404, 'a passage is handed over once');
  // And back: the satellite hands the walker home the same way.
  a.ws.close();
  await wait(() => !bodyOf(home, 'lobby', 'walker'), 'the old seat at home to go');
  Object.assign(bodyOf(sat, 'e1n0', 'walker'), { x: -8.3, z: 0.4, yaw: 270, vx: -6 });
  b.say({ type: 'walk', edge: 'west', in: 'e1n0' });
  await wait(() => b.messages.some((m) => m.type === 'travel'), 'the travel back');
  const back = b.messages.find((m) => m.type === 'travel');
  assert.equal(back.room, 'lobby');
  assert.match(back.passage, /^sat\.test\|/);
  await connect('home.test', 'lobby', 'walker-token', { stay: '1', passage: back.passage });
  await wait(() => { const p = bodyOf(home, 'lobby', 'walker'); return p && p.x > 6; }, 'the body back at home');
  { const x = bodyOf(home, 'lobby', 'walker').x; assert.ok(x > 7 && x < 8.6, `set down just inside the plaza, where its walls allow (x ${x.toFixed(2)})`); }
});

test('a passage from another host seats only the account it was given to', async (t) => {
  const { home, sat, connect, bodyOf } = await twoHosts(t);
  const a = await connect('home.test', 'lobby', 'walker-token', { stay: '1' });
  await wait(() => bodyOf(home, 'lobby', 'walker'));
  Object.assign(bodyOf(home, 'lobby', 'walker'), { x: 8.3, z: 0, y: 1.7, yaw: 90 });
  a.say({ type: 'walk', edge: 'east', in: 'lobby' });
  await wait(() => a.messages.some((m) => m.type === 'travel'));
  const { passage } = a.messages.find((m) => m.type === 'travel');
  await connect('sat.test', 'e1n0', 'other-token', { stay: '1', passage });
  await delay(600);
  const theirs = bodyOf(sat, 'e1n0', 'other');
  assert.ok(!theirs || theirs.x > -6, 'someone else\'s passage does not set them down at the line');
});

test('a satellite keeps only its own rooms, and home never moves a walker into them on the same socket', async (t) => {
  const { sat } = await twoHosts(t);
  const port = sat.server.address().port;
  const ws = new WebSocket(`ws://127.0.0.1:${port}/play?room=lobby&protocol=${PROTOCOL}`);
  const [code, why] = await once(ws, 'close');
  assert.equal(code, 1008);
  assert.equal(String(why), 'Not kept here');
});
