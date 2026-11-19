import { geometry } from '../objects.mjs';
import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { WebSocket, WebSocketServer } from 'ws';
import { createServer } from 'node:http';
import { attachSocketRoute } from '../../lith/socket-routing.mjs';
import { Match, INPUT_BATCH, INPUT_QUEUE, INPUT_GRACE } from '../match.mjs';
import { createRaxServer } from '../server.mjs';
import { spawn, spawnPoint, move, HZ, PROTOCOL, FRAG_LIMIT, MAX_PLAYERS } from '../world.mjs';
import { command, reconcile, interpolate } from '../net.mjs';

const cmd = (seq, extra = {}) => ({ seq, fwd: 0, right: 0, yaw: 0, pitch: 0,
  buttons: 0, fire: false, ...extra });
function playing() {
  const m = new Match(); m.join(); m.join();
  for (let i = 0; i < HZ * 3; i++) m.step();
  return m;
}
function shoot(m, seat = 0, extra = {}) {
  const s = m.seats[seat];
  m.input(seat, [cmd(s.ack + 1, { yaw: seat ? 180 : 0, fire: true, ...extra })]);
  m.step();
}

test('one simulation produces identical prediction and authority; ack replay converges', () => {
  const m = new Match(); m.join();
  let predicted = spawn(0);
  const pending = [];
  for (let seq = 1; seq <= 240; seq++) {
    const c = cmd(seq, { fwd: 1, right: seq % 60 < 30 ? 1 : -1, yaw: seq % 360, buttons: seq % 29 === 0 ? 1 : 0 });
    predicted = move(predicted, c); pending.push(c);
    m.input(0, [c]); m.step();
    assert.equal(predicted.x, m.seats[0].player.x);
    assert.equal(predicted.y, m.seats[0].player.y);
    assert.equal(predicted.z, m.seats[0].player.z);
    if (seq % 6 === 0) {
      const result = reconcile(m.seats[0].player, pending, m.seats[0].ack);
      assert.equal(result.pending.length, 0);
      assert.deepEqual(result.player, m.seats[0].player);
    }
  }
  const base = spawn(0), commands = [cmd(1, { fwd: 1 }), cmd(2, { right: 1 })];
  const server = move(base, commands[0]);
  assert.deepEqual(reconcile(server, commands, 1).player, commands.reduce((p, c) => move(p, c), base));
});

test('input validation, duplicate/reordered commands and flooding cannot buy simulation time', () => {
  assert.equal(command(cmd(1, { yaw: NaN })), null);
  assert.equal(command(cmd(1, { fwd: Infinity })), null);
  assert.equal(command(cmd(-1)), null);
  assert.equal(command(cmd(1, { dt: 99, health: 999 })).dt, undefined);
  const m = new Match(); m.join();
  const c = cmd(1, { fwd: 1 });
  for (let i = 0; i < 100; i++) m.input(0, [c, cmd(3), cmd(2)]);
  assert.equal(m.seats[0].queue.length, 3);
  m.step(); assert.equal(m.seats[0].ack, 1);
  assert.equal(m.seats[0].player.z, move(spawn(0), c).z);
  m.step(); assert.equal(m.seats[0].ack, 2);
  m.input(0, [cmd(100000)]);
  assert.equal(m.seats[0].queue.length, 1);
  const before = m.tick;
  for (let seq = 4; seq < 100; seq++) m.input(0, [cmd(seq, { fwd: 1 })]);
  assert.ok(m.seats[0].queue.length <= INPUT_QUEUE);
  m.step(); assert.equal(m.tick, before + 1);
});

test('late commands are run in the order sent: the server ends where the client predicted', () => {
  const m = new Match(); m.join();
  let local = spawn(0);
  const transit = [];
  let last = 0;
  // Every command is delivered in order (a socket is a stream), but late and
  // in clumps: up to nine ticks behind.
  for (let seq = 1; seq <= 300; seq++) {
    const c = cmd(seq, { fwd: seq % 50 < 40 ? 1 : 0, right: seq % 70 < 20 ? 1 : 0, yaw: (seq * 3) % 360 });
    local = move(local, c);
    last = Math.max(last, seq + [0, 0, 1, 9, 8, 7, 0, 3, 2, 1][seq % 10]);
    transit.push({ due: last, commands: [c] });
    for (const packet of transit.filter((p) => p.due === seq)) m.input(0, packet.commands);
    m.step();
  }
  for (const packet of transit.filter((p) => p.due > 300)) m.input(0, packet.commands);
  for (let i = 0; i < 12; i++) m.step();
  assert.equal(m.seats[0].ack, 300);
  const p = m.seats[0].player;
  assert.ok(Math.hypot(p.x - local.x, p.z - local.z) < 1e-9, 'no command was dropped or replaced by an idle tick');
});

test('a flood cannot run more commands than the time that has passed, plus the grace', () => {
  const m = new Match(); m.join();
  let seq = 0;
  for (let tick = 0; tick < 100; tick++) {
    // Twelve fresh commands every tick: twelve times real time, if it were allowed.
    m.input(0, Array.from({ length: INPUT_BATCH }, () => cmd(++seq, { fwd: 1 })));
    m.step();
  }
  const still = spawn(0);
  let honest = still;
  // A twentieth over real time while a backlog waits, and the grace once.
  for (let i = 0; i < 105 + INPUT_GRACE + 1; i++) honest = move(honest, cmd(1, { fwd: 1 }));
  const p = m.seats[0].player;
  assert.ok(Math.hypot(p.x - still.x, p.z - still.z) <= Math.hypot(honest.x - still.x, honest.z - still.z) + 1e-9);
});

test('after an outage nothing is left waiting: the queue never becomes standing latency', () => {
  const m = new Match(); m.join();
  let seq = 0;
  for (let i = 0; i < 60; i++) { m.input(0, [cmd(++seq, { fwd: 1 })]); m.step(); }
  // Half a second of nothing, then everything at once (in batches, as sent).
  const late = [];
  for (let i = 0; i < 28; i++) { late.push(cmd(++seq, { fwd: 1 })); m.step(); }
  for (let i = 0; i < late.length; i += INPUT_BATCH) m.input(0, late.slice(i, i + INPUT_BATCH));
  m.input(0, [cmd(++seq, { fwd: 1 })]); m.step();
  assert.equal(m.seats[0].ack, seq, 'the whole burst ran on the tick it landed');
  for (let i = 0; i < 120; i++) { m.input(0, [cmd(++seq, { fwd: 1 })]); m.step(); assert.ok(m.seats[0].queue.length <= 1); }
  let local = spawn(0);
  for (let i = 0; i < seq; i++) local = move(local, cmd(1, { fwd: 1 }));
  assert.ok(Math.abs(m.seats[0].player.z - local.z) < 1e-9, 'and nothing was dropped');
});

test('a player who goes silent is held for the grace, then the body runs on', () => {
  const m = new Match(); m.join();
  for (let seq = 1; seq <= 30; seq++) { m.input(0, [cmd(seq, { fwd: 1 })]); m.step(); }
  const moving = { ...m.seats[0].player };
  for (let i = 0; i < INPUT_GRACE; i++) m.step();
  assert.equal(m.seats[0].player.z, moving.z, 'held still while the commands may only be late');
  for (let i = 0; i < 30; i++) m.step();
  assert.notEqual(m.seats[0].player.z, moving.z, 'then friction takes over');
});

test('three-command backup recovers dropped packets and reconciles under jitter', () => {
  const m = new Match(); m.join();
  let local = spawn(0), pending = [];
  const transit = [];
  for (let seq = 1; seq <= 361; seq++) {
    const c = cmd(seq, { fwd: seq < 300 ? 1 : 0, yaw: seq % 90 });
    pending.push(c); local = move(local, c);
    if (seq % 4) transit.push({ due: seq + seq % 5, commands: pending.slice(-3) });
    for (const packet of transit.filter(p => p.due === seq)) m.input(0, packet.commands);
    m.step();
    if (seq % 3 === 0) {
      const result = reconcile(m.seats[0].player, pending, m.seats[0].ack);
      local = result.player; pending = result.pending;
      assert.ok(pending.length < 20);
      assert.ok(Number.isFinite(local.x));
    }
  }
  for (const packet of transit.filter(p => p.due > 361)) m.input(0, packet.commands);
  for (let i = 0; i < 12; i++) m.step();
  const result = reconcile(m.seats[0].player, pending, m.seats[0].ack);
  assert.equal(result.pending.length, 0);
  assert.deepEqual(result.player, m.seats[0].player);
});

test('rocket travel, cooldown, respawn and first-to-ten match', () => {
  const m = playing(); shoot(m);
  assert.equal(m.seats[1].player.alive, true, 'a rocket is not an instant hit');
  assert.equal(m.rockets.length, 1);
  shoot(m); assert.equal(m.rockets.length, 1, 'cooldown blocks another launch');
  for (let i = 0; i < HZ; i++) m.step();
  assert.equal(m.seats[0].player.score, 1);
  assert.equal(m.seats[1].player.alive, false);
  for (let i = 0; i < HZ; i++) m.step();
  assert.equal(m.seats[1].player.health, 100);
  assert.equal(m.seats[1].player.alive, true);
  m.seats[0].player.score = FRAG_LIMIT - 1;
  shoot(m);
  while (m.phase === 'playing' && m.rockets.length) m.step();
  assert.equal(m.phase, 'finished'); assert.equal(m.winner, 0);
  for (let i = 0; i < HZ * 5; i++) m.step();
  assert.equal(m.phase, 'countdown'); assert.equal(m.seats[0].player.score, 0);
});

// The arena has no fixed cover any more; a built object stands in for it.
const block = {
  id: 'cover',
  x: 6,
  z: 0,
  geometry: geometry({ name: 'Cover', boxes: [{ x: 0, y: 3, z: 0, w: 2, h: 6, d: 6 }] }),
};
test('cover stops rockets and warmup permits firing without scoring', () => {
  const cover = playing();
  cover.objects = [block];
  cover.seats[0].player.x = cover.seats[1].player.x = 6;
  shoot(cover);
  for (let i = 0; i < HZ; i++) cover.step();
  assert.equal(cover.seats[1].player.health, 100);
  assert.equal(cover.rockets.length, 0);
  const warmup = new Match(); warmup.join(); shoot(warmup);
  assert.equal(warmup.rockets.length, 1);
  assert.equal(warmup.seats[0].player.score, 0);
});

test('rockets sweep into targets, splash respects cover, and self splash gives jump impulse', () => {
  const m = playing(); shoot(m);
  for (let i = 0; i < HZ; i++) m.step();
  assert.equal(m.seats[0].player.score, 1);
  assert.equal(m.seats[1].player.alive, false);
  const jump = playing(); shoot(jump, 0, { pitch: -85 });
  for (let i = 0; i < 5; i++) jump.step();
  assert.ok(jump.seats[0].player.vy > 0);
  assert.ok(jump.seats[0].player.health < 100);
  const cover = playing();
  cover.objects = [block];
  Object.assign(cover.seats[1].player, { x: 7.7, z: 0 });
  cover.explode({ x: 4.99, y: 1, z: 0, owner: 0 }, -1);
  assert.equal(cover.seats[1].player.health, 100);
});

test('interpolation uses shortest yaw, freezes on starvation and never lerps across respawns', () => {
  const a = { tick: 10, players: [{ ...spawn(0), x: 0, yaw: 350 }] };
  const b = { tick: 20, players: [{ ...spawn(0), x: 10, yaw: 10 }] };
  assert.equal(interpolate([a, b], 15, 0).x, 5);
  assert.equal(interpolate([a, b], 15, 0).yaw, 360);
  assert.equal(interpolate([a, b], 100, 0).x, 10);
  b.players[0].life++;
  assert.equal(interpolate([a, b], 15, 0).x, 0);
  assert.equal(interpolate([a, b], 20, 0).x, 10);
});

test('the last rival leaving clears the score of the one left behind and frees the seat; nobody is moved by a coming or a going', () => {
  const m = playing();
  shoot(m);
  Object.assign(m.seats[0].player, { x: 3.3, z: -2.2 });
  const life = m.seats[0].player.life;
  m.leave(1);
  assert.equal(m.phase, 'waiting'); assert.equal(m.seats[0].player.score, 0);
  assert.equal(m.join(), 1); assert.equal(m.phase, 'playing');
  assert.deepEqual([m.seats[0].player.x, m.seats[0].player.z, m.seats[0].player.life], [3.3, -2.2, life]);
});

test('a room holds many players; late arrivals join the running match', () => {
  const m = playing();
  m.seats[0].player.score = 3;
  assert.equal(m.join(), 2);
  assert.equal(m.phase, 'playing', 'a third arrival does not restart the match');
  assert.equal(m.seats[0].player.score, 3);
  while (m.join() >= 0);
  assert.equal(m.count, MAX_PLAYERS);
  assert.equal(m.join(), -1, 'the room refuses a seat past MAX_PLAYERS');
  m.leave(5);
  assert.equal(m.phase, 'playing', 'one departure among many keeps the match');
  assert.equal(m.seats[0].player.score, 3);
  assert.equal(m.join(), 5, 'the freed seat is reused');
  const snap = m.snapshot();
  assert.equal(snap.players.filter(Boolean).length, MAX_PLAYERS);
  assert.equal(snap.acks.length, MAX_PLAYERS);
});

test('every seat spawns inside the walls, apart, facing the middle', () => {
  const points = Array.from({ length: MAX_PLAYERS }, (_, s) => spawnPoint(s));
  assert.deepEqual(points[0], { x: 0, z: -5, yaw: 0 });
  assert.deepEqual(points[1], { x: 0, z: 5, yaw: 180 });
  for (const [i, p] of points.entries()) {
    assert.ok(Math.abs(p.x) < 8 && Math.abs(p.z) < 8, 'seat ' + i + ' inside');
    for (const q of points.slice(i + 1)) assert.ok(Math.hypot(p.x - q.x, p.z - q.z) > 1.5, 'seat ' + i + ' apart');
    const toCenter = Math.atan2(-p.x, -p.z) * 180 / Math.PI;
    assert.ok(Math.abs(((toCenter - p.yaw + 540) % 360) - 180) < 0.1, 'seat ' + i + ' faces in');
  }
});

test('a third player\'s rocket frags under the free-for-all score', () => {
  const m = playing(); m.join();
  const shooter = m.seats[2].player, target = m.seats[0].player;
  target.health = 1;
  m.damage(0, 5, 2);
  assert.equal(m.seats[0].player.alive, false);
  assert.equal(m.seats[2].player.score, 1);
});

test('HTTP allowlist, actual two-socket match, full room, room isolation and cleanup', async t => {
  const app = createRaxServer({ guests: true }); t.after(() => app.close());
  app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
  const base = `http://127.0.0.1:${app.server.address().port}`;
  assert.equal((await fetch(base)).status, 200);
  assert.equal((await fetch(`${base}/server.mjs`)).status, 404);
  assert.equal((await fetch(`${base}/system/public/aesthetic.computer/lib/pmove.mjs`)).status, 200);
  const connect = (room = 'test', legacyMode = '') => {
    const ws = new WebSocket(`${base.replace('http', 'ws')}/play?room=${room}&mode=${legacyMode}&protocol=${PROTOCOL}`);
    ws.messages = []; ws.on('message', b => ws.messages.push(JSON.parse(b))); return ws;
  };
  const waitFor = async (ws, predicate) => {
    const abort = AbortSignal.timeout(3000);
    while (!ws.messages.some(predicate)) await once(ws, 'message', { signal: abort });
    return ws.messages.find(predicate);
  };
  const a = connect(); assert.equal((await waitFor(a, d => d.type === 'welcome')).seat, 0);
  const b = connect(); assert.equal((await waitFor(b, d => d.type === 'welcome')).seat, 1);
  await waitFor(a, d => d.phase === 'playing');
  const c = connect('test', 'instagib'); assert.equal((await waitFor(c, d => d.type === 'welcome')).seat, 2, 'a third socket plays too'); c.close(); await once(c, 'close');
  const other = connect('other-room'); assert.equal((await waitFor(other, d => d.type === 'welcome')).seat, 0);
  a.send(JSON.stringify({ type: 'input', commands: [cmd(1)] }));
  await waitFor(a, d => d.acks?.[0] === 1);
  b.close(); await once(b, 'close');
  await waitFor(a, d => d.phase === 'waiting' && d.tick > 2);
  const replacement = connect(); assert.equal((await waitFor(replacement, d => d.type === 'welcome')).seat, 1);
});

test('rax mounts below a prefix and shares HTTP and WebSockets without taking over other routes', async t => {
  const app = createRaxServer({ guests: true, prefix: '/rax' });
  const server = createServer((req, res) => {
    if (req.url.startsWith('/rax')) return app.handle(req, res);
    res.end('host application');
  });
  app.attach(server);
  const other = new WebSocketServer({ noServer: true });
  const detach = attachSocketRoute(server, ['/other'], (req, socket, head) => other.handleUpgrade(req, socket, head, ws => {
    ws.send('other service');
  }));
  t.after(async () => {
    await app.close(); detach();
    for (const ws of other.clients) ws.terminate();
    other.close(); server.close();
  });
  server.listen(0, '127.0.0.1'); await once(server, 'listening');
  const base = `http://127.0.0.1:${server.address().port}`;
  const redirect = await fetch(`${base}/rax?room=test`, { redirect: 'manual' });
  assert.equal(redirect.status, 307);
  assert.equal(redirect.headers.get('location'), '/rax/?room=test');
  assert.match(await (await fetch(`${base}/rax/`)).text(), /<title>rax/);
  assert.equal((await fetch(`${base}/rax/pmove.mjs`)).status, 200);
  for (const module of ['scene3d', 'scene3d-webgpu', 'scene3d-webgl', 'block-font']) {
    const response = await fetch(`${base}/rax/engine/${module}.mjs`);
    assert.equal(response.status, 200);
    assert.match(response.headers.get('content-type'), /javascript/);
  }
  assert.equal((await (await fetch(`${base}/rax/health`)).json()).protocol, PROTOCOL);
  assert.equal(await (await fetch(base)).text(), 'host application');
  const wsBase = base.replace('http', 'ws');
  const player = new WebSocket(`${wsBase}/rax/play?room=mount&protocol=${PROTOCOL}`, { origin: base });
  const [message] = await once(player, 'message');
  assert.equal(JSON.parse(message).type, 'welcome');
  player.close(); await once(player, 'close');
  const peer = new WebSocket(`${wsBase}/other`);
  const [response] = await once(peer, 'message');
  assert.equal(response.toString(), 'other service');
  peer.close(); await once(peer, 'close');
  const rejected = new WebSocket(`${wsBase}/rax/play?room=mount&protocol=${PROTOCOL}`, { origin: 'https://unrelated.example' });
  await once(rejected, 'error');
});

test('the mount it had before the rename still lands: pages are sent on, sockets and the old protocol are taken', async t => {
  const app = createRaxServer({ guests: true, prefix: '/rax', alias: '/dool' });
  const server = createServer((req, res) => app.handle(req, res));
  app.attach(server);
  t.after(async () => { await app.close(); server.close(); });
  server.listen(0, '127.0.0.1'); await once(server, 'listening');
  const base = `http://127.0.0.1:${server.address().port}`;
  for (const [from, to] of [['/dool', '/rax/'], ['/dool/?embed=prompt', '/rax/?embed=prompt'], ['/dool/map', '/rax/map']]) {
    const r = await fetch(base + from, { redirect: 'manual' });
    assert.equal(r.status, 307);
    assert.equal(r.headers.get('location'), to);
  }
  assert.equal((await (await fetch(`${base}/dool/health`)).json()).name, 'rax');
  for (const url of [`/dool/play?room=mount&protocol=duel-6`, `/rax/play?room=mount&protocol=${PROTOCOL}`]) {
    const player = new WebSocket(base.replace('http', 'ws') + url, { origin: base });
    const [message] = await once(player, 'message');
    assert.equal(JSON.parse(message).type, 'welcome');
    player.close(); await once(player, 'close');
  }
  assert.throws(() => createRaxServer({ alias: '/dool' }), /Invalid mount path/);
});

// 🍄 botbop
import { BOP, lands, LEDGES, isArena, arenaSpawn } from '../bop.mjs';
const idle = (m, seat) => m.input(seat, [cmd(m.seats[seat].ack + 1)]);
function land(m, seat = 0) {
  for (let i = 0; i < 240 && !m.seats[seat].player.onGround; i++) { for (const s of m.seats.filter(Boolean)) idle(m, s.player.seat); m.step(); }
  for (const s of m.seats.filter(Boolean)) idle(m, s.player.seat); m.step();
}
function over(m, height = 1.9) {
  const [a, q] = [m.seats[0].player, m.seats[1].player];
  Object.assign(a, { x: q.x, z: q.z, y: q.y + height, vy: -3, onGround: false });
}
test('botbop: coming down on a head bops it, bounces the stomper and counts', () => {
  const m = playing();
  over(m);
  for (let i = 0; i < 20 && !m.seats[0].combo && !m.seats[0].bops; i++) { idle(m, 0); idle(m, 1); m.step(); }
  assert.deepEqual([m.seats[0].combo, m.seats[0].pending, m.seats[0].bops || 0], [1, 1, 0], 'in the air it is pending');
  assert.ok(m.seats[0].player.vy > 0, 'the stomper leaves upward');
  assert.ok(m.seats[1].player.alive, 'outside the arena the bopped player stays');
  const e = m.events.find((x) => x.type === 'bop');
  assert.deepEqual([e.seat, e.attacker, e.combo, e.points], [1, 0, 1, 1]);
  assert.deepEqual(m.snapshot().combos[0], [1, 1]);
  land(m);
  assert.deepEqual(m.snapshot().bops.slice(0, 2), [1, 0]);
  assert.equal(m.snapshot().combos[0], 0);
  assert.equal(m.events.find((x) => x.type === 'bank').points, 1);
});
test('botbop: no bop from the side, from below, or while the head rests', () => {
  const m = playing();
  const [a, q] = [m.seats[0].player, m.seats[1].player];
  assert.equal(lands({ ...a, x: q.x + 2, z: q.z, y: q.y + 1.5, vy: -3, onGround: false }, q), false);
  assert.equal(lands({ ...a, x: q.x, z: q.z, y: q.y + 0.5, vy: -3, onGround: false }, q), false);
  assert.equal(lands({ ...a, x: q.x, z: q.z, y: q.y + 1.5, vy: 3, onGround: false }, q), false);
  assert.equal(lands({ ...a, x: q.x, z: q.z, y: q.y + 1.5, vy: -3, onGround: false }, q), true);
  over(m);
  for (let i = 0; i < 20 && !m.seats[0].combo && !m.seats[0].bops; i++) { idle(m, 0); idle(m, 1); m.step(); }
  over(m, 1.5); idle(m, 0); idle(m, 1); m.step();
  assert.equal(m.seats[0].combo, 1, 'a second landing inside the rest does not count');
  assert.ok(BOP.rest > 1);
});
test('botbop arena: one plane, no countdown, a bopped body comes back elsewhere', () => {
  assert.ok(isArena('botbop') && isArena('botbop-2') && isArena('hedbop') && !isArena('lobby'));
  const m = new Match(); m.setArena(LEDGES); m.join(); m.join();
  assert.equal(m.phase, 'playing');
  for (let i = 0; i < 90; i++) {
    m.input(0, [cmd(m.seats[0].ack + 1, { fwd: 1, yaw: 37, buttons: i % 20 === 0 ? 1 : 0 })]);
    idle(m, 1); m.step();
    assert.equal(m.seats[0].player.z, 0);
  }
  over(m);
  for (let i = 0; i < 20 && !m.seats[0].combo && !m.seats[0].bops; i++) { idle(m, 0); idle(m, 1); m.step(); }
  assert.equal(m.seats[1].player.alive, false);
  const life = m.seats[1].player.life;
  for (let i = 0; i < HZ + 2; i++) { idle(m, 0); idle(m, 1); m.step(); }
  assert.ok(m.seats[1].player.alive);
  assert.equal(m.seats[1].player.life, life + 1);
  assert.deepEqual(arenaSpawn(1, life + 1).z, 0);
  m.leave(1);
  assert.equal(m.phase, 'playing', 'alone in the arena is still play');
});
test('botbop arena: a ledge is jumped through from below and stood on from above', () => {
  const l = LEDGES[0], x = (l.xMin + l.xMax) / 2;
  let p = { ...spawn(0), x, z: 0 }, top = 0, prediction;
  for (let i = 0; i < 200; i++) {
    p = move(p, cmd(i + 1, { buttons: i === 0 || (p.onGround && i < 60) ? 1 : 0 }), [], i / HZ, LEDGES, true);
    top = Math.max(top, p.y - 1.7);
  }
  assert.ok(top > l.yMax, 'the jump passes up through the ledge');
  assert.ok(Math.abs(p.y - 1.7 - l.yMax) < 1e-6 && p.onGround, `rests on the ledge (${p.y - 1.7})`);
  // Walking off the end falls to the floor.
  for (let i = 0; i < 240; i++) p = move(p, cmd(i + 1, { fwd: 1, yaw: 270 }), [], i / HZ, LEDGES, true);
  assert.ok(Math.abs(p.y - 1.7) < 1e-6 && p.onGround);
  // Prediction and authority agree in the arena.
  const m = new Match(); m.setArena(LEDGES); m.join();
  m.drones = []; // a bop is the server's doing, as a blast is
  prediction = { ...m.seats[0].player };
  for (let seq = 1; seq <= 300; seq++) {
    const c = cmd(seq, { fwd: 1, yaw: seq % 140 < 70 ? 90 : 270, buttons: seq % 23 === 0 ? 1 : 0 });
    prediction = move(prediction, c, [], seq / HZ, LEDGES, true);
    m.input(0, [c]); m.step();
    assert.deepEqual([prediction.x, prediction.y, prediction.z], [m.seats[0].player.x, m.seats[0].player.y, m.seats[0].player.z]);
  }
});
test('botbop arena: a drone walks its stretch, is bopped for a count, and another rolls out', async () => {
  const { DRONE } = await import('../bop.mjs');
  const m = new Match(); m.setArena(LEDGES); m.join();
  assert.equal(m.drones.length, 5);
  for (let i = 0; i < HZ * 20; i++) {
    idle(m, 0); m.step();
    for (const d of m.drones) assert.ok(d.x >= d.lo && d.x <= d.hi);
  }
  const d = m.drones[0];
  Object.assign(m.seats[0].player, { x: d.x, z: 0, y: d.y + 1.7 + DRONE.tall + 0.2, vy: -3, onGround: false });
  for (let i = 0; i < 20 && !m.seats[0].combo && !m.seats[0].bops; i++) { idle(m, 0); m.step(); }
  assert.equal(m.seats[0].combo, 1);
  assert.equal(d.alive, false);
  assert.ok(m.seats[0].player.vy > 0);
  assert.equal(m.snapshot().drones.length, 4);
  assert.equal(m.events.find((e) => e.type === 'bop').drone, 0);
  for (let i = 0; i < DRONE.away + 2; i++) { idle(m, 0); m.step(); }
  assert.ok(d.alive);
  assert.equal(new Match().snapshot().drones.length, 0, 'no drones outside the arena');
});

test('botbop combos: a chain in the air is worth 1 + 2 + 3, banked on landing, lost if bopped', () => {
  const m = new Match(); m.setArena(LEDGES); m.join(); m.join();
  const s = m.seats[0];
  for (let k = 1; k <= 3; k++) {
    const d = m.drones.find((x) => x.alive);
    Object.assign(s.player, { x: d.x, z: 0, y: d.y + 1.7 + 1.3, vy: -3, onGround: false });
    for (let i = 0; i < 20 && (s.combo || 0) < k; i++) { idle(m, 0); idle(m, 1); m.step(); }
    assert.deepEqual([s.combo, s.pending], [k, (k * (k + 1)) / 2]);
  }
  assert.equal(s.bops || 0, 0);
  Object.assign(s.player, { x: 7, y: 1.7 + 0.5, vy: -1 }); // clear of every drone
  m.drones.forEach((d) => (d.alive = false, d.backAt = Infinity));
  land(m);
  assert.deepEqual([s.bops, s.combo, s.pending, s.best], [6, 0, 0, 3]);
  // A chain cut short by being bopped is lost.
  const o = m.seats[1];
  Object.assign(o, { combo: 2, pending: 3 });
  Object.assign(o.player, { x: 0, y: 1.7, onGround: true, vy: 0 });
  o.pending = 3; o.combo = 2; o.player.onGround = false; o.player.y = 1.7 + 0.4; o.player.vy = 2;
  Object.assign(s.player, { x: o.player.x, y: o.player.y + 1.9, vy: -6, onGround: false });
  for (let i = 0; i < 20 && o.player.alive; i++) { idle(m, 0); idle(m, 1); m.step(); }
  assert.equal(o.player.alive, false);
  assert.deepEqual([o.pending, o.combo, o.bops || 0], [0, 0, 0]);
});
test('botbop duck: drops through a ledge, falls faster, and lowers the head', async () => {
  const { ducked, BOP: B } = await import('../bop.mjs');
  const l = LEDGES[0], x = (l.xMin + l.xMax) / 2;
  let p = { ...spawn(0), x, z: 0, y: l.yMax + 1.7, onGround: true };
  for (let i = 0; i < 30; i++) p = move(p, cmd(i + 1), [], i / HZ, LEDGES, true);
  assert.ok(Math.abs(p.y - 1.7 - l.yMax) < 1e-6, 'stands on the ledge');
  for (let i = 0; i < 60; i++) p = move(p, cmd(i + 1, { duck: true }), [], i / HZ, LEDGES, true);
  assert.ok(Math.abs(p.y - 1.7) < 1e-6 && p.onGround && p.duck, 'ducking drops through to the floor');
  assert.equal(command({ ...cmd(1), duck: true }).duck, true);
  // A dive lands sooner than a fall.
  const fall = (duck) => { let q = { ...spawn(0), x: 7, z: 0, y: 1.7 + 4, onGround: false }, n = 0; while (!q.onGround && n < 300) { q = move(q, cmd(n + 1, { duck }), [], n / HZ, LEDGES, true); n++; } return n; };
  assert.ok(fall(true) < fall(false));
  // A ducked head is lower: feet that would bop a standing head pass over it.
  const q = { ...spawn(1), x: 0, z: 0, onGround: true, duck: true };
  const a = { ...spawn(0), x: 0, z: 0, y: q.y + 1.9, vy: -3, onGround: false };
  assert.equal(ducked(q), B.duck);
  assert.equal(lands(a, { ...q, duck: false }), true);
  assert.equal(lands({ ...a, y: q.y + 1.9 }, q), false, 'too high over a ducked head');
  assert.equal(lands({ ...a, y: q.y + 1.0 }, q), true, 'lower feet still bop it');
  // Outside a flat room the flag does nothing.
  const open = move({ ...spawn(0) }, cmd(1, { duck: true }));
  assert.ok(!open.duck);
});
