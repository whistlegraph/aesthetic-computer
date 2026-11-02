import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { WebSocket } from 'ws';
import { Match } from '../match.mjs';
import { createDuelServer } from '../server.mjs';
import { spawn, move, HZ, PROTOCOL, FRAG_LIMIT } from '../world.mjs';
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
  assert.deepEqual(reconcile(server, commands, 1).player, commands.reduce(move, base));
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
  assert.ok(m.seats[0].queue.length <= 12);
  m.step(); assert.equal(m.tick, before + 1);
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

test('cover stops rockets and warmup permits firing without scoring', () => {
  const cover = playing();
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

test('disconnect resets the remaining player and releases the second slot', () => {
  const m = playing(); assert.equal(m.join(), -1);
  shoot(m); m.leave(1);
  assert.equal(m.phase, 'waiting'); assert.equal(m.seats[0].player.score, 0);
  assert.equal(m.join(), 1); assert.equal(m.phase, 'countdown');
});

test('HTTP allowlist, actual two-socket match, full room, room isolation and cleanup', async t => {
  const app = createDuelServer(); t.after(() => app.close());
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
  await waitFor(a, d => d.phase === 'countdown');
  const c = connect('test', 'instagib'); const [code] = await once(c, 'close'); assert.equal(code, 1008);
  const other = connect('other-room'); assert.equal((await waitFor(other, d => d.type === 'welcome')).seat, 0);
  a.send(JSON.stringify({ type: 'input', commands: [cmd(1)] }));
  await waitFor(a, d => d.acks?.[0] === 1);
  b.close(); await once(b, 'close');
  await waitFor(a, d => d.phase === 'waiting' && d.tick > 2);
  const replacement = connect(); assert.equal((await waitFor(replacement, d => d.type === 'welcome')).seat, 1);
});
