// 🧵 One socket for the whole visit: a page that says stay=1 is moved from
// room to room by the server, on the wire it already has.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';
import { CAR_Z, inCar } from '../elevator.mjs';

const wait = async (test, ms = 3000) => {
  for (const end = Date.now() + ms; Date.now() < end; await delay(10)) if (test()) return;
  assert.fail('waited too long');
};
async function fixture(t) {
  const dir = await mkdtemp(join(tmpdir(), 'rax-stay-'));
  const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => ['jeffrey'],
    verifyIdentity: async (token) => ({ handle: token, sub: `account:${token}` }) });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `ws://127.0.0.1:${app.server.address().port}/play?`;
  t.after(async () => { await app.close(); await rm(dir, { recursive: true, force: true }); });
  const connect = async (room, extra = {}) => {
    const ws = new WebSocket(base + new URLSearchParams({ room, protocol: PROTOCOL, ...extra })), messages = [];
    let closed = false;
    ws.on('message', (raw) => messages.push(JSON.parse(raw)));
    ws.on('close', () => { closed = true; });
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity' }));
    await wait(() => [...app.rooms.get(room).sessions.values()].some((m) => m.ws && m.owner && m.seat >= 0));
    t.after(() => ws.close());
    return { ws, messages, closed: () => closed, say: (m) => ws.send(JSON.stringify(m)) };
  };
  const bodyIn = (room) => { const r = app.rooms.get(room), m = r && [...r.sessions.values()].find((s) => s.seat >= 0); return m ? r.match.seats[m.seat].player : null; };
  return { app, connect, bodyIn };
}

test('a walk over a line moves the body to the next room on the same socket', async (t) => {
  const { app, connect, bodyIn } = await fixture(t);
  const a = await connect('lobby', { stay: '1' });
  Object.assign(bodyIn('lobby'), { x: 8.3, z: 0.2, y: 1.7, yaw: 90, pitch: 12, vx: 6 });
  a.say({ type: 'walk', edge: 'east', in: 'lobby' });
  await wait(() => a.messages.some((m) => m.type === 'travel'));
  const travel = a.messages.find((m) => m.type === 'travel'), after = a.messages.slice(a.messages.indexOf(travel) + 1);
  assert.equal(travel.here, true);
  assert.equal(travel.room, 'e1n0');
  assert.equal(travel.passage, undefined, 'no passage: nothing is opened');
  assert.ok(travel.at.vx > 4, 'the motion crosses with the body');
  await wait(() => a.messages.some((m) => m.type === 'arrival') && bodyIn('e1n0'));
  // The next room's words follow the travel on the same wire, a seat first.
  assert.equal(after.find((m) => m.type === 'role')?.role, 'player');
  assert.ok(a.messages.indexOf(a.messages.find((m) => m.type === 'arrival')) > a.messages.indexOf(travel));
  assert.ok(Math.abs(bodyIn('e1n0').x + 7.7) < 0.5, `set down just over the line, at x ${bodyIn('e1n0').x}`);
  assert.equal(app.rooms.has('lobby'), false, 'the room left is empty and gone');
  assert.equal(a.closed(), false);
  assert.equal(a.ws.readyState, WebSocket.OPEN);
  // A word for the room left, still on its way when the body moved, is not the next room's.
  const seat = [...app.rooms.get('e1n0').sessions.values()][0].seat;
  a.say({ type: 'input', in: 'lobby', commands: [{ seq: 1, yaw: 0, pitch: 0, fwd: 1, right: 0, buttons: 0 }] });
  a.say({ type: 'input', in: 'e1n0', commands: [{ seq: 1, yaw: 270, pitch: 0, fwd: 0, right: 0, buttons: 0 }] });
  await wait(() => app.rooms.get('e1n0').match.seats[seat].ack === 1);
  assert.equal(app.rooms.get('e1n0').match.seats[seat].player.yaw, 270);
});

test('a ride moves the rider to the floor on the same socket, where they stood, with the door shut', async (t) => {
  const { app, connect, bodyIn } = await fixture(t);
  const a = await connect('lobby', { stay: '1' });
  Object.assign(bodyIn('lobby'), { x: 0.7, z: CAR_Z + 0.5, y: 1.7 });
  a.say({ type: 'elevator', to: '@jeffrey', in: 'lobby' });
  await wait(() => a.messages.some((m) => m.type === 'travel' || m.type === 'elevator-result'));
  const travel = a.messages.find((m) => m.type === 'travel');
  assert.ok(travel, JSON.stringify(a.messages.find((m) => m.type === 'elevator-result')));
  assert.equal(travel.here, true);
  assert.equal(travel.ride.to, app.tower.tower.forHandle('jeffrey').number);
  await wait(() => bodyIn(travel.room));
  const body = bodyIn(travel.room);
  assert.ok(inCar(body) && Math.abs(body.x - 0.7) < 0.3, `in the car where they stood: ${body.x}, ${body.z}`);
  assert.ok(body.shut > 0, 'the door is shut for the ride');
  assert.equal(a.closed(), false);
});

test('a page that does not stay is given a passage, as before', async (t) => {
  const { connect, bodyIn } = await fixture(t);
  const a = await connect('lobby');
  Object.assign(bodyIn('lobby'), { x: 8.3, z: 0.2, y: 1.7, yaw: 90 });
  a.say({ type: 'walk', edge: 'east' });
  await wait(() => a.messages.some((m) => m.type === 'travel'));
  const travel = a.messages.find((m) => m.type === 'travel');
  assert.ok(travel.passage && !travel.here);
  assert.ok(bodyIn('lobby'), 'the body waits in the room it is leaving');
});
