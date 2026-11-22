// 🪞 A drone signed in as someone who walks on another screen follows that
// body from room to room; and anyone watched walks on through a quiet wire.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL, HZ, INTERP_TICKS } from '../world.mjs';
import { SnapshotClock, RemoteEase, AHEAD_TICKS } from '../presentation.mjs';
import { interpolate } from '../net.mjs';

const wait = async (test, ms = 3000) => {
  for (const end = Date.now() + ms; Date.now() < end; await delay(10)) if (test()) return;
  assert.fail('waited too long');
};

test('a following drone is brought to the room its own body walks into, and a freed one is left', async (t) => {
  const dir = await mkdtemp(join(tmpdir(), 'rax-follow-'));
  const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => ['jeffrey'],
    verifyIdentity: async (token) => ({ handle: token, sub: `account:${token}` }) });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `ws://127.0.0.1:${app.server.address().port}/play?`;
  t.after(async () => { await app.close(); await rm(dir, { recursive: true, force: true }); });
  const connect = async () => {
    const ws = new WebSocket(base + new URLSearchParams({ room: 'lobby', protocol: PROTOCOL, stay: '1', account: '1' })), messages = [];
    ws.on('message', (raw) => messages.push(JSON.parse(raw)));
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity' }));
    ws.send(JSON.stringify({ type: 'account', accessToken: 'jeffrey' }));
    t.after(() => ws.close());
    return { ws, messages, say: (m) => ws.send(JSON.stringify(m)) };
  };
  const seen = () => [...app.rooms.entries()].flatMap(([key, r]) => [...r.sessions.values()].map((m) => ({ key, m })));
  // The first window is the drone once the second (the newer) holds the body.
  const drone = await connect();
  await wait(() => seen().some(({ m }) => m.handle === 'jeffrey' && m.seat >= 0));
  const droneMember = seen().find(({ m }) => m.handle === 'jeffrey').m;
  const walker = await connect();
  await wait(() => droneMember.seat < 0 && seen().some(({ m }) => m !== droneMember && m.handle === 'jeffrey' && m.seat >= 0));
  const walkerMember = seen().find(({ m }) => m !== droneMember && m.handle === 'jeffrey').m;
  const roomOf = (member) => seen().find(({ m }) => m === member)?.key;
  const step = async (edge, x) => {
    const from = roomOf(walkerMember), r = app.rooms.get(from);
    Object.assign(r.match.seats[walkerMember.seat].player, { x, z: 0.2, y: 1.7, yaw: 90, pitch: 0 });
    walker.say({ type: 'walk', edge, in: from });
    await wait(() => roomOf(walkerMember) !== from).catch((e) => { throw Error(`${e.message}: from ${from}, owner ${walkerMember.owner}, seat ${walkerMember.seat}, x ${app.rooms.get(from)?.match.seats[walkerMember.seat]?.player?.x}, said ${JSON.stringify(walker.messages.filter((m) => m.type !== 'snapshot').map((m) => m.type))}`); });
  };
  await wait(() => walkerMember.owner); // (a walk is heard only from a page that has said whose it is)
  drone.say({ type: 'follow', on: true });
  await wait(() => droneMember.following === true);
  await step('east', 8.3);
  await wait(() => roomOf(droneMember) === roomOf(walkerMember));
  assert.equal(droneMember.seat, -1, 'it looks on still');
  assert.ok(walkerMember.seat >= 0, 'the body keeps its seat');
  const travel = drone.messages.find((m) => m.type === 'travel');
  assert.equal(travel.room, roomOf(walkerMember));
  assert.equal(travel.here, true);
  // Taken back by hand: the body goes on alone.
  drone.say({ type: 'follow', on: false });
  await wait(() => droneMember.following === false);
  const stayed = roomOf(droneMember);
  await step('west', -8.3);
  await delay(150);
  assert.equal(roomOf(droneMember), stayed);
  // And following again from another room, it is brought at once.
  drone.say({ type: 'follow', on: true });
  await wait(() => roomOf(droneMember) === roomOf(walkerMember));
});

const snap = (tick, x, extra = {}) => ({ tick, players: [{ x, y: 1.7, z: 0, yaw: 0, pitch: 0, life: 1, alive: true, ...extra }] });

test('a watched walk goes on through a quiet wire, at its own pace, for a fifth of a second', () => {
  const clock = new SnapshotClock(), history = [];
  const frame = 1000 / 120, step = 0.1; // metres a tick
  let now = 0, last = null, most = 0;
  const hear = (tick) => { history.push(snap(tick, tick * step)); clock.receive(tick, now); };
  const draw = () => {
    const tick = clock.sample(now), p = interpolate(history, tick, 0, clock.ahead);
    if (last !== null) { assert.ok(p.x >= last - 1e-9, 'never back'); most = Math.max(most, p.x - last); }
    last = p.x;
    return p;
  };
  for (let tick = 1; tick <= 60; tick++) { hear(tick); now += 1000 / HZ; draw(); now += 0; }
  // The wire goes quiet for 250 ms: playback runs out after its delay, then the walk is carried on.
  const quietFrom = last;
  for (let i = 0; i < 30; i++) { now += frame; draw(); }
  assert.ok(clock.ahead > 0 && clock.ahead <= AHEAD_TICKS);
  assert.ok(last - quietFrom > INTERP_TICKS * step + 0.5, `walked on ${(last - quietFrom).toFixed(2)} m`);
  assert.ok(most < step * 1.2, `no frame moved more than a tick's walk: ${most.toFixed(3)}`);
  // The late words come in a clump; the picture goes on from where it was, with no throw forward.
  const before = last, ease = new RemoteEase();
  ease.sample(0, { ...history.at(-1).players[0], x: before }, true, 0);
  for (let tick = 61; tick <= 76; tick++) hear(tick);
  assert.equal(clock.ahead, 0);
  let shown = before;
  for (let i = 0; i < 60; i++) {
    now += frame;
    hear(77 + Math.floor(i / 2));
    const tick = clock.sample(now), p = ease.sample(0, interpolate(history, tick, 0, clock.ahead), clock.ahead > 0, frame / 1000);
    assert.ok(Math.abs(p.x - shown) < step * 2, `a step of ${(p.x - shown).toFixed(3)} m in one frame`);
    shown = p.x;
  }
  const tick = clock.sample(now);
  assert.ok(Math.abs(shown - interpolate(history, tick, 0).x) < 0.02, 'and it is where the room says, a moment on');
});

test('a guess is never made across a new life, a teleport or a stop', () => {
  assert.equal(interpolate([snap(1, 0), snap(2, 0)], 2, 0, 6).x, 0, 'standing still stays still');
  assert.equal(interpolate([snap(1, 0), snap(2, 50)], 2, 0, 6).x, 50 + 6 * (12 / 60), 'no faster than a sprint');
  assert.equal(interpolate([snap(1, 0), snap(2, 1, { life: 2 })], 2, 0, 6).x, 1);
  assert.equal(interpolate([snap(1, 0), snap(2, 1)], 2, 0, 0).x, 1, 'nothing guessed while the wire speaks');
  const ease = new RemoteEase();
  ease.sample(0, snap(1, 0).players[0], true, 0);
  assert.equal(ease.sample(0, snap(2, 30).players[0], false, 0).x, 30, 'a long way is a cut');
});
