// 🚀 A bike climbs out of home's air into space, flies to another planet and
// is set down over it, on one socket, and comes home the same way.
import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL, move, halfOf, CFG } from '../world.mjs';
import { VOID } from '../bike.mjs';
import { SPACE, LEAVE, ARRIVE, spaceOf, body, aimOf, reached, launch } from '../orbits.mjs';
import { phase } from '../sky.mjs';

const wait = async (f, what = 'it', ms = 4000) => {
  for (const end = Date.now() + ms; Date.now() < end; await delay(10)) if (f()) return;
  assert.fail(`waited too long for ${what}`);
};

test('in space everyone rides, nothing is under them, and nobody gets off', () => {
  let p = { x: 0, y: -40, z: 0, yaw: 0, pitch: -90, vx: 0, vy: 0, vz: 0, alive: true, onGround: false };
  for (let i = 0; i < 240; i++) p = move(p, { fwd: 1, right: 0, yaw: 0, pitch: -90, buttons: 0, bike: i === 100 }, [], 0, [VOID], false, null, halfOf(SPACE));
  assert.equal(p.bike, true);
  assert.ok(p.y < -80, `it flew on down, past where ground would be (${p.y.toFixed(1)})`);
  for (let i = 0; i < 6000; i++) p = move(p, { fwd: 1, right: 0, yaw: 0, pitch: -90, buttons: 0 }, [], 0, [VOID], false, null, halfOf(SPACE));
  assert.equal(p.y, -halfOf(SPACE), 'and stops at the edge of space');
});

test('home to pinch and back by bike', async (t) => {
  // The planets go round by the clock, and at their nearest leaving one is
  // arriving at the next. This walk is through open space: the clock is set
  // to a time when a bike out of home or pinch is clear of the others.
  const real = Date.now, began = real();
  let base = began;
  const clear = (id, now) => [0, 4000, 8000].every((later) => !reached(launch(id, now + later, phase(now + later), LEAVE + 1, 0, 0), now + later));
  while (!(clear('home', base) && clear('pinch', base))) base += 20_000;
  Date.now = () => base + (real() - began);
  t.after(() => { Date.now = real; });
  const dir = await mkdtemp(join(tmpdir(), 'rax-space-'));
  const app = createRaxServer({ stateDir: dir, listHandles: async () => ['rider'], verifyIdentity: async () => ({ handle: 'rider', sub: 'acct-rider' }) });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  t.after(async () => { await app.close(); await rm(dir, { recursive: true, force: true }); });
  const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?` + new URLSearchParams({ room: 'e0s1', protocol: PROTOCOL, account: '1', stay: '1' })), messages = [];
  ws.on('message', (raw) => messages.push(JSON.parse(raw)));
  await once(ws, 'open');
  ws.send(JSON.stringify({ type: 'identity' }));
  ws.send(JSON.stringify({ type: 'account', accessToken: 'rider-token' }));
  t.after(() => ws.close());
  const bodyIn = (room) => {
    const r = app.rooms.get(room), m = r && [...r.sessions.values()].find((s) => s.seat >= 0 && s.handle === 'rider');
    return m ? r.match.seats[m.seat].player : null;
  };
  await wait(() => bodyIn('e0s1'), 'a body at home');
  // On foot, however high, nobody leaves.
  Object.assign(bodyIn('e0s1'), { y: LEAVE + 20, vy: 0, onGround: false });
  await delay(80);
  assert.ok(bodyIn('e0s1'), 'a body in the air without a bike is still at home');
  // On the bike, past LEAVE: space, over home's top.
  Object.assign(bodyIn('e0s1'), { y: LEAVE + 2, vx: 0, vy: 5, vz: 0, yaw: 40, pitch: 60, bike: true, onGround: false });
  await wait(() => bodyIn(SPACE), 'the bike in space');
  await wait(() => messages.some((m) => m.type === 'travel' && m.room === SPACE) && messages.some((m) => m.type === 'arrival'), 'word of it');
  const out = messages.find((m) => m.type === 'travel' && m.room === SPACE);
  assert.equal(out.here, true, 'on the same socket');
  assert.equal(out.flight, true);
  assert.equal(out.at.bike, true);
  const now = Date.now(), home = spaceOf('home', now), p = bodyIn(SPACE);
  const far = Math.hypot(p.x - home[0], p.y - home[1], p.z - home[2]) - body('home').radius;
  assert.ok(Math.abs(far - (LEAVE + 2)) < 6, `as high over home as it was (${far.toFixed(1)})`);
  assert.equal(p.bike, true);
  assert.ok(messages.some((m) => m.type === 'arrival' && Number.isFinite(m.yaw) && Number.isFinite(m.pitch)), 'told which way it looks now');
  // Flown to pinch: set it just inside pinch's air, heading in.
  const c = spaceOf('pinch', Date.now()), r = body('pinch').radius + ARRIVE - 10;
  Object.assign(p, { x: c[0], y: c[1] + r, z: c[2], ...aimOf([0, -1, 0]), vx: 0, vy: -10, vz: 0 });
  assert.equal(reached(p, Date.now()).body.id, 'pinch');
  await wait(() => bodyIn('pinch'), 'the bike over pinch');
  const on = bodyIn('pinch');
  assert.ok(on.bike && on.y > 40 && on.y <= ARRIVE, `over pinch's ground (${on.y.toFixed(1)})`);
  assert.ok(Math.abs(on.x) < 3 && Math.abs(on.z) < 3);
  assert.equal(app.rooms.has(SPACE), false, 'space is put away when nobody is in it');
  // It can come down and stand there.
  Object.assign(on, { y: CFG.eyeHeight, vy: 0, bike: false, onGround: true });
  await delay(80);
  assert.ok(bodyIn('pinch'), 'standing on pinch');
  // And home again.
  Object.assign(bodyIn('pinch'), { y: LEAVE + 1, vy: 3, bike: true, onGround: false });
  await wait(() => bodyIn(SPACE), 'space again');
  const h = spaceOf('home', Date.now());
  Object.assign(bodyIn(SPACE), { x: h[0] + body('home').radius + 30, y: h[1], z: h[2], vx: -5, vy: 0, vz: 0 });
  await wait(() => bodyIn('e0s2'), 'the terrace south of the tower');
  assert.ok(bodyIn('e0s2').bike && bodyIn('e0s2').y > 20);
});
