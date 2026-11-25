import test from 'node:test';
import assert from 'node:assert/strict';
import { Rounds, handOf, ROUND_SPEED } from '../rounds.mjs';
import { Match } from '../match.mjs';
import { HZ } from '../world.mjs';

// 🔫 Rounds keep flying through a quiet stretch, fired rounds show at once,
// and a fired round hides the server's twin until the server's one ends.
test('a round flies on through a gap in snapshots instead of standing still', () => {
  const r = new Rounds();
  r.heard([{ id: 1, owner: 1, x: 0, y: 1.5, z: 0, vx: 0, vy: 0, vz: ROUND_SPEED }], 0, 0);
  r.step(0, [], 0);
  for (let t = 16; t <= 160; t += 16) r.step(t, [], 0);
  const [b] = r.step(160, [], 0);
  assert.ok(b.z > 4.5 && b.z < 5.5, `flew ${b.z.toFixed(2)} m in 160 ms`);
});

test('a round off a wall comes back', () => {
  const r = new Rounds();
  r.heard([{ id: 1, owner: 1, x: 0, y: 1.5, z: 0, vx: 0, vy: 0, vz: ROUND_SPEED }], 0, 0);
  const wall = { id: 'w', x: 0, z: 2, geometry: { name: 'Wall', boxes: [{ x: 0, y: 1.5, z: 0, w: 6, h: 3, d: 0.4 }] } };
  r.step(0, [wall], 0);
  for (let t = 16; t <= 200; t += 16) r.step(t, [wall], 0);
  const [b] = r.step(200, [wall], 0);
  assert.ok(b.vz < 0 && b.z < 2, 'bounced back off the wall');
});

test('your own shot shows at once and hides the server round that answers it', () => {
  const r = new Rounds();
  const me = { x: 0, y: 1.6, z: 0, yaw: 0, pitch: 0 };
  assert.equal(r.fire(me, 0), true);
  assert.equal(r.fire(me, 100), false, 'still cooling');
  let shown = r.step(0, [], 0);
  assert.equal(shown.length, 1);
  assert.deepEqual([shown[0].x, shown[0].z].map((v) => +v.toFixed(3)), [handOf(me).x, handOf(me).z].map((v) => +v.toFixed(3)));
  // The server's round for it arrives a round trip later: still one round drawn.
  r.heard([{ id: 9, owner: 0, x: 0, y: 1, z: 1, vx: 0, vy: 0, vz: ROUND_SPEED }], 0, 160);
  shown = r.step(160, [], 0);
  assert.equal(shown.length, 1);
  assert.match(String(shown[0].id), /^mine-/);
  // The server's round ends (it hit someone): ours ends with it.
  r.heard([], 0, 200);
  assert.equal(r.step(200, [], 0).length, 0);
});

test('a shot the server never answers is dropped', () => {
  const r = new Rounds();
  r.fire({ x: 0, y: 1.6, z: 0, yaw: 0, pitch: 0 }, 0);
  assert.equal(r.step(1300, [], 0).length, 0);
});

test('snapshots carry each round\'s owner and each item\'s kind', () => {
  const m = new Match();
  m.join(); m.join(); m.phase = 'playing';
  m.setObjects([{ id: 'g', x: 0, z: 0, item: 'shoot', geometry: { name: 'Gun', boxes: [{ x: 0, y: 0.1, z: 0, w: 0.2, h: 0.2, d: 0.4 }] } }]);
  Object.assign(m.seats[0].player, { x: 0, z: 0 });
  for (let k = 0; k < HZ / 2; k++) m.step();
  const snap = m.snapshot();
  assert.equal(snap.items[0].kind, 'shoot');
  assert.equal(snap.items[0].holder, 0);
  m.seats[0].ack = 0;
  m.input(0, [{ seq: 1, fwd: 0, right: 0, yaw: 0, pitch: 0, use: true }]);
  m.step();
  const fired = m.snapshot().bullets;
  assert.equal(fired.length, 1);
  assert.equal(fired[0].owner, 0);
});
