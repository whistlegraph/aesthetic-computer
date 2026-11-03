import test from 'node:test';
import assert from 'node:assert/strict';
import { Match } from '../match.mjs';
import { HZ } from '../world.mjs';

const ball = (id, x, z, item) => ({
  id, x, z, item,
  geometry: { name: 'Ball', boxes: [{ x: 0, y: 0.2, z: 0, w: 0.4, h: 0.4, d: 0.4 }] },
});

// Two players facing each other along z, the match playing.
function playing(items) {
  const m = new Match();
  m.join();
  m.join();
  m.phase = 'playing';
  Object.assign(m.seats[0].player, { x: 0, z: -3, yaw: 0, pitch: 0 });
  Object.assign(m.seats[1].player, { x: 0, z: 3, yaw: 180, pitch: 0 });
  m.setObjects(items);
  return m;
}
let seq = 1;
const send = (m, seat, extra = {}) => {
  const p = m.seats[seat].player;
  m.input(seat, [{ seq: seq++, fwd: 0, right: 0, yaw: p.yaw, pitch: p.pitch, ...extra }]);
  m.step();
};

test('items are not solid; running over one picks it up, X drops it', () => {
  const m = playing([ball('b', 0, -3, 'throw'), ball('wall', 5, 5, null)]);
  assert.equal(m.objects.length, 1, 'only the non-item collides');
  send(m, 0);
  assert.equal(m.items.get('b').holder, 0);
  send(m, 0, { drop: true });
  assert.equal(m.items.get('b').holder, -1);
  for (let i = 0; i < HZ / 2; i++) send(m, 0);
  assert.equal(m.items.get('b').holder, -1, 'a dropped item is not re-grabbed at once');
});

test('a thrown item arcs, lands, bounces to rest, and hits who it meets', () => {
  const m = playing([ball('b', 0, -3, 'throw')]);
  send(m, 0);
  m.seats[0].player.pitch = -10;
  send(m, 0, { use: true });
  const it = m.items.get('b');
  assert.equal(it.holder, -1);
  let top = 0;
  for (let i = 0; i < HZ * 3 && m.seats[1].player.health === 100; i++) {
    send(m, 1);
    top = Math.max(top, it.y);
  }
  assert.ok(top > 1, `it flew up first (${top.toFixed(2)})`);
  assert.equal(m.seats[1].player.health, 70, 'the throw hit the other player');
});

test('a poke reaches whoever is close in front', () => {
  const m = playing([ball('s', 0, -3, 'poke')]);
  send(m, 0);
  m.seats[1].player.z = -1.4;
  send(m, 0, { use: true });
  assert.equal(m.seats[1].player.health, 66);
  send(m, 0, { use: true });
  assert.equal(m.seats[1].player.health, 66, 'the poke has a cooldown');
});

test('a gun fires bullets that travel and hit', () => {
  const m = playing([ball('g', 0, -3, 'shoot')]);
  send(m, 0);
  send(m, 0, { use: true });
  assert.equal(m.bullets.length, 1);
  for (let i = 0; i < 10; i++) send(m, 1);
  assert.equal(m.seats[1].player.health, 75);
  assert.equal(m.items.get('g').holder, 0, 'shooting keeps the gun in hand');
});
