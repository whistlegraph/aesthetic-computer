// 🪑 A seat handed to a newer connection of the same person (a reload, a
// second window, the app beside the browser) must answer the newcomer's
// commands at once, though they count from one again.
import test from 'node:test';
import assert from 'node:assert/strict';
import { Room } from '../room.mjs';

const walk = (seq, yaw) => ({ seq, yaw, pitch: 0, fwd: 1, right: 0, buttons: 0 });
// A signed-in page joins unseated and is seated once its handle is verified.
function signIn(room, handle) {
  const member = room.add({}, true);
  member.handle = handle;
  member.ready = true;
  room.assign();
  return member;
}

test('a body handed to a second window walks for it at once', () => {
  const room = new Room(() => {});
  const first = signIn(room, 'jeffrey');
  for (let seq = 1; seq <= 600; seq++) {
    room.match.input(first.seat, [walk(seq, 0)]);
    room.match.step();
  }
  const seat = first.seat, stood = { ...room.match.seats[seat].player };
  const second = signIn(room, 'jeffrey');
  assert.equal(second.seat, seat, 'the newer window holds the seat');
  assert.equal(first.seat, -1, 'the older one looks on');
  const body = room.match.seats[seat].player;
  assert.deepEqual([body.x, body.z], [stood.x, stood.z], 'the body stays where it stood');
  for (let seq = 1; seq <= 60; seq++) {
    room.match.input(second.seat, [walk(seq, 180)]);
    room.match.step();
  }
  const moved = Math.hypot(room.match.seats[seat].player.x - stood.x, room.match.seats[seat].player.z - stood.z);
  assert.ok(moved > 3, `a second of walking moved the body ${moved.toFixed(2)} m`);
  assert.equal(room.match.seats[seat].ack, 60);
});
