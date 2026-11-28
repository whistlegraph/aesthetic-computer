import test from 'node:test';
import assert from 'node:assert/strict';
import { tableEye, wristOf, pokerOf, Headset, handOutline, handsPacked, handsValid, handsUnpacked, HAND_NUMBERS, roomPlanes, fistOf, grabStart, grabStep, turn, turnHeadset, handAim, headPacked, headValid, tapStep, TAP_MS } from '../immersive.mjs';

test('the room on a table: the eye is over the room as the head is over the table, times the room\'s smallness', () => {
  const table = { scale: 0.05, pivot: { x: 0, y: 0, z: 0 }, at: [0, -0.45, 0.6] };
  // The head where it began: 45 cm over the table's top and 60 cm back from its middle is 9 m up and 12 m back in the room.
  const e = tableEye(table, [0, 0, 0]);
  assert.ok(Math.abs(e.y - 9) < 1e-9 && Math.abs(e.z + 12) < 1e-9 && e.x === 0);
  // Lean ten centimetres in: two metres nearer.
  assert.ok(Math.abs(tableEye(table, [0, 0, 0.1]).z + 10) < 1e-9);
  // Head down on the table top is the floor's middle.
  assert.deepEqual(tableEye(table, [0, -0.45, 0.6]), { x: 0, y: 0, z: 0 });
});

test('the wrist and the poking tip come from tracked hands, or else from the controllers held', () => {
  const joint = (x) => ({ x, y: 0, z: 0 });
  const hand = (side, x) => ({ side, fingers: [[joint(x), joint(x + 0.01)], [joint(x), joint(x + 0.1)]] });
  const xr = { hands: [hand('left', -0.2), hand('right', 0.2)], rays: [] };
  assert.equal(wristOf(xr).x, -0.2);
  assert.ok(Math.abs(pokerOf(xr).x - 0.3) < 1e-9, 'the index finger\'s tip');
  const held = { hands: [], rays: [{ side: 'left', origin: joint(-0.3) }, { side: 'right', origin: joint(0.3) }] };
  assert.equal(wristOf(held).x, -0.3); assert.equal(pokerOf(held).x, 0.3);
  assert.equal(wristOf({ hands: [], rays: [] }), null);
});

test('B or Y on a held controller asks for the next view once a press', () => {
  const rig = new Headset(), I = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
  const pose = { transform: { matrix: I }, views: [] };
  const source = (pressed) => [{ handedness: 'right', gamepad: { axes: [0, 0, 0, 0], buttons: [{}, {}, {}, {}, {}, { pressed }] } }];
  assert.equal(rig.update(pose, source(false), 0).viewPress, false);
  assert.equal(rig.update(pose, source(true), 0).viewPress, true);
  assert.equal(rig.update(pose, source(true), 0).viewPress, false, 'held is not pressed again');
});

test('a hand is drawn as its outline: one closed line round all five fingers, or none when a joint is missing', () => {
  const j = (x, y) => ({ x, y, z: 0, radius: 0.008 });
  const finger = (x) => [j(0, 0), j(x, 0.03), j(x, 0.09), j(x, 0.12), j(x, 0.14), j(x, 0.16)];
  const fingers = [[j(0, 0), j(0.03, 0.02), j(0.05, 0.04), j(0.07, 0.06), j(0.08, 0.07)], finger(0.03), finger(0.01), finger(-0.01), finger(-0.03)];
  const edge = handOutline(fingers);
  assert.equal(edge.length, 2 + 4 * 8 + 8);
  // The little finger's two sides stand a finger's width apart, either side of its joints.
  assert.ok(Math.abs(edge[1].x - (-0.038)) < 1e-9 && Math.abs(edge[8].x - (-0.022)) < 1e-9);
  assert.ok(edge.every((p) => [p.x, p.y, p.z].every(Number.isFinite)));
  fingers[2][4] = undefined;
  assert.equal(handOutline(fingers), null);
});

test('hands are told to the room in its own metres and come back as they were, giant when the room is on a table', () => {
  const j = (x, y) => ({ x, y, z: 0.1, radius: 0.008 });
  const finger = (x) => [j(0, 0), j(x, 0.03), j(x, 0.09), j(x, 0.12), j(x, 0.14), j(x, 0.16)];
  const fingers = [[j(0, 0), j(0.03, 0.02), j(0.05, 0.04), j(0.07, 0.06), j(0.08, 0.07)], finger(0.03), finger(0.01), finger(-0.01), finger(-0.03)];
  const xr = { hands: [{ side: 'left', fingers }, { side: 'right', fingers: [...fingers.slice(0, 4), [j(0, 0), undefined, j(0, 0), j(0, 0), j(0, 0), j(0, 0)]] }] };
  const packed = handsPacked(xr, { x: 2, y: 10, z: -3 }, 20);
  assert.equal(packed.length, 2, 'a hand seen in part is sent whole: the joint not given stands at the one before');
  assert.deepEqual(packed[1].j.slice(72, 75), packed[1].j.slice(69, 72));
  assert.equal(packed[0].j.length, HAND_NUMBERS);
  assert.ok(handsValid(packed));
  const [hand] = handsUnpacked(JSON.parse(JSON.stringify(packed)));
  const tip = hand.fingers[1].at(-1);
  assert.ok(Math.abs(tip.x - (2 + 0.03 * 20)) < 1e-9 && Math.abs(tip.y - (10 + 0.16 * 20)) < 1e-9 && Math.abs(tip.z - (-3 + 0.1 * 20)) < 1e-9);
  assert.ok(Math.abs(tip.radius - 0.16) < 1e-9);
  assert.ok(handOutline(hand.fingers));
  assert.equal(handsValid([{ side: 'left', r: 0.01, j: [1, 2, 3] }]), false);
  assert.equal(handsValid([{ side: 'up', r: 0.01, j: packed[0].j }]), false);
  assert.equal(handsValid([packed[0], packed[0], packed[0]]), false);
});

test('the real room\'s planes are counted by their labels and outlined from the head, and a headset that gives none says so', () => {
  const I = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 2, 3, 1]; // (a plane standing at 1, 2, 3)
  const plane = (semanticLabel) => ({ semanticLabel, planeSpace: {}, polygon: [{ x: 0, y: 0, z: 0 }, { x: 1, y: 0, z: 0 }, { x: 1, y: 0, z: 1 }] });
  const frame = { detectedPlanes: new Set([plane('wall'), plane('wall'), plane('screen'), plane(undefined)]), getPose: () => ({ transform: { matrix: I } }) };
  const room = roomPlanes(frame, {}, (p) => ({ x: p.x - 1, y: p.y - 2, z: p.z - 3 }), { enabledFeatures: ['plane-detection'] });
  assert.equal(room.asked, true); assert.equal(room.count, 4);
  assert.deepEqual(room.labels, { wall: 2, screen: 1, unnamed: 1 });
  assert.deepEqual(room.outlines[2].points[1], { x: 1, y: 0, z: 0 });
  const none = roomPlanes({}, {}, (p) => p, { enabledFeatures: [] });
  assert.deepEqual([none.asked, none.known, none.count], [false, false, 0]);
});

import { once } from 'node:events';
import { createRequire } from 'node:module';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';
test('hands said by one screen are heard by the others in its room, and not by itself', async (t) => {
  const { WebSocket } = createRequire(new URL('../../lith/package.json', import.meta.url))('ws');
  const app = createRaxServer({ guests: true }); t.after(() => app.close());
  app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
  const connect = () => { const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?room=hands-test&protocol=${PROTOCOL}`); ws.messages = []; ws.on('message', (b) => ws.messages.push(JSON.parse(b))); return ws; };
  const waitFor = async (ws, predicate) => { const abort = AbortSignal.timeout(3000); while (!ws.messages.some(predicate)) await once(ws, 'message', { signal: abort }); return ws.messages.find(predicate); };
  const a = connect(), b = connect(); t.after(() => { a.close(); b.close(); });
  await waitFor(a, (d) => d.type === 'welcome'); await waitFor(b, (d) => d.type === 'welcome');
  const hands = [{ side: 'left', r: 0.16, j: Array.from({ length: HAND_NUMBERS }, (_, i) => -123.456 + i) }, { side: 'right', r: 0.16, j: Array.from({ length: HAND_NUMBERS }, (_, i) => 100.001 + i) }];
  a.send(JSON.stringify({ type: 'hands', hands }));
  const heard = await waitFor(b, (d) => d.type === 'hands');
  assert.deepEqual(heard.hands, hands); assert.equal(typeof heard.id, 'string');
  assert.ok(!a.messages.some((d) => d.type === 'hands'));
});

test('two fists hold the world: what was under them stays under them as they move, part and turn', () => {
  const table = { scale: 0.05, rot: 0, pivot: { x: 0, y: 0, z: 0 }, at: [0, -0.12, 0.6] };
  const under = (t, q) => { const r = turn(q, t.rot || 0); return [0, 1, 2].map((i) => [t.pivot.x, t.pivot.y, t.pivot.z][i] + (r[i] - t.at[i]) / t.scale); };
  const near = (a, b) => a.every((v, i) => Math.abs(v - b[i]) < 1e-6);
  const q1 = [-0.2, -0.2, 0.4], q2 = [0.2, -0.2, 0.4], hold = grabStart(table, q1, q2);
  // Pan: both hands 10 cm to the right and 5 up.
  let p1 = [-0.1, -0.15, 0.4], p2 = [0.3, -0.15, 0.4], t = grabStep(table, hold, p1, p2);
  assert.ok(Math.abs(t.scale - 0.05) < 1e-9 && Math.abs(t.rot) < 1e-9);
  assert.ok(near(under(t, p1), hold.a1) && near(under(t, p2), hold.a2));
  // Scale: twice as far apart is twice as large.
  p1 = [-0.4, -0.2, 0.4]; p2 = [0.4, -0.2, 0.4]; t = grabStep(table, hold, p1, p2);
  assert.ok(Math.abs(t.scale - 0.1) < 1e-9);
  assert.ok(near(under(t, p1), hold.a1) && near(under(t, p2), hold.a2));
  // Rotate: the right fist comes round toward the body, a quarter turn.
  p1 = [0, -0.2, 0.2]; p2 = [0, -0.2, 0.6]; t = grabStep(table, hold, p1, p2);
  assert.ok(Math.abs(Math.abs(t.rot) - Math.PI / 2) < 1e-9);
  assert.ok(near(under(t, p1), hold.a1) && near(under(t, p2), hold.a2));
  // Never smaller or larger than its limits.
  assert.equal(grabStep(table, hold, [-50, 0, 0], [50, 0, 0]).scale, 2);
  // A fist is four curled fingers; an open hand is not one.
  const j = (x, y) => ({ x, y, z: 0 }), open = (len) => [[j(0, 0), j(0.03, 0.02), j(0.05, 0.04), j(0.07, 0.06), j(0.08, 0.07)], ...[0.03, 0.01, -0.01, -0.03].map((x) => [j(0, 0), j(x, 0.03), j(x, 0.09), j(x, 0.1), j(x, 0.1), j(x, len)])];
  assert.equal(fistOf({ fingers: open(0.17) }), null);
  assert.deepEqual(fistOf({ fingers: open(0.05) }), j(0.01, 0.09));
  // The headset turned with the room: its place and its hands go round together, and it faces that much further round.
  const xr = turnHeadset({ offset: [1, 0, 0], forward: [0, 0, 1], rightAxis: [1, 0, 0], up: [0, 1, 0], yaw: 350, hands: [{ side: 'left', fingers: [[j(1, 0), undefined]] }], rays: [] }, Math.PI / 2);
  assert.ok(near(xr.offset, [0, 0, -1]) && near(xr.forward, [1, 0, 0]) && Math.abs(xr.yaw - 80) < 1e-9);
  assert.ok(Math.abs(xr.hands[0].fingers[0][0].z + 1) < 1e-9 && xr.hands[0].fingers[0][1] === undefined);
});

test('in the body the right hand aims the way it points, and its index finger curled in is the trigger', () => {
  const j = (x, y, z) => ({ x, y, z });
  const hand = (tip) => ({ side: 'right', fingers: [[j(0, 0, 0)], [j(0, 0, 0), j(0, 0, 0.03), j(0, 0, 0.09), j(0, 0, 0.12), j(0, 0, 0.14), tip]] });
  const ahead = handAim({ hands: [hand(j(0, 0, 0.17))] });
  assert.ok(Math.abs(ahead.yaw) < 1e-9 && Math.abs(ahead.pitch) < 1e-9 && ahead.pulled === false);
  assert.equal(handAim({ hands: [hand(j(0, -0.04, 0.07))] }).pulled, true);
  const up = handAim({ hands: [{ side: 'right', fingers: [[j(0, 0, 0)], [j(0, 0, 0), j(0, 0, 0), j(0.05, 0.05, 0), j(0, 0, 0), j(0, 0, 0), j(0.1, 0.1, 0)]] }] });
  assert.ok(Math.abs(up.yaw - 90) < 1e-9 && Math.abs(up.pitch - 45) < 1e-9);
  assert.equal(handAim({ hands: [{ side: 'left', fingers: [] }] }), null);
});

test('a ghost\'s head is told with its hands: where it is and the way it faces', () => {
  const head = headPacked({ x: 1, y: 9, z: -12 }, [1, 1, 0], 20);
  assert.deepEqual(head, [1, 9, -12, 90, 45, 20]);
  assert.ok(headValid(head) && headValid(undefined));
  assert.equal(headValid([1, 2, 3]), false); assert.equal(headValid([1, 2, 3, 0, 0, 0]), false);
});

test('a body tapped twice with a fingertip in time is a double tap; a tip that rests on it, or a second tap too late, is not', () => {
  const state = {};
  assert.deepEqual(tapStep(state, { right: 2 }, 0), { tap: 2, double: -1 });
  assert.deepEqual(tapStep(state, { right: 2 }, 100), { tap: -1, double: -1 }, 'resting on it');
  assert.deepEqual(tapStep(state, { right: -1 }, 200), { tap: -1, double: -1 });
  assert.deepEqual(tapStep(state, { right: 2 }, 400), { tap: -1, double: 2 });
  // Too late: a first tap again.
  tapStep(state, { right: -1 }, 500);
  assert.deepEqual(tapStep(state, { right: 2 }, 600), { tap: 2, double: -1 });
  tapStep(state, { right: -1 }, 700);
  assert.deepEqual(tapStep(state, { right: 2 }, 700 + TAP_MS + 1), { tap: 2, double: -1 });
  // Another body between is a tap of its own, and either hand may make the second.
  tapStep(state, { right: -1 }, 3000);
  assert.deepEqual(tapStep(state, { left: 1 }, 3100), { tap: 1, double: -1 });
  tapStep(state, {}, 3150);
  assert.deepEqual(tapStep(state, { right: 1 }, 3300), { tap: -1, double: 1 });
});
