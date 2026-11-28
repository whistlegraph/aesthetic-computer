import test from 'node:test';
import assert from 'node:assert/strict';
import { tableEye, wristOf, pokerOf, Headset, handOutline, handsPacked, handsValid, handsUnpacked, HAND_NUMBERS, roomPlanes } from '../immersive.mjs';

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
