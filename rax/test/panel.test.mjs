import test from 'node:test';
import assert from 'node:assert/strict';
import { panelHit, painter, pointerEvent, createPanels, textWidth, PANEL_REACH } from '../panel.mjs';
import * as keypad from '../pieces/keypad.mjs';
import { CAR_PAD, CAR, CAR_Z, inCar } from '../elevator.mjs';

const pad = { x: 0, y: 1.5, z: 5, yaw: 0, w: 0.6, h: 0.9, cols: 64, rows: 96 };
const north = { x: 0, y: 0, z: 1 };

test('the aim meets a panel in its own pixels: x from its left as it is read, y from its top', () => {
  const eye = { x: 0, y: 1.5, z: 3 };
  const mid = panelHit(pad, eye, north);
  assert.ok(Math.abs(mid.t - 2) < 1e-9);
  assert.deepEqual([mid.x, mid.y], [32, 48]);
  // Facing north, +x is on the left: the top left pixel is up and to +x.
  const corner = panelHit(pad, { x: 0.3 - 0.001, y: 1.95 - 0.001, z: 3 }, north);
  assert.deepEqual([Math.floor(corner.x), Math.floor(corner.y)], [0, 0]);
  const far = panelHit(pad, { x: -0.3 + 0.001, y: 1.05 + 0.001, z: 3 }, north);
  assert.deepEqual([Math.floor(far.x), Math.floor(far.y)], [63, 95]);
});

test('a panel is not met past its edge, from behind, looking away, or out of reach', () => {
  assert.equal(panelHit(pad, { x: 0.31, y: 1.5, z: 3 }, north), null);
  assert.equal(panelHit(pad, { x: 0, y: 2, z: 3 }, north), null);
  assert.equal(panelHit(pad, { x: 0, y: 1.5, z: 6 }, { x: 0, y: 0, z: -1 }), null, 'from behind');
  assert.equal(panelHit(pad, { x: 0, y: 1.5, z: 3 }, { x: 0, y: 0, z: -1 }), null, 'looking away');
  assert.equal(panelHit(pad, { x: 0, y: 1.5, z: 1 }, north, PANEL_REACH), null, 'four metres off');
  assert.ok(panelHit(pad, { x: 0, y: 1.5, z: 2.5 }, north, PANEL_REACH));
});

test('a panel turned by its yaw is met as the view draws it', () => {
  // The view turns a point (lx, lz) to (lx cos + lz sin, -lx sin + lz cos):
  // at 90 degrees the face (local -z) looks west and local +x lies to the south.
  const turned = { ...pad, x: 5, z: 0, yaw: 90 };
  const hit = panelHit(turned, { x: 3, y: 1.5, z: 0.1 }, { x: 1, y: 0, z: 0 });
  assert.ok(Math.abs(hit.t - 2) < 1e-9);
  assert.ok(hit.x > 32, 'north of the middle is the right half of the face');
  assert.equal(panelHit(turned, { x: 7, y: 1.5, z: 0 }, { x: -1, y: 0, z: 0 }), null);
});

test('a piece draws with wipe, ink, box, line and write', () => {
  const g = painter(16, 12), at = (x, y) => [...g.screen.pixels.slice((y * 16 + x) * 4, (y * 16 + x) * 4 + 3)];
  g.wipe(10, 20, 30);
  assert.deepEqual(at(15, 11), [10, 20, 30]);
  g.ink('red').box(2, 2, 3, 3);
  assert.deepEqual([at(2, 2), at(4, 4), at(5, 5)], [[255, 0, 0], [255, 0, 0], [10, 20, 30]]);
  g.ink(255).box(8, 2, 4, 4, 'outline');
  assert.deepEqual([at(8, 2), at(11, 5), at(9, 3)], [[255, 255, 255], [255, 255, 255], [10, 20, 30]]);
  g.ink([0, 255, 0]).line(0, 11, 15, 11);
  assert.deepEqual([at(0, 11), at(15, 11)], [[0, 255, 0], [0, 255, 0]]);
  // "1" in MatrixChunky8 is three wide: its second row is two pixels, the left two.
  g.wipe(0).ink('#ffffff').write('1', { x: 1, y: 0 });
  assert.deepEqual([at(2, 0), at(1, 1), at(2, 1), at(3, 1)], [[255, 255, 255], [255, 255, 255], [255, 255, 255], [0, 0, 0]]);
  assert.equal(textWidth('10'), 7);
  assert.equal(textWidth('10', 2), 14);
  g.box(-5, -5, 100, 100); // off the edges: nothing thrown
});

test('a pointer event answers is() as an AC event does', () => {
  const e = pointerEvent('touch', 3, 4);
  assert.ok(e.is('touch') && !e.is('lift') && !e.is('tou'));
  assert.deepEqual([e.x, e.y], [3, 4]);
  assert.ok(pointerEvent('touch:1', 0, 0).is('touch'));
});

// A car as the page gives one to the keypad.
const fakeCar = (state = {}) => {
  const car = { text: '', went: [], here: 0, mine: 7, ...state };
  return Object.assign(car, {
    typed: () => car.text, key: (d) => { car.text = (car.text + d).slice(-4); }, clear: () => { car.text = ''; },
    go: (to) => { car.went.push(to); car.text = ''; }, floor: () => car.here, home: () => car.mine, moving: () => false,
  });
};
const middle = (label) => { const k = keypad.KEYS.find((q) => q.label === label); return [k.x + k.w / 2, k.y + k.h / 2]; };
const press = (car, label) => {
  for (const name of ['move', 'touch', 'lift']) keypad.act({ event: pointerEvent(name, ...middle(label)), car });
};

test('the keypad dials digits, clears, and goes', () => {
  const car = fakeCar();
  keypad.boot();
  press(car, '8'); press(car, '9');
  assert.equal(car.text, '89');
  press(car, 'C');
  assert.equal(car.text, '');
  press(car, 'go');
  assert.deepEqual(car.went, [], 'nothing dialled: go goes nowhere');
  press(car, '1'); press(car, '0'); press(car, 'go');
  assert.deepEqual(car.went, ['10']);
  for (const d of '12345') press(car, d);
  assert.equal(car.text, '2345', 'four digits, as the keys dial');
});

test('the quick keys ride to the lobby, the top and home', () => {
  const car = fakeCar();
  keypad.boot();
  press(car, 'G'); press(car, 'top'); press(car, 'home');
  assert.deepEqual(car.went, ['lobby', 'top', 'home']);
});

test('a key is pressed only when the lift is on the key the touch began on', () => {
  const car = fakeCar();
  keypad.boot();
  keypad.act({ event: pointerEvent('touch', ...middle('5')), car });
  keypad.act({ event: pointerEvent('draw', ...middle('6')), car });
  keypad.act({ event: pointerEvent('lift', ...middle('6')), car });
  keypad.act({ event: pointerEvent('touch', ...middle('5')), car });
  keypad.act({ event: pointerEvent('lift', -1, -1), car });
  assert.equal(car.text, '');
  assert.equal(keypad.keyAt(0, 0), null);
  assert.equal(new Set(keypad.KEYS.map((k) => keypad.keyAt(k.x, k.y))).size, keypad.KEYS.length, 'no key lies over another');
});

test('the keypad shows what is dialled, and the floor when nothing is', () => {
  const car = fakeCar({ here: 3 }), g = painter(64, 96);
  keypad.boot();
  keypad.paint({ ...g, car });
  const floor = g.screen.pixels.slice();
  car.text = '12';
  keypad.paint({ ...g, car });
  assert.ok(floor.some((v, k) => v !== g.screen.pixels[k]), 'the readout changed');
});

test('the virtual mouse: move, touch, draw and lift go to the panel under the aim', () => {
  const heard = [], piece = { act: ({ event: e }) => heard.push(`${e.name} ${e.x},${e.y}`), paint: ({ wipe }) => wipe(0) };
  const panels = createPanels(), p = panels.mount({ id: 'p', piece, ...pad });
  const eye = { x: 0, y: 1.5, z: 3 };
  panels.point(panels.aim(eye, north));
  assert.equal(panels.hot.panel, p);
  assert.deepEqual(p.cursor, { x: 32, y: 48, down: false });
  assert.ok(panels.press());
  panels.point(panels.aim({ ...eye, x: 0.1 }, north));
  panels.release();
  assert.equal(p.cursor.down, false);
  // Off the edge: the piece hears the mouse leave, and the cursor is gone.
  panels.point(panels.aim({ ...eye, x: 2 }, north));
  assert.deepEqual(heard, ['move 32,48', 'touch 32,48', 'draw 21,48', 'lift 21,48', 'move -1,-1']);
  assert.equal(p.cursor, null);
  assert.equal(panels.hot, null);
  assert.equal(panels.press(), false, 'nothing under the aim: nothing pressed');
});

test('a press dragged off a panel is let go in the open; a tap lifts by itself', () => {
  const heard = [], piece = { act: ({ event: e }) => heard.push(`${e.name} ${e.x},${e.y}`) };
  const panels = createPanels();
  panels.mount({ id: 'p', piece, ...pad });
  const eye = { x: 0, y: 1.5, z: 3 };
  panels.point(panels.aim(eye, north));
  panels.press();
  panels.point(null);
  assert.deepEqual(heard.slice(-2), ['lift -1,-1', 'move -1,-1']);
  heard.length = 0;
  panels.point(panels.aim(eye, north));
  assert.ok(panels.tap(1000));
  panels.frame(1050);
  assert.deepEqual(heard, ['move 32,48', 'touch 32,48']);
  panels.frame(1200);
  assert.deepEqual(heard.slice(-1), ['lift 32,48']);
});

test('a finger presses where it lands and the crosshair waits for it', () => {
  const heard = [], piece = { act: ({ event: e }) => heard.push(`${e.name} ${e.x},${e.y}`) };
  const panels = createPanels();
  panels.mount({ id: 'p', piece, ...pad });
  const eye = { x: 0, y: 1.5, z: 3 };
  assert.ok(panels.pressAt(panels.aim({ ...eye, x: 0.1 }, north)));
  panels.point(panels.aim(eye, north)); // the crosshair, elsewhere on the panel
  panels.release();
  assert.deepEqual(heard, ['move 21,48', 'touch 21,48', 'lift 21,48', 'move -1,-1']);
  assert.equal(panels.pressAt(null), false);
});

test('a panel counts a frame only when its picture changes; a hidden one is not aimed at', () => {
  let n = 0;
  const panels = createPanels(), p = panels.mount({ id: 'p', piece: { paint: ({ wipe }) => wipe(n) }, ...pad });
  panels.frame(0); panels.frame(16);
  assert.equal(p.frame, 1);
  n = 9;
  panels.frame(32);
  assert.equal(p.frame, 2);
  p.hidden = true;
  assert.equal(panels.aim({ x: 0, y: 1.5, z: 3 }, north), null);
});

test('the car\'s pad is on its back wall, facing the door, in reach of anyone in the car', () => {
  assert.ok(CAR_PAD.z < CAR_Z + CAR && CAR_PAD.z > CAR_Z + CAR - 0.1, 'just inside the back wall');
  assert.ok(CAR_PAD.z + 0.004 + CAR_PAD.deep < CAR_Z + CAR + 0.06, 'its slab stops short of the mullion');
  assert.ok(CAR_PAD.y - CAR_PAD.h / 2 > 0.6 && CAR_PAD.y + CAR_PAD.h / 2 < 2.8, 'between the sill and the rail');
  assert.equal(CAR_PAD.cols / CAR_PAD.rows, CAR_PAD.w / CAR_PAD.h, 'square pixels');
  // From just inside the door, at eye height, looking at its middle.
  const eye = { x: 0, y: 1.6, z: CAR_Z - CAR + 0.3 };
  assert.ok(inCar({ ...eye, y: 0 }));
  const to = { x: 0, y: CAR_PAD.y - eye.y, z: CAR_PAD.z - eye.z }, l = Math.hypot(to.y, to.z);
  const hit = panelHit(CAR_PAD, eye, { x: 0, y: to.y / l, z: to.z / l }, PANEL_REACH);
  assert.ok(hit && Math.abs(hit.x - 32) < 1e-6 && Math.abs(hit.y - 48) < 1e-6);
});
