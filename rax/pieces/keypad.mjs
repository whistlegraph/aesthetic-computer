// Keypad, 2026.11.19
// 🛗 The elevator's pad: a piece on the screen at the back of the car
// (panel.mjs). Point at a key and press it.
//
// Tap either arrow once, or hold it to step through more floors.
export const KEYS = [
  { label: 'up', do: 'up', x: 4, y: 29, w: 56, h: 31 },
  { label: 'down', do: 'down', x: 4, y: 62, w: 56, h: 31 },
];
export const keyAt = (x, y) => KEYS.find((k) => x >= k.x && x < k.x + k.w && y >= k.y && y < k.y + k.h) || null;

let over = null, down = null; // the key under the mouse, and the one pressed

let repeatAt = 0, repeated = false;
function boot() {
  over = down = null;
}

function paint({ wipe, ink, box, write, text, screen, car }) {
  wipe(16, 20, 30);
  // The readout: the number being dialled, or the floor the car is at.
  const typed = car.typed(), floor = car.floor();
  const target = typed || floor;
  const number = String(target) === '-1' ? 'B1' : Number(target) === 0 ? 'G' : String(target);
  ink(8, 10, 16).box(3, 3, screen.width - 6, 22);
  ink(120, 130, 150).write(typed ? 'to' : car.moving() ? 'passing' : 'floor', { x: 6, y: 5 });
  const size = text.width(number, 2) > screen.width - 30 ? 1 : 2; // (a word, `roof`, is written small)
  ink(typed ? [255, 224, 102] : [255, 50, 150]).write(number, { x: screen.width - 6 - text.width(number, size), y: size === 2 ? 8 : 12, size });
  for (const k of KEYS) {
    const pressed = k === down && k === over, sink = pressed ? 1 : 0;
    ink(pressed ? [255, 224, 102] : k === over ? [70, 84, 116] : [44, 52, 74]).box(k.x, k.y + sink, k.w, k.h - 1);
    if (!pressed) ink(8, 10, 16).box(k.x, k.y + k.h - 1, k.w, 1); // the key's foot: it goes when the key is down
    // A big arrow: a head of widening rows and a short shaft, pointing the way the car will go.
    ink(pressed ? [16, 20, 30] : k.do === 'up' ? [155, 231, 160] : [255, 159, 67]);
    const cx = k.x + k.w / 2, top = k.y + 5 + sink, rows = 11;
    for (let r = 0; r < rows; r++) {
      const y = k.do === 'up' ? top + r : top + 20 - r, half = 1 + r * 1.5;
      box(Math.round(cx - half), y, Math.round(half * 2), 1);
    }
    box(Math.round(cx - 4), k.do === 'up' ? top + rows : top, 8, 10);
  }
}

function sim({ car }, now = performance.now()) {
  if (!down || over !== down || now < repeatAt) return;
  car.go(down.do);
  repeated = true;
  repeatAt = now + 140;
}

function act({ event: e, car }) {
  if (e.is('move') || e.is('draw')) over = keyAt(e.x, e.y);
  if (e.is('touch')) {
    over = down = keyAt(e.x, e.y);
    repeatAt = performance.now() + 400;
    repeated = false;
  }
  if (e.is('lift')) {
    // A key is pressed when the lift is on the key the touch began on.
    const k = keyAt(e.x, e.y);
    if (k && k === down && !repeated) car.go(k.do);
    down = null;
  }
}

function leave() {
  over = down = null;
}

export { boot, sim, paint, act, leave };
