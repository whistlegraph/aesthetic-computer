// Keypad, 2026.11.19
// 🛗 The elevator's pad: a piece on the screen at the back of the car
// (panel.mjs). Point at a key and press it.
//
// The host gives it `car`: typed() is the number being dialled (the same one
// the keyboard's digits dial), key(d) adds a digit, clear() empties it,
// go(to) rides ('lobby', 'top', 'home', or a number), floor() is the floor
// the car is at, home() your own floor's number, moving() whether it runs.

const W = 18, H = 12; // a key
const at = (col, row) => ({ x: 3 + col * 20, y: 28 + row * 13, w: W, h: H });
export const KEYS = [
  ...['1', '2', '3', '4', '5', '6', '7', '8', '9'].map((label, k) => ({ label, ...at(k % 3, Math.floor(k / 3)) })),
  { label: 'C', do: 'clear', ...at(0, 3) },
  { label: '0', ...at(1, 3) },
  { label: 'go', do: 'go', ...at(2, 3) },
  { label: 'G', do: 'lobby', ...at(0, 3), y: 82 },
  { label: 'top', do: 'top', ...at(1, 3), y: 82 },
  { label: 'home', do: 'home', ...at(2, 3), y: 82 },
];
export const keyAt = (x, y) => KEYS.find((k) => x >= k.x && x < k.x + k.w && y >= k.y && y < k.y + k.h) || null;

let over = null, down = null; // the key under the mouse, and the one pressed

function boot() {
  over = down = null;
}

function paint({ wipe, ink, box, write, text, screen, car }) {
  wipe(16, 20, 30);
  // The readout: the number being dialled, or the floor the car is at.
  const typed = car.typed(), floor = car.floor();
  const number = typed || (floor ? `${floor}` : 'G');
  ink(8, 10, 16).box(3, 3, screen.width - 6, 22);
  ink(120, 130, 150).write(typed ? 'to' : car.moving() ? 'passing' : 'floor', { x: 6, y: 5 });
  ink(typed ? [255, 224, 102] : [255, 50, 150]).write(number, { x: screen.width - 6 - text.width(number, 2), y: 8, size: 2 });
  for (const k of KEYS) {
    const quick = k.y > 80, pressed = k === down && k === over;
    const face = pressed ? [255, 224, 102] : k === over ? [70, 84, 116] : quick ? [36, 44, 64] : k.do ? [52, 44, 70] : [44, 52, 74];
    ink(face).box(k.x, k.y + (pressed ? 1 : 0), k.w, k.h - 1);
    if (!pressed) ink(8, 10, 16).box(k.x, k.y + k.h - 1, k.w, 1); // the key's foot: it goes when the key is down
    ink(pressed ? [16, 20, 30] : k.do === 'go' ? [155, 231, 160] : k.do ? [255, 122, 200] : [244, 241, 234])
      .write(k.label, { x: k.x + Math.floor((k.w - text.width(k.label)) / 2), y: k.y + 2 + (pressed ? 1 : 0) });
  }
}

function act({ event: e, car }) {
  if (e.is('move') || e.is('draw')) over = keyAt(e.x, e.y);
  if (e.is('touch')) over = down = keyAt(e.x, e.y);
  if (e.is('lift')) {
    // A key is pressed when the lift is on the key the touch began on.
    const k = keyAt(e.x, e.y);
    if (k && k === down) {
      if (!k.do) car.key(k.label);
      else if (k.do === 'clear') car.clear();
      else if (k.do === 'go') { if (car.typed()) car.go(car.typed()); }
      else car.go(k.do);
    }
    down = null;
  }
}

function leave() {
  over = down = null;
}

export { boot, paint, act, leave };
