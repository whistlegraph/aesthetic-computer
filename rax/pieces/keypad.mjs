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

let over = null;
const presses = new Map();
function boot() {
  over = null;
  presses.clear();
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
    const pressed = [...presses.values()].some(p => p.key === k && p.over === k), sink = pressed ? 1 : 0;
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
  for (const p of presses.values()) {
    if (!p.key || p.over !== p.key || now < p.repeatAt) continue;
    car.go(p.key.do, now - p.start);
    p.repeated = true;
    p.repeatAt = now + 140;
  }
}

function act({ event: e, car }) {
  over = keyAt(e.x, e.y);
  const id = e.pointer ?? 1, p = presses.get(id);
  if (e.is('touch')) {
    const start = performance.now();
    presses.set(id, { key: over, over, start, repeatAt: start + 400, repeated: false });
  } else if (p && e.is('draw')) p.over = over;
  else if (p && e.is('lift')) {
    presses.delete(id);
    if (over && over === p.key && !p.repeated) car.go(over.do);
  }
}
const leave = boot;

export { boot, sim, paint, act, leave };
