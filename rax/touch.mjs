// Multitouch: the left half is a floating move stick anchored where the thumb
// lands; the right half drags to look. Both are outlined on the lower half of
// the screen. While the build ring is open every touch is a tap on the ring.
// Four icons sit bottom right (2026-11-15): the $ menu (travel, your $ list,
// the view's switches), jump, chat (a message bubble) and build (a cube). Whatever else can be done to the thing in view appears
// as a cap above them and goes when the thing does. Each presses and
// releases its key. The left column, and tap and hold on the look side, are
// retired.
const B = (name, code, key, color, icon = null) => ({ name, code, key, color, icon });
// Eleven columns of ink on the cap's colour.
export const ICONS = {
  cube: ['..########.', '.#......##.', '########.#.', '#......#.#.', '#......#.#.', '#......#.#.', '#......#.#.', '#......#.#.', '#......##..', '########...'],
  bubble: ['.#########.', '#.........#', '#.........#', '#.##.#.##.#', '#.##.#.##.#', '#.........#', '#.........#', '.#########.', '..###......', '..##.......', '..#........'],
  dollar: ['.....#.....', '...######..', '..##.#.....', '..##.#.....', '...#####...', '.....#.##..', '.....#.##..', '..######...', '.....#.....'],
  jump: ['.....#.....', '....###....', '...#####...', '..#######..', '.#########.', '....###....', '....###....', '....###....', '....###....', '....###....'],
};
export const TOUCH_BUTTONS = {
  // Right to left along the bottom.
  play: [B('build', 'Enter', 'Enter', 0xffe066, 'cube'), B('chat', 'KeyT', 't', 0x9be7a0, 'bubble'), B('jump', 'Space', ' ', 0x6bb6ff, 'jump'), B('menu', 'Digit4', '$', 0xffd28a, 'dollar')],
  typing: [B('send', 'Enter', 'Enter', 0xffe066), B('cancel', 'Escape', 'Escape', 0xff6b6b)],
  // Chalk in the air: hold draw and look around, then build what you drew.
  chalk: [B('build', 'Enter', 'Enter', 0xffe066), B('cancel', 'Escape', 'Escape', 0xff6b6b), B('draw', 'KeyC', 'c', 0xe8e8e8)],
};
// What the thing in view offers, as caps: an item in hand, a wall, an object
// of your own, the floor.
export const CONTEXT = {
  use: (verb) => B(verb, 'KeyF', 'f', 0xff9f43),
  drop: B('drop', 'KeyQ', 'q', 0xff6b6b),
  change: B('change', 'KeyF', 'f', 0xffe066),
  remove: B('remove', 'Delete', 'Delete', 0xff6b6b),
  paint: B('paint', 'KeyX', 'x', 0xff7ac8),
  lock: B('lock on', 'KeyZ', 'z', 0x00ffff),
  unlock: B('unlock', 'KeyZ', 'z', 0x00ffff),
  chalk: B('chalk', 'KeyC', 'c', 0xe8e8e8),
  ring: B('ring', '', '', 0x9be7a0), // The client gives it its act.
};
const SIZE = 44, GAP = 6, EDGE = 10;
// Where they sit, in CSS pixels. `height` is what the phone's keyboard leaves
// in view; `inset` is the home bar's.
export function touchButtons(mode, width, height, { context = [], inset = 0 } = {}) {
  const list = TOUCH_BUTTONS[mode] || [];
  const bottom = height - inset - EDGE;
  // Words, not icons: one row, with what the moment adds to its left.
  if (mode === 'typing' || mode === 'chalk')
    return [...list, ...context].map((b, i) => ({ ...b, x: width - EDGE - 62 - i * (62 + GAP), y: bottom - 36, w: 62, h: 36 }));
  const row = list.map((b, i) => ({ ...b, x: width - EDGE - SIZE - i * (SIZE + GAP), y: bottom - SIZE, w: SIZE, h: SIZE }));
  if (!row.length) return row;
  const caps = context.map((b, i) => ({ ...b, x: width - EDGE - 68, y: bottom - SIZE - (i + 1) * (34 + GAP), w: 68, h: 34 }));
  return [...row, ...caps];
}
// The two thumb areas, outlined on the lower half above the icons.
export function touchZones(width, height, inset = 0) {
  // Clear of the avatar's face and name, bottom left.
  const top = Math.round(height / 2), bottom = height - inset - 104;
  if (bottom - top < 60) return [];
  const h = bottom - top, mid = width / 2;
  return [
    { name: 'move', side: 'left', x: EDGE, y: top, w: mid - EDGE - GAP / 2, h },
    { name: 'look', side: 'right', x: mid + GAP / 2, y: top, w: mid - EDGE - GAP / 2, h },
  ];
}
export class TouchPad {
  constructor() {
    this.reset();
    this.active = false;
  }
  reset() {
    this.stick = null;
    this.look = null;
    this.menu = new Map();
    this.pressed = new Map(); // Pointer id to the button under it.
    this.dx = this.dy = 0;
    this.events = [];
  }
  radius(width, height) {
    return Math.max(36, Math.min(width, height) * 0.12);
  }
  // Returns the button the touch landed on, if any.
  down({ pointerId: id, clientX: x, clientY: y }, width, height, now, menu = false, buttons = []) {
    this.active = true;
    const hit = buttons.find((b) => x >= b.x - 4 && x <= b.x + b.w + 4 && y >= b.y - 3 && y <= b.y + b.h + 3);
    if (hit) {
      this.pressed.set(id, hit);
      return hit;
    }
    if (menu) return void this.menu.set(id, { x, y });
    if (x < width / 2 && !this.stick) this.stick = { id, ox: x, oy: y, x, y };
    else if (!this.look)
      this.look = { id, x, y };
  }
  move({ pointerId: id, clientX: x, clientY: y }, width, height) {
    if (this.stick?.id === id) {
      const r = this.radius(width, height);
      // The anchor trails a thumb that overshoots, so reversing is immediate.
      const dx = x - this.stick.ox,
        dy = y - this.stick.oy,
        d = Math.hypot(dx, dy);
      if (d > r) {
        this.stick.ox = x - (dx / d) * r;
        this.stick.oy = y - (dy / d) * r;
      }
      this.stick.x = x;
      this.stick.y = y;
    } else if (this.look?.id === id) {
      this.dx += x - this.look.x;
      this.dy += y - this.look.y;
      this.look.x = x;
      this.look.y = y;
    }
  }
  // Returns the button the touch was holding, if any.
  up({ pointerId: id, clientX: x, clientY: y }, now) {
    const held = this.pressed.get(id);
    if (held) {
      this.pressed.delete(id);
      return held;
    }
    if (this.menu.has(id)) {
      this.menu.delete(id);
      this.events.push({ type: 'tap', x, y, menu: true });
    } else if (this.stick?.id === id) this.stick = null;
    else if (this.look?.id === id) this.look = null;
  }
  cancel({ pointerId: id }) {
    const held = this.pressed.get(id);
    this.pressed.delete(id);
    this.menu.delete(id);
    if (this.stick?.id === id) this.stick = null;
    if (this.look?.id === id) this.look = null;
    return held;
  }
  // Once per frame: stick vector, look delta in CSS pixels, ring taps.
  sample(width, height, now) {
    const result = { right: 0, fwd: 0, dx: this.dx, dy: this.dy, events: this.events };
    this.dx = this.dy = 0;
    this.events = [];
    if (this.stick) {
      const r = this.radius(width, height);
      const x = (this.stick.x - this.stick.ox) / r,
        y = (this.stick.y - this.stick.oy) / r;
      const length = Math.hypot(x, y);
      if (length > 0.15) {
        const strength = Math.min(1, (length - 0.15) / 0.85);
        result.right = (x / length) * strength;
        result.fwd = (-y / length) * strength;
      }
    }
    return result;
  }
  // What the view draws, in CSS pixels.
  state() {
    return this.active
      ? {
          stick: this.stick && { ox: this.stick.ox, oy: this.stick.oy, x: this.stick.x, y: this.stick.y },
          look: this.look && { x: this.look.x, y: this.look.y },
        }
      : null;
  }
}
