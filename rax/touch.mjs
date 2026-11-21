// During play, each half of the screen is a gesture surface. Only an open
// editor needs buttons, positioned above the keyboard or the home indicator.
const B = (name, code, key, color) => ({ name, code, key, color });
const buttons = {
  typing: [B('send', 'Enter', 'Enter', 0xffe066), B('cancel', 'Escape', 'Escape', 0xff6b6b)],
  // Chalk in the air: hold draw and look around, then build what you drew.
  chalk: [B('build', 'Enter', 'Enter', 0xffe066), B('cancel', 'Escape', 'Escape', 0xff6b6b), B('draw', 'KeyC', 'c', 0xe8e8e8)],
};
export function touchButtons(mode, width, height, { context = [], inset = 0 } = {}) {
  if (!buttons[mode]) return [];
  return [...buttons[mode], ...context].map((b, i) => ({
    ...b, x: width - 10 - 62 - i * 68, y: height - inset - 10 - 36, w: 62, h: 36,
  }));
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
