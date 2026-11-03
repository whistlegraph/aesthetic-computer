import { HOLD_SECONDS } from './radial.mjs';

// Multitouch: the left half is a floating move stick anchored where the thumb
// lands; the right half drags to look, taps to jump and holds to build.
// While the build ring is open every touch is a tap on the ring instead.
export class TouchPad {
  constructor() {
    this.reset();
    this.active = false;
  }
  reset() {
    this.stick = null;
    this.look = null;
    this.menu = new Map();
    this.dx = this.dy = 0;
    this.events = [];
  }
  radius(width, height) {
    return Math.max(36, Math.min(width, height) * 0.12);
  }
  down({ pointerId: id, clientX: x, clientY: y }, width, height, now, menu = false) {
    this.active = true;
    if (menu) return void this.menu.set(id, { x, y });
    if (x < width / 2 && !this.stick) this.stick = { id, ox: x, oy: y, x, y };
    else if (!this.look)
      this.look = { id, x, y, sx: x, sy: y, at: now, moved: false, held: false };
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
      if (Math.hypot(x - this.look.sx, y - this.look.sy) > 12) this.look.moved = true;
    }
  }
  up({ pointerId: id, clientX: x, clientY: y }, now) {
    if (this.menu.has(id)) {
      this.menu.delete(id);
      this.events.push({ type: 'tap', x, y, menu: true });
    } else if (this.stick?.id === id) this.stick = null;
    else if (this.look?.id === id) {
      if (!this.look.moved && !this.look.held && now - this.look.at < 300)
        this.events.push({ type: 'tap', x, y });
      this.look = null;
    }
  }
  cancel({ pointerId: id }) {
    this.menu.delete(id);
    if (this.stick?.id === id) this.stick = null;
    if (this.look?.id === id) this.look = null;
  }
  // Once per frame: stick vector, look delta in CSS pixels, taps and holds.
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
    const look = this.look;
    if (look && !look.moved && !look.held && now - look.at > HOLD_SECONDS * 1000) {
      look.held = true;
      result.events.push({ type: 'hold', x: look.x, y: look.y });
    }
    return result;
  }
  // What the view draws, in CSS pixels.
  state() {
    return this.active
      ? {
          stick: this.stick && { ox: this.stick.ox, oy: this.stick.oy, x: this.stick.x, y: this.stick.y },
          look: this.look && { x: this.look.x, y: this.look.y, holding: !this.look.moved && !this.look.held, at: this.look.at },
        }
      : null;
  }
}
