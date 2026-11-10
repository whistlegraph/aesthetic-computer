import { HOLD_SECONDS } from './radial.mjs';

// Multitouch: the left half is a floating move stick anchored where the thumb
// lands; the right half drags to look, taps to jump and holds to build.
// While the build ring is open every touch is a tap on the ring instead.
// A column of buttons up the left edge does what the keys do (2026-11-10):
// each one presses and releases its key, so held ones (paint) drag.
const B = (name, code, key, color) => ({ name, code, key, color });
export const TOUCH_BUTTONS = {
  play: [B('build', 'Enter', 'Enter', 0xffe066), B('chat', 'KeyT', 't', 0x9be7a0), B('paint', 'KeyX', 'x', 0xff7ac8),
    B('use', 'KeyF', 'f', 0xff9f43), B('view', 'KeyV', 'v', 0xb46bff), B('rooms', 'KeyR', 'r', 0x2ec4b6)],
  typing: [B('send', 'Enter', 'Enter', 0xffe066), B('cancel', 'Escape', 'Escape', 0xff6b6b)],
};
// Where they sit, in CSS pixels: under the corner label, above the avatar.
export function touchButtons(mode, width, height) {
  const list = TOUCH_BUTTONS[mode] || [];
  const top = 52, gap = 6, w = 62;
  const h = Math.max(24, Math.min(40, (height - top - 104) / Math.max(1, TOUCH_BUTTONS.play.length) - gap));
  return list.map((b, i) => ({ ...b, x: 8, y: top + i * (h + gap), w, h }));
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
    else if (this.look?.id === id) {
      if (!this.look.moved && !this.look.held && now - this.look.at < 300)
        this.events.push({ type: 'tap', x, y });
      this.look = null;
    }
  }
  cancel({ pointerId: id }) {
    const held = this.pressed.get(id);
    this.pressed.delete(id);
    this.menu.delete(id);
    if (this.stick?.id === id) this.stick = null;
    if (this.look?.id === id) this.look = null;
    return held;
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
