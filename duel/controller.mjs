// Standard Gamepad mapping covers 8BitDo Pro in XInput/Switch modes.
const zero = () => ({
  right: 0,
  fwd: 0,
  lookX: 0,
  lookY: 0,
  up: 0,
  boost: false,
  jump: false,
  prompt: false,
  hold: false,
  select: false,
  ring: false,
  use: false,
  dup: false,
  ddown: false,
  dleft: false,
  dright: false,
  camera: false,
  cancel: false,
  start: false,
  swap: false,
});
export function stick(x = 0, y = 0, deadzone = 0.16) {
  if (!Number.isFinite(x) || !Number.isFinite(y)) return [0, 0];
  const length = Math.hypot(x, y);
  if (length <= deadzone) return [0, 0];
  const strength = Math.min(1, (length - deadzone) / (1 - deadzone));
  return [(x / length) * strength, (y / length) * strength];
}
export class Controller {
  constructor() {
    this.previous = [];
    this.id = null;
    this.index = null;
    this.focused = false;
  }
  sample(gamepads, focused = true) {
    // Every connected controller at once: buttons from any, each stick from
    // whichever is pushed furthest. A second controller plugged in (or one
    // switched on later) just works, whichever index the browser gives it.
    const pads = Array.from(gamepads || []).filter((p) => p?.connected && p.axes.length >= 2);
    const pad = pads.length
      ? {
          id: pads.map((p) => p.id).join(' + '),
          index: pads.map((p) => p.index).join(','),
          buttons: Array.from({ length: Math.max(...pads.map((p) => p.buttons.length)) }, (_, i) => ({
            pressed: pads.some((p) => p.buttons[i]?.pressed || (p.buttons[i]?.value || 0) > 0.5),
            value: Math.max(...pads.map((p) => p.buttons[i]?.value || 0)),
          })),
          axes: Array.from({ length: 4 }, (_, i) => {
            const pair = i < 2 ? [0, 1] : [2, 3];
            const best = pads.reduce((a, p) => (Math.hypot(p.axes[pair[0]] || 0, p.axes[pair[1]] || 0) > Math.hypot(a.axes[pair[0]] || 0, a.axes[pair[1]] || 0) ? p : a), pads[0]);
            return best.axes[i] || 0;
          }),
        }
      : null;
    const result = zero();
    this.id = pad?.id || null;
    if (!pad) {
      this.previous = [];
      this.index = null;
      this.focused = false;
      return result;
    }
    const buttons = pad.buttons.map((b) => !!(b.pressed || b.value > 0.5));
    const changed = this.index !== pad.index;
    this.index = pad.index;
    const edge = (i) =>
      buttons[i] && !this.previous[i] && !changed && this.focused;
    if (focused) {
      const left = stick(pad.axes[0], pad.axes[1]),
        right = stick(pad.axes[2], pad.axes[3], 0.18);
      result.right = left[0];
      result.fwd = -left[1];
      result.lookX = Math.sign(right[0]) * Math.abs(right[0]) ** 1.4;
      result.lookY = Math.sign(right[1]) * Math.abs(right[1]) ** 1.4;
      // Left trigger jumps; A is kept for choosing (hold opens the ring).
      result.jump = edge(6);
      result.select = edge(0);
      // LB picks the aimed object (or empty floor) and opens the build ring.
      result.ring = edge(4);
      result.use = edge(5);
      result.dup = edge(12);
      result.ddown = edge(13);
      result.dleft = edge(14);
      result.dright = edge(15);
      result.hold = !!buttons[0] && !changed && this.focused;
      result.cancel = edge(1);
      result.prompt = edge(2);
      result.camera = edge(3);
      result.start = edge(9);
      result.swap = edge(8);
      result.up = Number(buttons[5]) - Number(buttons[4]);
      result.boost = !!buttons[7];
    }
    this.previous = buttons;
    this.focused = focused;
    return result;
  }
}
