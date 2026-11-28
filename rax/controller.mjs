// Standard Gamepad mapping covers 8BitDo Pro in XInput/Switch modes.
export const neutralPad = () => ({
  right: 0,
  fwd: 0,
  lookX: 0,
  lookY: 0,
  roll: 0,
  boost: false,
  jump: false,
  prompt: false,
  hold: false,
  select: false,
  focus: false,
  zoomClick: false,
  zoomHeld: false,
  zoomRelease: false,
  triggerHeld: false,
  use: false,
  dup: false,
  ddown: false,
  dleft: false,
  dright: false,
  debug: false,
  camera: false,
  cancel: false,
  backHeld: false,
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
    const result = neutralPad();
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
      // A jumps and chooses (on a screen it presses, and does not jump); the left trigger is the focus zoom (`lt`, below).
      result.jump = edge(0);
      result.select = edge(0);
      result.zoomClick = edge(10);
      result.zoomHeld = !!buttons[10] && !changed && this.focused;
      result.zoomRelease = !buttons[10] && this.previous[10] && !changed && this.focused;
      result.buildPress = edge(4);
      result.buildHeld = !!buttons[4] && !changed && this.focused;
      result.buildRelease = !buttons[4] && this.previous[4] && !changed && this.focused;
      result.focus = edge(5);
      result.use = edge(7);
      result.triggerHeld = !!buttons[7] && !changed && this.focused;
      result.dup = edge(12);
      result.ddown = edge(13);
      result.floorDirection = Number(!!buttons[12]) - Number(!!buttons[13]);
      result.dleft = edge(14);
      result.dright = edge(15);
      result.hold = !!buttons[0] && !changed && this.focused;
      result.cancel = edge(1);
      result.backHeld = !!buttons[1] && !changed && this.focused; // (B held: the bike sinks)
      result.prompt = edge(2);
      result.paintHeld = !!buttons[2] && !changed && this.focused; // (X held: the brush drags)
      result.camera = edge(3);
      result.cameraHeld = !!buttons[3] && !changed && this.focused; // (Y held: step back to a drone, client.mjs)
      result.start = edge(9);
      result.debug = edge(11); // the right stick, clicked: the debugging layer
      result.swap = edge(8);
      result.roll = Number(!!buttons[4]) - Number(!!buttons[5]);
      result.boost = !!buttons[7];
      // Analog trigger pressure, including LT focus zoom.
      result.lt = pad.buttons[6]?.value ?? Number(buttons[6]);
      result.rt = pad.buttons[7]?.value ?? Number(buttons[7]);
      // Past the standard seventeen: a pad's own extras (back paddles, L4 and R4), just pressed.
      result.extras = buttons.map((_, i) => i).filter((i) => i > 16 && edge(i));
    }
    this.previous = buttons;
    this.focused = focused;
    return result;
  }
}
