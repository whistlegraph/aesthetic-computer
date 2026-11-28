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
// 🎯 The sticks, as console shooters that are played to win have them
// (2026-11-27, from what is published of them: Kevin Ng's first-person
// control design, measurements of Halo's and Call of Duty's turn, and the
// presets players settle on in Apex and Warzone):
//   - a round deadzone, small, with what is left of the travel rescaled to
//     the whole range, so there is no step as the stick leaves it;
//   - an outer threshold short of the rim (OUTER), since a worn stick never
//     quite reaches 1 and full speed must be reachable;
//   - for looking, a curve on the stick's whole reach, not on each axis
//     (LOOK_CURVE, about 1.6: fine near the middle, quick at the edge, and a
//     diagonal is not bent toward an axis);
//   - no smoothing of any of it. What is added for turning far (the boost at
//     full tilt) is the client's, and only at the rim (TURN_*).
export const OUTER = 0.97, LOOK_CURVE = 1.6;
export const TURN_RIM = 0.92, TURN_BOOST = 0.7, TURN_RAMP = 0.3; // past TURN_RIM across, the turn grows by TURN_BOOST over TURN_RAMP seconds
export function stick(x = 0, y = 0, deadzone = 0.16, outer = 1) {
  if (!Number.isFinite(x) || !Number.isFinite(y)) return [0, 0];
  const length = Math.hypot(x, y);
  if (length <= deadzone) return [0, 0];
  const strength = Math.min(1, (length - deadzone) / (Math.max(deadzone + 0.01, outer) - deadzone));
  return [(x / length) * strength, (y / length) * strength];
}
// The look stick's reach, curved: the same way it points, its length raised to LOOK_CURVE.
export function lookCurve([x, y], power = LOOK_CURVE) {
  const length = Math.hypot(x, y);
  if (length < 1e-6) return [0, 0];
  const k = Math.min(1, length) ** power / length;
  return [x * k, y * k];
}
// 🎯 The deadzone is learned from the stick itself. A stick left alone sits
// at some small offset (its drift) and shivers a little round it. Once a
// stick has lain still for a second and a half its resting point is taken
// as its middle, and its deadzone shrinks to just cover the shiver — far
// smaller than the fixed one a stick starts with, so it answers a light push.
// (The least deadzone a stick is brought to once its rest is known: three hundredths. Before that, FIRST: enough for a stick that drifts a little, a third of what it was.)
const REST_FRAMES = 90, REST_REACH = 0.3, REST_STEP = 0.015, DEADZONE_MIN = 0.03, FIRST = [0.1, 0.08];
const newRest = () => ({ cx: 0, cy: 0, px: 0, py: 0, still: 0, noise: 0, known: false });
export class Controller {
  constructor() {
    this.rests = new Map(); // by pad index: its two sticks' resting points
    this.deadzones = [...FIRST]; // left, right, as last sampled
    this.previous = [];
    this.id = null;
    this.index = null;
    this.focused = false;
  }
  // One pad's four axes with each stick's learned resting point taken away.
  settle(p) {
    if (!this.rests.has(p.index)) this.rests.set(p.index, [newRest(), newRest()]);
    const rests = this.rests.get(p.index), out = [0, 0, 0, 0];
    for (let s = 0; s < 2; s++) {
      const r = rests[s], x = p.axes[2 * s] || 0, y = p.axes[2 * s + 1] || 0;
      r.still = Math.hypot(x, y) < REST_REACH && Math.hypot(x - r.px, y - r.py) < REST_STEP ? r.still + 1 : 0;
      r.px = x; r.py = y;
      if (r.still >= REST_FRAMES) {
        if (!r.known) { r.cx = x; r.cy = y; r.known = true; }
        r.cx += (x - r.cx) * 0.05; r.cy += (y - r.cy) * 0.05;
        r.noise = Math.max(r.noise * 0.999, Math.hypot(x - r.cx, y - r.cy));
      }
      out[2 * s] = x - r.cx; out[2 * s + 1] = y - r.cy;
    }
    return out;
  }
  sample(gamepads, focused = true) {
    // Every connected controller at once: buttons from any, each stick from
    // whichever is pushed furthest. A second controller plugged in (or one
    // switched on later) just works, whichever index the browser gives it.
    const pads = Array.from(gamepads || []).filter((p) => p?.connected && p.axes.length >= 2);
    for (const key of this.rests.keys()) if (!pads.some((p) => p.index === key)) this.rests.delete(key);
    const centred = new Map(pads.map((p) => [p, this.settle(p)]));
    const ax = (p, i) => centred.get(p)[i];
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
            const best = pads.reduce((a, p) => (Math.hypot(ax(p, pair[0]), ax(p, pair[1])) > Math.hypot(ax(a, pair[0]), ax(a, pair[1])) ? p : a), pads[0]);
            return ax(best, i);
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
      // Each stick's deadzone: the widest any connected pad still needs.
      this.deadzones = FIRST.map((fixed, s) =>
        Math.max(...pads.map((p) => {
          const r = this.rests.get(p.index)?.[s];
          return r?.known ? Math.min(fixed, Math.max(DEADZONE_MIN, r.noise * 2 + 0.03)) : fixed;
        })));
      const left = stick(pad.axes[0], pad.axes[1], this.deadzones[0], OUTER),
        right = stick(pad.axes[2], pad.axes[3], this.deadzones[1], OUTER);
      result.right = left[0];
      result.fwd = -left[1];
      [result.lookX, result.lookY] = lookCurve(right);
      result.lookRim = Math.abs(right[0]); // (how far across the look stick is, before the curve: the client's turn boost reads it)
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
