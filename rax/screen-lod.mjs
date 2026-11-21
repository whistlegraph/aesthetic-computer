// 🖥️ How much a standing screen is worth from where the eye is: its pixel
// grid (level of detail) and how often its program steps (computational
// level of detail). A screen across the room is a coarse grid stepping a
// few times a second; walked up to, it sharpens and runs at full rate; out
// of sight it holds its last frame and computes nothing.
//
// The tiers are for a screen SCREEN_SPAN metres wide. `within` is metres
// from the eye to the screen's middle.
export const MAX_FOCUS_ZOOM = 2.5;
export const OBJECT_PROXY_DISTANCE = 64;
export const SCREEN_SPAN = 4;
export const SCREEN_TIERS = [
  { within: 1.5, cols: 256, hz: 20 },
  { within: 4, cols: 192, hz: 20 },
  { within: 10, cols: 128, hz: 20 },
  { within: 22, cols: 64, hz: 12 },
  { within: 45, cols: 32, hz: 6 },
  { within: Infinity, cols: 16, hz: 2 },
];
export const screenRows = (cols) => Math.round((cols * 9) / 16);
// A tier is held until the eye is SLACK past its far edge, so a screen
// does not flicker between two grids at a boundary.
const SLACK = 1.2;
// The grid and the step rate for a screen `distance` metres off. `cols` is
// the grid it has now (0 for none); `seen` is false when it is out of view.
export function screenDetail(distance, { cols = 0, seen = true } = {}) {
  let i = SCREEN_TIERS.findIndex((t) => distance < t.within);
  const held = SCREEN_TIERS.findIndex((t) => t.cols === cols);
  // Coming away from a finer grid: keep it a little longer.
  if (held >= 0 && held < i && distance < SCREEN_TIERS[held].within * SLACK) i = held;
  // Unseen: no steps, and no finer grid than the one it has.
  if (!seen) return { cols: cols || SCREEN_TIERS.at(-1).cols, hz: 0 };
  return { cols: SCREEN_TIERS[i].cols, hz: SCREEN_TIERS[i].hz };
}
// Whether a screen at (x, z), facing `yaw` degrees, is in front of an eye
// looking along `look` (a unit vector on the ground plane): close screens
// always count, the rest must fall inside a wide cone.
export function screenSeen(eye, look, screen) {
  const dx = screen.x - eye.x, dz = screen.z - eye.z, d = Math.hypot(dx, dz);
  if (d < SCREEN_SPAN) return true;
  return (dx * look.x + dz * look.z) / d > 0.2;
}
// One grid's pixels carried into another, nearest cell to nearest cell, so
// a screen that sharpens starts from the picture it already showed.
export function carryPixels(from, fromCols, fromRows, to, toCols, toRows) {
  for (let j = 0; j < toRows; j++) {
    const sj = Math.min(fromRows - 1, Math.floor((j * fromRows) / toRows));
    for (let i = 0; i < toCols; i++) {
      const si = Math.min(fromCols - 1, Math.floor((i * fromCols) / toCols));
      const a = (sj * fromCols + si) * 4, b = (j * toCols + i) * 4;
      to[b] = from[a];
      to[b + 1] = from[a + 1];
      to[b + 2] = from[a + 2];
      to[b + 3] = from[a + 3];
    }
  }
  return to;
}
