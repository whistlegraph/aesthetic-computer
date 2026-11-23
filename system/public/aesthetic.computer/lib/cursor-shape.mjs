// Cursor Shape, 26.11.20
// Rax draws the mouse's cyan cross over world panels. A gamepad uses a
// small neutral point on the selected pixel.

// How far it reaches from its middle, each way: a canvas for it is
// REACH * 2 + 1 across with the middle at REACH.
export const REACH = 7;
export const INKS = {
  glow: [232, 251, 255], glowDown: [255, 240, 176],
  shadow: [0, 0, 0],
  ink: [0, 255, 255], inkDown: [255, 224, 102],
};

// → [[dx, dy, [r, g, b], a]], in drawing order (the dab, the shadow, the
// arms), dx right and dy down from the pixel the cursor is on; `a` is how
// much of the colour shows over what lies under it.
export function cursorCells(down = false) {
  const out = [], glow = down ? INKS.glowDown : INKS.glow, ink = down ? INKS.inkDown : INKS.ink;
  for (let dy = -5; dy <= 5; dy++)
    for (let dx = -5; dx <= 5; dx++) {
      const r = Math.hypot(dx, dy);
      if (r < 5.5) out.push([dx, dy, glow, 0.42 * (1 - r / 5.5) ** 1.4]);
    }
  const gap = down ? 2 : 3; // an arm's first pixel out from the dot
  for (const [off, color, a] of [[1, INKS.shadow, 0.3], [0, ink, 0.92]]) {
    out.push([off, off, color, a]);
    for (const at of [gap, -gap - 3])
      for (let k = 0; k < 4; k++) out.push([off + at + k, off, color, a], [off, off + at + k, color, a]);
  }
  return out;
}

// The cells laid over a picture: `under(dx, dy)` → [r, g, b] of what is there
// (or null off its edge). → Map "dx,dy" → [r, g, b], each cell's colours
// mixed in order. For a surface that has no see-through of its own.
export function cursorOver(under, down = false, cross = true) {
  const out = new Map();
  const cells = cross ? cursorCells(down) : [
    [1, 1, INKS.shadow, 0.85], [0, 0, down ? INKS.inkDown : [255, 255, 255], 1],
  ];
  for (const [dx, dy, [r, g, b], a] of cells) {
    const key = `${dx},${dy}`, was = out.get(key) || under(dx, dy);
    if (!was) continue;
    out.set(key, [was[0] + (r - was[0]) * a, was[1] + (g - was[1]) * a, was[2] + (b - was[2]) * a]);
  }
  return out;
}
