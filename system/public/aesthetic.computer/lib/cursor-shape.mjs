// Cursor Shape, 26.11.20
// The one software cursor: a soft dab of light with a crosshair over it, in
// whole pixels. rax lays it on the pixels of a world panel (rax/view.mjs) and
// the pad's cursor lays it on a piece's pixels (lib/pad-cursor.mjs), so a
// stick moves the same mark on the paper as an aim moves on a screen in the
// world.
//
// The dab is found at a glance where thin arms alone are lost, most of all
// when a stick is moving it: whole at the middle, every other pixel round
// that, a scatter at its edge (softness is a stipple: nothing here is
// see-through). Over it a dot and four arms four long, cyan on a black
// shadow one down and across; pressed, the arms close up by a pixel and the
// whole of it warms to yellow.

// How far it reaches from its middle, each way: a canvas for it is
// REACH * 2 + 1 across with the middle at REACH.
export const REACH = 7;
export const INKS = {
  glow: [232, 251, 255], glowDown: [255, 240, 176],
  shadow: [0, 0, 0],
  ink: [0, 255, 255], inkDown: [255, 224, 102],
};

// → [[dx, dy, [r, g, b], layer]], in drawing order (layer 0 the dab, 1 the
// shadow, 2 the arms), dx right and dy down from the pixel the cursor is on.
export function cursorCells(down = false) {
  const out = [], glow = down ? INKS.glowDown : INKS.glow, ink = down ? INKS.inkDown : INKS.ink;
  for (let dy = -5; dy <= 5; dy++)
    for (let dx = -5; dx <= 5; dx++) {
      const r = Math.hypot(dx, dy), ax = Math.abs(dx), ay = Math.abs(dy);
      if (r > 5.4) continue;
      if (r > 1.6 && (r <= 3.6 ? (ax + ay) % 2 : ax % 2 || ay % 2 || (ax + ay) % 4)) continue;
      out.push([dx, dy, glow, 0]);
    }
  const gap = down ? 2 : 3; // an arm's first pixel out from the dot
  for (const [off, color, layer] of [[1, INKS.shadow, 1], [0, ink, 2]]) {
    out.push([off, off, color, layer]);
    for (const a of [gap, -gap - 3])
      for (let k = 0; k < 4; k++) out.push([off + a + k, off, color, layer], [off, off + a + k, color, layer]);
  }
  return out;
}
