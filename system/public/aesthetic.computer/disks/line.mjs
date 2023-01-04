// Line, 22.09.19.12.44
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

// 🎨
// If `params` is empty then ink's RGBA will be randomized.
export function paint({ pen, params, system, page, screen }) {
  if (pen?.drawing) {
    params = params.map((str) => parseInt(str));
    page(system.painting).ink(params).line(pen.px, pen.py, pen.x, pen.y);
    page(screen).ink(params).line(pen.px, pen.py, pen.x, pen.y);
  }
}

export const system = "nopaint:dont-paint-on-leave";