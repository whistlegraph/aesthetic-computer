// Rect, 22.09.19.21.07
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

let needsBake = false;
let rect;

// 🎨
export function paint({
  params,
  pen,
  paste,
  ink,
  system,
  screen,
  page,
  geo: { Box },
}) {
  const color = (params.map((str) => parseInt(str)));

  if (rect && needsBake) {
    page(system.painting);
    ink(color).box(rect);
    page(screen);
    needsBake = false;
    rect = null;
  }

  if (pen.drawing) {
    paste(system.painting);
    rect = Box.copy(pen.dragBox).abs.crop(0, 0, screen.width, screen.height);
    ink(color).box(rect);
  } else {
    paste(system.painting);
  }

}

export function act({ event: e }) {
  if (e.is("lift")) {
    needsBake = true;
  }
}

export const system = "nopaint";