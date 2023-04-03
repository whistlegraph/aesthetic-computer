// Rect, 22.09.19.21.07
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

/* #region ✅ TODO 
  - [] Add support for panned navigation.
  + Done
  - [x] Add `n` and `p` support.
#endregion */

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
  const color = params.map((str) => parseInt(str));

  if (rect && needsBake) {
    page(system.painting);
    ink(color).box(rect);
    page(screen);
    needsBake = false;
    rect = null;
  }

  if (pen?.drawing && pen.dragBox) {
    paste(system.painting);
    rect = Box.copy(pen.dragBox).abs.crop(0, 0, screen.width, screen.height);
    ink(color).box(rect);
  } else {
    paste(system.painting);
  }


}

export function act({ event: e, system, api }) {
  system.nopaint.act(api); // Inherit nopaint's act functionality.
  if (e.is("lift")) needsBake = true;
}

export const system = "nopaint:dont-paint-on-leave";
