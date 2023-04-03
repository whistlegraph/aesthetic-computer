// Rect, 22.09.19.21.07
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

/* #region ✅ TODO 
  + Done
  - [x] Add support for panned navigation.
  - [x] Add `n` and `p` support.
#endregion */

let needsBake = false;
let rect;

// 🎨
export function paint({
  api,
  params,
  pen,
  ink,
  system: sys,
  screen,
  page,
  geo: { Box },
}) {
  const color = params.map((str) => parseInt(str));

  if (rect && needsBake) {
    const brush = sys.nopaint.brush;
    page(sys.painting)
      .ink(color)
      .box(Box.copy(brush.dragBox).abs.crop(0, 0, screen.width, screen.height))
      .page(screen);
    needsBake = false;
    rect = null;
    sys.nopaint.present(api); // Display the painting on the screen.
  }

  if (pen?.drawing && pen.dragBox) {
    sys.nopaint.present(api); // Display the painting on the screen.
    rect = Box.copy(pen.dragBox).abs.crop(0, 0, screen.width, screen.height);
    ink(color).box(rect);
  }
}

export function act({ event: e, system, api }) {
  system.nopaint.act(api); // Inherit nopaint's act functionality.
  if (e.is("lift")) needsBake = true;
}

export const system = "nopaint:dont-paint-on-leave";
