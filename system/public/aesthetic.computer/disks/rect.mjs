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

  if (needsBake) {
    needsBake = false;
    page(sys.painting).ink(color).box(rect).page(screen);
    sys.nopaint.present(api);
  }

  if (pen?.drawing && pen.dragBox) {
    sys.nopaint.present(api);

    ink(color).box(
      Box.copy(pen.dragBox).abs.crop(0, 0, screen.width, screen.height)
    ); // Render an overlay box.

    // Remember the brush box.
    rect = Box.copy(sys.nopaint.brush.dragBox).abs.crop(
      0,
      0,
      screen.width,
      screen.height
    );
  }
}

export function act({ event: e, system, api }) {
  system.nopaint.act(api); // Inherit nopaint's act functionality.
  if (e.is("lift")) needsBake = true;
}

export const system = "nopaint:dont-paint-on-leave";
