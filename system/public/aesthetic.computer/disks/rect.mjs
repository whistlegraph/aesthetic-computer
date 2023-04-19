// Rect, 22.09.19.21.07
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

/* #region ✅ TODO 
 - [🟢] Write the ideal "rect" brush code.
  - [] Abstract "needsBake" into nopaint. 
 + Done
 - [x] I need an abstraction to know whether we are making a brush
       stroke or not, in order to manage panning and drawing logic
       across platforms.
#endregion */

let needsBake = false;
let rect;

// 🎨
export function paint({ api, params, pen, ink, system, screen, page, geo }) {
  const color = params.map((str) => parseInt(str));

  if (needsBake && rect) {
    needsBake = false;
    page(system.painting).ink(color).box(rect).page(screen);
    rect = null;
    system.nopaint.present(api);
  }

  if (system.nopaint.is("painting") && pen?.dragBox) {
    system.nopaint.present(api);

    ink(color).box(
      geo.Box.copy(pen.dragBox).abs.crop(0, 0, screen.width, screen.height)
    ); // Render an overlay box.

    // Remember the brush box.
    rect = geo.Box.copy(system.nopaint.brush.dragBox).abs.crop(
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