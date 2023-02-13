// Oval, 23.02.13.01.24
// Make an oval

/* #region 📓 TODO 
  + Version 1
  - [-] Circle locking / alias circle to a locked oval!
  - [] Outlined ovals.
  + Done
  - [x] Filled ovals of different shapes and sizes.
#endregion */

// const filled = false;
const filled = true;
let bake = false;

// 🎨 Paint (Executes every display frame)
function paint({ params, pen, paste, screen, system, ink, page }) {
  if (bake) {
    page(system.painting).paste(screen).page(screen);
    bake = false;
  }

  if (pen?.drawing) {
    paste(system.painting); // TODO: How efficient is this?
    if (pen.dragBox) {
      // const radiusCicle = num.p2.dist(pen, pen.dragBox);
      const radiusX = pen.dragBox.w;
      const radiusY = pen.dragBox.h;
      ink(params.map((str) => parseInt(str))).oval(
        pen.dragBox.x,
        pen.dragBox.y,
        radiusX,
        radiusY,
        filled
      );

    }
  }
}

// ✒ Act
function act($) {
  $.system.nopaint.act($); // Inherit nopaint's act functionality.
  const { event: e } = $;
  if (e.is("lift:1")) bake = true;
}

export const system = "nopaint";

// 📚 Library (Useful functions used throughout the piece)
// ...

export { paint, act };
