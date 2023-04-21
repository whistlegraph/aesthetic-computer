// Oval, 23.02.13.01.24
// Make an oval (filled).

/* #region 📓 TODO 
  - [-] Generalize ranged parameters.
  - [] Circle locking / circle to a locked oval!
  - [] Outlined ovals with line thickness support.
  + Done
  - [x] Add pan support.
  - [x] Filled ovals of different shapes and sizes.
#endregion */

const filled = true; // Whether to draw an outline or not.
let bake = false;
let oval;

// 🎨 Paint (Executes every display frame)
function paint({ api, params, num, pen, screen, system, ink, page }) {
  if (bake) {
    page(system.painting);
    oval?.();
    oval = null;
    page(screen);
    bake = false;
    system.nopaint.present(api);
  }

  if (pen?.drawing) {
    if (pen.dragBox) {
      system.nopaint.present(api); // Display the painting on the screen.

      // const radiusCicle = num.p2.dist(pen, pen.dragBox);
      const radX = pen.dragBox.w;
      const radY = pen.dragBox.h;
      const color = num.rangedInts(params);

      ink(color).oval(pen.dragBox.x, pen.dragBox.y, radX, radY, filled);

      oval = () => {
        const { x, y } = system.nopaint.brush.dragBox;
        ink(color).oval(x, y, radX, radY, filled);
      };
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
