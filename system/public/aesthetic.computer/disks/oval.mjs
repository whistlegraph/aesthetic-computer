// Oval, 23.02.13.01.24
// Make an oval (filled).

/* #region 📓 TODO 
  - [-] Circle locking / circle to a locked oval!
  - [] Outlined ovals.
  - [] Generalize ranged parameters.
  + Done
  - [x] Add pan support.
  - [x] Filled ovals of different shapes and sizes.
#endregion */

const filled = true; // Whether to draw an outline or not.
let bake = false;
let oval;

// 🎨 Paint (Executes every display frame)
function paint({ api, params, pen, screen, system: sys, ink, page }) {
  if (bake) {
    page(sys.painting);
    oval?.();
    oval = null;
    page(screen);
    bake = false;
    sys.nopaint.present(api);
  }

  if (pen?.drawing) {
    if (pen.dragBox) {
      sys.nopaint.present(api); // Display the painting on the screen.

      // const radiusCicle = num.p2.dist(pen, pen.dragBox);
      const radX = pen.dragBox.w;
      const radY = pen.dragBox.h;
      const color = rangedParams(params);

      ink(color).oval(pen.dragBox.x, pen.dragBox.y, radX, radY, filled);

      oval = () => {
        const { x, y } = sys.nopaint.brush.dragBox;
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

function rangedParams(params) {
  // Parse color params with dashed ranges such as 200-255.
  return params.map((str) => {
    if (str.match(/^\d+-\d+$/)) {
      const range = str.split("-");
      return num.randIntRange(parseInt(range[0]), parseInt(range[1]));
    } else {
      return parseInt(str);
    }
  });
}

export const system = "nopaint";

// 📚 Library (Useful functions used throughout the piece)
// ...

export { paint, act };
