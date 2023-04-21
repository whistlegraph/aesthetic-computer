// Oval, 23.02.13.01.24
// Make an oval (filled).

/* #region 📓 TODO 
  - [-] Outlined ovals with line thickness support!
        (Similar to `rect`)
  - [-] Circle locking / circle to a locked oval!
  + Done
  - [x] Bring up to par with `rect`.
  - [x] Generalize ranged parameters.
  - [x] Add pan support.
  - [x] Filled ovals of different shapes and sizes.
#endregion */

const filled = true; // Whether to draw an outline or not.
let oval;

// 🎨 Paint (Executes every display frame)
function paint({ params, num, pen, system: { nopaint }, ink }) {
  if (nopaint.is("painting") && pen?.dragBox) {
    const color = num.rangedInts(params);

    const radX = pen.dragBox.w;
    const radY = pen.dragBox.h;

    ink(color).oval(pen.dragBox.x, pen.dragBox.y, radX, radY, filled);

    oval = () => {
      const { x, y } = nopaint.brush.dragBox;
      ink(color).oval(x, y, radX, radY, filled);
    };
  }
}

// 🍪 Prints to the current painting.
function bake() {
  oval?.();
  oval = null;
}

const system = "nopaint";

export { paint, bake, system };
