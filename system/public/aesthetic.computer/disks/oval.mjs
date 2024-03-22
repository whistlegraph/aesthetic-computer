// Oval, 23.02.13.01.24
// Make an oval (filled).

/* region docs 📚
  Draw colored ovals (and circles).
  Use `oval:type:circle color`
            ^ use `o`, or `f` for `outline`, `fill`
              `outline` takes an integer for thickness.

  Ex. `oval:o:c 255 0 0` for an red outlined 1px perfect circle.
              ^ `c` or `circle`

  Ex. `oval:o-2` for a randomly colored 2px inline rectangle.
              ^ add a number here for thickness! 
#endregion */

/* #region 📓 TODO 
  - [] Support rotated and zoomed paintings!
  + Done
  - [x] Outlined ovals with line thickness support!
        (Similar to `rect`)
  - [x] Circle locking / circle to a locked oval!
  - [x] Bring up to par with `rect`.
  - [x] Generalize ranged parameters.
  - [x] Add pan support.
  - [x] Filled ovals of different shapes and sizes.
#endregion */

let oval,
  color,
  mode = "fill",
  thickness = 1,
  circle = false;

function boot({ params, num, colon }) {
  color = num.parseColor(params);

  // Handle parameters for outline, inline, and fill.
  if (colon[0]?.startsWith("outline") || colon[0]?.startsWith("o")) {
    mode = "outline";
  }

  if (colon[0] === "fill" || colon[0] === "f") mode = "fill"; // Fill (default)

  // Set optional thickness.
  if (mode === "outline") thickness = parseInt(colon[0].split("-")[1]);

  // Draw a perfect circle or oval (default).
  if (colon[1] === "circle" || colon[1] === "c") circle = true;
}

// 🎨 Paint (Executes every display frame)
function paint({ params, num: { p2 }, pen, system: { nopaint }, ink }) {
  if (nopaint.is("painting") && pen?.dragBox) {
    let radX, radY;

    if (circle) {
      radX = radY = p2.dist(pen.dragBox, pen);
    } else {
      radX = pen.dragBox.w * 1.35;
      radY = pen.dragBox.h * 1.35;
    }

    const filled = mode === "fill";

    ink(color).oval(
      pen.dragBox.x,
      pen.dragBox.y,
      radX,
      radY,
      filled,
      thickness
    ); // UI: Paint a preview to the screen.

    const { x, y } = nopaint.brush.dragBox;

    oval = () => {
      ink(color).oval(x, y, radX, radY, filled, thickness);
      oval = null;
    }; // Painting: Write to the canvas permanently.
  }
}

// 🍪 Prints to the current painting.
function bake() {
  oval?.();
}

const system = "nopaint";

export { boot, paint, bake, system };
