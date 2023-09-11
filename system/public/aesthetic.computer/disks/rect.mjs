// 🟦 Rect, 23.04.21.12.16
//       22.09.19.21.07 (original)

/* region docs 📚
  Draw colored rectangles.
  // rect color
  Use `rect:type:center color`
            ^ use `o`, `i` or `f` for `outline`, `inline` or `fill`
              `outline` and `inline` both take an integer for thickness.

  Ex. `rect:o:c 255 0 0` for an outlined 1px rectangle.
  Ex. `rect:i-2` for a randomly colored 2px inline rectangle.
              ^ add a number here for thickness! 
#endregion */

/* #region ✅ TODO 
  - [] Support rotated and zoomed paintings!
  + Done
  - [x] Support ranged parameters
#endregion */

let rect,
  color,
  mode = "fill",
  thickness,
  centered = false;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ params, num, colon }) {
  color = num.parseColor(params);

  // Handle parameters for outline, inline, and fill.
  if (colon[0]?.startsWith("outline") || colon[0]?.startsWith("o")) {
    mode = "outline";
  }

  if (colon[0]?.startsWith("inline") || colon[0]?.startsWith("i")) {
    mode = "inline";
  }

  if (colon[0] === "fill" || colon[0] === "f") mode = "fill"; // Fill (default)

  // Set optional thickness.
  if (mode === "outline" || mode === "inline") {
    thickness = parseInt(colon[0].split("-")[1]);
    mode += ":" + thickness;
  }

  // Draw from center or corner (default).
  if (colon[1] === "center" || colon[1] === "c") {
    mode += "*center";
    centered = true;
  }
}

// 🎨 Paint (Executes every display frame)
function paint({ pen, screen, ink, page, system: { nopaint }, geo }) {
  if (nopaint.is("painting") && pen?.dragBox) {
    const db = !centered ? pen.dragBox : pen.dragBox.scale(2);

    //ink(color).box(db, mode); // UI: Paint a preview to the screen.

    const r = !centered
      ? nopaint.brush.dragBox
      : nopaint.brush.dragBox.scale(2);

    // page(nopaint.buffer);
    ink(color).box(db, mode); // UI: Paint a preview to the screen.
    // page(screen);

    rect = () => {
      ink(color).box(r, mode);
      rect = null;
    }; // Painting: Write to the canvas permanently.

  }
}

// 🍪 Prints to the current painting.
function bake() {
  rect?.();
}

const system = "nopaint";

export { boot, paint, bake, system };
