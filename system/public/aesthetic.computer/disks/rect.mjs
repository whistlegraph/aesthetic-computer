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
  - [] Support rotated paintings.
  + Done
  - [x] Support zoomed paintings.
  - [x] Support ranged parameters
#endregion */

let rect,
  color,
  mode = "fill",
  thickness,
  rainbow = false,
  centered = false,
  erase = false;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ params, num, colon }) {
  color = num.parseColor(params);

  // 🌈 Rainbow
  if (color[0] === "rainbow") {
    rainbow = true;
    color = [...num.rainbow(), color[1]];
  }
  // 🧱 Erase
  if (color[0] === -1) {
    erase = true; // Detect erase color.
    color = [32, color[3]]; // Don't use the -1 erase color here.
  }

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
function paint({
  pen,
  paste,
  num,
  screen,
  ink,
  page,
  system: { nopaint },
  blend,
}) {
  if (nopaint.is("painting") && pen?.dragBox) {
    const r = !centered
      ? nopaint.brush.dragBox
      : nopaint.brush.dragBox.scale(2);

    page(nopaint.buffer).wipe(255, 0);
    if (!erase) blend("blit");
    ink(color).box(r, mode); // UI: Paint a preview to the screen.
    if (!erase) blend();
    page(screen);

    rect = () => {
      if (erase) blend("erase");
      paste(nopaint.buffer);
      if (erase) blend();
      page(nopaint.buffer).wipe(255, 0);
      rect = null;
      if (rainbow) color = [...num.rainbow(), color[3]];
    }; // Painting: Write to the canvas permanently.
  }
}

// 🍪 Prints to the current painting.
function bake() {
  rect?.();
}

const system = "nopaint";
export { boot, paint, bake, system };
