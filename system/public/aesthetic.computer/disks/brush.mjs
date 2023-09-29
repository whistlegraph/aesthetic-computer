// Brush, 23.09.29.01.48
// The most basic brush.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [❤️‍🔥] How to adjust this template so that it is the "new normal" for all brushes?
  - [] Finish this template.
#endregion */

// 🥾 Boot
function boot() {}

// 🎨 Paint
function paint({ screen, system, ink, pen, page }) {
  if (pen?.drawing) {
    const brush = system.nopaint.brush;
    page(system.nopaint.buffer).wipe(255, 0);
    ink().line(brush.x, brush.y + 10, brush.x, brush.y - 10);
    page(screen);
  }
}

// 🥞 Bake (to the painting)
function bake({ system, paste }) {
  paste(system.nopaint.buffer);
}

// 📰 Meta
function meta() {
  return {
    title: "Brush",
    desc: "An extremely simple brush.",
  };
}

export const system = "nopaint";
export { boot, paint, bake, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
