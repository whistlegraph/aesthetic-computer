// Brush, 23.09.29.01.48
// The most basic brush.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [❤️‍🔥] How to adjust this template so that it is the "new normal" for all brushes?
  - [] Finish this template.
#endregion */

// 🥾 Boot
// function boot() {}

// 🖌️ Brush
function brush({ pen, ink }) {
  if (pen) {
    ink().line(pen.x, pen.y + 10, pen.x, pen.y - 10);
  }
}

export const system = "nopaint";
export { brush };

// 📚 Library
//   (Useful functions used throughout the piece)
