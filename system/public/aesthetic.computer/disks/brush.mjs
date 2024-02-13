// Brush, 23.09.29.01.48
// The most basic brush.

/* #region 📚 README 
  1.) Tap 🪟 Extensions icon on the left sidebar. Search for the
      `Aesthetic Computer` extension and install it.
  2.) Tap the new 🚪 icon in the sidebar to open the `prompt`.
  3.) Edit the code below and save this file to run the piece.
  4.) Enter `publish` in the `prompt` to put your work online.
  ❓  Enter `help` in `prompt` and ping `@helper` for personal support.
#endregion */

// 🖌️ Brush
function brush({ pen, ink }) {
  ink("yellow").circle(pen.x, pen.y, 16, true);
}

export { brush };