// Marker, 2024.4.17.00.53.23.779
// A brush interpolation test.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add auto-interpolation to this brush here.
#endregion */

function brush({ pen, ink }) {
  ink("blue").circle(pen.x, pen.y, brushSize, true);
}

export { brush };

// 📚 Library
//   (Useful functions used throughout the piece)