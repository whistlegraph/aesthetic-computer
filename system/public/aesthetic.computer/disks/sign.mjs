// Sign, 23.01.30.02.19
// Stamp a timestamped signature onto the margin of a painting.

/* #region 📚 TODO 
  - [] Prototype stamp behavior for bottom left corner.
#endregion */

function paint({ ink, write, num, screen: { width, height } }) {
  ink(0).write(`JAS ${num.timestamp()}`, {x: 6, y: height - 9 - 6});
  ink(255).write(`JAS ${num.timestamp()}`, {x: 6 - 2, y: height - 9 - 6 - 2});
  return false;
}

export { paint };

export const system = "nopaint";

// 📚 Library (Useful functions used throughout the piece)
// ...
