// Graphics, 2024.5.05.18.57.43.452
// A test of various low level AC graphics capabilities.

/* #region 🏁 TODO 
#endregion */

export const nohud = true;

// 🥾 Boot
function boot({ api, resolution }) {
  // Runs once at the start.
  resolution(16);
}

const marg = 1;

// 🎨 Paint
function paint({ wipe, ink, line, write, screen }) {
  wipe("blue");
  // ink("yellow", 64); // Would draw a diagonal line.
  // line(marg, marg, screen.width - marg, screen.height - marg);
  // ink("lime", 128); // Would draw a diagonal line.
  // line(screen.width - marg, marg, marg, screen.height - marg);
  ink("red", 255).write("S", { x: 1, y: 1 });
  return false;
}

export { boot, paint };

// 📚 Library
//   (Useful functions used throughout the piece)
