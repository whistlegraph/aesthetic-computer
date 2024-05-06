// Graphics, 2024.5.05.18.57.43.452
// A test of various low level AC graphics capabilities.

/* #region 🏁 TODO 
#endregion */

export const nohud = true;

// 🥾 Boot
function boot({ api, resolution }) {
  // Runs once at the start.
  resolution(24);
}

const marg = 1;

// 🎨 Paint
function paint({ wipe, ink, line, write, screen }) {
  ink("red").line(0, 0, 2, 2);
  // wipe("blue");
  //  ink("yellow", 64); // Would draw a diagonal line.
  //  line(marg, marg, screen.width - marg * 2, screen.height - marg * 2);
  //  ink("lime", 128); // Would draw a diagonal line.
  //  line(screen.width - marg * 2, marg, marg, screen.height - marg * 2);
  // ink("red").line(0, 0, 2, 2);

   ink("red").line(5, 5, 8, 6);
  // ink("red").line(0, 0, 2, 2);
  // ink("red").line(0, 0, 2, 2);
  //ink("red").line(0, 0, 2, 2);
  // ink("red").line(0, 0, 5, 1);
  // ink("red").line(1, 5, 5, 5);

  ink("red", 255).write("Shy", { x: 1, y: 8, size: 1 });

  return false;
}

export { boot, paint };

// 📚 Library
//   (Useful functions used throughout the piece)
