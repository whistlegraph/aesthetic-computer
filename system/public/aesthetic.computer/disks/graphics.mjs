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

// 🎨 Paint
function paint({ wipe, ink, ink2, line, write, screen }) {
  wipe("blue");
  // Draw an X across the screen.
  ink("red").ink2("yellow").line(0, 0, screen.width, screen.height);

  // 🧯 TODO: This one is weirdly off by 1 pixel on the top right...
  ink("lime").ink2("pink").line(screen.width, 0, 0, screen.height);

  // ink("pink", 128).write("Shy", { x: 3, y: 8, size: 1 });
  // ink("red").point(screen.width / 2, 4);
  // ink("lime").pan(2, 2).point(screen.width / 2, 4).unpan();
  return false;
}

export { boot, paint };

// 📚 Library
//   (Useful functions used throughout the piece)
