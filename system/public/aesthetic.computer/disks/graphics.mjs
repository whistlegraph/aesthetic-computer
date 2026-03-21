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
function paint({ wipe, ink, ink2, line, write, screen, stamp, painting }) {
  wipe("blue");
  // Draw an X across the screen.
  ink("red")
    .ink2("yellow")
    .line(0, 0, screen.width - 1, screen.height - 1);
  ink("lime")
    .ink2("pink")
    .line(screen.width - 1, 0, 0, screen.height - 1);
  ink("pink", 128).ink2(null).write("Shy", { x: 3, y: 8, size: 1 }); // Write a word.

  // TODO: Draw a bitmap here maybe something from a url?
  stamp(
    painting(6, 6, ({ noise16 }) => noise16()),
    screen.width / 2,
    screen.height - 4,
  );

  // stamp("https://assets.aesthetic.computer/images/favicon.png", 0, 0);
  return false;
}

export { boot, paint };

// 📚 Library
//   (Useful functions used throughout the piece)
