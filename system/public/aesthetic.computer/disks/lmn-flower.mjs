// LMN-flower, 2023.7.17.17.51.18
// Pull a petal.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ resolution }) {
  // Stuff here happens once.
  resolution(256, 256);
}

// 🎨 Paint
function paint({ wipe, ink, line, box, oval, screen, write }) {
  wipe("blue"); // Sky

  // Grass
  const horizonHeight = 3 * screen.height / 4;
  ink("green");
  box(0, horizonHeight, screen.width, screen.height - horizonHeight)

  // Stem
  ink("purple");
  line(screen.width / 2, horizonHeight - 1, screen.width / 2, screen.height / 2);

  // Flower
  ink("yellow");
  // function oval(x0, y0, radiusX, radiusY, filled = false, thickness = 1)
  oval(screen.width / 2, screen.height / 2, 20, 10, true )
  // Executes every display frame.


  // Text
  ink("pink");
  write("pick a petal", { center: "x", y: horizonHeight * 1.15 })
}

// 🎪 Act
function act({ event, jump }) {
  if (event.is("touch")) {
    jump("lmn-petal");
  }
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "flower",
    desc: "Pull a petal.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
