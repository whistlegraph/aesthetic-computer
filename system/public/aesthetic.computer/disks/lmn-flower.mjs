// LMN-flower, 2023.7.17.17.51.18
// Pull a petal.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ resolution }) {
  // Stuff here happens once.
}

// 🎨 Paint
function paint({ wipe, ink, line, rect, oval, screen }) {

  wipe("blue");

  // Horizon
  ink("green"); // Set our drawing color to green.
  // const horizon = { height: 3*screen.height/4 }; // for `horizon.height`
  const horizonHeight = 3 * screen.height / 4;
  line(0, horizonHeight, screen.width, horizonHeight); // (x1, y1), (x2, y2)

  // Stem
  ink("purple");
  line(screen.width / 2, horizonHeight, screen.width / 2, screen.height / 2);

  // Flower
  ink("yellow");
  // function oval(x0, y0, radiusX, radiusY, filled = false, thickness = 1)
  oval(screen.width / 2, screen.height / 2, 20, 10, true )
  // Executes every display frame.
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
