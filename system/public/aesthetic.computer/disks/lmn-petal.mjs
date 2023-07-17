// LMN-petal, 2023.7.17.18.30.41
// Touch me.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let message;

// 🥾 Boot
function boot({ help }) {
  message = help.choose("He loves me", "He loves me not");
}

// 🎨 Paint
function paint({ ink, wipe, oval, write, screen, help }) {
  wipe("blue");
  ink("white")
  // function oval(x0, y0, radiusX, radiusY, filled = false, thickness = 1)
  oval(screen.width / 2, screen.height / 2, 80, 17, true);
  ink("pink");
  write(message, { center: "xy" })
}

// 🎪 Act
// function act({ event }) {
//  // Respond to user input here.
// }

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
    title: "LMN-petal",
    desc: "Touch me.",
  };
}

export { boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
