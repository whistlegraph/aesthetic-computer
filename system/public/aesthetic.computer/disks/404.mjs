// 404, 2023.6.24.16.57.08
// Appears when a piece is not found.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ wipe, screen }) {
  // Runs once at the start.
  wipe("teal")
    .ink("aqua")
    .write(`Piece Not Found`, { center: "x", y: screen.height / 2 - 8})
    .ink("pink")
    .write(`Drag one in?`, { center: "x", y: screen.height / 2 + 8})
}

// 🎨 Paint
function paint({ ink }) {
  // Executes every display frame.
  return false;
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
// function meta() {
//   return {
//     title: "404",
//     desc: "Appears when a piece is not found.",
//   };
// }

export { boot, paint };

// 📚 Library
//   (Useful functions used throughout the piece)
