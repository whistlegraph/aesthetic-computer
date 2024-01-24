// 404, 2023.6.24.16.57.08
// Appears when a piece is not found.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ wipe, screen, params }) {
  // Runs once at the start.
  // TODO: Does this sometimes not show up?
  wipe(96)
    .ink(128)
    .write(`Not Found ${params.join("~")}`, { center: "xy" });
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
