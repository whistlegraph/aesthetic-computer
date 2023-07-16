// Crop, 2023.7.12.18.49.51
// Crop or extend your painting by drawing a rectangle.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟢] Draw an overlay rectangle to show the darkened crop on the painting, where
         returning to the prompt crops it.
#endregion */

// 🥾 Boot
function boot({ wipe, ink, line }) {
  // Runs once at the start.
}

// 🎨 Paint
function paint({ ink }) {
  // Executes every display frame.
  return false; // Uncomment for an animation loop.
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
    title: "Crop",
    desc: "Crop or extend your painting by drawing a rectangle.",
  };
}

export { boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
