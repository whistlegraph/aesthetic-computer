// Silly Snake, 2023.6.29.00.15.15
// A snake game where you eat colors and grow accordingly.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] 🐍 Make a reversible snake clone with colored lines. 
#endregion */

// 🥾 Boot
function boot({ wipe }) {
  // Runs once at the start.
  wipe(127);
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
    title: "Silly Snake",
    desc: "A snake game where you eat colors and grow accordingly.",
  };
}

export { boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
