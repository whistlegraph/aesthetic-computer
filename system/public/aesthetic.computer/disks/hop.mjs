// Hop, 2024.3.16.03.45.36.296
// A first-person shooter.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
// function boot({ api }) {
// Runs once at the start.
// }

// 🎨 Paint
function paint({ wipe, ink }) {
  wipe("gray").ink(0).line(); // Would draw a diagonal line.
}

// 🎪 Act
// function act({ event: e }) {
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
    title: "Hop",
    desc: "A first-person shooter.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
