// Keys, 2024.4.24.01.32.25.401
// Use keys to trigger actions like musical notes.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Write up ABCDEFG and the sharps.
  - [] Test n-key rollover / rollover capability somehow 
       visually.
  - [] Make the sounds nice / add a sustain.
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
    title: "Keys",
    desc: "Use keys to trigger actions like musical notes.",
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
