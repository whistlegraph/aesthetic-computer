// List, 2024.1.30.13.18.29.955
// A directory of all system pieces and prompt commands.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Draw a purple line.
#endregion */

// 🥾 Boot
// function boot({ api }) {
// Runs once at the start.
// }

// 🎨 Paint
function paint({ wipe, ink }) {
  wipe("red");
  ink(0).line(); // Would draw a diagonal line.
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
    title: "List",
    desc: "A directory of all system pieces and prompt commands.",
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
