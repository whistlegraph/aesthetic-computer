// $NAME, $TIMESTAMP
// $THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES

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
    title: "$NAME",
    desc: "$THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES",
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
