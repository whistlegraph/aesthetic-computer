// $NAME, $TIMESTAMP
// $THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES

/* #region 📚 README 
  - Welcome to your first aesthetic.computer piece.
  - You can `console.log(api);` to explore,
    and then destructure additional api commands
    to try them out!
  - Top-level commands make up each piece's application flow.
    Uncomment and export them to invoke their behavior and use their apis.
  - Enter `help` for the AC #help and ping @helper for additional explanation.
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
  wipe("orange");
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
