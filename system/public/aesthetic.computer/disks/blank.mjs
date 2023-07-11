// $NAME, $TIMESTAMP
// $THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ wipe, ink, line }) {
  // Runs once at the start.
  wipe(0);
  ink();
  line();
  ink();
  line();
  ink();
  line();
  ink();
  line();
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
    title: "$NAME",
    desc: "$THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES",
  };
}

export { boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
