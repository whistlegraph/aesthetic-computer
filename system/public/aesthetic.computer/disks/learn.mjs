// Learn, 23.05.13.16.17 
// Type `learn` followed by a command to read all about its use! 

/* #region 🤝 Read Me 
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
// function boot({wipe, ink, screen }) {
//   wipe(0);
// }

// 🎨 Paint (Executes every display frame)
function paint({ink, screen}) {
  ink(255).box(screen.width / 2, screen.height / 2, 64, "*center");
}

/*
// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/

export { paint }

// 📚 Library (Useful functions used throughout the piece)
// ...