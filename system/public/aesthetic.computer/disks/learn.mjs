// Learn, 23.05.13.16.17
// Type `learn` followed by a command to read all about its use!

/* #region 🤝 Read Me 
#endregion */

let piece, learn;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ net, params }) {
  try {
    piece = await import(`${net.pieces}/${params[0]}.mjs`);
    learn = piece.learn;
  } catch (err) {
    console.error(err);
  }
}

// 🎨 Paint (Executes every display frame)
function paint({ ink, params }) {
  ink(255)
    .wipe(127)
    .write(learn?.() || "Not Found", { x: 8, y: 24 });
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

export { boot, paint };

// 📚 Library (Useful functions used throughout the piece)
// ...
