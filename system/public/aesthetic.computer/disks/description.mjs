// About, 23.05.13.16.17
// Type `about` followed by a command to read all about its use!

/* #region 🏁 todo
  - [] Add a similar "read" or "line" command which takes the user to
       the source code of the piece.
  + Done
  - [x] Parse line and all its parameters.
#endregion */

let piece, parsed, about;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ net, params }) {
  try {
    parsed = net.parse(params.join(" "));
    piece = await import(`${net.pieces}/${parsed.piece}.mjs`);
    about = piece.about;
  } catch (err) {
    console.error(err);
  }
}

// 🎨 Paint (Executes every display frame)
function paint({ ink, num }) {
  if (piece) {
    ink(255)
      .wipe(127)
      .write(about?.({ ...parsed, num }) || "Not Found", { x: 7, y: 24 });
    return false;
  }
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
