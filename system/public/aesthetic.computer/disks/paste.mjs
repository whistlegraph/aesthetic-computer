// Paste, 23.05.07.13.38
// Load an image from an external source.

/* #region 📓 TODO 
  - [😫] Handle file cancel state. (It currently hangs)
  - [-] Add exact width and height scaler to `paste`.
  - [] Optimize / speed up `paste` function by alot?
  + Done
  - [x] Get this updating for larger files without frame skips / interaction weirdness.
  - [x] Just load a local file for now.
#endregion */

let needsPaste = false;

// 🥾 Boot (Runs once before first paint and sim)
function boot({store, system}) {
  if (store["file:opened"]) {
    system.nopaint.needsBake = true;
    needsPaste = true;
  }
}

function bake({ paste, store }) {
  console.log("baking!");
  if (needsPaste) {
    paste(store["file:opened"], 0, 0, 1 / 6);
    needsPaste = false;
  }
}

// ✒ Act (Runs once per user interaction)
async function act({ event: e, file, store, system }) {
  if (e.is("touch") && !store["file:opened"]) {
    try {
      store["file:opened"] = await file();
      needsPaste = true;
      system.nopaint.needsBake = true;
    } catch (err) {
      console.error(err);
    }
  }
}

export const system = "nopaint";

export { boot, bake, act };

// 📚 Library (Useful functions used throughout the piece)
// ...

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
