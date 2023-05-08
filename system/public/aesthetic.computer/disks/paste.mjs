// Paste, 23.05.07.13.38
// Load an image from an external source.

/* #region 📓 TODO 
  - [-] Add exact width and height scaler to `paste`.
  - [] Optimize / speed up `paste` function by alot?
  - [x] Just load a local file for now.
#endregion */

let yetToPaste = true,
  needsPaste = false;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ store }) {}

// 🎨 Paint (Executes every display frame)
function paint({ needsPaint, page, screen, paste, store, system }) {
  if (needsPaste) {
    page(system.painting);
    paste(store["file:opened"], 0, 0, 1 / 6);
    page(screen);
    needsPaste = false;
    needsPaint(); // TODO: Why is this necessary? 23.05.08.00.03
  }
}

function sim({ store }) {
  if (yetToPaste && store["file:opened"]) {
    console.log("simming", store["file:opened"]);
    yetToPaste = false;
    needsPaste = true;
  }
}

// ✒ Act (Runs once per user interaction)
async function act({ event: e, file, store }) {
  if (e.is("touch") && !store["file:opened"]) {
    try {
      store["file:opened"] = await file();
    } catch (err) {
      console.error(err);
    }
  }
}

export const system = "nopaint";

export { boot, sim, act, paint };

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
