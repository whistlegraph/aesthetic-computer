// Screentest, 22.09.29.10.11
// A. This piece is a test to make sure `resize`, `gap`, and `density` all work
//    synchronously.
// B. And also for testing bitmap scaling!

let oldScr, scale;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resolution, wipe, screen, ink, flatten, clonePixels }) {
  // WIP 🎇
  resolution(32);
  wipe(255, 255, 0);
  ink(0, 255, 0).line(screen.width - 1, 0, 0, screen.height - 1);
  ink(100, 100, 100).box(8, 12, 4, 4);
  ink(0, 0, 255).line(0, 0, screen.width - 1, screen.height - 1);
  flatten();
  oldScr = clonePixels(screen);
  resolution(128);
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  scale += 0.001;
  scale = scale % 4;
}


// 🎨 Paint (Executes every display frame)
function paint({ wipe, gap, paintCount, screen, paste, grid }) {
  wipe(0);

  let x = screen.width / 2;
  let y = screen.height / 2;
  x -= (oldScr.width * scale / 2);
  y -= (oldScr.height * scale / 2);
  paste(oldScr, x, y, scale);
  //return false; // Only once.
}

// ✒ Act (Runs once per user interaction)
function act({ event }) {}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat };
