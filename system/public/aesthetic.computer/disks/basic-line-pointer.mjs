// Basic Line Pointer, 2022.11.28.03.04

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize }) {
  // TODO: Runs only once!
  // resize(50, 20);
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // TODO: Move a ball here!
  //console.log($api);
}

let x = 0,
  y = 0;

// 🎨 Paint (Executes every display frame)
function paint({ wipe }) {
  wipe(100, 0, 0).ink(0, 255, 0).line(x, y, 100, 100); // x1, y1, x2, y2
}

// ✒ Act (Runs once per user interaction)
function act({ event }) {
  if (event.name === "move") {
    x = event.x;
    y = event.y;
  }
}

// 💗 Beat (Runs once per bpm)
function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat };
