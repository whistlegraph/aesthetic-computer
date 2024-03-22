// Lang, 2022.6.2.1.55
// Experimental JavaScript syntax for DSL building and learning.

class Lang {
  constructor() {
    console.log("Lang initialized!");
  }

  get o() {
    console.log("LINE");
    return this;
  }

  get x() {
    console.log("DOT");
    return this;
  }
}

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize }) {
  const l = new Lang();

  l.o.x.o.o.o;
  l.o.o.x.x.x;

  // TODO: Runs only once!
  // resize(50, 20);
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // TODO: Move a ball here!
  //console.log($api);
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe }) {
  wipe(128); // Draw a gray background
  return false; // Only once.
}

// ✒ Act (Runs once per user interaction)
function act({ event }) {}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat };
