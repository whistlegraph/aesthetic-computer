// Fly, 2022.01.11.22.52
// Bounce around in 3D!

// TODO: Make a bit of geometry along with a mouse + keyboard controlled camera.

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize }) {
  // TODO: Runs only once!
  // resize(50, 20);
}

// 🧮 Simulate (Runs once per logic frame (120fps)).
function sim($api) {
  // TODO: Move a ball here!
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ wipe, num: { randInt: r }, screen }) {
  wipe(0);
  // wipe(r(255), r(255), r(255)).ink(0).line(0, 0, screen.width, screen.height);
}

// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // console.log(event);
}

// 💗 Beat (Runs once per bpm)
function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library (Useful functions used throughout the program)
// ...

export { boot, sim, paint, act, beat };
