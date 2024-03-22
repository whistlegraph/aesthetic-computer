// Perf, 2022.01.27.02.16
// For testing and measuring graphics performance.

// TODO: Keep optimizing.
// TODO: Add dirtyRectangle that matches preview box, then return it.

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize }) {
  //resize(1024, 1024);
  resize(2048, 2048);
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ wipe, ink, pen }) {
  wipe(0);
  if (pen.x && pen.y) ink(255, 64).box(pen.x, pen.y, 32);
  //return false;
}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, paint };
