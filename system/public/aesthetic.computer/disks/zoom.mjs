// Zoom, 23.04.29.22.51
// 🥾 Boot (Runs once before first paint and sim)
// function boot({wipe, ink, screen }) {
//   wipe(0);
// }

// 🎨 Paint (Executes every display frame)
function paint({ wipe, paste, screen, system, paintCount }) {
  wipe(0, 0, 255);
  const osc = Math.sin(paintCount / 20);
  const scale = 1.25 + osc;
  const angle = (paintCount / 2);

  // Center the picture.
  const x = screen.width / 2 - (system.painting.width * scale) / 2;
  const y = screen.height / 2 - (system.painting.height * scale) / 2;

  // TODO: Rotate?
  paste(system.painting, x, y, { scale, angle });
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

export { paint };

// 📚 Library (Useful functions used throughout the piece)
// ...
