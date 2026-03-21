// A game for regulating body movement among players. 

let stop = true;

// 🎨 Paint (Executes every display frame)
function paint({ wipe }) {
  if (stop) {
    wipe(255, 0, 0); // Draw a red background
  } else {
    wipe(0, 255, 0); // Draw a green background
  }
  return false; // Don't draw automatically until the next `needsPaint()` 
}

// ✒ Act (Runs once per user interaction)
// function act({ event }) {}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat({ sound: { bpm, synth }, needsPaint }) {
  bpm(70);
  stop = !stop;
  synth({
    tone: stop ? 400 : 800,
    beats: 2 / 10,
    decay: 0.99
  });
  needsPaint();
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
// function sim($api) {}

// 🥾 Boot (Runs once before first paint and sim)
// function boot($api) {}

export { paint, beat };