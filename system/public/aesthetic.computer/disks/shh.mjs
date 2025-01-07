// Shh, 2025.1.07.21.50.24.103
// Play various noise drones.

/* 📝 Notes
 */

let t1;
let level = 0;

function boot({ sound: { synth } }) {
  t1 = synth({
    type: "noise-white",
    tone: 0,
    volume: 0,
    duration: "🔁",
  });
}

function paint({ api, wipe, ink, line, screen, box, circle, pen, write }) {
  wipe("gray");
  const y = (1 - level) * (screen.height - 1);
  ink("red").line(0, y, screen.width, y);
}

// 📚 Library

function act({ event: e, screen }) {
  if (e.is("touch")) {
    level = 1 - (e.y / screen.height);
    t1?.update({ volume: level, duration: 0.005 });
  }

  if (e.is("draw")) {
    level -= e.delta.y / screen.height;
    if (level < 0) level = 0;
    if (level > 1) level = 1;
    t1?.update({ volume: level, duration: 0.005 });
  }
}

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per system metronome (BPM) tick.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
