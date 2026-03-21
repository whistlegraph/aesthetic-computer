// Beat, 2024.8.28.19.28.06.877
// A rhythmic percussion instrument.

/* 📝 Notes
 */

function paint({ wipe, ink, line, screen }) {
  wipe("brown");
}

function act({ event: e, sound: { synth } }) {
  // Respond to user input here.
  if (e.device === "mouse" && e.is("touch")) {
    switch (e.button) {
      case 0: // Left
        console.log("Left");
        synth();
        break;
      case 1: // Middle
        console.log("Middle");
        synth();
        break;
      case 2: // Right
        console.log("Right");
        synth();
        break;
    }
  }
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per metronomic BPM.
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
