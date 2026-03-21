// 3x3, 2025.6.16.21.21.44.565
// A 3x3 ortholinear pad instrument.

/* 🍪 Make 
  - [🟡] WebUSB support for the Koolertron 3x3 keypad.
  - [] A 3x3 responsive grid using paint.
  - [] A qwerty keyboard / number mapping to match.
*/

function boot() {
}

function paint({ ink, wipe }) {
  wipe("blue");
}

function act({ event: e }) {
  if (e.is("keyboard:down")) {
    console.log(e);
  }
}

// 📚 Library

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
