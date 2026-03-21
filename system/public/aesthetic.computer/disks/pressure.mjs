// Pressure, 2025.3.22.05.21.57.973
// A pen HID pressure test.

/* 📝 Notes
 */

// function paint({ api, wipe, ink, line, screen, box, circle, pen, write }) {
// console.log(api); // Log the API or enter `docs` (WIP) in `prompt`.
//  wipe("white");
//}

const { floor } = Math;

// 🖌️ Brush
function brush({ pen, ink }) {
  // ink().circle(pen.x, pen.y, 16, true);
  if (pen) {
    ink("red", 1 + floor(pen.pressure * 255)).circle(pen.x, pen.y, 6);
    // Paint a cursor with text underneath.
  }
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function act({ event: e }) {
//  // Respond to user input here.
// }

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
