// Clock, 2025.5.04.03.29.18.101
// Just a standard clock.

/* 📝 Notes
 */

function boot() {
  // Runs once at the start.
  // TODO: Connect to the new chat instance here.
}

function paint({ wipe, ink, write }) {
  wipe("teal");

  // Making a digital clock display.
  const date = new Date();
  const morning = date.getHours() < 12 ? true : false;
  const hours = morning === false ? date.getHours() - 12 : date.getHours();
  const minutes = date.getMinutes();
  const seconds = date.getSeconds();
  const millis = date.getMilliseconds();
  const ampm = morning ? "AM" : "PM";

  // TODO: 10:9 is written instead of 10:09

  ink("white").write(
    hours + ":" + minutes + ":" + seconds + ":" + millis + " " + ampm,
    6,
    18,
  );
}

// 📚 Library

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
