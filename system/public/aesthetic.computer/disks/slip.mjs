// Slip, 2024.8.20.18.40.34.708
// A single voice instrument.

/* 📝 Notes 
  - [] Create the tone while tapping.
    - [] show minimap of current note etc.
  - [] Make volume go down when further from the center.
  - [] Add pitch draggability across two octaves with proper notching.
  - [] Add a visualizer graph.
  - [] Add a two column / comparison mode?
  - [] add on-screen support for switching wave types?
*/

let voice;

function paint({ wipe, ink, line, screen }) {
  wipe(voice ? 150 : "gray");

  // TODO: Plot the tone axis with all notes, and show the current position
  //       under the pen.
}

function act({ event: e, sound }) {
  // Respond to user input here.
  if (e.is("touch:1")) {
    // TODO: Find where we are on the tone axis.
    voice = sound.synth({ type: "sine", tone: "A4", attack: 0.005, duration: "🔁"});
  }

  if (e.is("draw:1")) {
    console.log("draw");
    // TODO: Slide up and down on the notches.
  }

  if (e.is("lift:1")) {
    voice?.kill();
    voice = null;
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