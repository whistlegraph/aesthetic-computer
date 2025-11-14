// Tv, 2024.12.08.03.22.28.351
// Remotely create pictures with `notepat`.

/* 📝 Notes
   - [] How to control rounds / reset after a certain amount
        of time has passed?
  *
  - [x] Send messages from `notepat` to control
        the state of a wandering turtle
        through UDP, and also relay the note-pressed.
 */

let server, lastNote;

function boot({ net: { udp } }) {
  server = udp((type, content) => {
    lastNote = content.note;
  });
}

function paint({ api, wipe, ink, crawl, left, right, up, down, goto, face }) {
  wipe("black");
  ink("yellow").write(lastNote || "none", { center: "xy", size: 6 });
}

// 📚 Library

function act({ event: e }) {
  //  // Respond to user input here.
  if (e.is("touch")) {
    server.send("tv", { test: true });
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
