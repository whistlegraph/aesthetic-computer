// Flap, 2025.2.12.21.19.05.558
// Two frames make a flap, more are welcome.

/* 📝 Notes
  - [] Flap combines multiple frames into an animated sequence.
  - [] Frames can be derived from paintings.
  - [] Frames need to be played back.
  - [] Frames need to persist locally.
  - [] A concept of finishing should occur.
  - [] To make a two frame flap on the CLI.
    -  [] Enter `flap 1` to make the current painting as frame 1.
    -  [] Enter `flap 2` to make the current painting as frame 2.
    -  [] Enter `flap` to play the flap at a certain framerate.
*/

let num = 0;
let rate = 1;

function boot({ params, system, store }) {
  const inFrame = parseInt(params[0]);
  console.log(inFrame);

  if (!isNaN(inFrame)) {
    console.log("🖼️ Grab painting as frame:", inFrame, system.painting);
    const frame = system.painting;
    const op = frame.pixels;
    const pixels = new Uint8ClampedArray(op.length);
    pixels.set(op);
    store[`flap~${inFrame}`] = {
      pixels,
      width: frame.width,
      height: frame.height,
    };
  }
}

function paint({ stamp, store, paintCount, ink, wipe, screen }) {
  wipe(0);

  if (paintCount % rate === 0) {
    if (store[`flap~${num + 1}`]) {
      num += 1;
    } else {
      num = 0;
    }
  }

  console.log(screen);

  if (store[`flap~${num}`]) stamp(store[`flap~${num}`], screen.width / 2, screen.height / 2);

  ink("yellow").write(rate, { right: 8, top: 6 });
  ink("blue").write(num, { right: 8, top: 16 });
}

function sim({ store }) {}

function act({ event: e }) {

  if (e.is("touch")) {
    rate *= 2;
    if (rate > 16) rate = 1;
  }

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
