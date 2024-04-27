// Keys, 2024.4.24.01.32.25.401
// Use keys to trigger actions like musical notes.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Write up ABCDEFG and the sharps.
  - [] Test n-key rollover / rollover capability somehow 
       visually.
  - [] Make the sounds nice / add a sustain.
#endregion */

// 🥾 Boot
function boot({ wipe, midi }) {
  wipe(0);
  midi.connect();
}

let note;
let pressure;
const low = 24;
const oct1 = 60;
const high = 104;
const pressureHigh = 127;

// 🎨 Paint
function paint({ wipe, ink, screen, num, sound }) {
  // wipe("gray").ink(0).line(); // Would draw a diagonal line.
  // wipe("blue");
  const hh = screen.height / 2;
  const gap = 12;

  const range = oct1 - low;
  const columnWidth = screen.width / range;
  const noteX = (note - low) * columnWidth;

  // export function map(num, inMin, inMax, outMin, outMax) {
  ink(255, num.map(note, low, oct1, 0, 255), 0, 128 + pressure).box(
    noteX - columnWidth / 2,
    0,
    columnWidth,
    screen.height,
  );

  // ink("red").line(noteX, 0, noteX, screen.height);

  // if (note !== undefined)
  //   ink("white").write(note || "0", { center: "x", y: hh - gap });
  // if (pressure !== undefined)
  //   ink("yellow").write(pressure || "0", { center: "x", y: hh + gap });
  ink(0, 8).box(0, 0, screen.width, screen.height);
}

// 🎪 Act
function act({ event: e, sound, num: { map }, help }) {
  // Respond to user input here.
  if (e.is("midi:keyboard")) {
    note = e.data?.[1];
    pressure = e.data?.[2];
    console.log("🎹 Keyboard:", e.data);

    if (pressure > 0) {
      sound.synth({
        type: help.choose("sine"),
        tone: map(note, low, oct1, 300, 700),
        attack: 0.01,
        decay: 0.98,
        volume: 0.25 + 0.85 * (pressure / 127), // + hsl[2] / 100 / 2,
        pan: 0,
        duration: help.choose(0.3, 0.3, 0.3, 0.3, 0.3),
      });
    }
  }
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

export { boot, paint, act };

// 📚 Library
//   (Useful functions used throughout the piece)
