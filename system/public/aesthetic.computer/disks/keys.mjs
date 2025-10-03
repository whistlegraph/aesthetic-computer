// Keys, 2024.4.24.01.32.25.401
// Use keys to trigger actions like musical notes.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🫠] Add a sustain with a fade out.
  - [] Add touch and keyboard shortcuts for notes.
    - [] Test n-key rollover / rollover capability somehow 
        visually.
#endregion */

// 🥾 Boot
function boot({ wipe, sound: { midi } }) {
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

  const range = Math.max(1, high - low + 1);
  const columnWidth = screen.width / range;
  const hasNote = typeof note === "number";
  const clampedNote = hasNote ? num.clamp(note, low, high) : null;
  const safePressure = typeof pressure === "number" ? num.clamp(pressure, 0, pressureHigh) : 0;

  if (hasNote && clampedNote !== null) {
    const noteIndex = clampedNote - low + 0.5; // center the column
    const noteX = noteIndex * columnWidth;
    const green = num.map(clampedNote, low, high, 0, 255);
    ink(255, green, 0, 128 + safePressure).box(
      noteX - columnWidth / 2,
      0,
      columnWidth,
      screen.height,
    );
  }

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
    const status = e.data?.[0] ?? 0;
    const receivedNote = e.data?.[1];
    const velocity = e.data?.[2] ?? 0;
    const command = status & 0xf0;

    if (command === 0x80 || (command === 0x90 && velocity === 0)) {
      // Note-off (or note-on with zero velocity)
      if (note === receivedNote) {
        pressure = 0;
        note = undefined;
      }
      return;
    }

    if (command === 0x90) {
      note = receivedNote;
      pressure = velocity;
      console.log("🎹 MIDI Keyboard Data:", e.data);

      const noteString = sound.midi.note(note);
      const tone = sound.freq(noteString);
      console.log("🎵 Note:", noteString, "📊 Frequency:", tone);

      sound.synth({
        type: help.choose("sine"),
        tone,
        attack: 0.01,
        decay: 0.98,
        volume: 0.25 + 0.85 * (pressure / 127),
        pan: 0,
        duration: 0.3,
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
