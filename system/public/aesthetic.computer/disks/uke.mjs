// Uke, 2023.11.25.16.17.59.867
// A live ukelele pitch / note detector.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Detect a sustained pitch.
  + Done
  - [x] Detect and report ukelele pitch. 
#endregion */

let mic,
  connecting = false,
  connected = false;

// 🥾 Boot
function boot({ api, wipe }) {
  // Runs once at the start.
  wipe("blue"); // Clear's the screen. Can use R, G, B or CSS colors.
}

// 🎨 Paint
function paint({ api, wipe }) {
  if (mic) {
    console.log(mic.amplitude);
    if (mic.amplitude > 0.05 && mic.pitch) {
      wipe(127)
        .ink(255)
        .write(mic.pitch.toFixed(2), { center: "xy" })
        .ink("yellow")
        .write(mic.amplitude, { x: 6, y: 20 })
        .ink("red")
        .write(closestNoteFromFreq(mic.pitch), { x: 6, y: 40 });
    }
  } else {
    wipe(196);
  }
}

// 🎪 Act
function act({ event: e, sound }) {
  if (e.is("touch") && !connected && !connecting) {
    if (!mic) mic = sound.microphone.connect();
    connecting = true;
  }

  if (e.is("microphone-connect:success")) {
    connecting = false;
    connected = true;
  }
}

// 🧮 Sim
function sim() {
  mic?.poll();
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Uke",
    desc: "A live ukelele pitch / note detector.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, sim, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

function closestNoteFromFreq(freq) {
  // Define a mapping of notes to frequencies in octave 4
  const noteFrequencies = {
    C4: 261.63,
    "C#4/Db4": 277.18,
    D4: 293.66,
    "D#4/Eb4": 311.13,
    E4: 329.63,
    F4: 349.23,
    "F#4/Gb4": 369.99,
    G4: 392.0,
    "G#4/Ab4": 415.3,
    A4: 440.0,
    "A#4/Bb4": 466.16,
    B4: 493.88,
  };

  // Function to calculate the frequency for a given note and octave
  function calcFreq(note, octave) {
    return noteFrequencies[note] * Math.pow(2, octave - 4);
  }

  // Find the closest note
  let closestNote = "";
  let smallestDiff = Infinity;

  for (const note in noteFrequencies) {
    for (let octave = 0; octave <= 8; octave++) {
      const noteFreq = calcFreq(note, octave);
      const diff = Math.abs(freq - noteFreq);

      if (diff < smallestDiff) {
        smallestDiff = diff;
        closestNote = note.replace("4", octave); // Replace the octave in the note name
      }
    }
  }

  return closestNote;
}
