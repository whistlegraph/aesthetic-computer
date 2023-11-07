// Song, 2023.7.02.09.27.28
// Notate both a melody and lyrics to sing along to.

//import { noteFrequencies } from "./common/music.mjs";

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  * Slides
  - [🧡] Fade the ending note so there is no pop.
  - [] How would I capture a tone and then add it to the list of notes in
       my song... / compose.
  * Volume + Pan
  - [] How to vary volume during playback?
  - [] Maybe there needs to be a separate slider or it
        can be for a secondary finger.
  - [] Dragging left and right adjusts the pan.
  + Done
  - [x] Pre-program `pgtw`. 
  - [x] Add big button for starting a continuous sine wave.
  - [x] Pushing the button down starts a tone and letting go ends it.
  - [x] Dragging up and down adjusts the pitch.
  + Later
#endregion */

let btn,
  sound,
  tone = 800;

//const pgtw = "F F G G A 6C A F C F F G 6C A".split(" ");
const pgtw = `4G C C D D E G E C
              4G C C D D E C
              C C D D E G E C
              A D F E C`.split(/\s+/);

const lyrics = `oh up and down the ci- -i- -ty road
                and in and out the ea- -gle 
                that's the way the mo- -o- -ney goes
                pop! goes the wea- -sel!`.split(/\s+/);

let index = 0;
let song = pgtw;

// 🥾 Boot
function boot({ ink, wipe, screen, ui }) {
  wipe(127);
  const m = 24;
  btn = new ui.Button(m, m, screen.width - m * 2, screen.height - m * 2);

  tone = freq(song[index]); // Set starting tone.
}

// 🎨 Paint
function paint({ ink, screen }) {
  // Executes every display frame.
  btn.paint(() => {
    ink(btn.down ? [245, 220, 50] : [230, 210, 100])
      .box(btn.box)
      .ink(255)
      .write(`${song[index]} - ${tone.toFixed(2)}`, { center: "xy" })
      .ink("red")
      .write(lyrics[index], { center: "x", y: screen.height / 2 - 16 });

    ink(0).line(0, screen.height - 1, screen.width, screen.height - 1);
    ink(255).line(
      0,
      screen.height - 1,
      ((index + 1) / song.length) * screen.width,
      screen.height - 1,
    );
    return false; // Uncomment for an animation loop.
  });
}

// 🎪 Act
function act({ event: e, sound: { synth }, needsPaint, num }) {
  // Drag up and down to change pitch.
  if (btn.down && e.is("draw")) {
    tone = num.clamp(tone - e.delta.y, 100, 1200);
    sound.update({ tone });
    // console.log("📈 Tone update:", tone);
    needsPaint();
  }

  btn.act(
    e,
    {
      push: () => {
        sound?.kill();
        needsPaint();
        index = (index + 1) % song.length; // Cycle through notes.
        tone = freq(song[index]);
      },
      down: () => {
        sound = synth({ type: "sine", tone, volume: 1.0, beats: Infinity });
        needsPaint();
      },
      rollover: () => {
        // needsPaint();
      },
      rollout: () => {
        // needsPaint();
      },
      cancel: () => {
        sound?.kill();
        needsPaint();
      },
    },
    // pens?.() // Enables multi-touch support for this button.
  );
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 📰 Meta
function meta() {
  return {
    title: "Song",
    desc: "Notate both a melody and lyrics to sing along to.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
const noteFrequencies = {
  C: 16.35,
  "C#": 17.32,
  Db: 17.32,
  D: 18.35,
  "D#": 19.45,
  Eb: 19.45,
  E: 20.6,
  F: 21.83,
  "F#": 23.12,
  Gb: 23.12,
  G: 24.5,
  "G#": 25.96,
  Ab: 25.96,
  A: 27.5,
  "A#": 29.14,
  Bb: 29.14,
  B: 30.87,
};

function freq(noteString) {
  let octave;
  let note;

  // Check if the first character is a digit to determine if an octave is provided
  if (!isNaN(noteString.charAt(0))) {
    // If the first character is a digit, it's the octave
    octave = parseInt(noteString.charAt(0), 10);
    note = noteString.substring(1);
  } else {
    // If no octave is provided, default to octave 5
    octave = 5;
    note = noteString;
  }

  // Look up the frequency for the note
  let frequency = noteFrequencies[note];
  if (!frequency) {
    throw new Error("Note not found in the list");
  }

  // Calculate the frequency for the given octave
  return frequency * Math.pow(2, octave - 1);
}
