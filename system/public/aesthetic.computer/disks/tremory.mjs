// Temory, 2025.3.31.22.47.48.311
// A temperal memory trainer.

/* 📝 Notes


  There needs to be a concept of a musical track or measure, with a progress bar.

  These measures can be owned either by the cpu or the human and
  they have specifications / threshholds that must be met otherwise
  the measure loops on itself in a failure state.

 */

const mode = "lead"; // "lead" or "follow"

let pattern = "1234567890";

/*
const intro = [
  { text: "Hello...", duration: 120 },
  { text: "We are going to play!", duration: 120 },
  { text: "Now watch me carefully...", duration: 120 },
  { text: "And follow along!", duration: 120 },
  { text: "Ready?", duration: 120 },
  { text: "3", duration: 60 },
  { text: "2", duration: 60 },
  { text: "1", duration: 60 },
];

let set = intro;

let slide = 0;
let duration = set[slide].duration; // 2 seconds at a 120fps step rate
let step = duration;
*/

let measure = 0;
let measureMax = 4;
const beatsPerMeasure = 4;
let currentBeatInMeasure = 0;
let measureSound;
let beatProgress = 0;
const { max } = Math;

function beat({ sound }) {
  needsFlash = true;
  currentBeatInMeasure += 1;
  if (currentBeatInMeasure > beatsPerMeasure - 1) {
    measure += 1;
    if (measure > measureMax - 1) measure = 0;
    currentBeatInMeasure = 0;
  }

  measureSound = sound.synth({
    type: "sine",
    tone: 0,
    beats: 1,
    volume: 0,
    pan: 0, // Should I pan left or right on every other beat?
  });
}

function boot({ speak }) {}

function paint({ wipe, ink, screen, sound }) {
  if (needsFlash) {
    needsFlash = false;
    wipe(128);
  } else {
    wipe(0);
  }

  // 📊 Details

  // Bpm
  ink("white").write("bpm: " + sound.bpm(), 6, 22);
  ink("white").box(0, 34, max(1, beatProgress * screen.width), 2);

  // Beat
  ink("white").write(
    `beat: ${currentBeatInMeasure + 1}/${beatsPerMeasure}`,
    6,
    22 + 12 + 6,
  );

  // Measure
  ink("white").box(
    0,
    22 + 22 + 6,
    ((currentBeatInMeasure + 1) / beatsPerMeasure) * screen.width,
    2,
  );

  ink("white").write(
    `measure: ${measure + 1}/${measureMax}`,
    6,
    22 + 22 + 6 + 6,
  );

  ink("white").box(
    0,
    22 + 22 + 22,
    ((measure + 1) / measureMax) * screen.width,
    2,
  );

  /*
  if (slide >= 0) {
    ink("white").write(
      intro[slide].text,
      { center: "xy" },
      null, // "red",
      screen.width,
    );
    ink("white").box(0, screen.height - 2, (step / duration) * screen.width, 2);
  }
  */
}

function sim({ api, sound: { time } }) {
  if (measureSound) {
    beatProgress = measureSound.progress(time);
    // if (p === 1) flash = false; // TODO: This might be skipping 1 frame.
  }
  /*
  if (slide >= 0) {
    step -= 1;
    if (step === 0) next(api);
  }
  */
}

// 📚 Library

// 🛝 Transition to the next screen / user message.
function next({ speak }) {
  slide += 1;
  if (slide > set.length - 1) {
    slide = -1;

    // Advance the set?
  } else {
    duration = set[slide].duration;
    step = duration;
    // speak(set[slide].text);
  }
}

let needsFlash = false;

// function act({ event: e }) {
//  // Respond to user input here.
// }

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
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
