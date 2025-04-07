// Temory, 2025.3.31.22.47.48.311
// A temperal memory trainer.

/* 📝 Notes
  - [-] These measures can be owned either by the cpu or the human and
       they have specifications / threshholds that must be met otherwise
       the measure loops on itself in a failure state.
    - [] Make a data model for a song. 

  - [] Trigger a sound on each section beat. 
    - [] (Would these be well timed?)
  + Done
  - [x] There needs to be a concept of a musical track or measure, with a progress
        bar.
*/

// Beat
let beatStart;
let beatProgress = 0;

// Measure
const beatsPerMeasure = 4;
let beatInMeasure = -1;

// Phrase
let measureInPhrase = 0;
let measuresInPhrase = 4;

let flash = false;
const { max, min } = Math;

function boot({ speak, sound }) {
  sound.bpm(60);
  beat.start = sound.time;
}

function beat({ sound }) {
  flash = true; // Set the screen to flash every beat.
  beatInMeasure += 1;
  if (beatInMeasure > beatsPerMeasure - 1) {
    measureInPhrase += 1;
    if (measureInPhrase > measuresInPhrase - 1) measureInPhrase = 0;
    beatInMeasure = 0;
  }
  beatStart = sound.time; // Reset the beat progress tracker.
}

function paint({ wipe, ink, screen, sound }) {
  wipe(flash ? 128 : 0);
  if (flash) flash = false;

  const ly = 20;
  ink("white")
    .write("bpm: " + sound.bpm(), 6, ly)
    .write(`beat: ${beatInMeasure + 1}/${beatsPerMeasure}`, 6, ly + 12)
    .write(
      `measure: ${measureInPhrase + 1}/${measuresInPhrase}`,
      6,
      ly + 12 + 12,
    );

  // 📊 Details
  const barHeight = 10;
  const barStartY = 72;
  let barNum = 1;
  let by;

  function nextBar() {
    by = barStartY + barHeight * barNum;
    barNum += 1;
  }

  // 🕰️ BPM Bar
  nextBar();
  ink("red").box(0, by, screen.width, barHeight);
  ink("white").box(0, by, max(1, beatProgress * screen.width), barHeight);
  ink("yellow").line(screen.width - 1, by, screen.width - 1, by + barHeight);

  // 📏 Measure Bar
  nextBar();
  const measureProgress = (beatInMeasure + beatProgress) / beatsPerMeasure;
  ink("gray").box(0, by, screen.width, barHeight);
  ink("red").box(
    0,
    by,
    ((beatInMeasure + 1) / beatsPerMeasure) * screen.width,
    barHeight,
  );
  ink("white").box(0, by, measureProgress * screen.width, barHeight);
  ink("yellow").box(
    0,
    by,
    (beatInMeasure / beatsPerMeasure) * screen.width,
    barHeight,
  );
  for (let b = 1; b <= beatsPerMeasure; b += 1) {
    const x = min((b / beatsPerMeasure) * screen.width, screen.width - 1);
    ink("yellow").line(x, by, x, by + barHeight - 1);
  }

  // 🎼 Composition bar
  nextBar();
  ink("gray").box(0, by, screen.width, barHeight);
  ink("red").box(
    0,
    by,
    ((measureInPhrase + 1) / measuresInPhrase) * screen.width,
    barHeight,
  );
  ink("white").box(
    0,
    by,
    (measureInPhrase / measuresInPhrase + measureProgress / measuresInPhrase) *
      screen.width,
    barHeight,
  );
  ink("yellow").box(
    0,
    by,
    (measureInPhrase / measuresInPhrase +
      beatInMeasure / beatsPerMeasure / measuresInPhrase) *
      screen.width,
    barHeight,
  );
  ink("yellow").line(
    screen.width - 1,
    by,
    screen.width - 1,
    by + barHeight - 1,
  );
}

function sim({ api, sound }) {
  if (beatStart) beatProgress = (sound.time - beatStart) / (60 / sound.bpm());
}

// 📚 Library

// 🛝 Transition to the next screen / user message.
/*
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
*/

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

/*
if (slide >= 0) {
  step -= 1;
  if (step === 0) next(api);
}
*/

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
