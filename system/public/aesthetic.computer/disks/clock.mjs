// Clock, 2025.5.04.03.29.18.101
// Just a standard clock.

/* 📝 Notes
  - [🟠] add a button to 'learn' the clock.
 */

let synced = false;

function boot({ ui, clock }) {
  // Runs once at the start.
  // Get the UTC offset from /api/clock and use that to set the clock.
  clock.resync();
}

// Helper function to pad numbers with leading zeros
function pad(num, size = 2) {
  num = num.toString();
  while (num.length < size) num = "0" + num;
  return num;
}

function paint({ wipe, ink, write, clock }) {
  const syncedDate = clock.time(); // Get time once at the beginning
  let bgColor;
  let currentSeconds; // To store seconds if syncedDate is valid

  if (syncedDate) {
    currentSeconds = syncedDate.getSeconds(); // Get seconds if time is valid
  }

  if (synced) {
    bgColor = "cyan";
  } else {
    if (syncedDate) {
      const lastDigitOfSecond = currentSeconds % 10;
      // Define 10 shades of purple, from darker to lighter/more bluish
      const purpleShades = [
        [130, 0, 60], // Second ends in 0
        [130, 0, 80], // Second ends in 1
        [130, 0, 100], // Second ends in 2
        [130, 0, 120], // Second ends in 3
        [130, 0, 140], // Second ends in 4
        [130, 0, 160], // Second ends in 5
        [130, 0, 180], // Second ends in 6
        [130, 0, 200], // Second ends in 7
        [130, 0, 220], // Second ends in 8
        [130, 0, 240], // Second ends in 9
      ];
      bgColor = purpleShades[lastDigitOfSecond];
    } else {
      bgColor = "purple"; // Fallback if syncedDate is not available (original behavior)
    }
  }
  wipe(bgColor);

  if (synced) {
    synced = false; // Reset synced flag after using it for wipe color
  }

  // Making a digital clock display.
  if (syncedDate) {
    const morning = syncedDate.getHours() < 12;
    let hours = syncedDate.getHours();
    hours = hours % 12;
    hours = hours ? hours : 12; // the hour '0' should be '12'
    const minutes = pad(syncedDate.getMinutes());
    const displaySeconds = pad(currentSeconds); // Use currentSeconds from above and pad it
    const millis = pad(syncedDate.getMilliseconds(), 3);
    const ampm = morning ? "AM" : "PM";

    ink("white").write(
      hours + ":" + minutes + ":" + displaySeconds + ":" + millis + " " + ampm,
      { center: "xy", size: 2 },
    );
  } else {
    ink("red").write("SYNCING...", { center: "xy", size: 2 });
  }
}

// 📚 Library

function act({ event: e, clock }) {
  //  // Respond to user input here.
  if (e.is("touch")) {
    // clock.resync();
  }
}

let lastSecond;
let lastHalfSecond;
let lastQuarterSecond;
let overallLastQuarterSecond; // Added for tracking every quarter second change
let lastEighthSecond;
let lastSixteenthSecond; // Re-added for tracking every sixteenth second change

function sim({ sound, beep, clock, num, help }) {
  // Runs once per logic frame. (120fps locked.)
  // Get the current time and beep at different intervals
  const time = clock.time();

  if (!time) return;

  const seconds = time.getSeconds();
  const milliseconds = time.getMilliseconds();
  const currentHalfSecond = Math.floor(milliseconds / 500); // 0 for first half, 1 for second half
  const currentQuarterSecond = Math.floor(milliseconds / 250); // 0-3 for each quarter
  const currentEighthSecond = Math.floor(milliseconds / 125);
  const currentSixteenthSecond = Math.floor(milliseconds / 62.5); // Re-added for sixteenths

  // Initialize last values if undefined
  if (lastSecond === undefined) lastSecond = seconds;
  if (lastHalfSecond === undefined) lastHalfSecond = currentHalfSecond;
  if (lastQuarterSecond === undefined) lastQuarterSecond = currentQuarterSecond;
  if (overallLastQuarterSecond === undefined)
    overallLastQuarterSecond = currentQuarterSecond; // Initialize new tracker
  if (lastEighthSecond === undefined) lastEighthSecond = currentEighthSecond; // Initialize eighth note tracker
  if (lastSixteenthSecond === undefined)
    lastSixteenthSecond = currentSixteenthSecond; // Initialize sixteenth note tracker

  // Beep when the second changes
  if (seconds !== lastSecond) {
    beep();

    clock.resync();
    //sound.synth({
    //  type: "sine",
    //  tone: help.choose("2G", "2B", "3D") /*num.randInt(800)*/,
    //  duration: 4.225,
    //  attack: 0.01,
    //  decay: 0.01,
    //  volume: 0.025,
    //});

    synced = true;


    lastSecond = undefined;
    lastHalfSecond = undefined;
    lastQuarterSecond = undefined;
    overallLastQuarterSecond = undefined; // Added for tracking every quarter second change
    lastEighthSecond = undefined;
    lastSixteenthSecond = undefined; // Re-added for tracking every sixteenth second change

    // lastSecond = seconds;
  }

  // Higher pitch beep at half-second
  if (currentHalfSecond !== lastHalfSecond && seconds === lastSecond) {
    // sound.synth({ type: "sine", tone: help.choose("8A", "8B"), duration: 0.01, volume: 0.25 });
    lastHalfSecond = currentHalfSecond;
  }

  // Even higher pitch beep at quarter-seconds (but not when half or full seconds)
  if (
    currentQuarterSecond !== lastQuarterSecond &&
    currentQuarterSecond % 2 !== 0 &&
    seconds === lastSecond
  ) {
    // sound.synth({ type: "sine", tone: help.choose("G", "E", "C"), duration: help.choose(0.1, 1.0, 2.0),  volume: help.choose(0.025, 0.05, 0.1) });
    lastQuarterSecond = currentQuarterSecond;
  }

  // New higher pitched sine click for every quarter second change
  if (currentQuarterSecond !== overallLastQuarterSecond) {
    // sound.synth({ type: "sine", tone: help.choose("B", "6B"), duration: 0.040, volume: 0.03 /*num.rand() * 0.7*/ });
  }

  // Eighth note ticks
  if (currentEighthSecond !== lastEighthSecond) {
    //if (help.choose(true, false)) {
    //  sound.synth({ type: "sine", tone: help.choose("4C", "4E", "4G"), duration: 2.84, volume: 0.025, attack: 0.5, decay: 0.99 });
    //}
  }

  // Sixteenth note ticks
  if (currentSixteenthSecond !== lastSixteenthSecond) {
    //if (help.choose(true, false)) {
      // sound.synth({ type: "sine", tone: 9000, duration: 0.005, volume: 0.25 }); // Adjusted tone and volume for distinction
    //}
  }

  // Update the overall quarter second tracker for the next frame
  overallLastQuarterSecond = currentQuarterSecond;
  lastEighthSecond = currentEighthSecond; // Update eighth note tracker for the next frame
  lastSixteenthSecond = currentSixteenthSecond; // Update sixteenth note tracker for the next frame
}

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
