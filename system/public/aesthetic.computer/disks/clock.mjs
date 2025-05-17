// Clock, 2025.5.04.03.29.18.101
// Just a standard clock.

/* 🐢 TODO
  - [] Figure out what the timeslice should be.
  - [-] Add a button to 'learn' the clock.
  - [✅] Automatically make make the font scale 1 if the text is too wide for screen.width.  
       (each character is 6 pixels wide, so if the text is longer than screen.width / 8, scale it down)
 */

let synced = false;
let sequence, sequenceIndex = 0;
let octave = 4;

function boot({ ui, clock, params, colon }) {
  // Get the UTC offset from /api/clock and use that to set the clock.
  clock.resync();
  if (params[0]) sequence = params[0].split("");
  octave = parseInt(colon[0]) || octave;
}

function paint({ wipe, ink, write, clock, screen }) {
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
      // Define colors based on ROYGBIV spectrum plus black, white, and gray
      const colors = [
        "red",       // Second ends in 0
        "orange",    // Second ends in 1
        "yellow",    // Second ends in 2
        "green",     // Second ends in 3
        "blue",      // Second ends in 4
        "indigo",    // Second ends in 5
        "violet",    // Second ends in 6
        "black",     // Second ends in 7
        "white",     // Second ends in 8
        "gray"       // Second ends in 9
      ];
      bgColor = colors[lastDigitOfSecond];
    } else {
      bgColor = "purple"; // Fallback if syncedDate is not available
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

    const timeString =
      hours + ":" + minutes + ":" + displaySeconds + ":" + millis + " " + ampm;

    let fontSize = 1; // Default size

    // if (timeString.length * 6 < screen.width) {
    //   fontSize = 1;
    // } else if (timeString.length * 6 < screen.width * 0.8) {
    // }

    ink("white").write(timeString, { center: "xy", size: fontSize }, "black");
    ink("red").line(0, screen.height / 2, 8 + 5, screen.height / 2);
    ink("red").line(screen.width - 9, screen.height / 2, screen.width, screen.height / 2);
  } else {
    ink("red").write("SYNCING...", { center: "xy", size: 2 });
  }
}

// 📚 Library

function act({ event: e, clock, sound: { synth } }) {
  //  // Respond to user input here.
  if (e.is("touch")) {
    // clock.resync();
    // synth();
  }
}

let lastSecond;
let lastHalfSecond;
let lastQuarterSecond;
let overallLastQuarterSecond; // Added for tracking every quarter second change
let lastEighthSecond;
let lastSixteenthSecond; // Re-added for tracking every sixteenth second change

function sim({ sound, beep, clock, num, help, params, colon }) {
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

  function bleep() {
    sound.synth({
      type: "sine",
      tone: octave + (sequence?.[sequenceIndex] || help.choose("G", "B", "D")) /*num.randInt(800)*/,
      duration: params[1] || 0.1,
      // attack: 0.01,
      // decay: 0.01,
      // volume: 0.5,
    });
    sequenceIndex = (sequenceIndex + 1) % sequence.length;
  }

  // Beep when the second changes
  if (seconds !== lastSecond) {
    // beep();

    clock.resync();

    if (!colon[1] || colon[1] === "sec") {
      bleep();
    }

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
  if (currentHalfSecond !== lastHalfSecond /* && seconds === lastSecond*/) {
    // sound.synth({ type: "sine", tone: help.choose("8A", "8B"), duration: 0.01, volume: 0.25 });
    lastHalfSecond = currentHalfSecond;
    if (colon[1] === "half") bleep();
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
    if (colon[1] === "quart") bleep();
  }

  // Eighth note ticks
  if (currentEighthSecond !== lastEighthSecond) {
    //if (help.choose(true, false)) {
    //  sound.synth({ type: "sine", tone: help.choose("4C", "4E", "4G"), duration: 2.84, volume: 0.025, attack: 0.5, decay: 0.99 });
    //}
    if (colon[1] === "eight") bleep();
  }

  // Sixteenth note ticks
  if (currentSixteenthSecond !== lastSixteenthSecond) {
    //if (help.choose(true, false)) {
    // sound.synth({ type: "sine", tone: 9000, duration: 0.005, volume: 0.25 }); // Adjusted tone and volume for distinction
    //}
    if (colon[1] === "sixt") bleep();
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

function pad(num, size = 2) {
  num = num.toString();
  while (num.length < size) num = "0" + num;
  return num;
}
