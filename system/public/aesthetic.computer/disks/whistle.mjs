// Whistle, 2023.5.27.21.02.43
// Whistle into the microphone and receive back the same melody as sine waves.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Done
  - [x] Rethink some audio engine stuff for oscillators.
#endregion */

let mic,
  connected = false,
  capturing = false,
  whistling = false;
const pitches = [];
const amps = [];
let pitchesIndex = 0;
const { min } = Math;

// 📰 Meta
function meta() {
  return {
    title: "Whistle",
    desc: "Whistle into the microphone and receive back the same melody as sine waves.",
  };
}

// 🥾 Boot
// function boot() {
//   // Runs once at the start.
// }

// 🎨 Paint
function paint({ wipe, ink }) {
  const w = capturing ? [0, 255, 0] : 127;
  if (
    mic?.pitch &&
    mic.pitch > 0 &&
    mic.pitch !== Infinity &&
    mic.amplitude > 0.075
  ) {
    wipe(w)
      .ink(255, 0, 0)
      .write(mic.pitch.toFixed(2), { x: 4, y: 20 }, 255)
      .ink(0, 0, 255)
      .write(mic.amplitude.toFixed(2), { x: 4, y: 36 }, 255);
  } else {
    wipe(w);
  }

  if (capturing) {
    ink(255).write("WHISTLE NOW", { center: "xy" }, 0);
  }

  if (!connected) {
    ink(255, 0, 0).write("CONNECT MICROPHONE", { center: "xy" }, 255);
  } else if (!capturing) {
    ink(255).write("PUSH DOWN TO WHISTLE", { center: "xy" }, 0);
  }
}

// 🧮 Sim
function sim() {
  mic?.poll(); // Query for updated amplitude and waveform data.
  if (mic && capturing) {
    let pitch = mic.pitch;
    if (pitch === Infinity || pitch === null) pitch = 0;
    pitches.push(pitch);
    amps.push(mic.amplitude);
  }

  if (whistling && sine) {
    pitchesIndex = (pitchesIndex + 1) % pitches.length; // Cycle through all
    //                                                     recorded pitches.

    sine.update({
      tone: pitches[pitchesIndex],
      volume: cutoff(),
    });
  }
}

// 🎪 Act
function act({ event: e }) {
  if (e.is("touch") && !capturing && connected) {
    capturing = true;
    whistling = false;
    pitches.length = 0;
    amps.length = 0;
    pitchesIndex = 0;

    sine?.kill();
    sine = null;
  }

  if (e.is("microphone-connect:success")) connected = true;

  if (e.is("lift") && capturing) {
    capturing = false;
    if (pitches.length > 0) {
      pitches.reverse();
      amps.reverse();
      whistling = true;
    }
  }
}

let sine;

// 🥁 Beat
function beat({ sound: { microphone, square, bpm } }) {
  if (!mic) mic = microphone.connect();

  // TODO: Rethink how oscillators and one-shot sounds work.
  if (whistling && !sine) {
    sine = square({
      tone: pitches[pitchesIndex],
      volume: cutoff(),
      beats: Infinity,
    });
  }
}

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

export { meta, paint, sim, act, beat };

// 📚 Library
function cutoff() {
  let volume = min(1, amps[pitchesIndex] * 10);
  if (volume < 0.1) volume = 0;
  return volume;
}
