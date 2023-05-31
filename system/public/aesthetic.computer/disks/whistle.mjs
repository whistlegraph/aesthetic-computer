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
  connecting = false,
  capturing = false,
  captureTimeout,
  whistling = false;

let minAmp = 0.05;

let pitches = [],
  amps = [];
let index = 0;

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
function paint({ wipe, ink, screen: { width, height }, pen }) {
  const w = capturing ? [0, 255, 0] : 127;
  if (mic?.pitch && mic.amplitude > minAmp) {
    wipe(w)
      .ink(255, 0, 0)
      .write(mic.pitch.toFixed(2), { x: 4, y: 20 }, 255)
      .ink(0, 0, 255)
      .write(mic.amplitude.toFixed(2), { x: 4, y: 36 }, 255);
  } else {
    wipe(w);
  }

  // Waveform & Amplitude Line
  if (mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
    const xStep = width / mic.waveform.length + 2;
    const yMid = height / 2,
      yMax = height / 2;

    // Amplitude bounding box.
    ink(capturing ? [255, 255, 0] : [255, 128]).box(
      width / 2,
      yMid,
      width,
      mic.amplitude * yMax * 2,
      "*center"
    );

    ink(255, 0, 0, 128).poly(
      mic.waveform.map((v, i) => [i * xStep, yMid + v * yMax])
    );

    // const y = height - mic.amplitude * height;
    // ink(255, 128).line(0, y, width, y); // Horiz. line for amplitude.
  }

  if (capturing) ink(255).write("NOW!", { center: "xy" }, 0);

  if (!connected) {
    const color = pen?.drawing || connecting ? [255, 0, 0] : [0, 0, 255];
    ink(color).write(
      connecting ? "CONNECTING..." : "CONNECT",
      { center: "xy" },
      255
    );
  } else if (!capturing) {
    ink(255).write("WHISTLE", { center: "xy" }, 0);
  }
}

// 🧮 Sim
function sim() {
  mic?.poll(); // Query for updated amplitude and waveform data.
  if (mic && capturing) {
    let pitch = mic.pitch;
    if (pitch === Infinity || pitch === null || pitch < 0) pitch = null;
    pitches.push(pitch);
    amps.push(mic.amplitude < minAmp ? 0 : mic.amplitude);
  }

  if (whistling && sine) {
    index = (index + 1) % pitches.length; // Cycle through all recorded pitches.
    sine.update({ tone: pitches[index], volume: amps[index] });
  }
}

// 🎪 Act
function act({ event: e }) {
  if (e.is("touch") && !connected && !connecting) connecting = true;

  if (e.is("touch") && !capturing && connected) {
    capturing = true;
    whistling = false;
    pitches.length = 0;
    amps.length = 0;
    index = 0;
    sine?.kill();
    sine = null;
  }

  if (e.is("microphone-connect:success")) {
    connecting = false;
    connected = true;
  }

  if (e.is("lift") && capturing) {
    capturing = false;
    if (pitches.length > 0) {
      // Reverse the playback.
      // pitches.reverse();
      // amps.reverse();

      let zeros = 0;
      zeros += 15; // Trim the first 1/8th second no matter what.
      // while (amps[zeros] === 0) zeros += 1;
      amps = amps.slice(zeros);
      pitches = pitches.slice(zeros);
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
      tone: pitches[index],
      volume: amps[index],
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
