// Whistle, 2023.5.27.21.02.43
// Whistle into the microphone and receive back the same melody as sine waves.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Add proper start and stop of sine wave with a delay on recording. 
    - [] Add a delay to the start.
  - [-] No longer process amps and pitches in this file. (See `sim`)
  - [] Add reversable playback mode as a parameter.
  + Later
  - [] Allow users to send whistles / instrumentalize whistles
       somehow...
  + Done
  - [x] Get working on mobile.
  - [x] Add visuals to playback.
  - [x] Rethink some audio engine stuff for oscillators.
#endregion */

let mic,
  sine,
  connected = false,
  connecting = false,
  capturing = false,
  whistling = false;

let minAmp = 0.05;

let pitches = [],
  amps = [],
  index = 0;

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
function paint({
  api,
  wipe,
  ink,
  screen: { width, height },
  pen,
  sound: { speaker: spk },
}) {
  const w = capturing ? [0, 255, 0] : 127;

  // Print pitch and amplitude information from mic.
  if (mic?.pitch && mic.amplitude > minAmp) {
    wipe(w)
      .ink(255, 0, 0)
      .write(mic.pitch.toFixed(2), { x: 4, y: 20 }, 255)
      .ink(0, 0, 255)
      .write(mic.amplitude.toFixed(2), { x: 4, y: 36 }, 255);
  } else {
    wipe(w);
  }

  // Microphone Waveform & Amplitude Line
  if (!whistling) {
    // Graph microphone (1 channel)
    if (mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
      paintSound(api, mic.amplitude, mic.waveform, 0, 0, width, height);
    }
  } else {
    // Graph speaker (2 channels)
    const hw = width / 2;
    paintSound(
      api,
      spk.amplitudes.left,
      spk.waveforms.left,
      0,
      0,
      hw,
      height,
      [255, 0, 0, 32]
    );
    paintSound(
      api,
      spk.amplitudes.left,
      spk.waveforms.left,
      hw,
      0,
      hw,
      height,
      [0, 0, 255, 32]
    );
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
function sim({ sound: { microphone, synth, speaker: spk } }) {
  // TODO:
  // I am only getting partial data here... it should probably all be routed
  // via BIOS, possibly through the creation of some kind of managed sound
  // buffer / sample record feature... or at least "poll" should grab
  // whatever is current and empty the last bit out?

  if (whistling && !sine) {
    sine = synth({
      type: "sine",
      tone: pitches[index],
      volume: amps[index],
      beats: Infinity,
    });
  }

  mic?.poll(); // Query for updated amplitude and waveform data.
  spk?.poll();

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
function act({ event: e, sound: { microphone } }) {
  if (e.is("touch") && !connected && !connecting) {
    if (!mic) mic = microphone.connect();
    connecting = true;
  }

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
      let zeros = 0;
      zeros += 16; // Trim a bit at first.
      // while (amps[zeros] === 0) zeros += 1;
      amps = amps.slice(zeros);
      pitches = pitches.slice(zeros);
      // pitches.reverse();
      // amps.reverse();
      whistling = true;
    }
  }
}

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

export { meta, paint, sim, act };

// 📚 Library

function paintSound({ ink }, amplitude, waveform, x, y, width, height, color) {
  const xStep = width / waveform.length + 2;
  const yMid = y + height / 2,
    yMax = height / 2;

  // Amplitude bounding box.
  ink(capturing ? [255, 255, 0] : color || [255, 128]).box(
    x + width / 2,
    yMid,
    width,
    amplitude * yMax * 2,
    "*center"
  );

  // Waveform
  ink(255, 0, 0, 128).poly(
    waveform.map((v, i) => [x + i * xStep, yMid + v * yMax])
  );

  // const y = screen.height - mic.amplitude * screen.height;
  // ink(255, 128).line(0, y, screen.width, y); // Horiz. line for amplitude.
}
