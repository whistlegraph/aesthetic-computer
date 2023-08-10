// Bak Tok, 2024.5.27.21.02.43
// Learn to talk backwards.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Turn "CONNECT" into a "ENABLE MICROPHONE" button / generally make
       a button abstraction that is also triggered from a screen tap.
  - [] Change background colors to be distinct from whistle.
  - [] Add a painting / illustration / with more instructions?
    - [] The slogan?
#endregion */

let mic,
  sample,
  connected = false,
  connecting = false,
  capturing = false,
  playing = false;

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
  const w = capturing ? [0, 255, 0] : "teal";
  wipe(w);

  // Microphone Waveform & Amplitude Line
  if (!playing) {
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
    ink(255).write("SPEAK", { center: "xy" }, 0);
  }
}

// 🧮 Sim
function sim({ sound: { microphone, synth, speaker: spk } }) {
  if (playing && !sample) {
    // TODO: Play recorded sample backwards on a loop.
    // sample = synth({
    //   type: "sine",
    //   tone: pitches[index],
    //   volume: amps[index],
    //   beats: Infinity,
    // });
  }

  mic?.poll(); // Query for updated amplitude and waveform data.
  spk?.poll();
}

// 🎪 Act
function act({ event: e, sound: { microphone, sfx }, rec }) {
  if (e.is("touch") && !connected && !connecting) {
    if (!mic) mic = microphone.connect();
    connecting = true;
  }

  if (e.is("touch") && !capturing && connected) {
    // rec: { rolling, cut, print, printProgress }

    // TODO: Start rolling an audio recording, then be able to play it back in reverse.

    rec.rolling("audio"); // 💚 Make sure this works.

    capturing = true;
    playing = false;
    sample?.kill();
    sample = null;
  }

  if (e.is("microphone-connect:success")) {
    connecting = false;
    connected = true;
  }

  if (e.is("lift") && capturing) {

    // rec.cut(); // 💚 Make sure this works.

    // 💚 Get a sample ID back with: rec.print();
    rec.print((a) => {
      console.log("Print completed:", a)
      // sfx.play({sample: id, reverse: true}) // Add sample id here.
    });

    capturing = false;
    playing = true;
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Bak Tok",
    desc: "Learn to talk backwards.",
  };
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
}
