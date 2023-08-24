// BakTok, 2024.5.27.21.02.43
// Learn 2 talk backwards.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Drag line to scrub recording head and add gestural recording and
       playback. (Easter egg?)
  - [] Change themes to be distinct from whistle.
    - [] Make start button green.
    - [] Make record button red.
    - [] Turn push and hold into "again" after the first time.
  - [] Add a painting or illustration in the background / a logo.
  - Pre-launch:
  - [] Make the microphone access / audio context work better.
  + Next version
  - [] Add layering option so multiple sounds can be layered and removed,
       stacked on top of one another, etc. 
  + Done
  - [x] Activity flow:
    - [x] Show waveform of the sample that is playing back?
    - [x] Let go and it plays back visually.
  - [x] Button: [Activate] or [Start] (change "Connect" to a button)
  - [x] Avoid accidental label taps.
  - [x] Record user voice and play it backwards, for backwards vocal training.
  - [x] Becomes...
          "SAY"

      [PUSH AND HOLD]
        
        "SOMETHING"
#endregion */

let mic,
  btn,
  sample,
  sampleData,
  connected = false,
  connecting = false,
  capturing = false,
  playing = false,
  progress;

let hideButton = false,
  hideButtonTimeout;

const { max } = Math;

// 🥾 Boot
function boot({ ui, screen }) {
  // Runs once at the start.
  btn = new ui.TextButton(`START`, { center: "xy", screen });
}

// 🎨 Paint
function paint({
  api,
  wipe,
  ink,
  screen: { width, height },
  num,
  pen,
  pan,
  unpan,
  help: { choose },
  sound: { speaker: spk },
}) {
  const w = capturing ? [0, 127, 0] : "teal";
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
      num.arrMax(sampleData),
      num.arrCompress(sampleData, 32),
      0,
      0,
      width,
      height,
      [0, 0, 255, 32]
    );

    if (typeof progress === "number") {
      const p = (1 - progress) * width;
      ink().line(p, 0, p, height);
    }
  }

  if (capturing) {
    const hh = height / 2;
    const hq = hh / 2;
    const yo = 10;
    pan(choose(-1, 0, 1), choose(-1, 0, 1));
    ink().write("SAY", { center: "x", y: hh - yo - hq, size: 2 });
    unpan();
    pan(choose(-1, 0, 1), choose(-1, 0, 1));
    ink().write("SOMETHING", { center: "x", y: hh - yo + hq, size: 2 });
    unpan();
  }

  if (!capturing && !playing) {
    const hh = height / 2;
    const hq = hh / 3.5;
    const yo = 10;
    pan(choose(-1, 0, 1), choose(-1, 0, 1));
    ink("cyan").write("LEARN 2 TALK", {
      center: "x",
      y: hh - yo - hq,
      size: 2,
    });
    unpan();
    pan(choose(-1, 0, 1), choose(-1, 0, 1));
    ink("cyan").write("BACKWARDS", { center: "x", y: hh - yo + hq, size: 2 });
    unpan();
  }

  if (!hideButton) btn.paint({ ink });
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

  sample?.progress().then((p) => (progress = p.progress)); // Get progress data.

  mic?.poll(); // Query for updated amplitude and waveform data.
  spk?.poll();
}

// 🎪 Act
async function act({
  event: e,
  screen,
  sound: { microphone, play, synth },
  rec,
}) {
  if (e.is("reframed")) {
    btn.reposition({ center: "xy", screen });
  }

  if (e.is("touch") && !connected) {
    btn.down = true;
    synth({
      tone: 400,
      beats: 0.1,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
  }

  if (e.is("lift") && btn.down && !connected) {
    synth({
      tone: 600,
      beats: 0.1,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
    btn.disabled = true;
    hideButton = true;
    clearTimeout(hideButtonTimeout);
    hideButtonTimeout = setTimeout(() => {
      hideButton = false;
    }, 250);
    btn.reposition({ center: "xy", screen }, "Enabling Microphone");
    if (!mic) mic = microphone.connect();
    connecting = true;
    btn.down = false;
  }

  btn.act(e); // Pass user events through to the button.

  // Start a microphone recording.
  if (e.is("touch") && !capturing && connected) {
    sample?.kill(); // Stop any existing sample.
    microphone.rec(); // Start recording.
    btn.down = true;
    capturing = true;
    playing = false;
    progress = null;
  }

  if (e.is("microphone-connect:success")) {
    connecting = false;
    connected = true;
    btn.disabled = false;
    clearTimeout(hideButtonTimeout);
    hideButton = false;
    // 🔴 Add a red color scheme here...
    btn.reposition({ center: "xy", screen }, "PUSH AND HOLD");
  }

  if (e.is("lift") && capturing) {
    btn.down = false;
    const { id, data } = await microphone.cut(); // End recording and get the sample.
    sampleData = data;
    sample = play(id, { reverse: true, loop: true }); // TODO: Get reverse working.
    capturing = false;
    playing = true;
    btn.reposition({ center: "xy", screen }, "REPEAT WHAT YOU HEAR");
  }
}

// 📰 Meta
function meta() {
  return {
    title: "BakTok · aesthetic.computer",
    desc: "Learn 2 talk backwards.",
  };
}

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

export { boot, paint, sim, act, meta };

// 📚 Library

// Paints a waveform with a bounding box based on amplitude.
function paintSound({ ink }, amplitude, waveform, x, y, width, height, color) {
  const xStep = width / (waveform.length - 1);
  const yMid = y + height / 2,
    yMax = height / 2;

  // Amplitude bounding box.
  if (amplitude) {
    ink(capturing ? [255, 255, 0] : color || [255, 128]).box(
      x + width / 2,
      yMid,
      width,
      amplitude * yMax * 2,
      "*center"
    );
  }

  ink(255, 0, 0, 128).poly(
    waveform.map((v, i) => {
      return [x + i * xStep, yMid + v * yMax];
    })
  );
}
