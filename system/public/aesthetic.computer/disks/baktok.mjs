// BakTok, 2024.5.27.21.02.43
// Learn 2 talk backwards.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add scrubbable audio.
  - [] Would it be possible to re-add microphone access via browser
       without a page refresh?
  - [] Drag line to scrub recording head and add gestural recording and
       playback. (Easter egg?)
    - [] Pitch loop 89 (Ableton Plugin)
    - [] Borderlands iPad App
  - [] Make the microphone access / audio context work better?
       (Seems to work fine.)
  + Next version
  - [] Add layering option so multiple sounds can be layered and removed,
       stacked on top of one another, etc. 
  + Done
  - [x] Push and Hold -> Press and Hold 
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
function boot({ ui, screen, sound: { microphone } }) {
  // Runs once at the start.
  btn = new ui.TextButton(`Start`, { center: "xy", screen });

  mic = microphone;
  if (mic.connected) {
    connected = true;
    hideButton = true;
  }
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
  wipe(
    capturing
      ? "maroon"
      : playing
      ? "yellow"
      : connecting
      ? "gray"
      : connected
      ? "pink"
      : btn.down
      ? [0, 64, 64]
      : "teal",
  );

  // Microphone Waveform & Amplitude Line
  if (!playing && !connecting) {
    // Graph microphone (1 channel)
    if (mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
      paintSound(api, mic.amplitude, mic.waveform, 0, 0, width, height);
    }
  } else if (!connecting) {
    paintSound(
      api,
      num.arrMax(sampleData),
      num.arrCompress(sampleData, 32),
      0,
      0,
      width,
      height,
      [0, 0, 255, 32],
    );

    if (typeof progress === "number") {
      const p = (1 - progress) * width;
      ink().line(p, 0, p, height);
    }
  }

  function instructions(gap, color, line1, line2) {
    const hh = height / 2;
    const hq = gap;
    const yo = 9;

    const noshake = btn?.down && color !== "yellow";

    pan(noshake ? 0 : choose(-1, 0, 1), noshake ? 0 : choose(-1, 0, 1));
    ink(color).write(line1, { center: "x", y: hh - yo - hq, size: 2 });
    unpan();
    pan(noshake ? 0 : choose(-1, 0, 1), noshake ? 0 : choose(-1, 0, 1));
    ink(color).write(line2, { center: "x", y: hh - yo + hq, size: 2 });
    unpan();
  }

  if (capturing) instructions(36, "yellow", "SAY", "SOMETHING");

  if (!capturing && connected && !playing)
    instructions(14, "red", "HOLD TO", "RECORD");

  if (!capturing && !playing && !connecting && !connected)
    instructions(36, btn.down ? "teal" : "cyan", "LEARN TO TALK", "BACKWARDS");

  if (!hideButton) {
    const lowTeal = [0, 158, 158];
    const startScheme = [
      [lowTeal, "cyan", "cyan", lowTeal],
      [64, lowTeal, lowTeal, 64],
    ];

    const pressAndHoldScheme = [
      ["red", "yellow", "yellow", "red"],
      ["yellow", "red", "red", "yellow"],
    ];

    let scheme = [];
    if (btn.txt === "Start") scheme = startScheme;
    if (btn.txt === "Repeat") scheme = pressAndHoldScheme;
    if (btn.txt !== "Capture") btn.paint({ ink }, ...scheme);
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
  delay,
}) {
  if (e.is("reframed")) {
    btn.reposition({ center: "xy", screen });
  }

  if (e.is("touch") && !connected && !connecting) {
    btn.down = true;
    synth({
      tone: 400,
      beats: 0.1,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
  }

  if (e.is("lift") && btn.down && !connected && !connecting) {
    synth({
      tone: 600,
      beats: 0.1,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
    btn.disabled = true;
    btn.reposition({ center: "xy", screen }, "Activating Microphone");
    microphone.connect();
    connecting = true;
    btn.down = false;
  }

  btn.act(e); // Pass user events through to the button.

  // Start a microphone recording.
  if (e.is("touch") && !capturing && connected && !playing) {
    sample?.kill(); // Stop any existing sample.
    microphone.rec(); // Start recording.
    btn.down = true;
    capturing = true;
    playing = false;
    progress = null;
  }

  if (e.is("touch") && playing) {
    btn.down = true;
  }

  // Stop playback.
  if (e.is("lift") && playing) {
    sample?.kill(); // Stop any existing sample.
    playing = false;
    progress = null;
    btn.reposition({ center: "xy", screen }, "Capture");
  }

  if (e.is("microphone-connect:success")) {
    // TODO: I need my own version of `setTimeout` called `delay` that instantiates an Hourglass Timer in `disk`. 23.09.07.16.26
    // setTimeout(() => {
    delay(() => {
      connecting = false;
      connected = true;
      btn.disabled = false;
      hideButton = false;
      btn.reposition({ center: "xy", screen }, "Capture");
    }, 60);
  }

  if (e.is("lift") && capturing) {
    btn.down = false;
    const { id, data } = await microphone.cut(); // End recording and get the sample.
    sampleData = data;
    sample = play(id, { reverse: true, loop: true });
    capturing = false;
    playing = true;

    // TODO: Queue / trigger video to record...

    hideButton = false;
    btn.reposition({ center: "xy", screen }, "Repeat");
  }
}

// 📰 Meta
function meta() {
  return {
    title: "BakTok",
    desc: "Learn to talk backwards.",
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
  // if (amplitude && capturing) {
  //   ink(!capturing ? [255, 255, 0] : color || [255, 128]).box(
  //     x + width / 2,
  //     yMid,
  //     width,
  //     amplitude * yMax * 2,
  //     "*center",
  //   );
  // }

  ink(capturing ? "yellow" : connected ? [255, 0, 0] : color).poly(
    waveform.map((v, i) => {
      return [x + i * xStep, yMid + v * yMax];
    }),
  );
}
