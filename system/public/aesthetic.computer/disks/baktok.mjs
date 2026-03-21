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
  flipBtn,
  sample,
  sampleData,
  sampleId,
  compressedWaveform, // Cached compressed waveform for faster rendering
  connected = false,
  connecting = false,
  capturing = false,
  playing = false,
  progress,
  reversed = true; // Start reversed (backwards) by default for BakTok

let hideButton = false,
  hideButtonTimeout;

const { max } = Math;

// 🥾 Boot
function boot({ ui, screen, sound: { microphone } }) {
  // Runs once at the start.
  btn = new ui.TextButton(`Start`, { center: "xy", screen });
  flipBtn = new ui.TextButton(`< Backwards`, { bottom: 8, right: 8, screen });
  // Prevent button conflict when dragging between buttons
  btn.btn.stickyScrubbing = true;
  flipBtn.btn.stickyScrubbing = true;

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
    if (compressedWaveform) {
      paintSound(
        api,
        1, // Use full amplitude for display
        compressedWaveform,
        0,
        0,
        width,
        height,
        [0, 0, 255, 32],
      );
    }

    if (typeof progress === "number") {
      // Backwards: progress goes 1→0, so bar at progress*width moves right→left
      // Forwards: progress goes 0→1, so bar at progress*width moves left→right
      const p = progress * width;
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

  // Show Flip button during playback (duotone theme: red/yellow)
  if (playing) {
    const flipScheme = [
      ["red", "yellow", "yellow", "red"],
      ["yellow", "red", "red", "yellow"],
    ];
    flipBtn.paint({ ink }, ...flipScheme);
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
  num,
  rec,
  delay,
}) {
  if (e.is("reframed")) {
    btn.reposition({ center: "xy", screen });
    flipBtn.reposition({ bottom: 8, right: 8, screen });
  }

  // Handle flip button during playback (check first to prevent fall-through)
  if (playing && flipBtn?.btn?.box?.contains(e)) {
    flipBtn.btn.act(e, {
      push: () => {
        reversed = !reversed;
        // Update button text to show current direction with arrow
        flipBtn.reposition({ bottom: 8, right: 8, screen }, reversed ? "< Backwards" : "Forwards >");
        // Flip direction seamlessly without restarting, keeping current position
        sample?.update({ sampleSpeed: reversed ? -1 : 1 });
        synth({
          tone: reversed ? 300 : 500,
          beats: 0.1,
          attack: 0.01,
          decay: 0.3,
          volume: 0.1,
        });
      },
    });
    return; // Don't process other events when on flip button
  }

  // Handle main button based on state
  if (!connected && !connecting) {
    // Start button - activates microphone
    btn.btn.act(e, {
      down: () => {
        synth({
          tone: 400,
          beats: 0.1,
          attack: 0.01,
          decay: 0.5,
          volume: 0.15,
        });
      },
      push: () => {
        synth({
          tone: 600,
          beats: 0.1,
          attack: 0.01,
          decay: 0.5,
          volume: 0.15,
        });
        btn.btn.disabled = true;
        btn.reposition({ center: "xy", screen }, "Activating Microphone");
        microphone.connect();
        connecting = true;
      },
    });
  } else if (connected && !capturing && !playing) {
    // Capture mode - full screen touch to record (hold to record)
    if (e.is("touch")) {
      sample?.kill(); // Stop any existing sample.
      microphone.rec(); // Start recording.
      btn.btn.down = true; // Set down state for visual feedback
      capturing = true;
      playing = false;
      progress = null;
    }
  } else if (capturing) {
    // Release to stop recording and play back
    if (e.is("lift")) {
      btn.btn.down = false; // Clear down state
      const { id, data } = await microphone.cut(); // End recording and get the sample.
      sampleData = data;
      sampleId = id;
      // Cache compressed waveform for faster rendering (compress once)
      compressedWaveform = num.arrCompress(sampleData, 64);
      reversed = true; // Reset to backwards when starting new recording
      flipBtn.reposition({ bottom: 8, right: 8, screen }, "< Backwards"); // Reset flip button text
      sample = play(sampleId, { speed: -1, loop: true, volume: 1 });
      capturing = false;
      playing = true;
      hideButton = false;
      btn.reposition({ center: "xy", screen }, "Repeat");
    }
  } else if (playing) {
    // Full screen touch to stop playback (except flip button area)
    if (e.is("touch")) {
      sample?.kill(); // Stop any existing sample.
      playing = false;
      progress = null;
      btn.reposition({ center: "xy", screen }, "Capture");
    }
  }

  if (e.is("microphone-connect:success")) {
    delay(() => {
      connecting = false;
      connected = true;
      btn.btn.disabled = false;
      hideButton = false;
      btn.reposition({ center: "xy", screen }, "Capture");
    }, 60);
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
