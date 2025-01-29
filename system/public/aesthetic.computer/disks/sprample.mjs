// Sprample, 2025.1.28.04.11.21.170
// Spread a sample across some pats.

/* 📝 Notes
  - [🟠] Show a tiny waveform in the record button for live feedback,
         and show the state of the recording process...
  - [] Add pitch shifting to the board by swiping left or right,
       or could just divide each strip into 3 pitches: lo, mid, and hi.

  + Done
  - [x] Record a sample and spread it automatically.
  - [x] Draw the waveform over the buttons.
  - [x] Spread out a predefined sample.
    - [x] Add start and end trimming to sound.play();
 */

let sfx,
  btns = [],
  pats = 3,
  anyDown = false;

const menuHeight = 32;
const labelHeight = 24;

const keyToIndexMap = {
  1: 0,
  2: 1,
  3: 2,
  4: 3,
  5: 4,
  6: 5,
  7: 6,
  8: 7,
  9: 8,
  0: 9,
};

const indexToKeyMap = Object.fromEntries(
  Object.entries(keyToIndexMap).map(([key, index]) => [index, Number(key)]),
);

const sounds = [],
  progressions = [];

let mic,
  micRecordButton,
  micConnected = false;

let sampleId, sampleData;

async function boot({
  net: { preload },
  sound: { microphone, getSampleData },
  play,
  ui,
  params,
  screen,
}) {
  const name = params[0] || "startup";
  sampleId = await preload(name);

  console.log("Sample loaded:", sampleId);

  genPats({ screen, ui });

  micRecordButton = new ui.Button(0, screen.height - 32, 32, 32);

  mic = microphone; // Microphone access.
  if (mic.connected) {
    connected = true;
    hideButton = true;
  }

  getSampleData(sampleId).then((data) => {
    sampleData = data;
    // console.log("🔴 Sample Data:", sampleData);
  });
}

function sim() {
  sounds.forEach((sound, index) => {
    sound?.progress().then((p) => (progressions[index] = p.progress)); // Get progress data.
  });
}

function paint({ api, wipe, ink, screen, num }) {
  wipe(0, 0, 255);
  btns.forEach((btn, index) => {
    btn.paint(() => {
      ink(btn.down ? "white" : "cyan").box(btn.box); // Paint box a teal color.
      ink("black").box(btn.box, "out"); // Outline in black.
      if (progressions[index]) {
        // console.log("Progress:", progressions[index]);
        ink("red", 64).box(
          btn.box.x,
          btn.box.y + btn.box.h,
          btn.box.w,
          -btn.box.h * progressions[index],
        );
      }
      ink("black").write(
        indexToKeyMap[btns.length - 1 - index],
        btn.box.x + 4,
        btn.box.y + 4,
      );
    });

    micRecordButton.paint((btn) => {
      ink(btn.down ? "white" : "red").box(btn.box);
      ink(btn.down ? "red" : "white").box(btn.box, "inline");
      ink(btn.down ? "red" : "white").write(
        "Rec",
        btn.box.x + 8,
        btn.box.y + 10,
      );
    });
  });

  ink("white").write(pats, { right: 6, top: 6 });

  // Paint waveForm...

  // Microphone Waveform & Amplitude Line
  // if (/*!playing && !connecting*/) {
  //   // Graph microphone (1 channel)
  //   if (mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
  //     paintSound(api, mic.amplitude, mic.waveform, 0, 0, width, height);
  //   }

  if (sampleData) {
    paintSound(
      api,
      num.arrMax(sampleData),
      num.arrCompress(sampleData, 256), // 🔴 TODO: This could be made much faster.
      0,
      labelHeight,
      screen.width,
      screen.height - menuHeight - labelHeight,
      [0, 0, 255, 32],
      { direction: "bottom-to-top" },
    );
  }
}

function act({ event: e, sound, pens, screen, ui }) {
  const sliceLength = 1 / btns.length; // Divide the total duration (1.0) by the number of buttons.

  btns.forEach((btn, index) => {
    const from = (btns.length - 1 - index) * sliceLength;
    const to = from + sliceLength;
    btn.act(
      e,
      {
        down: (btn) => {
          // if (downs[note]) return false; // Cancel the down if the key is held.
          anyDown = true;
          sounds[index] = sound.play(sampleId, { from, to });
        },
        over: (btn) => {
          if (btn.up && anyDown) {
            btn.up = false;
            btn.actions.down(btn);
          }
        },
        out: (btn) => {
          btn.down = false;
          btn.actions.up(btn);
        },
        up: (btn) => {
          // if (downs[note]) return false;
        },
      },
      pens?.(),
    );
  });

  micRecordButton?.act(e, {
    down: () => {
      if (!sound.microphone.connected) {
        sound.microphone.connect();
      } else {
        console.log("Start a recording...");
        sound.microphone.rec(); // Start recording.
      }

      // connecting = true;
    },
    up: async (btn) => {
      // ...

      // btn.down = false;

      if (sound.microphone.recording) {
        console.log("Recording... cutting!");
        const { id, data } = await sound.microphone.cut(); // End recording and get the sample.
        sampleData = data;
        sampleId = id;
      }

      // console.log(sound.microphone);

      // sample = play(id, { reverse: true, loop: true });
      // capturing = false;
      // playing = true;

      // TODO: Queue / trigger video to record...

      // hideButton = false;
      // btn.reposition({ center: "xy", screen }, "Repeat");
      //}
    },
  });

  if (e.is("microphone-connect:success")) {
    // TODO: I need my own version of `setTimeout` called `delay` that instantiates an Hourglass Timer in `disk`. 23.09.07.16.26
    console.log("🎤 CONNECTED!");
    // connected = true;
    // setTimeout(() => {
    // delay(() => {
    //   connecting = false;
    //   connected = true;
    //   btn.disabled = false;
    //   hideButton = false;
    //   btn.reposition({ center: "xy", screen }, "Capture");
    // }, 60);
  }

  if (e.is("keyboard:down") || e.is("keyboard:up")) {
    const index = keyToIndexMap[e.key];
    if (index !== undefined && btns[btns.length - 1 - index]) {
      // Ensure index is valid and button exists
      const btn = btns[btns.length - 1 - index];
      if (e.is("keyboard:down") && !btn.down) {
        // console.log(`${e.key} key pressed!`);
        btn.down = true;
        btn.actions.down(btn);
      } else if (e.is("keyboard:up")) {
        btn.down = false;
        btn.actions.up(btn);
      }
    }
  }

  if (e.is("reframed")) {
    genPats({ screen, ui });
    // micRecordButton.reposition()
    micRecordButton.box.y = screen.height - 32; // = new ui.Button(0, screen.height - 32, 32, 32);
  }
}

export { boot, paint, act, sim };

// 📚 Library

// Generate sectional strips of buttons to split the sample by.
function genPats({ screen, ui }) {
  btns.length = 0;
  for (let i = 0; i < pats; i += 1) {
    const strip = (screen.height - menuHeight - labelHeight) / pats,
      x = 0,
      y = labelHeight + strip * i,
      width = screen.width,
      height = strip;
    btns.push(new ui.Button(x, y, width, height));
  }
}

// Paints a waveform with a bounding box based on amplitude.
function paintSound(
  { ink },
  amplitude,
  waveform,
  x,
  y,
  width,
  height,
  color,
  options,
) {
  // console.log(amplitude, waveform.length);

  const direction = options?.direction || "left-to-right";

  if (direction === "left-to-right") {
    const xStep = width / (waveform.length - 1);

    const yMid = y + height / 2,
      yMax = height / 2;

    ink("yellow", 128).poly(
      waveform.map((v, i) => {
        const p = [x + i * xStep, yMid + v * yMax];
        return p;
      }),
    );
  } else if (direction === "bottom-to-top") {
    const yStep = height / (waveform.length - 1);
    const xMid = x + width / 2,
      xMax = width / 2;

    ink("blue", 128).poly(
      waveform.map((v, i) => {
        const p = [xMid + v * xMax, y + height - i * yStep];
        return p;
      }),
    );
  } else {
    console.warn("🌊 Unsupported direction.");
  }
}
