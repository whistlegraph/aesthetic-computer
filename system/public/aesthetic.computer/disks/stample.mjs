// Stample, 2025.1.28.04.11.21.170
// Spread a sample across some pats.

/* 📝 Notes
  - [🚗] Show the state of the microphone connection, recording and disconnect
         process.
  - [] Add pitch shifting to the board by swiping left or right,
       or could just divide each strip into 3 pitches: lo, mid, and hi.
  - [] Add visual printing / stamping of pixel data and loading of that
       data.
  + Done
  - [x] Show a tiny waveform in the record button for live feedback.
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

const BUTTON_LABEL_CONNECTING = "Wait...";

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
  micRecordButtonLabel = "Connect",
  micConnected = false;

let sampleId, sampleData;

async function boot({
  net: { preload },
  sound: { microphone, getSampleData },
  play,
  ui,
  params,
  screen,
  delay
}) {
  // const name = params[0] || "startup";
  const name = "startup"; // TODO: Recall previous samples from `store`.
  if (params[0]) pats = parseInt(params[0]);
  sampleId = await preload(name);
  genPats({ screen, ui });
  micRecordButton = new ui.Button(0, screen.height - 32, 64, 32);
  mic = microphone; // Microphone access.


  if (mic.permission === "granted") {
    micRecordButtonLabel = BUTTON_LABEL_CONNECTING;
    delay(() => {
      microphone.connect();
    }, 15)
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
  mic?.poll(); // Query for updated amplitude and waveform data.
}

function paint({ api, wipe, ink, screen, num, text }) {
  wipe(0, 0, 255);
  btns.forEach((btn, index) => {
    btn.paint(() => {
      ink(btn.down ? "white" : "cyan").box(btn.box); // Paint box a teal color.
      ink("black").box(btn.box, "out"); // Outline in black.
      if (progressions[index]) {
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
      const color = mic.connected ? "red" : "orange";
      //if (mic.connected)

      ink(btn.down ? "white" : color).box(btn.box);
      ink(btn.down ? color : "white").box(btn.box, "inline");

      ink(btn.down ? color : "white").write(
        micRecordButtonLabel,
        btn.box.x + btn.box.w / 2 - text.width(micRecordButtonLabel) / 2,
        btn.box.y + btn.box.h / 2 - text.height(micRecordButtonLabel) / 2,
      );

      // Graph microphone (1 channel)
      if (mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
        paintSound(
          api,
          mic.amplitude,
          mic.waveform,
          btn.box.x,
          btn.box.y,
          btn.box.w - 1,
          btn.box.h,
        );
      }
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

function act({ event: e, sound, pens, screen, ui, notice }) {
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
        micRecordButtonLabel = BUTTON_LABEL_CONNECTING;
      } else {
        sound.microphone.rec(); // Start recording.
      }
    },
    up: async (btn) => {
      if (sound.microphone.recording) {
        const { id, data } = await sound.microphone.cut(); // Get the sample.
        sampleData = data;
        sampleId = id;
      }
    },
  });

  if (e.is("microphone-connect:failure")) {
    console.log("🎤 Failed mic connection:", e);
    if (e.reason) notice(e.reason.toUpperCase(), ["yellow", "red"]);
    micRecordButtonLabel = "Connect";
  }

  if (e.is("microphone-connect:success")) {
    console.log("🎤 Connected.");
    micRecordButtonLabel = "Record";
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

  if (e.is("keyboard:down:arrowdown")) {
    pats -= 1;
    if (pats < 0) pats = 0;
    genPats({ screen, ui });
  }

  if (e.is("keyboard:down:arrowup")) {
    pats += 1;
    genPats({ screen, ui });
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
