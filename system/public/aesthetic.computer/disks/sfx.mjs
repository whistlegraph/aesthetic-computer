// Sfx, 2023.6.09.18.41.12
// Testing loading and playing sound effects and samples.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Fix reverse progress bar.
  - [] Add 'value' drag box.
  + Done
  - [x] Create a sampling recorder See: `stample`).
  - [x] Check decode delay / trigger a whole keyboard of samples.
  - [x] Eventually add information to read back / get streamed back.
  - [x] Add startup sound and keyboard sound to keyboard clicks and prompt!
  - [x] Add these sounds to the remote assets folder.
  - [x] How to load indidivudal sound effects by short name?
  - [x] With a parameter to set the name.
  - [x] Add an interface to play the sample. 
  - [x] Decide an architecture for loading and playing back samples.
#endregion */

// 🥾 Boot
let sfx, btn;

let speedBtn,
  speed = 1,
  fromBtn,
  from = 0,
  toBtn,
  to = 1;

let sfxData;
let playingSample;
let progress;

const { round } = Math;

async function boot({ net: { preload }, play, ui, params, sound }) {
  const name = params[0] || "startup";
  console.log("Sound name:", name);
  sfx = await preload(name);
  btn = new ui.TextButton(`Play "${name}"`);
  speedBtn = new ui.TextButton(speedLabel());
  fromBtn = new ui.TextButton(fromLabel());
  toBtn = new ui.TextButton(toLabel());

  sound.getSampleData(sfx).then((data) => {
    sfxData = data;
    // console.log("🔴 Sample Data:", sfxData);
  });
}

// 🖌️ Paint
function paint({ api, wipe, ink, screen, num }) {
  wipe(0, 0, 255);

  // Visualization

  if (sfxData) {
    paintSound(
      api,
      num.arrMax(sfxData),
      num.arrCompress(sfxData, 256), // 🔴 TODO: This could be made much faster.
      0,
      0,
      screen.width,
      screen.height,
      [255, 255, 255, 32],
      { direction: "left-to-right" },
    );
  }

  const fromX = num.clamp(from * screen.width - 1, 0, screen.width - 1);
  const toX = num.clamp(to * screen.width - 1, 0, screen.width - 1);

  ink("red").line(fromX, 0, fromX, screen.height);
  ink("lime").line(toX, 0, toX, screen.height);

  if (progress) {
    const progressX = num.clamp(fromX + progress * (toX - fromX), fromX, toX);
    ink("yellow").line(progressX, 0, progressX, screen.height);
  }

  // Buttons
  btn.reposition({ center: "xy", screen });
  btn.paint({ ink });

  speedBtn.reposition({ center: "xy", y: 30, screen });
  speedBtn.paint({ ink });

  fromBtn.reposition({ center: "xy", y: 30 + 30, screen });
  fromBtn.paint({ ink });

  toBtn.reposition({ center: "xy", y: 30 + 30 + 30, screen });
  toBtn.paint({ ink });
}

// 🎪 Act
function act({ event: e, sound, num: { clamp } }) {
  btn.act(e, () => {
    playingSample = sound.play(
      sfx,
      { speed, from, to },
      {
        kill: () => {
          // console.log("Killed...");
          // playingSample = null;
        },
      },
    );
  });

  speedBtn.act(e, {
    scrub: () => {
      speed = round((speed + e.delta.x / 100) * 100) / 100;
      speedBtn.txt = speedLabel();
    },
  });

  fromBtn.act(e, {
    scrub: () => {
      from = clamp(round((from + e.delta.x / 100) * 100) / 100, 0, 1);
      fromBtn.txt = fromLabel();
    },
  });

  toBtn.act(e, {
    scrub: () => {
      to = clamp(round((to + e.delta.x / 100) * 100) / 100, 0, 1);
      toBtn.txt = toLabel();
    },
  });
}

function sim() {
  playingSample?.progress().then((p) => {
    progress = p.progress;
    // console.log("📏 Progress:", progress);
  }); // Get progress data.
}

// 📰 Meta
function meta() {
  return {
    title: "Sfx",
    desc: "Testing loading and playing sound effects and samples.",
  };
}

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

const speedLabel = () => `< Speed: ${speed.toFixed(2)} >`;
const fromLabel = () => `< From: ${from.toFixed(2)} >`;
const toLabel = () => `< To: ${to.toFixed(2)} >`;

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
      xMax = width;

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
