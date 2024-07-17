// Bleep, 22.07.15.19.21
// A configurable interface of colored boxes that can be pushed to make tones.

/* #region 📚 README 
#endregion */

/* #region 🏁 todo
  - [🟡] Fix any button tap regressions. (Test rollover logic etc with a 2x1)
  - [] Add multiple wave-types and adjustable tones to the buttons.
  - [] Add the ability to record, play back, and download audio.
  - [] Add automatic qwerty key-mapping.
      (Only on a desktop and if a key is pressed.)
#endregion */

const debug = false;

const { floor } = Math;

const minFreq = 100;
const maxFreq = 1000;
const bleeps = [];
let bg = 0;

let grid, gridWidth, gridHeight;

class Bleep {
  button;
  needsBleep = false;
  tone;
  color;
  sound;

  constructor({ ui: { Button }, num: { randIntRange } }, geometry, bespoke) {
    if (bespoke?.tone) {
      this.tone = bespoke?.tone;
    } else {
      this.tone = randIntRange(minFreq, maxFreq);
    }

    this.color = bespoke?.color;
    this.button = new Button(...geometry);
  }

  paint({ ink, num: { map, shiftRGB } }) {
    let presentedColor;

    if (this.color) {
      presentedColor = this.button.down
        ? shiftRGB(this.color, [0, 0, 0], 0.5)
        : this.color;
    } else {
      const color = map(this.tone, minFreq, maxFreq, 0, 200);
      presentedColor = this.button.down ? 25 + color : 55 + color;
    }

    ink(presentedColor).box(this.button.box);
    ink(bg).box(this.button.box, "in"); // Outline
    // TODO: Add a text label or color things based on a tone value?
  }

  beep({ sound: { synth } }) {
    if (!this.needsBleep) return;
    this.needsBleep = false;
    this.sound = synth({
      tone: this.tone,
      duration: "🔁",
      decay: 0.99,
    });
  }
}

// Construct bleeps randomly or from a bespoke array of tones and colors.
function buildBleeps($, bespokes) {
  const {
    geo: { Grid, Box },
    screen,
  } = $;

  const gridRatio = gridHeight / gridWidth;
  const screenRatio = screen.height / screen.width;
  const margin = 8;

  if (debug)
    console.log("🐛", "gridRatio:", gridRatio, "screenRatio:", screenRatio);

  let x, y, scale;

  // 💁 Size the grid to be as tall as the screen.height.
  function fitToHeight() {
    const height = screen.height - margin * 2;
    scale = floor(height / gridHeight);
    y = floor((screen.height - scale * gridHeight) / 2);
    x = floor(screen.width / 2 - (scale * gridWidth) / 2);
  }

  // 💁 Size the grid to be as wide as the screen.width;
  function fitToWidth() {
    const width = screen.width - margin * 2;
    scale = floor(width / gridWidth);
    y = floor(screen.height / 2 - (scale * gridHeight) / 2);
    x = floor((screen.width - scale * gridWidth) / 2);
  }

  if (gridRatio > 1) {
    // Tall
    if (gridRatio > screenRatio) {
      fitToHeight();
    } else {
      fitToWidth();
    }
  } else if (gridRatio < 1) {
    // Wide
    if (gridRatio > screenRatio) {
      fitToHeight();
    } else {
      fitToWidth();
    }
  } else {
    // Square
    screenRatio > 1 ? fitToWidth() : fitToHeight();
  }

  grid = new Grid(x, y, gridWidth, gridHeight, scale);

  if (bleeps.length > 0) {
    grid.each((x, y, i) => {
      bleeps[i].button.box = new Box(...grid.get(x, y), grid.scale);
    });
  } else {
    grid.each((x, y, i) => {
      bleeps.push(new Bleep($, [...grid.get(x, y), grid.scale], bespokes?.[i]));
    });
  }
}

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  const { params, num, hud } = $;

  (gridWidth = num.randIntRange(1, 6)), (gridHeight = num.randIntRange(1, 6));

  if (params.length === 1) {
    if (params[0] === "phand" || params[0] === "wild") {
      gridWidth = 4;
      gridHeight = 1;
      // 🧚 `peter-hand` specific tones... 23.03.09.17.38 (for HOOK screening)
      // Bangarang frequencies
      // E4 - 329.63 Hz
      // G4 - 392.00 Hz
      // B4 - 493.88 Hz
      // D5 - 587.33 Hz
      buildBleeps($, [
        { tone: 329.63, color: [200, 200, 0] },
        { tone: 392, color: [100, 100, 255] },
        { tone: 493.88, color: [200, 40, 20] },
        { tone: 587.33, color: [0, 140, 0] },
      ]);
      bg = [32, 16, 32];
      // hud.label();
    } else {
      // Normal tone matrix generation.
      const split = params[0].split("x");
      gridWidth = parseInt(split[0]) || gridWidth;
      gridHeight = parseInt(split[1]) || gridHeight;
      buildBleeps($);
    }
  } else {
    buildBleeps($);
  }
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  const { wipe, screen, ink } = $;
  wipe(bg); // Draw a black background
  bleeps.forEach((bleep) => bleep.paint($)); // Draw every bleeper.
  // ink(255, 0, 0).grid(grid); // Paint grid overlay, for debugging purposes.
  return false; // Draw only once until `needsPaint` is called..
}

function sim($) {
  bleeps.forEach((bleep) => bleep.beep($));
}

let anyBleepDowned = false;

// ✒ Act (Runs once per user interaction)
function act($) {
  const { event, needsPaint, cursor, pens } = $;

  // Disable the cursor for touch input, and re-enable it if a mouse is used.
  event.device === "touch" ? cursor("none") : cursor("precise");

  if (event.is("reframed")) buildBleeps($);

  bleeps.forEach((bleep) => {
    bleep.button.act(
      event,
      {
        push: () => {
          anyBleepDowned = false;
          needsPaint();
          bleep.sound?.kill(0.1);
        },
        down: () => {
          anyBleepDowned = true;
          bleep.needsBleep = true;
          needsPaint();
        },
        rollover: () => {
          if (!anyBleepDowned) return;
          bleep.button.down = true;
          bleep.needsBleep = true;
          needsPaint();
        },
        rollout: () => {
          bleep.button.down = false;
          bleep.sound?.kill(0.1);
          needsPaint();
        },
        cancel: () => {
          anyBleepDowned = false;
          bleep.sound?.kill(0.1);
          needsPaint();
        },
      },
      pens?.(),
    ); // Passing pens here enables multi-touch support for ui buttons.
  });
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
// function beat($api) {
// }

// 🏃 Leave (Runs once while the piece is being exited)
function leave($api) {
  // TODO: reset BPM to default on leave
  // $api.sound.bpm(defaultBPM)
}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, paint, sim, act, leave };
