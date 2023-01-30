// Bleep, 22.07.15.19.21
// A configurable interface of colored boxes that can be pushed to make tones.

const debug = false;

const { floor } = Math;

const minFreq = 100;
const maxFreq = 1000;
const bleeps = [];

let grid, gridWidth, gridHeight;

class Bleep {
  button;
  needsBleep = false;
  tone;

  constructor({ ui: { Button }, screen, num: { randIntRange } }, geometry) {
    this.tone = randIntRange(minFreq, maxFreq);
    this.button = new Button(...geometry);
  }

  paint({ ink, num: { map } }) {
    const color = map(this.tone, minFreq, maxFreq, 0, 200);
    ink(this.button.down ? 25 + color : 55 + color).box(this.button.box);
    ink(0).box(this.button.box, "in"); // Outline
    // TODO: Add a text label or color things based on a tone value?
  }

  beep({ sound: { square } }) {
    if (!this.needsBleep) return;
    this.needsBleep = false;

    square({
      tone: this.tone,
      beats: 30,
      decay: 0.99,
    });
  }
}

function buildBleeps($) {
  const {
    geo: { Grid, Box },
    screen,
    num,
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
      bleeps.push(new Bleep($, [...grid.get(x, y), grid.scale]));
    });
  }
}

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  const { params, num } = $;

  // TODO: Density does not change `screen` right away... 22.08.29.22.13
  // $.density(2);

  (gridWidth = num.randIntRange(1, 6)), (gridHeight = num.randIntRange(1, 6));

  if (params.length === 1) {
    const split = params[0].split("x");
    gridWidth = parseInt(split[0]) || gridWidth;
    gridHeight = parseInt(split[1]) || gridHeight;
  }

  buildBleeps($);
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {}

// 🎨 Paint (Executes every display frame)
function paint($) {
  const { wipe, screen, ink } = $;
  wipe(0); // Draw a black background
  bleeps.forEach((bleep) => bleep.paint($)); // Draw every bleeper.
  // ink(255, 0, 0).grid(grid); // Paint grid overlay, for debugging purposes.
  return false; // Draw only once until `needsPaint` is called..
}

let anyBleepDowned = false;

// ✒ Act (Runs once per user interaction)
function act($) {
  const {
    event,
    needsPaint,
    cursor,
    pens
  } = $;

  // Disable the cursor for touch input, and re-enable it if a mouse is used.
  event.device === "touch" ? cursor("none") : cursor("precise");

  if (event.is("reframed")) {
    buildBleeps($);
    // Loop over bleeps and reposition them.
    for (let x = 0; x < grid.box.w; x += 1) {
      for (let y = 0; y < grid.box.h; y += 1) {
        const i = x + y * grid.box.w;
      }
    }
  }

  bleeps.forEach((bleep) => {
    bleep.button.act(event, {
      push: () => needsPaint(),
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
        needsPaint();
      },
      cancel: () => {
        anyBleepDowned = false;
        needsPaint();
      },
    }, pens?.()); // Passing pens here enables multi-touch support for ui buttons.
  });
}

let beatCount = 0n;

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  if (beatCount === 0n) {
    $api.sound.bpm(3600); // Set bpm to 3600 ~ 60fps
  }
  bleeps.forEach((bleep) => bleep.beep($api));
  beatCount += 1n;
}

// 🏃 Leave (Runs once while the piece is being exited)
function leave($api) {}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat, leave };
