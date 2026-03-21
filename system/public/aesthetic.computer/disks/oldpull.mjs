// Oldpull, 2022.01.17.02.44
// A simple painting tool / game that records rectangular selections and plays
// them back.

// TODO
// * Encode turns on the bottom.
// * Store and play back turns / run backwards and forwards through them?
// * Add modes for playback / preload.

const { ceil } = Math;

// Overall sequence mode.
let state = "rest";
let sketch;

// Dragging & making a selection.
let selection, selectionBuffer;

// This should be a simple switchbox that can reset?
let boxIsBlinking = false,
  boxBlink = false,
  blinkCount = 0;

// History
let history,
  turn,
  turns = [],
  plottedTurns = 0;

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ resolution, painting, screen, wipe, ink, glaze }) {
  resolution(32, 32 + 1);

  const palette = {
    light: {
      grey: 80,
      blue: [100, 100, 150],
    },
    dark: {
      grey: 40,
      blue: [40, 40, 60],
    },
  };

  // Make starting image a dark background with a light square in the center.
  const w = screen.width;
  const h = screen.height - 1;

  sketch = painting(w, h, ({ wipe }) => {
    const hw = ceil(w / 2);
    wipe(palette.dark)
      .ink(palette.light)
      .box(hw, ceil(h / 2), hw, "fill*center");
  });

  history = painting(screen.width, 1, () => wipe(0));

  // glaze("cloud", { color: "red" });
  // glaze({ on: true });
}

// 🧮 Simulate (Runs once per logic frame (120fps)).
// User flow moves between states: rest ~> selecting ~> selected ~> placing
//                                 1       2            3           4
// TODO: Start with states here as the highest condition?
export function sim() {}

// 🎨 Paint (Runs once per display refresh rate)
export function paint({
  ink,
  copy,
  plot,
  paste,
  clear,
  line,
  box,
  painting,
  screen,
  page,
  pen,
  paintCount,
}) {
  // 1. Caching
  // Always render the first frame, and then only on pen change,
  // or if actively dragging a selection.
  if (paintCount !== 0 && pen.changed === false && boxIsBlinking === false) {
    return false;
  }

  // 2. Background
  paste(sketch);

  // 2. Selection box
  if (selection) {
    if (state === "selecting") ink(255, 0, 0, 128);
    else if (state === "selected") ink(200, 0, 0, 128);

    if (state === "placing") {
      if (blinkCount % 60 === 0) {
        boxBlink = !boxBlink;
      }
      blinkCount += 1;
      ink(200, 0, 0, boxBlink ? 64 : 0);
    }

    box(selection.x, selection.y, selection.w, selection.h, "outline");
  }

  // 3. Create selection buffer if needed.
  // TODO: This should move to another function or be created in sim.
  if (
    state === "selected" &&
    !selectionBuffer &&
    selection.w > 0 &&
    selection.h > 0
  ) {
    selectionBuffer = painting(selection.w, selection.h, ({ copy }) => {
      // Copy the screen rectangle into our selection buffer.
      for (let x = 0; x < selection.w; x += 1) {
        for (let y = 0; y < selection.h; y += 1) {
          copy(x, y, selection.x + x, selection.y + y, sketch);
        }
      }
    });
    console.log("Captured selection:", selectionBuffer);
  }

  // 4. Render selection buffer.
  if (selectionBuffer) paste(selectionBuffer, selection.x, selection.y);

  // 5. Paste selection buffer.
  if (state === "rest" && selectionBuffer) {
    // Switch to surfaceBuffer.
    page(sketch);
    paste(selectionBuffer, selection.x, selection.y);
    // Copy selectionBuffer to surfaceBuffer.
    selectionBuffer = undefined;
    selection = undefined;

    // Switch back to screen buffer.
    page(screen);

    // Repaint screen with surfaceBuffer.
    paste(sketch);
  }

  // 6. Draw every turn, and plot the last if needed.
  if (plottedTurns < turns.length) {
    page(history);
    const turnToPlot = turns[plottedTurns];

    ink(turnToPlot[0], turnToPlot[1], turnToPlot[2]);
    plot(plottedTurns * 2, 0);

    // 4 and 5 are ending coordinates and can be signed, so we will add 127 to
    // them and when reading back, treat 127 as 0. This should work for a
    // resolution of up to 128?

    ink(turnToPlot[3], turnToPlot[4] + 127, turnToPlot[5] + 127);
    plot(plottedTurns * 2 + 1, 0);

    page(screen);

    plottedTurns += 1;
  }

  paste(history, 0, screen.height - 1);
}

// ✒ Act (Runs once per user interaction)
export function act({ screen, cursor, geo, event: e }) {
  // Start drag.
  if (e.is("touch")) {
    if (state === "rest") {
      state = "selecting";
      cursor("none");
      select(geo, { x: e.x, y: e.y, w: 1, h: 1 }, sketch);
    }
    if (state === "selected") state = "placing";
  }

  // Continue drag.
  if (e.is("draw")) {
    if (state === "selecting") {
      select(geo, e.drag, sketch);
    } else if (state === "placing") {
      selection.move(e.delta);
      cursor("tiny");
    }
  }

  // End drag.
  if (e.is("lift")) {
    if (state === "selecting" && selection) {
      state = "selected";
      blinkCount = 0;
      boxBlink = false;
      boxIsBlinking = true;
      cursor("tiny");
    } else if (state === "placing") {
      // Get finished turn data.
      turn.push(selection.x, selection.y);
      // Only add turn if we actually moved.
      if (turn[0] !== turn[4] || turn[1] !== turn[5]) {
        // And the destination is inside of the screen.
        const dx = turn[4];
        const dy = turn[5];
        const dw = turn[2];
        const dh = turn[3];

        const sx = 0;
        const sy = 0;
        const sw = screen.width;
        const sh = screen.height - 1;

        if ((dx + dw <= 0 || dx >= sw || dy + dh <= 0 || dy >= sh) === false) {
          // And we have enough pixels.
          if (turns.length < history.width / 2) {
            turns.push(turn); // x, y, w, h, endx, endy
          } else {
            console.log("Turn buffer is full!");
          }
        }
      }
      boxIsBlinking = false;
      state = "rest";
      cursor("precise");
    }
  }
}

// 💗 Beat (Runs once per bpm)
export function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library (Useful functions used throughout the program)

// This runs every time a selection needs to be made or updated.
function select(geo, { x, y, w, h }, { width, height }) {
  selection = new geo.Box(x, y, w, h).abs.crop(0, 0, width, height);
  const s = selection;
  turn = [s.x, s.y, s.w, s.h];
}