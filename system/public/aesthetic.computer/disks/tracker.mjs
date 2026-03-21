// Tracker, 2022.01.16.16.02
// A tool for composing, playing, and following along with 12 tones.
// Designed in collaboration w/ Oliver Laumann + Mija Milovic

// TODO: Recenter boxes on line 174.

// TODO: Rethink & implement these other tools:
//       - %: Add / toggle separator (single bpm notes into held notes)
//       - -(line)-: Background color change / all together.
//       - BPM: Adds a thick line to the grid and puts a new number on the right.

const { max, min } = Math;
import { parse } from "../lib/parse.mjs";

// Data
import { noteList, colors } from "./common/music.mjs";
let scoreData = []; // A 2 dimensional array for storing note info.
const style = {};

// Layout
let notes;
let score;
let scrolling = false;
const buttons = {};
let bar;
let currentTool = 0;
let playButtonPos;
let playIconOffset = [1, 1];

const { entries } = Object;
import { font1 } from "./common/fonts.mjs";

let glyphs = {};

// 🥾 Boot (Runs once before first paint and sim)
function boot({
  resolution,
  screen,
  store,
  geo: { Box, Grid },
  ui: { Button },
  net: { preload },
}) {
  // TODO: Reload `scoreData` and `stopRow` from store["tracker:score"] if it exists.

  let scoreHeight = 1;

  if (store["tracker:score"]) {
    ({ data: scoreData, stopRow: scoreHeight } = store["tracker:score"]);
  }

  resolution(160, 90); // 16x9

  // TODO: Eek! Screen height is not changing after resize...
  // resize(screen.width, screen.height); // 16x9 // TODO: Double resize doesn't work... why is that?

  const scale = 9;
  style.addMinusHeight = scale;

  notes = new Grid(scale * 3, 0, 12, 1, scale);
  score = new Grid(scale * 3, scale, 12, scoreHeight, scale);

  buttons.minus = new Button(0, 0, score.scaled.w / 2, style.addMinusHeight);
  buttons.add = new Button(buttons.minus.box);

  const w = 3 * 3,
    h = 4 * 3;

  buttons.load = new Button(3, screen.height - h - 3, w, h);
  buttons.save = new Button(
    buttons.load.box.x + buttons.load.box.w + 3,
    buttons.load.box.y,
    w,
    h
  );

  const toolCount = 2;
  bar = new Grid(3, 3, 1, toolCount, scale);

  // Add toolbar buttons.
  buttons.tools = {};
  buttons.tools.small = new Button(...bar.get(0, 0), bar.scale);
  buttons.tools.big = new Button(...bar.get(0, 1), bar.scale);

  // Add play button.
  playButtonPos = [screen.width - 14, screen.height - 16];
  buttons.play = new Button(...playButtonPos, 11, 13);

  // Preload all glyphs.
  entries(font1).forEach(([glyph, location]) => {
    preload(`aesthetic.computer/disks/drawings/font_1/${location}.json`).then((res) => {
      glyphs[glyph] = res;
    });
  });

  preload("aesthetic.computer/disks/drawings/arrow-up-3x6 2022.1.18.21.59.35.json").then((r) => {
    buttons.save.icon = r;
    buttons.load.icon = r;
  });
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ wipe, pan, unpan, ink, layer, num: { vec2, odd } }) {
  wipe(10).layer(1); // Make the background black.

  // ✔ 1. Top Row: for knowing what notes each column represents, and
  //               being able to toggle columns.

  // Draw colored boxes according to notes grid, with overlaying letters.
  // TODO: Make this in common with melody.js via common/music.js
  [...noteList].forEach((note, i) => {
    ink(colors.notes[note]).box(...notes.get(i, 0), notes.scale);
  });

  // TODO: `printLine` can be more elegantly implemented.
  ink(20, 40, 60, 225).printLine(
    noteList.toUpperCase(),
    glyphs,
    notes.box.x,
    notes.box.y,
    notes.scale,
    1,
    2
  );

  // ✔ 2. Composition: for placing and removing notes. Scrollable.
  layer(0).ink(255).grid(score); // Paint a grid to hold the score.

  // Paint the score data itself.
  if (odd(score.scale)) pan(1, 1); // Offset centered boxes when bar.scale is odd.
  scoreData.forEach((row, x) => {
    const color = colors.notes[noteList[x]];
    row?.forEach((column, y) => {
      // Render rows in sync with score's visibility.
      if (score.box.h - 1 < y) return;
      if (column === "small") {
        ink(color).box(...score.center(x, y), score.scale / 3, "fill*center");
      } else if (column === "big") {
        ink(color).box(...score.center(x, y), score.scale / 2, "fill*center");
      }
    });
  });
  unpan();

  // ✔ 2a. Scrolling UI
  // TODO: Maybe the mouse cursor should default to a scrolling one when
  //       it is over the background?

  // ✔ 2b. Plus / Minus Rows
  addMinusLayout();
  ink(255, 0, 0, 50).box(buttons.minus.box);
  ink(0, 255, 0, 50).box(buttons.add.box);

  // 2c. Button

  // Load Icon
  ink(255, 128, 0, 50).box(buttons.load.box);
  ink(255, 0, 0, 100).draw(
    buttons.load.icon,
    buttons.load.box.x + 1,
    buttons.load.box.y + 1,
    3,
    0
  );

  // Save Icon
  ink(128, 255, 0, 50).box(buttons.save.box);
  ink(0, 255, 0, 100).draw(
    buttons.save.icon,
    buttons.save.box.x + 1 + 6,
    buttons.save.box.y + 1 + 9,
    3,
    180
  );

  // ✔ 3. Toolbar
  ink(255, 255, 0).grid(bar);

  if (odd(bar.scale)) pan(1, 1); // Offset centered boxes when bar.scale is odd.

  // Small square tool (Quiet)
  ink(0, 180, 0, 100).box(...bar.center(0, 0), bar.scale / 3, "fill*center");

  // Big square (Loud)
  ink(0, 180, 0, 100).box(...bar.center(0, 1), bar.scale / 2, "fill*center");

  unpan();

  // Current Tool Highlight
  ink(255, 255, 0, 80).box(...bar.get(0, currentTool), bar.scale, "inline");

  // ✔️ 4. Play button (Button w/ a triangle)
  ink(255, 0, 0, 100).box(buttons.play.box);

  pan(...vec2.add([], playButtonPos, playIconOffset))
    .shape(0, 0, 0, 10, 8, 5)
    .unpan();

  /*
  // % - Add / toggle separator (single bpm notes into held notes)
  ink(0, 180, 0, 100).draw(
    glyphs["%"],
    ...vec2.add([], toolbar.get(0, 2), [1, -1])
  );

  // - Line: background color change / all together.
  ink(0, 180, 0, 100).line(
    ...vec2.add([], toolbar.get(0, 3), [1, toolbar.scale / 2 - 1]),
    ...vec2.add([], toolbar.get(0, 3), [
      toolbar.scale - 2,
      toolbar.scale / 2 - 1,
    ])
  );

  // - BPM - Adds a thick line to the grid and puts a new number on the right.
  ink(0, 180, 0, 100).draw(
    glyphs["B"],
    ...vec2.add([], toolbar.get(0, 4), [2, 0])
  );
  */
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, store, load, download, upload, num: { timestamp } }) {
  // Scrolling the score.
  if (e.is("touch")) {
    // Hit-test every region to make sure we are dragging on the background.
    scrolling =
      notes.scaled.misses(e) &&
      score.scaled.misses(e) &&
      bar.scaled.misses(e) &&
      buttons.minus.box.misses(e) &&
      buttons.add.box.misses(e) &&
      buttons.play.box.misses(e);
  }
  if (e.is("draw") && scrolling) scrollY(e.delta.y);
  if (e.is("lift")) scrolling = false;

  // Switching tools.
  buttons.tools.small.act(e, () => (currentTool = 0));
  buttons.tools.big.act(e, () => (currentTool = 1));

  const scoreHasData = scoreData.flat().filter(Boolean).length > 0;

  // Alter the score based on the selected tool.
  if (e.is("touch")) {
    score.under(e, (sq) => {
      affectScore(sq.gx, sq.gy, currentTool === 0 ? "small" : "big");
      // TODO: Put this in a method.
      if (scoreHasData) {
        // TODO: Cut out any bottom rows from exported scoreData depending
        //       on the height of the rows so end points can be easily adjusted
        //       during playback. 2022.01.16.16.31
        //       (Rather than just sending stopRow here.)
        store["tracker:score"] = { data: scoreData, stopRow: score.box.h };
      }
    });
  }

  // Saving & Loading
  // Relay event info to the save button.
  buttons.save.act(e, () => {
    if (scoreHasData) {
      download({
        filename: `melody-${timestamp()}.json`,
        data: JSON.stringify({ data: scoreData, stopRow: score.box.h }),
      });
    }
  });

  buttons.load.act(e, () => {
    upload(".json")
      .then((data) => {
        ({ data: scoreData, stopRow: score.box.h } = JSON.parse(data));
      })
      .catch((err) => {
        console.error("JSON load error:", err);
      });
  });

  // Adding and removing rows from the score.
  buttons.add.act(e, () => {
    score.box.h += 1;
  });

  buttons.minus.act(e, () => {
    score.box.h = max(score.box.h - 1, 1);
    if (score.scaled.bottom - style.addMinusHeight <= notes.scaled.bottom) {
      score.box.y = notes.scaled.bottom - (score.scaled.h - score.scale);
    }
  });

  // Playing the score.
  buttons.play.act(e, () => {
    // 1. Store scoreData in disk RAM if scoreData contains playable entries.
    // TODO: Put this in a method.
    if (scoreHasData) {
      // TODO: Cut out any bottom rows from exported scoreData depending
      //       on the height of the rows so end points can be easily adjusted
      //       during playback. 2022.01.16.16.31
      //       (Rather than just sending stopRow here.)
      store["tracker:score"] = { data: scoreData, stopRow: score.box.h };
      load(parse("melody"));
    }
    // 3. Check for scoreData in system RAM and play it immediately if present.
  });
}

// 🧮 Simulate (Runs once per logic frame (120fps)).
// function sim($api) {
// TODO: Move a ball here!
// }

// 💗 Beat (Runs once per bpm)
// function beat($api) { }

// 📚 Library (Useful functions used throughout the program)
function scrollY(y) {
  score.box.y = min(notes.scale, score.box.y + y);
  const scrollHeight = score.scaled.h - score.scale - style.addMinusHeight;
  if (score.box.y < -scrollHeight) score.box.y = -scrollHeight;
}

// Positions the `add` and `minus` buttons in relationship to the score.
function addMinusLayout() {
  buttons.minus.box.x = score.scaled.x;
  buttons.minus.box.y = score.scaled.bottom;
  buttons.add.box.x = score.scaled.x + score.scaled.w / 2;
  buttons.add.box.y = score.scaled.bottom;
}

// Automatically populates 2 dimensional array [x][y] to represent the score.
function affectScore(x, y, entry) {
  if (scoreData[x]?.[y] === entry) {
    scoreData[x][y] = undefined; // Clear an entry.
    // And the whole row if it is empty.
    if (scoreData[x].length === 0) scoreData[x] = undefined;
  } else {
    // Make this row if it doesn't exist.
    if (scoreData[x] === undefined) {
      scoreData[x] = [];
    }
    scoreData[x][y] = entry;
  }
}

export { boot, paint, act };
