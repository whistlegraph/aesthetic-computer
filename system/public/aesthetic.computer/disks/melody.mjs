// Melody, 2022.01.16.16.02
// Plays back a score that was composed with `disks/tracker`.

// TODO: Request user for file if started with no parameters and no working data.

import { colors, noteList, noteFrequencies } from "../disks/common/music.mjs";

import { font1 } from "../disks/common/fonts.mjs";
let glyphs = {};

const { abs, ceil } = Math;
const { entries } = Object;

let scoreData, stopRow;
const flashes = {};

let globalBeatCount = -4;
let beatCount = -4; // TODO: Implement this in $api.sound.
let beatProgress = 0;
let beatStartTime;

// 🥾 Boot (Runs once before first paint and sim)
function boot({
  resolution,
  store,
  net: { preload },
  num: { randIntRange },
  help: { choose },
}) {
  resolution(160, 90); // 16x9

  if (store["tracker:score"]) {
    ({ data: scoreData, stopRow } = store["tracker:score"]);
  } else {
    // Randomly generate a score with a length of 8 if we didn't load one.
    stopRow = 8;
    scoreData = [];
    for (let i = 0; i < randIntRange(stopRow, stopRow * 3); i += 1) {
      let noteX = randIntRange(0, noteList.length - 1);
      let noteY = randIntRange(0, stopRow);
      if (scoreData[noteX] === undefined) scoreData[noteX] = [];
      scoreData[noteX][noteY] = choose("small", "big");
    }
  }

  // TODO: Technically I only need to load the glyphs
  //       I need to display per disk... 2022.01.16.23.38
  // Preload all glyphs.
  entries(font1).forEach(([glyph, location]) => {
    preload(`aesthetic.computer/disks/drawings/font-1/${location}.json`).then(
      (res) => {
        glyphs[glyph] = res;
      }
    );
  });
}

// 💗 Beat (Runs once per bpm)
function beat({ sound: { bpm, synth, time }, store, gizmo: { Hourglass } }) {
  bpm(store["metronome:bpm"]); // Set the bpm from last metronome run.

  if (globalBeatCount >= 0) {
    // TODO: Score data should be stored in [y][x] instead of [x][y].
    scoreData?.forEach((row, x) => {
      row.forEach((column, y) => {
        if (row[y] && y === beatCount && y < stopRow) {
          synth({
            tone: noteFrequencies[noteList[x]],
            beats: 1,
            attack: 0.01,
            decay: row[y] === "small" ? 0.9 : 0.5,
            volume: row[y] === "small" ? 0.5 : 0.8,
            pan: -0.5 + x / (noteList.length - 1),
          });
          flashes[noteList[x]] = new Hourglass(30);
        }
      });
    });
  } else {
    const sq = {
      tone: 50,
      beats: 1 / 2,
      attack: 0.01,
      decay: 0.1,
      volume: 0.5,
      pan: 0.5,
    };
    synth(sq);
    sq.pan *= -1;
    sq.tone = 150;
    synth(sq);
  }

  // Make a tiny metronome sound.
  synth({
    tone: 50,
    beats: 1 / 8,
    attack: 0.01,
    decay: 0.1,
    volume: 0.15,
    pan: -0.1,
  });

  synth({
    tone: 40,
    beats: 1 / 8,
    attack: 0.01,
    decay: 0.1,
    volume: 0.15,
    pan: 0.1,
  });

  globalBeatCount += 1;
  beatCount += 1;
  if (beatCount === stopRow) {
    beatCount = 0;
  }
  beatStartTime = time;
  beatProgress = 0; // Reset beat progress (to not conflict with `sim`s guess.)
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ wipe, ink, geo: { Grid }, screen, num: { randIntRange } }) {
  wipe(0);

  // Draw note blocks on top.
  // TODO: Make this in common with tracker.js via common/music.js
  const scale = 12;
  const startX = 8;
  const notes = new Grid(startX, 40, 12, 1, scale);

  // Draw colored boxes according to notes grid, with overlaying letters.
  [...noteList].forEach((note, i) => {
    let alpha = 70;
    if (flashes[note]?.complete === false)
      alpha = 70 + (1 - flashes[note].progress) * 240;
    ink(...colors.notes[note], alpha).box(...notes.get(i, 0), notes.scale);
  });

  // Paint measurement line.
  const midline = notes.box.y + notes.centerOffset;
  ink(0, 50).line(0, midline, screen.width, midline);

  // Crossing score.
  const scoreY = ceil(scale * beatCount + beatProgress * scale);

  const score = new Grid(
    startX,
    notes.box.y + scale - scoreY,
    12,
    stopRow,
    scale
  );
  paintScore(score, 1, true);

  // Gray line above crossing blocks.
  ink(64, 255).line(0, midline - 1, screen.width, midline - 1);

  // Paint below the bar.
  let scoreBottom = score.scaled.bottom;
  while (scoreBottom < screen.height) {
    // Draw the next score.
    const score = new Grid(startX, scoreBottom, 12, stopRow, scale);
    paintScore(score);
    scoreBottom = score.scaled.bottom;
  }

  // Paint above the bar.
  //if (globalBeatCount >= stopRow) {
  let scoreTop = score.box.y;
  while (scoreTop > 0) {
    // Draw the next score.
    const height = scale * stopRow;
    const score = new Grid(startX, scoreTop - height, 12, stopRow, scale);
    paintScore(score, 0.1);
    scoreTop = score.box.y;
  }
  //}

  // TODO: Also make this in common with tracker.js via common/music.js
  function paintScore(score, alpha = 1, crossing = false) {
    scoreData.forEach((row, x) => {
      const color = colors.notes[noteList[x]];
      row?.forEach((column, y) => {
        // Render rows in sync with score's visibility.
        if (score.box.h - 1 < y) return;

        // Special option:
        // Set the alpha to match the upper loop as we scroll passed.
        if (crossing) {
          if (score.center(x, y)[1] < notes.box.y + notes.centerOffset) {
            alpha = 0.1;
          } else {
            alpha = 1;
          }
        }

        if (column === "small") {
          ink(...color, 250 * alpha).box(
            ...score.center(x, y),
            score.scale / 3,
            "fill*center"
          );
        } else if (column === "big") {
          ink(...color, 250 * alpha).box(
            ...score.center(x, y),
            score.scale / 2,
            "fill*center"
          );
        }
      });
    });
  }

  // Display countdown.
  if (globalBeatCount < 0 && globalBeatCount > -4) {
    ink(
      randIntRange(100, 250),
      randIntRange(100, 250),
      randIntRange(100, 250),
      225
    ).printLine(abs(globalBeatCount), glyphs, 4, 4, 6, 2, 0);
  }

  if (globalBeatCount === 0) {
    ink(
      randIntRange(30, 60),
      255,
      randIntRange(50, 100),
      randIntRange(20, 240)
    ).printLine("GO!", glyphs, 4, 4, 6, 2, 0);
  }
}

// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // console.log(event);
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim({ sound: { time, bpm } }) {
  const bpmInSec = 60 / bpm;
  beatProgress = (time - beatStartTime) / bpmInSec || 0;

  // TODO: Clear flashes when hourglasses complete!

  // Run though all flashes... deleting them on completion.
  for (const flash in flashes) {
    flashes[flash].step();
    if (flashes[flash].complete) {
      delete flashes[flash];
    }
  }
}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...
export { boot, paint, act, sim, beat };
