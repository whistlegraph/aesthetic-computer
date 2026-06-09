// notenom, 2026.06.08
// Munch the squares whose musical note matches the rule, on the beat.

import { Synth } from "../lib/synth.mjs"; // shared virtual synth + perc kit

/* #region 📚 README
  A music edition of the "nom" grid game (sibling to numbnom / engnom).
  A green muncher hops a 5x5 grid of note squares. Eat every square that
  satisfies the musical rule shown up top before the beat-timer runs out.
  Each munch plays its note; the scale plays on board start, an arpeggio on
  clear, and a kick rides the beat.

  Controls:
    Arrows / WASD / hjkl — move the Muncher
    Space / Enter        — munch the current square
    Tap a square         — slide toward it (tap your own square to munch)
#endregion */

const COLS = 5;
const ROWS = 5;
const START_LIVES = 3;

// 🎵 Note model ───────────────────────────────────────────────────────────────
const LETTERS = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const SEMI = { C: 0, "C#": 1, D: 2, "D#": 3, E: 4, F: 5, "F#": 6, G: 7, "G#": 8, A: 9, "A#": 10, B: 11 };

// Each pitch class gets a fixed hue so the board reads like a little keyboard.
const NOTE_COLORS = {
  C: [255, 105, 97], "C#": [255, 150, 80], D: [255, 205, 90], "D#": [200, 225, 90],
  E: [120, 220, 110], F: [90, 215, 175], "F#": [90, 205, 220], G: [95, 165, 255],
  "G#": [150, 150, 255], A: [195, 130, 255], "A#": [235, 120, 220], B: [255, 120, 170],
};
function noteColor(letter) {
  return NOTE_COLORS[letter] || [220, 220, 235];
}
function tileColor(letter) {
  const c = noteColor(letter);
  return [round(c[0] * 0.22) + 6, round(c[1] * 0.22) + 6, round(c[2] * 0.22) + 6];
}
function luma(c) {
  return 0.3 * c[0] + 0.59 * c[1] + 0.11 * c[2];
}

// 🎼 Rules — one per board. `test(letter, oct)` decides correctness, and
// `scale` is the ascending run played when the board begins.
const RULES = [
  {
    label: "C MAJOR",
    test: (l) => ["C", "D", "E", "F", "G", "A", "B"].includes(l),
    scale: ["C4", "D4", "E4", "F4", "G4", "A4", "B4", "C5"],
  },
  {
    label: "A MINOR",
    test: (l) => ["A", "B", "C", "D", "E", "F", "G"].includes(l),
    scale: ["A3", "B3", "C4", "D4", "E4", "F4", "G4", "A4"],
  },
  {
    label: "G MAJOR",
    test: (l) => ["G", "A", "B", "C", "D", "E", "F#"].includes(l),
    scale: ["G3", "A3", "B3", "C4", "D4", "E4", "F#4", "G4"],
  },
  {
    label: "SHARPS",
    test: (l) => l.includes("#"),
    scale: ["C#4", "D#4", "F#4", "G#4", "A#4", "C#5"],
  },
  {
    label: "C CHORD",
    test: (l) => ["C", "E", "G"].includes(l),
    scale: ["C4", "E4", "G4", "C5"],
  },
  {
    label: "HIGH",
    test: (l, o) => o >= 5,
    scale: ["C5", "E5", "G5", "C6"],
  },
  {
    label: "LOW",
    test: (l, o) => o <= 3,
    scale: ["C3", "E3", "G3", "C4"],
  },
];

// 🌐 Game state
let state = "title"; // title | play | clear | over
let grid = []; // [{ letter, oct, correct, eaten, failed, flash, known }]
let rule = RULES[0];
let muncher = { col: 2, row: 2 };
let muncherVis = { col: 2, row: 2 };
let lives = START_LIVES;
let level = 1;
let remaining = 0;
let boardCorrectTotal = 1;
let message = "";
let messageTimer = 0;
let lifeFlyers = [];

// 🔊 Feel
let synth = null; // virtual synth controller (notes + perc kit), lazy-created
let frames = 0;
let facing = { x: 1, y: 0 };
let chompPhase = 0;
let combo = 0;
let screenH = 512;
let confetti = [];
let flashRed = 0;
let flashGold = 0;
let flashGreen = 0;
let mouth = 0;

// ⏳ Per-board countdown — measured in musical beats.
const FPS = 60;
const BPM = 92;
const BEAT = Math.round((60 / BPM) * FPS); // frames per beat
let beatsLeft = 0;
let beatsMax = 1;
let beatPhase = 0;
let beatPulse = 0;
let beatCount = 0; // total beats elapsed (drives kick/hat pattern)
let heldVoices = null; // sustained voices while munch is held

const { abs, floor, min, max, round, sin, cos } = Math;

// 🥾 Boot
function boot({ hud }) {
  hud?.label?.("");
}

// 🎲 Round generation ──────────────────────────────────────────────────────────
function shuffle(arr, rnd) {
  for (let i = arr.length - 1; i > 0; i -= 1) {
    const j = rnd(i + 1);
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
  return arr;
}

function noteLabel(letter, oct) {
  return letter + oct; // e.g. "C4", "F#5" — 2-3 chars
}

function newRound({ randInt }) {
  const rnd = (n) => randInt(Math.max(1, n) - 1); // 0..n-1
  rule = RULES[rnd(RULES.length)];
  const total = COLS * ROWS;
  const nCorrect = min(total - 4, 5 + level + rnd(3));

  // Octave pool — keep it tight so HIGH/LOW rules have room to differ.
  const octs = [3, 4, 5];

  // Build a "correct" note (letter + octave satisfying the rule).
  const makeGood = () => {
    for (let tries = 0; tries < 80; tries += 1) {
      const l = LETTERS[rnd(LETTERS.length)];
      const o = octs[rnd(octs.length)];
      if (rule.test(l, o)) return { letter: l, oct: o };
    }
    // Fallback from the rule's own scale.
    const s = rule.scale[rnd(rule.scale.length)];
    const m = s.match(/^([A-G]#?)(\d)$/);
    return { letter: m[1], oct: parseInt(m[2], 10) };
  };
  const makeBad = () => {
    for (let tries = 0; tries < 80; tries += 1) {
      const l = LETTERS[rnd(LETTERS.length)];
      const o = octs[rnd(octs.length)];
      if (!rule.test(l, o)) return { letter: l, oct: o };
    }
    return { letter: "C", oct: 4 };
  };

  const cells = [];
  for (let i = 0; i < nCorrect; i += 1) cells.push({ ...makeGood(), correct: true });
  for (let i = 0; i < total - nCorrect; i += 1) cells.push({ ...makeBad(), correct: false });
  shuffle(cells, rnd);

  grid = cells.map((c) => ({
    ...c,
    correct: rule.test(c.letter, c.oct), // re-verify against the live rule
    eaten: false,
    failed: false,
    flash: 0,
    known: false,
  }));
  remaining = grid.filter((c) => c.correct).length;
  boardCorrectTotal = max(1, remaining);

  muncher = { col: 2, row: 2 };
  muncherVis = { col: 2, row: 2 };
  facing = { x: 1, y: 0 };

  beatsMax = max(24, 44 - level);
  beatsLeft = beatsMax;
  beatPhase = 0;
  beatCount = 0;
  combo = 0;
  flashRed = 0;
  flashGold = 0;
  confetti = [];
  chompPhase = 0;
}

function idx(col, row) {
  return row * COLS + col;
}

// 🧮 Sim ────────────────────────────────────────────────────────────────────
function sim({ sound }) {
  if (sound && !synth) synth = Synth(sound, { volume: 0.32 });
  frames += 1;
  mouth = (mouth + 1) % 40;
  if (chompPhase > 0) chompPhase -= 1;
  if (messageTimer > 0) {
    messageTimer -= 1;
    if (messageTimer === 0) message = "";
  }
  grid.forEach((c) => {
    if (c.flash > 0) c.flash -= 1;
  });
  if (flashRed > 0) flashRed -= 1;
  if (flashGold > 0) flashGold -= 1;
  if (flashGreen > 0) flashGreen -= 1;
  if (beatPulse > 0) beatPulse -= 1;

  // 🏃 Slide the Muncher's display position toward its logical cell.
  muncherVis.col += (muncher.col - muncherVis.col) * 0.3;
  muncherVis.row += (muncher.row - muncherVis.row) * 0.3;
  if (abs(muncher.col - muncherVis.col) < 0.01) muncherVis.col = muncher.col;
  if (abs(muncher.row - muncherVis.row) < 0.01) muncherVis.row = muncher.row;

  // 💔 Life-flyers sail from the bottom-left into the board.
  if (lifeFlyers.length) {
    lifeFlyers.forEach((f) => (f.t += 0.045));
    lifeFlyers = lifeFlyers.filter((f) => f.t < 1);
  }

  // ✨ Confetti physics (board-clear).
  if (confetti.length) {
    confetti.forEach((p) => {
      p.x += p.vx;
      p.y += p.vy;
      p.vy += 0.12;
    });
    confetti = confetti.filter((p) => p.y < screenH + 40);
  }

  if (state !== "play") return;

  // ⏳ Beat timeline — each beat ticks the metronome; running out costs a life.
  beatPhase += 1;
  if (beatPhase >= BEAT) {
    beatPhase = 0;
    beatPulse = 8;
    beatCount += 1;
    tick();
    if (beatsLeft > 0) beatsLeft -= 1;
    if (beatsLeft <= 0) timeUp();
  }
}

// 🥁 Metronome — kick on the downbeat, hat on the off-beats; a rising warning
// blip in the final three beats.
function tick() {
  if (!synth) return;
  if (beatsLeft <= 3 && beatsLeft > 0) {
    synth.kick({ volume: 0.5 });
    const warn = { 3: "a5", 2: "c6", 1: "e6" }[beatsLeft];
    if (warn) synth.note(warn, { wave: "square", duration: 0.12, volume: 0.3 });
    return;
  }
  if (beatCount % 4 === 0) synth.kick({ volume: 0.55 });
  else synth.hat({ volume: 0.26 });
}

function flash(msg) {
  message = msg;
  messageTimer = 90;
}

function timeUp() {
  flash("TIME!");
  loseLife();
  beatsLeft = beatsMax;
  beatPhase = 0;
}

function loseLife() {
  lifeFlyers.push({ slot: lives - 1, t: 0 });
  lives -= 1;
  combo = 0;
  flashRed = 18;
  if (synth) {
    // A short downward sigh.
    synth.note("a3", { wave: "triangle", duration: 0.16, volume: 0.32 });
    synth.note("d3", { wave: "sine", duration: 0.4, volume: 0.3 });
  }
  if (lives <= 0) {
    state = "over";
    flash("GAME OVER");
    jingleOver();
  } else {
    flash("OUCH!");
  }
}

// 🎮 Movement + munching ──────────────────────────────────────────────────────
function move(dx, dy) {
  if (state !== "play") return;
  const pc = muncher.col;
  const pr = muncher.row;
  muncher.col = (muncher.col + dx + COLS) % COLS;
  muncher.row = (muncher.row + dy + ROWS) % ROWS;
  if (dx || dy) facing = { x: dx, y: dy };
  if (abs(muncher.col - pc) > 1) muncherVis.col = muncher.col;
  if (abs(muncher.row - pr) > 1) muncherVis.row = muncher.row;
  if (muncher.col !== pc || muncher.row !== pr) moveBlip();
}

// Tap-to-move: slide along ONE axis (no diagonals).
function moveTo(col, row) {
  if (state !== "play") return;
  const dc = col - muncher.col;
  const dr = row - muncher.row;
  if (dc === 0 && dr === 0) return;
  if (abs(dc) >= abs(dr)) {
    facing = { x: Math.sign(dc), y: 0 };
    muncher.col = max(0, min(COLS - 1, col));
  } else {
    facing = { x: 0, y: Math.sign(dr) };
    muncher.row = max(0, min(ROWS - 1, row));
  }
  moveBlip();
}

function cellNote(cell) {
  return noteLabel(cell.letter, cell.oct).toLowerCase();
}

function munch() {
  if (state !== "play") return;
  const cell = grid[idx(muncher.col, muncher.row)];
  chompPhase = 10;
  if (!cell || cell.eaten || cell.failed) {
    if (synth) synth.note(180, { wave: "sine", duration: 0.04, volume: 0.16 });
    return;
  }

  // Always voice the square's actual note when you bite it.
  if (synth) synth.note(cellNote(cell), { wave: "triangle", duration: 0.2, volume: 0.34 });

  if (cell.correct) {
    cell.eaten = true;
    cell.flash = 14;
    combo += 1;
    remaining -= 1;
    beatsLeft = min(beatsMax, beatsLeft + 1); // earn a beat back

    // Light up matching (still-uneaten) duplicates of this note.
    const key = noteLabel(cell.letter, cell.oct);
    grid.forEach((c) => {
      if (!c.eaten && !c.failed && noteLabel(c.letter, c.oct) === key) {
        c.known = true;
        c.flash = 14;
      }
    });

    // A bright combo ping a touch above the note.
    if (synth) {
      const top = 660 + min(combo, 8) * 40;
      synth.note(top, { wave: "sine", duration: 0.08, volume: 0.2 });
    }

    if (remaining === 1) flash("LAST ONE!");
    if (remaining <= 0) {
      state = "clear";
      flashGold = 14;
      spawnConfetti();
      flash("BOARD CLEAR!");
      jingleClear();
    }
  } else {
    cell.flash = 14;
    cell.failed = true; // X it out
    if (synth) synth.note(140, { wave: "sawtooth", duration: 0.18, volume: 0.3 });
    loseLife();
  }
}

function moveBlip() {
  if (synth) synth.note(340, { wave: "sine", duration: 0.02, volume: 0.14 });
}

// 🎼 Play the rule's scale ascending when a board begins.
function playScale() {
  if (!synth) return;
  rule.scale.forEach((n, i) => {
    setTimeout(() => {
      if (synth) synth.note(n.toLowerCase(), { wave: "triangle", duration: 0.18, volume: 0.3 });
    }, i * 110);
  });
}

// 🎉 Victory arpeggio + cymbal on board clear.
function jingleClear() {
  if (!synth) return;
  ["c4", "e4", "g4", "c5", "e5", "g5", "c6"].forEach((n, i) => {
    setTimeout(() => {
      if (synth) synth.note(n, { wave: "triangle", duration: 0.16, volume: 0.3 });
    }, i * 80);
  });
  synth.crash?.({ volume: 0.4 });
  setTimeout(() => synth?.chord?.(["c5", "e5", "g5"], { wave: "sine", duration: 0.5, volume: 0.24 }), 600);
}

// 💀 A short minor turn on game over.
function jingleOver() {
  if (!synth) return;
  ["g4", "d#4", "c4", "g3"].forEach((n, i) => {
    setTimeout(() => {
      if (synth) synth.note(n, { wave: "triangle", duration: 0.3, volume: 0.3 });
    }, i * 170);
  });
}

function spawnConfetti() {
  confetti = [];
  const cols = [
    [255, 210, 90], [120, 235, 120], [255, 120, 160], [120, 180, 255], [255, 255, 255],
  ];
  const bcx = layout.x + (layout.cell * COLS) / 2;
  for (let i = 0; i < 44; i += 1) {
    confetti.push({
      x: bcx + (((i * 53) % 100) - 50) * (layout.cell * COLS) * 0.006,
      y: layout.y + 4,
      vx: (((i * 31) % 7) - 3) * 0.7,
      vy: -2.4 - ((i * 17) % 5) * 0.5,
      s: 2 + (i % 3),
      c: cols[i % cols.length],
    });
  }
}

// 🎪 Act ─────────────────────────────────────────────────────────────────────
function act({ event: e, sound, num: { randInt } }) {
  if (sound && !synth) synth = Synth(sound, { volume: 0.32 });

  const startBoard = () => {
    state = "play";
    flashGreen = 16;
    playScale();
  };

  const advance = () => {
    if (state === "title") {
      resetGame();
      newRound({ randInt });
    } else if (state === "clear") {
      level += 1;
      newRound({ randInt });
    } else if (state === "over") {
      resetGame();
      newRound({ randInt });
    } else return;
    startBoard();
  };

  if (state !== "play") {
    if (
      e.is("keyboard:down:space") ||
      e.is("keyboard:down:enter") ||
      e.is("keyboard:down:return") ||
      e.is("touch")
    )
      advance();
    return;
  }

  // Release the sustained munch voices.
  if (
    e.is("keyboard:up:space") ||
    e.is("keyboard:up:enter") ||
    e.is("keyboard:up:return") ||
    e.is("lift")
  )
    killHeld();

  // Movement — arrows + WASD + vim (hjkl).
  if (e.is("keyboard:down:arrowleft") || e.is("keyboard:down:a") || e.is("keyboard:down:h")) move(-1, 0);
  if (e.is("keyboard:down:arrowright") || e.is("keyboard:down:d") || e.is("keyboard:down:l")) move(1, 0);
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:w") || e.is("keyboard:down:k")) move(0, -1);
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:s") || e.is("keyboard:down:j")) move(0, 1);

  // Munch — and sustain the bitten note while the key is held.
  if (
    e.is("keyboard:down:space") ||
    e.is("keyboard:down:enter") ||
    e.is("keyboard:down:return")
  ) {
    munch();
    if (!heldVoices && synth) {
      const cell = grid[idx(muncher.col, muncher.row)];
      if (cell && !cell.eaten && !cell.failed) {
        const n = cellNote(cell);
        heldVoices = [
          synth.hold(n, { wave: "sine", volume: 0.14 }),
          synth.hold(n, { wave: "triangle", volume: 0.08 }),
        ];
      }
    }
  }

  // Touch: tap a square to slide there; tap your own square / off-board to munch.
  if (e.is("touch")) {
    const hit = cellAt(e.x, e.y);
    if (hit && (hit.col !== muncher.col || hit.row !== muncher.row)) moveTo(hit.col, hit.row);
    else munch();
  }
}

function killHeld() {
  heldVoices?.forEach((v) => v?.kill?.(0.16));
  heldVoices = null;
}

function resetGame() {
  lives = START_LIVES;
  level = 1;
  message = "";
  messageTimer = 0;
}

// 📐 Layout ───────────────────────────────────────────────────────────────────
let layout = { x: 0, y: 0, cell: 32, top: 54, bottomPad: 30 };

function computeLayout(screen) {
  screenH = screen.height;
  const sideMargin = max(28, floor(screen.width * 0.12));
  const top = 54;
  const bottomPad = 30;
  const availW = screen.width - sideMargin * 2;
  const availH = screen.height - top - bottomPad;
  const cell = max(18, floor(min(availW / COLS, availH / ROWS)));
  const gw = cell * COLS;
  const gh = cell * ROWS;
  layout = {
    cell,
    x: floor((screen.width - gw) / 2),
    y: top + floor((availH - gh) / 2),
    top,
    bottomPad,
  };
}

function cellAt(px, py) {
  const { x, y, cell } = layout;
  const col = floor((px - x) / cell);
  const row = floor((py - y) / cell);
  if (col < 0 || col >= COLS || row < 0 || row >= ROWS) return null;
  return { col, row };
}

// 🎨 Paint ────────────────────────────────────────────────────────────────────
function paint({ wipe, ink, screen, write, box, line }) {
  computeLayout(screen);
  wipe(8, 10, 26);
  paintBackground({ ink, box, screen });

  if (state === "title") return paintTitle({ ink, screen, write });

  const { x, y, cell } = layout;
  const tf = beatsMax ? beatsLeft / beatsMax : 0;
  const lowTime = beatsLeft <= 3 && state === "play";
  const blink = (frames >> 2) % 2 === 0;

  if (lowTime && blink) ink(150, 0, 0, 115).box(0, 0, screen.width, screen.height);

  // ⏳ Beat timeline bar across the top.
  const tcol = tf > 0.5 ? [120, 220, 120] : tf > 0.25 ? [240, 210, 90] : [240, 80, 80];
  ink(20, 24, 44).box(0, 0, screen.width, 3 + (beatPulse > 4 ? 1 : 0));
  ink(...tcol).box(0, 0, round(screen.width * tf), 3 + (beatPulse > 4 ? 1 : 0));

  // ⏳ Big beats-left counter, top-left.
  const timeCol = lowTime ? (blink ? [255, 60, 60] : [255, 235, 120]) : tcol;
  ink(...timeCol).write(`${beatsLeft}`, { x: 8, y: 6, size: 3 }, undefined, undefined, false, "unifont");

  // Rule title, top-right.
  const ruleX = max(4, screen.width - 8 - rule.label.length * 12);
  ink(255, 230, 120).write(rule.label, { x: ruleX, y: 9, size: 2 });

  // Grid.
  const gameOver = state === "over";
  for (let r = 0; r < ROWS; r += 1) {
    for (let c = 0; c < COLS; c += 1) {
      const cx = x + c * cell;
      const cy = y + r * cell;
      const cd = grid[idx(c, r)];
      const txt = noteLabel(cd.letter, cd.oct);
      const missed = gameOver && cd.correct && !cd.eaten;

      // Tile background.
      let bg;
      if (cd.flash > 0) bg = cd.correct ? [40, 150, 70] : [160, 45, 45];
      else if (cd.eaten) bg = [15, 17, 34];
      else if (cd.failed) bg = [44, 22, 24];
      else if (missed) bg = [72, 74, 82];
      else bg = tileColor(cd.letter);
      ink(...bg).box(cx + 1, cy + 1, cell - 2, cell - 2);

      // Outline.
      if (cd.known && !cd.eaten && !cd.failed)
        ink(120, 230, 140).box(cx + 1, cy + 1, cell - 2, cell - 2, "outline");
      else if (missed)
        ink(210, 214, 222).box(cx + 1, cy + 1, cell - 2, cell - 2, "outline");
      else ink(60, 70, 120).box(cx + 1, cy + 1, cell - 2, cell - 2, "outline");

      // Note label.
      if (!cd.eaten) {
        const fs = txt.length * 12 <= cell - 8 ? 2 : 1;
        let tx = cx + cell / 2 - (txt.length * 6 * fs) / 2;
        let ty = cy + cell / 2 - fs * 4;
        if (cd.known && !cd.failed) {
          tx += sin(frames * 0.9 + c * 1.7) * 1.6;
          ty += cos(frames * 1.1 + r * 1.3) * 1.2;
        }
        let col;
        if (cd.failed) col = [120, 95, 100];
        else if (missed) col = [228, 230, 238];
        else col = noteColor(cd.letter);
        ink(...col).write(txt, { x: tx, y: ty, size: fs });

        if (cd.failed) {
          ink(225, 70, 70);
          line(cx + 4, cy + 4, cx + cell - 4, cy + cell - 4);
          line(cx + cell - 4, cy + 4, cx + 4, cy + cell - 4);
        }
      }
    }
  }

  // Muncher.
  {
    const cx = x + muncherVis.col * cell;
    const cy = y + muncherVis.row * cell;
    paintMuncher({ ink, box }, cx, cy, cell, state === "clear");
  }

  // ✨ Confetti.
  confetti.forEach((p) => ink(p.c[0], p.c[1], p.c[2]).box(p.x, p.y, p.s, p.s));

  // 🎶 Beat ambience pulse.
  if (beatPulse > 0 && state === "play") {
    const vibe = [[80, 120, 255], [80, 220, 230], [170, 110, 240]][beatCount % 3];
    ink(vibe[0], vibe[1], vibe[2], beatPulse * 4).box(0, 0, screen.width, screen.height);
  }

  // Full-screen flashes.
  if (flashRed > 0) ink(210, 25, 25, min(190, flashRed * 11)).box(0, 0, screen.width, screen.height);
  if (flashGold > 0) ink(255, 215, 90, min(150, flashGold * 11)).box(0, 0, screen.width, screen.height);
  if (flashGreen > 0) ink(60, 220, 110, min(150, flashGreen * 11)).box(0, 0, screen.width, screen.height);

  // 💚 Lives — little monsters bottom-left.
  const mcx = x + muncherVis.col * cell + cell / 2;
  const mcy = y + muncherVis.row * cell + cell / 2;
  const lifeSz = 13;
  const lifeGap = 17;
  const lifeY = screen.height - lifeSz - 6;
  for (let i = 0; i < lives; i += 1)
    drawMiniMonster({ ink, box }, 6 + i * lifeGap, lifeY, lifeSz, mcx, mcy, healthColor(lives));

  // 💔 Lost-life flyers.
  const bcx = x + (cell * COLS) / 2;
  const bcy = y + (cell * ROWS) / 2;
  lifeFlyers.forEach((f) => {
    const sx = 6 + f.slot * lifeGap + lifeSz / 2;
    const sy = lifeY + lifeSz / 2;
    const e = f.t * f.t;
    const fx = sx + (bcx - sx) * e;
    const fy = sy + (bcy - sy) * e;
    const sz = max(4, lifeSz * (1 - 0.6 * f.t));
    drawMiniMonster({ ink, box }, fx - sz / 2, fy - sz / 2, sz, bcx, bcy, [235, 95, 95]);
  });

  // 🔢 Notes-left, bottom-right.
  const leftN = `${remaining}`;
  const leftCol = remaining <= 3 ? [245, 130, 130] : [150, 210, 175];
  const leftW = leftN.length * 16;
  ink(...leftCol).write(leftN, { x: screen.width - 10 - leftW, y: screen.height - 38 }, undefined, undefined, false, "unifont");
  ink(140, 165, 195).write("LEFT", { x: screen.width - 10 - leftW - 30, y: screen.height - 24 });

  // 📣 Transient message banner.
  if (message) {
    const ms = 2;
    const mw = message.length * 6 * ms;
    const mx = max(4, round(screen.width / 2 - mw / 2));
    const myy = layout.y - 8 * ms - 6;
    let mbg, mfg;
    if (/OUCH|TIME|OVER/.test(message)) (mbg = [205, 45, 45]), (mfg = [255, 240, 240]);
    else if (/CLEAR/.test(message)) (mbg = [45, 175, 85]), (mfg = [12, 30, 14]);
    else if (/LAST/.test(message)) (mbg = [235, 200, 60]), (mfg = [40, 28, 0]);
    else (mbg = [80, 90, 150]), (mfg = [240, 240, 255]);
    ink(...mbg).box(mx - 5, myy - 3, mw + 10, 8 * ms + 6);
    ink(0, 0, 0, 110).box(mx - 5, myy - 3, mw + 10, 8 * ms + 6, "outline");
    ink(...mfg).write(message, { x: mx, y: myy, size: ms });
  }

  // Overlays.
  if (state === "clear" && confetti.length === 0)
    overlay({ ink, screen, write }, "BOARD CLEAR", "press any key");
  if (state === "over")
    overlay({ ink, screen, write }, "GAME OVER", "press to retry");
}

function paintBackground({ ink, box, screen }) {
  const sp = 40;
  const dx = (frames * 0.18) % sp;
  const dy = (frames * 0.11) % sp;
  const pulse = beatPulse > 0 ? 6 : 0;
  for (let gy = -sp; gy < screen.height + sp; gy += sp) {
    for (let gx = -sp; gx < screen.width + sp; gx += sp) {
      const b = 4 + pulse + 3 * sin((gx + gy) * 0.05 + frames * 0.035);
      ink(14 + round(b * 0.4), 16 + round(b * 0.5), 32 + round(b));
      box(gx + dx, gy + dy, 2, 2);
    }
  }
}

function healthColor(n) {
  if (n >= 3) return [125, 235, 130];
  if (n === 2) return [205, 220, 95];
  return [235, 150, 70];
}

function drawMiniMonster({ ink, box }, x, y, s, lookX, lookY, color) {
  const legH = max(1, floor(s * 0.18));
  const bodyH = s - legH;
  ink(round(color[0] * 0.7), round(color[1] * 0.7), round(color[2] * 0.7));
  box(x + s * 0.14, y + bodyH, max(1, floor(s * 0.24)), legH);
  box(x + s * 0.6, y + bodyH, max(1, floor(s * 0.24)), legH);
  ink(...color).box(x, y, s, bodyH);
  const cxp = x + s / 2;
  const cyp = y + bodyH / 2;
  let dx = lookX - cxp;
  let dy = lookY - cyp;
  const d = Math.hypot(dx, dy) || 1;
  dx /= d;
  dy /= d;
  const eo = max(2, floor(s * 0.3));
  const eyy = y + s * 0.22;
  const eyL = x + s * 0.14;
  const eyR = x + s * 0.54;
  ink(255).box(eyL, eyy, eo, eo);
  ink(255).box(eyR, eyy, eo, eo);
  const pup = max(1, floor(eo * 0.5));
  const gx = round((dx * (eo - pup)) / 2);
  const gy = round((dy * (eo - pup)) / 2);
  ink(20, 30, 20).box(eyL + (eo - pup) / 2 + gx, eyy + (eo - pup) / 2 + gy, pup, pup);
  ink(20, 30, 20).box(eyR + (eo - pup) / 2 + gx, eyy + (eo - pup) / 2 + gy, pup, pup);
}

function paintMuncher({ ink, box }, cx, cy, cell, dance = false) {
  const fullness = max(0, min(1, 1 - remaining / boardCorrectTotal));
  let scale = 0.72 + fullness * 0.4;
  const dying = beatsLeft <= 3 && state === "play";
  if (dying) scale *= 0.66 + 0.12 * abs(sin(frames * 0.8));
  const s = max(8, floor((cell - 2) * 0.8 * scale));
  const legH = max(2, floor(s * 0.18));
  const bodyH = s - legH;
  const trem = dying ? 1.8 : 0;
  const ox = dying ? sin(frames * 1.4) * trem : 0;
  const oy =
    (dance ? -abs(sin(frames * 0.45)) * s * 0.28 : 0) +
    (dying ? cos(frames * 1.8) * trem : 0);
  const x = cx + (cell - s) / 2 + ox;
  const y = cy + (cell - s) / 2 + oy;

  const bob = (frames >> (dance ? 1 : 3)) % 2;
  const legW = max(2, floor(s * 0.26));
  const footY = y + bodyH;
  ink(60, 175, 80);
  box(x + s * 0.12, footY - (bob ? 1 : 0), legW, legH);
  box(x + s * 0.62, footY - (bob ? 0 : 1), legW, legH);

  ink(...(dying ? [150, 220, 130] : [125, 235, 130])).box(x, y, s, bodyH);

  const eo = max(2, floor(s * 0.2));
  const ey = y + s * 0.2;
  const exL = x + s * 0.18;
  const exR = x + s * 0.56;
  ink(255).box(exL, ey, eo, eo);
  ink(255).box(exR, ey, eo, eo);
  const pup = max(1, floor(eo * 0.5));
  const gx = floor((eo - pup) / 2) * facing.x;
  const gy = floor((eo - pup) / 2) * facing.y;
  ink(20, 40, 25).box(exL + (eo - pup) / 2 + gx, ey + (eo - pup) / 2 + gy, pup, pup);
  ink(20, 40, 25).box(exR + (eo - pup) / 2 + gx, ey + (eo - pup) / 2 + gy, pup, pup);

  const my = y + bodyH * 0.66;
  if (chompPhase > 0) {
    ink(25, 40, 25).box(x + s * 0.3, my - s * 0.05, s * 0.4, s * 0.26);
  } else {
    ink(40, 80, 45).box(x + s * 0.32, my, s * 0.36, max(1, floor(s * 0.08)));
  }
}

function paintTitle({ ink, screen, write }) {
  ink(255, 230, 120).write("NOTENOM", { center: "x", y: screen.height / 2 - 40, size: 3 });
  ink(120, 235, 120).write("music edition", { center: "x", y: screen.height / 2 - 6 });
  ink(180, 200, 255).write("eat the notes that match the rule", { center: "x", y: screen.height / 2 + 14 });
  ink(140, 160, 220).write("arrows move - space munches - mind the beat", { center: "x", y: screen.height / 2 + 28 });
  if (mouth < 24)
    ink(255).write("press any key to start", { center: "x", y: screen.height / 2 + 54 });
}

function overlay({ ink, screen, write }, title, sub) {
  ink(0, 0, 0, 125).box(0, 0, screen.width, screen.height);
  ink(255, 230, 120).write(title, { center: "x", y: screen.height / 2 - 18, size: 2 });
  ink(200, 215, 255).write(sub, { center: "x", y: screen.height / 2 + 12 });
}

// 📰 Meta
function meta() {
  return {
    title: "Notenom",
    desc: "Munch the squares whose musical note matches the rule, on the beat.",
  };
}

export { boot, sim, paint, act, meta };
