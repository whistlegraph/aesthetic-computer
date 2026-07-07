// nom — shared engine for the muncher games (numbnom / engnom / mexinom / dannom / rusnom / notenom / catnom), 2026.06.07
// Move the muncher around a 5×5 grid and eat every square that satisfies the
// rule at the top — but dodge the Troggles. `numbnom` = numbers, `engnom` = words,
// `notenom` = musical notes (note mode voices each square + plays the scale),
// `catnom` = Categories-game rounds (AC pieces, code words, slang, vibes).

import { Synth } from "./synth.mjs"; // shared virtual synth + perc kit

/* #region 📚 README
  Controls:
    Arrows / WASD   — move the Muncher
    Space / Enter   — munch the current square
    Tap a square    — walk there and munch it on arrival
    Tap own square  — munch it right now

  Modes (pass after a colon):
    munchers          → numbers
    munchers:numbers  → numbers
    munchers:words    → words
    munchers:notes    → musical notes (notenom)
    munchers:cat      → categories (catnom)
#endregion */

/* #region 🏁 TODO
  - [] Add a leaderboard / score persistence.
  - [] More Troggle personalities (some eat numbers, some teleport).
  - [] Sound for the Troggle stomp.
#endregion */

const COLS = 5;
const ROWS = 5;

// 🌐 Game state
let mode = "number"; // "number" | "word" | "note"
let lang = "en"; // word language: "en" engnom | "es" mexinom | "da" dannom | "ru" rusnom | "cat" catnom
let hiRes = false; // 🖼️ render via the hd() native-resolution Canvas2D layer (opt-in per edition)
let noteScale = []; // current board's scale (note mode), played on board start
let state = "title"; // title | play | clear | over | win
let grid = []; // [{ value, correct, eaten, flash }]
let ruleLabel = ""; // short on-screen token (e.g. "X3")
let ruleSpeech = ""; // spoken phrase (e.g. "multiples of 3")
let caption = ""; // 💬 last spoken text — always shown along the bottom
let captionFrame = 0; // frame the caption was set (for a quick fade-in)
const warmed = new Set(); // utterances already sent to the TTS cache this session
let muncher = { col: 2, row: 2 }; // logical grid position
let muncherVis = { col: 2, row: 2 }; // smoothed display position (slides)
let hover = null; // { col, row } cell under the mouse, for hover highlight
let swallowTap = false; // 👆 set when a tap was consumed (e.g. advancing an overlay)
let walkTarget = null; // 👆 tap-to-move destination cell — sim walks the muncher there
let walkTick = 0; // frames until the walker's next step
const WALK_STEP_FRAMES = 7; // tap-to-move cadence (~115ms per square)
let troggles = [];
let level = 1; // internal progression counter (board #) — never displayed
let remaining = 0; // correct cells left to eat
let foundValues = []; // distinct correct values eaten (shown in a bottom row)
let boardCorrectTotal = 1; // correct cells the board started with (fatness gauge)
let invuln = 0; // frames of board-start immunity (troggle spawn grace)
let mouth = 0; // muncher mouth animation phase
let troggleClock = null;
let message = ""; // transient feedback line
let messageTimer = 0;

// 🔊 + ✨ Feel
let snd = null; // live sound handle (captured each frame)
let synth = null; // virtual synth controller (notes + perc kit), lazy-created
let speakFn = null; // live speak() handle (captured in act)
let frames = 0; // global frame counter (drives legs + melody scheduler)
let facing = { x: 1, y: 0 }; // direction the Muncher's mouth points
let chompPhase = 0; // frames remaining in a chomp animation
let combo = 0; // consecutive correct munches → rising pitch
let melody = []; // queued notes: { at, type, tone, duration, volume }

// 🎥 Dynamic camera (smoothed follow-lean + zoom punch + shake), à la dumduel.
let cam = { x: 0, y: 0, zoom: 1, shake: 0 };
let camTargetZoom = 1;
const MAX_ZOOM = 1.18; // peak zoom punch — layout reserves headroom so it fits
let screenH = 512; // last seen screen height (for confetti culling)

// 💀 / 🎉 Sequenced animations.
let deathPhase = 0; // counts down during the death animation
let deathPos = { col: 2, row: 2 }; // where the Muncher died
let pendingOver = false; // game over fires after the death animation
let clearPhase = 0; // counts down during the level-clear celebration
let flashRed = 0; // red screen-flash intensity (death)
let flashGold = 0; // gold screen-flash intensity (clear)
let flashGreen = 0; // green screen-flash intensity (board start)
let confetti = []; // { x, y, vx, vy, s, c } celebration bits

// ⏳ Per-board countdown — measured in musical beats, ticked off the *wall
// clock* (clock.time(), network-synced) rather than a free-running frame
// counter. Every board everywhere shares the same global beat grid, so two
// games started seconds apart still pulse in unison — like clocks naturally do.
const FPS = 60;
const BPM = 74; // slower, more relaxed groove
const BEAT_MS = (60 / BPM) * 1000; // milliseconds per beat (~810ms, BPM 74)
let beatMs = BEAT_MS; // active beat length (ms) — note mode overrides to BPM 92
let nowMs = 0; // last seen synced wall-clock time (ms), captured each sim
let beatIndex = 0; // global beat number = floor(nowMs / beatMs)
let lastBeatIndex = null; // beat index at the previous tick (null until first)
let beatsLeft = 0; // beats of time remaining on this board
let beatsMax = 1; // beats the board started with
let beatPhase = 0; // 0..1 fraction into the current beat (drives sub-beat feel)
let beatPulse = 0; // brief visual pulse on each beat
let heldChord = null; // sustained synth voices while munch is held

// 🪧 Intro — the board is live from frame one (no title gate / "press any key").
// A brief name+edition card fades over the playing board; the beat budget is
// frozen until it clears so reading the title costs no time.
const INTRO_FRAMES = 132; // ~2.2s at 60fps
let introTimer = 0; // counts down while the intro card is showing
let pendingStart = false; // announce the rule + play the start chime once audio is up

// ♻️ Full state reset. This engine module is a session singleton shared by every
// nom disk (numbnom/engnom/mexinom/dannom/rusnom/notenom), so its module-level
// state survives navigation. boot() runs fresh on every entry, so reinitializing
// everything here makes each piece a clean, self-contained AC piece — no bleed
// between games (mirrors world_boot / nopaint_boot). Keep in sync with the
// declarations at the top of the file.
function reset() {
  mode = "number"; lang = "en"; hiRes = false; noteScale = [];
  state = "title"; grid = []; ruleLabel = ""; ruleSpeech = ""; caption = "";
  muncher = { col: 2, row: 2 }; muncherVis = { col: 2, row: 2 };
  hover = null; swallowTap = false; walkTarget = null; walkTick = 0;
  sprites.clear(); // drop last session's hd label sprites (fonts/theme may differ)
  pixelWashDark = null; // the hidden pixel buffer may hold another piece's art
  troggles = []; level = 1; remaining = 0;
  foundValues = []; boardCorrectTotal = 1;
  invuln = 0; mouth = 0; troggleClock = null; message = ""; messageTimer = 0;
  snd = null; synth = null; speakFn = null; frames = 0;
  facing = { x: 1, y: 0 }; chompPhase = 0; combo = 0; melody = [];
  cam = { x: 0, y: 0, zoom: 1, shake: 0 }; camTargetZoom = 1; screenH = 512;
  deathPhase = 0; deathPos = { col: 2, row: 2 }; pendingOver = false;
  clearPhase = 0; flashRed = 0; flashGold = 0; flashGreen = 0; confetti = [];
  beatMs = BEAT_MS; nowMs = 0; beatIndex = 0; lastBeatIndex = null;
  beatsLeft = 0; beatsMax = 1; beatPhase = 0; beatPulse = 0; heldChord = null;
  introTimer = 0; pendingStart = false;
}

// 🥾 Boot — reset all state, then drop straight onto a live board (no title gate
// / "press any key"); a brief intro card fades over the playing board.
function boot({ params, hud, clock, num: { randInt } }) {
  reset();
  clock?.resync?.(); // network-sync the wall clock; board beats ride this grid
  const m = resolveMode(params);
  mode = m.mode;
  lang = m.lang;
  hiRes = params?.includes?.("sd") !== true; // hi-res Canvas2D paint by default; "sd" opts out
  hud?.label?.(""); // hide the top-left corner label
  resetGame({ randInt });
  newRound({ randInt });
  state = "play";
  introTimer = INTRO_FRAMES; // title/edition card over the live board (beats frozen)
  flashGreen = 16; // green "go!" flash
  pendingStart = true; // announce the rule + chime once audio is available
}

// Map the first colon param → { mode, lang }. Shared by boot() (sets the live
// globals) and makeMeta() so a piece's identity is computed from its params,
// not from leftover singleton state — the system calls meta() *before* boot(),
// and this engine module is reused across nom pieces, so reading the globals in
// meta() would otherwise mislabel (e.g. dannom showing the number edition).
function resolveMode(params) {
  const p = params?.[0];
  if (p === "spanish" || p === "es" || p === "mexi" || p === "mexinom") return { mode: "word", lang: "es" };
  if (p === "danish" || p === "da" || p === "dansk" || p === "dannom") return { mode: "word", lang: "da" };
  if (p === "russian" || p === "ru" || p === "russkiy" || p === "rusnom") return { mode: "word", lang: "ru" };
  if (p === "cat" || p === "cats" || p === "categories" || p === "catnom") return { mode: "word", lang: "cat" };
  if (p === "words" || p === "word" || p === "english" || p === "en") return { mode: "word", lang: "en" };
  if (p === "notes" || p === "note" || p === "music" || p === "notenom") return { mode: "note", lang: "en" };
  return { mode: "number", lang: "en" }; // default (incl. bare / numbers)
}

// 🎲 Round generation ────────────────────────────────────────────────────────

const { abs, floor, min, max, round, sin, cos, pow } = Math;

function isPrime(n) {
  if (n < 2) return false;
  for (let i = 2; i * i <= n; i += 1) if (n % i === 0) return false;
  return true;
}

// Every digit 0–9 has its own fixed color; numbers are drawn digit-by-digit.
const DIGIT_COLORS = [
  [255, 105, 97],  // 0 coral
  [255, 170, 80],  // 1 orange
  [255, 225, 90],  // 2 yellow
  [150, 225, 90],  // 3 lime
  [90, 220, 140],  // 4 green
  [90, 215, 210],  // 5 teal
  [95, 180, 255],  // 6 blue
  [150, 150, 255], // 7 indigo
  [200, 140, 255], // 8 violet
  [255, 140, 210], // 9 pink
];
function charHue(ch) {
  if (ch >= "0" && ch <= "9") return DIGIT_COLORS[ch.charCodeAt(0) - 48];
  if (ch >= "a" && ch <= "z")
    return DIGIT_COLORS[(ch.charCodeAt(0) - 97) % DIGIT_COLORS.length];
  return null; // theme's plain text color
}
function charColor(ch) {
  const c = charHue(ch);
  return c ? themedText(c) : T.plainText;
}
// Theme-aware tint for a cell's background, keyed to its last character.
function tileColor(str) {
  return tileTint(charHue(str[str.length - 1] || "0") || [140, 150, 180]);
}

// Color-name words → their actual color (EN + ES), so COLORS/COLORES squares
// are literally that color.
const COLOR_WORDS = {
  red: [230, 60, 60], rojo: [230, 60, 60],
  blue: [70, 120, 240], azul: [70, 120, 240],
  green: [70, 200, 90], verde: [70, 200, 90],
  pink: [240, 120, 180], rosa: [240, 120, 180],
  gold: [240, 200, 70], oro: [240, 200, 70],
  gray: [150, 150, 160], gris: [150, 150, 160],
  black: [35, 35, 44], negro: [35, 35, 44],
  white: [235, 235, 245], blanco: [235, 235, 245],
  brown: [150, 90, 50], cafe: [150, 90, 50],
  purple: [160, 90, 220], morado: [160, 90, 220],
  tan: [210, 180, 140], lime: [160, 230, 90], teal: [80, 200, 190],
  navy: [60, 80, 170], cyan: [90, 220, 230], rose: [230, 120, 140],
  // 🇩🇰 Danish color words (real spelling — board renders them in MatrixChunky8).
  "rød": [230, 60, 60], "blå": [70, 120, 240], "grøn": [70, 200, 90],
  gul: [240, 210, 70], sort: [35, 35, 44], hvid: [235, 235, 245],
  "grå": [150, 150, 160], brun: [150, 90, 50],
  // 🇷🇺 Russian color words (Cyrillic — board renders them in MatrixChunky8).
  "красный": [230, 60, 60], "синий": [70, 120, 240], "зелёный": [70, 200, 90],
  "жёлтый": [240, 210, 70], "чёрный": [35, 35, 44], "белый": [235, 235, 245],
  "серый": [150, 150, 160], "розовый": [240, 120, 180],
};
function luma(c) {
  return 0.3 * c[0] + 0.59 * c[1] + 0.11 * c[2];
}

// 🌗 Theme — auto light/dark, following the system setting. paint() re-reads
// `api.dark` every frame, so flipping the OS theme restyles the live board.
// T holds every structural color; the rainbow hue tables (digits, notes,
// color-words) stay fixed and run through themedText / tileTint instead.
let dark = true;
let T = null;

function setTheme(isDark) {
  if (T && dark === isDark) return;
  dark = isDark;
  T = dark ? {
    bg: [8, 10, 26],
    trackBg: [20, 24, 44],
    timeHi: [120, 220, 120], timeMid: [240, 210, 90], timeLow: [240, 80, 80],
    timerPanic: [255, 60, 60], timerPanicAlt: [255, 235, 120],
    ruleLabel: [255, 230, 120],
    captionText: [245, 248, 255], captionBg: [0, 0, 0, 160],
    eaten: [15, 17, 34],
    failedBg: [44, 22, 24],
    missedBg: [72, 74, 82], missedText: [228, 230, 238], missedOutline: [210, 214, 222],
    outline: [60, 70, 120], knownOutline: [150, 245, 170],
    hoverFill: [255, 255, 255, 28], hoverOutline: [235, 242, 255],
    wrapHint: [110, 215, 255, 110],
    beckon: [170, 255, 195],
    leftLabel: [140, 165, 195], leftWarn: [245, 130, 130], leftOk: [150, 210, 175],
    introName: [255, 230, 120], introSub: [150, 220, 255],
    overlaySub: [210, 220, 255],
    plainText: [230, 230, 245],
  } : {
    bg: [236, 240, 247],
    trackBg: [213, 219, 231],
    timeHi: [40, 150, 70], timeMid: [190, 148, 16], timeLow: [198, 50, 50],
    timerPanic: [205, 32, 32], timerPanicAlt: [170, 130, 12],
    ruleLabel: [158, 116, 8],
    captionText: [28, 32, 44], captionBg: [255, 255, 255, 180],
    eaten: [224, 229, 238],
    failedBg: [240, 219, 219],
    missedBg: [203, 207, 216], missedText: [56, 60, 70], missedOutline: [128, 134, 148],
    outline: [175, 185, 207], knownOutline: [26, 138, 66],
    hoverFill: [20, 30, 50, 24], hoverOutline: [50, 70, 100],
    wrapHint: [24, 104, 164, 150],
    beckon: [20, 150, 80],
    leftLabel: [96, 110, 134], leftWarn: [198, 54, 54], leftOk: [34, 124, 86],
    introName: [150, 108, 6], introSub: [24, 98, 156],
    overlaySub: [64, 74, 110],
    plainText: [42, 46, 58],
  };
}
setTheme(true);

// Text hue adapted to the theme — the rainbow is tuned for the dark board;
// on paper each hue drops to ~58% so it keeps contrast on pastel tiles.
function themedText(c) {
  return dark ? c : [round(c[0] * 0.58), round(c[1] * 0.58), round(c[2] * 0.58)];
}

// Tile-background tint from a hue — deep tints on dark, pastels on light.
function tileTint(c) {
  return dark
    ? [round(c[0] * 0.24) + 6, round(c[1] * 0.24) + 6, round(c[2] * 0.24) + 6]
    : [255 - round((255 - c[0]) * 0.22), 255 - round((255 - c[1]) * 0.22), 255 - round((255 - c[2]) * 0.22)];
}

// 🔤 Text fitting ─────────────────────────────────────────────────────────────
// Word cells render in GNU unifont — fixed 8×16, highly legible, with full
// coverage of every edition's glyphs (ASCII, Danish æ/ø/å, Cyrillic). Numbers/
// notes stay on the default 6×10 font. Sizing is *measurement-free* on purpose:
// measuring proportional widths via text.box was unreliable because non-ASCII
// advances only arrive after the glyph BDF streams in, which scrambled early
// frames. unifont's fixed 8px advance makes size + centering exact and stable.
const CELL_MARGIN = 4; // px inset on each side of a cell
const FONT_METRICS = {
  unifont: { adv: 8, h: 16 }, // fixed-width — exact sizing, no async surprises
  MatrixChunky8: { adv: 5, h: 8 }, // proportional (kept for reference)
  _default: { adv: 6, h: 10 }, // font_1 (monospace)
};
// hd rasterizes unifont ~14% narrower than its bitmap advance, so the fixed
// grid reads gappy there. Tighter tracking → words fit bigger and closer
// (size / space / legibility). The pixel path keeps the true bitmap grid.
const FONT_METRICS_TIGHT = { unifont: { adv: 7, h: 16 } };
const fontMetrics = (font) =>
  (hiRes && FONT_METRICS_TIGHT[font]) || FONT_METRICS[font] || FONT_METRICS._default;

function cellFont() {
  return mode === "word" ? "unifont" : null; // null → default font_1
}

// Fit a label inside a cell at the LARGEST integer scale the cell allows —
// readability first, on every display. Words are still sacred: always the
// whole word on ONE line, never split / hyphenated. A word that would
// overflow even at scale 1 refits on the tile's DIAGONAL instead — 45° buys
// √2 more run — and drops to fractional scale when even that is tight (the
// hd layer rasterizes any px size; long dannom words live here).
const CELL_TEXT_MAX = 6; // sanity cap on cell label scale
const fitScratch = { lines: [""], size: 1, lineH: 10, width: 0, height: 0, font: null, rot: 0 };
function fitText(str, cell, font) {
  const { adv, h } = fontMetrics(font);
  const inner = max(8, cell - CELL_MARGIN * 2);
  const across = inner / (str.length * adv); // scale that fits horizontally
  let size, rot = 0;
  if (across >= 1) {
    size = max(1, min(CELL_TEXT_MAX, floor(min(across, inner / h))));
  } else {
    // A w×h label rotated 45° spans (w+h)/√2 — solve that for scale.
    rot = -45; // up-slope: reads bottom-left → top-right
    size = min(CELL_TEXT_MAX, (inner * Math.SQRT2) / (str.length * adv + h));
    if (size >= 1) size = floor(size);
  }
  // One shared scratch object — callers consume it synchronously, so this
  // avoids ~30 small allocations per frame.
  fitScratch.lines[0] = str;
  fitScratch.size = size;
  fitScratch.lineH = h * size;
  fitScratch.width = str.length * adv * size;
  fitScratch.height = h * size;
  fitScratch.font = font;
  fitScratch.rot = rot;
  return fitScratch;
}

// Draw a (possibly two-line) cell label, centered in the cell with an optional
// jitter offset + alpha. Shared by the live board and the wrap-preview ghosts.
// The pixel path (previews/icons) can't rotate or fractional-scale bitmap
// glyphs, so a diagonal fit falls back to the old scale-1 seam bleed there.
function drawCellText(ink, txt, cx, cy, cell, font, col, alpha = 255, jx = 0, jy = 0) {
  const fit = fitText(txt, cell, font);
  const { adv } = fontMetrics(font);
  const paint = alpha >= 255 ? ink(...col) : ink(col[0], col[1], col[2], alpha);
  const rot = paint.hd ? fit.rot : 0;
  const size = paint.hd ? fit.size : max(1, floor(fit.size));
  const lineH = (fit.lineH / fit.size) * size;
  const baseY = cy + (cell - lineH * fit.lines.length) / 2 + jy;
  fit.lines.forEach((ln, i) => {
    const lw = ln.length * adv * size;
    const lx = cx + (cell - lw) / 2 + jx;
    const ly = baseY + i * lineH;
    const pos = rot ? { x: lx, y: ly, size, rot } : { x: lx, y: ly, size };
    paint.write(ln, pos, undefined, undefined, false, font || undefined);
  });
}

// 🎵 Note model (notenom — `mode === "note"`) ────────────────────────────────
const LETTERS = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
// Each pitch class gets a fixed hue so the board reads like a little keyboard.
const NOTE_COLORS = {
  C: [255, 105, 97], "C#": [255, 150, 80], D: [255, 205, 90], "D#": [200, 225, 90],
  E: [120, 220, 110], F: [90, 215, 175], "F#": [90, 205, 220], G: [95, 165, 255],
  "G#": [150, 150, 255], A: [195, 130, 255], "A#": [235, 120, 220], B: [255, 120, 170],
};
function noteColor(letter) {
  return themedText(NOTE_COLORS[letter] || [220, 220, 235]);
}
function noteTile(letter) {
  return tileTint(NOTE_COLORS[letter] || [220, 220, 235]);
}
function noteLabel(letter, oct) {
  return letter + oct; // e.g. "C4", "F#5"
}
function cellNote(cell) {
  return noteLabel(cell.letter, cell.oct).toLowerCase();
}

// 🎼 Note rules — one per board. `test(letter, oct)` decides correctness, `say`
// is spoken on board start, and `scale` is the ascending run played underneath.
const NOTE_ROUNDS = [
  {
    label: "C MAJOR", say: "c major",
    test: (l) => ["C", "D", "E", "F", "G", "A", "B"].includes(l),
    scale: ["C4", "D4", "E4", "F4", "G4", "A4", "B4", "C5"],
  },
  {
    label: "A MINOR", say: "a minor",
    test: (l) => ["A", "B", "C", "D", "E", "F", "G"].includes(l),
    scale: ["A3", "B3", "C4", "D4", "E4", "F4", "G4", "A4"],
  },
  {
    label: "G MAJOR", say: "g major",
    test: (l) => ["G", "A", "B", "C", "D", "E", "F#"].includes(l),
    scale: ["G3", "A3", "B3", "C4", "D4", "E4", "F#4", "G4"],
  },
  {
    label: "SHARPS", say: "sharps",
    test: (l) => l.includes("#"),
    scale: ["C#4", "D#4", "F#4", "G#4", "A#4", "C#5"],
  },
  {
    label: "C CHORD", say: "c chord",
    test: (l) => ["C", "E", "G"].includes(l),
    scale: ["C4", "E4", "G4", "C5"],
  },
  {
    label: "HIGH", say: "high notes",
    test: (l, o) => o >= 5,
    scale: ["C5", "E5", "G5", "C6"],
  },
  {
    label: "LOW", say: "low notes",
    test: (l, o) => o <= 3,
    scale: ["C3", "E3", "G3", "C4"],
  },
  {
    label: "D MAJOR", say: "d major",
    test: (l) => ["D", "E", "F#", "G", "A", "B", "C#"].includes(l),
    scale: ["D4", "E4", "F#4", "G4", "A4", "B4", "C#5", "D5"],
  },
  {
    label: "E MINOR", say: "e minor",
    test: (l) => ["E", "F#", "G", "A", "B", "C", "D"].includes(l),
    scale: ["E4", "F#4", "G4", "A4", "B4", "C5", "D5", "E5"],
  },
  {
    label: "A MAJOR", say: "a major",
    test: (l) => ["A", "B", "C#", "D", "E", "F#", "G#"].includes(l),
    scale: ["A3", "B3", "C#4", "D4", "E4", "F#4", "G#4", "A4"],
  },
  {
    label: "G CHORD", say: "g chord",
    test: (l) => ["G", "B", "D"].includes(l),
    scale: ["G4", "B4", "D5", "G5"],
  },
  {
    label: "F CHORD", say: "f chord",
    test: (l) => ["F", "A", "C"].includes(l),
    scale: ["F4", "A4", "C5", "F5"],
  },
  {
    label: "AM CHORD", say: "a minor chord",
    test: (l) => ["A", "C", "E"].includes(l),
    scale: ["A3", "C4", "E4", "A4"],
  },
  {
    label: "D CHORD", say: "d chord",
    test: (l) => ["D", "F#", "A"].includes(l),
    scale: ["D4", "F#4", "A4", "D5"],
  },
  {
    label: "NATURALS", say: "natural notes",
    test: (l) => !l.includes("#"),
    scale: ["C4", "D4", "E4", "F4", "G4", "A4", "B4", "C5"],
  },
];

// Spanish → English (spoken on each munch in mexinom).
const TRANSLATE = {
  taco: "taco", mole: "mole sauce", elote: "corn", chile: "chili", salsa: "salsa",
  queso: "cheese", tamal: "tamale", atole: "atole", sopa: "soup", pan: "bread", frijol: "bean",
  gato: "cat", perro: "dog", burro: "donkey", coyote: "coyote", puma: "puma", lobo: "wolf",
  gallo: "rooster", toro: "bull", mono: "monkey", oso: "bear", pato: "duck",
  rojo: "red", azul: "blue", gris: "gray", rosa: "pink", verde: "green", negro: "black",
  blanco: "white", oro: "gold", cafe: "brown", morado: "purple",
  mango: "mango", guayaba: "guava", lima: "lime", melon: "melon", papaya: "papaya",
  mora: "berry", higo: "fig", coco: "coconut", pera: "pear", uva: "grape",
  pinata: "piñata", globo: "balloon", dulce: "candy", musica: "music", baile: "dance",
  feria: "fair", vela: "candle", regalo: "gift", fuego: "fire",
  mesa: "table", silla: "chair", reloj: "clock", libro: "book", tren: "train", gorra: "cap",
  clave: "key", vaso: "glass", foco: "bulb", muro: "wall", red: "net", taza: "cup", sol: "sun",
  casa: "house", jugo: "juice", flor: "flower", mar: "sea", pie: "foot", ojo: "eye", mano: "hand",
  dedo: "finger", luz: "light", rio: "river", sal: "salt", lapiz: "pencil", bota: "boot",
  clavo: "nail", papel: "paper", caja: "box",
  nopal: "cactus", pozole: "hominy stew", birria: "birria stew", carne: "meat",
  pollo: "chicken", arroz: "rice", tortilla: "tortilla",
  leon: "lion", tigre: "tiger", vaca: "cow", rana: "frog", pez: "fish",
  aguila: "eagle", conejo: "rabbit",
  amarillo: "yellow", naranja: "orange", violeta: "violet", plata: "silver",
  turquesa: "turquoise", celeste: "sky blue", lila: "lilac", vino: "maroon",
  fresa: "strawberry", limon: "lemon", platano: "banana", sandia: "watermelon",
  durazno: "peach", ciruela: "plum", pina: "pineapple", manzana: "apple",
  confeti: "confetti", banda: "band", mariachi: "mariachi", cohete: "firework",
  premio: "prize", fiesta: "party", corona: "crown", canto: "singing", amigo: "friend",
  boca: "mouth", nariz: "nose", oreja: "ear", cabeza: "head", pelo: "hair",
  cara: "face", brazo: "arm", pierna: "leg", diente: "tooth", lengua: "tongue",
  cuello: "neck", rodilla: "knee", codo: "elbow", corazon: "heart",
  cama: "bed", puerta: "door", ventana: "window", plato: "plate", cuchara: "spoon",
  tenedor: "fork", cuchillo: "knife", olla: "pot", sarten: "pan", escoba: "broom",
  toalla: "towel", jabon: "soap", llave: "key", lampara: "lamp", espejo: "mirror", cobija: "blanket",
  luna: "moon", estrella: "star", nube: "cloud", lluvia: "rain", viento: "wind",
  montana: "mountain", arbol: "tree", hoja: "leaf", roca: "rock", arena: "sand",
  lago: "lake", cielo: "sky", nieve: "snow", tierra: "earth",
  mama: "mom", papa: "dad", hijo: "son", hija: "daughter", hermano: "brother",
  hermana: "sister", abuelo: "grandfather", abuela: "grandmother", tio: "uncle",
  tia: "aunt", primo: "cousin", prima: "cousin", nieto: "grandson", nieta: "granddaughter",
  esposo: "husband", esposa: "wife", bebe: "baby", sobrino: "nephew",
};

// 🇩🇰 Danish → English (spoken on each munch in dannom). Real Danish spelling
// (æ ø å) — the board renders these with MatrixChunky8, which has the glyphs.
const TRANSLATE_DA = {
  "brød": "bread", ost: "cheese", "pølse": "sausage", kage: "cake", suppe: "soup",
  "smør": "butter", fisk: "fish", salt: "salt", sukker: "sugar", "grød": "porridge", "æg": "egg",
  kat: "cat", hund: "dog", ko: "cow", gris: "pig", hest: "horse", and: "duck",
  ravn: "raven", ulv: "wolf", "bjørn": "bear", "ræv": "fox", mus: "mouse", "får": "sheep",
  "rød": "red", "blå": "blue", "grøn": "green", gul: "yellow", sort: "black",
  hvid: "white", "grå": "gray", brun: "brown",
  "æble": "apple", "pære": "pear", blomme: "plum", banan: "banana", drue: "grape",
  fersken: "peach", citron: "lemon", melon: "melon", "bær": "berry",
  lys: "candle", te: "tea", bog: "book", sofa: "sofa", "tæppe": "blanket",
  ild: "fire", ven: "friend", kaffe: "coffee", hus: "house", sten: "stone",
  tog: "train", stol: "chair", bord: "table", ur: "clock", glas: "glass",
  sko: "shoe", "dør": "door", vej: "road", regn: "rain", vind: "wind",
  mel: "flour", "mælk": "milk", sild: "herring", "kød": "meat", honning: "honey",
  ged: "goat", "høne": "hen", fugl: "bird", "ørn": "eagle",
  lilla: "purple", pink: "pink", orange: "orange", turkis: "turquoise",
  guld: "gold", "sølv": "silver", beige: "beige", "lyserød": "light pink",
  "jordbær": "strawberry", "hindbær": "raspberry", "kirsebær": "cherry",
  appelsin: "orange", ananas: "pineapple", "blåbær": "blueberry", abrikos: "apricot",
  pude: "pillow", kakao: "cocoa", pejs: "fireplace", "bål": "bonfire",
  sok: "sock", hue: "beanie", kram: "hug", smil: "smile",
  "hånd": "hand", fod: "foot", "øje": "eye", "øre": "ear", "næse": "nose",
  mund: "mouth", arm: "arm", ben: "leg", "hår": "hair", hoved: "head",
  finger: "finger", "tå": "toe", hals: "neck", "knæ": "knee", tand: "tooth", ryg: "back",
  seng: "bed", lampe: "lamp", gulv: "floor", "væg": "wall", spejl: "mirror",
  skab: "cupboard", kop: "cup", kniv: "knife", ske: "spoon", gryde: "pot",
  gaffel: "fork", kande: "jug", vindue: "window",
  sol: "sun", "måne": "moon", "sø": "lake", hav: "sea", skov: "forest",
  "træ": "tree", blad: "leaf", flod: "river", bjerg: "mountain", himmel: "sky",
  sky: "cloud", stjerne: "star", strand: "beach", mark: "field", "græs": "grass", blomst: "flower",
  bluse: "blouse", bukser: "pants", "trøje": "sweater", kjole: "dress", hat: "hat",
  frakke: "coat", jakke: "jacket", vest: "vest", slips: "tie", "bælte": "belt",
  "støvle": "boot", skjorte: "shirt", handske: "glove",
};

// 🇷🇺 Russian → English (spoken on each munch in rusnom). Cyrillic — the board
// renders these with MatrixChunky8, which carries the full Russian alphabet.
const TRANSLATE_RU = {
  "хлеб": "bread", "сыр": "cheese", "суп": "soup", "мясо": "meat", "рыба": "fish",
  "сахар": "sugar", "соль": "salt", "каша": "porridge", "яйцо": "egg", "масло": "butter", "торт": "cake",
  "кот": "cat", "собака": "dog", "корова": "cow", "конь": "horse", "волк": "wolf",
  "лиса": "fox", "медведь": "bear", "заяц": "hare", "мышь": "mouse", "коза": "goat", "бык": "bull", "овца": "sheep",
  "красный": "red", "синий": "blue", "зелёный": "green", "жёлтый": "yellow", "чёрный": "black",
  "белый": "white", "серый": "gray", "розовый": "pink",
  "яблоко": "apple", "груша": "pear", "слива": "plum", "банан": "banana", "виноград": "grape",
  "лимон": "lemon", "персик": "peach", "дыня": "melon", "ягода": "berry",
  "солнце": "sun", "луна": "moon", "река": "river", "море": "sea", "гора": "mountain",
  "небо": "sky", "дождь": "rain", "снег": "snow", "ветер": "wind", "звезда": "star",
  "стол": "table", "стул": "chair", "книга": "book", "окно": "window", "дверь": "door",
  "ключ": "key", "часы": "clock", "лампа": "lamp", "пол": "floor", "стена": "wall",
  "дом": "house", "лес": "forest", "сад": "garden",
  "молоко": "milk", "борщ": "borscht", "блин": "pancake", "пирог": "pie", "мёд": "honey",
  "лев": "lion", "тигр": "tiger", "слон": "elephant", "олень": "deer",
  "оранжевый": "orange", "фиолетовый": "purple", "коричневый": "brown", "голубой": "light blue",
  "золотой": "gold", "серебряный": "silver", "бежевый": "beige", "бирюзовый": "turquoise",
  "апельсин": "orange", "мандарин": "tangerine", "ананас": "pineapple", "вишня": "cherry",
  "клубника": "strawberry", "малина": "raspberry", "абрикос": "apricot",
  "облако": "cloud", "трава": "grass", "цветок": "flower", "дерево": "tree", "озеро": "lake",
  "голова": "head", "рука": "hand", "нога": "leg", "глаз": "eye", "нос": "nose", "рот": "mouth",
  "ухо": "ear", "зуб": "tooth", "волосы": "hair", "палец": "finger", "живот": "belly",
  "спина": "back", "шея": "neck", "губа": "lip", "плечо": "shoulder", "колено": "knee",
  "кровать": "bed", "шкаф": "wardrobe", "диван": "sofa", "зеркало": "mirror", "ковёр": "carpet",
  "полка": "shelf", "чашка": "cup", "тарелка": "plate",
  "рубашка": "shirt", "футболка": "t-shirt", "платье": "dress", "юбка": "skirt", "шапка": "cap",
  "шарф": "scarf", "куртка": "jacket", "пальто": "coat", "свитер": "sweater", "пояс": "belt",
  "галстук": "tie", "носок": "sock", "перчатка": "glove", "кофта": "cardigan", "шляпа": "hat", "майка": "tank top",
  "мама": "mom", "папа": "dad", "брат": "brother", "сестра": "sister", "сын": "son", "дочь": "daughter",
  "бабушка": "grandmother", "дедушка": "grandfather", "дядя": "uncle", "тётя": "aunt",
  "внук": "grandson", "внучка": "granddaughter", "муж": "husband", "жена": "wife", "мать": "mother", "отец": "father",
};

// 😼 Catnom glossary — spoken on each munch (slang → plain meaning, code →
// one-line gloss), so eating a word teaches it. Words without an entry are
// just echoed back.
const TRANSLATE_CAT = {
  // slang
  rizz: "charisma", sus: "suspicious", cap: "a lie", goat: "greatest of all time",
  mid: "just okay", drip: "a stylish look", slay: "did amazing", bet: "okay, deal",
  yeet: "throw it hard", vibe: "a feeling", fam: "your people", bussin: "really good",
  // code
  bug: "a mistake in code", loop: "code that repeats", git: "saves code history",
  byte: "eight bits", bool: "true or false", null: "nothing at all",
  array: "a list of things", stack: "last in, first out", repo: "where code lives",
  pixel: "one screen dot", code: "instructions", crash: "when code dies",
  // aesthetic computer pieces
  prompt: "where you type", paint: "draw pictures", chat: "talk to everyone",
  notepat: "play notes", camera: "take pictures", wand: "draw in 3 d",
  tone: "make a tone", smear: "smudge the paint", bleep: "tap to beep",
  wipe: "clear the screen", melody: "play a tune",
  // internet
  wifi: "wireless internet", meme: "a shared joke", app: "a small program",
  link: "tap to go there", post: "share it online", ping: "a quick hello",
  spam: "junk messages", lag: "a slow connection", dm: "a direct message",
  emoji: "a tiny picture", blog: "an online journal",
  // slang
  lit: "exciting", flex: "show off", bop: "a great song", salty: "bitter or upset",
  // code
  hash: "a scrambled id", cache: "a fast stash", debug: "hunt for bugs", class: "a code blueprint",
  // aesthetic computer pieces
  clock: "shows the time", line: "draw a line", oval: "draw an oval",
  brush: "paint with a brush", spray: "spray paint",
  // vibes
  lofi: "soft lo-fi sound", zen: "calm and clear", mellow: "relaxed and gentle",
  serene: "calm and peaceful",
  // keys
  esc: "cancel or exit", ctrl: "control key", alt: "the alt key",
  cmd: "the command key", opt: "the option key", fn: "function key",
  // internet
  url: "a web address", feed: "a stream of posts", like: "a tap of approval",
  wiki: "an editable site", cloud: "storage online",
  // snacks
  pretzel: "a knotted salty snack", nachos: "chips with cheese",
  fudge: "soft chocolate candy", jerky: "dried meat snack",
  // space
  comet: "an icy space ball", orbit: "a path around", galaxy: "a sea of stars",
  nebula: "a space gas cloud", meteor: "a shooting star", cosmos: "all of space",
  void: "empty dark space",
  // music
  tempo: "the speed of music", chord: "notes played together", verse: "a song section",
  chorus: "the repeated part", remix: "a reworked song", riff: "a catchy phrase",
  pitch: "how high or low", rhythm: "the timing pattern", snare: "a sharp drum",
  synth: "an electronic sound", scale: "a ladder of notes", bass: "the low sound",
  // emotions
  joy: "great happiness", fear: "feeling afraid", brave: "full of courage",
  proud: "happy with yourself",
  // tools
  wrench: "turns nuts and bolts", pliers: "grips and bends", chisel: "carves wood or stone",
  clamp: "holds things tight", level: "checks if flat", sander: "smooths surfaces",
  file: "smooths rough edges", bolt: "a metal fastener",
};

function shuffle(arr, rnd) {
  for (let i = arr.length - 1; i > 0; i -= 1) {
    const j = rnd(i + 1);
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
  return arr;
}

// Word categories: correct words + matching distractors.
const WORD_ROUNDS = [
  {
    label: "ANIMALS", say: "animals",
    correct: ["cat", "dog", "fox", "owl", "bee", "cow", "ant", "ram", "hen", "elk", "pig", "bat", "rat", "hog", "ape", "doe", "goat", "wolf", "frog", "crab"],
    wrong: ["cup", "rug", "jar", "fan", "pen", "map", "key", "box", "hat", "mug", "net", "log", "cap", "tin"],
  },
  {
    label: "COLORS", say: "colors",
    correct: ["red", "tan", "pink", "blue", "gold", "lime", "teal", "gray", "rose", "navy", "cyan", "green", "brown", "black", "white", "olive", "coral", "mint", "beige", "amber"],
    wrong: ["leg", "oak", "bug", "dot", "sun", "egg", "tin", "pod", "cup", "jar", "box", "fan", "rug", "log"],
  },
  {
    label: "FRUITS", say: "fruits",
    correct: ["plum", "lime", "pear", "fig", "kiwi", "date", "lemon", "apple", "grape", "melon", "peach", "mango", "berry", "cherry", "banana", "orange", "guava", "papaya", "lychee", "apricot"],
    wrong: ["desk", "sock", "lamp", "fork", "coin", "boot", "rope", "drum", "vase", "nail", "brick", "spoon", "chair", "clock"],
  },
  {
    label: "RHYMES", say: "words that rhyme with cat",
    correct: ["cat", "hat", "bat", "rat", "mat", "sat", "fat", "vat", "pat", "flat", "chat", "gnat", "spat", "brat", "scat", "slat", "that", "splat", "drat", "prat"],
    wrong: ["cot", "hit", "bet", "rot", "mug", "sit", "fan", "van", "pen", "top", "cup", "dog", "sun", "box"],
  },
  {
    label: "S-WORDS", say: "words starting with s",
    correct: ["sun", "sky", "sea", "sip", "sad", "sob", "saw", "sit", "sum", "ski", "spy", "six", "sew", "set", "say", "son", "sir", "sap", "soy", "sag"],
    wrong: ["fun", "dry", "tea", "tip", "mad", "job", "jaw", "fit", "gum", "fly", "cry", "hop", "run", "bed"],
  },
  {
    label: "DOUBLES", say: "words with double letters",
    correct: ["bee", "see", "moo", "egg", "add", "odd", "ebb", "too", "zoo", "off", "ill", "boo", "all", "inn", "tree", "free", "book", "food", "deep", "feet"],
    wrong: ["bet", "set", "mob", "ego", "ace", "old", "elf", "tot", "zip", "oaf", "ink", "bow", "cat", "fan"],
  },
  {
    label: "BODY", say: "body parts",
    correct: ["arm", "leg", "ear", "eye", "toe", "hip", "jaw", "rib", "lip", "gum", "shin", "knee", "hand", "foot", "nose", "chin", "neck", "back", "head", "heel"],
    wrong: ["cup", "jar", "fan", "pen", "map", "box", "rug", "mug", "net", "log", "lamp", "sock", "fork", "coin"],
  },
  {
    label: "HOME", say: "household objects",
    correct: ["cup", "jar", "fan", "pen", "map", "box", "rug", "mug", "net", "lamp", "sock", "fork", "bowl", "dish", "sofa", "desk", "chair", "clock", "broom", "towel"],
    wrong: ["cat", "dog", "fox", "arm", "leg", "ear", "eye", "toe", "cow", "bee", "pig", "owl", "ram", "hen"],
  },
  {
    label: "FOOD", say: "food",
    correct: ["bread", "rice", "egg", "soup", "cake", "meat", "milk", "bean", "corn", "fish", "ham", "jam", "taco", "pie", "stew", "toast", "honey", "pasta", "salad", "cheese"],
    wrong: ["desk", "lamp", "sock", "fork", "coin", "boot", "rope", "drum", "vase", "nail", "brick", "clock", "chair", "spoon"],
  },
  {
    label: "WEATHER", say: "weather",
    correct: ["rain", "snow", "wind", "fog", "sun", "cloud", "storm", "ice", "hail", "frost", "mist", "sleet", "gale", "dew", "heat", "cold", "sunny", "windy", "cloudy", "rainy"],
    wrong: ["desk", "sock", "fork", "coin", "boot", "rope", "drum", "vase", "nail", "brick", "clock", "chair", "spoon", "lamp"],
  },
  {
    label: "VERBS", say: "action words",
    correct: ["run", "jump", "hop", "sit", "eat", "sip", "nap", "dig", "hug", "kick", "walk", "swim", "jog", "spin", "clap", "sing", "read", "cook", "wave", "throw"],
    wrong: ["cup", "jar", "fan", "desk", "lamp", "sock", "rock", "coin", "tree", "book", "door", "wall", "roof", "chair"],
  },
  {
    label: "SPACE", say: "outer space",
    correct: ["star", "moon", "sun", "mars", "comet", "orbit", "space", "alien", "rocket", "planet", "galaxy", "venus", "saturn", "pluto", "meteor", "cosmos", "nebula", "lunar", "solar", "crater"],
    wrong: ["cup", "jar", "fan", "pen", "desk", "lamp", "sock", "fork", "coin", "boot", "chair", "clock", "spoon", "broom"],
  },
];

// 🇲🇽 Mexinom — Spanish word categories with a Mexican-culture flavor
// (street food, lotería-style animals, fiesta colors, market fruit). Plain
// ASCII (no accents) so the bitmap font renders cleanly.
const ES_WORD_ROUNDS = [
  {
    label: "COMIDA", say: "comida mexicana",
    correct: ["taco", "mole", "elote", "chile", "salsa", "queso", "tamal", "atole", "sopa", "pan", "frijol", "nopal", "pozole", "birria", "carne", "pollo", "arroz", "tortilla"],
    wrong: ["mesa", "silla", "reloj", "libro", "tren", "gorra", "clave", "vaso", "foco", "muro", "red", "taza"],
  },
  {
    label: "ANIMALES", say: "animales",
    correct: ["gato", "perro", "burro", "coyote", "puma", "lobo", "gallo", "toro", "mono", "oso", "pato", "leon", "tigre", "vaca", "rana", "pez", "aguila", "conejo"],
    wrong: ["mesa", "taza", "sol", "casa", "jugo", "reloj", "silla", "vaso", "tren", "flor", "muro", "foco"],
  },
  {
    label: "COLORES", say: "colores",
    correct: ["rojo", "azul", "gris", "rosa", "verde", "negro", "blanco", "oro", "cafe", "morado", "amarillo", "naranja", "violeta", "plata", "turquesa", "celeste", "lila", "vino"],
    wrong: ["pato", "mesa", "mar", "pie", "ojo", "mano", "dedo", "casa", "luz", "rio", "sal", "vaso"],
  },
  {
    label: "FRUTAS", say: "frutas",
    correct: ["mango", "guayaba", "lima", "melon", "papaya", "mora", "higo", "coco", "pera", "uva", "fresa", "limon", "platano", "sandia", "durazno", "ciruela", "pina", "manzana"],
    wrong: ["mesa", "sol", "tren", "lapiz", "bota", "vaso", "gorra", "clavo", "silla", "foco", "red", "reloj"],
  },
  {
    label: "FIESTA", say: "cosas de fiesta",
    correct: ["pinata", "globo", "dulce", "musica", "baile", "feria", "vela", "regalo", "fuego", "confeti", "banda", "mariachi", "cohete", "premio", "fiesta", "corona", "canto", "amigo"],
    wrong: ["mesa", "tren", "reloj", "muro", "foco", "clavo", "papel", "caja", "vaso", "red", "silla", "libro"],
  },
  {
    label: "CUERPO", say: "partes del cuerpo",
    correct: ["ojo", "mano", "pie", "dedo", "boca", "nariz", "oreja", "cabeza", "pelo", "cara", "brazo", "pierna", "diente", "lengua", "cuello", "rodilla", "codo", "corazon"],
    wrong: ["mesa", "silla", "reloj", "libro", "tren", "taza", "vaso", "foco", "clavo", "papel", "caja", "gorra"],
  },
  {
    label: "CASA", say: "cosas de la casa",
    correct: ["mesa", "silla", "cama", "puerta", "ventana", "plato", "cuchara", "tenedor", "cuchillo", "olla", "sarten", "escoba", "toalla", "jabon", "llave", "lampara", "espejo", "cobija"],
    wrong: ["gato", "perro", "taco", "mango", "sol", "mar", "flor", "rio", "toro", "pato", "oso", "luz"],
  },
  {
    label: "NATURALEZA", say: "cosas de la naturaleza",
    correct: ["sol", "mar", "flor", "rio", "luna", "estrella", "nube", "lluvia", "viento", "montana", "arbol", "hoja", "roca", "arena", "lago", "cielo", "nieve", "tierra"],
    wrong: ["mesa", "silla", "reloj", "libro", "tren", "taza", "vaso", "foco", "gorra", "clavo", "papel", "caja"],
  },
  {
    label: "FAMILIA", say: "la familia",
    correct: ["mama", "papa", "hijo", "hija", "hermano", "hermana", "abuelo", "abuela", "tio", "tia", "primo", "prima", "nieto", "nieta", "esposo", "esposa", "bebe", "sobrino"],
    wrong: ["mesa", "silla", "taco", "mango", "sol", "gato", "perro", "flor", "vaso", "reloj", "libro", "tren"],
  },
];

// 🇩🇰 Dannom — Danish word categories (mad, dyr, farver, frugt, hygge) in real
// Danish spelling (æ ø å). Rendered with MatrixChunky8 (has the glyphs); each
// munch speaks the English translation aloud (see TRANSLATE_DA).
const DA_WORD_ROUNDS = [
  {
    label: "MAD", say: "danish food",
    correct: ["brød", "ost", "pølse", "kage", "suppe", "smør", "fisk", "salt", "sukker", "grød", "æg", "mel", "mælk", "sild", "kød", "honning"],
    wrong: ["bord", "stol", "ur", "bog", "tog", "sko", "glas", "sten", "vej", "dør", "hus"],
  },
  {
    label: "DYR", say: "danish animals",
    correct: ["kat", "hund", "ko", "gris", "hest", "and", "ravn", "ulv", "bjørn", "ræv", "mus", "får", "ged", "høne", "fugl", "ørn"],
    wrong: ["hus", "tæppe", "ur", "bog", "tog", "stol", "glas", "sten", "vej", "dør", "bord"],
  },
  {
    label: "FARVER", say: "danish colors",
    correct: ["rød", "blå", "grøn", "gul", "sort", "hvid", "grå", "brun", "lilla", "pink", "orange", "turkis", "guld", "sølv", "beige", "lyserød"],
    wrong: ["kat", "bord", "regn", "vind", "hus", "sten", "sko", "glas", "vej", "ur", "tog"],
  },
  {
    label: "FRUGT", say: "danish fruit",
    correct: ["æble", "pære", "blomme", "banan", "drue", "fersken", "citron", "melon", "bær", "jordbær", "hindbær", "kirsebær", "appelsin", "ananas", "blåbær", "abrikos"],
    wrong: ["bord", "stol", "ur", "bog", "tog", "sko", "glas", "sten", "vej", "dør", "hus"],
  },
  {
    label: "HYGGE", say: "cozy danish things",
    correct: ["lys", "te", "bog", "sofa", "tæppe", "ild", "ven", "kaffe", "pude", "kakao", "pejs", "bål", "sok", "hue", "kram", "smil"],
    wrong: ["regn", "vind", "sten", "tog", "ur", "glas", "sko", "dør", "vej", "hus", "bord"],
  },
  {
    label: "KROP", say: "danish body parts",
    correct: ["hånd", "fod", "øje", "øre", "næse", "mund", "arm", "ben", "hår", "hoved", "finger", "tå", "hals", "knæ", "tand", "ryg"],
    wrong: ["bord", "stol", "ur", "bog", "tog", "sko", "glas", "sten", "vej", "dør", "hus"],
  },
  {
    label: "HUS", say: "danish household objects",
    correct: ["stol", "bord", "seng", "lampe", "dør", "gulv", "væg", "spejl", "skab", "kop", "kniv", "ske", "gryde", "gaffel", "kande", "vindue"],
    wrong: ["kat", "hund", "fisk", "regn", "vind", "sten", "vej", "bær", "drue", "gris", "ræv"],
  },
  {
    label: "NATUR", say: "danish nature",
    correct: ["sol", "måne", "sø", "hav", "skov", "træ", "blad", "flod", "bjerg", "himmel", "sky", "stjerne", "strand", "mark", "græs", "blomst"],
    wrong: ["bord", "stol", "ur", "bog", "tog", "sko", "glas", "dør", "hus", "kop", "seng"],
  },
  {
    label: "TØJ", say: "danish clothing",
    correct: ["sko", "bluse", "bukser", "trøje", "kjole", "hat", "hue", "sok", "frakke", "jakke", "vest", "slips", "bælte", "støvle", "skjorte", "handske"],
    wrong: ["brød", "ost", "kat", "hund", "fisk", "sten", "træ", "sol", "hav", "regn", "vind"],
  },
];

// 🇷🇺 Rusnom — Russian word categories (еда, животные, цвета, фрукты, природа)
// in Cyrillic. Rendered with MatrixChunky8 (full Russian alphabet); each munch
// speaks the English translation aloud (see TRANSLATE_RU).
const RU_WORD_ROUNDS = [
  {
    label: "ЕДА", say: "russian food",
    correct: ["хлеб", "сыр", "суп", "мясо", "рыба", "сахар", "соль", "каша", "яйцо", "масло", "торт", "молоко", "борщ", "блин", "пирог", "мёд"],
    wrong: ["стол", "стул", "книга", "окно", "дверь", "ключ", "часы", "лампа", "пол", "стена", "дом"],
  },
  {
    label: "ЖИВОТНЫЕ", say: "russian animals",
    correct: ["кот", "собака", "корова", "конь", "волк", "лиса", "медведь", "заяц", "мышь", "коза", "бык", "овца", "лев", "тигр", "слон", "олень"],
    wrong: ["стол", "окно", "книга", "дверь", "ключ", "часы", "дом", "лампа", "пол", "стена", "стул"],
  },
  {
    label: "ЦВЕТА", say: "russian colors",
    correct: ["красный", "синий", "зелёный", "жёлтый", "чёрный", "белый", "серый", "розовый", "оранжевый", "фиолетовый", "коричневый", "голубой", "золотой", "серебряный", "бежевый", "бирюзовый"],
    wrong: ["кот", "стол", "окно", "дом", "ключ", "часы", "мышь", "рыба", "лес", "сад", "книга"],
  },
  {
    label: "ФРУКТЫ", say: "russian fruit",
    correct: ["яблоко", "груша", "слива", "банан", "виноград", "лимон", "персик", "дыня", "ягода", "апельсин", "мандарин", "ананас", "вишня", "клубника", "малина", "абрикос"],
    wrong: ["стол", "стул", "книга", "окно", "дверь", "ключ", "часы", "дом", "пол", "стена", "лампа"],
  },
  {
    label: "ПРИРОДА", say: "russian nature",
    correct: ["солнце", "луна", "река", "море", "лес", "гора", "небо", "дождь", "снег", "ветер", "звезда", "облако", "трава", "цветок", "дерево", "озеро"],
    wrong: ["стол", "книга", "окно", "ключ", "часы", "дом", "лампа", "пол", "стена", "дверь", "стул"],
  },
  {
    label: "ТЕЛО", say: "russian body parts",
    correct: ["голова", "рука", "нога", "глаз", "нос", "рот", "ухо", "зуб", "волосы", "палец", "живот", "спина", "шея", "губа", "плечо", "колено"],
    wrong: ["стол", "стул", "окно", "дверь", "ключ", "часы", "лампа", "дом", "книга", "пол", "стена"],
  },
  {
    label: "ДОМ", say: "russian household things",
    correct: ["стол", "стул", "книга", "окно", "дверь", "ключ", "часы", "лампа", "кровать", "шкаф", "диван", "зеркало", "ковёр", "полка", "чашка", "тарелка"],
    wrong: ["кот", "собака", "рыба", "солнце", "луна", "река", "лес", "гора", "снег", "дождь", "хлеб"],
  },
  {
    label: "ОДЕЖДА", say: "russian clothing",
    correct: ["рубашка", "футболка", "платье", "юбка", "шапка", "шарф", "куртка", "пальто", "свитер", "пояс", "галстук", "носок", "перчатка", "кофта", "шляпа", "майка"],
    wrong: ["хлеб", "сыр", "кот", "собака", "рыба", "солнце", "река", "лес", "снег", "стол", "окно"],
  },
  {
    label: "СЕМЬЯ", say: "russian family",
    correct: ["мама", "папа", "брат", "сестра", "сын", "дочь", "бабушка", "дедушка", "дядя", "тётя", "внук", "внучка", "муж", "жена", "мать", "отец"],
    wrong: ["стол", "окно", "ключ", "часы", "лампа", "солнце", "река", "лес", "хлеб", "кот", "дом"],
  },
];

// 😼 Catnom — the Categories parlor game, nom-style: every board is one
// category and you eat what belongs. Half the rounds are AC / code flavored,
// half are everyday gen-z language — all still simple word-learning at heart.
const CAT_WORD_ROUNDS = [
  {
    label: "SLANG", say: "slang words",
    correct: ["rizz", "sus", "cap", "goat", "mid", "drip", "slay", "bet", "yeet", "vibe", "fam", "bussin", "lit", "flex", "bop", "salty"],
    wrong: ["desk", "lamp", "fork", "sock", "rug", "mop", "jar", "pail", "hose", "rake", "crate", "broom"],
  },
  {
    label: "CODE", say: "coding words",
    correct: ["bug", "loop", "git", "byte", "bool", "null", "array", "stack", "repo", "pixel", "code", "crash", "hash", "cache", "debug", "class"],
    wrong: ["soup", "sand", "lake", "goose", "pony", "barn", "kite", "plum", "wool", "fog", "hay", "moss"],
  },
  {
    label: "PIECES", say: "aesthetic computer pieces",
    correct: ["prompt", "paint", "chat", "notepat", "wand", "tone", "camera", "smear", "bleep", "wipe", "melody", "clock", "line", "oval", "brush", "spray"],
    wrong: ["sofa", "ladder", "pickle", "anchor", "mitten", "faucet", "wallet", "helmet", "sponge", "teapot", "kettle", "basket"],
  },
  {
    label: "VIBES", say: "chill vibes",
    correct: ["chill", "cozy", "calm", "lofi", "zen", "mellow", "soft", "comfy", "peace", "dream", "warm", "quiet", "breezy", "snug", "serene", "gentle"],
    wrong: ["loud", "rush", "panic", "gloom", "harsh", "fuss", "riot", "blare", "jolt", "grind", "chaos", "stress"],
  },
  {
    label: "KEYS", say: "keyboard keys",
    correct: ["esc", "tab", "shift", "enter", "space", "ctrl", "alt", "caps", "home", "end", "del", "fn", "cmd", "opt", "win", "pgup"],
    wrong: ["door", "knob", "bell", "gate", "couch", "vent", "sill", "twig", "pond", "moss", "fern", "brook"],
  },
  {
    label: "ONLINE", say: "internet words",
    correct: ["wifi", "meme", "app", "link", "post", "blog", "ping", "spam", "lag", "dm", "emoji", "url", "feed", "like", "wiki", "cloud"],
    wrong: ["cave", "quilt", "anvil", "plow", "broom", "candle", "stump", "wagon", "shovel", "brick", "saddle", "barrel"],
  },
  {
    label: "SNACKS", say: "snacks",
    correct: ["chips", "candy", "cookie", "donut", "popcorn", "pretzel", "gummy", "nachos", "cracker", "muffin", "fudge", "jerky", "waffle", "taco", "pizza", "fries"],
    wrong: ["brick", "spoon", "towel", "pencil", "ladder", "hammer", "saddle", "bucket", "candle", "pillow", "magnet", "zipper"],
  },
  {
    label: "SPACE", say: "outer space",
    correct: ["moon", "star", "mars", "comet", "planet", "orbit", "galaxy", "meteor", "nebula", "sun", "venus", "saturn", "rocket", "cosmos", "pluto", "void"],
    wrong: ["chair", "spoon", "towel", "fence", "kettle", "jacket", "pillow", "ladder", "mailbox", "sandal", "blender", "wallet"],
  },
  {
    label: "MUSIC", say: "music words",
    correct: ["beat", "tempo", "chord", "note", "drum", "bass", "tune", "verse", "chorus", "remix", "riff", "pitch", "rhythm", "snare", "synth", "scale"],
    wrong: ["spoon", "towel", "fence", "kettle", "jacket", "pillow", "ladder", "wallet", "sandal", "mailbox", "pebble", "blender"],
  },
  {
    label: "EMOTIONS", say: "feelings",
    correct: ["happy", "sad", "angry", "scared", "calm", "proud", "shy", "bored", "joy", "fear", "mad", "glad", "upset", "brave", "tired", "worry"],
    wrong: ["spoon", "fork", "towel", "brick", "ladder", "kettle", "pillow", "wallet", "sandal", "magnet", "bucket", "pebble"],
  },
  {
    label: "TOOLS", say: "tools",
    correct: ["hammer", "wrench", "drill", "saw", "pliers", "screw", "nail", "ruler", "chisel", "clamp", "level", "tape", "sander", "wire", "bolt", "file"],
    wrong: ["apple", "pillow", "jacket", "cookie", "kitten", "bubble", "cloud", "meadow", "blanket", "muffin", "sandal", "teapot"],
  },
];

function newRound({ randInt }) {
  // AC's randInt(n) is inclusive (0..n); wrap it to a 0..n-1 index helper.
  const rnd = (n) => randInt(Math.max(1, n) - 1);
  const total = COLS * ROWS;
  // More correct cells as levels climb, but always solvable & not all-correct.
  const nCorrect = min(total - 4, 5 + level + rnd(3));
  const cells = [];

  if (mode === "note") {
    // 🎵 Build a board of note squares satisfying (or breaking) a musical rule.
    const noteRound = NOTE_ROUNDS[rnd(NOTE_ROUNDS.length)];
    const test = noteRound.test;
    ruleLabel = noteRound.label;
    ruleSpeech = noteRound.say;
    noteScale = noteRound.scale;
    const octs = [3, 4, 5]; // tight pool so HIGH/LOW rules have room to differ
    // 🚫 No repeats: enumerate every letter×octave combo, partition by the
    // rule, shuffle, and deal — each note appears at most once per board.
    const goodPool = [], badPool = [];
    for (const l of LETTERS)
      for (const o of octs) (test(l, o) ? goodPool : badPool).push({ letter: l, oct: o });
    shuffle(goodPool, rnd);
    shuffle(badPool, rnd);
    let nc = min(nCorrect, goodPool.length);
    nc = max(nc, min(goodPool.length, total - badPool.length));
    for (let i = 0; i < nc; i += 1) {
      const g = goodPool[i];
      cells.push({ value: noteLabel(g.letter, g.oct), letter: g.letter, oct: g.oct, correct: true });
    }
    for (let i = 0; i < total - nc; i += 1) {
      const b = badPool[i % badPool.length];
      cells.push({ value: noteLabel(b.letter, b.oct), letter: b.letter, oct: b.oct, correct: false });
    }
  } else if (mode === "word") {
    const rounds = lang === "es" ? ES_WORD_ROUNDS : lang === "da" ? DA_WORD_ROUNDS : lang === "ru" ? RU_WORD_ROUNDS : lang === "cat" ? CAT_WORD_ROUNDS : WORD_ROUNDS;
    const round = rounds[rnd(rounds.length)];
    ruleLabel = round.label;
    ruleSpeech = round.say;
    const goods = shuffle([...round.correct], rnd);
    const bads = shuffle([...round.wrong], rnd);
    // 🚫 No repeats: answers never duplicate (capped to the pool), and when
    // the decoy pool is smaller than the leftover space the board carries
    // more answers instead of cycling decoys — 25 distinct words.
    let nc = min(nCorrect, goods.length);
    nc = min(goods.length, max(nc, total - bads.length));
    for (let i = 0; i < nc; i += 1)
      cells.push({ value: goods[i], correct: true });
    for (let i = 0; i < total - nc; i += 1)
      cells.push({ value: bads[i % bads.length], correct: false });
  } else {
    // Pick a numeric rule + a per-board range. Ranges start at 40 so a 5×5
    // board can always be dealt from DISTINCT values (see the deal below).
    const maxVal = [40, 60, 80][rnd(3)];
    const rules = ["multiples", "factors", "primes", "even", "odd"];
    const pick = rules[rnd(rules.length)];
    let test, label, speech;

    if (pick === "multiples") {
      const n = 2 + rnd(8); // 2..9
      label = "X" + n; // "multiples of n", e.g. X3
      speech = `multiples of ${n}`;
      test = (v) => v % n === 0;
    } else if (pick === "factors") {
      const targets = [12, 16, 18, 20, 24, 36, 48];
      const n = targets[rnd(targets.length)];
      label = "F" + n; // "factors of n", e.g. F24
      speech = `factors of ${n}`;
      test = (v) => n % v === 0;
    } else if (pick === "primes") {
      label = "PRIMES";
      speech = "prime numbers";
      test = (v) => isPrime(v);
    } else if (pick === "even") {
      label = "EVENS";
      speech = "even numbers";
      test = (v) => v % 2 === 0;
    } else {
      label = "ODDS";
      speech = "odd numbers";
      test = (v) => v % 2 === 1;
    }

    ruleLabel = label;
    ruleSpeech = speech;
    // 🚫 No repeats: partition 1..maxVal by the rule, shuffle, and deal —
    // every answer and every decoy appears at most once per board. If the
    // decoy pool runs short the board carries more answers instead.
    const goodPool = [], badPool = [];
    for (let v = 1; v <= maxVal; v += 1) (test(v) ? goodPool : badPool).push(v);
    shuffle(goodPool, rnd);
    shuffle(badPool, rnd);
    let nc = min(nCorrect, goodPool.length);
    nc = max(nc, min(goodPool.length, total - badPool.length));
    for (let i = 0; i < nc; i += 1) cells.push({ value: goodPool[i], correct: true });
    for (let i = 0; i < total - nc; i += 1) cells.push({ value: badPool[i], correct: false });
  }

  shuffle(cells, rnd);
  grid = cells.map((c) => ({ ...c, eaten: false, flash: 0, known: false, failed: false }));
  remaining = grid.filter((c) => c.correct && !c.eaten).length;
  boardCorrectTotal = max(1, remaining);
  foundValues = [];

  // Place muncher centrally, away from any troggle.
  muncher = { col: 2, row: 2 };
  muncherVis = { col: 2, row: 2 };
  troggles = spawnTroggles(rnd);
  invuln = 60;
  facing = { x: 1, y: 0 };

  // ⏳ Per-board beat budget (generous; tightens a little as levels climb).
  // Note mode runs a peppier groove (BPM 92) with a slightly tighter budget.
  // Ticks ride the global wall-clock beat grid; resync the local index so the
  // fresh board doesn't immediately burn a beat at start.
  beatsMax = mode === "note" ? max(20, 44 - level) : max(24, 52 - level);
  beatsLeft = beatsMax;
  beatPhase = 0;
  beatMs = (60 / (mode === "note" ? 92 : BPM)) * 1000;
  lastBeatIndex = null;

  // Clear any leftover animation state from the previous round.
  deathPhase = 0;
  clearPhase = 0;
  pendingOver = false;
  flashRed = 0;
  flashGold = 0;
  confetti = [];
  chompPhase = 0;
  walkTarget = null;
  walkTick = 0;
}

function spawnTroggles(rnd) {
  // 0 at L1, 1 at L2-3, 2 at L4-5, 3 at L6-7, 4 at L8-9, 5 at L10+ — the danger
  // keeps climbing well past the old 3-troggle plateau.
  const count = min(5, floor(level / 2));
  const list = [];
  for (let i = 0; i < count; i += 1) {
    // Edge spawn, resampled until it lands ≥3 tiles (Manhattan) from the
    // muncher's fresh start — mid-edge cells sit only 2 away in his own
    // row/column and marched in for an unavoidable hit as spawn grace expired.
    let col, row, dir;
    do {
      const horizontal = rnd(2) === 0;
      col = horizontal ? (rnd(2) ? 0 : COLS - 1) : rnd(COLS);
      row = horizontal ? rnd(ROWS) : rnd(2) ? 0 : ROWS - 1;
      dir = horizontal
        ? { x: rnd(2) ? 1 : -1, y: 0 }
        : { x: 0, y: rnd(2) ? 1 : -1 };
    } while (abs(col - muncher.col) + abs(row - muncher.row) < 3);
    list.push({ col, row, dir, hue: [255, 90 + rnd(120), 60][i % 3] });
  }
  return list;
}

function idx(col, row) {
  return row * COLS + col;
}

// 🧮 Sim ───────────────────────────────────────────────────────────────────
function sim({ gizmo, seconds, sound, clock, num: { randInt } }) {
  snd = sound;
  if (sound && !synth) synth = Synth(sound);
  // 🕰️ Sample the network-synced wall clock and derive the global beat grid.
  nowMs = clock?.time?.()?.getTime?.() ?? (nowMs + 1000 / FPS);
  beatIndex = Math.floor(nowMs / beatMs);
  beatPhase = (nowMs % beatMs) / beatMs; // 0..1 within the current beat
  frames += 1;
  mouth = (mouth + 1) % 40;
  if (introTimer > 0) introTimer -= 1; // intro card fade (beats + troggles frozen)
  if (invuln > 0) invuln -= 1;
  if (chompPhase > 0) chompPhase -= 1;
  if (messageTimer > 0) {
    messageTimer -= 1;
    if (messageTimer === 0) message = "";
  }
  grid.forEach((c) => {
    if (c.flash > 0) c.flash -= 1;
  });

  // Fire any queued melody notes whose time has come.
  for (let i = melody.length - 1; i >= 0; i -= 1) {
    if (frames >= melody[i].at) {
      note(melody[i]);
      melody.splice(i, 1);
    }
  }

  // 🎥 Camera: smooth lean toward the Muncher, ease zoom home, decay shake.
  const cc = layout.cell || 32;
  const target = deathPhase > 0 ? deathPos : muncher;
  const leanX = (2 - target.col) * cc * 0.16;
  const leanY = (2 - target.row) * cc * 0.16;
  cam.x += (leanX - cam.x) * 0.1;
  cam.y += (leanY - cam.y) * 0.1;
  cam.zoom += (camTargetZoom - cam.zoom) * 0.18;
  camTargetZoom += (1 - camTargetZoom) * 0.08;
  cam.shake *= 0.86;
  if (cam.shake < 0.05) cam.shake = 0;

  // 🏃 Slide the Muncher's display position toward its logical cell.
  muncherVis.col += (muncher.col - muncherVis.col) * 0.3;
  muncherVis.row += (muncher.row - muncherVis.row) * 0.3;
  if (abs(muncher.col - muncherVis.col) < 0.01) muncherVis.col = muncher.col;
  if (abs(muncher.row - muncherVis.row) < 0.01) muncherVis.row = muncher.row;

  if (flashRed > 0) flashRed -= 1;
  if (flashGold > 0) flashGold -= 1;
  if (flashGreen > 0) flashGreen -= 1;
  if (beatPulse > 0) beatPulse -= 1;

  // ✨ Confetti physics (level-clear) — compacted in place, no per-frame array.
  if (confetti.length) {
    let live = 0;
    for (const p of confetti) {
      p.x += p.vx;
      p.y += p.vy;
      p.vy += 0.12;
      if (p.y < screenH + 40) confetti[live++] = p;
    }
    confetti.length = live;
  }

  // 💀 Death animation timeline (world frozen while it plays out) — sudden
  // death, so the fall always lands on the game-over screen.
  if (deathPhase > 0) {
    deathPhase -= 1;
    if (deathPhase === 0 && pendingOver) {
      pendingOver = false;
      state = "over";
      jingleOver();
    }
    return;
  }

  // 🎉 Level-clear celebration timeline.
  if (clearPhase > 0) clearPhase -= 1;

  if (state !== "play") return;

  // ⏳ Beat-based timeline, ticked off the global wall-clock grid: whenever the
  // synced beat index advances, this board loses a beat (metronome + visual
  // pulse). Running out costs a life. Because every board reads the same clock,
  // their metronomes pulse together no matter when each one started.
  // While the intro card is up, freeze the budget (keep the index aligned so it
  // doesn't lurch when the grace ends).
  if (lastBeatIndex === null || introTimer > 0) lastBeatIndex = beatIndex;
  if (introTimer === 0 && beatIndex > lastBeatIndex) {
    lastBeatIndex = beatIndex; // collapse any beats missed while backgrounded
    beatPulse = 8;
    if (beatsLeft > 0) {
      beatsLeft -= 1;
      tick(beatsLeft);
    }
    if (beatsLeft <= 0) timeUp();
  }
  if (beatsLeft <= 3) cam.shake = max(cam.shake, 2.4); // panic shake

  // 👣 Tap-to-move walker — step toward the tapped square on a steady cadence,
  // munching on arrival so one tap does the whole trip.
  if (walkTarget) {
    if (walkTick > 0) {
      walkTick -= 1;
    } else {
      stepToward(walkTarget);
      walkTick = WALK_STEP_FRAMES;
    }
    if (
      deathPhase === 0 && walkTarget &&
      walkTarget.col === muncher.col && walkTarget.row === muncher.row
    ) {
      walkTarget = null;
      munch();
    }
  }

  // Troggles step on a steady beat (speeds up with level) — keeps accelerating
  // to ~L14 instead of flattening out at L10.
  troggleClock ||= new gizmo.Hourglass(seconds(max(0.22, 0.72 - level * 0.035)), {
    flipped: () => {
      troggles.forEach((t) => {
        let nc = t.col + t.dir.x;
        let nr = t.row + t.dir.y;
        if (nc < 0 || nc >= COLS) {
          t.dir.x *= -1;
          nc = t.col + t.dir.x;
        }
        if (nr < 0 || nr >= ROWS) {
          t.dir.y *= -1;
          nr = t.row + t.dir.y;
        }
        t.col = max(0, min(COLS - 1, nc));
        t.row = max(0, min(ROWS - 1, nr));
      });
      checkTroggleHit();
    },
    autoFlip: true,
  });
  if (introTimer === 0) troggleClock?.step(); // troggles hold still during the intro
}

function checkTroggleHit() {
  if (invuln > 0) return;
  if (troggles.some((t) => t.col === muncher.col && t.row === muncher.row)) {
    loseLife();
  }
}

// ☠️ Sudden death — any hit, wrong munch, or time-out ends the game outright.
function loseLife() {
  combo = 0;
  walkTarget = null; // a hit stops the tap-to-move walk
  killChord();
  deathPos = { col: muncher.col, row: muncher.row };
  deathPhase = 54; // play the death animation, then game over
  cam.shake = 11; // hard jolt
  flashRed = 18; // red flash
  sadDeath();
  flash("GAME OVER");
  pendingOver = true; // over fires when the animation ends
}

function flash(msg) {
  message = msg;
  messageTimer = 90;
}

// ⏳ Ran out of beats on this board — sudden death.
function timeUp() {
  loseLife();
  flash("TIME!"); // show the reason; the overlay says GAME OVER after the fall
}

// 🥁 Metronome tick — real notepat perc kit: kick on the global downbeat (every
// 4th beat of the shared wall-clock grid, so all boards land the kick together),
// closed hi-hat on the off-beats (falls back to a synth click if the kit is
// absent). In the final 3 beats it plays a distinct, rising warning beep.
const WARN_BEEPS = { 3: 700, 2: 950, 1: 1300 }; // rising pitch = more urgent
function tick(remainingBeats) {
  const warn = WARN_BEEPS[remainingBeats];
  if (warn) {
    if (synth) synth.kick({ volume: 0.4 }); // pulse under the beep
    note({ type: "square", tone: warn, duration: 0.14, volume: 0.36, attack: 0.001 });
    if (remainingBeats === 1) // last gasp: a quick second blip
      playMelody([{ tone: warn * 1.5, type: "square", dur: 0.1, vol: 0.3, t: 6 }], 0);
    return;
  }
  const downbeat = beatIndex % 4 === 0; // global grid → synced across instances
  if (synth) {
    if (downbeat) synth.kick({ volume: 0.55 });
    else synth.hat({ volume: 0.3 });
  } else if (downbeat) {
    note({ type: "sine", tone: 92, duration: 0.09, volume: 0.22 });
  } else {
    note({ type: "square", tone: 1100, duration: 0.015, volume: 0.09 });
  }
  groove(downbeat);
}

// 🎶 Background groove — a tiny band riding the shared wall-clock beat grid:
// a walking triangle bass under the kick/hat pattern, a soft snare backbeat,
// and a sparkle arpeggio on each bar turn. Chords cycle i–VI–III–VII
// (Am F C G), one chord per 4 global beats, so every board everywhere is
// grooving in the same key at the same moment — like the synced metronomes.
// Note mode sits this out (its boards make their own music from the scale).
const GROOVE_ROOTS = [110, 87.31, 65.41, 98]; // A2 F2 C2 G2
const GROOVE_TRIADS = [
  [220, 261.63, 329.63], // Am — A3 C4 E4
  [174.61, 220, 261.63], // F — F3 A3 C4
  [130.81, 164.81, 196], // C — C3 E3 G3
  [196, 246.94, 293.66], // G — G3 B3 D4
];
function groove(downbeat) {
  if (mode === "note" || state !== "play") return;
  const bar = floor(beatIndex / 4) % GROOVE_ROOTS.length;
  const root = GROOVE_ROOTS[bar];
  const beatInBar = ((beatIndex % 4) + 4) % 4;
  // Bass walk: root on the one, up an octave between, the fifth on the three.
  const bass = beatInBar === 0 ? root : beatInBar === 2 ? root * 1.5 : root * 2;
  note({ type: "triangle", tone: bass, duration: 0.22, volume: 0.17, attack: 0.008 });
  if (beatInBar === 2 && synth) synth.snare({ volume: 0.16 }); // soft backbeat
  // Sparkle arp turning the bar — sits out in panic time (warn beeps own it).
  if (beatInBar === 3 && beatsLeft > 6) {
    const tri = GROOVE_TRIADS[bar];
    const half = round((beatMs / 1000) * FPS * 0.5);
    playMelody(
      [
        { tone: tri[1] * 2, type: "sine", dur: 0.09, vol: 0.1, t: 0 },
        { tone: tri[2] * 2, type: "sine", dur: 0.09, vol: 0.09, t: round(half / 2) },
        { tone: tri[0] * 4, type: "sine", dur: 0.12, vol: 0.08, t: half },
      ],
      0,
    );
  }
}

// 🎮 Movement + munching ─────────────────────────────────────────────────────
function move(dx, dy) {
  if (state !== "play" || deathPhase > 0) return;
  const pc = muncher.col,
    pr = muncher.row;
  muncher.col = (muncher.col + dx + COLS) % COLS; // wrap around edges
  muncher.row = (muncher.row + dy + ROWS) % ROWS;
  if (dx || dy) facing = { x: dx, y: dy };
  // On a wrap, snap the visual so it doesn't slide all the way across.
  if (abs(muncher.col - pc) > 1) muncherVis.col = muncher.col;
  if (abs(muncher.row - pr) > 1) muncherVis.row = muncher.row;
  if (muncher.col !== pc || muncher.row !== pr) moveBlip();
  checkTroggleHit();
}

function munch() {
  if (state !== "play") return;
  const cell = grid[idx(muncher.col, muncher.row)];
  chompPhase = 10; // chomp animation regardless of outcome
  if (!cell || cell.eaten) {
    note({ type: "sine", tone: 200, duration: 0.04, volume: 0.18 });
    return;
  }
  if (cell.failed) {
    note({ type: "sine", tone: 150, duration: 0.04, volume: 0.14 }); // already X'd
    return;
  }
  if (cell.correct) {
    cell.eaten = true;
    cell.flash = 14;
    combo += 1;
    remaining -= 1;
    beatsLeft = min(beatsMax, beatsLeft + 1); // munching earns a beat back
    if (mode === "note") noteMunchGood(cell);
    else chompGood();
    camTargetZoom = 1.06; // tiny zoom punch on every good munch

    // Collect the value + light up its (now frightened) duplicates.
    const v = String(cell.value);
    if (!foundValues.includes(v)) foundValues.push(v);
    grid.forEach((c) => {
      if (!c.eaten && String(c.value) === v) {
        c.known = true;
        c.flash = 14;
      }
    });

    if (remaining === 1) flash("LAST ONE!");
    // Speak the English translation (mexinom / dannom), else announce the last one.
    // Note mode stays musical — no speech, the note itself sounds.
    if (mode === "word" && lang !== "en") sayTranslation(v);
    else if (mode !== "note" && remaining === 1) say("last one");
    if (remaining <= 0) {
      state = "clear";
      clearPhase = 96;
      camTargetZoom = 1.16; // bigger celebratory push-in
      flashGold = 14;
      spawnConfetti();
      flash("LEVEL CLEAR!");
      jingleClear();
    }
  } else {
    cell.flash = 14;
    cell.failed = true; // X it out — can't be tried again
    if (mode === "note") noteMunchBad(cell);
    else {
      if (mode === "word" && lang !== "en") sayTranslation(cell.value);
      chompBad();
    }
    loseLife();
  }
}

// ⌨️ Smart munch (space/enter): munch what you're on; otherwise step toward the
// nearest confirmed-green square and munch it if you land on one — so rapid
// presses chain through a run of greens for combos.
function smartMunch() {
  if (state !== "play" || deathPhase > 0) return;
  const cur = grid[idx(muncher.col, muncher.row)];
  if (cur && cur.correct && !cur.eaten) {
    munch();
    return;
  }
  const tgt = nearestGreen();
  if (!tgt) {
    munch(); // nothing to chain to → normal munch (empty blip / risk)
    return;
  }
  stepToward(tgt);
  const now = grid[idx(muncher.col, muncher.row)];
  if (now && now.correct && !now.eaten) munch(); // landed on a green → eat it
}

// Nearest known-green uneaten cell (toroidal distance), excluding the current.
function nearestGreen() {
  let best = null,
    bestD = Infinity;
  for (let r = 0; r < ROWS; r += 1)
    for (let c = 0; c < COLS; c += 1) {
      const cd = grid[idx(c, r)];
      if (!cd || !cd.known || cd.eaten || cd.failed) continue;
      const dc = min((c - muncher.col + COLS) % COLS, (muncher.col - c + COLS) % COLS);
      const dr = min((r - muncher.row + ROWS) % ROWS, (muncher.row - r + ROWS) % ROWS);
      const d = dc + dr;
      if (d > 0 && d < bestD) {
        bestD = d;
        best = { col: c, row: r };
      }
    }
  return best;
}

// One step toward a target, taking the shorter (possibly wrapping) direction.
function stepToward(t) {
  const dcF = (t.col - muncher.col + COLS) % COLS,
    dcB = (muncher.col - t.col + COLS) % COLS;
  const drF = (t.row - muncher.row + ROWS) % ROWS,
    drB = (muncher.row - t.row + ROWS) % ROWS;
  const colDist = min(dcF, dcB),
    rowDist = min(drF, drB);
  if (colDist >= rowDist && colDist > 0) move(dcF <= dcB ? 1 : -1, 0);
  else if (rowDist > 0) move(0, drF <= drB ? 1 : -1);
}

// 🇲🇽 / 🇩🇰 / 🇷🇺 / 😼 Speak the meaning of a non-plain-English answer.
function sayTranslation(v) {
  const table = lang === "da" ? TRANSLATE_DA : lang === "ru" ? TRANSLATE_RU : lang === "cat" ? TRANSLATE_CAT : TRANSLATE;
  say(table[v] || String(v));
}

// 🔊 Sound ───────────────────────────────────────────────────────────────────
function note({ type = "sine", tone, duration = 0.1, volume = 0.3, attack = 0.004 }) {
  snd?.synth?.({
    type,
    tone,
    attack,
    decay: duration * 0.5,
    sustain: 0,
    release: 0.04,
    volume,
    duration,
  });
}

// Schedule a little tune; each note plays `gap` frames after the previous
// (unless it carries its own `t` offset). Times are relative to `frames` now.
function playMelody(notes, gap = 6) {
  let t = 0;
  notes.forEach((n) => {
    t = n.t != null ? n.t : t;
    melody.push({
      at: frames + t,
      type: n.type || "sine",
      tone: n.tone,
      duration: n.dur || 0.12,
      volume: n.vol || 0.3,
    });
    t += gap;
  });
}

function moveBlip() {
  note({ type: "sine", tone: 340, duration: 0.02, volume: 0.16 });
}

// 🗣️ Speak a phrase in Jeffrey's ElevenLabs voice (auto-cached on the AC CDN)
// and surface it as the on-screen caption. The caption is set synchronously, so
// the word shows the instant it's spoken — before the audio finishes loading.
function say(text) {
  if (!text) return;
  caption = text;
  captionFrame = frames;
  warmed.add(text); // it's about to be fetched; don't also prewarm it
  speakFn?.(text, "jeffrey", "cloud", { volume: 1, provider: "jeffrey" });
}

// 🔥 Warm the TTS cache for a phrase without playing it (preloadOnly), so the
// first munch that speaks it plays instantly. Deduped per session.
function warm(text) {
  if (!text || warmed.has(text)) return;
  warmed.add(text);
  speakFn?.(text, "jeffrey", "cloud", { provider: "jeffrey", preloadOnly: true });
}

// 🔥 Pre-warm every distinct utterance the current board can speak (each cell's
// meaning in a translation edition), so munches voice back with no lag.
function prewarmBoard() {
  if (mode !== "word" || lang === "en") return; // only translation editions speak per-munch
  const table = lang === "da" ? TRANSLATE_DA : lang === "ru" ? TRANSLATE_RU : lang === "cat" ? TRANSLATE_CAT : TRANSLATE;
  const seen = new Set();
  for (const cd of grid) {
    const t = table[cd.value] || String(cd.value);
    if (seen.has(t)) continue;
    seen.add(t);
    warm(t);
  }
}

// Stop the sustained held-munch chord.
function killChord() {
  heldChord?.forEach((v) => v?.kill?.(0.18));
  heldChord = null;
}

// 🎹 Pentatonic note for the held "durational munch" chord (varies by cell).
function pentatonic(col, row) {
  const scale = [261.63, 293.66, 329.63, 392.0, 440.0]; // C D E G A
  const oct = row < 2 ? 2 : row < 4 ? 1 : 0.5;
  return scale[col % scale.length] * oct;
}

function chompGood() {
  // A two-part "nom": a low chunk then a bright ping that rises with the combo.
  const ping = 540 + min(combo, 8) * 45;
  note({ type: "square", tone: 150, duration: 0.05, volume: 0.3 });
  playMelody(
    [
      { tone: ping, type: "sine", dur: 0.07, vol: 0.32, t: 1 },
      { tone: ping * 1.5, type: "triangle", dur: 0.05, vol: 0.22, t: 4 },
    ],
    0,
  );
}

function chompBad() {
  note({ type: "sawtooth", tone: 190, duration: 0.16, volume: 0.3 });
  playMelody([{ tone: 120, type: "sawtooth", dur: 0.2, vol: 0.28, t: 3 }], 0);
}

// 🎵 Note-mode munch — voice the square's actual note, then a combo ping above.
function noteMunchGood(cell) {
  if (!synth) return;
  synth.note(cellNote(cell), { wave: "triangle", duration: 0.2, volume: 0.34 });
  const top = 660 + min(combo, 8) * 40; // bright ping rises with the combo
  synth.note(top, { wave: "sine", duration: 0.08, volume: 0.2 });
}

// 🎵 Wrong note — sound it anyway (so you hear what you bit), then a sour buzz.
function noteMunchBad(cell) {
  if (!synth) return;
  synth.note(cellNote(cell), { wave: "triangle", duration: 0.2, volume: 0.34 });
  synth.note(140, { wave: "sawtooth", duration: 0.18, volume: 0.3 });
}

// 🎼 Play the current board's rule scale ascending when a board begins.
function playScale() {
  if (!synth || !noteScale.length) return;
  noteScale.forEach((n, i) => {
    setTimeout(() => {
      if (synth)
        synth.note(String(n).toLowerCase(), { wave: "triangle", duration: 0.18, volume: 0.3 });
    }, i * 110);
  });
}

// 😢 Death — a redder, sadder fall: a wobbling downward glissando into a
// low, mournful sigh.
function sadDeath() {
  const notes = [];
  const steps = 12;
  for (let i = 0; i < steps; i += 1) {
    const f = 415 * pow(2, -(i / 7)); // slide down ~1.7 octaves
    notes.push({ tone: f, type: "triangle", dur: 0.09, vol: 0.3, t: i * 2 });
  }
  playMelody(notes, 0);
  playMelody(
    [
      { tone: 147, type: "sine", dur: 0.45, vol: 0.34, t: steps * 2 + 3 }, // D3
      { tone: 110, type: "sine", dur: 0.7, vol: 0.32, t: steps * 2 + 11 }, // A2
    ],
    0,
  );
}

// 🎉 Level clear — triumphant fanfare with a sparkle on top.
// 🏆 Victory melody — a triumphant ascending fanfare, a sparkle run, and a
// big held final chord.
function jingleClear() {
  playMelody(
    [
      { tone: 523, dur: 0.12 }, // C5
      { tone: 659, dur: 0.12 }, // E5
      { tone: 784, dur: 0.12 }, // G5
      { tone: 1047, dur: 0.16 }, // C6
      { tone: 880, dur: 0.12 }, // A5
      { tone: 988, dur: 0.12 }, // B5
      { tone: 1047, dur: 0.14 }, // C6
      { tone: 1319, dur: 0.34 }, // E6 hold
    ],
    6,
  );
  // sparkle run on top
  playMelody(
    [
      { tone: 1568, type: "sine", dur: 0.08, vol: 0.2, t: 54 },
      { tone: 2093, type: "sine", dur: 0.08, vol: 0.2, t: 60 },
      { tone: 2637, type: "sine", dur: 0.16, vol: 0.2, t: 66 },
    ],
    0,
  );
  // big held C-major triad to finish
  playMelody(
    [
      { tone: 523, type: "triangle", dur: 0.6, vol: 0.3, t: 70 },
      { tone: 659, type: "triangle", dur: 0.6, vol: 0.24, t: 70 },
      { tone: 784, type: "triangle", dur: 0.6, vol: 0.2, t: 70 },
      { tone: 1047, type: "sine", dur: 0.6, vol: 0.2, t: 70 },
    ],
    0,
  );
  if (synth) synth.crash({ volume: 0.4 }); // cymbal hit on the win
}

function jingleOver() {
  playMelody(
    [
      { tone: 392, type: "triangle", dur: 0.18 }, // G4
      { tone: 311, type: "triangle", dur: 0.18 }, // Eb4 (minor turn)
      { tone: 262, type: "triangle", dur: 0.22 }, // C4
      { tone: 196, type: "triangle", dur: 0.5 }, // G3 long
    ],
    9,
  );
}

// 🎊 Burst of confetti from the top of the board (deterministic, no RNG).
function spawnConfetti() {
  confetti = [];
  const cols = [
    [255, 210, 90],
    [120, 235, 120],
    [255, 120, 160],
    [120, 180, 255],
    [255, 255, 255],
  ];
  const bcx = layout.x + (layout.cell * COLS) / 2;
  for (let i = 0; i < 44; i += 1) {
    confetti.push({
      x: bcx + (((i * 53) % 100) - 50) * (layout.cell * COLS) * 0.006,
      y: layout.y + 4,
      vx: (((i * 31) % 7) - 3) * 0.7,
      vy: -2.4 - ((i * 17) % 5) * 0.5,
      s: (2 + (i % 3)) * max(1, Math.ceil(hudScale / 2)), // bigger bits on big displays
      c: cols[i % cols.length],
    });
  }
}

function startChirp() {
  playMelody(
    [
      { tone: 392, dur: 0.06 },
      { tone: 523, dur: 0.06 },
      { tone: 784, dur: 0.1 },
    ],
    4,
  );
}

// 🎬 Announce a freshly-started board: green flash, start chime (or the scale in
// note mode), and speak the rule. Called on auto-start and on each advance.
function announceBoard() {
  flashGreen = 16;
  if (mode === "note") playScale();
  else startChirp();
  say(ruleSpeech);
  prewarmBoard(); // warm the board's munch utterances so they voice back instantly
}

// 🎪 Act ─────────────────────────────────────────────────────────────────────
function act({ event: e, sound, speak, cursor, num: { randInt } }) {
  snd = sound;
  // ⌨️/🖱️ Input mode: pressing a key hides the cursor + hover; moving the mouse
  // brings them back.
  if (e.name?.startsWith?.("keyboard:down")) {
    hover = null;
    cursor?.("none");
  }
  if (e.is("move") || e.is("draw") || e.is("touch")) cursor?.("native");
  speakFn = speak;
  if (sound && !synth) synth = Synth(sound);

  // 📣 The board auto-started in boot(); announce its rule + play the start chime
  // the moment audio is actually available (boot has no sound/speak handle).
  if (pendingStart && (synth || speakFn)) {
    pendingStart = false;
    announceBoard();
  }

  // Advance from end-of-board states (clear → next level, over/win → fresh game).
  const advance = () => {
    if (state === "clear") {
      level += 1;
      newRound({ randInt });
    } else if (state === "over" || state === "win") {
      resetGame({ randInt });
      newRound({ randInt });
      introTimer = INTRO_FRAMES; // re-show the title card on a fresh game
    } else return;
    state = "play";
    announceBoard();
  };

  if (state !== "play") {
    // Any key-down or a tap advances from the clear/over overlays.
    if (e.name?.startsWith?.("keyboard:down")) {
      advance();
    } else if (e.is("touch")) {
      advance();
      // This tap *started* the board — swallow its release so the trailing
      // lift doesn't immediately munch the center square.
      swallowTap = true;
    }
    return;
  }

  // Release the held chord even mid-death so it never sticks.
  if (
    e.is("keyboard:up:space") ||
    e.is("keyboard:up:enter") ||
    e.is("keyboard:up:return") ||
    e.is("lift")
  )
    killChord();

  // 👆 Tap to move / munch: tapping the muncher's own square (or off-board)
  // munches in place; tapping any other square sends the muncher walking there
  // — sim steps it across the grid and it munches on arrival, so one tap does
  // the whole trip. Always consume the swallow flag here — even mid-death —
  // so it can never leak into the next gesture.
  if (e.is("lift")) {
    const consumed = swallowTap;
    swallowTap = false;
    if (!consumed && deathPhase === 0) {
      const t = cellAt(e.x, e.y);
      if (!t || (t.col === muncher.col && t.row === muncher.row)) {
        walkTarget = null;
        munch();
      } else {
        walkTarget = { col: t.col, row: t.row };
        walkTick = 0; // first step lands immediately
        note({ type: "sine", tone: 500, duration: 0.03, volume: 0.14 }); // pick blip
      }
    }
  }

  // 🖱️ Hover highlight — track the cell under the pointer.
  if (e.is("move") || e.is("draw")) hover = cellAt(e.x, e.y);

  if (deathPhase > 0) return; // ignore other input mid-death

  // Movement — arrows + WASD + vim (hjkl). Manual steering cancels the walker.
  const step = (dx, dy) => { walkTarget = null; move(dx, dy); };
  if (e.is("keyboard:down:arrowleft") || e.is("keyboard:down:a") || e.is("keyboard:down:h")) step(-1, 0);
  if (e.is("keyboard:down:arrowright") || e.is("keyboard:down:d") || e.is("keyboard:down:l")) step(1, 0);
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:w") || e.is("keyboard:down:k")) step(0, -1);
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:s") || e.is("keyboard:down:j")) step(0, 1);

  // Munch (space + enter behave identically) — smart-chain to the nearest
  // green, and sustain a pentatonic chord while held (durational, musical).
  if (
    e.is("keyboard:down:space") ||
    e.is("keyboard:down:enter") ||
    e.is("keyboard:down:return")
  ) {
    walkTarget = null; // keyboard munch takes over from the walker
    smartMunch();
    if (!heldChord && synth) {
      if (mode === "note") {
        // 🎵 Sustain the square's actual note while held (a soft drone).
        const cell = grid[idx(muncher.col, muncher.row)];
        if (cell && !cell.eaten && !cell.failed) {
          const n = cellNote(cell);
          heldChord = [
            synth.hold(n, { wave: "sine", volume: 0.14 }),
            synth.hold(n, { wave: "triangle", volume: 0.08 }),
          ];
        }
      } else {
        // Sustained pentatonic chord via the shared synth controller.
        const root = pentatonic(muncher.col, muncher.row);
        heldChord = [
          synth.hold(root, { wave: "sine", volume: 0.18 }),
          synth.hold(root * 1.5, { wave: "sine", volume: 0.12 }),
          synth.hold(root * 2, { wave: "triangle", volume: 0.08 }),
        ];
      }
    }
  }

}

function resetGame({ randInt }) {
  level = 1;
  troggleClock = null;
  message = "";
  messageTimer = 0;
}

// 📐 Layout helpers (recomputed each paint from current screen) ───────────────
let layout = { x: 0, y: 0, cell: 32, top: 40 };

// 🔍 Readability-first chrome: every HUD element (timer, counters, banners,
// caption) scales with the display instead of sitting at fixed pixel sizes.
// hudScale 1 fits phones; large desktop windows climb toward 5.
let hudScale = 1;
let timerSize = 4; // big beat-timer glyph scale (drives the header height)
let capSize = null; // computeLayout pins the caption scale on short windows (else null)

// The caption strip's nominal size + reserved band height (pre-shrink), so
// the lives row and LEFT counter can sit above it instead of underneath.
function captionSize(screen) {
  // computeLayout pins capSize on short windows so the caption band shrinks in
  // lock-step with the space it reserves (see the short-window guard below).
  if (capSize != null) return capSize;
  return max(1, round(screen.height / 240), hudScale + 1);
}
function captionBandH(screen) {
  const { h } = fontMetrics(cellFont());
  const gh = h * captionSize(screen);
  return gh + round(gh * 0.4) + 5;
}

function computeLayout(screen) {
  screenH = screen.height;
  capSize = null; // recomputed below; a short window may pin it
  // 🔍 HUD scale from the display's short side — chrome text grows with the
  // window instead of sitting at fixed pixel sizes (see hudScale above).
  hudScale = max(1, min(5, floor(min(screen.width, screen.height) / 200)));
  timerSize = 3 + hudScale;
  // 📱 Phone-width screens trade chrome for board: slimmer margins so cells
  // get as big as possible under a thumb.
  const compact = screen.width < 460;
  // Word boards go near edge-to-edge: whole words render on one line
  // (never split — see fitText), so cells get every pixel the screen has.
  // Number/note boards keep the roomier framing.
  const sideMargin = mode === "word" ? 4 : compact ? 8 : max(14, floor(screen.width * 0.06));
  let top = 14 + 10 * timerSize; // header band sized to the big beat timer
  let bottomPad = 12 + captionBandH(screen); // HUD band above the caption strip
  // 🩳 Short-window guard: on a low viewport the header + footer bands would
  // crowd the 5×5 board off the screen (board overlapping the timer / counter /
  // caption). If they'd leave the board less than ~45% of the height, shrink the
  // timer AND caption glyphs themselves — not just their reserved bands — until
  // the board gets its share. Everything the HUD draws is sized from timerSize /
  // captionSize, so the reservation and the render stay in lock-step.
  const hudBudget = round(screen.height * 0.55);
  while (top + bottomPad > hudBudget && (timerSize > 1 || captionSize(screen) > 1)) {
    if (timerSize > 1) timerSize -= 1;
    const cs = captionSize(screen);
    if (cs > 1) capSize = cs - 1;
    top = 14 + 10 * timerSize;
    bottomPad = 12 + captionBandH(screen);
  }
  const availW = screen.width - sideMargin * 2;
  const availH = screen.height - top - bottomPad;
  // Smallest cell that still fits BETWEEN the bands (and the side margins), so a
  // squat window shrinks the board to fit instead of overflowing the HUD. Only
  // bites on tight screens — roomy windows keep the resting 18px minimum below.
  const minCell = max(6, min(18, floor(availH / ROWS), floor(availW / COLS)));
  // Resting fit inside the margined area …
  const restFit = min(availW / COLS, availH / ROWS);
  // … but never so large that the camera's max zoom punch overflows the full
  // viewport — mobile screens are tight, so keep the whole board on-screen even
  // at peak zoom (we still reserve a hair for lean + shake via the clamp below).
  // Word mode skips that reservation (cells need the width for whole words);
  // applyCamera just centers the board during the brief zoom punches instead.
  const zoomFit = min(screen.width / COLS, availH / ROWS) / MAX_ZOOM;
  const cell = max(minCell, floor(mode === "word" ? restFit : min(restFit, zoomFit)));
  const gw = cell * COLS;
  const gh = cell * ROWS;
  // Mutated in place — one long-lived object, no per-frame allocation.
  layout.cell = cell;
  layout.baseCell = cell;
  layout.x = floor((screen.width - gw) / 2);
  layout.y = top + floor((availH - gh) / 2);
  layout.top = top;
  layout.bottomPad = bottomPad;
}

// 🎥 Fold the dynamic camera (zoom about board center + lean + shake) into the
// live layout, so both drawing and touch hit-testing stay consistent.
function applyCamera(screen) {
  const z = cam.zoom;
  const cell = max(8, round(layout.baseCell * z));
  const gw = cell * COLS;
  const gh = cell * ROWS;
  const shx = cam.shake ? sin(frames * 1.7) * cam.shake : 0;
  const shy = cam.shake ? cos(frames * 2.3) * cam.shake : 0;
  // Centered + lean + shake, then clamped so no edge ever leaves the viewport.
  const bx = (screen.width - gw) / 2 + cam.x + shx;
  const by = layout.top + (screen.height - layout.top - layout.bottomPad - gh) / 2 + cam.y + shy;
  const pad = 2;
  const maxX = screen.width - gw - pad; // rightmost on-screen origin
  const maxY = screen.height - layout.bottomPad - gh; // keep clear of the HUD band
  layout.cell = cell;
  // If the (zoomed) board is wider/taller than the space, just center it;
  // otherwise clamp the leaned/shaken position to the on-screen range.
  layout.x = round(maxX < pad ? (screen.width - gw) / 2 : max(pad, min(maxX, bx)));
  layout.y = round(
    maxY < layout.top
      ? layout.top + (screen.height - layout.top - layout.bottomPad - gh) / 2
      : max(layout.top, min(maxY, by)),
  );
}

function cellAt(px, py) {
  const { x, y, cell } = layout;
  const col = floor((px - x) / cell);
  const row = floor((py - y) / cell);
  if (col < 0 || col >= COLS || row < 0 || row >= ROWS) return null;
  return { col, row };
}

// 🎨 Paint ───────────────────────────────────────────────────────────────────
// Either/or: an hd edition paints ONLY the hi-res Canvas2D layer (a worker
// OffscreenCanvas managed by disk's `hd()` API); everyone else paints the
// classic pixel buffer. disk's hd() hands back null in preview/icon mode —
// thumbnails capture the pixel buffer, so the game drops to full pixel
// painting for exactly those frames. Tapes stay hd end-to-end: bios mirrors
// each hd bitmap into a live MediaRecorder while recording.
let pixelWashDark = null; // theme of the last hidden-pixel-buffer wash
function paint(api) {
  setTheme(api.dark !== false); // 🌗 follow the system theme, live
  const layer = hiRes && api.hd ? api.hd() : null;
  if (layer) {
    // Keep the (hidden) pixel buffer a clean theme wash — freeze-frames and
    // piece transitions sample it, and a flat color beats stale art there.
    // The buffer persists between frames, so one wash per theme is enough;
    // skipping the rest saves a full-buffer clone + upload every frame.
    if (pixelWashDark !== dark) {
      api.wipe(...T.bg);
      pixelWashDark = dark;
    }
    const a = hdApi(layer);
    paintGame(a);
    paintCaption(a);
  } else {
    pixelWashDark = null; // pixel path owns the buffer again — wash on re-entry
    paintGame(api);
    paintCaption(api);
  }
}

// 💬 Always-on caption: the last spoken text, centered along the bottom over a
// translucent strip so it stays legible on any board. Uses the cell font so
// Cyrillic / accented meanings (rusnom, mexinom) render real glyphs.
// Caption band geometry — instructional content always sits ABOVE the board,
// never below, so the strip hugs the board's top edge. Shared with the
// transient message banner so the two stack instead of colliding.
function captionBand(screen) {
  if (!caption || state === "title") return null;
  const font = cellFont();
  const { adv, h } = fontMetrics(font);
  // Scale with the render resolution (hd draws at native pixels), then shrink
  // to fit long meanings ("aesthetic computer pieces") within the screen.
  const maxW = screen.width - 8;
  let size = captionSize(screen);
  while (size > 1 && caption.length * adv * size > maxW) size -= 1;
  const gh = h * size;
  const y = max(4, layout.y - gh - round(gh * 0.4) - 4);
  return { font, adv, size, gh, y };
}

function paintCaption({ ink, write, box, screen }) {
  const band = captionBand(screen);
  if (!band) return;
  const { font, adv, size, gh, y } = band;
  const w = caption.length * adv * size;
  const x = max(2, round((screen.width - w) / 2));
  const fade = min(1, (frames - captionFrame) / 6); // quick ease-in on change
  const [br, bg, bb, ba = 255] = T.captionBg;
  ink(br, bg, bb, round(ba * fade)).box(0, y - 3, screen.width, gh + 7);
  ink(...T.captionText).write(
    caption, { x, y, size }, undefined, undefined, false, font || undefined,
  );
}

// 🖼️ Canvas2D adapter — mirrors the exact slice of the AC paint API this
// engine uses (wipe / ink().box/.line/.write + bare box/line after ink) on
// the hd layer's pre-scaled 2d context, so paintGame runs unchanged at
// native device resolution. Text renders as vector glyphs at the same fixed
// per-glyph advances as FONT_METRICS, so every width/centering measurement
// in the game's layout math stays identical to the pixel path.
//
// Built once per (ctx, size) and reused every frame. Three caches keep the
// per-frame Canvas2D cost near zero:
//   color memo — rgba() strings rebuilt only when the ink actually changes,
//                and fill/stroke styles set only when they differ
//   glyphW     — per font+char advance measured ONCE (measureText is one of
//                the slowest 2d calls; advances scale linearly with size)
//   sprites    — whole rendered labels cached as OffscreenCanvases and drawn
//                back as a single drawImage (a board speaks a small
//                vocabulary, so per-frame text rasterization drops to zero)
let hdCached = null; // { ctx, width, height, api }
const glyphW = new Map(); // "font|char" → advance at 100px
const sprites = new Map(); // "font|size|r,g,b|text" → { c, w, h, pad }
const SPRITE_CAP = 480; // hard bound; cleared wholesale when exceeded

// Font styling for the hd layer — unifont stays a neutral monospace (it
// carries Cyrillic/accents for the word editions); everything else (numbers,
// HUD, titles) gets a rounded, chunky face for arcade character. Layout
// metrics stay FONT_METRICS regardless, so both paths measure identically.
function hdFont(font, px) {
  return font === "unifont"
    ? `500 ${px * 0.72}px ui-monospace, Menlo, Consolas, monospace`
    : `800 ${px * 0.8}px "Arial Rounded MT Bold", ui-rounded, "SF Pro Rounded", ui-monospace, Menlo, monospace`;
}

// Per-glyph advance from the cache, measuring on first sight only.
function glyphWidth(sctx, font, ch, px) {
  const key = `${font || "_"}|${ch}`;
  let w = glyphW.get(key);
  if (w === undefined) {
    sctx.font = hdFont(font, 100);
    w = sctx.measureText(ch).width;
    glyphW.set(key, w);
  }
  return (w * px) / 100;
}

// Rasterize a label once at device resolution: glyphs centered on the same
// fixed advances as the bitmap fonts, with padding so wide glyphs and
// descenders never clip at the sprite edge.
function makeSprite(str, font, size, scale, rgb) {
  const { adv, h } = fontMetrics(font);
  const px = h * size;
  const w = str.length * adv * size;
  const pad = Math.ceil(px * 0.3);
  const c = new OffscreenCanvas(
    max(1, Math.ceil((w + pad * 2) * scale)),
    max(1, Math.ceil((px + pad * 2) * scale)),
  );
  const sctx = c.getContext("2d");
  sctx.setTransform(scale, 0, 0, scale, 0, 0);
  sctx.textBaseline = "middle";
  // Measure first (glyphWidth may swap the font), then set the render font.
  const widths = [];
  for (let i = 0; i < str.length; i += 1)
    widths.push(glyphWidth(sctx, font, str[i], px));
  sctx.font = hdFont(font, px);
  sctx.fillStyle = rgb;
  const cy = pad + px / 2;
  for (let i = 0; i < str.length; i += 1)
    sctx.fillText(str[i], pad + i * adv * size + (adv * size - widths[i]) / 2, cy);
  return { c, w: w + pad * 2, h: px + pad * 2, pad };
}

function hdApi(layer) {
  if (
    hdCached && hdCached.ctx === layer.ctx &&
    hdCached.width === layer.width && hdCached.height === layer.height
  )
    return hdCached.api;
  const { ctx, width, height, scale } = layer;
  let r = 255, g = 255, b = 255, a = 1; // current ink
  let cr = -1, cg = -1, cb = -1, ca = -1; // memoized color components
  let cssStr = "rgba(255,255,255,1)";
  let lastFill = null, lastStroke = null;
  ctx.lineWidth = 1;
  const setColor = (args) => {
    if (args.length >= 3) {
      r = args[0]; g = args[1]; b = args[2];
      a = (args[3] ?? 255) / 255;
    } else {
      r = g = b = args[0] ?? 255;
      a = (args[1] ?? 255) / 255;
    }
    if (r !== cr || g !== cg || b !== cb || a !== ca) {
      cssStr = `rgba(${r},${g},${b},${a})`;
      cr = r; cg = g; cb = b; ca = a;
    }
  };
  const fill = () => {
    if (lastFill !== cssStr) ctx.fillStyle = lastFill = cssStr;
  };
  const stroke = () => {
    if (lastStroke !== cssStr) ctx.strokeStyle = lastStroke = cssStr;
  };
  const box = (x, y, w, h, mode2) => {
    if (mode2 === "outline") {
      stroke();
      ctx.strokeRect(x + 0.5, y + 0.5, w - 1, h - 1);
    } else {
      fill();
      ctx.fillRect(x, y, w, h);
    }
  };
  const line = (x1, y1, x2, y2) => {
    stroke();
    ctx.beginPath();
    ctx.moveTo(x1 + 0.5, y1 + 0.5);
    ctx.lineTo(x2 + 0.5, y2 + 0.5);
    ctx.stroke();
  };
  const write = (txt, pos = {}, _bg, _bounds, _wrap, font) => {
    const str = String(txt);
    if (!str) return;
    const { adv, h } = fontMetrics(font);
    const size = pos.size || 1;
    let x = pos.x ?? 0;
    if (pos.center === "x") x = (width - str.length * adv * size) / 2;
    const y = pos.y ?? 0;
    // Alpha rides globalAlpha at draw time so ghosted text (wrap previews,
    // fades) shares the opaque sprite instead of forking the cache.
    const key = `${font || "_"}|${size}|${scale}|${r},${g},${b}|${str}`;
    let spr = sprites.get(key);
    if (spr === undefined) {
      if (sprites.size >= SPRITE_CAP) sprites.clear();
      spr = makeSprite(str, font, size, scale, `rgb(${r},${g},${b})`);
      sprites.set(key, spr);
    }
    if (a < 1) ctx.globalAlpha = a;
    if (pos.rot) {
      // Spin around the label's own center (diagonal cell fits — see fitText).
      const w2 = str.length * adv * size, h2 = h * size;
      ctx.save();
      ctx.translate(x + w2 / 2, y + h2 / 2);
      ctx.rotate((pos.rot * Math.PI) / 180);
      ctx.drawImage(spr.c, -w2 / 2 - spr.pad, -h2 / 2 - spr.pad, spr.w, spr.h);
      ctx.restore();
    } else {
      ctx.drawImage(spr.c, x - spr.pad, y - spr.pad, spr.w, spr.h);
    }
    if (a < 1) ctx.globalAlpha = 1;
  };
  const chain = { box, line, write, hd: true };
  const ink = (...args) => {
    setColor(args);
    return chain;
  };
  const wipe = (...args) => {
    setColor(args);
    fill();
    ctx.fillRect(0, 0, width, height);
  };
  const api = { wipe, ink, box, line, write, screen: { width, height } };
  hdCached = { ctx, width: layer.width, height: layer.height, api };
  return api;
}

function paintGame({ wipe, ink, screen, write, box, line, text }) {
  computeLayout(screen);
  wipe(...T.bg);
  paintBackground({ ink, box, screen });

  applyCamera(screen); // dynamic camera → updates layout in place
  const { x, y, cell } = layout;

  const tf = beatsMax ? beatsLeft / beatsMax : 0;
  const lowTime = beatsLeft <= 3 && state === "play";
  const blink = (frames >> 2) % 2 === 0; // fast ~7.5Hz blink

  // 😱 Whole-screen red blink when about to die.
  if (lowTime && blink) ink(150, 0, 0, 115).box(0, 0, screen.width, screen.height);

  // ⏳ Beat timeline bar across the very top (height rides the HUD scale).
  const tcol = tf > 0.5 ? T.timeHi : tf > 0.25 ? T.timeMid : T.timeLow;
  const barH = 2 + hudScale + (beatPulse > 4 ? 1 : 0);
  ink(...T.trackBg).box(0, 0, screen.width, barH);
  ink(...tcol).box(0, 0, round(screen.width * tf), barH);

  // ⏳ Big timer (beats left), top-left — blinks near zero.
  const timeCol = lowTime ? (blink ? T.timerPanic : T.timerPanicAlt) : tcol;
  bigNum(ink, `${beatsLeft}`, 8, 8, timerSize, timeCol);

  // Rule title, top-right (no levels, no score). Word editions render it in
  // unifont so Cyrillic / accented category labels (ЕДА, ФРУКТЫ, …) show real
  // glyphs. Sized to the room left of the big timer so the two never collide
  // on narrow phone screens.
  const labelFont = cellFont();
  const { adv: lAdv, h: lH } = fontMetrics(labelFont);
  const timerEnd = 8 + `${beatsLeft}`.length * 6 * timerSize + 12; // timer's right edge
  let ruleSize = 1 + hudScale;
  while (ruleSize > 1 && timerEnd + ruleLabel.length * lAdv * ruleSize + 8 > screen.width)
    ruleSize -= 1;
  const ruleX = max(timerEnd, screen.width - 8 - ruleLabel.length * lAdv * ruleSize);
  ink(...T.ruleLabel).write(
    ruleLabel,
    { x: ruleX, y: max(6, round((layout.top - lH * ruleSize) / 2)), size: ruleSize },
    undefined, undefined, false, labelFont || undefined,
  );

  // Grid — two passes: every tile background/outline first, THEN every label.
  // Words always render whole at scale 1 (see fitText), so on a very tight
  // screen a long word can be a touch wider than its cell; painting all the
  // backgrounds first lets it bleed over the tile seam instead of getting
  // overpainted by its neighbour's background.
  const gameOver = state === "over";
  for (let r = 0; r < ROWS; r += 1) {
    for (let c = 0; c < COLS; c += 1) {
      const cx = x + c * cell;
      const cy = y + r * cell;
      const cd = grid[idx(c, r)];
      const txt = String(cd.value);
      const colorWord = COLOR_WORDS[txt]; // color-name → real color
      const missed = gameOver && cd.correct && !cd.eaten; // answer you didn't get

      // Tile background.
      let bg;
      if (cd.flash > 0) bg = cd.correct ? [40, 150, 70] : [160, 45, 45];
      else if (cd.eaten) bg = T.eaten;
      else if (cd.failed) bg = T.failedBg;
      else if (cd.known) bg = [40, 168, 80]; // confirmed-correct → whole square green
      else if (missed) bg = T.missedBg; // revealed missed answer = gray
      else if (mode === "note") bg = noteTile(cd.letter); // pitch-class tint
      else if (colorWord) bg = colorWord; // color squares ARE that color
      else bg = tileColor(txt);
      ink(...bg).box(cx + 1, cy + 1, cell - 2, cell - 2);

      // Outline.
      if (cd.known && !cd.eaten)
        ink(...T.knownOutline).box(cx + 1, cy + 1, cell - 2, cell - 2, "outline");
      else if (missed)
        ink(...T.missedOutline).box(cx + 1, cy + 1, cell - 2, cell - 2, "outline");
      else ink(...T.outline).box(cx + 1, cy + 1, cell - 2, cell - 2, "outline");

      // 🖱️ Hover highlight.
      if (hover && hover.col === c && hover.row === r && !cd.eaten && state === "play") {
        ink(...T.hoverFill).box(cx + 1, cy + 1, cell - 2, cell - 2);
        ink(...T.hoverOutline).box(cx + 1, cy + 1, cell - 2, cell - 2, "outline");
      }
    }
  }
  for (let r = 0; r < ROWS; r += 1) {
    for (let c = 0; c < COLS; c += 1) {
      const cd = grid[idx(c, r)];
      if (cd.eaten) continue;
      const cx = x + c * cell;
      const cy = y + r * cell;
      const txt = String(cd.value);
      const colorWord = COLOR_WORDS[txt];
      const missed = gameOver && cd.correct && !cd.eaten;

      // Value — ONE color per answer (color-words use their hue + contrast),
      // centered in the cell. Word editions: unifont, whole word, scale 1.
      const font = cellFont();
      let jx = 0, jy = 0;
      if (cd.known && !cd.failed) {
        jx = sin(frames * 0.9 + c * 1.7) * 1.6; // afraid jitter
        jy = cos(frames * 1.1 + r * 1.3) * 1.2;
      }
      let col;
      if (cd.failed) col = [120, 95, 100];
      else if (cd.known) col = [10, 40, 18]; // dark on the green known square
      else if (missed) col = T.missedText;
      else if (mode === "note") col = noteColor(cd.letter); // note's keyboard hue
      else if (colorWord) col = luma(colorWord) > 140 ? [25, 25, 30] : [245, 245, 255];
      else col = charColor(txt[0]);
      drawCellText(ink, txt, cx, cy, cell, font, col, 255, jx, jy);

      // ✖ X out tried-and-wrong cells (can't be tried again).
      if (cd.failed) {
        ink(225, 70, 70);
        line(cx + 4, cy + 4, cx + cell - 4, cy + cell - 4);
        line(cx + cell - 4, cy + 4, cx + 4, cy + cell - 4);
      }
    }
  }

  // 👻 Wrap preview — ONLY the wrap moves available from the muncher's current
  // edge (the squares closest to its possible moves), ghosted just outside that
  // edge. No far-side / whole-edge highlights.
  const wrapHint = (destCol, destRow, gx, gy) => {
    const cd = grid[idx(destCol, destRow)];
    if (!cd || cd.eaten) return;
    const t = String(cd.value);
    const font = cellFont();
    const cw = COLOR_WORDS[t];
    const gc = mode === "note" ? noteColor(cd.letter) : cw ? themedText(cw) : charColor(t[0]);
    drawCellText(ink, t, gx, gy, cell, font, gc, 95);
    ink(...T.wrapHint).box(gx + 2, gy + 2, cell - 4, cell - 4, "outline");
  };
  const mc = muncher.col,
    mr = muncher.row;
  if (state === "play" && deathPhase === 0) {
    if (mc === 0) wrapHint(COLS - 1, mr, x - cell, y + mr * cell); // wrap-left
    if (mc === COLS - 1) wrapHint(0, mr, x + COLS * cell, y + mr * cell); // wrap-right
    if (mr === 0) wrapHint(mc, ROWS - 1, x + mc * cell, y - cell); // wrap-up
    if (mr === ROWS - 1) wrapHint(mc, 0, x + mc * cell, y + ROWS * cell); // wrap-down

    // ✅ Emphasize any adjacent confirmed-correct (green) square you can step
    // onto and munch — a pulsing bright-green ring beckons.
    const pul = round(70 + 90 * (0.5 + 0.5 * sin(frames * 0.28)));
    for (const [dx, dy] of [[0, -1], [0, 1], [-1, 0], [1, 0]]) {
      const nc = (mc + dx + COLS) % COLS,
        nr = (mr + dy + ROWS) % ROWS;
      const cd = grid[idx(nc, nr)];
      if (cd && cd.known && !cd.eaten && !cd.failed) {
        const ncx = x + nc * cell,
          ncy = y + nr * cell;
        ink(T.beckon[0], T.beckon[1], T.beckon[2], pul).box(ncx + 1, ncy + 1, cell - 2, cell - 2, "outline");
        ink(T.beckon[0], T.beckon[1], T.beckon[2], pul).box(ncx + 2, ncy + 2, cell - 4, cell - 4, "outline");
      }
    }
  }

  // Troggles.
  troggles.forEach((t) => {
    const cx = x + t.col * cell;
    const cy = y + t.row * cell;
    paintTroggle({ ink, box, line }, cx, cy, cell, t.hue);
  });

  // Muncher — dying, celebrating, or normal.
  if (deathPhase > 0) {
    const cx = x + deathPos.col * cell;
    const cy = y + deathPos.row * cell;
    paintDeadMuncher({ ink, box, line }, cx, cy, cell);
  } else if (!(invuln > 0 && (invuln >> 2) % 2 === 0)) {
    const cx = x + muncherVis.col * cell;
    const cy = y + muncherVis.row * cell;
    paintMuncher({ ink, box }, cx, cy, cell, clearPhase > 0);
  }

  // ✨ Confetti (level-clear).
  confetti.forEach((p) => ink(p.c[0], p.c[1], p.c[2]).box(p.x, p.y, p.s, p.s));

  // 🎶 / 🔴 / 🟡 / 🟢 Full-screen washes — the beat vibe pulse plus the
  // death/clear/board-start flashes, folded into ONE src-over-composited fill
  // so a busy frame pays a single screen of overdraw instead of four.
  let wr = 0, wg = 0, wb = 0, wa = 0;
  const addWash = (rr, gg, bb, aa) => {
    const af = aa / 255;
    const na = af + wa * (1 - af);
    if (na <= 0) return;
    wr = (rr * af + wr * wa * (1 - af)) / na;
    wg = (gg * af + wg * wa * (1 - af)) / na;
    wb = (bb * af + wb * wa * (1 - af)) / na;
    wa = na;
  };
  if (beatPulse > 0 && state === "play") {
    const vibe = [[80, 120, 255], [80, 220, 230], [170, 110, 240]][(beatsMax - beatsLeft) % 3];
    addWash(vibe[0], vibe[1], vibe[2], beatPulse * 4);
  }
  if (flashRed > 0) addWash(210, 25, 25, min(190, flashRed * 11));
  if (flashGold > 0) addWash(255, 215, 90, min(150, flashGold * 11));
  if (flashGreen > 0) addWash(60, 220, 110, min(150, flashGreen * 11));
  if (wa > 0)
    ink(round(wr), round(wg), round(wb), round(wa * 255)).box(0, 0, screen.width, screen.height);

  // 🔢 Items-left, bottom-right, big — riding just above the caption strip.
  const hudBottom = screen.height - captionBandH(screen);
  const leftN = `${remaining}`;
  const lsz = timerSize;
  const leftW = leftN.length * 6 * lsz;
  const leftX = screen.width - 8 - leftW;
  bigNum(ink, leftN, leftX, hudBottom - 8 * lsz - 4, lsz, remaining <= 3 ? T.leftWarn : T.leftOk);
  const lls = max(1, hudScale);
  ink(...T.leftLabel).write("LEFT", {
    x: max(4, leftX - ("LEFT".length * 6 * lls + 8)),
    y: hudBottom - 8 * lls - 8,
    size: lls,
  });

  // 🍽️ Collected answers — stack down the left, measured to fit the margin.
  // Word boards run near edge-to-edge, so on narrow screens there's no gutter
  // at all — skip the list rather than stack it over the board.
  let fcy = layout.top + 6; // below the header band
  const leftRoom = layout.x - 6;
  const chipMin = 18; // slimmest gutter that still fits a size-1 chip
  const font = cellFont();
  const { adv: fAdv, h: fH } = fontMetrics(font);
  for (const v of foundValues) {
    if (leftRoom < chipMin) break;
    // Largest size whose nominal width fits the left gutter (stable, no async).
    let size = 1;
    for (let s = hudScale + 1; s >= 2; s -= 1)
      if (v.length * fAdv * s + 4 <= leftRoom) { size = s; break; }
    const fit = { size, width: v.length * fAdv * size, height: fH * size };
    if (fcy + fit.height > hudBottom - 10) break;
    const w = fit.width + 4;
    const ltr = mode === "note" ? v.slice(0, -1) : null; // "C4" → "C", "F#5" → "F#"
    const cw = mode === "note" ? null : COLOR_WORDS[v];
    const bgc = mode === "note" ? noteTile(ltr) : cw || tileColor(v);
    ink(...bgc).box(4, fcy, w, fit.height + 2);
    const tcol = mode === "note"
      ? noteColor(ltr)
      : cw ? (luma(cw) > 140 ? [25, 25, 30] : [245, 245, 255]) : charColor(v[0]);
    ink(...tcol).write(v, { x: 6, y: fcy + 1, size: fit.size }, undefined, undefined, false, font || undefined);
    fcy += fit.height + 4;
  }

  // 👆 Tap-to-move target — a pulsing golden ring on the destination square
  // so the walk-in-progress is always legible.
  if (walkTarget && state === "play" && deathPhase === 0) {
    const pul = round(120 + 100 * (0.5 + 0.5 * sin(frames * 0.4)));
    const tx = x + walkTarget.col * cell,
      ty = y + walkTarget.row * cell;
    ink(255, 220, 90, pul).box(tx + 1, ty + 1, cell - 2, cell - 2, "outline");
    ink(255, 220, 90, round(pul * 0.6)).box(tx + 3, ty + 3, cell - 6, cell - 6, "outline");
  }

  // 📣 Transient message banner — large, with a colored background. Stacks
  // above the caption band when one is showing (all instruction above the
  // board, none below).
  if (message) {
    let ms = 1 + hudScale;
    while (ms > 1 && message.length * 6 * ms + 10 > screen.width) ms -= 1;
    const mw = message.length * 6 * ms;
    const mx = max(4, round(screen.width / 2 - mw / 2));
    const band = captionBand(screen);
    const myy = max(4, (band ? band.y : layout.y) - 8 * ms - 6);
    let mbg, mfg;
    if (/OUCH|TIME|OVER/.test(message)) (mbg = [205, 45, 45]), (mfg = [255, 240, 240]);
    else if (/CLEAR/.test(message)) (mbg = [45, 175, 85]), (mfg = [12, 30, 14]);
    else if (/LAST/.test(message)) (mbg = [235, 200, 60]), (mfg = [40, 28, 0]);
    else (mbg = [80, 90, 150]), (mfg = [240, 240, 255]);
    ink(...mbg).box(mx - 5, myy - 3, mw + 10, 8 * ms + 6);
    ink(0, 0, 0, 110).box(mx - 5, myy - 3, mw + 10, 8 * ms + 6, "outline");
    ink(...mfg).write(message, { x: mx, y: myy, size: ms });
  }

  // 🪧 Intro card — name + native edition, fading over the live board. No
  // instructions, no "press any key": the board is already playing.
  if (introTimer > 0) paintIntro({ ink, screen, write, text });

  // Let the celebration play out, then show the prompt overlay.
  if (state === "clear" && clearPhase === 0)
    overlay({ ink, screen, write }, "LEVEL CLEAR", "press any key");
  if (state === "over")
    overlay({ ink, screen, write }, "GAME OVER", "press to retry");
}

// Cute monster: fatter the more it has eaten; shrinks + trembles when starving
// (low on beats). Its mouth only moves while actually munching.
// 🌌 Subtle animated backdrop — a slowly drifting dot grid that brightens on
// each beat. Sits behind everything, low-contrast so it never distracts.
function paintBackground({ ink, box, screen }) {
  // Spacing scales with the viewport so the dot count stays bounded (~15×15
  // worst case) on big desktop windows instead of growing with screen area.
  const sp = max(40, Math.ceil(screen.width / 15), Math.ceil(screen.height / 15));
  const dx = (frames * 0.18) % sp;
  const dy = (frames * 0.11) % sp;
  const pulse = beatPulse > 0 ? 6 : 0;
  for (let gy = -sp; gy < screen.height + sp; gy += sp) {
    for (let gx = -sp; gx < screen.width + sp; gx += sp) {
      // Faint, just barely off the background — brighter on dark, dimmer on light.
      const b = 4 + pulse + 3 * sin((gx + gy) * 0.05 + frames * 0.035);
      if (dark) ink(14 + round(b * 0.4), 16 + round(b * 0.5), 32 + round(b)); // dim blue-violet
      else ink(225 - round(b * 1.2), 229 - round(b * 1.1), 240 - round(b * 0.6)); // soft graphite-blue
      box(gx + dx, gy + dy, 2, 2);
    }
  }
}

// Big number in the default font (kerns cleanly at scale, unlike unifont).
// Returns the end x. Default glyph advance ≈ 6px × size.
function bigNum(ink, txt, x, y, sz, col) {
  ink(...col).write(txt, { x, y, size: sz });
  return x + txt.length * 6 * sz;
}

function paintMuncher({ ink, box }, cx, cy, cell, dance = false) {
  const fullness = max(0, min(1, 1 - remaining / boardCorrectTotal)); // hungry→full
  let scale = 0.72 + fullness * 0.4; // skinny → round
  const dying = beatsLeft <= 3 && state === "play";
  if (dying) scale *= 0.66 + 0.12 * abs(sin(frames * 0.8)); // shrink, panicked
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

  // 🦵 Legs — two simple feet that bob.
  const bob = (frames >> (dance ? 1 : 3)) % 2;
  const legW = max(2, floor(s * 0.26));
  const footY = y + bodyH;
  ink(60, 175, 80);
  box(x + s * 0.12, footY - (bob ? 1 : 0), legW, legH);
  box(x + s * 0.62, footY - (bob ? 0 : 1), legW, legH);

  // 🟩 Body — one flat healthy green; brightens if time is running out.
  const hc = [125, 235, 130];
  ink(...(dying ? [min(255, hc[0] + 30), min(255, hc[1] + 20), hc[2]] : hc)).box(x, y, s, bodyH);

  // 👀 Eyes — two squares; pupil glances toward the last move direction.
  const eo = max(2, floor(s * 0.2));
  const ey = y + s * 0.2;
  const exL = x + s * 0.18,
    exR = x + s * 0.56;
  ink(255).box(exL, ey, eo, eo);
  ink(255).box(exR, ey, eo, eo);
  const pup = max(1, floor(eo * 0.5));
  const gx = floor((eo - pup) / 2) * facing.x;
  const gy = floor((eo - pup) / 2) * facing.y;
  ink(20, 40, 25).box(exL + (eo - pup) / 2 + gx, ey + (eo - pup) / 2 + gy, pup, pup);
  ink(20, 40, 25).box(exR + (eo - pup) / 2 + gx, ey + (eo - pup) / 2 + gy, pup, pup);

  // 👄 Mouth — closed line; opens to a simple dark square while munching.
  const my = y + bodyH * 0.66;
  if (chompPhase > 0) {
    ink(25, 40, 25).box(x + s * 0.3, my - s * 0.05, s * 0.4, s * 0.26);
  } else {
    ink(40, 80, 45).box(x + s * 0.32, my, s * 0.36, max(1, floor(s * 0.08)));
  }
}

// 💀 The Muncher's sad end: turns deep red, sinks + squashes, X-ed-out eyes,
// a frown, and a falling blue tear.
function paintDeadMuncher({ ink, box, line }, cx, cy, cell) {
  const inset = cell * 0.12;
  const s = cell - inset * 2;
  const p = 1 - deathPhase / 54; // 0 → 1 over the animation
  const sink = s * 0.55 * p;
  const x = cx + inset;
  const y = cy + inset + sink;
  const bodyH = s * (1 - 0.3 * p); // squash down as it sinks
  const a = max(20, 255 - p * 130); // fade out

  // Body — deep, sad red.
  ink(170 - p * 70, 28, 30, a).box(x, y, s, bodyH);
  ink(90, 12, 14, a).box(x, y, s, bodyH, "outline");

  // ✖ Dead eyes.
  const ex = max(3, floor(s * 0.16));
  const ey = y + s * 0.16;
  ink(25, 0, 0, a);
  const xeye = (ox) => {
    line(x + ox, ey, x + ox + ex, ey + ex);
    line(x + ox + ex, ey, x + ox, ey + ex);
  };
  xeye(s * 0.16);
  xeye(s * 0.56);

  // ☹ Frown.
  ink(30, 0, 0, a);
  const my = y + bodyH * 0.64;
  box(x + s * 0.3, my, s * 0.4, max(1, floor(s * 0.08)));
  box(x + s * 0.26, my - max(1, floor(s * 0.06)), max(1, floor(s * 0.08)), max(1, floor(s * 0.06)));
  box(x + s * 0.66 - s * 0.08, my - max(1, floor(s * 0.06)), max(1, floor(s * 0.08)), max(1, floor(s * 0.06)));

  // 💧 Tear.
  if ((frames >> 2) % 2)
    ink(120, 180, 255, a).box(
      x + s * 0.22,
      ey + ex + 2 + p * s * 0.4,
      max(1, floor(s * 0.08)),
      max(2, floor(s * 0.16)),
    );
}

// 😠 Troggle — an angry red enemy: eyes with pupils, slanted-down eyebrows,
// and a jagged downturned mouth with fangs.
function paintTroggle({ ink, box, line }, cx, cy, cell, hue) {
  const pad = cell * 0.16;
  const s = cell - pad * 2;
  const x = cx + pad;
  const y = cy + pad;
  ink(205, 48, 46).box(x, y, s, s); // red body
  ink(120, 14, 14).box(x, y, s, s, "outline");

  // Eyes (white) with dark pupils that point at the muncher's general area.
  const eo = max(2, floor(s * 0.2));
  const ey = y + s * 0.32;
  const exL = x + s * 0.16,
    exR = x + s * 0.56;
  ink(255).box(exL, ey, eo, eo);
  ink(255).box(exR, ey, eo, eo);
  const pup = max(1, floor(eo * 0.5));
  ink(25, 0, 0).box(exL + (eo - pup) / 2, ey + (eo - pup) / 2 + 1, pup, pup);
  ink(25, 0, 0).box(exR + (eo - pup) / 2, ey + (eo - pup) / 2 + 1, pup, pup);

  // 😠 Angry eyebrows — slant down toward the center.
  ink(20, 0, 0);
  line(x + s * 0.1, y + s * 0.2, x + s * 0.38, y + s * 0.3);
  line(x + s * 0.9, y + s * 0.2, x + s * 0.62, y + s * 0.3);

  // 😡 Jagged, downturned mouth with two fangs.
  const my = y + s * 0.7;
  ink(25, 0, 0);
  line(x + s * 0.22, my + s * 0.06, x + s * 0.4, my - s * 0.04);
  line(x + s * 0.4, my - s * 0.04, x + s * 0.6, my - s * 0.04);
  line(x + s * 0.6, my - s * 0.04, x + s * 0.78, my + s * 0.06);
  ink(255);
  box(x + s * 0.4, my - s * 0.04, max(1, floor(s * 0.08)), max(1, floor(s * 0.1)));
  box(x + s * 0.54, my - s * 0.04, max(1, floor(s * 0.08)), max(1, floor(s * 0.1)));
}

function gameName() {
  if (mode === "note") return "NOTENOM";
  if (mode !== "word") return "NUMBNOM";
  return lang === "es" ? "MEXINOM" : lang === "da" ? "DANNOM" : lang === "ru" ? "RUSNOM" : lang === "cat" ? "CATNOM" : "ENGNOM";
}
// Native edition subtitle, in each language's own script/diacritics. Rendered in
// unifont (carries accents + Cyrillic) so the symbols are correct.
function editionText() {
  if (mode === "note") return "music edition";
  if (mode !== "word") return "number edition";
  return lang === "es" ? "edición mexicana"
    : lang === "da" ? "dansk udgave"
    : lang === "ru" ? "русское издание"
    : lang === "cat" ? "category edition"
    : "english edition";
}

// 🪧 Intro card over the live board — just the name + native edition, sized to
// the screen, fading out near the end. No instructions, no "press any key".
function paintIntro({ ink, screen, write }) {
  const fade = introTimer > 44 ? 1 : introTimer / 44; // ease out over the last ~0.7s
  const A = (v) => max(0, round(v * fade));
  // Dim the board so the title reads, but let it show through (board already up).
  ink(T.bg[0], T.bg[1], T.bg[2], A(dark ? 165 : 190)).box(0, 0, screen.width, screen.height);

  const cy = round(screen.height / 2);
  // Responsive game name (ASCII, default 6px font): fit ~78% of the width,
  // scaled up with the display (HUD scale) for readability.
  const name = gameName();
  const nameSize = max(1, min(2 + hudScale * 2, floor((screen.width * 0.78) / max(1, name.length * 6))));
  const nameH = nameSize * 10;
  ink(T.introName[0], T.introName[1], T.introName[2], A(255)).write(name, { center: "x", y: cy - nameH, size: nameSize });

  // Native edition subtitle in unifont (accents + Cyrillic render correctly).
  const sub = editionText();
  const subSize = max(1, min(hudScale + 1, floor((screen.width * 0.8) / max(1, sub.length * 8))));
  ink(T.introSub[0], T.introSub[1], T.introSub[2], A(255)).write(
    sub,
    { center: "x", y: cy + max(6, nameSize * 3), size: subSize },
    undefined, undefined, false, "unifont",
  );
}

function overlay({ ink, screen, write }, title, sub) {
  // Lighter scrim so the board (incl. revealed missed answers) shows through.
  ink(0, 0, 0, 125).box(0, 0, screen.width, screen.height);
  // Big title on a colored banner — red for game over, green for clear.
  const over = /OVER/.test(title);
  const bg = over ? [195, 45, 45] : [45, 175, 85];
  const ts = max(2, min(2 + hudScale, floor((screen.width * 0.85) / (title.length * 6))));
  const tw = title.length * 6 * ts;
  const tx = max(4, round(screen.width / 2 - tw / 2));
  const ty = round(screen.height / 2 - 8 * ts - 4);
  ink(...bg).box(tx - 10, ty - 6, tw + 20, 8 * ts + 12);
  ink(0, 0, 0, 150).box(tx - 10, ty - 6, tw + 20, 8 * ts + 12, "outline");
  ink(255, 248, 230).write(title, { x: tx, y: ty, size: ts });
  const ss = max(1, round(ts / 2));
  ink(...T.overlaySub).write(sub, { center: "x", y: round(screen.height / 2 + 8 * ts), size: ss });
}

// 📰 Meta — built from explicit params so the title is correct at load time,
// independent of boot() ordering or leftover singleton state. Each wrapper
// piece exports `meta() { return makeMeta([<its mode>]); }`.
function makeMeta(params) {
  const { mode: m, lang: l } = resolveMode(params);
  const title = m === "note"
    ? "Notenom"
    : m !== "word"
      ? "Numbnom"
      : l === "es" ? "Mexinom" : l === "da" ? "Dannom" : l === "ru" ? "Rusnom" : l === "cat" ? "Catnom" : "Engnom";
  return {
    title,
    desc: "Eat the squares that match the rule — numbnom (numbers), engnom (words), mexinom (español), dannom (dansk), rusnom (русский), notenom (notes), catnom (categories).",
  };
}
function meta() {
  return makeMeta([mode === "note" ? "notes" : mode === "word" ? lang : "numbers"]);
}

export { boot, sim, paint, act, meta, makeMeta };
