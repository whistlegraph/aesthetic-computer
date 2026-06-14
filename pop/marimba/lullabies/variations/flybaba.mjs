// flybaba.mjs — "lately, when i fly" — the butterfly motif lifts the whole
// piece up and away by literally FLYING THROUGH KEYS.
//
// The marimbaba seed has a butterfly phrase tucked into bars 8-9 (the flyHigh
// MOTIF: Bb5 D6 Bb5 C6 A5 — "way up high"). flybaba pulls that motif out front
// and makes it the LEAD, then develops it with CONTINUOUS UPWARD MODULATION —
// a chain of truck-driver key changes. The butterfly tune is restated again
// and again, each restatement bumped up into a NEW KEY (a whole step, then a
// minor third, then a step…) so it climbs an ecstatic ladder: F → G → Bb →
// C → D → F, the register and brightness rising with every lift. The whole
// sky-bed (vibraphone pad + bass root) modulates with it, so each key feels
// like a fresh altitude. At the very top the tune trills and shivers in the
// glockenspiel's brightest air; then one long, calm glide carries it all the
// way back down to ground-level F, where the marimbaba hush sigh sets it to
// sleep — closing the frame it opened with.
//
// We keep marimbaba's DNA: the descending hush sigh opens & closes it, the
// twinkle wave answers, the flyHigh contour is the recurring thread — but the
// melody now TRAVELS through six keys instead of restating in place. Voicing:
// kalimba + glockenspiel lead over a vibraphone sky and a clear bass, ~56 BPM,
// F major home, floating-away mood intact.
//
// Run:  node variations/flybaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 56;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4, like the seed

const events = [];
const push = (preset, startSec, midi, durSec, gain, decayMul, pan) =>
  events.push({ preset, startSec, midi, durSec, gain, decayMul, pan });
const at = (bar, beat) => bar * BAR + beat * BEAT;

// ── tiny note-cell toolkit (cells are [noteName, beats]) ────────────────────
const TR = (cell, semis) =>
  cell.map(([n, b]) => [m(n) + semis, b]); // transpose into midi numbers
const RETRO = (cell) => [...cell].reverse();
const AUG = (cell, k) => cell.map(([n, b]) => [n, b * k]);

// Play a cell (already in midi) starting at bar/beat, return the bar AFTER it.
function play(preset, cell, startBar, startBeat, gain, decayMul, panBase, panSpread = 0.18) {
  let bar = startBar, beat = startBeat;
  const total = cell.reduce((s, [, b]) => s + b, 0);
  let acc = 0;
  for (const [midi, beats] of cell) {
    const pan = panBase + (acc / Math.max(total, 1)) * panSpread;
    push(preset, at(bar, beat), midi, beats * BEAT, gain, decayMul, pan);
    beat += beats; acc += beats;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
  return bar + (beat > 0 ? 1 : 0);
}

// ── the butterfly LEAD cell — flyHigh re-contoured to climb (low→high) ──────
// flyHigh = [Bb5,D6,Bb5,C6,A5]; re-shaped as a hopeful ASCENT that ends on a
// ringing leap. This is the recurring thread carried through every key.
const FLY = [
  ["A5", 1.5],
  ["C6", 1.5],
  ["D6", 1],
  ["C6", 1],
  ["F6", 3], // the leap "up high", left to ring
];
// a compact answer tail (the twinkle wave, diminished) to round each key off.
const TAIL = [["G6", 0.5], ["F6", 0.5], ["E6", 0.5], ["D6", 0.5], ["C6", 1]];

// ── soft vibraphone sky: an open major triad on the CURRENT key root ────────
function sky(startBar, bars, rootMidi, gain, bright = 0) {
  // root, third(+4), fifth(+7), plus a top octave color when bright
  const tones = [rootMidi, rootMidi + 4, rootMidi + 7];
  if (bright) tones.push(rootMidi + 12);
  for (let i = 0; i < tones.length; i++) {
    push("vibraphone_off", at(startBar, 0), tones[i], bars * BAR, gain,
      DECAY.vibraphone_off * 1.3, i === tones.length - 1 ? -0.2 : 0.16);
  }
}

// ── clear, sensible bass: the key root, low ─────────────────────────────────
function bass(bar, rootMidi, gain = 0.4) {
  push("bass", at(bar, 0), rootMidi, 3 * BEAT, gain, DECAY.bass, 0);
}

// ── the hush sigh — marimbaba's "hush-hush" — frames the flights ────────────
function hush(startBar, transpose, gain, pan, stretch = 1.0) {
  let bar = startBar, beat = 0;
  for (const [note, beats] of MOTIFS.hush) {
    push("kalimba", at(bar, beat), m(note) + transpose, beats * stretch * BEAT,
      gain, DECAY.kalimba * 1.4, pan);
    beat += beats;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
}

// a quick ascending grace flurry (the lift between keys — the wing-beat)
function flurry(startBar, startBeat, fromMidi, steps, gain) {
  // chromatic-ish run up a perfect fifth into the next key's root area
  const ivals = [0, 2, 4, 5, 7]; // a little major run
  let beat = startBeat, bar = startBar;
  for (let i = 0; i < steps; i++) {
    push("glockenspiel", at(bar, beat), fromMidi + ivals[i % ivals.length] + Math.floor(i / ivals.length) * 12,
      0.18 * BEAT, gain, DECAY.kalimba, 0.12 + i * 0.02);
    beat += 0.25;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
}

// ═══════════════════════════ ARRANGE ════════════════════════════════════════
// The KEY LADDER. Each entry = { name, root (bass midi), shift (semitones to
// transpose FLY/TAIL from F-major home), bright, preset }. The butterfly tune
// climbs F → G → Bb → C → D, then glides home to F.  shift is measured from
// the home key F (the FLY cell as written sits in F major already).
const HOME = m("F2"); // bass home root

// ── 0) GROUND LEVEL: low hush sigh + open F sky (bars 0-3) ──────────────────
hush(0, -12, 0.34, -0.12, 1.2);
sky(0, 4, m("F3"), 0.15);
bass(0, HOME); bass(2, HOME);

// ── 1) FIRST FLIGHT — key of F (home). kalimba, soft. (bars 4-8) ────────────
let bar = 4;
play("kalimba", TR(FLY, 0), bar, 0, 0.34, DECAY.kalimba * 1.5, -0.14);
sky(4, 4, m("F3"), 0.15);
bass(4, HOME); bass(6, HOME);
// answer: the twinkle wave (mid-piece reply), in F
play("vibraphone", TR(TAIL, -7), 7, 0, 0.30, DECAY.vibraphone * 1.3, 0.22, 0.1);

// ── wing-beat lift up to G ──────────────────────────────────────────────────
flurry(8, 1.5, m("E5"), 6, 0.22);

// ── 2) SECOND FLIGHT — MODULATE UP A WHOLE STEP to G major (+2). (bars 9-13) ─
play("kalimba", TR(FLY, 2), 9, 0, 0.35, DECAY.kalimba * 1.45, -0.04, 0.2);
sky(9, 4, m("G3"), 0.15, 0); // sky lifts to G
bass(9, m("G2")); bass(11, m("D3"));
play("vibraphone", TR(TAIL, -5), 12, 0, 0.30, DECAY.vibraphone * 1.3, 0.2, 0.12);

// ── wing-beat lift up toward Bb (a minor third up) ──────────────────────────
flurry(13, 1.5, m("G5"), 7, 0.24);

// ── 3) THIRD FLIGHT — MODULATE UP A MINOR THIRD to Bb major (+5). (bars 14-18)
play("kalimba", TR(FLY, 5), 14, 0, 0.36, DECAY.kalimba * 1.4, 0.02, 0.22);
sky(14, 4, m("Bb3"), 0.15, 1); // brighter sky, top color
bass(14, m("Bb2")); bass(16, m("F3"));
// a high hush echo drifting far right as it keeps rising
hush(17, 0, 0.22, 0.32, 1.3);

// ── wing-beat lift toward C ─────────────────────────────────────────────────
flurry(18, 1.5, m("Bb5"), 6, 0.25);

// ── 4) FOURTH FLIGHT — MODULATE UP A STEP to C major (+7). glockenspiel takes
//      the lead here — the air gets bright and glassy. (bars 19-23) ──────────
play("glockenspiel", TR(FLY, 7), 19, 0, 0.30, DECAY.kalimba * 1.3, 0.06, 0.24);
sky(19, 4, m("C4"), 0.13, 1);
bass(19, m("C2")); bass(21, m("G2"));
play("vibraphone", TR(TAIL, 0), 22, 0, 0.28, DECAY.vibraphone * 1.3, 0.2, 0.14);

// ── wing-beat lift to the summit, D ─────────────────────────────────────────
flurry(23, 1.5, m("C6"), 6, 0.26);

// ── 5) SUMMIT FLIGHT — MODULATE UP A STEP to D major (+9). highest, widest,
//      ecstatic. glockenspiel, plus a shimmering trill at the peak. (bars 24-28)
play("glockenspiel", TR(FLY, 9), 24, 0, 0.30, DECAY.kalimba * 1.25, 0.12, 0.26);
sky(24, 5, m("D4"), 0.12, 1);
bass(24, m("D2")); bass(26, m("A2"));
// the peak trill — a fast oscillation on the top F#/A, the butterfly hovering
{
  let t = at(27, 0);
  const top = [m("A6"), m("F#6")];
  for (let i = 0; i < 8; i++) {
    push("glockenspiel", t, top[i % 2], 0.16, 0.22, DECAY.kalimba, 0.18);
    t += 0.16;
  }
}

// ── 6) THE LONG GLIDE BACK TO EARTH — one slow descending arc that steps the
//      keys back down D → C → Bb → G → F, augmented (stretched) so it feels
//      like a calm settling spiral. We use the FLY contour in RETROGRADE +
//      AUGMENTED, sinking through the ladder, register dropping each step.
//      (bars 29-39) ──────────────────────────────────────────────────────────
const glideKeys = [
  { shift: 9, root: m("D3"), oct: 0 },   // D
  { shift: 7, root: m("C3"), oct: -2 },  // C
  { shift: 5, root: m("Bb2"), oct: -4 }, // Bb
  { shift: 2, root: m("G2"), oct: -7 },  // G
];
let gb = 29;
const GLIDE = AUG(RETRO(FLY), 1.4); // backwards + stretched = a sinking sigh
for (const k of glideKeys) {
  const cell = TR(GLIDE, k.shift + k.oct).map(([mi, b]) => [mi, b]);
  play("vibraphone", cell, gb, 0, 0.26, DECAY.vibraphone * 1.4, 0.0, 0.12);
  sky(gb, 2, k.root, 0.12, 0);
  bass(gb, k.root - 12);
  gb += 2;
}

// ── 7) HOME / GROUND: the hush sigh returns low in F to set it down gently,
//      and one last high F left ringing off into the sky. (bars 37-41) ───────
const homeBar = gb; // ~37
hush(homeBar, -12, 0.30, -0.1, 1.4);
sky(homeBar, 3, m("F3"), 0.13);
bass(homeBar, HOME); bass(homeBar + 2, HOME);
// a final sleep-settle of marimbaba's closing descent, low and slow
play("kalimba", TR(MOTIFS.sleep, -12), homeBar, 0, 0.0, DECAY.kalimba, 0); // (silent placeholder removed below)
events.pop(); // drop the placeholder
play("kalimba", AUG(MOTIFS.sleep.map(([n, b]) => [m(n) - 12, b]), 1.2), homeBar + 3, 0, 0.24, DECAY.kalimba * 1.4, -0.08, 0.1);
// last butterfly note, high F, drifting away
push("kalimba", at(homeBar + 6, 0), m("F6"), 4.0 * BEAT, 0.20, DECAY.kalimba * 1.7, 0.28);

const { mp3, durationSec } = renderLullaby(events, {
  name: "flybaba",
  here: HERE,
  title: "flybaba",
  reverb: { wet: 0.42, decay: 0.9, damp: 0.3 }, // big airy sky for the lift
  fadeIn: 1.0,
  fadeOut: 5.5,
  tailSec: 6.0,
  peak: 0.84,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
