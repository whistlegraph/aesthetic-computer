// tuckbaba.mjs — the last lullaby, by LIQUIDATION (reverse-evolution).
//
// Every other variation winds down; tuckbaba IS the wind-down — but this time
// it earns the dark. It OPENS at full strength: the whole marimbaba tune,
// elaborated as densely as the seed ever gets — hush, twinkle, the wow wobble,
// the slinky ba-ba-ba-bap, the sleep settle — all stated at once with grace
// flurries, a vibraphone pad and kalimba sparkles, like a child wide awake and
// telling the whole story. Then it LIQUIDATES. Each pass keeps less: the
// ornaments go first, then whole phrases, then the contour itself flattens.
// The melody loses its top, widens its gaps, lengthens its rings. The slinky
// folds into the sleep descent; the sleep descent erodes to a falling third;
// the falling third erodes to a single low F; and the F dissolves into silence.
// One kalimba "goodnight" rings far away as the last waking thought.
//
// We stay home in F major (the seed's own key) and, as the liquidation deepens,
// fold the melody to F-major pentatonic {0,2,4,7,9} so the descent loses every
// edge — no leading tone to keep you awake. So the arc is also a smoothing:
// chromatic-rich at the top, pure-pentatonic at the bottom. The bass F2
// heartbeat slows and softens with each pass, the breaths spacing out as
// breathing does in sleep.
//
// Riffs the seed by: stating the ENTIRE marimbaba tune up front (not just its
// closing phrase) and then reverse-developing it — progressive liquidation —
// until only the seed's last gesture, the falling third to F, remains, and then
// nothing. The kalimba sparkle is the lone surviving ornament from "wow."
//
// Run:  node variations/tuckbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 50; // a touch over the old 46 — the open is awake, then it slows
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;

// ── F major pentatonic: root pitch-class F (5), scale {0,2,4,7,9} ────────────
const ROOT_PC = m("F4") % 12; // 5
const PENTA = [0, 2, 4, 7, 9];

// Fold a midi note to nearest F-major-pentatonic pitch; ties resolve downward.
function snap(midi, rootPc = ROOT_PC, scale = PENTA) {
  let best = midi, bestD = Infinity;
  for (let oct = -1; oct <= 1; oct++) {
    for (const deg of scale) {
      const pc = (rootPc + deg) % 12;
      const base = Math.round((midi - pc) / 12) * 12 + pc + oct * 12;
      const d = Math.abs(base - midi);
      if (d < bestD - 1e-6 || (Math.abs(d - bestD) < 1e-6 && base < best)) {
        best = base; bestD = d;
      }
    }
  }
  return best;
}

// ── little developmental helpers (arrays of [note, beats]) ───────────────────
const tn = (cell, semis) => cell.map(([n, b]) => [m(n) + semis, b]); // transpose (->midi)
const aug = (cell, k) => cell.map(([n, b]) => [n, b * k]);            // augment durations
const dim = (cell, k) => cell.map(([n, b]) => [n, b / k]);           // diminish (faster)
const retro = (cell) => cell.slice().reverse();                      // retrograde
// drop every note past `keep` (keeps the head), and lengthen what survives.
const liquidate = (cell, keep, stretch = 1) =>
  cell.slice(0, keep).map(([n, b]) => [n, b * stretch]);

const events = [];

// ── place a [note,beats] cell as rosewood, optionally pentatonic-folded ──────
function place(cell, startBar, startBeat, gain, pan, {
  preset = "rosewood", durMul = 1, octave = 0, fold = false, decayK = 1.4,
  grace = 0, // beats: tiny grace note a step above, before each strong note
} = {}) {
  let bar = startBar, beat = startBeat;
  for (const [note, beats] of cell) {
    const raw = (typeof note === "number" ? note : m(note));
    const midi = (fold ? snap(raw) : raw) + octave;
    const t = bar * BAR + beat * BEAT;
    if (grace > 0) {
      events.push({
        preset, startSec: t - grace * BEAT,
        midi: midi + 2, durSec: grace * 1.2 * BEAT,
        gain: gain * 0.5, decayMul: (DECAY[preset] ?? 1.6) * 1.1, pan,
      });
    }
    events.push({
      preset, startSec: t,
      midi, durSec: beats * durMul * BEAT,
      gain, decayMul: (DECAY[preset] ?? 1.8) * decayK, pan,
    });
    beat += beats;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
}

// add a vibraphone pad chord (held), pentatonic-safe F major triad-ish.
function pad(notes, startBar, beats, gain, pan = -0.16) {
  for (const n of notes) {
    events.push({
      preset: "vibraphone_off", startSec: startBar * BAR,
      midi: m(n), durSec: beats * BEAT, gain,
      decayMul: (DECAY.vibraphone_off ?? 1.4) * 1.2, pan,
    });
  }
}

// add a kalimba sparkle (single high note, rings far).
function sparkle(note, startBar, startBeat, gain, durBeats, pan = 0.34) {
  events.push({
    preset: "kalimba", startSec: startBar * BAR + startBeat * BEAT,
    midi: m(note), durSec: durBeats * BEAT, gain,
    decayMul: (DECAY.kalimba ?? 1.75) * 1.6, pan,
  });
}

// ════════════════════════════════════════════════════════════════════════════
// PASS 0 — FULLEST STATEMENT (bars 0–11): the whole tune, wide awake.
// hush → twinkle → wow → ba-ba-ba-bap, ornamented, padded, sparkling.
// ════════════════════════════════════════════════════════════════════════════

// hush (descending sigh) with grace flurries — the most elaborate it gets.
place(MOTIFS.hush, 0, 0, 0.5, -0.05, { grace: 0.18, decayK: 1.2 });
// answer the hush a third up (sequence), softer, other side — call/response.
place(tn(MOTIFS.hush, 4), 2, 0, 0.4, 0.12, { grace: 0.14, decayK: 1.25 });

// twinkle — the climbing wave, full and bright, with a kalimba on the peak.
place(MOTIFS.twinkle, 4, 0, 0.46, -0.04, { grace: 0.16, decayK: 1.2 });
sparkle("C6", 4, 2, 0.26, 3, 0.4);
pad(["F4", "A4", "C5"], 4, 9, 0.16);

// wow wobble — held vibraphone under a rosewood A-G-A and a staccato tumble.
events.push({ preset: "vibraphone", startSec: 7 * BAR, midi: m("G5"), durSec: 6 * BEAT, gain: 0.34, decayMul: (DECAY.vibraphone ?? 1.4) * 1.2, pan: 0.16 });
events.push({ preset: "vibraphone", startSec: 7 * BAR, midi: m("Bb5"), durSec: 6 * BEAT, gain: 0.3, decayMul: (DECAY.vibraphone ?? 1.4) * 1.2, pan: 0.16 });
place(MOTIFS.wow, 8, 0, 0.42, -0.02, { grace: 0.12, decayK: 1.15 });
// a quick staccato ratatata diminution of the wow tail — the busiest moment.
place(dim([["A5", 1], ["G5", 1], ["A5", 1], ["G5", 1], ["F5", 1], ["G5", 1], ["A5", 1], ["C6", 1]], 4),
  9, 0, 0.26, -0.24, { preset: "staccato", decayK: 1.0 });
sparkle("F6", 8, 1, 0.22, 2.5);

// ba-ba-ba-bap — slinky-dog wobble, the last fully-elaborate phrase.
place(MOTIFS.baba, 10, 0, 0.44, 0.06, { grace: 0.12, decayK: 1.25 });
sparkle("A5", 11, 2, 0.2, 2.5, 0.4);

// ════════════════════════════════════════════════════════════════════════════
// PASS 1 — FIRST LIQUIDATION (bars 12–18): ornaments gone, top trimmed.
// The tune restates but plainer: no grace notes, slinky folds toward the
// sleep settle, gaps widen, rings lengthen. Drop the staccato + pad entirely.
// ════════════════════════════════════════════════════════════════════════════

// hush + a fragment of twinkle, augmented (stretched), no ornaments.
place(aug(MOTIFS.hush, 1.3), 12, 0, 0.4, -0.05, { decayK: 1.5, durMul: 1.1 });
// only the head of twinkle survives, folded, slower — the climb is half gone.
place(liquidate(MOTIFS.twinkle, 3, 1.4), 14, 0, 0.36, 0.05, { fold: true, decayK: 1.55 });
// slinky liquidated to its first gesture, dropping into the sleep contour.
place(liquidate(MOTIFS.baba, 4, 1.3), 16, 0, 0.34, 0.04, { fold: true, decayK: 1.6 });
// one far sparkle — the last ornament — and a thinning bass already below.
sparkle("C6", 15, 1.5, 0.18, 4, 0.36);

// ════════════════════════════════════════════════════════════════════════════
// PASS 2 — SECOND LIQUIDATION (bars 19–24): only the sleep settle, pentatonic.
// The contour flattens to a pure stepwise sink to F, stated once, then its
// echo an octave lower. Long rings, almost no top. This is the old tuckbaba.
// ════════════════════════════════════════════════════════════════════════════

place(aug(MOTIFS.sleep, 1.25), 19, 0, 0.34, -0.06, { fold: true, decayK: 1.7 });
place(aug(MOTIFS.sleep, 1.4), 22, 0, 0.26, 0.07, { fold: true, decayK: 1.8, octave: -12 });

// ════════════════════════════════════════════════════════════════════════════
// PASS 3 — THIRD LIQUIDATION (bars 26–28): the falling third, alone.
// The sleep descent erodes to its bare cadence: a high note, a third below,
// and home on F. Almost nothing left of the contour.
// ════════════════════════════════════════════════════════════════════════════

place([["A4", 3], ["F4", 4]], 26, 0, 0.28, 0, { fold: true, decayK: 1.9 });

// ════════════════════════════════════════════════════════════════════════════
// PASS 4 — FINAL LIQUIDATION (bars 30+): a single low F, then silence.
// ════════════════════════════════════════════════════════════════════════════

events.push({
  preset: "rosewood", startSec: 30 * BAR, midi: snap(m("F3")),
  durSec: 5 * BEAT, gain: 0.24, decayMul: (DECAY.rosewood ?? 1.8) * 2.0, pan: 0,
});

// the lone "goodnight" sparkle — high, far, the last waking thought.
sparkle("C6", 31, 1.5, 0.18, 7, 0.32);

// ── the slow heartbeat: dense at the top, spacing out + softening as it sleeps
const breaths = [
  [0, 0.36], [1, 0.34], [2, 0.34], [3, 0.32],   // PASS 0 — full, every bar
  [4, 0.32], [6, 0.3], [8, 0.3], [10, 0.28],    // still steady
  [12, 0.28], [14, 0.26], [16, 0.24],           // PASS 1 — thinning
  [19, 0.24], [22, 0.2],                         // PASS 2 — sparse
  [26, 0.18],                                    // PASS 3
  [30, 0.16],                                    // PASS 4 — last breath
];
for (const [bar, gain] of breaths) {
  events.push({
    preset: "bass", startSec: bar * BAR,
    midi: m(bar >= 19 ? "F2" : (bar % 2 ? "F3" : "F2")),
    durSec: 3 * BEAT, gain,
    decayMul: (DECAY.bass ?? 1.8) * (bar >= 16 ? 1.7 : 1.3), pan: 0,
  });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "tuckbaba",
  here: HERE,
  title: "tuckbaba",
  reverb: { wet: 0.38, decay: 0.86, damp: 0.36 },
  fadeIn: 1.2,
  fadeOut: 6.0,
  tailSec: 6.5,
  peak: 0.8,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
