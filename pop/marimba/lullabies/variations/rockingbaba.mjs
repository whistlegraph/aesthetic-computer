// rockingbaba.mjs — GRADUAL ELABORATION OVER OSTINATO.
//
// The rocking-chair bass never changes: a slow, hypnotic F2->C2->F2 sway in
// every 3/4 bar (the chair tipping forward and rolling back) holds rock-steady
// from first breath to last. Over that unbroken ground, the MELODY is the whole
// drama — it is born as just TWO notes (the marimbaba "hush" sigh: A4->F4) and
// then grows, bar by bar, into an ever-more-ornate line:
//
//   • two notes        — the bare sigh, A4->F4, all the room in the world.
//   • + passing tones  — the gap between them filled in (A G F).
//   • + neighbor figs  — each note circled by its upper/lower neighbor.
//   • + turns          — four-note turns (above-note-below-note) bloom on beats.
//   • + sequences      — the turn figure sequenced up the pentatonic, climbing.
//   • + cascading runs — full descending pentatonic cascades, grace flurries,
//                        the marimbaba "twinkle" and "baba" cells diminished
//                        into fast tumbles. THIS is the most-awake bloom.
//   • then LIQUIDATION — the runs thin, the turns drop their tails, the
//     neighbors fall away, until the melody is two notes again (A4->F4), then
//     one note, then silence — sleep wins, but the chair keeps rocking a few
//     beats more before it, too, comes to rest.
//
// Identity kept: F MAJOR PENTATONIC, rosewood lead, the rocking ~50 BPM 3/4
// sway, the gentle blooming rings, and the marimbaba thread — every bloom is
// built from the hush / twinkle / baba / sleep MOTIF cells, just developed hard.
//
// Run:  node variations/rockingbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 50;            // slow rocking
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;      // 3/4
const DECAY = 2.1;         // long rosewood rings, blooming into each other

// ── F MAJOR PENTATONIC as an ordered ladder of MIDI pitches across octaves.
// We address melody by SCALE INDEX (…, F4=0, G4=1, A4=2, C5=3, D5=4, F5=5, …)
// so transpose / sequence / neighbor / turn are just integer math on the
// ladder. Root F, degrees {F,G,A,C,D}. ──────────────────────────────────────
const PENTA_PCS = [5, 7, 9, 0, 2]; // F G A C D (pitch classes)
const LADDER = [];
for (let oct = 1; oct <= 7; oct++) {
  for (const pc of PENTA_PCS) {
    LADDER.push((oct + 1) * 12 + pc); // MIDI; pc already 0..11
  }
}
LADDER.sort((a, b) => a - b);
// index of a named pitch, snapped to nearest ladder rung.
function idx(note) {
  const target = typeof note === "number" ? note : m(note);
  let best = 0, bestD = 99;
  for (let i = 0; i < LADDER.length; i++) {
    const d = Math.abs(LADDER[i] - target);
    if (d < bestD) { bestD = d; best = i; }
  }
  return best;
}
const pitch = (i) => LADDER[Math.max(0, Math.min(LADDER.length - 1, Math.round(i)))];

// anchor degrees we keep returning to (the marimbaba sigh A4->F4).
const A4 = idx("A4");
const F4 = idx("F4");
const F5 = idx("F5");
const A5 = idx("A5");
const C6 = idx("C6");

// ── melody helpers — a "cell" is [scaleIndex, beats] tuples. ────────────────
const transpose = (cell, steps) => cell.map(([i, b]) => [i + steps, b]);
const retrograde = (cell) => [...cell].reverse();
function invert(cell, pivot = cell[0][0]) {
  return cell.map(([i, b]) => [2 * pivot - i, b]);
}
// sequence: repeat a cell N times, shifting by `step` ladder-rungs each copy.
function sequence(cell, n, step) {
  const out = [];
  for (let k = 0; k < n; k++) out.push(...transpose(cell, k * step));
  return out;
}
// neighbor figure: note, upper-neighbor, note (decorate a single index).
const neighbor = (i, b, up = 1) => [[i, b / 2], [i + up, b / 4], [i, b / 4]];
// turn: above, note, below, note — the classic four-note ornament.
const turn = (i, b) => [[i + 1, b / 4], [i, b / 4], [i - 1, b / 4], [i, b / 4]];
// descending pentatonic run of `len` rungs starting at top, total `beats`.
function runDown(top, len, beats) {
  const out = [], step = beats / len;
  for (let k = 0; k < len; k++) out.push([top - k, step]);
  return out;
}
function runUp(bottom, len, beats) {
  const out = [], step = beats / len;
  for (let k = 0; k < len; k++) out.push([bottom + k, step]);
  return out;
}

// ── place a melody cell of [scaleIndex, beats] into events at a bar/beat. ────
const events = [];
function placeMelody(cell, barStart, gain, opts = {}) {
  const { gainSlope = 0, decayMul = DECAY, pan = 0, durMul = 1.25 } = opts;
  let beatPos = 0;
  const n = cell.length;
  cell.forEach(([i, b], k) => {
    const t = barStart * BAR + beatPos * BEAT;
    const g = gain + gainSlope * (k / Math.max(1, n - 1));
    events.push({
      preset: "rosewood",
      startSec: t,
      midi: pitch(i),
      durSec: b * BEAT * durMul,
      gain: Math.max(0.12, g),
      decayMul,
      pan: pan + (i - F5) * 0.012, // gently fan high notes right
    });
    beatPos += b;
  });
}

// grace-note flurry just BEFORE a beat: tiny soft pickups leading into `toIdx`.
function grace(barStart, beatInBar, fromIdx, toIdx, gain) {
  const steps = toIdx - fromIdx;
  const count = Math.min(4, Math.abs(steps));
  if (count === 0) return;
  const dir = Math.sign(steps);
  const gBeat = 0.085; // each grace ~very fast
  for (let k = 0; k < count; k++) {
    const i = toIdx - dir * (count - k);
    events.push({
      preset: "rosewood",
      startSec: barStart * BAR + (beatInBar - (count - k) * gBeat) * BEAT,
      midi: pitch(i),
      durSec: 0.4 * BEAT,
      gain: gain * 0.55,
      decayMul: DECAY * 0.8,
      pan: 0.05,
    });
  }
}

// ══════════════════════════════════════════════════════════════════════════
// THE UNCHANGING ROCKING-CHAIR BASS OSTINATO — F2 down-rock, C2 up-rock, every
// bar, rock-steady, hypnotic. It runs the whole length unchanged in feel.
// ══════════════════════════════════════════════════════════════════════════
const TOTAL_BARS = 30;
for (let bar = 0; bar < TOTAL_BARS; bar++) {
  events.push({
    preset: "bass",
    startSec: bar * BAR,
    midi: m("F2"),
    durSec: 2.3 * BEAT,
    gain: 0.5,
    decayMul: 1.9,
    pan: -0.07,
  });
  events.push({
    preset: "bass",
    startSec: bar * BAR + 1.5 * BEAT,
    midi: m("C2"),
    durSec: 1.5 * BEAT,
    gain: 0.4,
    decayMul: 1.85,
    pan: 0.07,
  });
  // a soft mid "rock" pad on the F to thicken the ground without changing it.
  if (bar % 2 === 0) {
    events.push({
      preset: "bass",
      startSec: bar * BAR,
      midi: m("F3"),
      durSec: 2.6 * BEAT,
      gain: 0.16,
      decayMul: 2.0,
      pan: 0.0,
    });
  }
}

// ══════════════════════════════════════════════════════════════════════════
// THE MELODY ARC — born two notes, bloom to ornate, liquidate to two notes.
// Each stage hangs on the SAME marimbaba sigh skeleton (A4 -> F4), elaborated.
// ══════════════════════════════════════════════════════════════════════════

// ─ Stage 0 (bars 0-3): TWO NOTES. The bare hush sigh. Vast space. ─
placeMelody([[A4, 1], [F4, 2]], 0, 0.42, { gainSlope: -0.04 });
placeMelody([[A4, 1.5], [F4, 1.5]], 2, 0.4, { gainSlope: -0.03 });

// ─ Stage 1 (bars 4-7): + PASSING TONES. Fill the A->F gap (A G F), and a
//   rising answer F->G->A so the line starts to walk. ─
placeMelody([[A4, 1], [A4 - 1, 1], [F4, 1]], 4, 0.44);           // A G F
placeMelody([[F4, 1], [F4 + 1, 1], [A4, 1]], 5, 0.44);           // F G A (answer)
placeMelody([[A4, 0.75], [A4 - 1, 0.75], [F4, 1.5]], 6, 0.44);   // quicker A G F
placeMelody([[F4, 1.5], [A4, 1.5]], 7, 0.42);                     // back to the sigh, wider

// ─ Stage 2 (bars 8-11): + NEIGHBOR FIGURES. Each pillar note circled by its
//   upper neighbor; the sigh decorated at both ends. ─
placeMelody([...neighbor(A4, 1.5, 1), ...neighbor(F4, 1.5, 1)], 8, 0.46);
placeMelody([...neighbor(A4, 1, 1), [A4 - 1, 1], ...neighbor(F4, 1, 1)], 9, 0.46);
// lower-neighbor variant + a small reach up to C5
placeMelody([[A4, 0.75], [A4 - 1, 0.5], [A4, 0.75], [idx("C5"), 1]], 10, 0.48);
placeMelody([...neighbor(A4, 1, -1), [F4, 1.5]], 11, 0.46, { gainSlope: -0.02 });

// ─ Stage 3 (bars 12-15): + TURNS, sequenced. The four-note turn blooms on the
//   sigh tones, then the turn figure climbs the pentatonic (sequence up). ─
placeMelody([...turn(A4, 1.5), ...turn(F4, 1.5)], 12, 0.5);
placeMelody(sequence(turn(F4, 1), 3, 1), 13, 0.5, { gainSlope: 0.04 }); // turns climbing
placeMelody([...turn(idx("C5"), 1), ...turn(A4, 1), ...turn(F4, 1)], 14, 0.5); // turns descending
// a turn that resolves up into the high octave — first reach for the bloom.
placeMelody([...turn(A4, 1), ...runUp(A4, 4, 1), [F5, 1]], 15, 0.52, { gainSlope: 0.05 });

// ─ Stage 4 (bars 16-20): CASCADING RUNS — the most ornate, most-awake bloom.
//   marimbaba "twinkle" + "baba" cells diminished into fast tumbles, grace
//   flurries, sequenced runs spanning two octaves. ─
// twinkle, diminished and pushed up an octave (climbing wave).
const twinkleCell = MOTIFS.twinkle.map(([nn, b]) => [idx(nn), b * 0.5]);
placeMelody(twinkleCell, 16, 0.56, { gainSlope: 0.05, pan: 0.06 });
grace(16, 0, F5, A5, 0.5);
// baba slinky cell, diminished — fast wobble tumble.
const babaCell = MOTIFS.baba.map(([nn, b]) => [idx(nn), b * 0.6]);
placeMelody(babaCell, 17, 0.56, { gainSlope: 0.04, pan: -0.05 });
// a big cascading descending run from the top of the bloom, two octaves.
placeMelody(runDown(C6, 9, 3), 18, 0.58, { gainSlope: -0.03, durMul: 0.9, pan: 0.08 });
grace(18, 0, A5, C6, 0.55);
// sequenced rising runs answering the cascade (climb back up, hemiola-ish).
placeMelody([...runUp(F4 + 7, 4, 1.5), ...runUp(F4 + 9, 4, 1.5)], 19, 0.56, { gainSlope: 0.04, pan: 0.05 });
// the peak: a turn + grace-flurry + cascade, the line at its most ornate.
placeMelody([...turn(C6, 0.75), ...runDown(C6, 6, 2.25)], 20, 0.58, { gainSlope: -0.04, durMul: 0.85, pan: 0.07 });
grace(20, 0, A5, C6, 0.56);

// ─ Stage 5 (bars 21-25): LIQUIDATION. Runs thin to turns, turns drop their
//   tails to neighbors, neighbors fall to passing tones — gravity pulling the
//   line back down toward the sigh, each bar simpler & quieter than the last. ─
placeMelody([...runDown(A5, 5, 1.5), ...turn(F5, 1.5)], 21, 0.5, { gainSlope: -0.05 }); // run -> turn
placeMelody([...turn(F5, 1), ...neighbor(idx("C5"), 1, 1), [A4, 1]], 22, 0.46, { gainSlope: -0.05 }); // turn -> neighbor
placeMelody([...neighbor(A4, 1.5, 1), [A4 - 1, 0.75], [F4, 0.75]], 23, 0.42, { gainSlope: -0.05 }); // neighbor -> passing
placeMelody([[A4, 1], [A4 - 1, 1], [F4, 1]], 24, 0.38, { gainSlope: -0.04 }); // passing tones only (A G F)
placeMelody([[A4, 1.5], [F4, 1.5]], 25, 0.34, { gainSlope: -0.03 }); // TWO NOTES again — the sigh returns

// ─ Stage 6 (bars 26-29): sleep wins. Two notes -> one note -> silence, while
//   the rocking chair keeps swaying then slows to rest. The marimbaba "sleep"
//   cell, reduced to its last falling pair, sunk low and barely-there. ─
placeMelody([[A4, 2], [F4, 4]], 26, 0.3, { gainSlope: -0.06, durMul: 1.4 }); // last sigh, stretched
placeMelody([[F4, 6]], 28, 0.24, { durMul: 1.6 }); // ONE NOTE — the held breath
// a single deep root far below, the eyelid closing.
events.push({ preset: "rosewood", startSec: 28 * BAR + 1.5 * BEAT, midi: pitch(F4) - 12, durSec: 7 * BEAT, gain: 0.18, decayMul: DECAY, pan: 0 });

// the rocking chair coming to rest a few beats after the melody dies: the last
// down-rock holds long and fades — but it does NOT change pattern, just slows.
events.push({ preset: "bass", startSec: TOTAL_BARS * BAR, midi: m("F1"), durSec: 9 * BEAT, gain: 0.36, decayMul: 2.2, pan: 0 });
events.push({ preset: "bass", startSec: TOTAL_BARS * BAR + 2 * BEAT, midi: m("C2"), durSec: 4 * BEAT, gain: 0.24, decayMul: 2.0, pan: 0.06 });

const { mp3, durationSec } = renderLullaby(events, {
  name: "rockingbaba",
  here: HERE,
  title: "rockingbaba (F major pentatonic, gradual elaboration over ostinato)",
  reverb: { wet: 0.32, decay: 0.88, damp: 0.5 },
  fadeIn: 1.8,
  fadeOut: 5.0,
  tailSec: 3.5,
  peak: 0.84,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
