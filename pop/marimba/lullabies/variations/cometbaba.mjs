// cometbaba.mjs — a streaking comet across the night, in D major.
//
// The marimbaba "twinkle" DNA (MOTIFS.twinkle — the climbing-falling wave, the
// hidden twinkle-little-star lyric) is taken and FLUNG UPWARD. The development
// strategy is RAPID ASCENDING SCALAR RUNS: each pass the comet streaks higher,
// the runs accelerate (longer, faster, climbing register), the twinkle head
// re-stated at the top of each arc — until a final blazing run launches into a
// long sparkling tail-off where the dust drifts back down and dims to one star.
//
//   Intro  — a far point of light, a slow rising glint (the comet appears).
//   Pass 1 — twinkle head stated low, then a gentle ascending run lifts it.
//   Pass 2 — the head an octave up; a longer, quicker run climbs past it.
//   Pass 3 — the head higher still; an accelerating run streaks the whole sky.
//   Apex   — the comet at perihelion: a blazing full-register run, twinkle blazed
//            at the top, then it tips over.
//   Tail   — a LONG sparkling tail-off: descending shimmer cascades, the dust
//            settling, kelon + glockenspiel drifting down to one last star.
//
// D major throughout. The twinkle contour (do-do-sol-sol-la-la-sol) and the
// climbing-wave shape stay the recognizable thread under all the streaking.
//
// Run:  node variations/cometbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

// ── D major mode: snap any midi to the nearest scale pitch ──────────────────
const ROOT = 2; // D
const MAJOR = [0, 2, 4, 5, 7, 9, 11];
const SCALE = MAJOR.map((d) => (d + ROOT) % 12); // D-relative pitch classes
function snap(midi) {
  const pc = ((midi % 12) + 12) % 12;
  let best = 0, bestD = 99;
  for (const d of SCALE) {
    const dist = Math.min(Math.abs(d - pc), 12 - Math.abs(d - pc));
    if (dist < bestD) { bestD = dist; best = d; }
  }
  return midi + (best - pc);
}
const snapName = (name, oct = 0) => snap(m(name) + oct * 12);

// D-major scale-degree → absolute midi (deg 0 = D in baseOct, wraps octaves)
function degToMidi(deg, baseOct = 5) {
  const o = Math.floor(deg / 7);
  const within = ((deg % 7) + 7) % 7;
  return 12 * (baseOct + 1) + ROOT + MAJOR[within] + 12 * o;
}

// ── timing — ~58 BPM (a touch slower than 64 so the comet breathes) ─────────
const BPM = 58;
const BEAT = 60 / BPM;
let BAR = 4 * BEAT;

const ev = [];
let t = 0; // running cursor in seconds

const GLOCK_DEC = 1.95;                  // high stars hang in the air
const KELON_DEC = (DECAY.kelon ?? 1.3) * 1.25; // warm wooden bed, stretched

// ── primitive emitters ──────────────────────────────────────────────────────

// glockenspiel star — the comet's light, way up high (default 2 octaves up)
function star(beatIn, name, beats, gain = 0.3, octShift = 2, pan = 0.2) {
  ev.push({
    preset: "glockenspiel",
    startSec: t + beatIn * BEAT,
    midi: snapName(name, octShift),
    durSec: beats * BEAT * 1.1,
    gain,
    decayMul: GLOCK_DEC,
    pan,
  });
}
// star by absolute midi (for runs computed in degrees)
function starMidi(beatIn, midi, beats, gain = 0.3, pan = 0.2, dec = GLOCK_DEC) {
  ev.push({ preset: "glockenspiel", startSec: t + beatIn * BEAT, midi: snap(midi), durSec: beats * BEAT * 1.1, gain, decayMul: dec, pan });
}
// faint shimmer — one fragile re-strike filling the gaps
function shimmer(beatIn, name, gain = 0.12, octShift = 2, pan = -0.24) {
  ev.push({ preset: "glockenspiel", startSec: t + beatIn * BEAT, midi: snapName(name, octShift), durSec: 0.5 * BEAT, gain, decayMul: GLOCK_DEC * 1.1, pan });
}
// warm kelon bed (the wooden glow the comet leaves)
function bed(beatIn, name, beats, gain = 0.32, octShift = 0, pan = -0.08) {
  ev.push({ preset: "kelon", startSec: t + beatIn * BEAT, midi: snapName(name, octShift), durSec: beats * BEAT * 1.18, gain, decayMul: KELON_DEC, pan });
}
function bedMidi(beatIn, midi, beats, gain = 0.32, pan = -0.08) {
  ev.push({ preset: "kelon", startSec: t + beatIn * BEAT, midi: snap(midi), durSec: beats * BEAT * 1.18, gain, decayMul: KELON_DEC, pan });
}
// low root, grounding the phrase (kelon, kept low + clear, no remap)
function root(name, gain = 0.3, beats = null) {
  ev.push({ preset: "kelon", startSec: t, midi: m(name), durSec: (beats ? beats * BEAT : BAR) * 1.12, gain, decayMul: KELON_DEC * 1.05, pan: 0 });
}

const nextPhrase = (mul = 1) => { t += BAR * mul; };

// ── the twinkle head as D-major scale-degree cells (the thread) ──────────────
// twinkle "do do sol sol la la sol" mapped to D major degrees:
//   D D A A B B A   (deg 0 0 4 4 5 5 4)
const HEAD = [[0, .5], [0, .5], [4, .5], [4, .5], [5, .5], [5, .5], [4, 1]];

// emit the twinkle head at a given starting octave, returning where it ends
function head(beatIn, baseOct, gStar = 0.3, gBed = 0.28, octShift = 2, pan = 0.18) {
  let b = beatIn;
  for (const [deg, beats] of HEAD) {
    const midi = degToMidi(deg, baseOct);
    starMidi(b, midi + octShift * 12, beats, gStar, pan);
    bedMidi(b, midi - 12, beats, gBed);
    b += beats;
  }
  return b;
}

// ── the core development device: a RAPID ASCENDING SCALAR RUN ────────────────
// climbs `steps` scale-degrees starting at `startDeg`, over `dur` beats, the
// notes accelerating (denser toward the top) and swelling — a comet streak.
function ascRun(beatIn, startDeg, steps, dur, baseOct, octShift, {
  gain0 = 0.12, gain1 = 0.26, accel = 1.0, pan = 0.0, dec = GLOCK_DEC,
} = {}) {
  // sub-beat positions: an accelerating ramp (gaps shrink toward the top)
  const pos = [];
  let acc = 0;
  const weights = [];
  for (let i = 0; i < steps; i++) weights.push(Math.pow(accel, -i)); // later = shorter
  const total = weights.reduce((a, w) => a + w, 0);
  for (let i = 0; i < steps; i++) { pos.push(acc); acc += weights[i] / total * dur; }
  for (let i = 0; i < steps; i++) {
    const f = i / Math.max(1, steps - 1);
    const deg = startDeg + i;
    const midi = degToMidi(deg, baseOct) + octShift * 12;
    const g = gain0 + (gain1 - gain0) * f; // swell up the run
    const p = (i % 2 ? 1 : -1) * pan; // ping-pong the streak across the sky
    starMidi(beatIn + pos[i], midi, 0.4, g, p, dec);
  }
}

// =====================================================================
// INTRO — a far point of light: one slow rising glint, the comet appears
// =====================================================================
root("D2", 0.24);
star(0.5, "D5", 1.4, 0.18, 1, -0.2);
shimmer(1.8, "F#5", 0.1, 1, 0.22);
star(2.6, "A5", 1.6, 0.2, 1, 0.18);
shimmer(3.6, "D6", 0.1, 1, -0.2);
nextPhrase();

// =====================================================================
// PASS 1 — twinkle head stated low; a gentle ascending run lifts it away.
// =====================================================================
root("D2", 0.3);
head(0, 4, 0.28, 0.28, 0, 0.0);       // head in the mid register, plain
// a soft rising run answers in the second half — the first faint streak
ascRun(2.0, 0, 8, 2.0, 4, 0, { gain0: 0.1, gain1: 0.22, accel: 1.08, pan: 0.26 });
nextPhrase();

// brief landing — a held A (dominant), the comet catching breath
root("A2", 0.26);
bed(0, "A4", 4, 0.26, 0);
bed(0, "D5", 4, 0.2, 0);
star(0.6, "A5", 1.4, 0.2, 1, 0.2);
shimmer(2.4, "C#6", 0.1, 1, -0.22);
// a small early streak crosses the rest, foreshadowing the climb
ascRun(2.6, 2, 6, 1.4, 4, 0, { gain0: 0.08, gain1: 0.18, accel: 1.06, pan: 0.24 });
nextPhrase();

// =====================================================================
// PASS 2 — the head an octave UP; a longer, quicker run climbs past it.
// =====================================================================
root("D2", 0.3);
head(0, 5, 0.3, 0.24, 0, 0.16);       // head one octave higher (brighter)
// the run starts mid-phrase, longer and a touch faster, climbing two octaves
ascRun(1.5, 0, 12, 2.5, 4, 0, { gain0: 0.1, gain1: 0.26, accel: 1.1, pan: 0.3 });
shimmer(3.9, "F#6", 0.1, 1, -0.2);
nextPhrase();

// =====================================================================
// PASS 3 — the head higher still; an accelerating run streaks the whole sky.
// =====================================================================
root("G2", 0.3);                       // lift to the subdominant — buoyant
head(0, 5, 0.3, 0.22, 0, 0.2);
// the streak now begins almost with the head and rips upward, fast + bright
ascRun(0.5, -3, 16, 3.2, 4, 0, { gain0: 0.1, gain1: 0.3, accel: 1.14, pan: 0.34 });
nextPhrase();

// a short suspension before perihelion — a held shimmering dominant chord
root("A2", 0.26);
bed(0, "A4", 4, 0.24, 0);
bed(0, "C#5", 4, 0.18, 0);
bed(0, "E5", 4, 0.18, 0);
shimmer(0.8, "A5", 0.12, 1, 0.2);
shimmer(2.0, "E6", 0.1, 1, -0.22);
shimmer(3.2, "C#6", 0.09, 1, 0.18);
nextPhrase();

// =====================================================================
// PASS 4 — the head re-voiced high in kelon below while a streak rides over;
// the comet visibly accelerating toward its closest approach.
// =====================================================================
root("D2", 0.3);
// the head's contour walked in warm kelon, mid register (the thread held)
{
  let b = 0;
  for (const [deg, beats] of HEAD) {
    bedMidi(b, degToMidi(deg, 5) - 12, beats * 1.1, 0.24);
    b += beats;
  }
}
// a bright fast streak rides across the whole bar over the head
ascRun(0.25, -2, 18, 3.5, 4, 0, { gain0: 0.1, gain1: 0.3, accel: 1.13, pan: 0.34 });
nextPhrase();

// final breath before perihelion — a hushed held tonic, the sky waiting
root("D2", 0.26);
bed(0, "D4", 4, 0.24, 0);
bed(0, "F#4", 4, 0.18, 0);
bed(0, "A4", 4, 0.18, 0);
shimmer(0.7, "D6", 0.11, 1, 0.2);
shimmer(2.1, "A5", 0.1, 1, -0.22);
shimmer(3.3, "F#6", 0.08, 1, 0.18);
nextPhrase();

// =====================================================================
// APEX — perihelion: a blazing FULL-REGISTER run, the twinkle head blazed
// at the very top, then the comet tips over and starts to fall.
// =====================================================================
BAR *= 1.05; // the apex breathes a hair wider
root("D2", 0.32);
bed(0, "D4", BAR / BEAT, 0.24, 0);
bed(0, "A4", BAR / BEAT, 0.2, 0);
// the longest, fastest ascending streak — from low, ripping clear to the top
ascRun(0.0, -7, 22, 3.4, 4, 0, { gain0: 0.1, gain1: 0.34, accel: 1.12, pan: 0.36 });
// the twinkle head BLAZED at the apex, way up high, the comet's crown
head(2.4, 6, 0.3, 0.0, 0, 0.22);
nextPhrase();
BAR /= 1.05;

// =====================================================================
// TAIL — a LONG sparkling tail-off. the comet falls away: descending
// shimmer cascades over a slowly settling kelon glow, dimming to one star.
// =====================================================================

// descending cascade helper — mirror of ascRun, decelerating + dimming
function descCascade(beatIn, startDeg, steps, dur, baseOct, octShift, {
  gain0 = 0.22, gain1 = 0.08, decel = 1.08, pan = 0.0,
} = {}) {
  const pos = [];
  let acc = 0;
  const weights = [];
  for (let i = 0; i < steps; i++) weights.push(Math.pow(decel, i)); // later = longer (slowing)
  const total = weights.reduce((a, w) => a + w, 0);
  for (let i = 0; i < steps; i++) { pos.push(acc); acc += weights[i] / total * dur; }
  for (let i = 0; i < steps; i++) {
    const f = i / Math.max(1, steps - 1);
    const deg = startDeg - i;
    const midi = degToMidi(deg, baseOct) + octShift * 12;
    const g = gain0 + (gain1 - gain0) * f; // dim as the dust falls
    const p = (i % 2 ? 1 : -1) * pan;
    starMidi(beatIn + pos[i], midi, 0.5, g, p, GLOCK_DEC * 1.1);
  }
}

// tail bar 1 — the big sparkling fall, the head's contour drifting down
BAR *= 1.1; // begin a gentle ritard for the descent
root("D2", 0.28);
bed(0, "F#4", BAR / BEAT, 0.22, 0);
bed(0, "A4", BAR / BEAT, 0.18, 0);
descCascade(0.0, 14, 16, 3.6, 4, 0, { gain0: 0.24, gain1: 0.1, decel: 1.1, pan: 0.3 });
nextPhrase();

// tail bar 2 — a smaller, softer secondary fall; kelon answers below
BAR *= 1.12;
root("G2", 0.26);
bed(0, "B3", BAR / BEAT, 0.24, 0);
bed(0, "D4", BAR / BEAT, 0.18, 0);
descCascade(0.2, 9, 10, 3.2, 4, 0, { gain0: 0.18, gain1: 0.08, decel: 1.12, pan: 0.26 });
// a low kelon echo of the twinkle head, slow + warm — the thread, settling
{
  let b = 0;
  for (const [deg, beats] of [[4, 1], [4, 1], [5, 1], [4, 1]]) {
    bedMidi(b, degToMidi(deg, 4) - 12, beats * 1.2, 0.22);
    b += beats;
  }
}
nextPhrase();

// tail bar 2.5 — a last gentle drift of dust, very soft, high and slow
BAR *= 1.14;
root("A2", 0.24);
bed(0, "A3", BAR / BEAT, 0.22, 0);
bed(0, "C#4", BAR / BEAT, 0.16, 0);
descCascade(0.3, 6, 7, 3.0, 4, 0, { gain0: 0.13, gain1: 0.06, decel: 1.14, pan: 0.22 });
nextPhrase();

// tail bar 3 — the dust nearly settled: a resting D-major chord, faint glints
BAR *= 1.18;
root("D2", 0.28);
bed(0, "F#4", BAR / BEAT, 0.24, 0);
bed(0, "A4", BAR / BEAT, 0.2, 0);
bed(0, "D5", BAR / BEAT, 0.2, 0);
star(0.5, "D5", 2, 0.16, 2, 0.18);
shimmer(2.2, "A5", 0.1, 2, -0.22);
shimmer(3.6, "F#5", 0.08, 2, 0.18);
nextPhrase();

// the very last star — one distant point of light, soft and high, fading out
ev.push({
  preset: "glockenspiel",
  startSec: t + 0.4 * BEAT,
  midi: snapName("D5", 2),
  durSec: 1.2 * BEAT,
  gain: 0.1,
  decayMul: GLOCK_DEC * 1.35,
  pan: 0.16,
});

// ── render ───────────────────────────────────────────────────────────────────
const { mp3, durationSec } = renderLullaby(ev, {
  name: "cometbaba",
  here: HERE,
  title: "cometbaba",
  reverb: { wet: 0.38, decay: 0.87, damp: 0.28 }, // wide, glassy, deep-space air
  fadeIn: 0.7,
  fadeOut: 5.2,
  tailSec: 6.0,
  peak: 0.82,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
