// lydianbaba.mjs — a floating, wondrous F-lydian riff on the marimbaba seed,
// developed hard into an ASCENDING MELODIC SEQUENCE.
//
// Identity kept: F lydian (raised 4th, Bb → B natural = the weightless #11
// shimmer), a kalimba lead over a slow vibraphone_off pad, ~58 BPM 3/4, and
// the flyHigh / butterfly ("way up high") motif as the recognizable thread.
//
// Development: the flyHigh motif does not merely repeat — it climbs in real
// sequences. Each restatement is transposed one lydian step higher, then by
// thirds, terracing up the scale into shimmering rising plateaus that keep
// lifting and never quite cadence — wondrous vertigo — until a final gentle
// float back down to the F-lydian tonic and sleep.
//
// Run:  node variations/lydianbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { buildMarimbaba, MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 58;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;

// ── F lydian: root pitch-class F (5), scale {0,2,4,6,7,9,11} ────────────────
const ROOT_PC = m("F4") % 12;            // 5
const LYDIAN = [0, 2, 4, 6, 7, 9, 11];

// Fold a midi note to the nearest pitch in the target scale (keeps octave
// region; ties resolve upward so the raised 4th brightens rather than dulls).
function snap(midi, rootPc = ROOT_PC, scale = LYDIAN) {
  let best = midi, bestD = Infinity;
  for (let oct = -1; oct <= 1; oct++) {
    for (const deg of scale) {
      const pc = (rootPc + deg) % 12;
      const base = Math.round((midi - pc) / 12) * 12 + pc + oct * 12;
      const d = Math.abs(base - midi);
      if (d < bestD - 1e-6 || (Math.abs(d - bestD) < 1e-6 && base > best)) {
        best = base; bestD = d;
      }
    }
  }
  return best;
}

// ── diatonic step helper: move a midi note up/down N degrees within lydian ──
// Snap the note to scale, find its scale index, shift by `steps`, rebuild.
function degreeShift(midi, steps, rootPc = ROOT_PC, scale = LYDIAN) {
  const snapped = snap(midi, rootPc, scale);
  const pc = ((snapped % 12) + 12) % 12;
  const rel = ((pc - rootPc) % 12 + 12) % 12;
  let idx = scale.indexOf(rel);
  if (idx < 0) {
    // not exactly on a degree — fall back to nearest
    let bd = Infinity;
    scale.forEach((deg, i) => { const dd = Math.abs(deg - rel); if (dd < bd) { bd = dd; idx = i; } });
  }
  const n = scale.length;
  const baseOct = Math.floor((snapped - (rootPc + scale[idx])) / 12);
  const newIdx = ((idx + steps) % n + n) % n;
  const octCarry = Math.floor((idx + steps) / n);
  return (baseOct + octCarry) * 12 + rootPc + scale[newIdx];
}

const MELODIC = new Set(["kalimba", "vibraphone", "vibraphone_off", "staccato", "glockenspiel"]);

// ── 1) the body: marimbaba, re-tempo'd, kalimba lead, dropped to lydian ─────
// We keep the opening (hush / twinkle, bars 0-9) and the closing settle
// (bars 18-23) as the recognizable frame, but CUT the middle (wow/baba/sleep
// transition, bars 10-17) — the ascending sequence terraces fill that span
// instead. Filter the held vibe triads (we lay a fresh lydian pad by hand).
const KEEP_BARS = (bar) => bar <= 9 || bar >= 18;
const body = buildMarimbaba({
  bpm: BPM,
  transpose: 0,
  leadPreset: "kalimba",
  gainMul: 0.78,
  decayMul: 1.25,
  filter: (voice, bar) => voice !== "vibraphone_off" && KEEP_BARS(bar),
}).map((e) => {
  if (MELODIC.has(e.preset)) return { ...e, midi: snap(e.midi) };
  return e; // bass roots stay as-is (F / C / Eb sit fine under lydian)
});

// ── 2) a hand-laid F-lydian pad (vibraphone_off), wide and slow ─────────────
const pad = [];
function chord(bar, notes, beats, gain = 0.16, pan = -0.2) {
  for (const n of notes) {
    pad.push({
      preset: "vibraphone_off",
      startSec: bar * BAR,
      midi: typeof n === "number" ? n : m(n),
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY.vibraphone_off ?? 1.4) * 1.3,
      pan,
    });
  }
}
// Frame pads: Fmaj9(#11) over the opening twinkle, B-natural reach, then a
// settle pad under the final descent.
chord(4, ["F4", "A4", "C5", "E5"], 9);          // bars 4-6
chord(7, ["G4", "B4", "D5", "F5"], 9);          // bars 7-9  — B natural = #11 glow
chord(18, ["F4", "A4", "C5"], 12, 0.14);        // bars 18-20 settle
chord(21, ["F4", "C5", "G5"], 9, 0.12);         // bars 21-23 open fifth/ninth rest

// ── 3) THE ASCENDING TERRACES — flyHigh climbing through F lydian ────────────
// flyHigh = [Bb5,D6,Bb5,C6,A5]; snapped to lydian its Bb folds to B natural
// (the signature #11). We sequence it: each terrace transposes the whole
// motif up by a chosen number of lydian DEGREES, so the butterfly keeps
// lifting. Steps: +0, +1, +2 (stepwise), then +4, +6 (by thirds) — the
// climb accelerates and widens, stacking shimmering plateaus.

const terraces = []; // melodic events
const climbPad = []; // a moving supporting chord under each terrace

// Place one statement of flyHigh starting at `startBar`, transposed `degShift`
// lydian degrees from its base, with a per-terrace timing stretch.
function terrace(startBar, degShift, opts = {}) {
  const {
    leadPreset = "kalimba",
    gain = 0.26,
    pan = 0.0,
    stretch = 1.0,       // beat-duration multiplier (augmentation)
    grace = false,       // add a glockenspiel grace flurry above
    graceGain = 0.14,
  } = opts;
  let beat = 0;
  let bar = startBar;
  let lastMidi = null;
  for (const [note, beats] of MOTIFS.flyHigh) {
    const midi = degreeShift(snap(m(note)), degShift);
    lastMidi = midi;
    terraces.push({
      preset: leadPreset,
      startSec: bar * BAR + beat * BEAT,
      midi,
      durSec: beats * 1.15 * stretch * BEAT,
      gain,
      decayMul: (DECAY[leadPreset] ?? 1.5) * 1.4,
      pan,
    });
    // sparkling grace note a third above, on the longer tones
    if (grace && beats >= 1) {
      terraces.push({
        preset: "glockenspiel",
        startSec: bar * BAR + (beat + 0.5) * BEAT,
        midi: degreeShift(midi, 2) + 12,
        durSec: 0.6 * BEAT,
        gain: graceGain,
        decayMul: (DECAY.glockenspiel ?? 1.5) * 1.3,
        pan: pan + 0.18,
      });
    }
    beat += beats * stretch;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
  return { endBar: bar, endBeat: beat, lastMidi };
}

// Supporting moving chord (root + #11 color) for a terrace, panned wide-left.
function climbChord(bar, rootName, beats, degShift) {
  const root = degreeShift(snap(m(rootName)), degShift);
  for (const off of [0, 2, 4]) { // stacked thirds in lydian
    climbPad.push({
      preset: "vibraphone_off",
      startSec: bar * BAR,
      midi: degreeShift(root, off),
      durSec: beats * BEAT,
      gain: 0.12,
      decayMul: (DECAY.vibraphone_off ?? 1.4) * 1.3,
      pan: -0.28,
    });
  }
}

// The terrace plan — bars 10 onward. flyHigh spans ~6 beats = 2 bars each.
// Stepwise climb first, then accelerate by thirds, density tightening.
//   T0  bar10  +0  (statement, in register, kalimba)
//   T1  bar12  +1  (one step up)
//   T2  bar14  +2  (another step — three rising plateaus, stepwise)
//   T3  bar15.5 +4 (leap by a third, faster, glock grace sparkles)
//   T4  bar16.5 +6 (highest terrace, by another third, shimmering, never lands)
// Then a falling cascade floats it back down into the closing settle (bar18).

// T0 — the butterfly appears, plain.
terrace(10, 0, { gain: 0.26, pan: -0.05, stretch: 1.0 });
climbChord(10, "F4", 6, 0);

// T1 — one lydian step higher, brighter pan, slight lift in gain.
terrace(12, 1, { gain: 0.25, pan: 0.12, stretch: 1.0, grace: true, graceGain: 0.11 });
climbChord(12, "F4", 6, 1);

// T2 — two steps up, the third stepwise plateau; grace sparkles increase.
terrace(14, 2, { gain: 0.24, pan: 0.22, stretch: 0.85, grace: true, graceGain: 0.12 });
climbChord(14, "F4", 5, 2);

// T3 — now the climb leaps by a THIRD (+4 degrees), compressed/faster (vertigo).
terrace(15.5, 4, { gain: 0.22, pan: 0.3, stretch: 0.7, grace: true, graceGain: 0.13 });
climbChord(15, "F4", 4, 4);

// T4 — the highest terrace, another third up (+6), shimmering and unresolved.
terrace(16.5, 6, { leadPreset: "glockenspiel", gain: 0.18, pan: 0.36, stretch: 0.62, grace: true, graceGain: 0.12 });
climbChord(16, "F4", 4, 6);

// ── 4) the gentle float DOWN — a descending lydian cascade releasing the
// vertigo back to the tonic, handing off into the closing settle at bar 18. ──
const cascade = [];
{
  // From the apex (top of flyHigh +6 ≈ very high) trickle down the lydian
  // scale in a soft, slowing run — like the butterfly spiralling to rest.
  const top = degreeShift(snap(m("D6")), 6); // apex-ish reference
  let bar = 17, beat = 1.0;
  const steps = [0, -1, -2, -3, -4, -5, -6, -7]; // descend lydian degrees
  let dur = 0.5;
  for (let i = 0; i < steps.length; i++) {
    const midi = degreeShift(top, steps[i]);
    cascade.push({
      preset: i < 4 ? "glockenspiel" : "kalimba",
      startSec: bar * BAR + beat * BEAT,
      midi,
      durSec: (dur + i * 0.06) * BEAT * 1.4,
      gain: 0.2 - i * 0.012,
      decayMul: (DECAY[i < 4 ? "glockenspiel" : "kalimba"] ?? 1.6) * 1.5,
      pan: 0.3 - i * 0.06,
    });
    beat += dur + i * 0.05; // gently slowing
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
}

// a single high kalimba grace note as the very last butterfly wingbeat,
// settling on the F-lydian tonic up high after everything has come to rest.
const finalWing = {
  preset: "kalimba",
  startSec: 23 * BAR + 0.4 * BEAT,
  midi: snap(m("F6")),
  durSec: 3 * BEAT,
  gain: 0.2,
  decayMul: (DECAY.kalimba ?? 1.75) * 1.5,
  pan: 0.32,
};

const events = [...body, ...pad, ...climbPad, ...terraces, ...cascade, finalWing];

const { mp3, durationSec } = renderLullaby(events, {
  name: "lydianbaba",
  here: HERE,
  title: "lydianbaba",
  reverb: { wet: 0.38, decay: 0.87, damp: 0.33 },
  fadeIn: 1.0,
  fadeOut: 4.5,
  tailSec: 5.0,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
