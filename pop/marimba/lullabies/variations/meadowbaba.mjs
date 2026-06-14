// meadowbaba.mjs — an open-meadow folk riff on the marimbaba lullaby,
// GROWN ONE NOTE AT A TIME.
//
// Direction: G major pentatonic, rosewood + kalimba, ~60 BPM. Wide-sky folk
// warmth — sun on long grass, nothing hurried. The melody is not stated and
// repeated; it is BUILT additively, Reich-style, so the tune assembles itself
// in front of you and then, at dusk, unbuilds.
//
// DEVELOPMENT STRATEGY — ADDITIVE PHRASE-BUILDING (Reich "Music for 18"):
//  - We define ONE full folk melody, FOLK, in G major pentatonic — a tune
//    that is itself the marimbaba contour re-moded (hush sigh → climbing
//    twinkle wave → wow lift → slinky bap → sleep cadence), folded onto the
//    five-note scale so it reads as open-meadow folk.
//  - The piece starts with the FIRST 2 NOTES of that melody, looped. Each
//    repetition ADDS ONE MORE NOTE from the front of the tune, so the phrase
//    grows organically: 2 → 3 → 4 → … until the whole folk tune blooms.
//  - At the crest, the full tune sings out clear (rosewood lead, kalimba
//    answer, soft pad + bass under). Then it SHEDS notes back down — losing
//    one from the tail each pass — until only the opening two notes remain,
//    and the meadow goes to sleep on them.
//  - A recognizable marimbaba thread survives: the contour is the whistlegraph
//    phrase set, the descending-then-climbing shape is intact, and the final
//    cadence settles to G (the home root) exactly as marimbaba settles to F.
//
// Run:  node variations/meadowbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 60;
const BEAT = 60 / BPM; // 1.0s — slow, breathing
// We work in free phrase-time (cumulative seconds), not bars, because the
// additive process grows the phrase length pass by pass.

// ── G major PENTATONIC fold (G A B D E). Snap any midi onto the five-note
//    scale so every voice stays in the open-meadow mode, no leading tones. ──
const ROOT = 7; // G
const PENTA = [0, 2, 4, 7, 9]; // G A B D E
function snap(midi) {
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let best = PENTA[0], bestD = 99;
  for (const s of PENTA) {
    const d = Math.min(Math.abs(s - rel), 12 - Math.abs(s - rel));
    if (d < bestD) { bestD = d; best = s; }
  }
  const base = midi - rel;
  let cand = base + best;
  if (cand - midi > 6) cand -= 12;
  if (midi - cand > 6) cand += 12;
  return cand;
}
const g = (name) => snap(m(name));

const LEAD = "rosewood";

// ════════════════════════════════════════════════════════════════════════
//  THE FOLK TUNE — assembled from the marimbaba MOTIFS, re-moded into G
//  pentatonic. Each entry is [midi, beats]. This is the tune the additive
//  process grows toward and then sheds. ~16 notes: a complete little folk
//  melody with a rise, a peak, and a homeward fall.
// ════════════════════════════════════════════════════════════════════════
const FOLK = [
  // opening sigh (hush, re-moded) — the two seed notes the meadow starts on
  ["D5", 1.0],   // 0
  ["B4", 1.0],   // 1
  ["G4", 1.5],   // 2  — settle to home
  // climbing meadow wave (twinkle re-moded)
  ["A4", 0.5],   // 3
  ["B4", 1.0],   // 4
  ["D5", 1.0],   // 5
  ["E5", 1.5],   // 6  — first crest
  // the lift (wow / "way up high")
  ["D5", 0.5],   // 7
  ["E5", 1.0],   // 8
  ["G5", 2.0],   // 9  — top of the sky
  // slinky bap coming down (baba re-moded)
  ["E5", 0.5],   // 10
  ["D5", 0.5],   // 11
  ["B4", 1.0],   // 12
  // sleep cadence home
  ["A4", 1.0],   // 13
  ["G4", 1.0],   // 14
  ["G4", 2.0],   // 15 — final settle
].map(([n, b]) => [g(n), b]);

// ── small developers (arrays of [midi,beats]) ──────────────────────────────
// take the first k notes of the tune (the growing phrase)
const grow = (k) => FOLK.slice(0, Math.max(2, k));
// take the first k notes (the shedding phrase loses from the tail)
const shed = (k) => FOLK.slice(0, Math.max(2, k));
// transpose a phrase by semitones, re-snapping into the pentatonic
const tpose = (c, semis) => c.map(([midi, beats]) => [snap(midi + semis), beats]);

// ── lay a phrase into events at an absolute start time (seconds). Returns the
//    time AFTER the phrase (for chaining passes back to back). High notes
//    drift a touch right so the meadow has air. ─────────────────────────────
function lay(out, c, { at, voice = LEAD, gain = 0.5, decayMul = 1.6, panBase = 0 } = {}) {
  let t = at;
  for (const [midi, beats] of c) {
    const reg = (midi - 67) / 18; // ~ -1..+1 around G4..G5
    const pan = Math.max(-0.4, Math.min(0.4, panBase + reg * 0.18));
    out.push({
      preset: voice,
      startSec: t,
      midi,
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1) * decayMul,
      pan,
    });
    t += beats * BEAT;
  }
  return t;
}

// kalimba "dewdrop" — a single soft high answer note.
function drop(out, { at, name, beats = 1.0, gain = 0.2, pan = 0.3, voice = "kalimba" }) {
  out.push({
    preset: voice,
    startSec: at,
    midi: g(name),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[voice] ?? 1) * 1.7,
    pan,
  });
}

// vibraphone_off pad — a slow open-fifth meadow haze under a span.
function pad(out, { at, names, lenSec, gain = 0.12 }) {
  names.forEach((nm, i) => {
    out.push({
      preset: "vibraphone_off",
      startSec: at,
      midi: g(nm),
      durSec: lenSec,
      gain,
      decayMul: 2.0,
      pan: i === 0 ? -0.2 : i === 1 ? 0.0 : 0.2,
    });
  });
}

// soft bass root, kept low + clear.
function bass(out, { at, name, lenSec, gain = 0.38 }) {
  out.push({
    preset: "bass",
    startSec: at,
    midi: g(name),
    durSec: lenSec,
    gain,
    decayMul: DECAY.bass * 1.4,
    pan: 0,
  });
}

const events = [];
let t = 0;

// gap between additive passes — a held breath so each new note is heard.
const REST = 0.9 * BEAT;

// ════════════════════════════════════════════════════════════════════════
//  PART I — DAWN: the phrase GROWS one note per pass (2 → full tune).
//  Each pass is the first k notes of FOLK; k climbs by 1. Bass + pad fade
//  in as the tune fills out, like sun rising over the field.
// ════════════════════════════════════════════════════════════════════════
const GROW_FROM = 2;
const GROW_TO = FOLK.length; // 16
// grow by 1 note while the phrase is short, then by larger steps as it fills,
// so the additive bloom is audible up front without the back half dragging.
const growSteps = [2, 3, 4, 5, 7, 9, 12, GROW_TO];
for (const k of growSteps) {
  const phrase = grow(k);
  const start = t;
  // the lead grows the phrase
  const gain = 0.4 + 0.12 * (k / GROW_TO); // swells gently as it fills
  t = lay(events, phrase, { at: start, gain, decayMul: 1.7 });
  const span = t - start;

  // the newest note gets a faint kalimba dewdrop echo a beat later — so the
  // ear notices the ADDITION each pass.
  if (k > GROW_FROM) {
    const [newMidi] = FOLK[k - 1];
    drop(events, {
      at: start + span + 0.15 * BEAT,
      name: ["G", "A", "B", "D", "E"][((newMidi % 12) + 12) % 12 % 5] + "5",
      beats: 1.0,
      gain: 0.13 + 0.04 * (k / GROW_TO),
      pan: 0.32,
    });
  }

  // harmony fades in as the phrase passes the midpoint
  if (k >= 5) {
    bass(events, { at: start, name: k % 2 === 0 ? "G2" : "D2", lenSec: span, gain: 0.3 });
  }
  if (k >= 8) {
    pad(events, { at: start, names: ["G3", "D4", "B4"], lenSec: span + REST, gain: 0.1 });
  }

  t += REST;
}

// ════════════════════════════════════════════════════════════════════════
//  PART II — NOON: the FULL TUNE in bloom, twice. First plain, then with a
//  kalimba canon a phrase-fifth above answering — the meadow at full light.
// ════════════════════════════════════════════════════════════════════════
{
  // full tune, clear and warm
  const start = t;
  t = lay(events, FOLK, { at: start, gain: 0.54, decayMul: 1.8 });
  const span = t - start;
  bass(events, { at: start, name: "G2", lenSec: span / 2, gain: 0.36 });
  bass(events, { at: start + span / 2, name: "D2", lenSec: span / 2 + REST, gain: 0.34 });
  pad(events, { at: start, names: ["G3", "D4", "B4"], lenSec: span + REST, gain: 0.12 });
  t += REST;
}
{
  // full tune again with a kalimba canon (tune up a 4th) entering a beat late —
  // two folk singers across the field.
  const start = t;
  t = lay(events, FOLK, { at: start, gain: 0.5, decayMul: 1.8 });
  const span = t - start;
  lay(events, tpose(FOLK, 5), {
    at: start + 2 * BEAT, voice: "kalimba", gain: 0.18, decayMul: 1.6, panBase: 0.28,
  });
  bass(events, { at: start, name: "G2", lenSec: span / 2, gain: 0.36 });
  bass(events, { at: start + span / 2, name: "E2", lenSec: span / 2 + REST, gain: 0.32 });
  pad(events, { at: start, names: ["E3", "B3", "G4"], lenSec: span + REST, gain: 0.12 });
  t += REST;
}

// ════════════════════════════════════════════════════════════════════════
//  PART III — DUSK: the phrase SHEDS notes, one per pass (full → 2). The
//  tune unbuilds and the meadow settles onto its two seed notes, then home.
// ════════════════════════════════════════════════════════════════════════
// shed in a few quick steps so dusk settles without dragging.
const shedSteps = [11, 8, 5, 3];
const SHED_FROM = shedSteps[0];
for (const k of shedSteps) {
  const phrase = shed(k);
  const start = t;
  const gain = 0.46 - 0.06 * ((SHED_FROM - k) / SHED_FROM); // softening
  t = lay(events, phrase, { at: start, gain, decayMul: 1.9 });
  const span = t - start;
  if (k >= 6) {
    bass(events, { at: start, name: "G2", lenSec: span, gain: 0.28 });
    pad(events, { at: start, names: ["G3", "D4", "B4"], lenSec: span + REST, gain: 0.09 });
  } else {
    bass(events, { at: start, name: "G2", lenSec: span + REST, gain: 0.24 });
  }
  t += REST * 1.2; // breaths lengthen as it falls asleep
}

// ── final settle: the two seed notes one last time, then home on G, faint. ──
{
  const start = t;
  t = lay(events, [FOLK[0], FOLK[1]], { at: start, gain: 0.38, decayMul: 2.0 });
  t = lay(events, [[g("G4"), 3.0]], { at: t, gain: 0.34, decayMul: 2.1 });
  bass(events, { at: start, name: "G2", lenSec: t - start, gain: 0.24 });
  pad(events, { at: start, names: ["G3", "D4", "G4"], lenSec: t - start + 2, gain: 0.1 });
  drop(events, { at: t - 1.5 * BEAT, name: "D6", beats: 3.0, gain: 0.08, pan: -0.26 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "meadowbaba",
  here: HERE,
  title: "meadowbaba",
  reverb: { wet: 0.36, decay: 0.85, damp: 0.36 }, // open-air, soft glass
  fadeIn: 1.4,
  fadeOut: 5.5,
  tailSec: 6.0,
  peak: 0.84,
  healingHz: 639, // Solfeggio FA — connection/warmth, sits well in G
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
