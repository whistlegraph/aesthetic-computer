// velvetbaba.mjs — a plush, lush riff on the marimbaba lullaby, scored for
// vibraphone in Db major at ~50 BPM.
//
// Direction: PLUSH & LUSH. The whistlegraph tune (hush → twinkle → wow →
// baba → sleep) is kept as the melodic DNA, but it is developed by
// REHARMONIZATION + a COUNTERMELODY in parallel thirds/sixths that weaves
// underneath the lead. The harmony shifts richly beneath a slowly
// elaborating tune: each return of a phrase sits over a new chord, so the
// same notes keep recoloring (a Db pedal heard as Db, then Bbm7, then Gbmaj7,
// then Absus, etc.). The countermelody shadows the lead a 3rd/6th below,
// so the line always travels in velvet doubles.
//
// DEVELOPMENT STRATEGY — REHARMONIZATION + PARALLEL-THIRDS COUNTERMELODY:
//  - PASS structure walks the tune through a lush descending-fifths-ish
//    progression in Db, revoicing the SAME melodic cells over fresh chords.
//  - A countermelody (harmonize() snaps each lead note down a diatonic 3rd
//    or 6th) doubles the lead through a softer voice (vibraphone_off / kelon),
//    so the tune is always plush, never bare.
//  - The tune slowly ELABORATES: bare statement → thirds-doubled → ornamented
//    (passing tones) → widening to sixths at the apex → settling back to a
//    close thirds-doubled cadence for sleep.
//  - Recognizable thread: the descending hush sigh opens it, the contour of
//    twinkle/wow/baba is intact, and it cadences home to Db.
//
// Run:  node variations/velvetbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 50;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4

// ── Db major scale-fold (root pc = 1). Snap melodic voices to the mode. ──
const ROOT = 1; // Db
const MAJOR = [0, 2, 4, 5, 7, 9, 11];
function snap(midi) {
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let best = MAJOR[0], bestD = 99;
  for (const s of MAJOR) {
    const d = Math.min(Math.abs(s - rel), 12 - Math.abs(s - rel));
    if (d < bestD) { bestD = d; best = s; }
  }
  const base = midi - rel;
  let cand = base + best;
  if (cand - midi > 6) cand -= 12;
  if (midi - cand > 6) cand += 12;
  return cand;
}

const LEAD = "vibraphone";
const SHADOW = "vibraphone_off"; // the velvet countermelody voice
const ORN = "kelon";             // softer ornament voice

// Db scale degrees as MIDI roots for chords/pads/bass.
// names spelled flat-friendly: Db Eb F Gb Ab Bb C
const DEGNAME = ["Db", "Eb", "F", "Gb", "Ab", "Bb", "C"];
const deg = (d, oct) => snap(m(DEGNAME[((d % 7) + 7) % 7] + oct));

// ── motif → Db-folded MIDI cells. The marimbaba MOTIFS are written in F;
//    transpose -4 toward Db then snap into the mode. Each cell is a list of
//    [midi, beats] pairs we can revoice / harmonize / ornament freely. ────────
function cell(motif) {
  return MOTIFS[motif].map(([name, beats]) => [snap(m(name) - 4), beats]);
}

// transpose a whole cell by an interval, keeping it diatonic.
function tpose(c, semis) { return c.map(([midi, beats]) => [snap(midi + semis), beats]); }

// ── DIATONIC HARMONIZER — the heart of the velvet sound. ───────────────────
// harmonize(cell, steps) shifts each note DOWN by `steps` scale-degrees
// (2 → a 3rd below, 5 → a 6th below), staying in Db major. This builds the
// parallel-thirds / -sixths countermelody that shadows the lead.
function scaleIndex(midi) {
  // nearest diatonic degree index (0..6) + octave so we can step by degree.
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let bestI = 0, bestD = 99;
  for (let i = 0; i < MAJOR.length; i++) {
    const d = Math.min(Math.abs(MAJOR[i] - rel), 12 - Math.abs(MAJOR[i] - rel));
    if (d < bestD) { bestD = d; bestI = i; }
  }
  const oct = Math.floor((midi - ROOT - MAJOR[bestI]) / 12);
  return { i: bestI, oct };
}
function degToMidi(i, oct) {
  let ii = i, o = oct;
  while (ii < 0) { ii += 7; o -= 1; }
  while (ii > 6) { ii -= 7; o += 1; }
  return ROOT + MAJOR[ii] + 12 * o;
}
function harmonize(c, steps) {
  return c.map(([midi, beats]) => {
    const { i, oct } = scaleIndex(midi);
    return [degToMidi(i - steps, oct), beats];
  });
}

// ── ORNAMENT — insert a passing tone between notes a 3rd apart, so the tune
//    "elaborates" without losing its shape. Returns a denser cell. ──────────
function ornament(c, depth = 0.5) {
  const out = [];
  for (let i = 0; i < c.length; i++) {
    const [midi, beats] = c[i];
    const next = c[i + 1];
    if (next && beats >= 1) {
      const gap = next[0] - midi;
      if (Math.abs(gap) >= 3 && Math.abs(gap) <= 5) {
        // split this note: keep most of it, then slip a stepwise passing tone
        const { i: di, oct } = scaleIndex(midi);
        const passing = degToMidi(di + (gap > 0 ? 1 : -1), oct);
        out.push([midi, beats - depth]);
        out.push([passing, depth]);
        continue;
      }
    }
    out.push([midi, beats]);
  }
  return out;
}

// ── lay a cell into events at a bar, voice, with register-following pan. ─────
function lay(out, c, { startBar, voice = LEAD, gain = 0.5, pan = 0, decayMul = 1.7, beat0 = 0 } = {}) {
  let beat = beat0;
  for (let i = 0; i < c.length; i++) {
    const [midi, beats] = c[i];
    out.push({
      preset: voice,
      startSec: startBar * BAR + beat * BEAT,
      midi,
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1) * decayMul,
      pan,
    });
    beat += beats;
  }
  return beat;
}

// ── lay the lead AND its parallel countermelody together. This is the velvet:
//    the tune always travels doubled in thirds (or sixths at the apex). The
//    shadow is panned opposite + softer so it weaves UNDER the lead. ─────────
function layVelvet(out, c, { startBar, steps = 2, leadGain = 0.5, shadGain = 0.3,
  leadPan = 0.14, shadPan = -0.2, shadVoice = SHADOW, decayMul = 1.7 } = {}) {
  lay(out, c, { startBar, voice: LEAD, gain: leadGain, pan: leadPan, decayMul });
  lay(out, harmonize(c, steps), { startBar, voice: shadVoice, gain: shadGain, pan: shadPan, decayMul: decayMul + 0.2 });
}

// ── a lush chord pad (vibraphone_off) breathing under a span of bars. The
//    REHARMONIZATION lives here: `chord` is a list of scale-degree+octave
//    pairs, so the same melody recolors as the chord beneath it changes. ─────
function pad(out, startBar, lenBars, chord, gain = 0.12) {
  for (let i = 0; i < chord.length; i++) {
    const [d, oct] = chord[i];
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: deg(d, oct),
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.1,
      pan: (i - (chord.length - 1) / 2) * 0.16,
    });
  }
}

// soft bass root under a span (kept low + clear).
function bass(out, startBar, d, oct, lenBars = 2, gain = 0.4) {
  out.push({
    preset: "bass",
    startSec: startBar * BAR,
    midi: deg(d, oct),
    durSec: lenBars * BAR,
    gain,
    decayMul: DECAY.bass * 1.4,
    pan: 0,
  });
}

// a single high sparkle (glockenspiel/kalimba) — velvet glints.
function star(out, { startBar, beat, name, beats, voice = "glockenspiel", gain = 0.11, pan = 0.3 }) {
  out.push({
    preset: voice,
    startSec: startBar * BAR + beat * BEAT,
    midi: snap(m(name)),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[voice] ?? 1) * 1.6,
    pan,
  });
}

const events = [];

// ════════════════════════════════════════════════════════════════════════
//  ARC: bare hush → thirds-doubled twinkle (reharmonized) → ornamented wow →
//        sixths-widened baba apex → close thirds cadence to sleep, home Db.
//  The chord bed walks: Db → Bbm7 → Gbmaj7 → Ab → Db ... re-coloring the
//  recurring tune so it keeps transforming under the velvet.
// ════════════════════════════════════════════════════════════════════════

// ── PASS 0 (bars 0–3): the hush sigh, almost BARE — the tune as first heard,
//    over a plush Db(add9) bed. A faint shadow a 3rd below enters only on the
//    held note, so the recognizable descending sigh stays legible. ──────────
{
  const hush = cell("hush");
  lay(events, hush, { startBar: 0, voice: LEAD, gain: 0.5, pan: 0.1, decayMul: 1.8 });
  // shadow just the final long note (a 6th below) — first hint of velvet
  lay(events, harmonize([hush[hush.length - 1]], 5), { startBar: 0, voice: SHADOW, gain: 0.26, pan: -0.18, decayMul: 2.0, beat0: 6 });
  bass(events, 0, 0, 2, 2, 0.42); // Db
  bass(events, 2, 0, 2, 2, 0.42);
  pad(events, 0, 4, [[0, 4], [2, 4], [4, 4], [1, 5]], 0.12); // Db add9 (Db F Ab Eb)
  star(events, { startBar: 2, beat: 2.2, name: "Ab5", beats: 1.5, gain: 0.08, pan: 0.32 });
}

// ── PASS 1 (bars 4–9): twinkle, now DOUBLED IN THIRDS — the velvet arrives.
//    REHARMONIZED: the climbing wave is heard first over Bbm7 (vi), then the
//    same wave sequenced up over Gbmaj7 (IV) — same contour, new color. ──────
{
  const tw = cell("twinkle");
  layVelvet(events, tw, { startBar: 4, steps: 2, leadGain: 0.48, shadGain: 0.3, leadPan: 0.16, shadPan: -0.22 });
  // answer: the wave sequenced up a 3rd, still doubled — the tune climbing
  layVelvet(events, tpose(tw, 3), { startBar: 6, steps: 2, leadGain: 0.44, shadGain: 0.28, leadPan: 0.18, shadPan: -0.24 });
  // bars 8–9: a flyHigh lift, doubled in thirds, the velvet reaching up
  layVelvet(events, cell("flyHigh"), { startBar: 8, steps: 2, leadGain: 0.46, shadGain: 0.28, leadPan: 0.2, shadPan: -0.26 });

  bass(events, 4, 5, 1, 2, 0.36); // Bb (vi)
  bass(events, 6, 3, 1, 2, 0.36); // Gb (IV)
  bass(events, 8, 4, 1, 2, 0.36); // Ab (V)
  pad(events, 4, 2, [[5, 3], [0, 4], [2, 4], [4, 4]], 0.12); // Bbm7 (Bb Db F Ab)
  pad(events, 6, 2, [[3, 3], [5, 3], [2, 4], [4, 4]], 0.12); // Gbmaj7 (Gb Bb F Ab... -> Gb Bb F)
  pad(events, 8, 2, [[4, 3], [6, 3], [2, 4]], 0.12); // Ab (Ab C F)
  star(events, { startBar: 5, beat: 1.0, name: "Db6", beats: 1.5, voice: "kalimba", gain: 0.12, pan: -0.26 });
  star(events, { startBar: 9, beat: 0.5, name: "Gb6", beats: 1.5, voice: "glockenspiel", gain: 0.1, pan: 0.34 });
}

// ── PASS 2 (bars 10–15): the "wow" wobble, now ORNAMENTED (passing tones)
//    and doubled in thirds — the tune slowly elaborating. The baba slinky-bap
//    follows, ornamented too. REHARMONIZED over a warm IV → ii → V swell. ────
{
  const wow = ornament(cell("wow"), 0.5);
  layVelvet(events, wow, { startBar: 10, steps: 2, leadGain: 0.44, shadGain: 0.28, leadPan: 0.16, shadPan: -0.22, decayMul: 1.8 });
  const baba = ornament(cell("baba"), 0.25);
  layVelvet(events, baba, { startBar: 13, steps: 2, leadGain: 0.42, shadGain: 0.26, leadPan: 0.18, shadPan: -0.24, decayMul: 1.7 });

  bass(events, 10, 3, 1, 2, 0.34); // Gb (IV)
  bass(events, 12, 1, 1, 2, 0.34); // Eb (ii)
  bass(events, 14, 4, 1, 2, 0.34); // Ab (V)
  pad(events, 10, 2, [[3, 3], [5, 3], [2, 4]], 0.12); // Gbmaj
  pad(events, 12, 2, [[1, 3], [3, 4], [5, 4]], 0.12); // Ebm7 (Eb Gb Bb)
  pad(events, 14, 2, [[4, 3], [6, 3], [2, 4]], 0.12); // Ab
  star(events, { startBar: 11, beat: 1.6, name: "Bb5", beats: 1.5, voice: "glockenspiel", gain: 0.1, pan: 0.32 });
  star(events, { startBar: 13, beat: 2.0, name: "F6", beats: 1.5, voice: "glockenspiel", gain: 0.1, pan: 0.36 });
}

// ── PASS 3 (bars 16–21): THE APEX — the velvet WIDENS to SIXTHS. The twinkle
//    wave returns up an octave, doubled a SIXTH below (plusher, more open),
//    over the richest reharmonization (Gbmaj7 → Db/F → Ab9). A second, inner
//    countermelody (a 3rd) fills the chord so the line travels in full thirds-
//    and-sixths velvet. ──────────────────────────────────────────────────────
{
  const tw = tpose(cell("twinkle"), 0);
  // lead up an octave, shadow a SIXTH below, plus an inner 3rd voice — triple velvet
  lay(events, tpose(tw, 12), { startBar: 16, voice: LEAD, gain: 0.46, pan: 0.18, decayMul: 1.8 });
  lay(events, harmonize(tpose(tw, 12), 5), { startBar: 16, voice: SHADOW, gain: 0.28, pan: -0.24, decayMul: 2.0 }); // a 6th below
  lay(events, harmonize(tpose(tw, 12), 2), { startBar: 16, voice: ORN, gain: 0.2, pan: -0.06, decayMul: 1.5 });    // inner 3rd

  const fly = cell("flyHigh");
  lay(events, tpose(fly, 12), { startBar: 18, voice: LEAD, gain: 0.44, pan: 0.2, decayMul: 1.8 });
  lay(events, harmonize(tpose(fly, 12), 5), { startBar: 18, voice: SHADOW, gain: 0.26, pan: -0.26, decayMul: 2.0 });

  // a final wide wow at the very top, sixths-doubled, before the descent
  const wow = cell("wow");
  layVelvet(events, tpose(wow, 12), { startBar: 20, steps: 5, leadGain: 0.42, shadGain: 0.26, leadPan: 0.16, shadPan: -0.26, decayMul: 1.8 });

  bass(events, 16, 3, 1, 2, 0.34); // Gb
  bass(events, 18, 0, 1, 2, 0.34); // Db (over F-ish bass color via pad)
  bass(events, 20, 4, 1, 2, 0.34); // Ab9
  pad(events, 16, 2, [[3, 3], [5, 3], [2, 4], [4, 4]], 0.12); // Gbmaj7
  pad(events, 18, 2, [[2, 3], [0, 4], [4, 4], [2, 5]], 0.12); // Db/F (F Db Ab F)
  pad(events, 20, 2, [[4, 3], [6, 3], [2, 4], [1, 4]], 0.12); // Ab9 (Ab C F Eb)
  star(events, { startBar: 17, beat: 1.5, name: "Db7", beats: 2, gain: 0.09, pan: 0.34 });
  star(events, { startBar: 21, beat: 0.8, name: "Ab6", beats: 2, voice: "kalimba", gain: 0.1, pan: -0.24 });
}

// ── PASS 4 (bars 22–27): SLEEP. The velvet settles back to a close thirds-
//    doubled cadence in the home register, harmony resolving V → I (Ab → Db).
//    The tune comes home, recognizable and plush. A last hush echo closes
//    the frame. ──────────────────────────────────────────────────────────────
{
  const sleep = cell("sleep");
  layVelvet(events, sleep, { startBar: 22, steps: 2, leadGain: 0.46, shadGain: 0.3, leadPan: 0.12, shadPan: -0.18, decayMul: 1.9 });
  // a final, faint, close hush echo — doubled a 3rd below (recognizable thread)
  layVelvet(events, cell("hush"), { startBar: 25, steps: 2, leadGain: 0.4, shadGain: 0.26, leadPan: 0.1, shadPan: -0.16, decayMul: 2.1 });

  bass(events, 22, 4, 1, 2, 0.36); // Ab (V)
  bass(events, 24, 0, 2, 2, 0.34); // Db (I)
  bass(events, 26, 0, 2, 2, 0.32); // Db
  pad(events, 22, 2, [[4, 3], [6, 3], [2, 4]], 0.12); // Ab (V)
  pad(events, 24, 4, [[0, 4], [2, 4], [4, 4], [1, 5]], 0.13); // Db add9 — long resolving home
  star(events, { startBar: 25, beat: 1.0, name: "Db6", beats: 4, gain: 0.08, pan: 0.3 });
  star(events, { startBar: 26, beat: 1.5, name: "Ab5", beats: 3, voice: "kalimba", gain: 0.08, pan: -0.22 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "velvetbaba",
  here: HERE,
  title: "velvetbaba",
  reverb: { wet: 0.4, decay: 0.86, damp: 0.36 }, // plush velvet room
  fadeIn: 1.4,
  fadeOut: 6.0,
  tailSec: 6.0,
  peak: 0.83,
  healingHz: 639, // Solfeggio FA — "connection/relationships", sits well in Db
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
