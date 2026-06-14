// tidebaba.mjs — an ocean-rocking riff on the marimbaba lullaby, breathing
// in and out like the tide.
//
// Direction: C major, ~52 BPM, vibraphone-led with rosewood for the wave
// crests. The whistlegraph contour (hush → twinkle → wow → baba → sleep) is
// kept as the melodic DNA, but here it is shaped by the SWELL of the sea.
//
// DEVELOPMENT STRATEGY — PHRASE EXPANSION / CONTRACTION + DYNAMIC SWELLS:
//  - Each "tide" is a phrase that GROWS then RECEDES. A motif cell is first
//    stated short and quiet (the water far out), then re-stated longer and
//    louder — note durations stretch, the cell is extended by sequencing its
//    own tail, gains crest — then it CONTRACTS again: durations shrink, the
//    phrase sheds notes from the end, and the dynamic ebbs back to a whisper.
//  - Across the whole piece the tides themselves grow: early tides are small
//    (a 2–3 bar swell), the middle tide is the spring tide (the big wave,
//    widest expansion + loudest crest), and the late tides recede to a single
//    low lapping cadence — the sea going to sleep.
//  - A recognizable thread survives: the descending hush sigh opens the first
//    tide, the slinky baba bap rides the spring-tide crest, and the final
//    sleep cadence settles to C in the home octave like the last wave on sand.
//
// Run:  node variations/tidebaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 52;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4 — a slow rocking meter

// ── C major scale-snap (root pc = 0). Keep every voice in the mode. ─────────
const ROOT = 0; // C
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
const CREST = "rosewood"; // the wave-crest voice, brighter mallet

// ── motif → C-folded MIDI cells. The marimbaba MOTIFS are written in F;
//    transpose -5 down into C territory then snap into the mode. Each cell is
//    a list of [midi, beats] pairs we can stretch / extend / shed freely. ────
function cell(motif) {
  return MOTIFS[motif].map(([name, beats]) => [snap(m(name) - 5), beats]);
}

// ── PHRASE-SHAPING ENGINE ───────────────────────────────────────────────────

// stretch(cell, factor) — multiply every note duration: the tide rolling in
// slow (factor > 1) or pulling out fast (factor < 1). The melody breathes.
function stretch(c, factor) {
  return c.map(([midi, beats]) => [midi, beats * factor]);
}

// extend(cell, n) — GROW the phrase by sequencing its own tail: append the
// last `n` notes again, each lifted one scale step, like a wave reaching
// further up the sand each surge. This is the expansion half of a tide.
function extend(c, n, stepUp = 2) {
  const tail = c.slice(Math.max(0, c.length - n));
  const more = tail.map(([midi, beats]) => [snap(midi + stepUp), beats]);
  return [...c, ...more];
}

// shed(cell, n) — CONTRACT the phrase: drop the last `n` notes. The water
// not reaching as far now — the ebb. The contraction half of a tide.
function shed(c, n) {
  return c.slice(0, Math.max(1, c.length - n));
}

// transpose a whole cell by an interval (for sequencing).
function tpose(c, semis) { return c.map(([midi, beats]) => [snap(midi + semis), beats]); }

// invert a cell around its first note (mirror the intervals) — the undertow.
function invert(c) {
  const axis = c[0][0];
  return c.map(([midi, beats]) => [snap(axis - (midi - axis)), beats]);
}

// ── lay a cell into events with a per-note DYNAMIC SWELL. `swell` shapes the
//    gains across the phrase: a half-cosine hump so the middle of the phrase
//    is loudest (the wave cresting) and the edges are quiet (the trough).
//    `crest` is the peak gain, `trough` the edge gain. ──────────────────────
function lay(out, c, {
  startBar, beat0 = 0, voice = LEAD,
  crest = 0.5, trough = 0.32, panBase = 0, panSpread = 0.3,
  decayMul = 1.6,
} = {}) {
  let beat = beat0;
  const N = c.length;
  for (let i = 0; i < N; i++) {
    const [midi, beats] = c[i];
    // swell envelope: 0 at the edges, 1 at the centre of the phrase.
    const t = N > 1 ? i / (N - 1) : 0.5;
    const swell = 0.5 - 0.5 * Math.cos(2 * Math.PI * t); // hump
    const gain = trough + (crest - trough) * swell;
    // pan drifts gently with register so the tide moves across the stereo sand
    const reg = (midi - 60) / 18;
    const pan = Math.max(-0.5, Math.min(0.5, panBase + reg * panSpread));
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
  return beat; // beats consumed
}

// vibraphone_off pad — a C-folded triad breathing under a span of bars, the
// deep ocean swell beneath the surface waves.
const padDeg = (deg, oct) => snap(m(["C", "D", "E", "F", "G", "A", "B"][deg] + oct));
function pad(out, startBar, lenBars, triad, baseOct, gain = 0.12) {
  for (let i = 0; i < triad.length; i++) {
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: padDeg(triad[i], baseOct),
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.0,
      pan: i === 0 ? -0.24 : i === 1 ? 0.0 : 0.24,
    });
  }
}

// soft bass root under a bar (kept low + clear) — the seabed.
function bass(out, startBar, name, lenBars = 1, gain = 0.4) {
  out.push({
    preset: "bass",
    startSec: startBar * BAR,
    midi: snap(m(name)),
    durSec: lenBars * BAR,
    gain,
    decayMul: DECAY.bass * 1.4,
    pan: 0,
  });
}

// a single high foam-sparkle (glockenspiel / kalimba) — sea spray off a crest.
function spray(out, { startBar, beat, name, beats, voice = "glockenspiel", gain = 0.11, pan = 0.3 }) {
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
//  ARC: small tides → swelling tides → the spring tide (big wave) →
//        receding tides → the last lapping cadence.
// ════════════════════════════════════════════════════════════════════════

// ── TIDE 1 (bars 0–5): the FIRST SMALL TIDE. The hush sigh, far out and
//    quiet, stated short — then re-stated stretched a little longer and a
//    touch louder (the water rolling in), then contracted back to a whisper.
{
  const hush = cell("hush");
  // far out: short + quiet
  lay(events, stretch(hush, 0.9), { startBar: 0, crest: 0.34, trough: 0.24, decayMul: 1.7, panSpread: 0.22 });
  // rolling in: stretched longer, a little louder, extended one surge further
  lay(events, stretch(extend(hush, 2), 1.15), { startBar: 2, crest: 0.46, trough: 0.3, decayMul: 1.8, panSpread: 0.26 });
  // ebbing: shed the tail, pull back quiet
  lay(events, stretch(shed(hush, 1), 0.85), { startBar: 4, crest: 0.34, trough: 0.22, decayMul: 1.9, panSpread: 0.2 });
  bass(events, 0, "C2", 2, 0.4); bass(events, 2, "C2", 2, 0.38); bass(events, 4, "G2", 2, 0.36);
  pad(events, 0, 6, [0, 2, 4], 3, 0.11); // C deep swell
  spray(events, { startBar: 3, beat: 1.6, name: "G5", beats: 1.5, gain: 0.08, pan: 0.3 });
}

// ── TIDE 2 (bars 6–11): a LARGER SWELL. The twinkle wave climbs in, this time
//    extended TWICE and stretched as it crests, the rosewood taking the very
//    top of the wave; then the undertow (inverted fragment) drags it back.
{
  const tw = cell("twinkle");
  // climbing in, growing: extend +2 notes, stretch slightly
  lay(events, stretch(extend(tw, 2), 1.1), { startBar: 6, crest: 0.5, trough: 0.32, voice: LEAD, decayMul: 1.7, panSpread: 0.3 });
  // the crest itself — same wave a third higher on the bright rosewood,
  // extended further, the loudest of this tide
  lay(events, stretch(extend(tpose(tw, 4), 3), 1.05), { startBar: 9, crest: 0.56, trough: 0.36, voice: CREST, decayMul: 1.5, panSpread: 0.32 });
  bass(events, 6, "C2", 2, 0.34); bass(events, 8, "F2", 2, 0.34); bass(events, 10, "G2", 2, 0.34);
  pad(events, 6, 3, [0, 2, 4], 3, 0.11);  // C
  pad(events, 9, 3, [3, 5, 0], 3, 0.11);  // F
  spray(events, { startBar: 7, beat: 1.2, name: "E5", beats: 1.5, voice: "kalimba", gain: 0.11, pan: -0.26 });
  spray(events, { startBar: 11, beat: 0.5, name: "C6", beats: 2, gain: 0.1, pan: 0.32 });
}

// ── TIDE 3 (bars 12–18): THE SPRING TIDE — the big wave. The wow wobble and
//    the slinky baba bap ride the crest, fully EXPANDED (extended + stretched
//    wide) and at the loudest swell of the piece, the rosewood blazing on top
//    while foam-spray rains overhead. This is the apex of the breathing.
{
  // the wow swell — expanded wide, stretched long, big crest
  const wow = cell("wow");
  lay(events, stretch(extend(wow, 3), 1.2), { startBar: 12, crest: 0.58, trough: 0.36, voice: LEAD, decayMul: 1.8, panSpread: 0.34 });
  // the baba slinky-bap rides the very top of the spring tide on rosewood,
  // extended and at full crest — the recognizable bap on the biggest wave
  const baba = cell("baba");
  lay(events, stretch(extend(baba, 3), 1.1), { startBar: 15, crest: 0.6, trough: 0.4, voice: CREST, decayMul: 1.5, panSpread: 0.34 });

  // foam-spray cascade raining off the crest
  const foam = [
    [12, 2.0, "C6", "glockenspiel", 0.34], [13, 1.0, "G5", "kalimba", -0.28],
    [14, 1.6, "E6", "glockenspiel", 0.36], [15, 2.2, "D6", "glockenspiel", 0.32],
    [16, 0.8, "G6", "glockenspiel", 0.4], [17, 1.4, "C6", "kalimba", -0.26],
    [18, 1.0, "E6", "glockenspiel", 0.3],
  ];
  for (const [b, bt, n, v, pn] of foam) spray(events, { startBar: b, beat: bt, name: n, beats: 1.5, voice: v, gain: 0.1, pan: pn });

  bass(events, 12, "C2", 2, 0.36); bass(events, 14, "G2", 2, 0.36); bass(events, 16, "A2", 2, 0.34); bass(events, 18, "F2", 1, 0.34);
  pad(events, 12, 3, [4, 6, 1], 3, 0.12); // G (dominant lift under the swell)
  pad(events, 15, 4, [0, 2, 4], 3, 0.12); // C
}

// ── TIDE 4 (bars 19–24): the tide RECEDING. The twinkle wave returns but now
//    it CONTRACTS — shed of its tail, durations shrinking, dynamics ebbing,
//    the undertow inversion pulling under. The sea growing calm.
{
  const tw = cell("twinkle");
  // shed two notes, slightly compressed durations, quieter
  lay(events, stretch(shed(extend(tw, 1), 2), 0.95), { startBar: 19, crest: 0.46, trough: 0.3, voice: LEAD, decayMul: 1.7, panSpread: 0.26 });
  // the undertow: an inverted, shed wow fragment, quieter still, pulling down
  lay(events, stretch(shed(invert(cell("wow")), 2), 0.85), { startBar: 22, crest: 0.38, trough: 0.26, voice: LEAD, decayMul: 1.8, panSpread: 0.22 });
  bass(events, 19, "F2", 2, 0.32); bass(events, 21, "G2", 2, 0.32); bass(events, 23, "C2", 2, 0.32);
  pad(events, 19, 3, [3, 5, 0], 3, 0.11); // F
  pad(events, 22, 3, [4, 6, 1], 3, 0.11); // G resolving home
  spray(events, { startBar: 20, beat: 1.5, name: "G5", beats: 2, gain: 0.08, pan: 0.3 });
}

// ── TIDE 5 (bars 25–30): THE LAST LAPPING. The sleep cadence settles to C in
//    the home octave — fully contracted, no expansion, the quietest swell of
//    all, the last wave running thin up the sand. A final faint hush echo
//    closes the frame (the recognizable thread, going to sleep).
{
  const sleep = cell("sleep");
  // play it close + low, gentle swell, soft — the sea coming to rest
  lay(events, stretch(sleep, 1.05), { startBar: 25, crest: 0.42, trough: 0.28, voice: LEAD, decayMul: 1.9, panSpread: 0.18 });
  // a final, faint, shed hush echo to close — the last lap of water
  lay(events, stretch(shed(cell("hush"), 1), 1.1), { startBar: 28, crest: 0.34, trough: 0.22, voice: LEAD, decayMul: 2.0, panSpread: 0.16 });
  bass(events, 25, "C2", 2, 0.34); bass(events, 27, "G2", 2, 0.32); bass(events, 29, "C2", 2, 0.3);
  pad(events, 25, 6, [0, 2, 4], 3, 0.12); // C — long resolving low tide
  spray(events, { startBar: 28, beat: 1.0, name: "C6", beats: 4, gain: 0.07, pan: 0.3 });
  spray(events, { startBar: 29, beat: 1.5, name: "G5", beats: 3, voice: "kalimba", gain: 0.08, pan: -0.24 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "tidebaba",
  here: HERE,
  title: "tidebaba",
  reverb: { wet: 0.4, decay: 0.86, damp: 0.36 }, // wide, washed sea-room
  fadeIn: 1.8,
  fadeOut: 6.0,
  tailSec: 6.0,
  peak: 0.83,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
