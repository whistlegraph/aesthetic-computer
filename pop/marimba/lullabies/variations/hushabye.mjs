// hushabye.mjs — the most classic, simple lullaby, given a real set of
// gentle THEME & VARIATIONS on a hushabye cadence.
//
// Direction: C major, rosewood, ~54 BPM, 3/4. Tender and timeless. Where
// moonbaba scatters the tune across the registers, hushabye does the
// opposite — it states one plain, singable hushabye theme and then keeps it
// in plain sight while dressing it five different ways, the way an old
// music-box turns the same little tune over and over until a child is asleep.
//
// DEVELOPMENT STRATEGY — THEME & VARIATIONS (classical):
//   THEME    (plain)         the bare hushabye cadence, one voice, lots of air
//   VAR I    (decorated)     same tune, ornamented — passing tones, grace
//                            neighbors, the line filled in like a music box
//   VAR II   (bass-takes-tune) the melody migrates DOWN into the bass marimba
//                            while a soft glock keeps the high frame
//   VAR III  (sparkle finale) the tune in canon — rosewood lead answered a
//                            bar later by a glockenspiel sparkle echo, the
//                            brightest, most awake pass before sleep
//   VAR IV   (hushed statement) the plainest pass of all, even barer than the
//                            theme, half-tempo feel, settling to a held C —
//                            the final goodnight
//
// The hushabye DNA = a recognizable thread kept in EVERY pass: the opening
// descending sigh (taken from the marimbaba "hush" motif, re-keyed to C) and
// the falling 3-2-1 (E-D-C) hushabye cadence that ends each variation.
//
// Run:  node variations/hushabye.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 56;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4

// ── C major scale-snap (root pc = 0). Keep every voice in the mode. ──────────
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

const LEAD = "rosewood";

// ── THE HUSHABYE THEME ──────────────────────────────────────────────────────
// A plain, singable cradle tune in C major, 3/4. It opens with the marimbaba
// "hush" descending sigh (re-keyed F→C, i.e. transpose +7), then rocks gently
// and lands on the 3-2-1 (E-D-C) hushabye cadence. Cells are [midi, beats].
//
// We keep it as named PHRASES so every variation can quote/decorate exactly
// the same line and the listener always hears "the same tune again."
const cell = (motif, semis = 7) =>
  MOTIFS[motif].map(([name, beats]) => [snap(m(name) + semis), beats]);

// hush sigh, re-keyed into C (G5-E5-C5, C5-held) — the recognizable opener.
const HUSH = cell("hush"); // ["C5"+7=G5, "A4"+7=E5, "F4"+7=C5, "C5" held]

// the rocking middle: a small up-and-back wave, then the cradle dip.
const ROCK = [
  ["G4", 1], ["A4", 1], ["G4", 1],     // bar a: rock up and back
  ["E4", 1.5], ["G4", 1.5],            // bar b: gentle lift
  ["A4", 1], ["G4", 1], ["E4", 1],     // bar c: rock back down
  ["D4", 3],                           // bar d: rest on the 2
].map(([n, b]) => [snap(m(n)), b]);

// the hushabye cadence: the falling 3-2-1 that closes every pass (E-D-C),
// preceded by a little neighbor so it sings "hush-a-bye, good-night".
const CADENCE = [
  ["E4", 1], ["F4", 1], ["E4", 1],     // hush-a-
  ["D4", 1.5], ["E4", 1.5],            // -bye,
  ["E4", 1], ["D4", 1], ["C4", 1],     // good-
  ["C4", 3],                           // -night (home)
].map(([n, b]) => [snap(m(n)), b]);

// ── melodic operators (arrays of [midi, beats]) ─────────────────────────────
// transpose a whole cell by an interval, keeping it in the mode.
function tpose(c, semis) { return c.map(([midi, beats]) => [snap(midi + semis), beats]); }
// invert a cell around its first note (mirror the intervals), folded to mode.
function invert(c) {
  const axis = c[0][0];
  return c.map(([midi, beats]) => [snap(axis - (midi - axis)), beats]);
}
// retrograde — play the cell backwards.
function retro(c) { return [...c].reverse(); }

// ORNAMENT — fill the line in, music-box style: between two melody notes insert
// a soft passing/neighbor tone, and split longer notes into a gentle pair.
// Returns a new, denser cell with the same overall contour & length.
function ornament(c) {
  const out = [];
  for (let i = 0; i < c.length; i++) {
    const [midi, beats] = c[i];
    const next = c[i + 1];
    if (beats >= 1 && next) {
      // split the note: first half stays, second half steps toward the next.
      const dir = Math.sign(next[0] - midi) || (i % 2 ? 1 : -1);
      const passing = snap(midi + dir * 2); // a scale step toward the target
      const half = beats / 2;
      out.push([midi, half]);
      out.push([passing, half]);
    } else if (beats >= 3) {
      // a long held note becomes note + upper-neighbor + return (a turn).
      const t = beats / 3;
      out.push([midi, t]);
      out.push([snap(midi + 2), t]);
      out.push([midi, t]);
    } else {
      out.push([midi, beats]);
    }
  }
  return out;
}

// ── layering helpers ────────────────────────────────────────────────────────
// lay a cell into events starting at a bar/beat with a single voice.
function lay(out, c, { startBar, beat0 = 0, voice = LEAD, gain = 0.5, decayMul = 1.7, pan = 0 } = {}) {
  let beat = beat0;
  for (const [midi, beats] of c) {
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

// soft bass root under a bar span (kept low + clear, the cradle's rock).
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

// a single soft high sparkle (glock/kalimba) — a music-box glint.
function glint(out, { startBar, beat, name, beats = 1.5, voice = "glockenspiel", gain = 0.11, pan = 0.3 }) {
  out.push({
    preset: voice,
    startSec: startBar * BAR + beat * BEAT,
    midi: snap(m(name)),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[voice] ?? 1) * 1.7,
    pan,
  });
}

// vibraphone_off pad (C-major triad voicing) breathing under a span of bars.
const padName = (deg, oct) => ["C", "D", "E", "F", "G", "A", "B"][deg] + oct;
function pad(out, startBar, lenBars, triad, baseOct, gain = 0.12) {
  for (let i = 0; i < triad.length; i++) {
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: snap(m(padName(triad[i], baseOct))),
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.0,
      pan: i === 0 ? -0.2 : i === 1 ? 0.0 : 0.2,
    });
  }
}

const events = [];

// ════════════════════════════════════════════════════════════════════════
//  THEME & VARIATIONS on the hushabye cadence.
//  Each pass is one statement of HUSH → ROCK → CADENCE (≈ 9 bars), dressed
//  differently. The bass rocks I–IV–V–I underneath throughout.
// ════════════════════════════════════════════════════════════════════════

// cradle bass: a gentle I–vi–IV–V–I rock under one ~9-bar statement.
function cradleBass(startBar, gain = 0.4) {
  bass(events, startBar + 0, "C2", 1, gain);
  bass(events, startBar + 1, "A2", 1, gain * 0.95);
  bass(events, startBar + 2, "F2", 1, gain);
  bass(events, startBar + 3, "C2", 1, gain * 0.95);
  bass(events, startBar + 4, "G2", 1, gain);
  bass(events, startBar + 5, "C2", 1, gain * 0.95);
  bass(events, startBar + 6, "F2", 1, gain);
  bass(events, startBar + 7, "G2", 1, gain);
  bass(events, startBar + 8, "C2", 1, gain);
}

// lay one full statement (theme melody) at startBar with a chosen voice/gain.
// HUSH = 4 cells / 6 beats (2 bars), ROCK = 4 bars, CADENCE = 3 bars → 9 bars.
function statement(c1, c2, c3, { startBar, voice = LEAD, gain = 0.5, decayMul = 1.7, pan = 0 }) {
  // HUSH occupies bars 0–1 (6 beats), ROCK bars 2–5 (12 beats? it's 4 bars),
  // CADENCE bars 6–8 (9 beats). Lay them sequentially in beats from startBar.
  let beat = lay(events, c1, { startBar, beat0: 0, voice, gain, decayMul, pan });
  beat = lay(events, c2, { startBar, beat0: beat, voice, gain, decayMul, pan });
  lay(events, c3, { startBar, beat0: beat, voice, gain, decayMul, pan });
}

const STMT = 9; // bars per statement

// ── THEME (bars 0–8): the bare hushabye cadence. One rosewood voice, lots of
//    air, a quiet bass rock, the faintest pad. Just the tune. ───────────────
{
  const b = 0;
  statement(HUSH, ROCK, CADENCE, { startBar: b, gain: 0.5, decayMul: 1.8 });
  cradleBass(b, 0.4);
  pad(events, b, 9, [0, 2, 4], 3, 0.10); // soft C pad bed
}

// ── VAR I — DECORATED (bars 9–17): the same tune, ornamented like a music
//    box. Passing tones and turns fill the line; a glock adds a glint or two.
//    The contour and cadence stay identical — clearly "the same tune again." ─
{
  const b = STMT * 1;
  statement(ornament(HUSH), ornament(ROCK), ornament(CADENCE), {
    startBar: b, gain: 0.46, decayMul: 1.7,
  });
  cradleBass(b, 0.38);
  pad(events, b, 9, [0, 2, 4], 3, 0.10);
  // a couple of high music-box glints answering the phrase ends.
  glint(events, { startBar: b + 1, beat: 2.2, name: "C6", gain: 0.10, pan: 0.32 });
  glint(events, { startBar: b + 5, beat: 1.0, name: "G5", gain: 0.10, pan: -0.26, voice: "kalimba" });
  glint(events, { startBar: b + 8, beat: 0.5, name: "E6", beats: 2, gain: 0.09, pan: 0.3 });
}

// ── VAR II — BASS TAKES THE TUNE (bars 18–26): the melody migrates DOWN an
//    octave into the bass marimba; a soft glock keeps the high frame (the
//    hush sigh up top) so the tune is "held" between registers. Tender, deep. ─
{
  const b = STMT * 2;
  // the tune, down an octave, in the bass voice.
  statement(tpose(HUSH, -12), tpose(ROCK, -12), tpose(CADENCE, -12), {
    startBar: b, voice: "bass", gain: 0.34, decayMul: 1.6, pan: 0,
  });
  // a thin rosewood high counter-line: the hush sigh up an octave, sparse.
  lay(events, tpose(HUSH, 12), { startBar: b, beat0: 0, voice: LEAD, gain: 0.22, decayMul: 1.9, pan: 0.18 });
  // glock keeps a high frame at the cadence so the descent is "answered" up top.
  glint(events, { startBar: b + 6, beat: 0, name: "E6", beats: 3, gain: 0.10, pan: 0.3 });
  glint(events, { startBar: b + 7, beat: 1.5, name: "D6", beats: 1.5, gain: 0.09, pan: 0.32 });
  glint(events, { startBar: b + 8, beat: 0, name: "C6", beats: 3, gain: 0.10, pan: 0.28 });
  pad(events, b, 9, [0, 2, 4], 3, 0.11);
  // bass cradle moves up an octave so it doesn't clash with the bass melody.
  bass(events, b + 0, "C3", 1, 0.30); bass(events, b + 2, "F3", 1, 0.30);
  bass(events, b + 4, "G3", 1, 0.30); bass(events, b + 6, "F3", 1, 0.30);
  bass(events, b + 8, "C3", 1, 0.30);
}

// ── VAR III — SPARKLE FINALE (bars 27–35): the brightest, most awake pass.
//    Rosewood states the tune; a glockenspiel CANON answers it a bar later an
//    octave up — a music box catching the light. Kalimba droplets sparkle. ───
{
  const b = STMT * 3;
  // lead voice, a touch brighter.
  statement(HUSH, ROCK, CADENCE, { startBar: b, gain: 0.48, decayMul: 1.6, pan: -0.12 });
  // glockenspiel canon: the SAME tune, one bar later, an octave up, softer.
  statement(tpose(HUSH, 12), tpose(ROCK, 12), tpose(CADENCE, 12), {
    startBar: b + 1, voice: "glockenspiel", gain: 0.13, decayMul: 1.7, pan: 0.3,
  });
  cradleBass(b, 0.38);
  pad(events, b, 9, [0, 2, 4], 3, 0.10);
  // kalimba droplets sprinkled between phrases.
  const drops = [
    [b + 2, 2.0, "C6", -0.26], [b + 4, 2.5, "E6", 0.3],
    [b + 6, 1.0, "G6", 0.34], [b + 7, 2.0, "C6", -0.24],
  ];
  for (const [bar, bt, n, pn] of drops) {
    glint(events, { startBar: bar, beat: bt, name: n, beats: 1.5, voice: "kalimba", gain: 0.10, pan: pn });
  }
}

// ── VAR IV — HUSHED FINAL STATEMENT (bars 36–45): the plainest pass of all,
//    even barer than the theme. Only HUSH and CADENCE (the ROCK middle is
//    dropped — the child is nearly asleep), stretched and quiet, settling onto
//    a long held C. The recognizable thread, said once more, then goodnight. ─
{
  const b = STMT * 4;
  // hush sigh, very soft, the home opener one last time.
  let beat = lay(events, HUSH, { startBar: b, beat0: 0, voice: LEAD, gain: 0.40, decayMul: 2.0 });
  // skip ROCK — go straight to the cadence, the goodnight.
  lay(events, CADENCE, { startBar: b, beat0: beat, voice: LEAD, gain: 0.38, decayMul: 2.1 });
  // a final, very faint glock echo of the 3-2-1 up top.
  glint(events, { startBar: b + 4, beat: 0, name: "E6", beats: 3, gain: 0.08, pan: 0.3 });
  glint(events, { startBar: b + 4, beat: 1.5, name: "D6", beats: 2, gain: 0.07, pan: 0.32 });
  glint(events, { startBar: b + 5, beat: 0, name: "C6", beats: 4, gain: 0.08, pan: 0.28 });
  // the long home cadence in the bass, settling to C.
  bass(events, b + 0, "C2", 2, 0.34);
  bass(events, b + 2, "G2", 2, 0.32);
  bass(events, b + 4, "F2", 1, 0.30);
  bass(events, b + 5, "C2", 3, 0.34); // home, held
  pad(events, b, 8, [0, 2, 4], 3, 0.12); // long resolving C pad
  // one last extra-low, extra-quiet C to put the room to sleep.
  bass(events, b + 6, "C2", 3, 0.26);
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "hushabye",
  here: HERE,
  title: "hushabye",
  reverb: { wet: 0.34, decay: 0.85, damp: 0.4 }, // warm, intimate nursery room
  fadeIn: 1.2,
  fadeOut: 5.0,
  tailSec: 5.5,
  peak: 0.84,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
