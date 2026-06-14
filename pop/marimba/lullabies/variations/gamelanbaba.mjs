// gamelanbaba.mjs — INTERLOCKING KOTEKAN development of the marimbaba seed.
//
// IDENTITY (kept): F whole-tone shimmer {0,2,4,6,8,10}, no leading tone, a
// floating slendro-tinted haze; bronze `gamelan` + `glockenspiel` halo; ~54 BPM;
// the long F bass anchors the dream; the held "wow" gong-wobble is still the
// metallic heart and the cadence still settles to F.
//
// DEVELOPMENT — kotekan (polos + sangsih). The marimbaba contour is reborn as
// ONE fast composite line that NO single voice plays: a bronze `gamelan` part
// (polos) plays one subset of the pulse and a `glockenspiel` part (sangsih)
// plays the complementary off-pulse, the two imbricated notes braiding into a
// glittering machine. Across the piece the interlock DENSIFIES (quarter →
// eighth → triplet → sixteenth nyog-cag) and the FIGURATION SHIFTS (which voice
// takes the on/off beats flips; the cell inverts and is sequenced up the
// whole-tone ladder), building simple statement → escalating glitter machine →
// resolution back to the slow wow-gong haze.
//
// Run:  node variations/gamelanbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 54;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;

// ── F whole-tone: root pitch-class F (5), scale {0,2,4,6,8,10} ───────────────
const ROOT_PC = m("F4") % 12; // 5
const WHOLETONE = [0, 2, 4, 6, 8, 10];

// Fold a midi note to the nearest whole-tone pitch (ties resolve upward → bright).
function snap(midi, rootPc = ROOT_PC, scale = WHOLETONE) {
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

// ── whole-tone ladder helpers (everything lives on the 6-step grid) ──────────
// Build the absolute midi for a ladder index relative to F4 (=65).
// Index 0 = F4; each step is one whole tone; 6 steps = an octave.
const F4 = m("F4"); // 65
function lad(idx) {
  const oct = Math.floor(idx / 6);
  const deg = ((idx % 6) + 6) % 6;
  return F4 + WHOLETONE[deg] + 12 * oct;
}
// invert a ladder index around a pivot (mirror the contour on the same grid).
const invert = (idx, pivot) => 2 * pivot - idx;

const events = [];
const push = (e) => events.push(e);

// per-voice ring + warmth tuned for bronze haze.
const GMUL = 0.9;

// ════════════════════════════════════════════════════════════════════════════
// KOTEKAN ENGINE
// Given a melodic CONTOUR as ladder indices (one per composite pulse), split it
// into polos (on-pulse) + sangsih (off-pulse) by alternating which physical
// voice strikes which pulse. `flip` swaps who takes the on/off beats. The two
// voices together sound every pulse — the interlock — but each alone is gapped.
// ════════════════════════════════════════════════════════════════════════════
function kotekan(contour, opts) {
  const {
    startBar, pulse,          // pulse = beats per composite note (density)
    polos = "gamelan", sangsih = "glockenspiel",
    polosOct = 0, sangsihOct = 1,
    gainP = 0.32, gainS = 0.22,
    flip = false,             // swap which voice gets even/odd pulses
    panP = -0.18, panS = 0.3,
    ring = 1.35,              // duration as a multiple of pulse (overlap = haze)
    pivot = null,             // if set, sangsih mirrors polos around this index
  } = opts;
  let t = startBar * BAR;
  contour.forEach((idx, i) => {
    const onPolos = flip ? (i % 2 === 1) : (i % 2 === 0);
    const voice = onPolos ? polos : sangsih;
    const oct = onPolos ? polosOct : sangsihOct;
    const gain = (onPolos ? gainP : gainS) * GMUL;
    const pan = onPolos ? panP : panS;
    // sangsih can take the mirror pitch (kotekan parts often answer in mirror).
    let useIdx = idx;
    if (!onPolos && pivot != null) useIdx = invert(idx, pivot);
    push({
      preset: voice,
      startSec: t,
      midi: lad(useIdx) + 12 * oct,
      durSec: pulse * BEAT * ring,
      gain,
      decayMul: (DECAY[voice] ?? 1.3) * 1.5,
      pan,
    });
    t += pulse * BEAT;
  });
  return t / BAR; // end bar (float)
}

// ── the marimbaba "thread" rendered as a ladder contour ──────────────────────
// hush sigh C5 A4 F4 → ladder ~ [ +? ]. Map the seed pitches onto the grid so
// the contour is RECOGNIZABLE (descending sigh, climbing twinkle, the cadence).
// We compute ladder indices by snapping the seed midi then measuring grid steps.
function toLad(noteName) {
  const sn = snap(m(noteName));
  // nearest ladder index to sn
  let best = 0, bestD = Infinity;
  for (let k = -12; k <= 18; k++) {
    const d = Math.abs(lad(k) - sn);
    if (d < bestD) { bestD = d; best = k; }
  }
  return best;
}

// Seed contours (ladder indices) drawn from the marimbaba motifs ----------------
const HUSH = ["C5", "A4", "F4", "G4", "F4"].map(toLad);            // descending sigh
const TWINKLE = ["F5", "A5", "C6", "A5", "G5", "F5", "G5", "A5"].map(toLad); // climbing wave
const BABA = ["A5", "G5", "A5", "F5", "C6", "Bb5", "C6", "A5"].map(toLad);   // slinky
const SLEEP = ["C5", "A4", "G4", "F4", "F4"].map(toLad);          // settle

// expand a short contour to a steady stream of `n` pulses by looping + walking
function stream(seed, n) {
  const out = [];
  for (let i = 0; i < n; i++) out.push(seed[i % seed.length]);
  return out;
}
// sequence a contour up the ladder by `step` whole-tones each repeat
function sequence(seed, reps, step) {
  const out = [];
  for (let r = 0; r < reps; r++) for (const x of seed) out.push(x + step * r);
  return out;
}

// ════════════════════════════════════════════════════════════════════════════
// FORM — an arc of densifying, shifting interlock.
// ════════════════════════════════════════════════════════════════════════════

// helper: a low bronze gong anchor under a region (keeps the dream grounded)
function gong(bar, note, beats, gain = 0.34, pan = 0) {
  push({
    preset: "gamelan",
    startSec: bar * BAR,
    midi: snap(m(note)),
    durSec: beats * BEAT,
    gain: gain * GMUL,
    decayMul: (DECAY.gamelan ?? 1.3) * 2.0,
    pan,
  });
}
// helper: whole-tone bronze pad (vibraphone_off), wide + slow, under a region.
function pad(bar, notes, beats, gain = 0.12, pan = -0.22) {
  for (const n of notes) {
    push({
      preset: "vibraphone_off",
      startSec: bar * BAR,
      midi: snap(m(n)),
      durSec: beats * BEAT,
      gain: gain * GMUL,
      decayMul: (DECAY.vibraphone_off ?? 1.4) * 1.6,
      pan,
    });
  }
}

// ── SECTION A (bars 0-5): SPARSE STATEMENT ───────────────────────────────────
// quarter-pulse kotekan of the hush sigh — slow, just the two voices kissing
// the on/off beats so you HEAR the interlock being born. Bronze pad bed below.
pad(0, ["F4", "A4", "B4"], 18, 0.12);
gong(0, "F2", 18, 0.3);
kotekan(stream(HUSH, 18), {
  startBar: 0, pulse: 1.0,        // quarter-note interlock (slow)
  gainP: 0.30, gainS: 0.18, ring: 1.6,
});

// ── SECTION B (bars 6-11): EIGHTH-NOTE INTERLOCK, twinkle climbs ─────────────
// density doubles to eighth-pulses; the climbing twinkle contour is sequenced
// UP the whole-tone ladder (+1 step each pass) so the machine spirals brighter.
pad(6, ["F4", "A4", "C5", "E5"], 18, 0.13, 0.2);
gong(6, "F3", 18, 0.26);
gong(9, "A2", 9, 0.22, -0.2);
{
  const twk = sequence(TWINKLE, 3, 1); // climb the ladder, anhemitonic spiral
  kotekan(twk, {
    startBar: 6, pulse: 0.5,      // eighth-note interlock
    gainP: 0.28, gainS: 0.2, ring: 1.4,
    panP: -0.22, panS: 0.34,
  });
}

// ── SECTION C (bars 12-15): THE WOW GONG-WOBBLE returns (slow center) ────────
// breathe: foreground the held "wow" wobble as slow bronze dyads — the metallic
// heart — answered by a glockenspiel mirror a beat behind (call/response).
const WOW = MOTIFS.wow; // [G5,Bb5,A5,G5,A5,F5]
{
  let bar = 12, beat = 0;
  for (const [note, beats] of WOW) {
    const lead = snap(m(note));
    push({
      preset: "gamelan", startSec: bar * BAR + beat * BEAT,
      midi: lead, durSec: beats * 1.3 * BEAT, gain: 0.4 * GMUL,
      decayMul: (DECAY.gamelan ?? 1.3) * 1.8, pan: -0.1,
    });
    // augmented-color bronze third below
    push({
      preset: "gamelan", startSec: bar * BAR + beat * BEAT,
      midi: lead - 4, durSec: beats * 1.3 * BEAT, gain: 0.22 * GMUL,
      decayMul: (DECAY.gamelan ?? 1.3) * 1.8, pan: -0.1,
    });
    // glockenspiel mirror answer, a hair behind, octave up
    push({
      preset: "glockenspiel", startSec: bar * BAR + beat * BEAT + 0.5 * BEAT,
      midi: lead + 12, durSec: beats * 1.1 * BEAT, gain: 0.18 * GMUL,
      decayMul: 2.0, pan: 0.34,
    });
    beat += beats;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
  gong(12, "F2", 12, 0.3);
}

// ── SECTION D (bars 16-21): NYOG-CAG MACHINE — triplet → sixteenth glitter ───
// the climax: the interlock densifies to triplets then sixteenths and the
// FIGURATION FLIPS (sangsih now takes the on-beat). The baba slinky contour is
// inverted (mirror) and braided so the composite line is a fast glittering line
// neither bronze nor bell plays alone.
pad(16, ["G4", "B4", "D5"], 18, 0.12, 0.22);
gong(16, "Eb2", 9, 0.28);
gong(18, "C3", 9, 0.26, 0.18);
gong(20, "F2", 9, 0.3);
{
  // triplet braid of the baba contour (bars 16-17)
  const baba3 = stream(BABA, 18);
  kotekan(baba3, {
    startBar: 16, pulse: 1 / 3,    // triplet eighths
    gainP: 0.24, gainS: 0.18, ring: 1.2,
    flip: true,                    // figuration shift: bell takes the on-beat
    pivot: BABA[0] + 4,            // sangsih answers in whole-tone mirror
  });
}
{
  // sixteenth nyog-cag glitter, inverted + sequenced (bars 18-21) — the machine
  const pivotIdx = BABA[0] + 2;
  const invBaba = BABA.map((x) => invert(x, pivotIdx));
  const climax = sequence(invBaba, 4, -1).concat(sequence(BABA, 4, 1));
  kotekan(climax, {
    startBar: 18, pulse: 0.25,     // sixteenth-note interlock — full glitter
    gainP: 0.2, gainS: 0.17, ring: 1.1,
    polosOct: 0, sangsihOct: 1,
    panP: -0.26, panS: 0.34,
  });
}

// ── SECTION E (bars 22-27): RESOLUTION — interlock thins back to a sigh ───────
// the machine decelerates: eighth → quarter → half, the SLEEP settle contour
// folding the two voices back into single tones, returning to the slow haze.
pad(22, ["F4", "A4", "C5"], 18, 0.13);
gong(22, "F2", 12, 0.3);
{
  // eighth, then quarter, then the cadence — a written ritardando in density
  kotekan(stream(SLEEP, 8), { startBar: 22, pulse: 0.5, gainP: 0.24, gainS: 0.18, ring: 1.5 });
  kotekan(stream(SLEEP, 6), { startBar: 24, pulse: 1.0, gainP: 0.26, gainS: 0.18, ring: 1.6 });
}

// final cadence: a low bronze gong tonic + a far high wind-bell shimmer.
gong(26, "F2", 8, 0.36);
push({
  preset: "glockenspiel",
  startSec: 26 * BAR + 0.5 * BEAT,
  midi: snap(m("F6")),
  durSec: 5 * BEAT, gain: 0.16 * GMUL, decayMul: 2.4, pan: 0.3,
});
// a last polos/sangsih kiss on the tonic — the machine exhaling.
push({ preset: "gamelan", startSec: 26 * BAR, midi: snap(m("F4")), durSec: 6 * BEAT, gain: 0.3 * GMUL, decayMul: 2.2, pan: -0.15 });
push({ preset: "glockenspiel", startSec: 26 * BAR + 0.75 * BEAT, midi: snap(m("C6")), durSec: 4 * BEAT, gain: 0.14 * GMUL, decayMul: 2.4, pan: 0.3 });

const { mp3, durationSec } = renderLullaby(events, {
  name: "gamelanbaba",
  here: HERE,
  title: "gamelanbaba",
  reverb: { wet: 0.42, decay: 0.88, damp: 0.3 }, // long glassy bronze room
  fadeIn: 1.2,
  fadeOut: 5.0,
  tailSec: 6.0,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
