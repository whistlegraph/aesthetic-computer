#!/usr/bin/env node
// render-nullabata.mjs — the sonata cut: nullaby + sonata.
//
// Four movements run ATTACCA (no silence between them), bound by one
// lullaby motif transformed in each:
//
//   I.   Andante   C maj    92 BPM   the theme, lyrical whistle + warm pads
//   II.  Scherzo   A min   152 BPM   theme shattered into a fast arp/twinkle cloud
//   III. Adagio    F maj    72 BPM   theme sung slow over sustained pads + drones
//   IV.  Finale    C maj   100 BPM   theme recapitulated: 2→bloom→2 accumulation
//
// Same trick as render-nullabye.mjs / render-nuellaby.mjs (after Andy
// Brewer's "this song has no instruments in it",
// https://youtu.be/_Rk-hmIMv6I): two copies of pink noise, one
// phase-inverted — perfect silence — and every note is a peaking EQ bell
// breaking the cancellation. No oscillator anywhere in this file.
//
// CLARITY IS A Q DECISION: a narrow (high-Q) bell on noise rings like a
// breathy tone; a wide (low-Q) bell passes a wide band of raw noise (hiss).
// This cut runs the engine at its TONAL extreme — high Q on every pitched
// voice, no wide "veil" wash, percussion kept narrow — so it reads as a
// pitched choir, not radio static.
//
// LANES (from nuellaby): a lane is one persistent EQ point; consecutive
// notes on a lane reuse it monophonically (the bell drags to the new
// frequency, clipping the old ring). The EQ chain is SERIAL — two bells
// at the same frequency ADD their dB — so every layer here owns disjoint
// frequencies. The spectrum is carved into the usual zones:
//   SUB 40–90 (kick, bass) · BASS 90–250 (drones) · LOW-MID 250–500 (pads)
//   MID 500–1k (lead, harmony, choir) · UPPER-MID 1–2.5k (octave lead, arps)
//   PRESENCE 2.5–5k (sparkles) · AIR 5–10k (narrow tone ticks)
//
// Run:
//   node pop/nullabye/bin/render-nullabata.mjs                 # → out/nullabata.mp3
//   node pop/nullabye/bin/render-nullabata.mjs --proof         # flat ⇒ bit-exact silence
//   node pop/nullabye/bin/render-nullabata.mjs --bake out.txt  # score for the C engine

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const BLOCK = 64;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const PROOF = process.argv.includes("--proof");

// ── the four movements ────────────────────────────────────────────────
// each has its own tempo and bar count; a movement's local clock is
// mt(mov, bar, beat). movements run attacca — no silence between them.
const GAP = 0;
const MOVES = [
  { id: "andante", title: "I. Andante",  bpm: 92,  bars: 12 }, // ~31 s
  { id: "scherzo", title: "II. Scherzo", bpm: 152, bars: 28 }, // ~44 s
  { id: "adagio",  title: "III. Adagio", bpm: 72,  bars: 10 }, // ~33 s
  { id: "finale",  title: "IV. Finale",  bpm: 100, bars: 24 }, // ~58 s
];
{
  let cursor = 0;
  for (const m of MOVES) {
    m.beat = 60 / m.bpm;
    m.bar = 4 * m.beat;
    m.dur = m.bars * m.bar;
    m.startSec = cursor;
    m.endSec = cursor + m.dur;
    cursor = m.endSec + GAP;
  }
}
const TAIL = 1.6; // let the finale's fade breathe
const totalSec = PROOF ? 6 : MOVES[MOVES.length - 1].endSec + TAIL;
const ns = Math.ceil(totalSec * SR);
const MOV = Object.fromEntries(MOVES.map((m) => [m.id, m]));

// absolute seconds for a (movement, bar, beat)
const mt = (m, bar, beat = 0) => m.startSec + bar * m.bar + beat * m.beat;

// ── note table (Hz) ───────────────────────────────────────────────────
const HZ = {
  A1: 55.0, Bb1: 58.27, C2: 65.41, D2: 73.42, F2: 87.31, G2: 98.0,
  A2: 110.0, Bb2: 116.54, C3: 130.81, D3: 146.83, F3: 174.61, G3: 196.0,
  A3: 220.0, Bb3: 233.08, B3: 246.94,
  C4: 261.63, D4: 293.66, E4: 329.63, F4: 349.23, G4: 392.0, A4: 440.0, Bb4: 466.16, B4: 493.88,
  C5: 523.25, D5: 587.33, E5: 659.26, F5: 698.46, G5: 783.99, A5: 880.0, B5: 987.77,
  C6: 1046.5, D6: 1174.66, E6: 1318.51, F6: 1396.91, G6: 1567.98, A6: 1760.0,
  C7: 2093.0, D7: 2349.32, E7: 2637.02,
};

// ── the lullaby motif — the sonata's binding idea ─────────────────────
// semitone offsets from a movement's "voice tonic" (the register the lead
// whistle sings in), laid over a 2-bar (8-beat) phrase. Every movement
// states a transformation of this same head: lyrical, shattered, stretched,
// recapitulated. transposing it is one multiply (tonic · 2^(semi/12)).
const THEME = [
  { semi: 4, beat: 0,   dur: 1.5 }, // 3rd
  { semi: 7, beat: 1.5, dur: 0.5 }, // 5th
  { semi: 9, beat: 2,   dur: 2 },   // 6th — the reach
  { semi: 7, beat: 4,   dur: 1 },   // 5th
  { semi: 4, beat: 5,   dur: 1 },   // 3rd
  { semi: 2, beat: 6,   dur: 2 },   // 2nd — settle
];
const semi = (tonic, s) => tonic * Math.pow(2, s / 12);
// diatonic third below, per motif semitone (C-major degrees) — keyed on the
// semitone offset, not the Hz value (computed Hz won't byte-match a table)
const THIRD3 = { 2: -1, 4: 0, 7: 4, 9: 5 };

// ── lanes: persistent monophonic EQ points ────────────────────────────
const lanes = new Map(); // name → { events: [] }

function note(lane, t0, { freq, freqEnd = null, peakDb, q, atk, hold, rel }) {
  if (PROOF) peakDb = 0;
  let L = lanes.get(lane);
  if (!L) { L = { events: [] }; lanes.set(lane, L); }
  const prev = L.events[L.events.length - 1];
  if (prev && prev.end > t0) { prev.cut = t0; prev.end = t0; } // mono: clip the old ring
  const end = t0 + atk + hold + rel;
  L.events.push({ t0, freq, freqEnd, peakDb, q, atk, hold, rel, end, cut: null });
}

function envOf(e, time) {
  const dt = time - e.t0;
  if (dt < 0 || dt >= e.atk + e.hold + e.rel) return 0;
  let v;
  if (dt < e.atk) v = dt / e.atk;
  else if (dt < e.atk + e.hold) v = 1;
  else v = 1 - (dt - e.atk - e.hold) / e.rel;
  if (e.cut !== null) v *= Math.min(1, Math.max(0, (e.cut - time) / 0.03));
  return v;
}

// shared articulation helpers. KEY TIMBRE FACT: one bell on noise is always
// a bit breathy (it IS filtered noise), and LOW Q widens the band → MORE
// air, not body. So we keep Q HIGH (narrow = tonal, minimal air) and build
// body from HARMONIC STACKS instead — a fundamental plus partials reads as a
// pitched instrument, full but not airy.
const lead = (m, lane, bar, beat, freq, beats, db = 26, q = 130) =>
  note(lane, mt(m, bar, beat), { freq, peakDb: db, q, atk: 0.06, hold: Math.max(0.04, beats * m.beat - 0.06), rel: 0.45 });
const pad = (m, lane, bar, freq, db = 19, q = 90) => {
  note(lane, mt(m, bar), { freq, peakDb: db, q, atk: 0.8, hold: m.bar - 2.0, rel: 1.2 });
  // octave partial = body without air
  note(`${lane}o`, mt(m, bar), { freq: freq * 2, peakDb: db - 9, q: q * 1.4, atk: 0.85, hold: m.bar - 2.1, rel: 1.2 });
};

// a VOICE = a harmonic stack of high-Q bells (fundamental + 3 partials with
// a natural rolloff). this is where the melody's body comes from.
const VOICE_PARTS = [
  { mult: 1, dbRel: 0, q: 140 },
  { mult: 2, dbRel: -5, q: 165 },
  { mult: 3, dbRel: -10, q: 190 },
  { mult: 4, dbRel: -15, q: 215 },
];
function leadVoice(m, lane, bar, beat, freq, beats, db = 26) {
  for (const pt of VOICE_PARTS)
    if (freq * pt.mult < 15000)
      lead(m, `${lane}_p${pt.mult}`, bar, beat, freq * pt.mult, beats, db + pt.dbRel, pt.q);
}

// state the motif on a lane at (movement, barBase, voiceTonic)
function statTheme(m, lane, barBase, voiceTonic, { db = 26, artic = 1, oct = 1 } = {}) {
  for (const n of THEME) {
    const f = semi(voiceTonic, n.semi) * oct;
    leadVoice(m, lane, barBase + Math.floor(n.beat / 4), n.beat % 4, f, n.dur * artic, db);
  }
}

// chord = root frequency (mid register) + interval set; bass an octave or two below
const chord = (rootMid, ivals, bassHz, ninth) => ({
  tones: ivals.map((s) => semi(rootMid, s)),
  ninth: ninth ?? semi(rootMid, 14),
  root: bassHz,
});
const MAJ = [0, 4, 7], MIN = [0, 3, 7];

// tiny deterministic LCG for the twinkle clouds (no Math.random — resume-safe)
const lcg = (seed) => { let s = seed >>> 0; return () => (s = (s * 1664525 + 1013904223) >>> 0) / 4294967296; };

// =======================================================================
// I. ANDANTE — C major. the theme stated plainly: whistle + warm pads,
// a heartbeat kick that wakes halfway, the gentlest hat ticks at the end.
// =======================================================================
function andante() {
  const m = MOV.andante;
  const PROG = [
    chord(HZ.C4, MAJ, HZ.C2), chord(HZ.A3, MIN, HZ.A1),
    chord(HZ.F3, MAJ, HZ.F2), chord(HZ.G3, MAJ, HZ.G2),
  ];
  for (let bar = 0; bar < m.bars; bar++) {
    const ch = PROG[bar % 4];
    // pads — low-mid, fade in over the first phrase
    const padDb = bar < 2 ? 15 : 20;
    pad(m, "padR", bar, ch.tones[0], padDb); pad(m, "pad5", bar, ch.tones[2], padDb);
    if (bar >= 4) pad(m, "pad3", bar, ch.tones[1], 18);
    if (bar >= 8) pad(m, "pad9", bar, ch.ninth, 16);
    // bass root, half notes once the texture settles
    if (bar < 4) note("bass", mt(m, bar), { freq: ch.root, peakDb: 24, q: 13, atk: 0.04, hold: m.bar - 0.6, rel: 0.4 });
    else [[0, 1], [2, 1.5]].forEach(([beat, mul]) =>
      note("bass", mt(m, bar, beat), { freq: ch.root * mul, peakDb: 25, q: 13, atk: 0.03, hold: 2 * m.beat - 0.2, rel: 0.3 }));
    // heartbeat kick from bar 6 — soft, in the noise sub-zone
    if (bar >= 6) for (const beat of [0, 2])
      note("kick", mt(m, bar, beat), { freq: 100, freqEnd: 48, peakDb: beat ? 24 : 27, q: 2.6, atk: 0.003, hold: 0.012, rel: 0.18 });
    // the gentlest tick in the last phrase — narrow + pitched, not a hiss
    if (bar >= 12) for (const beat of [1, 3])
      note("hat", mt(m, bar, beat), { freq: 6800, peakDb: 12, q: 9, atk: 0.003, hold: 0.01, rel: 0.06 });
  }
  // theme on its own 2-bar phrase grid, voice tonic C5
  for (let p = 0; p < m.bars / 2; p++)
    statTheme(m, "lead", p * 2, HZ.C5, { db: p === 0 ? 22 : 26 });
  // a quiet third-below harmony once the theme is established
  for (let p = 2; p < m.bars / 2; p++)
    for (const n of THEME)
      lead(m, "harm", p * 2 + Math.floor(n.beat / 4), n.beat % 4, semi(HZ.C5, THIRD3[n.semi]), n.dur, 18);
}

// =======================================================================
// II. SCHERZO — A minor, fast. the theme is shattered into staccato arp
// fragments and a twinkle cloud; a light kick on 1, driving eighth hats.
// playful, bittersweet (relative minor), highest register of the suite.
// =======================================================================
function scherzo() {
  const m = MOV.scherzo;
  const PROG = [
    chord(HZ.A3, MIN, HZ.A1), chord(HZ.F3, MAJ, HZ.F2),
    chord(HZ.C4, MAJ, HZ.C2), chord(HZ.G3, MAJ, HZ.G2),
  ];
  const arpOf = (ch) => [ch.tones[0], ch.tones[1], ch.tones[2], ch.tones[1]].map((f) => f * 2);
  const PENT = [HZ.A5, HZ.C6, HZ.D6, HZ.E6, HZ.G6];
  for (let bar = 0; bar < m.bars; bar++) {
    const ch = PROG[bar % 4];
    // pads, short and light (scherzo bars are tiny — fast attack/release);
    // octave partial for tone/body
    for (const [lane, f] of [["padR", ch.tones[0]], ["pad5", ch.tones[2]]]) {
      note(lane, mt(m, bar), { freq: f, peakDb: 17, q: 90, atk: 0.25, hold: m.bar - 0.7, rel: 0.45 });
      note(`${lane}o`, mt(m, bar), { freq: f * 2, peakDb: 9, q: 130, atk: 0.28, hold: m.bar - 0.75, rel: 0.45 });
    }
    // walking bass, two notes a bar
    [[0, 1], [2, 1.5]].forEach(([beat, mul]) =>
      note("bass", mt(m, bar, beat), { freq: ch.root * mul, peakDb: 25, q: 13, atk: 0.02, hold: 1.6 * m.beat, rel: 0.2 }));
    // light kick on 1 (and 3 once it's running)
    const kbeats = bar >= 4 ? [0, 2] : [0];
    for (const beat of kbeats)
      note("kick", mt(m, bar, beat), { freq: 105, freqEnd: 50, peakDb: beat ? 24 : 28, q: 3, atk: 0.003, hold: 0.01, rel: 0.13 });
    // light offbeat ticks — narrow + pitched, not a wash of hiss
    const hatBeats = bar >= 2 ? [0.5, 1.5, 2.5, 3.5] : [2];
    for (const beat of hatBeats)
      note("hat", mt(m, bar, beat), { freq: 7200, peakDb: 14, q: 9, atk: 0.002, hold: 0.008, rel: 0.05 });
    // the theme SHATTERED: every motif note becomes a fast up-down arp run
    const arp = arpOf(ch);
    for (const n of THEME) {
      const base = semi(HZ.A4, n.semi) * 2; // voice tonic A5 region
      const steps = [base, arp[1], arp[2], arp[3]];
      for (let k = 0; k < 4; k++) {
        const beat = (n.beat + k * 0.25) % 4;
        const barOff = bar + Math.floor((n.beat + k * 0.25) / 4);
        if (barOff >= m.bars) continue;
        note(`arp${k}`, mt(m, barOff, beat), { freq: steps[k], peakDb: 16, q: 150, atk: 0.012, hold: 0.07, rel: 0.18 });
        // octave + twelfth partials → tone, not a thin sine
        note(`arp${k}o`, mt(m, barOff, beat), { freq: steps[k] * 2, peakDb: 10, q: 180, atk: 0.012, hold: 0.06, rel: 0.16 });
      }
    }
    // twinkle cloud — accretes, presence/air register, A-minor pentatonic
    const cloudN = Math.min(10, 2 + Math.floor(bar / 2));
    for (let j = 0; j < cloudN; j++) {
      const rnd = lcg(j * 7919 + bar * 104729);
      const f = PENT[(j + bar) % PENT.length];
      const offs = [Math.floor(rnd() * 8) * 0.5, Math.floor(rnd() * 8) * 0.5].sort((a, z) => a - z);
      for (const beat of offs)
        note(`cloud${j}`, mt(m, bar, beat), { freq: f, peakDb: 11, q: 150, atk: 0.01, hold: 0.06, rel: 0.18 });
    }
  }
}

// =======================================================================
// III. ADAGIO — F major, very slow. the theme sung in long tones over a
// veil-bed drone and low choir partials. no percussion — pure breath.
// the warm, still heart of the sonata.
// =======================================================================
function adagio() {
  const m = MOV.adagio;
  const PROG = [
    chord(HZ.F3, MAJ, HZ.F2), chord(HZ.D3, MIN, HZ.D2),
    chord(HZ.Bb2, MAJ, HZ.Bb1), chord(HZ.C3, MAJ, HZ.C2),
  ];
  for (let bar = 0; bar < m.bars; bar++) {
    const ch = PROG[bar % 4];
    // sustained pads, very slow attack/release — the bars are long here.
    // higher Q than before: this movement used to live on a wide veil wash
    // (pure static); now the pads + drones carry it as tones
    // high-Q (tonal) fundamentals + octave partials = tone + body, no air
    for (const [lane, f, db] of [["padR", ch.tones[0], 21], ["pad5", ch.tones[2], 20], ["pad3", ch.tones[1], 18]]) {
      note(lane, mt(m, bar), { freq: f, peakDb: db, q: 90, atk: 1.4, hold: m.bar - 3.0, rel: 1.6 });
      note(`${lane}o`, mt(m, bar), { freq: f * 2, peakDb: db - 9, q: 130, atk: 1.5, hold: m.bar - 3.1, rel: 1.6 });
    }
    // low drone roots — root and fifth, sustained
    note("droneR", mt(m, bar), { freq: ch.root * 2, peakDb: 17, q: 90, atk: 1.0, hold: m.bar - 2.4, rel: 1.4 });
    note("droneF", mt(m, bar), { freq: ch.root * 3, peakDb: 14, q: 90, atk: 1.0, hold: m.bar - 2.4, rel: 1.4 });
    // choir — spice partials only (7,11,13): everything else is owned below
    [7, 11, 13].forEach((p, i) =>
      note(`choir${i}`, mt(m, bar), { freq: ch.root * p, peakDb: 8 - i * 0.5, q: 90, atk: 1.2, hold: m.bar - 3.0, rel: 1.6 }));
  }
  // theme in long tones, voice tonic F4 — each note stretched to ~a bar
  for (let p = 0; p < Math.floor(m.bars / 2); p++)
    for (const n of THEME)
      leadVoice(m, "lead", p * 2 + Math.floor(n.beat / 4), n.beat % 4,
        semi(HZ.F4, n.semi), n.dur * 1.4, p === 0 ? 22 : 25);
}

// =======================================================================
// IV. FINALE — C major. the theme recapitulated over a compact accumulation
// arch: the rack grows 2 → full → 2, blooms, then thins to the lone theme
// — nuellaby's Boléro logic, gathering the whole sonata home.
// =======================================================================
const FIN_ROSTER = [
  "lead", "bass", "padR", "pad5", "kick", "hat", "pad3", "pad9",
  "harm", "leadOct", "droneR", "droneF", "spark0", "spark1",
  "arp0", "arp1", "arp2",
];
{ // bloom groups (own disjoint frequencies; serial dB-add discipline)
  const groups = [
    Array.from({ length: 12 }, (_, j) => `halo${j}`),
    Array.from({ length: 10 }, (_, j) => `cloud${j}`),
    Array.from({ length: 4 }, (_, j) => `choir${j}`),
    Array.from({ length: 3 }, (_, j) => `airOct${j}`),
  ];
  let k = 0;
  while (groups.some((g) => g.length)) {
    if (groups[k].length) FIN_ROSTER.push(groups[k].shift());
    k = (k + 1) % groups.length;
  }
}
const FIN_NMAX = FIN_ROSTER.length;
const FIN_RANK = new Map(FIN_ROSTER.map((n, i) => [n, i]));

function finale() {
  const m = MOV.finale;
  const B = m.bars;
  // count curve, knees as fractions of B so the arch completes at any length:
  // hold 2 · accumulate to NMAX · bloom · shed back to 2 · lone theme to end
  const k1 = Math.max(1, B * 0.08), k2 = B * 0.55, k3 = B * 0.72, k4 = B * 0.93;
  const pointsAt = (bar) => {
    if (bar < k1) return 2;
    if (bar < k2) return 2 + ((bar - k1) / (k2 - k1)) * (FIN_NMAX - 2);
    if (bar < k3) return FIN_NMAX;
    if (bar < k4) return FIN_NMAX - ((bar - k3) / (k4 - k3)) * (FIN_NMAX - 2);
    return 2;
  };
  const on = (lane, bar) => { // roster gate: is this lane on the rack yet?
    const r = FIN_RANK.get(lane);
    return r === undefined || r < pointsAt(bar);
  };
  m._pointsAt = pointsAt; m._nmax = FIN_NMAX; // for the ride + log

  const PROG = [
    chord(HZ.C4, MAJ, HZ.C2), chord(HZ.A3, MIN, HZ.A1),
    chord(HZ.F3, MAJ, HZ.F2), chord(HZ.G3, MAJ, HZ.G2),
  ];
  const ARP = (ch) => [ch.tones[0] * 2, ch.tones[1] * 2, ch.tones[2] * 2];
  const HALO = [];
  for (const oct of [1, 2, 4]) for (const f of [HZ.C4, HZ.E4, HZ.G4, HZ.A4]) if (f * oct <= HZ.E7) HALO.push(f * oct);
  const PENT_HI = [HZ.C6, HZ.D6, HZ.E6, HZ.G6, HZ.A6, HZ.C7];

  for (let bar = 0; bar < B; bar++) {
    const ch = PROG[bar % 4];
    const n0 = pointsAt(bar);
    const frac = n0 / FIN_NMAX;
    // bass — root while sparse, walk once awake
    if (n0 < 8) note("bass", mt(m, bar), { freq: ch.root, peakDb: 26, q: 13, atk: 0.03, hold: m.bar - 0.6, rel: 0.4 });
    else [[0, 1], [1, 1.5], [2, 1], [3, 2]].forEach(([beat, mul]) =>
      note("bass", mt(m, bar, beat), { freq: ch.root * mul, peakDb: mul > 1.6 ? 20 : 26, q: 13, atk: 0.02, hold: 0.9 * m.beat, rel: 0.25 }));
    // pads
    if (on("padR", bar)) pad(m, "padR", bar, ch.tones[0]);
    if (on("pad5", bar)) pad(m, "pad5", bar, ch.tones[2]);
    if (on("pad3", bar)) pad(m, "pad3", bar, ch.tones[1]);
    if (on("pad9", bar)) pad(m, "pad9", bar, ch.ninth, 17);
    // kick
    if (on("kick", bar)) for (const beat of [0, 2])
      note("kick", mt(m, bar, beat), { freq: 110, freqEnd: 42, peakDb: beat ? 29 : 31, q: 2.2, atk: 0.003, hold: 0.012, rel: 0.18 });
    // ticks — narrow + pitched, not a wash of hiss
    if (on("hat", bar)) {
      const hatBeats = n0 >= FIN_NMAX * 0.4 ? [0.5, 1.5, 2.5, 3.5] : [1, 3];
      for (const beat of hatBeats)
        note("hat", mt(m, bar, beat), { freq: 7000, peakDb: 14, q: 9, atk: 0.002, hold: 0.008, rel: 0.05 });
    }
    // drones
    if (on("droneR", bar)) note("droneR", mt(m, bar), { freq: ch.root * 2, peakDb: 16, q: 90, atk: 0.5, hold: m.bar - 1.3, rel: 0.8 });
    if (on("droneF", bar)) note("droneF", mt(m, bar), { freq: ch.root * 3, peakDb: 13, q: 90, atk: 0.5, hold: m.bar - 1.3, rel: 0.8 });
    // glints
    const glint = [HZ.E6, HZ.G6, HZ.D6, HZ.E6][bar % 4];
    if (on(`spark${bar % 2}`, bar))
      note(`spark${bar % 2}`, mt(m, bar, bar % 2 ? 3.5 : 1.5), { freq: glint, peakDb: 18, q: 90, atk: 0.01, hold: 0.06, rel: 0.18 });
    // arpeggio — assembles note by note as its lanes join
    const arp = ARP(ch), steps = [0, 1, 2, 1, 0, 1, 2, 1];
    for (let k = 0; k < 8; k++)
      if (on(`arp${k % 3}`, bar)) {
        note(`arp${k % 3}`, mt(m, bar, k * 0.5), { freq: arp[steps[k]], peakDb: 15, q: 150, atk: 0.015, hold: 0.12, rel: 0.2 });
        note(`arp${k % 3}o`, mt(m, bar, k * 0.5), { freq: arp[steps[k]] * 2, peakDb: 9, q: 180, atk: 0.015, hold: 0.11, rel: 0.18 });
      }
    // air octave — chord tones two octaves up
    [...ch.tones.map((f) => f * 4)].forEach((f, i) => {
      if (on(`airOct${i}`, bar)) pad(m, `airOct${i}`, bar, f, 10, 30);
    });
    // choir — spice partials only
    [7, 11, 13, 14].forEach((p, i) => {
      if (on(`choir${i}`, bar)) note(`choir${i}`, mt(m, bar), { freq: ch.root * p, peakDb: 9 - i * 0.5, q: 90, atk: 0.7, hold: m.bar - 1.6, rel: 0.9 });
    });
    // halo — diatonic wash
    HALO.forEach((f, i) => { if (on(`halo${i}`, bar)) note(`halo${i}`, mt(m, bar), { freq: f, peakDb: 7.5, q: 70, atk: 0.9, hold: m.bar - 2.1, rel: 1.2 }); });
    // twinkle cloud
    for (let j = 0; j < 10; j++) {
      if (!on(`cloud${j}`, bar)) continue;
      const rnd = lcg(j * 7919 + bar * 104729);
      const f = PENT_HI[(j + bar) % PENT_HI.length];
      const offs = [Math.floor(rnd() * 8) * 0.5, Math.floor(rnd() * 8) * 0.5].sort((a, z) => a - z);
      for (const beat of offs)
        note(`cloud${j}`, mt(m, bar, beat), { freq: f, peakDb: 12, q: 90, atk: 0.012, hold: 0.08, rel: 0.22 });
    }
  }
  // the recapitulated theme — runs throughout, swelling at the bloom; an
  // octave double joins once leadOct is on the rack
  for (let p = 0; p < B / 2; p++) {
    const bloom = pointsAt(p * 2) > FIN_NMAX * 0.85;
    statTheme(m, "lead", p * 2, HZ.C5, { db: bloom ? 29 : 26 });
    if (on("harm", p * 2))
      for (const n of THEME)
        lead(m, "harm", p * 2 + Math.floor(n.beat / 4), n.beat % 4, semi(HZ.C5, THIRD3[n.semi]), n.dur, 20);
  }
}

// ── compose all four ───────────────────────────────────────────────────
andante();
scherzo();
adagio();
finale();

// ── the loudness-ride target: a per-second dBFS curve, movement-aware ───
// (each movement sits at its own level; quiet movements get mid/high
// content so K-weighting doesn't bury them. gaps target a low floor —
// they're literal zero anyway.) used by BOTH the JS ride and the C bake.
function rideTargetAt(timeSec) {
  for (const m of MOVES) {
    if (timeSec < m.startSec || timeSec >= m.endSec) continue;
    const f = (timeSec - m.startSec) / m.dur; // 0..1 within the movement
    switch (m.id) {
      case "andante": return -25.5 + 3.0 * Math.min(1, f * 2);          // swell in
      case "scherzo": return -21.0 - 1.5 * Math.cos(f * Math.PI);       // energetic, dips at edges
      case "adagio":  return -25.0 + 2.0 * Math.sin(f * Math.PI);       // quiet, arched
      case "finale": {
        const frac = (m._pointsAt ? m._pointsAt(f * m.bars) : 2) / (m._nmax || 1);
        return -26.0 + 8.0 * Math.sqrt(frac);                          // ride the count
      }
    }
  }
  return -52; // a gap: pull down (audio is silence here regardless)
}

// ── log the shape ───────────────────────────────────────────────────────
{
  console.log(`→ nullabata · sonata in four movements · ${(totalSec).toFixed(1)} s · ${lanes.size} lanes`);
  for (const m of MOVES)
    console.log(`   ${m.title.padEnd(12)} ${String(m.bpm).padStart(3)} BPM · ${m.bars} bars · ${m.startSec.toFixed(1)}–${m.endSec.toFixed(1)} s`);
}

// ── collect bands (time-sort per lane for the renderer) ──────────────────
const bands = [...lanes.values()];
for (const b of bands) b.events.sort((x, y) => x.t0 - y.t0);

// ── --bake: write the score for the C engine and exit ────────────────────
{
  const bakeIdx = process.argv.indexOf("--bake");
  if (bakeIdx >= 0) {
    const bakePath = process.argv[bakeIdx + 1] || resolve(HERE, "..", "out", "nullabata.score.txt");
    const L = [];
    L.push(`sr ${SR}`, `dur ${totalSec}`, `detune 1.0012 0.9988`, `seed ${0xBEEF} ${0xC0FFEE}`);
    L.push(`normpeak 0.9`, `fadein 0.004`, `fadeout ${TAIL}`);
    const RIDE_WIN = 1.0, RIDE_N = Math.ceil(totalSec);
    L.push(`ridewin ${RIDE_WIN} ${RIDE_N}`);
    for (let w = 0; w < RIDE_N; w++) L.push(String(rideTargetAt((w + 0.5) * RIDE_WIN).toFixed(3)));
    for (const band of bands) {
      L.push(`band ${band.events.length}`);
      for (const e of band.events)
        L.push([e.t0, e.freq, e.freqEnd ?? 0, e.peakDb, e.q, e.atk, e.hold, e.rel, e.cut ?? -1].join(" "));
    }
    mkdirSync(dirname(bakePath), { recursive: true });
    writeFileSync(bakePath, L.join("\n") + "\n");
    console.log(`✓ baked ${bands.length} bands → ${bakePath}`);
    process.exit(0);
  }
}

// ── the dual-noise null engine (identical to nuellaby) ───────────────────
function makePink(seed) {
  let s = seed >>> 0;
  const rnd = () => ((s = (s * 1664525 + 1013904223) >>> 0) / 4294967296) * 2 - 1;
  let b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0;
  return () => {
    const w = rnd();
    b0 = 0.99886 * b0 + w * 0.0555179;
    b1 = 0.99332 * b1 + w * 0.0750759;
    b2 = 0.969 * b2 + w * 0.153852;
    b3 = 0.8665 * b3 + w * 0.3104856;
    b4 = 0.55 * b4 + w * 0.5329522;
    b5 = -0.7616 * b5 - w * 0.016898;
    const out = (b0 + b1 + b2 + b3 + b4 + b5 + b6 + w * 0.5362) * 0.11;
    b6 = w * 0.115926;
    return out;
  };
}

const out = [new Float32Array(ns), new Float32Array(ns)];
const DETUNE = [1.0012, 0.9988];

for (let ch = 0; ch < 2; ch++) {
  const pink = makePink(ch ? 0xC0FFEE : 0xBEEF);
  const dry = new Float32Array(BLOCK);
  const wet = new Float32Array(BLOCK);
  const st = bands.map(() => ({ z1: 0, z2: 0, cursor: 0, b0: 1, b1: 0, b2: 0, a1: 0, a2: 0 }));

  for (let blockStart = 0; blockStart < ns; blockStart += BLOCK) {
    const n = Math.min(BLOCK, ns - blockStart);
    const time = blockStart / SR;
    for (let i = 0; i < n; i++) wet[i] = dry[i] = pink();

    for (let bi = 0; bi < bands.length; bi++) {
      const band = bands[bi], s = st[bi];
      while (s.cursor < band.events.length && time > band.events[s.cursor].end + 0.4) {
        s.cursor++; s.z1 = 0; s.z2 = 0;
      }
      const e = band.events[s.cursor];
      if (!e || time < e.t0 - 0.05) continue;

      const env = envOf(e, time);
      const db = e.peakDb * env;
      const ringing = Math.abs(s.z1) + Math.abs(s.z2) > 1e-9;
      if (db <= 0.01 && !ringing) continue;

      let freq = e.freq;
      if (e.freqEnd) {
        const p = Math.min(1, Math.max(0, (time - e.t0) / (e.end - e.t0)));
        freq = e.freq * Math.pow(e.freqEnd / e.freq, p);
      }
      const A = Math.pow(10, db / 40);
      const w0 = (2 * Math.PI * Math.min(freq * DETUNE[ch], SR * 0.45)) / SR;
      const alpha = Math.sin(w0) / (2 * e.q);
      const cosw = Math.cos(w0);
      const a0 = 1 + alpha / A;
      s.b0 = (1 + alpha * A) / a0;
      s.b1 = (-2 * cosw) / a0;
      s.b2 = (1 - alpha * A) / a0;
      s.a1 = (-2 * cosw) / a0;
      s.a2 = (1 - alpha / A) / a0;
      let { z1, z2 } = s;
      for (let i = 0; i < n; i++) {
        const x = wet[i];
        const y = s.b0 * x + z1;
        z1 = s.b1 * x - s.a1 * y + z2;
        z2 = s.b2 * x - s.a2 * y;
        wet[i] = y;
      }
      s.z1 = z1; s.z2 = z2;
    }

    const o = out[ch];
    for (let i = 0; i < n; i++) o[blockStart + i] = wet[i] - dry[i];
  }
}

if (PROOF) {
  let peak = 0;
  for (const ch of out) for (let i = 0; i < ns; i++) peak = Math.max(peak, Math.abs(ch[i]));
  console.log(peak === 0
    ? `✓ proof: all ${bands.length} bands flat → output is bit-exact silence (peak = 0)`
    : `✗ proof failed: peak = ${peak}`);
  process.exit(peak === 0 ? 0 : 1);
}

// ── loudness ride: per-second window RMS onto the movement-aware target ──
{
  const WIN = 1.0;
  const nWin = Math.ceil(totalSec / WIN);
  const gains = [];
  for (let w = 0; w < nWin; w++) {
    const a = Math.floor(w * WIN * SR), z = Math.min(ns, Math.floor((w + 1) * WIN * SR));
    let sum = 0;
    for (const chB of out) for (let j = a; j < z; j++) sum += chB[j] * chB[j];
    const rms = 10 * Math.log10(sum / Math.max(1, (z - a) * 2) + 1e-20);
    gains.push(Math.max(-60, Math.min(30, rideTargetAt((w + 0.5) * WIN) - rms)));
  }
  const sm = gains.map((_, i) => {
    const lo = Math.max(0, i - 1), hi = Math.min(gains.length - 1, i + 1);
    let s = 0; for (let k = lo; k <= hi; k++) s += gains[k];
    return s / (hi - lo + 1);
  });
  for (let i = 0; i < ns; i++) {
    const pos = i / SR / WIN - 0.5;
    const b0 = Math.max(0, Math.min(sm.length - 1, Math.floor(pos)));
    const b1 = Math.min(sm.length - 1, b0 + 1);
    const f = Math.max(0, Math.min(1, pos - b0));
    const lin = Math.pow(10, (sm[b0] * (1 - f) + sm[b1] * f) / 20);
    out[0][i] *= lin; out[1][i] *= lin;
  }
}

// ── normalize + edge fades ──────────────────────────────────────────────
let peak = 0;
for (const ch of out) for (let i = 0; i < ns; i++) peak = Math.max(peak, Math.abs(ch[i]));
const g = peak > 0 ? 0.9 / peak : 1;
const fadeIn = Math.floor(0.004 * SR), fadeOut = Math.floor(TAIL * SR);
for (const ch of out) {
  for (let i = 0; i < ns; i++) ch[i] *= g;
  for (let i = 0; i < fadeIn; i++) ch[i] *= i / fadeIn;
  for (let i = 0; i < fadeOut; i++) ch[ns - 1 - i] *= i / fadeOut;
}

// ── write out (lullaby master, mirrors run-c.mjs) ────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "nullabata.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const buf = Buffer.alloc(ns * 2 * 4);
for (let i = 0; i < ns; i++) {
  buf.writeFloatLE(out[0][i], i * 8);
  buf.writeFloatLE(out[1][i], i * 8 + 4);
}
writeFileSync(rawPath, buf);

const MASTER = [
  "highpass=f=28",
  "equalizer=f=220:t=q:w=1.1:g=2.8", // low-mid shelf = body/warmth in the notes
  "acompressor=threshold=-22dB:ratio=2.2:attack=25:release=220:makeup=2.0:knee=8",
  "lowpass=f=15000", // tame ultrasonic hiss (no treble boost — it lifted noise)
  "alimiter=limit=0.96:attack=5:release=80",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
if (!process.argv.includes("--keep-raw")) { try { unlinkSync(rawPath); } catch {} }
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (pop-mastered · ${(ns / SR).toFixed(1)} s)`);

// ── struct.json ──────────────────────────────────────────────────────────
{
  const struct = {
    _comment: "Section map for nullabata (the sonata cut): four carved-noise movements run attacca (no silence between). One lullaby motif, transformed per movement; high-Q bells so voices read as tones, not static.",
    meter: 4, bpm: MOV.finale.bpm, scale: "major", rootMidi: 60,
    totalSec: +(ns / SR).toFixed(6), prerollSec: 0,
    sections: MOVES.map((m) => ({
      name: m.id, title: m.title, bpm: m.bpm,
      startSec: +m.startSec.toFixed(6), endSec: +m.endSec.toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "nullabata.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
