#!/usr/bin/env node
// render-nuellaby.mjs — the accumulation cut: no stages, no doubling.
// EQ points are added ONE AT A TIME, roughly one per beat, until the whole
// rack (84 points) is sounding — then they leave again, last-in-first-out,
// about two per beat. The lead whistle and the bass are the first two
// points in and the last two out. Boléro logic, carved from cancelled noise.
//
// Same trick as render-nullabye.mjs (after Andy Brewer's "this song has no
// instruments in it", https://youtu.be/_Rk-hmIMv6I): two copies of pink
// noise, one phase-inverted — perfect silence — and every note is a peaking
// EQ bell breaking the cancellation. No oscillator anywhere in this file.
//
// LANES: a lane is one persistent EQ point on the rack; consecutive notes
// on a lane reuse it monophonically. Every lane has a fixed ROSTER rank —
// the order points join in — and a note only sounds if the count curve
// N(t) has reached its rank. Arpeggios assemble note by note, the twinkle
// cloud accretes twinkle by twinkle, the choir gains one partial at a time.
//
// The spectrum is carved into zones, each owned by a few layers:
//   SUB        40–90      kick (110→42 Hz pitch-drop bell), bass roots
//   BASS       90–250     drones (root+fifth)
//   LOW-MID    250–500    chord pads, detuned doubles
//   MID        500–1k     lead whistle, harmony 3rd, choir low partials
//   UPPER-MID  1–2.5k     octave lead, arpeggios, octave pads
//   PRESENCE   2.5–5k     sparkles, choir highs
//   AIR        5–13k      hats (closed+open), offbeat shaker, veils
//
// Count curve (in beats; 38 bars × 4 = 152 beats = 120.0 s exactly):
//   0–8     hold 2          the duet establishes
//   8–92    2 → 84          accumulating: ~1 new point per beat
//   92–100  hold 84         the bloom crowns
//   100–140 84 → 2          shedding: ~2 points leave per beat
//   140–152 2 → empty       lone whistle, downward veil, literal silence
//
// A breath swells whenever the count crosses a power of two — a quiet nod
// to the doubling this cut grew out of.
//
// Run:
//   node pop/nullabye/bin/render-nuellaby.mjs
//   node pop/nullabye/bin/render-nuellaby.mjs --proof   # flat ⇒ bit-exact silence

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

const BPM = 76;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 38; // 38 × 240/76 s = 120.0 s — exactly 2:00
const totalSec = PROOF ? 6 : TOTAL_BARS * BAR;
const ns = Math.ceil(totalSec * SR);

const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── the roster: every EQ point, in the order they join ────────────────
// foundation first, then the bloom layers interleaved across registers so
// the texture grows evenly instead of register by register
const ROSTER = [
  "lead", "bass", "veil", "breath", "padR", "pad5", "kick", "hat", "pad3",
  "pad9", "harm", "hatOpen", "leadOct", "spark0", "spark1",
  "arp0", "droneR", "arp1", "arp2", "droneF",
  "padOct0", "padOct1", "shaker", "spark2",
];
{
  // IMPORTANT: the EQ chain is SERIAL, so two bells near the same frequency
  // ADD their dB (pad 19 + double 13 + double 13 = +45 — the old detuned
  // doubles and root-partial choir both blew up this way). every group here
  // owns frequencies nothing else sustains.
  const groups = [
    Array.from({ length: 16 }, (_, j) => `cloud${j}`),
    Array.from({ length: 21 }, (_, j) => `halo${j}`),
    Array.from({ length: 4 }, (_, j) => `choir${j}`),   // spice partials only
    Array.from({ length: 4 }, (_, j) => `airOct${j}`),
    Array.from({ length: 3 }, (_, j) => `arpB${j}`),
    Array.from({ length: 13 }, (_, j) => `cloud2_${j}`),
  ];
  let k = 0;
  while (groups.some((g) => g.length)) {
    if (groups[k].length) ROSTER.push(groups[k].shift());
    k = (k + 1) % groups.length;
  }
}
const N_MAX = ROSTER.length; // 84
const RANK = new Map(ROSTER.map((n, i) => [n, i]));

// ── the count curve: how many points are on the rack at time `time` ───
function pointsAt(time) {
  const beat = time / BEAT;
  if (beat < 8) return 2;
  if (beat < 92) return 2 + ((beat - 8) / 84) * (N_MAX - 2); // accumulating
  if (beat < 100) return N_MAX;                              // the bloom
  if (beat < 140) return N_MAX - ((beat - 100) / 40) * (N_MAX - 2); // shedding
  return 2;
}

// ── note table (Hz) ───────────────────────────────────────────────────
const HZ = {
  A1: 55.0, C2: 65.41, F2: 87.31, G2: 98.0, A2: 110.0, C3: 130.81,
  F3: 174.61, G3: 196.0, A3: 220.0, B3: 246.94,
  C4: 261.63, D4: 293.66, E4: 329.63, F4: 349.23, G4: 392.0, A4: 440.0, B4: 493.88,
  C5: 523.25, D5: 587.33, E5: 659.26, F5: 698.46, G5: 783.99, A5: 880.0, B5: 987.77,
  C6: 1046.5, D6: 1174.66, E6: 1318.51, G6: 1567.98, A6: 1760.0,
  C7: 2093.0, D7: 2349.32, E7: 2637.02,
};

// ── the key journey: the whole system transposes with the arch ────────
// rising C → Eb → F into the bloom, falling back home through the
// shedding. every voice is a frequency, so a key change is one multiply.
// percussion lanes live in noise zones and stay put.
const KEYS = [1, 1, 1.1892, 1.1892, 1.3348, 1.3348, 1.1892, 1, 1]; // per 4-bar phrase
const keyAt = (t0) => KEYS[Math.min(KEYS.length - 1, Math.floor(t0 / (4 * BAR)))];
const PERC = new Set(["kick", "hat", "hatOpen", "shaker"]);

// ── lanes: persistent monophonic EQ points, gated by roster rank ──────
const lanes = new Map(); // name → { events: [] }

function note(lane, t0, { freq, freqEnd = null, peakDb, q, atk, hold, rel }) {
  const rank = RANK.get(lane);
  if (rank !== undefined && t0 < t(36) && rank >= pointsAt(t0)) return; // not on the rack yet / already left
  if (!PERC.has(lane)) {
    const k = keyAt(t0);
    freq *= k;
    if (freqEnd) freqEnd *= k;
  }
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

// ── score ─────────────────────────────────────────────────────────────
// C major, chord cycle C / Am9 / Fmaj9 / Gadd9 on bar % 4 — the harmony
// never notices the rack growing and shrinking around it.
const CHORDS = [
  { tones: [HZ.C4, HZ.E4, HZ.G4], ninth: HZ.D5, root: HZ.C2, arp: [HZ.C5, HZ.E5, HZ.G5] },
  { tones: [HZ.A3, HZ.C4, HZ.E4], ninth: HZ.B4, root: HZ.A1, arp: [HZ.A4, HZ.C5, HZ.E5] },
  { tones: [HZ.F3, HZ.A3, HZ.C4], ninth: HZ.G4, root: HZ.F2, arp: [HZ.F4, HZ.A4, HZ.C5] },
  { tones: [HZ.G3, HZ.B3, HZ.D4], ninth: HZ.A4, root: HZ.G2, arp: [HZ.G4, HZ.B4, HZ.D5] },
];

// melodies — [freq, startBeat, durBeats] per bar
const M1 = [
  [[HZ.E5, 0, 1.5], [HZ.G5, 1.5, 0.5], [HZ.A5, 2, 2]],
  [[HZ.G5, 0, 1], [HZ.E5, 1, 1], [HZ.D5, 2, 2]],
  [[HZ.C5, 0, 1.5], [HZ.D5, 1.5, 0.5], [HZ.E5, 2, 1], [HZ.G5, 3, 1]],
  [[HZ.D5, 0, 3]],
];
const M2 = [
  [[HZ.E5, 0, 1.5], [HZ.G5, 1.5, 0.5], [HZ.A5, 2, 2]],
  [[HZ.G5, 0, 1], [HZ.A5, 1, 1], [HZ.C6, 2, 2]],
  [[HZ.A5, 0, 1.5], [HZ.G5, 1.5, 0.5], [HZ.E5, 2, 1], [HZ.C5, 3, 1]],
  [[HZ.D5, 0, 2], [HZ.E5, 2, 2]],
];
const M3 = [ // soaring, for the bloom
  [[HZ.G5, 0, 4]],
  [[HZ.A5, 0, 2], [HZ.C6, 2, 2]],
  [[HZ.A5, 0, 2], [HZ.G5, 2, 2]],
  [[HZ.E5, 0, 4]],
];
// melody runs on its own 4-bar phrase grid — the tune is continuous while
// the architecture accumulates and sheds around it (bloom phrase = M3)
const MELODY_TRACK = [M1, M2, M1, M2, M1, M3, M2, M1, M2];
const THIRD_BELOW = new Map([
  [HZ.E5, HZ.C5], [HZ.G5, HZ.E5], [HZ.A5, HZ.F5], [HZ.D5, HZ.B4], [HZ.C5, HZ.A4], [HZ.C6, HZ.A5],
]);

const lead = (lane, bar, beat, freq, beats, db = 26) =>
  note(lane, t(bar, beat), { freq, peakDb: db, q: 55, atk: 0.06, hold: beats * BEAT - 0.06, rel: 0.45 });
const pad = (lane, bar, freq, db = 19, q = 28) =>
  note(lane, t(bar), { freq, peakDb: db, q, atk: 0.8, hold: BAR - 2.0, rel: 1.2 });

// tiny deterministic LCG for the twinkle clouds
const lcg = (seed) => { let s = seed >>> 0; return () => (s = (s * 1664525 + 1013904223) >>> 0) / 4294967296; };

const HALO = []; // diatonic C-major wash, C4 → B6 registers
for (const oct of [1, 2, 4])
  for (const f of [HZ.C4, HZ.D4, HZ.E4, HZ.F4, HZ.G4, HZ.A4, HZ.B4])
    if (f * oct <= HZ.E7 + 1) HALO.push(f * oct);
const PENT_HI = [HZ.C6, HZ.D6, HZ.E6, HZ.G6, HZ.A6, HZ.C7, HZ.D7, HZ.E7];
const PENT_MID = [HZ.C5, HZ.D5, HZ.E5, HZ.G5, HZ.A5];

// every bar emits the FULL arrangement; the roster gate inside note()
// decides what actually sounds. pattern density follows the local count.
for (let bar = 0; bar < 36; bar++) {
  const ch = CHORDS[bar % 4];
  const mline = MELODY_TRACK[Math.floor(bar / 4)][bar % 4];
  const n0 = pointsAt(t(bar)); // local density, for pattern choices

  // the founding duet
  for (const [f, beat, beats] of mline)
    lead("lead", bar, beat, f, beats, n0 > N_MAX * 0.9 ? 29 : 26);
  // bass layers different frequencies as it wakes: whole-note root while
  // sparse, then a root–fifth–root–octave walk through the low end
  if (n0 < 10) note("bass", t(bar), { freq: ch.root, peakDb: 26, q: 10, atk: 0.03, hold: BAR - 0.6, rel: 0.4 });
  else [[0, 1], [1, 1.5], [2, 1], [3, 2]].forEach(([beat, mul]) =>
    note("bass", t(bar, beat), { freq: ch.root * mul, peakDb: mul > 1.6 ? 20 : 26, q: 10, atk: 0.02, hold: 0.9 * BEAT, rel: 0.25 }));

  // the veil — a wide bell barely open across the WHOLE track, swelling
  // with the count: the noise-whisper never stops, sound runs throughout
  note("veil", t(bar), { freq: ch.root * 8, peakDb: 3.5 + 4.5 * (n0 / N_MAX), q: 0.55, atk: 0.4, hold: BAR - 0.4, rel: 0.8 });

  // pads
  pad("padR", bar, ch.tones[0]); pad("pad5", bar, ch.tones[2]);
  pad("pad3", bar, ch.tones[1]); pad("pad9", bar, ch.ninth, 17);
  pad("padOct0", bar, ch.tones[0] * 2, 14); pad("padOct1", bar, ch.tones[1] * 2, 14);

  // kick — pitch-dropping bell, the classic shape carved from noise.
  // +31 dB, not more: the residue scales ~(10^(dB/40)−1)× band noise, and
  // past ~32 dB its transient owns the peak normalizer.
  for (const beat of [0, 2])
    note("kick", t(bar, beat), { freq: 110, freqEnd: 42, peakDb: beat ? 29 : 31, q: 2.2, atk: 0.003, hold: 0.012, rel: 0.18 });
  if (n0 >= 24) // ghost kick once the groove is thick enough to want it
    note("kick", t(bar, 3.5), { freq: 90, freqEnd: 46, peakDb: 25, q: 2.5, atk: 0.003, hold: 0.01, rel: 0.12 });

  // hats — backbeat ticks while sparse, driving eighths once the rack fills
  const hatBeats = n0 >= 14 ? [0.5, 1, 1.5, 2.5, 3] : [1, 3];
  for (const beat of hatBeats)
    note("hat", t(bar, beat), { freq: 9000, peakDb: beat % 1 ? 23 : 21, q: 2.5, atk: 0.002, hold: 0.008, rel: 0.05 });
  note("hatOpen", t(bar, 3.5), { freq: 7600, peakDb: 21, q: 2, atk: 0.002, hold: 0.02, rel: 0.28 });

  // harmony 3rd + octave lead
  for (const [f, beat, beats] of mline) {
    const h = THIRD_BELOW.get(f);
    if (h) lead("harm", bar, beat, h, beats, 20);
    lead("leadOct", bar, beat, f * 2, beats, 14);
  }

  // glints
  const glint = [HZ.E6, HZ.G6, HZ.D6, HZ.E6][bar % 4];
  note(`spark${bar % 2}`, t(bar, bar % 2 ? 3.5 : 1.5), { freq: glint, peakDb: 18, q: 70, atk: 0.01, hold: 0.06, rel: 0.18 });
  if (bar % 2) note("spark2", t(bar, 2.75), { freq: HZ.D7, peakDb: 16, q: 70, atk: 0.01, hold: 0.05, rel: 0.16 });

  // arpeggio — assembles note by note as its three lanes join the rack
  const steps = [0, 1, 2, 1, 0, 1, 2, 1];
  for (let k = 0; k < 8; k++)
    note(`arp${k % 3}`, t(bar, k * 0.5), { freq: ch.arp[steps[k]], peakDb: 15, q: 60, atk: 0.015, hold: 0.12, rel: 0.2 });

  // drones + shaker
  note("droneR", t(bar), { freq: ch.root * 2, peakDb: 15, q: 14, atk: 0.5, hold: BAR - 1.3, rel: 0.8 });
  note("droneF", t(bar), { freq: ch.root * 3, peakDb: 13, q: 14, atk: 0.5, hold: BAR - 1.3, rel: 0.8 });
  for (const beat of [0.5, 2.5])
    note("shaker", t(bar, beat), { freq: 11500, peakDb: 10, q: 4, atk: 0.004, hold: 0.012, rel: 0.05 });

  // twinkle cloud — accretes one lane at a time (offsets sorted per lane;
  // a lane is monophonic, late-then-early insertion would clip the earlier note)
  for (let j = 0; j < 16; j++) {
    const rnd = lcg(j * 7919 + bar * 104729);
    const f = PENT_HI[(j + bar) % PENT_HI.length];
    const offs = [Math.floor(rnd() * 8) * 0.5, Math.floor(rnd() * 8) * 0.5].sort((a, z) => a - z);
    for (const beat of offs)
      note(`cloud${j}`, t(bar, beat), { freq: f, peakDb: 12, q: 70, atk: 0.012, hold: 0.08, rel: 0.22 });
  }
  for (let j = 0; j < 13; j++) {
    const rnd = lcg(j * 31337 + bar * 49157);
    const f = PENT_MID[(j + bar) % PENT_MID.length];
    note(`cloud2_${j}`, t(bar, Math.floor(rnd() * 8) * 0.5), { freq: f, peakDb: 10, q: 60, atk: 0.015, hold: 0.1, rel: 0.25 });
  }

  // air octave — chord tones two octaves up, a register nothing sustains
  [...ch.tones.map((f) => f * 4), ch.ninth * 2].forEach((f, i) =>
    pad(`airOct${i}`, bar, f, 10, 30));

  // second arpeggio — offbeat 16ths an octave up once its lanes join
  for (let k = 0; k < 8; k++)
    note(`arpB${k % 3}`, t(bar, 0.25 + k * 0.5), { freq: ch.arp[steps[k]] * 2, peakDb: 12, q: 60, atk: 0.012, hold: 0.08, rel: 0.16 });

  // choir — ONLY the spice partials (7, 11, 13, 14): every other partial
  // of the root is already owned by the bass / drones / pads, and serial
  // bells at the same frequency add their dB
  [7, 11, 13, 14].forEach((p, i) =>
    note(`choir${i}`, t(bar), { freq: ch.root * p, peakDb: 9 - i * 0.5, q: 35, atk: 0.7, hold: BAR - 1.6, rel: 0.9 }));

  // halo — the diatonic wash
  HALO.forEach((f, i) =>
    note(`halo${i}`, t(bar), { freq: f, peakDb: 7.5, q: 25, atk: 0.9, hold: BAR - 2.1, rel: 1.2 }));
}

// breaths — one swell each time the count crosses a power of two, rising
// center on the way up, falling on the way down
{
  const crossings = [];
  for (const p2 of [4, 8, 16, 32, 64, N_MAX]) {
    crossings.push([8 + ((p2 - 2) / (N_MAX - 2)) * 84, p2]); // accumulating
    if (p2 !== N_MAX) crossings.push([100 + ((N_MAX - p2) / (N_MAX - 2)) * 40, p2]); // shedding
  }
  crossings.sort((a, z) => a[0] - z[0]);
  for (const [beatPos, p2] of crossings)
    note("breath", beatPos * BEAT, { freq: 380 * Math.pow(1.7, Math.log2(p2) - 3), peakDb: 8.5, q: 0.9, atk: 1.0, hold: 0.2, rel: 1.2 });
}

// empty — bars 36–37: the rack is bare again. lone whistle on the tonic,
// a veil sweeping DOWN, then flat EQ — the song ends in literal silence.
lead("lead", 36, 0, HZ.C5, 6, 24);
note("breath", t(36, 2), { freq: 4500, freqEnd: 230, peakDb: 8, q: 0.8, atk: 1.6, hold: 0.6, rel: 2.4 });

// ── log the shape ─────────────────────────────────────────────────────
{
  const SPARK = "▁▂▃▄▅▆▇█";
  const line = Array.from({ length: TOTAL_BARS }, (_, b) =>
    SPARK[Math.min(7, Math.floor((pointsAt(t(b) + BAR / 2) / N_MAX) * 8))]).join("");
  console.log(`→ nuellaby · ${TOTAL_BARS} bars · C major @ ${BPM} BPM · ${N_MAX} EQ points, ~1 added per beat`);
  console.log(`   ${line}  (points per bar, ${lanes.size} lanes sounding)`);
}

// ── render (same dual-noise null engine as nullabye) ──────────────────
const bands = [...lanes.values()];
for (const b of bands) b.events.sort((x, y) => x.t0 - y.t0);

// --bake: write the score for the C engine (pop/nullabye/c/nullnoise.c)
// and exit. composition stays here in JS; the DSP runs in C.
{
  const bakeIdx = process.argv.indexOf("--bake");
  if (bakeIdx >= 0) {
    const bakePath = process.argv[bakeIdx + 1] || resolve(HERE, "..", "out", "nuellaby.score.txt");
    const L = [];
    L.push(`sr ${SR}`, `dur ${totalSec}`, `detune 1.0012 0.9988`, `seed ${0xBEEF} ${0xC0FFEE}`);
    L.push(`normpeak 0.88`, `fadein 0.004`, `fadeout 1.6`);
    const RIDE_BARS = 36; // leave the coda's fade alone (matches the JS ride)
    L.push(`ridewin ${BAR} ${RIDE_BARS}`);
    for (let b = 0; b < RIDE_BARS; b++)
      L.push(String(-26.5 + 8 * Math.sqrt(pointsAt(t(b) + BAR / 2) / N_MAX)));
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

// ── loudness ride: even out the accumulation ──────────────────────────
// raw loudness tracks band count (~43 dB between the duet and the bloom);
// measure per-bar RMS and ride it onto a gentle target that follows the
// count curve — present at 2 points, crowning (not crushing) at 84.
{
  const LAST_TRIM_BAR = 36; // leave the coda's fade alone
  const gains = [];
  for (let b = 0; b < LAST_TRIM_BAR; b++) {
    const a = Math.floor(t(b) * SR), z = Math.min(ns, Math.floor(t(b + 1) * SR));
    let sum = 0;
    for (const chB of out) for (let j = a; j < z; j++) sum += chB[j] * chB[j];
    const rms = 10 * Math.log10(sum / Math.max(1, (z - a) * 2) + 1e-20);
    // gentle: −26.5 at the duet → −18.5 at the bloom. with the veil bed
    // running throughout, the floor never drops out
    const target = -26.5 + 8 * Math.sqrt(pointsAt(t(b) + BAR / 2) / N_MAX);
    gains.push(Math.max(-60, Math.min(30, target - rms)));
  }
  const sm = gains.map((_, i) => { // 3-bar smoothing against per-bar pumping
    const lo = Math.max(0, i - 1), hi = Math.min(gains.length - 1, i + 1);
    let s = 0; for (let k = lo; k <= hi; k++) s += gains[k];
    return s / (hi - lo + 1);
  });
  console.log(`   ride: per-bar trims ${Math.min(...sm).toFixed(1)}…${Math.max(...sm).toFixed(1)} dB onto the count curve`);
  if (process.argv.includes("--ride-debug"))
    gains.forEach((gn, b) => console.log(`   bar ${String(b).padStart(2)}: trim ${gn.toFixed(1)} dB`));
  for (let i = 0; i < ns; i++) { // linear interp between bar centers
    const pos = i / SR / BAR - 0.5;
    const b0 = Math.max(0, Math.min(sm.length - 1, Math.floor(pos)));
    const b1 = Math.min(sm.length - 1, b0 + 1);
    const f = Math.max(0, Math.min(1, pos - b0));
    const gdb = sm[b0] * (1 - f) + sm[b1] * f;
    const lin = Math.pow(10, gdb / 20);
    out[0][i] *= lin; out[1][i] *= lin;
  }
}

// ── normalize + edge fades ────────────────────────────────────────────
let peak = 0;
for (const ch of out) for (let i = 0; i < ns; i++) peak = Math.max(peak, Math.abs(ch[i]));
const g = peak > 0 ? 0.88 / peak : 1;
const fadeIn = Math.floor(0.004 * SR), fadeOut = Math.floor(1.6 * SR);
for (const ch of out) {
  for (let i = 0; i < ns; i++) ch[i] *= g;
  for (let i = 0; i < fadeIn; i++) ch[i] *= i / fadeIn;
  for (let i = 0; i < fadeOut; i++) ch[ns - 1 - i] *= i / fadeOut;
}

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "nuellaby.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const buf = Buffer.alloc(ns * 2 * 4);
for (let i = 0; i < ns; i++) {
  buf.writeFloatLE(out[0][i], i * 8);
  buf.writeFloatLE(out[1][i], i * 8 + 4);
}
writeFileSync(rawPath, buf);

const MASTER = [
  "highpass=f=30",
  "acompressor=threshold=-22dB:ratio=2.2:attack=25:release=220:makeup=2.0:knee=8",
  "treble=g=0.8:f=9000",
  "alimiter=limit=0.96:attack=5:release=80",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
if (!process.argv.includes("--keep-raw")) { try { unlinkSync(rawPath); } catch {} }
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (pop-mastered · ${(ns / SR).toFixed(1)} s)`);

// ── struct.json ───────────────────────────────────────────────────────
{
  const struct = {
    _comment: "Section map for nuellaby (the accumulation cut). 4/4, 76 BPM, C major. 84 EQ points added ~1/beat, bloom, then shed ~2/beat. Landmarks derived from the count curve.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 60,
    totalSec: +(ns / SR).toFixed(6), prerollSec: 0,
    sections: [
      { name: "accumulating", startSec: 0, endSec: +(92 * BEAT).toFixed(6) },
      { name: "bloom", startSec: +(92 * BEAT).toFixed(6), endSec: +(100 * BEAT).toFixed(6) },
      { name: "shedding", startSec: +(100 * BEAT).toFixed(6), endSec: +t(36).toFixed(6) },
      { name: "empty", startSec: +t(36).toFixed(6), endSec: +(ns / SR).toFixed(6) },
    ],
  };
  writeFileSync(resolve(HERE, "..", "out", "nuellaby.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
