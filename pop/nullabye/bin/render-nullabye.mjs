#!/usr/bin/env node
// render-nullabye.mjs — a lullaby carved entirely out of cancelled noise.
//
// Technique study after Andy Brewer's "this song has no instruments in it"
// (https://youtu.be/_Rk-hmIMv6I): take pink noise, duplicate it, invert the
// phase of the copy — the sum is perfect silence. Put an EQ on the copy and
// every deviation from flat breaks the cancellation; what you hear is the
// *difference*. A narrow peaking bell at a note frequency becomes a breathy,
// pitched voice pulled out of the hiss. The whole song is EQ automation —
// there is no oscillator anywhere in this file.
//
// Honoring the original constraint: at most 24 EQ points (bands) exist.
// A small allocator hands notes to free bands and refuses a 25th voice.
// Pink noise + constant-Q bells = equal energy per voice at any pitch,
// which is the quiet genius of the source video.
//
// Voices (all the same instrument — a bell in the veil):
//   pads     — Q 28 chord tones, slow swells          (the choir)
//   lead     — Q 55 whistle melody, ±0.12% L/R detune (the whistler)
//   bass     — Q 10 low roots, half-note pulse        (the heartbeat root)
//   kick     — Q 4.5 @ 54 Hz, 130 ms                  (the heartbeat)
//   hat      — Q 5 @ 8.2 kHz ticks                    (the eyelash)
//   sparkle  — Q 70 high offbeat glints               (the mobile overhead)
//   veil     — Q 0.8 sweep, the one moment the raw noise shows through
//
// Run:
//   node pop/nullabye/bin/render-nullabye.mjs
//   node pop/nullabye/bin/render-nullabye.mjs --out ~/nullabye.mp3
//   node pop/nullabye/bin/render-nullabye.mjs --proof   # flat EQ ⇒ bit-exact silence
//
// 76 BPM 4/4 → beat ≈ 0.789 s, bar ≈ 3.158 s. 24 bars ≈ 76 s + tail.

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const BLOCK = 64; // coefficient-update granularity (~1.3 ms)

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const PROOF = process.argv.includes("--proof");

const BPM = 76;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = PROOF ? 6 : TOTAL_BARS * BAR + 3.0; // + tail for the last ring
const ns = Math.ceil(totalSec * SR);

const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (Hz — the EQ thinks in frequency, so we do too) ────────
const HZ = {
  F1: 43.65, A1: 55.0, C2: 65.41, F2: 87.31, G2: 98.0, A2: 110.0,
  F3: 174.61, G3: 196.0, A3: 220.0, B3: 246.94,
  C4: 261.63, D4: 293.66, E4: 329.63, G4: 392.0, A4: 440.0, B4: 493.88,
  C5: 523.25, D5: 587.33, E5: 659.26, G5: 783.99, A5: 880.0,
  C6: 1046.5, D6: 1174.66, E6: 1318.51, G6: 1567.98,
};

// ── the 24-band pool ──────────────────────────────────────────────────
const MAX_BANDS = 24;
const GUARD = 0.15; // s between notes on the same band (let the ring drain)
const bands = []; // { freeAt, events: [...] }
let peakConcurrent = 0;

function note(t0, { freq, freqEnd = null, peakDb, q, atk, hold, rel }) {
  if (PROOF) peakDb = 0; // flat EQ — the silence proof
  const end = t0 + atk + hold + rel;
  let band = null;
  for (const b of bands) if (b.freeAt <= t0) { band = b; break; }
  if (!band) {
    if (bands.length >= MAX_BANDS)
      throw new Error(`25th EQ point requested at t=${t0.toFixed(2)}s — the video only had 24, so do we`);
    band = { freeAt: 0, events: [] };
    bands.push(band);
  }
  band.freeAt = end + GUARD;
  band.events.push({ t0, freq, freqEnd, peakDb, q, atk, hold, rel, end });
  const live = bands.filter((b) => b.events.some((e) => e.t0 <= t0 && t0 < e.end)).length;
  if (live > peakConcurrent) peakConcurrent = live;
}

// envelope in dB-space: linear attack, hold, linear release (sounds exp)
function envOf(e, time) {
  const dt = time - e.t0;
  if (dt < 0 || dt >= e.atk + e.hold + e.rel) return 0;
  if (dt < e.atk) return dt / e.atk;
  if (dt < e.atk + e.hold) return 1;
  return 1 - (dt - e.atk - e.hold) / e.rel;
}

// ── score ─────────────────────────────────────────────────────────────
// C major lullaby. Chord cycle C / Am / F / G, one bar each.
const CHORDS = [
  [HZ.C4, HZ.E4, HZ.G4, HZ.D5],  // Cmaj9
  [HZ.A3, HZ.C4, HZ.E4, HZ.B4],  // Am9
  [HZ.F3, HZ.A3, HZ.C4, HZ.G4],  // Fmaj9
  [HZ.G3, HZ.B3, HZ.D4, HZ.A4],  // Gadd9
];
const BASS_ROOTS = [HZ.C2, HZ.A1, HZ.F2, HZ.G2];

const pad = (bar, freq, db = 20) =>
  note(t(bar), { freq, peakDb: db, q: 28, atk: 0.8, hold: BAR - 2.0, rel: 1.2 });
const lead = (bar, beat, freq, beats, db = 26) =>
  note(t(bar, beat), { freq, peakDb: db, q: 55, atk: 0.06, hold: beats * BEAT - 0.06, rel: 0.35 });
const bass = (bar, beat, freq) =>
  note(t(bar, beat), { freq, peakDb: 26, q: 10, atk: 0.02, hold: 0.9 * BEAT, rel: 0.25 });
const kick = (bar, beat, db = 30) =>
  note(t(bar, beat), { freq: 54, peakDb: db, q: 4.5, atk: 0.004, hold: 0.02, rel: 0.13 });
const hat = (bar, beat, db = 15) =>
  note(t(bar, beat), { freq: 8200, peakDb: db, q: 5, atk: 0.003, hold: 0.01, rel: 0.07 });
const sparkle = (bar, beat, freq, db = 20) =>
  note(t(bar, beat), { freq, peakDb: db, q: 70, atk: 0.01, hold: 0.06, rel: 0.18 });

// intro — bars 0–3: the veil breathes. wide bells swell in and out of the
// null so the listener hears the trick before they hear the song.
const BREATHS = [[0, 420], [1, 640], [2, 960], [3, 1440]];
for (const [bar, freq] of BREATHS)
  note(t(bar, 0.5), { freq, peakDb: 8.5, q: 0.9, atk: 1.1, hold: 0.3, rel: 1.3 });
for (const f of CHORDS[0]) pad(3, f, 14); // first chord ghosts in early

// pads — bars 4–20, one chord per bar
for (let bar = 4; bar <= 20; bar++)
  for (const f of CHORDS[bar % 4]) pad(bar, f, bar === 20 ? 17 : 20);

// melody — A section (bars 4–11)
const PHRASE_A = [
  [[HZ.E5, 0, 1.5], [HZ.G5, 1.5, 0.5], [HZ.A5, 2, 2]],
  [[HZ.G5, 0, 1], [HZ.E5, 1, 1], [HZ.D5, 2, 2]],
  [[HZ.C5, 0, 1.5], [HZ.D5, 1.5, 0.5], [HZ.E5, 2, 1], [HZ.G5, 3, 1]],
  [[HZ.D5, 0, 3]],
  [[HZ.E5, 0, 1.5], [HZ.G5, 1.5, 0.5], [HZ.A5, 2, 2]],
  [[HZ.G5, 0, 1], [HZ.A5, 1, 1], [HZ.C6, 2, 2]],
  [[HZ.A5, 0, 1.5], [HZ.G5, 1.5, 0.5], [HZ.E5, 2, 1], [HZ.C5, 3, 1]],
  [[HZ.D5, 0, 4]],
];
PHRASE_A.forEach((barNotes, i) =>
  barNotes.forEach(([f, b, d]) => lead(4 + i, b, f, d)));

// melody — B section (bars 12–19): restate, then close upward
const PHRASE_B = [
  ...PHRASE_A.slice(0, 4),
  [[HZ.E5, 0, 1.5], [HZ.G5, 1.5, 0.5], [HZ.A5, 2, 2]],
  [[HZ.C6, 0, 2], [HZ.A5, 2, 2]],
  [[HZ.G5, 0, 1.5], [HZ.E5, 1.5, 0.5], [HZ.D5, 2, 1], [HZ.C5, 3, 1]],
  [[HZ.D5, 0, 2], [HZ.E5, 2, 2]], // hangs on E into the final C — home
];
PHRASE_B.forEach((barNotes, i) =>
  barNotes.forEach(([f, b, d]) => lead(12 + i, b, f, d)));

// heartbeat — soft kick from bar 8, full from 12
for (let bar = 8; bar <= 19; bar++) {
  kick(bar, 0, bar < 12 ? 26 : 30);
  kick(bar, 2, bar < 12 ? 24 : 28);
}

// bass + hats + sparkles — B section only
for (let bar = 12; bar <= 19; bar++) {
  const root = BASS_ROOTS[bar % 4];
  bass(bar, 0, root);
  bass(bar, 2, root);
  hat(bar, 1.5);
  hat(bar, 3.5, 13);
  const glint = [HZ.E6, HZ.G6, HZ.D6, HZ.E6][bar % 4];
  sparkle(bar, bar % 2 ? 3.5 : 1.5, glint);
}

// outro — bars 20–23: the veil lifts (one wide sweeping bell lets the raw
// noise wash through), a last lone whistle, then the EQ goes flat and the
// nulled noise swallows everything. the song ends in *literal* silence.
note(t(20, 2), { freq: 250, freqEnd: 6000, peakDb: 9, q: 0.8, atk: 2.2, hold: 0.8, rel: 2.6 });
lead(21, 0, HZ.E5, 2, 21);
lead(22, 0, HZ.C5, 2, 17);

// ── render: two pink-noise tracks, one EQ'd + inverted, summed ────────
// per channel: residue = chain(noise) − noise. flat chain ⇒ exact zero.
for (const b of bands) b.events.sort((x, y) => x.t0 - y.t0);
console.log(`→ nullabye · ${TOTAL_BARS} bars · C major @ ${BPM} BPM · ${bands.length}/${MAX_BANDS} EQ points · peak ${peakConcurrent} voices sounding`);

// deterministic noise (LCG → Paul Kellett pink filter)
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
const DETUNE = [1.0012, 0.9988]; // ±0.12% L/R — the shimmer in the veil

for (let ch = 0; ch < 2; ch++) {
  const pink = makePink(ch ? 0xC0FFEE : 0xBEEF);
  const dry = new Float32Array(BLOCK);
  const wet = new Float32Array(BLOCK);
  // biquad state + per-band event cursor
  const st = bands.map(() => ({ z1: 0, z2: 0, cursor: 0, b0: 1, b1: 0, b2: 0, a1: 0, a2: 0 }));

  for (let blockStart = 0; blockStart < ns; blockStart += BLOCK) {
    const n = Math.min(BLOCK, ns - blockStart);
    const time = blockStart / SR;
    for (let i = 0; i < n; i++) wet[i] = dry[i] = pink();

    for (let bi = 0; bi < bands.length; bi++) {
      const band = bands[bi], s = st[bi];
      // advance past finished events (keep 0.4 s to drain the ring)
      while (s.cursor < band.events.length && time > band.events[s.cursor].end + 0.4) {
        s.cursor++; s.z1 = 0; s.z2 = 0;
      }
      const e = band.events[s.cursor];
      if (!e || time < e.t0 - 0.05) continue; // idle band: identity, skip

      const env = envOf(e, time);
      const db = e.peakDb * env;
      const ringing = Math.abs(s.z1) + Math.abs(s.z2) > 1e-9;
      if (db <= 0.01 && !ringing) continue;

      let freq = e.freq;
      if (e.freqEnd) { // exp glide across the event
        const p = Math.min(1, Math.max(0, (time - e.t0) / (e.end - e.t0)));
        freq = e.freq * Math.pow(e.freqEnd / e.freq, p);
      }
      // RBJ peaking EQ
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
      // transposed direct form II over the block
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

    // the trick itself: EQ'd copy minus the original. flat ⇒ silence.
    const o = out[ch];
    for (let i = 0; i < n; i++) o[blockStart + i] = wet[i] - dry[i];
  }
}

// ── proof mode: flat EQ must null to bit-exact digital zero ───────────
if (PROOF) {
  let peak = 0;
  for (const ch of out) for (let i = 0; i < ns; i++) peak = Math.max(peak, Math.abs(ch[i]));
  console.log(peak === 0
    ? "✓ proof: all 24 bands flat → output is bit-exact silence (peak = 0)"
    : `✗ proof failed: peak = ${peak}`);
  process.exit(peak === 0 ? 0 : 1);
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
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "nullabye.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const buf = Buffer.alloc(ns * 2 * 4);
for (let i = 0; i < ns; i++) {
  buf.writeFloatLE(out[0][i], i * 8);
  buf.writeFloatLE(out[1][i], i * 8 + 4);
}
writeFileSync(rawPath, buf);

// master chain — gentle: the material is all noise, so easy on the air band
// and a soft knee so the breaths keep breathing.
const MASTER = [
  "highpass=f=30",
  "acompressor=threshold=-22dB:ratio=2.2:attack=25:release=220:makeup=2.0:knee=8",
  "treble=g=0.8:f=9000",
  "alimiter=limit=0.96:attack=5:release=80",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (pop-mastered · ${(ns / SR).toFixed(1)} s)`);

// ── struct.json — section map for any future visualizer ───────────────
{
  const struct = {
    _comment: "Section map for nullabye (the cancelled-noise lullaby). 4/4, 76 BPM → bar ≈ 3.158 s. C major. Derived from render-nullabye.mjs.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 60,
    totalSec: +(ns / SR).toFixed(6), prerollSec: 0,
    sections: [
      { name: "breaths", startSec: 0, endSec: +t(4).toFixed(6) },
      { name: "lullaby-a", startSec: +t(4).toFixed(6), endSec: +t(12).toFixed(6) },
      { name: "lullaby-b", startSec: +t(12).toFixed(6), endSec: +t(20).toFixed(6) },
      { name: "veil-lifts", startSec: +t(20).toFixed(6), endSec: +(ns / SR).toFixed(6) },
    ],
  };
  writeFileSync(resolve(HERE, "..", "out", "nullabye.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
