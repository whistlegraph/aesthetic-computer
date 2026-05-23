#!/usr/bin/env node
// render-mommyboo.mjs — render the mommyboo search-for-mom track to mp3.
//
// Score notation lives in pop/marimba/mommyboo.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   rosewood       — the singer; the syllabic search
//   vibraphone_off — held thirds, the warm rooms / patches of light
//   bass           — gentle half-bar pulse on beat 1 (slow footstep)
//   kalimba        — questioning twinkles on phrase tails
//
// Run:
//   node pop/marimba/bin/render-mommyboo.mjs
//   node pop/marimba/bin/render-mommyboo.mjs --out ~/mommyboo.mp3
//
// All timing in seconds. 72 BPM 3/4 → beat = 60/72 ≈ 0.833, bar ≈ 2.500.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 72;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;            // 3/4 meter
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 3.0;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table ────────────────────────────────────────────────────────
// F major — F=65, F3=53, F2=41 per brief
const N = {
  F2: 41, A2: 45, C3: 48,
  F3: 53, A3: 57, Bb3: 58, C4: 60,
  D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, Bb4: 70, C5: 72,
  D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, Bb5: 82, C6: 84,
  D6: 86, E6: 88, F6: 89,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // rosewood — the singer
const padBuf   = new Float32Array(ns);   // vibraphone_off — warm rooms
const bassBuf  = new Float32Array(ns);   // bass marimba — soft footstep
const twinkBuf = new Float32Array(ns);   // kalimba — questioning twinkles

// ── voice routes ──────────────────────────────────────────────────────
// rosewood — slightly damped so each syllable is distinct in the slow tempo
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "rosewood", decayMul: 0.95 },
    leadBuf, { sampleRate: SR });
}
// vibraphone_off — long held pad, no motor, the warm rooms
function pad(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "vibraphone_off", decayMul: 1.6 },
    padBuf, { sampleRate: SR });
}
// bass — gentle low pulse, NOT a long drone (short footstep)
function bass(t0, midi, beats, gain = 0.78) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.6 },
    bassBuf, { sampleRate: SR });
}
// kalimba — bright questioning twinkles, slightly damped
function twink(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.85 },
    twinkBuf, { sampleRate: SR });
}

// ── sections (matches mommyboo.np) ────────────────────────────────────
const SECTIONS = [
  { name: "mommy1", bar0: 0,  bars: 4 },   // F  — calling, front hall
  { name: "where",  bar0: 4,  bars: 4 },   // Dm — searching empty room
  { name: "hi",     bar0: 8,  bars: 6 },   // Bb — finding her, warmth
  { name: "wow",    bar0: 14, bars: 4 },   // C  — relief
  { name: "hug",    bar0: 18, bars: 6 },   // F  — home cadence
];

// ── LEAD line (rosewood) — bar-by-bar from mommyboo.np ────────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § mommy1 (1–4) — "mom-MY / mom-MY / mom-MY / mom-MY" — calling, rising
L(1, 0,   N.A4, 1.5);   L(1, 1.5, N.C5, 1.5);
L(2, 0,   N.C5, 1.5);   L(2, 1.5, N.F5, 1.5);
L(3, 0,   N.A4, 1.5);   L(3, 1.5, N.C5, 1.5);
L(4, 0,   N.C5, 1.5);   L(4, 1.5, N.F5, 1.5);

// § where (5–8) — "where-IS / SHE / where-IS / SHE" — descending search
L(5, 0,   N.F5, 1.5);   L(5, 1.5, N.D5, 1.5);
L(6, 0,   N.C5, 3, 0.92);
L(7, 0,   N.F5, 1.5);   L(7, 1.5, N.D5, 1.5);
L(8, 0,   N.C5, 3, 0.92);

// § hi (9–14) — "hi-mom-MY / (ring) / hi-mom-MY / hi-mom / hi mom / (settle)"
L(9,  0,   N.F5, 1);     L(9,  1,   N.A5, 1);     L(9,  2, N.C6, 1);
L(10, 0,   N.C6, 3, 0.92);
L(11, 0,   N.F5, 1);     L(11, 1,   N.A5, 1);     L(11, 2, N.C6, 1);
L(12, 0,   N.A5, 1.5);   L(12, 1.5, N.G5, 1.5);
L(13, 0,   N.Bb5, 1.5);  L(13, 1.5, N.A5, 1.5);
L(14, 0,   N.F5, 3, 0.90);

// § wow (15–18) — "WOW / WOW / I-SEE / her" — relief
L(15, 0,   N.C6, 3, 0.95);
L(16, 0,   N.C6, 3, 0.92);
L(17, 0,   N.A5, 1.5);   L(17, 1.5, N.G5, 1.5);
L(18, 0,   N.F5, 3, 0.90);

// § hug (19–24) — "I-SEE / her / I-SEE / her / ho-ome / (settle)"
L(19, 0,   N.A5, 1.5);   L(19, 1.5, N.G5, 1.5);
L(20, 0,   N.F5, 3, 0.90);
L(21, 0,   N.A5, 1.5);   L(21, 1.5, N.G5, 1.5);
L(22, 0,   N.F5, 3, 0.90);
L(23, 0,   N.A4, 1.5);   L(23, 1.5, N.F4, 1.5);
L(24, 0,   N.F4, 3, 0.95);   // long final note, tail rings

// ── PAD (vibraphone_off) — chord arc, held thirds across sections ─────
// F – Dm – Bb – C – F  (one chord per section, sustained as a warm room)
function P(bar, b, midi, d, g) { pad(t(bar - 1, b), midi, d, g ?? 0.55); }

// mommy1 (1–4): F chord — F3 + A3 + C4 — front hall warmth, enters at bar 1
P(1, 0, N.F3, 12, 0.50);
P(1, 0, N.A3, 12, 0.42);
P(1, 0, N.C4, 12, 0.40);
// where (5–8): Dm chord — D4 + F4 + A4 — emptier, slightly sadder
P(5, 0, N.D4, 12, 0.48);
P(5, 0, N.F4, 12, 0.42);
P(5, 0, N.A4, 12, 0.38);
// hi (9–14): Bb chord — Bb3 + D4 + F4 — finding her, warm bloom
P(9, 0, N.Bb3, 18, 0.55);
P(9, 0, N.D4, 18, 0.45);
P(9, 0, N.F4, 18, 0.40);
// wow (15–18): C chord — C4 + E4 + G4 — relief, lift
P(15, 0, N.C4, 12, 0.55);
P(15, 0, N.E4, 12, 0.45);
P(15, 0, N.G4, 12, 0.40);
// hug (19–24): F chord — F3 + A3 + C4 — home cadence (echoes mommy1)
P(19, 0, N.F3, 18, 0.55);
P(19, 0, N.A3, 18, 0.45);
P(19, 0, N.C4, 18, 0.42);
// final long F2 root rings under bar 24
P(24, 0, N.F2, 6, 0.50);

// ── BASS — gentle half-bar pulse on beat 1, soft footstep ─────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.78); }

// mommy1 — slow footstep on every beat 1
for (let bar = 1; bar <= 4; bar++) B(bar, 0, N.F2, 2.5);
// where — drop to A2 / D3 alternating (Dm root + 5th)
B(5, 0, N.D4 - 24, 2.5);   // D2
B(6, 0, N.A2, 2.5);
B(7, 0, N.D4 - 24, 2.5);
B(8, 0, N.A2, 2.5);
// hi — Bb low, warmer, slightly louder as we find her
for (let bar = 9; bar <= 14; bar++) B(bar, 0, N.Bb3 - 12, 2.5, 0.82);   // Bb2
// wow — C low, lifted
for (let bar = 15; bar <= 18; bar++) B(bar, 0, N.C3, 2.5, 0.78);
// hug — F home, the final settling steps
B(19, 0, N.F2, 2.5);
B(20, 0, N.F2, 2.5);
B(21, 0, N.F2, 2.5);
B(22, 0, N.F2, 2.5);
B(23, 0, N.F2, 2.5, 0.72);
B(24, 0, N.F2, 4, 0.85);   // a final deep settling step under the long F4

// ── KALIMBA — questioning twinkles on phrase tails ────────────────────
function K(bar, b, midi, d, g) { twink(t(bar - 1, b), midi, d, g ?? 0.55); }
// mommy1 tails — light question-mark on the &-of-3 (bars 2 and 4)
K(2, 2.5, N.F6, 0.5, 0.42);
K(4, 2.5, N.A5, 0.5, 0.45);
// where — answering twinkles after the "SHE" holds (bars 6 + 8)
K(6, 2,   N.E6, 0.5, 0.50);
K(6, 2.5, N.D6, 0.5, 0.45);
K(8, 2,   N.E6, 0.5, 0.50);
K(8, 2.5, N.F6, 0.5, 0.50);
// hi — sparkle haloes over the "hi-mom-MY" climbs
K(9,  2.5, N.E6, 0.5, 0.55);
K(10, 1.5, N.E6, 0.5, 0.45);
K(10, 2.5, N.F6, 0.5, 0.40);
K(11, 2.5, N.E6, 0.5, 0.55);
K(13, 2.5, N.C6, 0.5, 0.45);
// wow — bright sparkles celebrate the relief
K(15, 1, N.E6, 0.5, 0.60);
K(15, 2, N.G5, 0.5, 0.50);
K(16, 1, N.E6, 0.5, 0.55);
K(16, 2, N.G5, 0.5, 0.50);
// hug — soft drifting twinkles, descending to home
K(19, 2.5, N.C6, 0.5, 0.45);
K(20, 1.5, N.A5, 0.5, 0.40);
K(21, 2.5, N.C6, 0.5, 0.42);
K(22, 1.5, N.A5, 0.5, 0.38);
K(23, 2.5, N.F5, 0.5, 0.40);
K(24, 1.5, N.A4, 0.5, 0.38);   // one last drifting question-twinkle

// ═══════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN
// ═══════════════════════════════════════════════════════════════════
const outL = new Float32Array(ns);
const outR = new Float32Array(ns);

function pan(p) {
  const a = (p + 1) * Math.PI / 4;
  return [Math.cos(a), Math.sin(a)];
}
function place(buf, p, gain) {
  const [lg, rg] = pan(p);
  for (let i = 0; i < ns; i++) {
    const s = buf[i] * gain;
    outL[i] += s * lg;
    outR[i] += s * rg;
  }
}
// per brief: rosewood 0, pad +0.25, bass 0, kalimba +0.40
place(leadBuf,   0.00, 1.00);
place(padBuf,   +0.25, 0.90);
place(bassBuf,   0.00, 0.92);
place(twinkBuf, +0.40, 0.82);

// scrub non-finite + peak-normalise
let nan = 0;
for (let i = 0; i < ns; i++) {
  if (!Number.isFinite(outL[i])) { outL[i] = 0; nan++; }
  if (!Number.isFinite(outR[i])) { outR[i] = 0; nan++; }
}
if (nan) console.warn(`     ! scrubbed ${nan} non-finite samples`);
let peak = 0;
for (let i = 0; i < ns; i++) {
  const a = Math.abs(outL[i]); if (a > peak) peak = a;
  const b = Math.abs(outR[i]); if (b > peak) peak = b;
}
if (peak > 0) { const nrm = 0.86 / peak;
  for (let i = 0; i < ns; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

// trim trailing silence + tiny fade-in, longer fade-out (1.5s) per brief
let lastLoud = ns - 1;
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < 0.004 &&
       Math.abs(outR[lastLoud]) < 0.004) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.8 * SR));
const fadeIn = Math.floor(0.004 * SR);
const fadeOut = Math.floor(1.5 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ mommyboo · ${TOTAL_BARS} bars · F major 3/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "mommyboo.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// SOFTENED master chain — slow/sweet, gentler than flutterbap
const MASTER = [
  "highpass=f=30",
  "acompressor=threshold=-20dB:ratio=2.0:attack=22:release=220:makeup=1.6:knee=6",
  "treble=g=1.0:f=7500",
  "alimiter=limit=0.96:attack=4:release=80",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (pop-mastered · ${(trimN / SR).toFixed(1)} s)`);

// ── struct.json — section map for any future visualizer ───────────────
{
  const structTotalSec = trimN / SR;
  const struct = {
    _comment: "Section map for mommyboo (looking for mom across the house). 3/4, 72 BPM → bar ≈ 2.500 s. F major. Derived from render-mommyboo.mjs SECTIONS.",
    meter: 3, bpm: BPM, scale: "major", rootMidi: 65,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "mommyboo.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
