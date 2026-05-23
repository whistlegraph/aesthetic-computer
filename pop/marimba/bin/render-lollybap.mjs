#!/usr/bin/env node
// render-lollybap.mjs — render the lollybap schoolyard candy-stand skip
// to mp3.
//
// Score doc lives in pop/marimba/lollybap.np (human-readable). This
// script holds the same music as scheduled per-voice events. Inline
// because there's no .np parser in the marimba lane yet.
//
// Meter is 6/8 COMPOUND: ~124 BPM dotted-quarter pulse. So BPM is the
// dotted-quarter rate, EIGHTH = 60/BPM/3, BAR = 6 * EIGHTH (two pulses).
// Syllables land on eighths 0..5 of each bar. Bar ≈ 0.968 s.
//
// Voices:
//   xylophone     — the singer; bright skip lead (lead bus)
//   bass          — low C/G dotted-quarter pulse on eighths 0 and 3
//   glockenspiel  — high sparkle accents on phrase tops
//   kalimba       — offbeat twinkles on eighths 2 and 5
//
// Run:
//   node pop/marimba/bin/render-lollybap.mjs
//   node pop/marimba/bin/render-lollybap.mjs --out ~/lollybap.mp3

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 124;   // dotted-quarter pulse
const EIGHTH = 60 / BPM / 3;                 // one eighth note (s)
const BAR = 6 * EIGHTH;                      // 6/8 bar (s) — two pulses
const TOTAL_BARS = 32;
const totalSec = TOTAL_BARS * BAR + 2.4;     // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// bar/eighth → seconds. e may be fractional (16th = 0.5).
const t = (bar, e = 0) => bar * BAR + e * EIGHTH;

// ── note table ────────────────────────────────────────────────────────
const N = {
  C2: 36, G2: 43,
  C3: 48, E3: 52, F3: 53, G3: 55, A3: 57, B3: 59,
  C4: 60, D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, F6: 89, G6: 91,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // xylophone — the singer
const bassBuf  = new Float32Array(ns);   // bass marimba — pulse
const bellBuf  = new Float32Array(ns);   // glockenspiel — high sparkle
const twinkBuf = new Float32Array(ns);   // kalimba — offbeat twinkles

// ── voice routes ──────────────────────────────────────────────────────
// xylophone is naturally short — extend slightly so the syllabic eighths
// still carry across the bar.
function lead(t0, midi, eighths, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: eighths * EIGHTH,
    gain, preset: "xylophone", decayMul: 1.15 },
    leadBuf, { sampleRate: SR });
}
// bass: tight dotted-quarter pulse — not a droning sub.
function bass(t0, midi, eighths, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: eighths * EIGHTH,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// glockenspiel: high steel, ring shimmery on phrase tops.
function bell(t0, midi, eighths, gain = 0.65) {
  mixEventMarimba({ startSec: t0, midi, durSec: eighths * EIGHTH,
    gain, preset: "glockenspiel", decayMul: 1.3 },
    bellBuf, { sampleRate: SR });
}
// kalimba: bright tine for offbeat twinkles.
function twink(t0, midi, eighths, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: eighths * EIGHTH,
    gain, preset: "kalimba", decayMul: 0.9 },
    twinkBuf, { sampleRate: SR });
}

// ── sections (matches lollybap.np) ────────────────────────────────────
const SECTIONS = [
  { name: "skip1",  bar0: 0,  bars: 8 },
  { name: "sweet",  bar0: 8,  bars: 8 },
  { name: "share",  bar0: 16, bars: 8 },
  { name: "skip2",  bar0: 24, bars: 8 },
];

// ── LEAD line (xylophone) — bar-by-bar from lollybap.np ───────────────
// L(bar1based, eighthOffset, midi, durEighths[, gain])
function L(bar, e, midi, d, g) { lead(t(bar - 1, e), midi, d, g ?? 1.0); }

// § skip1 (1–8) — "lol-LI-pop / lol-LI-pop" ascending bouncy
L(1, 0, N.E5, 1);  L(1, 1, N.G5, 1);                 L(1, 3, N.C6, 3);
L(2, 0, N.E5, 1);  L(2, 1, N.G5, 1);                 L(2, 3, N.C6, 3);
L(3, 0, N.D5, 1);  L(3, 1, N.G5, 1);                 L(3, 3, N.B5, 3);
L(4, 0, N.E5, 1);  L(4, 1, N.G5, 1);                 L(4, 3, N.C6, 1);  L(4, 4, N.G5, 1);  L(4, 5, N.C6, 1);
L(5, 0, N.E5, 1);  L(5, 1, N.G5, 1);                 L(5, 3, N.C6, 3);
L(6, 0, N.E5, 1);  L(6, 1, N.A5, 1);                 L(6, 3, N.C6, 3);
L(7, 0, N.G5, 1);  L(7, 1, N.A5, 1);                 L(7, 3, N.C6, 3);
L(8, 0, N.G5, 1);  L(8, 1, N.A5, 1);  L(8, 2, N.G5, 1);  L(8, 3, N.E5, 1);                  L(8, 5, N.C6, 1);

// § sweet (9–16) — rapid 6/8 on every eighth, F-chord coloration
L(9,  0, N.C5, 1);  L(9,  1, N.E5, 1);  L(9,  2, N.G5, 1);  L(9,  3, N.C5, 1);  L(9,  4, N.E5, 1);  L(9,  5, N.G5, 1);
L(10, 0, N.A5, 1);  L(10, 1, N.G5, 1);  L(10, 2, N.A5, 1);  L(10, 3, N.F5, 1);  L(10, 4, N.A5, 1);  L(10, 5, N.F5, 1);
L(11, 0, N.C5, 1);  L(11, 1, N.F5, 1);  L(11, 2, N.A5, 1);  L(11, 3, N.C5, 1);  L(11, 4, N.F5, 1);  L(11, 5, N.A5, 1);
L(12, 0, N.G5, 1);  L(12, 1, N.A5, 1);  L(12, 2, N.G5, 1);  L(12, 3, N.F5, 1);  L(12, 4, N.E5, 1);  L(12, 5, N.F5, 1);
L(13, 0, N.E5, 1);  L(13, 1, N.G5, 1);  L(13, 2, N.C6, 1);  L(13, 3, N.E5, 1);  L(13, 4, N.G5, 1);  L(13, 5, N.C6, 1);
L(14, 0, N.A5, 1);  L(14, 1, N.C6, 1);  L(14, 2, N.A5, 1);  L(14, 3, N.G5, 1);  L(14, 4, N.A5, 1);  L(14, 5, N.G5, 1);
L(15, 0, N.F5, 1);  L(15, 1, N.A5, 1);  L(15, 2, N.C6, 1);  L(15, 3, N.F5, 1);  L(15, 4, N.A5, 1);  L(15, 5, N.C6, 1);
L(16, 0, N.G5, 1);  L(16, 1, N.C6, 1);  L(16, 2, N.G5, 1);  L(16, 3, N.F5, 1);  L(16, 4, N.A5, 1);  L(16, 5, N.F5, 1);

// § share (17–24) — call (xylo) and response (kalimba reply happens
// on the K() pattern below). The xylo plays the "one-for-YOU" call.
L(17, 0, N.G5, 1);  L(17, 1, N.A5, 1);                 L(17, 3, N.C6, 3);
// bar 18 is the kalimba reply — xylo rests
L(19, 0, N.G5, 1);  L(19, 1, N.B5, 1);                 L(19, 3, N.D6, 3);
// bar 20 reply
L(21, 0, N.A5, 1);  L(21, 1, N.C6, 1);                 L(21, 3, N.E6, 3);
// bar 22 reply
L(23, 0, N.G5, 1);  L(23, 1, N.A5, 1);  L(23, 2, N.B5, 1);  L(23, 3, N.D6, 1);  L(23, 4, N.C6, 1);  L(23, 5, N.B5, 1);
// bar 24 reply

// § skip2 (25–32) — recap, big closing C major cadence
L(25, 0, N.E5, 1);  L(25, 1, N.G5, 1);  L(25, 2, N.C6, 1);  L(25, 3, N.G5, 1);  L(25, 4, N.A5, 1);  L(25, 5, N.C6, 1);
L(26, 0, N.E5, 1);  L(26, 1, N.G5, 1);  L(26, 2, N.C6, 1);  L(26, 3, N.G5, 1);  L(26, 4, N.A5, 1);  L(26, 5, N.C6, 1);
L(27, 0, N.G5, 1);  L(27, 1, N.A5, 1);  L(27, 2, N.C6, 1);  L(27, 3, N.A5, 1);  L(27, 4, N.G5, 1);  L(27, 5, N.E5, 1);
L(28, 0, N.G5, 1);  L(28, 1, N.A5, 1);  L(28, 2, N.C6, 1);  L(28, 3, N.G5, 1);  L(28, 4, N.A5, 1);  L(28, 5, N.C6, 1);
L(29, 0, N.C6, 1);  L(29, 1, N.G5, 1);  L(29, 2, N.E5, 1);  L(29, 3, N.C6, 1);  L(29, 4, N.G5, 1);  L(29, 5, N.E5, 1);
L(30, 0, N.G5, 1);  L(30, 1, N.A5, 1);  L(30, 2, N.C6, 1);  L(30, 3, N.E6, 1);  L(30, 4, N.D6, 1);  L(30, 5, N.C6, 1);
L(31, 0, N.E6, 3);                                          L(31, 3, N.G6, 3);
L(32, 0, N.C6, 1);  L(32, 1, N.G5, 1);  L(32, 2, N.E5, 1);  L(32, 3, N.C5, 3, 0.95);

// ── BASS — dotted-quarter pulse on eighths 0 and 3 ────────────────────
// Chord progression: C (bars 1-8), F (9-16), G (17-24), C (25-32).
function B(bar, e, midi, d, g) { bass(t(bar - 1, e), midi, d, g ?? 0.85); }

// skip1: C chord — low C on pulse 1, G on pulse 2
for (let bar = 1; bar <= 8; bar++) {
  B(bar, 0, N.C3, 2.7);
  B(bar, 3, N.G3, 2.7);
}
// sweet: F chord — low F on pulse 1, C on pulse 2 (slightly lighter, busier lead)
for (let bar = 9; bar <= 16; bar++) {
  B(bar, 0, N.F3, 2.7, 0.78);
  B(bar, 3, N.C4, 2.5, 0.72);
}
// share: G chord — low G on pulse 1, D on pulse 2
for (let bar = 17; bar <= 24; bar++) {
  B(bar, 0, N.G3, 2.7, 0.82);
  B(bar, 3, N.D4, 2.5, 0.74);
}
// skip2: back to C — full bounce
for (let bar = 25; bar <= 30; bar++) {
  B(bar, 0, N.C3, 2.7);
  B(bar, 3, N.G3, 2.7);
}
// bar 31 — bigger arrival: low C held across both pulses
B(31, 0, N.C3, 6, 0.92);
// bar 32 — final cadence: deep C2 landing
B(32, 0, N.C2, 6, 0.95);

// ── GLOCKENSPIEL — high sparkle on phrase tops ────────────────────────
function G(bar, e, midi, d, g) { bell(t(bar - 1, e), midi, d, g ?? 0.65); }

// skip1: just a tiny shimmer on bar 4's "pop-pop-POP" tail
G(4, 5, N.C6, 1, 0.55);
G(8, 5, N.E6, 1, 0.60);

// sweet (glockenspiel enters): light shimmer on bar tops of 9, 11, 13, 15
G(9,  0, N.C6, 2, 0.45);
G(11, 0, N.F6, 2, 0.50);
G(13, 0, N.G6, 2, 0.55);
G(15, 0, N.C6, 2, 0.55);
// extra sparkle on the busy bar 13/16 climax
G(16, 3, N.E6, 1, 0.45);

// share (glockenspiel sustains call tops): ring out the YOU notes
G(17, 3, N.C6, 3, 0.55);
G(19, 3, N.D6, 3, 0.55);
G(21, 3, N.E6, 3, 0.60);
G(23, 3, N.D6, 3, 0.50);

// skip2: high shimmer on phrase peaks
G(25, 2, N.C6, 1, 0.45);
G(27, 2, N.C6, 1, 0.45);
G(29, 0, N.C6, 1, 0.45);  G(29, 3, N.C6, 1, 0.45);
G(30, 3, N.E6, 1, 0.55);
G(31, 0, N.E6, 3, 0.60);  G(31, 3, N.G6, 3, 0.65);
G(32, 0, N.C6, 6, 0.70);

// ── KALIMBA — offbeat twinkles (eighths 2 & 5) + share replies ────────
function K(bar, e, midi, d, g) { twink(t(bar - 1, e), midi, d, g ?? 0.55); }

// skip1: light offbeat twinkles on eighths 2 & 5 of bars 3, 5, 7
K(3, 2, N.E6, 0.8, 0.42);  K(3, 5, N.G6, 0.8, 0.42);
K(5, 2, N.E6, 0.8, 0.42);  K(5, 5, N.G6, 0.8, 0.42);
K(7, 2, N.G6, 0.8, 0.45);  K(7, 5, N.E6, 0.8, 0.42);

// sweet: kalimba doubles the lead's offbeats lightly (every other bar)
for (const bar of [10, 12, 14, 16]) {
  K(bar, 2, N.C6, 0.8, 0.38);
  K(bar, 5, N.E6, 0.8, 0.38);
}

// share: this is the kalimba's moment — "one-for-ME" replies on bars 18,20,22,24
K(18, 0, N.G5, 1);  K(18, 1, N.A5, 1);                 K(18, 3, N.B5, 3);
K(20, 0, N.G5, 1);  K(20, 1, N.B5, 1);                 K(20, 3, N.D6, 3);
K(22, 0, N.A5, 1);  K(22, 1, N.C6, 1);                 K(22, 3, N.E6, 3);
K(24, 0, N.D5, 1);  K(24, 1, N.E5, 1);                 K(24, 3, N.G5, 3);

// skip2: twinkles on offbeats of bars 25, 27, 29
K(25, 5, N.E6, 0.8, 0.42);
K(26, 2, N.E6, 0.8, 0.40);  K(26, 5, N.G6, 0.8, 0.42);
K(27, 5, N.G6, 0.8, 0.42);
K(28, 2, N.E6, 0.8, 0.40);  K(28, 5, N.G6, 0.8, 0.42);
K(29, 1, N.G6, 0.5, 0.40);  K(29, 4, N.G6, 0.5, 0.40);
K(30, 5, N.E6, 0.8, 0.42);
K(31, 2, N.C6, 1, 0.40);
K(32, 1, N.E6, 1, 0.45);  K(32, 4, N.G5, 2, 0.42);

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
place(leadBuf,   0.00, 1.00);   // xylo center
place(bassBuf,   0.00, 0.92);   // bass center
place(bellBuf,   0.40, 0.82);   // glock right
place(twinkBuf, -0.30, 0.85);   // kalimba left

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

// trim trailing silence + tiny fades
let lastLoud = ns - 1;
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < 0.004 &&
       Math.abs(outR[lastLoud]) < 0.004) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.6 * SR));
const fadeIn = Math.floor(0.004 * SR);
const fadeOut = Math.floor(1.2 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ lollybap · ${TOTAL_BARS} bars · C major 6/8 @ ${BPM} BPM dotted · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "lollybap.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — same punchy daytime treatment as flutterbap:
// no low-shelf lift, gentle glue comp, a touch of air on top, brickwall.
const MASTER = [
  "highpass=f=35",
  "acompressor=threshold=-20dB:ratio=2.6:attack=18:release=180:makeup=2.2:knee=6",
  "treble=g=1.4:f=7500",
  "alimiter=limit=0.96:attack=4:release=70",
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
    _comment: "Section map for lollybap (schoolyard candy-stand skip). 6/8 compound, 124 BPM dotted-quarter → bar ≈ 0.968 s. C major. Derived from render-lollybap.mjs SECTIONS.",
    meter: 6, bpm: BPM, scale: "major", rootMidi: 60,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "lollybap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
