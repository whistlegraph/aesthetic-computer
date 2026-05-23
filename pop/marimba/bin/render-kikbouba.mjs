#!/usr/bin/env node
// render-kikbouba.mjs — render the kikbouba sound-symbolism study to mp3.
//
// Score notation lives in pop/marimba/kikbouba.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events, inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   woodblock  — the SHARP "kiki" hits  — dry, jagged-coded, panned L
//   gamelan    — the ROUND "bouba" hits — bronze ring,    panned R
//   xylophone  — the lead investigator, takes sides per phrase
//   kalimba    — the mediator twinkling between phrases
//
// Run:
//   node pop/marimba/bin/render-kikbouba.mjs
//   node pop/marimba/bin/render-kikbouba.mjs --out ~/kikbouba.mp3
//
// All timing in seconds. 84 BPM 5/4 → beat = 60/84 ≈ 0.714,
// bar = 5 * beat ≈ 3.571 s. Eighth = 0.5 beat ≈ 0.357 s.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 84;
const BEAT = 60 / BPM;
const BAR = 5 * BEAT;            // 5/4 — the asymmetry is the point
const TOTAL_BARS = 20;
const totalSec = TOTAL_BARS * BAR + 3.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
// beat can be 0..5 (because 5/4)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (A natural minor + dorian F#) ──────────────────────────
// pitch set: A B C D E F# G   (A natural minor with raised 6th)
const N = {
  A2: 45, A3: 57, B3: 59,
  C4: 60, D4: 62, E4: 64, F4: 65, "F#4": 66, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, "F#5": 78, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, "F#6": 90, G6: 91,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const sharpBuf    = new Float32Array(ns);  // woodblock — kiki strikes
const roundBuf    = new Float32Array(ns);  // gamelan   — bouba strikes
const leadBuf     = new Float32Array(ns);  // xylophone — investigator
const mediateBuf  = new Float32Array(ns);  // kalimba   — mediator twinkles

// ── voice routes ──────────────────────────────────────────────────────
// woodblock: short, dry, sharp. decayMul 0.35 → very fast clip.
function sharp(t0, midi, beats, gain = 0.95) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "woodblock", decayMul: 0.35 },
    sharpBuf, { sampleRate: SR });
}
// gamelan: long bronze ring — decayMul 1.5 so the bous breathe.
function round(t0, midi, beats, gain = 0.80) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "gamelan", decayMul: 1.5 },
    roundBuf, { sampleRate: SR });
}
// xylophone: lead investigator. Default ring 1.0.
function lead(t0, midi, beats, gain = 0.95) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "xylophone", decayMul: 1.0 },
    leadBuf, { sampleRate: SR });
}
// kalimba: bright tine mediator twinkles. decayMul 0.95.
function mediate(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.95 },
    mediateBuf, { sampleRate: SR });
}

// ── sections (matches kikbouba.np) ────────────────────────────────────
const SECTIONS = [
  { name: "kiki",   bar0: 0,  bars: 4 },
  { name: "bouba",  bar0: 4,  bars: 4 },
  { name: "which",  bar0: 8,  bars: 4 },
  { name: "are",    bar0: 12, bars: 4 },
  { name: "answer", bar0: 16, bars: 4 },
];

// Bar-by-bar helpers (1-based bars to match the .np doc).
// S = sharp (woodblock), R = round (gamelan), L = lead (xylophone), M = mediate (kalimba)
function S(bar, b, midi, d, g) { sharp(t(bar - 1, b),   midi, d, g ?? 0.95); }
function R(bar, b, midi, d, g) { round(t(bar - 1, b),   midi, d, g ?? 0.80); }
function L(bar, b, midi, d, g) { lead(t(bar - 1, b),    midi, d, g ?? 0.95); }
function M(bar, b, midi, d, g) { mediate(t(bar - 1, b), midi, d, g ?? 0.55); }

// ═════════════════════════════════════════════════════════════════════
//  § KIKI (bars 1–4) — SHARP side states itself.
//  woodblock strikes (2+3 eighth groupings); xylophone jagged ascending.
// ═════════════════════════════════════════════════════════════════════
// woodblock TOK on each beat 1 and 2 of the 5/4 bar, plus eighth jabs
for (let bar = 1; bar <= 4; bar++) {
  // five eighth-strikes per bar grouped (2 + 3)
  S(bar, 0,   N.C6, 0.5);
  S(bar, 0.5, N.C6, 0.5);
  S(bar, 1,   N.C6, 0.5);
  S(bar, 1.5, N.C6, 0.5);
  S(bar, 2,   N.C6, 0.5, 0.85);
  // a couple more sparser hits in the back half so the (2+3) reads
  S(bar, 3,   N.C6, 0.5, 0.70);
  S(bar, 4,   N.C6, 0.5, 0.70);
}
// xylophone JAGGED ascending steps — A5 B5 C6 with little zig-zags
// bar 1: ki-KI / ki-ki-KI
L(1, 0,   N.A5, 0.5);  L(1, 0.5, N.B5, 0.5);
L(1, 1.5, N.A5, 0.5);  L(1, 2,   N.B5, 0.5);  L(1, 2.5, N.C6, 0.5);
// bar 2: brighter — same shape, octave + bright
L(2, 0,   N.A5, 0.5);  L(2, 0.5, N.C6, 0.5);
L(2, 1.5, N.A5, 0.5);  L(2, 2,   N.B5, 0.5);  L(2, 2.5, N.C6, 0.5);
// bar 3: inverted — falling-rising
L(3, 0,   N.B5, 0.5);  L(3, 0.5, N.A5, 0.5);
L(3, 1.5, N.A5, 0.5);  L(3, 2,   N.C6, 0.5);  L(3, 2.5, N.B5, 0.5);
// bar 4: (3+2) inversion, final stab
L(4, 0,   N.A5, 0.5);  L(4, 0.5, N.B5, 0.5);  L(4, 1,   N.A5, 0.5);
L(4, 2,   N.A5, 0.5);  L(4, 2.5, N.C6, 1.0, 1.05);

// ═════════════════════════════════════════════════════════════════════
//  § BOUBA (bars 5–8) — ROUND side answers.
//  gamelan strikes on beat 1 and 4 of each 5/4 bar; xylophone humming descents.
// ═════════════════════════════════════════════════════════════════════
for (let bar = 5; bar <= 8; bar++) {
  R(bar, 0, N.E4, 4.5);   // round BONG on beat 1, long ring
  R(bar, 3, N.A3, 4.0, 0.70); // and a deeper second BONG on beat 4
}
// xylophone descending rounded hums E5 D5 C5
// bar 5: bou-bou-BA / bou-BA  (3+2)
L(5, 0,   N.E5, 0.5);  L(5, 0.5, N.D5, 0.5);  L(5, 1,   N.C5, 1.0);
L(5, 3,   N.E5, 0.5);  L(5, 3.5, N.C5, 0.5);
// bar 6: bou-BA / bou-bou-BA (2+3) wider
L(6, 0,   N.E5, 0.5);  L(6, 0.5, N.C5, 1.0);
L(6, 2,   N.E5, 0.5);  L(6, 2.5, N.D5, 0.5);  L(6, 3,   N.C5, 0.5);
// bar 7: long held vowels
L(7, 0,   N.E5, 2.0, 0.90);
L(7, 3,   N.D5, 1.0);  L(7, 4,   N.C5, 1.0);
// bar 8: 4+1 settling thud
L(8, 0,   N.E5, 0.5);  L(8, 0.5, N.D5, 0.5);  L(8, 1,   N.D5, 0.5);  L(8, 1.5, N.D5, 0.5);
L(8, 2,   N.C5, 1.5, 1.0);
// kalimba mediator twinkle at tail of bouba (eases into "which")
M(8, 4,   N.E6, 0.5);  M(8, 4.5, N.G5, 0.5);

// ═════════════════════════════════════════════════════════════════════
//  § WHICH (bars 9–12) — alternating SHARP + ROUND within the same bar.
// ═════════════════════════════════════════════════════════════════════
// Alternation: woodblock on odd eighths (KI), gamelan on even eighths (bou)
for (let bar = 9; bar <= 12; bar++) {
  S(bar, 0,   N.C6, 0.5);
  R(bar, 0.5, N.E4, 1.5, 0.65);
  S(bar, 1,   N.C6, 0.5);
  R(bar, 1.5, N.A3, 1.5, 0.60);
  S(bar, 2,   N.C6, 0.5);
  R(bar, 2.5, N.E4, 1.5, 0.60);
  S(bar, 3,   N.C6, 0.5, 0.85);
  R(bar, 3.5, N.D4, 1.5, 0.55);
  S(bar, 4,   N.C6, 0.5, 0.75);
}
// xylophone zigzag — first two bars jagged, last two rounded
// bar 9: A5-C6-B5-D6-A5 (jagged)
L(9,  0,   N.A5, 0.5);  L(9,  1,   N.C6, 0.5);
L(9,  2,   N.B5, 0.5);  L(9,  3,   N.D6, 0.5);  L(9,  4,   N.A5, 0.5);
// bar 10: E5-G5-A5-G5-D5 (round)
L(10, 0,   N.E5, 0.5);  L(10, 1,   N.G5, 0.5);
L(10, 2,   N.A5, 0.5);  L(10, 3,   N.G5, 0.5);  L(10, 4,   N.D5, 0.5);
// bar 11: jagged again — A5-D6-C6-B5-D6
L(11, 0,   N.A5, 0.5);  L(11, 1,   N.D6, 0.5);
L(11, 2,   N.C6, 0.5);  L(11, 3,   N.B5, 0.5);  L(11, 4,   N.D6, 0.5);
// bar 12: rounded — E5-G5-A5-G5-E5
L(12, 0,   N.E5, 0.5);  L(12, 1,   N.G5, 0.5);
L(12, 2,   N.A5, 0.5);  L(12, 3,   N.G5, 0.5);  L(12, 4,   N.E5, 0.5);
// kalimba twinkles between phrases
M(9,  4.5, N.E6, 0.5, 0.45);
M(11, 4.5, N.G6, 0.5, 0.45);

// ═════════════════════════════════════════════════════════════════════
//  § ARE (bars 13–16) — extended phrase, A dorian with raised F#.
//  woodblock + gamelan trade off; xylophone arpeggiates with F# pull.
// ═════════════════════════════════════════════════════════════════════
// bar 13: "WHICH / are-YOU" — long-then-short
S(13, 0,   N.C6, 1.5);                       // long WHICH (sharp)
R(13, 2,   N.A3, 3.0, 0.70);                 // long are (round)
S(13, 4,   N.C6, 0.5);                       // small ki at the end
L(13, 0,   N.A5, 1.0);
L(13, 1.5, N.B5, 0.5);
L(13, 2,   N.C6, 1.0);
L(13, 3.5, N.D6, 0.5);  L(13, 4,   N.A5, 1.0);
// bar 14: "are-you-are / YOU" — (3+2) escalating
R(14, 0,   N.E4, 1.5, 0.70);
S(14, 1.5, N.C6, 0.5);
R(14, 2,   N.A3, 3.0, 0.65);
S(14, 3.5, N.C6, 0.5);  S(14, 4,   N.C6, 0.5);
L(14, 0,   N.A5, 0.5);  L(14, 0.5, N.C6, 0.5);  L(14, 1,   N.A5, 0.5);
L(14, 2,   N.C6, 1.0);
// bar 15: "WHICH / are-you-YOU" — long-then-3, dorian F# appears!
S(15, 0,   N.C6, 1.0);
R(15, 1,   N.D4, 4.0, 0.65);          // chord pad = D (the IV)
L(15, 0,   N.E5, 1.0);
L(15, 1.5, N["F#5"], 0.5);            // the dorian flavor
L(15, 2,   N.G5, 0.5);
L(15, 3,   N.A5, 1.0);
// bar 16: "you-you-you / YOU-you" — 5 even sweeps, F#dim moment
S(16, 0,   N.C6, 0.5, 0.65);
R(16, 0.5, N["F#4"], 4.0, 0.60);      // F#dim (iiº) — tension
L(16, 0,   N.A5, 0.5);
L(16, 1,   N.G5, 0.5);
L(16, 2,   N["F#5"], 0.5);            // dorian again
L(16, 3,   N.E5, 1.0);
L(16, 4,   N.D5, 0.5);
M(16, 4.5, N.A5, 0.5);                // mediator twinkle into answer

// ═════════════════════════════════════════════════════════════════════
//  § ANSWER (bars 17–20) — hybrid resolution, all four voices interweave.
// ═════════════════════════════════════════════════════════════════════
// bar 17: ki-bou-KI / bou-BA — 3+2 hypothesis
S(17, 0,   N.C6, 0.5);
R(17, 0.5, N.E4, 2.0, 0.70);
S(17, 1,   N.C6, 0.5);
R(17, 2,   N.G4, 3.0, 0.65);          // chord pad G (VII)
S(17, 3,   N.C6, 0.5);
L(17, 0,   N.A5, 0.5);
L(17, 1,   N.E5, 0.5);
L(17, 1.5, N.B5, 0.5);
L(17, 3,   N.D5, 1.0);
L(17, 4.5, N.C6, 0.5);
M(17, 4,   N.E6, 0.5);
// bar 18: ki-bou-KI / ki-bou-BA — the experiment
S(18, 0,   N.C6, 0.5);
R(18, 0.5, N.E4, 1.5, 0.65);
S(18, 1.5, N.C6, 0.5);
S(18, 2,   N.C6, 0.5);
R(18, 2.5, N.A3, 2.5, 0.60);
S(18, 4,   N.C6, 0.5);
L(18, 0,   N.A5, 0.5);
L(18, 1,   N.E5, 0.5);
L(18, 2,   N.B5, 0.5);
L(18, 3,   N.A5, 0.5);
L(18, 3.5, N.E5, 0.5);
L(18, 4,   N.C6, 0.5);
M(18, 4.5, N.G5, 0.5);
// bar 19: ki-ki-bou / bou-BA-BA — uncertainty mixed signal
S(19, 0,   N.C6, 0.5);
S(19, 0.5, N.C6, 0.5);
R(19, 1,   N.D4, 1.5, 0.65);
R(19, 2,   N.A3, 3.0, 0.65);
S(19, 3,   N.C6, 0.5, 0.75);
R(19, 3.5, N.E4, 1.5, 0.60);
L(19, 0,   N.A5, 0.5);
L(19, 0.5, N.A5, 0.5);
L(19, 1,   N.E5, 0.5);
L(19, 1.5, N.D5, 0.5);
L(19, 2,   N.C5, 0.5);
L(19, 2.5, N.C5, 0.5);
M(19, 4,   N.E6, 0.5);
M(19, 4.5, N.A5, 0.5);
// bar 20: final TOK + final BONG together — settling on A
S(20, 0,   N.C6, 0.5, 1.10);                  // last sharp TOK
R(20, 0,   N.A2, 5.0, 0.90);                  // last round BONG, deep A2
L(20, 0,   N.A4, 5.0, 0.95);                  // long held A4 — home
M(20, 1.5, N.E6, 0.5, 0.50);
M(20, 3,   N.A5, 0.5, 0.50);

// ═════════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN
// ═════════════════════════════════════════════════════════════════════
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
// woodblock left, gamelan right, xylophone center, kalimba slight-right
place(sharpBuf,   -0.40, 0.95);
place(roundBuf,    0.35, 0.90);
place(leadBuf,     0.00, 1.00);
place(mediateBuf,  0.10, 0.80);

// scrub non-finite + peak-normalise to -1.3 dBFS
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
const fadeOut = Math.floor(1.4 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ kikbouba · ${TOTAL_BARS} bars · A natural minor (dorian-flavored) 5/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "kikbouba.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — mid-energy, room for the asymmetric grooves.
// Same shape as flutterbap: gentle glue comp + air + brickwall.
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
    _comment: "Section map for kikbouba (the sound-symbolism study). 5/4, 84 BPM → bar ≈ 3.571 s. A natural minor with quasi-dorian (raised F#). Derived from render-kikbouba.mjs SECTIONS.",
    meter: 5, bpm: BPM, scale: "natural-minor-dorian", rootMidi: 69,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "kikbouba.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
