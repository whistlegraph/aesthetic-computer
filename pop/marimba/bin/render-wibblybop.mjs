#!/usr/bin/env node
// render-wibblybop.mjs — render the wibblybop wobble-jelly groove to mp3.
//
// Score notation lives in pop/marimba/wibblybop.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   vibraphone    — the wobbler; motor ON, tremolo rides every note
//   kalimba       — jiggly off-beat responses
//   bass          — bass marimba pulse on 1 & 3 (F / C)
//   rosewood      — held chord pad underneath on phrase tails
//
// Run:
//   node pop/marimba/bin/render-wibblybop.mjs
//   node pop/marimba/bin/render-wibblybop.mjs --out ~/wibblybop.mp3
//
// All timing in seconds. 116 BPM 4/4 → beat = 60/116 ≈ 0.517, bar ≈ 2.069.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 116;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (F major focus) ────────────────────────────────────────
const N = {
  F2: 41, C3: 48, F3: 53, G3: 55, A3: 57, Bb3: 58, C4: 60,
  F4: 65, G4: 67, A4: 69, Bb4: 70, C5: 72, D5: 74, E5: 76,
  F5: 77, G5: 79, A5: 81, Bb5: 82, C6: 84, D6: 86, E6: 88,
  F6: 89, G6: 91, A6: 93,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // vibraphone — the wobbler
const twinkBuf = new Float32Array(ns);   // kalimba — jiggly off-beats
const bassBuf  = new Float32Array(ns);   // bass marimba — F/C pulse
const warmBuf  = new Float32Array(ns);   // rosewood — held chord pad

// ── voice routes ──────────────────────────────────────────────────────
// vibraphone: motor ON gives the wobble. Long decay so the tremolo
// has room to breathe through each note.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "vibraphone", decayMul: 1.6 },
    leadBuf, { sampleRate: SR });
}
// kalimba: bright tine for jiggly off-beats. Tighten the ring slightly
// so each jiggle reads as its own pluck.
function twink(t0, midi, beats, gain = 0.6) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.85 },
    twinkBuf, { sampleRate: SR });
}
// bass: tight platter-rim pulse — not a droning sub.
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// rosewood: warm chord body under the wobble.
function warm(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "rosewood", decayMul: 1.0 },
    warmBuf, { sampleRate: SR });
}

// ── sections (matches wibblybop.np) ───────────────────────────────────
const SECTIONS = [
  { name: "wibble",    bar0: 0,  bars: 4 },
  { name: "jiggle",    bar0: 4,  bars: 8 },
  { name: "wobwobwob", bar0: 12, bars: 8 },
  { name: "shake",     bar0: 20, bars: 4 },
];

// ── LEAD line (vibraphone) — bar-by-bar from wibblybop.np ─────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § wibble (1–4) — sine-wave around F5-A5-C6-A5
L(1, 0, N.F5, 1);   L(1, 1, N.A5, 1);   L(1, 2, N.C6, 1);   L(1, 3, N.A5, 1);
L(2, 0, N.F5, 1);   L(2, 1, N.A5, 1);   L(2, 2, N.C6, 1);   L(2, 3, N.A5, 1);
L(3, 0, N.G5, 1);   L(3, 1, N.Bb5, 1);  L(3, 2, N.D6, 1);   L(3, 3, N.Bb5, 1);
L(4, 0, N.C6, 1.5); L(4, 1.5, N.A5, 1); L(4, 2.5, N.F5, 1.5);

// § jiggle (5–12) — accented 8ths ping-ponging F5/Bb5
L(5, 0, N.F5, 1);   L(5, 1, N.Bb5, 1);  L(5, 2, N.F5, 1);   L(5, 3, N.Bb5, 1);
L(6, 0, N.F5, 1);   L(6, 1, N.Bb5, 1);  L(6, 2, N.F5, 1);   L(6, 3, N.Bb5, 1);
L(7, 0, N.A5, 1);   L(7, 1, N.C6, 1);   L(7, 2, N.A5, 2);
L(8, 0, N.G5, 1);   L(8, 1, N.Bb5, 1);  L(8, 2, N.G5, 2);
L(9, 0, N.F5, 1);   L(9, 1, N.Bb5, 1);  L(9, 2, N.F5, 1);   L(9, 3, N.Bb5, 1);
L(10, 0, N.A5, 1);  L(10, 1, N.D6, 1);  L(10, 2, N.A5, 1);  L(10, 3, N.D6, 1);
L(11, 0, N.C6, 1);  L(11, 1, N.Bb5, 1); L(11, 2, N.A5, 1);  L(11, 3, N.G5, 1);
L(12, 0, N.F5, 1);  L(12, 1, N.A5, 1);  L(12, 2, N.C6, 2);

// § wobwobwob (13–20) — held tremolo notes, Bb (IV) chord change
L(13, 0, N.Bb5, 2);  L(13, 2, N.D6, 2);
L(14, 0, N.D6, 2);   L(14, 2, N.C6, 1);  L(14, 3, N.Bb5, 1);
L(15, 0, N.Bb5, 2);  L(15, 2, N.D6, 2);
L(16, 0, N.F6, 1);   L(16, 1, N.D6, 1);  L(16, 2, N.Bb5, 2);
L(17, 0, N.C6, 2);   L(17, 2, N.E6, 2);
L(18, 0, N.E6, 2);   L(18, 2, N.D6, 1);  L(18, 3, N.C6, 1);
L(19, 0, N.Bb5, 2);  L(19, 2, N.A5, 2);
L(20, 0, N.G5, 2);   L(20, 2, N.A5, 2);

// § shake (21–24) — descending F major scale home, final wobble fall
L(21, 0, N.F6, 1);   L(21, 1, N.E6, 1);  L(21, 2, N.D6, 1);  L(21, 3, N.C6, 1);
L(22, 0, N.Bb5, 1);  L(22, 1, N.A5, 1);  L(22, 2, N.G5, 1);  L(22, 3, N.F5, 1);
L(23, 0, N.A5, 2);   L(23, 2, N.F5, 2);
L(24, 0, N.F4, 4, 0.9);

// ── KALIMBA — jiggly off-beat responses ───────────────────────────────
function K(bar, b, midi, d, g) { twink(t(bar - 1, b), midi, d, g ?? 0.6); }

// jiggle: off-beat eighths riding the ping-pong (bars 5–12)
for (const bar of [5, 6, 9, 10]) {
  K(bar, 0.5, N.D6, 0.5, 0.5);
  K(bar, 1.5, N.F6, 0.5, 0.5);
  K(bar, 2.5, N.D6, 0.5, 0.5);
  K(bar, 3.5, N.F6, 0.5, 0.5);
}
K(7, 3.5, N.E6, 0.5, 0.55);
K(8, 3.5, N.D6, 0.5, 0.55);
K(11, 3.5, N.F6, 0.5, 0.55);
K(12, 3, N.E6, 0.5);  K(12, 3.5, N.G6, 0.5);

// wobwobwob: little tail twinkles on phrase ends (bars 13–20)
K(13, 3.5, N.F6, 0.5, 0.45);
K(15, 3.5, N.F6, 0.5, 0.45);
K(17, 3.5, N.G6, 0.5, 0.45);
K(19, 3.5, N.D6, 0.5, 0.45);
K(20, 2.5, N.C6, 0.5, 0.4);  K(20, 3.5, N.E6, 0.5, 0.4);

// shake: sparkles trailing the descent (bars 21–24)
K(21, 1.5, N.A6, 0.5, 0.45);   // soft top sparkle
K(22, 2.5, N.D6, 0.5, 0.45);
K(23, 1.5, N.C6, 0.5, 0.5);  K(23, 3.5, N.A5, 0.5, 0.5);
K(24, 1.5, N.F5, 0.5, 0.35); // one last drifting jiggle

// ── BASS — F/C pulse on 1 & 3 ─────────────────────────────────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.85); }

// wibble (1–4): F on 1, C on 3 — finger-tap platter rim
for (let bar = 1; bar <= 4; bar++) {
  B(bar, 0, N.F3, 1.6);
  B(bar, 2, N.C3, 1.6);
}
// jiggle (5–12): same pulse, slightly tighter — driving but uncluttered
for (let bar = 5; bar <= 12; bar++) {
  B(bar, 0, N.F3, 1.5);
  B(bar, 2, N.C3, 1.5);
  if (bar % 2 === 0) B(bar, 3.5, N.F4, 0.4, 0.55);  // off-beat grace skip
}
// wobwobwob (13–20): chord change — Bb on 1, F on 3 — bass drops
// to a held floor during the held tremolo bars
B(13, 0, N.Bb3, 4, 0.7);
B(14, 0, N.Bb3, 2, 0.7);  B(14, 2, N.F3, 2, 0.7);
B(15, 0, N.Bb3, 4, 0.7);
B(16, 0, N.Bb3, 2, 0.7);  B(16, 2, N.F3, 2, 0.7);
B(17, 0, N.C3, 4, 0.7);
B(18, 0, N.C3, 2, 0.7);   B(18, 2, N.G3, 2, 0.7);
B(19, 0, N.Bb3, 4, 0.7);
B(20, 0, N.C3, 4, 0.7);
// shake (21–24): a final pulse pair, then sustain into the low F home
B(21, 0, N.F3, 1.6);  B(21, 2, N.C3, 1.6);
B(22, 0, N.Bb3, 1.6); B(22, 2, N.F3, 1.6);
B(23, 0, N.F3, 4);
B(24, 0, N.F2, 4, 0.8);   // deep landing tone under the F4 lead

// ── ROSEWOOD — held chord pad on phrase tails ─────────────────────────
function R(bar, b, midi, d, g) { warm(t(bar - 1, b), midi, d, g ?? 0.55); }

// wibble (1–4): a soft F major triad held under bar 4 (the phrase tail)
R(4, 0, N.F4, 4, 0.45);
R(4, 0, N.A4, 4, 0.40);
R(4, 0, N.C5, 4, 0.40);

// wobwobwob (13–20): the chord body of the section — IV (Bb) then V (C)
// Bb major: Bb-D-F. Held two bars at a time.
R(13, 0, N.Bb4, 2, 0.5);  R(13, 0, N.D5, 2, 0.42); R(13, 0, N.F5, 2, 0.38);
R(15, 0, N.Bb4, 2, 0.5);  R(15, 0, N.D5, 2, 0.42); R(15, 0, N.F5, 2, 0.38);
// C major (V): C-E-G
R(17, 0, N.C5, 2, 0.5);   R(17, 0, N.E5, 2, 0.42); R(17, 0, N.G5, 2, 0.38);
// back toward F via Bb again
R(19, 0, N.Bb4, 2, 0.45); R(19, 0, N.D5, 2, 0.38); R(19, 0, N.F5, 2, 0.35);

// shake (21–24): F major chord ringing out under the descent + final home
R(23, 0, N.F4, 4, 0.45); R(23, 0, N.A4, 4, 0.40); R(23, 0, N.C5, 4, 0.40);
R(24, 0, N.F3, 4, 0.45); R(24, 0, N.A4, 4, 0.32); R(24, 0, N.C5, 4, 0.32);

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
place(leadBuf,   0.00, 1.00);   // vibraphone — center, the wobbler
place(twinkBuf,  0.38, 0.88);   // kalimba — right, jiggly
place(bassBuf,   0.00, 0.92);   // bass — center, platter pulse
place(warmBuf,  -0.30, 0.85);   // rosewood — left, chord body

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
const fadeOut = Math.floor(1.2 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ wibblybop · ${TOTAL_BARS} bars · F major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "wibblybop.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — bright, light, daytime. Same as flutterbap.
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
    _comment: "Section map for wibblybop (the wobble-jelly groove). 4/4, 116 BPM → bar ≈ 2.069 s. F major. Derived from render-wibblybop.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 65,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "wibblybop.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
