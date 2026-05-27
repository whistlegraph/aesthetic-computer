#!/usr/bin/env node
// render-slinkybap.mjs — render the slinkybap staircase tumble to mp3.
//
// Score notation lives in pop/marimba/slinkybap.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   xylophone     — the lead; each note is one slinky-coil pinging the
//                   next stair (down on the tumbles, up on the climb)
//   bass          — low G/D pulse on 1 & 3 (root + fifth)
//   woodblock     — every stair-clack on the tumbles
//   kalimba       — the metal-coil ring on phrase tails
//
// Run:
//   node pop/marimba/bin/render-slinkybap.mjs
//   node pop/marimba/bin/render-slinkybap.mjs --out ~/slinkybap.mp3
//
// All timing in seconds. 96 BPM 4/4 → beat = 60/96 = 0.625, bar = 2.5.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 96;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (G major: G A B C D E F#) ──────────────────────────────
const N = {
  G2: 43, D3: 50, G3: 55,
  G4: 67, A4: 69, B4: 71, C5: 72, D5: 74, E5: 76, "F#5": 78,
  G5: 79, A5: 81, B5: 83, C6: 84, D6: 86, E6: 88,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // xylophone — slinky coil pings
const bassBuf  = new Float32Array(ns);   // bass marimba — root/fifth pulse
const clackBuf = new Float32Array(ns);   // woodblock — stair clacks
const ringBuf  = new Float32Array(ns);   // kalimba — phrase-tail ring

// ── voice routes ──────────────────────────────────────────────────────
// xylophone is naturally short — extend slightly so held notes still
// carry across a full bar without sounding choked.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "xylophone", decayMul: 1.15 },
    leadBuf, { sampleRate: SR });
}
// bass: low G/D pulse — tight, not droning. Shorten the natural
// bass-marimba ring so each pulse is distinct.
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// woodblock: each stair-clack. Very short — punchy, not ringing.
// Pitched between -0.20 and -0.40 register (we pass a low midi so the
// woodblock preset sits in its dry click range).
function clack(t0, midi, beats, gain = 0.65) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "woodblock", decayMul: 0.4 },
    clackBuf, { sampleRate: SR });
}
// kalimba: metal-coil ring on phrase tails.
function ring(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.95 },
    ringBuf, { sampleRate: SR });
}

// ── sections (matches slinkybap.np) ───────────────────────────────────
const SECTIONS = [
  { name: "top",      bar0: 0,  bars: 4 },
  { name: "tumble1",  bar0: 4,  bars: 6 },
  { name: "climb",    bar0: 10, bars: 4 },
  { name: "tumble2",  bar0: 14, bars: 6 },
  { name: "rest",     bar0: 20, bars: 4 },
];

// ── LEAD line (xylophone) — bar-by-bar from slinkybap.np ──────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § top (1–4) — "slin-ky DOG / at-the-TOP / one-STEP / ONE-step"
L(1, 0, N.G5, 1);   L(1, 1, N.B5, 1);   L(1, 2, N.D6, 2);
L(2, 0, N.B5, 1);   L(2, 1, N.D6, 1);   L(2, 2, N.D6, 2);
L(3, 0, N.G5, 1);   L(3, 1, N.B5, 1);   L(3, 2, N.D6, 2);
L(4, 0, N.D6, 4, 0.9);

// § tumble1 (5–10) — DESCENDING G major scale from D6 down to G4
//   bar 5:  D6 C6 B5 A5
L(5, 0, N.D6, 1);   L(5, 1, N.C6, 1);   L(5, 2, N.B5, 1);   L(5, 3, N.A5, 1);
//   bar 6:  G5 F#5 E5 D5
L(6, 0, N.G5, 1);   L(6, 1, N["F#5"], 1); L(6, 2, N.E5, 1); L(6, 3, N.D5, 1);
//   bar 7:  C5 B4 A4 G4
L(7, 0, N.C5, 1);   L(7, 1, N.B4, 1);   L(7, 2, N.A4, 1);   L(7, 3, N.G4, 1);
//   bar 8:  G4 held — the floor
L(8, 0, N.G4, 4, 0.9);
//   bar 9:  wider drop — D6 B5 G5 D5
L(9, 0, N.D6, 1);   L(9, 1, N.B5, 1);   L(9, 2, N.G5, 1);   L(9, 3, N.D5, 1);
//   bar 10: G4 held — the BO-i-OING settle
L(10, 0, N.G4, 4, 0.85);

// § climb (11–14) — ASCENDING G major scale, quarter notes
//   bar 11: G4 A4 B4 C5
L(11, 0, N.G4, 1);  L(11, 1, N.A4, 1);  L(11, 2, N.B4, 1);  L(11, 3, N.C5, 1);
//   bar 12: D5 E5 F#5 G5
L(12, 0, N.D5, 1);  L(12, 1, N.E5, 1);  L(12, 2, N["F#5"], 1); L(12, 3, N.G5, 1);
//   bar 13: A5 B5 C6 D6
L(13, 0, N.A5, 1);  L(13, 1, N.B5, 1);  L(13, 2, N.C6, 1);  L(13, 3, N.D6, 1);
//   bar 14: D6 held — back to top
L(14, 0, N.D6, 4, 0.9);

// § tumble2 (15–20) — second descent, faster (16ths in middle bars)
//   bar 15: D6 C6 B5 A5
L(15, 0, N.D6, 1);  L(15, 1, N.C6, 1);  L(15, 2, N.B5, 1);  L(15, 3, N.A5, 1);
//   bar 16: G5 F#5 E5 D5
L(16, 0, N.G5, 1);  L(16, 1, N["F#5"], 1); L(16, 2, N.E5, 1); L(16, 3, N.D5, 1);
//   bar 17: 8-step descent in eighth-notes (= "16ths!" feel at 96 BPM)
L(17, 0,   N.D6, 0.5); L(17, 0.5, N.C6, 0.5);
L(17, 1,   N.B5, 0.5); L(17, 1.5, N.A5, 0.5);
L(17, 2,   N.G5, 0.5); L(17, 2.5, N["F#5"], 0.5);
L(17, 3,   N.E5, 0.5); L(17, 3.5, N.D5, 0.5);
//   bar 18: bottom-out — D5 C5 B4 A4 then G4 repeated
L(18, 0,   N.D5, 0.5); L(18, 0.5, N.C5, 0.5);
L(18, 1,   N.B4, 0.5); L(18, 1.5, N.A4, 0.5);
L(18, 2,   N.G4, 0.5); L(18, 2.5, N.G4, 0.5);
L(18, 3,   N.G4, 0.5); L(18, 3.5, N.G4, 0.5);
//   bar 19: ka-LUNK ka-LUNK — D5 G4 D5 G4
L(19, 0, N.D5, 1);  L(19, 1, N.G4, 1);  L(19, 2, N.D5, 1);  L(19, 3, N.G4, 1);
//   bar 20: BOI-oi OING — B4 G4 G4
L(20, 0, N.B4, 1);  L(20, 1, N.G4, 1);  L(20, 2, N.G4, 2);

// § rest (21–24) — final wobble settling on long G4
//   bar 21: rest-ing HERE — G4 (held), B4, G4
L(21, 0, N.G4, 2, 0.85); L(21, 2, N.B4, 1); L(21, 3, N.G4, 1);
//   bar 22: long G4 ring — kalimba twinkles take over
L(22, 0, N.G4, 4, 0.7);
//   bar 23: small bo-ing — G4 B4
L(23, 0, N.G4, 1);  L(23, 1, N.B4, 1);  L(23, 2, N.G4, 2, 0.8);
//   bar 24: final held G4
L(24, 0, N.G4, 4, 0.85);

// ── BASS — low G/D pulse on 1 & 3 ─────────────────────────────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.85); }
// top: classic root/fifth — G2 on 1, D3 on 3
for (let bar = 1; bar <= 4; bar++) {
  B(bar, 0, N.G2, 1.6);
  B(bar, 2, N.D3, 1.6);
}
// tumble1: pulse continues, slightly louder — gives the descent gravity
for (let bar = 5; bar <= 10; bar++) {
  B(bar, 0, N.G2, 1.6, 0.9);
  B(bar, 2, N.D3, 1.4, 0.85);
  // tiny G3 grace on the &-of-4 on bars 6 and 8 — slinky-skip feel
  if (bar === 6 || bar === 8) B(bar, 3.5, N.G3, 0.4, 0.55);
}
// climb: pulse stays — root only on the climb-up bars 11–13, full again at 14
B(11, 0, N.G2, 4, 0.7);
B(12, 0, N.G2, 4, 0.7);
B(13, 0, N.D3, 4, 0.7);
B(14, 0, N.G2, 1.6);  B(14, 2, N.D3, 1.6);
// tumble2: classic pulse again — second descent gets the same gravity
for (let bar = 15; bar <= 20; bar++) {
  B(bar, 0, N.G2, 1.6, 0.9);
  B(bar, 2, N.D3, 1.4, 0.85);
}
// rest: one quiet G2 per bar, fading out
B(21, 0, N.G2, 1.6, 0.7);
B(21, 2, N.D3, 1.4, 0.6);
B(22, 0, N.G2, 4, 0.6);
B(23, 0, N.G2, 4, 0.55);
B(24, 0, N.G2, 4, 0.65);   // deep landing tone under the G4 lead

// ── WOODBLOCK — every stair-clack on the tumbles ──────────────────────
// Pitched low (between -0.20 and -0.40 register — using midi 50–55 range,
// a low D3–G3 so the woodblock preset sits in its dry click range).
function W(bar, b, midi, d, g) { clack(t(bar - 1, b), midi, d, g ?? 0.65); }
// tumble1: a clack on every stair (every beat the lead walks down)
const tumble1ClackPitch = [N.G3, N.G3, N.G3, N.G3, N.G3, N.G3];  // dry, consistent
// bars 5,6,7,9 — four-stair bars get a clack on every beat
for (const bar of [5, 6, 7, 9]) {
  for (let beat = 0; beat < 4; beat++) {
    W(bar, beat, tumble1ClackPitch[0], 0.25, 0.7);
  }
}
// bar 8 and 10 — held floor; a single ka-thunk on beat 1 only
W(8, 0, N.D3, 0.3, 0.55);
W(10, 0, N.D3, 0.3, 0.5);

// tumble2: bars 15, 16, 17, 18 — every step clacks. 17 + 18 = eighth-notes
for (const bar of [15, 16]) {
  for (let beat = 0; beat < 4; beat++) W(bar, beat, N.G3, 0.25, 0.75);
}
for (const bar of [17, 18]) {
  for (let h = 0; h < 8; h++) W(bar, h * 0.5, N.G3, 0.2, 0.7);
}
// bar 19: ka-LUNK ka-LUNK — two heavier clacks on 1 and 3
W(19, 0, N.D3, 0.3, 0.85);
W(19, 2, N.D3, 0.3, 0.85);
// bar 20: tiny tail clacks — BOI-oi OING
W(20, 0, N.G3, 0.25, 0.7);
W(20, 1, N.G3, 0.25, 0.55);

// ── KALIMBA — metal-coil ring on phrase tails ─────────────────────────
function K(bar, b, midi, d, g) { ring(t(bar - 1, b), midi, d, g ?? 0.55); }
// top: a soft halo at the end of each phrase
K(2, 3.5, N.D6, 0.5);
K(4, 1, N.D6, 0.5);  K(4, 2.5, N.G5, 0.5);
// tumble1: ring on the tail of every descending phrase
K(5, 3.5, N.G5, 0.5, 0.45);
K(6, 3.5, N.D5, 0.5, 0.45);
K(7, 3.5, N.G4, 0.5, 0.5);
K(8, 1.5, N.D5, 0.5, 0.45);  K(8, 3, N.G5, 0.5, 0.4);
K(9, 3.5, N.D5, 0.5, 0.5);
K(10, 1, N.G5, 0.5, 0.45);  K(10, 2.5, N.D6, 0.5, 0.4);  K(10, 3.5, N.G5, 0.5, 0.4);
// climb: off-beat sparkles riding the ascent
K(11, 1.5, N.G5, 0.5, 0.4);  K(11, 3.5, N.D5, 0.5, 0.4);
K(12, 1.5, N.A5, 0.5, 0.4);  K(12, 3.5, N.G5, 0.5, 0.4);
K(13, 1.5, N.D6, 0.5, 0.45); K(13, 3.5, N.G5, 0.5, 0.4);
K(14, 0, N.D6, 0.5, 0.55);   K(14, 2, N.G5, 0.5, 0.45);
// tumble2: tails on the faster descent
K(16, 3.5, N.D5, 0.5, 0.4);
K(18, 3.5, N.G4, 0.5, 0.45);
K(20, 2.5, N.D5, 0.5, 0.4);  K(20, 3.5, N.G5, 0.5, 0.4);
// rest: trailing twinkles into the night
K(22, 0, N.D5, 0.5, 0.5);   K(22, 1.5, N.G5, 0.5, 0.4);
K(22, 3, N.B5, 0.5, 0.35);
K(23, 2.5, N.D5, 0.5, 0.35);
K(24, 1, N.G5, 0.5, 0.4);   K(24, 2.5, N.D5, 0.5, 0.3);

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
place(leadBuf,   0.00, 1.00);   // xylophone — center
place(bassBuf,   0.00, 0.92);   // bass — center
place(clackBuf, -0.35, 0.85);   // woodblock — left of center
place(ringBuf,   0.38, 0.82);   // kalimba — right of center

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

console.log(`→ slinkybap · ${TOTAL_BARS} bars · G major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "slinkybap.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — bright, light, daytime. No low-shelf lift (no kick
// to flatter), gentle glue comp, a touch of air on top, brickwall.
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
    _comment: "Section map for slinkybap (the afternoon staircase tumble). 4/4, 96 BPM → bar = 2.5 s. G major. Derived from render-slinkybap.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 67,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "slinkybap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
