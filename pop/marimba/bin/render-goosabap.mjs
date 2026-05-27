#!/usr/bin/env node
// render-goosabap.mjs — render the goosabap migrating-geese pop track to mp3.
//
// Score notation lives in pop/marimba/goosabap.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   vibraphone   — the lead goose; long held honks with motor breath
//   bass         — wing-beat pulse (low G / D alternating on 1 & 3)
//   roll         — sustained mid-register pad: the V itself, gliding
//   rosewood     — soft counter-melody on phrase tails (second goose)
//
// Run:
//   node pop/marimba/bin/render-goosabap.mjs
//   node pop/marimba/bin/render-goosabap.mjs --out ~/goosabap.mp3
//
// All timing in seconds. 92 BPM 4/4 → beat = 60/92 ≈ 0.652, bar ≈ 2.609.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 92;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 28;
const totalSec = TOTAL_BARS * BAR + 3.5;   // + tail for final low-G horizon fade
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table ────────────────────────────────────────────────────────
const N = {
  G2: 43, D3: 50,
  G3: 55, A3: 57, B3: 59,
  C4: 60, D4: 62, E4: 64, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, G6: 91,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf    = new Float32Array(ns);   // vibraphone — lead goose
const bassBuf    = new Float32Array(ns);   // bass marimba — wing-beat
const padBuf     = new Float32Array(ns);   // roll — formation pad
const counterBuf = new Float32Array(ns);   // rosewood — counter-melody

// ── voice routes ──────────────────────────────────────────────────────
// vibraphone: long held honks with motor breath; let it ring long.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "vibraphone", decayMul: 1.4 },
    leadBuf, { sampleRate: SR });
}
// bass: wing-beat pump on 1 & 3. Short, distinct, never droning.
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// roll: sustained mid-pad — the V gliding across the sky.
function pad(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "roll", decayMul: 1.2 },
    padBuf, { sampleRate: SR });
}
// rosewood: soft warm counter-melody when the second goose answers.
function counter(t0, midi, beats, gain = 0.60) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "rosewood", decayMul: 0.95 },
    counterBuf, { sampleRate: SR });
}

// ── sections (matches goosabap.np) ────────────────────────────────────
const SECTIONS = [
  { name: "call",    bar0: 0,  bars: 4 },
  { name: "reply",   bar0: 4,  bars: 6 },
  { name: "fly",     bar0: 10, bars: 8 },
  { name: "south",   bar0: 18, bars: 6 },
  { name: "horizon", bar0: 24, bars: 4 },
];

// ── LEAD line (vibraphone) — bar-by-bar from goosabap.np ──────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § call (1–4) — "honk-HONK / honk-HONK / honk-HONK / HONK"
L(1, 0, N.D5, 2);                 L(1, 2, N.B4, 2);
L(2, 0, N.G5, 2);                 L(2, 2, N.D5, 2);
L(3, 0, N.D5, 2);                 L(3, 2, N.B4, 2);
L(4, 0, N.G5, 4);

// § reply (5–10) — second goose answers, V forms
L(5, 0, N.E5, 2);                 L(5, 2, N.C5, 2);
L(6, 0, N.A5, 2);                 L(6, 2, N.E5, 2);
L(7, 0, N.D5, 2);                 L(7, 2, N.G5, 2);
L(8, 0, N.G5, 2);                 L(8, 2, N.A5, 2);
L(9, 0, N.B5, 4);
L(10, 0, N.D6, 4);

// § fly (11–18) — wing-beat climbs
L(11, 0, N.G5, 1);  L(11, 1, N.A5, 1);  L(11, 2, N.B5, 2);
L(12, 0, N.B5, 1);  L(12, 1, N.A5, 1);  L(12, 2, N.G5, 2);
L(13, 0, N.A5, 1);  L(13, 1, N.B5, 1);  L(13, 2, N.D6, 2);
L(14, 0, N.D6, 1);  L(14, 1, N.B5, 1);  L(14, 2, N.A5, 2);
L(15, 0, N.G5, 1);  L(15, 1, N.A5, 1);  L(15, 2, N.B5, 2);
L(16, 0, N.D6, 2);  L(16, 2, N.A5, 2);
L(17, 0, N.G5, 1);  L(17, 1, N.A5, 1);  L(17, 2, N.B5, 2);
L(18, 0, N.D6, 4);

// § south (19–24) — cruising on G major chord tones
L(19, 0, N.D5, 1);  L(19, 1, N.G5, 1);  L(19, 2, N.B5, 2);
L(20, 0, N.G5, 1);  L(20, 1, N.D5, 1);  L(20, 2, N.B4, 2);
L(21, 0, N.D5, 1);  L(21, 1, N.G5, 1);  L(21, 2, N.D6, 2);
L(22, 0, N.D6, 1);  L(22, 1, N.B5, 1);  L(22, 2, N.G5, 2);
L(23, 0, N.G5, 1);  L(23, 1, N.B5, 1);  L(23, 2, N.D6, 2);
L(24, 0, N.B5, 1);  L(24, 1, N.G5, 1);  L(24, 2, N.D5, 2);

// § horizon (25–28) — long descending phrase to home
L(25, 0, N.G6, 2);  L(25, 2, N.D6, 1);  L(25, 3, N.B5, 1);
L(26, 0, N.G5, 4);
L(27, 0, N.D5, 4);
L(28, 0, N.G4, 4, 0.9);

// ── BASS — wing-beat pulse on 1 & 3 (the chest air-pump) ──────────────
// Quiet during the held-honk bars (4, 9, 10, 18); steady on the rest.
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.85); }
// call (1–4): low G / D wing-beat
for (let bar = 1; bar <= 3; bar++) {
  B(bar, 0, N.G2, 1.6);
  B(bar, 2, N.D3, 1.6);
}
B(4, 0, N.G2, 2.8, 0.7);   // one slow held wing-beat under the long honk
// reply (5–10): full wing-beat through the answer, soften at the held bars
for (let bar = 5; bar <= 8; bar++) {
  B(bar, 0, N.G2, 1.6);
  B(bar, 2, N.D3, 1.6);
}
B(9, 0, N.G2, 2.8, 0.65);
B(10, 0, N.D3, 2.8, 0.65);
// fly (11–18): firm wing-beat with downbeat emphasis (wing flaps)
for (let bar = 11; bar <= 17; bar++) {
  B(bar, 0, N.G2, 1.6, 0.95);   // downbeat emphasis
  B(bar, 2, N.D3, 1.4, 0.80);
}
B(18, 0, N.G2, 2.8, 0.70);
// south (19–24): cruising bass — G and D alternating
for (let bar = 19; bar <= 24; bar++) {
  B(bar, 0, N.G2, 1.6);
  B(bar, 2, N.D3, 1.6);
}
// horizon (25–28): one settle, then deep low G underneath
B(25, 0, N.G2, 1.6);  B(25, 2, N.D3, 1.6);
B(26, 0, N.G2, 3, 0.75);
B(27, 0, N.D3, 3, 0.70);
B(28, 0, N.G2, 4, 0.75);   // deep home tone under the G4 lead

// ── PAD (roll) — sustained mid-register formation glide ───────────────
// Enters at reply (bar 5), holds across phrase, follows I-V-IV-I.
function P(bar, b, midi, d, g) { pad(t(bar - 1, b), midi, d, g ?? 0.55); }
// reply: pad enters underneath, chord on G (I), then D (V)
P(5, 0, N.G3, 4, 0.45);    P(5, 0, N.B3, 4, 0.40);    P(5, 0, N.D4, 4, 0.40);
P(6, 0, N.G3, 4, 0.45);    P(6, 0, N.B3, 4, 0.40);    P(6, 0, N.D4, 4, 0.40);
P(7, 0, N.D4, 4, 0.45);    P(7, 0, N.A3, 4, 0.40);    P(7, 0, N.F5, 4, 0.35);   // F# implied via D7-ish, use A+D
P(8, 0, N.D4, 4, 0.45);    P(8, 0, N.A3, 4, 0.40);
P(9, 0, N.G3, 4, 0.50);    P(9, 0, N.B3, 4, 0.45);    P(9, 0, N.D4, 4, 0.45);
P(10, 0, N.G3, 4, 0.50);   P(10, 0, N.B3, 4, 0.45);   P(10, 0, N.D4, 4, 0.45);
// fly: pad holds across, brighter — C major (IV) and G (I) alternating
for (const [bar, root, third, fifth] of [
  [11, N.G3, N.B3, N.D4],
  [12, N.G3, N.B3, N.D4],
  [13, N.C4, N.E4, N.G4],
  [14, N.C4, N.E4, N.G4],
  [15, N.G3, N.B3, N.D4],
  [16, N.D4, N.A3, N.E4],   // V passing
  [17, N.G3, N.B3, N.D4],
  [18, N.G3, N.B3, N.D4],
]) {
  P(bar, 0, root, 4, 0.50);
  P(bar, 0, third, 4, 0.42);
  P(bar, 0, fifth, 4, 0.42);
}
// south: pad on G - Em - C - D - G - D progression
for (const [bar, root, third, fifth] of [
  [19, N.G3, N.B3, N.D4],
  [20, N.E4, N.G4, N.B3],   // Em (E G B)
  [21, N.C4, N.E4, N.G4],
  [22, N.D4, N.A3, N.E4],   // D (D A) — F# implied by lead
  [23, N.G3, N.B3, N.D4],
  [24, N.D4, N.A3, N.E4],
]) {
  P(bar, 0, root, 4, 0.50);
  P(bar, 0, third, 4, 0.42);
  P(bar, 0, fifth, 4, 0.42);
}
// horizon: pad thins to two voices, drops to home G as it fades
P(25, 0, N.G3, 4, 0.45);   P(25, 0, N.D4, 4, 0.40);
P(26, 0, N.G3, 4, 0.40);   P(26, 0, N.B3, 4, 0.35);
P(27, 0, N.D3, 4, 0.35);   P(27, 0, N.G3, 4, 0.32);
P(28, 0, N.G3, 4, 0.30);   // pad fades into the long G

// ── ROSEWOOD — soft counter-melody (the second goose) ─────────────────
function R(bar, b, midi, d, g) { counter(t(bar - 1, b), midi, d, g ?? 0.60); }
// enters at fly (11) — answers the lead a few beats behind
R(11, 1, N.D5, 1, 0.50);
R(12, 1, N.G5, 1, 0.50);
R(13, 1, N.E5, 1, 0.50);
R(14, 1, N.G5, 1, 0.50);
R(15, 1.5, N.D5, 0.5, 0.45);  R(15, 2.5, N.G5, 1, 0.55);
R(16, 1, N.B5, 1, 0.55);      R(16, 3, N.G5, 1, 0.50);
R(17, 1.5, N.D5, 0.5, 0.45);  R(17, 2.5, N.G5, 1, 0.55);
R(18, 1, N.B5, 1, 0.55);      R(18, 3, N.A5, 1, 0.50);
// south: weave a steady counter on chord tones underneath the cruise
R(19, 2.5, N.G5, 1.5, 0.55);
R(20, 0.5, N.E5, 1, 0.50);    R(20, 2.5, N.D5, 1.5, 0.55);
R(21, 1.5, N.G5, 1, 0.50);    R(21, 3, N.B5, 1, 0.50);
R(22, 0.5, N.A5, 1, 0.55);    R(22, 3, N.G5, 1, 0.50);
R(23, 1.5, N.E5, 1, 0.50);    R(23, 3, N.D5, 1, 0.50);
R(24, 0.5, N.G5, 1, 0.50);    R(24, 2.5, N.G4, 1.5, 0.55);
// horizon: small descending echo behind the lead
R(25, 2, N.B5, 1, 0.55);      R(25, 3, N.G5, 1, 0.50);
R(26, 1.5, N.D5, 1.5, 0.45);
R(27, 1.5, N.B4, 1.5, 0.40);
R(28, 1, N.G4, 2, 0.40);

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
// vibraphone center, bass center, pad slightly right, rosewood slightly left
place(leadBuf,     0.00, 1.00);
place(bassBuf,     0.00, 0.92);
place(padBuf,      0.25, 0.78);
place(counterBuf, -0.30, 0.82);

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

// trim trailing silence + long 2s fade for the horizon
let lastLoud = ns - 1;
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < 0.003 &&
       Math.abs(outR[lastLoud]) < 0.003) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.8 * SR));
const fadeIn = Math.floor(0.005 * SR);
const fadeOut = Math.floor(2.0 * SR);   // long horizon fade
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ goosabap · ${TOTAL_BARS} bars · G major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "goosabap.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — mid-softened. Gentle low cut, smoother glue comp,
// a small lift up top to keep the V shimmer, brickwall.
const MASTER = [
  "highpass=f=30",
  "acompressor=threshold=-20dB:ratio=2.4:attack=22:release=220:makeup=1.8:knee=6",
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
    _comment: "Section map for goosabap (autumn migrating geese). 4/4, 92 BPM → bar ≈ 2.609 s. G major. Derived from render-goosabap.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 67,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "goosabap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
