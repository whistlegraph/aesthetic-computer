#!/usr/bin/env node
// render-dingaba.mjs — render the dingaba doorbell-welcome to mp3.
//
// Score notation lives in pop/marimba/dingaba.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   glockenspiel  — the doorbell itself; chiming lead
//   bass          — heartbeat pulse on 1 & 3 (low Eb / Bb)
//   kalimba       — offbeat sparkle (sunlight on the welcome mat)
//   rosewood      — warm thirds underneath on phrase tails
//
// Run:
//   node pop/marimba/bin/render-dingaba.mjs
//   node pop/marimba/bin/render-dingaba.mjs --out ~/dingaba.mp3
//
// All timing in seconds. 110 BPM 4/4 → beat = 60/110 ≈ 0.545, bar ≈ 2.18.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 110;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final bell
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table — Eb major emphasis ────────────────────────────────────
const N = {
  Eb2: 39, Bb2: 46,
  Eb3: 51, F3: 53, G3: 55, Ab3: 56, Bb3: 58,
  Eb4: 63, F4: 65, G4: 67, Ab4: 68, Bb4: 70, C5_low: 72,
  D5: 74, Eb5: 75, F5: 77, G5: 79, Ab5: 80, Bb5: 82,
  C6: 84, D6: 86, Eb6: 87, F6: 89, G6: 91,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // glockenspiel — the doorbell
const bassBuf  = new Float32Array(ns);   // bass marimba — heartbeat
const sparkBuf = new Float32Array(ns);   // kalimba — sunlight sparkle
const warmBuf  = new Float32Array(ns);   // rosewood — warm thirds

// ── voice routes ──────────────────────────────────────────────────────
// glockenspiel: high steel chimes — let the doorbell ring out
function lead(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "glockenspiel", decayMul: 1.4 },
    leadBuf, { sampleRate: SR });
}
// bass: gentle heartbeat — short ring so each pulse is distinct
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// kalimba: bright tine for off-beat twinkles
function spark(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.85 },
    sparkBuf, { sampleRate: SR });
}
// rosewood: warm wooden thirds — gentle support underneath
function warm(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "rosewood", decayMul: 0.9 },
    warmBuf, { sampleRate: SR });
}

// ── sections (matches dingaba.np) ─────────────────────────────────────
const SECTIONS = [
  { name: "chime",   bar0: 0,  bars: 4 },
  { name: "hello",   bar0: 4,  bars: 8 },
  { name: "friend",  bar0: 12, bars: 8 },
  { name: "inside",  bar0: 20, bars: 4 },
];

// ── LEAD line (glockenspiel) — bar-by-bar from dingaba.np ─────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 0.85); }

// § chime (1–4) — the doorbell rings
// "ding-DONG-ding" — descending bell, then a quiet bar, then reply, then quiet
L(1, 0, N.Bb5, 1);  L(1, 1, N.G5, 1);   L(1, 2, N.Eb5, 2);
// bar 2: silent — the listening pause (bass pulses underneath)
L(3, 0, N.Eb5, 1);  L(3, 1, N.G5, 1);   L(3, 2, N.Bb5, 2);
// bar 4: silent — footsteps coming (kalimba tip-tap)

// § hello (5–12) — climbing answer, eighth-feel
L(5, 0, N.G5, 1);   L(5, 1, N.Bb5, 1);  L(5, 2, N.G5, 1);  L(5, 3, N.Eb5, 1);
L(6, 0, N.F5, 1);   L(6, 1, N.Ab5, 1);  L(6, 2, N.Bb5, 2);
L(7, 0, N.G5, 1);   L(7, 1, N.Bb5, 1);  L(7, 2, N.G5, 1);  L(7, 3, N.Eb5, 1);
L(8, 0, N.F5, 1);   L(8, 1, N.G5, 1);   L(8, 2, N.Ab5, 2);
L(9, 0, N.Bb5, 1);  L(9, 1, N.C6, 1);   L(9, 2, N.Bb5, 1); L(9, 3, N.Ab5, 1);
L(10, 0, N.G5, 1);  L(10, 1, N.Bb5, 1); L(10, 2, N.Eb6, 2);
L(11, 0, N.G5, 1);  L(11, 1, N.Bb5, 1); L(11, 2, N.G5, 1); L(11, 3, N.Eb5, 1);
L(12, 0, N.F5, 1);  L(12, 1, N.G5, 1);  L(12, 2, N.Bb5, 2);

// § friend (13–20) — the warm welcome
L(13, 0, N.Eb5, 1); L(13, 1, N.G5, 1);  L(13, 2, N.Bb5, 2);
L(14, 0, N.Ab5, 1); L(14, 1, N.G5, 1);  L(14, 2, N.F5, 2);
L(15, 0, N.Eb5, 1); L(15, 1, N.G5, 1);  L(15, 2, N.Bb5, 2);
L(16, 0, N.C6, 2);  L(16, 2, N.Bb5, 2);
L(17, 0, N.G5, 1);  L(17, 1, N.Ab5, 1); L(17, 2, N.Bb5, 2);
L(18, 0, N.C6, 1);  L(18, 1, N.Bb5, 1); L(18, 2, N.G5, 2);
L(19, 0, N.Bb5, 1); L(19, 1, N.G5, 1);  L(19, 2, N.Eb5, 2);
L(20, 0, N.F5, 1);  L(20, 1, N.G5, 1);  L(20, 2, N.Ab5, 1); L(20, 3, N.Bb5, 1);

// § inside (21–24) — settling cadence I-IV-V-I
L(21, 0, N.Eb5, 1); L(21, 1, N.G5, 1);  L(21, 2, N.Bb5, 1); L(21, 3, N.Eb6, 1);
L(22, 0, N.Ab5, 1); L(22, 1, N.C6, 1);  L(22, 2, N.Ab5, 2);
L(23, 0, N.Bb5, 1); L(23, 1, N.D6, 1);  L(23, 2, N.Bb5, 1); L(23, 3, N.F5, 1);
L(24, 0, N.Eb5, 1); L(24, 1, N.G5, 1);  L(24, 2, N.Bb5, 1); L(24, 3, N.Eb6, 2);

// ── BASS — heartbeat pulse on 1 & 3 (low Eb / Bb alternation) ─────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.85); }
// chime: gentle held pulses — Eb3 on bar 1/3 starts, low Bb on rests
B(1, 0, N.Eb3, 2);  B(1, 2, N.Bb2, 2);
B(2, 0, N.Eb3, 4, 0.7);
B(3, 0, N.Eb3, 2);  B(3, 2, N.Bb2, 2);
B(4, 0, N.Eb3, 4, 0.7);
// hello: classic heartbeat, alternating low Eb / Bb octave on 1 & 3
for (let bar = 5; bar <= 12; bar++) {
  B(bar, 0, N.Eb3, 1.6);
  B(bar, 2, N.Bb2, 1.6);
}
// friend: same heartbeat, slightly fuller — adds a low Ab on the IV bars
for (let bar = 13; bar <= 20; bar++) {
  B(bar, 0, N.Eb3, 1.6);
  B(bar, 2, N.Bb2, 1.6);
}
// bar 14 + 18 hint Ab below (IV color)
B(14, 0, N.Ab3, 1.6, 0.7);
B(18, 0, N.Ab3, 1.6, 0.7);
// inside: the I-IV-V-I cadence in the bass
B(21, 0, N.Eb3, 2);  B(21, 2, N.Bb2, 2);
B(22, 0, N.Ab3, 2);  B(22, 2, N.Ab3, 2, 0.75);
B(23, 0, N.Bb2, 2);  B(23, 2, N.F3, 2, 0.7);
B(24, 0, N.Eb3, 2);  B(24, 2, N.Eb2, 2, 0.8);   // deep landing under the lead

// ── KALIMBA — offbeat sparkle (sunlight on the welcome mat) ───────────
function K(bar, b, midi, d, g) { spark(t(bar - 1, b), midi, d, g ?? 0.55); }
// chime: tip-tap footsteps in the silent bars
K(2, 1, N.Bb5, 0.5);  K(2, 2, N.G5, 0.5);   K(2, 3, N.Bb5, 0.5);
K(4, 1, N.G5, 0.5);   K(4, 2, N.Bb5, 0.5);  K(4, 3, N.C6, 0.5);
// hello: offbeat 8ths riding the question — only on every other bar
for (const bar of [5, 7, 9, 11]) {
  K(bar, 0.5, N.Eb6, 0.5, 0.45);
  K(bar, 2.5, N.C6,  0.5, 0.45);
}
// hello tails: little three-note response
K(8, 3.5, N.Bb5, 0.5);
K(10, 3.5, N.G6, 0.5);
K(12, 3.5, N.Eb6, 0.5);
// friend: gentle sparkle on phrase tails
K(13, 3.5, N.Eb6, 0.5);
K(15, 3.5, N.Eb6, 0.5);
K(16, 3, N.G6, 0.5);
K(17, 3.5, N.C6, 0.5);
K(18, 3.5, N.Bb5, 0.5);
K(20, 3.5, N.Eb6, 0.5);
// inside: a small descending sparkle into the home
K(21, 3.5, N.G6, 0.5, 0.5);
K(22, 3.5, N.F6, 0.5, 0.5);
K(23, 3.5, N.Eb6, 0.5, 0.5);
K(24, 1.5, N.G6, 0.5, 0.5);
K(24, 2.5, N.Bb5, 0.5, 0.4);

// ── ROSEWOOD — warm thirds underneath on phrase tails ─────────────────
function R(bar, b, midi, d, g) { warm(t(bar - 1, b), midi, d, g ?? 0.55); }
// rosewood only enters with the friend section — the welcome
// thirds: Eb-G (I), Ab-C (IV), Bb-D (V) — drawn from the lead chord
R(13, 2, N.G4, 2, 0.55);   R(13, 2, N.Eb4, 2, 0.5);
R(14, 0, N.Ab4, 2, 0.55);  R(14, 0, N.F4, 2, 0.5);
R(15, 2, N.G4, 2, 0.55);   R(15, 2, N.Eb4, 2, 0.5);
R(16, 0, N.Bb4, 4, 0.55);  R(16, 0, N.G4, 4, 0.5);
R(17, 2, N.G4, 2, 0.55);   R(17, 2, N.Eb4, 2, 0.5);
R(18, 0, N.Ab4, 2, 0.55);  R(18, 0, N.F4, 2, 0.5);
R(19, 2, N.G4, 2, 0.55);   R(19, 2, N.Eb4, 2, 0.5);
R(20, 0, N.Bb4, 4, 0.55);  R(20, 0, N.D5, 4, 0.45);
// inside: I-IV-V-I in warm thirds underneath the cadence
R(21, 0, N.G4, 4, 0.55);   R(21, 0, N.Eb4, 4, 0.5);
R(22, 0, N.C5_low, 4, 0.55); R(22, 0, N.Ab4, 4, 0.5);
R(23, 0, N.D5, 4, 0.55);   R(23, 0, N.Bb4, 4, 0.5);
R(24, 0, N.G4, 4, 0.6);    R(24, 0, N.Eb4, 4, 0.55);

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
place(leadBuf,   0.00, 1.00);   // glockenspiel — center, the doorbell
place(bassBuf,   0.00, 0.92);   // bass — center, the heartbeat
place(sparkBuf,  0.35, 0.85);   // kalimba — right, the sparkle
place(warmBuf,  -0.30, 0.88);   // rosewood — left, the warmth

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

console.log(`→ dingaba · ${TOTAL_BARS} bars · Eb major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "dingaba.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — bright, cheerful midtempo. Mirror of flutterbap's chain.
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
    _comment: "Section map for dingaba (the doorbell music-box welcome). 4/4, 110 BPM → bar ≈ 2.18 s. Eb major. Derived from render-dingaba.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 63,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "dingaba.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
