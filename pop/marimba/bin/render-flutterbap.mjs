#!/usr/bin/env node
// render-flutterbap.mjs — render the flutterbap daytime hop to mp3.
//
// Score notation lives in pop/marimba/flutterbap.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   xylophone     — the singer; bright daytime syllabic lead
//   bass          — the bouncing ball (low C / G alternating on 1 & 3)
//   kalimba       — twinkles on phrase tails (off-beat sparkle)
//   glockenspiel  — high steel sparkle on mommy-wow + the fly climaxes
//
// Run:
//   node pop/marimba/bin/render-flutterbap.mjs
//   node pop/marimba/bin/render-flutterbap.mjs --out ~/flutterbap.mp3
//
// All timing in seconds. 124 BPM 4/4 → beat = 60/124 ≈ 0.484, bar ≈ 1.935.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 124;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 32;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

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
const bassBuf  = new Float32Array(ns);   // bass marimba — bouncing ball
const sparkBuf = new Float32Array(ns);   // kalimba — off-beat twinkles
const bellBuf  = new Float32Array(ns);   // glockenspiel — high sparkle

// ── voice routes ──────────────────────────────────────────────────────
// xylophone is naturally short — extend slightly so held notes still
// carry across a full bar without sounding choked.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "xylophone", decayMul: 1.15 },
    leadBuf, { sampleRate: SR });
}
// bass: tight bouncing-ball — not a droning sub. Shorten the natural
// bass-marimba ring so each bounce is distinct.
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// kalimba: bright tine for off-beat twinkles. Default ring works.
function spark(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.95 },
    sparkBuf, { sampleRate: SR });
}
// glockenspiel: high steel, let it shimmer.
function bell(t0, midi, beats, gain = 0.65) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "glockenspiel", decayMul: 1.35 },
    bellBuf, { sampleRate: SR });
}

// ── sections (matches flutterbap.np) ──────────────────────────────────
const SECTIONS = [
  { name: "butterfly",  bar0: 0,  bars: 4 },
  { name: "palofmine",  bar0: 4,  bars: 4 },
  { name: "mommywow",   bar0: 8,  bars: 4 },
  { name: "slinky",     bar0: 12, bars: 8 },
  { name: "fly",        bar0: 20, bars: 8 },
  { name: "land",       bar0: 28, bars: 4 },
];

// ── LEAD line (xylophone) — bar-by-bar from flutterbap.np ─────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § butterfly (1–4) — "but-ter-FLY / cos-PLAY-er / but-ter-FLY / FLY-by"
L(1, 0, N.E5, 1);   L(1, 1, N.G5, 1);   L(1, 2, N.C6, 2);
L(2, 0, N.C6, 1);   L(2, 1, N.E6, 1);   L(2, 2, N.C6, 2);
L(3, 0, N.D5, 1);   L(3, 1, N.G5, 1);   L(3, 2, N.B5, 2);
L(4, 0, N.C6, 2);   L(4, 2, N.G5, 2);

// § palofmine (5–8) — "but-ter-FLY / pal-of-MINE / fly-with-me / BY-the-blue"
L(5, 0, N.E5, 1);   L(5, 1, N.G5, 1);   L(5, 2, N.C6, 2);
L(6, 0, N.A5, 1);   L(6, 1, N.G5, 1);   L(6, 2, N.F5, 2);
L(7, 0, N.E5, 1);   L(7, 1, N.F5, 1);   L(7, 2, N.G5, 1);   L(7, 3, N.A5, 1);
L(8, 0, N.C6, 4);

// § mommywow (9–12) — held wonder
L(9, 0, N.G5, 4, 0.9);
L(10, 0, N.C6, 1);  L(10, 1, N.E6, 1);  L(10, 2, N.G6, 2);
L(11, 0, N.G5, 4, 0.9);
L(12, 0, N.G6, 1);  L(12, 1, N.E6, 1);  L(12, 2, N.C6, 1);  L(12, 3, N.G6, 1);

// § slinky (13–20) — wobbly springing
L(13, 0, N.C5, 1);  L(13, 1, N.E5, 1);  L(13, 2, N.G5, 2);
L(14, 0, N.G5, 1);  L(14, 1, N.F5, 1);  L(14, 2, N.E5, 1);  L(14, 3, N.D5, 1);
L(15, 0, N.C5, 1);  L(15, 1, N.E5, 1);  L(15, 2, N.G5, 2);
// bar 16 — ascending scale, eighth-notes
L(16, 0,   N.C5, 0.5); L(16, 0.5, N.D5, 0.5);
L(16, 1,   N.E5, 0.5); L(16, 1.5, N.F5, 0.5);
L(16, 2,   N.G5, 0.5); L(16, 2.5, N.A5, 0.5);
L(16, 3,   N.B5, 0.5); L(16, 3.5, N.C6, 0.5);
// bar 17 — descending scale, eighth-notes
L(17, 0,   N.C6, 0.5); L(17, 0.5, N.B5, 0.5);
L(17, 1,   N.A5, 0.5); L(17, 1.5, N.G5, 0.5);
L(17, 2,   N.F5, 0.5); L(17, 2.5, N.E5, 0.5);
L(17, 3,   N.D5, 0.5); L(17, 3.5, N.C5, 0.5);
L(18, 0, N.C5, 1);  L(18, 1, N.E5, 1);  L(18, 2, N.G5, 1);  L(18, 3, N.C6, 1);
L(19, 0, N.C6, 1);  L(19, 1, N.A5, 1);  L(19, 2, N.F5, 1);  L(19, 3, N.D5, 1);
L(20, 0, N.E5, 1);  L(20, 1, N.G5, 1);  L(20, 2, N.C5, 2);

// § fly (21–28) — "lately-when-I-FLY"
L(21, 0, N.C5, 1);  L(21, 1, N.D5, 1);  L(21, 2, N.E5, 1);  L(21, 3, N.G5, 1);
L(22, 0, N.C6, 4);
L(23, 0, N.E5, 1);  L(23, 1, N.F5, 1);  L(23, 2, N.G5, 1);  L(23, 3, N.A5, 1);
L(24, 0, N.D6, 4);
L(25, 0, N.E6, 1);  L(25, 1, N.D6, 1);  L(25, 2, N.C6, 1);  L(25, 3, N.G5, 1);
L(26, 0, N.A5, 1);  L(26, 1, N.C6, 1);  L(26, 2, N.G6, 2);
L(27, 0, N.G6, 1);  L(27, 1, N.E6, 1);  L(27, 2, N.C6, 1);  L(27, 3, N.A5, 1);
L(28, 0, N.G5, 1);  L(28, 1, N.E5, 1);  L(28, 2, N.C5, 2);

// § land (29–32) — settling
L(29, 0, N.C6, 1);  L(29, 1, N.G5, 1);  L(29, 2, N.E5, 1);  L(29, 3, N.C5, 1);
L(30, 0, N.D5, 1);  L(30, 1, N.C5, 3);
L(31, 0, N.E5, 1);  L(31, 1, N.D5, 1);  L(31, 2, N.C5, 2);
L(32, 0, N.C4, 4, 0.9);

// ── BASS — bouncing-ball pulse on 1 & 3 ───────────────────────────────
// Quiet during mommywow (held wonder) and dropped on the soaring fly bars.
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.85); }
// butterfly + palofmine: classic bouncing-ball, low/high octave alternation
for (let bar = 1; bar <= 8; bar++) {
  B(bar, 0, N.C3, 1.6);
  B(bar, 2, N.G3, 1.6);
}
// mommywow: one held low C per bar at lower volume — the floor of the wow
for (let bar = 9; bar <= 12; bar++) B(bar, 0, N.C3, 4, 0.55);
// slinky: bouncing returns, with a tiny C4 grace note on the &-of-4 for skip
for (let bar = 13; bar <= 20; bar++) {
  B(bar, 0, N.C3, 1.6);
  B(bar, 2, N.G3, 1.4);
  if (bar % 2 === 0) B(bar, 3.5, N.C4, 0.4, 0.6);
}
// fly: bass drops out on the soaring bars (22, 24, 26, 28); walking on the others
B(21, 0, N.C3, 1.6);  B(21, 2, N.G3, 1.6);
B(23, 0, N.C3, 1.6);  B(23, 2, N.G3, 1.6);
B(25, 0, N.C3, 1.6);  B(25, 2, N.G3, 1.6);
B(27, 0, N.C3, 1.6);  B(27, 2, N.G3, 1.6);
// land: a final bounce, then sustain into the low C4 landing
B(29, 0, N.C3, 1.6);  B(29, 2, N.G3, 1.6);
B(30, 0, N.C3, 3);
B(31, 0, N.C3, 2);
B(32, 0, N.C2, 4, 0.75);   // deep landing tone under the C4 lead

// ── KALIMBA — twinkles on phrase tails + slinky off-beats ─────────────
function K(bar, b, midi, d, g) { spark(t(bar - 1, b), midi, d, g ?? 0.55); }
// palofmine: little three-note response in the tail of each bar
K(5, 3.5, N.C6, 0.5);
K(6, 3.5, N.A5, 0.5);
K(7, 3.5, N.G5, 0.5);
K(8, 1, N.E6, 0.5);  K(8, 2, N.G6, 0.5);  K(8, 3, N.E6, 0.5);
// slinky: offbeat 8ths riding the wobble — only on the bounce bars (not the scales)
for (const bar of [13, 14, 15, 18, 19, 20]) {
  K(bar, 0.5, N.E6, 0.5, 0.45);
  K(bar, 2.5, N.C6, 0.5, 0.45);
}
// fly: sparkle haloes above the held high notes
K(22, 0, N.E6, 0.5);  K(22, 1, N.G6, 0.5);  K(22, 2, N.E6, 0.5);  K(22, 3, N.C6, 0.5);
K(24, 0, N.F6, 0.5);  K(24, 1, N.A5, 0.5);  K(24, 2, N.D6, 0.5);  K(24, 3, N.F6, 0.5);
K(26, 3, N.E6, 0.5);
K(28, 2.5, N.C6, 0.5); K(28, 3.5, N.E6, 0.5);
// land: a tiny goodnight twinkle on bar 30 and 31
K(30, 3.5, N.E5, 0.5);
K(31, 3.5, N.G5, 0.5);
K(32, 1.5, N.C6, 0.5, 0.4);   // one last drifting sparkle over the low C

// ── GLOCKENSPIEL — high steel halo on mommywow + fly ──────────────────
function G(bar, b, midi, d, g) { bell(t(bar - 1, b), midi, d, g ?? 0.65); }
// mommywow: ring out the wow chord above the held G5
G(9, 0, N.E6, 4, 0.4);
G(10, 2, N.G6, 2, 0.85);
G(11, 0, N.E6, 4, 0.4);
G(12, 0, N.G6, 1, 0.85);  G(12, 3, N.C6, 1, 0.75);
// fly: shimmer trails above the soaring notes
G(22, 0, N.G6, 4, 0.6);
G(24, 0, N.G6, 4, 0.6);
G(26, 2, N.G6, 2, 0.6);
G(28, 0, N.G5, 2, 0.45);
// land: a small final chime ringing the home C
G(32, 0, N.C6, 4, 0.5);
G(32, 1, N.E6, 3, 0.4);

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
place(leadBuf,   0.00, 1.00);
place(bassBuf,   0.00, 0.92);
place(sparkBuf,  0.38, 0.85);
place(bellBuf,  -0.32, 0.80);

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

console.log(`→ flutterbap · ${TOTAL_BARS} bars · C major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "flutterbap.mp3");
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
    _comment: "Section map for flutterbap (the daytime butterfly hop). 4/4, 124 BPM → bar ≈ 1.935 s. C major. Derived from render-flutterbap.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 60,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "flutterbap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
