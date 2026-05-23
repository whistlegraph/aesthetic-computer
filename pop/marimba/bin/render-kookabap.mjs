#!/usr/bin/env node
// render-kookabap.mjs — render the kookabap dawn cackle to mp3.
//
// Score notation lives in pop/marimba/kookabap.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   kelon       — the laugher; bright synthetic-marimba lead
//   bass        — low pulse (A2 / E3 alternating on 1 & 3)
//   kalimba     — dawn-twinkles, offbeat sparkle showers
//   woodblock   — the cackle, hard wooden chk-chk on offbeats
//
// Run:
//   node pop/marimba/bin/render-kookabap.mjs
//   node pop/marimba/bin/render-kookabap.mjs --out ~/kookabap.mp3
//
// All timing in seconds. 138 BPM 4/4 → beat = 60/138 ≈ 0.435, bar ≈ 1.739.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 138;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (A major: A B C# D E F# G#) ────────────────────────────
const N = {
  A2: 45, E3: 52, A3: 57,
  C4: 60, "C#4": 61, D4: 62, E4: 64, "F#4": 66, "G#4": 68, A4: 69, B4: 71,
  "C#5": 73, D5: 74, E5: 76, "F#5": 78, "G#5": 80, A5: 81, B5: 83,
  "C#6": 85, D6: 86, E6: 88, "F#6": 90, "G#6": 92, A6: 93,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const kelonBuf = new Float32Array(ns);   // kelon — the bright laugher
const bassBuf  = new Float32Array(ns);   // bass marimba — low pulse
const sparkBuf = new Float32Array(ns);   // kalimba — dawn twinkles
const woodBuf  = new Float32Array(ns);   // woodblock — cackle clicks

// ── voice routes ──────────────────────────────────────────────────────
// kelon: bright synthetic marimba — let the natural ring carry.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kelon", decayMul: 1.0 },
    kelonBuf, { sampleRate: SR });
}
// bass: tight low pulse — shorten the natural bass-marimba ring so
// each pulse is distinct.
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// kalimba: bright tine for dawn twinkles.
function spark(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.95 },
    sparkBuf, { sampleRate: SR });
}
// woodblock: short hard click — the cackle itself. Tight decay.
function wood(t0, midi, beats, gain = 0.7) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "woodblock", decayMul: 0.45 },
    woodBuf, { sampleRate: SR });
}

// ── sections (matches kookabap.np) ────────────────────────────────────
const SECTIONS = [
  { name: "wake",   bar0: 0,  bars: 4 },
  { name: "laugh",  bar0: 4,  bars: 8 },
  { name: "dawn",   bar0: 12, bars: 8 },
  { name: "chorus", bar0: 20, bars: 4 },
];

// ── LEAD line (kelon) — bar-by-bar from kookabap.np ───────────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § wake (1–4) — ascending major arpeggios
L(1, 0, N.E5, 1);    L(1, 1, N.A5, 1);    L(1, 2, N["C#6"], 2);
L(2, 0, N.A5, 1);    L(2, 1, N["C#6"], 1); L(2, 2, N.E6, 2);
L(3, 0, N["C#5"], 1); L(3, 1, N.E5, 1);   L(3, 2, N.A5, 1);   L(3, 3, N["C#6"], 1);
L(4, 0, N.E6, 2);    L(4, 2, N["C#6"], 1); L(4, 3, N.A5, 1);

// § laugh (5–12) — cackle bursts
L(5, 0, N.A5, 1);    L(5, 1, N["C#6"], 1); L(5, 2, N.A5, 1);   L(5, 3, N.E6, 1);
L(6, 0, N["C#6"], 1); L(6, 1, N.A5, 1);   L(6, 2, N.E6, 1);   L(6, 3, N["C#6"], 1);
L(7, 0, N.A5, 1);    L(7, 1, N["C#6"], 1); L(7, 2, N.E6, 1);   L(7, 3, N.A5, 1);
L(8, 0, N.E6, 1);    L(8, 1, N["C#6"], 1); L(8, 2, N.A5, 1);   L(8, 3, N.E5, 1);
L(9, 0, N.A5, 1);    L(9, 1, N.D6, 1);    L(9, 2, N.A5, 1);   L(9, 3, N["F#6"], 1);
L(10, 0, N.D6, 1);   L(10, 1, N.A5, 1);   L(10, 2, N["F#6"], 1); L(10, 3, N.D6, 1);
L(11, 0, N.A5, 1);   L(11, 1, N.E6, 1);   L(11, 2, N["C#6"], 2);
L(12, 0, N.B5, 1);   L(12, 1, N["C#6"], 1); L(12, 2, N.E6, 1);  L(12, 3, N.A5, 1);

// § dawn (13–20) — lifted, held high notes
L(13, 0, N.E6, 2);   L(13, 2, N["C#6"], 2);
L(14, 0, N.A5, 1);   L(14, 1, N.B5, 1);   L(14, 2, N["C#6"], 1); L(14, 3, N.E6, 1);
L(15, 0, N["F#6"], 2); L(15, 2, N.E6, 2);
L(16, 0, N.A5, 1);   L(16, 1, N["C#6"], 1); L(16, 2, N.E6, 1);  L(16, 3, N.A6, 1);
L(17, 0, N.A6, 4, 0.95);
L(18, 0, N.E6, 1);   L(18, 1, N["F#6"], 1); L(18, 2, N["G#6"], 1); L(18, 3, N.A6, 1);
L(19, 0, N.A6, 4, 0.9);
L(20, 0, N["G#6"], 1); L(20, 1, N.E6, 1);   L(20, 2, N["C#6"], 1); L(20, 3, N.A5, 1);

// § chorus (21–24) — ensemble outro, descend to A4 home
L(21, 0, N.A5, 1);   L(21, 1, N["C#6"], 1); L(21, 2, N.E6, 1);  L(21, 3, N.A6, 1);
L(22, 0, N.E6, 1);   L(22, 1, N.D6, 1);   L(22, 2, N["C#6"], 1); L(22, 3, N.B5, 1);
L(23, 0, N.A5, 1);   L(23, 1, N["G#5"], 1); L(23, 2, N["F#5"], 1); L(23, 3, N.E5, 1);
L(24, 0, N.A4, 4, 0.9);

// ── BASS — low pulse on 1 & 3 ─────────────────────────────────────────
// Pattern follows the I-IV-V-vi loose harmony: A (root) and E (5th)
// alternating, with D and F#m hints across the laugh + dawn sections.
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.85); }
// wake: classic bouncing pulse, low A / E alternation
for (let bar = 1; bar <= 4; bar++) {
  B(bar, 0, N.A2, 1.6);
  B(bar, 2, N.E3, 1.6);
}
// laugh: bouncing returns with D / F# colorings on bars 9-10 (IV / vi)
B(5, 0, N.A2, 1.6);  B(5, 2, N.E3, 1.6);
B(6, 0, N.A2, 1.6);  B(6, 2, N.E3, 1.6);
B(7, 0, N.A2, 1.6);  B(7, 2, N.E3, 1.6);
B(8, 0, N.A2, 1.6);  B(8, 2, N.E3, 1.6);
B(9, 0, N.D4 - 12, 1.6); B(9, 2, N.A2, 1.6);          // D2 root (IV)
B(10, 0, N["F#4"] - 24, 1.6, 0.8); B(10, 2, N["C#5"] - 24, 1.4, 0.75); // F#2 / C#3 (vi)
B(11, 0, N.E3, 1.6); B(11, 2, N.A2, 1.6);             // V → I
B(12, 0, N.A2, 1.6); B(12, 2, N.E3, 1.6);
// dawn: lighter pulse, one held A per bar at moderate volume
for (let bar = 13; bar <= 20; bar++) B(bar, 0, N.A2, 4, 0.6);
// chorus: full ensemble — bouncing returns
B(21, 0, N.A2, 1.6);  B(21, 2, N.E3, 1.6);
B(22, 0, N.D4 - 12, 1.6); B(22, 2, N.A2, 1.6);
B(23, 0, N.E3, 1.6);  B(23, 2, N.A2, 1.6);
B(24, 0, N.A2 - 12, 4, 0.85);   // A1 deep landing under the A4 lead

// ── KALIMBA — dawn twinkles + sparkle showers ─────────────────────────
function K(bar, b, midi, d, g) { spark(t(bar - 1, b), midi, d, g ?? 0.55); }
// laugh: little response sparkles in the tails of each bar
K(5, 3.5, N.E6, 0.5);
K(6, 3.5, N["C#6"], 0.5);
K(7, 3.5, N.A5, 0.5);
K(8, 1, N["C#6"], 0.5);  K(8, 2, N.E6, 0.5);  K(8, 3, N["C#6"], 0.5);
K(9, 3.5, N["F#6"], 0.5);
K(10, 3.5, N.D6, 0.5);
K(11, 3.5, N.E6, 0.5);
K(12, 3.5, N.A6, 0.5);
// dawn: sparkle haloes above the held high notes
K(13, 0, N.A6, 0.5);  K(13, 1, N.E6, 0.5);  K(13, 2, N["C#6"], 0.5);  K(13, 3, N.A6, 0.5);
K(14, 0.5, N.E6, 0.5, 0.45);  K(14, 2.5, N["C#6"], 0.5, 0.45);
K(15, 0, N.A6, 0.5);  K(15, 1, N["F#6"], 0.5);  K(15, 2, N.E6, 0.5);  K(15, 3, N.A6, 0.5);
K(16, 0.5, N.E6, 0.5, 0.45);  K(16, 2.5, N.A6, 0.5, 0.45);
K(17, 0, N["C#6"], 0.5);  K(17, 1, N.E6, 0.5);  K(17, 2, N.A6, 0.5);  K(17, 3, N.E6, 0.5);
K(18, 0.5, N["C#6"], 0.5, 0.45);
K(19, 0, N.A6, 0.5);  K(19, 1, N["G#6"], 0.5);  K(19, 2, N["F#6"], 0.5);  K(19, 3, N.E6, 0.5);
K(20, 0.5, N["C#6"], 0.5, 0.45);
// chorus: drifting sparkle on the descent
K(21, 3.5, N.A6, 0.5);
K(22, 3.5, N.E6, 0.5);
K(23, 3.5, N["C#6"], 0.5);
K(24, 1.5, N.E6, 0.5, 0.45);   // one last drifting sparkle over the low A

// ── WOODBLOCK — cackle clicks on offbeats ─────────────────────────────
function W(bar, b, midi, d, g) { wood(t(bar - 1, b), midi, d, g ?? 0.7); }
// wake: subtle clicks on the &-of-2 to establish the dawn pulse
W(1, 1.5, N.C5, 0.25, 0.5);  W(1, 3.5, N.C5, 0.25, 0.5);
W(2, 1.5, N.C5, 0.25, 0.5);  W(2, 3.5, N.C5, 0.25, 0.5);
W(3, 1.5, N.C5, 0.25, 0.5);  W(3, 3.5, N.C5, 0.25, 0.5);
W(4, 1.5, N.C5, 0.25, 0.5);  W(4, 3.5, N.C5, 0.25, 0.55);
// laugh: heavy cackle bursts on every &-of-beat
for (let bar = 5; bar <= 12; bar++) {
  W(bar, 0.5, N.C5, 0.25);
  W(bar, 1.5, N["C#5"], 0.25);
  W(bar, 2.5, N.C5, 0.25);
  W(bar, 3.5, N["C#5"], 0.25);
}
// dawn: lighter clicks, only on &-of-2
for (let bar = 13; bar <= 20; bar++) {
  W(bar, 1.5, N.C5, 0.25, 0.45);
  W(bar, 3.5, N.C5, 0.25, 0.45);
}
// chorus: bring back the cackle for the outro
for (let bar = 21; bar <= 23; bar++) {
  W(bar, 0.5, N.C5, 0.25, 0.65);
  W(bar, 1.5, N["C#5"], 0.25, 0.65);
  W(bar, 2.5, N.C5, 0.25, 0.65);
  W(bar, 3.5, N["C#5"], 0.25, 0.65);
}
W(24, 0, N.C5, 0.25, 0.5);   // final tiny cluck on the landing

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
place(kelonBuf,  0.00, 1.00);
place(bassBuf,   0.00, 0.92);
place(sparkBuf,  0.38, 0.85);
place(woodBuf,  -0.32, 0.80);

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

console.log(`→ kookabap · ${TOTAL_BARS} bars · A major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "kookabap.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — bright, light, daytime. No low-shelf lift, gentle
// glue comp, a touch of air on top, brickwall.
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
    _comment: "Section map for kookabap (the kookaburra-dawn cackle). 4/4, 138 BPM → bar ≈ 1.739 s. A major. Derived from render-kookabap.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 69,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "kookabap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
