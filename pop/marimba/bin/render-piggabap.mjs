#!/usr/bin/env node
// render-piggabap.mjs — render the piggabap barnyard chase to mp3.
//
// Score notation lives in pop/marimba/piggabap.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   xylophone   — the pig's high voice (squeals — the lead)
//   bass        — low F/C oinks (the deep grunt)
//   staccato    — sharp dry snorts (percussive marimba slap)
//   woodblock   — clip-clop trotters (the chase)
//
// Run:
//   node pop/marimba/bin/render-piggabap.mjs
//   node pop/marimba/bin/render-piggabap.mjs --out ~/piggabap.mp3
//
// All timing in seconds. 144 BPM 4/4 → beat = 60/144 ≈ 0.417, bar ≈ 1.667.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 144;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final snore
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table — F natural minor (F G Ab Bb C Db Eb) ──────────────────
const N = {
  F2: 41, C3: 48, F3: 53, G3: 55, Ab3: 56, Bb3: 58,
  C4: 60, Db4: 61, Eb4: 63, F4: 65, G4: 67, Ab4: 68, Bb4: 70,
  C5: 72, Db5: 73, D5: 74, Eb5: 75, E5: 76, F5: 77, G5: 79, Ab5: 80, Bb5: 82,
  C6: 84, Db6: 85, Eb6: 87, F6: 89,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const squealBuf = new Float32Array(ns);   // xylophone — the pig's voice
const oinkBuf   = new Float32Array(ns);   // bass marimba — deep grunt
const snortBuf  = new Float32Array(ns);   // staccato — dry snort
const clopBuf   = new Float32Array(ns);   // woodblock — trotters

// ── voice routes ──────────────────────────────────────────────────────
// xylophone — the squeal: bright, let it ring a touch over the bar
function squeal(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "xylophone", decayMul: 0.95 },
    squealBuf, { sampleRate: SR });
}
// bass — tight oinks, no drone. Short ring per hit.
function oink(t0, midi, beats, gain = 0.92) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    oinkBuf, { sampleRate: SR });
}
// staccato — sharp dry slap, very short
function snort(t0, midi, beats, gain = 0.78) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "staccato", decayMul: 0.4 },
    snortBuf, { sampleRate: SR });
}
// woodblock — clip-clop trotter pulse, very short
function clop(t0, midi, beats, gain = 0.7) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "woodblock", decayMul: 0.35 },
    clopBuf, { sampleRate: SR });
}

// ── sections (matches piggabap.np) ────────────────────────────────────
const SECTIONS = [
  { name: "oink",  bar0: 0,  bars: 4 },
  { name: "snort", bar0: 4,  bars: 6 },
  { name: "mud",   bar0: 10, bars: 4 },
  { name: "chase", bar0: 14, bars: 6 },
  { name: "flop",  bar0: 20, bars: 4 },
];

// per-voice 1-based bar helpers
function X(bar, b, midi, d, g) { squeal(t(bar - 1, b), midi, d, g ?? 1.0);  }   // xylo squeal
function O(bar, b, midi, d, g) { oink  (t(bar - 1, b), midi, d, g ?? 0.92); }   // bass oink
function S(bar, b, midi, d, g) { snort (t(bar - 1, b), midi, d, g ?? 0.78); }   // staccato snort
function C(bar, b, midi, d, g) { clop  (t(bar - 1, b), midi, d, g ?? 0.7);  }   // woodblock clop

// ═══════════════════════════════════════════════════════════════════
//  § OINK (1–4) — descending F minor third triads, bass oinks under
// ═══════════════════════════════════════════════════════════════════
// xylo squeals — "oink-OINK"
X(1, 0, N.F5, 2);                    X(1, 2, N.Db5, 2);
X(2, 0, N.Db5, 2);                   X(2, 2, N.Bb4, 2);
X(3, 0, N.F5, 1); X(3, 1, N.Db5, 1); X(3, 2, N.Bb4, 2);
X(4, 0, N.F5, 4);
// bass oinks on 1 & 3 (F2 / C3 alternation)
for (let bar = 1; bar <= 4; bar++) {
  O(bar, 0, N.F2, 1.4);
  O(bar, 2, N.C3, 1.4);
}
// little snort hits punctuate the oink bars
S(1, 1.5, N.F4, 0.3, 0.55);
S(2, 1.5, N.Db4, 0.3, 0.55);
S(3, 3.5, N.F4, 0.3, 0.55);
S(4, 2,   N.Ab4, 0.5, 0.5);

// ═══════════════════════════════════════════════════════════════════
//  § SNORT (5–10) — fast 16th-note bursts, snort fully in
// ═══════════════════════════════════════════════════════════════════
// bar 5: "snort-snort SNORT / snort-snort SNORT"
X(5, 0,   N.F5, 0.5); X(5, 0.5, N.Ab5, 0.5); X(5, 1,   N.C6, 1);
X(5, 2,   N.F5, 0.5); X(5, 2.5, N.Ab5, 0.5); X(5, 3,   N.C6, 1);
// bar 6: "snort-snort SNORT-snort"
X(6, 0,   N.F5, 0.5); X(6, 0.5, N.Ab5, 0.5);
X(6, 1,   N.C6, 0.5); X(6, 1.5, N.Eb6, 0.5);
X(6, 2,   N.C6, 1);   X(6, 3,   N.Eb6, 1);
// bar 7: "snort SNORT"
X(7, 0, N.Eb6, 1); X(7, 1, N.C6, 1); X(7, 2, N.Eb6, 1); X(7, 3, N.C6, 1);
// bar 8: "snort-snort SNORT"
X(8, 0,   N.Ab5, 0.5); X(8, 0.5, N.C6, 0.5);  X(8, 1,   N.Eb6, 1);
X(8, 2,   N.Ab5, 0.5); X(8, 2.5, N.C6, 0.5);  X(8, 3,   N.Eb6, 1);
// bar 9: "snort-snort snort-SNORT"
X(9, 0,   N.F5, 0.5); X(9, 0.5, N.Ab5, 0.5);
X(9, 1,   N.C6, 0.5); X(9, 1.5, N.Eb6, 0.5);
X(9, 2,   N.C6, 0.5); X(9, 2.5, N.Ab5, 0.5);
X(9, 3,   N.Eb6, 1);
// bar 10: held SNORT
X(10, 0, N.Eb6, 4);

// bass through snort — driving F/C pulse, busier on bars 5-9
for (let bar = 5; bar <= 9; bar++) {
  O(bar, 0, N.F2, 1.2);
  O(bar, 2, N.C3, 1.2);
}
O(10, 0, N.F2, 2.5, 0.85);
O(10, 2, N.F2, 2,   0.7);

// staccato snorts — punctuation hits on the off-beats
for (let bar = 5; bar <= 9; bar++) {
  S(bar, 1.5, N.F4,  0.25, 0.65);
  S(bar, 3.5, N.Ab4, 0.25, 0.6);
}
S(10, 1, N.Ab4, 0.3, 0.55);
S(10, 2, N.C5,  0.3, 0.55);
S(10, 3, N.Ab4, 0.3, 0.5);

// ═══════════════════════════════════════════════════════════════════
//  § MUD (11–14) — chromatic squelches, woodblock clops appear
// ═══════════════════════════════════════════════════════════════════
// "mud-MUD-mud / roll-A-ROUND" — xylo
X(11, 0, N.F5, 1); X(11, 1, N.E5,  1); X(11, 2, N.Eb5, 1); X(11, 3, N.F5,  1);
X(12, 0, N.F5, 1); X(12, 1, N.Db5, 1); X(12, 2, N.F5,  1); X(12, 3, N.Ab5, 1);
X(13, 0, N.Bb5,1); X(13, 1, N.Ab5, 1); X(13, 2, N.G5,  1); X(13, 3, N.F5,  1);
X(14, 0, N.F5, 4);

// bass through mud — slower, swampier (i → iv ish)
O(11, 0, N.F2, 1.4); O(11, 2, N.C3, 1.4);
O(12, 0, N.Bb3.valueOf?.() ?? 46, 1.4); // safety; not used
O(12, 0, 46, 1.4);   // Bb2 (iv root) — manual midi
O(12, 2, N.F2, 1.4);
O(13, 0, 43, 1.4);   // G2 (v) — manual midi
O(13, 2, N.C3, 1.4);
O(14, 0, N.F2, 3,  0.75);

// staccato squelches — every off-beat to suggest mud-flicks
for (let bar = 11; bar <= 13; bar++) {
  S(bar, 0.5, N.F4,  0.2, 0.55);
  S(bar, 1.5, N.Db4, 0.2, 0.55);
  S(bar, 2.5, N.F4,  0.2, 0.55);
  S(bar, 3.5, N.Ab4, 0.2, 0.55);
}
S(14, 1, N.F4, 0.4, 0.55);
S(14, 3, N.F4, 0.4, 0.45);

// woodblock clops — quarter-note trot enters here
for (let bar = 11; bar <= 14; bar++) {
  C(bar, 0,   N.C5, 0.25, 0.7);
  C(bar, 1,   N.C5, 0.25, 0.6);
  C(bar, 2,   N.C5, 0.25, 0.7);
  C(bar, 3,   N.C5, 0.25, 0.6);
}

// ═══════════════════════════════════════════════════════════════════
//  § CHASE (15–20) — climb-descend zigzag, all voices in
// ═══════════════════════════════════════════════════════════════════
// "chase-the CAT" — xylo zigzag
X(15, 0, N.F5,  1); X(15, 1, N.C6,  1); X(15, 2, N.Ab5, 1); X(15, 3, N.Eb6, 1);
X(16, 0, N.Eb6, 1); X(16, 1, N.Ab5, 1); X(16, 2, N.C6,  1); X(16, 3, N.F5,  1);
X(17, 0, N.F5,  1); X(17, 1, N.Ab5, 1); X(17, 2, N.C6,  1); X(17, 3, N.Eb6, 1);
X(18, 0, N.Eb6, 1); X(18, 1, N.C6,  1); X(18, 2, N.Ab5, 1); X(18, 3, N.F5,  1);
X(19, 0, N.F5,  1); X(19, 1, N.C6,  1); X(19, 2, N.F5,  1); X(19, 3, N.Eb6, 1);
// bar 20: GOTCHA — descending tumble
X(20, 0, N.C6,  1); X(20, 1, N.Ab5, 1); X(20, 2, N.F5,  1); X(20, 3, N.Db5, 1);

// bass — full chase, F2/C3 pumping on 1 and 3, with a kick on 4& for skip
for (let bar = 15; bar <= 19; bar++) {
  O(bar, 0,   N.F2, 1.2);
  O(bar, 2,   N.C3, 1.2);
  O(bar, 3.5, N.F3, 0.4, 0.7);  // little grace skip
}
// bar 20: settling tumble in the bass too
O(20, 0, N.C3, 1);
O(20, 1, N.Bb3, 1, 0.8);
O(20, 2, N.F2, 2,  0.85);

// staccato snorts — fast 16ths under the chase
for (let bar = 15; bar <= 19; bar++) {
  S(bar, 0.5, N.F4,  0.2, 0.6);
  S(bar, 1.5, N.Ab4, 0.2, 0.6);
  S(bar, 2.5, N.C5,  0.2, 0.6);
  S(bar, 3.5, N.Ab4, 0.2, 0.6);
}
S(20, 0.5, N.C5,  0.2, 0.55);
S(20, 1.5, N.Ab4, 0.2, 0.5);
S(20, 2.5, N.F4,  0.2, 0.45);

// woodblock clops — chase: eighth-note gallop
for (let bar = 15; bar <= 19; bar++) {
  for (let b = 0; b < 4; b += 0.5) {
    C(bar, b, N.C5, 0.2, b % 1 === 0 ? 0.78 : 0.55);
  }
}
// bar 20 — clops slow to quarter notes as it falls
C(20, 0, N.C5, 0.25, 0.75);
C(20, 1, N.C5, 0.25, 0.65);
C(20, 2, N.C5, 0.25, 0.55);
C(20, 3, N.C5, 0.25, 0.4);

// ═══════════════════════════════════════════════════════════════════
//  § FLOP (21–24) — settle and snore
// ═══════════════════════════════════════════════════════════════════
X(21, 0, N.Db5, 2);  X(21, 2, N.Bb4, 2);
X(22, 0, N.Ab4, 4);
X(23, 0, N.F4,  4, 0.9);
X(24, 0, N.F4,  4, 0.7);

O(21, 0, N.F2, 1.5);   O(21, 2, N.C3, 1.5);
O(22, 0, N.F2, 3, 0.75);
O(23, 0, N.F2, 4, 0.65);
O(24, 0, N.F2, 4, 0.55);   // long deep snore floor

// staccato — one final dry snort, then a sleeping "SNZZ"
S(21, 1.5, N.Db4, 0.4, 0.55);
S(22, 2,   N.F4,  0.5, 0.45);
S(23, 3,   N.F4,  0.6, 0.35);   // soft "snzz"
S(24, 2,   N.F4,  0.8, 0.25);   // softer "snzzzz" trailing into sleep

// ═══════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN — pan per voice
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
// bass center, staccato slightly right, xylo slightly left, woodblock further left
place(oinkBuf,    0.00, 0.95);
place(snortBuf,   0.30, 0.85);
place(squealBuf, -0.10, 1.00);
place(clopBuf,   -0.40, 0.80);

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

// trim trailing silence + fades
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

console.log(`→ piggabap · ${TOTAL_BARS} bars · F minor 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "piggabap.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — punchy cartoon energy. Tight comp, treble lift,
// brickwall limit. Same shape as flutterbap (the F minor cartoon
// version of the daytime hop).
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
    _comment: "Section map for piggabap (cartoon barnyard chase). 4/4, 144 BPM → bar ≈ 1.667 s. F natural minor. Derived from render-piggabap.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "minor", rootMidi: 65,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "piggabap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
