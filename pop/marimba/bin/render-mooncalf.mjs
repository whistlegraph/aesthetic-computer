#!/usr/bin/env node
// render-mooncalf.mjs — render the mooncalf moonlit-porch ballad to mp3.
//
// Score notation lives in pop/marimba/mooncalf.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   rosewood       — the singer; syllabic moon-question lead
//   vibraphone_off — held thirds, motor off (the night-sky pad)
//   bass           — gentle low E/B pulse on beat 1 of each bar
//   kalimba        — sparse star-pings on phrase tails
//
// Run:
//   node pop/marimba/bin/render-mooncalf.mjs
//   node pop/marimba/bin/render-mooncalf.mjs --out ~/mooncalf.mp3
//
// All timing in seconds. 60 BPM 3/4 → beat = 1.000 s, bar = 3.000 s.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 60;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;             // 3/4 meter
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 3.5;   // + tail for kalimba star-pings + fade
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (E minor friendly) ─────────────────────────────────────
const N = {
  E2: 40, B2: 47,
  C3: 48, D3: 50, E3: 52, G3: 55, A3: 57, B3: 59,
  C4: 60, D4: 62, E4: 64, F4s: 66, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, F5s: 78, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, G6: 91,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // rosewood — the singer
const padBuf   = new Float32Array(ns);   // vibraphone_off — night-sky pad
const bassBuf  = new Float32Array(ns);   // bass marimba — porch-board pulse
const twinkBuf = new Float32Array(ns);   // kalimba — star-pings

// ── voice routes ──────────────────────────────────────────────────────
// rosewood: warm woody lead — natural ring works for the wistful lullaby feel
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "rosewood", decayMul: 1.0 },
    leadBuf, { sampleRate: SR });
}
// vibraphone_off: long, dreamy hold for the night-sky pad (motor off)
function pad(t0, midi, beats, gain = 0.32) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "vibraphone_off", decayMul: 1.8 },
    padBuf, { sampleRate: SR });
}
// bass: tight, gentle porch-board pulse — short, just an anchor under beat 1
function bass(t0, midi, beats, gain = 0.7) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.65 },
    bassBuf, { sampleRate: SR });
}
// kalimba: bright tine star-pings on phrase tails
function twink(t0, midi, beats, gain = 0.5) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.95 },
    twinkBuf, { sampleRate: SR });
}

// ── sections (matches mooncalf.np) ────────────────────────────────────
const SECTIONS = [
  { name: "look",   bar0: 0,  bars: 4 },
  { name: "where",  bar0: 4,  bars: 6 },
  { name: "follow", bar0: 10, bars: 8 },
  { name: "far",    bar0: 18, bars: 6 },
];

// ── LEAD line (rosewood) — bar-by-bar from mooncalf.np ────────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § look (1–4) — "moon-CALF / look-UP / moon-CALF / look" (climbing)
L(1, 0, N.E5, 1.5);   L(1, 1.5, N.G5, 1.5);
L(2, 0, N.G5, 1.5);   L(2, 1.5, N.B5, 1.5);
L(3, 0, N.E5, 1.5);   L(3, 1.5, N.G5, 1.5);
L(4, 0, N.B5, 3, 0.95);

// § where (5–10) — descending wondering G5-E5, D5-E5...
L(5, 0, N.G5, 1.5);   L(5, 1.5, N.E5, 1.5);
L(6, 0, N.D5, 1.5);   L(6, 1.5, N.E5, 1.5);
L(7, 0, N.G5, 1.5);   L(7, 1.5, N.E5, 1.5);
L(8, 0, N.D5, 1.5);   L(8, 1.5, N.B4, 1.5);
L(9, 0, N.E5, 1.5);   L(9, 1.5, N.D5, 1.5);
L(10, 0, N.B4, 3, 0.9);

// § follow (11–18) — climbing again, "fol-low / the-LIGHT"
L(11, 0, N.E5, 1.5);  L(11, 1.5, N.G5, 1.5);
L(12, 0, N.A5, 1.5);  L(12, 1.5, N.B5, 1.5);
L(13, 0, N.E5, 1.5);  L(13, 1.5, N.G5, 1.5);
L(14, 0, N.A5, 1.5);  L(14, 1.5, N.B5, 1.5);
L(15, 0, N.G5, 1);    L(15, 1, N.A5, 1);    L(15, 2, N.B5, 1);
L(16, 0, N.B5, 3, 0.95);
L(17, 0, N.A5, 1);    L(17, 1, N.G5, 1);    L(17, 2, N.A5, 1);
L(18, 0, N.G5, 3, 0.9);

// § far (19–24) — final descending settle, "far-a-way / good-NIGHT"
L(19, 0, N.D5, 1);    L(19, 1, N.B4, 1);    L(19, 2, N.A4, 1);
L(20, 0, N.D5, 1);    L(20, 1, N.B4, 1);    L(20, 2, N.A4, 1);
L(21, 0, N.G4, 3, 0.9);
L(22, 0, N.A4, 1.5);  L(22, 1.5, N.G4, 1.5);
L(23, 0, N.E4, 3, 0.85);
// bar 24 — silence in the lead; kalimba star-pings sing above it (see twinkles)

// ── PAD (vibraphone_off) — held thirds on the i-VI-III-VII-i progression
// Em – C – G – D – Em across the song. Motor off, long swells.
function P(bar, b, midi, beats, g) { pad(t(bar - 1, b), midi, beats, g ?? 0.30); }

// look (1–4) — Em pad enters softly under bar 1, sustains through 4
P(1, 0, N.E4, 12, 0.22);
P(1, 0, N.G4, 12, 0.20);
P(1, 0, N.B4, 12, 0.18);

// where (5–10) — Em → C → G across 6 bars (descending wondering)
// bars 5–6 Em, bars 7–8 C, bars 9–10 G
P(5, 0, N.E4, 6, 0.22);
P(5, 0, N.G4, 6, 0.20);
P(5, 0, N.B4, 6, 0.18);
P(7, 0, N.C4, 6, 0.22);
P(7, 0, N.E4, 6, 0.20);
P(7, 0, N.G4, 6, 0.18);
P(9, 0, N.G3, 6, 0.22);
P(9, 0, N.B3, 6, 0.20);
P(9, 0, N.D5, 6, 0.18);

// follow (11–18) — pad swells. Em (11-12) → C (13-14) → G (15-16) → D (17-18)
P(11, 0, N.E4, 6, 0.26);
P(11, 0, N.G4, 6, 0.24);
P(11, 0, N.B4, 6, 0.22);
P(13, 0, N.C4, 6, 0.26);        // C
P(13, 0, N.E4, 6, 0.24);
P(13, 0, N.G4, 6, 0.22);
P(15, 0, N.G3, 6, 0.28);        // G — pad peaks here
P(15, 0, N.B3, 6, 0.26);
P(15, 0, N.D5, 6, 0.24);
P(17, 0, N.F4s, 6, 0.26);       // D major (in E natural minor we get D, but F# colors)
P(17, 0, N.A4, 6, 0.24);
P(17, 0, N.D5, 6, 0.22);

// far (19–24) — Em settles. bars 19-21 Em, 22-24 Em low (resolution)
P(19, 0, N.E4, 6, 0.24);
P(19, 0, N.G4, 6, 0.22);
P(19, 0, N.B4, 6, 0.20);
P(22, 0, N.E3, 9, 0.22);        // low Em — into the resolution tail
P(22, 0, N.G3, 9, 0.20);
P(22, 0, N.B3, 9, 0.18);

// ── BASS — gentle low E/B pulse on beat 1 of each bar ─────────────────
// Following the chord: Em (E), where→C (C), →G (G), follow→Em/C/G/D, far→Em
function B(bar, b, midi, beats, g) { bass(t(bar - 1, b), midi, beats, g ?? 0.7); }

// look (1–4) — Em
for (let bar = 1; bar <= 4; bar++) B(bar, 0, N.E2, 2.4, 0.65);

// where (5–10) — Em / C / G
B(5, 0, N.E2, 2.4, 0.60);
B(6, 0, N.E2, 2.4, 0.55);
B(7, 0, N.C3, 2.4, 0.60);
B(8, 0, N.C3, 2.4, 0.55);
B(9, 0, N.G3, 2.4, 0.60);
B(10, 0, N.G3, 2.4, 0.55);

// follow (11–18) — Em / C / G / D
B(11, 0, N.E2, 2.4, 0.70);
B(12, 0, N.E2, 2.4, 0.65);
B(13, 0, N.C3, 2.4, 0.70);
B(14, 0, N.C3, 2.4, 0.65);
B(15, 0, N.G3, 2.4, 0.72);
B(16, 0, N.G3, 2.4, 0.68);
B(17, 0, N.D3, 2.4, 0.70);
B(18, 0, N.D3, 2.4, 0.65);

// far (19–24) — Em settling
B(19, 0, N.E2, 2.4, 0.65);
B(20, 0, N.E2, 2.4, 0.62);
B(21, 0, N.E2, 2.4, 0.58);
B(22, 0, N.B2, 2.4, 0.55);       // V (B) under good-NIGHT
B(23, 0, N.E2, 3.5, 0.55);       // final low E, sustained
// bar 24 — no new bass attack; the bar-23 E rings into the tail

// ── KALIMBA — sparse star-pings on phrase tails ───────────────────────
function K(bar, b, midi, beats, g) { twink(t(bar - 1, b), midi, beats, g ?? 0.5); }

// look (1–4) — one tiny ping at the end of bar 4
K(4, 2.5, N.E6, 0.5, 0.4);

// where (5–10) — pings under each descending question
K(6, 2.5, N.B5, 0.5, 0.35);
K(8, 2.5, N.G5, 0.5, 0.35);
K(10, 2.5, N.E6, 0.5, 0.4);

// follow (11–18) — sparkle haloes above the climbing climaxes
K(12, 2.5, N.E6, 0.5, 0.42);
K(14, 2.5, N.G6, 0.5, 0.45);
K(16, 0.5, N.E6, 0.5, 0.40);
K(16, 2.5, N.G6, 0.5, 0.45);
K(18, 2.5, N.D6, 0.5, 0.42);

// far (19–24) — the long star-tail. Sparse, drifting pings above the
// settling low E. The "good-night" sky.
K(20, 2.5, N.E6, 0.5, 0.40);
K(22, 0.5, N.B5, 0.5, 0.38);
K(22, 2.5, N.G5, 0.5, 0.36);
K(23, 1.5, N.E6, 0.5, 0.40);
K(23, 2.5, N.G6, 0.5, 0.42);
K(24, 0.5, N.E6, 0.5, 0.36);
K(24, 1.5, N.B5, 0.5, 0.32);
K(24, 2.5, N.E6, 0.5, 0.30);

// ═══════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN — pad widens with Haas, lead centered, twinkle right
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

// rosewood — dead center
place(leadBuf, 0.00, 1.00);

// pad — widen with Haas: same source placed +0.30 right AND -0.30 left,
// with a tiny ~12ms delay on the right copy. Night-sky stereo bloom.
place(padBuf, -0.30, 0.55);
{
  const HAAS_SAMPLES = Math.floor(0.012 * SR);
  const [lg, rg] = pan(+0.30);
  const gain = 0.55;
  for (let i = HAAS_SAMPLES; i < ns; i++) {
    const s = padBuf[i - HAAS_SAMPLES] * gain;
    outL[i] += s * lg;
    outR[i] += s * rg;
  }
}

// bass — center, slightly under unity
place(bassBuf, 0.00, 0.85);

// kalimba — placed right at +0.40 for star-twinkle on that side
place(twinkBuf, 0.40, 0.78);

// scrub non-finite + peak-normalise to ~-1.3 dBFS
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

// trim trailing silence + LONG goodnight fade
let lastLoud = ns - 1;
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < 0.003 &&
       Math.abs(outR[lastLoud]) < 0.003) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.8 * SR));
const fadeIn = Math.floor(0.006 * SR);
const fadeOut = Math.floor(2.5 * SR);     // long goodnight fade
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ mooncalf · ${TOTAL_BARS} bars · E minor 3/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "mooncalf.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// SOFTENED master chain — keep the porch-night intimacy, no daytime
// shimmer. Gentle compression, just a whisper of air on top, brickwall.
const MASTER = [
  "highpass=f=28",
  "acompressor=threshold=-20dB:ratio=1.8:attack=22:release=220:makeup=1.4:knee=6",
  "treble=g=0.6:f=7000",
  "alimiter=limit=0.96:attack=4:release=80",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (porch-mastered · ${(trimN / SR).toFixed(1)} s)`);

// ── struct.json — section map for any future visualizer ───────────────
{
  const structTotalSec = trimN / SR;
  const struct = {
    _comment: "Section map for mooncalf (moonlit-porch ballad). 3/4, 60 BPM → bar = 3.000 s. E minor. Derived from render-mooncalf.mjs SECTIONS.",
    meter: 3, bpm: BPM, scale: "minor", rootMidi: 64,   // E5 root reference
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "mooncalf.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
