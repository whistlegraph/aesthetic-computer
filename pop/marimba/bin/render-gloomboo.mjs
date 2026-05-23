#!/usr/bin/env node
// render-gloomboo.mjs — render the gloomboo sad-cafe waltz to mp3.
//
// Score notation lives in pop/marimba/gloomboo.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   rosewood       — the asking-voice; slow syllabic lead
//   bass           — very slow low D / A pulse on beat 1 ONLY
//   vibraphone_off — held somber thirds, motor off (no vibrato)
//   kalimba        — a single questioning twinkle once or twice per section
//
// Run:
//   node pop/marimba/bin/render-gloomboo.mjs
//   node pop/marimba/bin/render-gloomboo.mjs --out ~/gloomboo.mp3
//
// All timing in seconds. 64 BPM 3/4 → beat = 60/64 = 0.9375, bar = 2.8125.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 64;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 4.0;   // long tail for the settling
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table — D natural minor (D E F G A Bb C), with C# for V ──────
const N = {
  D2: 38, A2: 45,
  D3: 50, F3: 53, G3: 55, A3: 57, Bb3: 58, C4: 60, Cs4: 61,
  D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, Bb4: 70, C5: 72, Cs5: 73,
  D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, Bb5: 82, C6: 84, Cs6: 85, D6: 86,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // rosewood — the asking-voice
const bassBuf  = new Float32Array(ns);   // bass marimba — slow low pulse
const padBuf   = new Float32Array(ns);   // vibraphone_off — held thirds
const twinkBuf = new Float32Array(ns);   // kalimba — questioning twinkles

// ── voice routes ──────────────────────────────────────────────────────
// rosewood: natural ring — the asking-voice carries the syllables.
function lead(t0, midi, beats, gain = 0.9) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "rosewood", decayMul: 1.0 },
    leadBuf, { sampleRate: SR });
}
// bass: tight muted thump — only on beat 1, never droning.
function bass(t0, midi, beats, gain = 0.65) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.65 },
    bassBuf, { sampleRate: SR });
}
// vibraphone_off: motor off — long sustaining metallic thirds.
function pad(t0, midi, beats, gain = 0.45) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "vibraphone_off", decayMul: 2.0 },
    padBuf, { sampleRate: SR });
}
// kalimba: a single questioning tine. Default ring.
function twink(t0, midi, beats, gain = 0.5) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 1.0 },
    twinkBuf, { sampleRate: SR });
}

// ── sections (matches gloomboo.np) ────────────────────────────────────
const SECTIONS = [
  { name: "gone", bar0: 0,  bars: 4 },
  { name: "back", bar0: 4,  bars: 6 },
  { name: "wait", bar0: 10, bars: 8 },
  { name: "sigh", bar0: 18, bars: 6 },
];

// ── LEAD line (rosewood) — bar-by-bar from gloomboo.np ────────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 0.9); }

// § gone (1–4) — "where-DID-it / GO / where-DID / GONE"
L(1, 0, N.F5, 1);   L(1, 1, N.E5, 1);   L(1, 2, N.D5, 1);
L(2, 0, N.D5, 3, 0.75);
L(3, 0, N.F5, 1.5); L(3, 1.5, N.E5, 1.5);
L(4, 0, N.D5, 3, 0.75);

// § back (5–10) — "when-WILL-it / COME / back-a-GAIN / (sigh) / when / BACK"
L(5, 0, N.F5, 1);   L(5, 1, N.G5, 1);   L(5, 2, N.A5, 1);
L(6, 0, N.A5, 3, 0.8);
L(7, 0, N.A5, 1);   L(7, 1, N.G5, 1);   L(7, 2, N.F5, 1);
L(8, 0, N.F5, 3, 0.7);
L(9, 0, N.G5, 1.5); L(9, 1.5, N.A5, 1.5);
L(10, 0, N.G5, 1.5); L(10, 1.5, N.F5, 1.5);

// § wait (11–18) — held longing; lead rests on bars 12 + 14 (pad carries)
L(11, 0, N.A5, 3, 0.7);
// bar 12 — silence in the lead, pad blooms
L(13, 0, N.D6, 3, 0.75);
// bar 14 — silence in the lead, pad sustains
L(15, 0, N.D6, 1.5); L(15, 1.5, N.C6, 1.5);
L(16, 0, N.A5, 3, 0.7);
L(17, 0, N.A5, 1.5); L(17, 1.5, N.G5, 1.5);
L(18, 0, N.F5, 3, 0.65);

// § sigh (19–24) — final descent settling on low D
L(19, 0, N.D6, 1.5); L(19, 1.5, N.C6, 1.5);
L(20, 0, N.A5, 1.5); L(20, 1.5, N.G5, 1.5);
L(21, 0, N.F5, 1.5); L(21, 1.5, N.D5, 1.5);
L(22, 0, N.D5, 3, 0.6);
L(23, 0, N.D4, 3, 0.7);     // low D — settling
// bar 24 — silence + tail

// ── BASS — slow low D / A pulse on beat 1 ONLY ────────────────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.65); }
// gone (1–4): rooted on D — Dm chord
B(1, 0, N.D3, 2.4);
B(2, 0, N.D3, 2.4, 0.55);
B(3, 0, N.D3, 2.4);
B(4, 0, N.D3, 2.4, 0.55);
// back (5–10): walk to Gm (G3) on 5, back to Dm on 7, A (V) on 9, Dm on 10
B(5, 0, N.G3, 2.4);            // iv
B(6, 0, N.G3, 2.4, 0.5);
B(7, 0, N.D3, 2.4);            // i
B(8, 0, N.D3, 2.4, 0.5);
B(9, 0, N.A3, 2.4);            // V
B(10, 0, N.D3, 2.4, 0.55);     // i
// wait (11–18): held + sparse. bass drops out on 12, 14 (pad-only bars)
B(11, 0, N.D3, 2.4, 0.55);
// bar 12 — bass rests
B(13, 0, N.G3, 2.4, 0.6);      // iv under the high D6
// bar 14 — bass rests
B(15, 0, N.G3, 2.4, 0.55);
B(16, 0, N.D3, 2.4, 0.55);
B(17, 0, N.A3, 2.4, 0.6);      // V
B(18, 0, N.D3, 2.4, 0.55);
// sigh (19–24): final cadence
B(19, 0, N.G3, 2.4, 0.55);     // iv
B(20, 0, N.A3, 2.4, 0.6);      // V
B(21, 0, N.D3, 2.4, 0.55);     // i
B(22, 0, N.D3, 2.4, 0.5);
B(23, 0, N.D2, 4, 0.7);        // very low D — settling
// bar 24 — bass rests, just the tail

// ── PAD (vibraphone_off) — held somber thirds, motor off ──────────────
// Chord pad: Dm (D F A) — Gm (G Bb D) — A major (A C# E, raised 7th) — Dm (D F A)
function P(bar, b, midi, d, g) { pad(t(bar - 1, b), midi, d, g ?? 0.45); }
// gone (1–4) — pad NOT present yet, lead carries alone (more lonely)
// back (5–10) — pad begins to bloom underneath
P(5, 0, N.D4, 6, 0.32);   P(5, 0, N.F4, 6, 0.28);            // Dm under (D F)
P(7, 0, N.D4, 6, 0.32);   P(7, 0, N.F4, 6, 0.28);            // Dm
P(9, 0, N.Cs4, 6, 0.34);  P(9, 0, N.E4, 6, 0.28);            // A major V (C# E)
// wait (11–18) — pad BLOOMS under the held longing
P(11, 0, N.D4, 12, 0.42); P(11, 0, N.F4, 12, 0.38);          // Dm long
P(13, 0, N.G4, 6, 0.42);  P(13, 0, N.Bb4, 6, 0.36);          // Gm (G Bb)
P(15, 0, N.G4, 6, 0.40);  P(15, 0, N.Bb4, 6, 0.34);          // Gm
P(17, 0, N.Cs4, 6, 0.40); P(17, 0, N.E4, 6, 0.34);           // A V (raised 7th)
// sigh (19–24) — pad walks the final cadence
P(19, 0, N.G4, 3, 0.40);  P(19, 0, N.Bb4, 3, 0.34);          // Gm
P(20, 0, N.Cs4, 3, 0.42); P(20, 0, N.E4, 3, 0.36);           // A V
P(21, 0, N.D4, 9, 0.40);  P(21, 0, N.F4, 9, 0.34);           // Dm settle, long ring

// ── KALIMBA — questioning twinkle, once or twice per section ──────────
function K(bar, b, midi, d, g) { twink(t(bar - 1, b), midi, d, g ?? 0.5); }
// gone — a single distant twinkle at the tail of "gone" (bar 4 beat 2.5)
K(4, 2.5, N.A5, 0.5, 0.42);
// back — twinkle as the wondering climbs (end of bar 6) and on the sigh (end of bar 8)
K(6, 2.5, N.D6, 0.5, 0.45);
K(8, 2.5, N.A5, 0.5, 0.40);
// wait — bar 12 (pad-only bar) gets a soft twinkle; bar 14 too
K(12, 1, N.D6, 0.5, 0.42);
K(14, 1, N.D6, 0.5, 0.40);
K(16, 2.5, N.C6, 0.5, 0.38);
// sigh — one final tiny twinkle at the very end (bar 24 first beat)
K(24, 0.5, N.A5, 0.5, 0.35);

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
// rosewood centered; bass centered (the floor); pad widened L/R; kalimba off to the right
// pad gets duplicated narrow-to-wide for a stereo bloom
place(leadBuf,   0.00, 1.00);
place(bassBuf,   0.00, 0.95);
place(padBuf,   -0.42, 0.78);
place(padBuf,    0.42, 0.78);    // pad widening — both ears get a halo
place(twinkBuf,  0.35, 0.85);

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

// trim trailing silence + tiny fade-in + LONG (3 s) fade-out
let lastLoud = ns - 1;
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < 0.003 &&
       Math.abs(outR[lastLoud]) < 0.003) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(1.0 * SR));
const fadeIn = Math.floor(0.006 * SR);
const fadeOut = Math.floor(3.0 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ gloomboo · ${TOTAL_BARS} bars · D minor 3/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "gloomboo.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — SOFTENED. warm-dark, gentle glue, no air sparkle,
// a touch of low-cut to keep the mud out without thinning the sadness.
const MASTER = [
  "highpass=f=28",
  "acompressor=threshold=-22dB:ratio=1.8:attack=24:release=260:makeup=1.2:knee=6",
  "treble=g=0.4:f=7000",
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
    _comment: "Section map for gloomboo (the sad-cafe waltz). 3/4, 64 BPM → bar ≈ 2.8125 s. D natural minor. Derived from render-gloomboo.mjs SECTIONS.",
    meter: 3, bpm: BPM, scale: "natural-minor", rootMidi: 62,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "gloomboo.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
