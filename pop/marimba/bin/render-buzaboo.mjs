#!/usr/bin/env node
// render-buzaboo.mjs — render the buzaboo bee-hover to mp3.
//
// Score notation lives in pop/marimba/buzaboo.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   kalimba       — the lead, tine-bzz, hovering not soaring
//   staccato      — sharp dry "tok" — the bee's mid-air hit-points
//   vibraphone    — MOTOR ON, the natural tremolo (the wing-pad)
//   bass          — low A / E pulse on 1 & 3, the flight floor
//
// Run:
//   node pop/marimba/bin/render-buzaboo.mjs
//   node pop/marimba/bin/render-buzaboo.mjs --out ~/buzaboo.mp3
//
// All timing in seconds. 132 BPM 4/4 → beat = 60/132 ≈ 0.4545, bar ≈ 1.818.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 132;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table ────────────────────────────────────────────────────────
const N = {
  A2: 45, E2: 40,
  A3: 57, B3: 59,
  C4: 60, D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, F6: 89, G6: 91, A6: 93,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf = new Float32Array(ns);   // kalimba — the lead
const beeBuf  = new Float32Array(ns);   // staccato — sharp bee-hits
const padBuf  = new Float32Array(ns);   // vibraphone — motor-on pad
const bassBuf = new Float32Array(ns);   // bass marimba — flight floor

// ── voice routes ──────────────────────────────────────────────────────
// kalimba: the lead — tine bzz, hovering. Keep ring fairly short so
// the 16th-note phrases don't smear into mud.
function lead(t0, midi, beats, gain = 0.95) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.85 },
    leadBuf, { sampleRate: SR });
}
// staccato: sharp dry "tok" — bee-hits. Very short.
function bee(t0, midi, beats, gain = 0.65) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "staccato", decayMul: 0.4 },
    beeBuf, { sampleRate: SR });
}
// vibraphone: motor-on pad — let it ring, the tremolo is the bee-wing.
function pad(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "vibraphone", decayMul: 1.8 },
    padBuf, { sampleRate: SR });
}
// bass: low A / E pulse — tight bouncing flight floor.
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}

// ── sections (matches buzaboo.np) ─────────────────────────────────────
const SECTIONS = [
  { name: "hover",  bar0: 0,  bars: 4 },
  { name: "flower", bar0: 4,  bars: 8 },
  { name: "lift",   bar0: 12, bars: 8 },
  { name: "fly",    bar0: 20, bars: 4 },
];

// ── LEAD line (kalimba) — bar-by-bar from buzaboo.np ──────────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 0.95); }

// § hover (1–4) — "buzz-BUZZ buzz" — fast 16th-feeling hits in mid register
L(1, 0, N.A5, 1);   L(1, 1, N.C6, 1);   L(1, 2, N.E6, 2);
L(2, 0, N.C6, 1);   L(2, 1, N.E6, 1);   L(2, 2, N.A5, 2);
L(3, 0, N.B5, 1);   L(3, 1, N.D6, 1);   L(3, 2, N.E6, 2);
L(4, 0, N.C6, 1);   L(4, 1, N.B5, 1);   L(4, 2, N.A5, 1);   L(4, 3, N.E5, 1);

// § flower (5–12) — "flow-er-FLOW-er / land-ON-it / drink-the-nec-tar"
L(5, 0, N.C6, 1);   L(5, 1, N.D6, 1);   L(5, 2, N.E6, 2);
L(6, 0, N.D6, 1);   L(6, 1, N.C6, 1);   L(6, 2, N.B5, 1);   L(6, 3, N.A5, 1);
L(7, 0, N.C6, 1);   L(7, 1, N.D6, 1);   L(7, 2, N.E6, 2);
L(8, 0, N.D6, 2);   L(8, 2, N.C6, 2);
L(9, 0, N.E6, 1);   L(9, 1, N.D6, 1);   L(9, 2, N.C6, 2);
L(10, 0, N.D6, 1);  L(10, 1, N.C6, 1);  L(10, 2, N.B5, 2);
L(11, 0, N.C6, 1);  L(11, 1, N.D6, 1);  L(11, 2, N.E6, 2);
L(12, 0, N.E6, 2);  L(12, 2, N.C6, 2);

// § lift (13–20) — ascending lift-off, gathering speed
L(13, 0, N.A5, 1);  L(13, 1, N.B5, 1);  L(13, 2, N.C6, 2);
L(14, 0, N.B5, 1);  L(14, 1, N.C6, 1);  L(14, 2, N.D6, 2);
L(15, 0, N.C6, 1);  L(15, 1, N.D6, 1);  L(15, 2, N.E6, 2);
L(16, 0, N.D6, 1);  L(16, 1, N.E6, 1);  L(16, 2, N.G6, 2);
L(17, 0, N.E6, 1);  L(17, 1, N.G6, 1);  L(17, 2, N.A6, 2);
L(18, 0, N.E6, 1);  L(18, 1, N.D6, 1);  L(18, 2, N.C6, 2);
L(19, 0, N.B5, 1);  L(19, 1, N.C6, 1);  L(19, 2, N.D6, 1);  L(19, 3, N.E6, 1);
L(20, 0, N.G6, 2);  L(20, 2, N.E6, 2);

// § fly (21–24) — descending zig-zag home
L(21, 0, N.E6, 1);  L(21, 1, N.D6, 1);  L(21, 2, N.C6, 2);
L(22, 0, N.D6, 1);  L(22, 1, N.B5, 1);  L(22, 2, N.A5, 2);
L(23, 0, N.C6, 1);  L(23, 1, N.B5, 1);  L(23, 2, N.A5, 1);  L(23, 3, N.E5, 1);
L(24, 0, N.A4, 4, 0.9);

// ── BEE (staccato) — darting 16th-note hits, the wing-blur ────────────
// Sharp dry "tok"s scattered around the lead — like the bee tapping
// the air. Don't double the lead, sit just off it.
function S(bar, b, midi, d, g) { bee(t(bar - 1, b), midi, d, g ?? 0.65); }

// hover: dense 16ths under each lead note, mid register
for (let bar = 1; bar <= 4; bar++) {
  S(bar, 0.5, N.E6, 0.5, 0.55);
  S(bar, 1.5, N.A5, 0.5, 0.55);
  S(bar, 2.5, N.C6, 0.5, 0.55);
  S(bar, 3.5, N.E6, 0.5, 0.55);
}
// flower: thinned, hovering — bee found a flower, less darting
for (let bar = 5; bar <= 12; bar++) {
  S(bar, 0.5, N.E6, 0.5, 0.5);
  S(bar, 2.5, N.C6, 0.5, 0.5);
}
// lift: building energy — bee taking off, 16th doubles on off-beats
for (let bar = 13; bar <= 20; bar++) {
  S(bar, 0.5, N.E6, 0.5, 0.55);
  S(bar, 1.5, N.G6, 0.5, 0.55);
  S(bar, 2.5, N.A6, 0.5, 0.5);
  S(bar, 3.5, N.E6, 0.5, 0.55);
}
// fly: home stretch — fewer hits, peeling off
S(21, 0.5, N.E6, 0.5, 0.55);   S(21, 2.5, N.C6, 0.5, 0.5);
S(22, 0.5, N.D6, 0.5, 0.5);    S(22, 2.5, N.A5, 0.5, 0.45);
S(23, 0.5, N.C6, 0.5, 0.45);
// bar 24 — silence on the bee track, just the home tone ringing

// ── PAD (vibraphone, MOTOR ON) — Am chord tremolo wash on the flower ──
// Held vibraphone chord-tones across the flower section, drawing out
// the bee's natural wing-tremolo as a held pad. Drops in lift (let
// the lead carry), brief reprise at the very end.
function P(bar, b, midi, d, g) { pad(t(bar - 1, b), midi, d, g ?? 0.55); }

// flower (5–12): rolling Am-iv chord pad. Two bars Am, two bars Dm,
// two bars Em, two bars Am.
//   bars 5–6: Am (A4 C5 E5)
P(5, 0, N.A4, 8, 0.4);   P(5, 0, N.C5, 8, 0.35);  P(5, 0, N.E5, 8, 0.3);
//   bars 7–8: Dm (D5 F5 A5)
P(7, 0, N.D5, 8, 0.4);   P(7, 0, N.F5, 8, 0.35);  P(7, 0, N.A5, 8, 0.3);
//   bars 9–10: Em (E5 G5 B5)
P(9, 0, N.E5, 8, 0.4);   P(9, 0, N.G5, 8, 0.35);  P(9, 0, N.B5, 8, 0.3);
//   bars 11–12: Am (A4 C5 E5)
P(11, 0, N.A4, 8, 0.4);  P(11, 0, N.C5, 8, 0.35); P(11, 0, N.E5, 8, 0.3);

// lift: brief halo on the climaxes (bars 17 + 20)
P(17, 0, N.E5, 4, 0.32);  P(17, 0, N.A5, 4, 0.28);
P(20, 0, N.E5, 4, 0.32);  P(20, 0, N.G5, 4, 0.28);

// fly: one last held Am pad ringing the home
P(24, 0, N.A3, 4, 0.5);   P(24, 0, N.C5, 4, 0.4);   P(24, 0, N.E5, 4, 0.35);

// ── BASS — low A / E pulse on 1 & 3, the flight floor ─────────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.85); }

// hover: classic bouncing-floor, A on 1, E on 3
for (let bar = 1; bar <= 4; bar++) {
  B(bar, 0, N.A2, 1.6);
  B(bar, 2, N.E2, 1.6);
}
// flower: harmonic motion — Am / Dm / Em / Am
// bars 5–6 Am
B(5, 0, N.A2, 1.6);   B(5, 2, N.E2, 1.6);
B(6, 0, N.A2, 1.6);   B(6, 2, N.E2, 1.6);
// bars 7–8 Dm (D bass on 1, A on 3)
B(7, 0, N.A2 - 7, 1.6);   B(7, 2, N.A2, 1.6);   // D2=38, A2=45
B(8, 0, N.A2 - 7, 1.6);   B(8, 2, N.A2, 1.6);
// bars 9–10 Em (E2 on 1, B on 3)
B(9, 0, N.E2, 1.6);   B(9, 2, N.B3, 1.6, 0.7);
B(10, 0, N.E2, 1.6);  B(10, 2, N.B3, 1.6, 0.7);
// bars 11–12 Am
B(11, 0, N.A2, 1.6);  B(11, 2, N.E2, 1.6);
B(12, 0, N.A2, 1.6);  B(12, 2, N.E2, 1.6);

// lift: bouncing climb — A / E alternation drives upward energy
for (let bar = 13; bar <= 20; bar++) {
  B(bar, 0, N.A2, 1.6);
  B(bar, 2, N.E2, 1.6);
  // small grace skip on even bars
  if (bar % 2 === 0) B(bar, 3.5, N.A3, 0.4, 0.55);
}

// fly: bass walks back down to the home
B(21, 0, N.A2, 1.6);   B(21, 2, N.E2, 1.6);
B(22, 0, N.A2 - 7, 1.6);  B(22, 2, N.A2, 1.6);   // Dm walk
B(23, 0, N.E2, 1.6);   B(23, 2, N.A2, 1.6);
B(24, 0, N.A2, 4, 0.8);   // deep home tone under the A4 lead

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
// kalimba lead just-right of center; staccato darting (alternating
// bar-to-bar in earlier per-event pans would be ideal — here we
// settle for a moderate left placement to feel "off-axis from lead");
// vibraphone wide left; bass centered.
place(leadBuf, 0.10, 1.00);
place(beeBuf, -0.20, 0.95);
place(padBuf, -0.25, 0.85);
place(bassBuf, 0.00, 0.92);

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

console.log(`→ buzaboo · ${TOTAL_BARS} bars · A minor 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "buzaboo.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — bright, light, daytime. (flutterbap's chain)
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
    _comment: "Section map for buzaboo (the bee-in-flowers hover). 4/4, 132 BPM → bar ≈ 1.818 s. A natural minor. Derived from render-buzaboo.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "minor", rootMidi: 69,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "buzaboo.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
