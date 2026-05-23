#!/usr/bin/env node
// render-dewabap.mjs — render the dewabap dew-drop morning to mp3.
//
// Score notation lives in pop/marimba/dewabap.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   glockenspiel    — the lead; each note one dewdrop, bell-pure
//   vibraphone_off  — misty pad, motor off, very long sustains
//   kalimba         — drips on phrase tails (off-beat sparkles)
//   bass (marimba)  — one sparse low G per section, the ground
//
// Run:
//   node pop/marimba/bin/render-dewabap.mjs
//   node pop/marimba/bin/render-dewabap.mjs --out ~/dewabap.mp3
//
// All timing in seconds. 78 BPM 4/4 → beat = 60/78 ≈ 0.769, bar ≈ 3.077.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 78;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 20;
const totalSec = TOTAL_BARS * BAR + 4.0;   // long tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table ────────────────────────────────────────────────────────
const N = {
  G2: 43, B2: 47,
  C3: 48, D3: 50, E3: 52, G3: 55, A3: 57, B3: 59,
  C4: 60, D4: 62, E4: 64, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, G6: 91,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf = new Float32Array(ns);   // glockenspiel — the dewdrops
const padBuf  = new Float32Array(ns);   // vibraphone_off — held mist
const dripBuf = new Float32Array(ns);   // kalimba — drips
const bassBuf = new Float32Array(ns);   // bass marimba — ground

// ── voice routes ──────────────────────────────────────────────────────
// glockenspiel: let it ring out, each note a long dewdrop.
function lead(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "glockenspiel", decayMul: 1.4 },
    leadBuf, { sampleRate: SR });
}
// vibraphone_off: motor off, very long sustain — the mist pad.
function pad(t0, midi, beats, gain = 0.45) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "vibraphone_off", decayMul: 2.0 },
    padBuf, { sampleRate: SR });
}
// kalimba: tine sparkles on phrase tails — default ring.
function drip(t0, midi, beats, gain = 0.5) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 1.0 },
    dripBuf, { sampleRate: SR });
}
// bass marimba: a sparse low G — short ring so it grounds without droning.
function bass(t0, midi, beats, gain = 0.65) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}

// ── sections (matches dewabap.np) ─────────────────────────────────────
const SECTIONS = [
  { name: "drop",  bar0: 0,  bars: 4 },
  { name: "shine", bar0: 4,  bars: 6 },
  { name: "wake",  bar0: 10, bars: 6 },
  { name: "bloom", bar0: 16, bars: 4 },
];

// ── LEAD line (glockenspiel) — bar-by-bar from dewabap.np ─────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 0.85); }

// § drop (1–4) — single droplets on beats 1 and 3, lots of space
L(1, 0, N.B5, 2);   L(1, 2, N.G5, 2);
L(2, 0, N.D6, 2);   L(2, 2, N.B5, 2);
L(3, 0, N.B5, 2);   L(3, 2, N.G5, 2);
L(4, 0, N.D6, 2);   L(4, 2, N.B5, 2);

// § shine (5–10) — gentle ascending then drift
L(5, 0,  N.D6, 2);  L(5, 2,  N.E6, 2);
L(6, 0,  N.E6, 2);  L(6, 2,  N.G6, 2);
L(7, 0,  N.D6, 2);  L(7, 2,  N.B5, 2);
L(8, 0,  N.E6, 2);  L(8, 2,  N.D6, 2);
L(9, 0,  N.B5, 2);  L(9, 2,  N.A5, 2);
L(10, 0, N.G5, 4, 0.8);

// § wake (11–16) — melody floats around G5-A5-B5-D6
L(11, 0, N.G5, 1);  L(11, 1, N.A5, 1);  L(11, 2, N.B5, 2);
L(12, 0, N.B5, 1);  L(12, 1, N.D6, 1);  L(12, 2, N.A5, 2);
L(13, 0, N.G5, 2);  L(13, 2, N.D6, 2);
L(14, 0, N.A5, 1);  L(14, 1, N.B5, 1);  L(14, 2, N.D6, 2);
L(15, 0, N.E6, 2);  L(15, 2, N.D6, 2);
L(16, 0, N.B5, 2);  L(16, 2, N.G5, 2);

// § bloom (17–20) — held G major chord across all voices, then a final
// glockenspiel ping high on G6
L(17, 0, N.G5, 4, 0.8);
L(18, 0, N.B5, 4, 0.8);
L(19, 0, N.D6, 4, 0.8);
L(20, 0, N.G6, 4, 0.95);   // the final dewdrop pinging on high G

// ── PAD (vibraphone_off) — held mist; G — Em — C — D chord pad ────────
// I-vi-IV-V, held 4 bars each. SPACE matters more than density —
// the pad only enters at the start of "shine" and grows from there.
function P(bar, b, midi, d, g) { pad(t(bar - 1, b), midi, d, g ?? 0.40); }

// drop (1–4): a single, very quiet pad note — barely-there mist
P(1, 0, N.G3, 16, 0.18);   // G held across all four bars, ppp

// shine (5–10): chord pad enters proper. G (5–6), Em (7–8), C (9), D (10)
// G chord — G B D
P(5, 0, N.G3, 8, 0.35);
P(5, 0, N.B3, 8, 0.30);
P(5, 0, N.D4, 8, 0.28);
// Em chord — E G B
P(7, 0, N.E3, 8, 0.35);
P(7, 0, N.G3, 8, 0.30);
P(7, 0, N.B3, 8, 0.28);
// C chord — C E G
P(9, 0, N.C4, 4, 0.40);
P(9, 0, N.E4, 4, 0.35);
P(9, 0, N.G4, 4, 0.32);
// D chord — D F# A (use D, A — keep diatonic skeletal)
P(10, 0, N.D4, 4, 0.40);
P(10, 0, N.A4, 4, 0.35);

// wake (11–16): G — Em — C — D with shorter holds (1 bar each, doubled)
// G (11), Em (12), C (13), D (14), G (15), G (16)
P(11, 0, N.G3, 4, 0.45); P(11, 0, N.B3, 4, 0.40); P(11, 0, N.D4, 4, 0.38);
P(12, 0, N.E3, 4, 0.45); P(12, 0, N.G3, 4, 0.40); P(12, 0, N.B3, 4, 0.38);
P(13, 0, N.C4, 4, 0.48); P(13, 0, N.E4, 4, 0.42); P(13, 0, N.G4, 4, 0.40);
P(14, 0, N.D4, 4, 0.48); P(14, 0, N.A4, 4, 0.42);
P(15, 0, N.G3, 4, 0.50); P(15, 0, N.B3, 4, 0.42); P(15, 0, N.D4, 4, 0.40);
P(16, 0, N.G3, 4, 0.50); P(16, 0, N.B3, 4, 0.42); P(16, 0, N.D4, 4, 0.40);

// bloom (17–20): the held G-major across the whole section — opens wide
P(17, 0, N.G3, 16, 0.55);
P(17, 0, N.B3, 16, 0.48);
P(17, 0, N.D4, 16, 0.45);
P(17, 0, N.G4, 16, 0.42);   // octave double for the bloom

// ── DRIPS (kalimba) — phrase tails ────────────────────────────────────
function K(bar, b, midi, d, g) { drip(t(bar - 1, b), midi, d, g ?? 0.5); }

// shine: a single drip on the tail of bars 6, 8, 10
K(6, 3.5, N.G6, 0.5, 0.40);
K(8, 3.5, N.E6, 0.5, 0.42);
K(10, 2, N.B5, 0.5, 0.38);  K(10, 3, N.D6, 0.5, 0.36);

// wake: drips ride the off-beats lightly, then a small tail on 14, 16
K(12, 3.5, N.D6, 0.5, 0.40);
K(13, 2.5, N.E6, 0.5, 0.38);
K(14, 3.5, N.E6, 0.5, 0.42);
K(15, 2.5, N.G6, 0.5, 0.40);
K(16, 3.5, N.D6, 0.5, 0.38);

// bloom: a soft scatter of drips above the held chord — like dew running
K(17, 2,   N.B5, 0.5, 0.38);
K(18, 1,   N.D6, 0.5, 0.40);
K(18, 3,   N.E6, 0.5, 0.38);
K(19, 1.5, N.G6, 0.5, 0.40);
K(19, 3,   N.D6, 0.5, 0.38);
K(20, 1.5, N.E6, 0.5, 0.40);
K(20, 3,   N.B5, 0.5, 0.36);   // final dew slipping off the leaf

// ── BASS — very sparse low G on bar 1 of each section only ────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.55); }
B(1,  0, N.G2, 4, 0.50);   // drop  — the ground arrives
B(5,  0, N.G2, 4, 0.55);   // shine
B(11, 0, N.G2, 4, 0.60);   // wake
B(17, 0, N.G2, 8, 0.70);   // bloom — held longer under the bloom chord

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

// pad gets a tiny Haas-style widen — sum it slightly delayed into R
// to spread the mist across the stereo field without phasing the leads.
function placeWide(buf, gain, haasMs = 14) {
  const dly = Math.max(1, Math.floor((haasMs / 1000) * SR));
  // left channel: pad slightly left
  const [lLg, lRg] = pan(-0.25);
  // right channel: pad slightly right, delayed
  const [rLg, rRg] = pan(+0.25);
  for (let i = 0; i < ns; i++) {
    const s = buf[i] * gain;
    outL[i] += s * lLg;
    outR[i] += s * lRg;
    if (i + dly < ns) {
      const s2 = buf[i] * gain * 0.85;
      outL[i + dly] += s2 * rLg;
      outR[i + dly] += s2 * rRg;
    }
  }
}

place(leadBuf, 0.00, 1.00);          // glockenspiel — center
placeWide(padBuf, 0.85, 14);         // vibraphone_off — wide mist
place(dripBuf, 0.35, 0.85);          // kalimba — slightly right
place(bassBuf, 0.00, 0.95);          // bass marimba — center

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

// trim trailing silence + tiny fade-in + LONG 2.5s fade-out (let the bloom breathe)
let lastLoud = ns - 1;
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < 0.003 &&
       Math.abs(outR[lastLoud]) < 0.003) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.8 * SR));
const fadeIn = Math.floor(0.008 * SR);
const fadeOut = Math.floor(2.5 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ dewabap · ${TOTAL_BARS} bars · G major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "dewabap.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// SOFTENED master chain — let the dewdrop dynamics breathe. No heavy
// compression, no low-shelf lift, gentle air on top, brickwall safety.
const MASTER = [
  "highpass=f=30",
  "acompressor=threshold=-22dB:ratio=2.0:attack=22:release=220:makeup=1.6:knee=6",
  "treble=g=0.8:f=7000",
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
    _comment: "Section map for dewabap (the dew-drop morning). 4/4, 78 BPM → bar ≈ 3.077 s. G major. Derived from render-dewabap.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 67,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "dewabap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
