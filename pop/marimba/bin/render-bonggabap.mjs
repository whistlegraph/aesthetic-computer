#!/usr/bin/env node
// render-bonggabap.mjs — render the bonggabap midnight-bell to mp3.
//
// Score notation lives in pop/marimba/bonggabap.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   gamelan       — the BELL; bronze inharmonic, long ring (lead)
//   bass          — deep gong-low C/G pulse on beat 1 only
//   glockenspiel  — high struck overtones, the bell's upper harmonics
//   rosewood      — the monk humming softly, a quiet counter-melody
//
// Run:
//   node pop/marimba/bin/render-bonggabap.mjs
//   node pop/marimba/bin/render-bonggabap.mjs --out ~/bonggabap.mp3
//
// All timing in seconds. 88 BPM 4/4 → beat = 60/88 ≈ 0.682, bar ≈ 2.727.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 88;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 4.5;   // + long tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table ────────────────────────────────────────────────────────
// C natural minor: C D Eb F G Ab Bb
const N = {
  C2: 36, G2: 43,
  C3: 48, Eb3: 51, F3: 53, G3: 55, Ab3: 56, Bb3: 58,
  C4: 60, D4: 62, Eb4: 63, F4: 65, G4: 67, Ab4: 68, Bb4: 70,
  C5: 72, D5: 74, Eb5: 75, F5: 77, G5: 79, Ab5: 80, Bb5: 82,
  C6: 84, D6: 86, Eb6: 87, F6: 89, G6: 91,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf  = new Float32Array(ns);   // gamelan — the bell
const bassBuf  = new Float32Array(ns);   // bass marimba — gong-low pulse
const bellBuf  = new Float32Array(ns);   // glockenspiel — high overtones
const monkBuf  = new Float32Array(ns);   // rosewood — monk's hum

// ── voice routes ──────────────────────────────────────────────────────
// gamelan: bronze inharmonic — this preset IS the bell. Long ring.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "gamelan", decayMul: 1.5 },
    leadBuf, { sampleRate: SR });
}
// bass marimba: gong-low. Longer ring so the floor sustains under the bell.
function bass(t0, midi, beats, gain = 0.9) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.85 },
    bassBuf, { sampleRate: SR });
}
// glockenspiel: the bell's upper harmonics, struck steel halo.
function bell(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "glockenspiel", decayMul: 1.4 },
    bellBuf, { sampleRate: SR });
}
// rosewood: the monk humming, soft counter-melody. Natural ring.
function monk(t0, midi, beats, gain = 0.42) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "rosewood", decayMul: 1.0 },
    monkBuf, { sampleRate: SR });
}

// ── sections (matches bonggabap.np) ───────────────────────────────────
const SECTIONS = [
  { name: "bong1",  bar0: 0,  bars: 4 },
  { name: "gong",   bar0: 4,  bars: 6 },
  { name: "hour",   bar0: 10, bars: 8 },
  { name: "chime",  bar0: 18, bars: 6 },
];

// ── LEAD line (gamelan — the bell) — bar-by-bar from bonggabap.np ─────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § bong1 (1–4) — single deep strike, ringing
L(1, 0, N.C4, 4);                                      // BONG (rings full bar)
// bar 2 — let it ring, no new strike
L(3, 0, N.C4, 2);   L(3, 2, N.G4, 2);                  // bong-BONG
// bar 4 — ring out

// § gong (5–10) — alternating root / fifth, ceremonial
L(5, 0, N.C5, 2);   L(5, 2, N.G4, 2);                  // gong-GONG
L(6, 0, N.G4, 2);   L(6, 2, N.C5, 2);                  // bong-BONG
L(7, 0, N.C5, 4);                                      // gong (held)
L(8, 0, N.G4, 4);                                      // GONG (held)
L(9, 0, N.C5, 2);   L(9, 2, N.G4, 2);                  // bong-BONG
L(10, 0, N.C5, 4);                                     // gong (held)

// § hour (11–18) — slow ascending bell sequence
L(11, 0, N.Eb5, 4);                                    // hour
L(12, 0, N.F5, 4);                                     // OF
L(13, 0, N.G5, 4);                                     // mid
L(14, 0, N.Bb5, 4);                                    // NIGHT
L(15, 0, N.Eb5, 2);  L(15, 2, N.G5, 2);                // hour-OF
L(16, 0, N.F5, 2);   L(16, 2, N.Bb5, 2);               // mid-NIGHT
L(17, 0, N.G5, 4);                                     // turn (monk under)
L(18, 0, N.F5, 4);                                     // ing  (monk under)

// § chime (19–24) — final triple-strike fading into the night
L(19, 0, N.C5, 1);   L(19, 1, N.G5, 1);   L(19, 2, N.C6, 2);   // ring-the-BELL
L(20, 0, N.C5, 1);   L(20, 1, N.G5, 1);   L(20, 2, N.C6, 2);   // ring-the-BELL
L(21, 0, N.C5, 4, 1.05);                                       // BONG (long, slightly louder)
// bars 22–24 — ring out, all voices fade

// ── BASS — gong-low C/G pulse on beat 1 only ──────────────────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.9); }
// bong1: deep gong-low C2 anchoring bar 1 and bar 3
B(1, 0, N.C2, 4, 0.95);
B(3, 0, N.C2, 4, 0.85);

// gong: pulse on beat 1 of each bar — alternating C2 / G2
B(5, 0, N.C2, 4, 0.85);
B(6, 0, N.G2, 4, 0.75);
B(7, 0, N.C2, 4, 0.85);
B(8, 0, N.G2, 4, 0.75);
B(9, 0, N.C2, 4, 0.85);
B(10, 0, N.C2, 4, 0.80);

// hour: ascending bars carry a softer floor — Cm → Fm motion
B(11, 0, N.C2, 4, 0.65);                  // Cm floor
B(12, 0, N.C2, 4, 0.60);                  // (still Cm)
B(13, 0, N.F2 ?? N.F3, 4, 0.0);           // skipped — let the gamelan breathe
B(14, 0, N.G2, 4, 0.65);                  // Gm motion
B(15, 0, N.C2, 4, 0.65);
B(16, 0, N.G2, 4, 0.65);
B(17, 0, N.C2, 4, 0.60);
B(18, 0, N.C2, 4, 0.55);

// chime: deep final tolls
B(19, 0, N.C2, 4, 0.90);
B(20, 0, N.C2, 4, 0.85);
B(21, 0, N.C2, 4, 0.95);                  // deep final BONG floor

// ── GLOCKENSPIEL — high struck overtones ──────────────────────────────
function G(bar, b, midi, d, g) { bell(t(bar - 1, b), midi, d, g ?? 0.55); }
// bong1: a single high ping on bar 1 to seed the bell color
G(1, 0, N.C6, 4, 0.35);

// gong: pings on phrase tops (bars 7, 8) when the gamelan holds
G(7, 0, N.C6, 4, 0.50);
G(8, 0, N.G5, 4, 0.45);
G(9, 2, N.G6, 2, 0.40);

// hour: high overtones ride above the ascending bell
G(11, 0, N.Eb6, 4, 0.35);
G(13, 0, N.G6, 4, 0.40);
G(14, 0, N.D6, 4, 0.35);   // Bb's third partial-ish color
G(16, 2, N.D6, 2, 0.45);

// chime: bright bell-top sparkle on the final triple-strikes
G(19, 2, N.C6, 2, 0.65);
G(20, 2, N.C6, 2, 0.65);
G(21, 0, N.G5, 3, 0.55);
G(21, 1, N.C6, 3, 0.50);
G(22, 0, N.Eb6, 4, 0.40);                  // a slow drifting ping into the night
G(23, 0, N.G6, 4, 0.30);                   // farthest, fading

// ── ROSEWOOD — the monk humming a counter-melody ──────────────────────
function R(bar, b, midi, d, g) { monk(t(bar - 1, b), midi, d, g ?? 0.42); }
// silent through bong1 and most of gong — the monk enters when the bell turns
R(9, 1, N.Eb4, 1, 0.30);   R(9, 2, N.D4, 1, 0.30);              // tiny breath under bar 9
R(10, 0, N.Eb4, 2, 0.32);  R(10, 2, N.G4, 2, 0.32);

// hour: full counter-melody — slow, different rhythm from the bell
R(11, 1, N.G4, 1, 0.40);   R(11, 2, N.Bb4, 2, 0.40);            // under "hour"
R(12, 0, N.C5, 1, 0.42);   R(12, 1, N.Bb4, 1, 0.40);   R(12, 2, N.Ab4, 2, 0.38);
R(13, 1, N.G4, 2, 0.42);   R(13, 3, N.F4, 1, 0.38);             // a sigh under "mid"
R(14, 0, N.F4, 1, 0.40);   R(14, 1, N.Eb4, 1, 0.38);   R(14, 2, N.D4, 2, 0.36);
R(15, 1, N.Eb4, 1, 0.40);  R(15, 3, N.G4, 1, 0.38);
R(16, 0, N.F4, 1, 0.40);   R(16, 2, N.Eb4, 2, 0.38);
R(17, 0, N.D4, 2, 0.42);   R(17, 2, N.Eb4, 2, 0.40);            // monk continues turning
R(18, 0, N.D4, 2, 0.40);   R(18, 2, N.C4, 2, 0.40);             // settling toward home

// chime: a final breath under the closing strikes
R(19, 3, N.G4, 1, 0.32);
R(20, 3, N.Eb4, 1, 0.30);
R(21, 0, N.C4, 4, 0.42);                                        // home tone
R(22, 0, N.C4, 4, 0.30);                                        // continues fading

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
place(leadBuf,   0.00, 1.00);    // gamelan — center
place(bassBuf,   0.00, 0.95);    // bass — center
place(bellBuf,   0.35, 0.82);    // glockenspiel — slightly right
place(monkBuf,  -0.25, 0.85);    // rosewood — slightly left

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
if (peak > 0) { const nrm = 0.84 / peak;
  for (let i = 0; i < ns; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

// trim trailing silence + LONG fade-out (3s) for the ringing bell into night
let lastLoud = ns - 1;
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < 0.003 &&
       Math.abs(outR[lastLoud]) < 0.003) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.5 * SR));
const fadeIn = Math.floor(0.005 * SR);
const fadeOut = Math.floor(3.0 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn; outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ bonggabap · ${TOTAL_BARS} bars · C minor 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "bonggabap.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// master chain — mid-softened for a solemn ceremonial bell. Highpass at 30
// to keep the gong floor. Gentle compression. Soft top air. Brickwall.
const MASTER = [
  "highpass=f=30",
  "acompressor=threshold=-20dB:ratio=2.2:attack=20:release=220:makeup=1.5:knee=6",
  "treble=g=0.8:f=7500",
  "alimiter=limit=0.96:attack=5:release=80",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (mid-softened · ${(trimN / SR).toFixed(1)} s)`);

// ── struct.json — section map for any future visualizer ───────────────
{
  const structTotalSec = trimN / SR;
  const struct = {
    _comment: "Section map for bonggabap (the midnight temple bell). 4/4, 88 BPM → bar ≈ 2.727 s. C minor. Derived from render-bonggabap.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "minor", rootMidi: 60,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "bonggabap.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
