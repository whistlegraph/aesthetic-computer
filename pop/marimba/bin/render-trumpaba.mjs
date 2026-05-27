#!/usr/bin/env node
// render-trumpaba.mjs — render the trumpaba town-crier fanfare to mp3.
//
// Score notation lives in pop/marimba/trumpaba.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   kelon          — the herald; synthetic-bar marimba lead (trumpet stand-in)
//   bass           — parade pulse (low B♭ / F alternating, on 1 & 3)
//   glockenspiel   — high steel ping on the phrase tops (heraldic chime)
//   staccato       — tongued attacks on the downbeats (announce-it bark)
//
// Run:
//   node pop/marimba/bin/render-trumpaba.mjs
//   node pop/marimba/bin/render-trumpaba.mjs --out ~/trumpaba.mp3
//
// All timing in seconds. 120 BPM 4/4 → beat = 0.500, bar = 2.000.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 120;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (B♭ major centric) ─────────────────────────────────────
// B♭ = MIDI 70 (B♭4). B♭3 = 58. B♭2 = 46.
const N = {
  Bb2: 46, F2: 41,
  Bb3: 58, D4: 62, F3: 53,
  Bb4: 70, D5: 74, F4: 65, F5: 77,
  Bb5: 82, D6: 86, F6: 89,
  // diatonic neighbors (B♭ major: B♭ C D E♭ F G A)
  C5: 72, Eb5: 75, G5: 79, A5: 81,
  C6: 84, Eb6: 87,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf   = new Float32Array(ns);   // kelon — the herald
const bassBuf   = new Float32Array(ns);   // bass marimba — parade pulse
const bellBuf   = new Float32Array(ns);   // glockenspiel — high heraldic ping
const tongueBuf = new Float32Array(ns);   // staccato — tongued bark

// ── voice routes ──────────────────────────────────────────────────────
// kelon: synthetic bar, brighter + slightly shorter than rosewood;
// closest mallet stand-in for tongued trumpet attacks.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kelon", decayMul: 1.0 },
    leadBuf, { sampleRate: SR });
}
// bass: tight parade pulse — punchy, not droning. Shorten the natural
// bass-marimba ring so each pulse is distinct.
function bass(t0, midi, beats, gain = 0.88) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    bassBuf, { sampleRate: SR });
}
// glockenspiel: high steel, let it ring heraldic.
function bell(t0, midi, beats, gain = 0.65) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "glockenspiel", decayMul: 1.30 },
    bellBuf, { sampleRate: SR });
}
// staccato: dry hard-mallet bark for the announce-it tongue attacks.
function tongue(t0, midi, beats, gain = 0.60) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "staccato", decayMul: 0.45 },
    tongueBuf, { sampleRate: SR });
}

// ── sections (matches trumpaba.np) ────────────────────────────────────
const SECTIONS = [
  { name: "ta_da",    bar0: 0,  bars: 4 },
  { name: "heralds",  bar0: 4,  bars: 6 },
  { name: "announce", bar0: 10, bars: 8 },
  { name: "parade",   bar0: 18, bars: 6 },
];

// ── LEAD line (kelon) — bar-by-bar from trumpaba.np ───────────────────
// K(bar1based, beatOffset, midi, durBeats[, gain])
function K(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § ta_da (1–4) — jumping-fifth fanfare motif
K(1, 0, N.Bb4, 2);  K(1, 2, N.F5,  2);
K(2, 0, N.F5,  2);  K(2, 2, N.Bb5, 2);
K(3, 0, N.Bb4, 2);  K(3, 2, N.F5,  2);
K(4, 0, N.F5,  2);  K(4, 2, N.Bb5, 2);

// § heralds (5–10) — three-note rising arpeggios
K(5, 0, N.Bb5, 1);  K(5, 1, N.D6,  1);  K(5, 2, N.F6,  2);
K(6, 0, N.Bb5, 1);  K(6, 1, N.D6,  1);  K(6, 2, N.F6,  2);
K(7, 0, N.Bb5, 1);  K(7, 1, N.D6,  1);  K(7, 2, N.F6,  2);
K(8, 0, N.Bb5, 1);  K(8, 1, N.D6,  1);  K(8, 2, N.F6,  2);
K(9, 0, N.Bb5, 1);  K(9, 1, N.D6,  1);  K(9, 2, N.F6,  1);  K(9, 3, N.D6, 1);
K(10, 0, N.Bb5, 1); K(10, 1, N.D6, 1);  K(10, 2, N.F6, 2);

// § announce (11–18) — march-like four-note phrases on B♭-D-F triad tones
K(11, 0, N.Bb4, 1); K(11, 1, N.D5,  1); K(11, 2, N.F5,  2);
K(12, 0, N.D5,  1); K(12, 1, N.F5,  1); K(12, 2, N.Bb5, 2);
K(13, 0, N.Bb4, 1); K(13, 1, N.D5,  1); K(13, 2, N.F5,  2);
K(14, 0, N.D5,  1); K(14, 1, N.F5,  1); K(14, 2, N.Bb5, 2);
K(15, 0, N.F5,  1); K(15, 1, N.Bb5, 1); K(15, 2, N.F5,  2);
K(16, 0, N.D5,  1); K(16, 1, N.F5,  1); K(16, 2, N.Bb5, 2);
K(17, 0, N.F5,  1); K(17, 1, N.Bb5, 1); K(17, 2, N.D6,  2);
K(18, 0, N.Bb5, 1); K(18, 1, N.D6,  1); K(18, 2, N.F6,  2);

// § parade (19–24) — final cadence, descending from F6 to home B♭4
K(19, 0, N.F6,  2); K(19, 2, N.D6,  2);
K(20, 0, N.D6,  2); K(20, 2, N.Bb5, 2);
K(21, 0, N.F5,  2); K(21, 2, N.D5,  2);
K(22, 0, N.Bb5, 2); K(22, 2, N.F5,  2);
K(23, 0, N.D5,  1); K(23, 1, N.F5,  1); K(23, 2, N.Bb5, 2);
// final home — B♭ major triad held across all lead voices
K(24, 0, N.Bb4, 4, 1.0);
K(24, 0, N.D5,  4, 0.85);
K(24, 0, N.F5,  4, 0.80);
K(24, 0, N.Bb5, 4, 0.75);

// ── BASS — parade pulse on 1 & 3 ──────────────────────────────────────
function B(bar, b, midi, d, g) { bass(t(bar - 1, b), midi, d, g ?? 0.88); }
// ta_da: low B♭ / F alternating, classic parade pulse on 1 & 3
B(1, 0, N.Bb2, 1.8); B(1, 2, N.F3,  1.8);
B(2, 0, N.Bb2, 1.8); B(2, 2, N.F3,  1.8);
B(3, 0, N.Bb2, 1.8); B(3, 2, N.F3,  1.8);
B(4, 0, N.Bb2, 1.8); B(4, 2, N.F3,  1.8);
// heralds: bass keeps the pace, alternating B♭2 / F2 octave
B(5, 0, N.Bb2, 1.8); B(5, 2, N.F2,  1.8);
B(6, 0, N.Bb2, 1.8); B(6, 2, N.F2,  1.8);
B(7, 0, N.Bb2, 1.8); B(7, 2, N.F2,  1.8);
B(8, 0, N.Bb2, 1.8); B(8, 2, N.F2,  1.8);
B(9, 0, N.Bb2, 1.8); B(9, 2, N.F2,  1.8);
B(10, 0, N.Bb2, 1.8); B(10, 2, N.F2, 1.8);
// announce: I-IV-V-I chord pad alternation, march-feel
//   bars 11-12: B♭ → E♭   (I → IV)
//   bars 13-14: B♭ → E♭   (I → IV)
//   bars 15-16: F → B♭    (V → I)
//   bars 17-18: F → B♭    (V → I, prep cadence)
B(11, 0, N.Bb2, 1.8); B(11, 2, N.F3,  1.8);
B(12, 0, N.Eb5 - 24, 1.8); B(12, 2, N.Bb2, 1.8);   // E♭2 = 39
B(13, 0, N.Bb2, 1.8); B(13, 2, N.F3,  1.8);
B(14, 0, N.Eb5 - 24, 1.8); B(14, 2, N.Bb2, 1.8);
B(15, 0, N.F2,  1.8); B(15, 2, N.Bb2, 1.8);
B(16, 0, N.F2,  1.8); B(16, 2, N.Bb2, 1.8);
B(17, 0, N.F2,  1.8); B(17, 2, N.Bb2, 1.8);
B(18, 0, N.F2,  1.8); B(18, 2, N.Bb2, 1.8);
// parade: cadence — descend through the bass
B(19, 0, N.F2,  1.8); B(19, 2, N.Bb2, 1.8);
B(20, 0, N.F2,  1.8); B(20, 2, N.Bb2, 1.8);
B(21, 0, N.Bb2, 1.8); B(21, 2, N.F2,  1.8);
B(22, 0, N.F2,  1.8); B(22, 2, N.Bb2, 1.8);
B(23, 0, N.F2,  1.5); B(23, 2, N.F2,  1.5);
// final landing — B♭2 held into the home triad
B(24, 0, N.Bb2, 4, 0.92);

// ── GLOCKENSPIEL — high heraldic ping on phrase tops ──────────────────
function G(bar, b, midi, d, g) { bell(t(bar - 1, b), midi, d, g ?? 0.65); }
// ta_da: a small ping on the DA of each fanfare
G(1, 2, N.F6,  2, 0.50);
G(2, 2, N.Bb5, 2, 0.55);
G(3, 2, N.F6,  2, 0.55);
G(4, 2, N.Bb5, 2, 0.65);
// heralds: ring the top F6 of each three-note arpeggio
G(5, 2, N.F6,  2, 0.70);
G(6, 2, N.F6,  2, 0.70);
G(7, 2, N.F6,  2, 0.75);
G(8, 2, N.F6,  2, 0.75);
G(9, 2, N.F6,  1, 0.80); G(9, 3, N.D6, 1, 0.65);
G(10, 2, N.F6, 2, 0.85);
// announce: chime on the top note of each four-note phrase
G(11, 2, N.F5,  2, 0.45);
G(12, 2, N.Bb5, 2, 0.55);
G(13, 2, N.F5,  2, 0.50);
G(14, 2, N.Bb5, 2, 0.60);
G(15, 2, N.F5,  2, 0.50);
G(16, 2, N.Bb5, 2, 0.65);
G(17, 2, N.D6,  2, 0.75);
G(18, 2, N.F6,  2, 0.80);
// parade: trail the descending lead with chimes
G(19, 0, N.F6,  2, 0.80);
G(20, 0, N.D6,  2, 0.70);
G(21, 0, N.F5,  2, 0.55);
G(22, 0, N.Bb5, 2, 0.65);
G(23, 2, N.Bb5, 2, 0.70);
// final cadence — ring out the home triad up top
G(24, 0, N.Bb5, 4, 0.75);
G(24, 0, N.D6,  4, 0.60);
G(24, 0, N.F6,  4, 0.55);

// ── STACCATO — tongued bark on the announce downbeats ─────────────────
function S(bar, b, midi, d, g) { tongue(t(bar - 1, b), midi, d, g ?? 0.55); }
// ta_da: a tiny bark in front of each phrase top (octave below lead's high)
S(1, 0, N.Bb4, 0.4, 0.55);
S(2, 0, N.F5,  0.4, 0.55);
S(3, 0, N.Bb4, 0.4, 0.55);
S(4, 0, N.F5,  0.4, 0.60);
// heralds: short bark on each downbeat to drive the parade
S(5, 0, N.Bb4, 0.35, 0.50);
S(6, 0, N.Bb4, 0.35, 0.50);
S(7, 0, N.Bb4, 0.35, 0.55);
S(8, 0, N.Bb4, 0.35, 0.55);
S(9, 0, N.Bb4, 0.35, 0.60);
S(10, 0, N.Bb4, 0.35, 0.60);
// announce: this is the staccato section — bark on EVERY beat the
// lead lands on (downbeats 1 & 3 + the ANSWER syllable in between)
S(11, 0, N.Bb3, 0.3); S(11, 1, N.D4, 0.3, 0.45); S(11, 2, N.F4, 0.3, 0.55);
S(12, 0, N.D4,  0.3); S(12, 1, N.F4, 0.3, 0.45); S(12, 2, N.Bb4, 0.3, 0.55);
S(13, 0, N.Bb3, 0.3); S(13, 1, N.D4, 0.3, 0.45); S(13, 2, N.F4, 0.3, 0.55);
S(14, 0, N.D4,  0.3); S(14, 1, N.F4, 0.3, 0.45); S(14, 2, N.Bb4, 0.3, 0.55);
S(15, 0, N.F4,  0.3); S(15, 1, N.Bb4, 0.3, 0.45); S(15, 2, N.F4, 0.3, 0.55);
S(16, 0, N.D4,  0.3); S(16, 1, N.F4, 0.3, 0.45); S(16, 2, N.Bb4, 0.3, 0.55);
S(17, 0, N.F4,  0.3); S(17, 1, N.Bb4, 0.3, 0.45); S(17, 2, N.D5, 0.3, 0.60);
S(18, 0, N.Bb4, 0.3); S(18, 1, N.D5, 0.3, 0.45); S(18, 2, N.F5, 0.3, 0.65);
// parade: barks reinforcing the cadence descent
S(19, 0, N.F5,  0.35, 0.55);
S(20, 0, N.D5,  0.35, 0.55);
S(21, 0, N.F4,  0.35, 0.50);
S(22, 0, N.Bb4, 0.35, 0.55);
S(23, 2, N.Bb4, 0.35, 0.65);
// final tongue bark on the home — emphatic announce-it
S(24, 0, N.Bb4, 0.5, 0.80);

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
place(leadBuf,    0.00, 1.00);   // kelon centered — the herald is the show
place(bassBuf,    0.00, 0.95);   // bass centered — parade pulse anchor
place(bellBuf,    0.35, 0.82);   // glockenspiel right — sparkle to one side
place(tongueBuf, -0.30, 0.78);   // staccato left — tongue barks to the other

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

console.log(`→ trumpaba · ${TOTAL_BARS} bars · B♭ major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "trumpaba.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — bright, punchy, heraldic. flutterbap's daytime
// chain works for trumpaba too: gentle glue comp, treble air for the
// glockenspiel, brickwall at the top. parade-bright energy.
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
    _comment: "Section map for trumpaba (the town-crier fanfare). 4/4, 120 BPM → bar = 2.0 s. B♭ major. Derived from render-trumpaba.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 70,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "trumpaba.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
