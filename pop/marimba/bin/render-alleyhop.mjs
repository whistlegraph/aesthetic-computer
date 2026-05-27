#!/usr/bin/env node
// render-alleyhop.mjs — render the alleyhop playground hop to mp3.
//
// Score notation lives in pop/marimba/alleyhop.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because there's no .np parser in the marimba lane yet.
//
// Voices:
//   xylophone   — the play-calling lead; bright A-major chatter
//   bass        — the BALL; low A2 on beat 1, A3 on beat 3, every bar
//   woodblock   — the rubber-on-wood clack; offbeats 2 & 4
//   kalimba     — the score sparkle; only on score moments
//
// Run:
//   node pop/marimba/bin/render-alleyhop.mjs
//   node pop/marimba/bin/render-alleyhop.mjs --out ~/alleyhop.mp3
//
// All timing in seconds. 128 BPM 4/4 → beat = 60/128 = 0.46875, bar = 1.875.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 128;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 24;
const totalSec = TOTAL_BARS * BAR + 2.5;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// time helper — t(barIndexZeroBased, beatOffsetWithinBar)
const t = (bar, beat = 0) => bar * BAR + beat * BEAT;

// ── note table (A major: A B C# D E F# G#) ────────────────────────────
// MIDI: A2=45, E3=52, A3=57, A4=69, E4=64, A5=81, B5=83, C#6=85, D6=86, E6=88, F#6=90
const N = {
  A2: 45, E3: 52, A3: 57, E4: 64,
  A4: 69, B4: 71, "C#5": 73, D5: 74, E5: 76, "F#5": 78, "G#5": 80,
  A5: 81, B5: 83, "C#6": 85, D6: 86, E6: 88, "F#6": 90, "G#6": 92,
};

// ── output buffers — one mono bus per voice, panned at mixdown ────────
const leadBuf    = new Float32Array(ns);   // xylophone — the play-calling lead
const ballBuf    = new Float32Array(ns);   // bass marimba — the bouncing ball
const dribbleBuf = new Float32Array(ns);   // woodblock — clack on offbeats
const sparkBuf   = new Float32Array(ns);   // kalimba — score sparkle

// ── voice routes ──────────────────────────────────────────────────────
// xylophone is naturally short — extend slightly so held notes still
// carry across a full bar without sounding choked.
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "xylophone", decayMul: 1.15 },
    leadBuf, { sampleRate: SR });
}
// bass: TIGHT bouncing-ball — must feel like a dribble, not a sustain.
// Shorten the natural bass-marimba ring so each bounce is distinct.
function ball(t0, midi, beats, gain = 0.95) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "bass", decayMul: 0.55 },
    ballBuf, { sampleRate: SR });
}
// woodblock: short clack on the offbeats — the rubber-on-wood floor sound.
function dribble(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "woodblock", decayMul: 0.35 },
    dribbleBuf, { sampleRate: SR });
}
// kalimba: bright tine for score-moment sparkle showers.
function spark(t0, midi, beats, gain = 0.6) {
  mixEventMarimba({ startSec: t0, midi, durSec: beats * BEAT,
    gain, preset: "kalimba", decayMul: 0.85 },
    sparkBuf, { sampleRate: SR });
}

// ── sections (matches alleyhop.np) ────────────────────────────────────
const SECTIONS = [
  { name: "dribble", bar0: 0,  bars: 4 },
  { name: "pass",    bar0: 4,  bars: 8 },
  { name: "score",   bar0: 12, bars: 8 },
  { name: "replay",  bar0: 20, bars: 4 },
];

// ── LEAD line (xylophone) — bar-by-bar from alleyhop.np ───────────────
// L(bar1based, beatOffset, midi, durBeats[, gain])
function L(bar, b, midi, d, g) { lead(t(bar - 1, b), midi, d, g ?? 1.0); }

// § dribble (1–4) — climbing major-third leaps "al-LEY OOP"
L(1, 0, N.A5,    2);   L(1, 2, N["C#6"], 2);
L(2, 0, N["C#6"], 2);  L(2, 2, N.E6,    2);
L(3, 0, N.A5,    2);   L(3, 2, N["C#6"], 2);
L(4, 0, N.E6,    2);   L(4, 2, N["C#6"], 2);

// § pass (5–12) — quick syncopated runs "one-TWO THREE / pass-IT-BACK"
L(5, 0, N.A5, 1);    L(5, 1, N.B5, 1);    L(5, 2, N["C#6"], 2);
L(6, 0, N.B5, 1);    L(6, 1, N["C#6"], 1); L(6, 2, N.E6, 2);
L(7, 0, N.A5, 1);    L(7, 1, N.B5, 1);    L(7, 2, N["C#6"], 2);
L(8, 0, N.E6, 1);    L(8, 1, N["C#6"], 1); L(8, 2, N.B5, 1);   L(8, 3, N.A5, 1);
L(9, 0, N.A5, 1);    L(9, 1, N["C#6"], 1); L(9, 2, N.E6, 2);
L(10, 0, N["F#5"], 1); L(10, 1, N["G#5"], 1); L(10, 2, N.A5, 1); L(10, 3, N.B5, 1);
L(11, 0, N.E6, 1);   L(11, 1, N.D6, 1);   L(11, 2, N["C#6"], 2);
L(12, 0, N["C#6"], 2); L(12, 2, N.B5, 1);  L(12, 3, N.A5, 1);

// § score (13–20) — held high E6 + descending celebration
L(13, 0, N.E6, 4, 0.95);
L(14, 0, N["C#6"], 1); L(14, 1, N.B5, 1);  L(14, 2, N.A5, 2);
L(15, 0, N.E6, 4, 0.95);
L(16, 0, N.E6, 1);   L(16, 1, N["C#6"], 1); L(16, 2, N.A5, 2);
L(17, 0, N.E6, 2);   L(17, 2, N["C#6"], 1); L(17, 3, N.E6, 1);
L(18, 0, N["F#6"], 1); L(18, 1, N.E6, 1);   L(18, 2, N["C#6"], 2);
L(19, 0, N.B5, 1);   L(19, 1, N["C#6"], 1); L(19, 2, N.E6, 1); L(19, 3, N.D6, 1);
L(20, 0, N["C#6"], 2); L(20, 2, N.A5, 2);

// § replay (21–24) — looping back to dribble, fades to A chord
L(21, 0, N.A5, 1);   L(21, 1, N.B5, 1);   L(21, 2, N["C#6"], 2);
L(22, 0, N["C#6"], 1); L(22, 1, N.E6, 1);
L(23, 0, N.A5, 1);   L(23, 1, N["C#6"], 1); L(23, 2, N.E6, 2);
L(24, 0, N.A4, 4, 0.9);   // low landing A

// ── BASS — the BOUNCING BALL: low A2 on beat 1, A3 on beat 3, every bar
// This is the dribble pulse. It does NOT stop until the final chord.
function B(bar, b, midi, d, g) { ball(t(bar - 1, b), midi, d, g ?? 0.95); }
// dribble + pass (bars 1–12): classic ball-bounce, low/high octave alternation
for (let bar = 1; bar <= 12; bar++) {
  B(bar, 0, N.A2, 1.8);
  B(bar, 2, N.A3, 1.8);
}
// score (bars 13–20): keep the ball bouncing — game still on
// add an E2-flavored variation on the "two mo' points" bar for lift
for (let bar = 13; bar <= 20; bar++) {
  if (bar === 17 || bar === 19) {
    B(bar, 0, N.E3, 1.8, 0.85);   // E for the IV→V color
    B(bar, 2, N.A3, 1.8);
  } else {
    B(bar, 0, N.A2, 1.8);
    B(bar, 2, N.A3, 1.8);
  }
}
// replay (21–23): final bounces
B(21, 0, N.A2, 1.8);   B(21, 2, N.A3, 1.8);
B(22, 0, N.A2, 1.8);   B(22, 2, N.E3, 1.8);
B(23, 0, N.A2, 1.8);   B(23, 2, N.A3, 1.8);
// bar 24: the landing — deep low A2 sustains under the A4 lead
B(24, 0, N.A2, 4, 0.85);

// ── WOODBLOCK — clack on every offbeat (2 & 4), the rubber-on-wood floor
function W(bar, b, midi, d, g) { dribble(t(bar - 1, b), midi, d, g ?? 0.55); }
// Steady clack across dribble + pass + score (1–20)
for (let bar = 1; bar <= 20; bar++) {
  W(bar, 1, N.A5, 0.4);   // beat 2
  W(bar, 3, N.A5, 0.4);   // beat 4
  // tiny &-of-1 / &-of-3 ghost clacks during pass section for syncopation
  if (bar >= 5 && bar <= 12) {
    W(bar, 0.5, N.A5, 0.3, 0.35);
    W(bar, 2.5, N.A5, 0.3, 0.35);
  }
}
// replay: clacks fade — keep them on 21–23, drop on 24 (final chord)
for (let bar = 21; bar <= 23; bar++) {
  W(bar, 1, N.A5, 0.4);
  W(bar, 3, N.A5, 0.4);
}

// ── KALIMBA — score sparkle showers (only on score + replay) ──────────
function K(bar, b, midi, d, g) { spark(t(bar - 1, b), midi, d, g ?? 0.6); }
// score: sparkle haloes above the held E6 + on the celebration bars
K(13, 0, N.E6, 0.5);   K(13, 1, N["G#6"], 0.5);  K(13, 2, N.E6, 0.5);  K(13, 3, N["C#6"], 0.5);
K(14, 3.5, N.A5, 0.5, 0.5);
K(15, 0, N["F#6"], 0.5); K(15, 1, N.E6, 0.5);   K(15, 2, N["C#6"], 0.5); K(15, 3, N.E6, 0.5);
K(16, 3.5, N["C#6"], 0.5, 0.5);
K(17, 1.5, N.E6, 0.5, 0.5);
K(18, 0.5, N["G#6"], 0.5, 0.5);
K(19, 2.5, N["F#6"], 0.5, 0.5);
K(20, 2, N.E6, 0.5);   K(20, 3, N["C#6"], 0.5);
// replay: small fading sparkle
K(22, 2, N.E6, 0.5, 0.5);
K(23, 3.5, N["C#6"], 0.5, 0.5);
K(24, 1, N.E6, 0.5, 0.45);   // one last drifting sparkle over the low A

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
// xylo center, bass center, woodblock slightly left, kalimba right
place(leadBuf,    0.00, 1.00);
place(ballBuf,    0.00, 0.95);
place(dribbleBuf,-0.35, 0.80);
place(sparkBuf,   0.40, 0.85);

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

console.log(`→ alleyhop · ${TOTAL_BARS} bars · A major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "alleyhop.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — bright, punchy, athletic. Same flutterbap chain.
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
    _comment: "Section map for alleyhop (the playground basketball hop). 4/4, 128 BPM → bar = 1.875 s. A major. Derived from render-alleyhop.mjs SECTIONS.",
    meter: 4, bpm: BPM, scale: "major", rootMidi: 69,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +t(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "alleyhop.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
