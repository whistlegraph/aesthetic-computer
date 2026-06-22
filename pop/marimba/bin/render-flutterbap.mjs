#!/usr/bin/env node
// render-flutterbap.mjs — render the flutterbap daytime hop to mp3.
//
// Score notation lives in pop/marimba/flutterbap.np (the human-readable
// form). This script holds the same music as scheduled per-voice events.
// Inline because there's no .np parser in the marimba lane yet.
//
// ── the build-out (next stage of /pop) ─────────────────────────────────
// flutterbap started as four mallet voices with no low end. This stage
// adds a synthesized rhythm section (../synths/perc.mjs — sample-free,
// same posture as the marimba modal synth) and complexifies the form:
//
//   • a 4-bar drum-led INTRO that builds the groove before the hook
//   • a bass-perc bassline (the "sick" gritty 808-flavoured low voice)
//     that bounces under the whole track and carries the groove
//   • a kick / snare / hat / shaker kit with per-section dynamics
//     (light under the hook, pulled back for the "mommy-wow" hush,
//      driving through slinky + fly, resolving under the landing)
//   • a vibraphone counter-pad sustaining the harmony under the lead
//   • a 4-bar OUTRO that reprises the butterfly hook over the groove
//     and buttons the song with a final resolved hit
//
// Mallet voices (unchanged from the original hop):
//   xylophone     — the singer; bright daytime syllabic lead
//   bass(marimba) — the bouncing-ball mallet bass on 1 & 3
//   kalimba       — twinkles on phrase tails (off-beat sparkle)
//   glockenspiel  — high steel sparkle on mommy-wow + the fly climaxes
// New voices (this stage):
//   vibraphone    — soft sustained chord pad under the lead
//   bass-perc     — gritty pitched sub; the groove's backbone
//   kick/snare/hat/shaker — the synthesized kit
//
// Run:
//   node pop/marimba/bin/render-flutterbap.mjs
//   node pop/marimba/bin/render-flutterbap.mjs --out ~/flutterbap.mp3
//
// All timing in seconds. 124 BPM 4/4 → beat = 60/124 ≈ 0.484, bar ≈ 1.935.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { mixKick, mixBassPerc, mixSnare, mixHat, mixShaker, mixReverseBell, mixReverseKick, mixScratch, mixScream, renderHat } from "../synths/perc.mjs";
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

// ── form ────────────────────────────────────────────────────────────────
// The original hook/verse content is 32 bars. We bookend it with a drum
// intro and an outro reprise. Everything mallet-related is written against
// the ORIGINAL 1-based body bar numbers and shifted by INTRO_BARS at the
// helper, so the hook score never had to be renumbered.
const INTRO_BARS = 4;
const BODY_BARS = 32;
// extended coda after the landing: cave (4) → squabble (2) → chord
// progression + button (5) = 11 bars.
const OUTRO_BARS = 11;
const TOTAL_BARS = INTRO_BARS + BODY_BARS + OUTRO_BARS;   // 47
const totalSec = TOTAL_BARS * BAR + 2.8;   // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// ── swing ────────────────────────────────────────────────────────────
// A straight grid marches; swing makes it skip. SWING is where the "&"
// sits inside each beat — 0.5 = dead straight, 0.667 = full triplet swing.
// We default to a relaxed 0.60 hop and expose --swing. swingBeat() maps a
// musical beat offset onto the swung grid: the first 8th of each beat is
// squeezed earlier, the & is pushed late, and 16ths stretch proportionally
// inside each half so fast runs swing too. Integer/half-beat downbeats are
// untouched (so section starts, risers and the kick anchor stay locked).
const SWING = Math.min(0.72, Math.max(0.5, Number(_argi("--swing")) || 0.60));
function swingBeat(x) {
  const q = Math.floor(x + 1e-9);       // which quarter
  const r = x - q;                      // position within the quarter (0..1)
  let rp;
  if (r < 0.5) rp = r * 2 * SWING;                       // first 8th → [0, SWING)
  else rp = SWING + (r - 0.5) * 2 * (1 - SWING);          // the & → [SWING, 1)
  return q + rp;
}
// time helper — t(absBarZeroBased, beatOffsetWithinBar), swing applied
const t = (bar, beat = 0) => bar * BAR + swingBeat(beat) * BEAT;
// body bar (1-based) → absolute time
const tb = (bar1, beat = 0) => t(bar1 - 1 + INTRO_BARS, beat);

// ── humanize ───────────────────────────────────────────────────────────
// A quantised grid sounds like a machine. Real players push and drag a few
// ms off the beat and never hit two notes at the same velocity. We add a
// deterministic (reproducible) per-note micro-timing jitter and a velocity
// (gain) wobble. Chord notes jitter independently, so block chords get a
// natural micro-strum. Pass --robotic to disable. mulberry32 RNG keeps the
// sequence identical run-to-run (the scheduling order is fixed).
const HUMANIZE = !process.argv.includes("--robotic");
let _hs = 0x1a2b3c4d >>> 0;
function hrand() {
  _hs = (_hs + 0x6d2b79f5) >>> 0;
  let x = Math.imul(_hs ^ (_hs >>> 15), 1 | _hs);
  x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x;
  return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
}
const hr2 = () => hrand() * 2 - 1;                               // −1..1
// jitter a start time by ±ms; never let it go negative (clip at song start)
const jit = (t0, ms) => (HUMANIZE ? Math.max(0, t0 + hr2() * ms / 1000) : t0);
// wobble a gain by ±amt (fraction); clamp ≥0
const vel = (g, amt) => (HUMANIZE ? Math.max(0, g * (1 + hr2() * amt)) : g);

// ── note table ────────────────────────────────────────────────────────
const N = {
  C1: 24, E1: 28, F1: 29, G1: 31, A1: 33,
  C2: 36, D2: 38, E2: 40, F2: 41, G2: 43, A2: 45,
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
const vibBuf   = new Float32Array(ns);   // vibraphone — sustained pad
const riseBuf  = new Float32Array(ns);   // reverse sine-bell risers (breaks)
const scrBuf   = new Float32Array(ns);   // turntable scratches
const screamBuf = new Float32Array(ns);  // scratchy kitten screams
const kickBuf  = new Float32Array(ns);   // kick drum
const subBuf   = new Float32Array(ns);   // bass-perc — gritty pitched sub
const snareBuf = new Float32Array(ns);   // snare
const hatBuf   = new Float32Array(ns);   // hats (closed + open)
const shakBuf  = new Float32Array(ns);   // shaker
const caveBuf  = new Float32Array(ns);   // CAVE section bus — gets a big Schroeder reverb at mixdown

// ── mallet voice routes (humanized: ±ms timing, ±% velocity) ──────────
function lead(t0, midi, beats, gain = 1.0) {
  mixEventMarimba({ startSec: jit(t0, 13), midi, durSec: beats * BEAT,
    gain: vel(gain, 0.14), preset: "xylophone", decayMul: 1.15 }, leadBuf, { sampleRate: SR });
}
function bass(t0, midi, beats, gain = 0.85) {
  mixEventMarimba({ startSec: jit(t0, 9), midi, durSec: beats * BEAT,
    gain: vel(gain, 0.12), preset: "bass", decayMul: 0.55 }, bassBuf, { sampleRate: SR });
}
function spark(t0, midi, beats, gain = 0.55) {
  mixEventMarimba({ startSec: jit(t0, 14), midi, durSec: beats * BEAT,
    gain: vel(gain, 0.16), preset: "kalimba", decayMul: 0.95 }, sparkBuf, { sampleRate: SR });
}
function bell(t0, midi, beats, gain = 0.65) {
  mixEventMarimba({ startSec: jit(t0, 10), midi, durSec: beats * BEAT,
    gain: vel(gain, 0.12), preset: "glockenspiel", decayMul: 1.35 }, bellBuf, { sampleRate: SR });
}
// vibraphone pad — long soft sustain, motor off, for held chord beds
function vib(t0, midi, beats, gain = 0.4) {
  mixEventMarimba({ startSec: jit(t0, 18), midi, durSec: beats * BEAT,
    gain: vel(gain, 0.10), preset: "vibraphone_off", decayMul: 1.0 }, vibBuf, { sampleRate: SR });
}

// ── kit routes (absolute 0-based bar + beat; humanized) ──────────────
// Drums keep tighter timing than the mallets but vary their velocity a lot
// (especially the hats — that's where a programmed beat gives itself away).
function KICK(ab, beat, g = 0.95, o = {}) {
  // deeper kick: lower sub fundamental (41 Hz vs 48) + longer body + a touch
  // more drive, so it lands with real chest weight. Per-call `o` still wins.
  mixKick({ startSec: jit(t(ab, beat), 4), gain: vel(g, 0.10),
    fEnd: 41, ampDecay: 0.46, pitchDecay: 0.062, drive: 1.75, ...o }, kickBuf, { sampleRate: SR });
}
function SNR(ab, beat, g = 0.7, o = {}) {
  mixSnare({ startSec: jit(t(ab, beat), 5), gain: vel(g, 0.14), ...o }, snareBuf, { sampleRate: SR });
}
function HAT(ab, beat, g = 0.28, o = {}) {
  mixHat({ startSec: jit(t(ab, beat), 6), gain: vel(g, 0.24), ...o }, hatBuf, { sampleRate: SR });
}
function SHK(ab, beat, g = 0.2) {
  mixShaker({ startSec: jit(t(ab, beat), 7), gain: vel(g, 0.22) }, shakBuf, { sampleRate: SR });
}
// bass-perc — the groove backbone. drive ↑ = grittier.
function BP(ab, beat, midi, durBeats, g = 0.78, o = {}) {
  mixBassPerc({ startSec: jit(t(ab, beat), 5), midi, durSec: durBeats * BEAT, gain: vel(g, 0.12), ...o },
    subBuf, { sampleRate: SR });
}
// ── reverse sine-bell riser — a glassy swell that lands on a downbeat ──
function rise(landBar, dur, midis, g) {
  mixReverseBell({ landSec: t(landBar, 0), dur, midis, gain: g }, riseBuf, { sampleRate: SR });
}
// ── reverse kick — the low-end whoosh under the bell riser ────────────
function riseKick(landBar, dur, g) {
  mixReverseKick({ landSec: t(landBar, 0), dur, gain: g }, kickBuf, { sampleRate: SR });
}
// ── reverse HAT — an open hat played backwards so it SWELLS up into the
// beat and snaps off on the downbeat. Lands at t(landAb, landBeat). ──
function rhat(targetBuf, landAb, landBeat, g = 0.32) {
  const seg = renderHat({ gain: g, open: true }, { sampleRate: SR });
  const startIdx = Math.floor(t(landAb, landBeat) * SR) - seg.length;
  for (let i = 0; i < seg.length; i++) {
    const d = startIdx + i;
    if (d >= 0 && d < targetBuf.length) targetBuf[d] += seg[seg.length - 1 - i];   // reversed
  }
}
// ── Schroeder reverb (4 parallel feedback combs → 2 series allpasses),
// applied in place to a bus. Used to drench the CAVE section. ──
function reverbInPlace(buf, { wet = 0.6, decay = 0.8 } = {}) {
  const combDelays = [0.0297, 0.0371, 0.0411, 0.0437], apDelays = [0.005, 0.0017];
  const out = new Float32Array(buf.length);
  for (const cd of combDelays) {
    const d = Math.max(1, Math.floor(cd * SR)), ring = new Float32Array(d);
    for (let i = 0; i < buf.length; i++) { const y = ring[i % d]; out[i] += y; ring[i % d] = buf[i] + y * decay; }
  }
  for (let i = 0; i < out.length; i++) out[i] /= combDelays.length;
  for (const ad of apDelays) {
    const d = Math.max(1, Math.floor(ad * SR)), ring = new Float32Array(d), g = 0.7;
    for (let i = 0; i < out.length; i++) { const bd = ring[i % d], y = -g * out[i] + bd; ring[i % d] = out[i] + g * y; out[i] = y; }
  }
  for (let i = 0; i < buf.length; i++) buf[i] = buf[i] * (1 - wet) + out[i] * wet;
}

// ── turntable scratches — four flavours ───────────────────────────────
const SCR = {
  baby:      { rate: 5.5, depth: 2.0, tone: 0.5, dur: 0.30 },               // steady forward-back
  scribble:  { rate: 14,  depth: 1.6, tone: 0.6, dur: 0.40 },               // fast tremolo scratch
  transform: { rate: 5,   depth: 2.2, gate: 12, gateDuty: 0.5,  tone: 0.55, dur: 0.42 }, // chopped
  chirp:     { rate: 7,   depth: 3.0, gate: 7,  gateDuty: 0.55, tone: 0.6,  dur: 0.26 }, // pitch-cut
};
function SC(ab, beat, kind, g = 0.6, midi = 50) {
  mixScratch({ startSec: jit(t(ab, beat), 8), midi, gain: vel(g, 0.15), ...SCR[kind] },
    scrBuf, { sampleRate: SR });
}
// ── scratchy kitten scream ────────────────────────────────────────────
function SCREAM(ab, beat, midi, g = 0.5, o = {}) {
  mixScream({ startSec: jit(t(ab, beat), 10), midi, gain: vel(g, 0.16), ...o },
    screamBuf, { sampleRate: SR });
}
// a closed-hat pattern across a bar — straight 8ths, accented offbeats
function hats8(ab, g = 0.26, openLast = false) {
  for (let e = 0; e < 8; e++) {
    const off = e % 2 === 1;
    HAT(ab, e * 0.5, off ? g : g * 0.55, { open: openLast && e === 7 });
  }
}

// ── sections (absolute bar map; body sections offset by INTRO_BARS) ────
const SECTIONS = [
  { name: "intro",     bar0: 0,  bars: INTRO_BARS },
  { name: "butterfly", bar0: 4,  bars: 4 },
  { name: "palofmine", bar0: 8,  bars: 4 },
  { name: "mommywow",  bar0: 12, bars: 4 },
  { name: "slinky",    bar0: 16, bars: 8 },
  { name: "fly",       bar0: 24, bars: 8 },
  { name: "land",        bar0: 32, bars: 4 },
  { name: "cave",        bar0: 36, bars: 4 },
  { name: "squabble",    bar0: 40, bars: 2 },
  { name: "progression", bar0: 42, bars: 5 },   // 42–45 + button at 46
];

// ══════════════════════════════════════════════════════════════════════
//  LEAD line (xylophone) — bar-by-bar from flutterbap.np (body bars 1–32)
// ══════════════════════════════════════════════════════════════════════
function L(bar, b, midi, d, g) { lead(tb(bar, b), midi, d, g ?? 1.0); }

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

// ── BASS (mallet bouncing-ball on 1 & 3) — body bars ──────────────────
function B(bar, b, midi, d, g) { bass(tb(bar, b), midi, d, g ?? 0.85); }
for (let bar = 1; bar <= 8; bar++) { B(bar, 0, N.C3, 1.6); B(bar, 2, N.G3, 1.6); }
for (let bar = 9; bar <= 12; bar++) B(bar, 0, N.C3, 4, 0.55);   // held wonder floor
for (let bar = 13; bar <= 20; bar++) {
  B(bar, 0, N.C3, 1.6); B(bar, 2, N.G3, 1.4);
  if (bar % 2 === 0) B(bar, 3.5, N.C4, 0.4, 0.6);              // skip grace
}
B(21, 0, N.C3, 1.6); B(21, 2, N.G3, 1.6);
B(23, 0, N.C3, 1.6); B(23, 2, N.G3, 1.6);
B(25, 0, N.C3, 1.6); B(25, 2, N.G3, 1.6);
B(27, 0, N.C3, 1.6); B(27, 2, N.G3, 1.6);
B(29, 0, N.C3, 1.6); B(29, 2, N.G3, 1.6);
B(30, 0, N.C3, 3); B(31, 0, N.C3, 2);
B(32, 0, N.C2, 4, 0.75);

// ── KALIMBA twinkles — body bars ──────────────────────────────────────
function K(bar, b, midi, d, g) { spark(tb(bar, b), midi, d, g ?? 0.55); }
K(5, 3.5, N.C6, 0.5); K(6, 3.5, N.A5, 0.5); K(7, 3.5, N.G5, 0.5);
K(8, 1, N.E6, 0.5);  K(8, 2, N.G6, 0.5);  K(8, 3, N.E6, 0.5);
for (const bar of [13, 14, 15, 18, 19, 20]) { K(bar, 0.5, N.E6, 0.5, 0.45); K(bar, 2.5, N.C6, 0.5, 0.45); }
K(22, 0, N.E6, 0.5);  K(22, 1, N.G6, 0.5);  K(22, 2, N.E6, 0.5);  K(22, 3, N.C6, 0.5);
K(24, 0, N.F6, 0.5);  K(24, 1, N.A5, 0.5);  K(24, 2, N.D6, 0.5);  K(24, 3, N.F6, 0.5);
K(26, 3, N.E6, 0.5);
K(28, 2.5, N.C6, 0.5); K(28, 3.5, N.E6, 0.5);
K(30, 3.5, N.E5, 0.5); K(31, 3.5, N.G5, 0.5);
K(32, 1.5, N.C6, 0.5, 0.4);

// ── GLOCKENSPIEL halo — body bars ─────────────────────────────────────
function G(bar, b, midi, d, g) { bell(tb(bar, b), midi, d, g ?? 0.65); }
G(9, 0, N.E6, 4, 0.4);  G(10, 2, N.G6, 2, 0.85);
G(11, 0, N.E6, 4, 0.4); G(12, 0, N.G6, 1, 0.85);  G(12, 3, N.C6, 1, 0.75);
G(22, 0, N.G6, 4, 0.6); G(24, 0, N.G6, 4, 0.6);
G(26, 2, N.G6, 2, 0.6); G(28, 0, N.G5, 2, 0.45);
G(32, 0, N.C6, 4, 0.5); G(32, 1, N.E6, 3, 0.4);

// ── VIBRAPHONE counter-pad — sustained chord beds under the lead ──────
// Soft, low in the mix; gives the bright mallets a harmonic floor that the
// hook never had. Triads follow the implied harmony of each section.
function V(bar, b, midis, d, g = 0.34) {
  for (const m of midis) vib(tb(bar, b), m, d, g);
  vib(tb(bar, b), midis[0] - 12, d, g * 0.72);   // octave-down root — fuller range + warmth under the lead
}
V(5, 0, [N.C4, N.E4, N.G4], 4);                    // C  under pal-of-mine
V(6, 0, [N.F3, N.A3, N.C4], 2);                    // F
V(7, 0, [N.G3, N.B3, N.D4], 2);                    // G
V(8, 0, [N.C4, N.E4, N.G4], 4);                    // C
V(9, 0, [N.C4, N.E4, N.G4], 8, 0.30);              // long bed across mommy-wow
V(21, 0, [N.C4, N.E4, N.G4], 4);                   // C, the fly soar
V(23, 0, [N.F3, N.A3, N.C4], 4);                   // F lift on "I fly"
V(25, 0, [N.C4, N.E4, N.G4], 2); V(26, 0, [N.A3, N.C4, N.E4], 2);
V(27, 0, [N.G3, N.B3, N.D4], 2); V(28, 0, [N.C4, N.E4, N.G4], 2);
V(29, 0, [N.C4, N.E4, N.G4], 4, 0.30);             // settle under the landing

// ══════════════════════════════════════════════════════════════════════
//  RHYTHM SECTION — bass-perc + kit, with per-section dynamics
// ══════════════════════════════════════════════════════════════════════
// Bass-perc lives low (C2/G1) and bounces; the kit fills in around it. The
// groove builds in the intro, eases for the hush, drives through the body,
// and resolves at the landing.

// ── INTRO (abs 0–3): start with PRESENCE, then build a clear arrival ──
// (was a slow fade-in from near-silence — felt hidden. Now the groove is
// already moving at bar 0 and crescendos into the hook at abs bar 4 ≈ 7.7s,
// which is where the song "really starts".)
// bar 0 — kick + bass-perc + shaker + a soft bell beacon set the key, moving
KICK(0, 0, 0.82); KICK(0, 2, 0.64);
BP(0, 0, N.C2, 1.7, 0.8, { drive: 1.9 }); BP(0, 2.5, N.G1, 1.0, 0.68);
for (let e = 0; e < 4; e++) SHK(0, e + 0.5, 0.2);
hats8(0, 0.16);
bell(t(0, 0), N.C6, 3, 0.34); bell(t(0, 2), N.G5, 2, 0.3);          // soft tonal beacon
// bar 1 — fuller kick + hats, bass bounces
KICK(1, 0, 0.9); KICK(1, 2, 0.76);
BP(1, 0, N.C2, 1.6, 0.8); BP(1, 2.5, N.G1, 1.0, 0.72);
hats8(1, 0.22);
spark(t(1, 3.5), N.E6, 0.5, 0.4);                                   // kalimba twinkle
// bar 2 — snare backbeat enters, bass-perc bounces
KICK(2, 0, 0.94); KICK(2, 2, 0.82);
SNR(2, 2, 0.62);
BP(2, 0, N.C2, 1.4, 0.82); BP(2, 1.5, N.C2, 0.5, 0.64); BP(2, 2.5, N.G1, 1.2, 0.74);
hats8(2, 0.26);
spark(t(2, 3.5), N.G5, 0.5, 0.45);                                  // kalimba pickup
// bar 3 — full preview + a crescendo fill spilling into the drop
KICK(3, 0, 0.96); KICK(3, 2, 0.84);
SNR(3, 1, 0.56); SNR(3, 3, 0.6);
BP(3, 0, N.C2, 1.4, 0.82); BP(3, 2.5, N.G1, 0.8, 0.74);
hats8(3, 0.28);
for (const fb of [3, 3.25, 3.5, 3.75]) SNR(3, fb, 0.4 + (fb - 3) * 0.6);   // crescendo fill
spark(t(3, 3.5), N.G5, 0.5, 0.48); spark(t(3, 3.75), N.C6, 0.5, 0.55);     // kalimba run-up

// ── helper: a standard groove bar (abs bar) at an intensity level ──
// level 0 = light (kick 1&3, soft hats), 1 = full (snare 2&4, busy hats),
// 2 = driving (adds kick &-of-3 + open hat).
function groove(ab, level, kg = 0.9) {
  KICK(ab, 0, kg); KICK(ab, 2, kg * 0.82);
  if (level >= 2) KICK(ab, 3.5, kg * 0.6);
  if (level >= 1) { SNR(ab, 1, 0.62); SNR(ab, 3, 0.66); }
  hats8(ab, level >= 1 ? 0.26 : 0.16, level >= 2);
  if (level >= 1) for (let e = 0; e < 4; e++) SHK(ab, e + 0.5, 0.14);
}

// ── BUTTERFLY (abs 4–7): light, bouncy groove under the hook ──
for (let ab = 4; ab <= 7; ab++) {
  groove(ab, 0);
  BP(ab, 0, N.C2, 1.5, 0.76); BP(ab, 2, N.G1, 1.0, 0.7); BP(ab, 3, N.C2, 0.8, 0.62);
}

// ── PALOFMINE (abs 8–11): full backbeat groove ──
const PAL_ROOT = [N.C2, N.F1, N.G1, N.C2];  // C F G C
for (let i = 0; i < 4; i++) {
  const ab = 8 + i, r = PAL_ROOT[i];
  groove(ab, 1);
  BP(ab, 0, r, 1.4, 0.78); BP(ab, 1.5, r, 0.5, 0.6); BP(ab, 2.5, r === N.C2 ? N.G1 : r, 1.2, 0.72);
}

// ── MOMMYWOW (abs 12–15): HUSH — pull the kit back for the held wonder ──
for (let ab = 12; ab <= 15; ab++) {
  KICK(ab, 0, 0.6);                      // soft heartbeat
  BP(ab, 0, N.C2, 3.6, 0.6, { drive: 1.6, decay: 0.9 });  // long sustained sub
  HAT(ab, 2, 0.18, { open: true });      // a single open-hat swell mid-bar
  for (let e = 0; e < 4; e++) SHK(ab, e + 0.5, 0.1);
}
SNR(15, 3, 0.4); SNR(15, 3.5, 0.5); SNR(15, 3.75, 0.6);  // fill back into slinky

// ── SLINKY (abs 16–23): the bass-perc really drives — springy bounce ──
for (let i = 0; i < 8; i++) {
  const ab = 16 + i;
  groove(ab, i >= 4 ? 2 : 1);
  // a wobbling, syncopated bass-perc line — the "slinky" spring in the low end
  BP(ab, 0, N.C2, 0.9, 0.8); BP(ab, 1.5, N.C2, 0.5, 0.62);
  BP(ab, 2, N.G1, 0.9, 0.74); BP(ab, 3.5, N.C2, 0.5, 0.6);
}

// ── FLY (abs 24–31): biggest energy — driving four-on-floor-ish ──
const FLY_ROOT = [N.C2, N.C2, N.F1, N.F1, N.C2, N.A1, N.G1, N.C2];
for (let i = 0; i < 8; i++) {
  const ab = 24 + i, r = FLY_ROOT[i];
  groove(ab, 2, 0.96);
  KICK(ab, 1, 0.55);                      // four-on-the-floor lift
  BP(ab, 0, r, 1.4, 0.82); BP(ab, 2, r, 0.9, 0.74); BP(ab, 3, r, 0.8, 0.66);
  if (i === 7) for (const fb of [3, 3.25, 3.5, 3.75]) SNR(ab, fb, 0.35 + (fb - 3) * 0.55);  // crescendo fill into land
}

// ── LAND (abs 32–35): wind down, resolve to C ──
for (let i = 0; i < 4; i++) {
  const ab = 32 + i;
  KICK(ab, 0, 0.85 - i * 0.12);
  if (i < 2) { SNR(ab, 1, 0.5); SNR(ab, 3, 0.5); hats8(ab, 0.2); }
  BP(ab, 0, N.C2, 2.2 - i * 0.4, 0.74 - i * 0.1);
}
BP(35, 0, N.C2, 3.0, 0.7, { decay: 1.1 });   // final low resolve

// ══════════════════════════════════════════════════════════════════════
//  EXTENDED CODA — cave → squabble → chord progression → resolve (abs 36–46)
//  After the landing the track drifts into a reverby cave, erupts in a
//  squeak-scratch squabble, then a chord progression walks it home to a
//  final resolved button.
// ══════════════════════════════════════════════════════════════════════

// ── CAVE (abs 36–39): sparse + drenched. Everything routes into caveBuf,
// which gets a big Schroeder reverb at mixdown. Reverse swells (bell + hat
// + kick) breathe in and out of the dark. ──
{
  const caveVib = (tt, m, beats, g) => mixEventMarimba({ startSec: tt, midi: m, durSec: beats * BEAT, gain: g, preset: "vibraphone_off", decayMul: 1.5 }, caveBuf, { sampleRate: SR });
  const caveLead = (tt, m, beats, g) => mixEventMarimba({ startSec: tt, midi: m, durSec: beats * BEAT, gain: g, preset: "xylophone", decayMul: 1.4 }, caveBuf, { sampleRate: SR });
  for (const m of [N.A3, N.C4, N.E4, N.A4]) caveVib(t(36, 0), m, 7, 0.30);   // Am pad floor
  for (const m of [N.F3, N.A3, N.C4]) caveVib(t(38, 0), m, 7, 0.28);          // F
  caveLead(t(36, 1), N.E5, 1, 0.50); caveLead(t(36, 2.5), N.A5, 1.5, 0.46);   // lost butterfly motif
  caveLead(t(38, 1), N.C6, 1, 0.46); caveLead(t(38, 2.5), N.G5, 1.5, 0.42);
  mixReverseBell({ landSec: t(36, 0), dur: 1.4 * BAR, midis: [N.A5, N.C6, N.E6], gain: 0.40 }, caveBuf, { sampleRate: SR });
  mixReverseBell({ landSec: t(38, 2), dur: 1.0 * BAR, midis: [N.E6, N.G6], gain: 0.32 }, caveBuf, { sampleRate: SR });
  mixKick({ startSec: t(36, 0), gain: 0.45, fEnd: 38, ampDecay: 0.6 }, caveBuf, { sampleRate: SR });   // distant heartbeat
  mixKick({ startSec: t(37, 2), gain: 0.45, fEnd: 38, ampDecay: 0.6 }, caveBuf, { sampleRate: SR });
  mixKick({ startSec: t(39, 0), gain: 0.5, fEnd: 38, ampDecay: 0.7 }, caveBuf, { sampleRate: SR });
  mixReverseKick({ landSec: t(38, 0), dur: 1.0 * BAR, gain: 0.4 }, caveBuf, { sampleRate: SR });
  rhat(caveBuf, 37, 0, 0.32); rhat(caveBuf, 39, 0, 0.34);                      // reverse hats swelling in
  mixHat({ startSec: t(36, 2), gain: 0.18, open: true }, caveBuf, { sampleRate: SR });
  mixHat({ startSec: t(38, 2), gain: 0.18, open: true }, caveBuf, { sampleRate: SR });
}

// ── SQUABBLE (abs 40–41): SQUEAK SCRATCH OUT SQUABBLE — the feral break.
// dense kitten-screams + turntable scratches trading blows, DRY + up-front
// so the squeaks bite, over a skeletal kick/hat groove. ──
SCREAM(40, 0,   90, 0.55, { bend: 9,  rasp: 0.75, dur: 0.50 });  SC(40, 0.5, "scribble",  0.60, 52);
SCREAM(40, 1,   85, 0.50, { bend: 11, rasp: 0.70, dur: 0.40 });  SC(40, 1.5, "transform", 0.62, 49);
SCREAM(40, 2,   93, 0.56, { bend: 7,  rasp: 0.78, dur: 0.45 });  SC(40, 2.5, "chirp",     0.60, 55);
SCREAM(40, 3,   88, 0.50, { bend: 10, rasp: 0.72, dur: 0.42 });  SC(40, 3.5, "scribble",  0.58, 50);
SCREAM(41, 0,   95, 0.58, { bend: 8,  rasp: 0.80, dur: 0.50 });  SC(41, 0.5, "transform", 0.64, 48);
SC(41, 1, "chirp", 0.60, 53);  SCREAM(41, 1.5, 84, 0.50, { bend: 12, rasp: 0.70, dur: 0.40 });
SC(41, 2, "baby", 0.56, 50);   SC(41, 2.5, "scribble", 0.62, 56);
SCREAM(41, 3,   91, 0.56, { bend: 9, rasp: 0.76, dur: 0.45 });
for (const fb of [3, 3.25, 3.5, 3.75]) SNR(41, fb, 0.4 + (fb - 3) * 0.6);     // snare fill into the progression
KICK(40, 0, 0.8); KICK(40, 2, 0.7); KICK(41, 0, 0.82); KICK(41, 2, 0.72);
for (let e = 0; e < 8; e++) { HAT(40, e * 0.5, 0.2, { open: e === 7 }); HAT(41, e * 0.5, 0.22, { open: e === 7 }); }

// ── PROGRESSION (abs 42–45): a real chord progression walks it home.
// C → G → Am → F (I–V–vi–IV); the button then resolves to C. Vibraphone
// triads + bass-perc roots + a butterfly-motif reprise riding the changes. ──
const PROG = [
  { root: N.C2, triad: [N.C4, N.E4, N.G4] },   // C
  { root: N.G1, triad: [N.G3, N.B3, N.D4] },   // G
  { root: N.A1, triad: [N.A3, N.C4, N.E4] },   // Am
  { root: N.F1, triad: [N.F3, N.A3, N.C4] },   // F
];
const PROG_TOP = [N.C6, N.B5, N.A5, N.A5];
for (let i = 0; i < 4; i++) {
  const ab = 42 + i, ch = PROG[i];
  groove(ab, 1, 0.86);
  BP(ab, 0, ch.root, 1.5, 0.76); BP(ab, 2, ch.root, 1.0, 0.70); BP(ab, 3, ch.root, 0.8, 0.60);
  for (const m of ch.triad) vib(t(ab, 0), m, 4, 0.34);
  vib(t(ab, 0), ch.triad[0] - 12, 4, 0.24);    // octave-down warmth
  lead(t(ab, 0), PROG_TOP[i], 1, 0.90);         // motif riding the changes
  lead(t(ab, 1), ch.triad[2], 1, 0.80);
  lead(t(ab, 2), ch.triad[1] + 12, 2, 0.82);
  spark(t(ab, 3.5), N.E6, 0.5, 0.40);
  bell(t(ab, 0), ch.triad[2] + 12, 2, 0.40);
}
rhat(hatBuf, 46, 0, 0.42);                      // reverse-hat swell into the button

// ── BUTTON (abs 46): final resolved C hit + ring ──
KICK(46, 0, 1.0); BP(46, 0, N.C2, 3.2, 0.86, { decay: 1.4 });
SNR(46, 0, 0.5);
bell(t(46, 0), N.C6, 4, 0.5); bell(t(46, 0.06), N.E6, 4, 0.4); bell(t(46, 0.12), N.G6, 4, 0.35);
vib(t(46, 0), N.C4, 4, 0.4); vib(t(46, 0), N.E4, 4, 0.35); vib(t(46, 0), N.G4, 4, 0.3);
lead(t(46, 0), N.C6, 4, 0.85);

// ── REVERSE SINE-BELL RISERS — at the breaks ──────────────────────────
// The signature transition: a glassy sine swell rising out of the texture
// and landing a bright bell on the downbeat of the next section.
//   • the big one: through the mommy-wow hush → DROP into slinky (abs 16).
//     2 bars of swell starting ≈0:27, landing on the slinky downbeat.
//   • a softer lift into the fly soar (abs 24).
//   • a wash into the outro hook reprise (abs 36).
rise(4,  1.5 * BAR, [N.G5, N.C6, N.E6, N.G6], 0.44); // intro → THE START: lands the hook at ~7.7s
rise(16, 2 * BAR, [N.E5, N.G5, N.C6, N.E6], 0.52);   // mommy-wow → slinky (the 0:27 break)
rise(24, 1.5 * BAR, [N.G5, N.C6, N.E6, N.G6], 0.40); // → fly soar
// reverse kicks — the low-end whoosh under the risers
riseKick(4,  1.0 * BAR, 0.55);                       // arrival whoosh into the hook
riseKick(16, 1.4 * BAR, 0.7);                        // big slinky drop
riseKick(24, 1.0 * BAR, 0.5);                        // fly soar
// a bright bell chime right on the hook downbeat — the song clearly "starts"
bell(t(4, 0), N.C6, 3, 0.55); bell(t(4, 0.05), N.E6, 3, 0.42); bell(t(4, 0.10), N.G6, 3, 0.34);

// ── SUPER SCRATCHES — DJ flavour through the wild sections ────────────
SC(3, 2, "scribble", 0.5, 52); SC(3, 3, "chirp", 0.55, 50);   // intro build into the drop
SC(16, 0, "transform", 0.72, 50);                              // land on the slinky drop
for (let ab = 16; ab <= 23; ab++) {                            // slinky: rhythmic scratch stabs
  SC(ab, 3, ab % 2 ? "baby" : "scribble", 0.5, 48 + (ab % 3) * 4);
  if (ab % 2 === 0) SC(ab, 1.5, "chirp", 0.44, 55);
}
SC(24, 0, "chirp", 0.6, 52);                                   // fly entrance
for (let ab = 24; ab <= 31; ab++)                              // fly: transform/chirp accents
  SC(ab, 2.5, ab % 2 ? "transform" : "chirp", 0.5, 47 + (ab % 4) * 3);
SC(31, 3, "scribble", 0.62, 50);                               // fill into the landing

// ── SCRATCHY KITTEN SCREAMS — feral little shrieks at the wild bits ───
SCREAM(16, 0, 88, 0.5, { bend: 8, rasp: 0.7, dur: 0.55 });     // ride the slinky drop
SCREAM(19, 3.5, 91, 0.42, { bend: 6, rasp: 0.65, dur: 0.4 });
SCREAM(22, 2, 84, 0.4, { bend: 9, rasp: 0.6, dur: 0.45 });
SCREAM(24, 0, 90, 0.5, { bend: 7, rasp: 0.72, dur: 0.5 });     // answer the fly soar
SCREAM(27, 3.5, 86, 0.44, { bend: 10, rasp: 0.7, dur: 0.42 });
SCREAM(30, 2.5, 93, 0.46, { bend: 6, rasp: 0.68, dur: 0.38 }); // high screech near the peak
SCREAM(39, 0, 88, 0.5, { bend: 8, rasp: 0.75, dur: 0.6 });     // last cry on the button

// ═══════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN
// ═══════════════════════════════════════════════════════════════════
const outL = new Float32Array(ns);
const outR = new Float32Array(ns);

function pan(p) { const a = (p + 1) * Math.PI / 4; return [Math.cos(a), Math.sin(a)]; }
function place(buf, p, gain) {
  const [lg, rg] = pan(p);
  for (let i = 0; i < ns; i++) { const s = buf[i] * gain; outL[i] += s * lg; outR[i] += s * rg; }
}
// Wider, clearer image: lead + low end stay centred; the harmonic/sparkle
// voices (vib, glock, kalimba) are pushed out AND lifted so they read as their
// own parts instead of hiding under the lead.
place(leadBuf,   0.00, 1.02);   // the singer, dead centre + a touch more present
place(bassBuf,   0.00, 0.72);   // mallet bass eases back — the bass-perc owns the low
place(vibBuf,   -0.24, 0.86);   // pad: wider + much more audible (was buried)
place(riseBuf,   0.00, 0.85);   // reverse-bell risers, centred + wide
place(scrBuf,    0.34, 0.80);   // scratches, off to the DJ hand
place(screamBuf,-0.50, 0.62);   // kitten screams, wide-left (the squeaks!) — kept proud
place(sparkBuf,  0.44, 0.88);   // kalimba twinkles, hard-ish right + brighter
place(bellBuf,  -0.42, 0.86);   // glockenspiel halo, left + brighter
place(kickBuf,   0.00, 1.02);   // deeper kick, centred
place(subBuf,    0.00, 0.98);   // bass-perc — the groove backbone, centred
place(snareBuf,  0.04, 0.80);
place(hatBuf,    0.34, 0.58);
place(shakBuf,  -0.34, 0.48);
// CAVE bus — drench in Schroeder reverb, then place centred + present
reverbInPlace(caveBuf, { wet: 0.62, decay: 0.82 });
place(caveBuf,   0.00, 0.92);

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
if (peak > 0) { const nrm = 0.84 / peak; for (let i = 0; i < ns; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

// trim trailing silence + tiny fades
let lastLoud = ns - 1;
while (lastLoud > 0 && Math.abs(outL[lastLoud]) < 0.004 && Math.abs(outR[lastLoud]) < 0.004) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.6 * SR));
const fadeIn = Math.floor(0.004 * SR);
const fadeOut = Math.floor(1.4 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) { const g = i / fadeIn; outL[i] *= g; outR[i] *= g; }
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut; outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ flutterbap · ${TOTAL_BARS} bars · C major 4/4 @ ${BPM} BPM · ${(trimN / SR).toFixed(1)} s · bass-perc kit · risers · scratches · kitten screams · swing=${SWING.toFixed(2)} · humanize=${HUMANIZE ? "on" : "off"}`);

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
for (let i = 0; i < trimN; i++) { b.writeFloatLE(outL[i], i * 8); b.writeFloatLE(outR[i], i * 8 + 4); }
writeFileSync(rawPath, b);

// pop master chain — now with real low end (kick + bass-perc): tighten and
// weight the sub, de-mud, keep the mallet presence + daytime air, glue +
// brickwall.
const MASTER = [
  "highpass=f=26",
  "equalizer=f=48:t=q:w=0.9:g=2.7",        // deep sub weight under the deeper kick + bass-perc
  "equalizer=f=90:t=q:w=1.0:g=1.2",        // kick/bass body punch
  "equalizer=f=240:t=q:w=1.1:g=-1.3",      // de-mud
  "equalizer=f=3000:t=q:w=1.5:g=0.9",      // mallet presence
  "treble=g=1.4:f=8000",                   // daytime air
  "acompressor=threshold=-18dB:ratio=2.8:attack=14:release=170:makeup=2.4:knee=6",
  "alimiter=limit=0.95:attack=4:release=70",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
if (ff.status !== 0) { try { unlinkSync(rawPath); } catch {} console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (pop-mastered · ${(trimN / SR).toFixed(1)} s)`);

// ── DistroKid WAV master — same chain → loudnorm to -14 LUFS / -1.5 TP,
// 44.1 kHz / 16-bit PCM. Gentle target keeps the squeaks + scratches alive.
const wavOut = outPath.replace(/\.mp3$/i, "") + ".distrokid.wav";
const ffw = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", `${MASTER},loudnorm=I=-14:TP=-1.5:LRA=11`,
  "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", wavOut], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ffw.status === 0) console.log(`✓ ${wavOut} (DistroKid master · 44.1 kHz / 16-bit · -14 LUFS)`);
else console.error("✗ DistroKid WAV master failed");

// ── struct.json — section map for any future visualizer ───────────────
{
  const structTotalSec = trimN / SR;
  const struct = {
    _comment: "Section map for flutterbap (the daytime butterfly hop, +bass-perc kit). 4/4, 124 BPM → bar ≈ 1.935 s. C major. Derived from render-flutterbap.mjs SECTIONS (absolute bar map; body offset by INTRO_BARS).",
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
