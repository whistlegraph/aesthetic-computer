#!/usr/bin/env node
// trance.mjs — render a trance / trancewaltz bed for pop/dance/.
//
// Bottom-up + native-portable: every voice resolves to AC's
// sound.synth(...) contract (the same one percussion.mjs uses, the same
// one fedac/native audio.c implements). Drums come from percussion.mjs.
// Supersaw lead/pad come from pop/dance/synths/supersaw.mjs (now N
// parallel sound.synth({type:"sawtooth"}) calls — runs everywhere).
// Sub-bass is one triangle voice — natively dark, no filter needed.
//
// Two meters supported:
//   --meter 4   (default trance, 4/4, kick-on-every-beat ×4)
//   --meter 3   (trancewaltz, 3/4, kick-on-every-beat ×3) — waltz lilt
//               retained, trance engine + arc on top.
//
// Per-section effects: each SECTION_TEMPLATES entry can carry `wobble`
// and `bitcrush` configs (with envelopes) applied to that section only.
//
// Vocal: pass `--vocal-stem path.mp3` to mix a pre-rendered vocal stem
// during the drops. (Generation lives in pop/dance/bin/vocal.mjs.)
//
// Usage:
//   node recap/bin/trance.mjs                                # 4/4 radio edit
//   node recap/bin/trance.mjs --meter 3 --bpm 108            # trancewaltz
//   node recap/bin/trance.mjs --section drop --bars 16       # 16-bar drop
//   node recap/bin/trance.mjs --vocal-stem ~/Desktop/vox.mp3 --out ~/Desktop/trance.mp3

import {
  readFileSync,
  writeFileSync,
  mkdirSync,
  unlinkSync,
  existsSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { playPercussion } from "../../system/public/aesthetic.computer/lib/percussion.mjs";
import { mixEventSinePower as mixEventLeadPad } from "../../pop/dance/synths/sinepower.mjs";
import { mixEventSupersaw } from "../../pop/dance/synths/supersaw.mjs";
import { makeBufferSynth } from "../../pop/dance/synths/bus.mjs";
import { applyWobble, applyBitcrush, applyFlange, softClip } from "../../pop/dance/synths/fx.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

// ── piano sample bank (native ships fedac/native/samples/piano) ───────
// Each anchor is a 48kHz mono Float32 raw at the given MIDI pitch.
const PIANO_DIR = `${REPO}/fedac/native/samples/piano`;
const PIANO_ANCHORS = [21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96];
let PIANO_BANK = null;
function loadPianoBank() {
  if (PIANO_BANK) return PIANO_BANK;
  PIANO_BANK = new Map();
  for (const m of PIANO_ANCHORS) {
    const path = `${PIANO_DIR}/${m}.raw`;
    if (!existsSync(path)) throw new Error(`missing piano anchor ${path}`);
    const buf = readFileSync(path);
    const f32 = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
    PIANO_BANK.set(m, Float32Array.from(f32));
  }
  return PIANO_BANK;
}
function pianoAnchorFor(midi) {
  let best = PIANO_ANCHORS[0];
  for (const a of PIANO_ANCHORS) if (Math.abs(a - midi) < Math.abs(best - midi)) best = a;
  const ratio = Math.pow(2, (midi - best) / 12);
  return { sample: PIANO_BANK.get(best), ratio };
}
// ── zoo sample bank (lion roar etc — for the drop BOOM) ─────────────
// Native ships fedac/native/samples/zoo with f32 mono 48k samples.
const ZOO_DIR = `${REPO}/fedac/native/samples/zoo`;
const ZOO_BANK = new Map();
function loadZooSample(name) {
  if (ZOO_BANK.has(name)) return ZOO_BANK.get(name);
  const path = `${ZOO_DIR}/${name}.raw`;
  if (!existsSync(path)) return null;
  const buf = readFileSync(path);
  const f32 = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
  const owned = Float32Array.from(f32);
  ZOO_BANK.set(name, owned);
  return owned;
}
function mixZooSample(ev, out, sampleRate) {
  const sample = loadZooSample(ev.name);
  if (!sample) return;
  const pitchRatio = ev.pitchRatio ?? 1.0;
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const lenSamples = Math.floor(sample.length / pitchRatio);
  const attackS = Math.min(0.020 * sampleRate, lenSamples * 0.05);
  const releaseS = Math.min(0.180 * sampleRate, lenSamples * 0.30);
  for (let i = 0; i < lenSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    const srcF = i * pitchRatio;
    const s0 = Math.floor(srcF);
    const s1 = s0 + 1;
    if (s1 >= sample.length) break;
    const frac = srcF - s0;
    const v = sample[s0] * (1 - frac) + sample[s1] * frac;
    let env = 1;
    if (i < attackS) env = i / attackS;
    else if (i > lenSamples - releaseS) env = Math.max(0, (lenSamples - i) / releaseS);
    out[dst] += v * env * (ev.gain ?? 1);
  }
}

// Cat DRONE — granular time-stretch of the meow so it sustains over
// many bars while its pitch is set independently (overlap-add Hann
// grains; source crawls across the whole sample over durSec, each
// grain repitched by pitchRatio). Layer several at consonant ratios
// for an autotuned, harmonized "cat choir" pad.
function mixCatDrone(out, startSec, durSec, pitchRatio, gain) {
  const sample = loadZooSample("cat");
  if (!sample) return;
  const SR = SAMPLE_RATE;
  const grain = Math.floor(0.09 * SR);
  const hop = Math.floor(grain * 0.5);
  const outStart = Math.floor(startSec * SR);
  const outLen = Math.floor(durSec * SR);
  if (outLen < grain) return;
  // BUGFIX: crawling the whole sample over durSec was a ~48× stretch
  // that froze on one instant (no audible elongation/pitch). Instead
  // loop the meow at a moderate slow pass (~4.5 s) so it's a
  // recognizably *slowed, evolving* meow, with a pitch GLIDE down
  // (deepen + darken across the drone) and a slow volume ROLL.
  const passSec = 4.5;
  const srcPerOut = sample.length / (passSec * SR);
  for (let gpos = 0; gpos + grain < outLen; gpos += hop) {
    const prog = gpos / outLen;                         // 0..1 over the drone
    const pr = pitchRatio * (1 - 0.45 * prog);          // glide down — darkens
    const roll = 0.55 + 0.45 * Math.sin(prog * Math.PI * 5.0 + 0.7); // vol roll
    const srcCenter = (gpos * srcPerOut) % sample.length;
    for (let k = 0; k < grain; k++) {
      const w = 0.5 - 0.5 * Math.cos((2 * Math.PI * k) / grain); // Hann
      let si = srcCenter + (k - grain / 2) * pr;
      si = ((si % sample.length) + sample.length) % sample.length;
      const i0 = Math.floor(si);
      const i1 = (i0 + 1) % sample.length;
      const fr = si - i0;
      const v = sample[i0] * (1 - fr) + sample[i1] * fr;
      const d = outStart + gpos + k;
      if (d >= 0 && d < out.length) out[d] += v * w * gain * roll;
    }
  }
}

function mixEventPiano(ev, out, sampleRate) {
  const { sample, ratio } = pianoAnchorFor(ev.midi);
  if (!sample) return;
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const durSamples = Math.floor(ev.durSec * sampleRate);
  const attack = Math.min(0.005 * sampleRate, durSamples * 0.05);
  const release = Math.min(0.10 * sampleRate, durSamples * 0.5);
  const lenOut = durSamples + Math.floor(release);
  for (let i = 0; i < lenOut; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    const srcF = i * ratio;
    const s0 = Math.floor(srcF);
    const s1 = s0 + 1;
    if (s1 >= sample.length) break;
    const frac = srcF - s0;
    const v = sample[s0] * (1 - frac) + sample[s1] * frac;
    let env = 1;
    if (i < attack) env = i / attack;
    else if (i > durSamples) env = Math.max(0, 1 - (i - durSamples) / release);
    out[dst] += v * env * ev.gain;
  }
}

// ── sinebells (pure additive synth — native-portable via sound.synth) ─
const BELL_PARTIALS = [
  { ratio: 0.5,  amp: 0.28, decayT60: 5.5 },
  { ratio: 1.0,  amp: 1.00, decayT60: 4.5 },
  { ratio: 2.0,  amp: 0.32, decayT60: 2.6 },
  { ratio: 2.4,  amp: 0.10, decayT60: 1.2 },
  { ratio: 3.0,  amp: 0.09, decayT60: 1.0 },
  { ratio: 4.5,  amp: 0.04, decayT60: 0.6 },
];
function mixEventSinebell(ev, out, sampleRate) {
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const tailSec = ev.tailSec ?? 4.0;
  const ringSamples = Math.floor((ev.durSec + tailSec) * sampleRate);
  // Attack — short by default (struck-bell), but the tail-pad mode
  // takes a much longer attack (slow swell, opening-pad character).
  const attackSec = ev.attackSec ?? 0.012;
  const attackS = attackSec * sampleRate;
  // Decay scale — multiply all partials' T60s by this. 1.0 = standard
  // struck bell. Higher = longer, more pad-like sustain. The tail-pad
  // mode passes ~3.0 so the bells turn into a slow opening drone.
  const decayScale = ev.decayScale ?? 1.0;
  const fundFreq = 440 * Math.pow(2, (ev.midi - 69) / 12);
  const twoPiOverSr = (2 * Math.PI) / sampleRate;
  const partials = BELL_PARTIALS.map((p) => ({
    omega: twoPiOverSr * fundFreq * p.ratio,
    amp: p.amp,
    decay: Math.exp(-Math.log(1000) / (p.decayT60 * decayScale * sampleRate)),
  }));
  for (let i = 0; i < ringSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    let s = 0;
    for (const p of partials) {
      const env = p.amp * Math.pow(p.decay, i);
      if (env < 1e-5) continue;
      s += Math.sin(p.omega * i) * env;
    }
    let att = 1;
    if (i < attackS) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackS);
    out[dst] += s * att * ev.gain * 0.42;
  }
}

// ── harp (low-octave plucked voice — pure harmonic partials) ─────────
// A close cousin of the sinebell — same additive-sine engine — but with
// strictly harmonic partial ratios (no inharmonicity) and a softer
// pluck envelope. Sounds warm and resonant at low pitches, perfect
// for the opening before any drums enter. Native-portable via the
// same sound.synth contract.
const HARP_PARTIALS = [
  { ratio: 1.0,  amp: 1.00, decayT60: 6.0 },
  { ratio: 2.0,  amp: 0.55, decayT60: 4.5 },
  { ratio: 3.0,  amp: 0.30, decayT60: 3.0 },
  { ratio: 4.0,  amp: 0.18, decayT60: 2.0 },
  { ratio: 5.0,  amp: 0.10, decayT60: 1.3 },
  { ratio: 6.0,  amp: 0.06, decayT60: 0.9 },
];
function mixEventHarp(ev, out, sampleRate) {
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const tailSec = 5.0;
  const ringSamples = Math.floor((ev.durSec + tailSec) * sampleRate);
  // Slow soft pluck (~30 ms) — not a sharp bell attack.
  const attackS = 0.030 * sampleRate;
  const fundFreq = 440 * Math.pow(2, (ev.midi - 69) / 12);
  const twoPiOverSr = (2 * Math.PI) / sampleRate;
  const partials = HARP_PARTIALS.map((p) => ({
    omega: twoPiOverSr * fundFreq * p.ratio,
    amp: p.amp,
    decay: Math.exp(-Math.log(1000) / (p.decayT60 * sampleRate)),
  }));
  for (let i = 0; i < ringSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    let s = 0;
    for (const p of partials) {
      const env = p.amp * Math.pow(p.decay, i);
      if (env < 1e-5) continue;
      s += Math.sin(p.omega * i) * env;
    }
    let att = 1;
    if (i < attackS) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackS);
    out[dst] += s * att * ev.gain * 0.55;
  }
}

// ── parse args ─────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const METER       = Number(flags.meter ?? 4);       // 4=trance, 3=trancewaltz
const isWaltz     = METER === 3;
// MODE — "normal" (default — full narrative track with drops, screams,
// impacts, lion roar, machine guns, vocal stem, tape-stop) or
// "chill" (study-vibes minimal dance mix — continuous bed, no drops,
// no roar, no machine guns, no screams, no vocal stem, no tape-stop,
// flatter dynamics, longer runtime). Triggered by --mode chill.
const MODE        = flags.mode || "normal";
const isChill     = MODE === "chill";
// Default tempo tuned so 64 bars in waltz mode = exactly 84.0 s (1:24
// — jeffrey's birthday). 137.143 bpm × 3/4 × 64 = 84.000 s. Effectively
// "138 bpm trance" within 0.6%, but lands on a clean loop boundary.
// Chill is a DANCE track — base tempo well above the 137 trancenwaltz
// cut, and it accelerates from here (see BPM_END). Explicit --bpm wins.
// (Non-chill keeps the birthday tempo.)
const BPM         = Number(flags.bpm ?? (isChill ? 150 : 137.143));
const SCALE_NAME  = flags.scale || "minor";
const ROOT_MIDI   = Number(flags.root ?? 57);
const SECTION     = flags.section || "full";
const BARS_FLAG   = flags.bars !== undefined ? Number(flags.bars) : null;
const SEED_STR    = flags.seed || (isWaltz ? "trancewaltz" : "trance");
const DRUM_GAIN   = Number(flags["drum-gain"] ?? 0.95);
const LEAD_GAIN   = Number(flags["lead-gain"] ?? 0.55);
const PAD_GAIN    = Number(flags["pad-gain"]  ?? 0.30);
const BASS_GAIN   = Number(flags["bass-gain"] ?? 0.45);
const SIDECHAIN   = flags.sidechain !== "off";
const DUCK_DEPTH  = Number(flags["duck-depth"] ?? 0.65);
const DUCK_MS     = Number(flags["duck-ms"] ?? 120);
const VOCAL_STEM  = expandHome(flags["vocal-stem"]) || null;
const VOCAL_GAIN  = Number(flags["vocal-gain"] ?? 2.9);
const VOCAL_MODE  = flags["vocal-mode"] || "loop"; // "single" | "drops" | "all" | "loop"
const VOCAL_DUCK  = Number(flags["vocal-duck"] ?? 0.6); // bed gain under vocal

// Per-section vocal level — applied as a per-sample envelope when in
// loop mode so the vocal isn't blasting at full gain during the intro
// or breakdowns. Drops keep full power; builds and outro mute the
// vocal so the drop impact and musical ending breathe.
const VOCAL_SECTION_GAIN = {
  intro:  0.75,  // strong from t=0 — words intelligible right away
  break1: 0.85,
  build1: 0.0,   // bed-only build = real drop punch
  drop1:  1.00,
  break2: 0.85,
  build2: 0.0,
  drop2:  1.00,
  outro:  0.0,   // musical ending — no vocal
};
const PIANO_GAIN  = Number(flags["piano-gain"] ?? 0.45);
const BELLS_GAIN  = Number(flags["bells-gain"] ?? 0.32);
const STRUCT_PATH = expandHome(flags["struct-out"]) || null;
const OUT_PATH    = expandHome(flags.out) || `${ROOT}/out/${isWaltz ? "trancewaltz" : "trance"}.mp3`;
// --master → a streaming RELEASE cut (DistroKid/Spotify): restore the
// proper musical fade-in/out (a real single ending, NOT the 6ms loop
// declick) and, when the output is .wav, encode lossless 16-bit/44.1k.
const RELEASE_MASTER = Boolean(flags.master);
const OUT_IS_WAV = /\.wav$/i.test(OUT_PATH);

const SAMPLE_RATE = 48_000;

// ── PRNG ──────────────────────────────────────────────────────────────
function hashString(s) {
  let h = 2166136261 >>> 0;
  for (let i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619); }
  return h >>> 0;
}
function makeRng(seedStr) {
  let s = hashString(seedStr) || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}
const rng = makeRng(SEED_STR);
const noiseRng = makeRng(SEED_STR + ":noise");

// ── humanize layer ───────────────────────────────────────────────────
// Tiny random timing offsets applied to every note/hit so the track
// feels organically played rather than grid-locked. Anchor moments
// (drop impacts, sniper, stamps, vocal-aligned events, the boot &
// shutdown melodies) skip humanize so the song's tentpoles stay
// musically locked.
const humanRng = makeRng(SEED_STR + ":humanize");
const HUMANIZE_DEFAULT_MS = 7;
function humanize(t, scaleMs = HUMANIZE_DEFAULT_MS) {
  // Symmetric ±scaleMs uniform jitter, with a tiny biased tail so a
  // few hits land further off the grid than the rest.
  const r = humanRng() * 2 - 1; // -1..+1
  const bias = humanRng() < 0.08 ? 1.6 : 1.0;
  return t + (r * scaleMs * bias) / 1000;
}

// ── musical theory ────────────────────────────────────────────────────
const SCALES = {
  major:    [0, 2, 4, 5, 7, 9, 11],
  minor:    [0, 2, 3, 5, 7, 8, 10],
  harmonic: [0, 2, 3, 5, 7, 8, 11],
  dorian:   [0, 2, 3, 5, 7, 9, 10],
};
const SCALE = SCALES[SCALE_NAME] || SCALES.minor;

function scaleNoteMidi(degree, octaveOffset = 0) {
  const len = SCALE.length;
  const idx = ((degree % len) + len) % len;
  const octShift = Math.floor(degree / len);
  return ROOT_MIDI + 12 * (octaveOffset + octShift) + SCALE[idx];
}
function chordMidis(rootDegree, octaveOffset = 0) {
  return [
    scaleNoteMidi(rootDegree,     octaveOffset),
    scaleNoteMidi(rootDegree + 2, octaveOffset),
    scaleNoteMidi(rootDegree + 4, octaveOffset),
  ];
}

// Multiple chord progressions that rotate every 8 bars. Gives real
// harmonic motion across the track instead of one looping cadence.
//   cycle 0: i  VI III  VII   — Am F C G   (canonical trance)
//   cycle 1: i  iv VI   VII   — Am Dm F G  (more melancholic)
//   cycle 2: i  III VI  iv    — Am C F Dm  (lifted)
//   cycle 3: i  VI iv   VII   — Am F Dm G  (resolution)
const PROGRESSIONS = [
  [0, 5, 2, 6],
  [0, 3, 5, 6],
  [0, 2, 5, 3],
  [0, 5, 3, 6],
];
const CHORD_BARS  = 2;
// Chill: a LONGER pool walked slowly, with chords held 3 bars — so it
// loops far less and travels through much more harmonic territory
// across the track ("more travel in the larger tones throughout").
const PROGRESSIONS_CHILL = [
  [0, 5, 2, 6], [0, 3, 5, 6], [0, 2, 5, 3], [0, 5, 3, 6],
  [0, 4, 5, 3], [0, 6, 3, 5], [0, 2, 3, 6], [0, 3, 6, 2],
];
const CHORD_BARS_CHILL = 3;
function progressionAt(barIdx) {
  if (isChill) {
    const step = Math.floor(barIdx / CHORD_BARS_CHILL);
    const prog = Math.floor(barIdx / (CHORD_BARS_CHILL * 4)) % PROGRESSIONS_CHILL.length;
    return PROGRESSIONS_CHILL[prog][step % 4];
  }
  const progCycle = Math.floor(barIdx / (CHORD_BARS * 4)) % PROGRESSIONS.length;
  const progStep  = Math.floor((barIdx % (CHORD_BARS * 4)) / CHORD_BARS);
  return PROGRESSIONS[progCycle][progStep];
}

// ── theme ─────────────────────────────────────────────────────────────
// 4/4 theme: 8 eighth-notes per bar × 4 bars.
const THEME_4 = [
  0, 2, 4, 3,  4, 7, 4, 3,
  4, 5, 7, 4,  7, 9, 7, 4,
  7, 9, 7, 5,  4, 3, 4, 2,
  3, 2, 0, 2,  3, 4, 2, 0,
];
// 3/4 theme: 6 eighth-notes per bar × 4 bars. Three-feel phrasing —
// ascending arc resolving down. Each bar has its own internal 1-2-3.
const THEME_3 = [
  0, 2, 4,  3, 2, 0,
  6, 0, 2,  4, 3, 2,
  3, 4, 7,  4, 3, 2,
  0, 6, 4,  0, 2, 0,
];
const THEME = isWaltz ? THEME_3 : THEME_4;
const NOTES_PER_BAR_LEAD = isWaltz ? 6 : 8;

// ── arrangement ───────────────────────────────────────────────────────
// Per-section: which layers are active + which fx envelopes to apply.
// fx.wobble / fx.bitcrush envelopes are interpreted with t=0 at the
// start of the section, ending at section length.
const SECTION_TEMPLATES_4 = {
  intro:  { bars: 8,  kick: false, hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: false, riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: true,  drumDensity: 0.5 },
  break1: { bars: 16, kick: false, hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: false, drumDensity: 0.5, fx: { wobble: { target: "filter", baseCutoffHz: 2400, rate: [{ time: 0, rate: 0.5 }, { time: 1, rate: 1.5 }], depth: [{ time: 0, depth: 0.15 }, { time: 1, depth: 0.55 }] } } },
  build1: { bars: 8,  kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 16 }, { time: 1, bits: 6 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 4 }], mix: 0.7 } } },
  drop1:  { bars: 16, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 0.5, bits: 10 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.5, downsample: 2 }, { time: 1, downsample: 1 }], mix: 0.3 } } },
  break2: { bars: 8,  kick: false, hat: false, sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: false, drumDensity: 0, fx: { wobble: { target: "filter", baseCutoffHz: 1800, rate: 2.0, depth: 0.45 } } },
  build2: { bars: 4,  kick: true,  hat: true,  sub: true,  lead: false, pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 1, bits: 5 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 6 }], mix: 0.8 } } },
  drop2:  { bars: 16, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 12 }, { time: 0.4, bits: 8 }, { time: 0.7, bits: 12 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.4, downsample: 3 }, { time: 0.7, downsample: 1 }, { time: 1, downsample: 1 }], mix: 0.35 } } },
  outro:  { bars: 4,  kick: true,  hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: true,  drumDensity: 0.7 },
};
const SECTION_TEMPLATES_3 = {
  intro:  { bars: 6,  kick: false, hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: false, riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: true,  drumDensity: 0.5 },
  break1: { bars: 12, kick: false, hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: false, drumDensity: 0.5, fx: { wobble: { target: "filter", baseCutoffHz: 2200, rate: [{ time: 0, rate: 0.4 }, { time: 1, rate: 1.2 }], depth: [{ time: 0, depth: 0.10 }, { time: 1, depth: 0.50 }] } } },
  build1: { bars: 6,  kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 16 }, { time: 1, bits: 6 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 4 }], mix: 0.65 } } },
  drop1:  { bars: 12, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 0.5, bits: 10 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.5, downsample: 2 }, { time: 1, downsample: 1 }], mix: 0.3 } } },
  break2: { bars: 6,  kick: false, hat: false, sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: false, drumDensity: 0, fx: { wobble: { target: "filter", baseCutoffHz: 1600, rate: 1.5, depth: 0.4 } } },
  build2: { bars: 3,  kick: true,  hat: true,  sub: true,  lead: false, pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 1, bits: 5 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 5 }], mix: 0.7 } } },
  drop2:  { bars: 12, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 12 }, { time: 0.4, bits: 8 }, { time: 0.7, bits: 12 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.4, downsample: 3 }, { time: 0.7, downsample: 1 }, { time: 1, downsample: 1 }], mix: 0.35 } } },
  outro:  { bars: 7,  kick: true,  hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: true,  drumDensity: 0.7 },
};
// CHILL mode — study-vibes minimal dance mix. Continuous bed, no
// drops, no machine guns, no riser sweeps. Bells + lead + pad + sub
// bass + hat + supersaw all running gently throughout. Long sections
// give the loops time to breathe. ~3 minutes total (135 bars × 1.3125 s).
const SECTION_TEMPLATES_3_CHILL = {
  // Intro is gentle — pad + sparse hat + slow bells only. NO kick
  // (which lands as a doubled drum in chill density and reads as
  // a glitch) and NO greeting vocal (the chill is wordless). The
  // kick enters at break1 so there's actual instrumental arrival.
  intro:   { bars: 12, kick: false, hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: false, drumDensity: 0.30 },
  break1:  { bars: 27, kick: true,  hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.55 },
  build1:  { bars: 27, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.75 },
  drop1:   { bars: 33, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.90 },
  break2:  { bars: 14, kick: true,  hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.55 },
  build2:  { bars: 0,  kick: false, hat: false, sub: false, lead: false, pad: false, piano: false, bells: false, riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: false, drumDensity: 0 },
  drop2:   { bars: 28, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.90 },
  outro:   { bars: 11, kick: true,  hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: true,  drumDensity: 0.40 },
};
const SECTION_TEMPLATES = isChill && isWaltz
  ? SECTION_TEMPLATES_3_CHILL
  : isWaltz ? SECTION_TEMPLATES_3 : SECTION_TEMPLATES_4;

let SECTIONS;
if (SECTION === "full") SECTIONS = ["intro", "break1", "build1", "drop1", "break2", "build2", "drop2", "outro"];
else if (SECTION === "drop")  SECTIONS = ["drop1"];
else if (SECTION === "break") SECTIONS = ["break1"];
else if (SECTION === "build") SECTIONS = ["build1"];
else { console.error(`unknown section '${SECTION}'`); process.exit(1); }
// Drop zero-bar sections from the layout (chill mode has no build2).
SECTIONS = SECTIONS.filter((s) => SECTION_TEMPLATES[s].bars > 0);

const sectionBars = SECTIONS.map((s) => SECTION_TEMPLATES[s].bars);
const naturalBars = sectionBars.reduce((a, b) => a + b, 0);
const TOTAL_BARS = BARS_FLAG !== null ? BARS_FLAG : naturalBars;

const beatSec  = 60 / BPM;
const barSec   = beatSec * METER;
// Opening prefix — boot melody chime + greeting voice play
// SIMULTANEOUSLY at t=0.05. Trimmed to 2.7 s so music + vocal enter
// while the greeting's "Angeles" tail is still resolving — the
// sniper bang lands on a 16th-note hihat tick just inside the
// overlap, making the entry punchier.
// In chill mode the opening boot-sonification (startup melody + the
// beep→hat blend) is removed, so there's no narrative prefix to host.
// Start the music almost immediately — just a hair of lead-in for the
// --master fade — instead of leaving ~2.7 s of dead air at the top of
// the single.
const OPENING_PREFIX_SEC = isChill ? 0.25 : 2.7;

// Tempo accelerando — chill ramps LINEARLY from the base BPM up to a
// faster end BPM across the whole track (the track's "linear swing →
// variable bpm": it's a dance track that should drive harder as it
// goes). Deterministic — a pure function of bar index, so struct.json /
// section boundaries / video alignment all regenerate consistently.
// Non-chill: BPM_END === BPM → constant tempo → trancenwaltz stays
// byte-identical.
const BPM_END = isChill ? 180 : BPM; // chill = exponential pacing top (for the log)
const barDurSec   = []; // per-bar duration (accelerating in chill)
const barStartRel = []; // music-relative start of each bar
                        // (length TOTAL_BARS+1 — last entry = musicSec)
{
  let acc = 0;
  for (let i = 0; i < TOTAL_BARS; i++) {
    const fr = TOTAL_BARS > 1 ? i / (TOTAL_BARS - 1) : 0;
    let bpmI;
    if (isChill) {
      // ONE totalizing arc — tons of ENERGY up front, then slowing it
      // all down the more it plays. Single monotonic DECELERANDO: starts
      // fast and progressively (increasingly) winds down to the slow end.
      const FAST = 180, SLOW = 146;
      bpmI = FAST - (FAST - SLOW) * Math.pow(fr, 1.5); // fast → slowing more over time

    } else {
      bpmI = BPM; // constant — trancenwaltz byte-identical
    }
    barStartRel.push(acc);
    const d = (60 / bpmI) * METER;
    barDurSec.push(d);
    acc += d;
  }
  barStartRel.push(acc); // final boundary = musicSec
}
const musicSec = barStartRel[TOTAL_BARS] ?? (barSec * TOTAL_BARS);
const totalSec = musicSec + OPENING_PREFIX_SEC;
const tailSec  = 0;
const totalSamples = Math.ceil((totalSec + tailSec) * SAMPLE_RATE);

console.log(`→ ${isWaltz ? "trancewaltz" : "trance"} · ${BPM}${BPM_END !== BPM ? ` exp-pacing⇅${BPM_END}×2` : ""} bpm · ${METER}/4 · ${SCALE_NAME} (root ${ROOT_MIDI}) · ${TOTAL_BARS} bars · ${totalSec.toFixed(1)}s · sections [${SECTIONS.join(", ")}]`);

// Bar → section info, with cumulative start time so fx envelopes
// know the local-zero of their section.
const sectionRanges = [];
{
  let accBars = 0;
  for (const name of SECTIONS) {
    const len = SECTION_TEMPLATES[name].bars;
    sectionRanges.push({
      name,
      startBar: accBars,
      endBar: accBars + len,
      // startSec/endSec include the opening prefix so any downstream
      // dispatch (machine guns, drop impact, screams, fx envelopes,
      // dynamic envelope) lands at wall-clock times correctly.
      startSec: barStartRel[accBars] + OPENING_PREFIX_SEC,
      endSec: barStartRel[accBars + len] + OPENING_PREFIX_SEC,
      template: SECTION_TEMPLATES[name],
    });
    accBars += len;
  }
}
function sectionAtBar(barIdx) {
  for (const r of sectionRanges) {
    if (barIdx < r.endBar) return { name: r.name, localBar: barIdx - r.startBar, sectionLen: r.endBar - r.startBar };
  }
  const last = sectionRanges[sectionRanges.length - 1];
  return { name: last.name, localBar: 0, sectionLen: last.endBar - last.startBar };
}

// ── buffers ───────────────────────────────────────────────────────────
const dryBuf  = new Float32Array(totalSamples); // kick, hat, snare roll, lead, riser
const duckBuf = new Float32Array(totalSamples); // pad (sidechained)
const bassBuf = new Float32Array(totalSamples); // sub bass — its own bus so we can wobble it deep without affecting the pad
const mgBuf   = new Float32Array(totalSamples); // machine-gun bus — bypasses bitcrush so depth survives
const sfxDryBuf = new Float32Array(totalSamples); // dry SFX bus — opening sniper, skips the mgBuf distance reverb
const sawBuf  = new Float32Array(totalSamples); // supersaw bus — gets fast flange before it joins the sidechained duckBuf
const shutdownBuf = new Float32Array(totalSamples); // ending bus — bye jeffrey + shutdown chime + ending hihats. NOT tape-stopped so the ending mirrors the opening: voice + chime + hats stay at full pitch while the bed slows.
const chillBellBuf = new Float32Array(totalSamples); // chill mode: super-low drawn-out bells routed here so we can flanger them
const kickTimes = [];

// ── lead variation state — subtle, evolving, microtonal ──────────────
// Random-walk drift across the track so no two loops are identical.
// Per-step: cent offset wanders (Brownian ±3¢, clamped ±25¢), small
// chance of ±1 scale-step shift, tiny chance of a rest.
const leadRng = makeRng(SEED_STR + ":lead-evolve");
let leadCentsDrift = 0;
// Bunny-bow comp state: deterministic rng + the last note it played
// (so it can voice-lead by nearest tone — the "bounce").
const bunnyRng = makeRng(SEED_STR + ":bunny-bow");
let bunnyPrevMidi = null;
function nextLeadVariation() {
  leadCentsDrift += (leadRng() - 0.5) * 6;
  if (leadCentsDrift > 25) leadCentsDrift = 25;
  if (leadCentsDrift < -25) leadCentsDrift = -25;
  let stepShift = 0;
  const r = leadRng();
  if (r < 0.06) stepShift = leadRng() < 0.5 ? 1 : -1;
  else if (r < 0.10) stepShift = leadRng() < 0.5 ? 2 : -2;
  const skip = leadRng() < 0.03;
  return { cents: leadCentsDrift, stepShift, skip };
}

// ── synths over the sound.synth bus ───────────────────────────────────
function fireDrum(target, startSec, letter, opts = {}) {
  const sound = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  playPercussion(sound, letter, { phase: "both", ...opts });
}

// ac-native boot melody — triangle C5→E5→G5 ascending (matches
// audio.c:audio_ready_melody) + a descending GLIDE tail that pitches
// down from G5 to A4 so the chime resolves into the square arp's
// starting note instead of stopping abruptly. The glide is a stepped
// exponential sweep emitting many tiny triangle voices.
function fireBootMelody(target, startSec, gain = 1.0) {
  const notes = [
    { tone: 523.25, dur: 0.15, vol: 0.70 }, // C5
    { tone: 659.25, dur: 0.15, vol: 0.70 }, // E5
    { tone: 783.99, dur: 0.20, vol: 0.80 }, // G5
  ];
  // Emit one SFX event per note so the visualizer can render a
  // fullscreen dink per beep (not just a single span for the whole
  // melody).
  {
    let bt = startSec;
    let idx = 0;
    for (const n of notes) {
      events.sfx.push({ t: bt, name: `boot-beep-${++idx}`, dur: n.dur, point: true });
      bt += n.dur + 0.060;
    }
  }
  let t = startSec;
  for (const n of notes) {
    const v = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    v.synth({
      type: "triangle",
      tone: n.tone,
      duration: n.dur,
      volume: gain * n.vol,
      attack: 0.003,
      decay: n.dur * 0.6,
    });
    t += n.dur + 0.060; // 60 ms gap between notes
  }
  // (No descending glide — the abrupt dip + climb made it sound like
  // two separate events. The pre-roll arp continues the boot melody
  // timbre (triangle, same C5-E5-G5 pitch range) so they read as
  // ONE voice that gradually morphs into the square wave by music
  // entry.)
}

// ac-native shutdown sound — triangle G5→E5→C5 descending. Matches
// audio.c:audio_shutdown_sound(). Same envelope/gap as boot melody.
function fireShutdownMelody(target, startSec, gain = 1.0) {
  const notes = [
    { tone: 783.99, dur: 0.15, vol: 0.70 }, // G5
    { tone: 659.25, dur: 0.15, vol: 0.70 }, // E5
    { tone: 523.25, dur: 0.20, vol: 0.80 }, // C5
  ];
  // One SFX event per descending note so the visualizer can render
  // a fullscreen "dink" per beep on the way out.
  {
    let bt = startSec;
    let idx = 0;
    for (const n of notes) {
      events.sfx.push({ t: bt, name: `shutdown-beep-${++idx}`, dur: n.dur, point: true });
      bt += n.dur + 0.060;
    }
  }
  let t = startSec;
  for (const n of notes) {
    const v = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    v.synth({
      type: "triangle",
      tone: n.tone,
      duration: n.dur,
      volume: gain * n.vol,
      attack: 0.003,
      decay: n.dur * 0.6,
    });
    t += n.dur + 0.060;
  }
}

// Whistle — approximation of the ac-native Cook/STK physically-
// modeled flute (audio.c:generate_whistle_sample). The full waveguide
// model would be a major port; this version captures the same
// perceptual character: pure tonal core + 5 Hz vibrato (±3 %) +
// breath noise rider. Emits many sustained sine segments stepping
// through the vibrato curve so the pitch wobbles like the real
// thing.
function fireWhistle(target, startSec, midi, durSec, gain = 1.0) {
  const baseFreq = 440 * Math.pow(2, (midi - 69) / 12);
  // For short notes (≤ 0.30 s) fire a single voice — vibrato barely
  // develops over such a short duration and the segmented-stepping
  // approach smeared the attack. For longer notes, use vibrato
  // segments stepping at a real 5 Hz rate.
  if (durSec <= 0.30) {
    const v = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine",     tone: baseFreq,        duration: durSec, volume: gain * 0.55, attack: 0.005, decay: durSec * 0.85 });
    v.synth({ type: "sine",     tone: baseFreq * 2,    duration: durSec, volume: gain * 0.18, attack: 0.008, decay: durSec * 0.85 });
    v.synth({ type: "sine",     tone: baseFreq * 3,    duration: durSec, volume: gain * 0.08, attack: 0.010, decay: durSec * 0.85 });
    v.synth({ type: "noise",    tone: baseFreq * 1.5,  duration: durSec, volume: gain * 0.28, attack: 0.005, decay: durSec * 0.85 });
    v.synth({ type: "noise",    tone: 4500,            duration: durSec, volume: gain * 0.10, attack: 0.005, decay: durSec * 0.85 });
    return;
  }
  // Long-note path — segmented vibrato (5 Hz, ±3 % pitch).
  const segDur = 0.045; // fixed-size segments → real-time vibrato rate
  const segments = Math.max(3, Math.floor(durSec / segDur));
  const VIBRATO_HZ = 5;
  for (let i = 0; i < segments; i++) {
    const sf = i / segments;
    const elapsedSec = sf * durSec;
    const vibrato = Math.sin(elapsedSec * 2 * Math.PI * VIBRATO_HZ) * 0.03;
    const tone = baseFreq * (1 + vibrato);
    const stepStart = startSec + elapsedSec;
    const env = Math.sin(sf * Math.PI);
    const v = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine",     tone,               duration: segDur + 0.04, volume: gain * 0.40 * env, attack: 0.010, decay: segDur * 0.9 });
    v.synth({ type: "sine",     tone: tone * 2,     duration: segDur + 0.04, volume: gain * 0.10 * env, attack: 0.012, decay: segDur * 0.9 });
    v.synth({ type: "sine",     tone: tone * 3,     duration: segDur + 0.04, volume: gain * 0.06 * env, attack: 0.014, decay: segDur * 0.9 });
    v.synth({ type: "noise",    tone: tone * 1.5,   duration: segDur,        volume: gain * 0.25 * env, attack: 0.008, decay: segDur * 0.85 });
    v.synth({ type: "noise",    tone: 4500,         duration: segDur,        volume: gain * 0.10 * env, attack: 0.008, decay: segDur * 0.85 });
  }
}

// Mosquito / drone-bird — a thin detuned sawtooth "insect" that
// wanders in pitch (slow random walk), buzzes (fast ~8-13 Hz wing
// vibrato), flutters in amplitude (comes near / drifts off) and
// circles the stereo field. Used in the chill mix instead of the
// sustained supersaw wall — a small triad of these zip around like
// annoying-but-musical flies.
function fireMosquito(target, startSec, midi, durSec, gain = 1.0, panBase = 0) {
  const baseFreq = 440 * Math.pow(2, (midi - 69) / 12);
  const segDur = 0.05;
  const segments = Math.max(4, Math.floor(durSec / segDur));
  const vibHz = 8 + noiseRng() * 5;            // 8–13 Hz buzzy wingbeat
  const panRate = 0.5 + noiseRng() * 1.6;      // how fast it circles
  const panPhase = noiseRng() * Math.PI * 2;
  let drift = 0;                                // slow pitch walk (cents)
  for (let i = 0; i < segments; i++) {
    const sf = i / segments;
    const elapsed = sf * durSec;
    drift += (noiseRng() - 0.5) * 22;
    if (drift > 120) drift = 120;
    if (drift < -120) drift = -120;
    const vib = Math.sin(elapsed * 2 * Math.PI * vibHz) * 0.04;
    const tone = baseFreq * Math.pow(2, drift / 1200) * (1 + vib);
    const flutter = 0.5 + 0.5 * Math.abs(Math.sin(elapsed * 2 * Math.PI * (vibHz * 0.5)));
    const env = Math.sin(sf * Math.PI);         // swell in, fade out
    const pan = Math.max(-1, Math.min(1,
      panBase + 0.9 * Math.sin(elapsed * panRate * 2 * Math.PI + panPhase)));
    const v = makeBufferSynth(target, startSec + elapsed, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sawtooth", tone, duration: segDur + 0.03, volume: gain * 0.5 * env * flutter, attack: 0.004, decay: segDur * 0.9, pan });
    v.synth({ type: "sawtooth", tone: tone * 1.004, duration: segDur + 0.03, volume: gain * 0.30 * env * flutter, attack: 0.004, decay: segDur * 0.9, pan: -pan });
  }
}

// Meditation gong — a high struck sine bell with a very long, slowly
// dissipating ring, trailing into a white-noise "wave" that swells,
// crests and zippers back out (a tone sweep up-then-down across a
// noise band). Fired every ~8 bars in the chill mix.
// Simulated CHINESE GONG / tam-tam (no real gong sample in the repo).
// A LOW fundamental + a stack of INHARMONIC partials (non-integer
// ratios → metallic clang), each slightly detuned for shimmer/beating,
// with a BLOOM envelope: the higher partials swell IN over ~0.6 s as a
// real gong "opens up", then a very long shimmering decay, with a dark
// metallic noise wash under the bloom. `deep` = bigger / lower / longer.
function fireGong(startSec, midi, gain = 1.0, deep = false, reverse = false) {
  const f = 440 * Math.pow(2, (midi - 69) / 12) * (deep ? 0.5 : 1.0);
  const dur = deep ? 10.0 : 7.5;
  const seg = 0.06;
  const segN = Math.floor(dur / seg);
  const partials = [
    { r: 1.00, g: 1.00, blo: 0.05 },
    { r: 1.48, g: 0.62, blo: 0.18 },
    { r: 2.03, g: 0.70, blo: 0.30 },
    { r: 2.74, g: 0.48, blo: 0.45 },
    { r: 3.46, g: 0.40, blo: 0.55 },
    { r: 4.21, g: 0.30, blo: 0.65 },
    { r: 5.40, g: 0.24, blo: 0.75 },
    { r: 6.79, g: 0.18, blo: 0.85 },
  ];
  for (let s = 0; s < segN; s++) {
    const u = s / segN;
    // reverse = the swell runs backward (quiet → bloom → strike): the
    // envelope timeline is time-flipped while segments stay in place.
    const ur = reverse ? 1 - u : u;
    const tt = ur * dur;
    const t0 = startSec + s * seg;
    const decay = Math.pow(reverse ? u : 1 - u, 1.5); // long ring / reverse swell
    const v = makeBufferSynth(dryBuf, t0, SAMPLE_RATE, noiseRng);
    for (let p = 0; p < partials.length; p++) {
      const P = partials[p];
      const peak = P.blo * (deep ? 1.4 : 1.0);     // higher partials bloom later
      const bloom = Math.exp(-Math.pow((tt - peak) / (peak + 0.5), 2) * 1.3);
      const det = 1 + (p % 2 === 0 ? 1 : -1) * 0.0016 * (p + 1); // beating
      const amp = gain * P.g * decay * bloom * (p === 0 ? 1.0 : 0.8);
      if (amp < 0.0008) continue;
      v.synth({ type: "sine", tone: f * P.r * det, duration: 0.12, volume: amp, attack: 0.012, decay: 0.11 });
    }
    const wash = Math.exp(-Math.pow((tt - 0.4) / 0.9, 2)) * decay;
    if (wash > 0.01) {
      const w = makeBufferSynth(dryBuf, t0, SAMPLE_RATE, noiseRng);
      w.synth({ type: "noise", tone: 320 + 380 * (1 - u), duration: seg + 0.03, volume: gain * 0.05 * wash, attack: 0.01, decay: seg });
    }
  }
}

// iPhone Taptic Engine emulation. Apple's haptic actuator is a linear
// resonant actuator tuned to ~150 Hz; iOS impact/selection haptics are
// very short transients around that pitch. We synth a ~150 Hz damped
// sine + a faint crisp tick. kinds: "tap" (single), "double" (peek/pop),
// "notify" (triple). Audio-only — a felt-in-time tactile pulse layer.
function fireHaptic(target, startSec, gain = 1.0, kind = "tap") {
  const F = 150; // Apple Taptic resonant frequency
  const one = (t0, amp) => {
    const v = makeBufferSynth(target, t0, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine",  tone: F,        duration: 0.016, volume: gain * 0.9 * amp,  attack: 0.0004, decay: 0.014 });
    v.synth({ type: "sine",  tone: F * 2,    duration: 0.010, volume: gain * 0.25 * amp, attack: 0.0004, decay: 0.009 });
    v.synth({ type: "noise", tone: 2200,     duration: 0.004, volume: gain * 0.18 * amp, attack: 0.0002, decay: 0.0035 }); // crisp tick
  };
  if (kind === "double") { one(startSec, 0.85); one(startSec + 0.072, 1.0); }
  else if (kind === "notify") { one(startSec, 0.8); one(startSec + 0.09, 0.8); one(startSec + 0.18, 0.95); }
  else one(startSec, 1.0);
}

// (Removed fireImessageDing — the iOS-style two-tone bell with
// wiggle tail. Drops now use an inlined CHORD version that fires
// 4 chord-tones with the same wiggle envelope. The single-tone
// helper was redundant after that refactor.)

// Sniper SFX — opening shot reads as an actual GUNSHOT, not a bell.
// Sharp transient + supersonic crack + descending noise tail. A
// short tonal body underneath ties it harmonically to the track
// (root A), but the bell-sustain has been removed so it doesn't
// bleed over the intro vocal.
//
// Routed to sfxDryBuf so the mgBuf distance-reverb chain doesn't
// drown the shot.
function fireSniper(target, startSec, gain = 1.0) {
  // Snare body — chest impact, pitched down.
  fireDrum(target, startSec, "d", { volume: gain * 1.5, pitchFactor: 0.45 });

  const body = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);

  // Deep boom — short low-frequency body.
  body.synth({ type: "sine", tone: 42, duration: 0.22, volume: gain * 1.10, attack: 0.002, decay: 0.18 });
  // Supersonic crack — bangin' top transient, snappier.
  body.synth({ type: "triangle", tone: 2400, duration: 0.040, volume: gain * 1.15, attack: 0.0005, decay: 0.035 });
  // Extra-high snap layer — short 4 kHz triangle whip that gives the
  // shot its "snap" — the cracking transient on top of the boom.
  body.synth({ type: "triangle", tone: 4200, duration: 0.020, volume: gain * 0.95, attack: 0.0003, decay: 0.018 });
  // Tiny noise pop at the very start for the gunpowder "snap".
  body.synth({ type: "noise",    tone: 6000, duration: 0.012, volume: gain * 0.85, attack: 0.0002, decay: 0.011 });
  // A-minor chord-tone sine + triangle ring (short — 0.5–0.8 s) so
  // the shot is harmonically grounded but clears out before the
  // intro vocal proper kicks in.
  body.synth({ type: "sine",     tone: 110, duration: 0.80, volume: gain * 0.30, attack: 0.005, decay: 0.78 });
  body.synth({ type: "triangle", tone: 220, duration: 0.70, volume: gain * 0.20, attack: 0.008, decay: 0.68 });
  body.synth({ type: "sine",     tone: 330, duration: 0.60, volume: gain * 0.15, attack: 0.012, decay: 0.58 });

  // Descending pitched-noise tail — 5 bursts over ~0.18 s, gives the
  // sniper its whip-crack tail without an over-long ring.
  for (let i = 0; i < 5; i++) {
    const t = startSec + i * 0.035;
    const tail = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    tail.synth({
      type: "noise",
      tone: 3500 - i * 480,
      duration: 0.06,
      volume: gain * 0.55 * Math.exp(-i * 0.40),
      attack: 0.003,
      decay: 0.055,
    });
  }
}

// Drop impact — overlapping sine bass cluster + 30 Hz boom + pitched-
// down kick transient. Fires once at the start of each drop section for
// a massive, deep "punch" the regular per-beat sub bass can't deliver
// alone. Lives on the SFX bus so the master section dyn envelope
// doesn't tame it.
//
// Voices stack across three octaves at the chord root, plus a 5th and
// a sub-detuned root for thickening. The sustain lets the impact ring
// for ~1 second behind the new section before settling into the regular
// sub-bass off-beat pattern.
function fireDropImpact(target, startSec, midi, gain = 1.0) {
  const baseFreq = 440 * Math.pow(2, (midi - 69) / 12);
  const sound = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  // 7 overlapping sine voices spanning 4 octaves around the root.
  const voices = [
    { freq: baseFreq * 0.25,                                  g: 1.60, dec: 1.80 }, // -2 oct
    { freq: baseFreq * 0.50,                                  g: 1.35, dec: 1.50 }, // -1 oct
    { freq: baseFreq * 0.50 * Math.pow(2, 5 / 1200),          g: 0.90, dec: 1.25 }, // -1 oct detuned
    { freq: baseFreq * 1.00,                                  g: 1.10, dec: 1.10 }, // root
    { freq: baseFreq * 1.00 * Math.pow(2, -5 / 1200),         g: 0.70, dec: 1.05 }, // root detuned
    { freq: baseFreq * 1.50,                                  g: 0.55, dec: 0.85 }, // 5th
    { freq: baseFreq * 2.00,                                  g: 0.38, dec: 0.65 }, // +1 oct
  ];
  for (const v of voices) {
    sound.synth({
      type: "sine",
      tone: v.freq,
      duration: 2.0,
      volume: gain * v.g * 0.65,
      attack: 0.004,
      decay: v.dec,
    });
  }
  // BOOM — sustained 25 Hz body, longer + louder than before.
  sound.synth({
    type: "sine",
    tone: 25,
    duration: 1.6,
    volume: gain * 1.55,
    attack: 0.001,
    decay: 1.30,
  });
  // Second sub-boom at 45 Hz overlapping for double-thump weight.
  sound.synth({
    type: "sine",
    tone: 45,
    duration: 1.0,
    volume: gain * 1.20,
    attack: 0.002,
    decay: 0.85,
  });
  // SUPERSONIC pitch-sweep — a downward whoosh from 8 kHz to 80 Hz
  // over 350 ms. Many overlapping sine voices step through the
  // exponential sweep so the drop entry has the classic bomb-falling
  // tail BEFORE the boom hits.
  const sweepSteps = 22;
  for (let i = 0; i < sweepSteps; i++) {
    const f = i / sweepSteps;
    const tone = 8000 * Math.pow(80 / 8000, f); // 8 kHz → 80 Hz expo
    const stepStart = startSec + f * 0.35;
    const stepDur = 0.08;
    const sweepVoice = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    sweepVoice.synth({
      type: "sine",
      tone,
      duration: stepDur,
      volume: gain * 0.65 * (0.4 + f * 0.6),
      attack: 0.001,
      decay: stepDur * 0.8,
    });
    // Triangle layer adds harmonic content so the sweep cuts through.
    sweepVoice.synth({
      type: "triangle",
      tone,
      duration: stepDur,
      volume: gain * 0.32 * (0.4 + f * 0.6),
      attack: 0.001,
      decay: stepDur * 0.8,
    });
  }
  // Triangle transient for the snap on top.
  sound.synth({
    type: "triangle",
    tone: 600,
    duration: 0.05,
    volume: gain * 0.50,
    attack: 0.001,
    decay: 0.045,
  });
  // Pitched-down kick at t=0 for the chest impact (louder).
  fireDrum(target, startSec, "c", { volume: gain * 1.9, pitchFactor: 0.50 });
}

// Machine-gun SFX — pitched melodic chromatic-descending pattern on
// the beat. Each hit is a staccato snare+sub-boom with a melodic
// triangle on top that walks down a chord-tone arpeggio, giving the
// gunfire a little "lil melody" instead of a random noise wash.
// Lives on its own bus so master fx don't shred the depth.
//
// Hits are snapped to the nearest 16th-note subdivision of the kick
// grid, so they feel on-the-beat rather than randomly scattered.
function fireMachineGun(target, startSec, durSec, gain = 1.0) {
  // 16th-note grid relative to the global tempo — guns sit on the
  // beat instead of free-running.
  const sixteenthSec = (60 / BPM) / 4;
  const total = Math.max(2, Math.floor(durSec / sixteenthSec));
  const beatIdx0 = Math.floor(startSec / sixteenthSec);
  const barIdx = Math.floor(startSec / barSec);
  const chordDeg = progressionAt(barIdx);
  // Pattern uses SCALE DEGREES (not semitones), so the melodic "bleep"
  // line stays diatonic to the minor scale — no more chromatic
  // neighbour notes that registered as "half-step off" against the
  // rest of the track. Values are degree offsets from the chord root,
  // resolved via scaleNoteMidi.
  const PATTERN_DEG = [0, 4, 2, 0, 4, 2, 7, 4, 0, 4, 2, 7, 4, 2, 0, -3];
  const fadeInBullets = Math.max(2, Math.floor(total * 0.35));
  for (let i = 0; i < total; i++) {
    const t = startSec + i * sixteenthSec;
    const deg = PATTERN_DEG[(beatIdx0 + i) % PATTERN_DEG.length];
    // Tight per-bullet pitch variation — ±15¢ so bullets feel pitched-
    // around their target note WITHOUT drifting into another scale
    // degree (which produced the previous half-step-off feeling).
    const centsJitter = (noiseRng() - 0.5) * 0.30;
    const noteMidi = scaleNoteMidi(chordDeg + deg, 0) + centsJitter;
    const noteFreq = 440 * Math.pow(2, (noteMidi - 69) / 12);
    // Harmony — a chord-tone 5th above (degree +4 in scale terms).
    const harmMidi = scaleNoteMidi(chordDeg + deg + 4, 0);
    const harmFreq = 440 * Math.pow(2, (harmMidi - 69) / 12);
    const onBeat = ((beatIdx0 + i) % 4) === 0;
    const fadeMul = i < fadeInBullets ? 0.3 + 0.7 * (i / fadeInBullets) : 1.0;
    const vol = (onBeat ? 0.95 : 0.65) * gain * fadeMul;
    // Quiet chest-impact snare — distant feel.
    fireDrum(target, t, "d", { volume: vol * 0.20, pitchFactor: 0.28 + (noiseRng() - 0.5) * 0.06 });
    const sound = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    // Sub-boom 2 octaves below the melodic note.
    sound.synth({
      type: "sine",
      tone: noteFreq * 0.25,
      duration: sixteenthSec * 0.85,
      volume: vol * 1.10,
      attack: 0.003,
      decay: sixteenthSec * 0.80,
    });
    // MAIN melodic triangle ping — boosted to cut through.
    sound.synth({
      type: "triangle",
      tone: noteFreq,
      duration: sixteenthSec * 0.55,
      volume: vol * 0.85,
      attack: 0.002,
      decay: sixteenthSec * 0.50,
    });
    // Harmony — perfect 5th up, sustained slightly less than main.
    sound.synth({
      type: "triangle",
      tone: harmFreq,
      duration: sixteenthSec * 0.50,
      volume: vol * 0.50,
      attack: 0.003,
      decay: sixteenthSec * 0.45,
    });
    // Octave-up shimmer on downbeats only.
    if (onBeat) {
      sound.synth({
        type: "triangle",
        tone: noteFreq * 2,
        duration: sixteenthSec * 0.30,
        volume: vol * 0.42,
        attack: 0.001,
        decay: sixteenthSec * 0.25,
      });
    }
    // ECHO — a delayed copy of the melodic ping a 16th later, at
    // lower gain, gives the bleeps a spatial bounce / call-and-
    // response feel.
    const echoT = t + sixteenthSec * 1.5;
    const echo = makeBufferSynth(target, echoT, SAMPLE_RATE, noiseRng);
    echo.synth({
      type: "triangle",
      tone: noteFreq,
      duration: sixteenthSec * 0.45,
      volume: vol * 0.35,
      attack: 0.002,
      decay: sixteenthSec * 0.42,
    });
  }
}

// Sub bass — wavetype morph + harmonies. Each hit fires:
//   - root @ blended triangle/square crossfade (morphT 0..1)
//   - 5th up @ sine (harmony layer, lower gain)
//   - octave up @ sine (sky harmony, even lower)
// morphT progresses across the track so the bass timbre evolves from
// warm (triangle) → harsh (square) → back. All voices are native via
// sound.synth({type:"..."}) — port to fedac/native unchanged.
function fireSubBass(target, startSec, midi, durSec, gain, morphT = 0, harmonies = true) {
  const sound = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  const freq = 440 * Math.pow(2, (midi - 69) / 12);
  // Crossfade triangle ↔ square. morphT clamped 0..1.
  const t = Math.max(0, Math.min(1, morphT));
  const triMix = Math.cos(t * Math.PI * 0.5);     // 1 → 0
  const sqMix  = Math.sin(t * Math.PI * 0.5);     // 0 → 1
  if (triMix > 0.01) {
    sound.synth({ type: "triangle", tone: freq, duration: durSec, volume: gain * triMix,        attack: 0.005, decay: durSec * 0.50 });
  }
  if (sqMix > 0.01) {
    // Square is loud — knock it down so it doesn't dominate the morph.
    sound.synth({ type: "square",   tone: freq, duration: durSec, volume: gain * sqMix * 0.45,  attack: 0.005, decay: durSec * 0.50 });
  }
  if (harmonies) {
    const fifth = freq * Math.pow(2, 7 / 12);   // perfect 5th up
    const oct   = freq * 2;                      // octave up
    sound.synth({ type: "sine", tone: fifth, duration: durSec, volume: gain * 0.28, attack: 0.008, decay: durSec * 0.40 });
    sound.synth({ type: "sine", tone: oct,   duration: durSec, volume: gain * 0.22, attack: 0.010, decay: durSec * 0.45 });
  }
}

// Chill intro→break1 bass ARC (chill only — the template has no sub
// here). `musicT` = seconds since music start. Three blended stages:
//   < 8 s    CRUNCH — punchy bit-crushed square stack + a noise spit
//   8–18 s   OCEAN  — crossfade into a deep, smooth lo-fi sub swell
//   ≥ 18 s   (caller adds the SWING — off-beats pushed late)
// `grind` 1→0: BVROOOM grindy wobble sub at the start that mellows into
// a smooth deep ocean sub as the track chills out.
function fireChillArcBass(startSec, midi, durSec, grind) {
  const freq = 440 * Math.pow(2, (midi - 69) / 12);
  const g = BASS_GAIN;
  const gr = Math.max(0, Math.min(1, grind));
  const smooth = 1 - gr;
  if (gr > 0.02) {
    // GRINDY WOB — sawtooth+square with an internal wobble LFO on pitch
    // + amplitude (the "woob"/Bvrooom), loudest at the start, easing
    // out as it chills. Segmented so the wobble actually moves.
    const segN = 8;
    const segLen = durSec / segN;
    const wobHz = 5.5;
    for (let s = 0; s < segN; s++) {
      const lfo = 0.5 + 0.5 * Math.sin((s / segN) * durSec * 2 * Math.PI * wobHz);
      const wob = 0.85 + 0.30 * lfo;            // pitch wobble
      const amp = (0.55 + 0.45 * lfo) * gr;
      const v = makeBufferSynth(bassBuf, startSec + s * segLen, SAMPLE_RATE, noiseRng);
      v.synth({ type: "sawtooth", tone: freq * 2 * wob,        duration: segLen * 1.3, volume: g * 0.80 * amp, attack: 0.004, decay: segLen });
      v.synth({ type: "square",   tone: freq * wob,            duration: segLen * 1.3, volume: g * 0.52 * amp, attack: 0.004, decay: segLen });
      v.synth({ type: "sawtooth", tone: freq * 4 * wob * 1.01, duration: segLen,       volume: g * 0.16 * amp, attack: 0.003, decay: segLen * 0.7 });
    }
  }
  if (smooth > 0.02) {
    // smooth deep ocean sub — fades IN as the grind eases (chilled).
    const wow = 1 + Math.sin(startSec * 2 * Math.PI * 0.7) * 0.012;
    const k = makeBufferSynth(bassBuf, startSec, SAMPLE_RATE, noiseRng);
    k.synth({ type: "sine",     tone: freq * wow,       duration: durSec * 1.4, volume: g * 1.00 * smooth, attack: 0.02, decay: durSec * 1.1 });
    k.synth({ type: "sine",     tone: freq * 0.5 * wow, duration: durSec * 1.5, volume: g * 0.80 * smooth, attack: 0.03, decay: durSec * 1.2 });
    k.synth({ type: "triangle", tone: freq * wow,       duration: durSec * 1.1, volume: g * 0.28 * smooth, attack: 0.02, decay: durSec * 0.85 });
  }
}

// Chill KICK — a deep, HARMONIC, tuned kick (not a sample). Pitched to
// a chord tone (harmonizes with the progression), 3 octaves down for
// weight, with per-hit variety: mixed body wavetypes, some long-decay
// booms / some super-short tight ones, and a phase-shifted sub layer
// (varying offset → comb/phase movement). No double-kicks. Deterministic
// via noiseRng so it's varied but not random-sounding.
function fireChillKick(target, startSec, chordDeg, idx, gain) {
  const r2 = noiseRng();
  // CLEAN classic kick ONLY — the synth harmonic/square body "broke
  // the mix" and sounded harsh, so it's gone. Just the percussion-bank
  // TR-808 "c" at a deep, controlled level (big headroom — it was way
  // too hot), with a quiet SINE sub for warmth/tuning (never square).
  const gK = gain * 0.55;                              // headroom cut
  const deep = 0.66 + r2 * 0.14;                       // deep 808, varied
  fireDrum(target, startSec, "c", { volume: gK, pitchFactor: deep });
  const midi = scaleNoteMidi(chordDeg, -3);            // chord root, deep
  const freq = 440 * Math.pow(2, (midi - 69) / 12);
  const dur  = noiseRng() < 0.4 ? 0.30 : 0.12;         // some longer, some tight
  const k = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  k.synth({ type: "sine", tone: freq, duration: dur, volume: gK * 0.45, attack: 0.004, decay: dur * 0.8 });
}

// "Bunny bow" — the comp glue. One BOWED, BRUSHED, sustained note: a
// slow swell (soft attack → crescendo → release) so consecutive notes
// OVERLAP/legato, warm timbre (triangle + soft saw bite + body), a
// brush/shaker noise grain riding the same swell, and gentle bow
// vibrato. Pitch is chosen by the caller via nearest-tone voice-leading
// through the chord (with passing/diminished steps) — it "bounces"
// around the other voices like a soft bow.
function fireBunnyBow(target, startSec, midi, durSec, gain) {
  const f0 = 440 * Math.pow(2, (midi - 69) / 12);
  const seg = 0.045;
  const n = Math.max(4, Math.floor(durSec / seg));
  const atk = 0.40, rel = 0.32;
  for (let i = 0; i < n; i++) {
    const sf = i / n;
    const tt = sf * durSec;
    let env;
    if (sf < atk) env = 0.5 - 0.5 * Math.cos(Math.PI * (sf / atk));
    else if (sf > 1 - rel) env = 0.5 - 0.5 * Math.cos(Math.PI * ((1 - sf) / rel));
    else env = 1;
    const vib = 1 + 0.013 * Math.sin(2 * Math.PI * 5.0 * tt); // gentle bow vibrato
    const t0 = startSec + i * seg;
    const v = makeBufferSynth(target, t0, SAMPLE_RATE, noiseRng);
    v.synth({ type: "triangle", tone: f0 * vib,     duration: seg + 0.03, volume: gain * 0.55 * env, attack: 0.012, decay: seg * 0.9 });
    v.synth({ type: "sawtooth", tone: f0 * vib,     duration: seg + 0.03, volume: gain * 0.15 * env, attack: 0.014, decay: seg * 0.9 }); // soft bow bite
    v.synth({ type: "sine",     tone: f0 * 2 * vib, duration: seg + 0.03, volume: gain * 0.10 * env, attack: 0.014, decay: seg * 0.8 }); // body
    v.synth({ type: "noise",    tone: 3200,         duration: seg,        volume: gain * 0.06 * env, attack: 0.010, decay: seg * 0.8 }); // brush/shaker grain
  }
}

// Riser — pitched sine sweep + chord-tone arpeggios with a thin noise
// top. The user wanted "more pitched noise" instead of plain white-
// noise sloshes, so the body is pitched tones; noise is a glaze on top.
function fireRiser(target, startSec, durSec, gain) {
  // Continuous pitched sweep — 18 sine steps from ~200 Hz to ~5 kHz.
  // Overlapping windows so the sweep is heard as one rising tone.
  const sweepSteps = 18;
  for (let i = 0; i < sweepSteps; i++) {
    const t = i / sweepSteps;
    const tone = 200 * Math.pow(25, t); // 200 Hz → ~5 kHz exponential
    const stepStart = startSec + t * durSec;
    const stepDur = durSec / sweepSteps + 0.12; // overlap with next step
    const sweep = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    sweep.synth({
      type: "sine",
      tone,
      duration: stepDur,
      volume: gain * 0.7 * (0.3 + t * 0.7),
      attack: stepDur * 0.25,
      decay: stepDur * 0.55,
    });
    // Layer a 5th-above sine for harmonic density
    sweep.synth({
      type: "sine",
      tone: tone * 1.5,
      duration: stepDur,
      volume: gain * 0.35 * (0.3 + t * 0.7),
      attack: stepDur * 0.30,
      decay: stepDur * 0.50,
    });
  }
  // Thin noise glaze (was the whole riser before — now just adds shimmer)
  const noiseSteps = 4;
  for (let i = 0; i < noiseSteps; i++) {
    const t = i / noiseSteps;
    const tone = 1500 * Math.pow(6, t); // 1.5 kHz → 9 kHz bandpass
    const stepStart = startSec + t * durSec;
    const stepDur = durSec / noiseSteps + 0.1;
    const noise = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    noise.synth({
      type: "noise",
      tone,
      duration: stepDur,
      volume: gain * 0.25 * (0.4 + t * 0.6),
      attack: stepDur * 0.3,
      decay: stepDur * 0.45,
    });
  }
}

// Lazy-load piano bank only if some section actually wants piano.
const needPiano = Object.values(SECTION_TEMPLATES).some((t) => t.piano);
if (needPiano) loadPianoBank();

// Per-instrument entry bars — instruments come in progressively across
// the track. Even if the SECTION template enables them, they're gated
// until their entry bar so the opening starts lean.
// Defaults work for the typical 60-bar trancewaltz arrangement.
const INSTRUMENT_ENTRY_BAR_NORMAL = {
  pad:       0,
  kick:      2,
  hat:       2,
  sub:       8,
  bells:     6,
  lead:      10,
  piano:     22,
  riser:     12,
  snareRoll: 14,
  supersaw:  6,
};
// CHILL mode entry bars — every voice starts MUCH sooner so the track
// doesn't wait until 30s for piano + lead. Beat is on from bar 0, the
// melodic voices settle in over the first 4 bars (~7 s).
const INSTRUMENT_ENTRY_BAR_CHILL = {
  pad:       0,
  kick:      0,
  hat:       0,
  sub:       4,
  bells:     0,
  lead:      2,
  piano:     4,
  riser:     999, // disabled
  snareRoll: 999, // disabled
  supersaw:  2,
};
const INSTRUMENT_ENTRY_BAR = isChill ? INSTRUMENT_ENTRY_BAR_CHILL : INSTRUMENT_ENTRY_BAR_NORMAL;
function instrumentEnabled(name, bar) {
  const entry = INSTRUMENT_ENTRY_BAR[name];
  if (entry === undefined) return true;
  return bar >= entry;
}

// ── build event list ──────────────────────────────────────────────────
let kickCount = 0, hatCount = 0, subCount = 0, leadCount = 0, padCount = 0, riserCount = 0, snareRollCount = 0, pianoCount = 0, bellsCount = 0, machineGunCount = 0, supersawCount = 0, dropImpactCount = 0;
// Event timeline — every fired note recorded for the scrolling-score
// visualization. Each entry: { t: startSec, dur: durSec, midi?: noteMidi, gain?: vol }
const events = {
  kick: [],
  hat: [],
  snare: [], // snare-roll subdivisions (build crescendo + main snare roll)
  sub: [],
  lead: [],
  bells: [],
  piano: [],
  supersaw: [],
  arp: [], // intro/break1 square-wave arpeggio (its own visible lane)
  dropImpact: [],
  sfx: [], // boot chime, sniper, shutdown chime, machine guns, drone flybys, cats, birds, lion roar
  vox: [], // greeting, booty, screams, bye, vocal stem chunks
};

// Machine-gun moments — guns now bypass per-section bitcrush (added
// to `out` after fx), so we can run them louder AND more frequent
// without losing depth. Bursts on every bar of every build section,
// plus scattered hits across each drop section.
const machineGunHits = [];
for (const r of sectionRanges) {
  if (r.name.startsWith("build")) {
    // Gentle, distant-sounding bursts in the second half of the build.
    const buildLen = r.endBar - r.startBar;
    const startBar = Math.min(2, Math.max(0, buildLen - 3));
    for (let b = startBar; b < buildLen; b++) {
      const barStart = r.startSec + b * barSec;
      const frac = (b - startBar) / Math.max(1, buildLen - 1 - startBar);
      const gain = 0.55 + frac * 0.55; // 0.55 → 1.10 (was 1.0 → 2.1)
      machineGunHits.push({ startSec: barStart + barSec * 0.10, durSec: barSec * 0.75, gain });
    }
  }
  if (r.name.startsWith("drop")) {
    // Both drops now use the same simpler 2-burst layout — drop2's
    // pattern (held + late accent) felt much cooler than drop1's 4
    // scattered bursts, so drop1 matches it for narrative consistency.
    const dropLen = r.endSec - r.startSec;
    machineGunHits.push({ startSec: r.startSec,                  durSec: barSec * 1.5, gain: 0.40 });
    machineGunHits.push({ startSec: r.startSec + dropLen * 0.65, durSec: 0.55,         gain: 0.45 });
  }
}

for (let bar = 0; bar < TOTAL_BARS; bar++) {
  // Per-bar tempo (accelerating in chill). These shadow the module-level
  // base beatSec/barSec for everything computed inside the loop, so all
  // note durations / sub-steps / arp grid follow the accelerando for free.
  const barSec  = barDurSec[bar];
  const beatSec = barSec / METER;
  const barStart = OPENING_PREFIX_SEC + barStartRel[bar];
  const { name: sec, localBar, sectionLen } = sectionAtBar(bar);
  const t = SECTION_TEMPLATES[sec];

  const chordDeg = progressionAt(bar);
  const triad    = chordMidis(chordDeg, 0);
  const bassMidi = scaleNoteMidi(chordDeg, -2);

  const isLastBar  = localBar === sectionLen - 1;
  const isFinalBar = sec === "outro" && isLastBar;
  const kickEnabled = t.kick && !(t.riser && isLastBar) && !isFinalBar && instrumentEnabled("kick", bar);
  const densityRoll = rng();

  // The track's linear "swing" is now a TEMPO accelerando (see BPM_END /
  // barDurSec) rather than an off-beat delay — pushing off-beats late
  // fought the "faster dance track" goal. swingSec stays 0 so the
  // existing `+ swingSec` call sites are harmless no-ops.
  const swingSec = 0;

  // Percussion-silent zones (chill): the first 8 s, the last 8 s, and a
  // mid bridge (~±7 s around the midpoint) have NO kick/hat — then they
  // come back. Lets the track open, breathe mid-way, and resolve.
  const _mt = barStart - OPENING_PREFIX_SEC;
  const _mid = totalSec / 2;
  const chillPercMute = isChill && (
    _mt < 8 ||
    barStart > totalSec - 8 ||
    Math.abs(barStart - _mid) < 7
  );
  // Hats stay out LONGER than the kick on the mid-bridge exit: the kick
  // returns first (no hats) for ~10 s, then the hats come back in.
  const chillHatMute = chillPercMute ||
    (isChill && barStart > _mid + 7 && barStart < _mid + 17);

  // Meditation gong — every 8 bars in chill. Every OTHER one (and
  // sometimes extra) is a big deep hollow warbling "BONGGG"; the rest
  // stay the high struck bell. Skips bar 0 so the intro breathes in.
  if (isChill && bar > 0 && bar % 8 === 0) {
    const gongIdx = Math.floor(bar / 8);
    const deep = gongIdx % 2 === 1 || noiseRng() < 0.2;
    fireGong(barStart, scaleNoteMidi(chordDeg, deep ? -1 : 0), deep ? 0.42 : 0.36, deep, gongIdx % 2 === 0);
    bellsCount++;
  }

  // Taptic haptic pulse (chill) — a felt-in-time ~150 Hz tap on the
  // downbeat; a peek/pop "double" on phrase boundaries. Muted in the
  // percussion-silent zones so the breaths stay clean.
  if (isChill && !chillPercMute) {
    const ht = humanize(barStart, 4);
    fireHaptic(dryBuf, ht, 0.12, bar % 8 === 0 ? "double" : "tap");
  }

  // Final musical-ending cadence — fires on the last bar of the outro.
  // Held tonic chord (root + 3rd + 5th + octave) on the pad, a bell
  // ring on the root-octave, and a deep root thud. All overshoot
  // totalSec into the tail; the natural decay ends the track without
  // any forced ffmpeg fade.
  if (isFinalBar) {
    // Final cadence — durations tuned so the natural pad+bell decay
    // tapers to silence within the last bar (totalSec). No more
    // 4-second overshoot into a tail that gets ffmpeg-faded; the
    // music decays itself, supporting the clean loop boundary.
    const remainingSec = totalSec - barStart;
    const finalTriad = chordMidis(0, 0);
    const finalNotes = [...finalTriad, scaleNoteMidi(0, 1)];
    for (const m of finalNotes) {
      mixEventLeadPad(
        { startSec: barStart, midi: m, gain: PAD_GAIN * 1.8, durSec: remainingSec },
        duckBuf,
        { preset: "pad", sampleRate: SAMPLE_RATE, seed: `final:${m}` }
      );
      padCount++;
    }
    mixEventSinebell(
      { startSec: barStart, midi: scaleNoteMidi(0, 1), gain: BELLS_GAIN * 1.45, durSec: remainingSec },
      dryBuf,
      SAMPLE_RATE
    );
    bellsCount++;
    fireSubBass(bassBuf, barStart, scaleNoteMidi(0, -2), Math.min(remainingSec, barSec * 1.5), BASS_GAIN * 1.3, 0, true);
    subCount++;
  }

  // Kick — every beat (METER hits per bar) with rhythmic variation.
  // Approaching the tape-stop window (last 4 s of music) the kicks
  // organically settle: density falls to a regular metronome and
  // volume tapers so the slowdown feels like the song breathing
  // out rather than being spliced into the shutdown.
  if (kickEnabled && !chillPercMute) {
    const TAPE_STOP_START = totalSec - 5.0;
    const SETTLE_WINDOW   = 4.0; // 4 s before tape-stop start
    const sixteenthSec_k = beatSec / 4;
    for (let beat = 0; beat < METER; beat++) {
      if (densityRoll > t.drumDensity && sec === "intro" && localBar < Math.floor(sectionLen / 2)) continue;
      const startSec = barStart + beat * beatSec;
      // Hard stop at tape-stop start — slowed kicks won't muddy the
      // shutdown chime.
      if (startSec >= TAPE_STOP_START) continue;
      // Settle factor — 1.0 during normal play, drops to 0.0 by the
      // tape-stop start. Used to gate variation + taper volume.
      let settleAmp = 1.0;
      let settleVar = 1.0;
      if (startSec > TAPE_STOP_START - SETTLE_WINDOW) {
        const u = (startSec - (TAPE_STOP_START - SETTLE_WINDOW)) / SETTLE_WINDOW; // 0 → 1
        settleAmp = 1.0 - u * 0.5;  // taper to 50 % volume
        settleVar = 1.0 - u;        // skips + ghosts fade out
      }
      // Variation: beat 1 always; other beats fire 85 % normally;
      // probability climbs to 1.0 in the settle window so the
      // rhythm becomes regular again.
      const baseProb = beat === 0 ? 1.0
                     : (sec === "break1" ? 0.70 : 0.92);
      const beatFireProb = baseProb + (1.0 - baseProb) * (1.0 - settleVar);
      // Chill: NEVER drop / skip kicks (the random drops read as
      // mistakes + too regular). Every beat fires; the life comes from
      // a slow DISTANCE arc — the kick recedes a bit and returns over
      // ~13 bars (it never goes near-silent, so it never feels dropped).
      if (!isChill && rng() > beatFireProb) continue;
      const accent = beat === 0 ? 1.05 : 0.95;
      const kickDist = isChill ? 0.72 + 0.28 * Math.cos(2 * Math.PI * bar / 13) : 1;
      // Humanize — tight ±5 ms jitter, scaled down in the settle window.
      const kickT = humanize(startSec, 5 * settleVar + 2);
      if (isChill) {
        fireChillKick(dryBuf, kickT, chordDeg, kickCount, accent * settleAmp * DRUM_GAIN * kickDist);
      } else {
        fireDrum(dryBuf, kickT, "c", { volume: accent * settleAmp * DRUM_GAIN });
      }
      kickTimes.push(kickT);
      events.kick.push({ t: kickT });
      kickCount++;
      // Ghost 16th — disabled in chill mode (the doubled hit reads as
      // a kick repeat at the top of the track), disabled in the
      // settle window for organic feel.
      if (!isChill && rng() < 0.12 * settleVar) {
        const ghostT = humanize(startSec + sixteenthSec_k, 8);
        if (ghostT < TAPE_STOP_START) {
          fireDrum(dryBuf, ghostT, "c", { volume: 0.45 * settleAmp * DRUM_GAIN });
          kickTimes.push(ghostT);
          events.kick.push({ t: ghostT, kind: "ghost" });
          kickCount++;
        }
      }
    }
  }

  // Closed hat — off-beats (the "and" of every kick). METER hits per bar.
  if (t.hat && instrumentEnabled("hat", bar) && !chillHatMute) {
    // Hat pattern with skips and 16th-note flams. Variation tapers
    // out in the last 4 s before tape-stop so the rhythm settles
    // into a regular pattern as the slowdown approaches — organic
    // transition, not spliced.
    const HAT_SUPPRESS_AFTER = totalSec - 5.0;
    const SETTLE_WINDOW_H = 4.0;
    const sixteenthSec_h = beatSec / 4;
    for (let beat = 0; beat < METER; beat++) {
      const startSec = barStart + (beat + 0.5) * beatSec + swingSec;
      if (startSec >= HAT_SUPPRESS_AFTER) continue;
      // Settle factor — 1.0 normally, drops to 0.0 by tape-stop.
      let settleVar = 1.0;
      if (startSec > HAT_SUPPRESS_AFTER - SETTLE_WINDOW_H) {
        settleVar = 1.0 - (startSec - (HAT_SUPPRESS_AFTER - SETTLE_WINDOW_H)) / SETTLE_WINDOW_H;
      }
      // Skip probability ramps from 0.20 down to 0 as we settle.
      if (rng() < 0.20 * settleVar) continue;
      const v = 0.32 + rng() * 0.10;
      // Humanize hats — ±8 ms feels like a real drummer.
      const hatT = humanize(startSec, 8 * settleVar + 1);
      // Chill: per-hit random pitchFactor scatters the noise band so no
      // two hats sound identical (more organic, stochastic shimmer).
      const hatPF = isChill ? 1 + (noiseRng() - 0.5) * 0.55 : 1;
      fireDrum(dryBuf, hatT, "g", { volume: v * DRUM_GAIN, pitchFactor: hatPF });
      events.hat.push({ t: hatT, dur: 0.025 });
      hatCount++;
      // Flam — disabled in chill mode (reads as a doubled hihat in
      // the intro at the slow chill density). Probability ramps out
      // in the settle window otherwise.
      if (!isChill && rng() < 0.22 * settleVar) {
        const flamT = humanize(startSec + sixteenthSec_h, 10);
        if (flamT < HAT_SUPPRESS_AFTER) {
          const vf = 0.20 + rng() * 0.08;
          fireDrum(dryBuf, flamT, "g", { volume: vf * DRUM_GAIN });
          events.hat.push({ t: flamT, dur: 0.025, kind: "flam" });
          hatCount++;
        }
      }
    }
    // Chill: occasional fast hi-hat ROLL into a phrase boundary — a
    // quick crescendo of hats across the last beat (fun little fill).
    if (isChill && (bar % 16 === 15 || noiseRng() < 0.02)) { // fewer rolls
      const rollN = 9 + Math.floor(noiseRng() * 5);      // 9–13 hits
      const rollStart = barStart + (METER - 1) * beatSec;
      const deepEcho = noiseRng() < 0.4;                  // some run deep + echoey
      const pf = deepEcho ? 0.34 + noiseRng() * 0.12 : 1 + (noiseRng() - 0.5) * 0.4;
      for (let h = 0; h < rollN; h++) {
        const rt = humanize(rollStart + (h / rollN) * beatSec, 3);
        if (rt >= HAT_SUPPRESS_AFTER) break;
        const rv = (0.10 + 0.22 * (h / rollN)) * DRUM_GAIN;
        fireDrum(dryBuf, rt, "g", { volume: rv, pitchFactor: pf });
        if (deepEcho) {
          // echoey: a couple of decaying delayed taps trailing each hit
          for (let e = 1; e <= 2; e++) {
            const et = rt + e * 0.085;
            if (et < HAT_SUPPRESS_AFTER) fireDrum(dryBuf, et, "g", { volume: rv * (0.45 / e), pitchFactor: pf * (1 - 0.04 * e) });
          }
        }
        events.hat.push({ t: rt, dur: 0.02, kind: "roll" });
        hatCount++;
      }
    }
    // Open hat on phrase boundary every 4 bars. Skipped in chill —
    // it lands on the same 16th as beat-2's main hat (METER-0.5) and
    // reads as a doubled hit.
    if (!isChill && (bar + 1) % 4 === 0) {
      const openHatSec = humanize(barStart + (METER - 0.5) * beatSec, 6);
      if (openHatSec < HAT_SUPPRESS_AFTER) {
        fireDrum(dryBuf, openHatSec, "a", { volume: 0.45 * DRUM_GAIN });
        events.hat.push({ t: openHatSec, dur: 0.10, kind: "open" });
      }
    }
  }

  // Chill bass ARC — intro + break1 have no template sub, so fill them
  // with the crunch→ocean→swung arc bass (on the beat). Crunchy bitted
  // punch for the first ~8 s, morphing into a deep ocean lo-fi sub,
  // then (≥18 s) levelling out on a swing (off-beats pushed late).
  if (isChill && (sec === "intro" || sec === "break1")) {
    for (let beat = 0; beat < METER; beat++) {
      const baseT  = barStart + beat * beatSec;
      const musicT = baseT - OPENING_PREFIX_SEC;
      const swung  = (musicT >= 18 && beat !== 0) ? 0.16 * beatSec : 0;
      const bt     = humanize(baseT + swung, 6);
      const m = (beat === 1 && METER >= 3)
        ? scaleNoteMidi(chordDeg + 4, -2)   // 5th up for color
        : scaleNoteMidi(chordDeg, -2);      // root, low
      const grind = Math.max(0, 1 - musicT / 60); // BVROOOM → chilled over ~60s
      fireChillArcBass(bt, m, beatSec * 0.62, grind);
      events.sub.push({ t: bt, midi: m, dur: beatSec * 0.62 });
      subCount++;
    }
  }

  // Sub bass — off-beat of every beat. Its own bus for deep wobble.
  // Wave morphs from triangle (early track) through square (mid) back
  // to triangle (late) following a track-position arc. Chord-tone
  // alternation: beat 1 = root, beat 2 = 5th up, beat 3 = root.
  if (t.sub && instrumentEnabled("sub", bar)) {
    const trackProgress = bar / Math.max(1, TOTAL_BARS - 1);
    // Sinusoidal morph: 0 → 1 → 0 across the track.
    const morphT = 0.5 - 0.5 * Math.cos(trackProgress * Math.PI * 2);
    const fifthBassMidi = scaleNoteMidi(chordDeg + 4, -2); // chord 5th, low octave
    // Sub bass entry-ramp — when sub turns on at a section boundary
    // (eg. break1 → build1) it used to cold-start at full volume,
    // causing an abrupt textural shift. Ramp the gain in over the
    // first 2 bars of the section so the bass texture emerges
    // gradually under the existing bed.
    let subEntryGain = 1.0;
    if (sec === "build1" || sec === "build2" || sec === "drop1" || sec === "drop2") {
      // Only ramp if the PREVIOUS section had sub: false (real entry).
      const prevSecIdx = sectionRanges.findIndex((r) => r.name === sec) - 1;
      const prevSec = prevSecIdx >= 0 ? sectionRanges[prevSecIdx] : null;
      if (prevSec && !prevSec.template.sub) {
        const beatsIntoSection = localBar * METER;
        const rampBeats = 2 * METER; // 2 bars of ramp
        if (beatsIntoSection < rampBeats) {
          subEntryGain = beatsIntoSection / rampBeats;
        }
      }
    }
    for (let beat = 0; beat < METER; beat++) {
      const startSec = humanize(barStart + (beat + 0.5) * beatSec + swingSec, 6);
      const useFifth = (beat === 1 && METER >= 3); // 2nd beat → 5th up for color
      const midiNote = useFifth ? fifthBassMidi : bassMidi;
      // Per-beat ramp so the FIRST beat of the entry bar is quietest
      // and each subsequent beat is a hair louder.
      const beatRamp = sec === "build1" && localBar < 2
        ? Math.min(1, (localBar * METER + beat) / (2 * METER))
        : subEntryGain;
      fireSubBass(bassBuf, startSec, midiNote, beatSec * 0.55, BASS_GAIN * beatRamp, morphT, true);
      events.sub.push({ t: startSec, midi: midiNote, dur: beatSec * 0.55 });
      subCount++;
    }
  }

  // Pad — long sustained chord per bar. Voicing varies: even cycles
  // play the triad, odd cycles add a 7th for color and sometimes a 9th.
  if (t.pad && instrumentEnabled("pad", bar)) {
    const cycle = Math.floor(bar / (CHORD_BARS * 4));
    const padNotes = [...triad];
    if (cycle % 2 === 1) padNotes.push(scaleNoteMidi(chordDeg + 6, 0)); // 7th
    if (cycle % 4 === 3) padNotes.push(scaleNoteMidi(chordDeg + 1, 1)); // 9th, higher octave
    for (const m of padNotes) {
      mixEventLeadPad(
        { startSec: barStart, midi: m, gain: PAD_GAIN, durSec: barSec * 0.98 },
        duckBuf,
        { preset: "pad", sampleRate: SAMPLE_RATE, seed: `pad:${bar}:${m}` }
      );
      padCount++;
    }
  }

  // Power supersaw — sustained background chord, octave above the pad.
  // Lives on duckBuf so it sidechain-pumps with the kick. Detuned
  // sawtooth stack (JP-8000 style) gives the "trance wall" texture
  // the sine-only voices can't carry alone.
  if (t.supersaw && instrumentEnabled("supersaw", bar)) {
   if (isChill) {
    // Chill: no trance wall. A triad of wandering sawtooth "mosquito"
    // drone-birds in a high whiny register that zip around the stereo
    // field and come/go — each voice sits out ~45 % of bars so they
    // circle in and out instead of droning continuously.
    // Slowly change MELODICALLY across the track, and DEEPEN + DARKEN:
    // start high/whiny (+24) and descend toward +7 by the end, with a
    // slow melodic walk that shifts every ~14 bars.
    const driftFr  = bar / Math.max(1, TOTAL_BARS - 1);
    const octShift = Math.round(24 - 17 * driftFr);          // 24 → 7
    const melWalk  = [0, 3, 5, 3, -2, 0, 4, 2][Math.floor(bar / 14) % 8];
    const mGain    = 0.085 * (1 - 0.4 * driftFr);            // darker later
    let vi = 0;
    for (const m of triad) {
      vi++;
      if (noiseRng() < 0.45) continue;             // this fly sits this bar out
      const startOff = noiseRng() * barSec * 0.5;  // enters at a random point
      const dur = barSec * (0.5 + noiseRng() * 0.5);
      const panBase = vi === 1 ? -0.5 : vi === 2 ? 0.0 : 0.5;
      const mm = m + octShift + melWalk;            // descends + walks melodically
      fireMosquito(sawBuf, barStart + startOff, mm, dur, mGain, panBase);
      events.supersaw.push({ t: barStart + startOff, midi: mm, dur });
      supersawCount++;
    }
   } else {
    // High supersaw — pulled back from 0.20 to 0.13 (it was masking
    // the imessage ding + lion roar + melodic gunfire on the drops).
    for (const m of triad) {
      mixEventSupersaw(
        { startSec: barStart, midi: m + 12, gain: 0.13, durSec: barSec * 0.98 },
        sawBuf,
        { preset: "pad", sampleRate: SAMPLE_RATE, seed: `saw:${bar}:${m}` }
      );
      events.supersaw.push({ t: barStart, midi: m + 12, dur: barSec * 0.98 });
      supersawCount++;
    }
    // Super-low harsh supersaw — root + 5th, also reduced.
    if (sec.startsWith("drop") || sec.startsWith("build")) {
      const rootDeg = chordDeg;
      const fifthDeg = chordDeg + 4;
      mixEventSupersaw(
        { startSec: barStart, midi: scaleNoteMidi(rootDeg, -1), gain: 0.10, durSec: barSec * 0.98 },
        sawBuf,
        { preset: "lead", detuneCents: 32, voices: 7, sampleRate: SAMPLE_RATE, seed: `saw-low:${bar}:r` }
      );
      mixEventSupersaw(
        { startSec: barStart, midi: scaleNoteMidi(fifthDeg, -1), gain: 0.07, durSec: barSec * 0.98 },
        sawBuf,
        { preset: "lead", detuneCents: 32, voices: 7, sampleRate: SAMPLE_RATE, seed: `saw-low:${bar}:5` }
      );
      supersawCount += 2;
    }
   }
  }

  // Lead — base theme + subtle microtonal drift + per-bar octave AND
  // rhythmic-subdivision shuffles. Each bar rolls:
  //   octave    : -7 / 0 / +7 / +14 scale-degrees (down 1 → up 2 octaves)
  //   subdivision: 0.5× (half-time) / 1× / 2× (double-time)
  // gives a vastly wider melodic and rhythmic range than the fixed theme.
  if (t.lead && instrumentEnabled("lead", bar)) {
    const themeOffsetBar = bar % 4;
    const rhRoll = leadRng();
    const ocRoll = leadRng();
    const subdivMul = rhRoll < 0.18 ? 0.5 : rhRoll < 0.82 ? 1.0 : 2.0;
    const octaveShift =
      ocRoll < 0.14 ? -7 :
      ocRoll < 0.28 ?  7 :
      ocRoll < 0.36 ? 14 : 0;
    // Skip-bar — about 1/3 of bars get a syncopated rhythm: every 3rd
    // note becomes a rest, and the surviving notes get pushed back by a
    // 16th-note for that "delay skip" / behind-the-beat feel. Creates
    // the bouncing skipping melody instead of a flat march.
    // Chill: skip more bars AND thin each bar to ~half the notes so the
    // melody holds + breathes instead of running.
    const skipBar  = leadRng() < (isChill ? 0.55 : 0.34);
    const notesThisBar = Math.max(1, Math.floor(NOTES_PER_BAR_LEAD * subdivMul * (isChill ? 0.5 : 1)));
    const noteSec = barSec / notesThisBar;
    const skipDelay = noteSec * 0.18; // 16th-note nudge late
    for (let s = 0; s < notesThisBar; s++) {
      // Skip pattern — drop every 3rd slot (positions 1, 4, 7…) so the
      // remaining notes feel bunched + bouncy.
      if (skipBar && s % 3 === 1) continue;
      // For double-time wrap THEME; for half-time take every other note.
      const sourceIdx = subdivMul >= 1
        ? s % NOTES_PER_BAR_LEAD
        : Math.floor(s * (1 / subdivMul));
      const themeIdx = themeOffsetBar * NOTES_PER_BAR_LEAD + sourceIdx;
      const baseDeg = THEME[themeIdx % THEME.length];
      if (baseDeg == null) continue;
      const v = nextLeadVariation();
      if (v.skip) continue;
      // Chill: extra rests — leave space between phrases.
      if (isChill && leadRng() < 0.28) continue;
      const deg = baseDeg + v.stepShift + octaveShift;
      // On-beat notes in skip bars get nudged late so the bar feels
      // like it's lagging then catching up.
      const skipOffset = skipBar && s % 3 === 2 ? skipDelay : 0;
      // Humanize lead — ±11 ms feels expressive without losing the line.
      // Chill rides the swing pocket too, so the line lays back with the groove.
      const startSec = humanize(barStart + s * noteSec + skipOffset + swingSec, 11);
      const leadMidi = scaleNoteMidi(deg, 0) + v.cents / 100;
      const inBreak = !t.kick;
      // Chill: notes HOLD ~2× their slot so they ring across the rests
      // (sustained, not plucky). Otherwise the original plucky envelope.
      const durMul = isChill
        ? (skipBar ? 1.1 : 1.9)
        : (skipBar ? 0.65 : (inBreak ? 1.4 : 0.85));
      const durSec = noteSec * durMul;
      mixEventLeadPad(
        { startSec, midi: leadMidi, gain: LEAD_GAIN, durSec },
        dryBuf,
        { preset: "lead", sampleRate: SAMPLE_RATE, seed: `lead:${bar}:${s}` }
      );
      events.lead.push({ t: startSec, midi: leadMidi, dur: durSec });
      leadCount++;
    }
  }

  // Bunny-bow comp (chill) — overlapping bowed/brushed notes that
  // voice-lead by NEAREST chord tone to the last one ("bounce"), with
  // occasional scale/diminished passing approaches. Long swells that
  // bleed into each other = the legato 4th-octave glue the sine-chime
  // comp was missing. Routed to duckBuf so it sits in the harmonic bed.
  if (isChill) {
    const strokes = 2;
    const step = barSec / strokes;
    for (let nb = 0; nb < strokes; nb++) {
      const t0 = barStart + nb * step;
      const cands = [];
      for (const tone of [0, 2, 4, 6]) {
        for (const oc of [0, 1]) {
          let m = scaleNoteMidi(chordDeg + tone, oc);
          while (m < 60) m += 12;
          while (m > 81) m -= 12;
          cands.push(m);
        }
      }
      let target;
      if (bunnyPrevMidi == null) {
        target = scaleNoteMidi(chordDeg, 1);
      } else {
        let best = 1e9;
        target = cands[0];
        for (const m of cands) {
          const d = Math.abs(m - bunnyPrevMidi);
          if (d < best) { best = d; target = m; }
        }
      }
      if (bunnyRng() < 0.14) target += 12; // little hop
      const dur = step * 1.7;              // overlaps into the next stroke
      const g = 0.16;
      if (bunnyPrevMidi != null && bunnyRng() < 0.24) {
        const dir = target >= bunnyPrevMidi ? 1 : -1;
        const pass = target - dir * (bunnyRng() < 0.5 ? 1 : 2); // semitone(dim)/whole step
        fireBunnyBow(duckBuf, humanize(t0, 8), pass, step * 0.5, g * 0.8);
        fireBunnyBow(duckBuf, humanize(t0 + step * 0.42, 8), target, dur * 0.75, g);
      } else {
        fireBunnyBow(duckBuf, humanize(t0, 8), target, dur, g);
      }
      events.lead.push({ t: t0, midi: target, dur });
      bunnyPrevMidi = target;
    }
  }

  // Riser — fires once at the start of a build section.
  if (t.riser && localBar === 0 && instrumentEnabled("riser", bar)) {
    fireRiser(dryBuf, barStart, barSec * sectionLen, 0.40);
    riserCount++;
  }

  // Piano stabs — backbeat (beats 2 and METER) during drops. Adds a
  // bright harmonic accent on top of the supersaw lead.
  if (t.piano && instrumentEnabled("piano", bar)) {
    const stabBeats = isWaltz ? [1] : [1, 3]; // 3/4: beat 2 only; 4/4: 2 and 4
    for (const sb of stabBeats) {
      // One humanized stab time for the whole chord so the triad
      // stays struck together (a "hand" of stabbed notes).
      const stabSec = humanize(barStart + sb * beatSec, 9);
      for (const m of triad) {
        mixEventPiano(
          { startSec: stabSec, midi: m + 12, gain: PIANO_GAIN, durSec: beatSec * 0.6 },
          duckBuf,
          SAMPLE_RATE
        );
        events.piano.push({ t: stabSec, midi: m + 12, dur: beatSec * 0.6 });
        pianoCount++;
      }
    }
  }

  // Sinebells — counter-melody arpeggio with a "delay skip" feel.
  // Each bar fires 4 chord-tone notes on dotted-skip positions, so the
  // bells dance rather than hold. Offsets in 8ths of a bar:
  //   0,    3,    5,    8   (out of 8) — skips the strict 1-2-3-4 grid
  // The 3rd and 5th positions are intentionally "delayed" so the
  // pattern feels off-the-beat / skipping along the rhythm.
  if (t.bells && instrumentEnabled("bells", bar)) {
    // Bar-position determined chord tones — root, 3rd, 5th, octave —
    // give the arpeggio harmonic shape rather than a single drone.
    // Patterns rotate by bar-group so the bells don't repeat every
    // 2 bars (the old ascending/descending toggle). Chill mode needs
    // longer-cycle variation since it has 12-24 bar sections.
    const patternIdx = Math.floor(bar / 2) % 6;
    const PATTERNS = [
      // ascending close
      { degs: [4, 6, 2, 7], octs: [1, 1, 2, 2] },
      // descending close
      { degs: [7, 4, 2, 6], octs: [2, 2, 1, 1] },
      // open-voiced wide jump
      { degs: [0, 4, 2, 7], octs: [2, 1, 2, 1] },
      // high register
      { degs: [4, 2, 7, 6], octs: [2, 2, 2, 1] },
      // low register
      { degs: [0, 4, 6, 2], octs: [1, 1, 0, 1] },
      // zig-zag
      { degs: [4, 7, 2, 6], octs: [1, 2, 2, 1] },
    ];
    const pat = PATTERNS[patternIdx];
    const arpDegrees = pat.degs.map((d) => chordDeg + d);
    let arpOctaves = pat.octs.slice();
    // CHILL mode bell variation — every 8 bars, transpose the whole
    // arp by one octave for a longer-cycle melodic shape so the bells
    // don't feel locked. Cycle: +0, +0, -1, +0, +1, +0, -1, +0, ...
    if (isChill) {
      const chillCycle = Math.floor(bar / 8) % 4;
      const chillShift = [0, -1, 1, 0][chillCycle];
      arpOctaves = arpOctaves.map((o) => o + chillShift);
    }
    // Eighth-note positions inside the bar, with delayed-skip offsets.
    // 3/4 bar = 6 eighths;  4/4 bar = 8 eighths. Scale by METER.
    const eighthSec = barSec / (METER * 2);
    const positions = [0, 3, 5, 8].map((p) => p * eighthSec * 0.85);
    // Tail-pad transition — in the LAST ~10 s of music, the bells
    // morph from struck-bell arps into slow opening-pad swells. They
    // drop a full octave, take a long slow attack, and ring with a
    // much longer T60 so they bleed into the tape-stop as a warm
    // sub-octave drone. Goal: by 1:17-ish the bells are no longer
    // arpeggiating, they're a held sub pad behind the shutdown.
    const TAIL_PAD_START = totalSec - 10;
    const inTailPad = (barStart + barSec * 0.5) > TAIL_PAD_START;
    // Smooth ramp 0 → 1 across the 10-second window so the morph isn't
    // abrupt — the bell character bends down rather than cutting.
    const tailPadT = inTailPad
      ? Math.min(1, Math.max(0, (barStart + barSec * 0.5 - TAIL_PAD_START) / 10))
      : 0;
    for (let i = 0; i < arpDegrees.length; i++) {
      // Humanize bell arp — ±10 ms gives the dotted-skip pattern an
      // organic rubato feel instead of a quantized music-box.
      const startSec = humanize(barStart + Math.min(positions[i], barSec * 0.92), 10);
      // Lower the octave by 1 in tail-pad mode. arpOctaves are already
      // {0,1,2} so subtract 1 → {-1,0,1}, dropping the whole arp down.
      const padOctaveOffset = tailPadT > 0 ? -1 : 0;
      const midi = scaleNoteMidi(arpDegrees[i], arpOctaves[i] + padOctaveOffset);
      // Hold time stretches up in tail-pad mode: from 0.85 bar to
      // ~2.5 bars on the held first note, so the pad sustain bleeds
      // organically into the tape-stop.
      const padDurMul = 1 + tailPadT * 2.0;
      const durSec = barSec * (i === 0 ? 0.85 : 0.45 - i * 0.05) * padDurMul;
      const gainMul = i === 0 ? 1.0 : 0.65 - i * 0.06;
      // Slow opening swell attack in tail-pad mode: 12 ms → 380 ms.
      const attackSec = 0.012 + tailPadT * 0.37;
      // Decay scale: 1× struck → 3× slow pad ring.
      const decayScale = 1 + tailPadT * 2.0;
      // CHILL bells — very LOW tone (drop 2 octaves), super DRAWN
      // OUT (long attack + 4× decay scale), quieter, and routed to
      // a dedicated bus that gets a slow flanger before mixing in.
      // They sit underneath the bed as a low harmonic wash instead
      // of competing for attention.
      let chillMidi = midi;
      let chillGain = BELLS_GAIN * gainMul;
      let chillAttack = attackSec;
      let chillDecay = decayScale;
      let chillTail = 4 + tailPadT * 5;
      let bellTarget = dryBuf;
      if (isChill) {
        chillMidi = midi - 24;                  // drop 2 octaves — sub-low harmonic wash
        chillGain = BELLS_GAIN * gainMul * 0.55; // quieter, less attention
        chillAttack = 0.45;                      // slow swell instead of struck attack
        chillDecay = 4.0;                        // 4× longer T60 — super drawn out
        chillTail = 8;                           // long ring tail
        bellTarget = chillBellBuf;               // dedicated bus for the flanger
      }
      mixEventSinebell(
        { startSec, midi: chillMidi, gain: chillGain, durSec, attackSec: chillAttack, decayScale: chillDecay, tailSec: chillTail },
        bellTarget,
        SAMPLE_RATE
      );
      events.bells.push({ t: startSec, midi: chillMidi, dur: durSec });
      bellsCount++;
    }
  }

  // Square arpeggio — a voice that transforms over the track:
  //   Phase 1 (~0-4 s of arp life): bright square-wave 16ths, the
  //     classic intro expression.
  //   Phase 2 (4 s → ~8 s): crossfade square → sine, OCTAVE DOWN,
  //     volume DIPS WAY back, notes ELONGATE to ~half rate.
  //   Phase 3 (8 s → end of break1, ~30 s wall-clock): low elongated
  //     sine voice tapers to silence — a different instrument
  //     entirely, lingering as the bed takes over.
  //
  // Arp enters at music entry (intro bar 0) — bright square arpeggio
  // crescendos in over 4 s, hits full volume by mid-intro, then runs
  // through break1 with the timbre/octave/density transformation.
  const ARP_START_SEC = OPENING_PREFIX_SEC;
  if (sec === "intro" || sec === "break1") {
    const baseSteps = METER === 3 ? 12 : 16;
    const arpPattern = METER === 3
      ? [0, 2, 4, 6, 4, 2, 0, 4, 2, 6, 4, 2]
      : [0, 2, 4, 6, 4, 2, 0, 4, 2, 6, 4, 2, 0, 4, 6, 4];
    // Sample the arp time at the bar's midpoint to pick the phase.
    const tArp = (barStart + barSec * 0.5) - ARP_START_SEC;
    // Phase decision:
    //   tArp <= 4   → Phase 1 (pure square, full density, normal vol)
    //   4 < tArp < 8 → Phase 2 (crossfade, octave down ramps in)
    //   tArp >= 8   → Phase 3 (sine only, elongated, fading out)
    let waveMix;  // 0 = pure square, 1 = pure sine
    let octShift; // semitones to subtract
    let steps;    // notes per bar
    let durMul;   // duration multiplier per note (relative to stepSec)
    let fadeMul;
    if (tArp <= 4.0) {
      // Phase 1 — bright square, linear crescendo over the first 4 s.
      waveMix = 0;
      octShift = 0;
      steps = baseSteps;
      durMul = 0.85;
      fadeMul = Math.max(0.10, Math.min(1, tArp / 4.0));
    } else if (tArp < 8.0) {
      // Phase 2 — crossfade. 4 → 8 seconds of arp life.
      const u = (tArp - 4.0) / 4.0; // 0 → 1
      waveMix = u;
      octShift = 12 * u; // ramps from 0 to one octave down
      steps = Math.round(baseSteps - u * (baseSteps - baseSteps / 2));
      durMul = 0.85 + u * 0.65; // 0.85 → 1.5 (longer notes)
      fadeMul = 1.0 - u * 0.75; // dips from 1.0 → 0.25
    } else {
      // Phase 3 — transformed voice. Sine, octave down, elongated.
      waveMix = 1;
      octShift = 12;
      steps = Math.max(3, Math.floor(baseSteps / 2));
      durMul = 1.5;
      // Across break1, fade from 0.25 → 0 by end of section. With
      // break1 ~12 bars, this happens between localBar ~3 and end.
      const breakProgress = sec === "break1" ? localBar / Math.max(1, sectionLen - 1) : 0;
      fadeMul = 0.25 * Math.max(0, 1 - breakProgress);
    }
    // Chill mix: the arp is a pure SINE one octave down for its entire
    // life — no bright square intro expression. The phase logic still
    // governs the crescendo / elongation / fade dynamics; only timbre
    // and octave are pinned. trancenwaltz keeps the square→sine morph.
    if (isChill) {
      waveMix = 1;     // sine only — never the square
      octShift = 12;   // one octave down throughout
      if (sec === "intro") {
        // START: fast + CACOPHONOUS — dense, short, dissonant shimmer
        // (a chaotic open that then settles into the meditative arp).
        steps = Math.max(baseSteps, Math.round(steps * 2));
        durMul = durMul * 0.5;
      } else {
        // Settle: half the notes, long holds — breathes.
        steps = Math.max(3, Math.round(steps / 2));
        durMul = durMul * 2.0;
      }
    }
    if (fadeMul > 0.01) {
      const stepSec = barSec / steps;
      for (let sIdx = 0; sIdx < steps; sIdx++) {
        // Chill: extra rests so the arp skips + leaves space — but NOT
        // during the intro (the start stays dense + cacophonous).
        if (isChill && sec !== "intro" && noiseRng() < 0.22) continue;
        // Chill swings its off-step (odd) notes into the same late pocket
        // as the hats/sub. Humanize the arp — ±6 ms keeps it alive.
        const swung = (isChill && sIdx % 2 === 1) ? swingSec : 0;
        const startSec = humanize(barStart + sIdx * stepSec + swung, 6);
        const patIdx = sIdx % arpPattern.length;
        const deg = chordDeg + arpPattern[patIdx];
        const octBump = sIdx % 4 < 2 ? 1 : 2;
        const midi = scaleNoteMidi(deg, octBump) - octShift;
        const freq = 440 * Math.pow(2, (midi - 69) / 12);
        const synth = makeBufferSynth(dryBuf, startSec, SAMPLE_RATE, noiseRng);
        // Emit BOTH square and sine voices with complementary
        // weights — crossfade the timbre cleanly without needing a
        // morph waveform.
        const sqVol = (1 - waveMix) * 0.20 * fadeMul;
        const siVol = waveMix       * 0.32 * fadeMul; // sine gets a small boost to match perceived loudness
        if (sqVol > 0.001) {
          synth.synth({ type: "square", tone: freq, duration: stepSec * durMul, volume: sqVol, attack: 0.003, decay: stepSec * 0.55 });
        }
        if (siVol > 0.001) {
          synth.synth({ type: "sine",   tone: freq, duration: stepSec * durMul, volume: siVol, attack: 0.008, decay: stepSec * 0.85 });
        }
        // Chill intro CACOPHONY — a slightly-detuned tritone an
        // octave+ up, clashing against the root for a dissonant,
        // chaotic shimmer at the very start only.
        if (isChill && sec === "intro" && siVol > 0.001) {
          const clashTone = freq * Math.pow(2, (12 + 6) / 12) * 1.013;
          synth.synth({ type: "sine", tone: clashTone, duration: stepSec * durMul, volume: siVol * 0.55, attack: 0.004, decay: stepSec * 0.55 });
        }
        // Emit the arp note so the visualizer draws it as its own lane
        // (the intro square arp was previously invisible).
        if (sqVol > 0.001 || siVol > 0.001) {
          events.arp.push({ t: startSec, midi, dur: stepSec * durMul });
        }
      }
    }
  }

  // Snare roll — subdivisions double across the section. Volume range
  // pulled back so it doesn't clip when stacked under the sub bass +
  // lead. Earlier code peaked at 0.76 which slammed the mix.
  if (t.snareRoll && !isLastBar && instrumentEnabled("snareRoll", bar)) {
    const progress = localBar / Math.max(1, sectionLen - 1);
    const subdiv = progress < 0.33 ? METER * 2 : progress < 0.66 ? METER * 4 : METER * 8;
    for (let h = 0; h < subdiv; h++) {
      // Humanize snare-roll subdivisions — small ±4 ms keeps the roll
      // tight while the hand-played feel comes through.
      const startSec = humanize(barStart + (h / subdiv) * barSec, 4);
      const v = 0.12 + (h / subdiv) * 0.32; // peaks ~0.44 instead of 0.80
      fireDrum(dryBuf, startSec, "d", { volume: v * DRUM_GAIN * 0.85 });
      events.snare.push({ t: startSec, dur: 0.025, kind: "roll" });
      snareRollCount++;
    }
  }

  // Snare-roll pre-roll — the 4 bars BEFORE every build section get a
  // very quiet whispering snare roll that crescendos into the build.
  // Gives the listener a sense the climb is coming, without the
  // dramatic clipping the build-section snare alone causes.
  //
  // Kick pre-roll — same 4-bar window, but for the KICK. Single hit
  // on beat 1 at -4 bars (whisper), then progressively more hits and
  // louder, so the kick has a narrative entry rather than slamming
  // in at the build downbeat. Skipped in chill mode — the chill
  // kicks come in naturally without a crescendo build.
  if (!isChill) {
    const nextBuildStart = (() => {
      for (const r of sectionRanges) {
        if (r.name.startsWith("build") && r.startBar > bar) return r.startBar;
      }
      return -1;
    })();
    const barsAhead = nextBuildStart - bar;
    // Pre-roll window EXTENDED from 4 → 6 bars so the percussion
    // textural shift starts emerging deeper into break1. The
    // crescendo is gentler and longer — the listener feels the build
    // "wake up" gradually instead of snapping on 4 bars before.
    const PREROLL_BARS = 6;
    if (barsAhead > 0 && barsAhead <= PREROLL_BARS) {
      const prerollT = (PREROLL_BARS - barsAhead) / PREROLL_BARS; // 0 → 0.83
      const subdiv = METER * 2;
      for (let h = 0; h < subdiv; h++) {
        // Humanize pre-roll subdivisions — same ±4 ms as the main roll.
        const startSec = humanize(barStart + (h / subdiv) * barSec, 4);
        const v = prerollT * (0.05 + (h / subdiv) * 0.18);
        if (v < 0.01) continue;
        fireDrum(dryBuf, startSec, "d", { volume: v * DRUM_GAIN * 0.7 });
        events.snare.push({ t: startSec, dur: 0.025, kind: "preroll" });
        snareRollCount++;
      }
      // ── kick pre-roll — SMOOTH crescendo across the 6 bars ──
      // Continuous probability + velocity curves; per-beat in each bar:
      //   probability of firing rises 0.20 → 1.00 across the 6 bars
      //   velocity rises 0.10 → 0.95 across the same window
      //   beat 1 always fires; other beats roll dice
      const arrivalT = 1 - (barsAhead / PREROLL_BARS); // 0 → ~0.83
      for (let beat = 0; beat < METER; beat++) {
        const beatT = arrivalT + (beat / METER) * (1 / PREROLL_BARS);
        const fireProb = 0.20 + beatT * 0.80; // 0.20 → 1.0
        const beatVel  = 0.10 + beatT * 0.85; // 0.10 → 0.95
        // Always fire beat 1; roll dice for others.
        const fires = beat === 0 || noiseRng() < fireProb;
        if (!fires) continue;
        // Humanize the pre-roll kicks too — looser ±9 ms because these
        // are sparse and crescendoing, so the sloppy timing helps the
        // narrative of "drummer waking up to the build".
        const kickT = humanize(barStart + beat * beatSec, 9);
        fireDrum(dryBuf, kickT, "c", { volume: beatVel * DRUM_GAIN });
        kickTimes.push(kickT);
        events.kick.push({ t: kickT, kind: "preroll" });
        kickCount++;
      }
    }
  }
}

// Fire all machine-gun bursts into their OWN bus so master bitcrush
// (which lives on `out`) doesn't shred the depth. Skipped in chill
// mode — no machine guns in the study-vibes mix.
if (!isChill) {
  for (const mg of machineGunHits) {
    fireMachineGun(mgBuf, mg.startSec, mg.durSec, mg.gain);
    events.sfx.push({ t: mg.startSec, name: "machine-gun", dur: mg.durSec });
    machineGunCount++;
  }
}

// Drop impact — stacked sine bass cluster at the start of every drop
// section. Lives on the SFX bus (mgBuf) so it survives both the
// section dynamic envelope and the master bitcrush.
//
// CHILL mode gets a soft drop: sub bass donk + chord ding, NO roar,
// NO machine guns, NO massive impact. Just a gentle "arrival" cue
// when each drop section enters — fun without being a hard drop.
for (const r of sectionRanges) {
  if (isChill && r.name.startsWith("drop")) {
    events.dropImpact.push({ t: r.startSec, name: r.name });
    events.sfx.push({ t: r.startSec, name: `chill-drop-${r.name}`, dur: 0.45 });
    const chordDeg = progressionAt(r.startBar);
    // Gentle chord-tone ding — root + 5th, sine + 2nd harmonic, no
    // pitch wiggle, longer decay. Sits soft in the mix.
    const dingMidis = [chordDeg, chordDeg + 4];
    for (const dm of dingMidis) {
      const dingFreq = 440 * Math.pow(2, (scaleNoteMidi(dm, 1) - 69) / 12);
      const synth = makeBufferSynth(sfxDryBuf, r.startSec, SAMPLE_RATE, noiseRng);
      synth.synth({ type: "sine", tone: dingFreq,     duration: 0.45, volume: 0.22, attack: 0.005, decay: 0.42 });
      synth.synth({ type: "sine", tone: dingFreq * 2, duration: 0.30, volume: 0.07, attack: 0.005, decay: 0.28 });
    }
    // Soft sub bass donk — single octave-down note, gentle decay.
    const donkMidi = scaleNoteMidi(chordDeg, -1);
    fireSubBass(bassBuf, r.startSec, donkMidi, 2.0, 0.65 * BASS_GAIN, 0.2, false);
    dropImpactCount++;
    continue;
  }
  if (!isChill && r.name.startsWith("drop")) {
    const chordDeg = progressionAt(r.startBar);
    const impactMidi = scaleNoteMidi(chordDeg, -2); // root, low octave
    fireDropImpact(mgBuf, r.startSec, impactMidi, 1.25);
    events.sfx.push({ t: r.startSec, name: "drop-impact+lion", dur: 0.5 });
    // BIG VIBEY DONK — sustained low bass that rings under the drop
    // entry so it doesn't feel like blip emptiness after the hush.
    // Drop1 gets the biggest donk (3.5 s decay), drop2 a slightly
    // shorter one. Multiple sine voices stacked at sub octaves with
    // a fifth-up harmony for movement.
    const donkGain = r.name === "drop1" ? 1.6 : 1.2;
    const donkDur  = r.name === "drop1" ? 3.5 : 2.5;
    fireSubBass(bassBuf, r.startSec, impactMidi, donkDur, donkGain * BASS_GAIN, 0.3, true);
    // Second deeper donk — another full octave down for SEISMIC weight.
    fireSubBass(bassBuf, r.startSec, impactMidi - 12, donkDur * 0.85, donkGain * BASS_GAIN * 0.75, 0, false);
    // Lion roar from ac-native zoo bank — pitched down for BIG drop
    // weight. BOOSTED gain (1.4 → 2.6) and routed to sfxDryBuf so the
    // roar isn't smeared into the distance-reverb tail.
    mixZooSample(
      { startSec: r.startSec, name: "lion", gain: 2.6, pitchRatio: 0.55 },
      sfxDryBuf,
      SAMPLE_RATE
    );
    // CHORDIFIED ding — fire the pitch-wiggle ding voice across four
    // chord-tones (root + 3rd + 5th + octave) so the drop bleep
    // sounds spacious rather than two-tone. Routed dry so it punches
    // through the post-hush silence.
    const chordTones = [
      scaleNoteMidi(chordDeg,     1), // root, oct +1
      scaleNoteMidi(chordDeg + 2, 1), // 3rd
      scaleNoteMidi(chordDeg + 4, 1), // 5th
      scaleNoteMidi(chordDeg,     2), // octave
    ];
    for (const m of chordTones) {
      // Treat each chord-tone as a (sine + 2nd harmonic) bell with
      // the iMessage-style 6-cycle pitch wiggle on the tail.
      const f = 440 * Math.pow(2, (m - 69) / 12);
      const steady = makeBufferSynth(sfxDryBuf, r.startSec, SAMPLE_RATE, noiseRng);
      steady.synth({ type: "sine", tone: f,     duration: 0.20, volume: 1.10 * 0.55, attack: 0.002, decay: 0.18 });
      steady.synth({ type: "sine", tone: f * 2, duration: 0.15, volume: 1.10 * 0.16, attack: 0.003, decay: 0.14 });
      const wiggleSteps = 22;
      const wiggleDur   = 0.35;
      for (let i = 0; i < wiggleSteps; i++) {
        const sf = i / wiggleSteps;
        const wobble = Math.sin(sf * Math.PI * 2 * 6);
        const wTone = f * Math.pow(2, (wobble * 1.2) / 12);
        const stepStart = r.startSec + 0.18 + sf * wiggleDur;
        const stepDur = (wiggleDur / wiggleSteps) + 0.04;
        const wiggle = makeBufferSynth(sfxDryBuf, stepStart, SAMPLE_RATE, noiseRng);
        wiggle.synth({
          type: "sine",
          tone: wTone,
          duration: stepDur,
          volume: 1.10 * 0.36 * (1 - sf * 0.85),
          attack: 0.002,
          decay: stepDur * 0.85,
        });
      }
    }
    dropImpactCount++;
  }
}

// ── drone flybys (drop1 → break2 bridge, ~45-50 s) ──────────────────
// Slower, harmonic flybys. Skipped in chill mode — no whooshes.
if (!isChill) {
  const flybyCount = 4;
  // Drones run inside break2 (post-prefix break2 ≈ 50.25-58.13 s).
  // Shifted with the music by OPENING_PREFIX_SEC so they stay
  // musically aligned to the section structure.
  const startSec = 50.5 + OPENING_PREFIX_SEC;
  const endSec   = 55.0 + OPENING_PREFIX_SEC;
  const spanSec = endSec - startSec;
  // Each flyby anchored on a chord tone — root (deg 0), 5th (deg 4),
  // octave (deg 7), back to root — climbs harmonically.
  const flybyDegrees = [0, 4, 7, 0];
  // Reference chord at 45 s = bar 34 in the 137.143 bpm waltz
  const refBar = Math.floor(startSec / barSec);
  const refChord = progressionAt(refBar);
  for (let f = 0; f < flybyCount; f++) {
    const tFraction = flybyCount > 1 ? f / (flybyCount - 1) : 0;
    const baseT = startSec + tFraction * (spanSec - 1.8);
    events.sfx.push({ t: baseT, name: "drone-flyby", dur: 1.8 });
    // Pitch the flyby to a chord tone, octave +2 (whistle-band).
    const flybyMidi = scaleNoteMidi(refChord + flybyDegrees[f], 2);
    const basePitch = 440 * Math.pow(2, (flybyMidi - 69) / 12);
    const flybyDur = 1.8;   // slow drone — 3× the previous speed
    const flybyGain = 0.28;
    const sweepSteps = 36;
    for (let i = 0; i < sweepSteps; i++) {
      const sf = i / sweepSteps;
      // Gentler doppler curve: 1.0× → 1.08× → 0.94× (less extreme).
      const dopplerCurve = sf < 0.5
        ? 1.0 + 0.08 * (sf * 2)
        : 1.08 - 0.14 * ((sf - 0.5) * 2);
      const stepStart = baseT + sf * flybyDur;
      const stepDur = flybyDur / sweepSteps + 0.08;
      // Longer, smoother amplitude envelope — quieter ride.
      let envAmp;
      if (sf < 0.35)        envAmp = sf / 0.35;
      else if (sf > 0.65)   envAmp = (1 - sf) / 0.35;
      else                  envAmp = 1.0;
      const voice = makeBufferSynth(duckBuf, stepStart, SAMPLE_RATE, noiseRng);
      voice.synth({
        type: "sine",
        tone: basePitch * dopplerCurve,
        duration: stepDur,
        volume: flybyGain * envAmp,
        attack: 0.004,
        decay: stepDur * 0.90,
      });
      // 5th-up harmony layer — chord tone above the drone for density.
      const harmMidi = scaleNoteMidi(refChord + flybyDegrees[f] + 4, 2);
      const harmFreq = 440 * Math.pow(2, (harmMidi - 69) / 12);
      voice.synth({
        type: "sine",
        tone: harmFreq * dopplerCurve,
        duration: stepDur,
        volume: flybyGain * envAmp * 0.45,
        attack: 0.005,
        decay: stepDur * 0.90,
      });
    }
  }
}

// ── flute whistle sings along with the SECOND "computer" ────────────
// Lyric: i / A / love / computer / computer / C / surviving / death.
// At 92 bpm × 0.652 s/beat, the SECOND "computer" lands ~9.13 s into
// the vocal stem; vocal starts at OPENING_PREFIX_SEC (2.7 s) →
// wall-clock 11.83 s.
if (isChill) {
  // Chill whistle: a slow, LOW tone that wanders the WHOLE track on its
  // OWN rhythm — irregular, non-bar-aligned spacing, with varied pitch /
  // length each time so it never loops. Long sustained tones (engages
  // fireWhistle's vibrato path), pitched two octaves below the old chill
  // whistle. A ghostly recurring sigh, not a one-off blip.
  const wEnd = totalSec - 8;
  let wt = 9 + noiseRng() * 4;             // first entry ~9–13 s
  let walk = 0;                             // slow melodic drift
  while (wt < wEnd) {
    const wbar = Math.max(0, Math.floor((wt - OPENING_PREFIX_SEC) / barSec));
    const wchord = progressionAt(wbar);
    walk += Math.round((noiseRng() - 0.5) * 3);
    if (walk > 4) walk = 4;
    if (walk < -4) walk = -4;
    const deg = wchord + [0, 2, 4, 6][Math.abs(walk) % 4];
    const oct = noiseRng() < 0.5 ? -1 : 0;
    const midi = scaleNoteMidi(deg, oct) - 12;   // much lower
    const dur = 2.6 + noiseRng() * 2.8;          // 2.6–5.4 s — very slow
    fireWhistle(sfxDryBuf, wt, midi, dur, 0.16 + noiseRng() * 0.06);
    events.sfx.push({ t: wt, name: "whistle", dur });
    wt += dur + 4 + noiseRng() * 9;              // irregular 4–13 s gap
  }
} else {
  const compStart = 11.83;
  const compBar = Math.floor(compStart / barSec);
  const compChord = progressionAt(compBar);
  // SKIPPY 3-note phrase — short staccato whistle notes tracking the
  // punchy "com-PU-ter" syllables of the vocal.
  const compPhrase = [
    { deg: 0, oct: 1, dur: 0.18, gap: 0.08 },
    { deg: 2, oct: 1, dur: 0.18, gap: 0.08 },
    { deg: 4, oct: 1, dur: 0.32, gap: 0.0  },
  ];
  let t = compStart;
  for (const n of compPhrase) {
    const midi = scaleNoteMidi(compChord + n.deg, n.oct);
    fireWhistle(sfxDryBuf, t, midi, n.dur, 0.32);
    events.sfx.push({ t, name: "whistle", dur: n.dur });
    t += n.dur + n.gap;
  }
}

// ── flute whistle phrase at build2 (~58 s) ───────────────────────────
// Cook/STK physically-modeled whistle approximation playing a short
// 4-note melodic phrase across the build2 entry. Adds a cool tonal
// embellishment that lifts the bed entering drop2.
{
  const whistleStartSec = 58.0;
  const chordAtWhistle = progressionAt(Math.floor(whistleStartSec / barSec));
  // Phrase: chord root (oct +1) → 3rd → 5th → root (oct +2). Climbs
  // a chord-tone arpeggio over ~4 s, ending right at drop2 entry.
  const phrase = [
    { deg: 0, oct: 1, dur: 0.95 },
    { deg: 2, oct: 1, dur: 0.95 },
    { deg: 4, oct: 1, dur: 0.95 },
    { deg: 0, oct: 2, dur: 1.30 }, // soar into the drop
  ];
  let t = whistleStartSec;
  for (const n of phrase) {
    const midi = scaleNoteMidi(chordAtWhistle + n.deg, n.oct);
    // Quiet — sits underneath the build bed rather than soaring on top.
    fireWhistle(sfxDryBuf, t, midi, n.dur, 0.18);
    events.sfx.push({ t, name: "whistle", dur: n.dur });
    t += n.dur;
  }
}

// (Helicopter removed — too loud, distracting from the break2 mix.
// Tried noise-pulse and chord-tone versions; both crowded the
// breakdown. Birds + cats handle the texture in this window.)

// ── jeffrey "booty" SUNG melodic vocal — skipped in chill mode ──────
if (!isChill) {
  const BOOTY_PATH = `${REPO}/pop/dance/out/.booty-sung.mp3`;
  if (existsSync(BOOTY_PATH)) {
    const tmpRaw = `${dirname(OUT_PATH)}/.booty.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", BOOTY_PATH,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpRaw,
    ]);
    if (dec.status === 0 && existsSync(tmpRaw)) {
      const raw = readFileSync(tmpRaw);
      const booty = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
      let bpeak = 0;
      for (let i = 0; i < booty.length; i++) { const a = Math.abs(booty[i]); if (a > bpeak) bpeak = a; }
      const norm = bpeak > 0 ? 0.95 / bpeak : 1.0;
      // Place at drop1 bar 26 (= 8 bars into drop1) so it lands ~37 s
      // and the ascending pitch peaks during the drop's busiest beat.
      const bootyStartSec = OPENING_PREFIX_SEC + barSec * 26;
      const bootyDurSec = booty.length / SAMPLE_RATE;
      events.vox.push({ t: bootyStartSec, name: "booty", dur: bootyDurSec });
      const bootyGain = 0.85;
      const startIdx = Math.floor(bootyStartSec * SAMPLE_RATE);
      const fadeS = Math.floor(0.04 * SAMPLE_RATE);
      for (let i = 0; i < booty.length; i++) {
        const j = startIdx + i;
        if (j < 0 || j >= sfxDryBuf.length) break;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > booty.length - fadeS) fade = Math.max(0, (booty.length - i) / fadeS);
        sfxDryBuf[j] += booty[i] * norm * bootyGain * fade;
      }
      try { unlinkSync(tmpRaw); } catch {}
      console.log(`  booty · sung melodic stem at ${bootyStartSec.toFixed(2)}s (drop1 bar 26)`);
    }
  }
}

// ── kitty meows (48-53 s) ───────────────────────────────────────────
// Cat samples from the ac-native zoo bank, scattered through break2.
// Skipped in chill mode — chill outdoor vibe stays clean without
// cat/bird zoo samples (which are positioned at narrative-specific
// timestamps that don't line up with the chill arrangement anyway).
if (!isChill) {
  // Cats pulled 5 s earlier than before and pitched HIGH — kittens
  // chirping rather than full-grown growls. Birds tweet through the
  // same window so they read as a chorus together.
  // Widely-varied pitches — from chunky low growl to tiny kitten
  // squeak (0.65× to 2.4×) so each meow reads as a distinct cat.
  const meows = [
    { offsetSec: 43.10 + OPENING_PREFIX_SEC, pitch: 1.55, gain: 0.30 },
    { offsetSec: 44.20 + OPENING_PREFIX_SEC, pitch: 2.40, gain: 0.22 },
    { offsetSec: 45.60 + OPENING_PREFIX_SEC, pitch: 0.78, gain: 0.34 },
    { offsetSec: 46.70 + OPENING_PREFIX_SEC, pitch: 1.95, gain: 0.24 },
    { offsetSec: 47.80 + OPENING_PREFIX_SEC, pitch: 0.65, gain: 0.32 },
  ];
  for (const m of meows) {
    mixZooSample(
      { startSec: m.offsetSec, name: "cat", gain: m.gain, pitchRatio: m.pitch },
      sfxDryBuf,
      SAMPLE_RATE
    );
    events.sfx.push({ t: m.offsetSec, name: "cat", dur: 0.35 });
  }
  // Bird tweets interspersed with the kittens — same window, varied
  // pitches so it reads as a chorus of small creatures, not echoes.
  const tweets = [
    { offsetSec: 43.50 + OPENING_PREFIX_SEC, pitch: 1.10, gain: 0.35 },
    { offsetSec: 44.80 + OPENING_PREFIX_SEC, pitch: 1.35, gain: 0.30 },
    { offsetSec: 46.10 + OPENING_PREFIX_SEC, pitch: 0.92, gain: 0.32 },
    { offsetSec: 47.30 + OPENING_PREFIX_SEC, pitch: 1.50, gain: 0.28 },
  ];
  for (const tw of tweets) {
    mixZooSample(
      { startSec: tw.offsetSec, name: "bird", gain: tw.gain, pitchRatio: tw.pitch },
      sfxDryBuf,
      SAMPLE_RATE
    );
    events.sfx.push({ t: tw.offsetSec, name: "bird", dur: 0.30 });
  }
}

// Chill cat CHOIR — over the roller-coaster window (bars 6–22) a
// HARMONIZED stack of long, time-stretched, autotuned meow drones
// (a minor-triad shape: root / m3 / 5th / octave at consonant ratios,
// so it reads in-tune with itself) spanning many bars — plus a few
// short high kitten accents on top so it still reads as cats.
if (isChill && barStartRel.length > 22) {
  const tA = OPENING_PREFIX_SEC + barStartRel[6];
  const tB = OPENING_PREFIX_SEC + barStartRel[22];
  const span = tB - tA;
  const r = 0.42; // base stretch/pitch — mellow low register
  const chord = [
    { ratio: r,                          gain: 0.075 }, // root
    { ratio: r * Math.pow(2, 3 / 12),    gain: 0.060 }, // minor 3rd
    { ratio: r * Math.pow(2, 7 / 12),    gain: 0.055 }, // 5th
    { ratio: r * 2,                      gain: 0.040 }, // octave shimmer
  ];
  chord.forEach((c, vi) => {
    const off = vi * (span * 0.04);                 // staggered bloom-in
    mixCatDrone(sfxDryBuf, tA + off, span - off, c.ratio, c.gain);
    events.sfx.push({ t: tA + off, name: "cat", dur: span - off });
  });
  // a few short high kitten accents so the choir still reads "cat"
  for (let k = 0; k < 4; k++) {
    const t = tA + span * ((k + 0.5) / 4) + (noiseRng() - 0.5) * (span * 0.15);
    mixZooSample({ startSec: t, name: "cat", gain: 0.13, pitchRatio: 1.9 + noiseRng() * 0.8 }, sfxDryBuf, SAMPLE_RATE);
    events.sfx.push({ t, name: "cat", dur: 0.35 });
  }
}

// Boot sequence — system chime → voice → bang → music:
//   0.05 s   ac-native boot melody (triangle C5→E5→G5)
//   0.75 s   greeting "good evening jeffrey. enjoy los angeles."
//   3.72 s   greeting ends
//   2.625 s  sniper BANG — snapped to 16th-note hihat tick 24 (= 2
//             bars at 137.143 bpm), lands during "Angeles" tail just
//             before music entry. Bumps earlier than the 8th-tick
//             position so the bang punches more inside the voice.
//   2.700 s  music + vocal enter
// Boot chime always plays — sets the AC voice. Sniper bang skipped
// in chill mode (no dramatic openings for study vibes).
// ── TYPING INTRO — someone types "trancenwaltz" into the AC prompt
//    with quick keystroke clicks, THEN the boot melody fires. The
//    visualizer (cover-video.mjs) draws the pink AC prompt block +
//    types the title char-by-char synced to these keyclick events.
//    12 clicks = the 12 letters of "trancenwaltz". Fast (~70 ms
//    apart). All inside the 2.7 s opening prefix — music entry and
//    the sniper (2.625 s) are unaffected.
const TYPE_N = 12;            // "trancenwaltz"
const TYPE_START = 0.18;
const TYPE_GAP = 0.072;       // ~14 keys/sec — quick typing
// --master (release WAV): DROP the typing keyclicks + enter thunk
// entirely — a clean streaming single. The boot / startup melody
// (fired below, ungated) is KEPT.
if (!isChill && !RELEASE_MASTER) {
  const kRng = makeRng(SEED_STR + ":keyclick");
  for (let i = 0; i < TYPE_N; i++) {
    const kt = TYPE_START + i * TYPE_GAP;
    // short bright tick, slight per-key pitch jitter so it reads as
    // mechanical-keyboard typing rather than one repeated blip.
    // LOWER-PITCHED tick — chunkier mechanical-keyboard thock.
    const tone = 620 + (kRng() - 0.5) * 240;
    const ks = makeBufferSynth(sfxDryBuf, kt, SAMPLE_RATE, noiseRng);
    ks.synth({ type: "square", tone, duration: 0.016, volume: 0.17, attack: 0.0009, decay: 0.014 });
    const ks2 = makeBufferSynth(sfxDryBuf, kt, SAMPLE_RATE, noiseRng);
    ks2.synth({ type: "triangle", tone: tone * 1.9, duration: 0.011, volume: 0.09, attack: 0.0006, decay: 0.010 });
    events.sfx.push({ t: kt, name: `keyclick-${i}`, dur: 0.05, point: true });
  }
  // ENTER — a chunkier, lower return-key thunk after the last letter,
  // a beat before the boot melody. Marks "command submitted".
  const enterT = TYPE_START + TYPE_N * TYPE_GAP + 0.07;
  const en1 = makeBufferSynth(sfxDryBuf, enterT, SAMPLE_RATE, noiseRng);
  en1.synth({ type: "square", tone: 300, duration: 0.045, volume: 0.22, attack: 0.001, decay: 0.04 });
  const en2 = makeBufferSynth(sfxDryBuf, enterT, SAMPLE_RATE, noiseRng);
  en2.synth({ type: "triangle", tone: 150, duration: 0.06, volume: 0.16, attack: 0.001, decay: 0.055 });
  events.sfx.push({ t: enterT, name: "prompt-enter", dur: 0.08, point: true });
}
// Boot melody fires AFTER the typing finishes (was 0.05 s — now
// gated behind the ~0.95 s typing burst). Still well before the
// sniper (2.625 s) and music entry (2.7 s).
// BOOT_SEC stays defined in chill — the non-chill pre-roll hats
// anchor on it — but the audible startup melody is skipped below.
const BOOT_SEC = isChill ? 0.05 : (TYPE_START + TYPE_N * TYPE_GAP + 0.13);
// Startup beeps — skipped in chill mode. The study mix has no
// system-boot narrative; it just begins on the music.
if (!isChill) {
  fireBootMelody(sfxDryBuf, BOOT_SEC, 1.0);
  // Boot chime — tagged as a point marker so the visualizer renders
  // it decorrelated from the boot-beeps waveform behind it. Three
  // chime tones C5→E5→G5 at ~0.4s each.
  events.sfx.push({ t: BOOT_SEC, name: "boot-chime", dur: 0.4, point: true });
}
const sixteenthSec_BOOT = (60 / BPM) / 4;
const sniperSec = 24 * sixteenthSec_BOOT;
if (!isChill) {
  fireSniper(sfxDryBuf, sniperSec, 2.4);
  events.sfx.push({ t: sniperSec, name: "sniper", dur: 0.12 });
}

// ── shutdown sequence — mirrors the boot, NOT tape-stopped ──────────
// Routes to shutdownBuf so the bye voice + chime + ending hats stay
// at full pitch while the music underneath spins down. Mirrors the
// opening: voice + chime + hats at the same wall-clock anchor on
// both ends of the track. Skipped in chill mode — no shutdown story,
// the chill outro is a natural fade.
if (!isChill) {
  // bye lands AFTER the tape-stop slowdown has already started (at
  // totalSec - 5.0), so the voice rides on top of the music's
  // already-slowing collapse — feels more like a system saying goodbye
  // as it dies rather than announcing the shutdown.
  const BYE_SEC             = totalSec - 3.4;
  const SHUTDOWN_MELODY_SEC = totalSec - 1.5;
  fireShutdownMelody(shutdownBuf, SHUTDOWN_MELODY_SEC, 0.65);
  events.sfx.push({ t: SHUTDOWN_MELODY_SEC, name: "shutdown-chime", dur: 0.4, point: true });
  events.vox.push({ t: BYE_SEC, name: "bye-jeffrey", dur: 1.6 });
  const BYE_PATH = `${REPO}/pop/dance/out/.ac-bye.mp3`;
  if (existsSync(BYE_PATH)) {
    const tmpBye = `${dirname(OUT_PATH)}/.ac-bye.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", BYE_PATH,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpBye,
    ]);
    if (dec.status === 0 && existsSync(tmpBye)) {
      const raw = readFileSync(tmpBye);
      const bye = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
      let bpeak = 0;
      for (let i = 0; i < bye.length; i++) { const a = Math.abs(bye[i]); if (a > bpeak) bpeak = a; }
      const norm = bpeak > 0 ? 0.85 / bpeak : 1.0;
      const startIdx = Math.floor(BYE_SEC * SAMPLE_RATE);
      const fadeS = Math.floor(0.05 * SAMPLE_RATE);
      for (let i = 0; i < bye.length; i++) {
        const j = startIdx + i;
        if (j < 0 || j >= shutdownBuf.length) break;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > bye.length - fadeS) fade = Math.max(0, (bye.length - i) / fadeS);
        shutdownBuf[j] += bye[i] * norm * fade;
      }
      try { unlinkSync(tmpBye); } catch {}
      console.log(`  shutdown · "bye jeffrey" at ${BYE_SEC.toFixed(2)}s + chime at ${SHUTDOWN_MELODY_SEC.toFixed(2)}s`);
    }
  }
}

// ── post-roll hihats — "system shutting down" ───────────────────────
// DECELERATE from 8th-note rate to a half-note crawl across the tail,
// with progressive timing jitter and amplitude quantization (mini
// bitcrush per hit) so the hats turn into "smushy bitcrunches" as
// the track dies. Skipped in chill mode — chill outro is just a
// natural decay, no system-shutdown narrative.
if (!isChill) {
  const eighthSec     = beatSec / 2;
  const halfNoteSec   = beatSec * 2;       // very slow tail rate
  const tailHatStart  = totalSec - 5.0;
  const tailHatEnd    = totalSec - 2.0; // earlier cutoff — no hat near the final beep
  let t = tailHatStart;
  while (t < tailHatEnd) {
    const f = (t - tailHatStart) / Math.max(0.001, tailHatEnd - tailHatStart);
    // Timing jitter grows quadratically across the tail.
    const jitter = (noiseRng() - 0.5) * 0.12 * f * f;
    // Amplitude bitcrush — quantize the hat's volume to fewer levels
    // as the track dies. f=0: 16 levels (clean). f=1: 2 levels (very
    // crushed).
    const levels = Math.max(2, Math.round(16 * (1 - f * 0.85)));
    const rawV = 0.32 + noiseRng() * 0.10;
    const crushedV = Math.round(rawV * levels) / levels;
    // Volume also tapers toward silence so the hats fade out.
    const ampEnv = 1 - f * 0.55;
    // randomly pitched around a bit per tick — the dying tail hats
    // wobble in pitch as they smush out.
    const hatPF = 1 + (noiseRng() - 0.5) * 0.36; // ~±2.8 semitones
    fireDrum(shutdownBuf, t + jitter, "g", { volume: crushedV * ampEnv * DRUM_GAIN, pitchFactor: hatPF });
    hatCount++;
    // Step DECELERATES from 8th → half-note across the tail (the
    // opposite of "taking off" — the system is winding down).
    const step = eighthSec + (halfNoteSec - eighthSec) * f * f;
    t += step;
  }
}

// ── pre-roll hihats — beeps that morph into noisy hats ──────────────
// Each tick fires TWO layers crossfading from beep → hat across an
// EXTENDED window that runs PAST the prefix into the first ~2 bars
// of music — beeps stay tonal longer before yielding to the hat
// spine. Quadratic beep weight (1-f)^2 keeps tonality dominant for
// roughly the first 70 % of the crossfade.
//
// Rate decelerates from 16th → 8th (the "coming in for a landing"
// curve).
//
// SKIPPED IN CHILL MODE — this is the startup "beep bop"
// sonification, and its hat layer overlaps the intro's own in-bar
// hats (which begin at music entry), producing an audible DOUBLED
// hi-hat at the top of the track. The chill study mix opens clean
// on the bed instead.
const BEEP_BLEND_END_SEC = OPENING_PREFIX_SEC + 2.3; // ≈ 5.0 s
if (!isChill) {
  const sixteenthSec = beatSec / 4;
  const eighthSec    = beatSec / 2;
  // Starts with the (moved) boot melody so the typing window stays
  // clean — just keyclicks — before the "beep bop" sonification.
  const firstHatSec = BOOT_SEC;
  const endHatSec = BEEP_BLEND_END_SEC;
  const winSec = endHatSec - firstHatSec;
  // Span entry for the whole boot-beep window — gives the SFX lane
  // a single waveform region covering the opening "beep bop" so the
  // timeline shows the startup sonification.
  events.sfx.push({ t: firstHatSec, name: "boot-beeps", dur: winSec });
  let t = firstHatSec;
  while (t < endHatSec) {
    const f = (t - firstHatSec) / Math.max(0.001, winSec); // 0 → 1
    // Quadratic beep curve — tonality persists longer before fading.
    const beepWeight = (1.0 - f) * (1.0 - f);
    const hatWeight  = f;
    // LOW tone climbs from 110 Hz → 220 Hz across the prefix. Kept
    // well below the boot melody's C5 (523 Hz) start so the system
    // beeps and the pre-roll beeps occupy different frequency bands
    // and don't compete. Dropped the harmonic partial that was
    // muddying the boot melody's low-mids.
    const beepTone = 110 + 110 * f;
    if (beepWeight > 0.01) {
      const beep = makeBufferSynth(dryBuf, t, SAMPLE_RATE, noiseRng);
      beep.synth({ type: "sine", tone: beepTone, duration: 0.040, volume: 0.20 * beepWeight, attack: 0.001, decay: 0.038 });
    }
    if (hatWeight > 0.01) {
      const v = 0.16 + noiseRng() * 0.06;
      // randomly pitched around a bit per tick so the fast intro hats
      // shimmer instead of being one repeated sample.
      const hatPF = 1 + (noiseRng() - 0.5) * 0.36; // ~±2.8 semitones
      fireDrum(dryBuf, t, "g", { volume: v * hatWeight * DRUM_GAIN, pitchFactor: hatPF });
      // Capture EACH pre-roll hat tick so the timeline shows the
      // beep→hat blend layer in the HAT lane during the intro,
      // not just the in-bar hats that start at bar 0.
      events.hat.push({ t });
      hatCount++;
    }
    // Step decelerates from 16th → 8th.
    const step = sixteenthSec + (eighthSec - sixteenthSec) * f;
    t += step;
  }
}

// ── ac-native boot greeting ("hi @jeffrey. welcome to los angeles.")
// Fires right after the opening sniper. Cached at:
//   pop/dance/out/.ac-greeting.mp3
// Generated by bin/say.mjs (jeffrey-pvc neutral voice, calm settings).
const GREETING_PATH = `${REPO}/pop/dance/out/.ac-greeting.mp3`;
if (!isChill && existsSync(GREETING_PATH)) {
  events.vox.push({ t: 0.05, name: "greeting", dur: 3.7 });
  const tmpGreetRaw = `${dirname(OUT_PATH)}/.ac-greeting.f32.raw`;
  const dec = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", GREETING_PATH,
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
    tmpGreetRaw,
  ]);
  if (dec.status === 0 && existsSync(tmpGreetRaw)) {
    const raw = readFileSync(tmpGreetRaw);
    const greet = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
    let gpeak = 0;
    for (let i = 0; i < greet.length; i++) { const a = Math.abs(greet[i]); if (a > gpeak) gpeak = a; }
    const norm = gpeak > 0 ? 0.85 / gpeak : 1.0;
    // Greeting + boot melody both start at the very top so the chime
    // and voice ring together — same wall-clock entry as an actual
    // ac-native boot where the C5→E5→G5 ascending chime plays
    // simultaneously with the time-of-day greeting.
    const greetStartSec = 0.05;
    const greetStartIdx = Math.floor(greetStartSec * SAMPLE_RATE);
    const fadeS = Math.floor(0.06 * SAMPLE_RATE);
    for (let i = 0; i < greet.length; i++) {
      const j = greetStartIdx + i;
      if (j < 0 || j >= sfxDryBuf.length) break;
      let fade = 1;
      if (i < fadeS) fade = i / fadeS;
      else if (i > greet.length - fadeS) fade = Math.max(0, (greet.length - i) / fadeS);
      sfxDryBuf[j] += greet[i] * norm * fade;
    }
    try { unlinkSync(tmpGreetRaw); } catch {}
    console.log(`  greeting · "good evening jeffrey. enjoy los angeles." (flite slt) at ${greetStartSec}s`);
  }
}

// (Bird tweets removed — the ending should kill cleanly with only the
// boot-melody shutdown chime as the final sound; no jungle texture
// bleeding into the cut.)

// (Pre-roll square/triangle arpeggio removed — was distracting in
// the boot intro. The per-bar arp now handles the arpeggio entirely
// from music bar 0 onward.)

// Low harp — sustained warm harmonic voice across the first 4 measures
// of the intro. Fills the rhythmic gap before kick/bells/lead enter
// without competing with the vocal arc. Each bar gets a new low note
// from the bar's chord; the previous note's tail bleeds into the
// next so the harp feels continuous.
let harpCount = 0;
{
  const harpBars = Math.min(4, TOTAL_BARS);
  for (let bar = 0; bar < harpBars; bar++) {
    const chordDeg = progressionAt(bar);
    // Low octave (-1) root, plus the chord 5th one octave below the
    // root for warmth. Both are below middle C so they sit under the
    // vocal without masking.
    const rootMidi  = scaleNoteMidi(chordDeg,     -1);
    const fifthMidi = scaleNoteMidi(chordDeg + 4, -2);
    mixEventHarp(
      { startSec: bar * barSec + OPENING_PREFIX_SEC, midi: rootMidi,  gain: 0.55, durSec: barSec * 1.4 },
      dryBuf,
      SAMPLE_RATE
    );
    mixEventHarp(
      { startSec: bar * barSec + OPENING_PREFIX_SEC, midi: fifthMidi, gain: 0.40, durSec: barSec * 1.4 },
      dryBuf,
      SAMPLE_RATE
    );
    harpCount += 2;
  }
}

console.log(`→ events · kick=${kickCount} hat=${hatCount} sub=${subCount} lead=${leadCount} pad=${padCount} saw=${supersawCount} piano=${pianoCount} bells=${bellsCount} harp=${harpCount} riser=${riserCount} snareRoll=${snareRollCount} mg=${machineGunCount} dropImpact=${dropImpactCount}`);

// ── supersaw flange ───────────────────────────────────────────────────
// Slower flange — the previous 6 Hz rate warbled too fast for the
// trance tempo. 2.5 Hz now gives a more languid jet-sweep that
// breathes with the music instead of fluttering through it.
applyFlange(sawBuf, {
  rate: 2.5,
  depthMs: 4.5,
  baseDelayMs: 5,
  feedback: 0.45,
  mix: 0.55,
  sampleRate: SAMPLE_RATE,
});
for (let i = 0; i < totalSamples; i++) duckBuf[i] += sawBuf[i];

// ── chill bell flanger ────────────────────────────────────────────────
// Chill mode bells live on chillBellBuf — slow LFO flanger here gives
// the low-tone drawn-out wash a hypnotic moving timbre.
if (isChill) {
  applyFlange(chillBellBuf, {
    rate: 0.18,        // very slow — a 5.5 s LFO period
    depthMs: 8.0,      // wide depth for noticeable flange motion
    baseDelayMs: 6,
    feedback: 0.55,
    mix: 0.65,
    sampleRate: SAMPLE_RATE,
  });
  // Mix into duckBuf so the chill bells share the sidechain pumping
  // with the pad — keeps them tucked underneath the kick.
  for (let i = 0; i < totalSamples; i++) duckBuf[i] += chillBellBuf[i];
}

// ── sidechain ─────────────────────────────────────────────────────────
if (SIDECHAIN && kickTimes.length > 0) {
  const recoverS = (DUCK_MS / 1000) * SAMPLE_RATE;
  const gain = new Float32Array(totalSamples);
  for (let i = 0; i < totalSamples; i++) gain[i] = 1;
  const attackS = Math.max(1, Math.floor(0.003 * SAMPLE_RATE));
  for (const kt of kickTimes) {
    const idx = Math.floor(kt * SAMPLE_RATE);
    for (let i = 0; i < attackS; i++) {
      const j = idx + i;
      if (j < 0 || j >= totalSamples) continue;
      const tt = i / attackS;
      const target = 1 - DUCK_DEPTH;
      gain[j] = gain[j] + (target - gain[j]) * tt;
    }
    for (let i = 0; i < recoverS; i++) {
      const j = idx + attackS + i;
      if (j < 0 || j >= totalSamples) continue;
      const tt = i / recoverS;
      const floor = 1 - DUCK_DEPTH;
      const ducked = floor + (1 - floor) * (1 - Math.exp(-4 * tt));
      if (ducked < gain[j]) continue;
      gain[j] = ducked;
    }
  }
  for (let i = 0; i < totalSamples; i++) duckBuf[i] *= gain[i];
}

// ── deep bass wobble (bass bus only) ─────────────────────────────────
// Slow LFO modulating a low-pass cutoff on the sub bass. Gives a
// dubstep-style "wub wub" without affecting the pad above it.
applyWobble(bassBuf, {
  target: "filter",
  baseCutoffHz: 480,
  rate:  [{ time: 0, rate: 1.2 }, { time: totalSec / 2, rate: 2.6 }, { time: totalSec, rate: 1.8 }],
  depth: [{ time: 0, depth: 0.65 }, { time: totalSec / 2, depth: 0.88 }, { time: totalSec, depth: 0.75 }],
  sampleRate: SAMPLE_RATE,
  startSec: 0,
  endSec: totalSec,
});

// ── per-section dynamic envelope ──────────────────────────────────────
// Apply a track-shape multiplier so the bed isn't slammed-to-1 all the
// way through. Intro fades up, breaks dip slightly, builds ramp into
// drops, drops at full, outro fades out. Gives a real build-up arc.
// More dramatic dynamic arc — quieter intro, big climb, drops can
// overshoot. Wider range than before so the track breathes.
const SECTION_LEVELS_NORMAL = {
  intro:  [0.12, 0.38],
  break1: [0.38, 0.58],
  build1: [0.58, 1.20],
  drop1:  [1.20, 1.15],
  break2: [0.30, 0.42],
  build2: [0.42, 1.35],
  drop2:  [1.35, 1.20],
  outro:  [1.20, 0.75],
};
// CHILL — FLAT dynamic arc. Range 0.55-0.95 instead of 0.12-1.35 so
// the track stays in study-vibes territory throughout. No big drops,
// no deep breaks. Sections still have gentle contour for interest.
const SECTION_LEVELS_CHILL = {
  intro:  [0.55, 0.70],
  break1: [0.70, 0.78],
  build1: [0.78, 0.88],
  drop1:  [0.88, 0.92],
  break2: [0.70, 0.78],
  build2: [0.78, 0.88],
  drop2:  [0.88, 0.95],
  outro:  [0.85, 0.55],
};
const SECTION_LEVELS = isChill ? SECTION_LEVELS_CHILL : SECTION_LEVELS_NORMAL;
const dynEnv = new Float32Array(totalSamples);
for (let i = 0; i < totalSamples; i++) dynEnv[i] = 1;
for (const r of sectionRanges) {
  const [g0, g1] = SECTION_LEVELS[r.name] || [1, 1];
  const startIdx = Math.floor(r.startSec * SAMPLE_RATE);
  const endIdx = Math.floor(r.endSec * SAMPLE_RATE);
  // Build sections use an exponential climb (slow start, fast end)
  // so the crescendo psychoacoustically maps to the perception of a
  // build — drama LATE, not a flat ramp the whole way. Other
  // sections use linear interpolation as before.
  const isBuild = r.name.startsWith("build");
  for (let i = startIdx; i < endIdx && i < totalSamples; i++) {
    const t = (i - startIdx) / Math.max(1, endIdx - startIdx);
    const shapedT = isBuild ? Math.pow(t, 2.4) : t; // ease-in for builds
    dynEnv[i] = g0 + (g1 - g0) * shapedT;
  }
}

// ── unified tape-stop ending — ONE clock dialing down ───────────────
// The whole mix decelerates together as if a single master clock is
// being turned down. Same pitch-curve applied to every instrument
// bus so the slowdown reads as cohesive (a tape spinning down).
// Skipped in chill mode — no system-shutdown narrative, the outro
// just decays naturally.
//
// EVENT REMAPPING — captured events in the tail window are pushed
// to a NEW displayed time so the visualizer scatters/decelerates
// them in sync with the audio. The pitch curve is END_PITCH^f =
// 0.20^f over the 5 s tail. An event at source-tail-offset s
// (0..2.487 s of source) is heard at output-tail-offset:
//   f = ln(1 + s · ln(END_PITCH) / TAIL) / ln(END_PITCH)
// (clamped to 1.0 — anything past max coverage is never heard).
if (!isChill) {
  const TAIL_BUFSEC_REMAP = 5.0;
  const END_PITCH_REMAP = 0.20;
  const tailStart = totalSec - TAIL_BUFSEC_REMAP;
  const lnEnd = Math.log(END_PITCH_REMAP);
  const maxSrcCoverage = TAIL_BUFSEC_REMAP * (END_PITCH_REMAP - 1) / lnEnd;
  function remapInTail(evt) {
    if (evt.t < tailStart) return;
    const s = evt.t - tailStart;
    if (s > maxSrcCoverage) {
      evt.skipped = true;
      return;
    }
    const f = Math.log(1 + (s * lnEnd) / TAIL_BUFSEC_REMAP) / lnEnd;
    evt.displayedT = tailStart + TAIL_BUFSEC_REMAP * Math.min(1, Math.max(0, f));
  }
  for (const k of Object.keys(events)) {
    for (const evt of events[k]) remapInTail(evt);
  }
  // Add a tape-stop waveform event to the SFX lane so the visualizer
  // shows the slowdown audio as a 5 s waveform region.
  events.sfx.push({ t: tailStart, name: "tape-stop", dur: TAIL_BUFSEC_REMAP });
}
if (!isChill) {
  const TAIL_BUFSEC = 5.0;
  const tailStartIdx = Math.floor((totalSec - TAIL_BUFSEC) * SAMPLE_RATE);
  const tailEndIdx = Math.floor(totalSec * SAMPLE_RATE);
  // ONE master pitch curve — exponential from 1.0× → 0.20× over the
  // 5-second tail. Every bus gets the same curve so they all dial
  // down together. The amplitude fade is also shared so the whole
  // mix collapses to silence as a single entity.
  const END_PITCH = 0.20;
  function tapeStopBus(buf) {
    if (!buf || tailEndIdx <= tailStartIdx) return;
    const len = tailEndIdx - tailStartIdx;
    const src = buf.slice(tailStartIdx, tailEndIdx);
    let srcPos = 0;
    for (let i = 0; i < len; i++) {
      const f = i / len;
      const pitch = Math.pow(END_PITCH, f); // shared 1.0 → 0.20 curve
      srcPos += pitch;
      const s0 = Math.floor(srcPos);
      const s1 = s0 + 1;
      let sample = 0;
      if (s1 < src.length) {
        const frac = srcPos - s0;
        sample = src[s0] * (1 - frac) + src[s1] * frac;
      }
      // Shared amplitude fade — kicks in after the 60% mark so the
      // music has weight up until the very last second.
      const amp = f < 0.6 ? 1 : (1 - f) / 0.4;
      buf[tailStartIdx + i] = sample * amp;
    }
  }
  tapeStopBus(bassBuf);
  tapeStopBus(duckBuf);
  tapeStopBus(dryBuf);
  tapeStopBus(mgBuf);
  tapeStopBus(sfxDryBuf);
  console.log(`  tape-stop · unified one-clock slowdown last ${TAIL_BUFSEC}s (1.0× → ${END_PITCH}×)`);
}

// ── sum buses (with dynamic envelope applied to the BED only — the
// machine-gun bus stays loud regardless of section level, and is
// added in AFTER the per-section fx loop so wobble/bitcrush don't
// crush the gun depth) ────────────────────────────────────────────
const out = new Float32Array(totalSamples);
for (let i = 0; i < totalSamples; i++) {
  out[i] = (dryBuf[i] + duckBuf[i] + bassBuf[i]) * dynEnv[i];
}

// ── pre-drop hush ─────────────────────────────────────────────────────
// Gentle dip across 500 ms before each drop using a single smooth
// cosine ease. mgBuf (drop impact + lion + ding) bypasses this duck.
// Skipped in chill mode — chill has no drops, no hush.
if (!isChill) {
  const HUSH_MS = 500;
  const HUSH_SAMPLES = Math.floor((HUSH_MS / 1000) * SAMPLE_RATE);
  for (const r of sectionRanges) {
    if (!r.name.startsWith("drop")) continue;
    const dropIdx = Math.floor(r.startSec * SAMPLE_RATE);
    for (let i = 0; i < HUSH_SAMPLES; i++) {
      const j = dropIdx - HUSH_SAMPLES + i;
      if (j < 0 || j >= out.length) continue;
      const t = i / HUSH_SAMPLES;
      // Smooth cosine ease — continuous gentle dip from 1.0 → 0.30
      // across the whole 500 ms window. No flat segment, no plunge.
      const duck = 0.30 + 0.35 * (1 + Math.cos(Math.PI * t));
      out[j] *= duck;
    }
  }
}

// ── per-section fx envelopes ──────────────────────────────────────────
for (const r of sectionRanges) {
  const fx = r.template.fx;
  if (!fx) continue;
  if (fx.wobble) {
    applyWobble(out, {
      ...fx.wobble,
      sampleRate: SAMPLE_RATE,
      startSec: r.startSec,
      endSec: r.endSec,
    });
  }
  if (fx.bitcrush) {
    applyBitcrush(out, {
      ...fx.bitcrush,
      sampleRate: SAMPLE_RATE,
      startSec: r.startSec,
      endSec: r.endSec,
    });
  }
}

// ── add machine-gun / SFX bus AFTER per-section fx ────────────────────
// Bitcrush would otherwise crunch the gun depth into a thin click. By
// adding mgBuf here, the guns stay full-bandwidth and louder than the
// bed even during bitcrushed drops.
//
// Distance treatment — gives the guns the "far-away" feel they have on
// AC native: gentle one-pole lowpass to roll off the harsh top end +
// short pre-delay reverb tap that trails each bullet. Without these
// the guns sit too close in your face.
{
  // 6.5 kHz one-pole lowpass — kill the snare-snap brightness.
  const cutoffHz = 6500;
  const rc = 1 / (2 * Math.PI * cutoffHz);
  const alpha = (1 / SAMPLE_RATE) / (rc + 1 / SAMPLE_RATE);
  let lp = 0;
  for (let i = 0; i < totalSamples; i++) {
    lp += alpha * (mgBuf[i] - lp);
    mgBuf[i] = lp;
  }
  // Short echo tail — 110ms + 240ms feedback taps, lower-bandwidth.
  // Adds spatial depth so the guns feel like they're echoing across a
  // room rather than right at your ear.
  const tapMs = [110, 240, 420];
  const tapGain = [0.42, 0.26, 0.14];
  for (let k = 0; k < tapMs.length; k++) {
    const tapSamples = Math.floor((tapMs[k] / 1000) * SAMPLE_RATE);
    for (let i = totalSamples - 1; i >= tapSamples; i--) {
      mgBuf[i] += mgBuf[i - tapSamples] * tapGain[k];
    }
  }
}
for (let i = 0; i < totalSamples; i++) {
  out[i] += mgBuf[i] * 0.65; // distant — pulled back further
}

// ── add dry SFX bus (opening sniper) — no distance reverb ─────────────
for (let i = 0; i < totalSamples; i++) {
  out[i] += sfxDryBuf[i];
}

// ── add shutdown bus (bye + chime + tail hats) — NOT tape-stopped ─────
// shutdownBuf was filled after the per-bus tape-stop pass so its
// contents stay at full pitch while the music collapses underneath.
// This is what gives the ending its symmetry with the opening.
for (let i = 0; i < totalSamples; i++) {
  out[i] += shutdownBuf[i];
}

// ── mix vocal stem if provided ────────────────────────────────────────
// VOCAL_MODE controls placement:
//   "single" — once at start of drop1
//   "drops"  — at start of drop1 + drop2 + start of break1 (anthem path)
//   "all"    — at start of every section with vocal:true in template
// Vocal duck: lift surrounding mix slightly to make the vocal poke
// through (small +amp duck of the bed where the vocal lives is faked
// with a higher vocal gain — keeps the bed engine intact).
if (!isChill && VOCAL_STEM && existsSync(VOCAL_STEM)) {
  console.log(`→ mixing vocal stem · ${VOCAL_STEM} · mode=${VOCAL_MODE}`);
  const tmpRaw = `${dirname(OUT_PATH)}/.vocal-stem.f32.raw`;
  const dec = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", VOCAL_STEM,
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
    tmpRaw,
  ]);
  if (dec.status === 0 && existsSync(tmpRaw)) {
    const raw = readFileSync(tmpRaw);
    const vocalSrc = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);

    // Peak-normalize the vocal stem to 0.95. /api/say output is famously
    // quiet (~-19 dBFS peak) which loses against the supersaw bed even
    // at high VOCAL_GAIN. Normalize first → gain becomes meaningful.
    let vocalPeak = 0;
    for (let i = 0; i < vocalSrc.length; i++) { const a = Math.abs(vocalSrc[i]); if (a > vocalPeak) vocalPeak = a; }
    const vocal = new Float32Array(vocalSrc.length);
    if (vocalPeak > 0) {
      const norm = 0.95 / vocalPeak;
      for (let i = 0; i < vocalSrc.length; i++) vocal[i] = vocalSrc[i] * norm;
      console.log(`  vocal · stem peak ${(20 * Math.log10(vocalPeak)).toFixed(1)} dBFS → normalized to -0.4 dBFS`);
    } else {
      vocal.set(vocalSrc);
    }

    // Pick anchor times based on vocal mode.
    let anchorSecs = [];
    const vocalDurSec = vocal.length / SAMPLE_RATE;
    if (VOCAL_MODE === "loop") {
      // Tile the stem across the music section of the track, starting
      // AT the opening prefix so the greeting plays in silence and
      // the sung vocal only kicks in when the music does.
      const stride = Math.max(0.1, vocalDurSec - 0.3);
      for (let t = OPENING_PREFIX_SEC; t < totalSec; t += stride) anchorSecs.push(t);
    } else if (VOCAL_MODE === "single") {
      const drop = sectionRanges.find((r) => r.name === "drop1");
      if (drop) anchorSecs = [drop.startSec];
    } else if (VOCAL_MODE === "drops") {
      for (const r of sectionRanges) {
        if (r.name === "drop1" || r.name === "drop2") anchorSecs.push(r.startSec);
        if (r.name === "break1") anchorSecs.push(r.startSec + barSec * 4);
      }
    } else { // "all"
      for (const r of sectionRanges) {
        if (r.template.vocal) anchorSecs.push(r.startSec);
      }
    }

    // Step 1: duck the bed under each vocal placement.
    // In "loop" mode the vocal is continuous → duck applies for the
    // whole track. A deep duck (0.32-0.6) would crush the bed
    // permanently. Use a light, constant duck (0.85) for loop mode;
    // per-placement deep duck for the other modes.
    const fadeS    = Math.floor(0.20 * SAMPLE_RATE); // 200ms fade so loop crossovers don't snap
    if (VOCAL_MODE === "loop") {
      const LOOP_DUCK = Math.max(VOCAL_DUCK, 0.85);
      for (let j = 0; j < out.length; j++) out[j] *= LOOP_DUCK;
    } else {
      const duckSlope = Math.floor(0.06 * SAMPLE_RATE);
      const VOCAL_DUCK_FLOOR = VOCAL_DUCK;
      for (const ts of anchorSecs) {
        const baseIdx = Math.floor(ts * SAMPLE_RATE);
        const endIdx  = baseIdx + vocal.length;
        for (let j = Math.max(0, baseIdx - duckSlope); j < Math.min(out.length, endIdx + duckSlope); j++) {
          let g = VOCAL_DUCK_FLOOR;
          if (j < baseIdx) {
            const t = (baseIdx - j) / duckSlope;
            g = VOCAL_DUCK_FLOOR + (1 - VOCAL_DUCK_FLOOR) * t;
          } else if (j > endIdx) {
            const t = (j - endIdx) / duckSlope;
            g = VOCAL_DUCK_FLOOR + (1 - VOCAL_DUCK_FLOOR) * t;
          }
          if (g < 1) out[j] *= g;
        }
      }
    }

    // Build a per-sample vocal section arc — quiet in intro/breaks,
    // full in drops, silent in builds/outro. Crossfade across section
    // boundaries so the level changes are inaudible.
    const vocalArc = new Float32Array(out.length);
    // Default to 0 in the opening prefix so any vocal mix-in there
    // gets silenced — the greeting owns the first OPENING_PREFIX_SEC.
    const prefixEndIdx = Math.floor(OPENING_PREFIX_SEC * SAMPLE_RATE);
    for (let i = 0; i < out.length; i++) vocalArc[i] = i < prefixEndIdx ? 0 : 1;
    for (const r of sectionRanges) {
      const g = VOCAL_SECTION_GAIN[r.name] ?? 1;
      const startIdx = Math.floor(r.startSec * SAMPLE_RATE);
      const endIdx   = Math.floor(r.endSec   * SAMPLE_RATE);
      for (let i = startIdx; i < endIdx && i < out.length; i++) vocalArc[i] = g;
    }
    // 250 ms boxcar smoothing across section boundaries.
    const arcSmooth = Math.max(1, Math.floor(0.125 * SAMPLE_RATE));
    {
      const src = Float32Array.from(vocalArc);
      let acc = 0;
      const W = arcSmooth * 2 + 1;
      for (let i = 0; i < arcSmooth && i < src.length; i++) acc += src[i];
      for (let i = 0; i < src.length; i++) {
        const add = i + arcSmooth < src.length ? src[i + arcSmooth] : src[src.length - 1];
        const sub = i - arcSmooth - 1 >= 0 ? src[i - arcSmooth - 1] : src[0];
        acc += add - sub;
        vocalArc[i] = acc / W;
      }
    }

    // Step 2: mix the vocal in with short fades so repeats don't pop.
    let vocalMixCount = 0;
    for (const ts of anchorSecs) {
      const baseIdx = Math.floor(ts * SAMPLE_RATE);
      for (let i = 0; i < vocal.length; i++) {
        const j = baseIdx + i;
        if (j < 0 || j >= out.length) continue;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > vocal.length - fadeS) fade = Math.max(0, (vocal.length - i) / fadeS);
        out[j] += vocal[i] * VOCAL_GAIN * fade * vocalArc[j];
      }
      events.vox.push({ t: ts, name: "vocal", dur: vocalDurSec });
      vocalMixCount++;
    }

    // ── chopped & screwed at drops ────────────────────────────────────
    // At DROP1 only: layer a DJ-Screw style slowed + pitched-down
    // version of a vocal slice + a couple of quiet stutter-chops snapped
    // to the 16th-note grid. Drop2 is intentionally cleaner — its
    // entry reads as one event, with the held gunfire as the only
    // sustained texture (per user feedback that drop2 felt like "two
    // drops" at the 1:00 mark).
    for (const r of sectionRanges) {
      if (r.name !== "drop1") continue;
      const dropIdx = Math.floor(r.startSec * SAMPLE_RATE);

      // Source slice — read from a "memorable" position in the vocal
      // (3.5s in — usually after the first phrase has settled).
      const srcStartSec = Math.min(3.5, (vocal.length / SAMPLE_RATE) * 0.3);
      const srcStartIdx = Math.floor(srcStartSec * SAMPLE_RATE);
      const screwDurSec = 1.5; // 1.5s slowed → reads 0.75s of vocal
      const screwOutSamples = Math.floor(screwDurSec * SAMPLE_RATE);
      const screwFadeS = Math.floor(0.06 * SAMPLE_RATE);

      // Screwed (pitched-down by 1 octave via 2× duration via linear
      // resample) — kicks in at the drop entry.
      for (let i = 0; i < screwOutSamples; i++) {
        const j = dropIdx + i;
        if (j < 0 || j >= out.length) continue;
        const srcF = srcStartIdx + i * 0.5;
        const s0 = Math.floor(srcF);
        const s1 = s0 + 1;
        if (s1 >= vocal.length) break;
        const frac = srcF - s0;
        const sample = vocal[s0] * (1 - frac) + vocal[s1] * frac;
        let fade = 1;
        if (i < screwFadeS) fade = i / screwFadeS;
        else if (i > screwOutSamples - screwFadeS) fade = Math.max(0, (screwOutSamples - i) / screwFadeS);
        out[j] += sample * VOCAL_GAIN * 0.65 * fade;
      }

      // Chop stutter — TWO quiet triplet chops snapped to the 16th-note
      // grid relative to the drop entry. Previous 4-chop pattern at
      // arbitrary intervals registered as a second drop, especially at
      // the 1-minute drop2 entry. Now they're closer to a tasteful fill
      // than a separate drop event.
      const sixteenthSec = (60 / BPM) / 4;
      const chopSliceSamples = Math.floor(0.18 * SAMPLE_RATE);
      const chopStartIdx = dropIdx + screwOutSamples;
      const chopCount = 2;
      for (let c = 0; c < chopCount; c++) {
        // Snap each chop to the nearest 16th-note position.
        const chopOutIdx = chopStartIdx + Math.floor(c * sixteenthSec * SAMPLE_RATE);
        for (let i = 0; i < chopSliceSamples; i++) {
          const j = chopOutIdx + i;
          if (j < 0 || j >= out.length) continue;
          const src = srcStartIdx + Math.floor(chopSliceSamples * 0.5) + i;
          if (src >= vocal.length) break;
          let fade = 1;
          const eIn = Math.floor(0.005 * SAMPLE_RATE);
          const eOut = Math.floor(0.020 * SAMPLE_RATE);
          if (i < eIn) fade = i / eIn;
          else if (i > chopSliceSamples - eOut) fade = Math.max(0, (chopSliceSamples - i) / eOut);
          // Quieter chops — they're an ornament, not a re-entry.
          const chopGain = 0.30 - c * 0.10;
          out[j] += vocal[src] * VOCAL_GAIN * chopGain * fade;
        }
      }
    }
    try { unlinkSync(tmpRaw); } catch {}
    console.log(`  vocal · ${vocalMixCount} placements · gain=${VOCAL_GAIN}`);
  } else {
    console.warn(`✗ vocal decode failed — skipping`);
  }
}

// ── screamed build vocals ────────────────────────────────────────────
// Adds the screamed command phrases ("turn around", "look up", "look
// down", "look in circles", "stare in spirals") on top of each build
// section. Raw stem cached at pop/dance/out/.trance-screams.mp3.
//
// MELODIC PREPROCESSING — each build gets its own melodic stem,
// rendered to EXACTLY that build's duration so the 12 ascending notes
// stretch evenly across the full build (not just the first few
// seconds). Cache key includes build duration so build1 (7.83s) and
// build2 (3.91s) each get their own pre-rendered file.
//
// Pitch+time decoupling uses ffmpeg's asetrate + aresample + atempo
// chain — pitch shift via asetrate, time-stretch via atempo, the
// aresample brings the rate back to SR after the asetrate.
const SCREAMS_RAW_PATH = `${REPO}/pop/dance/out/.trance-screams.mp3`;
// 3 notes climbing — each held very long so consonants smear into
// sustained vowel tones (the "SUNG" quality the user wants). With
// build1 = 7.87 s split 3 ways, each note holds ~2.6 s — well into
// sustained-pitch territory. Pitch ratios stay ≤ 2× so atempo math
// is clean (no squeegee).
const SCREAMS_CLIMB_SEMIS = [0, 7, 12];
const SCREAMS_RAW_DUR_SEC = 2.229116; // measured length of the raw stem
                                       // ("i fucked up, i made a sound. still around.")
function buildMelodicScreamsStem(targetDurSec) {
  const cachePath = `${REPO}/pop/dance/out/.trance-screams-melodic-${targetDurSec.toFixed(2)}s.mp3`;
  if (existsSync(cachePath)) return cachePath;
  if (!existsSync(SCREAMS_RAW_PATH)) return null;
  const N = SCREAMS_CLIMB_SEMIS.length;
  const inputSliceSec  = SCREAMS_RAW_DUR_SEC / N;
  const outputSliceSec = targetDurSec / N;
  const filterParts = [];
  for (let i = 0; i < N; i++) {
    const semis = SCREAMS_CLIMB_SEMIS[i];
    const pitchRatio = Math.pow(2, semis / 12);
    // After asetrate=SR*pitchRatio: slice duration = inputSliceSec / pitchRatio
    // After atempo=A: slice duration = inputSliceSec / (pitchRatio * A)
    // We want output = outputSliceSec → A = inputSliceSec / (pitchRatio * outputSliceSec)
    let atempo = inputSliceSec / (pitchRatio * outputSliceSec);
    // atempo only supports 0.5..100 per filter; chain multiple if needed.
    const atempoChain = [];
    while (atempo < 0.5) { atempoChain.push("0.5"); atempo /= 0.5; }
    while (atempo > 100) { atempoChain.push("2");   atempo /= 2;   }
    atempoChain.push(atempo.toFixed(4));
    const start = (i * inputSliceSec).toFixed(4);
    const end   = ((i + 1) * inputSliceSec).toFixed(4);
    // Per-slice chain:
    //   atrim   →  slice
    //   asetrate + aresample  →  pitch shift
    //   atempo  →  time stretch
    //   agate   →  dry out the natural ElevenLabs verb tail between
    //              words (fast attack/release, -28 dB threshold)
    //   equalizer 1.8kHz +5dB  →  emphasize the whistle band
    //   equalizer 280Hz -3dB   →  thin out chest resonance for a
    //                             more "whistle-like" tonal character
    filterParts.push(
      `[0:a]atrim=${start}:${end},asetrate=${SAMPLE_RATE}*${pitchRatio.toFixed(4)},aresample=${SAMPLE_RATE},${atempoChain.map((a) => `atempo=${a}`).join(",")},agate=threshold=-28dB:ratio=6:attack=8:release=120,equalizer=f=1800:t=q:w=1.2:g=5,equalizer=f=280:t=q:w=1.0:g=-3[s${i}]`
    );
  }
  const concatInputs = Array.from({ length: N }, (_, i) => `[s${i}]`).join("");
  const fullFilter = [...filterParts, `${concatInputs}concat=n=${N}:v=0:a=1[out]`].join(";");
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", SCREAMS_RAW_PATH,
    "-filter_complex", fullFilter,
    "-map", "[out]",
    "-c:a", "libmp3lame", "-q:a", "3",
    cachePath,
  ]);
  if (r.status === 0) {
    console.log(`  screams · melodic stem rendered (${N} notes → ${targetDurSec.toFixed(2)}s)`);
    return cachePath;
  }
  console.warn(`✗ melodic screams render failed at ${targetDurSec.toFixed(2)}s`);
  return null;
}

// Render a melodic stem for each unique build duration up front so the
// mix step can decode them as needed.
const SCREAMS_PER_BUILD = new Map(); // sectionName → path
// Gated by !isChill: the chill mix never mixes screams (see below), so
// don't waste an ffmpeg pass pre-rendering a stem it won't use.
if (!isChill && existsSync(SCREAMS_RAW_PATH)) {
  for (const r of sectionRanges) {
    if (!r.name.startsWith("build")) continue;
    const dur = r.endSec - r.startSec;
    const path = buildMelodicScreamsStem(dur);
    if (path) SCREAMS_PER_BUILD.set(r.name, path);
  }
}
// Decode each build's pre-rendered melodic stem and mix at its
// position. Each build has its own stem (already stretched/pitched to
// fit its exact duration), so the 12 ascending notes always span the
// FULL build — no compressed-to-first-few-seconds bug.
if (!isChill && SCREAMS_PER_BUILD.size > 0) {
  let screamCount = 0;
  const screamGain = 0.55;
  const screamFadeS = Math.floor(0.05 * SAMPLE_RATE);
  for (const r of sectionRanges) {
    if (!r.name.startsWith("build")) continue;
    const stemPath = SCREAMS_PER_BUILD.get(r.name);
    if (!stemPath || !existsSync(stemPath)) continue;
    const tmpScreams = `${dirname(OUT_PATH)}/.screams-${r.name}.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", stemPath,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpScreams,
    ]);
    if (dec.status !== 0 || !existsSync(tmpScreams)) continue;
    const raw = readFileSync(tmpScreams);
    const screamsSrc = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
    let peak = 0;
    for (let i = 0; i < screamsSrc.length; i++) { const a = Math.abs(screamsSrc[i]); if (a > peak) peak = a; }
    const screams = new Float32Array(screamsSrc.length);
    if (peak > 0) {
      const norm = 0.95 / peak;
      for (let i = 0; i < screamsSrc.length; i++) screams[i] = screamsSrc[i] * norm;
    } else screams.set(screamsSrc);
    const outStartIdx = Math.floor(r.startSec * SAMPLE_RATE);
    const buildSamples = Math.floor((r.endSec - r.startSec) * SAMPLE_RATE);
    const outSamples = Math.min(screams.length, buildSamples);
    events.vox.push({ t: r.startSec, name: `screams-${r.name}`, dur: outSamples / SAMPLE_RATE });
    for (let i = 0; i < outSamples; i++) {
      const j = outStartIdx + i;
      if (j < 0 || j >= out.length) continue;
      let fade = 1;
      if (i < screamFadeS) fade = i / screamFadeS;
      else if (i > outSamples - screamFadeS) fade = Math.max(0, (outSamples - i) / screamFadeS);
      out[j] += screams[i] * screamGain * fade;
    }
    try { unlinkSync(tmpScreams); } catch {}
    screamCount++;
  }
  console.log(`  screams · ${screamCount} build placements · gain=${screamGain}`);
}

// ── master mix-down — proper headroom for the ffmpeg chain ───────────
// Many voices stack here (lead + pad + supersaw + bass + bells + harp +
// drop-impact + machine guns + vocal). Pre-mix peaks routinely hit 3-5×
// full-scale; the previous softClip(1.15) tanh-saturated everything
// above 1.15 hard, which sounded like clipping rather than glue.
//
// New strategy: measure peak, scale to a sensible pre-master headroom
// (0.85), then soft-clip with a low threshold so only the very rare
// over-target peaks get gentle saturation. The ffmpeg chain that
// follows runs a proper compressor + alimiter brick wall so this stage
// only needs to deliver clean audio in the right ballpark.
{
  let peakIn = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peakIn) peakIn = a; }
  if (peakIn > 0) {
    // Lower pre-master headroom — house-style mixes prioritize air
    // and dynamics over loudness; the alimiter + loudnorm chain
    // brings the perceived level back without crushing transients.
    const target = 0.72;
    const gain = target / peakIn;
    for (let i = 0; i < out.length; i++) out[i] *= gain;
    console.log(`  master · peak ${(20 * Math.log10(peakIn)).toFixed(1)} dBFS → -${(-20 * Math.log10(target)).toFixed(1)} dBFS`);
  }
  // Catch the very rare overshoot from the wobble/bitcrush stages.
  softClip(out, 0.95);
}


// ── write structure JSON (for diagram + downstream tooling) ───────────
const structJson = {
  meter: METER,
  bpm: BPM,
  scale: SCALE_NAME,
  rootMidi: ROOT_MIDI,
  totalBars: TOTAL_BARS,
  totalSec,
  sections: sectionRanges.map((r) => ({
    name: r.name,
    startBar: r.startBar,
    endBar: r.endBar,
    startSec: r.startSec,
    endSec: r.endSec,
    layers: {
      kick: !!r.template.kick,
      hat: !!r.template.hat,
      sub: !!r.template.sub,
      pad: !!r.template.pad,
      lead: !!r.template.lead,
      piano: !!r.template.piano,
      bells: !!r.template.bells,
      riser: !!r.template.riser,
      snareRoll: !!r.template.snareRoll,
      supersaw: !!r.template.supersaw,
      vocal: !!r.template.vocal,
    },
    fx: r.template.fx ? Object.keys(r.template.fx) : [],
  })),
  counts: { kickCount, hatCount, subCount, leadCount, padCount, pianoCount, bellsCount, riserCount, snareRollCount, supersawCount, dropImpactCount, harpCount },
  // Per-note events for the scrolling-score video visualizer.
  events,
};
// Tuck intermediate files (struct.json, etc.) under a `<basename>.assets/`
// subfolder next to the final mp3 so the desktop stays uncluttered.
const baseStem = OUT_PATH.replace(/\.mp3$/, "");
const assetsDir = `${baseStem}.assets`;
const defaultStruct = `${assetsDir}/struct.json`;
const finalStructPath = STRUCT_PATH || defaultStruct;
mkdirSync(dirname(finalStructPath), { recursive: true });
writeFileSync(finalStructPath, JSON.stringify(structJson, null, 2));
console.log(`→ struct · ${finalStructPath}`);

// ── write mp3 ─────────────────────────────────────────────────────────
const outDir = dirname(OUT_PATH);
mkdirSync(outDir, { recursive: true });
const rawPath = `${outDir}/.trance-${SEED_STR}.f32.raw`;

// ── AC voice stamp (release master only) ───────────────────────────
// High-pitched "aesthetic dot computer" branding ID dropped mid-track
// (~13 s, just after the music enters). Deterministic seed → re-cutting
// --master reproduces the exact same take with just this added.
if (RELEASE_MASTER) {
  const STAMP_PATH = `${REPO}/pop/dance/out/.ac-dot-stamp-vocal.mp3`;
  if (existsSync(STAMP_PATH)) {
    const tmpStamp = `${outDir}/.ac-dot-stamp.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", STAMP_PATH,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpStamp,
    ]);
    if (dec.status === 0 && existsSync(tmpStamp)) {
      const raw = readFileSync(tmpStamp);
      const st = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
      let speak = 0;
      for (let i = 0; i < st.length; i++) { const a = Math.abs(st[i]); if (a > speak) speak = a; }
      const norm = speak > 0 ? 0.85 / speak : 1.0;
      // Chill: stretch it WAY out — slow + low (PITCH < 1 lengthens the
      // playback and drops the pitch). Non-chill keeps the quick high ID.
      const PITCH = isChill ? 0.55 : 1.6;
      const STAMP_SEC = 58.0;            // early in the build2 climb into drop2
      const startIdx = Math.floor(STAMP_SEC * SAMPLE_RATE);
      const outLen = Math.floor(st.length / PITCH);
      const fadeS = Math.floor(0.03 * SAMPLE_RATE);
      const GAIN = 0.18;                 // quiet — a subtle whisper ID
      for (let i = 0; i < outLen; i++) {
        const j = startIdx + i;
        if (j < 0 || j >= out.length) break;
        const s = st[Math.floor(i * PITCH)] || 0;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > outLen - fadeS) fade = Math.max(0, (outLen - i) / fadeS);
        out[j] += s * norm * GAIN * fade;
      }
      try { unlinkSync(tmpStamp); } catch {}
      console.log(`  stamp · "aesthetic dot computer" (${isChill ? "stretched slow+low" : "high-pitched"}) at ${STAMP_SEC}s`);
    }
  }
}

// ── STEREO REFACTOR (chill only) ─────────────────────────────────────
// Non-chill keeps the exact mono raw write → byte-identical trancenwaltz.
// Chill rebuilds a true STEREO pair from the individual buses, each with
// its own slow independent pan LFO — genuine per-track "panning stories"
// (bass sways wide + slow, pad/saw drifts opposite, drums/lead stay
// near-centre, the sfx voices — whistle/cat/gong — wander widest).
// Equal-power panning. dynEnv matches the original (dry+duck+bass)*env;
// sfx/mg/shutdown added flat as in the mono path.
let STEREO = false;
let buf;
if (isChill) {
  STEREO = true;
  buf = Buffer.alloc(out.length * 2 * 4);
  const SR = SAMPLE_RATE;
  for (let i = 0; i < out.length; i++) {
    const t = i / SR;
    const pBass = 0.55 * Math.sin(t * 2 * Math.PI * 0.028);
    const pPad  = 0.40 * Math.sin(t * 2 * Math.PI * 0.017 + 2.1);
    const pDry  = 0.14 * Math.sin(t * 2 * Math.PI * 0.045 + 4.0);
    const pSfx  = 0.85 * Math.sin(t * 2 * Math.PI * 0.022 + 1.0);
    const m = dynEnv[i];
    const dB = bassBuf[i] * m, dD = duckBuf[i] * m, dY = dryBuf[i] * m;
    const sX = sfxDryBuf[i];
    const ctr = (mgBuf[i] * 0.65 + shutdownBuf[i]) * 0.7071;
    const aB = (pBass + 1) * Math.PI / 4, aD = (pPad + 1) * Math.PI / 4;
    const aY = (pDry + 1) * Math.PI / 4,  aS = (pSfx + 1) * Math.PI / 4;
    const L = dB * Math.cos(aB) + dD * Math.cos(aD) + dY * Math.cos(aY) + sX * Math.cos(aS) + ctr;
    const R = dB * Math.sin(aB) + dD * Math.sin(aD) + dY * Math.sin(aY) + sX * Math.sin(aS) + ctr;
    buf.writeFloatLE(L, i * 8);
    buf.writeFloatLE(R, i * 8 + 4);
  }
} else {
  buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
}
writeFileSync(rawPath, buf);

// Mono f32 → stereo mp3 with:
//   reverb taps panned L + R         spatial depth
//   3-band mid-range EQ boost        fills the sine-only mix where it lacks bite
//   master compressor                glues dynamics, fills out the loud sections
//   loudnorm                         EBU R128 perceived-loudness normalize so
//                                    the whole track sits at a consistent level
//                                    (replaces brick-wall alimiter)
// Master chain — proper headroom and limiting so the final mix isn't
// hard-clipping like the previous gentle-compressor + loudnorm-only
// path. Input arrives at ~-1.5 dBFS peak from the pre-master soft
// clip. Chain:
//   stereo reverb taps  →  short fades  →  gentle EQ to fill the mids
//   →  stereo width  →  proper musical compressor  →  brick-wall
//   alimiter (-1 dBTP)  →  perceptual loudnorm (quieter target so the
//   compressor stays in control of dynamics).
// Time-varying reverb envelope — wet signals fade in over the first
// 1.5 s ("reverb coming online" at the boot) and fade out smoothly
// over the last 3 s ("returning to reality" as the system shuts
// down). Fade-out uses a quadratic ease so the transition to dry
// audio doesn't have a perceptible volume "skip" near the very end.
const REVERB_FADE_IN_SEC = 1.5;
const REVERB_FADE_OUT_START = totalSec - 3.0;
const REVERB_FADE_OUT_END = totalSec;
// pow(max(0, (END-t)/3), 2) — quadratic ease-out from 1 → 0 over 3 s.
// Squaring keeps the last second very soft so there's no abrupt drop.
const wetEnv =
  `if(lt(t\\,${REVERB_FADE_IN_SEC})\\,t/${REVERB_FADE_IN_SEC}\\,` +
  `if(gt(t\\,${REVERB_FADE_OUT_START})\\,` +
  `pow(max(0\\,(${REVERB_FADE_OUT_END}-t)/3)\\,2)\\,1))`;
// Chill wants a TINY, soft, dead room — a small carpeted basement, NOT
// the long house reverb: short, absorbed early reflections, very low
// wet, narrow image. The master is also eased for the quiet bed
// (gentler glue compression, softer loudness) so nothing is squashed.
const reverbDefs = isChill
  ? `[0:a]aecho=0.55:0.45:7|13|23|37:0.16|0.11|0.07|0.045,pan=stereo|c0=0.55*c0|c1=0.45*c0,volume='${wetEnv}':eval=frame[wet1];` +
    `[0:a]aecho=0.5:0.4:11|29:0.09|0.05,pan=stereo|c0=0.45*c0|c1=0.55*c0,volume='${wetEnv}':eval=frame[wet2];`
  : `[0:a]aecho=0.85:0.85:60|140|260|480|820:0.40|0.27|0.18|0.11|0.06,pan=stereo|c0=0.35*c0|c1=0.65*c0,volume='${wetEnv}':eval=frame[wet1];` +
    `[0:a]aecho=0.85:0.85:90|200|360|620:0.30|0.20|0.13|0.08,pan=stereo|c0=0.65*c0|c1=0.35*c0,volume='${wetEnv}':eval=frame[wet2];`;
const wetWeights = isChill ? "1 0.12 0.07" : "1 0.28 0.28";
const stereoStage = isChill ? "extrastereo=m=1.05:c=true," : "extrastereo=m=1.5:c=true,";
const compStage = isChill
  ? "acompressor=threshold=-18dB:ratio=1.4:attack=30:release=320:makeup=1:knee=6,"
  : "acompressor=threshold=-14dB:ratio=1.8:attack=18:release=200:makeup=1:knee=4,";
const loudStage = isChill
  ? "loudnorm=I=-16:TP=-1.5:LRA=20[out]"
  : "loudnorm=I=-15:TP=-1.5:LRA=20[out]";

const filter =
  (STEREO ? "[0:a]acopy[dry];" : "[0:a]pan=stereo|c0=c0|c1=c0[dry];") +
  // Long multi-tap reverb (60-820 ms) — restored. The short cluster
  // tried earlier (12-95 ms) made the mix feel boxy + the aggressive
  // compressor that paired with it pumped audibly. Back to the prior
  // tap layout that gave the house-y depth.
  reverbDefs +
  `[dry][wet1][wet2]amix=inputs=3:weights='${wetWeights}':duration=longest:normalize=0,` +
  // PERFECT LOOP — the bed is bar-aligned so totalSec wraps to t=0 on
  // the next downbeat, and the kick-settle ending already winds down to
  // the same gentle level the intro starts at. The old 0.15s fade-in /
  // 1.6s fade-out drove both ends to digital silence → an audible dip
  // at the loop seam. Replaced with ~6ms declicks (sub-perceptual, just
  // kills end-sample pops) so the ending flows straight into the intro.
  (RELEASE_MASTER
    ? "afade=t=in:st=0:d=0.15," +
      `afade=t=out:st=${(totalSec - 1.6).toFixed(3)}:d=1.6,`
    : "afade=t=in:st=0:d=0.006," +
      `afade=t=out:st=${(totalSec - 0.006).toFixed(3)}:d=0.006,`) +
  // House-style EQ — more AIR up top, less mid crowding, subtle low.
  "equalizer=f=180:t=q:w=1.2:g=1.0," +
  "equalizer=f=900:t=q:w=1.4:g=0.8," +
  "equalizer=f=2400:t=q:w=1.3:g=1.0," +
  "equalizer=f=10000:t=q:w=0.9:g=2.5," +
  "equalizer=f=60:t=q:w=1.0:g=1.0," +
  stereoStage +
  // Gentle compressor — was 1.6:1 / 20 ms / 240 ms. The 2.4:1 / 4 ms
  // version was creating an audible oscillating envelope pump on the
  // master. Back to slow musical compression that glues without
  // breathing.
  compStage +
  // Brick-wall safety limiter — mid-ceiling so the limiter isn't
  // working hard but still catches stray peaks. 0.94 was clipping
  // the loudnorm chain into pumping; 0.88 is the sweet spot.
  "alimiter=limit=0.88:attack=5:release=50:level=disabled," +
  // Loudness target — back to -15 LUFS (was -13 which was forcing
  // the limiter and acompressor to overdrive into pump). LRA=20 keeps
  // the new wider dynamic arc.
  loudStage;
const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", STEREO ? "2" : "1",
  "-i", rawPath,
  "-filter_complex", filter,
  "-map", "[out]",
  // Hard-trim at exactly totalSec so the file boundary lines up with
  // bar 60 — the file loops cleanly to t=0 on the next downbeat.
  "-t", String(totalSec),
  ...(OUT_IS_WAV
    ? ["-c:a", "pcm_s16le", "-ar", "44100"]   // DistroKid release WAV
    : ["-c:a", "libmp3lame", "-q:a", "3"]),
  OUT_PATH,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }

// (AC stamp is now mixed into the buffer at ~13 s — earlier in the
// track as a branding ID. The shutdown chime is the absolute final
// sound. No post-encode stamp overlay needed.)
console.log(`✓ ${OUT_PATH}`);

// (AC stamp is mixed INTO the out buffer in the outro section so the
// final mp3 is exactly bar-60-aligned and loops cleanly. See the
// stamp mix-in step before the ffmpeg encode.)
