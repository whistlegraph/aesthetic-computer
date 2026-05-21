#!/usr/bin/env node
// render-sound-samples.mjs — render short sample-preview mp3s for the
// pop dashboard: (1) the full AC percussion kit, (2) ambient beds.
//
// Usage:  node pop/bin/render-sound-samples.mjs [--perc-only] [--bed-only] [--notepat-only]
// Output: pop/demos/samples/perc-<id>.mp3        (12 drums)
//         pop/demos/samples/bed-<id>.mp3          (up to 5 beds)
//         pop/demos/samples/notepat-<voice>.mp3   (4 notepat wavetypes)

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

import { playPercussion, PERCUSSION_NAMES } from "../../system/public/aesthetic.computer/lib/percussion.mjs";
import { makeBufferSynth } from "../dance/synths/bus.mjs";

const HERE    = dirname(fileURLToPath(import.meta.url));
const POP     = resolve(HERE, "..");
const OUT_DIR = resolve(POP, "demos/samples");
const SR      = 48_000;

const args         = process.argv.slice(2);
const PERC_ONLY    = args.includes("--perc-only");
const BED_ONLY     = args.includes("--bed-only");
const NOTEPAT_ONLY = args.includes("--notepat-only");

// ── deterministic RNG (mulberry32) ───────────────────────────────────
function makeRng(seed = 0x9e3779b9) {
  let a = seed >>> 0;
  return function () {
    a = (a + 0x6D2B79F5) | 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// ── helpers ───────────────────────────────────────────────────────────
function normalize(buf, target = 0.9) {
  let peak = 0;
  for (let i = 0; i < buf.length; i++) { const a = Math.abs(buf[i]); if (a > peak) peak = a; }
  if (peak > 0) { const nrm = target / peak; for (let i = 0; i < buf.length; i++) buf[i] *= nrm; }
}

function encodeToMp3(buf, outPath) {
  const rawPath = `${outPath}.f32.raw`;
  const b = Buffer.alloc(buf.length * 4);
  for (let i = 0; i < buf.length; i++) b.writeFloatLE(buf[i], i * 4);
  writeFileSync(rawPath, b);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "4", outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) throw new Error(`ffmpeg failed for ${outPath}`);
}

// ── Part 1: AC Percussion ─────────────────────────────────────────────
// 12 drums, 5-6 hits each at musical spacing, slight volume variation.

// Maps letter → slug (matching PERCUSSION_NAMES)
const PERC_ORDER = [
  ["c",  "kick"],
  ["d",  "snare"],
  ["e",  "clap"],
  ["f",  "snap"],
  ["g",  "hat-c"],
  ["a",  "hat-o"],
  ["b",  "ride"],
  ["c#", "crash"],
  ["d#", "splash"],
  ["f#", "cowbell"],
  ["g#", "block"],
  ["a#", "tambo"],
];

// Per-drum tuning for hit count + spacing + clip duration.
// Some drums need longer clips (ride, crash, open hat) to ring out.
const PERC_OPTS = {
  "c":  { hits: 6, spacing: 0.45, dur: 3.0 },   // kick: tight, 6 hits
  "d":  { hits: 6, spacing: 0.40, dur: 3.0 },   // snare
  "e":  { hits: 5, spacing: 0.50, dur: 3.0 },   // clap
  "f":  { hits: 6, spacing: 0.40, dur: 3.0 },   // snap: short decay
  "g":  { hits: 6, spacing: 0.35, dur: 3.0 },   // hat-c: tight
  "a":  { hits: 4, spacing: 0.60, dur: 3.5 },   // hat-o: sustains, needs room
  "b":  { hits: 3, spacing: 0.80, dur: 3.5 },   // ride: long shimmer
  "c#": { hits: 2, spacing: 1.20, dur: 3.5 },   // crash: long wash, sparse
  "d#": { hits: 3, spacing: 0.90, dur: 3.5 },   // splash
  "f#": { hits: 5, spacing: 0.45, dur: 3.0 },   // cowbell: short
  "g#": { hits: 6, spacing: 0.38, dur: 3.0 },   // block: very short
  "a#": { hits: 5, spacing: 0.50, dur: 3.0 },   // tambo
};

function renderPercussion(letter, id) {
  const opts   = PERC_OPTS[letter] || { hits: 5, spacing: 0.50, dur: 3.0 };
  const bufLen = Math.ceil(opts.dur * SR);
  const out    = new Float32Array(bufLen);
  const rng    = makeRng(letter.charCodeAt(0) * 31337);

  for (let h = 0; h < opts.hits; h++) {
    const t = h * opts.spacing;
    if (t >= opts.dur) break;
    // Slight volume variation: 0.75–1.0 for ghost hits, 1.0 on beat 1
    const vol = (h === 0) ? 1.0 : 0.75 + rng() * 0.25;
    const sound = makeBufferSynth(out, t, SR, rng);
    playPercussion(sound, letter, { volume: vol, phase: "both" });
  }

  normalize(out, 0.9);
  const outPath = resolve(OUT_DIR, `perc-${id}.mp3`);
  encodeToMp3(out, outPath);
  console.log(`✓ perc-${id} (${letter}) → ${outPath}`);
}

// ── Part 2: Beds ──────────────────────────────────────────────────────
// Each bed renders ~7 s of audio. These inline implementations lift the
// synth cores directly from the source scripts, keeping this script
// self-contained.

const BED_DUR   = 7.0;
const BED_SAMPS = Math.ceil(BED_DUR * SR);

// midi → Hz helper shared by beds
function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// ── bed: sinebells — A minor arpeggiated bell strikes ─────────────────
// Partials from melody-bells.mjs / recap/bin/waltz.mjs (sinebells voice).
const BELL_PARTIALS = [
  { ratio: 0.5,  amp: 0.28, decayT60: 5.5 },
  { ratio: 1.0,  amp: 1.00, decayT60: 4.5 },
  { ratio: 2.0,  amp: 0.32, decayT60: 2.6 },
  { ratio: 2.4,  amp: 0.10, decayT60: 1.2 },
  { ratio: 3.0,  amp: 0.09, decayT60: 1.0 },
  { ratio: 4.5,  amp: 0.04, decayT60: 0.6 },
  { ratio: 5.4,  amp: 0.02, decayT60: 0.4 },
];
const BELL_GAIN  = 0.42;
const RING_TAIL  = 3.5; // seconds of ring after last hit

function renderBedSinebells() {
  const totalLen = Math.ceil((BED_DUR + RING_TAIL) * SR);
  const out      = new Float32Array(totalLen);

  // A-minor arpeggio: A3 C4 E4 A4 E4 C4 (8 quarter notes @ 92 bpm)
  const bpm = 92;
  const beat = 60 / bpm;
  const phrase = [57, 60, 64, 69, 64, 60, 57, 60]; // MIDI
  const twoPiOverSr = (2 * Math.PI) / SR;
  const ATTACK_SEC = 0.012;

  for (let n = 0; n < phrase.length; n++) {
    const startSec  = n * beat;
    if (startSec >= BED_DUR) break;
    const startIdx  = Math.floor(startSec * SR);
    const fundFreq  = midiToFreq(phrase[n]);
    const ringSamps = Math.floor((beat * 2 + RING_TAIL) * SR);
    const attackS   = ATTACK_SEC * SR;

    const partials = BELL_PARTIALS.map((p) => ({
      omega: twoPiOverSr * fundFreq * p.ratio,
      amp:   p.amp,
      decay: Math.exp(-Math.log(1000) / (p.decayT60 * SR)),
    }));

    for (let i = 0; i < ringSamps; i++) {
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
      out[dst] += s * att * BELL_GAIN * 0.9;
    }
  }

  normalize(out, 0.9);
  const outPath = resolve(OUT_DIR, "bed-sinebells.mp3");
  encodeToMp3(out.subarray(0, totalLen), outPath);
  console.log(`✓ bed-sinebells → ${outPath}`);
}

// ── bed: sacred-drone — 3-voice hum drone (lifted from sacred-drone.mjs) ─
// Root A2 (midi 45) to match 7s sample; detuned ±4 cents, breath-LFO.
function renderBedSacredDrone() {
  const DUR  = BED_DUR;
  const N    = Math.ceil(DUR * SR);
  const outL = new Float32Array(N);
  const outR = new Float32Array(N);

  const ROOT   = 45; // A2 (lower for moody 7s sample)
  const cents  = (c) => Math.pow(2, c / 1200);
  const midiHz = (m) => 440 * Math.pow(2, (m - 69) / 12);

  const voices = [
    { midi: ROOT,      panL: 0.62, panR: 0.62, lfoHz: 0.11, lfoPhase: 0.0,           gain: 0.95 },
    { midi: ROOT + 7,  panL: 0.78, panR: 0.42, lfoHz: 0.17, lfoPhase: Math.PI * 0.6, gain: 0.75 },
    { midi: ROOT + 12, panL: 0.42, panR: 0.78, lfoHz: 0.23, lfoPhase: Math.PI * 1.3, gain: 0.60 },
  ];

  const ATTACK  = 1.5;  // faster fade-in for a 7s clip
  const RELEASE = 2.5;

  for (const v of voices) {
    const f1 = midiHz(v.midi) * cents(-4);
    const f2 = midiHz(v.midi) * cents(+4);
    const ph1Off = (v.midi % 17) / 17 * Math.PI * 2;
    const ph2Off = (v.midi % 13) / 13 * Math.PI * 2;
    for (let i = 0; i < N; i++) {
      const t = i / SR;
      let env = v.gain;
      if (t < ATTACK)         env *= t / ATTACK;
      if (t > DUR - RELEASE)  env *= Math.max(0, (DUR - t) / RELEASE);
      const lfo = 0.92 + 0.08 * Math.sin(2 * Math.PI * v.lfoHz * t + v.lfoPhase);
      env *= lfo;
      const s = (Math.sin(2 * Math.PI * f1 * t + ph1Off) +
                 Math.sin(2 * Math.PI * f2 * t + ph2Off)) * 0.5 * env;
      outL[i] += s * v.panL;
      outR[i] += s * v.panR;
    }
  }

  // Soft tanh saturation
  let peak = 0;
  for (let i = 0; i < N; i++) {
    outL[i] = Math.tanh(outL[i] * 1.05);
    outR[i] = Math.tanh(outR[i] * 1.05);
    peak = Math.max(peak, Math.abs(outL[i]), Math.abs(outR[i]));
  }
  const tgt  = Math.pow(10, -3 / 20);
  const gain = peak > 0 ? tgt / peak : 1;
  for (let i = 0; i < N; i++) { outL[i] *= gain; outR[i] *= gain; }

  // Interleave stereo → f32le, then ffmpeg stereo mp3
  const stereo = new Float32Array(N * 2);
  for (let i = 0; i < N; i++) { stereo[i * 2] = outL[i]; stereo[i * 2 + 1] = outR[i]; }

  const outPath = resolve(OUT_DIR, "bed-sacred-drone.mp3");
  const rawPath = `${outPath}.f32.raw`;
  const b = Buffer.alloc(stereo.length * 4);
  for (let i = 0; i < stereo.length; i++) b.writeFloatLE(stereo[i], i * 4);
  writeFileSync(rawPath, b);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "4", outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) throw new Error(`ffmpeg failed for bed-sacred-drone`);
  console.log(`✓ bed-sacred-drone → ${outPath}`);
}

// ── bed: cool-sine — sub + chord pad + sparkle bells (lifted from
//    big-pictures/bin/cool-sine-layer.mjs, trimmed to 7 s) ───────────
function renderBedCoolSine() {
  const DUR  = BED_DUR;
  const N    = Math.ceil(DUR * SR);
  const outL = new Float32Array(N);
  const outR = new Float32Array(N);

  const BEAT       = 60 / 82;   // ~82 bpm feels bouncy at 7s
  const CHORD_LEN  = 4 * BEAT;  // 4 beats per chord slot
  const PROG       = [0, 3, 0, 4]; // I IV I V
  const ROOT_MIDI  = 57;           // A3 (matches sinebells)

  const SCALE_SEMI = [0, 2, 4, 5, 7, 9, 11];
  function chordMidis(degree) {
    const s    = SCALE_SEMI[degree % 7];
    const root = ROOT_MIDI + s;
    return { root, fifth: root + 7, oct: root + 12 };
  }
  function hz(m) { return 440 * Math.pow(2, (m - 69) / 12); }

  // sine SUB
  for (let i = 0; i < N; i++) {
    const t        = i / SR;
    const cIdx     = Math.floor(t / CHORD_LEN) % PROG.length;
    const c        = chordMidis(PROG[cIdx]);
    const fSub     = hz(c.root - 12);
    const fSub2    = hz(c.root - 12) * 1.0035;
    let env = 0.18;
    if (t < 1.5)         env *= t / 1.5;
    if (t > DUR - 2.0)   env *= Math.max(0, (DUR - t) / 2.0);
    const s = (Math.sin(2 * Math.PI * fSub * t) + Math.sin(2 * Math.PI * fSub2 * t)) * 0.5 * env;
    outL[i] += s * 0.9;
    outR[i] += s * 0.9;
  }

  // sine PAD (root + 5th + oct)
  for (let i = 0; i < N; i++) {
    const t    = i / SR;
    const cIdx = Math.floor(t / CHORD_LEN) % PROG.length;
    const c    = chordMidis(PROG[cIdx]);
    const lfo  = 0.85 + 0.15 * Math.sin(2 * Math.PI * 0.18 * t);
    let env    = 0.08 * lfo;
    if (t < 2.0)         env *= t / 2.0;
    if (t > DUR - 2.5)   env *= Math.max(0, (DUR - t) / 2.5);
    const sL = Math.sin(2*Math.PI*hz(c.root)*t)*0.5 + Math.sin(2*Math.PI*hz(c.fifth)*t)*0.7 + Math.sin(2*Math.PI*hz(c.oct)*t)*0.35;
    const sR = Math.sin(2*Math.PI*hz(c.root)*t)*0.5 + Math.sin(2*Math.PI*hz(c.fifth)*t)*0.35 + Math.sin(2*Math.PI*hz(c.oct)*t)*0.7;
    outL[i] += sL * env;
    outR[i] += sR * env;
  }

  // SPARKLE — one bell ping per chord boundary
  const totalChords = Math.ceil(DUR / CHORD_LEN);
  for (let k = 0; k < totalChords; k++) {
    const tStart = k * CHORD_LEN;
    if (tStart < 1.0 || tStart > DUR - 2.0) continue;
    const c        = chordMidis(PROG[k % PROG.length]);
    const fS1      = hz(c.root + 24);
    const fS2      = hz(c.fifth + 24);
    const decay    = 3.5;
    const sStart   = Math.floor(tStart * SR);
    const sEnd     = Math.min(N, sStart + Math.floor(decay * SR));
    for (let i = sStart; i < sEnd; i++) {
      const dt  = (i - sStart) / SR;
      const env = 0.055 * Math.exp(-dt * 1.4);
      const s1  = Math.sin(2 * Math.PI * fS1 * dt);
      const s2  = Math.sin(2 * Math.PI * fS2 * dt);
      outL[i] += s1 * env * 0.8 + s2 * env * 0.3;
      outR[i] += s1 * env * 0.3 + s2 * env * 0.8;
    }
  }

  // tanh soft-clip + peak normalize
  let peak = 0;
  for (let i = 0; i < N; i++) {
    outL[i] = Math.tanh(outL[i] * 1.1);
    outR[i] = Math.tanh(outR[i] * 1.1);
    peak = Math.max(peak, Math.abs(outL[i]), Math.abs(outR[i]));
  }
  const tgt  = Math.pow(10, -3 / 20);
  const gain = peak > 0 ? tgt / peak : 1;
  for (let i = 0; i < N; i++) { outL[i] *= gain; outR[i] *= gain; }

  const stereo = new Float32Array(N * 2);
  for (let i = 0; i < N; i++) { stereo[i * 2] = outL[i]; stereo[i * 2 + 1] = outR[i]; }

  const outPath = resolve(OUT_DIR, "bed-cool-sine.mp3");
  const rawPath = `${outPath}.f32.raw`;
  const b = Buffer.alloc(stereo.length * 4);
  for (let i = 0; i < stereo.length; i++) b.writeFloatLE(stereo[i], i * 4);
  writeFileSync(rawPath, b);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "4", outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) throw new Error(`ffmpeg failed for bed-cool-sine`);
  console.log(`✓ bed-cool-sine → ${outPath}`);
}

// ── bed: noise-sweep — synthesized pink-noise ocean sweep ──────────────
// Lifted from chillwave/bin/render.mjs's pink-noise + LFO pattern but
// standalone: no .np score required. Paul Kellet economy filter.
function renderBedNoiseSweep() {
  const N    = Math.ceil(BED_DUR * SR);
  const outL = new Float32Array(N);
  const outR = new Float32Array(N);
  const rng  = makeRng(0xdeadbeef);

  // Pink noise state (7-register Paul Kellet filter)
  const state = new Float32Array(7);
  function pinkSample() {
    const w = rng() * 2 - 1;
    state[0] = 0.99886 * state[0] + w * 0.0555179;
    state[1] = 0.99332 * state[1] + w * 0.0750759;
    state[2] = 0.96900 * state[2] + w * 0.1538520;
    state[3] = 0.86650 * state[3] + w * 0.3104856;
    state[4] = 0.55000 * state[4] + w * 0.5329522;
    state[5] = -0.7616 * state[5] - w * 0.0168980;
    const pink = state[0]+state[1]+state[2]+state[3]+state[4]+state[5]+state[6]+w*0.5362;
    state[6] = w * 0.115926;
    return pink * 0.11;
  }

  // Slow LFO amplitude modulation: 0.12 Hz ≈ 8s wave period
  const LFO_HZ = 0.12;
  const ATT    = 1.5;
  const REL    = 2.0;

  for (let i = 0; i < N; i++) {
    const t    = i / SR;
    let env    = 0.55;
    if (t < ATT)          env *= t / ATT;
    if (t > BED_DUR - REL) env *= Math.max(0, (BED_DUR - t) / REL);
    // slow wave swell: 0.35 .. 1.0
    const lfo  = 0.35 + 0.65 * (0.5 + 0.5 * Math.sin(2 * Math.PI * LFO_HZ * t - Math.PI * 0.5));
    const s    = pinkSample() * env * lfo;
    // Tiny stereo spread via 3-sample offset
    outL[i] += s;
    if (i >= 3) outR[i - 3] += s * 0.97;
    else outR[i] += s * 0.97;
  }

  let peak = 0;
  for (let i = 0; i < N; i++) peak = Math.max(peak, Math.abs(outL[i]), Math.abs(outR[i]));
  const norm = peak > 0 ? 0.82 / peak : 1;
  for (let i = 0; i < N; i++) { outL[i] *= norm; outR[i] *= norm; }

  const stereo = new Float32Array(N * 2);
  for (let i = 0; i < N; i++) { stereo[i * 2] = outL[i]; stereo[i * 2 + 1] = outR[i]; }

  const outPath = resolve(OUT_DIR, "bed-noise-sweep.mp3");
  const rawPath = `${outPath}.f32.raw`;
  const b = Buffer.alloc(stereo.length * 4);
  for (let i = 0; i < stereo.length; i++) b.writeFloatLE(stereo[i], i * 4);
  writeFileSync(rawPath, b);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "4", outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) throw new Error(`ffmpeg failed for bed-noise-sweep`);
  console.log(`✓ bed-noise-sweep → ${outPath}`);
}

// Note: bed-ocean is SKIPPED — it requires a real .waves.wav recording
// (freesound CC0 sample cached at pop/chillwave/out/.waves.wav) plus
// the full chillwave .np score parser. The dashboard gracefully shows
// "sample pending" when the file is absent.

// ── Part 3: Notepat Voices ────────────────────────────────────────────
// Renders a short rising arpeggio + held note for each wave type that
// makeBufferSynth supports. harp + whistle are browser-only algorithms
// (they fall through to `default: s = 0` in bus.mjs) and are skipped —
// the dashboard shows "sample pending" gracefully for absent files.
//
// MIDI phrase: D3 F#3 A3 D4 E4 F#4 A4 — rising arpeggio then a held D5.
// Each note 0.35 s; total ~3.15 s with a short tail.

const NP_MIDI   = [50, 54, 57, 62, 64, 66, 69, 74];
const NP_DUR    = 0.35;    // seconds per note
const NP_ATK    = 0.01;    // attack
const NP_DECAY  = 0.30;    // decay (note release)
const NP_VOL    = 0.70;
const NP_TAIL   = 0.8;     // silence at end so the last note rings out
const NP_SR     = SR;

// Voices that bus.mjs actually implements (harp/whistle → s=0, omit them).
const NOTEPAT_VOICES = ["sine", "triangle", "sawtooth", "square"];

function midiToHz(m) { return 440 * Math.pow(2, (m - 69) / 12); }

function renderNotepatVoice(type) {
  const totalDur = NP_MIDI.length * NP_DUR + NP_TAIL;
  const bufLen   = Math.ceil(totalDur * NP_SR);
  const out      = new Float32Array(bufLen);
  const rng      = makeRng(0xac0 ^ type.charCodeAt(0));

  for (let n = 0; n < NP_MIDI.length; n++) {
    const startSec = n * NP_DUR;
    const hz       = midiToHz(NP_MIDI[n]);
    const sound    = makeBufferSynth(out, startSec, NP_SR, rng);
    sound.synth({
      type,
      tone:     hz,
      duration: NP_DUR,
      attack:   NP_ATK,
      decay:    NP_DECAY,
      volume:   NP_VOL,
    });
  }

  normalize(out, 0.9);
  const outPath = resolve(OUT_DIR, `notepat-${type}.mp3`);
  encodeToMp3(out, outPath);
  console.log(`✓ notepat-${type} → ${outPath}`);
}

// ── main ──────────────────────────────────────────────────────────────
mkdirSync(OUT_DIR, { recursive: true });

let ok = 0, fail = 0;

if (!BED_ONLY && !NOTEPAT_ONLY) {
  console.log("\n── Percussion kit ──────────────────────────────────");
  for (const [letter, id] of PERC_ORDER) {
    try {
      renderPercussion(letter, id);
      ok++;
    } catch (err) {
      console.error(`✗ perc-${id}: ${err.message}`);
      fail++;
    }
  }
}

if (!PERC_ONLY && !NOTEPAT_ONLY) {
  console.log("\n── Beds ─────────────────────────────────────────────");
  const BEDS = [
    ["sinebells",   renderBedSinebells],
    ["sacred-drone",renderBedSacredDrone],
    ["cool-sine",   renderBedCoolSine],
    ["noise-sweep", renderBedNoiseSweep],
    // ocean: skipped (needs real wav recording + .np score)
  ];
  for (const [id, fn] of BEDS) {
    try {
      fn();
      ok++;
    } catch (err) {
      console.error(`✗ bed-${id}: ${err.message}`);
      fail++;
    }
  }
  console.log("  (bed-ocean: skipped — requires pop/chillwave/out/.waves.wav)");
}

if (!PERC_ONLY && !BED_ONLY) {
  console.log("\n── Notepat voices ───────────────────────────────────");
  for (const voice of NOTEPAT_VOICES) {
    try {
      renderNotepatVoice(voice);
      ok++;
    } catch (err) {
      console.error(`✗ notepat-${voice}: ${err.message}`);
      fail++;
    }
  }
  console.log("  (notepat-harp, notepat-whistle: skipped — browser-only algorithms)");
}

console.log(`\ndone — ${ok} rendered, ${fail} failed → ${OUT_DIR}`);
