#!/usr/bin/env node
// taksmukkeklokkewattajetta — Taksmukke's wandering clock hands and shifting
// oscillator voices composed inside Wattajetta's water/material flight plan.
// This is a tempo-locked hybrid arrangement, not a concatenation of masters.

import { mkdirSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
mkdirSync(OUT, { recursive: true });

const SR = 48_000;
const BPM = 138;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const BARS = 96;
const DUR = BARS * BAR + 5.5;
const FRAMES = Math.ceil(DUR * SR);
const BASE = resolve(OUT, "wattajetta-tinybells.mp3");
const RAW = resolve(OUT, ".taksmukkeklokkewattajetta-melody.f32le");
const MP3 = resolve(OUT, "taksmukkeklokkewattajetta-dance-v3.mp3");
const RECEIPT = resolve(OUT, "taksmukkeklokkewattajetta-dance-v3.production.json");
const audio = new Float32Array(FRAMES * 2);

let seed = 0x74616b77;
const rnd = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296);
const clamp = (x, lo, hi) => Math.max(lo, Math.min(hi, x));
const midiHz = (m) => 440 * 2 ** ((m - 69) / 12);

// Each breath changes harmonic weather. The borrowed bII and dominant b9 are
// deliberate adventure; common tones keep the turns loose instead of abrupt.
const harmony = [
  { name: "Em9",       notes: [52, 55, 59, 66] },
  { name: "Cmaj7#11",  notes: [48, 52, 59, 66] },
  { name: "Am9",       notes: [45, 52, 59, 60] },
  { name: "D13sus",    notes: [50, 55, 59, 64] },
  { name: "Gmaj9",     notes: [43, 50, 54, 57] },
  { name: "Bbmaj7#11", notes: [46, 50, 57, 64] },
  { name: "F#m11",     notes: [42, 49, 52, 59] },
  { name: "B7b9",      notes: [47, 51, 57, 60] },
];
const timbres = ["sine", "triangle", "hollow-square", "soft-saw"];
const events = [];
const beatEvents = [];

function osc(kind, phase) {
  const p = phase - Math.floor(phase);
  if (kind === "sine") return Math.sin(2 * Math.PI * p);
  // A close, clean whistle that can sit beside the FEM bells without turning
  // into another pad: fundamental forward, just enough upper air to speak.
  if (kind === "whistle") return 0.9 * Math.sin(2 * Math.PI * p)
    + 0.075 * Math.sin(4 * Math.PI * p)
    + 0.025 * Math.sin(6 * Math.PI * p);
  if (kind === "triangle") return 1 - 4 * Math.abs(p - 0.5);
  if (kind === "hollow-square") return Math.sin(2 * Math.PI * p) + 0.22 * Math.sin(6 * Math.PI * p);
  return 0.72 * Math.sin(2 * Math.PI * p) + 0.2 * Math.sin(4 * Math.PI * p) + 0.08 * Math.sin(6 * Math.PI * p);
}

function beatHit({ t, dur, pan = 0, gain = 1, sample }) {
  const begin = Math.max(0, Math.floor(t * SR));
  const end = Math.min(FRAMES, Math.ceil((t + dur) * SR));
  const gl = Math.sqrt((1 - pan) * 0.5), gr = Math.sqrt((1 + pan) * 0.5);
  for (let f = begin; f < end; f++) {
    const age = f / SR - t;
    const x = sample(age, dur) * gain;
    audio[f * 2] += x * gl;
    audio[f * 2 + 1] += x * gr;
  }
}

function kick(t, gain) {
  let phase = 0;
  beatHit({ t, dur: 0.34, gain, sample(age) {
    const hz = 44 + 105 * Math.exp(-age * 25);
    phase += hz / SR;
    const body = Math.sin(2 * Math.PI * phase) * Math.exp(-age * 11);
    const click = (Math.sin(2 * Math.PI * 1800 * age) * Math.exp(-age * 150));
    return body * 0.92 + click * 0.08;
  }});
  beatEvents.push({ t: +t.toFixed(4), kind: "kick", gain });
}

function hat(t, gain, open = false) {
  let noise = ((Math.floor(t * SR) ^ 0x51f15e) >>> 0) || 1;
  const dur = open ? 0.16 : 0.055;
  beatHit({ t, dur, pan: Math.sin(t * 2.7) * 0.32, gain, sample(age) {
    noise ^= noise << 13; noise ^= noise >>> 17; noise ^= noise << 5;
    const white = ((noise >>> 0) / 2147483648) - 1;
    return white * Math.exp(-age * (open ? 24 : 75)) * (0.72 + 0.28 * Math.sin(2 * Math.PI * 9100 * age));
  }});
  beatEvents.push({ t: +t.toFixed(4), kind: open ? "open-hat" : "hat", gain });
}

function clap(t, gain) {
  let noise = ((Math.floor(t * SR) ^ 0xc1a0) >>> 0) || 1;
  beatHit({ t, dur: 0.12, pan: 0.08, gain, sample(age) {
    noise ^= noise << 13; noise ^= noise >>> 17; noise ^= noise << 5;
    const white = ((noise >>> 0) / 2147483648) - 1;
    const burst = Math.max(
      Math.exp(-age * 42),
      age > 0.018 ? 0.72 * Math.exp(-(age - 0.018) * 48) : 0,
      age > 0.036 ? 0.48 * Math.exp(-(age - 0.036) * 55) : 0);
    return white * burst;
  }});
  beatEvents.push({ t: +t.toFixed(4), kind: "clap", gain });
}

function rim(t, gain, pan) {
  beatHit({ t, dur: 0.045, pan, gain, sample(age) {
    const env = Math.exp(-age * 105);
    return (0.72 * Math.sin(2 * Math.PI * 1680 * age)
      + 0.28 * Math.sin(2 * Math.PI * 2470 * age)) * env;
  }});
  beatEvents.push({ t: +t.toFixed(4), kind: "rim", gain });
}

// Establish the floor immediately: first four bars are a restrained heartbeat,
// then hats and claps reveal the full dance grid. Wattajetta's own rhythm still
// supplies the character; this bus makes the hybrid's opening intention clear.
for (let b = 0; b < BARS; b++) {
  const intro = b < 4;
  for (let q = 0; q < 4; q++) kick(b * BAR + q * BEAT, intro ? 0.105 : 0.16);
  if (b >= 2) {
    for (let e = 0; e < 8; e++) hat(b * BAR + (e + 0.5) * BEAT / 2,
      (e % 2 ? 0.025 : 0.034) * (b < 4 ? 0.7 : 1), e === 7 && b % 4 === 3);
  }
  if (b >= 4) { clap(b * BAR + BEAT, 0.055); clap(b * BAR + 3 * BEAT, 0.055); }
  // The first 45 seconds keep discovering the kit: dry little rim answers
  // enter after the basic floor is understood, then disappear at bar 26.
  if (b >= 8 && b < 26) {
    rim(b * BAR + 1.5 * BEAT, 0.042, -0.42);
    if (b % 2) rim(b * BAR + 3.25 * BEAT, 0.035, 0.42);
  }
}

function note({ t, dur, midi, gain, pan, voice, glideFrom = midi }) {
  const a = Math.min(0.035, dur * 0.18), r = Math.min(0.24, dur * 0.42);
  const begin = Math.max(0, Math.floor(t * SR));
  const end = Math.min(FRAMES, Math.ceil((t + dur) * SR));
  const gl = Math.sqrt((1 - pan) * 0.5), gr = Math.sqrt((1 + pan) * 0.5);
  let phase = 0;
  for (let f = begin; f < end; f++) {
    const age = f / SR - t;
    const env = age < a ? age / a : age > dur - r ? Math.max(0, (dur - age) / r) : 1;
    const slide = Math.min(1, age / Math.min(0.13, dur * 0.35));
    const hz = midiHz(glideFrom + (midi - glideFrom) * (slide * slide * (3 - 2 * slide)));
    phase += hz / SR;
    const shimmer = 0.84 * osc(voice, phase) + 0.16 * Math.sin(2 * Math.PI * phase * 2.003);
    const x = shimmer * env * gain;
    audio[f * 2] += x * gl;
    audio[f * 2 + 1] += x * gr;
  }
  events.push({ t: +t.toFixed(4), dur: +dur.toFixed(4), midi, voice, chord: harmony[Math.floor(t / (2 * BAR)) % harmony.length].name });
}

// A quiet, breathing voicing underneath every two bars. It leaves the root to
// Wattajetta's sub and choir, so extensions rather than density do the work.
for (let b = 0; b < 80; b += 2) {
  const chord = harmony[(b / 2) % harmony.length];
  const voice = timbres[Math.floor(b / 8) % timbres.length];
  for (let n = 1; n < chord.notes.length; n++)
    note({ t: b * BAR + 0.08 + n * 0.018, dur: 1.62 * BAR, midi: chord.notes[n] + 12,
      gain: 0.026, pan: [-0.48, 0.18, 0.48][n - 1], voice });
}

// Bar 64 (~1:51 in the first render) is the composition's center: low sine
// voices, widest harmony, stone bells, and a hard four-on-floor. Build the
// entire song from that grammar. Every entrance is on a sixteenth/eighth grid;
// the sine calls land with the kick and the whistle answers rock on offbeats.
const hookRhythm = [0, 0.75, 1.5, 2, 2.75, 3.5];
const hookDegrees = [0, 2, 1, 3, 2, 1];
for (let b = 2; b < 80; b += 2) {
  const chord = harmony[(b / 2) % harmony.length];
  const low = chord.notes.map((n) => clamp(n + 12, 60, 76));
  const variation = Math.floor(b / 8) % 4;

  // Early under-sine: a quiet eighth-note current beneath the audible hook.
  // It shares the grid with the hats and alternates root/fifth, making the
  // first 45 seconds rock even when the whistle leaves a gap.
  if (b < 26) {
    for (let e = 0; e < 16; e++) {
      const midi = low[e % 4 === 3 ? 2 : 0];
      note({ t: b * BAR + (e + 0.5) * BEAT / 2, dur: 0.16 * BEAT,
        midi, gain: e % 4 === 3 ? 0.042 : 0.032,
        pan: e % 2 ? 0.12 : -0.12, voice: "sine" });
    }
  }

  // Kick-side call: the lower sine-whistle body that made the stone drop work.
  for (let i = 0; i < hookRhythm.length; i++) {
    const midi = low[(hookDegrees[i] + variation) % low.length];
    note({
      t: b * BAR + hookRhythm[i] * BEAT,
      dur: (i === 3 ? 0.68 : 0.32) * BEAT,
      midi,
      glideFrom: i ? low[(hookDegrees[i - 1] + variation) % low.length] : midi,
      gain: b < 4 ? 0.068 : 0.092,
      pan: -0.28 + (i % 2) * 0.16,
      voice: "sine",
    });
  }

  // Bell-side answer: one octave is deliberately avoided; this stays in the
  // human whistle register and shares pitch territory with the FEM bowls.
  const answerChord = harmony[(b / 2 + 1) % harmony.length];
  const answer = answerChord.notes.map((n) => clamp(n + 17, 64, 79));
  for (let i = 0; i < hookRhythm.length; i++) {
    const midi = answer[(hookDegrees[hookDegrees.length - 1 - i] + variation) % answer.length];
    note({
      t: (b + 1) * BAR + (hookRhythm[i] + 0.5) * BEAT,
      dur: (i === hookRhythm.length - 1 ? 0.72 : 0.3) * BEAT,
      midi,
      glideFrom: i ? answer[(hookDegrees[hookDegrees.length - i] + variation) % answer.length] : midi,
      gain: b < 4 ? 0.075 : (b >= 64 ? 0.112 : 0.098),
      pan: 0.34 - (i % 2) * 0.14,
      voice: "whistle",
    });
  }
}

// In the mist, the four original oscillator identities answer one another and
// resolve the B7b9 tension back to a bare E/B fifth around the final glass bell.
for (let b = 80; b < 94; b += 2) {
  const i = (b - 80) / 2;
  note({ t: b * BAR + (i % 2 ? 1.25 : 0.5) * BEAT, dur: (2.1 - i * 0.14) * BEAT,
    midi: [76, 79, 78, 71, 72, 66, 64][i], glideFrom: i ? [76, 76, 79, 78, 71, 72, 66][i] : 71,
    gain: 0.065 - i * 0.004, pan: i % 2 ? 0.48 : -0.48, voice: timbres[i % timbres.length] });
}

// Gentle safety saturation on the newly composed bus only.
for (let i = 0; i < audio.length; i++) audio[i] = Math.tanh(audio[i] * 1.35) / 1.35;
writeFileSync(RAW, Buffer.from(audio.buffer));

const ff = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-i", BASE, "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", RAW,
  "-filter_complex", "[0:a]volume=0.82[water];[1:a]highpass=f=28,lowpass=f=11000,equalizer=f=2500:t=q:w=1.1:g=2.5,volume=1.08[clock];[water][clock]amix=inputs=2:duration=longest:normalize=0,acompressor=threshold=-18dB:ratio=2.2:attack=12:release=120:makeup=1.5:knee=5,alimiter=limit=0.93:attack=5:release=80[out]",
  "-map", "[out]", "-ar", String(SR), "-codec:a", "libmp3lame", "-b:a", "320k",
  "-metadata", "title=taksmukkeklokkewattajetta", "-metadata", "artist=aesthetic.computer",
  "-metadata", "comment=Taksmukke clock hands inside Wattajetta's water-material arc", MP3], { stdio: "inherit" });
if (ff.status !== 0) process.exit(ff.status ?? 1);

writeFileSync(RECEIPT, JSON.stringify({
  track: "taksmukkeklokkewattajetta", renderer: "bin/render-taksmukkeklokkewattajetta.mjs",
  relationship: "dance composition: early four-on-floor grid, FEM-aligned whistle hook, Wattajetta material arc",
  transport: { bpm: BPM, meter: "4/4", bars: BARS, durationSec: DUR },
  sourceRhythmSection: "out/wattajetta-tinybells.mp3", output: "out/taksmukkeklokkewattajetta-dance-v3.mp3",
  harmony: harmony.map((h) => h.name), oscillatorRotation: timbres,
  structure: ["0-3 heartbeat: four-on-floor arrives immediately", "4-27 glass/drop: full dance grid + whistle/FEM call-and-response", "28-47 bronze/breath: low sine calls rock against offbeat whistles", "48-63 steel/drop: beat-locked register pressure", "64-79 stone/drop: widest harmonic travel", "80-95 mist: four-voice call-and-response resolution"],
  melodicEventCount: events.length, beatEventCount: beatEvents.length, events, beatEvents,
}, null, 2) + "\n");
console.log(`→ ${MP3} · ${events.length} melodic events · ${DUR.toFixed(1)}s`);
