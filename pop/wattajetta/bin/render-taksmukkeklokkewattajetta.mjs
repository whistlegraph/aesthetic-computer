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
const BASE = resolve(OUT, "wattajetta.mp3");
const RAW = resolve(OUT, ".taksmukkeklokkewattajetta-melody.f32le");
const MP3 = resolve(OUT, "taksmukkeklokkewattajetta.mp3");
const RECEIPT = resolve(OUT, "taksmukkeklokkewattajetta.production.json");
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

function osc(kind, phase) {
  const p = phase - Math.floor(phase);
  if (kind === "sine") return Math.sin(2 * Math.PI * p);
  if (kind === "triangle") return 1 - 4 * Math.abs(p - 0.5);
  if (kind === "hollow-square") return Math.sin(2 * Math.PI * p) + 0.22 * Math.sin(6 * Math.PI * p);
  return 0.72 * Math.sin(2 * Math.PI * p) + 0.2 * Math.sin(4 * Math.PI * p) + 0.08 * Math.sin(6 * Math.PI * p);
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

// The loose hand: chord tones plus 9ths/11ths, irregular entrances, occasional
// skipped answers, and small portamenti. Voices rotate at Wattajetta's breaths.
let lastMidi = 71;
const phraseStarts = [4, 8, 16, 20, 24, 32, 36, 48, 52, 56, 64, 68, 72];
for (const b of phraseStarts) {
  const voice = timbres[Math.floor(b / 16) % timbres.length];
  let cursor = b * BAR + (0.2 + rnd() * 0.55) * BEAT;
  const count = 5 + Math.floor(rnd() * 5);
  for (let i = 0; i < count; i++) {
    const chord = harmony[Math.floor(cursor / (2 * BAR)) % harmony.length];
    const palette = [...chord.notes.map((n) => n + 24), chord.notes[1] + 26, chord.notes[2] + 29];
    const candidates = palette.sort((a, c) => Math.abs(a - lastMidi) - Math.abs(c - lastMidi)).slice(0, 4);
    let midi = candidates[Math.floor(rnd() * candidates.length)];
    if (rnd() < 0.2) midi += rnd() < 0.5 ? -12 : 12;
    midi = clamp(midi, 64, 88);
    const durBeats = [0.32, 0.48, 0.72, 1.15, 1.7][Math.floor(rnd() * 5)];
    note({ t: cursor, dur: durBeats * BEAT, midi, glideFrom: rnd() < 0.38 ? lastMidi : midi,
      gain: 0.055 + rnd() * 0.025, pan: Math.sin(cursor * 0.73) * 0.62, voice });
    lastMidi = midi;
    cursor += [0.5, 0.75, 1, 1.5][Math.floor(rnd() * 4)] * BEAT;
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
  "-filter_complex", "[0:a]volume=0.86[water];[1:a]highpass=f=125,lowpass=f=9500,volume=1.0[clock];[water][clock]amix=inputs=2:duration=longest:normalize=0,alimiter=limit=0.93:attack=5:release=80[out]",
  "-map", "[out]", "-ar", String(SR), "-codec:a", "libmp3lame", "-b:a", "320k",
  "-metadata", "title=taksmukkeklokkewattajetta", "-metadata", "artist=aesthetic.computer",
  "-metadata", "comment=Taksmukke clock hands inside Wattajetta's water-material arc", MP3], { stdio: "inherit" });
if (ff.status !== 0) process.exit(ff.status ?? 1);

writeFileSync(RECEIPT, JSON.stringify({
  track: "taksmukkeklokkewattajetta", renderer: "bin/render-taksmukkeklokkewattajetta.mjs",
  relationship: "tempo-locked hybrid arrangement; source masters preserved",
  transport: { bpm: BPM, meter: "4/4", bars: BARS, durationSec: DUR },
  sourceRhythmSection: "out/wattajetta.mp3", output: "out/taksmukkeklokkewattajetta.mp3",
  harmony: harmony.map((h) => h.name), oscillatorRotation: timbres,
  structure: ["0-27 glass/drop: drifting extensions", "28-47 bronze/breath: chromatic common-tone turns", "48-63 steel/drop: portamento answers", "64-79 stone/drop: widest harmonic travel", "80-95 mist: four-voice call-and-response resolution"],
  melodicEventCount: events.length, events,
}, null, 2) + "\n");
console.log(`→ ${MP3} · ${events.length} melodic events · ${DUR.toFixed(1)}s`);
