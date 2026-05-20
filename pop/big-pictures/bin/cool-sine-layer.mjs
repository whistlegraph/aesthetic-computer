#!/usr/bin/env node
// cool-sine-layer.mjs — render a "cooler" sine accompaniment that
// runs underneath the verse-1 amazing-grace stem and extends it to
// 1:24 with bed-only intro + outro.
//
// Voices (all Math.sin, hellsine-family):
//   • Sine SUB bass — octave below the score's chord root, with slow
//     amp envelope (pad-like).
//   • Sine PAD — sustained 3-voice cluster on the bar's chord
//     (root + 5th + octave), very soft, slowly modulated low-pass via
//     amplitude only (we can't lowpass without DSP, but additive 3-osc
//     stays clean).
//   • Sine SPARKLE — top-octave crystalline bell triggered on the
//     downbeat of each phrase, exponential decay (~4 s).
//
// Output: stereo WAV at 44.1 kHz, normalized.
//
// Usage: node pop/big-pictures/bin/cool-sine-layer.mjs --bpm 70
//          --duration 84 --out ~/Desktop/cool-sine.wav

import { writeFileSync } from "node:fs";
import { resolve } from "node:path";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  }
}

const BPM = Number(flags.bpm ?? 70);
const DUR = Number(flags.duration ?? 84);  // seconds
const SR = 44100;
const OUT = resolve(String(flags.out || "/tmp/cool-sine.wav").replace(/^~/, process.env.HOME));
const N = Math.floor(DUR * SR);

// G-major progression (0=I G, 3=IV C, 0=I G, 4=V D) — matches the
// cli's amazing waltz bed (progression 0,3,0,4 with transposeBed=7).
// Each chord lasts 4 beats. 70 bpm → 1 beat = 0.857s, 1 bar = 3.43s
// (3/4), 1 chord = 4 beats = 3.43s. We use 4-beat blocks for clean
// chord changes regardless of meter.
const BEAT = 60 / BPM;
const CHORD_LEN_S = 4 * BEAT;
const PROG = [0, 3, 0, 4]; // I, IV, I, V (degree offsets, 0-indexed)

// Major-scale-degree → semitones-from-root
const SCALE_SEMI = [0, 2, 4, 5, 7, 9, 11];

// Root note G3 = midi 55. SUB an octave lower, PAD around 55-67,
// SPARKLE around 79.
const ROOT_MIDI = 55;

function midiToHz(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// chord midis for degree (root, 5th, octave) above ROOT_MIDI
function chordMidis(degree) {
  const s = SCALE_SEMI[degree % 7];
  const root = ROOT_MIDI + s;
  const fifth = root + 7;
  const oct = root + 12;
  return { root, fifth, oct };
}

// Pre-fill stereo buffer (interleaved L,R floats during synth)
const L = new Float32Array(N);
const R = new Float32Array(N);

// ── voice: sine SUB ─────────────────────────────────────────────────
// Continuous, switches pitch on chord boundaries. Pad-like amp.
// Two slightly detuned osc for chorus thickness.
{
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const chordIdx = Math.floor(t / CHORD_LEN_S) % PROG.length;
    const c = chordMidis(PROG[chordIdx]);
    const fSub = midiToHz(c.root - 12); // one octave below root
    const fSub2 = midiToHz(c.root - 12) * 1.0035; // 6 cent detune
    // Slow attack at start, sustain, slow release at end
    let env = 0.18;
    const ATT = 8.0;   // 8 s slow fade-in for the "cool intro"
    const REL = 12.0;  // 12 s slow tail
    if (t < ATT) env *= t / ATT;
    if (t > DUR - REL) env *= Math.max(0, (DUR - t) / REL);
    // Smooth chord boundary so we don't click
    const tInChord = t - chordIdx * CHORD_LEN_S - Math.floor(t / (CHORD_LEN_S * PROG.length)) * (CHORD_LEN_S * PROG.length);
    const xfadeStart = CHORD_LEN_S - 0.15; // 150 ms boundary smooth
    if (tInChord > xfadeStart) {
      const k = (tInChord - xfadeStart) / 0.15;
      env *= (1 - k * 0.4); // dip a touch, then re-rise next chord
    }
    const ph1 = 2 * Math.PI * fSub * t;
    const ph2 = 2 * Math.PI * fSub2 * t;
    const s = (Math.sin(ph1) + Math.sin(ph2)) * 0.5 * env;
    L[i] += s * 0.9;
    R[i] += s * 0.9;
  }
}

// ── voice: sine PAD (3-osc chord) ───────────────────────────────────
{
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const chordIdx = Math.floor(t / CHORD_LEN_S) % PROG.length;
    const c = chordMidis(PROG[chordIdx]);
    const fRoot = midiToHz(c.root);
    const fFifth = midiToHz(c.fifth);
    const fOct = midiToHz(c.oct);
    // Slow tremolo via amp LFO @ 0.18 Hz
    const lfo = 0.85 + 0.15 * Math.sin(2 * Math.PI * 0.18 * t);
    let env = 0.08 * lfo;
    const ATT = 10.0;
    const REL = 14.0;
    if (t < ATT) env *= t / ATT;
    if (t > DUR - REL) env *= Math.max(0, (DUR - t) / REL);
    const s1 = Math.sin(2 * Math.PI * fRoot * t);
    const s2 = Math.sin(2 * Math.PI * fFifth * t);
    const s3 = Math.sin(2 * Math.PI * fOct * t);
    // Wide stereo: root center, fifth slightly left, octave slightly right.
    const sL = s1 * 0.5 + s2 * 0.7 + s3 * 0.35;
    const sR = s1 * 0.5 + s2 * 0.35 + s3 * 0.7;
    L[i] += sL * env;
    R[i] += sR * env;
  }
}

// ── voice: sine SPARKLE — pinged bells on the downbeat of each chord
// Two-octave-up bell, ~4 s exponential decay.
{
  const totalChords = Math.ceil(DUR / CHORD_LEN_S);
  for (let k = 0; k < totalChords; k++) {
    const tStart = k * CHORD_LEN_S;
    const chordIdx = k % PROG.length;
    const c = chordMidis(PROG[chordIdx]);
    const fSpark = midiToHz(c.root + 24); // two octaves up
    const fSpark2 = midiToHz(c.fifth + 24); // fifth two octaves up
    const decay = 4.0;
    const startSample = Math.floor(tStart * SR);
    // Only ring during the "active" portion — skip the silent
    // intro lead-in and outro tail so the sparkle doesn't appear too
    // early.  Keep sparkles between t=14 and t=DUR-16.
    if (tStart < 14 || tStart > DUR - 16) continue;
    const endSample = Math.min(N, startSample + Math.floor(decay * SR));
    for (let i = startSample; i < endSample; i++) {
      const dt = (i - startSample) / SR;
      const env = 0.06 * Math.exp(-dt * 1.4);
      const s1 = Math.sin(2 * Math.PI * fSpark * dt);
      const s2 = Math.sin(2 * Math.PI * fSpark2 * dt);
      // Pan: sparkle one left, fifth one right
      L[i] += s1 * env * 0.8 + s2 * env * 0.3;
      R[i] += s1 * env * 0.3 + s2 * env * 0.8;
    }
  }
}

// ── light tape-like saturation (soft-clip via tanh) ──────────────────
let peak = 0;
for (let i = 0; i < N; i++) {
  L[i] = Math.tanh(L[i] * 1.1);
  R[i] = Math.tanh(R[i] * 1.1);
  peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
}
// Peak-normalize to -3 dB headroom
const target = Math.pow(10, -3 / 20);
const gain = peak > 0 ? target / peak : 1;
for (let i = 0; i < N; i++) { L[i] *= gain; R[i] *= gain; }
console.log(`▸ synth peak ${peak.toFixed(3)} → gain ${gain.toFixed(3)} (target -3 dB)`);

// ── encode WAV (16-bit PCM stereo, 44.1 kHz) ────────────────────────
const nFrames = N;
const dataSize = nFrames * 2 * 2; // 2 ch × 2 bytes
const buf = Buffer.alloc(44 + dataSize);
buf.write("RIFF", 0);
buf.writeUInt32LE(36 + dataSize, 4);
buf.write("WAVE", 8);
buf.write("fmt ", 12);
buf.writeUInt32LE(16, 16);          // PCM chunk size
buf.writeUInt16LE(1, 20);           // PCM format
buf.writeUInt16LE(2, 22);           // channels
buf.writeUInt32LE(SR, 24);
buf.writeUInt32LE(SR * 2 * 2, 28);  // byte rate
buf.writeUInt16LE(4, 32);           // block align
buf.writeUInt16LE(16, 34);          // bits/sample
buf.write("data", 36);
buf.writeUInt32LE(dataSize, 40);
let off = 44;
for (let i = 0; i < N; i++) {
  buf.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(L[i] * 32767))), off); off += 2;
  buf.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(R[i] * 32767))), off); off += 2;
}

writeFileSync(OUT, buf);
console.log(`✓ wrote ${OUT} (${(dataSize/1024/1024).toFixed(1)} MB · ${DUR}s · ${SR}Hz · stereo)`);
