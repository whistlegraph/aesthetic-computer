#!/usr/bin/env node
// jam.mjs — granular remix of oskie's bounce. The drum break stays as
// rhythm, but every other voice is granularly sampled from the original
// recording: pitched grains of bass.wav for the harmonic pad, pitched
// grains of source.wav for the stabs and bell-rings. No clean synths.
// The grit, the noise floor, the tonal ambiguity — all of it lives in
// the source, so the remix sounds like the source playing itself back
// in different shapes.
//
// Pitch math: the analyzed bass fundamental is ~41 Hz (E1, MIDI 28).
// To make a grain ring at MIDI m we play it at rate = 2^((m - 28)/12).
//
// Form: intro 4 / verse 8 / chorus 8 / bridge 4 / chorus 8 / outro 4
// = 36 bars @ 101.33 BPM, trimmed to exactly 84 s.

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { readWavMono } from "../../lib/wav.mjs";
import * as progress from "../../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const BASS_PATH = resolve(HERE, "stems/htdemucs/bass.wav");
const DRUMS_PATH = resolve(HERE, "stems/htdemucs/drums.wav");
const OUT_PATH = resolve(HERE, "oskie-jam.wav");

const SR = 44100;
const BPM = 101.33;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const LOOP_START_S = 0.813;
const LOOP_BARS = 4;
const LOOP_S = LOOP_BARS * BAR;
const TARGET_S = 84.0;

// E major (analyzed key), bass anchored at MIDI 28 (E1, ~41 Hz)
const BASS_F0_MIDI = 28;

// ── chord progression ────────────────────────────────────────────────
// 4-bar cycle in E major. CHORDS_A is the classic I–V–vi–IV
// ("axis of awesome"); CHORDS_B is its rotation (vi–IV–I–V), used to
// make chorus 2 ≠ chorus 1 without leaving the key. Each entry is
// { root: pitchClass, third: semitonesAboveRoot, fifth: semitonesAboveRoot }.
const CHORDS = {
  E:  { root: 4,  third: 4, fifth: 7 },   // E major
  B:  { root: 11, third: 4, fifth: 7 },   // B major
  Cs: { root: 1,  third: 3, fifth: 7 },   // C# minor
  A:  { root: 9,  third: 4, fifth: 7 },   // A major
};
const CHORDS_A = [CHORDS.E, CHORDS.B, CHORDS.Cs, CHORDS.A];
const CHORDS_B = [CHORDS.Cs, CHORDS.A, CHORDS.E, CHORDS.B];

function pcToMidi(pc, octave) { return pc + 12 * (octave + 1); }
function chordRoot(ch, octave) { return pcToMidi(ch.root, octave); }
function chordTriad(ch, octave) {
  const r = chordRoot(ch, octave);
  return [r, r + ch.third, r + ch.fifth];
}
// 8th-note arp through chord tones across one bar.
function chordArp(ch, octave) {
  const r = chordRoot(ch, octave);
  return [r, r + ch.fifth, r + 12, r + ch.third,
          r + ch.fifth, r + 12, r + ch.third, r + ch.fifth];
}

// Deterministic RNG so re-renders are stable
function makeRng(seed = 13) {
  let s = seed >>> 0;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}
const rand = makeRng(0xbeef);

progress.begin({ type: "audio", label: "oskie jam (granular)" });
progress.update(0);

// ── load source + bass + drums ───────────────────────────────────────
console.log("loading samples ...");
const { samples: bassBuf, sampleRate: bassSr } = readWavMono(BASS_PATH);
const { samples: drumsFull, sampleRate: drumsSr } = readWavMono(DRUMS_PATH);
for (const [name, sr] of [["bass", bassSr], ["drums", drumsSr]]) {
  if (sr !== SR) throw new Error(`${name} SR ${sr} ≠ ${SR}`);
}
const BASS_LEN = bassBuf.length;
const drumStartIdx = Math.floor(LOOP_START_S * SR);
const loopN = Math.floor(LOOP_S * SR);
const drumLoop = drumsFull.subarray(drumStartIdx, drumStartIdx + loopN);
progress.update(8);

// ── grain helpers ────────────────────────────────────────────────────
function pickGrain(buf, startSec, durSec) {
  const a = Math.max(0, Math.floor(startSec * SR));
  const n = Math.floor(durSec * SR);
  const end = Math.min(a + n, buf.length);
  return buf.subarray(a, end);
}

// Linear-interp resample. rate > 1 → higher pitch + shorter duration.
function pitchShift(grain, rate) {
  const outN = Math.max(1, Math.floor(grain.length / rate));
  const out = new Float32Array(outN);
  for (let i = 0; i < outN; i++) {
    const x = i * rate;
    const i0 = Math.floor(x);
    const i1 = i0 + 1;
    if (i1 >= grain.length) break;
    const f = x - i0;
    out[i] = grain[i0] * (1 - f) + grain[i1] * f;
  }
  return out;
}

// Apply a Hann window in-place (good for crossfade-friendly overlap-add).
function hannInPlace(buf) {
  const n = buf.length;
  if (n < 2) return buf;
  for (let i = 0; i < n; i++) {
    buf[i] *= 0.5 - 0.5 * Math.cos((2 * Math.PI * i) / (n - 1));
  }
  return buf;
}

// AR envelope (attack secs, release secs); rest is full gain.
function arEnvelope(buf, attack, release) {
  const n = buf.length;
  const aN = Math.min(n, Math.max(1, Math.floor(attack * SR)));
  const rN = Math.min(n, Math.max(1, Math.floor(release * SR)));
  for (let i = 0; i < aN; i++) buf[i] *= i / aN;
  for (let i = 0; i < rN; i++) buf[n - rN + i] *= 1 - i / rN;
  return buf;
}

// Soft saturation (tanh-ish) — industrial bite for stab voices.
function softSat(buf, drive = 1.6) {
  for (let i = 0; i < buf.length; i++) {
    buf[i] = Math.tanh(buf[i] * drive) / Math.tanh(drive);
  }
  return buf;
}

// 1-pole lowpass — keep synthesized voices inside the source's
// spectral envelope (rolloff85 ≈ 2.8 kHz). Stops pitched-up bass
// grains from spitting harmonics that sit above the source's natural
// band, which is what made the synth bells feel bolted-on.
function lowpass(buf, cutoffHz) {
  const a = Math.exp((-2 * Math.PI * cutoffHz) / SR);
  let y = 0;
  for (let i = 0; i < buf.length; i++) {
    y = (1 - a) * buf[i] + a * y;
    buf[i] = y;
  }
  return buf;
}

// Sidechain ducking — multi-track awareness. Drives a fast attack /
// slow release envelope follower from `source` and uses it to duck
// `target` when source is loud. This is what gives a dance mix the
// "breathing" feel — pad/bells get out of the drum's way.
function sidechain(target, source,
                   attackS = 0.005, releaseS = 0.18,
                   threshold = 0.05, depth = 0.55) {
  const aAtk = Math.exp(-1 / (attackS * SR));
  const aRel = Math.exp(-1 / (releaseS * SR));
  let env = 0;
  const n = Math.min(target.length, source.length);
  for (let i = 0; i < n; i++) {
    const v = Math.abs(source[i]);
    const a = v > env ? aAtk : aRel;
    env = a * env + (1 - a) * v;
    if (env > threshold) {
      const excess = env - threshold;
      const duck = Math.max(1 - depth, 1 - excess * (depth * 6));
      target[i] *= duck;
    }
  }
}

// Time-varying 1-pole lowpass — filter sweep applied in-place across
// [startSec, endSec]. Sweep is exponential in cutoff Hz (perceptually
// linear). A short pre-roll primes the filter state so there's no
// click at the sweep boundary.
function sweepLP(buf, startSec, endSec, hzStart, hzEnd) {
  const a0 = Math.max(0, Math.floor(startSec * SR));
  const a1 = Math.min(buf.length, Math.floor(endSec * SR));
  const span = a1 - a0;
  if (span <= 1) return;
  const log0 = Math.log(hzStart);
  const log1 = Math.log(hzEnd);
  let y = 0;
  const pre = Math.max(0, a0 - Math.floor(0.005 * SR));
  const aStart = Math.exp((-2 * Math.PI * hzStart) / SR);
  for (let p = pre; p < a0; p++) {
    y = (1 - aStart) * buf[p] + aStart * y;
  }
  for (let i = a0; i < a1; i++) {
    const t = (i - a0) / span;
    const fc = Math.exp(log0 + (log1 - log0) * t);
    const a = Math.exp((-2 * Math.PI * fc) / SR);
    y = (1 - a) * buf[i] + a * y;
    buf[i] = y;
  }
}

// ── granular voices ──────────────────────────────────────────────────
const RATE = (midi) => Math.pow(2, (midi - BASS_F0_MIDI) / 12);

// PAD: overlap-add bass-stem grains, pitched to midi, filling durSec.
function gPad(midi, durSec, gain = 0.5) {
  const rate = RATE(midi);
  const grainLen = 0.30;
  const advance = 0.15;             // 50% overlap
  const out = new Float32Array(Math.floor(durSec * SR));
  let pos = 0;
  while (pos < out.length) {
    const start = 1.0 + rand() * Math.max(0.1, BASS_LEN / SR - grainLen - 2.0);
    const g = new Float32Array(pickGrain(bassBuf, start, grainLen));
    hannInPlace(g);
    const s = pitchShift(g, rate);
    for (let i = 0; i < s.length && pos + i < out.length; i++) {
      out[pos + i] += s[i] * gain;
    }
    pos += Math.floor(advance * SR);
  }
  // shape and band-limit so it blends with the source's spectrum
  arEnvelope(out, 0.05, 0.10);
  lowpass(out, 2200);
  return out;
}

// STAB: pitched grain from BASS stem (stable E1 fundamental → clean
// pitch shift), short envelope + soft drive. Industrial chord stab
// that still shares the source's timbre because the material IS the
// source's harmonic body.
function gStab(midi, durSec, gain = 0.6) {
  const rate = RATE(midi);
  const grainLen = 0.22;
  const start = 0.5 + rand() * Math.max(0.1, BASS_LEN / SR - grainLen - 1.0);
  const g = new Float32Array(pickGrain(bassBuf, start, grainLen));
  const s = pitchShift(g, rate);
  lowpass(s, 2800);
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(Math.max(n, s.length));
  for (let i = 0; i < s.length; i++) out[i] = s[i];
  arEnvelope(out, 0.003, Math.min(0.08, durSec * 0.6));
  softSat(out, 1.8);
  for (let i = 0; i < out.length; i++) out[i] *= gain;
  return out.subarray(0, n);
}

// LOW SINE BELL — pure additive bell with inharmonic partials, tuned
// to low MIDI notes (E2–B3 territory). Different tone family from the
// granular voices: clean, warm, sustained — gives the mix a sine spine
// the gritty grains can rub against.
function gLowSineBell(midi, durSec, gain = 0.45) {
  const freq = 440 * Math.pow(2, (midi - 69) / 12);
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  // Bell partials: [ratio_to_fundamental, amp, decay_seconds]
  const parts = [
    [1.0,  1.00, durSec * 0.55],
    [2.01, 0.42, durSec * 0.30],
    [2.76, 0.22, durSec * 0.18],
    [4.10, 0.11, durSec * 0.10],
  ];
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    let s = 0;
    for (const [r, a, d] of parts) {
      s += a * Math.sin(2 * Math.PI * freq * r * t) * Math.exp(-t / d);
    }
    out[i] = s * gain * 0.5;
  }
  const aN = Math.min(n, Math.floor(0.002 * SR));
  for (let i = 0; i < aN; i++) out[i] *= i / aN;
  return out;
}

// DRUM BELL — grab a drum-stem grain (centroid ~1.2 kHz, very short),
// pitch-shift it onto a target MIDI note, apply a bell envelope. The
// drums literally morph into pitched ringing — the "drum-into-bell"
// move. Different harmonic content from gBell (which uses bass.wav)
// so the two bell families read as distinct instruments.
function gDrumBell(midi, durSec, gain = 0.4) {
  // Drum hits sit around 1.0–1.5 kHz; treat as ~MIDI 84 (C6 ≈ 1047 Hz).
  const SOURCE_MIDI = 84;
  const rate = Math.pow(2, (midi - SOURCE_MIDI) / 12);
  const grainLen = 0.08;
  const dStart = LOOP_START_S + rand() * (LOOP_S - grainLen - 0.1);
  const a = Math.floor(dStart * SR);
  const g = new Float32Array(drumsFull.subarray(a, a + Math.floor(grainLen * SR)));
  const s = pitchShift(g, rate);
  lowpass(s, 3500);

  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < s.length && i < n; i++) out[i] = s[i];
  // bell envelope: fast attack, bright head, exp body decay
  const head = Math.min(n, Math.floor(0.04 * SR));
  const tau = durSec * 0.38;
  for (let i = 0; i < n; i++) {
    const headBoost = i < head ? 1 + (1.1 * (1 - i / head)) : 1;
    out[i] *= headBoost * Math.exp(-(i / SR) / tau) * gain;
  }
  return out;
}

// BELL: pitched BASS grain shaped like a marimba — fast attack, bright
// initial transient, exponential body decay, lowpass to stay in band.
// Drawn from the bass stem so the underlying material has a clean pitch
// for shifting; pitching that material up gives a wooden bell tone that
// blends with the rest of the mix instead of sitting above it.
function gBell(midi, durSec, gain = 0.5) {
  const rate = RATE(midi);
  const grainLen = 0.18;
  const start = 0.5 + rand() * Math.max(0.1, BASS_LEN / SR - grainLen - 1.0);
  const g = new Float32Array(pickGrain(bassBuf, start, grainLen));
  const s = pitchShift(g, rate);
  lowpass(s, 3200);

  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < s.length && i < n; i++) out[i] = s[i];

  // Marimba-style envelope: fast 1ms attack, brighter 60ms "head"
  // decay (modeling the mallet-impulse + upper modes fading first),
  // then slower body decay over the full durSec for the warm tail.
  const head = Math.min(n, Math.floor(0.06 * SR));
  const tau = durSec * 0.32;
  for (let i = 0; i < n; i++) {
    const headBoost = i < head ? 1 + (1.4 * (1 - i / head)) : 1;
    out[i] *= headBoost * Math.exp(-(i / SR) / tau) * gain;
  }
  const aN = Math.min(n, Math.floor(0.001 * SR));
  for (let i = 0; i < aN; i++) out[i] *= i / aN;
  return out;
}

// ── per-voice buffers (mixed → stereo at the end) ────────────────────
const TOTAL_BARS_PLANNED = 36;
const totalN = Math.floor(TOTAL_BARS_PLANNED * BAR * SR) + Math.floor(2.0 * SR);
const padBuf = new Float32Array(totalN);
const stabBuf = new Float32Array(totalN);
const bellBuf = new Float32Array(totalN);
const sineBellBuf = new Float32Array(totalN);  // low sine bells
const drumBellBuf = new Float32Array(totalN);  // drum-grain pitched bells
const drumsBuf = new Float32Array(totalN);

function placeInto(target, src, startSec) {
  const off = Math.floor(startSec * SR);
  for (let i = 0; i < src.length && off + i < totalN; i++) target[off + i] += src[i];
}
// Tile the 4-bar drum loop across [startBar, startBar+nBars) — but
// retrigger only every LOOP_BARS, so the loop plays cleanly instead of
// overlapping itself once per bar (the old bug — drums fused into one
// dense mass). `variants` is an optional per-loop schedule:
//   "full"        — full 4-bar loop
//   "mute1stBar"  — silence the first bar of the loop, drums kick in on bar 2
//   "muteLastBar" — play bars 1-3, drop bar 4 for breathing room
//   "halfSilent"  — first 2 bars only, second 2 bars silent
//   "stutter1"    — first bar repeated 4 times
//   "reverse"     — loop played backwards
//   "silent"      — no drums in this 4-bar slot
function tileDrums(startBar, nBars, gain = 1.0, variants = null) {
  const loopN = drumLoop.length;
  const barN = Math.floor(BAR * SR);
  const loops = Math.ceil(nBars / LOOP_BARS);
  for (let lp = 0; lp < loops; lp++) {
    const v = variants ? variants[lp % variants.length] : "full";
    if (v === "silent") continue;
    const off = Math.floor((startBar + lp * LOOP_BARS) * BAR * SR);
    const remaining = (nBars - lp * LOOP_BARS) * barN;
    const writeN = Math.min(loopN, remaining);
    if (v === "full") {
      for (let i = 0; i < writeN && off + i < totalN; i++)
        drumsBuf[off + i] += drumLoop[i] * gain;
    } else if (v === "mute1stBar") {
      for (let i = barN; i < writeN && off + i < totalN; i++)
        drumsBuf[off + i] += drumLoop[i] * gain;
    } else if (v === "muteLastBar") {
      const stop = Math.min(writeN, 3 * barN);
      for (let i = 0; i < stop && off + i < totalN; i++)
        drumsBuf[off + i] += drumLoop[i] * gain;
    } else if (v === "halfSilent") {
      const stop = Math.min(writeN, 2 * barN);
      for (let i = 0; i < stop && off + i < totalN; i++)
        drumsBuf[off + i] += drumLoop[i] * gain;
    } else if (v === "stutter1") {
      for (let rep = 0; rep < 4; rep++) {
        const rOff = off + rep * barN;
        for (let i = 0; i < barN && rOff + i < totalN; i++)
          drumsBuf[rOff + i] += drumLoop[i] * gain;
      }
    } else if (v === "reverse") {
      for (let i = 0; i < writeN && off + i < totalN; i++)
        drumsBuf[off + i] += drumLoop[loopN - 1 - i] * gain;
    }
  }
}

// ── arrangement ──────────────────────────────────────────────────────
let cursor = 0;

function intro(nBars = 4) {
  tileDrums(cursor, nBars, 0.4, ["halfSilent"]);  // drums kick in on bar 3
  for (let bar = 0; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = CHORDS_A[bar % 4];
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.35), t);
    placeInto(sineBellBuf, gLowSineBell(chordRoot(ch, 3), BAR * 0.9, 0.40), t);
    // granular bell sparkle: chord octave/fifth alternating
    const sparkleNote = chordRoot(ch, 5) + (bar % 2 ? ch.fifth : 0);
    placeInto(bellBuf, gBell(sparkleNote, BEAT * 2.5, 0.32), t + 1.5 * BEAT);
    // teaser drum-bells — drum morphs to chord's third / root
    if (bar === 1) placeInto(drumBellBuf,
      gDrumBell(chordRoot(ch, 4) + ch.third, BEAT * 2, 0.35), t + 3 * BEAT);
    if (bar === 3) placeInto(drumBellBuf,
      gDrumBell(chordRoot(ch, 4), BEAT * 1.5, 0.40), t + 2.5 * BEAT);
  }
  cursor += nBars;
}

function verse(nBars = 8, progression = CHORDS_A) {
  // verse drums: first loop full, second mutes last bar for breathing room
  tileDrums(cursor, nBars, 0.9, ["full", "muteLastBar"]);
  for (let bar = 0; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = progression[bar % 4];
    const phrase = Math.floor(bar / 4) % 2;
    if (phrase === 0) {
      placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.38), t);
      placeInto(sineBellBuf, gLowSineBell(chordRoot(ch, 3), BAR * 0.9, 0.38), t);
      if (bar % 2 === 1) placeInto(sineBellBuf,
        gLowSineBell(chordRoot(ch, 3) + ch.fifth, BAR * 0.7, 0.22), t + BEAT);
      placeInto(bellBuf, gBell(chordRoot(ch, 4), BEAT * 1.6, 0.28), t);
    } else {
      placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.42), t);
      placeInto(padBuf, gPad(chordRoot(ch, 3) + ch.third, BAR, 0.20), t);
      const echoes = chordTriad(ch, 4).concat([chordRoot(ch, 4)]);
      for (let i = 0; i < 4; i++) {
        if (rand() < 0.55) {
          placeInto(drumBellBuf,
            gDrumBell(echoes[i], BEAT * 0.9, 0.30),
            t + i * BEAT + BEAT * 0.5);
        }
      }
      placeInto(sineBellBuf,
        gLowSineBell(chordRoot(ch, 3), BAR * 0.95, 0.34), t);
    }
  }
  cursor += nBars;
}

function chorus(nBars = 8, intensity = 1.0, progression = CHORDS_A) {
  tileDrums(cursor, nBars, 1.0 * intensity, ["full", "full"]);
  for (let bar = 0; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = progression[bar % 4];
    const arpAsc = chordArp(ch, 4);
    const arpDesc = [...arpAsc].reverse();
    const dir = Math.floor(bar / 2) % 2 === 0 ? arpAsc : arpDesc;
    // pad triad in octave 3
    const triad = chordTriad(ch, 3);
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.40 * intensity), t);
    placeInto(padBuf, gPad(triad[1], BAR, 0.22 * intensity), t);
    placeInto(padBuf, gPad(triad[2], BAR, 0.22 * intensity), t);
    placeInto(sineBellBuf,
      gLowSineBell(chordRoot(ch, 3), BAR * 0.95, 0.36 * intensity), t);
    // stabs flip placement halfway
    const stabBeats = bar < nBars / 2 ? [1, 3] : [0, 2.5];
    for (const beat of stabBeats) {
      placeInto(stabBuf, gStab(triad[0], BEAT * 0.6, 0.50 * intensity), t + beat * BEAT);
      placeInto(stabBuf, gStab(triad[2], BEAT * 0.6, 0.38 * intensity), t + beat * BEAT);
    }
    // 8th-note granular bell arp through chord tones
    for (let i = 0; i < 8; i++) {
      placeInto(bellBuf,
        gBell(dir[i % dir.length], BEAT * 0.85, 0.26 * intensity),
        t + i * (BEAT / 2));
    }
    // bars 3 & 7 — drum-bell climb hitting the NEXT chord (turnaround)
    if (bar === 3 || bar === 7) {
      const nextCh = progression[(bar + 1) % 4];
      const climb = chordTriad(nextCh, 4).concat([chordRoot(nextCh, 5)]);
      for (let i = 0; i < 4; i++) {
        placeInto(drumBellBuf,
          gDrumBell(climb[i], BEAT * 0.7, 0.38 * intensity),
          t + i * BEAT);
      }
    }
  }
  cursor += nBars;
}

function bridge(nBars = 4, progression = CHORDS_A) {
  // bridge drums: only first 2 bars play, lets the wow show
  tileDrums(cursor, nBars, 0.55, ["halfSilent"]);
  const t0 = cursor * BAR;
  // walking pad: each bar gets its own chord triad
  for (let bar = 0; bar < nBars; bar++) {
    const t = t0 + bar * BAR;
    const ch = progression[bar % 4];
    const triad = chordTriad(ch, 3);
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.45), t);
    placeInto(padBuf, gPad(triad[1], BAR, 0.30), t);
    placeInto(padBuf, gPad(triad[2], BAR, 0.30), t);
    placeInto(sineBellBuf,
      gLowSineBell(chordRoot(ch, 2), BAR, 0.42), t);
  }
  // 16-step drum-bell arp walks each chord's tones (root → 5 → 8 → 3)
  for (let bar = 0; bar < nBars; bar++) {
    const ch = progression[bar % 4];
    const r = chordRoot(ch, 4);
    const tones = [r, r + ch.fifth, r + 12, r + ch.third];
    for (let s = 0; s < 4; s++) {
      placeInto(drumBellBuf,
        gDrumBell(tones[s], BEAT * 1.2, 0.38),
        t0 + bar * BAR + s * BEAT);
    }
  }
  // granular bell anchors on chord root / fifth per bar
  for (let bar = 0; bar < nBars; bar++) {
    const ch = progression[bar % 4];
    const t = (cursor + bar) * BAR;
    placeInto(bellBuf, gBell(chordRoot(ch, 5), BEAT * 3, 0.34), t + 0 * BEAT);
    placeInto(bellBuf,
      gBell(chordRoot(ch, 4) + ch.fifth, BEAT * 3, 0.28), t + 2 * BEAT);
  }
  cursor += nBars;
}

function outro(nBars = 4, progression = CHORDS_A) {
  tileDrums(cursor, nBars, 0.65, ["muteLastBar"]);
  for (let bar = 0; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = progression[bar % 4];
    const triad = chordTriad(ch, 3);
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.45), t);
    placeInto(padBuf, gPad(triad[1], BAR, 0.28), t);
    placeInto(padBuf, gPad(triad[2], BAR, 0.28), t);
    placeInto(sineBellBuf, gLowSineBell(chordRoot(ch, 2), BAR, 0.48), t);
    placeInto(sineBellBuf,
      gLowSineBell(chordRoot(ch, 3), BAR * 0.9, 0.30), t);
  }
  // closing bell + drum-bell tag on the last chord
  const tStart = cursor * BAR;
  const lastCh = progression[(nBars - 1) % 4];
  placeInto(bellBuf, gBell(chordRoot(lastCh, 5), 4.0, 0.42), tStart);
  placeInto(drumBellBuf,
    gDrumBell(chordRoot(lastCh, 4), BEAT * 4, 0.32), tStart + 3 * BAR);
  cursor += nBars;
}

console.log("rendering intro ...");
intro();    progress.update(22);
console.log("rendering verse ...");
verse();    progress.update(38);
console.log("rendering chorus 1 ...");
chorus(8, 0.9); progress.update(58);
console.log("rendering bridge ...");
bridge();   progress.update(70);
console.log("rendering chorus 2 ...");
chorus(8, 1.0, CHORDS_B); progress.update(85);
console.log("rendering outro ...");
outro();    progress.update(92);

// ── filter sweeps (section-shaped automation) ────────────────────────
// Bar boundaries (TOTAL_BARS_PLANNED = 36): 0 intro, 4 V1, 12 CH1,
// 20 bridge, 24 CH2, 32 outro, 36 end.
// 1) intro drums fade open (250 Hz → 16 kHz across the 4-bar intro)
sweepLP(drumsBuf, 0 * BAR, 4 * BAR, 250, 16000);
// 2) end-of-CH1 pad lift into the bridge (last bar of chorus 1)
sweepLP(padBuf, 19 * BAR, 20 * BAR, 2200, 4500);
// 3) bridge wow — pad sweeps closed then back open
sweepLP(padBuf, 20 * BAR, 22 * BAR, 4500, 500);
sweepLP(padBuf, 22 * BAR, 24 * BAR, 500, 3800);
// 4) outro drums sweep closed (16 kHz → 250 Hz across the 4-bar outro)
sweepLP(drumsBuf, 32 * BAR, 36 * BAR, 16000, 250);

// ── multi-track awareness: sidechain ducking from drums ──────────────
// Pad / sineBell / bell duck slightly under each drum hit so the kit
// pokes through. Stabs and drum-bells ride on top — no duck.
sidechain(padBuf,      drumsBuf, 0.005, 0.18, 0.045, 0.45);
sidechain(sineBellBuf, drumsBuf, 0.005, 0.16, 0.045, 0.40);
sidechain(bellBuf,     drumsBuf, 0.004, 0.14, 0.050, 0.30);

// ── mix to stereo ────────────────────────────────────────────────────
const left = new Float32Array(totalN);
const right = new Float32Array(totalN);
function pan(src, l, r) {
  for (let i = 0; i < totalN; i++) {
    left[i]  += src[i] * l;
    right[i] += src[i] * r;
  }
}
pan(drumsBuf,    1.00, 1.00);
pan(padBuf,      0.95, 0.95);
pan(sineBellBuf, 0.95, 0.95);
pan(stabBuf,     1.00, 0.85);    // stabs lean left
pan(bellBuf,     0.85, 1.00);    // granular bells lean right
pan(drumBellBuf, 0.90, 0.95);    // drum-bells slight right of center

// trim
const finalN = Math.floor(TARGET_S * SR);
const outL = left.subarray(0, finalN);
const outR = right.subarray(0, finalN);

// soft limit (linked L/R)
let peak = 0;
for (let i = 0; i < finalN; i++) {
  const m = Math.max(Math.abs(outL[i]), Math.abs(outR[i]));
  if (m > peak) peak = m;
}
if (peak > 0.95) {
  const g = 0.95 / peak;
  for (let i = 0; i < finalN; i++) { outL[i] *= g; outR[i] *= g; }
}

// fade in/out
const fi = Math.floor(0.3 * SR);
const fo = Math.floor(2.0 * SR);
for (let i = 0; i < fi; i++) { const g = i / fi; outL[i] *= g; outR[i] *= g; }
for (let i = 0; i < fo; i++) {
  const g = Math.pow(1 - i / fo, 1.4);
  outL[finalN - fo + i] *= g; outR[finalN - fo + i] *= g;
}
progress.update(97);

// ── write 16-bit stereo WAV ──────────────────────────────────────────
function writeWavStereo16(path, l, r, sr) {
  const n = l.length;
  const bytes = n * 4;
  const header = Buffer.alloc(44);
  header.write("RIFF", 0);
  header.writeUInt32LE(36 + bytes, 4);
  header.write("WAVE", 8);
  header.write("fmt ", 12);
  header.writeUInt32LE(16, 16);
  header.writeUInt16LE(1, 20);
  header.writeUInt16LE(2, 22);
  header.writeUInt32LE(sr, 24);
  header.writeUInt32LE(sr * 4, 28);
  header.writeUInt16LE(4, 32);
  header.writeUInt16LE(16, 34);
  header.write("data", 36);
  header.writeUInt32LE(bytes, 40);
  const pcm = Buffer.alloc(bytes);
  for (let i = 0; i < n; i++) {
    let lf = l[i] > 1 ? 1 : (l[i] < -1 ? -1 : l[i]);
    let rf = r[i] > 1 ? 1 : (r[i] < -1 ? -1 : r[i]);
    pcm.writeInt16LE((lf * 32767) | 0, i * 4);
    pcm.writeInt16LE((rf * 32767) | 0, i * 4 + 2);
  }
  writeFileSync(path, Buffer.concat([header, pcm]));
}
writeWavStereo16(OUT_PATH, outL, outR, SR);

progress.update(100);
progress.end();
console.log(`✓ ${OUT_PATH}  (${(finalN/SR).toFixed(2)}s)`);
