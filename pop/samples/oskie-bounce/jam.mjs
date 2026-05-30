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
import { cachedSamples } from "../../lib/freesound.mjs";
import { renderSkrill } from "../../dance/synths/skrill.mjs";
import * as progress from "../../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SRC_PATH = resolve(HERE, "source.wav");
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
// Both progressions collapse to static E so our voices stay in the
// source's tonality (oskie's mix is a fixed E pedal). Variation now
// comes from voicing + glitches + filter sweeps, not chord changes.
const CHORDS_A = [CHORDS.E];
const CHORDS_B = [CHORDS.E];

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
const { samples: srcFull,   sampleRate: srcSr   } = readWavMono(SRC_PATH);
const { samples: bassBuf,   sampleRate: bassSr  } = readWavMono(BASS_PATH);
const { samples: drumsFull, sampleRate: drumsSr } = readWavMono(DRUMS_PATH);
for (const [name, sr] of [["src", srcSr], ["bass", bassSr], ["drums", drumsSr]]) {
  if (sr !== SR) throw new Error(`${name} SR ${sr} ≠ ${SR}`);
}
const BASS_LEN = bassBuf.length;
const drumStartIdx = Math.floor(LOOP_START_S * SR);
const loopN = Math.floor(LOOP_S * SR);
// Extract 80ms of TRAILING audio past the theoretical loop end so
// adjacent tiles can LINEAR crossfade for seamless tiling. Linear
// is correct here because the two sides of the seam are correlated
// (same musical content) — equal-power would give a √2 amplitude
// swell at the midpoint.
const TILE_TAIL_N = Math.floor(0.080 * SR);
const drumLoop = drumsFull.subarray(drumStartIdx, drumStartIdx + loopN + TILE_TAIL_N);
const srcLoop  = srcFull.subarray(drumStartIdx,   drumStartIdx + loopN + TILE_TAIL_N);
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

// High-shelf brightener — adds the high band of `buf` back into itself
// at `lift`. The high band is computed as (signal − lowpass(signal)).
// Used to brighten oskie's drums without re-EQ'ing the whole mix.
function brightenDrums(buf, lift = 0.55, hpFreq = 2200) {
  const lp = new Float32Array(buf);
  lowpass(lp, hpFreq);
  for (let i = 0; i < buf.length; i++) {
    const hi = buf[i] - lp[i];
    buf[i] += hi * lift;
  }
}

// Glitchify — randomly micro-reverse / stutter / bitcrush small chunks
// in-place. Used on the bell layer to add the "scratched" character.
function glitchify(buf, prob = 0.10, chunkSec = 0.045) {
  const chunkN = Math.max(1, Math.floor(chunkSec * SR));
  for (let i = chunkN; i + chunkN < buf.length; i += chunkN) {
    if (rand() >= prob) continue;
    const which = Math.floor(rand() * 3);
    if (which === 0) {
      // micro-reverse
      const tmp = new Float32Array(chunkN);
      for (let j = 0; j < chunkN; j++) tmp[j] = buf[i + j];
      for (let j = 0; j < chunkN; j++) buf[i + j] = tmp[chunkN - 1 - j];
    } else if (which === 1) {
      // micro-stutter: repeat first half over second half
      const half = chunkN >> 1;
      for (let j = 0; j < half; j++) buf[i + half + j] = buf[i + j];
    } else {
      // soft bitcrush (6-bit — gentler than the 4-bit version that
      // was popping; still audibly grainy without the step pops)
      for (let j = 0; j < chunkN; j++) {
        buf[i + j] = Math.round(buf[i + j] * 32) / 32;
      }
    }
  }
}

// Detect snare-like hits in a drum loop: highpass to isolate the snare
// band, build an envelope, peak-pick above a threshold with a minimum
// gap. Returns sample indices within the loop.
function detectSnares(loop) {
  const lp = new Float32Array(loop);
  lowpass(lp, 1500);
  const hp = new Float32Array(loop.length);
  for (let i = 0; i < loop.length; i++) hp[i] = Math.abs(loop[i] - lp[i]);
  // 8ms moving-avg envelope (sliding window)
  const winN = Math.max(1, Math.floor(0.008 * SR));
  const env = new Float32Array(loop.length);
  let acc = 0;
  for (let i = 0; i < winN && i < loop.length; i++) acc += hp[i];
  for (let i = 0; i < loop.length; i++) {
    if (i + winN < loop.length) acc += hp[i + winN];
    if (i - winN - 1 >= 0) acc -= hp[i - winN - 1];
    env[i] = acc / winN;
  }
  // peak-pick: at least 80 ms apart, envelope above threshold
  const minGap = Math.floor(0.08 * SR);
  const peaks = [];
  let last = -minGap;
  let maxEnv = 0;
  for (let i = 0; i < env.length; i++) if (env[i] > maxEnv) maxEnv = env[i];
  const threshold = maxEnv * 0.35;
  for (let i = 1; i < env.length - 1; i++) {
    if (env[i] < threshold) continue;
    if (i - last < minGap) continue;
    if (env[i] >= env[i - 1] && env[i] >= env[i + 1]) {
      peaks.push(i);
      last = i;
    }
  }
  return peaks;
}

// Master bus — tilt EQ (gentle low cut, gentle high lift via complementary
// shelf), soft glue (tanh), then HF de-hiss (lowpass at 17 kHz). Applied
// after sidechain + pan, before fade in/out. Stereo, linked.
function masterBus(l, r) {
  const n = l.length;
  // gentle low cut at ~40 Hz to remove rumble
  const aHP = Math.exp(-2 * Math.PI * 40 / SR);
  let yLpL = 0, yLpR = 0;
  for (let i = 0; i < n; i++) {
    yLpL = (1 - aHP) * l[i] + aHP * yLpL;
    yLpR = (1 - aHP) * r[i] + aHP * yLpR;
    l[i] -= yLpL;
    r[i] -= yLpR;
  }
  // soft glue (was 1.10 — squashed transients; 1.03 is "barely there")
  const drive = 1.03;
  const norm = Math.tanh(drive);
  for (let i = 0; i < n; i++) {
    l[i] = (Math.tanh(l[i] * drive) / norm) * 0.97;
    r[i] = (Math.tanh(r[i] * drive) / norm) * 0.97;
  }
  // very gentle HF taper at 19.5 kHz (was 17 kHz — softened drums)
  lowpass(l, 19500);
  lowpass(r, 19500);
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
const padBuf       = new Float32Array(totalN);
const stabBuf      = new Float32Array(totalN);
const bellBuf      = new Float32Array(totalN);
const sineBellBuf  = new Float32Array(totalN);  // low sine bells
const drumBellBuf  = new Float32Array(totalN);  // drum-grain pitched bells
const snareBellBuf = new Float32Array(totalN);  // short sinebells on snares
const hatBuf       = new Float32Array(totalN);  // high-end hats + sweeps
const gobbleBuf    = new Float32Array(totalN);  // turkey gobbles
const smgBuf       = new Float32Array(totalN);  // submachine guns
const kickBuf      = new Float32Array(totalN);  // cyberkicks at drops
const dogBuf       = new Float32Array(totalN);  // police dogs
const percBuf      = new Float32Array(totalN);  // claps + tom fills + snare rolls
const scratchBuf   = new Float32Array(totalN);  // turntable scratches of source
const pianoBuf     = new Float32Array(totalN);  // glitched grand piano
const churchBuf    = new Float32Array(totalN);  // church bell chords
const topBellBuf   = new Float32Array(totalN);  // top-level sine bells
const voxBuf       = new Float32Array(totalN);  // jeffrey-pvc sung vowels
const chainsawBuf  = new Float32Array(totalN);  // harmonized chainsaws
const whistleBuf   = new Float32Array(totalN);  // coach whistle
const shockBuf     = new Float32Array(totalN);  // UT2004 shock rifle accents
const tornadoBuf   = new Float32Array(totalN);  // tornado atmospheres
const laughBuf     = new Float32Array(totalN);  // sitcom laugh track
const reverbBuf    = new Float32Array(totalN);  // wet reverb send for drops
const loopBuf      = new Float32Array(totalN);  // tiled source.wav (his mix)
const drumsBuf     = new Float32Array(totalN);  // tiled drum stem (sidechain key, not output)

// High-end hat — short HP-filtered noise burst. `hpCutoff` controls
// the "pitch" (lower = darker / lower-pitched, higher = brighter).
function gHat(durSec = 0.045, gain = 0.18, hpCutoff = 6000) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    out[i] = (rand() * 2 - 1) * Math.exp(-i / (0.008 * SR));
  }
  // 1-pole HPF via subtract LP @ hpCutoff
  const lp = new Float32Array(out);
  lowpass(lp, hpCutoff);
  for (let i = 0; i < n; i++) out[i] = (out[i] - lp[i]) * gain;
  return out;
}

// Open hat — longer decay, brighter.
function gOpenHat(durSec = 0.18, gain = 0.16) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    out[i] = (rand() * 2 - 1) * Math.exp(-i / (0.05 * SR));
  }
  const lp = new Float32Array(out);
  lowpass(lp, 5000);
  for (let i = 0; i < n; i++) out[i] = (out[i] - lp[i]) * gain;
  return out;
}

// Downward filter sweep — white noise with bandpass collapsing from
// 8 kHz down to ~200 Hz, used to telegraph section transitions.
function gSweepDown(durSec = 1.2, gain = 0.16) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) out[i] = rand() * 2 - 1;
  // amplitude envelope: short rise + long fade
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const env = t < 0.05 ? t / 0.05 : Math.exp(-(t - 0.05) / (durSec * 0.4));
    out[i] *= env;
  }
  // time-varying LP from 8 kHz down to 200 Hz
  const log0 = Math.log(8000), log1 = Math.log(200);
  let y = 0;
  for (let i = 0; i < n; i++) {
    const t = i / (n - 1);
    const fc = Math.exp(log0 + (log1 - log0) * t);
    const a = Math.exp(-2 * Math.PI * fc / SR);
    y = (1 - a) * out[i] + a * y;
    out[i] = y * gain;
  }
  return out;
}

// REAL TURKEY CLIPS from freesound (vault-cached). Loaded once at
// startup; if any are available, gTurkey picks one at random per call,
// resamples to fit durSec / target pitch, fades and gains it. Falls
// back to the synth turkey if the cache is empty (no vault creds or
// nothing fetched yet).
function loadCachedClips(filter) {
  try {
    const paths = cachedSamples({ tagFilter: filter, prefer: "wav" });
    return paths.map((p) => {
      try {
        const { samples, sampleRate } = readWavMono(p);
        if (sampleRate === SR) return samples;
        const ratio = sampleRate / SR;
        const outN = Math.floor(samples.length / ratio);
        const out = new Float32Array(outN);
        for (let i = 0; i < outN; i++) {
          const x = i * ratio;
          const i0 = Math.floor(x); const i1 = i0 + 1;
          if (i1 >= samples.length) break;
          const f = x - i0;
          out[i] = samples[i0] * (1 - f) + samples[i1] * f;
        }
        return out;
      } catch { return null; }
    }).filter(Boolean);
  } catch { return []; }
}

const TURKEY_CLIPS = loadCachedClips("turkey");
// SMG = submachine guns: matches "submachine", "mp5", "machine_gun", "fnp90"
const SMG_CLIPS = [
  ...loadCachedClips("submachine"),
  ...loadCachedClips("mp5"),
  ...loadCachedClips("machine_gun"),
  ...loadCachedClips("fnp90"),
];
const KICK_CLIPS = loadCachedClips("hardcorekick");
// Dogs — match by freesound `name` field which uses spaces, not the
// file-slug. "dog" alone catches the jazz-the-dog series + generic
// barks; we de-dupe by path to avoid double-loading.
const DOG_CLIPS = (() => {
  const seen = new Set();
  const out = [];
  for (const clip of [...loadCachedClips("dog"), ...loadCachedClips("bark")]) {
    // can't key by buffer identity reliably; use length+first-sample fingerprint
    const key = `${clip.length}:${clip[0]}:${clip[clip.length - 1] ?? 0}`;
    if (seen.has(key)) continue;
    seen.add(key);
    out.push(clip);
  }
  return out;
})();
const PIANO_CLIPS = [
  ...loadCachedClips("piano"),
  ...loadCachedClips("dissonant"),
];
const TORNADO_CLIPS = [
  ...loadCachedClips("tornado"),
  ...loadCachedClips("swirling"),
  ...loadCachedClips("vortex"),
];
const LAUGH_CLIPS = [
  ...loadCachedClips("laugh"),
];
// Jeffrey-PVC sung vowels (ah/iy/uw isolated + word forms ay/eye/owe/we)
// from pop/voice/lab/jeffrey-pvc/ — recorded with the ElevenLabs
// jeffrey voice we set up. Natural speaking pitch ≈ A2 (MIDI 45,
// ~110 Hz); we pitch-shift to chord tones in E major.
const JEFFREY_VOWELS = (() => {
  const dir = resolve(HERE, "../../voice/lab/jeffrey-pvc");
  const names = [
    "jeffrey-pvc-ah-iso.wav", "jeffrey-pvc-iy-iso.wav", "jeffrey-pvc-uw-iso.wav",
    "jeffrey-pvc-word-ay.wav", "jeffrey-pvc-word-eye.wav",
    "jeffrey-pvc-word-owe.wav", "jeffrey-pvc-word-we.wav",
  ];
  const out = [];
  for (const n of names) {
    try {
      const path = resolve(dir, n);
      const { samples, sampleRate } = readWavMono(path);
      if (sampleRate === SR) { out.push(samples); continue; }
      // resample to SR if needed
      const ratio = sampleRate / SR;
      const outN = Math.floor(samples.length / ratio);
      const buf = new Float32Array(outN);
      for (let i = 0; i < outN; i++) {
        const x = i * ratio;
        const i0 = Math.floor(x); const i1 = i0 + 1;
        if (i1 >= samples.length) break;
        const f = x - i0;
        buf[i] = samples[i0] * (1 - f) + samples[i1] * f;
      }
      out.push(buf);
    } catch { /* skip missing */ }
  }
  return out;
})();
console.log(`jeffrey vowels: ${JEFFREY_VOWELS.length}`);

const CHAINSAW_CLIPS = [
  ...loadCachedClips("chainsaw"),
  ...loadCachedClips("chain_saw"),
];
const WHISTLE_CLIPS = [
  ...loadCachedClips("coach_whistle"),
  ...loadCachedClips("referee_whistle"),
];
// UT2004 Shock Rifle from the hellsine lane — used on drop kicks +
// big moments for the rave snap.
const SHOCK_RIFLE = (() => {
  try {
    const path = resolve(HERE, "../../hellsine/samples/ut2004-shock-rifle.wav");
    const { samples, sampleRate } = readWavMono(path);
    if (sampleRate === SR) return samples;
    const ratio = sampleRate / SR;
    const outN = Math.floor(samples.length / ratio);
    const buf = new Float32Array(outN);
    for (let i = 0; i < outN; i++) {
      const x = i * ratio;
      const i0 = Math.floor(x), i1 = i0 + 1;
      if (i1 >= samples.length) break;
      const f = x - i0;
      buf[i] = samples[i0] * (1 - f) + samples[i1] * f;
    }
    return buf;
  } catch { return null; }
})();
console.log(`shock rifle: ${SHOCK_RIFLE ? "loaded" : "missing"}`);

const BELL_CLIPS = (() => {
  const seen = new Set();
  const out = [];
  for (const clip of [
    ...loadCachedClips("church"),
    ...loadCachedClips("cathedral"),
    ...loadCachedClips("campana"),
  ]) {
    const key = `${clip.length}:${clip[0]}:${clip[clip.length - 1] ?? 0}`;
    if (seen.has(key)) continue;
    seen.add(key);
    out.push(clip);
  }
  return out;
})();
console.log(`turkey: ${TURKEY_CLIPS.length}  smg: ${SMG_CLIPS.length}  kick: ${KICK_CLIPS.length}  dog: ${DOG_CLIPS.length}`);

// TURKEY GOBBLE — real freesound sample if available, else synth.
function gTurkey(durSec = 0.7, pitchHz = 280, gain = 0.35) {
  if (TURKEY_CLIPS.length > 0) return gTurkeyClip(durSec, pitchHz, gain);
  return gTurkeySynth(durSec, pitchHz, gain);
}

// Granular time-stretch — preserves pitch while changing duration.
// Picks 60 ms grains from `samples`, Hann-windows them, hops along the
// source at `srcHop`, and places them at `dstHop` in the output. For
// stretch > 1 dstHop > srcHop (slower output); for stretch < 1 (sped
// up), srcHop > dstHop.
function granularStretch(samples, stretchFactor) {
  if (stretchFactor === 1 || samples.length < 256) return new Float32Array(samples);
  const grainN = Math.floor(0.060 * SR);
  const dstHop = Math.floor(grainN / 2);
  const srcHop = Math.max(1, Math.floor(dstHop / stretchFactor));
  const numGrains = Math.max(1, Math.floor((samples.length - grainN) / srcHop));
  const outN = numGrains * dstHop + grainN;
  const out = new Float32Array(outN);
  const win = new Float32Array(grainN);
  for (let j = 0; j < grainN; j++) {
    win[j] = 0.5 - 0.5 * Math.cos((2 * Math.PI * j) / (grainN - 1));
  }
  for (let g = 0; g < numGrains; g++) {
    const srcStart = g * srcHop;
    const dstStart = g * dstHop;
    for (let j = 0; j < grainN && dstStart + j < outN; j++) {
      out[dstStart + j] += samples[srcStart + j] * win[j];
    }
  }
  return out;
}

// Real turkey clip pipeline: random clip → granular time-stretch to
// fill durSec at native pitch → pitch-shift via resample to target
// pitchHz → normalize → edge-fade. `stretch > 1` makes it droopier;
// independent of pitch since we stretch BEFORE resampling.
function gTurkeyClip(durSec, pitchHz, gain, stretch = 1) {
  const clip = TURKEY_CLIPS[Math.floor(rand() * TURKEY_CLIPS.length)];
  const rate = pitchHz / 280;
  // Pick a random window — gives variation across calls
  const naturalSec = clip.length / SR;
  const wantSrcSec = (durSec * rate) / stretch;
  const wantSrcN = Math.floor(wantSrcSec * SR);
  if (wantSrcN <= 0 || naturalSec <= 0) return new Float32Array(Math.floor(durSec * SR));
  const grabN = Math.min(wantSrcN, clip.length - 100);
  const maxStart = Math.max(0, clip.length - grabN - 100);
  const startIdx = Math.floor(rand() * maxStart);
  const seg = clip.subarray(startIdx, startIdx + grabN);
  // 1) time-stretch (preserves pitch)
  const stretched = stretch === 1 ? seg : granularStretch(seg, stretch);
  // 2) pitch-shift via resample (changes duration AND pitch)
  const shifted = pitchShift(stretched, rate);
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  const copyN = Math.min(n, shifted.length);
  let peak = 0;
  for (let i = 0; i < copyN; i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  // edge-fade
  const fadeN = Math.min(Math.floor(0.010 * SR), Math.floor(n / 6));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// JEFFREY VOWEL — pick a cached jeffrey-pvc phoneme, pitch-shift to
// target MIDI, time-stretch independently so we can sustain a vowel
// across multiple beats. Natural pitch ≈ A2 (MIDI 45). Index picks
// which vowel (0=ah, 1=iy, 2=uw, 3=ay, 4=eye, 5=owe, 6=we).
function gJeffreyVowel(midi, durSec = 1.0, vowelIdx = -1, gain = 0.5, stretch = 1) {
  if (JEFFREY_VOWELS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = vowelIdx >= 0 && vowelIdx < JEFFREY_VOWELS.length
    ? JEFFREY_VOWELS[vowelIdx]
    : JEFFREY_VOWELS[Math.floor(rand() * JEFFREY_VOWELS.length)];
  const NATURAL_MIDI = 45;
  const rate = Math.pow(2, (midi - NATURAL_MIDI) / 12);
  // pick a window in the middle of the clip (skip silent edges)
  const trimSec = 0.05;
  const usableSec = Math.max(0.05, clip.length / SR - 2 * trimSec);
  const wantSrcSec = (durSec * rate) / stretch;
  const grabSec = Math.min(wantSrcSec, usableSec);
  const grabN = Math.floor(grabSec * SR);
  const startIdx = Math.floor(trimSec * SR + rand() * Math.max(0, (usableSec - grabSec) * SR));
  const seg = clip.subarray(startIdx, startIdx + grabN);
  const stretched = stretch === 1 ? seg : granularStretch(seg, stretch);
  const shifted = pitchShift(stretched, rate);
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  // medium fades for vocal smoothness
  const fadeN = Math.min(Math.floor(0.025 * SR), Math.floor(n / 6));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// TORNADO — sustained roar / wind from cached sample, optional rate
// shift. Used as a tense atmospheric layer.
function gTornado(durSec = 3.0, gain = 0.30, rate = 1.0) {
  if (TORNADO_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = TORNADO_CLIPS[Math.floor(rand() * TORNADO_CLIPS.length)];
  const n = Math.floor(durSec * SR);
  const wantSrcN = Math.floor(n * rate);
  const grabN = Math.min(wantSrcN, clip.length - 100);
  const maxStart = Math.max(0, clip.length - grabN - 100);
  const startIdx = Math.floor(rand() * maxStart);
  const seg = clip.subarray(startIdx, startIdx + grabN);
  const shifted = rate === 1 ? seg : pitchShift(seg, rate);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  // Long fades — tornadoes don't start abruptly
  const fadeN = Math.min(Math.floor(0.20 * SR), Math.floor(n / 4));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// LAUGH TRACK — sitcom-style audience laugh, light pitch shift.
function gLaugh(durSec = 2.5, gain = 0.30, rate = 1.0) {
  if (LAUGH_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = LAUGH_CLIPS[Math.floor(rand() * LAUGH_CLIPS.length)];
  const n = Math.floor(durSec * SR);
  const wantSrcN = Math.floor(n * rate);
  const grabN = Math.min(wantSrcN, clip.length - 100);
  const seg = clip.subarray(0, grabN);
  const shifted = rate === 1 ? seg : pitchShift(seg, rate);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  const fadeN = Math.min(Math.floor(0.05 * SR), Math.floor(n / 8));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// SHOCK RIFLE — UT2004 shock-combo style snap. Single hit, optional
// pitch shift. Layers on top of cyberkicks for the rave-snap accent.
function gShockRifle(durSec = 0.6, gain = 0.55, rate = 1.0) {
  if (!SHOCK_RIFLE) return new Float32Array(Math.floor(durSec * SR));
  const n = Math.floor(durSec * SR);
  const wantSrcN = Math.floor(n * rate);
  const grabN = Math.min(wantSrcN, SHOCK_RIFLE.length - 100);
  const seg = SHOCK_RIFLE.subarray(0, grabN);
  const shifted = rate === 1 ? seg : pitchShift(seg, rate);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  const fadeN = Math.min(Math.floor(0.003 * SR), Math.floor(n / 16));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// COACH WHISTLE — short blast, optional pitch shift. Used to call
// section changes like a coach. Light touch — single hits at key spots.
function gWhistle(durSec = 0.4, gain = 0.45, rate = 1.0) {
  if (WHISTLE_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = WHISTLE_CLIPS[Math.floor(rand() * WHISTLE_CLIPS.length)];
  const n = Math.floor(durSec * SR);
  const wantSrcN = Math.floor(n * rate);
  const grabN = Math.min(wantSrcN, clip.length - 100);
  const startIdx = Math.floor(0.05 * SR);
  const seg = clip.subarray(startIdx, startIdx + grabN);
  const shifted = rate === 1 ? seg : pitchShift(seg, rate);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  const fadeN = Math.min(Math.floor(0.01 * SR), Math.floor(n / 8));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// CHAINSAW — pitched cached chainsaw, with random window for variety.
// Used as harmonized chord layer at the wild-climb peaks (root + 3rd
// + 5th + octave fired in parallel). Heavy saturation + edge fade.
function gChainsaw(midi, durSec = 1.5, gain = 0.3) {
  if (CHAINSAW_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = CHAINSAW_CLIPS[Math.floor(rand() * CHAINSAW_CLIPS.length)];
  // assume chainsaw natural pitch ≈ MIDI 52 (E3, ~165 Hz growl)
  const rate = Math.pow(2, (midi - 52) / 12);
  const wantSrcN = Math.floor(durSec * SR * rate);
  const grabN = Math.min(wantSrcN, clip.length - 100);
  const maxStart = Math.max(0, clip.length - grabN - 100);
  const startIdx = Math.floor(rand() * maxStart);
  const seg = clip.subarray(startIdx, startIdx + grabN);
  const shifted = pitchShift(seg, rate);
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  softSat(out, 1.5);
  // medium edge fades — chainsaws fade in/out naturally
  const fadeN = Math.min(Math.floor(0.02 * SR), Math.floor(n / 6));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// CHURCH BELL CHORD — pick a cached bell, layer 3 pitch-shifted copies
// at chord tones (root, fifth, octave) for harmonic body. Bells are
// inharmonic and long-decaying, so layering produces real chord ring.
function gChurchBellChord(rootMidi, durSec = 4.0, gain = 0.45) {
  if (BELL_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = BELL_CLIPS[Math.floor(rand() * BELL_CLIPS.length)];
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  // assume natural pitch ≈ MIDI 60 (C4); pitch-shift to each chord tone
  const NATURAL_MIDI = 60;
  const tones = [rootMidi, rootMidi + 7, rootMidi + 12];
  const grabSec = Math.min(durSec, (clip.length / SR) - 0.1);
  const grabN = Math.floor(grabSec * SR);
  const maxStart = Math.max(0, clip.length - grabN - 100);
  const startIdx = Math.floor(rand() * maxStart);
  const seg = clip.subarray(startIdx, startIdx + grabN);
  for (let k = 0; k < tones.length; k++) {
    const rate = Math.pow(2, (tones[k] - NATURAL_MIDI) / 12);
    const shifted = pitchShift(seg, rate);
    const layerGain = k === 0 ? 1.0 : (k === 1 ? 0.65 : 0.5);
    let peak = 0;
    for (let i = 0; i < Math.min(n, shifted.length); i++) {
      if (Math.abs(shifted[i]) > peak) peak = Math.abs(shifted[i]);
    }
    if (peak > 0) {
      const norm = (0.7 / peak) * layerGain;
      for (let i = 0; i < Math.min(n, shifted.length); i++) {
        out[i] += shifted[i] * norm;
      }
    }
  }
  // gain + long exp decay envelope
  const tau = durSec * 0.45;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    out[i] *= gain * Math.exp(-t / tau);
  }
  const fadeN = Math.min(Math.floor(0.005 * SR), Math.floor(n / 16));
  for (let i = 0; i < fadeN; i++) out[i] *= i / fadeN;
  return out;
}

// GLITCHED GRAND PIANO — cached piano stab, pitch-shifted to target
// MIDI, then aggressively destroyed: heavy bitcrush, micro-stutter via
// glitchify, soft-saturate. Different "sample feel" from the granular
// bass-stem bells — clearer pitch, more harmonic, but TORN UP.
function gPianoStab(midi, durSec = 0.5, gain = 0.4) {
  if (PIANO_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = PIANO_CLIPS[Math.floor(rand() * PIANO_CLIPS.length)];
  // assume clip natural pitch is around C3 (MIDI 48); rough but works
  const rate = Math.pow(2, (midi - 48) / 12);
  const wantSrcN = Math.floor(durSec * SR * rate);
  const grabN = Math.min(wantSrcN, clip.length - 100);
  const maxStart = Math.max(0, clip.length - grabN - 100);
  const startIdx = Math.floor(rand() * maxStart);
  const seg = clip.subarray(startIdx, startIdx + grabN);
  const shifted = pitchShift(seg, rate);
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  // AGGRESSIVE GLITCH — high probability + short chunks → constant tearing
  glitchify(out, 0.35, 0.028);
  // 3-bit destroy: another pass with smaller resolution
  for (let i = 0; i < n; i++) out[i] = Math.round(out[i] * 8) / 8;
  // distortion bite
  softSat(out, 2.4);
  // edge-fade
  const fadeN = Math.min(Math.floor(0.004 * SR), Math.floor(n / 8));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// TIMEWARP — read a section of `buf` at a time-varying rate (slow LFO
// dip then return), creating a tape-stop-and-start effect that lasts
// `endSec − startSec` in real time but warps internal timing. The
// rate envelope is 1 → minRate → 1 (cosine shape), and the source
// span is exactly the same length as the output so we don't drift.
function gTimewarp(buf, startSec, endSec, minRate = 0.45) {
  const a = Math.floor(startSec * SR);
  const b = Math.min(buf.length, Math.floor(endSec * SR));
  const n = b - a;
  if (n < 256) return;
  // build rate envelope and integrate
  const rates = new Float32Array(n);
  let totalRate = 0;
  for (let i = 0; i < n; i++) {
    const t = i / n;
    const lfo = (1 - Math.cos(2 * Math.PI * t)) / 2;   // 0 → 1 → 0
    rates[i] = 1.0 - lfo * (1.0 - minRate);             // 1 → minRate → 1
    totalRate += rates[i];
  }
  const scale = n / totalRate;
  const src = new Float32Array(buf.subarray(a, b));     // snapshot before write
  const out = new Float32Array(n);
  let pos = 0;
  for (let i = 0; i < n; i++) {
    const clamped = Math.min(n - 2, pos);
    const i0 = Math.floor(clamped); const f = clamped - i0;
    out[i] = src[i0] * (1 - f) + src[i0 + 1] * f;
    pos += rates[i] * scale;
  }
  // 20ms crossfade boundaries — disguises the warp ins/outs
  const fadeN = Math.min(Math.floor(0.020 * SR), Math.floor(n / 16));
  for (let i = 0; i < n; i++) {
    let g = 1;
    if (i < fadeN) g = i / fadeN;
    else if (i > n - fadeN) g = (n - i) / fadeN;
    buf[a + i] = out[i] * g + buf[a + i] * (1 - g);
  }
}

// SNARE — pitched body (~200 Hz) decays fast, noise burst gives the
// rattle, bandpass to ~2 kHz crack.
function gSnare(durSec = 0.16, gain = 0.55) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const body = Math.sin(2 * Math.PI * 200 * t) * 0.45;
    const noise = (rand() * 2 - 1) * 0.85;
    const env = Math.exp(-t / 0.035);
    out[i] = (body + noise) * env;
  }
  const lpHi = new Float32Array(out); lowpass(lpHi, 5200);
  const lpLo = new Float32Array(out); lowpass(lpLo, 320);
  for (let i = 0; i < n; i++) out[i] = (lpHi[i] - lpLo[i]) * gain * 1.5;
  // tiny attack fade
  const fadeN = Math.min(Math.floor(0.001 * SR), Math.floor(n / 16));
  for (let i = 0; i < fadeN; i++) out[i] *= i / fadeN;
  return out;
}

// PITCH-SHIFT A SECTION while preserving duration — granular time-
// stretch by `rate`, then resample by `rate`. Output length equals
// input length, pitch is multiplied by `rate`. Used to "pitch his
// sample around" — different sections of the song get different
// playback pitches without breaking the grid.
//
// PAD the source on each side before processing so the granular
// stretch's Hann-windowed first/last grains (which fade in/out to
// zero) land OUTSIDE the writable region. Without this the output
// drops to silence for ~30 ms at every pitchSection boundary.
function pitchSection(buf, startSec, endSec, rate) {
  if (rate === 1) return;
  const a = Math.max(0, Math.floor(startSec * SR));
  const b = Math.min(buf.length, Math.floor(endSec * SR));
  if (b - a < 256) return;
  const PAD_S = 0.120;                          // 120 ms padding each side
  const padN = Math.floor(PAD_S * SR);
  const aPad = Math.max(0, a - padN);
  const bPad = Math.min(buf.length, b + padN);
  const leadPadN = a - aPad;                    // actual lead pad samples
  const section = new Float32Array(buf.subarray(aPad, bPad));
  const stretched = granularStretch(section, rate);
  const shifted   = pitchShift(stretched, rate);
  // The Hann-fade artifact is at the very start/end of `shifted`.
  // Read from leadPadN into shifted to skip the start artifact; the
  // end artifact is past our write window (b - a) samples.
  const n = b - a;
  const fadeN = Math.min(Math.floor(0.010 * SR), Math.floor(n / 16));
  for (let i = 0; i < n; i++) {
    let g = 1;
    if (i < fadeN) g = i / fadeN;
    else if (i > n - fadeN) g = (n - i) / fadeN;
    const srcIdx = leadPadN + i;
    const srcVal = (srcIdx >= 0 && srcIdx < shifted.length) ? shifted[srcIdx] : 0;
    buf[a + i] = srcVal * g + buf[a + i] * (1 - g);
  }
}

// TURNTABLE SCRATCH — read the source.wav at a variable rate driven by
// an LFO. Different `mode` values pick the scratch shape:
//   "baby"   → smooth sine LFO back-and-forth (the classic "wikky wikky")
//   "chirp"  → fast pitched-up forward sweep
//   "tear"   → step pattern: forward / pause / reverse / pause
function gScratch(durSec = 0.4, mode = "baby", startSec = 1.0, gain = 0.5) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  let pos = startSec * SR;
  const minPos = 0.5 * SR;
  const maxPos = Math.min(srcFull.length - 1, (startSec + 1.5) * SR);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    let rate;
    if (mode === "baby") {
      rate = Math.sin(2 * Math.PI * 5 * t) * 2.5;             // ±2.5×
    } else if (mode === "chirp") {
      rate = 0.5 + 4.5 * (t / durSec);                         // 0.5× → 5×
    } else { // "tear"
      const ph = (t * 8) % 1;
      rate = ph < 0.25 ? 2.5 : ph < 0.5 ? 0 : ph < 0.75 ? -2.5 : 0;
    }
    pos += rate;
    if (pos < minPos) pos = minPos;
    if (pos > maxPos) pos = maxPos;
    const i0 = Math.floor(pos); const i1 = i0 + 1;
    const f = pos - i0;
    out[i] = srcFull[i0] * (1 - f) + (srcFull[i1] || 0) * f;
  }
  // soft saturation gives scratch its "crunchy" tone
  softSat(out, 1.4);
  // edge-fade + gain
  const fadeN = Math.min(Math.floor(0.005 * SR), Math.floor(n / 8));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  for (let i = 0; i < n; i++) out[i] *= gain;
  return out;
}

// HAND CLAP — 4 micro-bursts staggered 0/8/18/30 ms for the "ensemble
// of hands" feel, bandpass-ish around 1.5 kHz, fast decay.
function gClap(durSec = 0.15, gain = 0.5) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  const bursts = [0, 0.008, 0.018, 0.030];
  for (const bt of bursts) {
    const start = Math.floor(bt * SR);
    if (start >= n) continue;
    const burstLen = Math.min(Math.floor(0.05 * SR), n - start);
    for (let i = 0; i < burstLen; i++) {
      out[start + i] += (rand() * 2 - 1) * Math.exp(-i / (0.012 * SR));
    }
  }
  // bandpass ≈ LP(3.5k) − LP(800) — keeps the click + body
  const lpHi = new Float32Array(out); lowpass(lpHi, 3500);
  const lpLo = new Float32Array(out); lowpass(lpLo, 800);
  for (let i = 0; i < n; i++) out[i] = (lpHi[i] - lpLo[i]) * gain * 1.6;
  return out;
}

// TOM HIT — pitched sine with 30% pitch drop + attack click. Used by
// gTomFill to build descending tom rolls.
function gTomHit(freqHz = 220, durSec = 0.18, gain = 0.55) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const pitchEnv = freqHz * (1 - 0.3 * (t / durSec));
    const body = Math.sin(2 * Math.PI * pitchEnv * t);
    const env  = Math.exp(-t / (durSec * 0.35));
    const click = t < 0.004 ? (rand() * 2 - 1) * 0.45 : 0;
    out[i] = (body * env + click) * gain;
  }
  return out;
}

// TOM FILL — N hits descending from startHz to endHz across durSec.
function gTomFill(numHits = 4, durSec = 1.0, startHz = 240, endHz = 80, gain = 0.55) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  const hitInterval = n / numHits;
  for (let h = 0; h < numHits; h++) {
    const pitch = startHz + (endHz - startHz) * (h / Math.max(1, numHits - 1));
    const hit = gTomHit(pitch, Math.min(0.20, durSec / numHits), gain);
    const off = Math.floor(h * hitInterval);
    for (let i = 0; i < hit.length && off + i < n; i++) out[off + i] += hit[i];
  }
  return out;
}

// SNARE ROLL — accelerating noise bursts that crescendo into a drop.
function gSnareRoll(durSec = 0.6, gain = 0.50) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  let t = 0;
  while (t < durSec) {
    const prog = t / durSec;
    const density = 6 + 36 * prog;        // 6 → 42 hits/sec
    const hitLen  = Math.floor(0.035 * SR);
    const off     = Math.floor(t * SR);
    const ampScale = 0.4 + 0.6 * prog;     // ramp up the loudness
    for (let i = 0; i < hitLen && off + i < n; i++) {
      out[off + i] += (rand() * 2 - 1) * Math.exp(-i / (0.010 * SR)) * ampScale;
    }
    t += 1 / density;
  }
  // band-emphasize the snare crack
  const lpHi = new Float32Array(out); lowpass(lpHi, 4500);
  const lpLo = new Float32Array(out); lowpass(lpLo, 600);
  for (let i = 0; i < n; i++) out[i] = (lpHi[i] - lpLo[i]) * gain * 1.4;
  return out;
}

// POLICE DOG — cached bark, random window, edge-faded. Pitch-shift
// optionally to make it sound more aggressive (rate > 1).
function gDog(durSec = 0.5, gain = 0.55, rate = 1.0) {
  if (DOG_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = DOG_CLIPS[Math.floor(rand() * DOG_CLIPS.length)];
  const n = Math.floor(durSec * SR);
  const sourceN = Math.floor(n * rate);
  const wantN = Math.min(sourceN, clip.length - 100);
  const maxStart = Math.max(0, clip.length - wantN - 100);
  const startIdx = Math.floor(rand() * maxStart);
  const seg = clip.subarray(startIdx, startIdx + wantN);
  const shifted = rate === 1 ? seg : pitchShift(seg, rate);
  const out = new Float32Array(n);
  let peak = 0;
  for (let i = 0; i < Math.min(n, shifted.length); i++) {
    out[i] = shifted[i];
    const a = Math.abs(shifted[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  const fadeN = Math.min(Math.floor(0.008 * SR), Math.floor(n / 8));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// Schroeder reverb — 4 parallel comb filters + 2 series allpass for
// diffusion. Cheap O(n) per channel, gives a big spatial "room" tail
// for drops. `decayK` controls tail length (0.85 = short, 0.96 = long).
function applyReverb(buf, decayK = 0.93, wetMix = 0.45) {
  const n = buf.length;
  // 4 comb delays (samples, ~28-37 ms), prime-relative for less coloration
  const combs = [
    { d: 1557, g: decayK         },
    { d: 1617, g: decayK - 0.01 },
    { d: 1491, g: decayK - 0.02 },
    { d: 1422, g: decayK - 0.03 },
  ];
  const wet = new Float32Array(n);
  for (const { d, g } of combs) {
    const cBuf = new Float32Array(n);
    for (let i = 0; i < n; i++) {
      const fb = i >= d ? cBuf[i - d] * g : 0;
      cBuf[i] = buf[i] + fb;
      wet[i] += cBuf[i] * 0.25;
    }
  }
  // 2 series allpass (cleaner tail)
  const apgs = [{ d: 556, g: 0.5 }, { d: 441, g: 0.5 }];
  let cur = wet;
  for (const { d, g } of apgs) {
    const nxt = new Float32Array(n);
    for (let i = 0; i < n; i++) {
      const prev = i >= d ? nxt[i - d] : 0;
      const v = cur[i] + g * prev;
      nxt[i] = -g * v + prev;
    }
    cur = nxt;
  }
  // wet/dry mix in place
  for (let i = 0; i < n; i++) buf[i] = buf[i] * (1 - wetMix) + cur[i] * wetMix;
  return buf;
}

// SUBMACHINE GUN — cached clip, random window, edge-faded. Punchy.
function gSMG(durSec = 0.4, gain = 0.45) {
  if (SMG_CLIPS.length === 0) return new Float32Array(Math.floor(durSec * SR));
  const clip = SMG_CLIPS[Math.floor(rand() * SMG_CLIPS.length)];
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  const wantN = Math.min(n, clip.length - 100);
  const maxStart = Math.max(0, clip.length - wantN - 100);
  const startIdx = Math.floor(rand() * maxStart);
  let peak = 0;
  for (let i = 0; i < wantN; i++) {
    out[i] = clip[startIdx + i];
    const a = Math.abs(out[i]); if (a > peak) peak = a;
  }
  if (peak > 0) {
    const norm = (0.85 / peak) * gain;
    for (let i = 0; i < n; i++) out[i] *= norm;
  }
  const fadeN = Math.min(Math.floor(0.005 * SR), Math.floor(n / 8));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

// CYBERKICK — distorted hardcore kick body + skrill growl tail at E1
// for the cyber low rumble + soft-saturation glue. Used at drops.
function gCyberKick(durSec = 0.45, gain = 0.95) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  // body: cached hardcore kick (random one)
  if (KICK_CLIPS.length > 0) {
    const clip = KICK_CLIPS[Math.floor(rand() * KICK_CLIPS.length)];
    let peak = 0;
    for (let i = 0; i < n && i < clip.length; i++) {
      out[i] = clip[i];
      const a = Math.abs(clip[i]); if (a > peak) peak = a;
    }
    if (peak > 0) {
      const norm = 0.85 / peak;
      for (let i = 0; i < n; i++) out[i] *= norm;
    }
  }
  // tail: skrill stab on E1 (~41 Hz), short — adds the cyber growl rumble
  try {
    const skrill = renderSkrill(
      { midi: 28, durSec: Math.min(durSec * 0.9, 0.35), startSec: 0, gain: 0.55 },
      { sampleRate: SR, preset: "stab" }
    );
    for (let i = 0; i < skrill.length && i < n; i++) out[i] += skrill[i];
  } catch { /* skrill failed; kick body alone is fine */ }
  // saturation + final gain
  softSat(out, 1.8);
  for (let i = 0; i < n; i++) out[i] *= gain;
  // short attack & release fades to avoid clicks
  const fadeN = Math.min(Math.floor(0.002 * SR), Math.floor(n / 16));
  for (let i = 0; i < fadeN; i++) {
    out[i] *= i / fadeN;
    out[n - 1 - i] *= i / fadeN;
  }
  return out;
}

function gTurkeySynth(durSec = 0.7, pitchHz = 280, gain = 0.35) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  const pulseRate = 12 + (rand() - 0.5) * 3;      // 10.5 – 13.5 Hz
  const period = SR / pulseRate;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const pulseIdx = Math.floor(i / period);
    const phase = (i - pulseIdx * period) / period;
    const pulseEnv = Math.exp(-phase * 7);
    // pitch wobble per pulse: pseudo-random but smooth
    const pw = 1 + 0.08 * Math.sin(pulseIdx * 1.3) + 0.05 * Math.sin(pulseIdx * 0.7);
    const f  = pitchHz * pw;
    const sine = Math.sin(2 * Math.PI * f * t);
    const noise = (rand() * 2 - 1) * 0.35;
    out[i] = (sine * 0.7 + noise) * pulseEnv * gain;
  }
  // global envelope: short rise, long tapered fall
  for (let i = 0; i < n; i++) {
    const t = i / n;
    const env = t < 0.08 ? t / 0.08 : Math.pow(1 - (t - 0.08) / 0.92, 1.4);
    out[i] *= env;
  }
  lowpass(out, 2800);
  return out;
}

// Upward riser — opposite of gSweepDown, used to lift into chorus.
function gSweepUp(durSec = 1.0, gain = 0.14) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) out[i] = rand() * 2 - 1;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const env = Math.pow(t / (durSec), 1.5);  // accelerating rise
    out[i] *= env;
  }
  const log0 = Math.log(200), log1 = Math.log(8000);
  let y = 0;
  for (let i = 0; i < n; i++) {
    const t = i / (n - 1);
    const fc = Math.exp(log0 + (log1 - log0) * t);
    const a = Math.exp(-2 * Math.PI * fc / SR);
    y = (1 - a) * out[i] + a * y;
    out[i] = y * gain;
  }
  return out;
}

// Detect snare positions in the 4-bar drum loop once — these get short
// sinebells layered on top every time the drum loop tiles.
const SNARE_OFFSETS = detectSnares(drumLoop);
console.log(`detected ${SNARE_OFFSETS.length} snare hits in drum loop`);

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
function tileDrums(startBar, nBars, gain = 1.0, variants = null,
                   snareChord = null) {
  const loopN = drumLoop.length;
  const barN = Math.floor(BAR * SR);
  const loops = Math.ceil(nBars / LOOP_BARS);
  for (let lp = 0; lp < loops; lp++) {
    const v = variants ? variants[lp % variants.length] : "full";
    if (v === "silent") continue;
    // Anchor each tile to integer loopN spacing — no sub-sample drift.
    // Floor(startBar*BAR*SR) once for the section start, then tiles
    // are placed at exact loopN multiples from there. Tile N+1 starts
    // EXACTLY where tile N's loop content ends → wrapped-tail crossfade
    // can sum to constant amplitude across the seam.
    const off = Math.floor(startBar * BAR * SR) + lp * loopN;
    const remaining = (nBars - lp * LOOP_BARS) * barN;
    const writeN = Math.min(loopN, remaining);
    // SEAMLESS TILING — equal-power sin/cos crossfade across the
    // boundary. Each tile writes loopN samples + a TILE_TAIL_N tail
    // that fades out cos-shaped; the next tile's first TILE_TAIL_N
    // samples fade in sin-shaped. sin² + cos² = 1 → constant power
    // across the seam. srcLoop/drumLoop are extracted with the extra
    // TILE_TAIL_N samples so this is just a buffer read past loopN.
    // LINEAR crossfade across the seam. Equal-power was creating a
    // √2 amplitude bump in the overlap region (sin+cos peaks higher
    // than either alone when both contributions are correlated — same
    // musical content on both sides of the loop point). Linear keeps
    // amplitude constant across the seam. Tail fade-out doesn't cut
    // off prematurely — it reads TILE_TAIL_N samples PAST loopN.
    const fadeN = Math.min(TILE_TAIL_N, Math.floor(writeN / 16));
    const place = (j, srcVal, drumVal) => {
      let g = 1;
      if (j < fadeN) g = j / fadeN;
      else if (j >= writeN) g = 1 - (j - writeN) / fadeN;
      loopBuf[off + j]  += srcVal  * gain * g;
      drumsBuf[off + j] += drumVal * gain * g;
    };
    const writeEnd = writeN + fadeN;   // play into the tail for crossfade
    // Past loopN the source recording is silent (the bounce stops after
    // 4 bars). For seamless looping we WRAP the tail read back to the
    // start of srcLoop so the next tile's fade-in and this tile's
    // fade-out share the same musical content → sum to constant audio.
    const readIdx = (i) => i < loopN ? i : i - loopN;
    if (v === "full") {
      for (let i = 0; i < writeEnd && off + i < totalN; i++) {
        const r = readIdx(i);
        place(i, srcLoop[r], drumLoop[r]);
      }
    } else if (v === "mute1stBar") {
      for (let i = barN; i < writeEnd && off + i < totalN; i++) {
        const r = readIdx(i);
        place(i, srcLoop[r], drumLoop[r]);
      }
    } else if (v === "muteLastBar") {
      const stop = Math.min(writeEnd, 3 * barN + fadeN);
      for (let i = 0; i < stop && off + i < totalN; i++) {
        const r = readIdx(i);
        place(i, srcLoop[r], drumLoop[r]);
      }
    } else if (v === "halfSilent") {
      const stop = Math.min(writeEnd, 2 * barN + fadeN);
      for (let i = 0; i < stop && off + i < totalN; i++) {
        const r = readIdx(i);
        place(i, srcLoop[r], drumLoop[r]);
      }
    } else if (v === "stutter1") {
      for (let rep = 0; rep < 4; rep++) {
        const rOff = off + rep * barN;
        for (let i = 0; i < barN && rOff + i < totalN; i++) {
          loopBuf[rOff + i]  += srcLoop[i]  * gain;
          drumsBuf[rOff + i] += drumLoop[i] * gain;
        }
      }
    } else if (v === "reverse") {
      for (let i = 0; i < writeN && off + i < totalN; i++) {
        loopBuf[off + i]  += srcLoop[loopN - 1 - i]  * gain;
        drumsBuf[off + i] += drumLoop[loopN - 1 - i] * gain;
      }
    }
    // little sinebells riding each detected snare hit in the loop
    if (snareChord && v !== "silent") {
      const midi = chordRoot(snareChord, 3) + snareChord.fifth;  // 5th in oct3
      const alt  = chordRoot(snareChord, 4);                     // root in oct4
      for (const sOff of SNARE_OFFSETS) {
        if (sOff >= writeN) continue;
        // skip snares that fall in muted regions for muteLastBar / halfSilent
        if (v === "muteLastBar" && sOff >= 3 * barN) continue;
        if (v === "halfSilent" && sOff >= 2 * barN) continue;
        if (v === "mute1stBar" && sOff < barN) continue;
        const tSec = (off + sOff) / SR;
        const m = (sOff % 2 === 0) ? midi : alt;
        const seg = gLowSineBell(m, 0.18, 0.25);
        const startIdx = Math.floor(tSec * SR);
        for (let j = 0; j < seg.length && startIdx + j < totalN; j++) {
          snareBellBuf[startIdx + j] += seg[j];
        }
      }
    }
  }
}

// ── arrangement ──────────────────────────────────────────────────────
let cursor = 0;

function intro(nBars = 4) {
  // OPEN with turkeys + a shock rifle, then a "fallout" reverb tail
  // into the void before the rhythm-sample percussion kicks in.
  // Bar 0 = cold open turkey + shock rifle CRACK that rings out.
  // Bar 1 = the void (mostly reverb tail).
  // Bar 2 onwards = source rhythm + hats lead through.
  const t0 = cursor * BAR;
  // Opening turkey gobbles (a small flock answers)
  placeInto(gobbleBuf, gTurkeyClip(0.85, 200, 0.45, 1.5), t0 + 0.1 * BEAT);
  placeInto(gobbleBuf, gTurkeyClip(0.45, 350, 0.32, 1),   t0 + 0.7 * BEAT);
  placeInto(gobbleBuf, gTurkeyClip(0.70, 260, 0.38, 1.2), t0 + 1.5 * BEAT);
  // SHOCK RIFLE crack on the opening — gets things going
  placeInto(shockBuf, gShockRifle(1.2, 0.75, 0.95), t0 + 1 * BEAT);
  // Send the opening turkey + shock to the reverb bus for fallout
  // (the reverb post-render pass will tail this out into the void).
  const sendN = Math.floor(2.5 * BAR * SR);
  const sendStart = Math.floor(t0 * SR);
  for (let i = 0; i < sendN && sendStart + i < totalN; i++) {
    reverbBuf[sendStart + i] += gobbleBuf[sendStart + i] * 0.6;
    reverbBuf[sendStart + i] += shockBuf[sendStart + i]  * 0.5;
  }
  // Source loop comes in QUIETLY from bar 2 onward (the percussion
  // lead-in after the void)
  tileDrums(cursor + 2, nBars - 2, 0.5, ["full"], CHORDS_A[0]);
  for (let bar = 2; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = CHORDS_A[bar % CHORDS_A.length];
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.06 * (bar - 1)), t);
  }
  // Upward riser landing on the verse downbeat
  placeInto(hatBuf, gSweepUp(BAR * 0.9, 0.13), (cursor + nBars - 1) * BAR + 0.1 * BAR);
  cursor += nBars;
}

function verse(nBars = 8, progression = CHORDS_A) {
  tileDrums(cursor, nBars, 1.0, ["full", "muteLastBar"], progression[0]);
  for (let bar = 0; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = progression[bar % progression.length];
    const phrase = Math.floor(bar / 4) % 2;
    if (phrase === 0) {
      placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.22), t);
      placeInto(sineBellBuf, gLowSineBell(chordRoot(ch, 3), BAR * 0.9, 0.22), t);
      if (bar % 2 === 1) placeInto(sineBellBuf,
        gLowSineBell(chordRoot(ch, 3) + ch.fifth, BAR * 0.7, 0.14), t + BEAT);
      placeInto(bellBuf, gBell(chordRoot(ch, 4), BEAT * 1.6, 0.16), t);
    } else {
      placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.24), t);
      placeInto(padBuf, gPad(chordRoot(ch, 3) + ch.third, BAR, 0.12), t);
      const echoes = chordTriad(ch, 4).concat([chordRoot(ch, 4)]);
      for (let i = 0; i < 4; i++) {
        if (rand() < 0.45) {
          placeInto(drumBellBuf,
            gDrumBell(echoes[i], BEAT * 0.9, 0.18),
            t + i * BEAT + BEAT * 0.5);
        }
      }
      placeInto(sineBellBuf,
        gLowSineBell(chordRoot(ch, 3), BAR * 0.95, 0.20), t);
    }
    // Light 16th-grid hats with off-16th accent
    for (let s = 0; s < 16; s++) {
      const accent = (s % 4 === 2) ? 0.13 : (s % 2 === 1 ? 0.08 : 0.05);
      placeInto(hatBuf, gHat(0.025, accent), t + s * (BEAT / 4));
    }
    // sparse gobble: one every 4 bars
    if (bar % 4 === 2) {
      const pitch = 230 + (bar * 17) % 90;
      placeInto(gobbleBuf, gTurkey(0.55, pitch, 0.28), t + 1 * BEAT);
    }
  }
  // Upward sweep landing on chorus 1 downbeat
  placeInto(hatBuf, gSweepUp(BAR * 0.9, 0.16), (cursor + nBars - 1) * BAR + 0.1 * BAR);
  cursor += nBars;
}

function chorus(nBars = 8, intensity = 1.0, progression = CHORDS_A) {
  tileDrums(cursor, nBars, 0.9 * intensity, ["full", "full"], progression[0]);
  for (let bar = 0; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = progression[bar % progression.length];
    const arpAsc = chordArp(ch, 4);
    const arpDesc = [...arpAsc].reverse();
    const dir = Math.floor(bar / 2) % 2 === 0 ? arpAsc : arpDesc;
    const triad = chordTriad(ch, 3);
    // pad + sineBell as background drones, lower than before
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.26 * intensity), t);
    placeInto(padBuf, gPad(triad[2], BAR, 0.14 * intensity), t);
    placeInto(sineBellBuf,
      gLowSineBell(chordRoot(ch, 3), BAR * 0.95, 0.22 * intensity), t);
    // stabs lower so they sit IN the mix, not on top
    const stabBeats = bar < nBars / 2 ? [1, 3] : [0, 2.5];
    for (const beat of stabBeats) {
      placeInto(stabBuf, gStab(triad[0], BEAT * 0.6, 0.32 * intensity), t + beat * BEAT);
    }
    // sparser granular bells, every other 8th
    for (let i = 0; i < 8; i++) {
      if (i % 2 === 0 || rand() < 0.4) {
        placeInto(bellBuf,
          gBell(dir[i % dir.length], BEAT * 0.85, 0.18 * intensity),
          t + i * (BEAT / 2));
      }
    }
    // bar 3/7 turnaround — drum-bell climb (lower gain now)
    if (bar === 3 || bar === 7) {
      const nextCh = progression[(bar + 1) % progression.length];
      const climb = chordTriad(nextCh, 4).concat([chordRoot(nextCh, 5)]);
      for (let i = 0; i < 4; i++) {
        placeInto(drumBellBuf,
          gDrumBell(climb[i], BEAT * 0.7, 0.26 * intensity),
          t + i * BEAT);
      }
    }
    // HIGH-END HATS — 16th-note closed hats with off-16th accents
    for (let s = 0; s < 16; s++) {
      const accent = (s % 4 === 2) ? 1.0 : (s % 2 === 1 ? 0.7 : 0.45);
      placeInto(hatBuf,
        gHat(0.025, 0.13 * intensity * accent),
        t + s * (BEAT / 4));
    }
    if (bar % 4 === 3) {
      placeInto(hatBuf,
        gOpenHat(0.20, 0.18 * intensity),
        t + 3.5 * BEAT);
    }
    // TURKEY GOBBLE: every 2 bars, on the and-of-3
    if (bar % 2 === 0) {
      const pitch = 260 + (bar * 23) % 80;   // pseudo-random pitch per gobble
      placeInto(gobbleBuf, gTurkey(0.6, pitch, 0.32 * intensity), t + 2.5 * BEAT);
    }
  }
  cursor += nBars;
}

function bridge(nBars = 4, progression = CHORDS_A) {
  // Downward sweep on the bridge downbeat — texture transition
  placeInto(hatBuf, gSweepDown(BAR * 1.2, 0.18), cursor * BAR);
  // Drums quieter but PRESENT through the whole bridge (was halfSilent
  // which left :54 sounding too empty) — bars 3-4 muted-last to make
  // room for the turkey call-and-response
  tileDrums(cursor, nBars, 0.7, ["muteLastBar"], progression[0]);
  const t0 = cursor * BAR;
  for (let bar = 0; bar < nBars; bar++) {
    const t = t0 + bar * BAR;
    const ch = progression[bar % progression.length];
    const triad = chordTriad(ch, 3);
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.28), t);
    placeInto(padBuf, gPad(triad[1], BAR, 0.18), t);
    placeInto(padBuf, gPad(triad[2], BAR, 0.18), t);
    placeInto(sineBellBuf, gLowSineBell(chordRoot(ch, 2), BAR, 0.26), t);
  }
  // drum-bell arp walks E chord tones across the 4 bars
  for (let bar = 0; bar < nBars; bar++) {
    const ch = progression[bar % progression.length];
    const r = chordRoot(ch, 4);
    const tones = [r, r + ch.fifth, r + 12, r + ch.third];
    for (let s = 0; s < 4; s++) {
      placeInto(drumBellBuf,
        gDrumBell(tones[s], BEAT * 1.2, 0.24),
        t0 + bar * BAR + s * BEAT);
    }
  }
  for (let bar = 0; bar < nBars; bar++) {
    const ch = progression[bar % progression.length];
    const t = (cursor + bar) * BAR;
    placeInto(bellBuf, gBell(chordRoot(ch, 5), BEAT * 3, 0.20), t + 0 * BEAT);
    placeInto(bellBuf,
      gBell(chordRoot(ch, 4) + ch.fifth, BEAT * 3, 0.16), t + 2 * BEAT);
  }
  // TURKEY CONVERSATION — pitches snap to E major chord tones (E/G#/B
  // across octaves 2-4) so the calls harmonize with the source's E
  // pedal. Stretches range from 1× (sharp) to 3.5× (droopy doom).
  // E2=82, G#2=104, B2=123, E3=164, G#3=207, B3=247, E4=329, G#4=415, B4=494
  placeInto(gobbleBuf, gTurkeyClip(1.6, 207, 0.38, 2.0), t0 + 0.5 * BEAT);          // G#3 stretched
  placeInto(gobbleBuf, gTurkeyClip(0.45, 494, 0.32, 1),  t0 + 2.5 * BEAT);          // B4 sharp
  placeInto(gobbleBuf, gTurkeyClip(2.2, 164, 0.42, 3.0), t0 + 1 * BAR + 0.5 * BEAT); // E3 deep stretch
  placeInto(gobbleBuf, gTurkeyClip(0.55, 415, 0.34, 1.1), t0 + 1 * BAR + 3 * BEAT);  // G#4 punctuation
  placeInto(gobbleBuf, gTurkeyClip(0.70, 247, 0.34, 1.3), t0 + 2 * BAR + 0.5 * BEAT); // B3
  placeInto(gobbleBuf, gTurkeyClip(0.50, 329, 0.30, 1),  t0 + 2 * BAR + 2 * BEAT);   // E4
  placeInto(gobbleBuf, gTurkeyClip(1.0, 207, 0.36, 1.7), t0 + 3 * BAR + 0.5 * BEAT); // G#3
  placeInto(gobbleBuf, gTurkeyClip(2.8, 82, 0.48, 3.5),  t0 + 3 * BAR + 1.5 * BEAT); // E2 ULTRA-DROOPY FINALE

  // POLICE DOGS responding to the turkeys — call-and-response across
  // the bridge, faster rate makes them sound more aggressive
  placeInto(dogBuf, gDog(0.5, 0.48, 1.2),  t0 + 1.5 * BEAT);
  placeInto(dogBuf, gDog(0.65, 0.45, 1.0), t0 + 1 * BAR + 1.8 * BEAT);
  placeInto(dogBuf, gDog(0.45, 0.50, 1.4), t0 + 2 * BAR + 1.2 * BEAT);
  placeInto(dogBuf, gDog(0.55, 0.46, 1.1), t0 + 2 * BAR + 3 * BEAT);
  placeInto(dogBuf, gDog(0.50, 0.52, 1.3), t0 + 3 * BAR + 1 * BEAT);
  // Riser into chorus 2 on the last bar
  placeInto(hatBuf, gSweepUp(BAR * 0.95, 0.17), (cursor + nBars - 1) * BAR + 0.05 * BAR);
  cursor += nBars;
}

function outro(nBars = 4, progression = CHORDS_A) {
  tileDrums(cursor, nBars, 0.65, ["muteLastBar"], progression[0]);
  for (let bar = 0; bar < nBars; bar++) {
    const t = (cursor + bar) * BAR;
    const ch = progression[bar % progression.length];
    const triad = chordTriad(ch, 3);
    placeInto(padBuf, gPad(chordRoot(ch, 2), BAR, 0.28), t);
    placeInto(padBuf, gPad(triad[1], BAR, 0.18), t);
    placeInto(padBuf, gPad(triad[2], BAR, 0.18), t);
    placeInto(sineBellBuf, gLowSineBell(chordRoot(ch, 2), BAR, 0.32), t);
    placeInto(sineBellBuf,
      gLowSineBell(chordRoot(ch, 3), BAR * 0.9, 0.20), t);
  }
  // outro starts with a downward sweep + parting turkey calls,
  // ending on a deeply stretched droopy goodbye
  placeInto(hatBuf, gSweepDown(BAR * 1.4, 0.15), cursor * BAR);
  const tOut = cursor * BAR;
  placeInto(gobbleBuf, gTurkeyClip(0.70, 260, 0.30, 1),  tOut + 1 * BAR);
  placeInto(gobbleBuf, gTurkeyClip(1.0, 220, 0.28, 1.5), tOut + 2 * BAR + 0.5 * BEAT);
  placeInto(gobbleBuf, gTurkeyClip(2.5, 110, 0.30, 2.8), tOut + 3 * BAR);  // long droopy
  // closing bell + drum-bell tag on the last chord
  const tStart = cursor * BAR;
  const lastCh = progression[(nBars - 1) % progression.length];
  placeInto(bellBuf, gBell(chordRoot(lastCh, 5), 4.0, 0.26), tStart);
  placeInto(drumBellBuf,
    gDrumBell(chordRoot(lastCh, 4), BEAT * 4, 0.20), tStart + 3 * BAR);
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

// ── per-track polish ────────────────────────────────────────────────
// very subtle brightener — was smearing the drum transients
brightenDrums(loopBuf, 0.10, 2400);
// gentler glitch on the bells
glitchify(bellBuf, 0.06, 0.045);

// ── THREE MELODY PATTERNS ───────────────────────────────────────────
// Each spans 2 bars (8 beats). placeMelody picks one per call so the
// melody dances between slow holds, fast violin runs, and wild climbs.

// SLOW HOLDS — long sustained notes, only 3-4 notes per 2 bars.
// Lets the warble decay tails do the talking.
const MELODY_SLOW = [
  { b: 0.0, m: 76, d: 2.8 },  // long E5
  { b: 3.0, m: 80, d: 1.0 },  // G#5
  { b: 4.0, m: 83, d: 3.2 },  // long B5 hold
];

// WILD CLIMB — 16th-note scale screams up 3 octaves to a single high
// sustained saw note → then STRAIGHT DOWN to a thick multisaw cluster
// (root + 3rd + 5th + octave layered) for the "WHaoaoa" plunge.
// The descent + cluster is rendered as multiple parallel calls in
// placeMelody when it sees melody notes tagged { cluster: [...] }.
const MELODY_WILD_CLIMB = [
  // bar 0: rapid ascending scale E4 → E7
  { b: 0.00, m: 64, d: 0.16 },  // E4
  { b: 0.20, m: 66, d: 0.16 },  // F#4
  { b: 0.40, m: 68, d: 0.16 },  // G#4
  { b: 0.60, m: 71, d: 0.16 },  // B4
  { b: 0.80, m: 73, d: 0.16 },  // C#5
  { b: 1.00, m: 76, d: 0.16 },  // E5
  { b: 1.20, m: 78, d: 0.16 },  // F#5
  { b: 1.40, m: 80, d: 0.16 },  // G#5
  { b: 1.60, m: 83, d: 0.16 },  // B5
  { b: 1.80, m: 85, d: 0.16 },  // C#6
  { b: 2.00, m: 88, d: 0.16 },  // E6
  { b: 2.20, m: 90, d: 0.16 },  // F#6
  { b: 2.40, m: 92, d: 0.16 },  // G#6
  { b: 2.60, m: 95, d: 0.16 },  // B6
  { b: 2.80, m: 97, d: 0.16 },  // C#7
  { b: 3.00, m: 88, d: 2.0 },   // ★ HELD E6 (softer than the E7 scream)
  // bar 1: PLUNGE — multisaw cluster on E maj across octaves 2-4
  { b: 5.00, m: 40, d: 3.0, cluster: [40, 47, 52, 56, 59, 64] },
  // (root E2, B2, E3, G#3, B3, E4 — 6-saw stack, less wall, more chord)
];

// FAST VIOLIN — 16th-note snake around drum hits, dancing in/out of
// the kick/snare grid. Climbing arcs + descending runs.
const MELODY_FAST = [
  // ── bar 0 — opening 16th run climbing through B5 to peak ───────────
  { b: 0.25, m: 76, d: 0.20 },  // e:  E5
  { b: 0.50, m: 80, d: 0.20 },  // &:  G#5
  { b: 0.75, m: 83, d: 0.20 },  // a:  B5
  { b: 1.00, m: 85, d: 0.40 },  // 2:  C#6 (snare anchor, longer)
  { b: 1.50, m: 83, d: 0.20 },
  { b: 1.75, m: 80, d: 0.20 },
  // (skip kick at 2.0)
  { b: 2.25, m: 78, d: 0.20 },  // e of 3
  { b: 2.50, m: 80, d: 0.20 },
  { b: 2.75, m: 83, d: 0.20 },
  { b: 3.00, m: 88, d: 0.40 },  // 4:  E6 TOP (snare anchor)
  { b: 3.50, m: 83, d: 0.20 },
  { b: 3.75, m: 80, d: 0.20 },
  // ── bar 1 — descending run through pentatonic + dance back up ──────
  { b: 4.25, m: 80, d: 0.20 },  // e of 1
  { b: 4.50, m: 78, d: 0.20 },
  { b: 4.75, m: 76, d: 0.20 },
  { b: 5.00, m: 73, d: 0.40 },  // 2:  C#5 (snare anchor, dipping low)
  { b: 5.50, m: 76, d: 0.20 },
  { b: 5.75, m: 80, d: 0.20 },
  // (skip kick at 6.0)
  { b: 6.25, m: 83, d: 0.20 },
  { b: 6.50, m: 80, d: 0.20 },
  { b: 6.75, m: 76, d: 0.20 },
  { b: 7.00, m: 71, d: 0.65 },  // 4:  B4 LONG hold (snare anchor)
  { b: 7.75, m: 76, d: 0.25 },  // pickup back to bar 0
];
// Custom supersaw renderer with per-voice warble that activates during
// the decay tail. Each of the 7 saw voices has its own LFO rate (3-9 Hz)
// and phase, so the voices DRIFT OUT OF HARMONY through the decay —
// the chord starts clean and falls apart into a gobbling turkey-like
// detuned mess by the end of each note. The warble depth is 0 during
// attack + early sustain, ramps up through late sustain, peaks at
// ±320 cents (≈ 1/3 semitone) during the decay.
function mixSupersawWarble(ev, out) {
  const VOICES = 7;
  const DETUNE_CENTS = 18;
  const CENTER_GAIN = 0.65;
  const ATTACK = 0.015;
  const DECAY  = 0.50;
  const baseFreq = 440 * Math.pow(2, (ev.midi - 69) / 12);
  const startIdx = Math.floor(ev.startSec * SR);
  const half = (VOICES - 1) / 2;
  const detune = new Array(VOICES);
  const amps = new Array(VOICES);
  let ampSum = 0;
  for (let v = 0; v < VOICES; v++) {
    detune[v] = ((v - half) / half) * DETUNE_CENTS;
    amps[v] = (v === Math.floor(half)) ? CENTER_GAIN : 1.0;
    ampSum += amps[v];
  }
  const norm = 1 / ampSum;
  // Per-voice independent warble LFOs
  const warbleRate = new Array(VOICES);
  const warblePhase = new Array(VOICES);
  const phase = new Array(VOICES);
  for (let v = 0; v < VOICES; v++) {
    warbleRate[v]  = 3 + 6 * rand();    // 3–9 Hz, each voice different
    warblePhase[v] = rand() * 2 * Math.PI;
    phase[v]       = rand();             // random starting saw phase
  }
  const sustainN = Math.max(1, Math.floor(ev.durSec * SR));
  const attN = Math.max(1, Math.floor(ATTACK * SR));
  const decN = Math.max(1, Math.floor(DECAY  * SR));
  const totalN = sustainN + decN;
  // Warble depth schedule (in cents):
  //   0..50% of sustain   → 0 (clean unison)
  //   50..100% of sustain → 0 → 60 cents linear
  //   decay (full)        → 60 → 320 cents linear
  for (let i = 0; i < totalN; i++) {
    const dst = startIdx + i;
    if (dst >= out.length) break;
    const t = i / SR;
    // amplitude envelope
    let env;
    if (i < attN) env = i / attN;
    else if (i < sustainN) env = 1;
    else {
      const df = (i - sustainN) / decN;
      env = 1 - df;
      if (env <= 0) break;
    }
    // warble depth
    let depth;
    const sustainHalf = sustainN * 0.5;
    if (i < sustainHalf) depth = 0;
    else if (i < sustainN) depth = 60 * ((i - sustainHalf) / sustainHalf);
    else depth = 60 + 260 * ((i - sustainN) / decN);
    let sample = 0;
    for (let v = 0; v < VOICES; v++) {
      const lfo = Math.sin(2 * Math.PI * warbleRate[v] * t + warblePhase[v]);
      const cents = detune[v] + depth * lfo;
      const freq = baseFreq * Math.pow(2, cents / 1200);
      phase[v] += freq / SR;
      if (phase[v] >= 1) phase[v] -= 1;
      const saw = phase[v] * 2 - 1;       // sawtooth -1..+1
      sample += saw * amps[v] * norm;
    }
    out[dst] += sample * env * ev.gain;
  }
}

// Chorded-sine vocal-envelope voice — for each melody note, stack 3
// pure sine tones (root + perfect 5th + octave), apply a slow vocal
// envelope (60ms attack, sustain, 250ms release), light vibrato.
// Cleaner / more "vocal" than the warbling supersaw — sounds like a
// hummed chord.
function mixChordedSineVocal(ev, out) {
  const tones = [ev.midi, ev.midi + 7, ev.midi + 12];
  const WEIGHTS = [1.0, 0.55, 0.45];
  const ATTACK_S  = 0.060;
  const RELEASE_S = 0.250;
  const VIB_HZ    = 5.2;
  const VIB_CENTS = 11;
  const sustainN = Math.max(1, Math.floor(ev.durSec * SR));
  const attN = Math.max(1, Math.floor(ATTACK_S * SR));
  const relN = Math.max(1, Math.floor(RELEASE_S * SR));
  const totalN = sustainN + relN;
  const startIdx = Math.floor(ev.startSec * SR);
  const phase = [rand() * 0.1, rand() * 0.1, rand() * 0.1];
  let wsum = 0; for (const w of WEIGHTS) wsum += w;
  const norm = 1 / wsum;
  for (let i = 0; i < totalN; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) break;
    let env;
    if (i < attN) env = i / attN;
    else if (i < sustainN) env = 1;
    else { env = 1 - (i - sustainN) / relN; if (env <= 0) break; }
    const t = i / SR;
    const vib = Math.sin(2 * Math.PI * VIB_HZ * t) * VIB_CENTS;
    let sample = 0;
    for (let v = 0; v < tones.length; v++) {
      const baseHz = 440 * Math.pow(2, (tones[v] - 69) / 12);
      const freq   = baseHz * Math.pow(2, vib / 1200);
      phase[v] += freq / SR;
      if (phase[v] >= 1) phase[v] -= 1;
      sample += Math.sin(2 * Math.PI * phase[v]) * WEIGHTS[v];
    }
    out[dst] += sample * norm * env * ev.gain;
  }
}

function placeMelody(startBar, gainScale = 1.0, melody = MELODY_FAST) {
  for (const note of melody) {
    const tSec = (startBar * BAR) + note.b * BEAT;
    if (tSec >= TARGET_S) break;
    if (note.cluster) {
      // MULTISAW CLUSTER — keeps the warbling supersaws (this IS the
      // WHaoaoa plunge moment, not a clean vocal)
      for (const m of note.cluster) {
        mixSupersawWarble(
          { midi: m, durSec: note.d * BEAT, startSec: tSec, gain: 0.18 * gainScale },
          topBellBuf
        );
      }
    } else {
      // Chorded-sine vocal envelope — upper register (lead)
      mixChordedSineVocal(
        { midi: note.m, durSec: note.d * BEAT, startSec: tSec, gain: 0.45 * gainScale },
        topBellBuf
      );
      // Lower octave (body)
      mixChordedSineVocal(
        { midi: note.m - 12, durSec: note.d * BEAT, startSec: tSec, gain: 0.32 * gainScale },
        topBellBuf
      );
    }
  }
}
// Melody more present — still skips bars 0-9 (percussion intro)
placeMelody(10, 0.30, MELODY_WILD_CLIMB);  // ★ first appearance — climb into chorus 1
placeMelody(12, 0.50, MELODY_FAST);        // chorus 1 start: violin
placeMelody(14, 0.65, MELODY_FAST);        // chorus 1 mid
placeMelody(16, 0.78, MELODY_SLOW);        // chorus 1 dramatic hold
placeMelody(18, 0.85, MELODY_WILD_CLIMB);  // ★ climb into bridge
placeMelody(22, 0.65, MELODY_WILD_CLIMB);  // ★ climb out of bridge → CH2 drop
placeMelody(24, 0.95, MELODY_FAST);        // chorus 2: violin
placeMelody(26, 1.05, MELODY_SLOW);        // chorus 2 dramatic hook
placeMelody(28, 1.15, MELODY_WILD_CLIMB);  // ★ peak climb
placeMelody(30, 1.00, MELODY_FAST);        // chorus 2 end: dance
placeMelody(32, 0.45, MELODY_SLOW);        // outro fade

// ── GLITCHED PIANO STABS ─────────────────────────────────────────────
// E major chord stabs on the heavy beats — destroyed by bitcrush + dist
const stabsCh1 = [
  { bar: 12, beat: 1.5, midi: 52 }, { bar: 13, beat: 0.5, midi: 56 },
  { bar: 15, beat: 2.5, midi: 59 }, { bar: 16, beat: 1.0, midi: 52 },
  { bar: 18, beat: 0.0, midi: 64 }, { bar: 19, beat: 3.0, midi: 56 },
];
const stabsCh2 = [
  { bar: 24, beat: 0.5, midi: 52 }, { bar: 25, beat: 2.5, midi: 56 },
  { bar: 26, beat: 0.0, midi: 64 }, { bar: 27, beat: 3.5, midi: 59 },
  { bar: 28, beat: 1.0, midi: 56 }, { bar: 29, beat: 0.0, midi: 52 },
  { bar: 30, beat: 2.5, midi: 64 }, { bar: 31, beat: 0.0, midi: 56 },
];
for (const s of [...stabsCh1, ...stabsCh2]) {
  placeInto(pianoBuf,
    gPianoStab(s.midi, 0.45, 0.32),
    s.bar * BAR + s.beat * BEAT);
}

// ── SUNG VOWELS (jeffrey-pvc) — phonemes pitched to E maj chord tones ─
// Verses get long sustained "ah" + "iy" droning under the source mix.
// Choruses get short "ay"/"owe"/"we" word stabs on chord changes.
// Bridge gets a slow "uw" pad as the turkeys / dogs do their thing.
// E2=40, G#2=44, B2=47, E3=52, G#3=56, B3=59, E4=64, G#4=68, B4=71
const VOX = [
  // intro: distant "ah" on E3, sustained
  { bar: 1.0,  dur: 2.0 * BAR, m: 52, idx: 0, g: 0.16, str: 1.5 },
  // verse phrase A: "ah" pad at E3 across the whole 4 bars
  { bar: 4.0,  dur: 4.0 * BAR, m: 52, idx: 0, g: 0.22, str: 1.8 },
  // verse phrase B: "iy" on G#3 layered with "ah" on E2
  { bar: 8.0,  dur: 4.0 * BAR, m: 56, idx: 1, g: 0.18, str: 1.6 },
  { bar: 8.0,  dur: 4.0 * BAR, m: 40, idx: 0, g: 0.14, str: 2.0 },
  // chorus 1: short word stabs on the chord roots
  { bar: 12.0, dur: 1.2,       m: 52, idx: 3, g: 0.32, str: 1 },     // "ay"
  { bar: 14.0, dur: 1.0,       m: 56, idx: 5, g: 0.28, str: 1 },     // "owe"
  { bar: 16.0, dur: 1.2,       m: 59, idx: 6, g: 0.30, str: 1 },     // "we"
  { bar: 18.0, dur: 1.4,       m: 52, idx: 4, g: 0.32, str: 1.1 },   // "eye"
  // bridge: sustained "uw" pad on E2 across all 4 bars — droning oo
  { bar: 20.0, dur: 4.0 * BAR, m: 40, idx: 2, g: 0.26, str: 2.2 },
  { bar: 22.0, dur: 2.0 * BAR, m: 47, idx: 1, g: 0.18, str: 1.6 },   // "iy" higher
  // chorus 2: bigger word stabs
  { bar: 24.0, dur: 1.4,       m: 64, idx: 3, g: 0.36, str: 1 },     // "ay" high
  { bar: 25.5, dur: 1.0,       m: 56, idx: 6, g: 0.30, str: 1 },     // "we"
  { bar: 27.0, dur: 1.2,       m: 59, idx: 5, g: 0.32, str: 1 },     // "owe"
  { bar: 28.5, dur: 1.0,       m: 52, idx: 4, g: 0.30, str: 1 },     // "eye"
  { bar: 30.0, dur: 1.6,       m: 64, idx: 3, g: 0.34, str: 1.1 },   // "ay" again
  // outro: long "ah" closing
  { bar: 32.0, dur: 3.5 * BAR, m: 52, idx: 0, g: 0.26, str: 2.5 },
];
for (const v of VOX) {
  placeInto(voxBuf,
    gJeffreyVowel(v.m, v.dur, v.idx, v.g, v.str),
    v.bar * BAR);
}

// ── Extra SHOCK RIFLE hits on the BIG parts (chorus 2 turnarounds) ──
placeInto(shockBuf, gShockRifle(0.6, 0.50, 1.10), 27 * BAR + 3 * BEAT);   // CH2 mid pre-bar
placeInto(shockBuf, gShockRifle(0.6, 0.55, 0.95), 29 * BAR + 0 * BEAT);   // CH2 peak entry
placeInto(shockBuf, gShockRifle(0.7, 0.60, 1.0),  31 * BAR + 0 * BEAT);   // CH2 final downbeat

// ── LAUGH TRACK at ~41 s + TORNADO in the bridge ────────────────────
// 41 s lands at bar 17.3 — late chorus 1, perfect comedic timing.
placeInto(laughBuf, gLaugh(3.0, 0.42, 1.0), 41.0);
// Tornado sustained roar through the deep half of the bridge (bars 20-22)
placeInto(tornadoBuf, gTornado(4.5, 0.32, 0.85), 20 * BAR);
// Second tornado swirl into chorus 2 drop
placeInto(tornadoBuf, gTornado(2.5, 0.40, 1.05), 23 * BAR + 1 * BEAT);

// ── COACH WHISTLES — call the section changes ───────────────────────
placeInto(whistleBuf, gWhistle(0.50, 0.55, 1.0),  10 * BAR + 3.5 * BEAT);  // pre-CH1 cue
placeInto(whistleBuf, gWhistle(0.40, 0.55, 1.15), 20 * BAR + 0.1 * BAR);   // bridge entry
placeInto(whistleBuf, gWhistle(0.55, 0.65, 1.0),  23 * BAR + 3.0 * BEAT);  // CH2 drop cue
placeInto(whistleBuf, gWhistle(0.45, 0.50, 1.20), 28 * BAR + 3.5 * BEAT);  // peak

// ── HARMONIZED CHAINSAWS — E maj 7 stacks at the wild-climb peaks ─────
// fires at the same bars as the wild-climb melody calls (bars 10, 18,
// 22, 28) — chainsaws BLOOM as the saws scream up, tuned to chord tones.
const CHAINSAW_HITS = [
  { bar: 10, gain: 0.30 },  // climb into chorus 1
  { bar: 18, gain: 0.34 },  // climb into bridge
  { bar: 22, gain: 0.38 },  // climb into chorus 2 (the big drop)
  { bar: 28, gain: 0.42 },  // chorus 2 PEAK climb
];
const CHAINSAW_CHORD = [40, 47, 52, 56, 59, 64];  // E2 B2 E3 G#3 B3 E4
for (const hit of CHAINSAW_HITS) {
  const tBase = hit.bar * BAR;
  for (let i = 0; i < CHAINSAW_CHORD.length; i++) {
    // Stagger slightly so they don't all start at the exact same sample
    const t = tBase + (i * 0.012);
    const dur = 2.4 + rand() * 0.6;
    placeInto(chainsawBuf,
      gChainsaw(CHAINSAW_CHORD[i], dur, hit.gain * (0.55 + 0.45 * rand())),
      t);
  }
}

// ── CHURCH BELL CHORDS — E maj at section markers ─────────────────────
placeInto(churchBuf, gChurchBellChord(52, 5.5, 0.40), 0 * BAR);    // intro opening
placeInto(churchBuf, gChurchBellChord(52, 4.0, 0.42), 20 * BAR);   // bridge entrance
placeInto(churchBuf, gChurchBellChord(52, 5.0, 0.46), 24 * BAR);   // chorus 2 drop
placeInto(churchBuf, gChurchBellChord(52, 6.0, 0.36), 32 * BAR);   // outro entrance

// ── TIMEWARP — 6 bars in the first 60s (bars 14-20, ~33s-47s) ────────
// rate dips to 0.40 mid-warp then returns to 1.0 — tape-stop-and-start
gTimewarp(loopBuf, 14 * BAR, 20 * BAR, 0.40);

// ── PITCH HIS SAMPLE AROUND ──────────────────────────────────────────
// Opening pitchSections REMOVED — the granular tradeoffs were making
// the first 10 bars sound chopped up. Source plays at natural pitch
// from bar 0. Pitch shifts only kept in long stable sections.
pitchSection(loopBuf, 20 * BAR, 22 * BAR, 0.82);  // bridge first half DEEP
pitchSection(loopBuf, 22 * BAR, 24 * BAR, 0.88);  // bridge second half lifting
pitchSection(loopBuf, 24 * BAR, 28 * BAR, 1.06);  // chorus 2 first half UP
pitchSection(loopBuf, 34 * BAR, 36 * BAR, 0.85);  // outro winding down

// ── PUNCTUATE THE TIME-STRETCHED SECTIONS ────────────────────────────
// Mark every pitch transition + add accents on beats 1 & 3 within
// the stretched bars so the listener feels the pitch movement.
{
  const PITCH_MARKERS = [
    { bar: 20, kind: "deep"   },  // ▼ bridge deep entry
    { bar: 22, kind: "lift"   },  // ▲ bridge lift
    { bar: 24, kind: "up"     },  // ▲▲ chorus 2 up
    { bar: 28, kind: "level"  },  // ◆ back to normal
    { bar: 34, kind: "down"   },  // ▼ outro winding down
  ];
  for (const m of PITCH_MARKERS) {
    const t = m.bar * BAR;
    // shock rifle snap announces every transition
    const rate = m.kind === "down" ? 0.85 : m.kind === "up" ? 1.10 : 1.0;
    placeInto(shockBuf, gShockRifle(0.55, 0.45, rate), t - 0.02);
    // stutter the source for a quick "skip" right before the transition
    stutterLoop(loopBuf, t - 0.25 * BEAT, 0.25, 2);
  }
  // Add cyberkicks on beats 1 & 3 of every stretched bar (bars 20-27, 34-35)
  // so the pitched-down/pitched-up bars have a percussive backbone.
  const STRETCHED_BARS = [20, 21, 22, 23, 24, 25, 26, 27, 34, 35];
  for (const bar of STRETCHED_BARS) {
    const intensity = bar < 24 ? 0.45 : bar < 28 ? 0.55 : 0.40;
    placeInto(kickBuf, gCyberKick(0.30, intensity), bar * BAR + 0 * BEAT);
    placeInto(kickBuf, gCyberKick(0.25, intensity * 0.85), bar * BAR + 2 * BEAT);
  }
}

// ── KICK + SNARE in breaks ───────────────────────────────────────────
// Every break gets a dense kick/snare backbeat replacement so the
// drum drop-out fills with our own percussion. Pattern escalates
// into the drop on bigger breaks (bar 11, 23).
//
// bar 11 (verse → CH1 build, 4-on-floor + snare on 2 & 4):
placeInto(kickBuf, gCyberKick(0.35, 0.70), 11 * BAR + 0 * BEAT);
placeInto(percBuf, gSnare(0.16, 0.55),     11 * BAR + 1 * BEAT);
placeInto(kickBuf, gCyberKick(0.30, 0.62), 11 * BAR + 2 * BEAT);
placeInto(percBuf, gSnare(0.16, 0.55),     11 * BAR + 3 * BEAT);
placeInto(percBuf, gSnare(0.10, 0.45),     11 * BAR + 3.5 * BEAT);
// bar 23 (bridge → CH2 build, ESCALATING 16th-note snares):
placeInto(kickBuf, gCyberKick(0.40, 0.80), 23 * BAR + 0 * BEAT);
placeInto(percBuf, gSnare(0.16, 0.55),     23 * BAR + 1 * BEAT);
placeInto(kickBuf, gCyberKick(0.35, 0.70), 23 * BAR + 2 * BEAT);
placeInto(percBuf, gSnare(0.16, 0.60),     23 * BAR + 2.5 * BEAT);
placeInto(percBuf, gSnare(0.16, 0.60),     23 * BAR + 3 * BEAT);
placeInto(percBuf, gSnare(0.12, 0.55),     23 * BAR + 3.25 * BEAT);
placeInto(percBuf, gSnare(0.12, 0.55),     23 * BAR + 3.5 * BEAT);
placeInto(percBuf, gSnare(0.10, 0.50),     23 * BAR + 3.75 * BEAT);
// bar 35 (outro final bar, sparse closing pattern):
placeInto(kickBuf, gCyberKick(0.30, 0.55), 35 * BAR + 0 * BEAT);
placeInto(percBuf, gSnare(0.16, 0.40),     35 * BAR + 2 * BEAT);
// bar 7 (between verse phrases — drums are present, but snare adds lift):
placeInto(percBuf, gSnare(0.12, 0.32),     7 * BAR + 1 * BEAT);
placeInto(percBuf, gSnare(0.12, 0.32),     7 * BAR + 3 * BEAT);

// ── BREAKS + SCRATCHES — fill the drum drop-outs with percussion ─────
// (claps removed per request — hats now carry the backbeat)
// breaks (where drums drop):
// bar 11 (verse muteLastBar) — tom fill descending + snare roll into CH1
placeInto(percBuf, gTomFill(5, 1.4, 280, 90, 0.55), 11 * BAR + 1 * BEAT);
placeInto(percBuf, gSnareRoll(0.55, 0.50),          11 * BAR + 3 * BEAT);
// bar 23 (bridge muteLastBar) — BIG tom fill + snare roll into the
// chorus 2 drop (the :54 moment)
placeInto(percBuf, gTomFill(6, 1.8, 320, 70, 0.62), 23 * BAR + 0.5 * BEAT);
placeInto(percBuf, gSnareRoll(0.75, 0.55),          23 * BAR + 3 * BEAT);
// bar 35 (outro muteLastBar) — softer tom fill, dying away
placeInto(percBuf, gTomFill(4, 1.2, 200, 70, 0.40), 35 * BAR + 1 * BEAT);
// LEADING HATS through the opening — DARKER hats (low HP cutoff,
// "pitched down") + only OFF-BEATS (1.5/2.5/3.5/4.5) so they stay
// out of the way of oskie's drums. Cutoff is randomized per hit so
// the hats wobble in pitch as they lead through.
for (let bar = 0; bar < 10; bar++) {
  // off-eighths only: positions 1, 3, 5, 7 → beats 0.5, 1.5, 2.5, 3.5
  for (let s = 1; s < 8; s += 2) {
    const hp = 1400 + 2200 * rand();         // ~1.4–3.6 kHz darker hats
    const gain = 0.07 + 0.03 * rand();
    placeInto(hatBuf, gHat(0.028, gain, hp), bar * BAR + s * (BEAT / 2));
  }
  // open hat once every 4 bars on the and-of-4 (sparser than before)
  if (bar % 4 === 3) {
    placeInto(hatBuf, gOpenHat(0.20, 0.12), bar * BAR + 3.5 * BEAT);
  }
}

// SCRATCHES — turntable scratches of the original beat, scattered at
// transitions + breaks
// verse → chorus 1 build
placeInto(scratchBuf, gScratch(0.40, "baby", 1.2, 0.45), 11 * BAR + 2.5 * BEAT);
placeInto(scratchBuf, gScratch(0.25, "chirp", 2.5, 0.42), 11 * BAR + 3.5 * BEAT);
// chorus 1 odd bars get a quick scratch
placeInto(scratchBuf, gScratch(0.18, "tear", 0.9, 0.36), 13 * BAR + 0.5 * BEAT);
placeInto(scratchBuf, gScratch(0.18, "baby", 3.0, 0.36), 17 * BAR + 2.5 * BEAT);
// bridge fills — scratches between turkey calls
placeInto(scratchBuf, gScratch(0.30, "tear", 1.5, 0.42), 20 * BAR + 3 * BEAT);
placeInto(scratchBuf, gScratch(0.25, "baby", 4.0, 0.40), 21 * BAR + 2 * BEAT);
placeInto(scratchBuf, gScratch(0.35, "chirp", 1.8, 0.48), 23 * BAR + 2.5 * BEAT);
// chorus 2 — denser scratching
placeInto(scratchBuf, gScratch(0.20, "baby", 2.0, 0.40), 25 * BAR + 0.5 * BEAT);
placeInto(scratchBuf, gScratch(0.30, "tear", 3.5, 0.42), 27 * BAR + 2 * BEAT);
placeInto(scratchBuf, gScratch(0.22, "chirp", 5.0, 0.44), 29 * BAR + 3 * BEAT);
placeInto(scratchBuf, gScratch(0.40, "baby", 1.0, 0.45), 31 * BAR + 2.5 * BEAT);

// ── DROPS: cyberkicks + SMG rolls + filter sweeps ────────────────────
// Drop points (absolute bars):
//   bar 11  end of verse 1 → chorus 1
//   bar 19  end of chorus 1 → bridge
//   bar 23  end of bridge → chorus 2
//   bar 31  end of chorus 2 → outro
const DROPS = [
  { dropBar: 11, intoLight: false },  // verse → chorus 1
  { dropBar: 19, intoLight: true  },  // chorus 1 → bridge (lighter)
  { dropBar: 23, intoLight: false },  // bridge → chorus 2 (huge drop)
  { dropBar: 31, intoLight: true  },  // chorus 2 → outro (lighter)
];
for (const { dropBar, intoLight } of DROPS) {
  const tLast = dropBar * BAR;            // start of the last bar before drop
  const tDrop = (dropBar + 1) * BAR;       // the drop itself
  const gain  = intoLight ? 0.65 : 1.0;
  // Cyberkick lands ON the drop (slightly before so attack hits the 1)
  placeInto(kickBuf, gCyberKick(0.45, 0.90 * gain), tDrop - 0.015);
  // SHOCK RIFLE snap layered on every drop — rave accent
  placeInto(shockBuf, gShockRifle(0.7, 0.55 * gain, 1.0), tDrop - 0.005);
  // SMG roll across the last beat of the build bar
  placeInto(smgBuf,  gSMG(0.55, 0.55 * gain), tLast + 3 * BEAT);
  // Police dog bark right after the kick — the chaos arrives
  placeInto(dogBuf,  gDog(0.45, 0.50 * gain, 1.15), tDrop + 0.3 * BEAT);
  // For big drops: extra cyberkick on beat 4 of the build bar + SMG burst
  if (!intoLight) {
    placeInto(kickBuf, gCyberKick(0.30, 0.65), tLast + 3 * BEAT);
    placeInto(smgBuf,  gSMG(0.35, 0.45),       tLast + 2 * BEAT);
    // EXTRA dog bark at the entry, varied rate
    placeInto(dogBuf,  gDog(0.55, 0.55, 1.3),  tDrop + 1 * BEAT);
    // REVERB SEND — copy the big kick + SMG into the reverb buffer so
    // we can wash them with a spatial tail without affecting dry signal
    const sendStart = Math.floor((tDrop - 0.015) * SR);
    const sendN     = Math.floor(2.5 * BEAT * SR);
    for (let i = 0; i < sendN && sendStart + i < totalN; i++) {
      reverbBuf[sendStart + i] += kickBuf[sendStart + i] * 0.7;
      reverbBuf[sendStart + i] += smgBuf[sendStart + i]  * 0.5;
    }
  }
  // Filter sweep on the SOURCE LOOP across the build bar — opens up
  // from muffled to full, the classic "wssssh" into the drop
  sweepLP(loopBuf, tLast, tDrop, intoLight ? 18000 : 600, 18000);
}

// Process the reverb send — big spatial Schroeder reverb tail
applyReverb(reverbBuf, 0.95, 1.0);  // fully wet — we mix the wet into the L/R bus

// ── filter sweeps (section-shaped automation) ────────────────────────
// Bar boundaries (TOTAL_BARS_PLANNED = 36): 0 intro, 4 V1, 12 CH1,
// 20 bridge, 24 CH2, 32 outro, 36 end.
// 1) intro source fade open over the first 2 bars only — was 4 bars
// which dragged the whole intro
sweepLP(loopBuf, 0 * BAR, 2 * BAR, 350, 16000);
// 2) end-of-CH1 pad lift into the bridge (last bar of chorus 1)
sweepLP(padBuf, 19 * BAR, 20 * BAR, 2200, 4500);
// 3) bridge wow — pad sweeps closed then back open
sweepLP(padBuf, 20 * BAR, 22 * BAR, 4500, 500);
sweepLP(padBuf, 22 * BAR, 24 * BAR, 500, 3800);
// 4) outro source sweep closed over the last 2 bars only (was 4)
sweepLP(loopBuf, 34 * BAR, 36 * BAR, 16000, 350);

// ── SWINGY HIP-HOP CHOPS on the source loop ─────────────────────────
// stutter — take a small window and repeat it `repeats` times
function stutterLoop(buf, startSec, chunkBeats = 0.5, repeats = 3) {
  const a = Math.floor(startSec * SR);
  const chunkN = Math.floor(chunkBeats * BEAT * SR);
  const chunk = new Float32Array(chunkN);
  for (let i = 0; i < chunkN && a + i < buf.length; i++) chunk[i] = buf[a + i];
  // 5ms fade-in/out on each repeat edge to mask the click
  const fadeN = Math.min(Math.floor(0.005 * SR), Math.floor(chunkN / 8));
  for (let r = 0; r < repeats; r++) {
    const off = a + r * chunkN;
    for (let i = 0; i < chunkN && off + i < buf.length; i++) {
      let g = 1;
      if (i < fadeN) g = i / fadeN;
      else if (i > chunkN - fadeN) g = (chunkN - i) / fadeN;
      buf[off + i] = chunk[i] * g;
    }
  }
}
// pause — short mute on the source (creates a rest)
function pauseLoop(buf, startSec, durSec) {
  const a = Math.floor(startSec * SR);
  const n = Math.floor(durSec * SR);
  const fadeN = Math.min(Math.floor(0.003 * SR), Math.floor(n / 4));
  for (let i = 0; i < n && a + i < buf.length; i++) {
    let g = 1;
    if (i < fadeN) g = 1 - i / fadeN;
    else if (i > n - fadeN) g = 1 - (n - i) / fadeN;
    else g = 0;
    buf[a + i] *= g;
  }
}
// skip — replay an earlier section in place of the current one
function skipLoop(buf, dstSec, srcSec, durSec) {
  const dst = Math.floor(dstSec * SR);
  const src = Math.floor(srcSec * SR);
  const n = Math.floor(durSec * SR);
  const fadeN = Math.min(Math.floor(0.004 * SR), Math.floor(n / 8));
  for (let i = 0; i < n && dst + i < buf.length && src + i < buf.length; i++) {
    let g = 1;
    if (i < fadeN) g = i / fadeN;
    else if (i > n - fadeN) g = (n - i) / fadeN;
    buf[dst + i] = buf[src + i] * g + buf[dst + i] * (1 - g);
  }
}

// Apply hip-hop chops to loopBuf at moments that lock with the form
// stutters: last beat or half-beat repeats leading into transitions
stutterLoop(loopBuf, 7.5 * BAR,   0.5, 3);   // verse → chorus 1 build
stutterLoop(loopBuf, 11.25 * BAR, 0.25, 4);  // tighter stutter into CH1
stutterLoop(loopBuf, 15.75 * BAR, 0.25, 4);  // mid-chorus skip
stutterLoop(loopBuf, 19.5 * BAR,  0.5, 2);   // CH1 → bridge transition
stutterLoop(loopBuf, 23.5 * BAR,  0.5, 2);   // bridge → CH2 drop build
stutterLoop(loopBuf, 27.75 * BAR, 0.25, 4);  // CH2 first half end
stutterLoop(loopBuf, 31.75 * BAR, 0.25, 4);  // CH2 → outro
// pauses (tasty hip-hop breaths): brief mutes on off-beats
pauseLoop(loopBuf, 13 * BAR + 1.5 * BEAT, 0.10);   // CH1 bar 2
pauseLoop(loopBuf, 17 * BAR + 3.5 * BEAT, 0.10);
pauseLoop(loopBuf, 25 * BAR + 1.75 * BEAT, 0.12);  // CH2 bar 1
pauseLoop(loopBuf, 29 * BAR + 3.5 * BEAT, 0.10);
// skips: replay an earlier passage in place — keeps the listener guessing
skipLoop(loopBuf, 14 * BAR + 3 * BEAT, 12 * BAR + 0 * BEAT, BEAT);
skipLoop(loopBuf, 26 * BAR + 0 * BEAT, 24 * BAR + 2 * BEAT, BEAT);

// ── PERCUSSION-FOCUS ZONE (bars 0-9 / first 21.3 s) ──────────────────
// Scale DOWN every non-percussion layer in the opening — pad, sine
// bells, granular bells, drum-bells, vox, gobbles, piano, church bells,
// chainsaws all get heavily muted until the wild-climb at bar 10 lifts
// us into chorus 1. Drums + perc + hats stay full.
{
  const muteZoneEnd = Math.floor(9.5 * BAR * SR);
  const fadeInEnd   = Math.floor(10.0 * BAR * SR);  // 0.5-bar ramp back to full
  const scaleAt = (i) => {
    if (i >= fadeInEnd) return 1.0;
    if (i >= muteZoneEnd) return (i - muteZoneEnd) / (fadeInEnd - muteZoneEnd);
    return 0.0;
  };
  const SUPPRESS = [
    padBuf, sineBellBuf, bellBuf, drumBellBuf, snareBellBuf,
    voxBuf, gobbleBuf, pianoBuf, churchBuf, chainsawBuf, stabBuf,
  ];
  for (let i = 0; i < fadeInEnd && i < totalN; i++) {
    const g = scaleAt(i);
    for (const b of SUPPRESS) b[i] *= g;
  }
}

// ── multi-track awareness: sidechain ducking from drums ──────────────
// Pad / sineBell / bell duck slightly under each drum hit so the kit
// pokes through. Stabs / drum-bells / snare-bells ride on top — no duck.
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
// Auto-pan: slow LFO sweeps a voice L↔R across `periodSec`. Phase
// offset lets different voices not move in sync.
function panAuto(src, periodSec = 16, phase = 0, depth = 0.7) {
  const w = 2 * Math.PI / (periodSec * SR);
  for (let i = 0; i < totalN; i++) {
    const lfo = Math.sin(w * i + phase) * depth;   // -depth..+depth
    const lG = 1 - Math.max(0,  lfo);   // 1 when lfo ≤ 0
    const rG = 1 - Math.max(0, -lfo);   // 1 when lfo ≥ 0
    left[i]  += src[i] * lG;
    right[i] += src[i] * rG;
  }
}
// loopBuf (his mix) is the audible foundation; drumsBuf is sidechain
// key only, NOT panned to output. Wider stereo field across the voices.
pan(loopBuf,      1.00, 1.00);                         // center, dry
pan(padBuf,       1.00, 0.85);                         // pad slight left
pan(sineBellBuf,  0.85, 1.00);                         // sine bells slight right
pan(stabBuf,      1.05, 0.70);                         // stabs left of center
pan(drumBellBuf,  0.75, 1.05);                         // drum-bells right of center
pan(snareBellBuf, 0.70, 1.05);                         // snare-bells right
pan(hatBuf,       0.80, 1.05);                         // hats slightly right
pan(kickBuf,      0.85, 0.85);                         // cyberkicks chilled
// reverb tail: wide stereo via two slightly different gains, gives
// the "spatial" feel without needing true stereo decorrelation
pan(reverbBuf,    1.10, 0.95);
panAuto(bellBuf,    10, 0,           0.55);            // bells sweep L↔R every 10s
panAuto(gobbleBuf,  6.5, Math.PI/2,  0.75);            // turkeys hop L↔R faster
panAuto(smgBuf,     2.7, Math.PI,    0.85);            // SMG sprays L↔R fast + wide
panAuto(dogBuf,     4.0, Math.PI/3,  0.80);            // dogs prowl L↔R
panAuto(percBuf,    8.0, Math.PI/4,  0.35);            // perc gentle pan
// small dry boost on percussion — keeps backbeat audible without LOUD
for (let i = 0; i < totalN; i++) { left[i] += percBuf[i] * 0.18; right[i] += percBuf[i] * 0.18; }
panAuto(scratchBuf, 1.7, 0,          0.95);            // scratches hard + fast L↔R
panAuto(pianoBuf,   5.5, Math.PI/2,  0.70);            // glitched piano dances L↔R
pan(churchBuf,      1.05, 0.95);                       // bells wide stereo
pan(topBellBuf,     1.00, 0.95);                       // supersaw lead (softened)
pan(voxBuf,         1.20, 1.10);                       // jeffrey vox forward (mellower)
pan(chainsawBuf,    0.75, 0.80);                       // chainsaws softer
pan(whistleBuf,     0.95, 1.05);                       // whistles wide
pan(shockBuf,       1.10, 1.05);                       // shock rifle wide + forward
pan(tornadoBuf,     1.10, 1.10);                       // tornado wide atmosphere
pan(laughBuf,       1.05, 1.00);                       // laugh slightly off-center

// trim
const finalN = Math.floor(TARGET_S * SR);
const outL = left.subarray(0, finalN);
const outR = right.subarray(0, finalN);

// master bus: low cut → soft glue → HF de-hiss
masterBus(outL, outR);

// ── MID-TOWN ENVELOPE-FOLLOWER SINE ───────────────────────────────────
// Profile the mix's amplitude envelope, then generate a sine whose
// pitch oscillates around E4 (330 Hz) following the envelope shape.
// Louder track moments push the sine higher in pitch and amplitude —
// the sine "rides" the track's energy contour from the inside.
{
  const winN = Math.floor(0.060 * SR);   // 60ms RMS-ish window
  const env = new Float32Array(finalN);
  let acc = 0;
  for (let i = 0; i < winN && i < finalN; i++) {
    acc += Math.abs(outL[i]) + Math.abs(outR[i]);
  }
  for (let i = 0; i < finalN; i++) {
    if (i + winN < finalN) acc += Math.abs(outL[i + winN]) + Math.abs(outR[i + winN]);
    if (i > winN) acc -= Math.abs(outL[i - winN - 1]) + Math.abs(outR[i - winN - 1]);
    env[i] = acc / (winN * 2);
  }
  let envMax = 0;
  for (let i = 0; i < finalN; i++) if (env[i] > envMax) envMax = env[i];
  if (envMax > 0) for (let i = 0; i < finalN; i++) env[i] /= envMax;
  // Generate the mid-town sine — pitch & amp both follow envelope
  const BASE_HZ = 330;        // E4 anchor
  const RANGE_HZ = 220;       // up to ~A4 at peak
  const SINE_GAIN = 0.10;     // sits behind the mix
  let phase = 0;
  for (let i = 0; i < finalN; i++) {
    const freq = BASE_HZ + RANGE_HZ * env[i];
    phase += freq / SR;
    if (phase >= 1) phase -= 1;
    const s = Math.sin(2 * Math.PI * phase) * env[i] * SINE_GAIN;
    outL[i] += s * 1.05;
    outR[i] += s * 0.95;
  }
}

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
