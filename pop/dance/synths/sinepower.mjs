// sinepower.mjs — stacked harmonic sines for the dance lane.
// Drop-in replacement for supersaw at the lead/pad/stab slots.
// All voices resolve to sound.synth({type:"sine", ...}) — same contract
// percussion.mjs uses, native-portable via AC's audio runtime.
//
// Voice = fundamental + octave + fifth + (optional) sub-octave + (pad)
// third + (pad) high brilliance. All sine, all phase-locked relative
// to the source clock — no aliasing, smooth crossfades.
//
// Library:
//   import { playSinePower, mixEventSinePower, SINEPOWER_PRESETS } from "...";
//   playSinePower(sound, { midi, durSec, gain }, opts)   // any AC runtime
//   mixEventSinePower(ev, out, opts)                      // node buffer

import { makeBufferSynth } from "./bus.mjs";

export const SINEPOWER_PRESETS = {
  // Lead — bright + cutting but smooth. Fundamental forward, octave +
  // fifth for body, sub for low end.
  lead: {
    partials: [
      { ratio: 1.0,  amp: 1.00 },   // fundamental
      { ratio: 2.0,  amp: 0.45 },   // octave up
      { ratio: 1.5,  amp: 0.28 },   // perfect fifth
      { ratio: 0.5,  amp: 0.18 },   // sub-octave
    ],
    attack: 0.015,
    decay: 0.45,
  },
  // Pad — wide, slow attack, more partials for a richer chord pad.
  pad: {
    partials: [
      { ratio: 1.0,  amp: 1.00 },
      { ratio: 2.0,  amp: 0.35 },
      { ratio: 1.5,  amp: 0.30 },
      { ratio: 1.25, amp: 0.20 },   // major third tinge
      { ratio: 0.5,  amp: 0.25 },   // sub
      { ratio: 4.0,  amp: 0.08 },   // 2-octave brilliance
    ],
    attack: 0.40,
    decay: 1.80,
  },
  // Stab — quick, percussive, mid-only.
  stab: {
    partials: [
      { ratio: 1.0, amp: 1.00 },
      { ratio: 2.0, amp: 0.40 },
      { ratio: 1.5, amp: 0.25 },
    ],
    attack: 0.005,
    decay: 0.18,
  },
};

const DEFAULT_PRESET = "lead";
const DEFAULT_SAMPLE_RATE = 48_000;

function midiToFreq(midi) {
  return 440 * Math.pow(2, (midi - 69) / 12);
}

export function playSinePower(sound, ev, opts = {}) {
  if (!sound?.synth) return;
  const preset = SINEPOWER_PRESETS[opts.preset || DEFAULT_PRESET] || SINEPOWER_PRESETS[DEFAULT_PRESET];
  const attack = opts.attack ?? preset.attack;
  const decay  = opts.decay  ?? preset.decay;
  const eventGain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) return;
  if (eventGain === 0) return;

  const partials = opts.partials || preset.partials;
  let ampSum = 0;
  for (const p of partials) ampSum += p.amp;
  const norm = 1 / Math.max(0.001, ampSum);

  const fund = midiToFreq(ev.midi);
  for (const p of partials) {
    sound.synth({
      type: "sine",
      tone: fund * p.ratio,
      duration: ev.durSec,
      volume: eventGain * p.amp * norm,
      attack,
      decay,
    });
  }
}

export function mixEventSinePower(ev, out, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  if (!(out instanceof Float32Array)) return;
  const sound = makeBufferSynth(out, ev.startSec, sampleRate);
  playSinePower(sound, ev, opts);
}
