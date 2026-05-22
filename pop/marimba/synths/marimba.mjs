#!/usr/bin/env node
// marimba.mjs — synthesized marimba: a struck wooden bar with a tuned
// tube resonator under it. Modal synthesis (sum of exponentially-damped
// sines) + mallet-impulse excitation + a high-Q SVF band-pass standing
// in for the half-wavelength tube. Same module shape as the rest of the
// pop/ stack — see ../../dance/synths/sinepower.mjs and
// ../../hippyhayzard/synths/zitar.mjs.
//
// ── why modal, not sampled ─────────────────────────────────────────────
// The pop/ posture is bottom-up (SCORE.md): synthesize, don't sample.
// Marimba is one of the FRIENDLIEST instruments to model from physics
// because a free-free wooden bar has only a handful of musically
// significant modes, and those modes are well-documented:
//
//   • Rossing (2000), *Science of Percussion Instruments* — chapter 4
//     tabulates measured modal frequencies for tuned marimba bars.
//   • Bork (1995), "Practical tuning of xylophone bars and resonators,"
//     *Applied Acoustics* 46:103–127 — gives the canonical undercut
//     tuning recipe and the resonator-coupling Q values used below.
//   • Bretos, Santamaría, Moral (1999), *JASA* 105(3) — FEM analysis
//     of an undercut bar confirming the 1 : 4 : 10 design target.
//   • Chaigne & Doutaut (1997), "Numerical simulations of xylophones,"
//     *JASA* 101(1):539–557 — the gold-standard time-domain PDE solve
//     of the actual beam. Overkill for a synth lane but worth knowing
//     it exists; our modal sum IS its steady-state impulse response.
//   • Essl & Cook (1999), "Banded waveguides…," ICMC — STK's marimba
//     in waveguide form. For a strike (impulse excitation, no bow)
//     a banded waveguide REDUCES algebraically to one damped sine
//     per band, i.e. the modal additive synth below. We use the
//     simpler form because there is no audible difference for mallets.
//
// ── the marimba bar in one paragraph ──────────────────────────────────
// A uniform free-free wooden bar has inharmonic modes at ratios
// 1 : 2.756 : 5.404 : 8.933 (Euler-Bernoulli beam). Marimba bars are
// deeply UNDERCUT (an arch milled out of the underside, deepest at the
// center) so that mode 2 lands precisely on 4f (two octaves up) and
// mode 3 lands near 10f (three octaves + a major third). The 4:1
// octave is held to within a few cents by every professional maker;
// the third mode floats between 9.2× (rosewood, vintage) and 10× (Kelon
// synthetic bars). A half-wavelength PVC tube hung under each bar,
// closed at the far end, narrow-band-boosts ONLY the fundamental — the
// woody upper partials pass through unmagnified, which is why the
// attack is so percussive and the sustain is so pure.
//
// ── mallet excitation ─────────────────────────────────────────────────
// A mallet strike is an impulse of finite duration set by how long the
// mallet head stays in contact with the bar — typically 1 ms (hard
// rubber) to 4 ms (yarn-wrapped). A short contact passes broadband
// content (all modes excited equally → bright, woody). A long contact
// low-pass-filters the impulse (only the fundamental gets through →
// warm, dark). We model the strike as a half-cosine pulse whose width
// IS the mallet contact time, which gives both behaviors from one knob.
//
// ── library ──────────────────────────────────────────────────────────
//   import { mixEventMarimba, renderMarimba, MARIMBA_PRESETS } from "...";
//   mixEventMarimba(ev, out, opts)         // node bed render path
//   renderMarimba(ev, opts) -> Float32     // bare DSP engine
//   playMarimba(sound, ev, opts)           // AC sound.synth fallback
//
// ── CLI demo ─────────────────────────────────────────────────────────
//   node pop/marimba/synths/marimba.mjs                       # rosewood
//   node pop/marimba/synths/marimba.mjs --preset kelon
//   node pop/marimba/synths/marimba.mjs --preset bass --out ~/m.mp3
//
// ── C-port path (ac-native) ──────────────────────────────────────────
// Every op below is pure float — no JS-only constructs — so the model
// drops into fedac/native/src/audio.c next to generate_harp_sample()
// as generate_marimba_sample() with no math changes. The JS mirror
// (lib/sound/synth.mjs) would add a `marimba` case identical to this
// renderMarimba() body.

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "rosewood";

// ── presets ────────────────────────────────────────────────────────────
// partials  : modal ratios relative to the fundamental (the pitch)
// amps      : initial amplitudes (relative; the renderer normalises)
// decays    : per-mode T60 in seconds (mode 1 is the ring; higher modes
//             decay much faster — that's where the woody attack lives)
// mallet    : mallet contact time in seconds (the half-cosine pulse
//             width — 0.001 = hard rubber, 0.004 = soft yarn)
// resQ      : tube resonator Q (higher = narrower, longer ring)
// resGain   : tube resonator level (0 disables the tube entirely)
// strike    : 0..1 along the bar — 0.5 = center (clean fundamental),
//             0.27 = halfway to the node-line (more partial 2/3 colour)
// tremHz    : optional resonator-disc tremolo in Hz (vibraphone motor;
//             0 = no motor). Modulates total amplitude.
// tremDepth : 0..1 tremolo depth (0.6 = classic slow-vibes wobble)
// noise     : 0..1 broadband stick-click noise burst at the strike,
//             layered with the modal bank (woodblock + xylophone)
export const MARIMBA_PRESETS = {
  // Classic rosewood concert marimba. Long warm fundamental, balanced
  // overtones, full resonator. The "default" sound most people picture.
  rosewood: {
    partials: [1.0, 4.0, 9.2],
    amps:     [1.00, 0.32, 0.10],
    decays:   [1.60, 0.32, 0.09],
    mallet: 0.0025, resQ: 18, resGain: 0.65, strike: 0.5,
    attack: 0.0005, decay: 0.2,
  },
  // Kelon / Klyperon synthetic bar. Higher 3rd mode lands exactly on
  // 10:1 (the design target), slightly brighter, slightly shorter ring.
  kelon: {
    partials: [1.0, 4.0, 10.0],
    amps:     [1.00, 0.40, 0.16],
    decays:   [1.10, 0.26, 0.08],
    mallet: 0.0018, resQ: 20, resGain: 0.55, strike: 0.5,
    attack: 0.0005, decay: 0.2,
  },
  // Bass marimba (4-octave low extension). Mostly tube — the upper
  // partials barely make it out of a 1m PVC pipe. Soft yarn mallet.
  bass: {
    partials: [1.0, 4.0],
    amps:     [1.00, 0.18],
    decays:   [2.40, 0.55],
    mallet: 0.0038, resQ: 25, resGain: 1.00, strike: 0.5,
    attack: 0.0008, decay: 0.3,
  },
  // Dry hard-mallet bar (no resonator). Woody "tok" — exposes the bar
  // alone, useful for layering or for the A/B against the tube boost.
  staccato: {
    partials: [1.0, 4.0, 10.0, 17.0],
    amps:     [1.00, 0.55, 0.28, 0.12],
    decays:   [0.55, 0.18, 0.06, 0.02],
    mallet: 0.0010, resQ: 12, resGain: 0.0, strike: 0.33,
    attack: 0.0003, decay: 0.15,
  },
  // Roll voice: very short attack, just-enough decay to overlap.
  // For tremolo/sustained passages — pair with notes shorter than ring.
  roll: {
    partials: [1.0, 4.0, 9.5],
    amps:     [1.00, 0.30, 0.10],
    decays:   [0.65, 0.20, 0.07],
    mallet: 0.0022, resQ: 16, resGain: 0.50, strike: 0.5,
    attack: 0.0004, decay: 0.12,
  },

  // ── tuned-percussion family ──────────────────────────────────────────
  // The same modal engine generalises across the whole mallet family
  // — change the partial ratios and decays and you've got xylophone,
  // vibraphone, glockenspiel, gamelan-saron, woodblock. Tabulated
  // ratios come from Rossing (2000) chapter 4 + Fletcher & Rossing
  // (1998), *The Physics of Musical Instruments*, chapter 19.

  // Xylophone. Ratios 1 : 3 : 6 (Rossing — "The xylophone has a
  // characteristic 'bright' sound because the third harmonic is at
  // the twelfth, not at the double octave like the marimba.")
  // Rosewood / synthetic bars, much shorter ring than marimba, smaller
  // tube resonators (or none on student xylos). Hard rubber mallet.
  xylophone: {
    partials: [1.0, 3.0, 6.0, 9.6],
    amps:     [1.00, 0.55, 0.28, 0.10],
    decays:   [0.45, 0.18, 0.07, 0.025],
    mallet: 0.0012, resQ: 14, resGain: 0.35, strike: 0.5,
    attack: 0.0004, decay: 0.15, noise: 0.05,
  },

  // Vibraphone. Aluminium bars, ratios 1 : 4 : 10 (same as marimba)
  // but VERY long decay (metal vs. wood losses) — T60 of 5–10s is
  // typical for the fundamental, and the natural attack is much
  // softer because the soft yarn mallet contact time is similar but
  // the bar can't generate the high-mode click that wood does. The
  // motorised disc above each resonator opens and closes the tube
  // mouth at 1–10 Hz, producing the characteristic warble.
  vibraphone: {
    partials: [1.0, 4.0, 10.0],
    amps:     [1.00, 0.22, 0.08],
    decays:   [4.50, 0.90, 0.25],
    mallet: 0.0040, resQ: 22, resGain: 0.80, strike: 0.5,
    tremHz: 5.5, tremDepth: 0.55,
    attack: 0.0010, decay: 0.5,
  },

  // Vibraphone with motor off (the player can disengage the disc).
  // Same as `vibraphone` but no tremolo. Useful for sustained chords.
  vibraphone_off: {
    partials: [1.0, 4.0, 10.0],
    amps:     [1.00, 0.22, 0.08],
    decays:   [4.50, 0.90, 0.25],
    mallet: 0.0040, resQ: 22, resGain: 0.80, strike: 0.5,
    attack: 0.0010, decay: 0.5,
  },

  // Glockenspiel. Small steel bars, high register (G5–C8 typically).
  // Ratios approximately 1 : 2.76 : 5.4 (Fletcher & Rossing — these
  // are LESS tuned than marimba, much closer to the uniform-beam
  // ratios since glockenspiel bars are mostly un-undercut). The
  // perceived pitch is dominated by mode 2 in the high register —
  // mode 1 is so high that mode 2 sits where the ear expects pitch.
  glockenspiel: {
    partials: [1.0, 2.76, 5.40, 8.93],
    amps:     [0.70, 1.00, 0.45, 0.18],
    decays:   [2.20, 1.40, 0.45, 0.12],
    mallet: 0.0008, resQ: 8, resGain: 0.0, strike: 0.5,
    attack: 0.0003, decay: 0.2,
  },

  // Bronze gamelan bar (saron / gendér). Ratios are inharmonic and
  // wider — Fletcher & Rossing give ~1 : 2.4 : 4.7 for a saron-style
  // bronze bar. Slow attack from soft padded mallet, long bronze ring.
  gamelan: {
    partials: [1.0, 2.4, 4.7, 7.2],
    amps:     [1.00, 0.50, 0.30, 0.12],
    decays:   [3.20, 1.20, 0.45, 0.12],
    mallet: 0.0030, resQ: 12, resGain: 0.30, strike: 0.5,
    attack: 0.0008, decay: 0.4,
  },

  // Woodblock / temple block — barely pitched, mostly attack. Very
  // high partials, very fast decay. Half noise, half tone.
  woodblock: {
    partials: [1.0, 1.8, 2.7, 4.1, 6.3],
    amps:     [1.00, 0.85, 0.65, 0.40, 0.20],
    decays:   [0.18, 0.10, 0.06, 0.03, 0.015],
    mallet: 0.0008, resQ: 6, resGain: 0.0, strike: 0.4,
    attack: 0.0002, decay: 0.05, noise: 0.35,
  },

  // Kalimba / mbira / thumb piano. Tine (metal lamella) plucked from
  // the end — single near-pure mode with a slight sub-harmonic from
  // the resonator box. Long decay, low attack noise (no mallet).
  kalimba: {
    partials: [1.0, 5.9, 8.1],
    amps:     [1.00, 0.20, 0.08],
    decays:   [1.80, 0.30, 0.12],
    mallet: 0.0025, resQ: 14, resGain: 0.25, strike: 0.5,
    attack: 0.0010, decay: 0.3,
  },
};

// ── helpers ────────────────────────────────────────────────────────────
function midiToFreq(midi) { return 440 * Math.pow(2, (midi - 69) / 12); }

// ── the DSP engine ─────────────────────────────────────────────────────
// One render pass per note. Mallet impulse → modal sum → tube SVF →
// outer envelope. All single-precision-friendly; no allocations inside
// the sample loop besides the output buffer.
export function renderMarimba(ev, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const presetName = ev.preset || opts.preset || DEFAULT_PRESET;
  const P = { ...(MARIMBA_PRESETS[presetName] || MARIMBA_PRESETS[DEFAULT_PRESET]), ...opts.params };

  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) {
    return new Float32Array(0);
  }
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);

  const attack = opts.attack ?? P.attack ?? 0.0005;
  const release = opts.decay ?? P.decay ?? 0.2;

  // Note runs until the slowest mode has rung out — clipping early
  // truncates the natural decay and makes the bar sound choked.
  // decayMul (read again below) is applied here so the buffer is long
  // enough to hold the stretched tail.
  const decayMulPre = opts.decayMul ?? ev.decayMul ?? 1.0;
  const longestDecay = Math.max(...P.decays) * decayMulPre;
  const tailSec = Math.max(ev.durSec, longestDecay * 1.2) + release;
  const ns = Math.ceil(tailSec * sampleRate);
  const attS = Math.max(1, Math.floor(attack * sampleRate));
  const relS = Math.max(1, Math.floor(release * sampleRate));
  const releaseStart = Math.max(attS + 1, Math.ceil(ev.durSec * sampleRate));
  const out = new Float32Array(ns);

  const f0 = midiToFreq(ev.midi);

  // ── modal oscillator bank ────────────────────────────────────────────
  // Each mode is a damped sinusoid at ratio*f0 with its own decay
  // constant α_n = ln(1000) / T60_n  (so amp drops by 60 dB in T60).
  const M = P.partials.length;
  const freq = new Float64Array(M);
  const amp0 = new Float64Array(M);
  const alpha = new Float64Array(M);
  const phase = new Float64Array(M);
  const ampSum = P.amps.reduce((s, a) => s + a, 0) || 1;

  // decayMul scales every mode's T60 by the same factor. 1.0 = preset
  // value (physically accurate); >1 = longer dreamier ring (lullaby /
  // ambient beds); <1 = drier / more staccato. Same knob applies to
  // every mode so the woody-attack-vs-sustaining-fundamental balance
  // is preserved. Already read above as decayMulPre (for tail sizing).
  const decayMul = decayMulPre;

  // Strike position multiplies each mode by its mode-shape value at
  // that fraction of the bar. For a free-free beam mode n the shape is
  // ~cos(nπx) close enough for our purposes; this is the standard
  // simplification used in Bork (1995) and Chaigne/Doutaut (1997).
  const x = P.strike ?? 0.5;
  for (let n = 0; n < M; n++) {
    freq[n] = f0 * P.partials[n];
    const shape = Math.abs(Math.cos((n + 1) * Math.PI * x));
    amp0[n] = (P.amps[n] / ampSum) * (0.35 + 0.65 * shape);
    alpha[n] = Math.log(1000) / Math.max(0.01, P.decays[n] * decayMul);
  }

  // ── mallet impulse ──────────────────────────────────────────────────
  // Half-cosine pulse over the mallet contact time. Width directly
  // controls the spectrum: short = flat (all modes), long = low-passed
  // (mostly fundamental). This is the same low-cost model used in the
  // Chaigne/Doutaut bar simulations as the boundary excitation force.
  const malletS = Math.max(1, Math.floor((P.mallet ?? 0.002) * sampleRate));

  // ── tube resonator — Chamberlin SVF band-pass at f0 ─────────────────
  // The PVC tube under a marimba bar is closed at the bottom and open
  // at the top (the bar end). At resonance it's a quarter-wavelength
  // resonator emphasising f0 only. We model it as a 2-pole BPF — the
  // SVF is unconditionally stable, parameterised in Hz and Q directly,
  // and matches what we already use for sympathetic strings in zitar.
  const resK = 2 * Math.sin(Math.PI * Math.min(f0, sampleRate / 6) / sampleRate);
  const resDamp = 1 / Math.max(1, P.resQ ?? 18);
  const resGain = P.resGain ?? 0.6;
  let svfLp = 0, svfBp = 0;

  // ── vibraphone-style motor tremolo on the resonator output ──────────
  // Each tube has a spinning disc at its mouth; the disc periodically
  // opens / closes the tube, modulating how much the fundamental is
  // boosted. Modelled here as a sinusoidal gain on the resonator output
  // (closer to "amplitude tremolo" than "vibrato" — the pitch itself
  // does not move). Players set the speed 1–10 Hz.
  const tremHz = P.tremHz ?? 0;
  const tremDepth = Math.max(0, Math.min(1, P.tremDepth ?? 0));
  const tremActive = tremHz > 0 && tremDepth > 0;
  let tremPhase = 0;

  // ── optional stick-click noise — woodblock / xylophone attack ───────
  const noiseAmt = P.noise ?? 0;
  // Simple linear-congruential RNG, deterministic per note. Pure float
  // so it ports straight to C.
  let rngState = (Math.floor(((ev.startSec ?? 0) * 7919 + ev.midi * 41) >>> 0) | 1) >>> 0;
  function noiseSample() {
    rngState = (Math.imul(rngState, 1664525) + 1013904223) >>> 0;
    return (rngState / 0xffffffff) * 2 - 1;
  }
  // Noise burst envelope is a fast exponential — most of the click
  // happens in the first ~5 ms.
  const noiseDecaySec = 0.012;
  const noiseAlpha = noiseAmt > 0 ? Math.log(1000) / noiseDecaySec : 0;

  // ── render loop ─────────────────────────────────────────────────────
  const dt = 1 / sampleRate;
  for (let i = 0; i < ns; i++) {
    // mallet contact force: half-cosine bump, zero outside [0, malletS)
    let force = 0;
    if (i < malletS) {
      force = 0.5 * (1 - Math.cos((Math.PI * 2 * i) / malletS));
    }

    // sum the modal bank — each mode is force-driven during contact,
    // and rings down exponentially as soon as contact ends.
    let dry = 0;
    for (let n = 0; n < M; n++) {
      phase[n] += freq[n] * dt;
      if (phase[n] >= 1) phase[n] -= Math.floor(phase[n]);
      const decayEnv = Math.exp(-alpha[n] * i * dt);
      const s = Math.sin(2 * Math.PI * phase[n]);
      // During contact the force injects energy (modulates amplitude);
      // after contact only the decay remains. Mathematically equivalent
      // to convolving the impulse with each mode's damped-sine kernel.
      const drive = force > 0 ? (0.6 + 0.4 * force) : 1.0;
      dry += s * amp0[n] * decayEnv * drive;
    }

    // optional stick noise — broadband click decaying ~12ms
    if (noiseAmt > 0 && i * dt < 6 * noiseDecaySec) {
      const nEnv = Math.exp(-noiseAlpha * i * dt);
      dry += noiseSample() * noiseAmt * nEnv;
    }

    // tube resonator: band-pass at f0, sum back in
    const hi = dry - svfLp - resDamp * svfBp;
    svfBp += resK * hi;
    svfLp += resK * svfBp;
    let resOut = svfBp * resGain;

    // vibraphone-motor tremolo on the resonator output only — the
    // bar itself keeps ringing; the disc just covers / uncovers the
    // tube mouth, so unboosted partials pass through steady.
    if (tremActive) {
      tremPhase += tremHz * dt;
      if (tremPhase >= 1) tremPhase -= 1;
      const lfo = 0.5 * (1 + Math.sin(2 * Math.PI * tremPhase));
      resOut *= 1 - tremDepth + tremDepth * lfo;
    }

    const wet = dry + resOut;

    // outer envelope: tiny click-suppression attack + linear release
    let env = 1;
    if (i < attS) env = i / attS;
    else if (i >= releaseStart) {
      const r = (i - releaseStart) / relS;
      env = r >= 1 ? 0 : 1 - r;
    }
    if (env <= 0 && i > releaseStart) break;

    out[i] = wet * env * gain;
  }

  return out;
}

// ── node-side buffer mixer ─────────────────────────────────────────────
export function mixEventMarimba(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderMarimba(ev, { ...opts, sampleRate });
  const startIdx = Math.floor((ev.startSec ?? 0) * sampleRate);
  for (let i = 0; i < seg.length; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    out[dst] += seg[i];
  }
}

// ── AC sound.synth fallback for in-browser pieces ──────────────────────
// When the page-side runtime asks for a marimba and we don't have a
// custom node hooked up, fall back to stacked sine voices. Imperfect
// (no per-mode decay envelopes — sound.synth has one ADSR per voice),
// but the modal ratios still come through and it's drop-in compatible
// with the same contract as playSinePower.
export function playMarimba(sound, ev, opts = {}) {
  if (!sound?.synth) return;
  const presetName = opts.preset || DEFAULT_PRESET;
  const P = MARIMBA_PRESETS[presetName] || MARIMBA_PRESETS[DEFAULT_PRESET];
  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) return;
  const evGain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (evGain === 0) return;
  const f0 = 440 * Math.pow(2, (ev.midi - 69) / 12);
  const ampSum = P.amps.reduce((s, a) => s + a, 0) || 1;
  for (let n = 0; n < P.partials.length; n++) {
    sound.synth({
      type: "sine",
      tone: f0 * P.partials[n],
      duration: Math.min(ev.durSec + P.decays[n], P.decays[n] * 1.5),
      volume: evGain * (P.amps[n] / ampSum),
      attack: 0.001,
      decay: P.decays[n] * 0.6,
    });
  }
}

// ── CLI demo ───────────────────────────────────────────────────────────
const isMain =
  process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
if (isMain) {
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
  const HERE = dirname(fileURLToPath(import.meta.url));
  const SR = 48_000;
  const preset = flags.preset || DEFAULT_PRESET;
  const outPath = expandHome(flags.out) || resolve(HERE, "..", "out", `marimba-${preset}.mp3`);

  // A full showcase across the marimba range — opening triad statement,
  // climbing pentatonic run, descending answer, low walking bass, busy
  // 16th-figure in the high register, slow held octaves, then a long
  // tutti cadence. ~75 s. Picks pitches and durations so the per-
  // register colour shifts (and the resonator boost on the low notes,
  // and the woody attack on the high ones) are all obvious in one pass.
  const seq = [
    // I.  opening — rolled triad, mid register
    [60, 0.45], [64, 0.45], [67, 0.45], [72, 0.90],
    [67, 0.45], [64, 0.45], [60, 0.90],

    // II. climbing pentatonic run across two octaves
    [55, 0.30], [57, 0.30], [60, 0.30], [62, 0.30],
    [64, 0.30], [67, 0.30], [69, 0.30], [72, 0.30],
    [74, 0.30], [76, 0.30], [79, 0.60], [76, 0.30], [72, 0.90],

    // III. descending answer, slower
    [79, 0.45], [76, 0.45], [72, 0.45], [69, 0.45],
    [67, 0.45], [64, 0.45], [60, 0.45], [57, 0.90],

    // IV. low walking bass figure — the resonator should boom here
    [48, 0.60], [52, 0.60], [55, 0.60], [60, 0.60],
    [55, 0.60], [52, 0.60], [48, 1.20],
    [43, 0.60], [48, 0.60], [52, 0.60], [55, 0.60],
    [52, 0.60], [48, 0.60], [43, 1.20],

    // V.  busy 16th-figure way up high — mode 2 and 3 should sparkle
    [72, 0.18], [76, 0.18], [79, 0.18], [84, 0.18],
    [79, 0.18], [76, 0.18], [72, 0.18], [76, 0.18],
    [74, 0.18], [77, 0.18], [81, 0.18], [86, 0.18],
    [81, 0.18], [77, 0.18], [74, 0.18], [77, 0.18],
    [76, 0.18], [79, 0.18], [83, 0.18], [88, 0.18],
    [83, 0.18], [79, 0.18], [76, 0.18], [79, 0.18],
    [84, 0.60], [83, 0.60], [81, 0.60], [79, 1.20],

    // VI. slow held octaves — exposes the long mode-1 decay
    [60, 1.80], [62, 1.80], [64, 1.80], [67, 2.40],

    // VII. low/mid two-handed call-and-response
    [48, 0.40], [60, 0.40], [52, 0.40], [64, 0.40],
    [55, 0.40], [67, 0.40], [60, 0.40], [72, 0.80],
    [55, 0.40], [67, 0.40], [52, 0.40], [64, 0.40],
    [48, 0.40], [60, 0.40], [43, 0.80], [55, 0.80],

    // VIII. final tutti — low chord arpeggio building, big held resolve
    [36, 0.45], [48, 0.45], [55, 0.45], [60, 0.45],
    [64, 0.45], [67, 0.45], [72, 0.45], [76, 0.45],
    [79, 0.45], [84, 0.45],
    [36, 3.60], [48, 3.60], [60, 3.60], [64, 3.60], [67, 3.60], [72, 3.60],
  ];
  let t = 0;
  const events = [];
  for (const [midi, d] of seq) {
    events.push({ startSec: t, midi, durSec: d, gain: 0.85, preset });
    t += d;
  }
  const total = t + 2.2;
  const out = new Float32Array(Math.ceil(total * SR));
  console.log(`→ marimba demo · preset=${preset} · ${seq.length} notes`);
  for (const ev of events) mixEventMarimba(ev, out, { sampleRate: SR, preset });

  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const nrm = 0.9 / peak; for (let i = 0; i < out.length; i++) out[i] *= nrm; }
  mkdirSync(dirname(outPath), { recursive: true });
  const rawPath = `${outPath}.f32.raw`;
  const b = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) b.writeFloatLE(out[i], i * 4);
  writeFileSync(rawPath, b);
  const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "3", outPath], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
}
