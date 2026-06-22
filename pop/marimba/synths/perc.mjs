#!/usr/bin/env node
// perc.mjs — a synthesized percussion + bass-perc kit for the marimba lane.
//
// The marimba lane is sample-free on purpose (see ../../SCORE.md and the
// long note at the top of marimba.mjs): we model from physics / first
// principles rather than reaching for a drum sample. A mallet ensemble
// like flutterbap has no low-end groove of its own, so this module adds
// the missing rhythm section as pure synthesis — a punchy kick, a gritty
// pitched "bass-perc" (the 808-flavoured low voice that is half bassline,
// half drum), a snappy snare, hats (closed + open) and a shaker.
//
// ── why these models ───────────────────────────────────────────────────
// Each voice is the cheapest model that still reads as the real thing:
//
//   • kick   — a sine body whose pitch drops fast from a "click" pitch to
//     a sub fundamental (the classic 808/909 pitch-envelope), softened
//     through a tanh so it punches, plus a tiny broadband transient for
//     the beater click. (Pitch-drop kick: every drum machine since the
//     TR-808; see Gordon Reid, "Synth Secrets" pt.35, Sound on Sound.)
//   • bass   — a pitched sub voice with a short downward pitch glide and
//     optional tanh drive. One knob (`drive`) walks it from a clean sine
//     sub to a saturated, harmonically-rich bass that cuts through a mix
//     on small speakers. It IS the bassline and the low percussion at
//     once — hence "bass-perc".
//   • snare  — two low sines (the drum's tuned shell modes ~180/330 Hz)
//     plus a high-passed noise burst (the snares). Classic additive snare.
//   • hat    — high-passed white noise with a fast (closed) or slow (open)
//     exponential decay. Cheap but convincing for a bright daytime track.
//   • shaker — band-passed noise with a soft (non-transient) attack — the
//     "tss" of beads rather than a struck membrane.
//
// All DSP is pure float with deterministic per-event noise — no JS-only
// constructs — so every voice ports straight into C (fedac/native audio.c
// or a maytrax-style engine) with no math changes, exactly like
// marimba.mjs.
//
// ── library ──────────────────────────────────────────────────────────
//   import { mixKick, mixBassPerc, mixSnare, mixHat, mixShaker } from "...";
//   mixKick({ startSec, gain }, out, { sampleRate })
//   mixBassPerc({ startSec, midi, durSec, gain, drive }, out, { sampleRate })
//   renderKick(ev, opts) -> Float32Array          // bare DSP
//
// ── CLI demo ───────────────────────────────────────────────────────────
//   node pop/marimba/synths/perc.mjs                 # one bar of the kit
//   node pop/marimba/synths/perc.mjs --out ~/kit.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const LN1000 = Math.log(1000); // T60 helper: alpha = ln(1000) / T60

function midiToFreq(midi) { return 440 * Math.pow(2, (midi - 69) / 12); }

// Deterministic LCG noise, seeded per event so a given hit always sounds
// identical (reproducible renders). Pure float → ports to C verbatim.
function makeNoise(seed) {
  let s = (seed >>> 0) || 1;
  return () => {
    s = (Math.imul(s, 1664525) + 1013904223) >>> 0;
    return (s / 0xffffffff) * 2 - 1;
  };
}

// ── KICK ─────────────────────────────────────────────────────────────
// A pitch-enveloped sine drum. The instantaneous frequency starts high
// (the beater "click" pitch) and decays exponentially to a sub
// fundamental; the body amplitude rings down over its own T60. A short
// noise transient adds the beater attack. tanh keeps the low end punchy
// rather than letting big sine excursions go flabby.
export function renderKick(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  const fStart = ev.fStart ?? 150;     // click pitch (Hz)
  const fEnd   = ev.fEnd ?? 48;        // sub fundamental (Hz)
  const pitchT = ev.pitchDecay ?? 0.055; // how fast the pitch falls (s)
  const ampT60 = ev.ampDecay ?? 0.34;  // body T60 (s)
  const clickAmt = ev.click ?? 0.55;
  const drive = ev.drive ?? 1.6;       // tanh punch
  if (gain === 0) return new Float32Array(0);

  const tail = ampT60 * 1.3 + 0.02;
  const ns = Math.ceil(tail * SR);
  const out = new Float32Array(ns);
  const ampA = LN1000 / ampT60;
  const noise = makeNoise(Math.floor((ev.startSec ?? 0) * 9173) | 1);
  let phase = 0;
  const dt = 1 / SR;
  for (let i = 0; i < ns; i++) {
    const t = i * dt;
    const f = fEnd + (fStart - fEnd) * Math.exp(-t / pitchT);
    phase += f * dt;
    const body = Math.tanh(Math.sin(2 * Math.PI * phase) * drive) / Math.tanh(drive);
    const ampEnv = Math.exp(-ampA * t);
    let s = body * ampEnv;
    // beater click — broadband, gone in ~4 ms
    if (t < 0.006) s += noise() * clickAmt * Math.exp(-t / 0.0012);
    out[i] = s * gain;
  }
  return out;
}

// ── BASS-PERC ──────────────────────────────────────────────────────────
// The signature low voice: a pitched sub with a quick downward pitch glide
// at the onset (the "dwop") and a tanh drive that adds bite. Decays over
// `decay` (T60) so a sparse line rings like an 808, or set a short decay +
// fast notes for a plucky percussive bass. A tiny transient click gives it
// the percussive snap that ties it to the drums. This is what carries the
// groove under the mallets.
export function renderBassPerc(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  if (!Number.isFinite(ev.midi)) return new Float32Array(0);
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);
  const f0 = midiToFreq(ev.midi);
  const pitchUp = ev.pitchUp ?? 5;     // semitones the onset glides down from
  const pitchT = ev.pitchDecay ?? 0.022;
  const decay = ev.decay ?? 0.5;       // body T60 (s)
  const drive = ev.drive ?? 2.2;       // grit / saturation
  const sub = ev.sub ?? 0.35;          // amount of -1 octave reinforcement
  const clickAmt = ev.click ?? 0.18;
  const durSec = ev.durSec ?? decay;

  // ring until the body has decayed; release-fade if the note is gated short
  const tail = Math.max(durSec, decay) * 1.25 + 0.03;
  const ns = Math.ceil(tail * SR);
  const out = new Float32Array(ns);
  const ampA = LN1000 / decay;
  const fHi = f0 * Math.pow(2, pitchUp / 12);
  const noise = makeNoise((Math.floor((ev.startSec ?? 0) * 7919) + ev.midi * 53) | 1);
  const relS = Math.floor(0.03 * SR);
  const gateS = Math.floor(durSec * SR);
  let phase = 0, phaseSub = 0;
  const dt = 1 / SR;
  const norm = Math.tanh(drive);
  for (let i = 0; i < ns; i++) {
    const t = i * dt;
    const f = f0 + (fHi - f0) * Math.exp(-t / pitchT);
    phase += f * dt;
    phaseSub += (f * 0.5) * dt;
    let body = Math.sin(2 * Math.PI * phase) + sub * Math.sin(2 * Math.PI * phaseSub);
    body = Math.tanh(body * drive) / norm;
    let env = Math.exp(-ampA * t);
    // gate release so a short note doesn't ring into the next
    if (i > gateS) { const r = (i - gateS) / relS; env *= r >= 1 ? 0 : 1 - r; }
    let s = body * env;
    if (t < 0.005) s += noise() * clickAmt * Math.exp(-t / 0.0015);
    out[i] = s * gain;
    if (env <= 0 && i > gateS) break;
  }
  return out;
}

// ── SNARE ────────────────────────────────────────────────────────────
// Tuned shell modes (two low sines) + a high-passed noise burst for the
// snares. toneAmt / noiseAmt balance the body against the rasp.
export function renderSnare(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);
  const toneT60 = ev.toneDecay ?? 0.10;
  const noiseT60 = ev.noiseDecay ?? 0.16;
  const toneAmt = ev.tone ?? 0.55;
  const noiseAmt = ev.noise ?? 0.9;
  const f1 = ev.f1 ?? 185, f2 = ev.f2 ?? 330;
  const tail = Math.max(toneT60, noiseT60) * 1.4 + 0.01;
  const ns = Math.ceil(tail * SR);
  const out = new Float32Array(ns);
  const aT = LN1000 / toneT60, aN = LN1000 / noiseT60;
  const noise = makeNoise(Math.floor((ev.startSec ?? 0) * 6271) | 1);
  let hp = 0, prevN = 0;
  const dt = 1 / SR;
  for (let i = 0; i < ns; i++) {
    const t = i * dt;
    const tone = (Math.sin(2 * Math.PI * f1 * t) * 0.6 + Math.sin(2 * Math.PI * f2 * t) * 0.4)
      * Math.exp(-aT * t) * toneAmt;
    const nRaw = noise();
    hp = 0.85 * (hp + nRaw - prevN); prevN = nRaw; // one-pole high-pass → bright snares
    const nz = hp * Math.exp(-aN * t) * noiseAmt;
    out[i] = (tone + nz) * gain;
  }
  return out;
}

// ── HAT ──────────────────────────────────────────────────────────────
// High-passed white noise; fast decay for a closed hat, long for an open
// one (pass ev.open = true). Bright, simple, daytime.
export function renderHat(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);
  const t60 = ev.decay ?? (ev.open ? 0.26 : 0.045);
  const tail = t60 * 1.5 + 0.005;
  const ns = Math.ceil(tail * SR);
  const out = new Float32Array(ns);
  const a = LN1000 / t60;
  const noise = makeNoise(Math.floor((ev.startSec ?? 0) * 5147) | 1);
  let hp = 0, prevN = 0, hp2 = 0, prevH = 0;
  const dt = 1 / SR;
  for (let i = 0; i < ns; i++) {
    const t = i * dt;
    const nRaw = noise();
    hp = 0.92 * (hp + nRaw - prevN); prevN = nRaw;   // two cascaded high-passes →
    hp2 = 0.92 * (hp2 + hp - prevH); prevH = hp;       // very bright, metallic-ish
    out[i] = hp2 * Math.exp(-a * t) * gain;
  }
  return out;
}

// ── SHAKER ──────────────────────────────────────────────────────────
// Band-passed noise with a soft raised-cosine attack — beads sliding, not
// a struck head. Sits on the off-beats to keep the groove moving.
export function renderShaker(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);
  const t60 = ev.decay ?? 0.06;
  const attS = Math.max(1, Math.floor((ev.attack ?? 0.008) * SR));
  const tail = t60 * 1.6 + 0.01;
  const ns = Math.ceil(tail * SR);
  const out = new Float32Array(ns);
  const a = LN1000 / t60;
  const noise = makeNoise(Math.floor((ev.startSec ?? 0) * 4019) | 1);
  // band-pass ≈ one-pole HP then one-pole LP, centred ~6 kHz
  let hp = 0, prevN = 0, lp = 0;
  const lpC = Math.min(1, (2 * Math.PI * 6500) / SR);
  const dt = 1 / SR;
  for (let i = 0; i < ns; i++) {
    const t = i * dt;
    const nRaw = noise();
    hp = 0.7 * (hp + nRaw - prevN); prevN = nRaw;
    lp += lpC * (hp - lp);
    let env = Math.exp(-a * t);
    if (i < attS) env *= i / attS;
    out[i] = lp * env * gain;
  }
  return out;
}

// ── REVERSE SINE BELL (transition riser) ───────────────────────────────
// Not percussion, but it ships with the kit because risers live with the
// drums in production. A glassy FM sine bell rendered forward (sharp attack
// + exponential decay) and then PLAYED BACKWARDS: the decay tail becomes a
// swell that rises out of nothing, and the bell's bright attack transient
// lands exactly on the downbeat. The modulator index is brightest at the
// original attack, so reversed the swell gets glassier as it rises — the
// classic reverse-cymbal riser, but tonal and pure sine (same family as
// the glasstrax bells). Pass a chord via `midis` (or a single `midi`), and
// `landSec` = the downbeat the transient should hit.
export function renderReverseBell(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 0.5;
  const dur = ev.dur ?? 2.0;           // swell length (s)
  const decay = ev.decay ?? 2.6;       // forward decay rate (↑ = quicker swell)
  const ratio = ev.ratio ?? 3.5;       // FM ratio — glassy bell timbre
  const midis = ev.midis ?? (Number.isFinite(ev.midi) ? [ev.midi] : [84]);
  const ns = Math.ceil(dur * SR);
  const fwd = new Float32Array(ns);
  const dt = 1 / SR;
  // render the bell(s) forward — same FM voice as glasstrax's bell()
  for (const m of midis) {
    const fc = midiToFreq(m), fm = fc * ratio;
    let phc = 0, phm = 0;
    for (let i = 0; i < ns; i++) {
      const tt = i * dt;
      const env = Math.exp(-tt * decay);
      const mi = 6.0 * Math.exp(-tt * 11.0);          // bright attack transient
      phm += 2 * Math.PI * fm * dt;
      phc += 2 * Math.PI * fc * dt + Math.sin(phm) * mi * dt;
      fwd[i] += Math.sin(phc) * env;
    }
  }
  // reverse → swell, normalise by chord size
  const out = new Float32Array(ns);
  const norm = gain / Math.max(1, Math.sqrt(midis.length));
  for (let i = 0; i < ns; i++) out[i] = fwd[ns - 1 - i] * norm;
  return out;
}

// ── REVERSE KICK (low-end riser) ───────────────────────────────────────
// The reverse bell's low-end companion: a kick rendered forward with a long
// boomy tail, then played BACKWARDS. Reversed, the pitch envelope sweeps
// UP (low → high) and the amplitude swells, so it whooshes upward and the
// beater transient lands on the downbeat (`landSec`). Sits under a reverse
// bell at a break for a big two-band (sub + glass) lift into the drop.
export function renderReverseKick(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 0.7;
  const dur = ev.dur ?? 1.2;            // swell length (s)
  const fStart = ev.fStart ?? 190;      // (forward) click pitch
  const fEnd = ev.fEnd ?? 48;           // (forward) sub fundamental
  const pitchT = ev.pitchDecay ?? 0.13;
  const ampT60 = ev.ampDecay ?? dur * 0.55;  // boomy long tail → smooth swell
  const drive = ev.drive ?? 1.6;
  const ns = Math.ceil(dur * SR);
  const fwd = new Float32Array(ns);
  const ampA = LN1000 / ampT60;
  const noise = makeNoise(Math.floor((ev.landSec ?? 0) * 8311) | 1);
  const norm = Math.tanh(drive);
  let phase = 0;
  const dt = 1 / SR;
  for (let i = 0; i < ns; i++) {
    const t = i * dt;
    const f = fEnd + (fStart - fEnd) * Math.exp(-t / pitchT);
    phase += f * dt;
    const body = Math.tanh(Math.sin(2 * Math.PI * phase) * drive) / norm;
    let s = body * Math.exp(-ampA * t);
    if (t < 0.006) s += noise() * 0.5 * Math.exp(-t / 0.0012);
    fwd[i] = s;
  }
  const out = new Float32Array(ns);
  for (let i = 0; i < ns; i++) out[i] = fwd[ns - 1 - i] * gain;
  return out;
}
export function mixReverseKick(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderReverseKick(ev, opts);
  const land = ev.landSec ?? ((ev.startSec ?? 0) + (ev.dur ?? 1.2));
  const startIdx = Math.floor(land * SR) - seg.length;
  for (let i = 0; i < seg.length; i++) { const d = startIdx + i; if (d >= 0 && d < out.length) out[d] += seg[i]; }
}

// ── TURNTABLE SCRATCH ──────────────────────────────────────────────────
// A scratch built from scratch — no vinyl sampled, the record is synthetic.
// Step 1: synthesize a buzzy vocal-ish source (saw harmonics + surface grit,
// shaped by a resonant formant ≈ an "aah"). Step 2: "scrub" it — read the
// source back at a position that oscillates with the DJ's hand. The read
// VELOCITY is depth·sin(2π·rate·t): on the forward swing the tone pitches up,
// on the back swing it plays in REVERSE — that bending back-and-forth is the
// scratch. A crossfader gate chops it on/off for transform / chirp styles.
//   rate  — hand back-and-forth speed (Hz): ~5 baby, ~14 scribble
//   depth — how hard the hand throws it (read-speed swing → pitch range)
//   gate  — crossfader chop rate (Hz); 0 = open fader (baby scratch)
export function renderScratch(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 0.6;
  const dur = ev.dur ?? 0.32;
  const midi = ev.midi ?? 50;
  const rate = ev.rate ?? 7;
  const depth = ev.depth ?? 2.6;
  const gateHz = ev.gate ?? 0;
  const gateDuty = ev.gateDuty ?? 0.5;
  const tone = ev.tone ?? 0.55;       // source brightness / buzz
  const f0 = midiToFreq(midi);
  const dt = 1 / SR;
  const noise = makeNoise((Math.floor((ev.startSec ?? 0) * 3301) + midi * 17) | 1);

  // ── source: a rich buzzy "aah", long enough to scrub within ──
  const srcLen = Math.ceil(0.4 * SR);
  const src = new Float32Array(srcLen);
  const nh = Math.max(4, Math.floor(8 + tone * 22));     // # harmonics (saw-ish)
  let bp = 0, lp = 0;
  const fc = 900, k = 2 * Math.sin(Math.PI * fc / SR), damp = 0.22;  // formant ≈ vowel
  for (let i = 0; i < srcLen; i++) {
    const tt = i * dt;
    let s = 0;
    for (let h = 1; h <= nh; h++) s += Math.sin(2 * Math.PI * f0 * h * tt) / h;
    s = s * 0.5 + noise() * 0.18 * tone;
    const hi = s - lp - damp * bp; bp += k * hi; lp += k * bp;   // resonant formant
    src[i] = s * 0.5 + bp * 0.9;
  }
  let sp = 0; for (let i = 0; i < srcLen; i++) { const a = Math.abs(src[i]); if (a > sp) sp = a; }
  if (sp > 0) { const n = 0.9 / sp; for (let i = 0; i < srcLen; i++) src[i] *= n; }

  // ── scrub: read src at the oscillating hand position ──
  const ns = Math.ceil(dur * SR);
  const out = new Float32Array(ns);
  const center = srcLen * 0.5;
  const amp = depth * SR / (2 * Math.PI * rate);     // position swing (samples)
  const attS = Math.floor(0.004 * SR), relS = Math.floor(0.02 * SR);
  let gSmooth = 1;
  for (let i = 0; i < ns; i++) {
    const tt = i * dt;
    // pos = center − amp·cos → velocity = depth·sin (starts at rest, like a hand)
    let pos = center - amp * Math.cos(2 * Math.PI * rate * tt);
    if (pos < 0) pos = 0; else if (pos >= srcLen - 1) pos = srcLen - 2;
    const i0 = Math.floor(pos), fr = pos - i0;
    let smp = src[i0] * (1 - fr) + src[i0 + 1] * fr;
    if (gateHz > 0) {                                  // crossfader chop (smoothed ~1ms)
      const ph = (tt * gateHz) % 1;
      gSmooth += ((ph < gateDuty ? 1 : 0) - gSmooth) * 0.02;
      smp *= gSmooth;
    }
    let env = 1;
    if (i < attS) env = i / attS;
    else if (i > ns - relS) env = Math.max(0, (ns - i) / relS);
    out[i] = smp * env * gain;
  }
  return out;
}
export function mixScratch(ev, out, opts = {}) {
  if (out instanceof Float32Array) mixInto(renderScratch(ev, opts), ev, out, opts.sampleRate ?? DEFAULT_SAMPLE_RATE);
}

// ── KITTEN SCREAM ──────────────────────────────────────────────────────
// A scratchy little shriek. A high glottal saw source sweeps up then falls
// (the "myaaa!" cry contour) with fast vibrato; two vocal-tract formants
// give it a vowel; and the SCREAM comes from going nonlinear like a real
// strained throat — a subharmonic an octave down flutters in, pitch
// roughness jitters the fundamental, and a tanh stage drives the whole
// thing into rasp. `rasp` (0..1) sets how feral. Pitch it high (midi ~81–91)
// for a kitten; lower for a bigger, angrier cat.
export function renderScream(ev, opts = {}) {
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const gain = Number.isFinite(ev.gain) ? ev.gain : 0.5;
  const dur = ev.dur ?? 0.5;
  const midi = ev.midi ?? 86;          // base cry pitch (high)
  const bend = ev.bend ?? 7;           // semitone rise of the cry
  const rasp = Math.max(0, Math.min(1, ev.rasp ?? 0.65));
  const f0 = midiToFreq(midi);
  const ns = Math.ceil(dur * SR);
  const out = new Float32Array(ns);
  const dt = 1 / SR;
  const noise = makeNoise((Math.floor((ev.startSec ?? 0) * 2719) + midi * 29) | 1);
  // two vocal-tract formants
  let bp1 = 0, lp1 = 0, bp2 = 0, lp2 = 0;
  const k1 = 2 * Math.sin(Math.PI * 950 / SR), k2 = 2 * Math.sin(Math.PI * 2500 / SR), dmp = 0.16;
  let ph = 0, phSub = 0, roughLP = 0;
  const drive = 1 + rasp * 3;
  const norm = Math.tanh(drive);
  for (let i = 0; i < ns; i++) {
    const tt = i * dt, u = tt / dur;
    // cry contour: pitch rises fast over the first third, then sags
    const rise = Math.pow(2, (bend / 12) * Math.sin(Math.PI * Math.min(1, u * 1.35)));
    const vibrato = 1 + 0.035 * Math.sin(2 * Math.PI * 17 * tt);
    roughLP += (noise() - roughLP) * 0.4;                  // smoothed pitch roughness
    const f = f0 * rise * vibrato * (1 + rasp * 0.05 * roughLP);
    ph += f * dt; phSub += f * 0.5 * dt;
    // glottal saw source (band-limited to Nyquist) + subharmonic strain
    let s = 0; const nh = Math.min(24, Math.floor(SR / 2 / f));
    for (let h = 1; h <= nh; h++) s += Math.sin(2 * Math.PI * ph * h) / h;
    s = s * 0.5 + rasp * 0.45 * Math.sin(2 * Math.PI * phSub) * Math.min(1, u * 4);  // strain fades in
    s += noise() * rasp * 0.22;                            // breath / hiss
    // formants
    const hi1 = s - lp1 - dmp * bp1; bp1 += k1 * hi1; lp1 += k1 * bp1;
    const hi2 = s - lp2 - dmp * bp2; bp2 += k2 * hi2; lp2 += k2 * bp2;
    let v = s * 0.35 + bp1 * 0.8 + bp2 * 0.5;
    v = Math.tanh(v * drive) / norm;                       // scream into nonlinearity
    // amplitude cry envelope: fast in, swell at the rise, fall away
    let env = Math.min(1, u * 12) * (0.55 + 0.45 * Math.sin(Math.PI * Math.min(1, u * 1.1)));
    if (u > 0.85) env *= Math.max(0, (1 - u) / 0.15);
    out[i] = v * env * gain;
  }
  return out;
}
export function mixScream(ev, out, opts = {}) {
  if (out instanceof Float32Array) mixInto(renderScream(ev, opts), ev, out, opts.sampleRate ?? DEFAULT_SAMPLE_RATE);
}

// ── node-side buffer mixers ────────────────────────────────────────────
function mixInto(seg, ev, out, SR) {
  const startIdx = Math.floor((ev.startSec ?? 0) * SR);
  for (let i = 0; i < seg.length; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    out[dst] += seg[i];
  }
}
export function mixKick(ev, out, opts = {}) {
  if (out instanceof Float32Array) mixInto(renderKick(ev, opts), ev, out, opts.sampleRate ?? DEFAULT_SAMPLE_RATE);
}
export function mixBassPerc(ev, out, opts = {}) {
  if (out instanceof Float32Array) mixInto(renderBassPerc(ev, opts), ev, out, opts.sampleRate ?? DEFAULT_SAMPLE_RATE);
}
export function mixSnare(ev, out, opts = {}) {
  if (out instanceof Float32Array) mixInto(renderSnare(ev, opts), ev, out, opts.sampleRate ?? DEFAULT_SAMPLE_RATE);
}
export function mixHat(ev, out, opts = {}) {
  if (out instanceof Float32Array) mixInto(renderHat(ev, opts), ev, out, opts.sampleRate ?? DEFAULT_SAMPLE_RATE);
}
export function mixShaker(ev, out, opts = {}) {
  if (out instanceof Float32Array) mixInto(renderShaker(ev, opts), ev, out, opts.sampleRate ?? DEFAULT_SAMPLE_RATE);
}
// reverse bell lands its transient on ev.landSec — the swell occupies the
// `dur` seconds BEFORE that downbeat.
export function mixReverseBell(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const SR = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderReverseBell(ev, opts);
  const land = ev.landSec ?? ((ev.startSec ?? 0) + (ev.dur ?? 2.0));
  const startIdx = Math.floor(land * SR) - seg.length;
  for (let i = 0; i < seg.length; i++) { const d = startIdx + i; if (d >= 0 && d < out.length) out[d] += seg[i]; }
}

// ── CLI demo — one bar of the full kit at 120 BPM ──────────────────────
const isMain = process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
if (isMain) {
  const argv = process.argv.slice(2);
  const flags = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a.startsWith("--")) { const k = a.slice(2), n = argv[i + 1]; if (n && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true; }
  }
  const expand = (p) => (p && p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p);
  const HERE = dirname(fileURLToPath(import.meta.url));
  const SR = 48_000;
  const BPM = 120, BEAT = 60 / BPM;
  const bars = 4, total = bars * 4 * BEAT + 1.0;
  const out = new Float32Array(Math.ceil(total * SR));
  for (let bar = 0; bar < bars; bar++) {
    const b0 = bar * 4 * BEAT;
    // four-on-the-floor-ish groove with the bass-perc walking C–G
    mixKick({ startSec: b0 + 0 * BEAT, gain: 0.95 }, out, { sampleRate: SR });
    mixKick({ startSec: b0 + 2 * BEAT, gain: 0.85 }, out, { sampleRate: SR });
    mixSnare({ startSec: b0 + 1 * BEAT, gain: 0.7 }, out, { sampleRate: SR });
    mixSnare({ startSec: b0 + 3 * BEAT, gain: 0.7 }, out, { sampleRate: SR });
    for (let e = 0; e < 8; e++)
      mixHat({ startSec: b0 + e * 0.5 * BEAT, gain: e % 2 ? 0.32 : 0.18, open: e === 7 }, out, { sampleRate: SR });
    mixBassPerc({ startSec: b0 + 0 * BEAT, midi: 36, durSec: BEAT * 1.5, gain: 0.8 }, out, { sampleRate: SR });
    mixBassPerc({ startSec: b0 + 2.5 * BEAT, midi: 43, durSec: BEAT * 1.0, gain: 0.7 }, out, { sampleRate: SR });
  }
  let peak = 0; for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const n = 0.9 / peak; for (let i = 0; i < out.length; i++) out[i] *= n; }
  const outPath = expand(flags.out) || resolve(HERE, "..", "out", "perc-demo.mp3");
  mkdirSync(dirname(outPath), { recursive: true });
  const raw = `${outPath}.f32.raw`;
  const buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
  writeFileSync(raw, buf);
  const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", raw,
    "-c:a", "libmp3lame", "-q:a", "3", outPath], { stdio: "inherit" });
  try { unlinkSync(raw); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
}
