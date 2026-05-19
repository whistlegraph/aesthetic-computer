#!/usr/bin/env node
// zitar.mjs — the "zitar": a synthesized sitar. Plucked Karplus-Strong
// string + the jawari bridge buzz + a sympathetic-string (taraf)
// resonator halo. Same module shape as ../synths/hoover.mjs and
// ../../dance/synths/skrill.mjs.
//
// WHY SYNTHESIZED, not a Freesound/sampled sitar: the pop/ posture is
// bottom-up (SCORE.md) — no top-down sampling, same reason the jungle
// lane synthesizes its break instead of looping the Amen. So this is a
// model, grounded in known research:
//
//   • Karplus & Strong (1983), Jaffe & Smith EKS (1983) — the plucked
//     string. AC already ships this as the `harp` voice, mirrored in
//     JS (lib/sound/synth.mjs #harpBuf) AND C (fedac/native/src/
//     audio.c generate_harp_sample). The KS core below uses the SAME
//     math + constants so JS↔C parity holds for the ac-native port.
//   • The jawari (curved bone bridge): the string repeatedly buzzes
//     against the bridge, an amplitude-dependent nonlinearity that
//     also slightly shortens the effective delay (a signal-dependent
//     fractional delay — Välimäki et al). That buzz IS the sitar.
//   • Taraf (sympathetic strings) + chikari (drone): a bank of tuned
//     resonators ringing along under the played string. STK's Sitar
//     (Cook/Scavone) is the canonical FOSS model of all this.
//
// C-PORT PATH (ac-native): extend fedac/native/src/audio.c next to
// generate_harp_sample() → generate_zitar_sample(): same delay line,
// add the jawari foldback + delay-mod + a fixed taraf SVF bank. The
// JS mirror would live beside the `harp` case in synth.mjs. Every op
// below is pure float (no JS-only constructs) for exactly that reason.
//
// Library:
//   import { mixEventZitar, renderZitar, ZITAR_PRESETS } from "...";
//   mixEventZitar(ev, out, opts)        // node bed render path
//   renderZitar(ev, opts) -> Float32    // bare DSP engine
//
// CLI demo:
//   node pop/hippyhayzard/synths/zitar.mjs                # sitar preset
//   node pop/hippyhayzard/synths/zitar.mjs --preset drone --out ~/z.mp3
//
// Per-event limitation: the taraf bank rings within each note (great on
// held notes — most of the sitar character). True cross-note sympathetic
// resonance needs a lane-level persistent bank; logged for later.

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "sitar";
const STRING_N = 2048;                  // matches AC harp delay-line cap

// ── presets ────────────────────────────────────────────────────────────
// stretch   : KS decay multiplier (→1 = longer ring; sitar sustains)
// damp      : KS one-pole averaging (lower = brighter/metallic)
// jawari    : bridge-buzz blend 0..1 (the signature)
// jdrive    : buzz waveshaper drive
// jmod      : amplitude→delay shortening, samples (the buzzy pitch-zing)
// taraf     : sympathetic pitches as semitone offsets from the note
// symGain   : sympathetic halo level
// symQ      : sympathetic resonance (higher = longer ring)
// chikari   : drone-octave twang level (open rhythm strings)
export const ZITAR_PRESETS = {
  // Full sitar — buzzing, shimmering, long ring.
  sitar: {
    stretch: 0.9985, damp: 0.18, jawari: 0.62, jdrive: 3.2, jmod: 0.7,
    taraf: [-12, -5, 0, 4, 7, 12, 16], symGain: 0.20, symQ: 42, chikari: 0.10,
    attack: 0.002, decay: 1.9,
  },
  // Brighter melodic lead — less halo, more zing, shorter.
  lead: {
    stretch: 0.9975, damp: 0.12, jawari: 0.70, jdrive: 4.0, jmod: 0.9,
    taraf: [-12, 0, 7, 12], symGain: 0.12, symQ: 30, chikari: 0.06,
    attack: 0.002, decay: 1.1,
  },
  // Drone — chikari-heavy, long tanpura-ish bed.
  drone: {
    stretch: 0.9990, damp: 0.22, jawari: 0.45, jdrive: 2.4, jmod: 0.4,
    taraf: [-24, -12, -5, 0, 7, 12], symGain: 0.28, symQ: 55, chikari: 0.34,
    attack: 0.004, decay: 3.2,
  },
  // Dry KS only — no jawari, for A/B against the buzz.
  dry: {
    stretch: 0.9980, damp: 0.30, jawari: 0.0, jdrive: 1, jmod: 0,
    taraf: [], symGain: 0, symQ: 20, chikari: 0,
    attack: 0.002, decay: 1.2,
  },
};

// ── helpers ────────────────────────────────────────────────────────────
function midiToFreq(midi) { return 440 * Math.pow(2, (midi - 69) / 12); }

function makeRng(seedStr) {
  let s = 2166136261 >>> 0;
  for (let i = 0; i < seedStr.length; i++) {
    s ^= seedStr.charCodeAt(i);
    s = Math.imul(s, 16777619);
  }
  s = s >>> 0 || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}

// ── the DSP engine ─────────────────────────────────────────────────────
export function renderZitar(ev, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const presetName = ev.preset || opts.preset || DEFAULT_PRESET;
  const P = { ...(ZITAR_PRESETS[presetName] || ZITAR_PRESETS[DEFAULT_PRESET]), ...opts.params };

  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) {
    return new Float32Array(0);
  }
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);

  const attack = opts.attack ?? P.attack ?? 0.002;
  const decay  = opts.decay  ?? P.decay  ?? 1.5;
  const durS = Math.ceil(ev.durSec * sampleRate);
  const attS = Math.max(1, Math.floor(attack * sampleRate));
  const decS = Math.max(1, Math.floor(decay * sampleRate));
  const ns   = Math.max(durS, attS + decS);
  const decayStart = ns - decS;
  const out = new Float32Array(ns);

  const freq = midiToFreq(ev.midi);
  let stringDelay = sampleRate / freq;
  if (stringDelay > STRING_N - 2) stringDelay = STRING_N - 2;

  // ── KS pluck: prefill one wavelength with pre-smoothed noise ──
  const rng = makeRng(`zitar:${presetName}:${ev.midi}:${(ev.startSec ?? 0).toFixed(4)}`);
  const buf = new Float32Array(STRING_N);
  const n0 = Math.max(2, Math.round(stringDelay));
  let smooth = 0;
  for (let i = 0; i < n0; i++) {
    const w = rng() * 2 - 1;
    smooth = 0.5 * (w + smooth);          // pre-smooth → softer pluck
    buf[i] = smooth;
  }
  let widx = n0 % STRING_N;
  let lp1 = 0;

  // ── sympathetic (taraf) resonator bank — Chamberlin SVF band-pass ──
  const taraf = P.taraf || [];
  const symN = taraf.length;
  const symK = new Float64Array(symN);
  const symL = new Float64Array(symN);
  const symB = new Float64Array(symN);
  const symDamp = 1 / Math.max(1, P.symQ ?? 40);
  for (let s = 0; s < symN; s++) {
    let fc = freq * Math.pow(2, taraf[s] / 12);
    fc = Math.min(Math.max(40, fc), sampleRate / 6);
    symK[s] = 2 * Math.sin(Math.PI * fc / sampleRate);
  }

  const jawari = P.jawari ?? 0.6;
  const jdrive = P.jdrive ?? 3.0;
  const jmod   = P.jmod ?? 0.7;
  const stretch = P.stretch ?? 0.998;
  const dampMix = P.damp ?? 0.2;          // 0 = pure average (dark), →0.5 brighter
  const chikari = P.chikari ?? 0.1;
  const chF = freq * 2;                   // drone octave
  let chPh = rng();

  for (let i = 0; i < ns; i++) {
    let env;
    if (i < attS) env = i / attS;
    else if (i < decayStart) env = 1;
    else { env = 1 - (i - decayStart) / decS; if (env <= 0) break; }

    // ── jawari: the buzz shortens the effective delay with amplitude
    // (signal-dependent fractional delay — the bridge contact) ──
    let rd = widx - (stringDelay - jmod * Math.min(1, Math.abs(lp1) * 4));
    while (rd < 0) rd += STRING_N;
    const i0 = Math.floor(rd) % STRING_N;
    const i1 = (i0 + 1) % STRING_N;
    const f = rd - Math.floor(rd);
    const delayed = buf[i0] * (1 - f) + buf[i1] * f;

    // KS loop filter: average toward brightness (less damping than harp)
    const filtered = (0.5 + dampMix) * delayed + (0.5 - dampMix) * lp1;
    lp1 = delayed;
    buf[widx] = filtered * stretch;
    widx = (widx + 1) % STRING_N;

    // jawari waveshape: asymmetric foldback buzz blended over the string
    const drv = filtered * jdrive;
    const buzz = Math.tanh(drv) - 0.18 * drv * Math.exp(-drv * drv);
    let s = filtered * (1 - jawari) + buzz * jawari;

    // sympathetic halo — string excites the taraf bank, sums back in
    if (symN) {
      let sym = 0;
      for (let r = 0; r < symN; r++) {
        const hi = filtered - symL[r] - symDamp * symB[r];
        symB[r] += symK[r] * hi;
        symL[r] += symK[r] * symB[r];
        sym += symB[r];
      }
      s += (sym / symN) * (P.symGain ?? 0.2);
    }

    // chikari drone twang (open octave string), decays with the note
    if (chikari > 0) {
      chPh += chF / sampleRate; if (chPh >= 1) chPh -= 1;
      s += (2 * chPh - 1) * chikari * Math.exp(-(i / sampleRate) * 3.0) * 0.5;
    }

    out[i] = s * env * gain;
  }
  return out;
}

// ── node-side buffer mixer ─────────────────────────────────────────────
export function mixEventZitar(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderZitar(ev, { ...opts, sampleRate });
  const startIdx = Math.floor((ev.startSec ?? 0) * sampleRate);
  for (let i = 0; i < seg.length; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    out[dst] += seg[i];
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
  const outPath = expandHome(flags.out) || resolve(HERE, `zitar-${preset}.mp3`);

  // A slow raga-ish phrase in A so the buzz + taraf ring is obvious.
  const seq = [
    [57, 0.9], [60, 0.5], [62, 0.9], [64, 1.4],
    [62, 0.5], [60, 0.9], [57, 1.8],
    [64, 0.6], [66, 0.6], [69, 2.4],
  ];
  let t = 0;
  const events = [];
  for (const [midi, d] of seq) { events.push({ startSec: t, midi, durSec: d, gain: 0.9, preset }); t += d; }
  const total = t + 1.5;
  const out = new Float32Array(Math.ceil(total * SR));
  console.log(`→ zitar demo · preset=${preset} · ${seq.length} notes`);
  for (const ev of events) mixEventZitar(ev, out, { sampleRate: SR, preset });

  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const nrm = 0.85 / peak; for (let i = 0; i < out.length; i++) out[i] *= nrm; }
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
