#!/usr/bin/env node
// hoover.mjs — the "hoover": the Roland Alpha Juno-2 "What The..."
// patch (Second Phase "Mentasm", 1991) that every rave / happy-hardcore
// track is legally required to contain. The gating new voice for the
// hippyhayzard lane, same contract shape as ../../dance/synths/skrill.mjs.
//
// Like skrill, the hoover CANNOT be a fan-out of plain sound.synth()
// voices — its identity is three things the AC voice contract (bus.mjs)
// has no slot for:
//
//   1. a detuned SAW+PULSE stack with PWM (pulse width swept slowly) —
//      the buzzy chorused body.
//   2. the WHOOP — a per-note pitch envelope that bends down into the
//      note and springs back up. Without the whoop it is just a saw
//      stack; the whoop is the hoover.
//   3. a resonant low-pass (SVF) giving the honky vowel-ish formant.
//
// So it hand-rolls per-sample DSP straight into the render buffer, then
// soft-drives for the chunky rave grit.
//
// Library:
//   import { mixEventHoover, renderHoover, HOOVER_PRESETS } from "...";
//   mixEventHoover(ev, out, opts)        // node bed render path
//   renderHoover(ev, opts) -> Float32    // bare DSP engine
//
// CLI demo:
//   node pop/hippyhayzard/synths/hoover.mjs                 # whoop preset
//   node pop/hippyhayzard/synths/hoover.mjs --preset stab --bpm 174
//   node pop/hippyhayzard/synths/hoover.mjs --preset hazard --out ~/h.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "whoop";
const DEFAULT_BPM = 174;

// ── presets ────────────────────────────────────────────────────────────
// voices    : detuned saw count (PWM pulse is added on top)
// detune    : ± cents spread across the saw stack
// pulseGain : square/PWM layer level (the buzz)
// pwmRate   : Hz, pulse-width sweep rate
// whoop     : [startSemitones, settleSec] — pitch bends from start →0
//             over settleSec then a small overshoot up (the rave whoop)
// vibRate/Depth : post-whoop pitch vibrato (cents)
// cutoff    : SVF low-pass base (× note freq, clamped) — the honk
// q         : resonance
// drive     : tanh grit
// subGain   : clean sine sub for chunk
export const HOOVER_PRESETS = {
  // Classic Mentasm hoover: big down-whoop, fat, mid honk.
  whoop: {
    voices: 5, detune: 22, pulseGain: 0.55, pwmRate: 1.3,
    whoop: [-7, 0.11], vibRate: 5.5, vibDepth: 14,
    cutoff: 7.0, q: 4.5, drive: 2.0, subGain: 0.34,
    attack: 0.004, decay: 0.16,
  },
  // Tight stab — short whoop, plucky, for 16th-note rave riffs.
  stab: {
    voices: 5, detune: 18, pulseGain: 0.5, pwmRate: 2.0,
    whoop: [-4, 0.05], vibRate: 6.0, vibDepth: 8,
    cutoff: 8.5, q: 4.0, drive: 2.2, subGain: 0.28,
    attack: 0.003, decay: 0.09,
  },
  // Hazard siren — deep slow whoop, narrow honk, the dark switch.
  hazard: {
    voices: 7, detune: 30, pulseGain: 0.7, pwmRate: 0.7,
    whoop: [-12, 0.30], vibRate: 4.0, vibDepth: 26,
    cutoff: 5.0, q: 6.5, drive: 2.8, subGain: 0.5,
    attack: 0.010, decay: 0.40,
  },
  // Sustained chord pad — almost no whoop, slow PWM, long tail.
  pad: {
    voices: 7, detune: 26, pulseGain: 0.4, pwmRate: 0.4,
    whoop: [-1.5, 0.25], vibRate: 4.5, vibDepth: 6,
    cutoff: 6.0, q: 3.0, drive: 1.4, subGain: 0.4,
    attack: 0.20, decay: 1.6,
  },
};

// ── helpers ────────────────────────────────────────────────────────────
function midiToFreq(midi) {
  return 440 * Math.pow(2, (midi - 69) / 12);
}

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

function buildDetune(voices, cents) {
  const t = new Float64Array(voices);
  const half = (voices - 1) / 2;
  for (let i = 0; i < voices; i++) {
    t[i] = ((i - half) / Math.max(1, half)) * cents;
  }
  return t;
}

// ── the DSP engine ─────────────────────────────────────────────────────
export function renderHoover(ev, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const presetName = ev.preset || opts.preset || DEFAULT_PRESET;
  const P = { ...(HOOVER_PRESETS[presetName] || HOOVER_PRESETS[DEFAULT_PRESET]), ...opts.params };

  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) {
    return new Float32Array(0);
  }
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);

  const attack = opts.attack ?? P.attack ?? 0.004;
  const decay  = opts.decay  ?? P.decay  ?? 0.16;
  const durS = Math.ceil(ev.durSec * sampleRate);
  const attS = Math.max(1, Math.floor(attack * sampleRate));
  const decS = Math.max(1, Math.floor(decay * sampleRate));
  const ns   = Math.max(durS, attS + decS);
  const decayStart = ns - decS;
  const out = new Float32Array(ns);

  const baseF = midiToFreq(ev.midi);
  const voices = P.voices ?? 5;
  const detune = buildDetune(voices, P.detune ?? 22);
  const sawPhase = new Float64Array(voices);
  const rng = makeRng(`hoover:${presetName}:${ev.midi}:${(ev.startSec ?? 0).toFixed(4)}`);
  for (let v = 0; v < voices; v++) sawPhase[v] = rng();

  let pulPhase = rng(), pwmPhase = rng(), subPhase = 0, vibPhase = rng();
  let sLow = 0, sBand = 0;
  const damp = 1 / Math.max(0.5, P.q ?? 4.5);
  const [whoopSemi, whoopSec] = P.whoop ?? [-7, 0.11];
  const whoopSamp = Math.max(1, Math.floor(whoopSec * sampleRate));
  const ampNorm = 1 / Math.max(1, voices);
  const TWO_PI = 2 * Math.PI;

  for (let i = 0; i < ns; i++) {
    let env;
    if (i < attS) env = i / attS;
    else if (i < decayStart) env = 1;
    else { env = 1 - (i - decayStart) / decS; if (env <= 0) break; }

    // ── the WHOOP: pitch bends from whoopSemi → 0 with a small spring
    // overshoot, then settles into post-whoop vibrato. ──
    let bendSemi;
    if (i < whoopSamp) {
      const w = i / whoopSamp;                     // 0..1
      // ease-out + a touch of overshoot past 0 (the spring up)
      bendSemi = whoopSemi * (1 - w) + 1.2 * Math.sin(Math.PI * w) * (w > 0.6 ? 1 : 0);
    } else {
      bendSemi = 0;
    }
    vibPhase += (P.vibRate ?? 5.5) / sampleRate; if (vibPhase >= 1) vibPhase -= 1;
    const vibSemi = (Math.sin(TWO_PI * vibPhase) * (P.vibDepth ?? 14)) / 100;
    const pitchMul = Math.pow(2, (bendSemi + vibSemi) / 12);

    // ── detuned saw stack ──
    let saw = 0;
    for (let v = 0; v < voices; v++) {
      const f = baseF * Math.pow(2, detune[v] / 1200) * pitchMul;
      sawPhase[v] += f / sampleRate; if (sawPhase[v] >= 1) sawPhase[v] -= 1;
      saw += 2 * sawPhase[v] - 1;
    }
    saw *= ampNorm;

    // ── PWM pulse layer (the buzz) ──
    const f0 = baseF * pitchMul;
    pulPhase += f0 / sampleRate; if (pulPhase >= 1) pulPhase -= 1;
    pwmPhase += (P.pwmRate ?? 1.3) / sampleRate; if (pwmPhase >= 1) pwmPhase -= 1;
    const width = 0.5 + 0.42 * Math.sin(TWO_PI * pwmPhase);  // 0.08..0.92
    const pulse = (pulPhase < width ? 1 : -1) * (P.pulseGain ?? 0.55);

    let voice = saw + pulse;

    // ── resonant low-pass (Chamberlin SVF) — the honk ──
    const fc = Math.min(Math.max(80, baseF * (P.cutoff ?? 7)), sampleRate / 6);
    const k = 2 * Math.sin(Math.PI * fc / sampleRate);
    const hi = voice - sLow - damp * sBand;
    sBand += k * hi;
    sLow  += k * sBand;
    let s = sLow + sBand * 0.3;                     // LP + a little band sheen

    // ── grit + clean sub ──
    s = Math.tanh(s * (P.drive ?? 2.0));
    subPhase += f0 * 0.5 / sampleRate; if (subPhase >= 1) subPhase -= 1;
    const sub = Math.sin(TWO_PI * subPhase) * (P.subGain ?? 0.34);

    out[i] = (s * 0.7 + sub) * env * gain;
  }
  return out;
}

// ── node-side buffer mixer ─────────────────────────────────────────────
export function mixEventHoover(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderHoover(ev, { ...opts, sampleRate });
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
  const bpm = Number(flags.bpm ?? DEFAULT_BPM);
  const outPath = expandHome(flags.out) || resolve(HERE, `hoover-${preset}.mp3`);

  // A bright major rave riff so the whoop + honk are obvious.
  const beat = 60 / bpm;
  const sx = beat / 4; // 16th
  // A major pentatonic-ish hoover hook (the "hippy" euphoric side).
  const seq = [
    [69, 2], [76, 2], [74, 2], [69, 2],
    [72, 2], [76, 4], [81, 2],
    [79, 2], [76, 2], [74, 4], [69, 2],
  ];
  let t = 0;
  const events = [];
  for (const [midi, sixteenths] of seq) {
    events.push({ startSec: t, midi, durSec: sx * sixteenths * 0.95, gain: 0.95, preset });
    t += sx * sixteenths;
  }
  const total = t + 0.5;
  const out = new Float32Array(Math.ceil(total * SR));
  console.log(`→ hoover demo · preset=${preset} · bpm=${bpm} · ${seq.length} notes`);
  for (const ev of events) mixEventHoover(ev, out, { sampleRate: SR, bpm, preset });

  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const n = 0.85 / peak; for (let i = 0; i < out.length; i++) out[i] *= n; }

  mkdirSync(dirname(outPath), { recursive: true });
  const rawPath = `${outPath}.f32.raw`;
  const buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
  writeFileSync(rawPath, buf);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "3", outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
}
