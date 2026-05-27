#!/usr/bin/env node
// rhodes.mjs — Rhodes-y FM electric piano for the booch lane.
// Pure-float per-sample DSP (C-portable). Classic 2-op FM:
//   carrier (sine at note freq) modulated by a sine modulator
//   (typically ratio 1:1 or 14:1 for tine bell), with a fast-decay
//   modulator envelope giving the "bell tine" attack, and a longer
//   carrier envelope giving the warm body. Slow tremolo LFO on output.
//
// Library:
//   import { mixEventRhodes, renderRhodes, RHODES_PRESETS } from "...";
//   mixEventRhodes(ev, out, opts)        // node bed render path
//   renderRhodes(ev, opts) -> Float32    // bare DSP engine
//
// CLI demo:
//   node pop/booch/synths/rhodes.mjs                  # mellow preset
//   node pop/booch/synths/rhodes.mjs --preset stab --bpm 94

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "mellow";
const DEFAULT_BPM = 94;

// ── presets ────────────────────────────────────────────────────────────
// modRatio   : modulator freq = carrier × modRatio (1.0 = warm, 14 = bell)
// modIndex   : peak modulation depth (FM index, higher = brighter/edgier)
// modDecay   : sec — how fast the bell tine fades into warm sine
// tremRate   : Hz — slow amplitude LFO
// tremDepth  : 0..1 — tremolo wet amount
// detune     : ± cents tiny detune for two-voice unison thickness
// attack     : sec
// decay      : sec — exp tail length
export const RHODES_PRESETS = {
  // Warm classic — modIndex 4, slight bell, slow trem.
  mellow: {
    modRatio: 1.0, modIndex: 4.0, modDecay: 0.18,
    tremRate: 4.8, tremDepth: 0.18,
    detune: 6, attack: 0.005, decay: 1.6,
  },
  // Bright — higher mod index + bell-ratio modulator, more sparkle.
  bright: {
    modRatio: 2.0, modIndex: 6.0, modDecay: 0.10,
    tremRate: 5.6, tremDepth: 0.22,
    detune: 8, attack: 0.003, decay: 1.2,
  },
  // Stab — short percussive Rhodes hit for the boom-bap 2 & 4 chord.
  stab: {
    modRatio: 1.0, modIndex: 5.5, modDecay: 0.08,
    tremRate: 0.0, tremDepth: 0.0,
    detune: 4, attack: 0.002, decay: 0.28,
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
export function renderRhodes(ev, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const presetName = ev.preset || opts.preset || DEFAULT_PRESET;
  const P = { ...(RHODES_PRESETS[presetName] || RHODES_PRESETS[DEFAULT_PRESET]), ...opts.params };

  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) return new Float32Array(0);
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);

  const attack = opts.attack ?? P.attack;
  const decay  = opts.decay  ?? P.decay;
  const durS = Math.ceil(ev.durSec * sampleRate);
  const attS = Math.max(1, Math.floor(attack * sampleRate));
  const decS = Math.max(1, Math.floor(decay * sampleRate));
  const ns   = Math.max(durS, attS + decS);
  const decayStart = ns - decS;
  const modDecS = Math.max(1, Math.floor((P.modDecay ?? 0.18) * sampleRate));
  const out = new Float32Array(ns);

  const baseF = midiToFreq(ev.midi);
  const rng = makeRng(`rhodes:${presetName}:${ev.midi}:${(ev.startSec ?? 0).toFixed(4)}`);

  // Two-voice unison (slight detune) — fattens without chorus.
  const detCents = P.detune ?? 6;
  const v1F = baseF * Math.pow(2, +detCents / 2 / 1200);
  const v2F = baseF * Math.pow(2, -detCents / 2 / 1200);
  let cph1 = rng(), cph2 = rng();
  let mph1 = rng(), mph2 = rng();
  let lfoPh = rng();
  const TWO_PI = 2 * Math.PI;

  const modRatio = P.modRatio ?? 1.0;
  const modIndex = P.modIndex ?? 4.0;
  const tremRate = P.tremRate ?? 4.8;
  const tremDepth = P.tremDepth ?? 0.18;

  for (let i = 0; i < ns; i++) {
    let env;
    if (i < attS) env = i / attS;
    else if (i < decayStart) env = 1;
    else { env = 1 - (i - decayStart) / decS; if (env <= 0) break; }

    // modulator envelope: fast exp decay — the tine bell attack fades
    // into the warm carrier sine.
    const mEnv = Math.exp(-i / modDecS);
    const idx = modIndex * mEnv;

    // voice 1
    mph1 += (v1F * modRatio) / sampleRate; if (mph1 >= 1) mph1 -= 1;
    const m1 = Math.sin(TWO_PI * mph1) * idx;
    cph1 += v1F / sampleRate; if (cph1 >= 1) cph1 -= 1;
    const s1 = Math.sin(TWO_PI * cph1 + m1);

    // voice 2
    mph2 += (v2F * modRatio) / sampleRate; if (mph2 >= 1) mph2 -= 1;
    const m2 = Math.sin(TWO_PI * mph2) * idx;
    cph2 += v2F / sampleRate; if (cph2 >= 1) cph2 -= 1;
    const s2 = Math.sin(TWO_PI * cph2 + m2);

    let s = (s1 + s2) * 0.5;

    // tremolo (slow amplitude LFO)
    if (tremDepth > 0 && tremRate > 0) {
      lfoPh += tremRate / sampleRate; if (lfoPh >= 1) lfoPh -= 1;
      const trem = 1 - tremDepth * (0.5 - 0.5 * Math.cos(TWO_PI * lfoPh));
      s *= trem;
    }

    out[i] = s * env * gain;
  }
  return out;
}

// ── node-side buffer mixer ─────────────────────────────────────────────
export function mixEventRhodes(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderRhodes(ev, { ...opts, sampleRate });
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
  const outPath = expandHome(flags.out) || resolve(HERE, `rhodes-${preset}.mp3`);

  // A short jazzy comp — Dm9 → G13 → Am11 → Dm9, swung quarters.
  const beat = 60 / bpm;
  const CHORDS = [
    [62, 65, 69, 72, 76],   // Dm9: D F A C E
    [55, 59, 62, 65, 69],   // G13: G B D F A (skipping the 13th E for cleanliness)
    [57, 60, 64, 67, 71],   // Am11: A C E G B
    [62, 65, 69, 72, 76],   // Dm9
  ];
  const events = [];
  let t = 0;
  for (const chord of CHORDS) {
    for (const m of chord) {
      events.push({ startSec: t, midi: m, durSec: beat * 3.8, gain: 0.55, preset });
    }
    t += beat * 4;
  }
  const total = t + 1.0;
  const out = new Float32Array(Math.ceil(total * SR));
  console.log(`→ rhodes demo · preset=${preset} · bpm=${bpm} · ${CHORDS.length} chords`);
  for (const ev of events) mixEventRhodes(ev, out, { sampleRate: SR, preset });

  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const n = 0.8 / peak; for (let i = 0; i < out.length; i++) out[i] *= n; }
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
