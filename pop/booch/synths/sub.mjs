#!/usr/bin/env node
// sub.mjs — fat P-funk sub bass for the booch lane.
// Pure-float per-sample DSP (C-portable). Single sine carrier + a touch
// of softclip/tape saturation that adds 2nd/3rd harmonic warmth without
// turning the sine into a saw. Sidechain ducking is handled by the
// renderer (per-sample duck buffer), not here.
//
// Library:
//   import { mixEventSub, renderSub, SUB_PRESETS } from "...";
//   mixEventSub(ev, out, opts)        // node bed render path
//   renderSub(ev, opts) -> Float32    // bare DSP engine
//
// CLI demo:
//   node pop/booch/synths/sub.mjs                  # funk preset
//   node pop/booch/synths/sub.mjs --preset wide --bpm 94

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "funk";
const DEFAULT_BPM = 94;

// ── presets ────────────────────────────────────────────────────────────
// drive       : softclip drive (1.0 = clean sine, 2.5 = warm warmth)
// h2Gain      : additive 2nd harmonic (octave up) — adds body presence
// h3Gain      : additive 3rd harmonic — adds clarity
// pitchEnv    : [startSemi, settleSec] — short pitch drop at attack
//               (the P-funk "thump" — makes the kick & sub bleed together)
// attack      : sec
// decay       : sec
export const SUB_PRESETS = {
  // Funk — warm, with a tiny pitch thump and 2nd harm presence so it
  // pokes through the mix even on small speakers.
  funk: {
    drive: 2.2, h2Gain: 0.18, h3Gain: 0.06,
    pitchEnv: [4, 0.04],
    attack: 0.004, decay: 0.30,
  },
  // Clean — pure sine, no harmonic boost. Sits under everything.
  clean: {
    drive: 1.0, h2Gain: 0.0, h3Gain: 0.0,
    pitchEnv: [0, 0.0],
    attack: 0.006, decay: 0.30,
  },
  // Wide — more drive + more 2nd harmonic for a meaty Bootsy-ish bottom.
  wide: {
    drive: 2.8, h2Gain: 0.28, h3Gain: 0.10,
    pitchEnv: [5, 0.05],
    attack: 0.003, decay: 0.34,
  },
};

// ── helpers ────────────────────────────────────────────────────────────
function midiToFreq(midi) { return 440 * Math.pow(2, (midi - 69) / 12); }

// ── the DSP engine ─────────────────────────────────────────────────────
export function renderSub(ev, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const presetName = ev.preset || opts.preset || DEFAULT_PRESET;
  const P = { ...(SUB_PRESETS[presetName] || SUB_PRESETS[DEFAULT_PRESET]), ...opts.params };

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
  const out = new Float32Array(ns);

  const baseF = midiToFreq(ev.midi);
  const [pSemi, pSec] = P.pitchEnv ?? [0, 0];
  const pSampS = Math.max(1, Math.floor((pSec || 1) * sampleRate));
  let ph1 = 0, ph2 = 0, ph3 = 0;
  const TWO_PI = 2 * Math.PI;

  for (let i = 0; i < ns; i++) {
    let env;
    if (i < attS) env = i / attS;
    else if (i < decayStart) env = 1;
    else { env = 1 - (i - decayStart) / decS; if (env <= 0) break; }

    // pitch thump — drops from +pSemi → 0 over pSec at attack
    let bend = 0;
    if (pSec > 0 && i < pSampS) {
      const w = i / pSampS;
      bend = pSemi * (1 - w);
    }
    const f = baseF * Math.pow(2, bend / 12);

    ph1 += f / sampleRate;     if (ph1 >= 1) ph1 -= 1;
    ph2 += (f * 2) / sampleRate; if (ph2 >= 1) ph2 -= 1;
    ph3 += (f * 3) / sampleRate; if (ph3 >= 1) ph3 -= 1;

    const fund = Math.sin(TWO_PI * ph1);
    const h2 = Math.sin(TWO_PI * ph2) * (P.h2Gain ?? 0.18);
    const h3 = Math.sin(TWO_PI * ph3) * (P.h3Gain ?? 0.06);
    let s = fund + h2 + h3;

    // tape-style softclip (smooth, asymmetric-free)
    const drive = P.drive ?? 2.2;
    if (drive > 1) s = Math.tanh(s * drive) / Math.tanh(drive);

    out[i] = s * env * gain;
  }
  return out;
}

// ── node-side buffer mixer ─────────────────────────────────────────────
export function mixEventSub(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderSub(ev, { ...opts, sampleRate });
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
  const outPath = expandHome(flags.out) || resolve(HERE, `sub-${preset}.mp3`);

  // A simple funky walk in D — D F G D / G A C D
  const beat = 60 / bpm;
  const seq = [
    [38, 1], [41, 1], [43, 1], [38, 1],
    [43, 1], [45, 1], [48, 1], [38, 1],
  ];
  const events = [];
  let t = 0;
  for (const [midi, beats] of seq) {
    events.push({ startSec: t, midi, durSec: beat * beats * 0.92, gain: 0.9, preset });
    t += beat * beats;
  }
  const total = t + 0.6;
  const out = new Float32Array(Math.ceil(total * SR));
  console.log(`→ sub demo · preset=${preset} · bpm=${bpm} · ${seq.length} notes`);
  for (const ev of events) mixEventSub(ev, out, { sampleRate: SR, preset });

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
