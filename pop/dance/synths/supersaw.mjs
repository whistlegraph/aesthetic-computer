#!/usr/bin/env node
// supersaw.mjs — JP-8000-style detuned sawtooth stack, native-portable.
//
// Voice is expressed as N parallel `sound.synth({type:"sawtooth", ...})`
// calls — exactly the same contract `percussion.mjs` uses to talk to
// AC's web AudioContext + fedac/native audio.c. The supersaw is just
// "play seven detuned saws at once" — no buffer-side custom DSP. Means
// it ports to native by definition.
//
// Library:
//   import { playSupersaw, mixEventSupersaw, SUPERSAW_PRESETS } from "...";
//
//   // In any AC runtime (web/native/node):
//   playSupersaw(sound, { midi, durSec, gain }, opts)
//
//   // Node-side bed renderer (Float32Array buffer):
//   mixEventSupersaw(ev, out, opts)
//
// CLI demo:
//   node pop/dance/synths/supersaw.mjs                       # lead preset
//   node pop/dance/synths/supersaw.mjs --preset pad
//   node pop/dance/synths/supersaw.mjs --voices 9 --detune 28 --out ~/Desktop/saw.mp3

import {
  writeFileSync,
  mkdirSync,
  unlinkSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { makeBufferSynth } from "./bus.mjs";

// ── presets ────────────────────────────────────────────────────────────
// Envelope is AD only (linear attack → exponential decay) — that's what
// the AC native sound.synth contract supports. "Pad" gets a long decay
// so the tail rings; "lead" gets a short crisp tail; "stab" is plucky.
export const SUPERSAW_PRESETS = {
  lead: { voices: 7, detuneCents: 18, centerGain: 0.65, attack: 0.015, decay: 0.50 },
  pad:  { voices: 7, detuneCents: 28, centerGain: 0.55, attack: 0.40,  decay: 2.00 },
  stab: { voices: 5, detuneCents: 12, centerGain: 0.70, attack: 0.005, decay: 0.18 },
};

const DEFAULT_PRESET = "lead";
const DEFAULT_SAMPLE_RATE = 48_000;

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

// Symmetric detune spread around the center voice.
function buildDetuneTable(voices, detuneCents) {
  const t = new Float64Array(voices);
  const half = (voices - 1) / 2;
  for (let i = 0; i < voices; i++) {
    const offset = (i - half) / Math.max(1, half);
    t[i] = offset * detuneCents;
  }
  return t;
}

// ── runtime-agnostic play (web / native / node) ───────────────────────
// Takes an AC `sound` object and an event, fires N sawtooth voices.
// Same calling convention as `playPercussion(sound, letter, opts)`.
export function playSupersaw(sound, ev, opts = {}) {
  if (!sound?.synth) return;
  const presetName = opts.preset || DEFAULT_PRESET;
  const preset = SUPERSAW_PRESETS[presetName] || SUPERSAW_PRESETS[DEFAULT_PRESET];

  const voices      = opts.voices      ?? preset.voices;
  const detuneCents = opts.detuneCents ?? preset.detuneCents;
  const centerGain  = opts.centerGain  ?? preset.centerGain;
  const attack      = opts.attack      ?? preset.attack;
  const decay       = opts.decay       ?? preset.decay;
  const seed        = opts.seed        ?? `${ev.midi}:${(ev.startSec ?? 0).toFixed(4)}`;
  const eventGain   = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) return;
  if (eventGain === 0) return;

  const detune = buildDetuneTable(voices, detuneCents);
  const fundFreq = midiToFreq(ev.midi);
  const half = (voices - 1) / 2;
  const centerIndex = voices % 2 === 1 ? half : -1;

  // Compute normalization: sum of voice amplitudes so total loudness
  // stays sane regardless of voice count.
  let ampSum = 0;
  const amps = new Array(voices);
  for (let v = 0; v < voices; v++) {
    amps[v] = v === centerIndex ? centerGain : 1.0;
    ampSum += amps[v];
  }
  const norm = 1 / Math.max(0.001, ampSum);

  // Per-voice random phase so unison doesn't briefly cohere on attack.
  // Deterministic via seed → same input always yields same waveform.
  const rng = makeRng(`supersaw:${seed}`);

  for (let v = 0; v < voices; v++) {
    const tone = fundFreq * Math.pow(2, detune[v] / 1200);
    sound.synth({
      type: "sawtooth",
      tone,
      duration: ev.durSec,
      volume: eventGain * amps[v] * norm,
      attack,
      decay,
      phase: rng(),
    });
  }
}

// ── node-side buffer mixer ─────────────────────────────────────────────
// Wraps playSupersaw with a buffer-mock-synth bound to ev.startSec, so
// the bed renderer can call it like any other event mixer.
export function mixEventSupersaw(ev, out, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  if (!(out instanceof Float32Array)) return;
  const sound = makeBufferSynth(out, ev.startSec, sampleRate);
  playSupersaw(sound, ev, opts);
}

// ── CLI demo ──────────────────────────────────────────────────────────
const isMain = process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
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
  const SAMPLE_RATE = 48_000;
  const preset = flags.preset || "lead";
  const voices = flags.voices !== undefined ? Number(flags.voices) : undefined;
  const detuneCents = flags.detune !== undefined ? Number(flags.detune) : undefined;
  const bpm = Number(flags.bpm ?? 138);
  const outPath = expandHome(flags.out) || resolve(HERE, `supersaw-${preset}.mp3`);

  const opts = {
    preset,
    sampleRate: SAMPLE_RATE,
    ...(voices !== undefined ? { voices } : {}),
    ...(detuneCents !== undefined ? { detuneCents } : {}),
  };

  const beatSec = 60 / bpm;
  const noteScale = preset === "pad" ? 1.6 : preset === "stab" ? 0.35 : 0.7;
  const arp = [57, 60, 64, 67, 64, 60, 57, 64];
  const events = [];
  for (let i = 0; i < arp.length; i++) {
    events.push({ startSec: i * beatSec * 0.5, midi: arp[i], gain: 0.9, durSec: beatSec * noteScale });
  }
  const stabStart = arp.length * beatSec * 0.5 + beatSec * 0.25;
  for (const m of [45, 57, 60, 64]) {
    events.push({ startSec: stabStart, midi: m, gain: 0.7, durSec: beatSec * 2.0 });
  }

  const tailSec = preset === "pad" ? 2.5 : 1.0;
  const totalSec = stabStart + beatSec * 2.0 + tailSec;
  const out = new Float32Array(Math.ceil(totalSec * SAMPLE_RATE));

  console.log(`→ supersaw demo · preset=${preset} · bpm=${bpm} · ${events.length} notes`);
  for (const ev of events) mixEventSupersaw(ev, out, opts);

  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const norm = 0.7 / peak; for (let i = 0; i < out.length; i++) out[i] *= norm; }

  mkdirSync(dirname(outPath), { recursive: true });
  const rawPath = `${outPath}.f32.raw`;
  const buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
  writeFileSync(rawPath, buf);

  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
    "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "3",
    outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
}
