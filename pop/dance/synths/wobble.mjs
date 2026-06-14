#!/usr/bin/env node
// wobble.mjs — the "wobble bass": a fat detuned Reese swept by a
// beat-synced RESONANT LOWPASS. The canonical dubstep "wub wub".
//
// Sibling to skrill.mjs, but a different animal. The skrill *talks* — two
// band-pass FORMANTS morph between vowels, so it says "yoi-yow". The
// wobble doesn't talk; it BREATHES. One resonant low-pass cutoff is
// swung open and shut by an LFO locked to the beat grid. Low cutoff =
// dark, muffled "wuh"; high cutoff = bright, buzzy "weew". Sweep that
// between them on an 1/8 note and you get the wobble.
//
// Like the skrill, this cannot be a fan-out of plain `sound.synth()`
// voices — the AC voice contract (see bus.mjs) is oscillator + AD
// envelope only, no swept resonant filter. So the wobble hand-rolls its
// per-sample DSP straight into the Float32 buffer, glues with tanh, and
// lays a clean sine sub underneath for body.
//
// The wub is BEAT-SYNCED via skrill's resolveLfoHz — a note division
// ("1/8", "1/4", "1/8t" triplet, "1/8d" dotted, or a raw Hz number)
// locked to `bpm`, so it rides the grid instead of drifting. That makes
// `wobble` composable as a verb in a .np line:
//
//   D1  wobble:woe:1/4     → slow mournful half-wob
//   E1  wobble:bomp:1/8    → punchy square-gate bounce
//   A1  wobble:row:1/8t    → rolling triplet wobble
//
// Library:
//   import { mixEventWobble, renderWobble, WOBBLE_PRESETS } from "...";
//   mixEventWobble(ev, out, opts)        // node bed renderer (the /pop path)
//   renderWobble(ev, opts) -> Float32    // the bare DSP engine
//   playWobble(sound, ev, opts)          // live-AC seam (custom voice)
//
// CLI demo:
//   node pop/dance/synths/wobble.mjs                      # woe preset
//   node pop/dance/synths/wobble.mjs --preset bomp --lfo 1/8
//   node pop/dance/synths/wobble.mjs --preset row --bpm 140 --out ~/wob.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { resolveLfoHz } from "./skrill.mjs"; // single-sourced grid-sync

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "woe";
const DEFAULT_BPM = 140;

// ── presets ────────────────────────────────────────────────────────────
// voices/detuneCents : the Reese — N saws fanned ± cents apart; the
//                      beating between them is the fat foundation.
// lfo                : default note-division for the wub (overridable per note)
// lfoShape           : "sine"|"tri"|"saw"|"square"|"sh" — the wob contour.
//                      square = hard on/off gate; sine = smooth breathe.
// lfoDepth           : 0..1 — how far toward cutHi the sweep actually opens.
// cutLo/cutHi        : the resonant-lowpass sweep endpoints (Hz). Dark→bright.
// q                  : filter resonance (higher = sharper, more vocal/whistly).
// drive              : tanh saturation amount (the grit).
// crush              : 0 = off, else bit depth (e.g. 6 = harsh).
// subGain            : clean unmodulated sine sub an octave down (body).
// edge               : square blended into the saw source (extra buzz/cut).
export const WOBBLE_PRESETS = {
  // woe — slow, mournful half-wob. Triangle LFO eases the cutoff up and
  // down like a sigh; dark ceiling, fat sub. The sad one.
  woe: {
    voices: 3, detuneCents: 14, lfo: "1/4", lfoShape: "tri", lfoDepth: 1.0,
    cutLo: 90, cutHi: 1200, q: 7, drive: 2.2, crush: 0,
    subGain: 0.50, edge: 0.15, attack: 0.020, decay: 0.26,
  },
  // bomp — punchy square gate. Cutoff snaps open/shut on the 1/8, so it
  // pumps "bomp-bomp-bomp". Brighter ceiling, snappier envelope.
  bomp: {
    voices: 3, detuneCents: 18, lfo: "1/8", lfoShape: "square", lfoDepth: 1.0,
    cutLo: 120, cutHi: 2200, q: 6, drive: 2.8, crush: 0,
    subGain: 0.42, edge: 0.25, attack: 0.006, decay: 0.12,
  },
  // row — rolling triplet wobble. Sine LFO on an 1/8 triplet rolls
  // "row-row-row"; wide detune, high resonance, bright + busy.
  row: {
    voices: 4, detuneCents: 22, lfo: "1/8t", lfoShape: "sine", lfoDepth: 1.0,
    cutLo: 140, cutHi: 2600, q: 8, drive: 3.0, crush: 0,
    subGain: 0.36, edge: 0.30, attack: 0.005, decay: 0.10,
  },
  // reese — slow, fat, almost no wub; the foundation Reese rumble.
  reese: {
    voices: 5, detuneCents: 26, lfo: "1/2", lfoShape: "sine", lfoDepth: 0.4,
    cutLo: 160, cutHi: 900, q: 4, drive: 1.8, crush: 0,
    subGain: 0.55, edge: 0.10, attack: 0.012, decay: 0.24,
  },
};

// ── helpers ────────────────────────────────────────────────────────────
function midiToFreq(midi) {
  return 440 * Math.pow(2, (midi - 69) / 12);
}

// Deterministic xorshift RNG (same pattern as skrill/supersaw) — keeps
// per-voice phase + S&H reproducible per event.
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

// One unipolar LFO sample in [0,1]. `sh` holds a value per cycle for S&H.
function lfoValue(shape, phase, sh) {
  switch (shape) {
    case "tri":    return phase < 0.5 ? phase * 2 : 2 - phase * 2;
    case "saw":    return phase;
    case "square": return phase < 0.5 ? 0 : 1;
    case "sh":     return sh.value;
    case "sine":
    default:       return 0.5 + 0.5 * Math.sin(2 * Math.PI * phase);
  }
}

// Symmetric detune spread (cents) around the center voice — the Reese.
function buildDetune(voices, detuneCents) {
  const t = new Float64Array(voices);
  const half = (voices - 1) / 2;
  for (let i = 0; i < voices; i++) {
    const offset = half === 0 ? 0 : (i - half) / half;
    t[i] = offset * detuneCents;
  }
  return t;
}

// ── the DSP engine ─────────────────────────────────────────────────────
// Renders ONE wobble event into a fresh mono Float32Array. ev: { midi,
// durSec, gain?, preset?, lfo? }. Per-note `preset`/`lfo` override opts so
// a .np verb token maps straight through.
export function renderWobble(ev, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const bpm        = opts.bpm        ?? DEFAULT_BPM;
  const presetName = ev.preset || opts.preset || DEFAULT_PRESET;
  const P = { ...(WOBBLE_PRESETS[presetName] || WOBBLE_PRESETS[DEFAULT_PRESET]), ...opts.params };

  if (!Number.isFinite(ev.midi) || !Number.isFinite(ev.durSec) || ev.durSec <= 0) {
    return new Float32Array(0);
  }
  const gain = Number.isFinite(ev.gain) ? ev.gain : 1.0;
  if (gain === 0) return new Float32Array(0);

  const attack = opts.attack ?? P.attack ?? 0.008;
  const decay  = opts.decay  ?? P.decay  ?? 0.14;
  // Match bus.mjs / native envelope window: auto-extend so the linear
  // decay completes to true silence without an end click.
  const durS = Math.ceil(ev.durSec * sampleRate);
  const attS = Math.max(1, Math.floor(attack * sampleRate));
  const decS = Math.max(1, Math.floor(decay * sampleRate));
  const ns   = Math.max(durS, attS + decS);
  const decayStart = ns - decS;
  const out = new Float32Array(ns);

  const fund     = midiToFreq(ev.midi);
  const lfoHz    = resolveLfoHz(ev.lfo || opts.lfo || P.lfo, bpm);
  const lfoDepth = P.lfoDepth ?? 1.0;
  const voices   = Math.max(1, P.voices ?? 3);
  const detune   = buildDetune(voices, P.detuneCents ?? 16);
  const edge     = P.edge ?? 0.2;

  const rng = makeRng(`wobble:${presetName}:${ev.midi}:${(ev.startSec ?? 0).toFixed(4)}`);

  // Per-voice saw phase (random start so unison doesn't cohere on attack)
  // and per-sample phase increment.
  const phase = new Float64Array(voices);
  const inc   = new Float64Array(voices);
  for (let v = 0; v < voices; v++) {
    phase[v] = rng();
    inc[v] = (fund * Math.pow(2, detune[v] / 1200)) / sampleRate;
  }
  const norm = 1 / voices;

  // State: LFO + one resonant Chamberlin lowpass (the wub filter).
  let lfoPhase = rng();
  const sh = { value: rng() };
  let low = 0, band = 0;
  const damp = 1 / Math.max(0.5, P.q ?? 6);     // 1/Q → resonance
  const cutLo = Math.max(40, P.cutLo ?? 120);
  const cutHi = Math.max(cutLo + 1, P.cutHi ?? 2200);
  const ratio = cutHi / cutLo;                  // exponential sweep span
  const subInc = (fund * 0.5) / sampleRate;     // sub an octave down
  let subPhase = rng();
  const TWO_PI = 2 * Math.PI;

  for (let i = 0; i < ns; i++) {
    // ── envelope (linear attack → hold → linear decay) ──
    let env;
    if (i < attS) env = i / attS;
    else if (i < decayStart) env = 1;
    else { env = 1 - (i - decayStart) / decS; if (env <= 0) break; }

    // ── beat-synced wub LFO ──
    lfoPhase += lfoHz / sampleRate;
    if (lfoPhase >= 1) { lfoPhase -= 1; sh.value = rng(); }
    const lv = lfoValue(P.lfoShape || "sine", lfoPhase, sh) * lfoDepth;

    // ── Reese source: detuned saws (+ optional square edge) ──
    let src = 0;
    for (let v = 0; v < voices; v++) {
      let ph = phase[v] + inc[v];
      if (ph >= 1) ph -= Math.floor(ph);
      phase[v] = ph;
      const saw = 2 * ph - 1;
      const sq  = ph < 0.5 ? 1 : -1;
      src += saw * (1 - edge) + sq * edge;
    }
    src *= norm;

    // ── the wub: resonant lowpass, cutoff swept exponentially by the LFO ──
    let fc = cutLo * Math.pow(ratio, lv);
    fc = Math.min(Math.max(40, fc), sampleRate / 6);
    const f = 2 * Math.sin(Math.PI * fc / sampleRate);
    const high = src - low - damp * band;
    band += f * high;
    low  += f * band;
    let s = low;

    // ── grit: tanh saturation + optional bitcrush ──
    s = Math.tanh(s * (P.drive ?? 2.5));
    if (P.crush && P.crush > 0) {
      const steps = Math.pow(2, P.crush);
      s = Math.round(s * steps) / steps;
    }

    // ── clean sine sub an octave down (unmodulated body) ──
    subPhase += subInc; if (subPhase >= 1) subPhase -= 1;
    const sub = Math.sin(TWO_PI * subPhase) * (P.subGain ?? 0.4);

    let mix = s * 0.8 + sub;
    // Trap NaN/Inf from a runaway resonant filter (cf. gm_synth lesson) —
    // reset state and emit silence for this sample rather than poison the buffer.
    if (!Number.isFinite(mix)) { low = 0; band = 0; mix = 0; }
    out[i] = mix * env * gain;
  }
  return out;
}

// ── node-side buffer mixer (the /pop bed-render path) ──────────────────
export function mixEventWobble(ev, out, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderWobble(ev, { ...opts, sampleRate });
  const startIdx = Math.floor((ev.startSec ?? 0) * sampleRate);
  for (let i = 0; i < seg.length; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    out[dst] += seg[i];
  }
}

// ── live-AC seam ───────────────────────────────────────────────────────
// supersaw/sinepower fan out plain sound.synth() voices; the wobble
// can't (no swept resonant filter in the voice contract). Renders through
// AC's type:"custom" generator seam when the live runtime validates it;
// a documented no-op until then (same posture as playSkrill).
export function playWobble(sound, ev, opts = {}) {
  if (!sound?.synth) return;
  const seg = renderWobble(ev, opts);
  if (!seg.length) return;
  try {
    let pos = 0;
    sound.synth({
      type: "custom",
      tone: midiToFreq(ev.midi),
      duration: seg.length / (opts.sampleRate ?? DEFAULT_SAMPLE_RATE),
      volume: 1, attack: 0, decay: 0,
      generator: () => (pos < seg.length ? seg[pos++] : 0),
    });
  } catch {
    /* live custom-voice path not yet validated — see header */
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
  const SAMPLE_RATE = 48_000;
  const preset = flags.preset || DEFAULT_PRESET;
  const bpm = Number(flags.bpm ?? DEFAULT_BPM);
  const lfo = flags.lfo;
  const outPath = expandHome(flags.out) || resolve(HERE, `wobble-${preset}.mp3`);

  // A simple half-time bassline rooting on the tonic with a couple moves.
  const beatSec = 60 / bpm;
  const root = 38; // D1
  const line = [root, root, root + 5, root + 3, root, root, root - 2, root + 3];
  const events = [];
  for (let i = 0; i < line.length; i++) {
    events.push({
      startSec: i * beatSec, midi: line[i], gain: 0.95,
      durSec: beatSec * 0.95, preset, ...(lfo ? { lfo } : {}),
    });
  }

  const totalSec = line.length * beatSec + 0.5;
  const out = new Float32Array(Math.ceil(totalSec * SAMPLE_RATE));
  console.log(`→ wobble demo · preset=${preset} · bpm=${bpm} · ${events.length} notes`);
  for (const ev of events) mixEventWobble(ev, out, { sampleRate: SAMPLE_RATE, bpm });

  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const g = 0.85 / peak; for (let i = 0; i < out.length; i++) out[i] *= g; }

  mkdirSync(dirname(outPath), { recursive: true });
  const rawPath = `${outPath}.f32.raw`;
  const buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
  writeFileSync(rawPath, buf);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "3", outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
}
