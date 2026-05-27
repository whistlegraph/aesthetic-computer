#!/usr/bin/env node
// gen-word-pitches.mjs — for each WAV that has a sibling .words.txt
// sidecar, estimate the median f0 (MIDI) of each word using AMDF
// autocorrelation on 40 ms frames. Write a .pitches.txt sidecar with
// `word\tfromMs\ttoMs\tmidi\tHz` per line. Used by the C engine to
// pitch-match ElevenLabs words to wizard-take words.
//
// Run after gen-triad-vocals.mjs + gen-wizard-words.mjs:
//   node pop/hellsine/bin/gen-word-pitches.mjs

import { readFileSync, writeFileSync, existsSync, readdirSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SAMPLES = resolve(HERE, "../samples");
const CHOIR_DIR = `${SAMPLES}/say-choir`;

const PAIRS = [
  ...["money", "honey", "bunnies"].map((v) => [
    `${SAMPLES}/jeffrey-vocal-${v}.wav`,
    `${SAMPLES}/jeffrey-vocal-${v}.words.txt`,
    `eleven:${v}`,
  ]),
  ...["money", "honey", "bunnies"].map((v) => [
    `${SAMPLES}/jeffrey-live-archived-102516/jeffrey-live-that-${v}.wav`,
    `${SAMPLES}/jeffrey-live-archived-102516/jeffrey-live-that-${v}.words.txt`,
    `wizard:${v}`,
  ]),
];
// Add every choir sample that has a sibling .words.txt sidecar.
if (existsSync(CHOIR_DIR)) {
  for (const f of readdirSync(CHOIR_DIR).sort()) {
    if (!f.endsWith(".wav")) continue;
    const wav = `${CHOIR_DIR}/${f}`;
    const words = wav.replace(/\.wav$/, ".words.txt");
    if (existsSync(words)) {
      PAIRS.push([wav, words, `choir:${f.replace(/\.wav$/, "")}`]);
    }
  }
}

// Read a WAV (pcm_f32le or pcm_s16le) and return { samples (mono Float32), sr }.
function readWav(path) {
  const buf = readFileSync(path);
  if (buf.toString("ascii", 0, 4) !== "RIFF") throw new Error(`bad RIFF: ${path}`);
  let pos = 12, fmtCode = 0, ch = 1, bits = 16, sr = 0;
  while (pos < buf.length - 8) {
    const id = buf.toString("ascii", pos, pos + 4);
    const sz = buf.readUInt32LE(pos + 4);
    if (id === "fmt ") {
      fmtCode = buf.readUInt16LE(pos + 8);
      ch      = buf.readUInt16LE(pos + 10);
      sr      = buf.readUInt32LE(pos + 12);
      bits    = buf.readUInt16LE(pos + 22);
    } else if (id === "data") {
      const stride = (bits / 8) * ch;
      const N = Math.floor(sz / stride);
      const out = new Float32Array(N);
      for (let i = 0; i < N; i++) {
        const base = pos + 8 + i * stride;
        let s = 0;
        if (fmtCode === 3 && bits === 32) {
          s = buf.readFloatLE(base);
          if (ch === 2) s = (s + buf.readFloatLE(base + 4)) / 2;
        } else if (bits === 16) {
          s = buf.readInt16LE(base) / 32768;
          if (ch === 2) s = (s + buf.readInt16LE(base + 2) / 32768) / 2;
        } else throw new Error(`unsupported wav fmt=${fmtCode} bits=${bits}`);
        out[i] = s;
      }
      return { samples: out, sr };
    }
    pos += 8 + sz;
  }
  throw new Error(`no data chunk: ${path}`);
}

// AMDF (Average Magnitude Difference Function) pitch detector. Returns
// f0 in Hz, or 0 for unvoiced/low-confidence frames.
function amdfPitchHz(frame, sr, fmin = 70, fmax = 350) {
  const minLag = Math.max(2, Math.floor(sr / fmax));
  const maxLag = Math.min(frame.length - 1, Math.floor(sr / fmin));
  if (maxLag <= minLag) return 0;
  let bestLag = -1, bestScore = Infinity;
  for (let lag = minLag; lag <= maxLag; lag++) {
    let sum = 0;
    const n = frame.length - lag;
    for (let i = 0; i < n; i++) sum += Math.abs(frame[i] - frame[i + lag]);
    sum /= n;
    if (sum < bestScore) { bestScore = sum; bestLag = lag; }
  }
  if (bestLag < 0) return 0;
  // Parabolic refinement around bestLag for sub-sample precision
  let alpha = 0, beta = 0, gamma = 0;
  const amdfAt = (lag) => {
    let s = 0; const n = frame.length - lag;
    for (let i = 0; i < n; i++) s += Math.abs(frame[i] - frame[i + lag]);
    return s / n;
  };
  if (bestLag - 1 >= minLag && bestLag + 1 <= maxLag) {
    alpha = amdfAt(bestLag - 1);
    beta  = bestScore;
    gamma = amdfAt(bestLag + 1);
    const denom = (alpha - 2 * beta + gamma);
    if (Math.abs(denom) > 1e-9) {
      const delta = 0.5 * (alpha - gamma) / denom;
      const refined = bestLag + delta;
      return sr / refined;
    }
  }
  return sr / bestLag;
}

// Median-of-frames f0 estimate over [startS, endS]. Skips frames with
// very low RMS (silence/breath).
function medianPitchMidi(samples, sr, startS, endS) {
  const FRAME = Math.floor(0.040 * sr);    // 40 ms
  const HOP   = Math.floor(0.010 * sr);    // 10 ms hop
  const startI = Math.max(0, Math.floor(startS * sr));
  const endI   = Math.min(samples.length, Math.floor(endS   * sr));
  if (endI - startI < FRAME) return 0;
  const pitches = [];
  for (let i = startI; i + FRAME < endI; i += HOP) {
    const frame = samples.subarray(i, i + FRAME);
    // RMS gate
    let rms = 0;
    for (let k = 0; k < FRAME; k++) rms += frame[k] * frame[k];
    rms = Math.sqrt(rms / FRAME);
    if (rms < 0.005) continue;
    const f0 = amdfPitchHz(frame, sr);
    if (f0 > 50 && f0 < 500) pitches.push(f0);
  }
  if (pitches.length === 0) return 0;
  pitches.sort((a, b) => a - b);
  const mid = pitches[Math.floor(pitches.length / 2)];
  return 69 + 12 * Math.log2(mid / 440);
}

console.log(`[gen-pitches] estimating per-word f0 (AMDF, 40ms frames)`);
console.log("─".repeat(70));

// Track median f0 per label so we can compute one absolute TARGET that
// all layers should harmonize to (instead of relative per-layer shifts
// that compound base-pitch differences).
const layerMedians = new Map();

for (const [wav, wordsTxt, label] of PAIRS) {
  if (!existsSync(wav))      { console.error(`[gen-pitches] missing wav: ${wav}`); continue; }
  if (!existsSync(wordsTxt)) { console.error(`[gen-pitches] missing words sidecar: ${wordsTxt}`); continue; }
  const { samples, sr } = readWav(wav);
  const words = readFileSync(wordsTxt, "utf8").trim().split(/\n+/).map((ln) => {
    const m = ln.split(/\s+/);
    return { word: m[0], fromS: +m[1] / 1000, toS: +m[2] / 1000 };
  });
  const lines = [];
  console.log(`\n${label}  · ${basename(wav)} · sr=${sr}Hz dur=${(samples.length/sr).toFixed(2)}s`);
  for (const w of words) {
    const midi = medianPitchMidi(samples, sr, w.fromS, w.toS);
    const hz   = midi > 0 ? 440 * Math.pow(2, (midi - 69) / 12) : 0;
    const note = midi > 0 ? midiName(midi) : "—";
    lines.push(`${w.word}\t${Math.round(w.fromS * 1000)}\t${Math.round(w.toS * 1000)}\t${midi.toFixed(2)}\t${hz.toFixed(1)}`);
    console.log(`  ${w.word.padEnd(8)} ${(w.fromS).toFixed(2)}→${(w.toS).toFixed(2)}s   midi=${midi.toFixed(1).padStart(5)}  ${hz.toFixed(0).padStart(4)}Hz  (${note})`);
  }
  const out = wav.replace(/\.wav$/, ".pitches.txt");
  writeFileSync(out, lines.join("\n") + "\n");
  console.log(`  → ${basename(out)}`);

  // Median of voiced words (40-65 MIDI = E2-F4 vocal range) for this layer.
  const voiced = words
    .map((w) => medianPitchMidi(samples, sr, w.fromS, w.toS))
    .filter((m) => m > 40 && m < 65);
  if (voiced.length > 0) {
    voiced.sort((a, b) => a - b);
    const med = voiced[Math.floor(voiced.length / 2)];
    layerMedians.set(label, med);
  }
}

// Print the absolute pitch-target summary so the C engine has a single
// reference frequency every layer should harmonize toward.
console.log("\n" + "═".repeat(70));
console.log("ABSOLUTE PITCH BASELINES — per-layer median f0 (MIDI)");
console.log("═".repeat(70));
const all = [...layerMedians.values()].sort((a, b) => a - b);
const targetMidi = all[Math.floor(all.length / 2)];
const targetHz = 440 * Math.pow(2, (targetMidi - 69) / 12);
console.log(`TARGET (cross-layer median): ${targetMidi.toFixed(2)} MIDI  ${targetHz.toFixed(1)}Hz  (${midiName(targetMidi)})`);
console.log("─".repeat(70));
const targets = [];
for (const [label, med] of layerMedians) {
  const shift = targetMidi - med;
  const hz = 440 * Math.pow(2, (med - 69) / 12);
  targets.push(`${label}\t${med.toFixed(2)}\t${hz.toFixed(1)}\t${shift.toFixed(2)}`);
  console.log(`  ${label.padEnd(35)} base=${med.toFixed(1).padStart(5)}  ${hz.toFixed(0).padStart(4)}Hz   shift→target ${shift >= 0 ? "+" : ""}${shift.toFixed(1)} st`);
}
writeFileSync(`${SAMPLES}/layer-pitches.txt`,
  `# layer\tmedianMidi\tmedianHz\tshiftToTargetSt\n# TARGET ${targetMidi.toFixed(2)} MIDI (${midiName(targetMidi)}) ${targetHz.toFixed(1)} Hz\n` +
  targets.join("\n") + "\n");
console.log(`\n[gen-pitches] wrote layer-pitches.txt with absolute targets`);

// eslint-disable-next-line no-unused-vars
function midiName(midi) {
  const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
  const m = Math.round(midi);
  const oct = Math.floor(m / 12) - 1;
  return `${NAMES[((m % 12) + 12) % 12]}${oct}`;
}

console.log("\n[gen-pitches] done");
