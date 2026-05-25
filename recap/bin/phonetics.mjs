#!/usr/bin/env node
// phonetics.mjs — map the actual phonetic events in jeffrey's spoken
// recap.mp3, audio-side (not whisper-side, because whisper writes 0ms
// gaps between every token — its boundaries are not real). Output is
// `recap/out/phonetics.json` with classified events:
//
//   { sr, durationSec, frameMs, events: [
//       { type: "silence"|"voice"|"plosive", startSec, endSec,
//         rmsDb, centroidHz, ... }, ... ] }
//
// Today the classifier is simple (RMS-only silence/voice split + a
// crude plosive flag from spectral-flux spikes). That's enough for
// rap.mjs to know where the REAL silence gaps live so it can stretch
// them rhythmically without ever splicing inside a word.
//
// Future iterations will sharpen this into:
//   - vowel-onset timing (formant tracker, to drive beat-anchoring)
//   - plosive timing (to drive stutter doubling)
//   - syllable-nucleus timing (to drive vowel elongation by sustain
//     looping)
//
// Usage:
//   node bin/phonetics.mjs                    # reads recap/out/recap.mp3
//   node bin/phonetics.mjs --threshold -38    # silence threshold in dBFS
//   node bin/phonetics.mjs --min-silence 60   # ms — min run length to count as silence

import { writeFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  }
}

const voicePath = `${ROOT}/out/recap.mp3`;
const outPath = `${ROOT}/out/phonetics.json`;
if (!existsSync(voicePath)) { console.error(`✗ missing ${voicePath}`); process.exit(1); }

const ff = "/opt/homebrew/opt/ffmpeg-full/bin/ffmpeg";
const SR = 16000;                                      // analysis sample rate
const FRAME_MS = 10;
const HOP = Math.floor((FRAME_MS / 1000) * SR);
const SILENCE_DB = flags.threshold ? parseFloat(flags.threshold) : -38;
const MIN_SILENCE_MS = flags["min-silence"] !== undefined ? parseFloat(flags["min-silence"]) : 60;

// ── decode ────────────────────────────────────────────────────────────
const decode = spawnSync(ff, [
  "-hide_banner", "-loglevel", "error",
  "-i", voicePath,
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-",
], { encoding: "buffer", maxBuffer: 1024 * 1024 * 128 });
if (decode.status !== 0 || !decode.stdout) {
  console.error(`✗ decode failed: ${decode.stderr?.toString().slice(0, 400)}`);
  process.exit(1);
}
const audio = new Float32Array(decode.stdout.buffer, decode.stdout.byteOffset, decode.stdout.byteLength / 4);
const durationSec = audio.length / SR;

// ── frame features (RMS + spectral centroid + ZCR) ────────────────────
const nFrames = Math.floor(audio.length / HOP);
const rmsDb = new Float32Array(nFrames);
const centroid = new Float32Array(nFrames);    // crude high/low band ratio
const flux = new Float32Array(nFrames);        // spectral flux for plosives
const zcr = new Float32Array(nFrames);         // zero-crossing rate for voicing
let prevHigh = 0, prevLow = 0;
for (let f = 0; f < nFrames; f++) {
  const i0 = f * HOP;
  const i1 = Math.min(audio.length, i0 + HOP);
  let sum = 0, hi = 0, lo = 0, prev = 0, crossings = 0;
  let prevSign = i0 > 0 ? Math.sign(audio[i0 - 1]) : 0;
  for (let i = i0; i < i1; i++) {
    const s = audio[i];
    sum += s * s;
    const hp = s - 0.92 * prev;
    hi += Math.abs(hp);
    lo += Math.abs(s) - Math.abs(hp);
    prev = s;
    const sign = s >= 0 ? 1 : -1;
    if (sign !== prevSign && prevSign !== 0) crossings++;
    prevSign = sign;
  }
  const n = Math.max(1, i1 - i0);
  const rms = Math.sqrt(sum / n);
  rmsDb[f] = rms > 1e-7 ? 20 * Math.log10(rms) : -120;
  const total = hi + lo;
  centroid[f] = total > 0 ? hi / total : 0;
  flux[f] = Math.max(0, hi - prevHigh) + Math.max(0, lo - prevLow);
  zcr[f] = crossings / n;
  prevHigh = hi; prevLow = lo;
}

// ── silence regions ───────────────────────────────────────────────────
// A frame is "silent" when its RMS is below SILENCE_DB. Group runs of
// silent frames; keep runs longer than MIN_SILENCE_MS.
const minSilenceFrames = Math.ceil(MIN_SILENCE_MS / FRAME_MS);
const silences = [];
let runStart = -1;
for (let f = 0; f < nFrames; f++) {
  const silent = rmsDb[f] < SILENCE_DB;
  if (silent) {
    if (runStart < 0) runStart = f;
  } else if (runStart >= 0) {
    if (f - runStart >= minSilenceFrames) {
      silences.push({ startSec: runStart * FRAME_MS / 1000, endSec: f * FRAME_MS / 1000 });
    }
    runStart = -1;
  }
}
if (runStart >= 0 && nFrames - runStart >= minSilenceFrames) {
  silences.push({ startSec: runStart * FRAME_MS / 1000, endSec: nFrames * FRAME_MS / 1000 });
}

// ── voice regions = the inverse of silences ───────────────────────────
const voices = [];
let cursor = 0;
for (const s of silences) {
  if (s.startSec > cursor + 0.001) {
    voices.push({ startSec: cursor, endSec: s.startSec });
  }
  cursor = s.endSec;
}
if (cursor < durationSec - 0.001) {
  voices.push({ startSec: cursor, endSec: durationSec });
}

// ── plosive flags (spectral-flux peaks inside voice regions) ──────────
const sortedFlux = Array.from(flux).slice().sort((a, b) => a - b);
const medianFlux = sortedFlux[Math.floor(sortedFlux.length / 2)] || 0;
const fluxThresh = Math.max(medianFlux * 2.0, sortedFlux[Math.floor(sortedFlux.length * 0.85)] || 0);
const plosives = [];
for (const v of voices) {
  const f0 = Math.floor(v.startSec / (FRAME_MS / 1000));
  const f1 = Math.min(nFrames, Math.ceil(v.endSec / (FRAME_MS / 1000)));
  for (let f = f0 + 1; f < f1 - 1; f++) {
    if (flux[f] > fluxThresh && flux[f] > flux[f - 1] && flux[f] >= flux[f + 1]) {
      if (centroid[f] > 0.35) {
        plosives.push({ startSec: f * FRAME_MS / 1000, intensity: flux[f] / fluxThresh, centroid: centroid[f] });
        f += 3;
      }
    }
  }
}

// ── vowel sustains (voiced periodic energy: high RMS, low centroid, low ZCR) ──
// These are the candidates for elongation by pitch-period looping in rap.mjs.
const vowels = [];
for (const v of voices) {
  const f0 = Math.floor(v.startSec / (FRAME_MS / 1000));
  const f1 = Math.min(nFrames, Math.ceil(v.endSec / (FRAME_MS / 1000)));
  let runStart = -1;
  for (let f = f0; f < f1; f++) {
    // Vowel frame heuristic: loud-ish, low spectral centroid (formants
    // dominate, not fricative noise), low zero-crossing rate (periodic).
    // ZCR threshold ~0.10 = ~480 crossings/sec at 16kHz frame, well
    // below sibilants (typically > 0.20).
    const isVowel = rmsDb[f] > -25 && centroid[f] < 0.40 && zcr[f] < 0.10;
    if (isVowel) {
      if (runStart < 0) runStart = f;
    } else {
      if (runStart >= 0 && f - runStart >= 4) {        // ≥ 40ms sustain
        vowels.push({ startSec: runStart * FRAME_MS / 1000, endSec: f * FRAME_MS / 1000 });
      }
      runStart = -1;
    }
  }
  if (runStart >= 0 && f1 - runStart >= 4) {
    vowels.push({ startSec: runStart * FRAME_MS / 1000, endSec: f1 * FRAME_MS / 1000 });
  }
}

// ── assemble events list (sorted by start) ────────────────────────────
const events = [];
for (const s of silences) events.push({ type: "silence", startSec: s.startSec, endSec: s.endSec });
for (const v of voices)   events.push({ type: "voice",   startSec: v.startSec, endSec: v.endSec });
for (const p of plosives) events.push({ type: "plosive", startSec: p.startSec, endSec: p.startSec + 0.02, intensity: p.intensity, centroid: p.centroid });
for (const v of vowels)   events.push({ type: "vowel",   startSec: v.startSec, endSec: v.endSec });
events.sort((a, b) => a.startSec - b.startSec);

const out = {
  sr: SR,
  durationSec,
  frameMs: FRAME_MS,
  silenceThresholdDb: SILENCE_DB,
  minSilenceMs: MIN_SILENCE_MS,
  events,
};
writeFileSync(outPath, JSON.stringify(out, null, 2));

console.log(`phonetics · ${voicePath.replace(ROOT + "/", "")}`);
console.log(`  ${durationSec.toFixed(2)}s · ${nFrames} frames @ ${FRAME_MS}ms`);
console.log(`  silences=${silences.length} (>= ${MIN_SILENCE_MS}ms @ < ${SILENCE_DB}dB)`);
console.log(`  voices  =${voices.length}`);
console.log(`  plosives=${plosives.length} (spectral-flux peaks, centroid > 0.35)`);
console.log(`  vowels  =${vowels.length} (RMS > -25dB, centroid < 0.40, ZCR < 0.10, ≥ 40ms sustain)`);
console.log(`✓ ${outPath}`);

// Helpful preview: dump first few silence gaps.
console.log("");
console.log("first silences:");
for (const s of silences.slice(0, 8)) {
  console.log(`  ${s.startSec.toFixed(2)}s → ${s.endSec.toFixed(2)}s  (${Math.round((s.endSec - s.startSec) * 1000)}ms)`);
}
