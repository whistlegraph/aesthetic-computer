#!/usr/bin/env node
// detect-whistlepops.mjs — find whistle events in a meeting WAV.
//
// Pipeline:
//   1. Decode WAV (16-bit PCM, mono-downmix from stereo).
//   2. Sliding STFT (FFT_SIZE = 2048 samples, HOP = 512 samples).
//      Hann window.
//   3. Per-frame features in the whistle band (800–4000 Hz):
//        peakFreq, peakMag, peakStrength (peak / mean spectrum),
//        harmonicEnergy (mag at 2f+3f+4f relative to f),
//        bandConcentration (mag in ±50 Hz around peak / total band).
//   4. Threshold each frame; group contiguous whistle-like frames into
//      events (min 80 ms, bridge <60 ms gaps).
//   5. Classify each event by duration into short/medium/long. When the
//      wave-wizard/samples/whistlepops/takes/ corpus exists, additionally
//      run k-NN over feature centroids to refine the label (handles
//      rising/falling two-tone variants).
//   6. Emit JSON: [{ t_start, t_end, kind, confidence, meanFreq }].
//
// No external deps — radix-2 FFT in this file. Built for offline meeting
// audio (≤60 min); processes in one pass without streaming.

import { readFileSync, writeFileSync, existsSync, readdirSync, statSync } from "node:fs";
import { resolve, dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";

// ─── tuning ──────────────────────────────────────────────────────────

const FFT_SIZE = 2048;
const HOP = 512;
const WHISTLE_LO_HZ = 800;
const WHISTLE_HI_HZ = 4000;
const PEAK_CONCENTRATION_HZ = 50;     // ±50 Hz around peak for concentration

// Frame-classification thresholds. Tuned conservatively — better to miss
// a soft whistle than to flag a vowel as one. Re-tune from corpus.
const T_STRENGTH = 5.0;      // peakMag / meanMag of whistle band
const T_HARMONIC = 0.6;      // (mag@2f + mag@3f + mag@4f) / mag@f
const T_CONCENTRATION = 0.35; // mag concentrated within ±50 Hz of peak

// Event-grouping.
const MIN_EVENT_MS = 80;
const MAX_GAP_MS = 60;

// ─── WAV decoder ─────────────────────────────────────────────────────

function decodeWAV(path) {
  const buf = readFileSync(path);
  if (buf.toString("ascii", 0, 4) !== "RIFF" ||
      buf.toString("ascii", 8, 12) !== "WAVE") {
    throw new Error(`not a WAV: ${path}`);
  }
  // Walk chunks to find 'fmt ' and 'data' (chunk order isn't fixed).
  let off = 12;
  let fmt = null;
  let dataOff = 0, dataLen = 0;
  while (off + 8 <= buf.length) {
    const id = buf.toString("ascii", off, off + 4);
    const size = buf.readUInt32LE(off + 4);
    if (id === "fmt ") {
      fmt = {
        format: buf.readUInt16LE(off + 8),
        channels: buf.readUInt16LE(off + 10),
        sampleRate: buf.readUInt32LE(off + 12),
        bitsPerSample: buf.readUInt16LE(off + 22),
      };
    } else if (id === "data") {
      dataOff = off + 8;
      dataLen = size;
    }
    off += 8 + size + (size & 1); // RIFF chunks are even-padded
  }
  if (!fmt || !dataOff) throw new Error("WAV missing fmt or data chunk");
  if (fmt.format !== 1) throw new Error(`unsupported WAV format ${fmt.format} (only PCM)`);
  if (fmt.bitsPerSample !== 16) throw new Error(`unsupported bits ${fmt.bitsPerSample} (only 16-bit)`);

  const samplesPerChannel = dataLen / 2 / fmt.channels;
  const out = new Float32Array(samplesPerChannel);
  if (fmt.channels === 1) {
    for (let i = 0; i < samplesPerChannel; i++) {
      out[i] = buf.readInt16LE(dataOff + i * 2) / 32768;
    }
  } else {
    // Downmix all channels to mono by averaging.
    for (let i = 0; i < samplesPerChannel; i++) {
      let sum = 0;
      for (let c = 0; c < fmt.channels; c++) {
        sum += buf.readInt16LE(dataOff + (i * fmt.channels + c) * 2) / 32768;
      }
      out[i] = sum / fmt.channels;
    }
  }
  return { samples: out, sampleRate: fmt.sampleRate, channels: fmt.channels };
}

// ─── radix-2 iterative FFT (in-place) ────────────────────────────────
// Standard Cooley-Tukey. Operates on parallel real + imag arrays so we
// don't allocate per call. N must be a power of two.

function fftInPlace(real, imag) {
  const N = real.length;
  // Bit reversal
  for (let i = 1, j = 0; i < N; i++) {
    let bit = N >> 1;
    for (; j & bit; bit >>= 1) j ^= bit;
    j ^= bit;
    if (i < j) {
      [real[i], real[j]] = [real[j], real[i]];
      [imag[i], imag[j]] = [imag[j], imag[i]];
    }
  }
  for (let size = 2; size <= N; size *= 2) {
    const half = size / 2;
    const tableStep = (-2 * Math.PI) / size;
    for (let i = 0; i < N; i += size) {
      for (let j = 0; j < half; j++) {
        const angle = tableStep * j;
        const wr = Math.cos(angle);
        const wi = Math.sin(angle);
        const k = i + j + half;
        const tr = wr * real[k] - wi * imag[k];
        const ti = wr * imag[k] + wi * real[k];
        real[k] = real[i + j] - tr;
        imag[k] = imag[i + j] - ti;
        real[i + j] += tr;
        imag[i + j] += ti;
      }
    }
  }
}

// Pre-compute a Hann window of length FFT_SIZE.
function hannWindow(N) {
  const w = new Float32Array(N);
  for (let i = 0; i < N; i++) w[i] = 0.5 * (1 - Math.cos((2 * Math.PI * i) / (N - 1)));
  return w;
}

// ─── feature extraction ──────────────────────────────────────────────

function extractFeatures(samples, sampleRate) {
  const win = hannWindow(FFT_SIZE);
  const real = new Float32Array(FFT_SIZE);
  const imag = new Float32Array(FFT_SIZE);
  const mags = new Float32Array(FFT_SIZE / 2);

  const binHz = sampleRate / FFT_SIZE;
  const loBin = Math.max(1, Math.floor(WHISTLE_LO_HZ / binHz));
  const hiBin = Math.min(FFT_SIZE / 2 - 1, Math.ceil(WHISTLE_HI_HZ / binHz));
  const concentrationBins = Math.max(1, Math.round(PEAK_CONCENTRATION_HZ / binHz));

  const numFrames = Math.max(0, Math.floor((samples.length - FFT_SIZE) / HOP));
  const features = new Array(numFrames);

  for (let f = 0; f < numFrames; f++) {
    const start = f * HOP;
    for (let i = 0; i < FFT_SIZE; i++) {
      real[i] = samples[start + i] * win[i];
      imag[i] = 0;
    }
    fftInPlace(real, imag);
    for (let i = 0; i < mags.length; i++) {
      mags[i] = Math.hypot(real[i], imag[i]);
    }

    // Whistle band: find peak + mean.
    let peakBin = loBin, peakMag = mags[loBin], bandSum = 0, bandCount = 0;
    for (let i = loBin; i <= hiBin; i++) {
      if (mags[i] > peakMag) { peakMag = mags[i]; peakBin = i; }
      bandSum += mags[i];
      bandCount++;
    }
    const meanMag = bandSum / Math.max(1, bandCount);
    const peakStrength = peakMag / Math.max(1e-9, meanMag);

    // Concentration: sum within ±50 Hz of peak / total band sum.
    let concSum = 0;
    const c0 = Math.max(loBin, peakBin - concentrationBins);
    const c1 = Math.min(hiBin, peakBin + concentrationBins);
    for (let i = c0; i <= c1; i++) concSum += mags[i];
    const concentration = concSum / Math.max(1e-9, bandSum);

    // Harmonic energy at 2f, 3f, 4f relative to f.
    const f2 = peakBin * 2, f3 = peakBin * 3, f4 = peakBin * 4;
    const h = ((mags[f2] || 0) + (mags[f3] || 0) + (mags[f4] || 0))
            / Math.max(1e-9, peakMag);

    features[f] = {
      t: (start + FFT_SIZE / 2) / sampleRate, // frame center
      peakFreq: peakBin * binHz,
      peakMag,
      peakStrength,
      harmonic: h,
      concentration,
    };
  }
  return features;
}

// ─── thresholding + grouping ─────────────────────────────────────────

function frameIsWhistle(f) {
  return f.peakStrength > T_STRENGTH
      && f.harmonic < T_HARMONIC
      && f.concentration > T_CONCENTRATION;
}

function groupEvents(features) {
  const events = [];
  let cur = null;
  const maxGap = MAX_GAP_MS / 1000;
  for (const f of features) {
    if (frameIsWhistle(f)) {
      if (!cur) {
        cur = { t_start: f.t, t_end: f.t, frames: [f] };
      } else if (f.t - cur.t_end <= maxGap) {
        cur.t_end = f.t;
        cur.frames.push(f);
      } else {
        events.push(cur);
        cur = { t_start: f.t, t_end: f.t, frames: [f] };
      }
    }
  }
  if (cur) events.push(cur);
  // Discard too-short events (probably transient noise).
  return events.filter((e) => (e.t_end - e.t_start) * 1000 >= MIN_EVENT_MS);
}

// ─── classification ──────────────────────────────────────────────────

// Heuristic: duration alone, no corpus. Sufficient for v1 lone-whistle
// punctuation grammar. Rising/falling distinction requires corpus.
function heuristicKind(event) {
  const ms = (event.t_end - event.t_start) * 1000;
  if (ms < 500) return "short";
  if (ms < 1500) return "medium";
  return "long";
}

// Feature vector for k-NN. Median peakFreq + freq slope + duration +
// mean concentration + mean peakStrength. Median is robust against
// noisy edge frames.
function eventVector(event) {
  const fs = event.frames.map((f) => f.peakFreq).sort((a, b) => a - b);
  const median = fs[Math.floor(fs.length / 2)];
  const slope = (event.frames[event.frames.length - 1].peakFreq
               - event.frames[0].peakFreq) / Math.max(0.01, event.t_end - event.t_start);
  const dur = event.t_end - event.t_start;
  const meanConc = event.frames.reduce((a, f) => a + f.concentration, 0) / event.frames.length;
  const meanStr = event.frames.reduce((a, f) => a + f.peakStrength, 0) / event.frames.length;
  return [median, slope, dur, meanConc, meanStr];
}

// Load the wave-wizard corpus if it's been recorded. Returns
// { label: vec[] } map, or {} if no takes are present yet.
function loadCorpus(repoRoot) {
  const takesDir = join(repoRoot, "wave-wizard/samples/whistlepops/takes");
  if (!existsSync(takesDir)) return {};
  const wavs = readdirSync(takesDir).filter((f) => f.endsWith(".wav"));
  if (wavs.length === 0) return {};
  const byLabel = {};
  for (const f of wavs) {
    const m = f.match(/^(pos|neg)-([a-z]+)-/);
    if (!m) continue;
    const label = `${m[1]}-${m[2]}`; // e.g. "pos-short", "neg-hum"
    const path = join(takesDir, f);
    try {
      const wav = decodeWAV(path);
      const feats = extractFeatures(wav.samples, wav.sampleRate);
      const events = groupEvents(feats);
      // Each take should be one event after auto-trim. Skip if zero/multi.
      if (events.length !== 1) continue;
      const v = eventVector(events[0]);
      (byLabel[label] ??= []).push(v);
    } catch (e) {
      console.error(`[detect] corpus load failed for ${f}: ${e.message}`);
    }
  }
  return byLabel;
}

// k-NN with k=3, Euclidean. Returns { label, confidence } where confidence
// is (votes for winner) / k. Throws empty corpus → fall back to heuristic.
function classifyKNN(corpus, vec) {
  const flat = [];
  for (const [label, vecs] of Object.entries(corpus)) {
    for (const v of vecs) flat.push({ label, v });
  }
  if (flat.length === 0) return null;
  for (const entry of flat) {
    let d = 0;
    for (let i = 0; i < vec.length; i++) {
      const x = (entry.v[i] - vec[i]);
      d += x * x;
    }
    entry.d = Math.sqrt(d);
  }
  flat.sort((a, b) => a.d - b.d);
  const k = Math.min(3, flat.length);
  const top = flat.slice(0, k);
  const counts = {};
  for (const e of top) counts[e.label] = (counts[e.label] || 0) + 1;
  let winner = top[0].label, winCount = 0;
  for (const [l, c] of Object.entries(counts)) {
    if (c > winCount) { winner = l; winCount = c; }
  }
  return { label: winner, confidence: winCount / k };
}

// ─── entry ───────────────────────────────────────────────────────────

const args = process.argv.slice(2);
const wavPath = args[0];
let outPath = null;
for (let i = 1; i < args.length; i++) {
  if (args[i] === "--out") outPath = args[++i];
}
if (!wavPath) {
  console.error("usage: detect-whistlepops.mjs <wav> [--out <path>]");
  process.exit(2);
}
if (!existsSync(wavPath)) {
  console.error(`no such file: ${wavPath}`);
  process.exit(2);
}

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(HERE, "..");

const sizeMB = statSync(wavPath).size / (1024 * 1024);
console.error(`[detect] ${basename(wavPath)} (${sizeMB.toFixed(1)} MB)`);

const wav = decodeWAV(wavPath);
console.error(`[detect] ${wav.sampleRate} Hz · ${wav.channels} ch · ${(wav.samples.length / wav.sampleRate).toFixed(1)} s`);

const features = extractFeatures(wav.samples, wav.sampleRate);
const rawEvents = groupEvents(features);
console.error(`[detect] ${rawEvents.length} candidate events`);

const corpus = loadCorpus(REPO_ROOT);
const corpusLabels = Object.keys(corpus);
if (corpusLabels.length > 0) {
  console.error(`[detect] corpus: ${corpusLabels.length} classes (${corpusLabels.join(", ")})`);
} else {
  console.error("[detect] no corpus — using duration heuristic for labels");
}

const events = rawEvents.map((e) => {
  const vec = eventVector(e);
  let kind, confidence;
  const knn = corpusLabels.length > 0 ? classifyKNN(corpus, vec) : null;
  if (knn && knn.label.startsWith("neg-")) {
    // Negative-class winner — likely a false positive (laugh, vowel,
    // kettle). Drop.
    return null;
  }
  if (knn) {
    // pos-short → "short"; preserve the acoustic variant for the DSL.
    kind = knn.label.replace(/^pos-/, "");
    confidence = knn.confidence;
  } else {
    kind = heuristicKind(e);
    confidence = Math.min(1, e.frames[0].peakStrength / 20);
  }
  const meanFreq = vec[0];
  return {
    t_start: Number(e.t_start.toFixed(3)),
    t_end: Number(e.t_end.toFixed(3)),
    kind,
    confidence: Number(confidence.toFixed(2)),
    meanFreq: Number(meanFreq.toFixed(1)),
  };
}).filter(Boolean);

console.error(`[detect] ${events.length} whistlepop events`);

const payload = {
  source: resolve(wavPath),
  generatedAt: new Date().toISOString(),
  detector: {
    fftSize: FFT_SIZE, hop: HOP,
    band: [WHISTLE_LO_HZ, WHISTLE_HI_HZ],
    thresholds: {
      strength: T_STRENGTH, harmonic: T_HARMONIC, concentration: T_CONCENTRATION,
    },
    corpusLabels,
  },
  events,
};

if (outPath) {
  writeFileSync(outPath, JSON.stringify(payload, null, 2) + "\n");
  console.error(`[detect] wrote ${outPath}`);
} else {
  process.stdout.write(JSON.stringify(payload, null, 2) + "\n");
}
