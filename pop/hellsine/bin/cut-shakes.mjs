#!/usr/bin/env node
// cut-shakes.mjs — slice a recorded rattle/shaker WAV into individual
// shake events by amplitude envelope, sort them by length, write each
// as a declicked mono WAV named `shake-<idx>-<dur>ms.wav`.
//
// Usage: node pop/hellsine/bin/cut-shakes.mjs [src.wav] [outDir]
// Defaults: pop/hellsine/samples/rattle-intro.wav  →  ../samples/shakes/

import { readFileSync, writeFileSync, mkdirSync, readdirSync, unlinkSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SAMP = resolve(HERE, "../samples");
// Sources: explicit paths via argv, else auto-discover rattle-intro*.wav.
// Combined output goes to one shakes/ dir sorted by length across all takes.
let SRCS = process.argv.slice(2).filter((a) => a.endsWith(".wav"));
if (!SRCS.length) {
  SRCS = readdirSync(SAMP)
    .filter((f) => /^rattle-intro.*\.wav$/.test(f))
    .map((f) => resolve(SAMP, f));
}
if (!SRCS.length) { console.error("no sources — pass paths or drop rattle-intro*.wav into samples/"); process.exit(1); }
const OUT_DIR = resolve(SAMP, "shakes");

// ── WAV reader (mono Float32) ────────────────────────────────────────
function loadWavMono(path) {
  const buf = readFileSync(path);
  let p = 12, fmt = null, dOff = 0, dLen = 0;
  while (p + 8 <= buf.length) {
    const id = buf.toString("ascii", p, p + 4);
    const sz = buf.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = {
      format: buf.readUInt16LE(p + 8), channels: buf.readUInt16LE(p + 10),
      sr: buf.readUInt32LE(p + 12), bits: buf.readUInt16LE(p + 22),
    };
    else if (id === "data") { dOff = p + 8; dLen = sz; }
    p += 8 + sz + (sz & 1);
  }
  if (!fmt || !dOff) throw new Error(`bad WAV: ${path}`);
  const { format, channels, bits } = fmt;
  const fb = (bits / 8) * channels, frames = Math.floor(dLen / fb);
  const mono = new Float32Array(frames);
  for (let i = 0; i < frames; i++) {
    let acc = 0;
    for (let c = 0; c < channels; c++) {
      const o = dOff + i * fb + c * (bits / 8);
      if (format === 3 && bits === 32) acc += buf.readFloatLE(o);
      else if (bits === 16) acc += buf.readInt16LE(o) / 32768;
      else if (bits === 24)
        acc += (buf.readUInt8(o) | (buf.readUInt8(o + 1) << 8) | (buf.readInt8(o + 2) << 16)) / 8388608;
      else if (bits === 32) acc += buf.readInt32LE(o) / 2147483648;
    }
    mono[i] = acc / channels;
  }
  return { samples: mono, sr: fmt.sr };
}

// ── WAV writer (mono 16-bit PCM) ─────────────────────────────────────
function writeWavMono(path, samples, sr) {
  const n = samples.length, bps = 2, dataLen = n * bps;
  const buf = Buffer.alloc(44 + dataLen);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + dataLen, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(sr, 24);
  buf.writeUInt32LE(sr * bps, 28); buf.writeUInt16LE(bps, 32);
  buf.writeUInt16LE(16, 34);
  buf.write("data", 36); buf.writeUInt32LE(dataLen, 40);
  let o = 44;
  for (let i = 0; i < n; i++) {
    const s = Math.max(-1, Math.min(1, samples[i]));
    buf.writeInt16LE(Math.round(s * 32767), o); o += 2;
  }
  writeFileSync(path, buf);
}

// ── pull events from every source, then combine + sort by length ─────
const allSegs = [];   // { samples (mono), start, end, len, srcLabel }
let SR_OUT = 48000;

for (const SRC of SRCS) {
  const { samples, sr } = loadWavMono(SRC);
  SR_OUT = sr;
  const lbl = basename(SRC).replace(/\.wav$/, "");
  console.log(`source · ${lbl} · ${samples.length} samples · ${sr} Hz · ${(samples.length / sr).toFixed(2)}s`);

  // RMS envelope, 10 ms windows hopped every 5 ms
  const winN = Math.floor(sr * 0.010), hopN = Math.floor(sr * 0.005);
  const nHops = Math.floor((samples.length - winN) / hopN);
  const env = new Float32Array(nHops);
  for (let i = 0; i < nHops; i++) {
    let s = 0; const st = i * hopN;
    for (let k = 0; k < winN; k++) { const v = samples[st + k]; s += v * v; }
    env[i] = Math.sqrt(s / winN);
  }
  let peak = 0;
  for (let i = 0; i < env.length; i++) if (env[i] > peak) peak = env[i];
  const TH = peak * 0.12;

  // Find runs above threshold; 50 ms below-threshold sustain ends an event.
  const events = [];
  const gapHops = Math.ceil(0.05 / 0.005);
  let inEv = false, evStart = 0, belowRun = 0;
  for (let i = 0; i < env.length; i++) {
    if (env[i] > TH) { if (!inEv) { inEv = true; evStart = i; } belowRun = 0; }
    else if (inEv) {
      belowRun++;
      if (belowRun >= gapHops) {
        events.push([evStart, i - belowRun + 1]);
        inEv = false; belowRun = 0;
      }
    }
  }
  if (inEv) events.push([evStart, env.length - 1]);
  const minHops = Math.ceil(0.04 / 0.005);
  const kept = events.filter(([a, b]) => (b - a) >= minHops);
  console.log(`  · ${events.length} events · ${kept.length} kept (≥40 ms)`);

  const padIn = Math.floor(sr * 0.015), padOut = Math.floor(sr * 0.040);
  for (const [a, b] of kept) {
    const s0 = Math.max(0, a * hopN - padIn);
    const s1 = Math.min(samples.length, b * hopN + winN + padOut);
    allSegs.push({ samples, start: s0, end: s1, len: s1 - s0, srcLabel: lbl });
  }
}

// Sort all events across all takes by length, shortest first.
allSegs.sort((a, b) => a.len - b.len);

// Clean + write, with 3 ms declick fades on each shake.
mkdirSync(OUT_DIR, { recursive: true });
for (const f of readdirSync(OUT_DIR)) {
  if (f.endsWith(".wav")) try { unlinkSync(resolve(OUT_DIR, f)); } catch {}
}
console.log("");
const fadeN = Math.floor(SR_OUT * 0.003);
allSegs.forEach((seg, i) => {
  const slice = seg.samples.subarray(seg.start, seg.end);
  const out = new Float32Array(slice.length);
  for (let k = 0; k < slice.length; k++) {
    let v = slice[k];
    if (k < fadeN) v *= k / fadeN;
    if (slice.length - k < fadeN) v *= (slice.length - k) / fadeN;
    out[k] = v;
  }
  const durMs = Math.round(seg.len * 1000 / SR_OUT);
  const name = `shake-${String(i + 1).padStart(2, "0")}-${String(durMs).padStart(4, "0")}ms.wav`;
  writeWavMono(resolve(OUT_DIR, name), out, SR_OUT);
  console.log(`  ${name}  · from ${seg.srcLabel}`);
});
console.log(`\n✓ wrote ${allSegs.length} shakes from ${SRCS.length} take(s) → ${OUT_DIR.replace(process.env.HOME, "~")}/`);
