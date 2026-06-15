#!/usr/bin/env node
// sync-whistle.mjs — lock the whistle to cornerfive's grid AND key, with
// rubberband (pitch-preserving). For each detected note we:
//   • stretch it (rubberband --time) so its onset→next-onset span lands
//     exactly on the 16th-note grid → tight sync, no drift
//   • pitch-shift it (rubberband --pitch) onto the nearest B-major tone
// rubberband preserves timbre/formants, so it stays true to the whistle
// while being synced + tuned — no WORLD resynth, no choppy reglue.
//
// Output: src/whistle-synced.wav + src/whistle-synced.notes.json (the grid
// onsets + pitches, so the remix renderer can chop on phrase boundaries).
//
// Usage: node bin/sync-whistle.mjs [--quantize 16|8]

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync, rmSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const ANALYSIS  = resolve(ROOT, "src/whistle.analysis.json");
const IN_WAV    = resolve(ROOT, "src/whistle.wav");
const OUT_WAV   = resolve(ROOT, "src/whistle-synced.wav");  // beat-aligned + shifted
const OUT_NOTES = resolve(ROOT, "src/whistle-synced.notes.json");
const TMP       = resolve(ROOT, "src/.slices");

const BPM = 73.2;
const STEP = (4 * 60 / BPM) / 16;

const flags = {};
for (let i = 2; i < process.argv.length; i++)
  if (process.argv[i].startsWith("--")) flags[process.argv[i].slice(2)] = process.argv[i + 1];
const QGRID = STEP * (flags.quantize === "8" ? 2 : 1); // quantize unit

// Autotune target = the whistle's OWN observed melody (each note locked to
// the nearest semitone of what it actually whistled — chromatic, no key).
const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const nameOf = (m) => `${NAMES[((m % 12) + 12) % 12]}${Math.floor(m / 12) - 1}`;

function writeWav(path, samples, sr) {
  const n = samples.length, buf = Buffer.alloc(44 + n * 2);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 2, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(sr, 24); buf.writeUInt32LE(sr * 2, 28);
  buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34); buf.write("data", 36); buf.writeUInt32LE(n * 2, 40);
  for (let i = 0; i < n; i++) buf.writeInt16LE(Math.round(Math.max(-1, Math.min(1, samples[i])) * 32767), 44 + i * 2);
  writeFileSync(path, buf);
}

// ── merge adjacent same-pitch notes (keep the ORIGINAL melody) ──────────
const { melody } = JSON.parse(readFileSync(ANALYSIS, "utf8"));
const notes = [];
for (const n of melody) {
  const tgt = n.midi; // its OWN observed pitch
  const last = notes[notes.length - 1];
  if (last && last.tgt === tgt) continue;
  notes.push({ tgt, src: n.startSec, cents: n.centsOff || 0 });
}

// grid-quantize onsets, strictly increasing
let prev = -Infinity;
for (const n of notes) {
  let q = Math.round(n.src / QGRID) * QGRID;
  if (q <= prev) q = prev + QGRID;
  n.dst = q;
  prev = q;
}

const { samples: x, sampleRate: SR } = readWavMono(IN_WAV);
const srcDur = x.length / SR;
const lastSeg = Math.max(STEP, Math.round((srcDur - notes[notes.length - 1].src) / QGRID) * QGRID);
const dstDurTotal = notes[notes.length - 1].dst + lastSeg;

rmSync(TMP, { recursive: true, force: true });
mkdirSync(TMP, { recursive: true });

const synced = new Float32Array(Math.ceil(dstDurTotal * SR) + SR);
const xf = Math.floor(0.008 * SR); // 8ms join crossfade

// ── rubberband each note: beat-align (stretch to grid) + shift (tune to
// the semitone center via cents). Keeps the ORIGINAL melody/contour — no
// WORLD f0-replace, no flattening. ────────────────────────────────────────
console.log(`→ beat-aligning + shifting ${notes.length} notes (rubberband, original melody)…`);
for (let i = 0; i < notes.length; i++) {
  const srcStart = notes[i].src;
  const srcEnd   = i + 1 < notes.length ? notes[i + 1].src : srcDur;
  const dstStart = notes[i].dst;
  const dstEnd   = i + 1 < notes.length ? notes[i + 1].dst : dstDurTotal;
  const sLen = Math.max(0.02, srcEnd - srcStart);
  const dLen = Math.max(0.02, dstEnd - dstStart);

  const a = Math.floor(srcStart * SR), b = Math.min(x.length, Math.floor(srcEnd * SR));
  const slice = x.subarray(a, b);
  const inP = resolve(TMP, `in-${i}.wav`), outP = resolve(TMP, `out-${i}.wav`);
  writeWav(inP, slice, SR);

  const ratio = Math.max(0.25, Math.min(4, dLen / sLen)); // beat-align
  const shift = (-notes[i].cents / 100).toFixed(3);       // nudge into tune
  const r = spawnSync("rubberband", [
    "--time", ratio.toFixed(4), "--pitch", shift, "-q", inP, outP,
  ], { encoding: "utf8" });
  let piece;
  if (r.status === 0) piece = readWavMono(outP).samples;
  else { console.warn(`  ⚠ rubberband note ${i} failed, using dry slice`); piece = slice; }

  const off = Math.floor(dstStart * SR);
  for (let k = 0; k < piece.length; k++) {
    let g = 1;
    if (k < xf) g = k / xf;
    if (piece.length - k < xf) g = (piece.length - k) / xf;
    const di = off + k; if (di >= synced.length) break;
    synced[di] += piece[k] * g;
  }
}
rmSync(TMP, { recursive: true, force: true });
writeWav(OUT_WAV, synced, SR);

writeFileSync(OUT_NOTES, JSON.stringify({
  tuning: "original melody (beat-aligned + cents-shifted)", bpm: BPM,
  quantize: flags.quantize === "8" ? 8 : 16,
  notes: notes.map((n) => ({ note: nameOf(n.tgt), midi: n.tgt, startSec: +n.dst.toFixed(3) })),
}, null, 2) + "\n");
console.log(`✓ beat-aligned + shifted → ${OUT_WAV.replace(process.env.HOME, "~")} (${dstDurTotal.toFixed(1)}s)`);
console.log(`  ${notes.map((n) => nameOf(n.tgt)).join(" ")}`);
