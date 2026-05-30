#!/usr/bin/env node
// profile-segments.mjs — slide a window across a long vocal recording,
// profile each window for dryness, rank + plot the result. Useful when
// a long lecture has both dry and reverby passages — we want to extract
// just the dry segments for ElevenLabs PVC retraining.
//
// Algorithm:
//   1. ffmpeg → 16k mono PCM
//   2. Slide a WINDOW_SEC window with HOP_SEC step across the whole file
//   3. For each window, run the same floor/active ratio metric as
//      profile-vocal.mjs
//   4. Emit a per-window JSON + a printed table + an ascii sparkline
//   5. (optional) emit ffmpeg `concat`-format file listing the dry
//      windows only — pipe into ffmpeg to render a "dry-only" cut.
//
// Usage:
//   node bin/profile-segments.mjs <mp3>
//   node bin/profile-segments.mjs <mp3> --window 30 --hop 15
//   node bin/profile-segments.mjs <mp3> --ratio-max 0.04 --emit-cut dry-only.mp3
//   node bin/profile-segments.mjs <mp3> --json segments.json
//   node bin/profile-segments.mjs <mp3> --match-reference <ref.mp3>
//      ↑ tighter gate: each window must (a) pass --ratio-max AND
//        (b) have active RMS within ±RMS-TOL of the reference AND
//        (c) ratio within ±RATIO-TOL of the reference's ratio.
//        Defaults: --rms-tol 0.35 (±35%)   --ratio-tol 0.015

import { spawnSync } from "node:child_process";
import { existsSync, writeFileSync, mkdtempSync, rmSync } from "node:fs";
import { resolve, basename } from "node:path";
import { tmpdir } from "node:os";

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  } else positional.push(a);
}

const mp3 = positional[0] && resolve(process.cwd(), positional[0]);
if (!mp3 || !existsSync(mp3)) {
  console.error("usage: profile-segments.mjs <mp3> [--window 30] [--hop 15] [--ratio-max 0.04] [--emit-cut out.mp3] [--json segments.json]");
  process.exit(2);
}

const WINDOW = Number(flags.window ?? 30);   // seconds per analysis window
const HOP = Number(flags.hop ?? WINDOW / 2); // step size — default 50% overlap
const RATIO_MAX = Number(flags["ratio-max"] ?? 0.04);
const REFERENCE = flags["match-reference"]
  ? resolve(process.cwd(), flags["match-reference"])
  : null;
const RMS_TOL = Number(flags["rms-tol"] ?? 0.35);     // ±35% on active RMS
const RATIO_TOL = Number(flags["ratio-tol"] ?? 0.015); // ±0.015 on dryness ratio
const SR = 16000;
const FRAME = Math.round(SR * 0.02);  // 20ms RMS frames

// ── reference acoustics (optional) ────────────────────────────────────
let refStats = null;
if (REFERENCE) {
  if (!existsSync(REFERENCE)) {
    console.error(`✗ reference not found: ${REFERENCE}`);
    process.exit(2);
  }
  const r = spawnSync("ffmpeg", [
    "-v", "error", "-i", REFERENCE, "-ac", "1", "-ar", String(SR),
    "-f", "s16le", "-",
  ], { maxBuffer: 1024 * 1024 * 1024 });
  if (r.status !== 0) { console.error(r.stderr.toString()); process.exit(1); }
  const rbuf = r.stdout;
  const rsamples = new Int16Array(rbuf.buffer, rbuf.byteOffset, rbuf.byteLength / 2);
  const rN = Math.floor(rsamples.length / FRAME);
  const rRms = new Float64Array(rN);
  for (let i = 0; i < rN; i++) {
    let sum = 0; const st = i * FRAME;
    for (let j = 0; j < FRAME; j++) { const v = rsamples[st+j]/32768; sum += v*v; }
    rRms[i] = Math.sqrt(sum / FRAME);
  }
  const sortedR = Float64Array.from(rRms).sort();
  const floor = sortedR.slice(Math.floor(0.05*sortedR.length), Math.floor(0.25*sortedR.length));
  const active = sortedR.slice(Math.floor(0.75*sortedR.length), Math.floor(0.95*sortedR.length));
  refStats = {
    floor: floor[Math.floor(floor.length/2)] || 0,
    active: active[Math.floor(active.length/2)] || 0,
  };
  refStats.ratio = refStats.floor / Math.max(refStats.active, 1e-9);
  console.log(`→ reference: ${basename(REFERENCE)}  ·  active ${refStats.active.toFixed(5)}  ratio ${refStats.ratio.toFixed(4)}`);
  console.log(`  gate: ratio < ${RATIO_MAX}  ·  active within ±${(RMS_TOL*100).toFixed(0)}% of reference  ·  ratio within ±${RATIO_TOL.toFixed(3)} of reference`);
}

// ── decode ────────────────────────────────────────────────────────────
console.log(`→ decoding ${basename(mp3)} (16k mono)…`);
const ff = spawnSync("ffmpeg", [
  "-v", "error", "-i", mp3, "-ac", "1", "-ar", String(SR),
  "-f", "s16le", "-",
], { maxBuffer: 1024 * 1024 * 1024 });
if (ff.status !== 0) { console.error(ff.stderr.toString()); process.exit(1); }
const buf = ff.stdout;
const samples = new Int16Array(buf.buffer, buf.byteOffset, buf.byteLength / 2);
const durationSec = samples.length / SR;
console.log(`  duration: ${durationSec.toFixed(1)}s  (~${(durationSec/60).toFixed(1)} min)`);
console.log(`  window:   ${WINDOW}s  hop: ${HOP}s  → ~${Math.floor((durationSec - WINDOW) / HOP) + 1} segments`);

// ── per-frame RMS over the whole file ─────────────────────────────────
const nFrames = Math.floor(samples.length / FRAME);
const rms = new Float64Array(nFrames);
for (let i = 0; i < nFrames; i++) {
  let sum = 0;
  const start = i * FRAME;
  for (let j = 0; j < FRAME; j++) {
    const v = samples[start + j] / 32768;
    sum += v * v;
  }
  rms[i] = Math.sqrt(sum / FRAME);
}

const FRAMES_PER_SEC = SR / FRAME;  // 50
const FRAMES_PER_WINDOW = Math.round(WINDOW * FRAMES_PER_SEC);
const FRAMES_PER_HOP = Math.round(HOP * FRAMES_PER_SEC);

function medianRange(arr, lo, hi) {
  const sorted = Float64Array.from(arr).sort();
  const a = sorted.slice(Math.floor(lo * sorted.length), Math.floor(hi * sorted.length));
  return a[Math.floor(a.length / 2)] || 0;
}

// ── slide window + measure ────────────────────────────────────────────
const segments = [];
for (let start = 0; start + FRAMES_PER_WINDOW <= nFrames; start += FRAMES_PER_HOP) {
  const slice = rms.subarray(start, start + FRAMES_PER_WINDOW);
  const floor = medianRange(slice, 0.05, 0.25);
  const active = medianRange(slice, 0.75, 0.95);
  const ratio = floor / Math.max(active, 1e-9);
  const startSec = start / FRAMES_PER_SEC;
  const endSec = (start + FRAMES_PER_WINDOW) / FRAMES_PER_SEC;
  // Default gate: just dryness ratio.
  // With --match-reference: also require active RMS within ±RMS_TOL and
  // ratio within ±RATIO_TOL of the reference, so we don't admit
  // "technically dry but sounds different" windows.
  let pass = ratio < RATIO_MAX;
  let matchReason = pass ? "dry" : "wet";
  if (pass && refStats) {
    const rmsLo = refStats.active * (1 - RMS_TOL);
    const rmsHi = refStats.active * (1 + RMS_TOL);
    const ratioLo = refStats.ratio - RATIO_TOL;
    const ratioHi = refStats.ratio + RATIO_TOL;
    if (active < rmsLo || active > rmsHi) { pass = false; matchReason = "rms-off"; }
    else if (ratio < ratioLo || ratio > ratioHi) { pass = false; matchReason = "ratio-off"; }
    else matchReason = "match";
  }
  segments.push({
    startSec: +startSec.toFixed(2),
    endSec: +endSec.toFixed(2),
    floor: +floor.toFixed(5),
    active: +active.toFixed(5),
    ratio: +ratio.toFixed(4),
    pass,
    reason: matchReason,
  });
}

// ── ranked print ──────────────────────────────────────────────────────
const dry = segments.filter((s) => s.pass).sort((a, b) => a.ratio - b.ratio);
const wet = segments.filter((s) => !s.pass).sort((a, b) => a.ratio - b.ratio);

console.log(`\n${segments.length} windows · ${dry.length} DRY · ${wet.length} WET`);
console.log(`DRY-fraction: ${(dry.length / segments.length * 100).toFixed(1)}%`);

function fmtSec(s) {
  const m = Math.floor(s / 60);
  const r = (s - m * 60).toFixed(1);
  return `${String(m).padStart(2, "0")}:${r.padStart(4, "0")}`;
}

if (dry.length) {
  console.log(`\n── driest windows ──`);
  for (const s of dry.slice(0, 12)) {
    console.log(`  ${fmtSec(s.startSec)} → ${fmtSec(s.endSec)}   ratio ${s.ratio.toFixed(4)} ✓`);
  }
}
console.log(`\n── borderline-driest (top 6 WET) ──`);
for (const s of wet.slice(0, 6)) {
  console.log(`  ${fmtSec(s.startSec)} → ${fmtSec(s.endSec)}   ratio ${s.ratio.toFixed(4)}`);
}

// ── ascii sparkline over time ─────────────────────────────────────────
const blocks = "▁▂▃▄▅▆▇█";
const ratios = segments.map((s) => s.ratio);
const lo = Math.min(...ratios);
const hi = Math.max(...ratios);
const spark = segments.map((s) => {
  const norm = (s.ratio - lo) / (hi - lo + 1e-9);
  const idx = Math.min(blocks.length - 1, Math.floor(norm * blocks.length));
  return s.pass ? blocks[idx] : blocks[blocks.length - 1];
}).join("");
console.log(`\n── ratio sparkline (low=dry, high=wet) ──`);
console.log(spark);
console.log(`time 0────────${fmtSec(durationSec / 4)}────────${fmtSec(durationSec / 2)}────────${fmtSec(3 * durationSec / 4)}────────${fmtSec(durationSec)}`);
console.log(`scale: ${lo.toFixed(4)} → ${hi.toFixed(4)}    ✓ = pass < ${RATIO_MAX}`);

// ── json emit ─────────────────────────────────────────────────────────
if (flags.json) {
  const path = resolve(process.cwd(), flags.json);
  writeFileSync(path, JSON.stringify({
    source: mp3, durationSec, window: WINDOW, hop: HOP, ratioMax: RATIO_MAX,
    segments,
  }, null, 2));
  console.log(`\n✓ wrote ${path}`);
}

// ── emit-cut: build a "dry-only" mp3 via ffmpeg concat ────────────────
if (flags["emit-cut"]) {
  if (!dry.length) {
    console.warn(`\n✗ --emit-cut requested but 0 windows pass dry threshold`);
    process.exit(1);
  }
  // Merge overlapping/adjacent dry windows into contiguous spans
  const sortedDry = [...dry].sort((a, b) => a.startSec - b.startSec);
  const spans = [];
  for (const s of sortedDry) {
    const last = spans[spans.length - 1];
    if (last && s.startSec <= last.endSec) {
      last.endSec = Math.max(last.endSec, s.endSec);
    } else spans.push({ startSec: s.startSec, endSec: s.endSec });
  }
  const totalDrySec = spans.reduce((a, s) => a + (s.endSec - s.startSec), 0);
  console.log(`\n→ ${spans.length} dry spans · ${totalDrySec.toFixed(1)}s total (~${(totalDrySec/60).toFixed(1)} min)`);

  const tmp = mkdtempSync(`${tmpdir()}/profile-segments-`);
  const concatList = spans.map((s, i) => {
    const out = `${tmp}/span-${String(i).padStart(3, "0")}.wav`;
    const r = spawnSync("ffmpeg", [
      "-y", "-v", "error", "-i", mp3,
      "-ss", String(s.startSec), "-to", String(s.endSec),
      "-ac", "1", "-ar", "44100", out,
    ]);
    if (r.status !== 0) { console.error(`✗ slice failed: ${r.stderr.toString()}`); process.exit(1); }
    return out;
  });
  const listPath = `${tmp}/list.txt`;
  writeFileSync(listPath, concatList.map((p) => `file '${p}'`).join("\n"));
  const cutOut = resolve(process.cwd(), flags["emit-cut"]);
  const cat = spawnSync("ffmpeg", [
    "-y", "-v", "error", "-f", "concat", "-safe", "0",
    "-i", listPath, "-ac", "1", "-ar", "44100", "-b:a", "192k", cutOut,
  ]);
  if (cat.status !== 0) { console.error(cat.stderr.toString()); process.exit(1); }
  console.log(`✓ ${cutOut}`);
  rmSync(tmp, { recursive: true, force: true });
}
