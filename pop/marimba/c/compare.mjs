#!/usr/bin/env node
// compare.mjs — A/B the C engine (fluttabap360) against the JS reference
// (render-fluttabap360.mjs) on the same score. Both render pre-master f32;
// we diff per-sample. Expected: near bit-exact (libm-ulp drift only,
// well under -60 dB RMS below the reference). Mirrors pop/nullabye/c/compare.mjs.
//
// Usage:  node pop/marimba/c/compare.mjs

import { readFileSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
mkdirSync(OUT, { recursive: true });

const run = (label, cmd, args) => {
  console.log(`[compare] ${label}`);
  const r = spawnSync(cmd, args, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
};

// --solo <csv> → compare only the named (already-ported) voices in isolation,
// for milestone-by-milestone parity while the port is incomplete. Omit once
// every voice is ported to compare the full mix.
const _ai = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const solo = _ai("--solo");
const jsArgs = ["--keep-raw", "--out", resolve(OUT, "_cmp.mp3")];
if (solo) jsArgs.push("--solo", solo);

// 1 — JS reference render, keeping the pre-master raw
run(`JS render${solo ? ` (solo ${solo})` : ""}`, "node", [resolve(LANE, "bin/render-fluttabap360.mjs"), ...jsArgs]);
const jsRaw = resolve(OUT, "_cmp.mp3.f32.raw");

// 2 — bake the score + C engine render (pre-master raw only)
run("bake", "node", [resolve(LANE, "bin/render-fluttabap360.mjs"), "--bake", resolve(OUT, "fluttabap360.score.txt")]);
const cArgs = [resolve(HERE, "run-c.mjs"), resolve(OUT, "fluttabap360.score.txt"), "--raw", resolve(OUT, "fluttabap360-c.f32.raw")];
if (solo) cArgs.push("--solo", solo);   // restrict the C engine to the same voices
run("C render", "node", cArgs);
const cRaw = resolve(OUT, "fluttabap360-c.f32.raw");

// 3 — diff
const a = new Float32Array(readFileSync(jsRaw).buffer.slice(0));
const b = new Float32Array(readFileSync(cRaw).buffer.slice(0));
const n = Math.min(a.length, b.length);
if (a.length !== b.length) console.log(`  length mismatch: js ${a.length} vs c ${b.length} samples`);
let sumSq = 0, sumRefSq = 0, peakDiff = 0, peakAt = 0;
for (let i = 0; i < n; i++) {
  const d = a[i] - b[i];
  sumSq += d * d;
  sumRefSq += a[i] * a[i];
  if (Math.abs(d) > peakDiff) { peakDiff = Math.abs(d); peakAt = i; }
}
const rmsDiffDb = 10 * Math.log10(sumSq / n + 1e-30);
const refDb = 10 * Math.log10(sumRefSq / n + 1e-30);
console.log(`  ref level   : ${refDb.toFixed(1)} dBFS RMS`);
console.log(`  diff        : ${rmsDiffDb.toFixed(1)} dBFS RMS (${(rmsDiffDb - refDb).toFixed(1)} dB below ref)`);
console.log(`  peak diff   : ${peakDiff.toExponential(2)} at sample ${peakAt} (t=${(peakAt / 2 / 48000).toFixed(2)}s)`);
console.log(rmsDiffDb - refDb < -60 ? "✓ engines match (diff > 60 dB down)" : "✗ engines DIVERGE — investigate");
