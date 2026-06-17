#!/usr/bin/env node
// compare.mjs — A/B the C engine (nullnoise) against the JS reference
// (render-nuellaby.mjs) on the same score. Both render pre-master f32;
// we diff per-sample. Expected: near bit-exact (libm-ulp drift only,
// well under -100 dB RMS).
//
// Usage:  node pop/nullabye/c/compare.mjs

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

// 1 — JS reference render, keeping the pre-master raw
run("JS render", "node", [resolve(LANE, "bin/render-nuellaby.mjs"), "--keep-raw"]);
const jsRaw = resolve(OUT, "nuellaby.mp3.f32.raw");

// 2 — bake the score + C engine render
run("bake", "node", [resolve(LANE, "bin/render-nuellaby.mjs"), "--bake", resolve(OUT, "nuellaby.score.txt")]);
run("C render", "node", [resolve(HERE, "run-c.mjs"), resolve(OUT, "nuellaby.score.txt"), "--raw", resolve(OUT, "nuellaby-c.f32.raw")]);
const cRaw = resolve(OUT, "nuellaby-c.f32.raw");

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
