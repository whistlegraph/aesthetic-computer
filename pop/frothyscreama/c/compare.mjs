#!/usr/bin/env node
// compare.mjs — hold the C engine (frothyscreama.c) to its JS reference
// (bin/render-frothyscreama.mjs --js) on the same score. One JS run bakes the
// score and writes the premaster; the C engine renders the score; the two
// premasters are diffed per sample. Expected: the same record to within
// libm's last bit — well over 60 dB under the reference.
//
//   node pop/frothyscreama/c/compare.mjs            bake, render both, diff
//   node pop/frothyscreama/c/compare.mjs --no-chaos the same, without the chaos bank
//   node pop/frothyscreama/c/compare.mjs --no-bake  reuse the score + premaster already in out/
//        (after a plain render that premaster is the engine's own, so this only checks the engine against itself)

import { readFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
const NOCHAOS = process.argv.includes("--no-chaos"), STEM = "frothyscreama";
const score = resolve(OUT, `${STEM}.score.txt`), cRaw = resolve(OUT, "frothyscreama-c.f32.raw");
const run = (label, cmd, args) => {
  console.log(`[compare] ${label}`);
  const r = spawnSync(cmd, args, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
};

if (!process.argv.includes("--no-bake")) run("JS render + bake", "node", [resolve(HERE, "../bin/render-frothyscreama.mjs"), "--js", "--bake", score, ...(NOCHAOS ? ["--no-chaos"] : [])]);
run("C render", "node", [resolve(HERE, "run-c.mjs"), score, "--raw", cRaw]);

// the premaster on disk is a float WAV; its samples start after the "data" chunk header
const f32 = (buf, skip) => new Float32Array(buf.buffer.slice(buf.byteOffset + skip, buf.byteOffset + buf.byteLength));
const wavBody = (buf) => { let i = 12; while (buf.toString("ascii", i, i + 4) !== "data") i += 8 + buf.readUInt32LE(i + 4) + (buf.readUInt32LE(i + 4) & 1); return i + 8; };
const js = readFileSync(resolve(OUT, `${STEM}-premaster.wav`));
const a = f32(js, wavBody(js)), b = f32(readFileSync(cRaw), 0);
const n = Math.min(a.length, b.length);
if (a.length !== b.length) console.log(`  length mismatch: js ${a.length} vs c ${b.length} samples`);
let sumSq = 0, sumRefSq = 0, peakDiff = 0, peakAt = 0, same = 0;
for (let i = 0; i < n; i++) {
  const d = a[i] - b[i];
  sumSq += d * d; sumRefSq += a[i] * a[i];
  if (d === 0) same++;
  if (Math.abs(d) > peakDiff) { peakDiff = Math.abs(d); peakAt = i; }
}
const diffDb = 10 * Math.log10(sumSq / n + 1e-30), refDb = 10 * Math.log10(sumRefSq / n + 1e-30);
console.log(`  ref level   : ${refDb.toFixed(1)} dBFS RMS`);
console.log(`  diff        : ${diffDb.toFixed(1)} dBFS RMS (${(diffDb - refDb).toFixed(1)} dB below ref)`);
console.log(`  peak diff   : ${peakDiff.toExponential(2)} at sample ${peakAt} (t=${(peakAt / 2 / 48000).toFixed(2)}s)`);
console.log(`  identical   : ${(100 * same / n).toFixed(2)}% of samples`);
const ok = a.length === b.length && diffDb - refDb < -60;
console.log(ok ? "✓ engines match (diff > 60 dB down)" : "✗ engines DIVERGE — investigate");
process.exit(ok ? 0 : 1);
