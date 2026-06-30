#!/usr/bin/env node
// compare.mjs — parity harness: the C modal render vs a JS reference rebuilt
// from the SAME exported mode table. Mirrors the convention in
// pop/nullabye/c/compare.mjs and pop/marimba/c/compare.mjs.
//
// Both sides render with the strike transient and normalization disabled
// (--nostrike --nonorm) so the comparison is a clean deterministic modal sum;
// agreement then validates that the JS-side modal synth (used elsewhere in the
// monorepo) matches the C engine within libm tolerance.
//
// Usage:  node pop/bell/c/compare.mjs [--note A4] [--material bronze]

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, statSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "bell");
const args = process.argv.slice(2);
const val = (k, d) => {
  const i = args.indexOf(k);
  return i >= 0 ? args[i + 1] : d;
};
const note = val("--note", "A4");
const material = val("--material", "bronze");
const geometry = val("--geometry", "church");
const dur = parseFloat(val("--dur", "3"));
const SR = 48000;

if (!existsSync(ENGINE) || statSync(resolve(HERE, "bell.c")).mtimeMs > statSync(ENGINE).mtimeMs) {
  spawnSync("bash", [resolve(HERE, "build.sh")], { stdio: "inherit" });
}

mkdirSync("/tmp/bell-cmp", { recursive: true });
const wav = "/tmp/bell-cmp/c.wav";
const modesPath = "/tmp/bell-cmp/modes.json";

// C render: pure modal sum (no strike, no normalize).
const r = spawnSync(ENGINE, [
  "--note", note, "--material", material, "--geometry", geometry,
  "--dur", String(dur), "--vel", "0.9",
  "--nostrike", "--nonorm", "--out", wav, "--modes", modesPath,
], { stdio: ["ignore", "ignore", "inherit"] });
if (r.status !== 0) { console.error("C render failed"); process.exit(1); }

// Read the C WAV (f32 stereo) -> left channel.
function readWavF32Left(path) {
  const buf = readFileSync(path);
  // find "data" chunk
  let off = 12;
  while (off + 8 <= buf.length) {
    const id = buf.toString("ascii", off, off + 4);
    const sz = buf.readUInt32LE(off + 4);
    if (id === "data") {
      const n = sz / 8; // stereo f32
      const L = new Float64Array(n);
      for (let i = 0; i < n; i++) L[i] = buf.readFloatLE(off + 8 + i * 8);
      return L;
    }
    off += 8 + sz + (sz & 1);
  }
  throw new Error("no data chunk");
}
const cL = readWavF32Left(wav);

// JS reference: rebuild from the modes JSON exactly as bell_render does
// (left channel). amp_k = part * vel / sqrt(2*pi*f); peak = sum|amp|; then
// s = (amp/peak) * exp(-i/(tau*sr)) * sin(2*pi*phase) * panLeft.
const model = JSON.parse(readFileSync(modesPath, "utf8"));
const modes = model.modes;
const vel = 0.9;
const TAU = 2 * Math.PI;
const amp = modes.map((m) => m.part * vel / Math.sqrt(TAU * m.freq));
let peak = 0;
for (const a of amp) peak += Math.abs(a);
if (peak < 1e-30) peak = 1;
const N = Math.min(cL.length, Math.floor(dur * SR));
const jL = new Float64Array(N);
for (let k = 0; k < modes.length; k++) {
  const m = modes[k];
  const a = amp[k] / peak;
  const sp = 0.16 * ((m.m % 5) - 2);
  const gl = 0.5 - 0.5 * sp; // panLeft (matches C)
  const inc = m.freq / SR;
  let phase = 0;
  for (let i = 0; i < N; i++) {
    const env = Math.exp(-i / (m.tau * SR));
    jL[i] += a * env * Math.sin(TAU * phase) * gl;
    phase += inc;
    if (phase >= 1) phase -= 1;
  }
}

let linf = 0, l2 = 0;
for (let i = 0; i < N; i++) {
  const d = Math.abs(cL[i] - jL[i]);
  if (d > linf) linf = d;
  l2 += d * d;
}
l2 = Math.sqrt(l2 / N);
const TOL = 5e-4; // JSON-precision + libm ulps
const ok = linf < TOL;
console.log(`compare ${geometry}/${material} ${note}: ${modes.length} modes, ${N} samples`);
console.log(`  L-inf = ${linf.toExponential(3)}   RMS = ${l2.toExponential(3)}   tol = ${TOL}`);
console.log(ok ? "  ✓ C and JS modal renders agree" : "  ✗ MISMATCH");
process.exit(ok ? 0 : 1);
