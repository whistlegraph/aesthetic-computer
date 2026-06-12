#!/usr/bin/env node
// compare.mjs — A/B the C engine's master (out/momabobasheep-c.mp3)
// against the JS reference (out/momabobasheep.mp3). The hellsine rule:
// don't chase bit-exactness, match the SOUND. Checks:
//
//   • duration          — must match to the frame budget (±0.1 s)
//   • integrated LUFS   — within ±0.5 dB
//   • per-minute RMS    — 10 one-minute buckets, each within ±1.5 dB
//                         (the dynamic night arc must survive the port)
//   • spectrum sanity   — six octave-ish bands (sub/bass/low-mid/mid/
//                         high-mid/high), each within ±3 dB → every layer
//                         (drone, bass, choir+pad, marimba+whistle,
//                         bells+sparkle ring) is present and balanced
//
// usage: node pop/momboba/c/compare.mjs
//        node pop/momboba/c/compare.mjs --js out/x.mp3 --c out/y.mp3

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const jsPath = resolve(_argi("--js") ?? join(ROOT, "out", "momabobasheep.mp3"));
const cPath = resolve(_argi("--c") ?? join(ROOT, "out", "momabobasheep-c.mp3"));
for (const p of [jsPath, cPath]) {
  if (!existsSync(p)) { console.error(`✗ missing ${p}`); process.exit(1); }
}

const SR = 44_100;

function duration(path) {
  const r = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "csv=p=0", path], { encoding: "utf8" });
  return parseFloat(r.stdout);
}

function lufs(path) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
    "-i", path, "-af", "loudnorm=print_format=summary", "-f", "null", "-"], { encoding: "utf8" });
  const im = (r.stderr || "").match(/Input Integrated:\s*(-?[\d.]+|inf)/);
  const tp = (r.stderr || "").match(/Input True Peak:\s*(-?[\d.]+|inf)/);
  return { i: im ? parseFloat(im[1]) : NaN, tp: tp ? parseFloat(tp[1]) : NaN };
}

// decode to mono f32 once per file (sequential — 8 GB box, no parallel ffmpeg)
function decodeMono(path) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", path, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"],
    { maxBuffer: 1 << 30 });
  if (r.status !== 0) { console.error(`✗ decode failed: ${path}`); process.exit(1); }
  return new Float32Array(r.stdout.buffer, r.stdout.byteOffset, Math.floor(r.stdout.length / 4));
}

const db = (x) => 20 * Math.log10(Math.max(x, 1e-12));

function minuteRms(buf) {
  const out = [];
  const win = 60 * SR;
  for (let b = 0; b * win < buf.length; b++) {
    const s0 = b * win, s1 = Math.min(buf.length, s0 + win);
    let e = 0;
    for (let i = s0; i < s1; i++) e += buf[i] * buf[i];
    out.push(db(Math.sqrt(e / (s1 - s0))));
  }
  return out;
}

// RBJ biquad band-pass chain: butterworth-ish HP + LP pair per band.
function biquadHP(fc) { return rbj(fc, true); }
function biquadLP(fc) { return rbj(fc, false); }
function rbj(fc, hp) {
  const w = 2 * Math.PI * fc / SR, cw = Math.cos(w), sw = Math.sin(w);
  const alpha = sw / (2 * Math.SQRT1_2);              // Q = 1/√2
  const a0 = 1 + alpha;
  const b = hp
    ? [(1 + cw) / 2, -(1 + cw), (1 + cw) / 2]
    : [(1 - cw) / 2, 1 - cw, (1 - cw) / 2];
  return { b0: b[0] / a0, b1: b[1] / a0, b2: b[2] / a0, a1: -2 * cw / a0, a2: (1 - alpha) / a0 };
}
function bandRms(buf, lo, hi) {
  const f1 = biquadHP(lo), f2 = biquadLP(hi);
  let x1 = 0, x2 = 0, y1 = 0, y2 = 0;     // stage 1 state
  let u1 = 0, u2 = 0, v1 = 0, v2 = 0;     // stage 2 state
  let e = 0;
  for (let i = 0; i < buf.length; i++) {
    const x = buf[i];
    const y = f1.b0 * x + f1.b1 * x1 + f1.b2 * x2 - f1.a1 * y1 - f1.a2 * y2;
    x2 = x1; x1 = x; y2 = y1; y1 = y;
    const v = f2.b0 * y + f2.b1 * u1 + f2.b2 * u2 - f2.a1 * v1 - f2.a2 * v2;
    u2 = u1; u1 = y; v2 = v1; v1 = v;
    e += v * v;
  }
  return db(Math.sqrt(e / buf.length));
}

const BANDS = [
  ["sub/drone root", 30, 90],
  ["bass", 90, 200],
  ["low-mid (choir+pad)", 200, 500],
  ["mid (marimba+whistle)", 500, 1200],
  ["high-mid (bells+spark)", 1200, 3000],
  ["high (ring/air)", 3000, 8000],
];

// ── measure ──────────────────────────────────────────────────────────────
console.log(`A (js): ${jsPath}`);
console.log(`B (c) : ${cPath}\n`);

const durJ = duration(jsPath), durC = duration(cPath);
const luJ = lufs(jsPath), luC = lufs(cPath);
console.log("· decoding js…");
const bufJ = decodeMono(jsPath);
const rmsJ = minuteRms(bufJ);
const bandsJ = BANDS.map(([, lo, hi]) => bandRms(bufJ, lo, hi));
console.log("· decoding c…");
const bufC = decodeMono(cPath);
const rmsC = minuteRms(bufC);
const bandsC = BANDS.map(([, lo, hi]) => bandRms(bufC, lo, hi));

// ── verdict table ────────────────────────────────────────────────────────
const rows = [];
const check = (name, a, b, tol, unit) => {
  const d = b - a;
  const ok = Math.abs(d) <= tol;
  rows.push([name, a.toFixed(2), b.toFixed(2), `${d >= 0 ? "+" : ""}${d.toFixed(2)} ${unit}`, `±${tol}`, ok ? "PASS" : "FAIL"]);
  return ok;
};

let pass = true;
pass &= check("duration (s)", durJ, durC, 0.1, "s");
pass &= check("integrated LUFS", luJ.i, luC.i, 0.5, "dB");
rmsJ.forEach((v, i) => {
  if (i >= rmsC.length) return;
  pass &= check(`RMS min ${i}–${i + 1}`, v, rmsC[i], 1.5, "dB");
});
BANDS.forEach(([name], i) => {
  pass &= check(`band ${name}`, bandsJ[i], bandsC[i], 3.0, "dB");
});
const clip = luC.tp >= -0.05;
rows.push(["true peak (c)", luJ.tp.toFixed(2), luC.tp.toFixed(2), "", "< -0.05", clip ? "FAIL" : "PASS"]);
pass &= !clip;

const widths = [24, 9, 9, 12, 8, 6];
const fmt = (r) => r.map((c, i) => String(c).padEnd(widths[i])).join(" ");
console.log("\n" + fmt(["measure", "js", "c", "delta", "tol", "verdict"]));
console.log("-".repeat(widths.reduce((a, b) => a + b + 1, 0)));
for (const r of rows) console.log(fmt(r));
console.log(`\n${pass ? "✓ PASS — the C engine matches the JS reference" : "✗ FAIL — divergence above; listen + investigate"}`);
process.exit(pass ? 0 : 1);
