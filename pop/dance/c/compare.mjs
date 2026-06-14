#!/usr/bin/env node
// compare.mjs — prove the C wobble engine matches the JS reference
// (pop/dance/synths/wobble.mjs) sample-for-sample, within libm ulps.
//
// For each preset it builds the same bassline, renders it two ways:
//   JS  — renderWobble per event, summed into a Float32 buffer
//   C   — emitWobbleScore → wobble.c → raw f32
// then reports max |Δ| and RMS Δ. PASS if max |Δ| < 1e-4 (float storage +
// libm ulps). Same posture as pop/nullabye/c/compare.mjs.
//
// Usage: node pop/dance/c/compare.mjs [--preset bomp] [--keep]

import { spawnSync } from "node:child_process";
import { existsSync, statSync, mkdtempSync, writeFileSync, readFileSync, rmSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

import { renderWobble, emitWobbleScore } from "../synths/wobble.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "wobble");
const SR = 48_000;
const BPM = 140;
const PASS_EPS = 1e-4;

const args = process.argv.slice(2);
const argi = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const onlyPreset = argi("--preset");
const keep = args.includes("--keep");

// build engine if missing or stale
const cSrc = resolve(HERE, "wobble.c");
if (!existsSync(ENGINE) || statSync(cSrc).mtimeMs > statSync(ENGINE).mtimeMs) {
  console.log("[compare] building wobble…");
  const b = spawnSync("bash", [resolve(HERE, "build.sh")], { stdio: "inherit" });
  if (b.status !== 0) process.exit(1);
}

// a representative bassline per preset (covers detune, all LFO shapes, crush=0)
function lineFor(preset) {
  const beat = 60 / BPM;
  const roots = { woe: [38, 36, 34, 33], bomp: [40, 40, 43, 38], row: [33, 33, 41, 43], reese: [38, 38, 41, 36] };
  const r = roots[preset] || roots.woe;
  const events = [];
  for (let bar = 0; bar < 4; bar++) {
    for (let half = 0; half < 2; half++) {
      events.push({
        startSec: (bar * 4 + half * 2) * beat, midi: r[bar], gain: 0.95,
        durSec: 2 * beat * 0.98, preset,
      });
    }
  }
  return events;
}

function renderJS(events) {
  const score = emitWobbleScore(events, { sampleRate: SR, bpm: BPM });
  const ns = Math.ceil(parseFloat(score.match(/^dur (\S+)/m)[1]) * SR);
  const out = new Float32Array(ns);
  for (const ev of events) {
    const seg = renderWobble(ev, { sampleRate: SR, bpm: BPM });
    const start = Math.floor((ev.startSec ?? 0) * SR);
    for (let i = 0; i < seg.length; i++) {
      const dst = start + i; if (dst >= 0 && dst < out.length) out[dst] += seg[i];
    }
  }
  return { out, score };
}

function renderC(score, tmp) {
  const scorePath = resolve(tmp, "score.txt");
  const rawPath = resolve(tmp, "out.f32");
  writeFileSync(scorePath, score);
  const r = spawnSync(ENGINE, [scorePath, "--raw", rawPath], { stdio: ["ignore", "ignore", "inherit"] });
  if (r.status !== 0) { console.error("✗ C engine failed"); process.exit(1); }
  const buf = readFileSync(rawPath);
  const out = new Float32Array(buf.buffer, buf.byteOffset, Math.floor(buf.byteLength / 4));
  return out;
}

const presets = onlyPreset ? [onlyPreset] : ["woe", "bomp", "row", "reese"];
const tmp = mkdtempSync(resolve(tmpdir(), "wobble-cmp-"));
let allPass = true;

for (const p of presets) {
  const events = lineFor(p);
  const { out: js, score } = renderJS(events);
  const c = renderC(score, tmp);

  const n = Math.min(js.length, c.length);
  let maxAbs = 0, sumSq = 0;
  for (let i = 0; i < n; i++) {
    const d = Math.abs(js[i] - c[i]);
    if (d > maxAbs) maxAbs = d;
    sumSq += d * d;
  }
  const rms = Math.sqrt(sumSq / Math.max(1, n));
  const lenOk = js.length === c.length;
  const pass = lenOk && maxAbs < PASS_EPS;
  allPass = allPass && pass;
  console.log(
    `${pass ? "✓" : "✗"} ${p.padEnd(6)} samples=${n} (js ${js.length}/c ${c.length}) ` +
    `maxΔ=${maxAbs.toExponential(3)} rmsΔ=${rms.toExponential(3)}`,
  );
}

if (!keep) rmSync(tmp, { recursive: true, force: true });
console.log(allPass ? "\nPASS — C engine matches JS reference." : "\nFAIL — divergence above threshold.");
process.exit(allPass ? 0 : 1);
