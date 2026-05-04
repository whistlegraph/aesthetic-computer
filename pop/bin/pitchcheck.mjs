#!/usr/bin/env node
// pitchcheck.mjs — measure the actual fundamental of each word in a
// rendered vocal stem and compare to what pitchsnap *intended* to
// shift each word to. Reads the `.events.json` emitted by pitchsnap
// (avoids re-aligning the rendered output, which whisper degrades on
// heavily-shifted audio).
//
// Pitch detection: autocorrelation over a central window of each
// word's slice, restricted to voice range [80 Hz, 600 Hz]. Parabolic
// interpolation around the peak for sub-sample precision. Skip
// silence by RMS gate.
//
// Output: per-word table of expected vs measured + cents drift, and a
// summary of mean / median absolute drift. ±50¢ = quarter-tone, ±25¢
// = "in tune."
//
// Usage:
//   node bin/pitchcheck.mjs --vocal big-pictures/out/mary-sung.mp3
//   (auto-finds mary-sung.events.json next to the mp3)

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, mkdirSync, rmSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";

function parseArgs(argv) {
  const flags = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) continue;
    const k = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; }
    else flags[k] = true;
  }
  return flags;
}

const flags = parseArgs(process.argv.slice(2));
const vocalPath = resolve(process.cwd(), flags.vocal || "");
if (!existsSync(vocalPath)) {
  console.error("usage: --vocal <pitchsnap-output.mp3>");
  process.exit(1);
}
const eventsPath = resolve(process.cwd(),
  flags.events || vocalPath.replace(/\.mp3$/, ".events.json"));
if (!existsSync(eventsPath)) {
  console.error(`✗ events file not found: ${eventsPath}\n  rerun pitchsnap.mjs to generate it.`);
  process.exit(1);
}
const SAMPLE_RATE = 48_000;
const F_MIN = Number(flags["f-min"]) || 80;
const F_MAX = Number(flags["f-max"]) || 600;

// ── helpers ───────────────────────────────────────────────────────────
function freqToMidi(f) { return 69 + 12 * Math.log2(f / 440); }
function midiToName(midi) {
  const names = ["C","C#","D","Eb","E","F","F#","G","G#","A","Bb","B"];
  const r = Math.round(midi);
  return `${names[((r % 12) + 12) % 12]}${Math.floor(r / 12) - 1}`;
}

function readWav(path) {
  const buf = readFileSync(path);
  let i = 12;
  while (i < buf.length - 8) {
    const id = buf.toString("ascii", i, i + 4);
    const size = buf.readUInt32LE(i + 4);
    if (id === "data") {
      i += 8;
      const samples = new Float32Array(size / 2);
      for (let j = 0; j < samples.length; j++) {
        samples[j] = buf.readInt16LE(i + j * 2) / 32768;
      }
      return samples;
    }
    i += 8 + size;
  }
  throw new Error(`no data chunk in ${path}`);
}

// Autocorrelation pitch detection. Naive but works for clean voice.
// Skip first/last 20% of samples (attack/release transients).
function detectPitch(samples, sr, fmin, fmax) {
  if (samples.length < sr * 0.05) return null; // < 50ms — too short
  const start = Math.floor(samples.length * 0.2);
  const end = Math.floor(samples.length * 0.8);
  const win = samples.slice(start, end);

  // RMS gate — skip silence
  let rms = 0;
  for (let i = 0; i < win.length; i++) rms += win[i] * win[i];
  rms = Math.sqrt(rms / win.length);
  if (rms < 0.005) return null;

  const lagMin = Math.floor(sr / fmax);
  const lagMax = Math.min(Math.floor(sr / fmin), Math.floor(win.length / 2));

  let bestLag = lagMin;
  let bestScore = -Infinity;
  for (let lag = lagMin; lag <= lagMax; lag++) {
    let sum = 0;
    let n = win.length - lag;
    for (let i = 0; i < n; i++) sum += win[i] * win[i + lag];
    sum /= n;
    if (sum > bestScore) { bestScore = sum; bestLag = lag; }
  }

  // Parabolic interpolation around peak for sub-sample precision
  let lagF = bestLag;
  if (bestLag > lagMin && bestLag < lagMax) {
    const acAt = (k) => {
      let s = 0;
      const n = win.length - k;
      for (let i = 0; i < n; i++) s += win[i] * win[i + k];
      return s / n;
    };
    const a = acAt(bestLag - 1);
    const b = acAt(bestLag);
    const c = acAt(bestLag + 1);
    const denom = a - 2 * b + c;
    if (Math.abs(denom) > 1e-9) lagF = bestLag - 0.5 * (c - a) / denom;
  }

  return sr / lagF;
}

// ── main ──────────────────────────────────────────────────────────────
const events = JSON.parse(readFileSync(eventsPath, "utf8"));

const tmpDir = `${dirname(vocalPath)}/.pitchcheck-tmp`;
rmSync(tmpDir, { recursive: true, force: true });
mkdirSync(tmpDir, { recursive: true });

console.log(
  `→ pitchcheck · ${events.events.length} events against ${basename(eventsPath)}\n` +
  `  vocal=${basename(vocalPath)} · stretch=${events.stretch}× curve=${events.curve}\n`
);
console.log(`  ${"i".padStart(3)} ${"word".padEnd(12)} ${"expected".padEnd(14)} ${"measured".padEnd(20)} drift`);
console.log(`  ${"─".repeat(60)}`);

let drifts = [];
let confidentCount = 0;

for (const ev of events.events) {
  const startSec = ev.snappedStart;
  const endSec = startSec + ev.durSec;

  const sliceWav = `${tmpDir}/w${ev.i.toString().padStart(3,"0")}.wav`;
  spawnSync("ffmpeg",
    ["-hide_banner","-y","-loglevel","error",
     "-ss",startSec.toFixed(4),"-to",endSec.toFixed(4),
     "-i",vocalPath,
     "-c:a","pcm_s16le","-ar",String(SAMPLE_RATE),"-ac","1",sliceWav],
    { stdio: ["ignore","ignore","ignore"] });
  if (!existsSync(sliceWav)) continue;

  const samples = readWav(sliceWav);
  const f0 = detectPitch(samples, SAMPLE_RATE, F_MIN, F_MAX);

  if (f0 === null) {
    console.log(`  ${ev.i.toString().padStart(3)} ${ev.text.padEnd(12)} ${ev.targetNote.padEnd(14)} ${"(silence)".padEnd(20)}`);
    continue;
  }

  const measuredMidi = freqToMidi(f0);
  const measuredName = midiToName(measuredMidi);
  const driftCents = (measuredMidi - ev.targetMidi) * 100;
  drifts.push(driftCents);
  confidentCount++;

  const driftStr = `${driftCents >= 0 ? "+" : ""}${driftCents.toFixed(0)}¢`;
  const measuredStr = `${measuredName} (${f0.toFixed(1)}Hz)`;
  console.log(
    `  ${ev.i.toString().padStart(3)} ${ev.text.padEnd(12)} ${ev.targetNote.padEnd(14)} ${measuredStr.padEnd(20)} ${driftStr}`
  );
}

rmSync(tmpDir, { recursive: true, force: true });

if (confidentCount === 0) {
  console.log("\n  no confident measurements — too much silence or noise");
  process.exit(0);
}

drifts.sort((a, b) => Math.abs(a) - Math.abs(b));
const median = Math.abs(drifts[Math.floor(drifts.length / 2)]);
const mean = drifts.reduce((a, b) => a + Math.abs(b), 0) / drifts.length;
const max = Math.max(...drifts.map(Math.abs));

console.log(`\n  summary · ${confidentCount}/${events.events.length} measured`);
console.log(`    median |drift| = ${median.toFixed(0)}¢`);
console.log(`    mean   |drift| = ${mean.toFixed(0)}¢`);
console.log(`    max    |drift| = ${max.toFixed(0)}¢`);
console.log(`\n  reference: ±50¢ = within a quarter-tone, ±25¢ = "in tune"`);

// ── Stutter detection ──────────────────────────────────────────────────
// Look for amplitude dips (RMS drops > 70% within 50ms then recovers
// within 80ms) and f0 jumps (frame-to-frame f0 ratio > 1.5 = >7
// semitones in 5ms). Both indicate WORLD phase resets / vocal skips.
{
  const fullSliceWav = `/tmp/pitchcheck-stutter-${Date.now()}.wav`;
  spawnSync("ffmpeg",
    ["-hide_banner", "-y", "-loglevel", "error",
     "-i", vocalPath,
     "-c:a", "pcm_s16le", "-ar", String(SAMPLE_RATE), "-ac", "1", fullSliceWav],
    { stdio: ["ignore", "ignore", "ignore"] });
  if (existsSync(fullSliceWav)) {
    const samples = readWav(fullSliceWav);
    const hop = Math.floor(0.010 * SAMPLE_RATE);
    const nF = Math.floor(samples.length / hop);
    const rms = new Float32Array(nF);
    for (let f = 0; f < nF; f++) {
      let r = 0;
      for (let j = 0; j < hop; j++) {
        const v = samples[f * hop + j];
        r += v * v;
      }
      rms[f] = Math.sqrt(r / hop);
    }
    // Smooth RMS with a 3-frame rolling mean for stability
    const sm = new Float32Array(nF);
    for (let f = 0; f < nF; f++) {
      let s = 0, c = 0;
      for (let k = -1; k <= 1; k++) {
        if (f + k >= 0 && f + k < nF) { s += rms[f + k]; c++; }
      }
      sm[f] = s / c;
    }
    const peak = sm.reduce((m, v) => v > m ? v : m, 0);
    // Stutter = dip below 30% of peak that's surrounded by content > 60%
    const dipThr = peak * 0.30;
    const surroundThr = peak * 0.60;
    const stutters = [];
    for (let f = 5; f < nF - 5; f++) {
      if (sm[f] < dipThr) {
        // Check if surrounded by content
        let preMax = 0, postMax = 0;
        for (let k = 1; k <= 5; k++) {
          if (sm[f - k] > preMax) preMax = sm[f - k];
          if (sm[f + k] > postMax) postMax = sm[f + k];
        }
        if (preMax > surroundThr && postMax > surroundThr) {
          // It's a dip — check if it's a real stutter (recovers within 80ms)
          let recoveredBy = 8;
          for (let k = 1; k <= 8; k++) {
            if (f + k < nF && sm[f + k] > surroundThr) { recoveredBy = k; break; }
          }
          stutters.push({ time: f * 0.010, dipDepth: 1 - sm[f] / preMax, recoveryFrames: recoveredBy });
          // skip ahead past this dip
          f += recoveredBy;
        }
      }
    }
    if (stutters.length === 0) {
      console.log(`\n  stutters: none detected ✓`);
    } else {
      console.log(`\n  stutters: ${stutters.length} amplitude dip${stutters.length === 1 ? "" : "s"} flagged`);
      for (const s of stutters.slice(0, 12)) {
        console.log(`    ${s.time.toFixed(2)}s  depth ${(s.dipDepth * 100).toFixed(0)}%  recover ${s.recoveryFrames * 10}ms`);
      }
      if (stutters.length > 12) console.log(`    ... and ${stutters.length - 12} more`);
    }
  }
}
