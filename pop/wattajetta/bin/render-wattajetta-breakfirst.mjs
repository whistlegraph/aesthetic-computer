#!/usr/bin/env node
// Rebegin Wattajetta at the musical state heard at 1:24 in the accepted
// original master. This is an arrangement of that state, not a trim: long
// phrases keep the groove legible while increasingly hard half-/quarter-beat
// edits turn the steel drop into a crunchier, slicier second composition.

import { mkdirSync, readFileSync, writeFileSync, unlinkSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
mkdirSync(OUT, { recursive: true });

const SR = 48000;
const BPM = 138;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const BARS = 96;
const SOURCE_START = 84; // user's chosen musical downbeat in the original master
const SPATIAL = process.argv.includes("--spatial");
const sourceMp3 = resolve(OUT, "wattajetta.mp3");
const decoded = resolve(OUT, ".wattajetta-breakfirst.source.f32");
const mixed = resolve(OUT, ".wattajetta-breakfirst.mixed.f32");
const output = resolve(OUT, SPATIAL ? "wattajetta-breakfirst-spatial.mp3"
                                  : "wattajetta-breakfirst.mp3");

const decode = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-i", sourceMp3, "-f", "f32le", "-ar", String(SR), "-ac", "2", decoded],
  { stdio: "inherit" });
if (decode.status !== 0) process.exit(decode.status ?? 1);

const bytes = readFileSync(decoded);
const src = new Float32Array(bytes.buffer, bytes.byteOffset, bytes.byteLength / 4);
const sourceFrames = src.length / 2;
const frames = Math.floor(BARS * BAR * SR);
const out = new Float32Array(frames * 2);
const frame = (seconds) => Math.floor(seconds * SR);
const beatFrame = frame(BEAT);
const barFrame = frame(BAR);
const chosenFrame = frame(SOURCE_START);

// Short equal-power edges keep a slice percussive without digital clicks.
function copySlice(dst, source, length, gain = 1, reverse = false, edgeMs = 7) {
  const n = Math.min(length, frames - dst, reverse ? source + 1 : sourceFrames - source);
  const edge = Math.max(1, frame(edgeMs / 1000));
  for (let i = 0; i < n; i++) {
    const si = reverse ? source - i : source + i;
    if (si < 0 || si >= sourceFrames) break;
    const e = Math.min(1, i / edge, (n - 1 - i) / edge);
    const g = gain * Math.sin(Math.max(0, e) * Math.PI * 0.5);
    out[2 * (dst + i)] += src[2 * si] * g;
    out[2 * (dst + i) + 1] += src[2 * si + 1] * g;
  }
}

function straight(dstBar, bars, sourceSec) {
  copySlice(dstBar * barFrame, frame(sourceSec), bars * barFrame, 1, false, 2);
}

// Each bar still states one recognizable source bar. Only selected cells are
// displaced, repeated or reversed; the unfilled cells are intentional air.
const HALF_ORDER = [[0, 1, 3, 2, 4, 5, 6, 7], [0, 2, 1, 3, 4, 6, 5, 7]];
function halfBeatPhrase(dstBar, bars, sourceSec, sparse = false) {
  for (let b = 0; b < bars; b++) {
    const order = HALF_ORDER[b % HALF_ORDER.length];
    for (let cell = 0; cell < 8; cell++) {
      if (sparse && ((b + cell) % 5 === 3 || cell === 6)) continue;
      const sourceCell = order[cell];
      const dst = (dstBar + b) * barFrame + Math.floor(cell * beatFrame / 2);
      const from = frame(sourceSec) + b * barFrame + Math.floor(sourceCell * beatFrame / 2);
      copySlice(dst, from, Math.floor(beatFrame / 2), cell === 7 ? 1.08 : 1,
                sparse && cell === 5 && b % 2 === 1, 5);
    }
  }
}

function quarterBeatPhrase(dstBar, bars, sourceSec) {
  const cellFrames = Math.floor(beatFrame / 4);
  for (let b = 0; b < bars; b++) {
    for (let cell = 0; cell < 16; cell++) {
      // Kick/downbeat cells remain fixed. Three cells per bar become negative
      // space; the remaining edits make a single spicy syncopated answer.
      if ([3, 10, 14].includes(cell)) continue;
      let sourceCell = cell;
      if (cell === 5 || cell === 9) sourceCell -= 1;
      if (cell === 7 || cell === 13) sourceCell += 1;
      const dst = (dstBar + b) * barFrame + cell * cellFrames;
      const from = frame(sourceSec) + b * barFrame + sourceCell * cellFrames;
      copySlice(dst, from, cellFrames, cell % 4 === 0 ? 1.08 : 0.96,
                cell === 13 && b % 2 === 1, 3);
    }
  }
}

// Form: recognition → first cuts → forward travel → air → harder rebegin →
// deepest forward state → knife-work → release. Every return starts on the
// exact 84-second downbeat, so the user's chosen moment is the composition's
// tonic as well as its first sound.
straight(0, 16, SOURCE_START);          // actual 1:24 state, uninterrupted
halfBeatPhrase(16, 8, SOURCE_START);    // first controlled re-cut
straight(24, 16, SOURCE_START + 16 * BAR);
halfBeatPhrase(40, 8, SOURCE_START + 8 * BAR, true);
straight(48, 8, SOURCE_START);          // hard second rebegin
quarterBeatPhrase(56, 8, SOURCE_START + 8 * BAR);
straight(64, 16, SOURCE_START + 24 * BAR);
quarterBeatPhrase(80, 8, SOURCE_START + 28 * BAR);
straight(88, 8, SOURCE_START + 40 * BAR);

// Parallel industrial bite. It is rhythmically keyed to the existing bars:
// no new free-running voice, just transient emphasis and crunchy material
// contrast. Later acts get more bite while breaths retain their holes.
let heldL = 0, heldR = 0;
const hold = 4;
for (let f = 0; f < frames; f++) {
  const b = Math.floor(f / barFrame);
  const phase = (f % beatFrame) / beatFrame;
  if (f % hold === 0) { heldL = out[2 * f]; heldR = out[2 * f + 1]; }
  const bite = b < 16 ? 0.04 : b < 48 ? 0.10 : b < 80 ? 0.16 : 0.20;
  const transient = Math.exp(-phase / 0.055);
  const drive = 1.8 + 1.2 * transient;
  const crushedL = Math.tanh(heldL * drive) / Math.tanh(drive);
  const crushedR = Math.tanh(heldR * drive) / Math.tanh(drive);
  out[2 * f] = out[2 * f] * (1 - bite) + crushedL * bite * (1 + 0.16 * transient);
  out[2 * f + 1] = out[2 * f + 1] * (1 - bite) + crushedR * bite * (1 + 0.16 * transient);
}

// A coherent spatial stage for the separate audition. Wattajetta arrives as a
// stereo master, so the crossover is deliberately conservative: everything
// below 155 Hz is mono and un-delayed, and sharp drum transients pull the upper
// body toward center. Material chapters own stable anchors. Only selected edit
// passages describe one smooth arc; azimuth is continuously slewed, never
// changed at a block or slice boundary.
if (SPATIAL) {
  const dry = out.slice();
  const staged = new Float32Array(out.length);
  const delaySize = 128;
  const monoDelay = new Float32Array(delaySize);
  let delayWrite = 0, lowL = 0, lowR = 0, az = -0.26, depth = 0.16;
  let env = 0, previousMono = 0;
  const lowA = 1 - Math.exp(-2 * Math.PI * 155 / SR);
  const azSlew = 1 - Math.exp(-1 / (0.28 * SR));
  const depthSlew = 1 - Math.exp(-1 / (0.65 * SR));
  for (let f = 0; f < frames; f++) {
    const bFloat = f / barFrame;
    const b = Math.floor(bFloat);
    const barPhase = bFloat - b;
    // Steel is left/high and near; the later stone body is right/lower and
    // deeper. The hard rebegin returns to steel's known location.
    let azTarget = b < 24 ? -0.26 : b < 48 ? 0.24 : b < 64 ? -0.18 : 0.28;
    let depthTarget = b < 24 ? 0.14 : b < 48 ? 0.22 : b < 64 ? 0.16 : 0.25;
    const edited = (b >= 16 && b < 24) || (b >= 40 && b < 48)
                || (b >= 56 && b < 64) || (b >= 80 && b < 88);
    if (edited) {
      // One deliberate bar-long glide, alternating direction. It crosses no
      // more than ~24 degrees and returns to the chapter's material anchor.
      const arc = Math.sin(Math.PI * barPhase) * (b % 2 ? -1 : 1);
      azTarget += 0.16 * arc;
      depthTarget += 0.035 * Math.sin(Math.PI * barPhase);
    }
    az += (azTarget - az) * azSlew;
    depth += (depthTarget - depth) * depthSlew;

    const l = dry[2 * f], r = dry[2 * f + 1];
    lowL += lowA * (l - lowL); lowR += lowA * (r - lowR);
    const lowMono = 0.5 * (lowL + lowR);
    const highL = l - lowL, highR = r - lowR;
    const highMono = 0.5 * (highL + highR);
    const originalSide = 0.5 * (highL - highR);

    // Broadband derivative is only a transient detector. Its smoothed value
    // centers attacks; it never gates audio or changes delay discontinuously.
    const derivative = Math.abs(highMono - previousMono);
    previousMono = highMono;
    env += (derivative - env) * (derivative > env ? 0.12 : 0.0018);
    const centerAttack = Math.min(1, env * 15);
    const effectiveAz = az * (1 - 0.72 * centerAttack);
    const itd = Math.abs(effectiveAz) * 0.00056 * SR;
    monoDelay[delayWrite] = highMono;
    const delayedIndex = (delayWrite - Math.floor(itd) + delaySize) % delaySize;
    const delayed = monoDelay[delayedIndex];
    delayWrite = (delayWrite + 1) % delaySize;
    const near = highMono;
    const far = delayed * (0.50 - 0.22 * Math.abs(effectiveAz));
    const placedL = effectiveAz < 0 ? near : far;
    const placedR = effectiveAz < 0 ? far : near;
    const spatialWet = (0.57 + depth * 0.46) * (1 - 0.55 * centerAttack);
    // Retain the accepted stereo detail underneath the listener-relative body.
    staged[2 * f] = lowMono + highL * (1 - spatialWet)
                  + (placedL + originalSide * 0.42) * spatialWet;
    staged[2 * f + 1] = lowMono + highR * (1 - spatialWet)
                      + (placedR - originalSide * 0.42) * spatialWet;
  }

  // Four low-level, cross-room early images, following the first-party room
  // pattern. Prime-ish delays prevent a metallic comb; returns are high-passed
  // by subtraction of a 190 Hz one-pole so the mono low end never enters them.
  const roomSource = staged.slice();
  const delays = [0.011, 0.019, 0.031, 0.047].map((s) => frame(s));
  const gains = [0.036, 0.025, 0.017, 0.011];
  const roomHigh = new Float32Array(staged.length);
  let roomLowL = 0, roomLowR = 0;
  const roomA = 1 - Math.exp(-2 * Math.PI * 190 / SR);
  for (let f = 0; f < frames; f++) {
    roomLowL += roomA * (roomSource[2 * f] - roomLowL);
    roomLowR += roomA * (roomSource[2 * f + 1] - roomLowR);
    roomHigh[2 * f] = roomSource[2 * f] - roomLowL;
    roomHigh[2 * f + 1] = roomSource[2 * f + 1] - roomLowR;
  }
  for (let f = 0; f < frames; f++) {
    for (let q = 0; q < delays.length; q++) {
      const from = f - delays[q];
      if (from < 0) continue;
      // Cross returns imply a room wall while remaining too quiet to smear cuts.
      const reflectedL = roomHigh[2 * from];
      const reflectedR = roomHigh[2 * from + 1];
      staged[2 * f] += reflectedR * gains[q];
      staged[2 * f + 1] += reflectedL * gains[q];
    }
  }
  out.set(staged);
}

// Preserve the chosen source frame at time zero, then taper only the last four
// bars. Re-peak before mastering.
for (let f = frames - 4 * barFrame; f < frames; f++) {
  const p = (f - (frames - 4 * barFrame)) / (4 * barFrame);
  const g = Math.cos(p * Math.PI * 0.5) ** 2;
  out[2 * f] *= g; out[2 * f + 1] *= g;
}
let peak = 0;
for (const x of out) peak = Math.max(peak, Math.abs(x));
if (peak > 0.94) for (let i = 0; i < out.length; i++) out[i] *= 0.94 / peak;

writeFileSync(mixed, Buffer.from(out.buffer, out.byteOffset, out.byteLength));
const master = [
  "highpass=f=25",
  "equalizer=f=260:t=q:w=1.0:g=-1.4",
  "equalizer=f=2400:t=q:w=0.8:g=1.1",
  "acompressor=threshold=-18dB:ratio=2.4:attack=7:release=115:makeup=1.5:knee=5",
  "alimiter=limit=0.94:attack=3:release=65",
  "loudnorm=I=-12.5:LRA=8:TP=-1.0",
];
const encode = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", mixed,
  "-af", master.join(","), "-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", `title=wattajetta — breakfirst${SPATIAL ? " spatial" : ""}`,
  "-metadata", "album=pixsies", output],
  { stdio: "inherit" });
for (const path of [decoded, mixed]) { try { unlinkSync(path); } catch {} }
if (encode.status !== 0) process.exit(encode.status ?? 1);
console.log(`✓ ${output} (84.000s becomes bar zero · 96 bars · crunchy controlled slices${SPATIAL ? " · coherent spatial stage" : ""})`);
