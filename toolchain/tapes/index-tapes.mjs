#!/usr/bin/env node
// index-tapes.mjs — build a queryable audio-property index of every posted
// tape, so pieces and jam tooling can pick tapes musically ("give me a
// bright, busy tape near 120 BPM that fits the bar grid").
//
//   node toolchain/tapes/index-tapes.mjs             # index all tapes
//   node toolchain/tapes/index-tapes.mjs --limit 10  # first N only
//   node toolchain/tapes/index-tapes.mjs --code udd  # one tape
//
// Per tape: duration, loudness (rms/peak dBFS), silence ratio, onset
// density, zero-crossing brightness, estimated BPM (onset-envelope
// autocorrelation, 60–180) with confidence, and how the length sits on
// the 120 BPM bar grid (bars + drift ms) — the jam's grid currency.
//
// Output: system/public/aesthetic.computer/tape-index.json (live-served →
// queryable at aesthetic.computer/aesthetic.computer/tape-index.json).
// Media downloads cache in toolchain/tapes/.tape-cache/ so re-runs only
// analyze new tapes; --force reanalyzes everything.

import { execFileSync, spawnSync } from "node:child_process";
import { mkdirSync, existsSync, readFileSync, writeFileSync } from "node:fs";
import { join, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const __dir = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(__dir, "..", "..");
const CACHE = join(__dir, ".tape-cache");
const OUT = join(REPO, "system", "public", "aesthetic.computer", "tape-index.json");
const API = "https://aesthetic.computer/api/tv/tapes?types=tape&limit=200";

const args = process.argv.slice(2);
const val = (f, d) => {
  const i = args.indexOf(f);
  return i >= 0 && args[i + 1] ? args[i + 1] : d;
};
const LIMIT = parseInt(val("--limit", "0"), 10) || 0;
const ONLY = val("--code", null);
const FORCE = args.includes("--force");

const SR = 22050; // Analysis sample rate (mono)
const HOP = 512; // Envelope hop (≈23ms)
const BAR_MS = 2000; // 120 BPM bar

mkdirSync(CACHE, { recursive: true });

// ---- audio analysis -------------------------------------------------------

function analyzePCM(f32) {
  const n = f32.length;
  if (!n) return null;

  // Loudness + silence + zero-crossing brightness.
  let sumSq = 0;
  let peak = 0;
  let zc = 0;
  for (let i = 0; i < n; i++) {
    const v = f32[i];
    sumSq += v * v;
    const a = Math.abs(v);
    if (a > peak) peak = a;
    if (i > 0 && v * f32[i - 1] < 0) zc++;
  }
  const rms = Math.sqrt(sumSq / n);
  const db = (x) => (x > 0 ? +(20 * Math.log10(x)).toFixed(1) : -120);

  // Frame energy envelope.
  const frames = Math.floor(n / HOP);
  const env = new Float64Array(frames);
  let silent = 0;
  for (let f = 0; f < frames; f++) {
    let e = 0;
    const o = f * HOP;
    for (let i = 0; i < HOP; i++) e += f32[o + i] * f32[o + i];
    env[f] = Math.sqrt(e / HOP);
    if (env[f] < 0.003) silent++; // ≈ −50 dBFS
  }

  // Onset strength: positive energy flux.
  const flux = new Float64Array(frames);
  for (let f = 1; f < frames; f++) flux[f] = Math.max(0, env[f] - env[f - 1]);
  let mean = 0;
  for (const v of flux) mean += v;
  mean /= frames || 1;
  let sd = 0;
  for (const v of flux) sd += (v - mean) * (v - mean);
  sd = Math.sqrt(sd / (frames || 1));
  let onsets = 0;
  let last = -10;
  for (let f = 0; f < frames; f++) {
    if (flux[f] > mean + 1.5 * sd && f - last > 4) {
      onsets++;
      last = f;
    }
  }
  const durS = n / SR;

  // BPM: autocorrelate the flux envelope across 60–180 BPM lags.
  const hopS = HOP / SR;
  let bestBpm = 0;
  let bestScore = 0;
  let totalScore = 0;
  for (let bpm = 60; bpm <= 180; bpm++) {
    const lag = Math.round(60 / bpm / hopS);
    if (lag < 2 || lag >= frames / 2) continue;
    let s = 0;
    let c = 0;
    for (let f = 0; f + lag < frames; f++) {
      s += flux[f] * flux[f + lag];
      c++;
    }
    s /= c || 1;
    totalScore += s;
    if (s > bestScore) {
      bestScore = s;
      bestBpm = bpm;
    }
  }
  const bpmConfidence = totalScore
    ? +((bestScore / (totalScore / 121)) - 1).toFixed(2) // vs. flat spectrum
    : 0;

  return {
    durationS: +durS.toFixed(3),
    rmsDb: db(rms),
    peakDb: db(peak),
    silenceRatio: +(silent / (frames || 1)).toFixed(3),
    onsetsPerSec: +(onsets / durS).toFixed(2),
    brightnessZcr: +(zc / n).toFixed(4), // 0..~0.5, higher = brighter
    bpm: bestBpm || null,
    bpmConfidence: Math.max(0, bpmConfidence),
  };
}

// ---- pipeline -------------------------------------------------------------

async function main() {
  const res = await fetch(API);
  const data = await res.json();
  let tapes = data.media?.tapes || [];
  if (ONLY) tapes = tapes.filter((t) => t.code === ONLY);
  if (LIMIT) tapes = tapes.slice(0, LIMIT);
  console.log(`🗂️  indexing ${tapes.length} tape(s)`);

  let index = { generatedAt: null, tapes: {} };
  if (existsSync(OUT) && !FORCE) {
    try {
      index = JSON.parse(readFileSync(OUT, "utf8"));
    } catch {}
  }

  for (const t of tapes) {
    if (!FORCE && index.tapes[t.code]?.audio) {
      console.log(`   ${t.code} — cached`);
      continue;
    }
    const url = t.media?.videoUrl;
    if (!url) {
      console.log(`   ${t.code} — no media url, skipped`);
      continue;
    }
    const mp4 = join(CACHE, `${t.code}.mp4`);
    try {
      if (!existsSync(mp4)) {
        execFileSync("curl", ["-sfL", "-o", mp4, url], { timeout: 120000 });
      }
      // Decode to mono float PCM on stdout.
      const dec = spawnSync(
        "ffmpeg",
        ["-v", "error", "-i", mp4, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"],
        { maxBuffer: 1024 * 1024 * 512, timeout: 120000 },
      );
      if (dec.status !== 0 || !dec.stdout?.length) {
        console.log(`   ${t.code} — decode failed (${dec.stderr?.toString().slice(0, 80)})`);
        continue;
      }
      const f32 = new Float32Array(
        dec.stdout.buffer,
        dec.stdout.byteOffset,
        Math.floor(dec.stdout.length / 4),
      );
      const audio = analyzePCM(f32);
      if (!audio) {
        console.log(`   ${t.code} — empty audio`);
        continue;
      }
      // Grid fit: how the tape length sits on the 120 BPM bar grid.
      const durMs = audio.durationS * 1000;
      const bars = Math.max(1, Math.round(durMs / BAR_MS));
      audio.grid120 = {
        bars,
        offMs: Math.round(durMs - bars * BAR_MS),
        fits: Math.abs(durMs - bars * BAR_MS) < 100,
      };
      index.tapes[t.code] = {
        code: t.code,
        owner: t.owner?.handle || null,
        when: t.when,
        acUrl: t.acUrl || `https://aesthetic.computer/!${t.code}`,
        videoUrl: url,
        audio,
      };
      console.log(
        `   ${t.code} — ${audio.durationS}s  ${audio.bpm}bpm(${audio.bpmConfidence})  ` +
          `${audio.rmsDb}dB  onsets/s ${audio.onsetsPerSec}  ` +
          `${audio.grid120.fits ? "fits" : "off"} ${audio.grid120.bars} bars (${audio.grid120.offMs}ms)`,
      );
    } catch (e) {
      console.log(`   ${t.code} — failed: ${e.message.slice(0, 80)}`);
    }
  }

  index.generatedAt = new Date().toISOString();
  writeFileSync(OUT, JSON.stringify(index, null, 2) + "\n");
  console.log(`\n🗂️  wrote ${Object.keys(index.tapes).length} entries → ${OUT}`);
}

main().catch((e) => {
  console.error("index-tapes failed:", e);
  process.exit(1);
});
