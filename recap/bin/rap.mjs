#!/usr/bin/env node
// rap.mjs — beat-align jeffrey-pvc's spoken vocal by SLIDING speech
// continuously into place (time-stretching each voice region) instead
// of splicing words with silence between them. No internal cuts → no
// skipping. The vocal slows down or speeds up to land each voice
// region's anchor (= first strong plosive inside it) on a beat slot.
//
// Algorithm:
//   1. Voice regions come from the phonetic map (audio silences,
//      not whisper word boundaries — whisper writes 0ms gaps).
//   2. Each region's ANCHOR = first plosive inside (else region start).
//   3. Walk regions in order, snapping each anchor to the nearest beat
//      slot (subject to "never overlap the previous region" floor).
//   4. Output duration of region i = next region's newStart - this
//      region's newStart. Each region is time-stretched (formant-
//      preserving rubberband) to exactly fill its allocated slot.
//   5. Real silences between voice regions in the source are still
//      represented as silence (no stretching inside silence).
//
// Pitch stays untouched (autotune is a separate post-pass in
// bin/autotune.mjs).
//
// Usage:
//   node bin/rap.mjs <audience>
//   node bin/rap.mjs <audience> --bpm 86 --grid 8
//   node bin/rap.mjs <audience> --no-stretch    # debug: passthrough (will sound chopped)
//   node bin/rap.mjs <audience> --max-stretch 1.6   # cap per-region slowdown

import { existsSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--no-")) { flags[a.slice(2)] = false; continue; }
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  } else positional.push(a);
}
const audienceName = positional[0];
if (!audienceName) {
  console.error("usage: rap.mjs <audience> [--bpm N] [--grid 4|8|16] [--min-gap ms] [--max-stretch X] [--no-anchor] [--no-stretch]");
  process.exit(2);
}

const mod = await import(`${ROOT}/audience/${audienceName}.mjs`);
const audience = mod.audience || mod.default;

const voicePath = `${ROOT}/out/recap.mp3`;
const phoneticsPath = `${ROOT}/out/phonetics.json`;
if (!existsSync(voicePath)) { console.error(`✗ missing ${voicePath} — run tts.mjs`); process.exit(1); }
if (!existsSync(phoneticsPath)) { console.error(`✗ missing ${phoneticsPath} — run bin/phonetics.mjs first`); process.exit(1); }

const bpm = flags.bpm ? parseFloat(flags.bpm) : (audience.waltz && audience.waltz.bpm) || 86;
const grid = flags.grid ? parseInt(flags.grid, 10) : 8;
const slotSec = (60 / bpm) * (4 / grid);
const minGapSec = (flags["min-gap"] !== undefined ? parseFloat(flags["min-gap"]) : 30) / 1000;
const useAnchor = flags.anchor !== false;
const useStretch = flags.stretch !== false;
const maxStretch = flags["max-stretch"] !== undefined ? parseFloat(flags["max-stretch"]) : 1.8;

const ff = "/opt/homebrew/opt/ffmpeg-full/bin/ffmpeg";
const SR = 48000;

// ── decode recap.mp3 → mono Float32 @ SR ──────────────────────────────
console.log(`▸ decoding ${voicePath.replace(ROOT + "/", "")} → f32 mono @ ${SR} Hz`);
const decode = spawnSync(ff, [
  "-hide_banner", "-loglevel", "error",
  "-i", voicePath,
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-",
], { encoding: "buffer", maxBuffer: 1024 * 1024 * 256 });
if (decode.status !== 0 || !decode.stdout) {
  console.error(`✗ decode failed: ${decode.stderr?.toString().slice(0, 400)}`);
  process.exit(1);
}
const src = new Float32Array(decode.stdout.buffer, decode.stdout.byteOffset, decode.stdout.byteLength / 4);
const srcDur = src.length / SR;

// ── load phonetics (whole voice regions, no plosive subdivision) ──────
const phonetics = JSON.parse(readFileSync(phoneticsPath, "utf8"));
const allVoices = phonetics.events.filter((e) => e.type === "voice");
const allPlosives = phonetics.events.filter((e) => e.type === "plosive");

// Filter out tiny voice regions (< 80ms). These are usually
// silence-detection false positives (a single phoneme bracketed by
// breath); time-stretching them by 10× explodes their character.
const MIN_REGION_MS = 80;
const regions = allVoices
  .filter((v) => (v.endSec - v.startSec) * 1000 >= MIN_REGION_MS)
  .map((v) => {
    const myPlosives = allPlosives
      .filter((p) => p.startSec > v.startSec + 0.04 && p.startSec < v.endSec - 0.02)
      .sort((a, b) => a.startSec - b.startSec);
    const anchorSec = (useAnchor && myPlosives.length > 0) ? myPlosives[0].startSec : v.startSec;
    return {
      origStart: v.startSec,
      origEnd: Math.min(v.endSec, srcDur),
      anchorSec,
      anchorOffset: anchorSec - v.startSec,
      plosives: myPlosives,
    };
  });
if (!regions.length) { console.error(`✗ no voice regions`); process.exit(1); }
console.log(`▸ ${regions.length} voice regions (${allVoices.length - regions.length} tiny filtered) · ${allPlosives.length} plosives`);

// ── placement: anchor on slot, never start before earliest legal ──────
const placed = [];
let cursor = 0;
// First region — push forward so its anchor lands on the nearest slot
// (or later, never earlier than its natural origStart so we don't slam in).
{
  const r = regions[0];
  const targetAnchorOut = Math.round(r.anchorSec / slotSec) * slotSec;
  const desiredStart = targetAnchorOut - r.anchorOffset;
  const newStart = Math.max(desiredStart, r.origStart);
  placed.push({ ...r, newStart });
  cursor = newStart + (r.origEnd - r.origStart);
}
for (let i = 1; i < regions.length; i++) {
  const r = regions[i];
  const naturalGap = Math.max(0, r.origStart - regions[i - 1].origEnd);
  const targetAnchorOut = Math.round(r.anchorSec / slotSec) * slotSec;
  const desiredStart = targetAnchorOut - r.anchorOffset;
  const earliest = cursor + Math.max(minGapSec, naturalGap);
  const newStart = Math.max(desiredStart, earliest);
  placed.push({ ...r, newStart });
  cursor = newStart + (r.origEnd - r.origStart);
}

// Resolve per-region output duration. Each region's audio is
// TIME-STRETCHED to fill its allocated slot — gives the "slide" feel
// (continuous tempo curve) instead of splice + silence. Short
// inter-phrase silences (< LONG_PAUSE_THRESH) are STRETCHED THROUGH:
// the previous region's audio extends through what was breath, smoothly
// arriving at the next anchor. Longer silences (real chapter pauses)
// are preserved as silence so the listener gets to breathe.
//
// If the required stretch exceeds maxStretch (= rubberband quality
// limit), only stretch to maxStretch and let the remainder be silence
// at the end of the region. Capped so the result still sounds natural.
const LONG_PAUSE_THRESH = 0.20;     // seconds — natural breath vs real pause
for (let i = 0; i < placed.length; i++) {
  const r = placed[i];
  const naturalDur = r.origEnd - r.origStart;
  if (i + 1 < placed.length) {
    const next = placed[i + 1];
    const naturalGapBetween = Math.max(0, next.origStart - r.origEnd);
    const slotBetween = next.newStart - r.newStart;
    let silenceAfter;
    if (naturalGapBetween < LONG_PAUSE_THRESH) {
      silenceAfter = 0;                 // stretch through the micro-gap
    } else {
      silenceAfter = Math.min(slotBetween, naturalGapBetween);
    }
    let wantedOutDur = Math.max(0.001, slotBetween - silenceAfter);
    // Cap stretch — if the slot is too generous for this region, only
    // stretch as far as maxStretch lets us; remainder reads as silence.
    const maxAllowedOut = naturalDur * maxStretch;
    if (wantedOutDur > maxAllowedOut) {
      silenceAfter += (wantedOutDur - maxAllowedOut);
      wantedOutDur = maxAllowedOut;
    }
    r.outDur = wantedOutDur;
    r.silenceAfter = silenceAfter;
  } else {
    r.outDur = naturalDur;
    r.silenceAfter = 0;
  }
  r.naturalDur = naturalDur;
  r.stretchFactor = r.outDur / naturalDur;
}

const tailSec = 0.3;
const finalDur = placed[placed.length - 1].newStart + placed[placed.length - 1].outDur + tailSec;

// stats
let stretched = 0, passthrough = 0;
let minF = Infinity, maxF = 0, totalStretchedTime = 0;
for (const r of placed) {
  if (Math.abs(r.stretchFactor - 1) > 0.02) {
    stretched++;
    totalStretchedTime += r.outDur;
  } else passthrough++;
  if (r.stretchFactor < minF) minF = r.stretchFactor;
  if (r.stretchFactor > maxF) maxF = r.stretchFactor;
}

console.log(`rap (slide) · ${audienceName}`);
console.log(`  bpm=${bpm}  grid=${grid}ths  slot=${(slotSec * 1000).toFixed(1)}ms  min-gap=${Math.round(minGapSec * 1000)}ms  max-stretch=${maxStretch}`);
console.log(`  regions=${placed.length}  src=${srcDur.toFixed(2)}s  out=${finalDur.toFixed(2)}s`);
console.log(`  stretch: ${stretched} stretched · ${passthrough} passthrough · factor range ${minF.toFixed(2)}..${maxF.toFixed(2)}`);

// ── per-region stretch via ffmpeg rubberband (formant-preserving) ─────
// Spawns one ffmpeg per stretched region. ~50ms each = a few seconds
// total for typical 60-region recaps. Output is read back as raw f32
// and spliced into the target buffer.
const tmp = `${tmpdir()}/recap-rap-stretch`;
mkdirSync(tmp, { recursive: true });

function stretchRegion(srcStartI, srcEndI, factor) {
  // rubberband `tempo` is playback rate: > 1 means faster (shorter
  // output), < 1 means slower (longer output). We want output longer
  // by `factor` ⇒ tempo = 1/factor.
  const tempo = 1 / Math.min(maxStretch, Math.max(0.5, factor));
  // Cap factor so we don't blow up; if requested factor > maxStretch,
  // the region won't fully fill its slot (audio will be followed by
  // silence pad to reach the slot boundary).
  const bytesIn = Buffer.from(src.buffer, src.byteOffset + srcStartI * 4, (srcEndI - srcStartI) * 4);
  const r = spawnSync(ff, [
    "-hide_banner", "-loglevel", "error", "-y",
    "-f", "f32le", "-ar", String(SR), "-ac", "1",
    "-i", "-",
    "-af", `rubberband=tempo=${tempo.toFixed(6)}:pitchq=quality`,
    "-f", "f32le", "-ar", String(SR), "-ac", "1",
    "-",
  ], { input: bytesIn, maxBuffer: 1024 * 1024 * 256 });
  if (r.status !== 0 || !r.stdout || r.stdout.length === 0) {
    console.warn(`  ⚠ rubberband failed (status=${r.status}, bytes=${r.stdout?.length || 0}); passing through`);
    return new Float32Array(src.buffer, src.byteOffset + srcStartI * 4, srcEndI - srcStartI);
  }
  return new Float32Array(r.stdout.buffer, r.stdout.byteOffset, r.stdout.byteLength / 4);
}

// ── splice each region into target buffer ─────────────────────────────
const target = new Float32Array(Math.ceil(finalDur * SR));
const fadeMs = 5;
const fadeN = Math.floor((fadeMs / 1000) * SR);

function spliceArrayInto(arr, dstStartI) {
  const len = arr.length;
  for (let k = 0; k < len; k++) {
    const dst = dstStartI + k;
    if (dst >= target.length) break;
    let s = arr[k];
    if (k < fadeN) s *= k / fadeN;
    else if (k > len - fadeN) s *= Math.max(0, (len - k) / fadeN);
    target[dst] += s;
  }
}

let progressEvery = Math.max(1, Math.floor(placed.length / 8));
for (let i = 0; i < placed.length; i++) {
  const p = placed[i];
  const srcStartI = Math.floor(p.origStart * SR);
  const srcEndI = Math.min(src.length, Math.floor(p.origEnd * SR));
  const dstStartI = Math.floor(p.newStart * SR);
  let segment;
  if (useStretch && Math.abs(p.stretchFactor - 1) > 0.02) {
    segment = stretchRegion(srcStartI, srcEndI, p.stretchFactor);
  } else {
    segment = new Float32Array(src.buffer, src.byteOffset + srcStartI * 4, srcEndI - srcStartI);
  }
  spliceArrayInto(segment, dstStartI);
  if (i % progressEvery === 0) process.stdout.write(`  · ${i}/${placed.length} regions placed\r`);
}
process.stdout.write(`  · ${placed.length}/${placed.length} regions placed  \n`);

// ── encode target → mp3 ───────────────────────────────────────────────
console.log(`▸ encoding → recap/out/recap-rap.mp3`);
const enc = spawnSync(ff, [
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "1",
  "-i", "-",
  "-ac", "2",
  "-c:a", "libmp3lame", "-b:a", "192k",
  `${ROOT}/out/recap-rap.mp3`,
], { input: Buffer.from(target.buffer, target.byteOffset, target.byteLength), stdio: ["pipe", "inherit", "inherit"] });
if (enc.status !== 0) { console.error(`✗ encode failed (exit ${enc.status})`); process.exit(1); }

writeFileSync(`${ROOT}/out/recap-rap.duration.txt`, finalDur.toFixed(3) + "\n");

// Placement plan — bin/autotune.mjs uses this to map a vowel timestamp
// in SPOKEN time → its position in RAP output time (needed to look up
// which melody chord is playing at that vowel). Includes the stretch
// factor per region so autotune can map source→output time correctly.
writeFileSync(
  `${ROOT}/out/recap-rap.placement.json`,
  JSON.stringify({
    bpm, grid, slotSec, minGapSec, finalDur,
    regions: placed.map((p) => ({
      origStart: p.origStart, origEnd: p.origEnd, newStart: p.newStart,
      outDur: p.outDur, stretchFactor: p.stretchFactor,
      anchorSec: p.anchorSec,
    })),
  }, null, 2),
);

console.log(`✓ ${ROOT}/out/recap-rap.mp3`);
console.log(`✓ ${ROOT}/out/recap-rap.placement.json (${placed.length} regions)`);
