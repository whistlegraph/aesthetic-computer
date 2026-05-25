#!/usr/bin/env node
// autotune.mjs — subtle per-vowel pitch shift in recap-rap.mp3 toward
// the closest chord tone of the melody at that moment. Output:
// `recap/out/recap-rap-tuned.mp3`.
//
// Conservative by design:
//   - Only shifts vowels with detected F0 confidence > 0.6
//   - Hard-clamps shift to ±MAX_CENTS (default 200 = 2 semitones)
//   - Vowels shorter than MIN_VOWEL_MS are left alone
//   - Crossfades vowel↔consonant boundaries (~12ms) so the shift
//     transitions blend with the surrounding speech
//
// Algorithm (PSOLA, basic form):
//   1. Detect pitch period T_in via autocorrelation at vowel center
//   2. Compute T_out = T_in / ratio  (ratio = target_f / source_f)
//   3. Place 2-period Hann windows at output pitch marks every T_out,
//      each copied from the nearest source pitch mark
//
// Usage:
//   node bin/autotune.mjs <audience>
//   node bin/autotune.mjs <audience> --max-cents 150     # tighter shift cap
//   node bin/autotune.mjs <audience> --confidence 0.7    # only very clear vowels

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  } else positional.push(a);
}
const audienceName = positional[0];
if (!audienceName) { console.error("usage: autotune.mjs <audience> [--max-cents N] [--confidence 0..1] [--min-vowel-ms N]"); process.exit(2); }

const mod = await import(`${ROOT}/audience/${audienceName}.mjs`);
const audience = mod.audience || mod.default;
const melodyCfg = audience.melody || { morph: [{ progression: [["A3", "C4", "E4", "A4"]], weight: 1 }] };

const inputPath = `${ROOT}/out/recap-rap.mp3`;
const phoneticsPath = `${ROOT}/out/phonetics.json`;
const placementPath = `${ROOT}/out/recap-rap.placement.json`;
for (const p of [inputPath, phoneticsPath, placementPath]) {
  if (!existsSync(p)) { console.error(`✗ missing ${p}`); process.exit(1); }
}

const MAX_CENTS = flags["max-cents"] !== undefined ? parseFloat(flags["max-cents"]) : 200;
const MIN_CONFIDENCE = flags.confidence !== undefined ? parseFloat(flags.confidence) : 0.6;
const MIN_VOWEL_MS = flags["min-vowel-ms"] !== undefined ? parseFloat(flags["min-vowel-ms"]) : 80;
// DEFAULT: region-wide shift. Each voice region is pitch-shifted in
// its entirety by the ratio of its dominant vowel → audible, locked,
// vowels and consonants both sing on pitch. Opt-in --gradient mode
// interpolates ratios between vowels (smoother but averages out into
// near-inaudibility, kept for experimentation).
const GRADIENT_MODE = flags.gradient === true;
const REGION_MODE = !GRADIENT_MODE;

const ff = "/opt/homebrew/opt/ffmpeg-full/bin/ffmpeg";
const SR = 48000;

// ── decode recap-rap.mp3 → mono Float32 @ SR ──────────────────────────
console.log(`▸ decoding ${inputPath.replace(ROOT + "/", "")}`);
const decode = spawnSync(ff, [
  "-hide_banner", "-loglevel", "error",
  "-i", inputPath,
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-",
], { encoding: "buffer", maxBuffer: 1024 * 1024 * 256 });
if (decode.status !== 0 || !decode.stdout) {
  console.error(`✗ decode failed: ${decode.stderr?.toString().slice(0, 400)}`); process.exit(1);
}
const audio = new Float32Array(decode.stdout.buffer, decode.stdout.byteOffset, decode.stdout.byteLength / 4);

// ── load phonetics + placement ─────────────────────────────────────────
const phon = JSON.parse(readFileSync(phoneticsPath, "utf8"));
const vowels = phon.events.filter((e) => e.type === "vowel" && (e.endSec - e.startSec) * 1000 >= MIN_VOWEL_MS);
const placement = JSON.parse(readFileSync(placementPath, "utf8"));
const bpm = placement.bpm || 86;
const beatsPerBar = 4;
const beatSec = 60 / bpm;
const barSec = beatSec * beatsPerBar;
const totalSec = placement.finalDur;

console.log(`▸ ${vowels.length} vowel sustains (≥ ${MIN_VOWEL_MS}ms) from phonetics`);

// ── map vowel from SPOKEN time → RAP output time via placement ────────
// Each region is TIME-STRETCHED to its outDur (= naturalDur * stretchFactor).
// So a vowel at spokenSec inside the region maps to:
//   rapSec = region.newStart + (spokenSec - region.origStart) * stretchFactor
// Both the vowel START and END must be mapped this way so the
// stretched-vowel length in RAP time = spokenLength * stretchFactor.
function spokenToRap(spokenSec) {
  for (const r of placement.regions) {
    if (spokenSec >= r.origStart - 0.001 && spokenSec < r.origEnd + 0.001) {
      const naturalDur = Math.max(0.0001, r.origEnd - r.origStart);
      const stretch = (r.outDur || naturalDur) / naturalDur;
      const offsetInRegion = spokenSec - r.origStart;
      return r.newStart + offsetInRegion * stretch;
    }
  }
  return null;
}

// ── melody schedule (same logic as audio-preview.mjs) ─────────────────
const morph = (melodyCfg.morph && melodyCfg.morph.length)
  ? melodyCfg.morph
  : [{ label: "static", weight: 1, progression: melodyCfg.progression || [["A3", "C4", "E4", "A4"]] }];
const totalWeight = morph.reduce((a, m) => a + m.weight, 0);
const totalBars = Math.max(1, Math.ceil(totalSec / barSec));
const sectionForBar = new Array(totalBars).fill(0);
let bIdx = 0;
for (let s = 0; s < morph.length; s++) {
  const span = Math.max(1, Math.round(totalBars * morph[s].weight / totalWeight));
  const end = Math.min(totalBars, bIdx + span);
  for (let i = bIdx; i < end; i++) sectionForBar[i] = s;
  bIdx = end;
}
while (bIdx < totalBars) { sectionForBar[bIdx++] = morph.length - 1; }
const barsInSection = new Array(totalBars).fill(0);
{
  let runStart = 0;
  for (let b = 0; b < totalBars; b++) {
    if (b > 0 && sectionForBar[b] !== sectionForBar[b - 1]) runStart = b;
    barsInSection[b] = b - runStart;
  }
}

function noteToFreq(name) {
  const m = name.match(/^([A-G])(#|b)?(\d+)$/);
  if (!m) return 440;
  const base = { C:0,D:2,E:4,F:5,G:7,A:9,B:11 }[m[1]];
  const acc = m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0;
  const oct = Number(m[3]);
  const midi = 12 * (oct + 1) + base + acc;
  return 440 * Math.pow(2, (midi - 69) / 12);
}

// Returns the SPECIFIC melody note playing at the given rap-output
// time, octave-shifted to be the closest octave to `sourceFreq`. This
// makes the vocal lock to the melodic line, not just any chord tone.
function targetMelodyTone(rapTimeSec, sourceFreq) {
  const barNum = Math.floor(rapTimeSec / barSec);
  const beatIdx = Math.floor(rapTimeSec / beatSec);
  const beatInBar = beatIdx % beatsPerBar;
  const sec = morph[Math.min(sectionForBar[barNum] || 0, morph.length - 1)];
  const row = sec.progression[barsInSection[barNum] % sec.progression.length];
  const noteName = row[beatInBar % row.length];
  const m = noteName.match(/^([A-G])(#|b)?(\d+)$/);
  if (!m) return null;
  const base = { C:0,D:2,E:4,F:5,G:7,A:9,B:11 }[m[1]];
  const acc = m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0;
  const oct = Number(m[3]);
  const midi = 12 * (oct + 1) + base + acc;
  let freq = 440 * Math.pow(2, (midi - 69) / 12);
  // Octave-shift to the octave nearest the source pitch.
  while (freq > sourceFreq * Math.SQRT2) freq /= 2;
  while (freq < sourceFreq / Math.SQRT2) freq *= 2;
  return { name: noteName, freq };
}

// ── F0 detection via autocorrelation around a sample window ───────────
function detectPeriodSamples(buf, centerI, winLen = 1536) {
  const half = Math.floor(winLen / 2);
  const start = Math.max(0, centerI - half);
  const end = Math.min(buf.length, start + winLen);
  const w = buf.subarray(start, end);
  const minLag = Math.floor(SR / 400);
  const maxLag = Math.floor(SR / 80);
  let mean = 0;
  for (let i = 0; i < w.length; i++) mean += w[i];
  mean /= Math.max(1, w.length);
  let bestLag = 0, bestCorr = -Infinity;
  for (let lag = minLag; lag <= maxLag; lag++) {
    let corr = 0, na = 0, nb = 0;
    const L = w.length - lag;
    for (let i = 0; i < L; i++) {
      const a = w[i] - mean;
      const b = w[i + lag] - mean;
      corr += a * b; na += a * a; nb += b * b;
    }
    const denom = Math.sqrt(na * nb);
    const norm = denom > 0 ? corr / denom : 0;
    if (norm > bestCorr) { bestCorr = norm; bestLag = lag; }
  }
  return { period: bestLag, confidence: bestCorr };
}

// ── pitch shift via ffmpeg rubberband (formant-preserving, quality) ───
// One spawn per segment. ~30ms each on this machine. For region-wide
// shift on 56 voice regions = ~2s total.
function pitchShiftRubberband(srcI, srcLen, ratio) {
  const out = new Float32Array(srcLen);
  if (Math.abs(ratio - 1) < 1e-4) {
    for (let i = 0; i < srcLen; i++) out[i] = audio[srcI + i];
    return out;
  }
  const bytesIn = Buffer.from(audio.buffer, audio.byteOffset + srcI * 4, srcLen * 4);
  const r = spawnSync(ff, [
    "-hide_banner", "-loglevel", "error", "-y",
    "-f", "f32le", "-ar", String(SR), "-ac", "1",
    "-i", "-",
    "-af", `rubberband=pitch=${ratio.toFixed(6)}:pitchq=quality:formant=preserved`,
    "-f", "f32le", "-ar", String(SR), "-ac", "1",
    "-",
  ], { input: bytesIn, maxBuffer: 1024 * 1024 * 256 });
  if (r.status !== 0 || !r.stdout || r.stdout.length === 0) {
    for (let i = 0; i < srcLen; i++) out[i] = audio[srcI + i];
    return out;
  }
  const shifted = new Float32Array(r.stdout.buffer, r.stdout.byteOffset, r.stdout.byteLength / 4);
  // rubberband output may differ slightly in length from input due to
  // internal latency; truncate or pad to match input length.
  const copyLen = Math.min(shifted.length, srcLen);
  for (let i = 0; i < copyLen; i++) out[i] = shifted[i];
  return out;
}

// ── apply autotune ────────────────────────────────────────────────────
const FADE_MS = 12;
const fadeN = Math.floor((FADE_MS / 1000) * SR);
let shifted = 0, skippedConf = 0, skippedRange = 0, skippedNotInRap = 0;
let totalCentShift = 0;

const target = new Float32Array(audio.length);
target.set(audio);

if (GRADIENT_MODE) {
  // ── build ratio curve sampled at every vowel center ─────────────────
  const ratioCurve = [];
  for (const v of vowels) {
    const rapStart = spokenToRap(v.startSec);
    const rapEnd = spokenToRap(v.endSec);
    if (rapStart == null || rapEnd == null) continue;
    const centerI = Math.floor(((rapStart + rapEnd) / 2) * SR);
    const { period, confidence } = detectPeriodSamples(audio, centerI);
    if (confidence < MIN_CONFIDENCE) { skippedConf++; continue; }
    const sourceFreq = SR / period;
    const tone = targetMelodyTone((rapStart + rapEnd) / 2, sourceFreq);
    if (!tone) { skippedRange++; continue; }
    const cents = 1200 * Math.log2(tone.freq / sourceFreq);
    if (Math.abs(cents) > MAX_CENTS) { skippedRange++; continue; }
    ratioCurve.push({ timeSec: (rapStart + rapEnd) / 2, ratio: tone.freq / sourceFreq, cents });
  }
  if (!ratioCurve.length) {
    console.warn("⚠ gradient mode: no ratio anchors found (all vowels skipped). Falling back to passthrough.");
  }
  ratioCurve.sort((a, b) => a.timeSec - b.timeSec);
  const avgGradientCents = ratioCurve.reduce((a, r) => a + Math.abs(r.cents), 0) / Math.max(1, ratioCurve.length);
  console.log(`▸ gradient mode: ${ratioCurve.length} anchor vowels · avg ${avgGradientCents.toFixed(0)}c shift target`);

  function ratioAt(t) {
    if (!ratioCurve.length) return 1;
    if (t <= ratioCurve[0].timeSec) return ratioCurve[0].ratio;
    if (t >= ratioCurve[ratioCurve.length - 1].timeSec) return ratioCurve[ratioCurve.length - 1].ratio;
    // linear scan (small N); switch to binary if it ever matters
    for (let i = 0; i < ratioCurve.length - 1; i++) {
      const a = ratioCurve[i], b = ratioCurve[i + 1];
      if (t >= a.timeSec && t < b.timeSec) {
        const frac = (t - a.timeSec) / Math.max(1e-6, b.timeSec - a.timeSec);
        return a.ratio + (b.ratio - a.ratio) * frac;
      }
    }
    return 1;
  }

  // ── process in 200ms windows, Hann-overlap-add (50% hop) ────────────
  const CHUNK_MS = flags["chunk-ms"] !== undefined ? parseFloat(flags["chunk-ms"]) : 200;
  const CHUNK_SAMPLES = Math.max(1024, Math.floor((CHUNK_MS / 1000) * SR));
  const HOP = Math.floor(CHUNK_SAMPLES / 2);
  const weight = new Float32Array(audio.length);
  target.fill(0);                                         // overlap-add into empty buffer

  let chunks = 0;
  for (let start = 0; start + HOP < audio.length; start += HOP) {
    const end = Math.min(audio.length, start + CHUNK_SAMPLES);
    const len = end - start;
    if (len < 1024) break;
    const centerTime = (start + len / 2) / SR;
    const ratio = ratioAt(centerTime);
    const shifted = pitchShiftRubberband(start, len, ratio);
    // Hann window
    for (let k = 0; k < len; k++) {
      const w = 0.5 * (1 - Math.cos(2 * Math.PI * k / Math.max(1, len - 1)));
      target[start + k] += shifted[k] * w;
      weight[start + k] += w;
    }
    chunks++;
    if (chunks % 25 === 0) process.stdout.write(`  · ${chunks} chunks shifted\r`);
  }
  process.stdout.write(`  · ${chunks} chunks shifted  \n`);
  // Normalize by accumulated window weight
  for (let i = 0; i < audio.length; i++) {
    if (weight[i] > 0.05) target[i] /= weight[i];
    else target[i] = audio[i];          // edges (no overlap) — passthrough
  }
  shifted = chunks;
  totalCentShift = ratioCurve.reduce((a, r) => a + Math.abs(r.cents), 0);
} else if (REGION_MODE) {
  // For each voice REGION (from placement), find its dominant vowel,
  // detect F0 there, pick target tone, then shift the WHOLE region by
  // that ratio. Phrase reads as sung end-to-end (vowels AND consonants).
  console.log(`▸ region-wide mode: shifting whole voice phrases (${placement.regions.length} regions)`);
  for (let i = 0; i < placement.regions.length; i++) {
    const r = placement.regions[i];
    // Find vowels inside this region (in SPOKEN time)
    const inside = vowels.filter((v) => v.startSec >= r.origStart - 0.005 && v.startSec < r.origEnd + 0.005);
    if (!inside.length) continue;
    // Dominant vowel = longest
    inside.sort((a, b) => (b.endSec - b.startSec) - (a.endSec - a.startSec));
    const dom = inside[0];
    const domRapStart = spokenToRap(dom.startSec);
    if (domRapStart == null) { skippedNotInRap++; continue; }
    // Detect F0 from the dominant vowel position in RAP audio
    const centerI = Math.floor((domRapStart + (dom.endSec - dom.startSec) * 0.5 * (r.outDur / Math.max(0.0001, r.origEnd - r.origStart))) * SR);
    const { period, confidence } = detectPeriodSamples(audio, centerI);
    if (confidence < MIN_CONFIDENCE) { skippedConf++; continue; }
    const sourceFreq = SR / period;
    // Target = melody note at the REGION's MIDPOINT — for regions
    // spanning multiple beats, this picks whichever chord tone the
    // melody is on in the meat of the region (not just the first beat).
    const regionMid = r.newStart + (r.outDur || (r.origEnd - r.origStart)) / 2;
    const bestTone = targetMelodyTone(regionMid, sourceFreq);
    if (!bestTone) { skippedRange++; continue; }
    const cents = 1200 * Math.log2(bestTone.freq / sourceFreq);
    if (Math.abs(cents) > MAX_CENTS) { skippedRange++; continue; }
    const ratio = bestTone.freq / sourceFreq;

    const srcStartI = Math.floor(r.newStart * SR);
    const srcEndI = Math.min(audio.length, Math.floor((r.newStart + (r.outDur || (r.origEnd - r.origStart))) * SR));
    const segLen = srcEndI - srcStartI;
    if (segLen <= period * 2) continue;

    const shiftedSeg = pitchShiftRubberband(srcStartI, segLen, ratio);
    for (let k = 0; k < segLen; k++) {
      let mix = 1;
      if (k < fadeN) mix = k / fadeN;
      else if (k > segLen - fadeN) mix = Math.max(0, (segLen - k) / fadeN);
      target[srcStartI + k] = audio[srcStartI + k] * (1 - mix) + shiftedSeg[k] * mix;
    }
    shifted++;
    totalCentShift += Math.abs(cents);
    if (i % 10 === 0) process.stdout.write(`  · ${i}/${placement.regions.length} regions shifted\r`);
  }
  process.stdout.write(`  · ${placement.regions.length}/${placement.regions.length} regions shifted  \n`);
} else {
  // Per-vowel mode (original)
  for (const v of vowels) {
    const rapStart = spokenToRap(v.startSec);
    const rapEnd = spokenToRap(v.endSec);
    if (rapStart == null || rapEnd == null) { skippedNotInRap++; continue; }
    const dur = rapEnd - rapStart;
    if (dur * 1000 < MIN_VOWEL_MS) continue;
    const centerI = Math.floor((rapStart + dur * 0.5) * SR);
    const { period, confidence } = detectPeriodSamples(audio, centerI);
    if (confidence < MIN_CONFIDENCE) { skippedConf++; continue; }
    const sourceFreq = SR / period;
    const bestTone = targetMelodyTone(rapStart, sourceFreq);
    if (!bestTone) { skippedRange++; continue; }
    const cents = 1200 * Math.log2(bestTone.freq / sourceFreq);
    if (Math.abs(cents) > MAX_CENTS) { skippedRange++; continue; }
    const ratio = bestTone.freq / sourceFreq;
    const srcStartI = Math.floor(rapStart * SR);
    const srcEndI = Math.min(audio.length, Math.floor(rapEnd * SR));
    const segLen = srcEndI - srcStartI;
    if (segLen <= period * 2) continue;
    const shiftedSeg = pitchShiftRubberband(srcStartI, segLen, ratio);
    for (let k = 0; k < segLen; k++) {
      let mix = 1;
      if (k < fadeN) mix = k / fadeN;
      else if (k > segLen - fadeN) mix = Math.max(0, (segLen - k) / fadeN);
      target[srcStartI + k] = audio[srcStartI + k] * (1 - mix) + shiftedSeg[k] * mix;
    }
    shifted++;
    totalCentShift += Math.abs(cents);
  }
}

const targetCount = REGION_MODE ? placement.regions.length : vowels.length;
const avgCents = shifted > 0 ? totalCentShift / shifted : 0;
console.log(`▸ shifted ${shifted}/${targetCount} ${REGION_MODE ? "regions" : "vowels"} · avg ${avgCents.toFixed(0)} cents`);
console.log(`  skipped: ${skippedConf} low-confidence · ${skippedRange} out-of-range · ${skippedNotInRap} unmapped`);

// ── encode → recap-rap-tuned.mp3 ──────────────────────────────────────
console.log(`▸ encoding → recap/out/recap-rap-tuned.mp3`);
const enc = spawnSync(ff, [
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "1",
  "-i", "-",
  "-ac", "2",
  "-c:a", "libmp3lame", "-b:a", "192k",
  `${ROOT}/out/recap-rap-tuned.mp3`,
], { input: Buffer.from(target.buffer, target.byteOffset, target.byteLength), stdio: ["pipe", "inherit", "inherit"] });
if (enc.status !== 0) { console.error(`✗ encode failed (exit ${enc.status})`); process.exit(1); }

writeFileSync(`${ROOT}/out/recap-rap-tuned.duration.txt`, totalSec.toFixed(3) + "\n");
console.log(`✓ ${ROOT}/out/recap-rap-tuned.mp3`);
