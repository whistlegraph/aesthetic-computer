#!/usr/bin/env node
// realign-from-whisper.mjs — use whisper's RAW timing on the final mp3
// to retime each storyboard slide.start to where audio actually fires.
//
// Why: pitchsnap reports snappedStart as the INTENDED placement, but
// WORLD synthesis introduces per-word timing offsets (±1.5s observed).
// Whisper detects actual audio onsets — even when its TEXT recognition
// is wrong (autotune smear: "amazing"→"Crazy", "wretch"→"turn"),
// the BOUNDARIES are at the right wall-clock positions.
//
// Approach: walk whisper words and canonical lyric words in parallel,
// skipping whisper non-word artifacts (♪ etc), and map each canonical
// word to its corresponding whisper-detected onset.
//
// Usage: node bin/realign-from-whisper.mjs --slug amazing

import { readFileSync, writeFileSync, existsSync } from "node:fs";

const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else flags[a.slice(2)] = next;
}

const SLUG = flags.slug || "amazing";
const POP  = "/Users/jas/aesthetic-computer/pop";
const SB   = flags.storyboard || `${POP}/big-pictures/out/${SLUG}.storyboard.json`;
const WORDS = flags.words    || `${POP}/big-pictures/out/${SLUG}-final-words.json`;

if (!existsSync(SB))    { console.error(`✗ storyboard missing: ${SB}`);    process.exit(1); }
if (!existsSync(WORDS)) { console.error(`✗ words missing: ${WORDS}`); process.exit(1); }

const sb = JSON.parse(readFileSync(SB, "utf8"));
const whisper = JSON.parse(readFileSync(WORDS, "utf8"));

// Filter out non-word whisper detections (musical notes, stray symbols).
const realWords = whisper.filter(w => /[a-zA-Z]/.test(w.text));
console.log(`→ ${whisper.length} whisper entries → ${realWords.length} real words`);

// Group score syllables into canonical words (a-/-ma-/-zing → 1 word).
function isWordStart(slide) {
  const r = (slide.rawText ?? slide.text ?? "").trim();
  return !r.startsWith("-");
}
const wordGroups = [];
for (let i = 0; i < sb.slides.length; i++) {
  if (isWordStart(sb.slides[i]) || wordGroups.length === 0) {
    wordGroups.push({ slideIdxs: [i] });
  } else {
    wordGroups[wordGroups.length - 1].slideIdxs.push(i);
  }
}
console.log(`→ ${sb.slides.length} slides grouped into ${wordGroups.length} canonical words`);

// Map canonical word → whisper-detected onset by ORDER (positional).
// Whisper may have more or fewer entries (mishearings produce extra
// tokens), so we use min of the two and warn on mismatch.
const n = Math.min(wordGroups.length, realWords.length);
if (wordGroups.length !== realWords.length) {
  console.warn(`⚠ count mismatch (canonical=${wordGroups.length} whisper=${realWords.length}); aligning ${n}`);
}

const total = sb.audioDuration ?? sb.duration ?? 0;
let drift_sum = 0;
for (let wi = 0; wi < n; wi++) {
  const grp = wordGroups[wi];
  const wd = realWords[wi];
  const wordStart = wd.fromMs / 1000;
  const wordEnd = wd.toMs / 1000;
  const sylN = grp.slideIdxs.length;
  for (let si = 0; si < sylN; si++) {
    const slide = sb.slides[grp.slideIdxs[si]];
    const newStart = wordStart + (wordEnd - wordStart) * (si / sylN);
    drift_sum += Math.abs(newStart - slide.start) * 1000;
    slide.start = Number(newStart.toFixed(3));
  }
}

// Recompute slide.end as next-slide's start (contiguous).
for (let i = 0; i < sb.slides.length; i++) {
  const s = sb.slides[i];
  const next = sb.slides[i + 1];
  s.end = Number((next ? next.start : total).toFixed(3));
  s.duration = Number((s.end - s.start).toFixed(3));
}

// Min-duration clamp for single-char and zero-duration slides.
const MIN_MULTI = 0.18, MIN_SINGLE = 0.55;
for (let i = 0; i < sb.slides.length; i++) {
  const s = sb.slides[i];
  const txt = (s.text ?? "").replace(/[^a-zA-Z]/g, "");
  const minS = txt.length <= 1 ? MIN_SINGLE : MIN_MULTI;
  if (s.duration >= minS) continue;
  const prev = sb.slides[i - 1];
  if (!prev) continue;
  const prevTxt = (prev.text ?? "").replace(/[^a-zA-Z]/g, "");
  const prevMin = prevTxt.length <= 1 ? MIN_SINGLE : MIN_MULTI;
  const need = minS - s.duration;
  const takeable = Math.min(need, Math.max(0, prev.duration - prevMin));
  if (takeable <= 0) continue;
  prev.end = Number((prev.end - takeable).toFixed(3));
  prev.duration = Number((prev.end - prev.start).toFixed(3));
  s.start = prev.end;
  s.duration = Number((s.end - s.start).toFixed(3));
}

console.log(`→ avg slide.start drift: ${(drift_sum / n).toFixed(0)}ms per word`);
writeFileSync(SB, JSON.stringify(sb, null, 2));
console.log(`✓ ${SB}`);
console.log(`  first 6:`);
for (const s of sb.slides.slice(0, 6)) {
  console.log(`    ${String(s.i).padStart(2)} '${s.text.padEnd(8)}' ${s.start.toFixed(2)}-${s.end.toFixed(2)}s (${s.duration.toFixed(2)}s)`);
}
