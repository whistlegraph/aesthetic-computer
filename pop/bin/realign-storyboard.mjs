#!/usr/bin/env node
// realign-storyboard.mjs — rewrite a storyboard.json so each slide.start
// matches the ACTUAL audio onset position from MFA-aligned word data,
// instead of the rigid beat × 60/BPM grid that pitchsnap targets.
//
// Why: pitchsnap places each word's WORLD-snapped position at the score's
// beat boundary, but the audible vowel attack lands ~200-1300ms later
// (consonant onset, cross-fade ramps, formant warm-up). Slides timed to
// the score grid therefore lead the audio noticeably — words show up on
// screen before they're heard. Variable per-word, so a global shift
// can't fix it; we have to use per-word measurements.
//
// Approach: group consecutive score syllables into "words" by hyphen
// markers (a-/-ma-/-zing → 'amazing'), then index-map them onto the MFA
// word array (canonical lyric order matches). Each word's first
// syllable inherits the MFA fromMs as its slide.start; remaining
// syllables of multi-syllable words subdivide the MFA window evenly.
// slide.end = next slide's slide.start (so contiguous, no gaps).
//
// Usage:
//   node bin/realign-storyboard.mjs --slug amazing
//     [--storyboard <path>] [--mfa <path>] [--out <path>]

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";

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
const MFA  = flags.mfa        || `${POP}/big-pictures/out/${SLUG}-mfa-words.json`;
const OUT  = flags.out        || SB;

if (!existsSync(SB))  { console.error(`✗ storyboard missing: ${SB}`);  process.exit(1); }
if (!existsSync(MFA)) { console.error(`✗ mfa words missing: ${MFA}`); process.exit(1); }

const sb  = JSON.parse(readFileSync(SB, "utf8"));
const mfa = JSON.parse(readFileSync(MFA, "utf8"));

// Group slides into "words" by score-syllable hyphenation markers.
// raw text examples: "a-", "-ma-", "-zing", "grace", "lit-", "-tle"
function isWordStart(slide) {
  const r = (slide.rawText ?? slide.text ?? "").trim();
  return !r.startsWith("-");
}
const wordGroups = []; // [{slideIdxs: [...]}]
for (let i = 0; i < sb.slides.length; i++) {
  const s = sb.slides[i];
  if (isWordStart(s) || wordGroups.length === 0) {
    wordGroups.push({ slideIdxs: [i] });
  } else {
    wordGroups[wordGroups.length - 1].slideIdxs.push(i);
  }
}
console.log(`→ ${sb.slides.length} slides grouped into ${wordGroups.length} words`);
console.log(`→ ${mfa.length} mfa words to map`);

if (wordGroups.length !== mfa.length) {
  console.warn(`⚠ word count mismatch (${wordGroups.length} score vs ${mfa.length} mfa) — falling back to length-min mapping`);
}

const n = Math.min(wordGroups.length, mfa.length);
const totalDur = sb.audioDuration ?? sb.duration ?? 0;

let drift_ms_sum = 0;
let drift_count = 0;
for (let wi = 0; wi < n; wi++) {
  const grp = wordGroups[wi];
  const mw = mfa[wi];
  const wordStart = mw.fromMs / 1000;
  const wordEnd = mw.toMs / 1000;
  const sylN = grp.slideIdxs.length;
  // Sub-divide the mfa word's window evenly across its syllables.
  // This gives multi-syllable words like 'amazing' three slide.starts
  // anchored to one mfa onset.
  for (let si = 0; si < sylN; si++) {
    const slide = sb.slides[grp.slideIdxs[si]];
    const newStart = wordStart + (wordEnd - wordStart) * (si / sylN);
    const oldStart = slide.start;
    drift_ms_sum += Math.abs(newStart - oldStart) * 1000;
    drift_count++;
    slide.start = Number(newStart.toFixed(3));
  }
}

// Now rewrite slide.end to be the next slide's slide.start (contiguous).
// Last slide: end = audioDuration.
for (let i = 0; i < sb.slides.length; i++) {
  const s = sb.slides[i];
  const next = sb.slides[i + 1];
  s.end = Number((next ? next.start : totalDur).toFixed(3));
  s.duration = Number((s.end - s.start).toFixed(3));
}

// Fix too-short slides. Happens when:
//   - MFA can't separate adjacent words (e.g. 'saved a wretch' merged
//     to 'same direct', so 'a' got 0s window)
//   - Single-character words ('a', 'i') just have very short natural
//     durations even when correctly detected (~0.2-0.3s in source)
// User feedback: single-char words like 'a' need to be HELD longer,
// like score-marked held notes — they're too short to read otherwise.
// Steal time from the previous slide to expand short ones.
const MIN_SLIDE_MULTI = 0.18;  // multi-char words: brief but readable
const MIN_SLIDE_SINGLE = 0.55; // single-char words ('a', 'i'): hold like a beat
for (let i = 0; i < sb.slides.length; i++) {
  const s = sb.slides[i];
  const txt = (s.text ?? "").replace(/[^a-zA-Z]/g, "");
  const minS = txt.length <= 1 ? MIN_SLIDE_SINGLE : MIN_SLIDE_MULTI;
  if (s.duration >= minS) continue;
  const prev = sb.slides[i - 1];
  if (!prev) continue;
  const need = minS - s.duration;
  // Don't steal more than what leaves prev with its own minimum.
  const prevMin = (prev.text ?? "").replace(/[^a-zA-Z]/g, "").length <= 1 ? MIN_SLIDE_SINGLE : MIN_SLIDE_MULTI;
  const takeable = Math.min(need, Math.max(0, prev.duration - prevMin));
  if (takeable <= 0) continue;
  prev.end = Number((prev.end - takeable).toFixed(3));
  prev.duration = Number((prev.end - prev.start).toFixed(3));
  s.start = prev.end;
  s.duration = Number((s.end - s.start).toFixed(3));
  console.log(`  → expanded slide ${i} '${s.text}' to ${minS.toFixed(2)}s by stealing from prev`);
}

const avgDrift = drift_ms_sum / Math.max(1, drift_count);
console.log(`→ avg drift correction: ${avgDrift.toFixed(0)}ms per slide`);

writeFileSync(OUT, JSON.stringify(sb, null, 2));
console.log(`✓ ${OUT}`);
console.log(`  first 5 slides:`);
for (const s of sb.slides.slice(0, 5)) {
  console.log(`    ${String(s.i).padStart(2)} '${s.text.padEnd(8)}' ${s.start.toFixed(2)}-${s.end.toFixed(2)}s (${s.duration.toFixed(2)}s)`);
}
