#!/usr/bin/env node
// mfa-align.mjs — end-of-pipeline forced alignment.
//
// PURPOSE: produce per-word timings on the FINAL (post-pitchsnap) audio
// with the correct lyric text — replacing the lossy whisper-on-distorted-
// audio path that currently misrecognizes words ("saved a wretch" →
// "same direct") because pitchsnap distorts formants.
//
// IDEAL: Montreal Forced Aligner (Kaldi-based, ~30ms phoneme accuracy)
//   `pip install montreal-forced-aligner` → installs a python wrapper
//   but the underlying _kalpy native module is conda-only on Python 3.14.
//   On this 8GB machine with no conda, MFA does not install.
//
// AENEAS FALLBACK: requires espeak + numpy build chain that fails on
//   Python 3.14 (numpy ImportError mid-build).
//
// PRAGMATIC SOLUTION (used here): "alignment by substitution".
//   Whisper's word *boundaries* on the final audio are accurate even
//   when its *transcription* is wrong (it segments correctly, just
//   mishears phonemes). We:
//     1. Load the whisper words.json (correct timings, wrong text).
//     2. Load the lyric .txt (correct text, no timings).
//     3. Run Needleman-Wunsch to align the two word sequences.
//     4. For each lyric-word, find the matched whisper-word and
//        substitute the canonical lyric text into that timing slot.
//     5. For lyric-words with no whisper match (insertions), interpolate
//        timing from neighbors.
//
//   This is what MFA would do, minus the phoneme model — and for
//   ElevenLabs-generated speech (clean signal, known lyrics) the
//   accuracy is comparable.
//
// OUTPUT: ${slug}-mfa-words.json with shape `[{text, fromMs, toMs}]`
//   matching whisper words.json so timeline.py can pick it up
//   transparently.
//
// USAGE:
//   node bin/mfa-align.mjs --audio big-pictures/out/amazing-final.mp3 \
//                          --text big-pictures/amazing.txt \
//                          [--whisper big-pictures/out/amazing-final-words.json]

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}

const AUDIO = flags.audio ? resolve(process.cwd(), flags.audio) : null;
const TEXT = flags.text ? resolve(process.cwd(), flags.text) : null;
let WHISPER = flags.whisper ? resolve(process.cwd(), flags.whisper) : null;
let OUT = flags.out ? resolve(process.cwd(), flags.out) : null;

if (!AUDIO || !TEXT) {
  console.error("usage: node bin/mfa-align.mjs --audio <final.mp3> --text <lyrics.txt> [--whisper <words.json>] [--out <path>]");
  process.exit(1);
}
if (!existsSync(AUDIO)) { console.error(`✗ audio not found: ${AUDIO}`); process.exit(1); }
if (!existsSync(TEXT))  { console.error(`✗ text not found: ${TEXT}`); process.exit(1); }

// Auto-derive whisper + out paths from audio basename.
const audioDir = dirname(AUDIO);
const audioStem = basename(AUDIO).replace(/\.[^.]+$/, "");
if (!WHISPER) {
  for (const cand of [
    `${audioDir}/${audioStem}-words.json`,
    `${audioDir}/${audioStem}.words.json`,
  ]) {
    if (existsSync(cand)) { WHISPER = cand; break; }
  }
}
if (!OUT) {
  // Match the slug by stripping common suffixes (-final, -world, etc.)
  const slug = audioStem.replace(/-(final|world|vocal|perline|mix|snapped).*$/, "");
  OUT = `${audioDir}/${slug}-mfa-words.json`;
}

// ── Lyric text → ordered list of canonical words ────────────────────
// Mirror say.mjs's lyric parser: drop section headers, blank lines, and
// hyphens; lowercase for matching; keep original casing/punctuation in
// a parallel array for the output.
const HEADER_RE = /^(hook|verse \d+|outro|bridge|chorus|intro)$/i;
const lyricLines = readFileSync(TEXT, "utf8").split("\n")
  .map((l) => l.trim())
  .filter((l) => l && !HEADER_RE.test(l));
const lyricWordsRaw = lyricLines.join(" ").split(/\s+/).filter(Boolean);
const norm = (s) => s.toLowerCase().replace(/[^a-z0-9']/g, "");
const lyricKeys = lyricWordsRaw.map(norm).filter(Boolean);

// Re-derive lyricWordsRaw to match the filter (in case some tokens
// were pure punctuation).
const lyricWords = lyricWordsRaw.filter((w) => norm(w));

console.log(`  lyric: ${lyricWords.length} words from ${TEXT}`);

// ── Whisper words (timings only — text will be substituted) ────────
let whisperWords = [];
if (WHISPER && existsSync(WHISPER)) {
  whisperWords = JSON.parse(readFileSync(WHISPER, "utf8"));
  console.log(`  whisper: ${whisperWords.length} words from ${WHISPER}`);
} else {
  console.error(`✗ no whisper words.json found near ${AUDIO}; cannot do alignment-by-substitution.`);
  console.error(`  Run pitchwords/whisper on the final.mp3 first.`);
  process.exit(1);
}

// ── Needleman-Wunsch alignment ──────────────────────────────────────
// Standard DP with substitution = -1 (different) / +2 (same after
// normalization), gap = -1. This handles:
//   - lyric word missing from whisper (whisper dropped it)   → gap in B
//   - whisper word with no lyric counterpart (hallucination) → gap in A
//   - both present but text differs (mishearing)             → match
const A = lyricKeys;
const B = whisperWords.map((w) => norm(w.text));
const M = A.length, N = B.length;

const GAP = -1;
const SUB_DIFF = -1;
const SUB_SAME = 2;
// Phoneme-leniency: if first letter matches we treat as partial credit.
// Helps "saved" pair with "same" instead of dropping.
const SUB_PARTIAL = 0;

const score = Array.from({ length: M + 1 }, () => new Int32Array(N + 1));
const trace = Array.from({ length: M + 1 }, () => new Uint8Array(N + 1)); // 0=match, 1=delA, 2=delB
for (let i = 0; i <= M; i++) { score[i][0] = i * GAP; trace[i][0] = 1; }
for (let j = 0; j <= N; j++) { score[0][j] = j * GAP; trace[0][j] = 2; }
trace[0][0] = 0;

for (let i = 1; i <= M; i++) {
  for (let j = 1; j <= N; j++) {
    const a = A[i - 1], b = B[j - 1];
    let s;
    if (a === b)                       s = SUB_SAME;
    else if (a[0] && b[0] && a[0] === b[0]) s = SUB_PARTIAL;
    else                               s = SUB_DIFF;
    const diag = score[i - 1][j - 1] + s;
    const up   = score[i - 1][j] + GAP;
    const left = score[i][j - 1] + GAP;
    if (diag >= up && diag >= left) { score[i][j] = diag; trace[i][j] = 0; }
    else if (up >= left)            { score[i][j] = up;   trace[i][j] = 1; }
    else                            { score[i][j] = left; trace[i][j] = 2; }
  }
}

// Walk back to build the alignment.
const pairs = []; // {ai, bj} where -1 = gap
let i = M, j = N;
while (i > 0 || j > 0) {
  const t = trace[i][j];
  if (i > 0 && j > 0 && t === 0) { pairs.push({ ai: i - 1, bj: j - 1 }); i--; j--; }
  else if (i > 0 && t === 1)     { pairs.push({ ai: i - 1, bj: -1 });    i--; }
  else                           { pairs.push({ ai: -1,    bj: j - 1 }); j--; }
}
pairs.reverse();

// Stats
let matches = 0, mismatches = 0, gaps = 0;
for (const p of pairs) {
  if (p.ai < 0 || p.bj < 0) gaps++;
  else if (A[p.ai] === B[p.bj]) matches++;
  else mismatches++;
}
console.log(`  align: ${matches} exact / ${mismatches} mishearings-corrected / ${gaps} gaps`);

// ── Build output: one entry per LYRIC word, with timing borrowed ─────
// from its matched whisper word (or interpolated when there's no match).
const out = new Array(lyricWords.length).fill(null);
for (const p of pairs) {
  if (p.ai < 0) continue; // whisper-only insertion: ignore
  const lyricIdx = p.ai;
  if (p.bj < 0) continue; // lyric word with no whisper match: interpolate later
  const w = whisperWords[p.bj];
  out[lyricIdx] = {
    text: lyricWords[lyricIdx],
    fromMs: w.fromMs,
    toMs: w.toMs,
  };
}

// Interpolate gaps. For a run of unmatched lyric-words, split the
// neighbor's window proportionally.
for (let li = 0; li < out.length; li++) {
  if (out[li] !== null) continue;
  // Find the previous matched anchor and the next matched anchor.
  let prevIdx = li - 1;
  while (prevIdx >= 0 && out[prevIdx] === null) prevIdx--;
  let nextIdx = li + 1;
  while (nextIdx < out.length && out[nextIdx] === null) nextIdx++;
  const prev = prevIdx >= 0 ? out[prevIdx] : null;
  const next = nextIdx < out.length ? out[nextIdx] : null;
  // Count consecutive nulls in this run.
  let runStart = li;
  while (runStart > 0 && out[runStart - 1] === null) runStart--;
  let runEnd = li;
  while (runEnd < out.length - 1 && out[runEnd + 1] === null) runEnd++;
  const runLen = runEnd - runStart + 1;
  // Window: from prev.toMs to next.fromMs, or extrapolate.
  let winStart, winEnd;
  if (prev && next) { winStart = prev.toMs; winEnd = next.fromMs; }
  else if (prev) { winStart = prev.toMs; winEnd = prev.toMs + 800 * runLen; }
  else if (next) { winEnd = next.fromMs; winStart = Math.max(0, next.fromMs - 800 * runLen); }
  else { winStart = 0; winEnd = 800 * runLen; }
  const slot = (winEnd - winStart) / runLen;
  const localIdx = li - runStart;
  out[li] = {
    text: lyricWords[li],
    fromMs: Math.round(winStart + slot * localIdx),
    toMs: Math.round(winStart + slot * (localIdx + 1)),
  };
}

// Sanity: ensure no nulls remain.
const final = out.filter((w) => w !== null);
if (final.length !== lyricWords.length) {
  console.warn(`  ⚠ ${lyricWords.length - final.length} lyric words could not be timed`);
}

writeFileSync(OUT, JSON.stringify(final, null, 2));
console.log(`✓ ${OUT} (${final.length} words)`);
console.log(`  first 5: ${final.slice(0, 5).map((w) => `${w.text}@${w.fromMs}ms`).join(" ")}`);
console.log(`  last 3:  ${final.slice(-3).map((w) => `${w.text}@${w.fromMs}ms`).join(" ")}`);
