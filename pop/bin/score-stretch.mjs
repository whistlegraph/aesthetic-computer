#!/usr/bin/env node
// score-stretch.mjs — time-stretch an already-pitched vocal to the
// score's beat structure. Per-word rubberband (formant-preserving,
// no pitch shift), placed at the score's hymn-paced start times.
//
// Why this exists: score-render.mjs did pitch + stretch in one pass,
// which meant WORLD f0 replacement was running on rubberband-stretched
// audio (10-25× per word) — artifacts smeared the pitch. score-pitch.mjs
// now does WORLD on the natural-paced source (clean), and this script
// stretches the result without touching pitch — best of both.
//
// Input:
//   - pitched mp3        (already at correct pitches, source-paced)
//   - alignment.json     (per-word fromMs/toMs in source time)
//   - .np score          (per-word target beats)
//
// Output:
//   - hymn-paced mp3 with correct pitches and held notes
//
// Usage:
//   node bin/score-stretch.mjs \
//        --in big-pictures/out/amazing-7verse-pitched.mp3 \
//        --alignment big-pictures/out/amazing-7verse-pitched-alignment.json \
//        --slug amazing --section all --bpm 70 --inter-verse-beats 2 \
//        --out big-pictures/out/amazing-7verse-stretched.mp3

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
import { resolve } from "node:path";
import { tmpdir } from "node:os";

const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else flags[a.slice(2)] = next;
}

const POP = "/Users/jas/aesthetic-computer/pop";
const SLUG = flags.slug || "amazing";
const SECTION = (flags.section || "all").toLowerCase();
const BPM = Number(flags.bpm ?? 70);
const INTER_VERSE_BEATS = Number(flags["inter-verse-beats"] ?? 2);
const MAX_STRETCH = Number(flags["max-stretch"] ?? 6.0); // cap to avoid artifacts
// Whisper marks word-start at the consonant onset, but the perceived
// pitch onset is the vowel — typically 40-80ms later. Shifting the
// source-cut earlier by this amount means the consonant lands BEFORE
// the score beat and the vowel lands ON it. Negative = no shift.
const ONSET_SHIFT_MS = Number(flags["onset-shift-ms"] ?? 0);
// Crossfade overlap between adjacent words. Extends each word's source
// cut by overlap-ms/stretch on both ends (so post-stretch the clip is
// overlap-ms longer at head and tail than its target slot). Apply a
// linear afade=in / afade=out of overlap-ms at the edges, then schedule
// the clip overlap-ms earlier. Adjacent words crossfade for 2*overlap-ms
// at constant amplitude (linear) → no audible gap between sung words.
const OVERLAP_MS = Number(flags["overlap-ms"] ?? 0);

const IN_PATH = flags.in
  ? resolve(process.cwd(), flags.in)
  : `${POP}/big-pictures/out/${SLUG}-7verse-pitched.mp3`;
const ALIGN_PATH = flags.alignment
  ? resolve(process.cwd(), flags.alignment)
  : `${POP}/big-pictures/out/${SLUG}-7verse-pitched-alignment.json`;
const SCORE_PATH = flags.score
  ? resolve(process.cwd(), flags.score)
  : `${POP}/big-pictures/${SLUG}.np`;
const OUT_PATH = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${POP}/big-pictures/out/${SLUG}-7verse-stretched.mp3`;

// --words-only just needs the .np score (no audio / no alignment).
const requiredInputs = flags["words-only"] ? [SCORE_PATH] : [IN_PATH, ALIGN_PATH, SCORE_PATH];
for (const p of requiredInputs) {
  if (!existsSync(p)) { console.error(`✗ missing: ${p}`); process.exit(1); }
}

// ── parse score (mirrors score-pitch.mjs) ────────────────────────────
const scoreLines = readFileSync(SCORE_PATH, "utf8").split("\n");
const syllables = [];
function parseRange(startIdx) {
  for (let i = startIdx; i < scoreLines.length; i++) {
    const l = scoreLines[i].trim();
    if (!l) continue;
    if (l.startsWith("#")) continue;
    if (/^[a-z]+ \d+$/i.test(l)) break;
    for (const tok of l.split(/\s+/)) {
      const m = tok.match(/^([A-Ga-g][#b]?-?\d):(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
      if (!m) continue;
      syllables.push({ note: m[1], raw: m[2], weight: Number(m[3] ?? 1) });
    }
  }
}
if (SECTION === "all") {
  const headers = [];
  for (let i = 0; i < scoreLines.length; i++) {
    if (/^verse \d+$/i.test(scoreLines[i].trim())) headers.push(i);
  }
  headers.forEach((h, idx) => {
    const before = syllables.length;
    parseRange(h + 1);
    if (idx < headers.length - 1 && syllables.length > before && INTER_VERSE_BEATS > 0) {
      syllables[syllables.length - 1].weight += INTER_VERSE_BEATS;
    }
  });
} else {
  const sStart = scoreLines.findIndex((l) => l.trim().toLowerCase() === SECTION);
  if (sStart < 0) { console.error(`✗ section missing: ${SECTION}`); process.exit(1); }
  parseRange(sStart + 1);
}

// Group syllables → words. Each word's target duration = sum of its
// syllables' beat weights × beat_s. Start = cumulative beat position.
const beat_s = 60.0 / BPM;
const scoreWords = [];
let cur = null;
let beat_pos = 0;
for (const s of syllables) {
  if (s.raw.startsWith("-") && cur) {
    cur.weights.push(s.weight);
    beat_pos += s.weight;
  } else {
    if (cur) scoreWords.push(cur);
    cur = { text: s.raw.replace(/^-|-$/g, ""), weights: [s.weight],
            start_beat: beat_pos };
    beat_pos += s.weight;
  }
}
if (cur) scoreWords.push(cur);
for (const w of scoreWords) {
  w.start_s = w.start_beat * beat_s;
  w.dur_s   = w.weights.reduce((a, b) => a + b, 0) * beat_s;
}
const totalDur = beat_pos * beat_s;
console.log(`→ ${scoreWords.length} words, ${totalDur.toFixed(1)}s hymn-pace target`);

// --words-only <path>: dump the STRETCHED per-word timeline (the
// exact start/dur the audio is rubberbanded to) and exit, no audio
// render. Lets the karaoke overlay match the slow sung stem.
if (flags["words-only"]) {
  // Rebuild full word text by re-walking syllables with the SAME
  // grouping (continuation `-` syllables append), so "com-/-pu-/-ter"
  // reads "computer" not "com". Timing comes from scoreWords (the
  // exact stretched start/dur the audio is rubberbanded to).
  // Join real syllable splits (com+pu+ter→computer) but DON'T repeat
  // melisma where the same word is held across notes (love/-love/-love
  // → "love", not "lovelovelove"): only append a piece that differs
  // from the previous piece in this word.
  const fullText = [];
  let lastPiece = null;
  for (const s of syllables) {
    const piece = s.raw.replace(/^-+|-+$/g, "");
    if (s.raw.startsWith("-") && fullText.length) {
      if (piece && piece !== lastPiece) fullText[fullText.length - 1] += piece;
    } else {
      fullText.push(piece);
    }
    lastPiece = piece;
  }
  const out = scoreWords.map((w, i) => ({
    text: (fullText[i] || w.text),
    fromMs: Math.round(w.start_s * 1000),
    toMs: Math.round((w.start_s + w.dur_s) * 1000),
  }));
  const wp = resolve(process.cwd(), String(flags["words-only"]));
  writeFileSync(wp, JSON.stringify(out, null, 0));
  console.log(`✓ stretched words → ${wp} (${out.length} words, ${totalDur.toFixed(1)}s)`);
  process.exit(0);
}

// ── alignment per word ───────────────────────────────────────────────
const aligned = JSON.parse(readFileSync(ALIGN_PATH, "utf8"));
if (aligned.length !== scoreWords.length) {
  console.warn(`⚠ alignment has ${aligned.length} words, score has ${scoreWords.length}; aligning by index`);
}

// ── per-word cut + rubberband stretch ────────────────────────────────
const tmp = mkdtempSync(`${tmpdir()}/score-stretch-${SLUG}-`);
const inputs = [];
const filters = [];
let nClipped = 0;

for (let i = 0; i < Math.min(scoreWords.length, aligned.length); i++) {
  const w = scoreWords[i];
  const a = aligned[i];
  // Initial stretch ratio (without overlap, just to compute how much
  // source audio to grab for the overlap band).
  const base_src_dur = (a.toMs - Math.max(0, a.fromMs - ONSET_SHIFT_MS)) / 1000;
  if (base_src_dur <= 0.02) continue;
  const wantStretch = w.dur_s / base_src_dur;
  const stretch = Math.min(wantStretch, MAX_STRETCH);
  if (wantStretch > MAX_STRETCH) nClipped++;
  // Extend the source cut by OVERLAP_MS-worth-in-source on each side
  // (= OVERLAP_MS / stretch). Post-stretch, the head and tail each
  // gain OVERLAP_MS of audio for the crossfade band.
  const overlap_src_sec = (OVERLAP_MS / 1000) / Math.max(0.05, stretch);
  const cut_from_ms = Math.max(0, a.fromMs - ONSET_SHIFT_MS - overlap_src_sec * 1000);
  const cut_to_ms = a.toMs + overlap_src_sec * 1000;
  const src_dur = (cut_to_ms - cut_from_ms) / 1000;
  // Output duration ≈ src_dur * stretch ≈ tgt + 2 * overlap_ms/1000

  const cutWav   = `${tmp}/w${String(i).padStart(3,"0")}-cut.wav`;
  const stretchWav = `${tmp}/w${String(i).padStart(3,"0")}-out.wav`;

  spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-ss", String(cut_from_ms / 1000), "-i", IN_PATH,
    "-t", String(src_dur), "-ar", "44100", "-ac", "1",
    cutWav,
  ]);
  if (!existsSync(cutWav)) continue;

  spawnSync("rubberband", [
    "-t", String(stretch), "--formant",
    cutWav, stretchWav,
  ], { stdio: ["ignore", "ignore", "ignore"] });
  if (!existsSync(stretchWav)) continue;

  inputs.push("-i", stretchWav);
  const idx = inputs.length / 2 - 1;
  // Schedule OVERLAP_MS earlier so the word body (after fade-in) aligns
  // with the original target start.
  const delay_ms = Math.max(0, Math.round(w.start_s * 1000) - OVERLAP_MS);
  const out_dur = src_dur * stretch;
  const fade_s = OVERLAP_MS / 1000;
  const fadeIn = OVERLAP_MS > 0 ? `afade=t=in:st=0:d=${fade_s.toFixed(3)},` : "";
  const fadeOut = OVERLAP_MS > 0 ? `afade=t=out:st=${(out_dur - fade_s).toFixed(3)}:d=${fade_s.toFixed(3)},` : "";
  filters.push(`[${idx}:a]${fadeIn}${fadeOut}adelay=${delay_ms}|${delay_ms}[d${idx}]`);
  if (i % 30 === 0) {
    process.stdout.write(`  [${String(i).padStart(3)}/${scoreWords.length}] '${w.text}' src=${base_src_dur.toFixed(2)}s → tgt=${w.dur_s.toFixed(2)}s (×${stretch.toFixed(1)})\n`);
  }
}
if (nClipped > 0) {
  console.log(`  ⚠ ${nClipped} words clipped to max-stretch=${MAX_STRETCH}× (would have been longer)`);
}

// ── mix ──────────────────────────────────────────────────────────────
const sumLabels = filters.map((_, idx) => `[d${idx}]`).join("");
const filterGraph = filters.join(";") +
  `;${sumLabels}amix=inputs=${filters.length}:duration=longest:dropout_transition=0:normalize=0[out]`;

console.log(`→ mixing ${filters.length} clips into ${totalDur.toFixed(1)}s base`);
const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  ...inputs,
  "-filter_complex", filterGraph,
  "-map", "[out]",
  "-c:a", "libmp3lame", "-q:a", "3",
  OUT_PATH,
]);
rmSync(tmp, { recursive: true, force: true });
if (r.status !== 0) { console.error("✗ mix failed"); process.exit(1); }
console.log(`✓ ${OUT_PATH}`);
