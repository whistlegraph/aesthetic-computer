#!/usr/bin/env node
// storyboard.mjs — generate a hand-editable storyboard JSON for a
// big-pictures track, driven directly from the .np SCORE (not from
// the rendered audio's whisper alignment).
//
// Source of timing truth (in priority order):
//   1. <slug>.np score — per-syllable note + beat-weight. THE LAW.
//      Each `note:syllable*weight` token becomes ONE slide.
//   2. BPM (CLI flag or score comment) — converts beat positions to
//      seconds.
//   3. The mixed audio's true duration (ffprobe) — only used as a
//      sanity-check on the storyboard total + as the loop endpoint.
//
// Each syllable becomes its own slide so multi-syllable words like
// 'Amazing' (a-/ma-/zing on D3/G3/B3) get individual visual hits at
// each note transition, instead of one static word held over 3 notes.
//
// Output: <slug>.storyboard.json. tiktok.mjs reads it verbatim and
// FLUX-generates one word image per slide (so the syllable text is
// the displayed word — "a", "ma", "zing", "grace", ...).
//
// Usage:
//   node bin/storyboard.mjs --slug amazing
//     [--score big-pictures/amazing.np]
//     [--section "verse 1"]
//     [--bpm 70]
//     [--audio big-pictures/out/amazing-final.mp3]

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";

const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (a.startsWith("--")) flags[a.slice(2)] = process.argv[i + 1];
}

const SLUG = flags.slug || "amazing";
const POP = "/Users/jas/aesthetic-computer/pop";
const SCORE_PATH = flags.score
  ? resolve(process.cwd(), flags.score)
  : `${POP}/big-pictures/${SLUG}.np`;
const SECTION = (flags.section || "verse 1").toLowerCase();
const BPM = Number(flags.bpm) || 70;
const AUDIO = flags.audio
  ? resolve(process.cwd(), flags.audio)
  : `${POP}/big-pictures/out/${SLUG}-final.mp3`;
const OUT = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${POP}/big-pictures/out/${SLUG}.storyboard.json`;
const IMG_DIR = flags["img-dir"]
  ? resolve(process.cwd(), flags["img-dir"])
  : `${POP}/big-pictures/out/${SLUG}-tiktok-frames`;

if (!existsSync(SCORE_PATH)) {
  console.error(`✗ score file missing: ${SCORE_PATH}`);
  process.exit(1);
}
if (!existsSync(AUDIO)) {
  console.error(`✗ audio missing: ${AUDIO}`);
  process.exit(1);
}

// ── Parse the .np score ─────────────────────────────────────────────
// Each line in the section is space-separated `note:syllable*weight`
// tokens. Syllables can be:
//   "a-"     — start of multi-syllable word
//   "-ma-"   — middle of multi-syllable word
//   "-zing"  — end of multi-syllable word
//   "grace"  — single-syllable word
const scoreText = readFileSync(SCORE_PATH, "utf8");
const scoreLines = scoreText.split("\n");
const sectionStart = scoreLines.findIndex(
  (l) => l.trim().toLowerCase() === SECTION,
);
if (sectionStart < 0) {
  console.error(`✗ section '${SECTION}' not found in ${SCORE_PATH}`);
  process.exit(1);
}

const syllables = [];
for (let i = sectionStart + 1; i < scoreLines.length; i++) {
  const line = scoreLines[i].trim();
  if (!line) break;          // empty line ends section
  if (line.startsWith("#")) continue;
  if (/^[a-z]+ \d/i.test(line)) break;  // next section header
  for (const tok of line.split(/\s+/)) {
    // Match note:syllable[*weight] — weight is optional, defaults to 1
    // beat. Aligns with folk_backing.py's parser; otherwise scores
    // like mary.np (bare tokens, only the held final syllable carries
    // *N) collapse to a single slide instead of one-per-syllable.
    const m = tok.match(/^([A-Ga-g][#b]?-?\d):(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
    if (!m) continue;
    syllables.push({
      note: m[1],
      raw: m[2],
      weight: Number(m[3] ?? 1),
    });
  }
}

if (syllables.length === 0) {
  console.error(`✗ no syllables found under section '${SECTION}'`);
  process.exit(1);
}

// ── Probe true audio duration (sanity check only) ───────────────────
const probe = spawnSync("ffprobe", [
  "-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", AUDIO,
], { encoding: "utf8" });
const audioDur = Number(probe.stdout.trim());

// 28-color emotional arc — extended from 26 to cover the per-syllable
// expansion. Verse 1 (awakening) → verse 2 (humility) → verse 3
// (climbing) → verse 4 (revelation).
const EMOTIONAL_COLORS = [
  // Awakening / morning
  { bg: "peachpuff",      letters: "saddlebrown" },
  { bg: "moccasin",       letters: "darkred" },
  { bg: "wheat",          letters: "indigo" },
  { bg: "khaki",          letters: "darkolivegreen" },
  { bg: "palegoldenrod",  letters: "maroon" },
  { bg: "lightyellow",    letters: "darkgoldenrod" },
  { bg: "lemonchiffon",   letters: "darkslateblue" },
  { bg: "papayawhip",     letters: "saddlebrown" },
  // Humility / grounded
  { bg: "burlywood",      letters: "darkslategray" },
  { bg: "tan",            letters: "ivory" },
  { bg: "rosybrown",      letters: "white" },
  { bg: "thistle",        letters: "indigo" },
  { bg: "lavender",       letters: "darkviolet" },
  { bg: "mistyrose",      letters: "maroon" },
  { bg: "plum",           letters: "ivory" },
  // Climbing / seeking
  { bg: "skyblue",        letters: "navy" },
  { bg: "lightblue",      letters: "midnightblue" },
  { bg: "mediumturquoise", letters: "darkslategray" },
  { bg: "mediumaquamarine", letters: "ivory" },
  { bg: "lightseagreen",  letters: "lemonchiffon" },
  { bg: "palegreen",      letters: "darkgreen" },
  { bg: "lightgreen",     letters: "darkolivegreen" },
  { bg: "aquamarine",     letters: "darkslategray" },
  // Revelation / bright
  { bg: "hotpink",        letters: "white" },
  { bg: "deeppink",       letters: "lemonchiffon" },
  { bg: "violet",         letters: "white" },
  { bg: "orchid",         letters: "ivory" },
  { bg: "salmon",         letters: "white" },
  { bg: "gold",           letters: "black" },
];

const TYPOGRAPHY_STYLES = [
  "chunky pixel-art block letters, fat strokes, square pixels",
  "narrow tall pixel-art letters, condensed, square pixels",
  "wide squat pixel-art letters, low-resolution display style",
  "outlined pixel-art letters, hollow centers, single-pixel borders",
  "8-bit terminal pixel-art letters, retro arcade style",
  "thick rounded pixel-art letters, friendly chunky bitmap",
];

// ── Build slides directly from the score ─────────────────────────────
const beatSec = 60.0 / BPM;
let beatPos = 0;
const slides = syllables.map((syl, i) => {
  const start = beatPos * beatSec;
  beatPos += syl.weight;
  const end = beatPos * beatSec;
  // Strip leading/trailing hyphens for the displayed text — those
  // are score-syntax markers indicating multi-syllable continuity.
  const visible = syl.raw.replace(/^-|-$/g, "").replace(/[.,!?;:]/g, "");
  const colorIdx = i % EMOTIONAL_COLORS.length;
  const typoIdx = i % TYPOGRAPHY_STYLES.length;
  const dur = end - start;
  const transitionMs = Math.round(Math.max(120, Math.min(450, dur * 280)));
  return {
    i,
    start: Number(start.toFixed(3)),
    end: Number(end.toFixed(3)),
    duration: Number(dur.toFixed(3)),
    text: visible,
    rawText: syl.raw,
    note: syl.note,
    weight: syl.weight,
    image: `word-${String(i).padStart(3, "0")}.jpg`,
    transition: "slideleft",
    transitionMs,
    bgColor: EMOTIONAL_COLORS[colorIdx].bg,
    letterColor: EMOTIONAL_COLORS[colorIdx].letters,
    typography: TYPOGRAPHY_STYLES[typoIdx],
  };
});

const totalScoreSec = beatPos * beatSec;
const storyboard = {
  schema: "ac/big-pictures/storyboard@2",
  slug: SLUG,
  audio: AUDIO.replace(`${POP}/`, "pop/"),
  score: SCORE_PATH.replace(`${POP}/`, "pop/"),
  bpm: BPM,
  section: SECTION,
  // Use the SCORE total as the duration; audio file may have trailing
  // silence (ID3 padding) but the visual cycle is locked to the score.
  duration: Number(Math.max(totalScoreSec, audioDur).toFixed(3)),
  scoreDuration: Number(totalScoreSec.toFixed(3)),
  audioDuration: Number(audioDur.toFixed(3)),
  resolution: { w: 1080, h: 1920 },
  framerate: 30,
  imageDir: IMG_DIR.replace(`${POP}/`, "pop/"),
  defaults: {
    transition: "slideleft",
    transitionMs: 220,
    fontFamily: "/System/Library/Fonts/Supplemental/Futura.ttc",
  },
  slides,
};

writeFileSync(OUT, JSON.stringify(storyboard, null, 2));
console.log(`✓ ${OUT}`);
console.log(`  ${slides.length} slides · score=${totalScoreSec.toFixed(2)}s audio=${audioDur.toFixed(2)}s · ${BPM} BPM`);
console.log(`  first 5:`);
for (const s of slides.slice(0, 5)) {
  console.log(`    ${String(s.i).padStart(2)} '${s.text.padEnd(8)}' ${s.start.toFixed(2)}-${s.end.toFixed(2)}s (${s.duration.toFixed(2)}s × ${s.weight}b) → ${s.note}`);
}
console.log(`  last 5:`);
for (const s of slides.slice(-5)) {
  console.log(`    ${String(s.i).padStart(2)} '${s.text.padEnd(8)}' ${s.start.toFixed(2)}-${s.end.toFixed(2)}s (${s.duration.toFixed(2)}s × ${s.weight}b) → ${s.note}`);
}
