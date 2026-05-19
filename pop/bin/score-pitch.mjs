#!/usr/bin/env node
// score-pitch.mjs — pitch-snap a natural-paced vocal to a .np score
// in ONE WORLD pass (no time-stretching).
//
// Why this exists: score-render.mjs slices the source per-word and
// rubberband-stretches each clip to the score's hymn-paced beats.
// When source words are 0.2-0.5s and target durs are 4-6s, stretches
// reach 10-25× and the time-stretch artifacts swamp the pitch
// replacement — listener hears smear, not melody.
//
// This script keeps the source timing intact: it asks WORLD to apply
// a target-f0 curve across the whole vocal, anchored to whisper word
// start times. The output is the same length as the input, but every
// voiced frame sings the score's pentatonic note. Jeffrey's prosody
// + ElevenLabs character preserved.
//
// Usage:
//   node bin/score-pitch.mjs \
//        --slug amazing --section all \
//        --vocal big-pictures/out/amazing-7verse-vocal.mp3 \
//        --words big-pictures/out/amazing-7verse-vocal-words.json \
//        --transpose -5 \
//        --out big-pictures/out/amazing-7verse-pitched.mp3

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
import { resolve } from "node:path";
import { tmpdir } from "node:os";
import { alignWords } from "./align-words.mjs";

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
const TRANSPOSE = Number(flags.transpose ?? -5);
const VOCAL_PATH = flags.vocal
  ? resolve(process.cwd(), flags.vocal)
  : `${POP}/big-pictures/out/${SLUG}-7verse-vocal.mp3`;
const WORDS_PATH = flags.words
  ? resolve(process.cwd(), flags.words)
  : `${POP}/big-pictures/out/${SLUG}-7verse-vocal-words.json`;
const SCORE_PATH = flags.score
  ? resolve(process.cwd(), flags.score)
  : `${POP}/big-pictures/${SLUG}.np`;
const OUT_PATH = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${POP}/big-pictures/out/${SLUG}-7verse-pitched.mp3`;

if (!existsSync(VOCAL_PATH)) { console.error(`✗ vocal missing: ${VOCAL_PATH}`); process.exit(1); }
if (!existsSync(WORDS_PATH)) { console.error(`✗ words missing: ${WORDS_PATH}`); process.exit(1); }
if (!existsSync(SCORE_PATH)) { console.error(`✗ score missing: ${SCORE_PATH}`); process.exit(1); }

// ── parse score (mirrors score-render.mjs --section all path) ────────
const NOTE_BASE = { C:0,"C#":1,DB:1,D:2,"D#":3,EB:3,E:4,F:5,"F#":6,
                    GB:6,G:7,"G#":8,AB:8,A:9,"A#":10,BB:10,B:11 };
function noteToMidi(s) {
  s = s.toUpperCase();
  return 12 * (parseInt(s.slice(-1), 10) + 1) + NOTE_BASE[s.slice(0, -1)];
}
function midiToNote(m) {
  const oct = Math.floor(m / 12) - 1;
  const n = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"][m % 12];
  return `${n}${oct}`;
}

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
  headers.forEach((h) => parseRange(h + 1));
} else {
  const sStart = scoreLines.findIndex((l) => l.trim().toLowerCase() === SECTION);
  if (sStart < 0) { console.error(`✗ section missing: ${SECTION}`); process.exit(1); }
  parseRange(sStart + 1);
}

// Group syllables → words (hyphenated chains merge). Each word keeps
// its full per-syllable note + weight list so we can drive a melisma:
// "amazing" (D3,G3,B3) hits all three notes within the word window,
// not just D3 the whole way.
const scoreWords = [];
let cur = null;
for (const s of syllables) {
  if (s.raw.startsWith("-") && cur) {
    cur.notes.push(s.note);
    cur.weights.push(s.weight);
  } else {
    if (cur) scoreWords.push(cur);
    cur = {
      text: s.raw.replace(/^-|-$/g, "").toLowerCase(),
      notes: [s.note],
      weights: [s.weight],
    };
  }
}
if (cur) scoreWords.push(cur);
const totalSyl = scoreWords.reduce((n, w) => n + w.notes.length, 0);
console.log(`→ score: ${scoreWords.length} words across ${totalSyl} syllables`);

// ── load whisper words + run alignment ───────────────────────────────
const whisper = JSON.parse(readFileSync(WORDS_PATH, "utf8"));
console.log(`→ whisper: ${whisper.length} words spanning ${(whisper[whisper.length-1].toMs/1000).toFixed(1)}s`);

const aligned = alignWords(scoreWords.map((w) => w.text), whisper);
const matched = aligned.filter((a) => a).length;
console.log(`→ aligner: ${matched}/${scoreWords.length} score words placed in source time`);

// Build per-SYLLABLE notes + starts. For each whisper-aligned word
// window [fromMs, toMs], distribute its syllables proportional to
// score weights so multi-note words sing the actual melisma.
const notes = [];
const starts = [];
for (let i = 0; i < scoreWords.length; i++) {
  const w = scoreWords[i];
  const a = aligned[i];
  const window_ms = Math.max(40, a.toMs - a.fromMs);
  const totalWeight = w.weights.reduce((x, y) => x + y, 0);
  let cum = 0;
  for (let s = 0; s < w.notes.length; s++) {
    const tStart = a.fromMs + (cum / totalWeight) * window_ms;
    notes.push(midiToNote(noteToMidi(w.notes[s]) + TRANSPOSE));
    starts.push((tStart / 1000).toFixed(3));
    cum += w.weights[s];
  }
}

// ── decode mp3 → wav, run WORLD, encode wav → mp3 ────────────────────
const tmp = mkdtempSync(`${tmpdir()}/score-pitch-${SLUG}-`);
const inWav = `${tmp}/in.wav`;
const outWav = `${tmp}/out.wav`;

console.log(`→ decode mp3 → wav`);
let r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", VOCAL_PATH, "-ar", "44100", "-ac", "1", inWav,
]);
if (r.status !== 0) { console.error("✗ ffmpeg decode failed"); process.exit(1); }

console.log(`→ WORLD f0 replacement · ${notes.length} notes (transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st)`);
console.log(`  first 6 notes: ${notes.slice(0, 6).join(", ")}`);
console.log(`  first 6 starts: ${starts.slice(0, 6).join("s, ")}s`);
const XFADE_MS = String(flags["xfade-ms"] ?? 30);
const VOICING_RAMP_MS = String(flags["voicing-ramp-ms"] ?? 20);
const VIBRATO_HZ = String(flags["vibrato-hz"] ?? 0);
const VIBRATO_CENTS = String(flags["vibrato-cents"] ?? 0);
const RETAIN = String(flags["retain"] ?? 1.0);
r = spawnSync(`${POP}/.venv/bin/python`, [
  `${POP}/bin/pitchsnap_world.py`, inWav, outWav,
  "--notes", notes.join(","),
  "--note-starts", starts.join(","),
  "--retain", RETAIN,
  "--xfade-ms", XFADE_MS,
  "--voicing-ramp-ms", VOICING_RAMP_MS,
  "--vibrato-hz", VIBRATO_HZ,
  "--vibrato-cents", VIBRATO_CENTS,
], { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ WORLD failed"); process.exit(1); }

console.log(`→ encode wav → mp3`);
r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", outWav, "-c:a", "libmp3lame", "-q:a", "2", OUT_PATH,
]);
if (r.status !== 0) { console.error("✗ ffmpeg encode failed"); process.exit(1); }

rmSync(tmp, { recursive: true, force: true });

// Emit alignment sidecar (used by track-poster.py + downstream tools).
// Includes per-syllable note + weight breakdown so consumers can render
// melismatic moments correctly.
const alignmentPath = OUT_PATH.replace(/\.mp3$/, "-alignment.json");
writeFileSync(alignmentPath, JSON.stringify(scoreWords.map((w, i) => ({
  text: w.text,
  note: w.notes[0],
  notes: w.notes,
  weights: w.weights,
  midi: noteToMidi(w.notes[0]) + TRANSPOSE,
  midis: w.notes.map((n) => noteToMidi(n) + TRANSPOSE),
  fromMs: aligned[i].fromMs,
  toMs: aligned[i].toMs,
})), null, 2));
console.log(`✓ ${OUT_PATH}`);
console.log(`✓ ${alignmentPath}  (${scoreWords.length} words · ${totalSyl} syllables)`);
