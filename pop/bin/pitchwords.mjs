#!/usr/bin/env node
// pitchwords.mjs — first stage of vocal-post: pitch each word in a
// vocal stem to its target note from the .np score.
//
// Slice plan: for each whisper word at [fromMs, toMs], grab the audio
// from fromMs to the NEXT word's fromMs (or end of file for the last
// word). This keeps the inter-word silence with each word so the
// concatenated output preserves rhythm.
//
// Pitch plan: walk whisper words in order; map each word to a syllable
// in the .np section at the proportional index (whisper words are
// usually fewer than .np syllables — multi-syllable words pick one
// syllable's note). Pitch-shift the slice by (target − ref) semitones
// using the `rubberband` CLI (formant-preserving).
//
// Aggressive by default — full target-note pitch shift, no smoothing.
// Future: gentle / firm / off knobs per the post-prod memory.
//
// Usage:
//   node bin/pitchwords.mjs --vocal big-pictures/out/plork-hook-vocal.mp3 \
//        --score big-pictures/plork.np --section hook \
//        --ref-note C3 \
//        --out big-pictures/out/plork-hook-pitched.mp3

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync, rmSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";

// ── arg parse ─────────────────────────────────────────────────────────
function parseArgs(argv) {
  const flags = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) continue;
    const k = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; }
    else flags[k] = true;
  }
  return flags;
}

const flags = parseArgs(process.argv.slice(2));

const vocalPath = resolve(process.cwd(), flags.vocal || "");
if (!existsSync(vocalPath)) {
  console.error("usage: --vocal <stem.mp3> --score <path.np> [--section hook] [--ref-note C3] [--out path.mp3]");
  process.exit(1);
}
const wordsPath = resolve(process.cwd(), flags.words || vocalPath.replace(/\.mp3$/, "-words.json"));
if (!existsSync(wordsPath)) {
  console.error(`✗ words.json not found at ${wordsPath}. run bin/align.mjs first.`);
  process.exit(1);
}
const scorePath = resolve(process.cwd(), flags.score || "");
if (!existsSync(scorePath)) {
  console.error(`✗ --score file required (path to .np)`);
  process.exit(1);
}
const SECTION = (flags.section || "hook").toLowerCase();
const REF_NOTE = flags["ref-note"] || "C3";
const OUT_PATH = flags.out
  ? resolve(process.cwd(), flags.out)
  : vocalPath.replace(/\.mp3$/, "-pitched.mp3");

// ── helpers ───────────────────────────────────────────────────────────
const NOTE_TO_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToMidi(p) {
  const m = p.trim().toLowerCase().match(/^([a-g])([#b]?)(-?\d+)$/);
  if (!m) throw new Error(`bad note: ${p}`);
  let semi = NOTE_TO_SEMI[m[1]];
  if (m[2] === "#") semi += 1;
  if (m[2] === "b") semi -= 1;
  const oct = parseInt(m[3], 10);
  return 12 * (oct + 1) + semi;
}

// Same parser shape as recap/bin/vocal.mjs's parseNp — flatten to a
// list of {pitch, syl} per section.
function parseNp(text) {
  const sections = {};
  let current = null;
  for (const raw of text.split("\n")) {
    const line = raw.trim();
    if (!line || line.startsWith("#")) continue;
    if (!line.includes(":") && /^[a-z][a-z0-9 ]*$/.test(line)) {
      current = line.toLowerCase();
      if (!sections[current]) sections[current] = [];
      continue;
    }
    if (!current) { current = "default"; sections[current] = []; }
    const tokens = line.split(/\s+/).filter(Boolean);
    for (const tok of tokens) {
      const m = tok.match(/^([A-Ga-g][#b]?):(.+)$/);
      if (!m) continue;
      const note = m[1].charAt(0).toUpperCase() + m[1].slice(1);
      sections[current].push({ pitch: note, syl: m[2] });
    }
  }
  return sections;
}

function probeDuration(p) {
  const r = spawnSync(
    "ffprobe",
    ["-v", "error", "-show_entries", "format=duration",
     "-of", "default=noprint_wrappers=1:nokey=1", p],
    { encoding: "utf8" }
  );
  return Number(r.stdout.trim());
}

// ── main ──────────────────────────────────────────────────────────────
const words = JSON.parse(readFileSync(wordsPath, "utf8"));
const score = parseNp(readFileSync(scorePath, "utf8"));
const syllables = score[SECTION];
if (!syllables || !syllables.length) {
  console.error(`✗ section '${SECTION}' empty in ${scorePath}`);
  process.exit(1);
}

const refMidi = noteToMidi(REF_NOTE);
const totalDur = probeDuration(vocalPath);

const tmpDir = `${dirname(OUT_PATH)}/.${basename(OUT_PATH).replace(/\..*$/, "")}-pw-tmp`;
rmSync(tmpDir, { recursive: true, force: true });
mkdirSync(tmpDir, { recursive: true });

console.log(`→ pitchwords · ${words.length} whisper words → ${syllables.length} score syllables · ref=${REF_NOTE}`);

const slicePaths = [];
for (let i = 0; i < words.length; i++) {
  const w = words[i];
  const startSec = w.fromMs / 1000;
  const endSec = i < words.length - 1 ? words[i + 1].fromMs / 1000 : totalDur;

  // Proportional mapping word i → syllable
  const sylIdx = Math.min(syllables.length - 1,
                          Math.floor(i * syllables.length / words.length));
  const syl = syllables[sylIdx];
  const noteStr = /\d/.test(syl.pitch) ? syl.pitch : syl.pitch + "3";
  const targetMidi = noteToMidi(noteStr);
  const semitones = targetMidi - refMidi;

  const sliceWav = `${tmpDir}/w${i.toString().padStart(3, "0")}.wav`;
  const cut = spawnSync(
    "ffmpeg",
    ["-hide_banner", "-y", "-loglevel", "error",
     "-ss", startSec.toFixed(4), "-to", endSec.toFixed(4),
     "-i", vocalPath,
     "-c:a", "pcm_s16le", "-ar", "48000", "-ac", "1", sliceWav],
    { stdio: ["ignore", "ignore", "inherit"] }
  );
  if (cut.status !== 0) {
    console.error(`✗ ffmpeg slice failed at word ${i}`);
    process.exit(1);
  }

  let pieceWav = sliceWav;
  if (Math.abs(semitones) >= 0.01) {
    pieceWav = `${tmpDir}/w${i.toString().padStart(3, "0")}-p.wav`;
    const rb = spawnSync(
      "rubberband",
      ["-p", String(semitones), sliceWav, pieceWav],
      { stdio: ["ignore", "ignore", "ignore"] }
    );
    if (rb.status !== 0) {
      console.error(`✗ rubberband failed at word ${i} (Δ${semitones} st)`);
      process.exit(1);
    }
  }
  slicePaths.push(pieceWav);

  const arrow = semitones >= 0 ? "+" : "";
  console.log(
    `  ${i.toString().padStart(2, "0")} ${w.text.padEnd(14)} ` +
    `${startSec.toFixed(2)}–${endSec.toFixed(2)}s  →  ${syl.syl.padEnd(8)} ${noteStr} ` +
    `(${arrow}${semitones} st)`
  );
}

// ── concat ─────────────────────────────────────────────────────────────
const concatList = `${tmpDir}/concat.txt`;
writeFileSync(concatList, slicePaths.map(p => `file '${p}'`).join("\n") + "\n");

const concat = spawnSync(
  "ffmpeg",
  ["-hide_banner", "-y", "-loglevel", "error",
   "-f", "concat", "-safe", "0", "-i", concatList,
   "-c:a", "libmp3lame", "-q:a", "3", OUT_PATH],
  { stdio: "inherit" }
);
if (concat.status !== 0) {
  console.error("✗ ffmpeg concat failed");
  process.exit(1);
}

rmSync(tmpDir, { recursive: true, force: true });
console.log(`✓ ${OUT_PATH}`);
