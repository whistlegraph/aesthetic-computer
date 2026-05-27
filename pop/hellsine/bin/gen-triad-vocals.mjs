#!/usr/bin/env node
// gen-triad-vocals.mjs — generate jeffrey vocal samples by:
//   1. requesting "the X you" triads from ElevenLabs (via /api/say)
//      at slowest speech speed for maximum vowel/plosive detail,
//   2. reading the with-timestamps alignment to find the inner word,
//   3. slicing that inner word with ffmpeg into 48 kHz mono float WAV,
//   4. writing into pop/hellsine/samples/jeffrey-vocal-{variant}.wav
//      (overwriting the existing per-variant cuts).
//
// Run once when you want to regenerate the vocal samples. C engine
// loads these at render time — no changes needed there.

import { writeFileSync, readFileSync, mkdirSync, existsSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const SAMPLES = resolve(HERE, "../samples");
const WORK = "/tmp/hellsine-triads";
mkdirSync(WORK, { recursive: true });

const SAY = `${REPO}/pop/bin/say.mjs`;
const VARIANTS = ["money", "honey", "bunnies"];

// ElevenLabs settings — slowest speed for longest speech with most
// vowel/plosive detail. stability ≥ 0.5 keeps jeffrey identity (memory).
const SAY_FLAGS = [
  "--provider", "jeffrey",
  "--voice", "neutral:0",
  "--speed", "0.7",
  "--stability", "0.55",
  "--similarity", "0.9",
  "--style", "0.4",
  "--timestamps",
  "--force",
];

const run = (cmd, args, label) => {
  console.log(`\n[gen-triad] ${label}`);
  const r = spawnSync(cmd, args, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`[gen-triad] FAILED: ${label}`); process.exit(1); }
};

// Canonical mantra (extended from 10 → 12 words 2026-05-26):
// "i hope that we get all of the [money|honey|bunnies] that we want"
const MANTRA = (variant) =>
  `i hope that we get all of the ${variant === "bunnies" ? "bun-nies" : variant.replace("oney", "o-ney")} that we want`;

for (const variant of VARIANTS) {
  // 1. write lyric file — FULL mantra phrase
  const lyric = `${WORK}/mantra-${variant}.txt`;
  writeFileSync(lyric, `hook\n${MANTRA(variant)}\n`);

  // 2. ElevenLabs synthesis → mp3 + alignment JSON
  const mp3 = `${WORK}/mantra-${variant}.mp3`;
  run("node", [SAY, lyric, "--section", "hook", ...SAY_FLAGS, "--out", mp3],
    `synth · "${MANTRA(variant)}" @ speed=0.7`);

  // 3. convert full mantra to mono 48 kHz float WAV
  const outWav = `${SAMPLES}/jeffrey-vocal-${variant}.wav`;
  run("ffmpeg", [
    "-y", "-i", mp3,
    "-ar", "48000", "-ac", "1", "-c:a", "pcm_f32le",
    outWav,
  ], `full-mantra WAV · ${variant} → ${outWav}`);

  // 4. write a .words.txt sidecar with `word fromMs toMs` per line so the
  //    C engine can align each word to a melody note.
  const alignPath = `${mp3}.alignment.json`;
  if (existsSync(alignPath)) {
    const align = JSON.parse(readFileSync(alignPath, "utf8"));
    const lines = align.words.map((w) => `${w.text}\t${w.fromMs}\t${w.toMs}`);
    const wordsPath = outWav.replace(/\.wav$/, ".words.txt");
    writeFileSync(wordsPath, lines.join("\n") + "\n");
    console.log(`  · word boundaries → ${wordsPath} (${align.words.length} words)`);
  }
}

console.log(`\n[gen-triad] done · ${VARIANTS.length} jeffrey-vocal-*.wav files refreshed`);
console.log(`[gen-triad] next: re-bake the C engine to pick up the new samples`);
console.log(`            node pop/hellsine/c/bake-c.mjs`);
