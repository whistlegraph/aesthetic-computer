#!/usr/bin/env node
// gen-choir-words.mjs — for each say-choir/<voice>-<variant>.wav,
// run whisper-cli to get word-level timestamps and write a sibling
// .words.txt sidecar. Voices that whisper can't transcribe (Bahh,
// Bells as pure-tone, Boing, Wobble, Bubbles — singing/sound voices
// with non-linguistic output) just don't get a sidecar; the C engine
// falls back to the glitch-blip path for those.
//
// Also canonicalizes against the expected 10-word mantra: if whisper
// over-segments the variant word (mo/ney/etc), collapse extras into
// position 7 just like gen-wizard-words.mjs does.

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, readdirSync, statSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const CHOIR_DIR = resolve(HERE, "../samples/say-choir");
const MODEL = `${homedir()}/.whisper-models/ggml-base.en.bin`;
// 12-word mantra: "i hope that we get all of the {variant} that we want"
// (extended from 10 → 12 to better match THEME note density).
const EXPECTED = 12;
// Position to merge whisper over-segments into. The variant word
// ("money"/"honey"/"bunnies") now sits at index 8 (was 7 in the
// 10-word mantra).
const MERGE_AT = 8;

const files = readdirSync(CHOIR_DIR)
  .filter((f) => f.endsWith(".wav"))
  .map((f) => resolve(CHOIR_DIR, f));

let ok = 0, skipped = 0, failed = 0;
for (const wav of files) {
  const stem  = basename(wav, ".wav");
  const wavId = `/tmp/cw-${stem}`;
  const sixteen = `${wavId}-16k.wav`;
  const jsonBase = wavId;

  const ff = spawnSync("ffmpeg", [
    "-y", "-i", wav, "-ar", "16000", "-ac", "1", "-c:a", "pcm_s16le", sixteen,
  ], { stdio: "pipe" });
  if (ff.status !== 0) { failed++; console.error(`[choir-words] ffmpeg failed ${stem}`); continue; }

  const wh = spawnSync("whisper-cli", [
    "-m", MODEL, "-f", sixteen, "-l", "en",
    "-ml", "1", "-oj", "-of", jsonBase, "-t", "4", "--no-prints",
  ], { stdio: "pipe" });
  if (wh.status !== 0) { failed++; console.error(`[choir-words] whisper failed ${stem}`); continue; }

  const jsonPath = `${jsonBase}.json`;
  if (!existsSync(jsonPath)) { failed++; continue; }
  const j = JSON.parse(readFileSync(jsonPath, "utf8"));

  let words = [];
  for (const seg of (j.transcription || [])) {
    const word = (seg.text || "").trim().replace(/[^A-Za-z'-]/g, "");
    if (!word) continue;
    words.push({
      word,
      fromMs: seg.offsets?.from ?? 0,
      toMs:   seg.offsets?.to   ?? (seg.offsets?.from ?? 0) + 100,
    });
  }
  // Skip voices whisper can't read — they stay on the glitch-blip path.
  if (words.length < 6) {
    skipped++;
    console.log(`[choir-words] SKIP ${stem.padEnd(28)} (only ${words.length} words detected)`);
    continue;
  }
  // Canonicalize to EXPECTED by collapsing extras at MERGE_AT.
  if (words.length > EXPECTED) {
    const extra = words.length - EXPECTED;
    const merged = words[MERGE_AT];
    for (let k = 1; k <= extra; k++) {
      merged.word += words[MERGE_AT + k].word;
      merged.toMs  = words[MERGE_AT + k].toMs;
    }
    words.splice(MERGE_AT + 1, extra);
  } else if (words.length < EXPECTED) {
    // Some voices drop "I" or "we" — pad with the mantra position so
    // the count matches but the boundary times remain whisper-genuine.
    skipped++;
    console.log(`[choir-words] SKIP ${stem.padEnd(28)} (only ${words.length} words — below target)`);
    continue;
  }
  const lines = words.map((w) => `${w.word}\t${w.fromMs}\t${w.toMs}`);
  const outPath = wav.replace(/\.wav$/, ".words.txt");
  writeFileSync(outPath, lines.join("\n") + "\n");
  ok++;
  console.log(`[choir-words] OK   ${stem.padEnd(28)} ${words.length} words → ${basename(outPath)}`);
}
console.log(`\n[choir-words] ok=${ok}  skipped=${skipped}  failed=${failed}  total=${files.length}`);
