#!/usr/bin/env node
// gen-wizard-words.mjs — run whisper-cli on the jeffrey-live wizard
// takes and emit .words.txt sidecars in the same format as the
// ElevenLabs ones (`word\tfromMs\ttoMs` per line). The C engine then
// loads both sets identically and time-warps each take to the THEME
// beat grid.

import { spawnSync } from "node:child_process";
import { writeFileSync, existsSync, readFileSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const SAMPLES = resolve(HERE, "../samples/jeffrey-live-archived-102516");
const MODEL = `${homedir()}/.whisper-models/ggml-base.en.bin`;
const VARIANTS = ["money", "honey", "bunnies"];

for (const variant of VARIANTS) {
  const wav = `${SAMPLES}/jeffrey-live-that-${variant}.wav`;
  if (!existsSync(wav)) { console.error(`[gen-wizard] missing ${wav}`); continue; }

  // whisper-cli wants 16k mono — convert first.
  const sixteen = `/tmp/wiz-${variant}-16k.wav`;
  const ff = spawnSync("ffmpeg", [
    "-y", "-i", wav, "-ar", "16000", "-ac", "1", "-c:a", "pcm_s16le", sixteen,
  ], { stdio: "pipe" });
  if (ff.status !== 0) {
    console.error(`[gen-wizard] ffmpeg failed for ${variant}: ${ff.stderr.toString().slice(-200)}`);
    continue;
  }

  // JSON output with word-level timestamps (-ml 1 forces word-per-segment).
  const jsonBase = `/tmp/wiz-${variant}`;
  const wh = spawnSync("whisper-cli", [
    "-m", MODEL, "-f", sixteen, "-l", "en",
    "-ml", "1", "-oj", "-of", jsonBase, "-t", "4", "--no-prints",
  ], { stdio: "pipe" });
  if (wh.status !== 0) {
    console.error(`[gen-wizard] whisper failed for ${variant}: ${wh.stderr.toString().slice(-200)}`);
    continue;
  }
  const jsonPath = `${jsonBase}.json`;
  if (!existsSync(jsonPath)) { console.error(`[gen-wizard] no json at ${jsonPath}`); continue; }

  const j = JSON.parse(readFileSync(jsonPath, "utf8"));
  // whisper-cli -ml 1 gives one transcription segment per word.
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
  // Wizard takes are LIVE pre-recorded mantras that say the old 10-word
  // form: "I hope that we get all the {variant} we want". They are NOT
  // re-recorded for the new 12-word mantra — they get mapped onto a
  // subset of the new mantra's grid in the C engine.
  const EXPECTED = 10;
  if (words.length > EXPECTED) {
    const extra = words.length - EXPECTED;
    const merged = words[7];
    for (let k = 1; k <= extra; k++) {
      merged.word += words[7 + k].word;
      merged.toMs  = words[7 + k].toMs;
    }
    words.splice(8, extra);
    console.log(`[gen-wizard]   · canonicalized ${variant} from ${words.length + extra} → ${words.length} (merged "${merged.word}")`);
  }
  const lines = words.map((w) => `${w.word}\t${w.fromMs}\t${w.toMs}`);
  if (lines.length === 0) {
    console.error(`[gen-wizard] ${variant}: whisper returned 0 word segments`);
    continue;
  }
  const outPath = wav.replace(/\.wav$/, ".words.txt");
  writeFileSync(outPath, lines.join("\n") + "\n");
  console.log(`[gen-wizard] ${variant} → ${basename(outPath)} (${lines.length} words)`);
  for (const ln of lines) console.log(`              ${ln}`);
}
