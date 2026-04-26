#!/usr/bin/env node
// transcribe.mjs — run whisper-cli on out/recap.mp3 and emit out/words.json
// in a flat shape: [{text, fromMs, toMs}, ...].
// Usage: node bin/transcribe.mjs

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const MP3 = `${ROOT}/out/recap.mp3`;
const MODEL = `${ROOT}/models/ggml-base.en.bin`;

if (!existsSync(MP3)) {
  console.error(`✗ missing ${MP3} — run bin/tts.mjs first`);
  process.exit(1);
}
if (!existsSync(MODEL)) {
  console.error(`✗ missing ${MODEL} — download with:\n  curl -L -o ${MODEL} https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin`);
  process.exit(1);
}

console.log(`→ whisper-cli · ${MP3}`);
execFileSync(
  "whisper-cli",
  ["-m", MODEL, "-f", MP3, "-ojf", "-of", `${ROOT}/out/recap`, "--max-len", "1", "-ml", "1", "-sow"],
  { stdio: ["ignore", "ignore", "inherit"] },
);

const raw = JSON.parse(readFileSync(`${ROOT}/out/recap.json`, "utf8"));
const words = raw.transcription
  .map((s) => ({ text: s.text.trim(), fromMs: s.offsets.from, toMs: s.offsets.to }))
  .filter((w) => w.text.length > 0);

writeFileSync(`${ROOT}/out/words.json`, JSON.stringify(words, null, 2));
console.log(`✓ ${ROOT}/out/words.json · ${words.length} words · ${(words[words.length - 1].toMs / 1000).toFixed(2)}s`);
