#!/usr/bin/env node
// transcribe.mjs — run whisper-cli on out/recap.mp3 and emit out/words.json
// in a flat shape: [{text, fromMs, toMs}, ...].
//
// Caching: keyed on a content hash of recap.mp3. If `out/words.json` exists
// AND `out/words.json.hash` matches, skip the whisper run (~90s on oven CPU).
// Pass `--force` to bypass.
//
// Usage:
//   node bin/transcribe.mjs
//   node bin/transcribe.mjs --force

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const MP3 = `${ROOT}/out/recap.mp3`;
const MODEL = `${ROOT}/models/ggml-base.en.bin`;
const force = process.argv.includes("--force");

if (!existsSync(MP3)) {
  console.error(`✗ missing ${MP3} — run bin/tts.mjs first`);
  process.exit(1);
}
if (!existsSync(MODEL)) {
  console.error(`✗ missing ${MODEL} — download with:\n  curl -L -o ${MODEL} https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin`);
  process.exit(1);
}

// Hash recap.mp3 contents (whisper output depends entirely on the audio).
const inputHash = createHash("sha256")
  .update(readFileSync(MP3))
  .digest("hex")
  .slice(0, 16);

const wordsPath = `${ROOT}/out/words.json`;
const hashFile = `${wordsPath}.hash`;

if (!force && existsSync(wordsPath) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  if (cached === inputHash) {
    const words = JSON.parse(readFileSync(wordsPath, "utf8"));
    const last = words[words.length - 1];
    console.log(`✓ ${wordsPath} cached · ${words.length} words · hash ${inputHash} — skipping whisper`);
    if (last) console.log(`  audio ends at ${(last.toMs / 1000).toFixed(2)}s`);
    process.exit(0);
  }
}

console.log(`→ whisper-cli · ${MP3}`);
// -dtw computes token-level timestamps via cross-attention dynamic time warping
// — far more accurate word start/stop boundaries than the default heuristic,
// which matters for the per-word karaoke captions. DTW is incompatible with
// flash-attention (which is on by default), so -nfa disables it. Must match the
// model preset (base.en).
execFileSync(
  "whisper-cli",
  ["-m", MODEL, "-f", MP3, "-ojf", "-of", `${ROOT}/out/recap`, "--max-len", "1", "-ml", "1", "-sow", "-nfa", "-dtw", "base.en"],
  { stdio: ["ignore", "ignore", "inherit"] },
);

const raw = JSON.parse(readFileSync(`${ROOT}/out/recap.json`, "utf8"));
const words = raw.transcription
  .map((s) => ({ text: s.text.trim(), fromMs: s.offsets.from, toMs: s.offsets.to }))
  .filter((w) => w.text.length > 0);

writeFileSync(wordsPath, JSON.stringify(words, null, 2));
// Stable snapshot of fresh whisper output — sing.mjs reads from this
// so its score never gets fed back its own previous output.
writeFileSync(`${ROOT}/out/words.whisper.json`, JSON.stringify(words, null, 2));
writeFileSync(hashFile, inputHash + "\n");
console.log(`✓ ${wordsPath} · ${words.length} words · ${(words[words.length - 1].toMs / 1000).toFixed(2)}s · hash ${inputHash}`);
