#!/usr/bin/env node
// align.mjs — run whisper-cli on a vocal stem, emit per-word timestamps.
//
// Mirrors `recap/bin/transcribe.mjs` but takes the stem path as an
// argument and writes `<stem>-words.json` next to the source. Uses the
// same whisper.cpp model already in recap/models/. Caches by source
// content hash; --force to bypass.
//
// Output format (matches recap pipeline): [{text, fromMs, toMs}, ...]
//
// Usage:
//   node bin/align.mjs ../big-pictures/out/plork-hook-vocal.mp3
//   node bin/align.mjs <stem.mp3> --force

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, unlinkSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");
const MODEL = `${REPO}/recap/models/ggml-base.en.bin`;

const argv = process.argv.slice(2);
const force = argv.includes("--force");
const stemPath = resolve(process.cwd(), argv.find((a) => !a.startsWith("--")) || "");

if (!stemPath || !existsSync(stemPath)) {
  console.error("usage: node bin/align.mjs <stem.mp3> [--force]");
  process.exit(1);
}
if (!existsSync(MODEL)) {
  console.error(`✗ missing whisper model: ${MODEL}\n  download: curl -L -o ${MODEL} https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin`);
  process.exit(1);
}

const inputHash = createHash("sha256")
  .update(readFileSync(stemPath))
  .digest("hex").slice(0, 16);

const wordsPath = stemPath.replace(/\.mp3$/, "-words.json");
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

console.log(`→ whisper-cli · ${stemPath}`);
const stemDir = dirname(stemPath);
const stemBase = basename(stemPath).replace(/\.mp3$/, "");
const tmpJson = `${stemDir}/${stemBase}.json`;

execFileSync(
  "whisper-cli",
  ["-m", MODEL, "-f", stemPath, "-ojf", "-of", `${stemDir}/${stemBase}`, "--max-len", "1", "-ml", "1", "-sow"],
  { stdio: ["ignore", "ignore", "inherit"] },
);

const raw = JSON.parse(readFileSync(tmpJson, "utf8"));
const words = raw.transcription
  .map((s) => ({ text: s.text.trim(), fromMs: s.offsets.from, toMs: s.offsets.to }))
  .filter((w) => w.text.length > 0);

writeFileSync(wordsPath, JSON.stringify(words, null, 2));
writeFileSync(hashFile, inputHash + "\n");
try { unlinkSync(tmpJson); } catch {}

const last = words[words.length - 1];
console.log(`✓ ${wordsPath} · ${words.length} words · ${(last.toMs / 1000).toFixed(2)}s · hash ${inputHash}`);
console.log(`  first 6: ${words.slice(0, 6).map(w => `${w.text}@${(w.fromMs/1000).toFixed(2)}s`).join(" ")}`);
