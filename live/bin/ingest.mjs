#!/usr/bin/env node
// ingest.mjs — youtube talking head → whisper words → librosa onset-refined words.
// Uses pop/bin/refine_words.py (the real refiner) instead of hand-rolling one.
//
//   START=600 DUR=90 node ingest.mjs <url> <slug>

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = "/Users/jas/aesthetic-computer/pop";
const PY = `${POP}/.venv/bin/python`;
const MODEL = "/Users/jas/aesthetic-computer/recap/models/ggml-base.en.bin";

const url = process.argv[2];
const slug = process.argv[3] || "clip";
const START = Number(process.env.START ?? 0);
const DUR = Number(process.env.DUR ?? 90);

const DIR = resolve(process.env.AC_LIVE_DIR || process.cwd(), slug);
mkdirSync(DIR, { recursive: true });
const M4A = resolve(DIR, "source.m4a");
const WAV = resolve(DIR, "vocal.wav");   // 16k mono for whisper
const MP3 = resolve(DIR, "vocal.mp3");   // what score-pitch wants
const RAW = resolve(DIR, "words-raw.json");
const REF = resolve(DIR, "words.json");

const run = (c, a) => execFileSync(c, a, { stdio: ["ignore", "pipe", "pipe"] });

if (!existsSync(M4A)) {
  console.log("→ yt-dlp…");
  run("yt-dlp", ["-f", "bestaudio[ext=m4a]/bestaudio", "--no-playlist", "-o", M4A, url]);
} else console.log("  source.m4a cached");

console.log(`→ ffmpeg → ${START}s +${DUR}s (16k mono wav + mp3)…`);
const trim = ["-ss", String(START), "-t", String(DUR)];
run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", ...trim,
  "-i", M4A, "-ac", "1", "-ar", "16000", "-c:a", "pcm_s16le", WAV]);
run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", ...trim,
  "-i", M4A, "-ac", "1", "-ar", "44100", "-b:a", "192k", MP3]);

if (!existsSync(RAW)) {
  console.log("→ whisper-cli (word-level, DTW)…");
  const t0 = Date.now();
  run("whisper-cli", ["-m", MODEL, "-f", WAV,
    "-ojf", "-of", resolve(DIR, "raw"),
    "--max-len", "1", "-ml", "1", "-sow", "-nfa", "-dtw", "base.en", "-np"]);
  const raw = JSON.parse(readFileSync(resolve(DIR, "raw.json"), "utf8"));
  const words = raw.transcription
    .map((t) => ({ text: t.text.trim(), fromMs: t.offsets.from, toMs: t.offsets.to }))
    .filter((w) => w.text && !/^\[.*\]$/.test(w.text));
  writeFileSync(RAW, JSON.stringify(words, null, 2));
  console.log(`  ${words.length} words in ${((Date.now() - t0) / 1000).toFixed(1)}s`);
} else console.log("  words-raw.json cached");

// ── the real refiner: librosa onset snap ────────────────────────────────
console.log("→ refine_words.py (librosa onset snap)…");
const out = run(PY, [`${POP}/bin/refine_words.py`, WAV, RAW, REF, "--win-ms", "200"]);
process.stdout.write(out.toString());

const rawW = JSON.parse(readFileSync(RAW, "utf8"));
const refW = JSON.parse(readFileSync(REF, "utf8"));
const drift = refW.map((w, i) => w.fromMs - rawW[i].fromMs);
const moved = drift.filter((d) => Math.abs(d) > 10).length;
const avg = drift.reduce((a, b) => a + Math.abs(b), 0) / drift.length;

console.log(`\n── ${slug} ──`);
console.log(`words        : ${refW.length}`);
console.log(`onset-snapped: ${moved}/${refW.length} words moved >10ms  (avg |drift| ${avg.toFixed(1)}ms)`);
console.log(`\ntranscript:`);
console.log("  " + refW.map((w) => w.text).join(" "));
