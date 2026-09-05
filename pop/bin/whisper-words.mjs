#!/usr/bin/env node
// whisper-words.mjs — local whisper inference on any audio, out comes
// the standard words JSON [{text, fromMs, toMs}] the rest of the pop
// toolset speaks (@jeffrey 2026-09-04: "these tools should be part of
// our toolset"). whisper.cpp (`whisper-cli`, brew) + the cached ggml
// model — no API, no upload. Chain with mfa-align.mjs when the true
// lyric is known: whisper's boundaries are good even when its ears
// aren't.
//
//   node pop/bin/whisper-words.mjs <audio> [--out words.json]
//     [--model ~/.whisper-models/ggml-small.bin] [--lang en]
//     [--align lyrics.txt]   → also runs alignment-by-substitution

import { readFileSync, writeFileSync, existsSync, mkdtempSync } from "node:fs";
import { resolve, basename } from "node:path";
import { tmpdir } from "node:os";
import { spawnSync } from "node:child_process";

const argv = process.argv.slice(2);
const AUDIO = argv.find((a) => !a.startsWith("--"));
const flag = (n, d) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
if (!AUDIO || !existsSync(AUDIO)) {
  console.error("usage: node pop/bin/whisper-words.mjs <audio> [--out words.json] [--model ggml] [--lang en] [--align lyrics.txt]");
  process.exit(1);
}
const MODEL = flag("model",
  [`${process.env.HOME}/.whisper-models/ggml-small.bin`,
   `${process.env.HOME}/.whisper-models/ggml-base.en.bin`,
   `${process.env.HOME}/.cache/whisper-models/ggml-base.en.bin`].find(existsSync));
if (!MODEL) { console.error("✗ no ggml model found — brew's whisper-cli needs one in ~/.whisper-models"); process.exit(1); }
const OUT = resolve(flag("out", AUDIO.replace(/\.[a-z0-9]+$/i, "") + "-words.json"));

const work = mkdtempSync(`${tmpdir()}/whisper-words-`);
const wav16 = `${work}/in.wav`;
spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", AUDIO,
  "-ac", "1", "-ar", "16000", wav16], { stdio: "inherit" });

// one word per segment: -ml 1 -sow; JSON beside the wav
const r = spawnSync("whisper-cli", ["-m", MODEL, "-f", wav16, "-l", flag("lang", "en"),
  "-ml", "1", "-sow", "-oj", "-of", `${work}/out`, "-np"], { stdio: ["ignore", "ignore", "inherit"] });
if (r.status !== 0) { console.error("✗ whisper-cli failed"); process.exit(1); }
const doc = JSON.parse(readFileSync(`${work}/out.json`, "utf8"));
const toMs = (ts) => { // "00:00:01,230"
  const m = /(\d+):(\d+):(\d+)[,.](\d+)/.exec(ts);
  return ((+m[1] * 60 + +m[2]) * 60 + +m[3]) * 1000 + +m[4];
};
const words = (doc.transcription ?? [])
  .map((s) => ({ text: s.text.trim(), fromMs: toMs(s.timestamps.from), toMs: toMs(s.timestamps.to) }))
  .filter((w) => w.text && !/^[\[(]/.test(w.text));
writeFileSync(OUT, JSON.stringify(words, null, 1));
console.log(`✓ ${OUT} (${words.length} words · ${basename(MODEL)})`);
for (const w of words) console.log(`  ${String(w.fromMs).padStart(6)}–${String(w.toMs).padEnd(6)} ${w.text}`);

const LYR = flag("align", null);
if (LYR) {
  const mfa = spawnSync("node", [resolve(import.meta.dirname, "mfa-align.mjs"),
    "--audio", AUDIO, "--text", resolve(LYR), "--whisper", OUT,
    "--out", OUT.replace(/-words\.json$/, "-mfa-words.json")], { stdio: "inherit" });
  process.exit(mfa.status ?? 0);
}
