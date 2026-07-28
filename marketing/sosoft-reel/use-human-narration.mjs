#!/usr/bin/env node
// Assemble Narrator Wizard's selected line takes, transcribe their word timing,
// and switch the reel build to the human narration source.
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, isAbsolute, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { wordsFromWhisper } from "../lib/words.mjs";

const ROOT = dirname(fileURLToPath(import.meta.url));
const valueAfter = (flag) => {
  const index = process.argv.indexOf(flag);
  return index >= 0 ? process.argv[index + 1] : undefined;
};
const specPath = resolve(valueAfter("--spec") ?? resolve(ROOT, "narrator-spec.json"));
const SPEC_ROOT = dirname(specPath);
const OUT = resolve(valueAfter("--output-dir") ?? resolve(ROOT, "out"));
const WORK = resolve(OUT, "human-narration");
const spec = JSON.parse(readFileSync(specPath, "utf8"));
const takeRoot = resolve(SPEC_ROOT, spec.outDir);
const manifestPath = resolve(takeRoot, "manifest.json");
if (!existsSync(manifestPath)) throw new Error(`record the screenplay first: ${manifestPath} is missing`);
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const chosen = spec.lines.map((line) => {
  const state = manifest.lines.find((item) => item.id === line.id);
  if (!state?.selectedTake) return { line, error: "no kept take" };
  if (!existsSync(state.selectedTake)) return { line, error: `missing ${state.selectedTake}` };
  return { line, path: state.selectedTake };
});
const missing = chosen.filter((item) => item.error);
if (missing.length) {
  throw new Error(`Narrator Wizard session is incomplete:\n${missing.map((item) => `  ${item.line.id}: ${item.error}`).join("\n")}`);
}
if (process.argv.includes("--check")) {
  console.log(`ready: ${chosen.length} kept takes`);
  process.exit(0);
}

mkdirSync(WORK, { recursive: true });
const run = (cmd, args, options = {}) => {
  const result = spawnSync(cmd, args, { encoding: "utf8", ...options });
  if (result.status !== 0) throw new Error(`${cmd} failed: ${(result.stderr || "").slice(-1200)}`);
  return result.stdout;
};
const duration = (path) => Number(run("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", path]).trim());

const gapMs = Number(manifest.gapMs ?? spec.gapMs ?? 350);
const gap = resolve(WORK, "gap.wav");
run("ffmpeg", ["-y", "-v", "error", "-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono", "-t", String(gapMs / 1000), "-c:a", "pcm_s24le", gap]);

const concatLines = [];
const timeline = [];
let cursor = 0;
for (let i = 0; i < chosen.length; i++) {
  const item = chosen[i];
  const normalized = resolve(WORK, `${item.line.id}.wav`);
  run("ffmpeg", ["-y", "-v", "error", "-i", item.path,
    "-af", "highpass=f=70,lowpass=f=15000",
    "-ar", "48000", "-ac", "1", "-c:a", "pcm_s24le", normalized]);
  const seconds = duration(normalized);
  timeline.push({
    id: item.line.id,
    title: item.line.title,
    text: item.line.text,
    startSec: cursor,
    endSec: cursor + seconds,
    selectedTake: item.path,
  });
  concatLines.push(`file '${normalized.replaceAll("'", "'\\''")}'`);
  cursor += seconds;
  if (i + 1 < chosen.length) {
    concatLines.push(`file '${gap}'`);
    cursor += gapMs / 1000;
  }
}
const concatPath = resolve(WORK, "takes.ffconcat");
writeFileSync(concatPath, `ffconcat version 1.0\n${concatLines.join("\n")}\n`);
const audio = resolve(OUT, "narration-human.wav");
run("ffmpeg", ["-y", "-v", "error", "-f", "concat", "-safe", "0", "-i", concatPath,
  "-af", "dynaudnorm=f=250:g=9:p=0.9:m=6", "-ar", "48000", "-ac", "1", "-c:a", "pcm_s24le", audio]);

const mono = resolve(WORK, "narration-16k.wav");
run("ffmpeg", ["-y", "-v", "error", "-i", audio, "-ar", "16000", "-ac", "1", "-c:a", "pcm_s16le", mono]);
const modelCandidates = [
  process.env.WHISPER_MODEL,
  resolve(ROOT, "../../recap/models/ggml-large-v3-turbo.bin"),
  resolve(homedir(), ".whisper-models/ggml-base.en.bin"),
].filter(Boolean);
const model = modelCandidates.find(existsSync);
if (!model) throw new Error("no Whisper model found; set WHISPER_MODEL");
const whisperBase = resolve(WORK, "whisper");
console.log(`transcribing human narration with ${model}`);
run("whisper-cli", ["-m", model, "-f", mono, "-oj", "-ojf", "-ml", "1", "-l", "en", "-ng", "-of", whisperBase]);
const heard = wordsFromWhisper(`${whisperBase}.json`);

const script = spec.lines.flatMap((line) => line.text.split(/\s+/).filter(Boolean));
const norm = (value) => value.toLowerCase().replaceAll("æ", "ae").replaceAll("œ", "oe")
  .normalize("NFKD").replace(/\p{M}/gu, "").replace(/[^a-z0-9]/g, "");
const A = script.map(norm), B = heard.map((word) => norm(word.text));
const n = A.length, m = B.length;
const dp = Array.from({ length: n + 1 }, () => new Uint16Array(m + 1));
for (let i = n - 1; i >= 0; i--) {
  for (let j = m - 1; j >= 0; j--) {
    dp[i][j] = A[i] && A[i] === B[j] ? dp[i + 1][j + 1] + 1 : Math.max(dp[i + 1][j], dp[i][j + 1]);
  }
}
const match = new Array(n).fill(-1);
for (let i = 0, j = 0; i < n && j < m;) {
  if (A[i] && A[i] === B[j]) { match[i] = j; i++; j++; }
  else if (dp[i + 1][j] >= dp[i][j + 1]) i++;
  else j++;
}
const words = script.map((text, k) => {
  if (match[k] >= 0) return { text, fromMs: heard[match[k]].fromMs, toMs: heard[match[k]].toMs };
  let p = k - 1; while (p >= 0 && match[p] < 0) p--;
  let q = k + 1; while (q < n && match[q] < 0) q++;
  const from = p >= 0 ? heard[match[p]].toMs : 0;
  const to = q < n ? heard[match[q]].fromMs : Math.round(cursor * 1000);
  const slots = q - p;
  return {
    text,
    fromMs: Math.round(from + (to - from) * (k - p) / slots),
    toMs: Math.round(from + (to - from) * (k - p + 1) / slots),
  };
});
writeFileSync(resolve(OUT, "words.json"), `${JSON.stringify(words, null, 2)}\n`);
writeFileSync(resolve(OUT, "narration-timeline.json"), `${JSON.stringify({
  source: "human", totalDuration: duration(audio), gapMs, lines: timeline,
}, null, 2)}\n`);
writeFileSync(resolve(OUT, "narration-source.json"), `${JSON.stringify({
  kind: "human",
  audio: OUT === resolve(ROOT, "out") ? "out/narration-human.wav" : audio,
  takeManifest: isAbsolute(manifestPath) ? manifestPath : resolve(SPEC_ROOT, manifestPath),
  generatedAt: new Date().toISOString(),
}, null, 2)}\n`);
const matched = match.filter((value) => value >= 0).length;
console.log(`human narration ready · ${timeline.length} scenes · ${words.length} words · ${matched} direct timing matches`);
console.log(audio);
