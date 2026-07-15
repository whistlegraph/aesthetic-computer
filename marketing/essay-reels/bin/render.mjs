#!/usr/bin/env node
// Podcast reading → word-timed 9:16 Instagram Reel / Story.

import { spawn, spawnSync } from "node:child_process";
import { copyFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, loadImage } from "canvas";
import { wordsFromWhisper } from "../../lib/words.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");
const POD = resolve(REPO, "marketing", "podcast", "out");
const OUT_DIR = resolve(HERE, "..", "out");
const MODEL = resolve(REPO, "recap", "models", "ggml-large-v3-turbo.bin");
const W = 1080;
const H = 1920;
const FPS = 30;

const argv = process.argv.slice(2);
const slug = argv.find((a) => !a.startsWith("--"));
const flag = (name, fallback) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[i + 1] : fallback;
};
const has = (name) => argv.includes(`--${name}`);
if (!slug) {
  console.error("usage: render.mjs <podcast-slug> [--start 7] [--duration 45] [--open]");
  process.exit(2);
}

const audio = resolve(POD, `${slug}.mp3`);
const coverPath = resolve(POD, `${slug}-cover.png`);
const metaPath = resolve(POD, `${slug}.json`);
if (![audio, coverPath, metaPath, MODEL].every(existsSync)) {
  console.error(`✗ missing podcast audio, cover, metadata, or Whisper model for ${slug}`);
  process.exit(1);
}

mkdirSync(OUT_DIR, { recursive: true });
const work = resolve(OUT_DIR, `.work-${slug}`);
mkdirSync(work, { recursive: true });
const start = Number(flag("start", "7"));
const duration = Number(flag("duration", "45"));
const out = resolve(flag("out", resolve(OUT_DIR, `${slug}-reel.mp4`)));
const story = resolve(OUT_DIR, `${slug}-story.mp4`);
const wav = resolve(work, "excerpt.wav");
const mono = resolve(work, "excerpt-16k.wav");
const whisperBase = resolve(work, "words");
const whisperJson = `${whisperBase}.json`;
const meta = JSON.parse(readFileSync(metaPath, "utf8"));

const run = (cmd, args, opts = {}) => {
  const r = spawnSync(cmd, args, { encoding: "utf8", ...opts });
  if (r.status !== 0) throw new Error(`${cmd}: ${r.stderr?.slice(-800)}`);
  return r.stdout;
};

console.log(`· excerpt ${start}s → ${start + duration}s`);
run("ffmpeg", ["-y", "-ss", String(start), "-t", String(duration), "-i", audio,
  "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", wav], { stdio: "ignore" });
run("ffmpeg", ["-y", "-i", wav, "-ar", "16000", "-ac", "1", mono], { stdio: "ignore" });

console.log("· transcribing word timing");
run("whisper-cli", ["-m", MODEL, "-f", mono, "-oj", "-ojf", "-ml", "1", "-l", "en", "-ng", "-of", whisperBase]);
const words = wordsFromWhisper(whisperJson);
writeFileSync(resolve(OUT_DIR, `${slug}-words.json`), JSON.stringify(words, null, 2) + "\n");

// Decode a light-weight mono envelope for the pulse.
const pcm = spawnSync("ffmpeg", ["-v", "error", "-i", wav, "-f", "s16le", "-ac", "1", "-ar", "8000", "-"], { encoding: null }).stdout;
const samples = new Int16Array(pcm.buffer, pcm.byteOffset, Math.floor(pcm.byteLength / 2));
const energyAt = (sec) => {
  const a = Math.max(0, Math.floor(sec * 8000));
  const b = Math.min(samples.length, a + 640);
  let sum = 0;
  for (let i = a; i < b; i++) sum += (samples[i] / 32768) ** 2;
  return Math.min(1, Math.sqrt(sum / Math.max(1, b - a)) * 5.5);
};

const cover = await loadImage(coverPath);
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const frames = Math.round(duration * FPS);

// Group words into short readable phrases, never more than seven words.
const phrases = [];
for (let i = 0; i < words.length;) {
  const group = [];
  const fromMs = words[i].fromMs;
  while (i < words.length && group.length < 7) {
    group.push(words[i++]);
    if (/[.!?]$/.test(group.at(-1).text) && group.length >= 3) break;
  }
  phrases.push({ words: group, fromMs, toMs: group.at(-1).toMs + 280 });
}

const roundRect = (x, y, w, h, r) => {
  ctx.beginPath(); ctx.roundRect(x, y, w, h, r); ctx.fill();
};
const wrap = (items, maxWidth) => {
  const lines = [[]];
  for (const item of items) {
    const test = [...lines.at(-1), item].map((w) => w.text).join(" ");
    if (ctx.measureText(test).width > maxWidth && lines.at(-1).length) lines.push([item]);
    else lines.at(-1).push(item);
  }
  return lines;
};

const enc = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-i", wav, "-map", "0:v", "-map", "1:a", "-c:v", "libx264", "-preset", "veryfast",
  "-crf", "18", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "192k", "-shortest",
  "-movflags", "+faststart", out], { stdio: ["pipe", "inherit", "inherit"] });
const write = (buf) => new Promise((ok) => enc.stdin.write(buf) ? ok() : enc.stdin.once("drain", ok));

console.log(`· rendering ${frames} frames`);
for (let f = 0; f < frames; f++) {
  const t = f / FPS;
  const ms = t * 1000;
  const zoom = 1.12 + 0.025 * Math.sin(t * 0.16);
  const cw = W * zoom;
  const ch = cw;
  ctx.fillStyle = "#fff9fc";
  ctx.fillRect(0, 0, W, H);
  ctx.globalAlpha = 0.23;
  ctx.drawImage(cover, (W - cw) / 2, (H - ch) / 2, cw, ch);
  ctx.globalAlpha = 1;
  const fade = ctx.createLinearGradient(0, 0, 0, H);
  fade.addColorStop(0, "rgba(255,249,252,.94)");
  fade.addColorStop(.35, "rgba(255,249,252,.72)");
  fade.addColorStop(.75, "rgba(255,249,252,.78)");
  fade.addColorStop(1, "rgba(255,249,252,.96)");
  ctx.fillStyle = fade; ctx.fillRect(0, 0, W, H);

  ctx.textAlign = "center";
  ctx.fillStyle = "#777";
  ctx.font = "700 30px Arial";
  ctx.fillText("A READING FROM AESTHETIC.COMPUTER", W / 2, 150);
  ctx.fillStyle = "#40384a";
  ctx.font = "700 48px Arial";
  ctx.fillText(meta.title, W / 2, 225);

  const e = energyAt(t);
  ctx.strokeStyle = "#b44887";
  ctx.lineWidth = 10;
  ctx.beginPath(); ctx.arc(W / 2, 390, 58 + e * 28, 0, Math.PI * 2); ctx.stroke();
  ctx.fillStyle = "#fff"; ctx.beginPath(); ctx.arc(W / 2, 390, 20, 0, Math.PI * 2); ctx.fill();

  const phrase = phrases.find((p) => ms >= p.fromMs - 180 && ms <= p.toMs) || phrases.find((p) => ms < p.fromMs) || phrases.at(-1);
  ctx.font = "700 78px Arial";
  const lines = wrap(phrase.words, 820);
  const lineH = 102;
  const boxH = lines.length * lineH + 120;
  ctx.fillStyle = "rgba(64,56,74,.92)";
  roundRect(90, 680 - boxH / 2, 900, boxH, 36);
  let y = 680 - ((lines.length - 1) * lineH) / 2 + 26;
  for (const line of lines) {
    const widths = line.map((w) => ctx.measureText(w.text).width);
    const total = widths.reduce((a, b) => a + b, 0) + (line.length - 1) * 24;
    let x = (W - total) / 2;
    ctx.textAlign = "left";
    line.forEach((word, i) => {
      ctx.fillStyle = ms >= word.fromMs ? "#46c85a" : "#fff9fc";
      ctx.fillText(word.text, x, y);
      x += widths[i] + 24;
    });
    y += lineH;
  }

  ctx.textAlign = "center";
  ctx.fillStyle = "#b44887";
  ctx.font = "700 42px Arial";
  ctx.fillText("PLAYBACK CAN BE AN INSTRUMENT", W / 2, 1570);
  ctx.fillStyle = "#40384a";
  ctx.font = "32px Arial";
  ctx.fillText("The Record Is a Better Interface · @jeffrey", W / 2, 1640);
  ctx.fillStyle = "#777";
  ctx.font = "26px Arial";
  ctx.fillText("read the essay · listen to the episode", W / 2, 1700);
  await write(canvas.toBuffer("raw"));
  if (f % 150 === 0) process.stdout.write(`\r  ${f}/${frames}`);
}
enc.stdin.end();
await new Promise((ok, fail) => enc.on("close", (code) => code === 0 ? ok() : fail(new Error(`ffmpeg ${code}`))));
copyFileSync(out, story);
process.stdout.write(`\r  ${frames}/${frames}\n✓ ${out}\n✓ ${story}\n`);
if (has("open")) spawnSync("open", [out]);
