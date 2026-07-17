#!/usr/bin/env node
import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, registerFont } from "canvas";
import { renderSineBed } from "../podcast/bin/jingle.mjs";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const VIDEO = resolve(OUT, "unboxing-spine-realtime.mp4");
const VOICE = resolve(OUT, "narration.mp3");
const BED = resolve(OUT, "sine-bed.wav");
const MIX = resolve(OUT, "narration-sine-mix.wav");
const OUTPUT = resolve(OUT, "scores-for-social-software-captioned-08.mp4");
const words = JSON.parse(readFileSync(resolve(OUT, "words.json"), "utf8"));
if (!existsSync(VIDEO) || !existsSync(VOICE)) throw new Error("render the real-time spine and narration first");

const W = 1080, H = 1920, FPS = 30;
const duration = Number(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", VOICE], { encoding: "utf8" }).stdout.trim());
const YWFT_BOLD = resolve(ROOT, "../../slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf");
const YWFT_REGULAR = resolve(ROOT, "../../slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf");
try {
  registerFont(YWFT_BOLD, { family: "YWFT Processing", weight: "bold" });
  registerFont(YWFT_REGULAR, { family: "YWFT Processing", weight: "normal" });
} catch {}

console.log("generate sine-wave bed");
renderSineBed(duration + 0.5, BED);
console.log("duck bed beneath Jeffrey and master to -14 LUFS");
let r = spawnSync("ffmpeg", ["-y", "-v", "error", "-i", VOICE, "-i", BED, "-filter_complex",
  "[0:a]apad,asplit=2[v][key];[1:a]volume=0.30[bed];[bed][key]sidechaincompress=threshold=0.045:ratio=7:attack=8:release=420[duck];[v][duck]amix=inputs=2:duration=first:normalize=0,loudnorm=I=-14:TP=-1.5:LRA=11[out]",
  "-map", "[out]", "-t", String(duration), "-ar", "48000", "-ac", "2", MIX]);
if (r.status !== 0) throw new Error("audio mix failed");

// Short phrases; each word retains ElevenLabs' exact time for highlighting.
const phrases = [];
for (let i = 0; i < words.length;) {
  const group = [];
  while (i < words.length && group.length < 7) {
    group.push(words[i++]);
    if (/[.!?]$/.test(group.at(-1).text)) break;
  }
  phrases.push({ words: group, fromMs: group[0].fromMs - 120, toMs: group.at(-1).toMs + 260 });
}

const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
const FRAME_BYTES = W * H * 4;
const frame = Buffer.alloc(FRAME_BYTES);
const image = ctx.createImageData(W, H);
const font = "bold 68px YWFT Processing, Arial";
const lineH = 86, maxW = 850, gap = 19;
const narration = readFileSync(resolve(ROOT, "narration.txt"), "utf8").trim();
const alignment = JSON.parse(readFileSync(resolve(OUT, "narration-alignment.json"), "utf8")).alignment;
const chapterStart = (phrase) => alignment.character_start_times_seconds[narration.indexOf(phrase)] * 1000;
const chapters = [
  { artist: "SCORES FOR SOCIAL SOFTWARE", work: "INTRODUCTION", fromMs: 0 },
  { artist: "JEFFREY ALAN SCUDDER", work: "NOTEPAT", fromMs: chapterStart("My contribution") },
  { artist: "ÆTHER CAVENDISH", work: "VIGIL SCORE", fromMs: chapterStart("Æther Cavendish") },
  { artist: "CHELLY JIN", work: "SOFTWARE AS A CHOREOGRAPHY", fromMs: chapterStart("Chelly Jin") },
  { artist: "JORDAN SILVER", work: "SONIC ARCHITECTURE", fromMs: chapterStart("Jordan Silver") },
  { artist: "EM LUGO", work: "CUES FOR LOSING DIRECTION", fromMs: chapterStart("Em Lugo") },
  { artist: "DARLYN PHAN", work: "LINE PIECE 1", fromMs: chapterStart("Darlyn Phan") },
  { artist: "THOMAS NOYA", work: "BIOPHONÍA", fromMs: chapterStart("Thomas Noya") },
  { artist: "BANYI HUANG", work: "A COSMOGRAPHIC SCORE", fromMs: chapterStart("Banyi Huang") },
  { artist: "ALEXANDER ESPINOSA", work: "MUSIC FOR WORLD COMPUTERS", fromMs: chapterStart("Alexander Espinosa") },
  { artist: "MAVYN VU", work: "THE RADIO IS AN ALTAR: PORTAL", fromMs: chapterStart("Mavyn Vu") },
  { artist: "LAUREN LEE MCCARTHY + CASEY REAS", work: "AUTO TUNE", fromMs: chapterStart("Casey Reas") },
];
const chapterPlates = chapters.map((chapter) => {
  const plate = createCanvas(1000, 105);
  const p = plate.getContext("2d");
  p.textBaseline = "alphabetic";
  p.textAlign = "left";
  p.lineJoin = "round";
  p.strokeStyle = "rgba(0,0,0,.82)";
  p.fillStyle = "#ffffff";
  p.lineWidth = 7;
  p.font = "bold 38px YWFT Processing, Arial";
  p.strokeText(chapter.artist, 10, 48);
  p.fillText(chapter.artist, 10, 48);
  p.font = "normal 27px YWFT Processing, Arial";
  p.strokeText(chapter.work, 10, 88);
  p.fillText(chapter.work, 10, 88);
  return plate;
});
function linesFor(items) {
  ctx.font = font;
  const lines = [[]];
  for (const item of items) {
    const candidate = [...lines.at(-1), item];
    const width = candidate.reduce((n, w, i) => n + ctx.measureText(w.text).width + (i ? gap : 0), 0);
    if (width > maxW && lines.at(-1).length) lines.push([item]); else lines[lines.length - 1] = candidate;
  }
  return lines;
}
function drawCaptions(ms) {
  const phrase = phrases.find((p) => ms >= p.fromMs && ms <= p.toMs);
  if (!phrase) return;
  const lines = linesFor(phrase.words);
  const boxH = lines.length * lineH + 70;
  const y0 = 1510 - boxH / 2;
  let y = y0 + 58;
  ctx.font = font; ctx.textBaseline = "top"; ctx.textAlign = "left";
  ctx.lineJoin = "round";
  ctx.lineWidth = 11;
  ctx.strokeStyle = "rgba(0,0,0,.82)";
  for (const line of lines) {
    const widths = line.map((w) => ctx.measureText(w.text).width);
    const total = widths.reduce((a, b) => a + b, 0) + gap * (line.length - 1);
    let x = (W - total) / 2;
    line.forEach((word, i) => {
      ctx.strokeText(word.text, x, y);
      ctx.fillStyle = ms >= word.fromMs && ms <= word.toMs + 90 ? "#f6cd3f" : "#ffffff";
      ctx.fillText(word.text, x, y);
      x += widths[i] + gap;
    });
    y += lineH;
  }
}

function drawChapter(ms) {
  let index = chapters.findLastIndex((chapter) => ms >= chapter.fromMs);
  if (index < 0) index = 0;
  ctx.save();
  ctx.drawImage(chapterPlates[index], 32, 1722);

  const barY = 1889, barH = 31;
  const total = duration * 1000;
  const playedX = Math.max(0, Math.min(W, ms / total * W));
  for (let i = 0; i < chapters.length; i++) {
    const x0 = chapters[i].fromMs / total * W;
    const x1 = (chapters[i + 1]?.fromMs ?? total) / total * W;
    ctx.fillStyle = i === index ? "rgba(255,255,255,.40)" : "rgba(255,255,255,.16)";
    ctx.fillRect(x0, barY, x1 - x0, barH);
    if (playedX > x0) {
      ctx.fillStyle = i === index ? "#ffffff" : "rgba(210,210,210,.72)";
      ctx.fillRect(x0, barY, Math.min(x1, playedX) - x0, barH);
    }
    ctx.fillStyle = "rgba(0,0,0,.38)";
    ctx.fillRect(x1 - 2, barY, 2, barH);
  }
  ctx.restore();
}

console.log(`burn exact captions · ${phrases.length} phrases`);
const dec = spawn("ffmpeg", ["-v", "error", "-i", VIDEO, "-vf", `tpad=stop_mode=clone:stop_duration=2,trim=duration=${duration}`, "-f", "rawvideo", "-pix_fmt", "rgba", "-"], { stdio: ["ignore", "pipe", "inherit"] });
// node-canvas' raw buffer is native-endian BGRA. Declaring it as RGBA swaps
// red and blue, turning the publication's blue cover orange.
const enc = spawn("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-", "-i", MIX,
  "-map", "0:v", "-map", "1:a", "-c:v", "libx264", "-preset", "veryfast", "-crf", "18", "-pix_fmt", "yuv420p",
  "-color_primaries", "bt2020", "-color_trc", "arib-std-b67", "-colorspace", "bt2020nc",
  "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart", OUTPUT],
  { stdio: ["pipe", "inherit", "inherit"] });
let off = 0, fi = 0;
for await (const chunk of dec.stdout) {
  let at = 0;
  while (at < chunk.length) {
    const n = Math.min(FRAME_BYTES - off, chunk.length - at);
    chunk.copy(frame, off, at, at + n); off += n; at += n;
    if (off === FRAME_BYTES) {
      off = 0; image.data.set(frame); ctx.putImageData(image, 0, 0);
      const ms = (fi / FPS) * 1000;
      drawCaptions(ms);
      drawChapter(ms);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
      fi++;
      if (fi % 300 === 0) process.stdout.write(`\r${fi} frames`);
    }
  }
}
enc.stdin.end();
await new Promise((ok, fail) => enc.on("close", (code) => code === 0 ? ok() : fail(new Error(`encode ${code}`))));
process.stdout.write(`\r${fi} frames\n${OUTPUT}\n`);
