#!/usr/bin/env node
import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, registerFont } from "canvas";
import { renderSineBed } from "../podcast/bin/jingle.mjs";
import { makeSosoftSideIdentity } from "../lib/sosoft-side-identity.mjs";
import { loadNarrationSource, loadNarrationTimeline, sceneStart } from "./timing.mjs";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const VIDEO = resolve(OUT, "unboxing-spine-realtime.mp4");
const narrationSource = loadNarrationSource(ROOT);
const timing = loadNarrationTimeline(ROOT);
const VOICE = narrationSource.audio;
const BED = resolve(OUT, "sine-bed.wav");
const MIX = resolve(OUT, "narration-sine-mix.wav");
const OUTPUT = resolve(OUT, "scores-for-social-software-captioned-08.mp4");
const words = JSON.parse(readFileSync(resolve(OUT, "words.json"), "utf8"));
if (!existsSync(VIDEO) || !existsSync(VOICE)) throw new Error("render the real-time spine and narration first");

const W = 1080, H = 1920, FPS = 30;
const PANEL_Y = 1440;
const duration = Number(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", VOICE], { encoding: "utf8" }).stdout.trim());
try {
  registerFont("/System/Library/Fonts/Supplemental/Arial.ttf", { family: "Arial", weight: "normal" });
  registerFont("/System/Library/Fonts/Supplemental/Arial Bold.ttf", { family: "Arial", weight: "bold" });
} catch {}

console.log("generate sine-wave bed");
renderSineBed(duration + 0.5, BED);
console.log(`duck bed beneath ${narrationSource.kind} narration and master to -14 LUFS`);
let r = spawnSync("ffmpeg", ["-y", "-v", "error", "-i", VOICE, "-i", BED, "-filter_complex",
  "[0:a]apad,asplit=2[v][key];[1:a]volume=0.30[bed];[bed][key]sidechaincompress=threshold=0.045:ratio=7:attack=8:release=420[duck];[v][duck]amix=inputs=2:duration=first:normalize=0,loudnorm=I=-14:TP=-1.5:LRA=11[out]",
  "-map", "[out]", "-t", String(duration), "-ar", "48000", "-ac", "2", MIX]);
if (r.status !== 0) throw new Error("audio mix failed");

// Short phrases; each word retains the selected narrator's exact time.
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
const font = "bold 68px Arial";
const lineH = 86, maxW = 850, gap = 19;
const chapterStart = (id) => sceneStart(timing, id) * 1000;
const chapters = [
  { artist: "SCORES FOR SOCIAL SOFTWARE", work: "INTRODUCTION", fromMs: 0 },
  { artist: "JEFFREY ALAN SCUDDER", work: "NOTEPAT", fromMs: chapterStart("SSF-01") },
  { artist: "ÆTHER CAVENDISH", work: "VIGIL SCORE", fromMs: chapterStart("SSF-02") },
  { artist: "CHELLY JIN", work: "SOFTWARE AS A CHOREOGRAPHY", fromMs: chapterStart("SSF-03") },
  { artist: "JORDAN SILVER", work: "SONIC ARCHITECTURE", fromMs: chapterStart("SSF-04") },
  { artist: "EM LUGO", work: "CUES FOR LOSING DIRECTION", fromMs: chapterStart("SSF-05") },
  { artist: "DARLYN PHAN", work: "LINE PIECE 1", fromMs: chapterStart("SSF-06") },
  { artist: "THOMAS NOYA", work: "BIOPHONÍA", fromMs: chapterStart("SSF-07") },
  { artist: "BANYI HUANG", work: "A COSMOGRAPHIC SCORE", fromMs: chapterStart("SSF-08") },
  { artist: "ALEXANDER ESPINOSA", work: "MUSIC FOR WORLD COMPUTERS", fromMs: chapterStart("SSF-09") },
  { artist: "MAVYN VU", work: "THE RADIO IS AN ALTAR: PORTAL", fromMs: chapterStart("SSF-10") },
  { artist: "LAUREN LEE MCCARTHY + CASEY REAS", work: "SCORES FOR SOCIAL SOFTWARE", fromMs: chapterStart("SSF-11") },
];
// A twelve-step family sampled around the publication envelope's powder blue.
// The lower field, captions, chapter labels, and timeline all inherit the
// current chapter color so the edit has no generic black information panel.
const chapterBlues = [
  "#c9e8f7", "#bde2f4", "#b1dcf1", "#a5d6ee",
  "#99d0eb", "#8dc9e7", "#81c3e4", "#75bde1",
  "#69b7de", "#5db1db", "#51abd8", "#45a5d5",
];
const hexRgb = (hex) => [1, 3, 5].map((at) => parseInt(hex.slice(at, at + 2), 16));
const mixHex = (from, to, amount) => {
  const a = hexRgb(from), b = hexRgb(to);
  return `#${a.map((value, i) => Math.round(value + (b[i] - value) * amount).toString(16).padStart(2, "0")).join("")}`;
};
const colorsFor = (index) => {
  const blue = chapterBlues[index];
  return {
    panel: mixHex(blue, "#ffffff", 0.56),
    caption: mixHex(blue, "#164d70", 0.68),
    active: mixHex(blue, "#073653", 0.84),
    played: mixHex(blue, "#164d70", 0.34),
  };
};
const chapterIndexAt = (ms) => Math.max(0, chapters.findLastIndex((chapter) => ms >= chapter.fromMs));
const rgba = (hex, alpha) => {
  const [r, g, b] = hexRgb(hex);
  return `rgba(${r},${g},${b},${alpha})`;
};
const chapterCards = chapters.map((chapter, index) => {
  const plate = createCanvas(960, 260);
  const p = plate.getContext("2d");
  const colors = colorsFor(index);
  p.beginPath();
  p.roundRect(0, 0, plate.width, plate.height, 28);
  p.clip();
  p.fillStyle = rgba(colors.panel, 0.94);
  p.fillRect(0, 0, plate.width, plate.height);
  const stripeW = 64;
  for (let x = 0, stripe = 0; x < plate.width; x += stripeW, stripe++) {
    p.fillStyle = rgba(mixHex(chapterBlues[index], "#ffffff", stripe % 2 ? 0.56 : 0.72), 0.72);
    p.fillRect(x, 0, stripeW, plate.height);
  }
  p.textBaseline = "alphabetic";
  p.textAlign = "center";
  p.lineJoin = "round";
  p.fillStyle = colors.active;
  p.font = "bold 46px Arial";
  p.fillText(chapter.artist, plate.width / 2, 92);
  p.font = "normal 38px Arial";
  const words = chapter.work.split(/\s+/);
  const lines = [""];
  for (const word of words) {
    const candidate = lines.at(-1) ? `${lines.at(-1)} ${word}` : word;
    if (p.measureText(candidate).width > 820 && lines.at(-1)) lines.push(word);
    else lines[lines.length - 1] = candidate;
  }
  const firstY = lines.length > 1 ? 158 : 174;
  lines.forEach((line, lineIndex) => p.fillText(line, plate.width / 2, firstY + lineIndex * 47));
  return plate;
});
const identityAssets = resolve(OUT, "sosoft-identity-assets");
mkdirSync(identityAssets, { recursive: true });
const sideIdentity = await makeSosoftSideIdentity({
  w: W, h: H, fps: FPS, frames: Math.ceil(duration * FPS), assetsDir: identityAssets,
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
  const colors = colorsFor(chapterIndexAt(ms));
  const lines = linesFor(phrase.words);
  const boxH = lines.length * lineH + 70;
  const y0 = 1580 - boxH / 2;
  let y = y0 + 58;
  ctx.font = font; ctx.textBaseline = "top"; ctx.textAlign = "left";
  ctx.lineJoin = "round";
  for (const line of lines) {
    const widths = line.map((w) => ctx.measureText(w.text).width);
    const total = widths.reduce((a, b) => a + b, 0) + gap * (line.length - 1);
    let x = (W - total) / 2;
    line.forEach((word, i) => {
      ctx.fillStyle = ms >= word.fromMs && ms <= word.toMs + 90 ? colors.active : colors.caption;
      ctx.fillText(word.text, x, y);
      x += widths[i] + gap;
    });
    y += lineH;
  }
}

function drawChapter(ms) {
  const index = chapterIndexAt(ms);
  const colors = colorsFor(index);
  ctx.save();
  ctx.fillStyle = colors.panel;
  ctx.fillRect(0, PANEL_Y, W, H - PANEL_Y);
  const stripeW = 54;
  for (let x = 0, stripe = 0; x < W; x += stripeW, stripe++) {
    ctx.fillStyle = mixHex(chapterBlues[index], "#ffffff", stripe % 2 ? 0.56 : 0.72);
    ctx.fillRect(x, PANEL_Y, stripeW, H - PANEL_Y);
  }
  ctx.fillStyle = colors.played;
  ctx.fillRect(0, PANEL_Y, W, 4);

  const barY = 1889, barH = 31;
  const total = duration * 1000;
  const playedX = Math.max(0, Math.min(W, ms / total * W));
  for (let i = 0; i < chapters.length; i++) {
    const x0 = chapters[i].fromMs / total * W;
    const x1 = (chapters[i + 1]?.fromMs ?? total) / total * W;
    const segment = colorsFor(i);
    ctx.fillStyle = chapterBlues[i];
    ctx.fillRect(x0, barY, x1 - x0, barH);
    if (playedX > x0) {
      ctx.fillStyle = segment.played;
      ctx.fillRect(x0, barY, Math.min(x1, playedX) - x0, barH);
    }
    ctx.fillStyle = mixHex(chapterBlues[i], "#ffffff", 0.72);
    ctx.fillRect(x1 - 2, barY, 2, barH);
  }
  const playheadX = Math.max(2, Math.min(W - 2, playedX));
  ctx.fillStyle = colors.active;
  ctx.fillRect(playheadX - 2, barY - 5, 4, barH + 10);
  ctx.restore();
}

function drawChapterIntroduction(ms) {
  const index = chapterIndexAt(ms);
  const elapsed = ms - chapters[index].fromMs;
  const holdMs = 2600;
  if (elapsed < 0 || elapsed > holdMs) return;
  const fade = Math.min(1, elapsed / 280, (holdMs - elapsed) / 340);
  ctx.save();
  ctx.globalAlpha = Math.max(0, fade);
  ctx.drawImage(chapterCards[index], (W - chapterCards[index].width) / 2, 360);
  ctx.restore();
}

console.log(`burn exact captions · ${phrases.length} phrases`);
// Recover more of the portrait for motion: enlarge the clean 1080x1280 image
// to 1440px high with a modest centered crop, leaving only the bottom quarter
// for the replaceable information field.
const visualFilter = [
  "crop=1080:1280:0:0",
  `scale=-2:${PANEL_Y}:flags=lanczos`,
  `crop=${W}:${PANEL_Y}:(iw-${W})/2:0`,
  `pad=${W}:${H}:0:0:color=white`,
  "tpad=stop_mode=clone:stop_duration=2",
  `trim=duration=${duration}`,
].join(",");
const dec = spawn("ffmpeg", ["-v", "error", "-i", VIDEO, "-vf", visualFilter, "-f", "rawvideo", "-pix_fmt", "rgba", "-"], { stdio: ["ignore", "pipe", "inherit"] });
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
      drawChapter(ms);
      drawCaptions(ms);
      drawChapterIntroduction(ms);
      const identityEnvelope = Math.max(0, Math.sin(ms / 1000 * Math.PI * 2 * 1.8)) ** 5;
      sideIdentity.draw(ctx, ms / 1000, identityEnvelope);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
      fi++;
      if (fi % 300 === 0) process.stdout.write(`\r${fi} frames`);
    }
  }
}
enc.stdin.end();
await new Promise((ok, fail) => enc.on("close", (code) => code === 0 ? ok() : fail(new Error(`encode ${code}`))));
process.stdout.write(`\r${fi} frames\n${OUTPUT}\n`);
