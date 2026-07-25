#!/usr/bin/env node
// Deterministic Captutor card for seats where live Stage capture is blocked.
// The foreground is a sharp, outlined text raster; translucent client marks
// float independently behind it in the encoded video.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const value = (flag, fallback = "") => {
  const i = args.indexOf(flag);
  return i < 0 ? fallback : args[i + 1];
};
const out = resolve(value("--out"));
const title = value("--title");
const subtitle = value("--subtitle");
const kicker = value("--kicker", "FUSER TUTOR");
const footer = value("--footer", "app.fuser.studio");
const duration = Number(value("--duration", "4"));
const width = Number(value("--width", "2560"));
const height = Number(value("--height", "1440"));
const markAsset = resolve(value("--mark", join(HERE, "..", "assets", "fuser-mark.svg")));
if (!out || !title || !existsSync(markAsset)) {
  console.error("usage: offline-card --out card.mp4 --title text [--subtitle text --duration 4]");
  process.exit(2);
}

const work = mkdtempSync(join(tmpdir(), "captutor-card-"));
const mark = join(work, "mark.png");
const tinted = join(work, "mark-tinted.png");
const text = join(work, "text.png");
const latin = join(HERE, "..", "assets", "Marund.ttf");
const devanagari = "/System/Library/Fonts/Kohinoor.ttc";
mkdirSync(dirname(out), { recursive:true });

execFileSync("rsvg-convert", ["-w", "360", "-h", "360", "-o", mark, markAsset]);
execFileSync("magick", [mark, "-fill", "#7E0FFF", "-colorize", "100", tinted]);
const titleFont = /[\u0900-\u097f]/u.test(title) ? devanagari : latin;
const subtitleFont = /[\u0900-\u097f]/u.test(subtitle) ? devanagari : latin;
execFileSync("magick", [
  "-size", `${width}x${height}`, "xc:none", "-gravity", "center",
  "-font", latin, "-pointsize", String(Math.round(height * 0.045)),
  "-fill", "#7E0FFF", "-stroke", "none", "-annotate", "+0-330", kicker,
  "-font", titleFont, "-pointsize", String(Math.round(height * 0.115)),
  "-fill", "rgba(0,0,0,.78)", "-stroke", "none", "-annotate", "+5-72", title,
  "-fill", "#111111", "-stroke", "white", "-strokewidth", "4",
  "-annotate", "+0-78", title,
  "-font", subtitleFont, "-pointsize", String(Math.round(height * 0.055)),
  "-fill", "rgba(0,0,0,.68)", "-stroke", "none", "-annotate", "+3+126", subtitle,
  "-fill", "#171717", "-stroke", "white", "-strokewidth", "2",
  "-annotate", "+0+122", subtitle,
  "-font", latin, "-pointsize", String(Math.round(height * 0.034)),
  "-fill", "#4b4b55", "-stroke", "none", "-annotate", "+0+350", footer,
  text,
]);

const markFilter = [
  `[1:v]format=rgba,colorchannelmixer=aa=.13,split=6[m0][m1][m2][m3][m4][m5]`,
  `[m0]scale=180:180[a0]`, `[m1]scale=260:260[a1]`, `[m2]scale=130:130[a2]`,
  `[m3]scale=220:220[a3]`, `[m4]scale=150:150[a4]`, `[m5]scale=300:300[a5]`,
  `[0:v][a0]overlay=x=150+20*sin(t*.7):y='mod(H+220-t*34,H+400)-220'[v0]`,
  `[v0][a1]overlay=x=W-430+28*sin(t*.5+1):y='mod(H+620-t*27,H+520)-260'[v1]`,
  `[v1][a2]overlay=x=530+18*sin(t*.9+2):y='mod(H+960-t*39,H+300)-130'[v2]`,
  `[v2][a3]overlay=x=W-900+24*sin(t*.65+3):y='mod(H+1260-t*30,H+440)-220'[v3]`,
  `[v3][a4]overlay=x=1050+16*sin(t*.8+4):y='mod(H+340-t*43,H+340)-150'[v4]`,
  `[v4][a5]overlay=x=W-250+30*sin(t*.55+5):y='mod(H+1040-t*24,H+600)-300'[v5]`,
  `[v5][2:v]overlay=0:0:shortest=1[outv]`,
].join(";");
execFileSync("ffmpeg", [
  "-y", "-f", "lavfi", "-i", `color=c=#F4F3F7:s=${width}x${height}:r=60:d=${duration}`,
  "-loop", "1", "-i", tinted, "-loop", "1", "-i", text,
  "-filter_complex_threads", "1", "-filter_complex", markFilter,
  "-map", "[outv]", "-t", duration.toFixed(3), "-r", "60",
  "-c:v", "libx264", "-preset", "medium", "-crf", "16", "-pix_fmt", "yuv420p",
  "-an", "-movflags", "+faststart", out,
], { stdio:["ignore", "ignore", "pipe"] });
console.log(JSON.stringify({ out, duration, width, height, title, subtitle }, null, 2));
