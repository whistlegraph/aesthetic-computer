#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, loadImage } from "canvas";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const SELECTS = resolve(OUT, "selects");
const OVERLAYS = resolve(OUT, "overlays");
const MASTER = "/Users/jas/Downloads/IMG_4889.MOV";
const BIO = "/Users/jas/Downloads/Biophonia_SoSoft_Fuser_V2.mp4";
const project = JSON.parse(readFileSync(resolve(ROOT, "index.json"), "utf8"));
mkdirSync(SELECTS, { recursive: true });
mkdirSync(OVERLAYS, { recursive: true });

const run = (cmd, args) => {
  const r = spawnSync(cmd, args, { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`${cmd}: ${r.stderr?.slice(-1200)}`);
};

const slug = (item) => String(item.order).padStart(2, "0") + "-" + item.title
  .normalize("NFKD").replace(/[^a-zA-Z0-9]+/g, "-").replace(/^-|-$/g, "").toLowerCase();

for (const item of project.items) {
  const base = slug(item);
  const still = resolve(SELECTS, `${base}.jpg`);
  if (!existsSync(still)) {
    const src = item.title === "Biophonía" ? BIO : MASTER;
    const at = item.title === "Biophonía" ? 120 : item.unboxingTime;
    console.log(`extract ${base} @ ${at}s`);
    run("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-hwaccel", "videotoolbox",
      "-ss", String(at), "-i", src, "-frames:v", "1", "-vf", "scale=1200:-2:flags=lanczos", "-q:v", "2", still]);
  }

  const image = await loadImage(still);
  const canvas = createCanvas(1080, 1920);
  const ctx = canvas.getContext("2d");
  ctx.clearRect(0, 0, 1080, 1920);

  // A translucent scrim lets the moving unboxing remain visible around the
  // canonical still instead of replacing it with a static slide.
  ctx.fillStyle = "rgba(10,12,16,.34)";
  ctx.fillRect(0, 0, 1080, 1920);
  ctx.fillStyle = "rgba(6,8,12,.94)";
  ctx.roundRect(82, 150, 916, 1570, 34);
  ctx.fill();
  ctx.strokeStyle = "#86c9ea";
  ctx.lineWidth = 7;
  ctx.stroke();

  const box = { x: 112, y: 190, w: 856, h: 900 };
  const scale = Math.min(box.w / image.width, box.h / image.height);
  const w = image.width * scale, h = image.height * scale;
  ctx.fillStyle = "#edf2f0";
  ctx.fillRect(box.x, box.y, box.w, box.h);
  ctx.drawImage(image, box.x + (box.w - w) / 2, box.y + (box.h - h) / 2, w, h);

  ctx.textAlign = "left";
  ctx.fillStyle = "#86c9ea";
  ctx.font = "700 28px Arial";
  ctx.fillText(`${String(item.order).padStart(2, "0")} / 10 · SCORES FOR SOCIAL SOFTWARE`, 118, 1165);
  ctx.fillStyle = "#ffffff";
  ctx.font = "700 39px Arial";
  ctx.fillText(item.artist.toUpperCase(), 118, 1225);

  const wrap = (text, maxWidth, font, lineHeight, y, color) => {
    ctx.font = font; ctx.fillStyle = color;
    const words = text.split(/\s+/); let line = "";
    for (const word of words) {
      const test = line ? `${line} ${word}` : word;
      if (ctx.measureText(test).width > maxWidth && line) { ctx.fillText(line, 118, y); y += lineHeight; line = word; }
      else line = test;
    }
    if (line) { ctx.fillText(line, 118, y); y += lineHeight; }
    return y;
  };
  let y = wrap(item.title, 840, "italic 700 53px Arial", 62, 1300, "#f6cd3f");
  y += 24;
  wrap(item.visualDescription, 840, "36px Arial", 47, y, "#ffffff");
  writeFileSync(resolve(OVERLAYS, `${base}.png`), canvas.toBuffer("image/png"));
}

console.log(`wrote ${project.items.length} selects + overlays`);
