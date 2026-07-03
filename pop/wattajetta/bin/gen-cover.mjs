#!/usr/bin/env node
// gen-cover.mjs — the wattajetta cover, following the hellsine gen-illy
// convention: prompt lives in wattajetta.illy.txt, references go to the
// gpt-image-2 EDITS endpoint as images (the water-jet still anchors the
// jet's design; the pals logo is a glyph the model must draw INTO the
// illustration — nothing is ever composited on afterward). Upscales to
// 3000×3000 (DistroKid floor) and embeds into out/wattajetta.mp3.
//
//   node pop/wattajetta/bin/gen-cover.mjs              → out/cover.png (cached)
//   node pop/wattajetta/bin/gen-cover.mjs --force      → regen
//   node pop/wattajetta/bin/gen-cover.mjs --embed-only → just embed into the mp3

import { writeFileSync, readFileSync, existsSync, renameSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, loadImage } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const LANE = resolve(HERE, "..");
const OUT_PATH = resolve(LANE, "out/cover.png");
const MP3_PATH = resolve(LANE, "out/wattajetta.mp3");
const PROMPT_PATH = resolve(LANE, "wattajetta.illy.txt");
const FORCE = process.argv.includes("--force");

const REFS = [
  `${LANE}/assets/water-jet-ref.png`, // the jet design to redraw
  `${LANE}/assets/pals-logo.png`,     // the glyph to hand-pen in the corner
].filter((p) => {
  if (existsSync(p)) return true;
  console.warn(`  ⚠ ref missing, dropping: ${p}`);
  return false;
});

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault))
    for (const line of readFileSync(vault, "utf8").split("\n"))
      if (line.startsWith("OPENAI_API_KEY="))
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("OPENAI_API_KEY not set and not found in vault");
}

function embedCover() {
  if (!existsSync(OUT_PATH) || !existsSync(MP3_PATH)) {
    console.warn(`  ⚠ skip embed — need both ${OUT_PATH} and ${MP3_PATH}`);
    return;
  }
  const tmp = `${MP3_PATH}.embed.mp3`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", MP3_PATH, "-i", OUT_PATH,
    "-map", "0:a", "-map", "1:v", "-c", "copy", "-id3v2_version", "3",
    "-metadata:s:v", "title=cover", "-metadata:s:v", "comment=Cover (front)",
    "-disposition:v", "attached_pic", tmp], { stdio: "inherit" });
  if (r.status !== 0 || !existsSync(tmp)) { console.error("✗ embed failed"); return; }
  renameSync(tmp, MP3_PATH);
  console.log(`✓ cover embedded → ${MP3_PATH.replace(REPO + "/", "")}`);
}

if (process.argv.includes("--embed-only")) { embedCover(); process.exit(0); }
if (existsSync(OUT_PATH) && !FORCE) {
  console.log(`✓ cached cover → ${OUT_PATH.replace(REPO + "/", "")} (use --force to regen)`);
  embedCover();
  process.exit(0);
}

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
console.log(`▸ wattajetta cover · 1024x1024 · ${REFS.length} refs`);

const fd = new FormData();
fd.append("model", "gpt-image-2");
fd.append("prompt", prompt);
fd.append("size", "1024x1024");
fd.append("quality", "high");
fd.append("n", "1");
for (const ref of REFS) {
  const buf = readFileSync(ref);
  fd.append("image[]", new Blob([buf], { type: "image/png" }), ref.split("/").pop());
}
const res = await fetch("https://api.openai.com/v1/images/edits", {
  method: "POST",
  headers: { Authorization: `Bearer ${apiKey}` },
  body: fd,
});
const body = await res.json();
if (!res.ok) { console.error(`✗ ${JSON.stringify(body).slice(0, 300)}`); process.exit(1); }
const png = Buffer.from(body.data[0].b64_json, "base64");

// upscale to 3000×3000 — nothing composited; the prompt owns the page
const img = await loadImage(png);
const c = createCanvas(3000, 3000);
const ctx = c.getContext("2d");
ctx.imageSmoothingQuality = "high";
ctx.drawImage(img, 0, 0, 3000, 3000);
writeFileSync(OUT_PATH, c.toBuffer("image/png"));
console.log(`✓ ${OUT_PATH.replace(REPO + "/", "")}`);
embedCover();
