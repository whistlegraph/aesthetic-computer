#!/usr/bin/env node
// Master the ten canonical work stills as a 4:5 Instagram multi-swipe set.

import { mkdirSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import sharp from "sharp";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out/carousel");
const SELECTS = resolve(ROOT, "out/selects");
const items = JSON.parse(readFileSync(resolve(ROOT, "index.json"), "utf8")).items;
mkdirSync(OUT, { recursive: true });

const escape = (s) => s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
const files = [
  "01-notepat.jpg", "02-vigil-score.jpg", "03-software-as-a-choreography.jpg",
  "04-sonic-architecture.jpg", "05-cues-for-losing-direction.jpg", "06-line-piece-1.jpg",
  "07-biophoni-a.jpg", "08-a-cosmographic-score-for-folding-back-into-the-kernel.jpg",
  "09-music-for-world-computers.jpg", "10-the-radio-is-an-altar-portal.jpg",
];

for (let i = 0; i < files.length; i++) {
  const item = items[i];
  const meta = await sharp(resolve(SELECTS, files[i])).metadata();
  const horizontal = meta.width / meta.height > 1.2;
  const label = Buffer.from(`<svg width="1080" height="1350" xmlns="http://www.w3.org/2000/svg">
    <style>.a{font:700 31px Arial,sans-serif;letter-spacing:1.2px}.t{font:700 42px Arial,sans-serif}.n{font:700 25px Arial,sans-serif}</style>
    <rect x="0" y="1180" width="1080" height="170" fill="#111" fill-opacity=".88"/>
    <text x="54" y="1232" class="a" fill="#fff">${escape(item.artist.toUpperCase())}</text>
    <text x="54" y="1288" class="t" fill="#fff">${escape(item.title)}</text>
    <rect x="902" y="1210" width="56" height="42" fill="none" stroke="#fff" stroke-width="3"/><text x="930" y="1240" text-anchor="middle" class="n" fill="#fff">SO</text>
    <path d="M958 1231 C974 1216 986 1246 1002 1231" fill="none" stroke="#fff" stroke-width="3"/>
    <rect x="1002" y="1210" width="70" height="42" fill="none" stroke="#fff" stroke-width="3"/><text x="1037" y="1240" text-anchor="middle" class="n" fill="#fff">SOFT</text>
    <text x="1026" y="1310" text-anchor="end" class="n" fill="#fff">${String(i + 1).padStart(2, "0")} / 10</text>
  </svg>`);
  let image = sharp(resolve(SELECTS, files[i])).rotate().toColourspace("srgb").modulate({ saturation: 1.025, brightness: 0.985 }).sharpen({ sigma: 0.55 });
  image = horizontal
    ? image.resize(1080, 1350, { fit: "contain", background: "#f6f3ec" })
    : image.resize(1080, 1350, { fit: "cover", position: "attention" });
  await image.composite([{ input: label, top: 0, left: 0 }]).jpeg({ quality: 94, chromaSubsampling: "4:4:4", progressive: true }).withMetadata({ density: 72 }).toFile(resolve(OUT, files[i]));
}
console.log(`${OUT} (${files.length} slides, 1080x1350)`);
