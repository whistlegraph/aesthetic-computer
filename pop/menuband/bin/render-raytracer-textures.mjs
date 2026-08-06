#!/usr/bin/env node
// Build perspective-correct textures for the Metal Menu Band paper-loop renderer.

import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, registerFont } from "canvas";
import { STRIP_MIDIS, loadStripRig, stripKeyRect, stripKeyColor } from "./reel-lib.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const arg = (flag, fallback = null) => {
  const index = process.argv.indexOf(flag);
  return index >= 0 && process.argv[index + 1] ? process.argv[index + 1] : fallback;
};
const manifestPath = resolve(process.cwd(), arg("--manifest", "pop/menuband/waltzes/menu-band-waltzes.json"));
const id = arg("--id", "01-lantern");
const outDir = resolve(process.cwd(), arg("--out-dir", `pop/menuband/raytracer/assets/${id}`));
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const entry = manifest.variations.find((item) => item.id === id);
if (!entry) throw new Error(`unknown waltz: ${id}`);
const baseDir = resolve(dirname(manifestPath), manifest.baseDir || manifest.defaults?.baseDir || ".");
const scorePath = resolve(baseDir, entry.notesPath);
const score = JSON.parse(readFileSync(scorePath, "utf8"));
const duration = Number(entry.durationSec ?? manifest.defaults?.durationSec ?? score.durationSec);
const labels = new Map([
  [60, "C"], [62, "D"], [64, "E"], [65, "F"], [67, "G"], [69, "A"], [71, "B"],
  [72, "H"], [74, "I"], [76, "J"], [77, "K"], [79, "L"], [81, "M"], [83, "N"],
]);
try { registerFont("/System/Library/Fonts/SFNSRounded.ttf", { family: "MBSansRounded" }); } catch {}

const rig = await loadStripRig();
mkdirSync(outDir, { recursive: true });

const scoreWidth = 2048;
const scoreHeight = Math.ceil(duration * 140);
const scoreCanvas = createCanvas(scoreWidth, scoreHeight);
const scoreContext = scoreCanvas.getContext("2d");
scoreContext.clearRect(0, 0, scoreWidth, scoreHeight);

// The score texture is empty except for notes. The Metal renderer uses its
// alpha as physical occupancy, so the loop itself stays transparent.

for (const note of score.notes || []) {
  const midi = Number(note.displayMidi ?? note.keyMidi ?? note.visualMidi ?? note.midi);
  if (!STRIP_MIDIS.includes(midi)) continue;
  const rect = stripKeyRect(rig, midi, { x: 0, w: scoreWidth });
  const inset = Math.max(5, rect.w * 0.045);
  const x = rect.x + inset;
  const width = Math.max(16, rect.w - inset * 2);
  const bottom = scoreHeight - Number(note.t) / duration * scoreHeight;
  const top = scoreHeight - (Number(note.t) + Number(note.dur || 0.2)) / duration * scoreHeight;
  const height = Math.max(20, bottom - top);
  const color = stripKeyColor(rig, midi);
  scoreContext.fillStyle = `rgba(${color[0]},${color[1]},${color[2]},0.98)`;
  roundedRect(scoreContext, x, top, width, height, Math.min(14, width * 0.12));
  scoreContext.fill();

  const label = labels.get(midi);
  const labelSize = Math.min(68, Math.max(30, Math.min(height * 0.52, width * 0.54)));
  if (label && labelSize >= 30) {
    const luminance = color[0] * 0.299 + color[1] * 0.587 + color[2] * 0.114;
    scoreContext.fillStyle = luminance > 155 ? "rgba(25,19,37,0.94)" : "rgba(255,255,255,0.96)";
    scoreContext.font = `900 ${Math.floor(labelSize)}px MBSansRounded`;
    scoreContext.textAlign = "center";
    scoreContext.textBaseline = "middle";
    scoreContext.fillText(label, x + width / 2, Math.max(top + labelSize * 0.65, bottom - labelSize * 0.72));
  }
}
writeFileSync(resolve(outDir, "score.png"), scoreCanvas.toBuffer("image/png"));

const keyboardWidth = 2048;
const keyboardHeight = Math.ceil(keyboardWidth / rig.aspect);
const keyboardCanvas = createCanvas(keyboardWidth, keyboardHeight);
const keyboardContext = keyboardCanvas.getContext("2d");
keyboardContext.clearRect(0, 0, keyboardWidth, keyboardHeight);
for (const midi of STRIP_MIDIS) {
  const rect = stripKeyRect(rig, midi, { x: 0, w: keyboardWidth });
  const label = labels.get(midi);
  const capHeight = keyboardHeight * 0.52;
  const capWidth = Math.min(rect.w * 0.62, capHeight * 0.88);
  const capX = rect.x + (rect.w - capWidth) / 2;
  const capY = keyboardHeight * 0.24;
  keyboardContext.fillStyle = "rgba(255,255,255,0.92)";
  roundedRect(keyboardContext, capX, capY, capWidth, capHeight, capWidth * 0.22);
  keyboardContext.fill();
  keyboardContext.fillStyle = "rgba(24,18,36,0.96)";
  keyboardContext.font = `900 ${Math.max(28, Math.min(62, rect.w * 0.48))}px MBSansRounded`;
  keyboardContext.textAlign = "center";
  keyboardContext.textBaseline = "middle";
  keyboardContext.fillText(label, rect.x + rect.w / 2, capY + capHeight * 0.52);
}
writeFileSync(resolve(outDir, "keyboard.png"), keyboardCanvas.toBuffer("image/png"));

writeFileSync(resolve(outDir, "scene.json"), JSON.stringify({
  id, duration, scoreWidth, scoreHeight, keyboardWidth, keyboardHeight,
  scorePath: scorePath.replace(`${REPO}/`, ""), notes: (score.notes || []).length,
}, null, 2) + "\n");
console.log(`✓ ${id} raytracer textures → ${outDir}`);

function roundedRect(context, x, y, width, height, radius) {
  const r = Math.max(0, Math.min(radius, width / 2, height / 2));
  context.beginPath();
  context.moveTo(x + r, y);
  context.arcTo(x + width, y, x + width, y + height, r);
  context.arcTo(x + width, y + height, x, y + height, r);
  context.arcTo(x, y + height, x, y, r);
  context.arcTo(x, y, x + width, y, r);
  context.closePath();
}
