#!/usr/bin/env node
// Build perspective-correct textures for the Metal Menu Band paper-loop renderer.

import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, registerFont } from "canvas";
import { STRIP_MIDIS, loadStripRig, drawStrip, stripKeyRect, stripKeyColor } from "./reel-lib.mjs";

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

// Boundary marks live between lanes. The transparent texture lets the Metal
// paper material remain physically opaque beneath the printed ink.
scoreContext.save();
scoreContext.strokeStyle = "rgba(28,22,40,0.11)";
scoreContext.lineWidth = 2;
scoreContext.setLineDash([4, 12]);
for (const midi of STRIP_MIDIS) {
  const rect = stripKeyRect(rig, midi, { x: 0, w: scoreWidth });
  scoreContext.beginPath();
  scoreContext.moveTo(rect.x, 0);
  scoreContext.lineTo(rect.x, scoreHeight);
  scoreContext.stroke();
}
scoreContext.restore();

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

  scoreContext.save();
  scoreContext.strokeStyle = color[0] * 0.299 + color[1] * 0.587 + color[2] * 0.114 > 155
    ? "rgba(28,22,40,0.75)" : "rgba(255,255,255,0.82)";
  scoreContext.lineWidth = 3;
  scoreContext.setLineDash([5, 12]);
  scoreContext.beginPath();
  scoreContext.moveTo(x + 2, top + 8); scoreContext.lineTo(x + 2, bottom - 8);
  scoreContext.moveTo(x + width - 2, top + 8); scoreContext.lineTo(x + width - 2, bottom - 8);
  scoreContext.stroke();
  scoreContext.restore();

  const label = labels.get(midi);
  const capHeight = Math.min(72, Math.max(34, Math.min(height - 10, width * 0.68)));
  if (label && capHeight >= 32) {
    const capWidth = Math.min(width - 12, capHeight * 0.92);
    const capX = x + (width - capWidth) / 2;
    const capY = Math.max(top + 5, bottom - capHeight - 8);
    scoreContext.fillStyle = "rgba(255,255,255,0.97)";
    scoreContext.strokeStyle = "rgba(27,21,39,0.94)";
    scoreContext.lineWidth = 3;
    roundedRect(scoreContext, capX, capY, capWidth, capHeight, 10);
    scoreContext.fill(); scoreContext.stroke();
    scoreContext.fillStyle = "rgb(25,19,37)";
    scoreContext.font = `900 ${Math.floor(capHeight * 0.68)}px MBSansRounded`;
    scoreContext.textAlign = "center";
    scoreContext.textBaseline = "middle";
    scoreContext.fillText(label, capX + capWidth / 2, capY + capHeight * 0.52);
  }
}
writeFileSync(resolve(outDir, "score.png"), scoreCanvas.toBuffer("image/png"));

const keyboardWidth = 2048;
const keyboardHeight = Math.ceil(keyboardWidth / rig.aspect);
const keyboardCanvas = createCanvas(keyboardWidth, keyboardHeight);
const keyboardContext = keyboardCanvas.getContext("2d");
drawStrip(keyboardContext, rig, [], 0, 0, keyboardWidth);
for (const midi of STRIP_MIDIS) {
  const rect = stripKeyRect(rig, midi, { x: 0, w: keyboardWidth });
  const label = labels.get(midi);
  keyboardContext.fillStyle = "rgba(24,18,36,0.74)";
  keyboardContext.font = `800 ${Math.max(28, Math.min(62, rect.w * 0.48))}px MBSansRounded`;
  keyboardContext.textAlign = "center";
  keyboardContext.textBaseline = "middle";
  keyboardContext.fillText(label, rect.x + rect.w / 2, keyboardHeight * 0.72);
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
