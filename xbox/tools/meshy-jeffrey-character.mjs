#!/usr/bin/env node
// Build a detailed, textured, rigged Jeffrey character from the canonical
// platter modeling plate. Meshy v6 multi-image returns the base mesh plus its
// standard walk/run clips so Xbox can switch animation with locomotion.

import { existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { basename, dirname, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import sharp from "sharp";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const defaultPlate = resolve(REPO,
  "../../thespianjas/assets/versions/v002/refs/jeffrey-modeling-plate.png");
const plate = resolve(process.argv[2] || defaultPlate);
const outputDir = resolve(process.argv[3] || resolve(REPO, "xbox/assets/jeffrey-meshy-v6"));
const targetPolycount = Math.max(1000, Math.min(100000,
  Number.parseInt(process.argv[4] || "50000", 10) || 50000));
const queuePath = resolve(outputDir, "queue.json");
const resultPath = resolve(outputDir, "result.json");
const refsDir = resolve(outputDir, "refs");
const endpoint = "fal-ai/meshy/v6/multi-image-to-3d";

if (!existsSync(plate)) throw new Error(`modeling plate not found: ${plate}`);
const key = process.env.FAL_KEY;
if (!key) throw new Error("FAL_KEY must be provided through the environment");
mkdirSync(refsDir, { recursive: true });

const metadata = await sharp(plate).metadata();
const halfWidth = Math.floor(metadata.width / 2);
const halfHeight = Math.floor(metadata.height / 2);
const views = [
  ["front", 0, 0],
  ["profile", halfWidth, 0],
  ["three-quarter", 0, halfHeight],
  ["full-body", halfWidth, halfHeight],
];

for (const [name, left, top] of views) {
  const path = resolve(refsDir, `${name}.png`);
  await sharp(plate)
    .extract({ left, top, width: halfWidth, height: halfHeight })
    .resize(1024, 1024, { fit: "contain", background: "#efede8" })
    .png({ compressionLevel: 9 })
    .toFile(path);
}

const dataUri = (path) => {
  const extension = extname(path).slice(1).toLowerCase();
  return `data:image/${extension === "jpg" ? "jpeg" : extension};base64,${readFileSync(path).toString("base64")}`;
};
const auth = { Authorization: `Key ${key}`, "Content-Type": "application/json" };
const sleep = (milliseconds) => new Promise((accept) => setTimeout(accept, milliseconds));
const input = {
  image_urls: views.map(([name]) => dataUri(resolve(refsDir, `${name}.png`))),
  topology: "triangle",
  target_polycount: targetPolycount,
  symmetry_mode: "auto",
  should_remesh: true,
  should_texture: true,
  enable_pbr: true,
  texture_prompt:
    "identity-faithful Jeffrey Alan Scudder portrait from the supplied photographic views; " +
    "natural warm skin, green eyes, medium brown ear-length hair with individual locks, " +
    "white pinstripe overshirt, peach t-shirt, cropped blue trousers, realistic matte fabric",
  enable_rigging: true,
  rigging_height_meters: 1.82,
  enable_safety_checker: true,
};

let queue = existsSync(queuePath) ? JSON.parse(readFileSync(queuePath, "utf8")) : null;
if (!queue) {
  console.log(`Meshy v6 multi-image: ${basename(plate)} -> ${outputDir}`);
  const response = await fetch(`https://queue.fal.run/${endpoint}`, {
    method: "POST", headers: auth, body: JSON.stringify(input),
  });
  if (!response.ok) throw new Error(`submit ${response.status}: ${(await response.text()).slice(0, 400)}`);
  queue = await response.json();
  writeFileSync(queuePath, JSON.stringify(queue, null, 2));
} else {
  console.log(`Resuming Meshy queue ${queue.request_id || "job"}`);
}

const started = Date.now();
let lastStatus = "";
for (;;) {
  const response = await fetch(queue.status_url, { headers: auth });
  if (!response.ok) throw new Error(`status ${response.status}: ${(await response.text()).slice(0, 300)}`);
  const status = await response.json();
  if (status.status !== lastStatus) {
    lastStatus = status.status;
    console.log(`${lastStatus.toLowerCase()} ${Math.round((Date.now() - started) / 1000)}s`);
  }
  if (lastStatus === "COMPLETED") break;
  if (lastStatus === "FAILED" || status.error)
    throw new Error(`Meshy failed: ${JSON.stringify(status).slice(0, 500)}`);
  await sleep(4000);
}

const response = await fetch(queue.response_url, { headers: auth });
if (!response.ok) throw new Error(`result ${response.status}: ${(await response.text()).slice(0, 300)}`);
const result = await response.json();
writeFileSync(resultPath, JSON.stringify({
  source: "thespianjas/assets/versions/v002/refs/jeffrey-modeling-plate.png",
  endpoint,
  generated: new Date().toISOString(),
  input: { ...input, image_urls: views.map(([name]) => `refs/${name}.png`) },
  result,
}, null, 2));

const downloads = new Map();
const add = (name, file) => { if (file?.url) downloads.set(name, file.url); };
add("character.glb", result.model_glb || result.model_urls?.glb);
add("character-rigged.glb", result.rigged_character_glb);
add("character-animated.glb", result.animation_glb);
add("preview.png", result.thumbnail);
add("walk.glb", result.basic_animations?.walking_glb);
add("walk-armature.glb", result.basic_animations?.walking_armature_glb);
add("run.glb", result.basic_animations?.running_glb);
add("run-armature.glb", result.basic_animations?.running_armature_glb);
for (const [index, texture] of (result.texture_urls || []).entries()) {
  add(`texture-${index}-base.png`, texture.base_color);
  add(`texture-${index}-normal.png`, texture.normal);
  add(`texture-${index}-metallic.png`, texture.metallic);
  add(`texture-${index}-roughness.png`, texture.roughness);
}

for (const [name, url] of downloads) {
  const download = await fetch(url);
  if (!download.ok) throw new Error(`download ${name}: ${download.status}`);
  const bytes = Buffer.from(await download.arrayBuffer());
  writeFileSync(resolve(outputDir, name), bytes);
  console.log(`${name} ${(bytes.length / 1024 / 1024).toFixed(2)} MiB`);
}
unlinkSync(queuePath);
console.log(`done ${downloads.size} artifacts`);
