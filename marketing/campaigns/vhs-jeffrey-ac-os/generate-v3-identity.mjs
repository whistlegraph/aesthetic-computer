#!/usr/bin/env node

import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import {
  generateKlingMotionControlV3,
  generateSync3Lipsync,
} from "../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const source = resolve(HERE, "source-v3-identity.png");
const driver = resolve(HERE, "jeffrey-ac-os-omnihuman-v1.5.mp4");
const face = resolve(HERE, "../../../slab/menuband/marketing/notepat-launch/refs/jeffrey-face.jpg");
const faceContext = resolve(HERE, "../../../slab/menuband/marketing/notepat-launch/refs/jeffrey-alt.jpg");
const audio = resolve(HERE, "jeffrey-pvc-v2.mp3");
const identityMotion = resolve(HERE, "jeffrey-ac-os-kling-v3-identity.mp4");
const clean = resolve(HERE, "jeffrey-ac-os-clean-v3-identity.mp4");
const final = resolve(HERE, "jeffrey-ac-os-vhs-v3-identity.mp4");

if (!existsSync(identityMotion)) {
  const result = await generateKlingMotionControlV3({
    image: source,
    video: driver,
    faceImage: face,
    faceReferences: [faceContext],
    prompt: "@Element1 is Jeffrey Alan Scudder. Preserve his exact real facial geometry, natural asymmetry, long narrow nose, blue-green eyes, slim jaw, mouth shape, hairline, and uneven medium-brown hair throughout every frame. Transfer only the body movement, head movement, gaze changes, free-hand gestures, timing, and camera movement from the driving video. The source image owns Jeffrey's appearance, clothing, black ThinkPad, readable Aesthetic Computer screen, and the entire community-TV studio. Do not inherit facial identity from the driving video. Keep the laptop rigid, coherent, and supported in his left hand.",
    characterOrientation: "video",
    keepOriginalSound: true,
    outPath: identityMotion,
    label: "vhs-jeffrey-kling-v3-identity",
  });
  if (!result.ok) throw new Error(result.error);
  console.log(`✓ ${identityMotion}`);
}

if (!existsSync(clean)) {
  const result = await generateSync3Lipsync({
    video: identityMotion,
    audio,
    outPath: clean,
    label: "vhs-jeffrey-sync-3-identity",
  });
  if (!result.ok) throw new Error(result.error);
  console.log(`✓ ${clean}`);
}

const videoFilter = [
  "scale=-2:1920:flags=lanczos",
  "pad=1080:1920:(ow-iw)/2:0:black",
  "eq=contrast=1.07:brightness=-0.022:saturation=0.72:gamma=0.95",
  "chromashift=cbh=-4:crh=3",
  "noise=alls=10:allf=t+u",
  "vignette=PI/5",
  "fps=30000/1001",
  "format=yuv420p",
].join(",");

const ffmpeg = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-i", clean,
  "-vf", videoFilter,
  "-af", "highpass=f=90,lowpass=f=7600,acompressor=threshold=-18dB:ratio=2.2:attack=15:release=180",
  "-c:v", "libx264", "-preset", "slow", "-crf", "20",
  "-c:a", "aac", "-b:a", "160k",
  "-movflags", "+faststart",
  final,
], { stdio: "inherit" });

if (ffmpeg.status !== 0) throw new Error(`ffmpeg exited ${ffmpeg.status}`);
console.log(`✓ ${final}`);
