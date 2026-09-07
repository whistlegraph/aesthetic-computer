#!/usr/bin/env node

import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateKlingMotionControlV3 } from "../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const repo = resolve(HERE, "../../..");
const source = resolve(HERE, "source-v3-identity.png");
const driver = resolve(HERE, "jeffrey-ac-os-omnihuman-v1.5.mp4");
const front = resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/front.png");
const threeQuarter = resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/three-quarter.png");
const profile = resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/profile.png");
const fullBody = resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/full-body.png");
const clean = resolve(HERE, "jeffrey-ac-os-kling-v3-multiview.mp4");
const final = resolve(HERE, "jeffrey-ac-os-vhs-v4-multiview.mp4");

if (!existsSync(clean)) {
  const result = await generateKlingMotionControlV3({
    image: source,
    video: driver,
    faceImage: front,
    faceReferences: [threeQuarter, profile, fullBody],
    prompt: "@Element1 is Jeffrey Alan Scudder, one real person defined jointly by the supplied frontal, three-quarter, profile, and full-body photographs. Preserve Jeffrey's exact facial geometry at every angle: natural eye size and spacing, blue-green eyes, long narrow nose and profile, high forehead, slim jaw and chin, ears, mouth shape, facial asymmetry, skin texture, hairline, and uneven medium-brown hair. The source image exclusively defines Jeffrey's clothing, the black ThinkPad and its Aesthetic Computer screen, lighting, framing, and the entire community-TV studio. Transfer only body movement, head orientation, gaze, expression timing, free-hand gestures, weight shift, forward approach, and camera movement from the driving video. Do not inherit the driving video's identity, facial geometry, hair, clothing, laptop, or environment. Keep Jeffrey recognizable and anatomically stable in wide, medium, close, and partial-profile views. Keep one coherent laptop rigidly supported in his left hand. Preserve the source's photographic texture; no beauty filter, face smoothing, generic presenter face, enlarged eyes, widened jaw, shortened nose, duplicated equipment, or malformed hands.",
    characterOrientation: "video",
    keepOriginalSound: true,
    outPath: clean,
    label: "vhs-jeffrey-kling-v3-multiview",
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
