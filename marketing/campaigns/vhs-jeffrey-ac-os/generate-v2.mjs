#!/usr/bin/env node

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateOmniHuman, generateSync3Lipsync } from "../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const source = resolve(HERE, "source-v2-full-body.png");
const scriptPath = resolve(HERE, "script-v2.txt");
const audio = resolve(HERE, "jeffrey-pvc-v2.mp3");
const performance = resolve(HERE, "jeffrey-ac-os-omnihuman-v1.5.mp4");
const clean = resolve(HERE, "jeffrey-ac-os-clean-v2.mp4");
const final = resolve(HERE, "jeffrey-ac-os-vhs-v2.mp4");

if (!existsSync(audio)) {
  const text = readFileSync(scriptPath, "utf8").trim();
  console.log(`→ Jeffrey PVC: ${text}`);
  const response = await fetch("https://aesthetic.computer/api/say", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      from: text,
      provider: "jeffrey",
      voice: "neutral:0",
      speed: 0.98,
    }),
  });
  if (!response.ok) {
    throw new Error(`/api/say ${response.status}: ${(await response.text()).slice(0, 240)}`);
  }
  writeFileSync(audio, Buffer.from(await response.arrayBuffer()));
  console.log(`✓ ${audio}`);
}

if (!existsSync(performance)) {
  const result = await generateOmniHuman({
    image: source,
    audio,
    outPath: performance,
    label: "vhs-jeffrey-omnihuman-v1.5",
  });
  if (!result.ok) throw new Error(result.error);
  console.log(`✓ ${performance}`);
}

if (!existsSync(clean)) {
  const result = await generateSync3Lipsync({
    video: performance,
    audio,
    outPath: clean,
    label: "vhs-jeffrey-sync-3",
  });
  if (!result.ok) throw new Error(result.error);
  console.log(`✓ ${clean}`);
}

const videoFilter = [
  "scale=1092:1092:flags=lanczos",
  "crop=1080:1080:x='6+4*sin(t*11)':y=6",
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
