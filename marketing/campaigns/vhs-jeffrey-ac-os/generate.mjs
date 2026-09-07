#!/usr/bin/env node

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateSync3Avatar } from "../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const source = resolve(HERE, "source.png");
const scriptPath = resolve(HERE, "script.txt");
const audio = resolve(HERE, "jeffrey-pvc.mp3");
const clean = resolve(HERE, "jeffrey-ac-os-clean.mp4");
const final = resolve(HERE, "jeffrey-ac-os-vhs.mp4");

mkdirSync(HERE, { recursive: true });

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
      speed: 1.02,
    }),
  });
  if (!response.ok) {
    throw new Error(`/api/say ${response.status}: ${(await response.text()).slice(0, 240)}`);
  }
  writeFileSync(audio, Buffer.from(await response.arrayBuffer()));
  console.log(`✓ ${audio}`);
}

if (!existsSync(clean)) {
  const result = await generateSync3Avatar({
    image: source,
    audio,
    outPath: clean,
    label: "vhs-jeffrey-ac-os",
  });
  if (!result.ok) throw new Error(result.error);
  console.log(`✓ ${clean}`);
}

const videoFilter = [
  "scale=540:810:flags=lanczos",
  "crop=532:810:x='4+3*sin(t*11)'",
  "scale=720:1096:flags=bicubic",
  "pad=720:1280:0:92:black",
  "eq=contrast=1.06:brightness=-0.018:saturation=0.76:gamma=0.96",
  "chromashift=cbh=-3:crh=3",
  "noise=alls=9:allf=t+u",
  "vignette=PI/5",
  "fps=30000/1001",
  "format=yuv420p",
].join(",");

const ffmpeg = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-i", clean,
  "-vf", videoFilter,
  "-af", "highpass=f=90,lowpass=f=7800,acompressor=threshold=-18dB:ratio=2.2:attack=15:release=180",
  "-c:v", "libx264", "-preset", "slow", "-crf", "20",
  "-c:a", "aac", "-b:a", "160k",
  "-movflags", "+faststart",
  final,
], { stdio: "inherit" });

if (ffmpeg.status !== 0) throw new Error(`ffmpeg exited ${ffmpeg.status}`);
console.log(`✓ ${final}`);
