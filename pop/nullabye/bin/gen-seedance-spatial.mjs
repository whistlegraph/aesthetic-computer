#!/usr/bin/env node
// One-shot Seedance 2.0 reference-video pilot for spatial-sineabye.
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateReferenceShot } from "../../lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out/seedance");
const reference = resolve(OUT, "spatial-ref-00-15.mp4");
const source = resolve(LANE, "out/spatial-sineabye-hrtf.mp4");
const raw = resolve(OUT, "spatial-sineabye-pilot-01-silent.mp4");
const final = resolve(OUT, "spatial-sineabye-pilot-01.mp4");
const promptPath = resolve(LANE, "seedance-spatial-prompt.txt");
const prompt = readFileSync(promptPath, "utf8").trim();

if (!existsSync(reference) || !existsSync(source)) throw new Error("prepare/reference video missing");
if (!existsSync(raw)) {
  const result = await generateReferenceShot({
    videos: [reference], prompt, duration: "15", ratio: "1:1",
    resolution: "720p", tier: "fast", audio: false,
    outPath: raw, label: "spatial-sineabye-pilot",
  });
  if (!result.ok) throw new Error(result.error);
  writeFileSync(`${raw}.illy.json`, JSON.stringify({
    kind: "motion-reference", provider: "fal.ai",
    model: "bytedance/seedance-2.0/fast/reference-to-video",
    reference, promptPath, prompt, duration: 15, resolution: "720p",
    aspectRatio: "1:1", seed: result.seed, generatedAt: new Date().toISOString(),
  }, null, 2) + "\n");
}

const mux = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-i", raw, "-ss", "0", "-t", "15", "-i", source,
  "-map", "0:v:0", "-map", "1:a:0", "-c:v", "copy", "-c:a", "aac",
  "-b:a", "256k", "-shortest", final], { stdio: "inherit" });
if (mux.status !== 0) process.exit(mux.status ?? 1);
console.log(`✓ ${final}`);
