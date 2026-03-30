#!/usr/bin/env node
// Render animated KidLisp pieces or sonic fixtures to MP4 with a latent hybrid soundtrack.
// Usage: node mp4.mjs <piece.lisp|fixture:name> [frames] [fps] [size] [sample-rate] [min-video-size] [profile] [sound-style]

import { mkdirSync, mkdtempSync, rmSync, statSync, writeFileSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { spawn } from "child_process";
import sharp from "sharp";
import { createSonicFrameEngine, encodeStereoWav } from "./sonic-frame.mjs";
import { getSonicFixture, listSonicFixtures } from "./sonic-fixtures.mjs";
import { hashString, instantiatePiece, loadPiece, renderFrame } from "./runtime.mjs";

const OUT_DIR = new URL("./output/", import.meta.url).pathname;
mkdirSync(OUT_DIR, { recursive: true });

const input = process.argv[2] || "anim.lisp";
const frames = parseInt(process.argv[3], 10) || 120;
const fps = parseInt(process.argv[4], 10) || 30;
const size = parseInt(process.argv[5], 10) || 256;
const sampleRate = parseInt(process.argv[6], 10) || 48000;
const minVideoSize = parseInt(process.argv[7], 10) || 768;
const profile = process.argv[8] || "default";
const soundStyle = process.argv[9] || "default";

function runFfmpeg(args) {
  return new Promise((resolve, reject) => {
    const proc = spawn("ffmpeg", args);
    let stderr = "";

    proc.stderr.on("data", (chunk) => {
      stderr += chunk.toString();
    });

    proc.on("error", reject);
    proc.on("close", (code) => {
      if (code === 0) {
        resolve();
        return;
      }
      reject(new Error(`ffmpeg exited with code ${code}\n${stderr}`));
    });
  });
}

function computeDisplayScale(width, height, minimumSize) {
  const smallestEdge = Math.max(1, Math.min(width, height));
  return Math.max(1, Math.ceil(minimumSize / smallestEdge));
}

async function loadRenderSource(name, frameCount, renderSize) {
  if (name.startsWith("fixture:")) {
    const fixtureName = name.slice("fixture:".length);
    const fixture = getSonicFixture(fixtureName);
    if (!fixture) {
      throw new Error(`Unknown fixture '${fixtureName}'. Available fixtures: ${listSonicFixtures().join(", ")}`);
    }

    const source = `fixture:${fixture.name}`;
    return {
      name: fixture.name,
      source,
      seed: hashString(source),
      wasmBytes: null,
      description: fixture.description,
      render(frameIndex) {
        return fixture.render({ width: renderSize, height: renderSize, frame: frameIndex, frames: frameCount });
      },
    };
  }

  const piece = loadPiece(name);
  const seed = hashString(`${piece.name}:${piece.source}`);
  const { instance, wasmBytes } = await instantiatePiece(piece.source, { seed });
  return {
    name: piece.name,
    source: piece.source,
    seed,
    wasmBytes,
    description: `KidLisp piece ${piece.name}`,
    render(frameIndex) {
      return renderFrame(instance, renderSize, renderSize, frameIndex);
    },
  };
}

const renderSource = await loadRenderSource(input, frames, size);
const sonic = createSonicFrameEngine({
  source: renderSource.source,
  width: size,
  height: size,
  fps,
  sampleRate,
  seed: renderSource.seed,
  style: soundStyle,
});

const displayScale = computeDisplayScale(size, size, minVideoSize);
const displayWidth = size * displayScale;
const displayHeight = size * displayScale;
const ffmpegScaleFilter = [
  `scale=${displayWidth}:${displayHeight}:flags=neighbor`,
  "pad=ceil(iw/2)*2:ceil(ih/2)*2",
].join(",");
const ffmpegAudioFilter = "loudnorm=I=-12:TP=-1.5:LRA=10,alimiter=limit=0.97";

const tempDir = mkdtempSync(join(tmpdir(), `${renderSource.name}-kidlisp-wasm-`));
const leftChunks = [];
const rightChunks = [];
const baseName = [
  renderSource.name,
  soundStyle !== "default" ? soundStyle : null,
  profile === "vscode" ? "vscode" : null,
].filter(Boolean).join(".");
const soundtrackPath = `${OUT_DIR}${baseName}.wav`;
const mp4Path = `${OUT_DIR}${baseName}.mp4`;
const audioCodecArgs = profile === "vscode"
  ? ["-c:a", "libmp3lame", "-ar", String(sampleRate), "-b:a", "192k"]
  : ["-c:a", "aac", "-ar", String(sampleRate), "-b:a", "192k"];

console.log(`${renderSource.description}`);
console.log(`${frames} frames @ ${fps}fps | render ${size}x${size} | video ${displayWidth}x${displayHeight} | ${sampleRate}Hz | profile ${profile} | sound ${soundStyle}`);
if (renderSource.wasmBytes) {
  console.log(`WASM: ${renderSource.wasmBytes.length} bytes`);
}
console.log(`Sonic seed: ${renderSource.seed}`);

try {
  for (let frame = 0; frame < frames; frame += 1) {
    const rgba = renderSource.render(frame);
    const framePath = join(tempDir, `frame-${String(frame).padStart(5, "0")}.png`);

    await sharp(Buffer.from(rgba), {
      raw: { width: size, height: size, channels: 4 },
    }).png().toFile(framePath);

    const sonicFrame = sonic.synthesizeFrame(rgba, frame);
    leftChunks.push(sonicFrame.left);
    rightChunks.push(sonicFrame.right);

    if ((frame + 1) % 30 === 0 || frame === frames - 1) {
      process.stdout.write(`\r  Rendered ${frame + 1}/${frames} frames`);
    }
  }
  console.log();

  writeFileSync(soundtrackPath, encodeStereoWav(leftChunks, rightChunks, sampleRate));
  console.log(`Wrote soundtrack: ${soundtrackPath}`);

  console.log("Encoding MP4 with ffmpeg...");
  await runFfmpeg([
    "-y",
    "-framerate",
    String(fps),
    "-i",
    join(tempDir, "frame-%05d.png"),
    "-i",
    soundtrackPath,
    "-vf",
    ffmpegScaleFilter,
    "-c:v",
    "libx264",
    "-pix_fmt",
    "yuv420p",
    "-movflags",
    "+faststart",
    ...audioCodecArgs,
    "-af",
    ffmpegAudioFilter,
    "-shortest",
    mp4Path,
  ]);

  const wavSize = statSync(soundtrackPath).size;
  const mp4Size = statSync(mp4Path).size;
  console.log(`${baseName}.wav ${(wavSize / 1024).toFixed(1)}KB`);
  console.log(`${baseName}.mp4 ${(mp4Size / 1024).toFixed(1)}KB`);
} finally {
  rmSync(tempDir, { recursive: true, force: true });
}
