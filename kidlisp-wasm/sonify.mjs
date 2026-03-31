#!/usr/bin/env node
// Render KidLisp piece to animated WebP + WAV audio via self-contained WASM.
// Usage: node sonify.mjs <piece.lisp> [frames] [fps] [size] [sampleRate]

import { readFileSync, writeFileSync, mkdirSync } from "fs";
import { basename } from "path";
import { execSync } from "child_process";
import sharp from "sharp";
import { Compiler } from "./compiler.mjs";

const OUT_DIR = new URL("./output/", import.meta.url).pathname;
mkdirSync(OUT_DIR, { recursive: true });

const input = process.argv[2] || "roz.lisp";
const FRAMES = parseInt(process.argv[3]) || 300;
const FPS = parseInt(process.argv[4]) || 30;
const SIZE = parseInt(process.argv[5]) || 256;
const SAMPLE_RATE = parseInt(process.argv[6]) || 44100;
const SAMPLES_PER_FRAME = Math.floor(SAMPLE_RATE / FPS);

const path = new URL(input, import.meta.url).pathname;
const source = readFileSync(path, "utf-8");
const name = basename(input, ".lisp");

console.log(`Compiling ${input}...`);
const compiler = new Compiler();
const wasmBytes = compiler.compile(source);
const mathImports = {
  math: {
    sin: (x) => Math.fround(Math.sin(x)),
    cos: (x) => Math.fround(Math.cos(x)),
    random: () => Math.fround(Math.random()),
  },
};
const { instance } = await WebAssembly.instantiate(wasmBytes, mathImports);
console.log(
  `WASM: ${wasmBytes.length} bytes | ${FRAMES} frames @ ${FPS}fps | ${SIZE}x${SIZE} | ${SAMPLE_RATE}Hz`,
);

// Audio buffer offset: w * h * 8 (after pixel + temp buffers)
const audioOffset = SIZE * SIZE * 8;
const totalSamples = FRAMES * SAMPLES_PER_FRAME;
const allAudio = new Float32Array(totalSamples);

// Render frames
const framePngs = [];
for (let f = 0; f < FRAMES; f++) {
  instance.exports.paint(SIZE, SIZE, f);
  instance.exports.sound(SAMPLE_RATE, FPS, f);

  // Read pixels
  const mem = new Uint8Array(instance.exports.memory.buffer);
  const pixels = Buffer.from(mem.slice(0, SIZE * SIZE * 4));
  const png = await sharp(pixels, {
    raw: { width: SIZE, height: SIZE, channels: 4 },
  })
    .png()
    .toBuffer();
  framePngs.push(png);

  // Read audio samples (f32 at audioOffset)
  const audioView = new Float32Array(
    instance.exports.memory.buffer,
    audioOffset,
    SAMPLES_PER_FRAME,
  );
  allAudio.set(audioView, f * SAMPLES_PER_FRAME);

  if ((f + 1) % 30 === 0 || f === FRAMES - 1) {
    process.stdout.write(`\r  Rendered ${f + 1}/${FRAMES} frames`);
  }
}
console.log();

// ─── Write WAV ───────────────────────────────────────────────────────
function writeWav(filename, samples, sampleRate) {
  const numSamples = samples.length;
  const bytesPerSample = 2; // 16-bit PCM
  const dataSize = numSamples * bytesPerSample;
  const buffer = Buffer.alloc(44 + dataSize);

  // RIFF header
  buffer.write("RIFF", 0);
  buffer.writeUInt32LE(36 + dataSize, 4);
  buffer.write("WAVE", 8);

  // fmt chunk
  buffer.write("fmt ", 12);
  buffer.writeUInt32LE(16, 16); // chunk size
  buffer.writeUInt16LE(1, 20); // PCM
  buffer.writeUInt16LE(1, 22); // mono
  buffer.writeUInt32LE(sampleRate, 24);
  buffer.writeUInt32LE(sampleRate * bytesPerSample, 28); // byte rate
  buffer.writeUInt16LE(bytesPerSample, 32); // block align
  buffer.writeUInt16LE(16, 34); // bits per sample

  // data chunk
  buffer.write("data", 36);
  buffer.writeUInt32LE(dataSize, 40);

  // Convert f32 samples to 16-bit PCM
  for (let i = 0; i < numSamples; i++) {
    const s = Math.max(-1, Math.min(1, samples[i]));
    const pcm = Math.round(s * 32767);
    buffer.writeInt16LE(pcm, 44 + i * 2);
  }

  writeFileSync(filename, buffer);
}

const wavPath = `${OUT_DIR}${name}.wav`;
writeWav(wavPath, allAudio, SAMPLE_RATE);
const wavSize = (allAudio.length * 2) / 1024;
console.log(`${name}.wav (${SAMPLE_RATE}Hz mono, ${(totalSamples / SAMPLE_RATE).toFixed(1)}s, ${wavSize.toFixed(0)}KB)`);

// ─── Encode WebP ─────────────────────────────────────────────────────
console.log("Encoding animated WebP...");
const tmpDir = `${OUT_DIR}.frames-${name}`;
mkdirSync(tmpDir, { recursive: true });

for (let f = 0; f < framePngs.length; f++) {
  const framePath = `${tmpDir}/frame-${String(f).padStart(5, "0")}.png`;
  await sharp(framePngs[f]).toFile(framePath);
}

const webpPath = `${OUT_DIR}${name}.webp`;
execSync(
  `ffmpeg -y -framerate ${FPS} -i "${tmpDir}/frame-%05d.png" -loop 0 -lossless 1 "${webpPath}" 2>/dev/null`,
);
execSync(`rm -rf "${tmpDir}"`);

const { statSync } = await import("fs");
const webpSize = statSync(webpPath).size;
console.log(`${name}.webp (${SIZE}x${SIZE}, ${FRAMES} frames, ${(webpSize / 1024).toFixed(1)}KB)`);

// ─── Mux video + audio to mp4 ────────────────────────────────────────
try {
  const mp4Path = `${OUT_DIR}${name}.mp4`;
  const mp4Video = `${OUT_DIR}${name}_video.mp4`;

  // Step 1: encode video-only mp4 from frames
  const frameDir2 = `${OUT_DIR}.frames2-${name}`;
  mkdirSync(frameDir2, { recursive: true });
  for (let f = 0; f < framePngs.length; f++) {
    await sharp(framePngs[f]).toFile(
      `${frameDir2}/frame-${String(f).padStart(5, "0")}.png`,
    );
  }
  execSync(
    `ffmpeg -y -framerate ${FPS} -i "${frameDir2}/frame-%05d.png" -c:v libx264 -pix_fmt yuv420p "${mp4Video}" 2>/dev/null`,
  );
  execSync(`rm -rf "${frameDir2}"`);

  // Step 2: mux video + audio
  execSync(
    `ffmpeg -y -i "${mp4Video}" -i "${wavPath}" -map 0:v -map 1:a -c:v copy -c:a libmp3lame -ar 24000 -ac 2 -b:a 160k -movflags +faststart -shortest "${mp4Path}" 2>/dev/null`,
  );
  execSync(`rm -f "${mp4Video}"`);

  const mp4Size = statSync(mp4Path).size;
  console.log(`${name}.mp4 (video+audio, ${(mp4Size / 1024).toFixed(1)}KB)`);
} catch {
  console.log("(ffmpeg mp4 mux not available, WAV + WebP saved separately)");
}
