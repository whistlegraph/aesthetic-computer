#!/usr/bin/env node
// Render animated KidLisp pieces to WebP via self-contained WASM.
// Usage: node animate.mjs <piece.lisp> [frames] [fps] [size]

import { readFileSync, mkdirSync } from "fs";
import { basename } from "path";
import sharp from "sharp";
import { Compiler } from "./compiler.mjs";

const OUT_DIR = new URL("./output/", import.meta.url).pathname;
mkdirSync(OUT_DIR, { recursive: true });

const input = process.argv[2] || "anim.lisp";
const FRAMES = parseInt(process.argv[3]) || 120;
const FPS = parseInt(process.argv[4]) || 30;
const SIZE = parseInt(process.argv[5]) || 256;

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
console.log(`WASM: ${wasmBytes.length} bytes | ${FRAMES} frames @ ${FPS}fps | ${SIZE}x${SIZE}`);

// Render all frames
const delay = Math.round(1000 / FPS);
const framePngs = [];

for (let f = 0; f < FRAMES; f++) {
  instance.exports.paint(SIZE, SIZE, f);

  const mem = new Uint8Array(instance.exports.memory.buffer);
  const pixels = Buffer.from(mem.slice(0, SIZE * SIZE * 4));

  const png = await sharp(pixels, {
    raw: { width: SIZE, height: SIZE, channels: 4 },
  }).png().toBuffer();

  framePngs.push(png);

  if ((f + 1) % 30 === 0 || f === FRAMES - 1) {
    process.stdout.write(`\r  Rendered ${f + 1}/${FRAMES} frames`);
  }
}
console.log();

// Encode animated WebP
console.log("Encoding animated WebP...");
const outPath = `${OUT_DIR}${name}.webp`;

await sharp(framePngs[0], { animated: true })
  .webp({ quality: 80 })
  .toFile(outPath + ".tmp");

// sharp doesn't do animated WebP natively from frames,
// so use ffmpeg which is available
import { execSync } from "child_process";

// Write frames to temp dir
const tmpDir = `${OUT_DIR}.frames-${name}`;
mkdirSync(tmpDir, { recursive: true });

for (let f = 0; f < framePngs.length; f++) {
  const framePath = `${tmpDir}/frame-${String(f).padStart(5, "0")}.png`;
  await sharp(framePngs[f]).toFile(framePath);
}

execSync(
  `ffmpeg -y -framerate ${FPS} -i "${tmpDir}/frame-%05d.png" -loop 0 -lossless 1 "${outPath}" 2>/dev/null`,
);

// Clean up
execSync(`rm -rf "${tmpDir}" "${outPath}.tmp"`);

const { statSync } = await import("fs");
const size = statSync(outPath).size;
console.log(`${name}.webp (${SIZE}x${SIZE}, ${FRAMES} frames, ${(size / 1024).toFixed(1)}KB)`);
