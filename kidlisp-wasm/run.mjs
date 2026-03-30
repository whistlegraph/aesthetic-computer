#!/usr/bin/env node
// KidLisp WASM Runner — Verifiable Visual Compute
//
// The WASM module contains everything: renderer + pixel buffer + piece code.
// This host only provides memory and reads pixels out. Nothing to fake.

import { readFileSync, writeFileSync } from "fs";
import { Compiler } from "./compiler.mjs";

const WIDTH = parseInt(process.argv[3]) || 128;
const HEIGHT = parseInt(process.argv[4]) || 128;

const input = process.argv[2] || "hello.lisp";
const source = readFileSync(new URL(input, import.meta.url).pathname, "utf-8");

console.log(`Compiling ${input}...`);
const compiler = new Compiler();
const wasmBytes = compiler.compile(source);
console.log(`WASM binary: ${wasmBytes.length} bytes (self-contained renderer)`);

// Instantiate — math imports only, rendering is self-contained.
const { instance } = await WebAssembly.instantiate(wasmBytes, {
  math: {
    sin: (x) => Math.fround(Math.sin(x)),
    cos: (x) => Math.fround(Math.cos(x)),
    random: () => Math.fround(Math.random()),
  },
});

console.log(`Running paint(${WIDTH}, ${HEIGHT}, 0)...`);
instance.exports.paint(WIDTH, HEIGHT, 0);

// Read pixels directly from WASM linear memory
const mem = new Uint8Array(instance.exports.memory.buffer);

// ─── Output PPM ─────────────────────────────────────────────────────

const header = `P6\n${WIDTH} ${HEIGHT}\n255\n`;
const ppm = Buffer.alloc(header.length + WIDTH * HEIGHT * 3);
ppm.write(header);
let offset = header.length;
for (let i = 0; i < WIDTH * HEIGHT * 4; i += 4) {
  ppm[offset++] = mem[i];
  ppm[offset++] = mem[i + 1];
  ppm[offset++] = mem[i + 2];
}

const outFile = input.replace(/\.lisp$/, ".ppm");
writeFileSync(new URL(outFile, import.meta.url).pathname, ppm.slice(0, offset));
console.log(`Wrote ${outFile} (${WIDTH}x${HEIGHT})`);

// ─── Terminal Preview ───────────────────────────────────────────────

const PW = Math.min(WIDTH, 64);
const sx = WIDTH / PW;
const sy = (HEIGHT / PW) * 2;

console.log(`\nPreview (${PW} cols):`);
for (let row = 0; row < PW; row++) {
  let line = "";
  for (let col = 0; col < PW; col++) {
    const tx = Math.floor(col * sx);
    const ty = Math.floor(row * sy);
    const by = Math.min(Math.floor(row * sy + sy / 2), HEIGHT - 1);
    const ti = (ty * WIDTH + tx) * 4;
    const bi = (by * WIDTH + tx) * 4;
    line += `\x1b[38;2;${mem[ti]};${mem[ti+1]};${mem[ti+2]};48;2;${mem[bi]};${mem[bi+1]};${mem[bi+2]}m\u2580`;
  }
  process.stdout.write(line + "\x1b[0m\n");
}

// ─── Verify ─────────────────────────────────────────────────────────

// Hash the pixel buffer for verifiability
const { createHash } = await import("crypto");
const pixelData = mem.slice(0, WIDTH * HEIGHT * 4);
const hash = createHash("sha256").update(pixelData).digest("hex").slice(0, 16);
console.log(`\nPixel hash: ${hash}`);
console.log(`Module size: ${wasmBytes.length} bytes | Buffer: ${WIDTH}x${HEIGHT} RGBA`);
