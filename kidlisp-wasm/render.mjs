#!/usr/bin/env node
// Render KidLisp pieces to PNG via self-contained WASM.

import { readFileSync, mkdirSync } from "fs";
import { basename } from "path";
import sharp from "sharp";
import { Compiler } from "./compiler.mjs";

const OUT_DIR = new URL("./output/", import.meta.url).pathname;
mkdirSync(OUT_DIR, { recursive: true });

const pieces = process.argv.slice(2);
if (pieces.length === 0) pieces.push("hello.lisp");

const WIDTH = 256;
const HEIGHT = 256;

for (const input of pieces) {
  const path = new URL(input, import.meta.url).pathname;
  const source = readFileSync(path, "utf-8");
  const name = basename(input, ".lisp");

  const compiler = new Compiler();
  const wasmBytes = compiler.compile(source);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  instance.exports.paint(WIDTH, HEIGHT, 0);

  const mem = new Uint8Array(instance.exports.memory.buffer);
  const pixels = mem.slice(0, WIDTH * HEIGHT * 4);

  const png = await sharp(Buffer.from(pixels), {
    raw: { width: WIDTH, height: HEIGHT, channels: 4 },
  }).png().toBuffer();

  const outPath = `${OUT_DIR}${name}.png`;
  await sharp(png).toFile(outPath);
  console.log(`${name}.png (${WIDTH}x${HEIGHT}, ${wasmBytes.length}B wasm → ${png.length}B png)`);
}
