import { readFileSync } from "fs";
import { basename } from "path";
import { Compiler } from "./compiler.mjs";

export function hashString(text) {
  let hash = 0x811c9dc5;
  for (let i = 0; i < text.length; i += 1) {
    hash ^= text.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193);
  }
  return hash >>> 0;
}

export function createSeededRandom(seed) {
  let state = (seed >>> 0) || 1;
  return () => {
    state ^= state << 13;
    state ^= state >>> 17;
    state ^= state << 5;
    return (state >>> 0) / 0x100000000;
  };
}

export function createMathImports(random = Math.random) {
  return {
    math: {
      sin: (x) => Math.fround(Math.sin(x)),
      cos: (x) => Math.fround(Math.cos(x)),
      random: () => Math.fround(random()),
    },
  };
}

export function loadPiece(input) {
  const path = new URL(input, import.meta.url).pathname;
  const source = readFileSync(path, "utf-8");
  return {
    input,
    path,
    source,
    name: basename(input, ".lisp"),
  };
}

export async function instantiatePiece(source, options = {}) {
  const compiler = new Compiler();
  const wasmBytes = compiler.compile(source);
  const seed = options.seed ?? hashString(source);
  const random = options.random ?? createSeededRandom(seed);
  const { instance } = await WebAssembly.instantiate(wasmBytes, createMathImports(random));
  return { instance, wasmBytes, seed };
}

export function renderFrame(instance, width, height, frame) {
  instance.exports.paint(width, height, frame);
  const byteLength = width * height * 4;
  return new Uint8Array(instance.exports.memory.buffer, 0, byteLength).slice();
}
