#!/usr/bin/env node
// anim.mjs — dump an organism's frames for ffmpeg.
//
//   node neofarm/anim.mjs seeds/first.lisp /tmp/frames 24
//
// execute() is re-run per frame count (progressive prefixes of the same
// deterministic run), so what animates is exactly what the oracle computed.

import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { assemble, decode, execute } from "./isa.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const input = resolve(here, process.argv[2] || "seeds/first.lisp");
const outDir = process.argv[3] || resolve(here, "frames");
const frames = parseInt(process.argv[4] || "24", 10);

const source = readFileSync(input, "utf-8");
const genome = input.endsWith(".lisp") ? assemble(source) : decode(readFileSync(input));
mkdirSync(outDir, { recursive: true });

for (let k = 1; k <= frames; k += 1) {
  const run = execute(genome, { width: 128, height: 128, frames: k, beats: 1 });
  const header = `P6\n128 128\n255\n`;
  const ppm = Buffer.alloc(header.length + run.field.length);
  ppm.write(header);
  run.field.forEach((value, i) => { ppm[header.length + i] = Math.round(value * 255); });
  writeFileSync(`${outDir}/frame-${String(k).padStart(3, "0")}.ppm`, ppm);
}
console.log(`${frames} frames → ${outDir}`);
