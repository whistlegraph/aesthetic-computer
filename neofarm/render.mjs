#!/usr/bin/env node
// render.mjs — run one organism at display scale and write a PPM.
//
//   node neofarm/render.mjs seeds/first.lisp [out.ppm]
//
// This is the certification microscope, not the observatory: reference
// interpreter only, so what you see is exactly what the gate judged.

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { assemble, decode, encode, execute, disassemble } from "./isa.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const input = resolve(here, process.argv[2] || "seeds/first.lisp");
const output = resolve(here, process.argv[3] || input.replace(/\.\w+$/, "") + ".ppm");

const source = readFileSync(input, "utf-8");
const genome = input.endsWith(".lisp") ? assemble(source) : decode(readFileSync(input));

const run = execute(genome, { width: 128, height: 128, frames: 24, beats: 16 });

const header = `P6\n${run.width} ${run.height}\n255\n`;
const ppm = Buffer.alloc(header.length + run.field.length);
ppm.write(header);
run.field.forEach((value, i) => { ppm[header.length + i] = Math.round(value * 255); });
writeFileSync(output, ppm);

const size = encode(genome).length;
console.log(`${input.split("/").pop()} → ${output.split("/").pop()}`);
console.log(`bytecode: ${size} bytes · hash ${run.hash.toString(16)}`);
console.log(`stats: lum ${run.stats.lumMean.toFixed(3)} var ${run.stats.lumVar.toFixed(4)} Δt ${run.stats.temporalDelta.toFixed(4)} events ${run.stats.eventCount}`);
for (const event of run.events.slice(0, 8)) {
  console.log(`  beat event slot ${event.slot} freq ${event.freq.toFixed(3)} amp ${event.amp.toFixed(3)} wave ${event.wave}`);
}
