#!/usr/bin/env node
// sheet.mjs — a 4×3 population contact sheet, farm-wall style.
//
//   node neofarm/sheet.mjs [out.ppm]
//
// Tile 0 is seeds/first.lisp; the rest are random byte strings pushed through
// the same gate the daemon will use. The tried/kept ratio it prints is the
// honest number: how much of raw genome space is already alive.

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { assemble, decode, execute } from "./isa.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const output = resolve(here, process.argv[2] || "sheet.ppm");
const COLS = 4, ROWS = 3, TILE = 128, GAP = 6;

let state = 0xfa12;
function rng() {
  state ^= state << 13; state ^= state >>> 17; state ^= state << 5;
  state >>>= 0;
  return state / 4294967296;
}

function gate(run) {
  return run.stats.lumVar > 0.002 && run.stats.temporalDelta > 0.003 && run.stats.lumMean > 0.01;
}

const kept = [assemble(readFileSync(resolve(here, "seeds/first.lisp"), "utf-8"))];
let tried = 0;
while (kept.length < COLS * ROWS && tried < 4000) {
  tried += 1;
  const size = 200 + Math.floor(rng() * 700);
  const bytes = new Uint8Array(size);
  for (let i = 0; i < size; i += 1) bytes[i] = Math.floor(rng() * 256);
  const genome = decode(bytes);
  const probe = execute(genome, { width: 32, height: 32, frames: 5, beats: 4 });
  if (gate(probe)) kept.push(genome);
}
console.log(`population: ${kept.length} organisms (${tried} random genomes tried)`);

const width = COLS * TILE + (COLS + 1) * GAP;
const height = ROWS * TILE + (ROWS + 1) * GAP;
const sheet = new Uint8Array(width * height * 3).fill(9);

kept.forEach((genome, index) => {
  const run = execute(genome, { width: TILE, height: TILE, frames: 12, beats: 8 });
  const ox = GAP + (index % COLS) * (TILE + GAP);
  const oy = GAP + Math.floor(index / COLS) * (TILE + GAP);
  for (let y = 0; y < TILE; y += 1) {
    for (let x = 0; x < TILE; x += 1) {
      const from = (y * TILE + x) * 3;
      const to = ((oy + y) * width + ox + x) * 3;
      for (let c = 0; c < 3; c += 1) sheet[to + c] = Math.round(run.field[from + c] * 255);
    }
  }
  console.log(`  tile ${index}: hash ${run.hash.toString(16).padStart(8, "0")} lum ${run.stats.lumMean.toFixed(3)} Δt ${run.stats.temporalDelta.toFixed(4)} events ${run.stats.eventCount}`);
});

const header = `P6\n${width} ${height}\n255\n`;
const ppm = Buffer.concat([Buffer.from(header), Buffer.from(sheet)]);
writeFileSync(output, ppm);
console.log(`→ ${output}`);
