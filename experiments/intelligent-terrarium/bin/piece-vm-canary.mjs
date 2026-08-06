#!/usr/bin/env node
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { compilePieceLisp, runPieceVm, verifyPieceProgram } from "../src/piece-vm.mjs";

const here = resolve(fileURLToPath(new URL("..", import.meta.url)));
const sourcePath = resolve(here, "examples/piece-vm-canary.lisp");
const outputPath = resolve(process.argv[2] || "./piece-vm-canary.ppm");
const frames = Number(process.argv[3] || 1);
if (!Number.isInteger(frames) || frames < 1 || frames > 256) throw new TypeError("frames must be an integer in 1..256");
const source = await readFile(sourcePath, "utf8");
const program = compilePieceLisp(source, { resolution: 64 });
const proof = verifyPieceProgram(program);
if (!proof.valid) throw new Error(`PieceVM canary failed verification: ${proof.error}`);
let result;
for (let frame = 0; frame < frames; frame += 1) {
  result = runPieceVm(program, { state: result?.state });
  if (result.fault) throw new Error(`PieceVM canary fault on frame ${frame + 1}: ${result.fault}`);
}
const pixels = Buffer.from(result.state.buffers[result.state.front]);
const output = outputPath.endsWith(".rgb") ? pixels : Buffer.concat([Buffer.from(`P6\n${program.resolution} ${program.resolution}\n255\n`), pixels]);
await writeFile(outputPath, output);
console.log(JSON.stringify({ outputPath, bytecodeHash: program.bytecodeHash,
  traceHash: result.traceHash, frontHash: result.frontHash,
  instructions: program.instructionCount, registers: program.registerCount, frames,
  fuelUsed: result.fuelUsed, proof }, null, 2));
