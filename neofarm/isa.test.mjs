// isa.test.mjs — the totality proof, run directly: node neofarm/isa.test.mjs
//
// Claim 1 (totality): ANY byte string decodes and executes within budget,
//   producing finite pixels and bounded events. This is what mechanical
//   mutation stands on, so it is fuzzed, not assumed.
// Claim 2 (determinism): same genome → same hash, twice.
// Claim 3 (round-trip): decode → disassemble → assemble → same program.

import { decode, encode, execute, disassemble, assemble, MAX_SETUP, MAX_PIXEL, MAX_BEAT, EVENT_SLOTS } from "./isa.mjs";

let state = 0xace1;
function rng() {
  state ^= state << 13; state ^= state >>> 17; state ^= state << 5;
  state >>>= 0;
  return state / 4294967296;
}

const ROUNDS = 500;
let failures = 0;

for (let round = 0; round < ROUNDS; round += 1) {
  const size = Math.floor(rng() * 2048);
  const bytes = new Uint8Array(size);
  for (let i = 0; i < size; i += 1) bytes[i] = Math.floor(rng() * 256);

  let genome;
  try {
    genome = decode(bytes);
    if (genome.setup.length > MAX_SETUP || genome.pixel.length > MAX_PIXEL || genome.beat.length > MAX_BEAT) {
      throw new Error(`budget exceeded: ${genome.setup.length}/${genome.pixel.length}/${genome.beat.length}`);
    }
    const run = execute(genome, { width: 16, height: 16, frames: 3, beats: 4 });
    for (const value of run.field) {
      if (!Number.isFinite(value) || value < 0 || value > 1) throw new Error(`bad pixel ${value}`);
    }
    if (run.events.length > 4 * EVENT_SLOTS) throw new Error(`event overflow ${run.events.length}`);
    const again = execute(genome, { width: 16, height: 16, frames: 3, beats: 4 });
    if (again.hash !== run.hash) throw new Error(`nondeterministic: ${run.hash} vs ${again.hash}`);
    const rebuilt = assemble(disassemble(genome));
    rebuilt.seed = genome.seed;
    const a = encode(genome), b = encode(rebuilt);
    if (a.length !== b.length || a.some((byte, i) => byte !== b[i])) throw new Error("round-trip drift");
  } catch (error) {
    failures += 1;
    console.error(`round ${round} (${size} bytes): ${error.message}`);
    if (failures > 5) break;
  }
}

if (failures === 0) {
  console.log(`totality: ${ROUNDS}/${ROUNDS} random byte strings ran, deterministic, round-tripped`);
  process.exit(0);
}
console.error(`${failures} failures`);
process.exit(1);
