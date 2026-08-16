#!/usr/bin/env node
// daemon.mjs — the farmer. Runs forever; one tick = a litter of candidates
// pushed through the gate, survivors admitted by novelty crowding.
//
//   node neofarm/daemon.mjs            # foreground, logs to stdout
//   NEOFARM_STATE=/path node ...       # override state dir
//
// All mutation is at the bytecode level, where isa.mjs guarantees totality —
// a child can be ugly but it cannot be broken. Everything the daemon decides
// is written down: status.json every tick (the menubar reads it), a lineage
// line per admission, population saved on interval and on SIGTERM/SIGINT.

import { readFileSync, writeFileSync, appendFileSync, mkdirSync, readdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { hostname } from "node:os";
import { assemble, decode, encode, execute, OPS } from "./isa.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const STATE = process.env.NEOFARM_STATE || resolve(process.env.HOME, ".local/share/neofarm");
mkdirSync(STATE, { recursive: true });
mkdirSync(resolve(STATE, "digests"), { recursive: true });

// Knobs the poker may turn, each clamped to its declared bounds on load.
const KNOB_BOUNDS = {
  tickMs: [250, 60000], triesPerTick: [1, 32], populationCap: [8, 256],
  immigrantRate: [0, 0.5], mutationsPerChild: [1, 8],
  minLumMean: [0.001, 0.2], minLumVar: [0.0005, 0.05],
  minTemporalDelta: [0.0005, 0.1], maxSpatialDelta: [0.05, 0.5],
};
const knobsPath = resolve(STATE, "knobs.json");
const knobs = {
  tickMs: 1000, triesPerTick: 8, populationCap: 64,
  immigrantRate: 0.12, mutationsPerChild: 3,
  minLumMean: 0.01, minLumVar: 0.002, minTemporalDelta: 0.003, maxSpatialDelta: 0.22,
};
if (existsSync(knobsPath)) {
  try {
    for (const [key, value] of Object.entries(JSON.parse(readFileSync(knobsPath, "utf-8")))) {
      const bounds = KNOB_BOUNDS[key];
      if (bounds && typeof value === "number") knobs[key] = Math.min(bounds[1], Math.max(bounds[0], value));
    }
  } catch (error) {
    console.log(`knobs.json unreadable, defaults kept: ${error.message}`);
  }
} else {
  writeFileSync(knobsPath, JSON.stringify(knobs, null, 2));
}

let rngState = (Date.now() ^ process.pid) >>> 0 || 1;
function rng() {
  rngState ^= rngState << 13; rngState ^= rngState >>> 17; rngState ^= rngState << 5;
  rngState >>>= 0;
  return rngState / 4294967296;
}

// ── gate: the probe run plus the coherence measure the sheet demanded ───────

function spatialDelta(run) {
  const { field, width, height } = run;
  let delta = 0, samples = 0;
  for (let y = 0; y < height - 1; y += 1) {
    for (let x = 0; x < width - 1; x += 1) {
      const at = (y * width + x) * 3;
      for (let c = 0; c < 3; c += 1) {
        delta += Math.abs(field[at + c] - field[at + 3 + c]);
        delta += Math.abs(field[at + c] - field[at + width * 3 + c]);
      }
      samples += 6;
    }
  }
  return delta / samples;
}

function probe(genome) {
  const run = execute(genome, { width: 32, height: 32, frames: 5, beats: 4 });
  const spatial = spatialDelta(run);
  const { lumMean, lumVar, temporalDelta, eventCount } = run.stats;
  const pass = lumMean > knobs.minLumMean && lumVar > knobs.minLumVar
    && temporalDelta > knobs.minTemporalDelta && spatial < knobs.maxSpatialDelta;
  return {
    pass, hash: run.hash,
    descriptor: [lumMean, Math.sqrt(lumVar) * 3, temporalDelta * 5, spatial * 3, eventCount / 32],
  };
}

// ── reproduction, all on decoded structure (totality holds regardless) ──────

function randomGenome() {
  const bytes = new Uint8Array(200 + Math.floor(rng() * 700));
  for (let i = 0; i < bytes.length; i += 1) bytes[i] = Math.floor(rng() * 256);
  return decode(bytes);
}

function mutate(parent) {
  const child = structuredClone(parent);
  for (let n = 0; n < knobs.mutationsPerChild; n += 1) {
    const sections = ["setup", "pixel", "beat"];
    const section = child[sections[Math.floor(rng() * 3)]];
    if (section.length === 0) continue;
    const instr = section[Math.floor(rng() * section.length)];
    const move = rng();
    if (move < 0.3) instr.op = Math.floor(rng() * OPS.length);
    else if (move < 0.5) instr.dst = Math.floor(rng() * 16);
    else if (move < 0.7) instr.a = Math.floor(rng() * 32);
    else if (move < 0.85) instr.b = Math.floor(rng() * 32);
    else instr.imm = Math.fround(instr.imm + (rng() - 0.5) * 2) || Math.fround((rng() - 0.5) * 4);
  }
  if (rng() < 0.1) child.seed = (child.seed + 1 + Math.floor(rng() * 1e6)) >>> 0;
  return decode(encode(child)); // renormalize through the codec
}

function crossover(a, b) {
  const child = structuredClone(a);
  for (const section of ["setup", "pixel", "beat"]) {
    if (rng() < 0.5 && b[section].length > 0) {
      const from = Math.floor(rng() * b[section].length);
      const span = 1 + Math.floor(rng() * (b[section].length - from));
      const at = Math.floor(rng() * (child[section].length + 1));
      child[section].splice(at, Math.floor(rng() * span), ...structuredClone(b[section].slice(from, from + span)));
    }
  }
  return decode(encode(child));
}

// ── population: novelty crowding over the descriptor space ──────────────────

const popPath = resolve(STATE, "population.json");
let population = [];
if (existsSync(popPath)) {
  population = JSON.parse(readFileSync(popPath, "utf-8")).map((entry) => ({
    ...entry, genome: decode(Uint8Array.from(Buffer.from(entry.bytes, "base64"))),
  }));
  console.log(`resumed population of ${population.length}`);
} else {
  for (const file of readdirSync(resolve(here, "seeds")).filter((f) => f.endsWith(".lisp"))) {
    const genome = assemble(readFileSync(resolve(here, "seeds", file), "utf-8"));
    const seed = probe(genome);
    population.push({ genome, hash: seed.hash, descriptor: seed.descriptor, born: Date.now(), origin: `seed:${file}` });
  }
  console.log(`seeded population of ${population.length}`);
}

function distance(a, b) {
  let sum = 0;
  for (let i = 0; i < a.length; i += 1) sum += (a[i] - b[i]) ** 2;
  return Math.sqrt(sum);
}

function admit(candidate) {
  const nearest = population.reduce(
    (best, member) => {
      const d = distance(candidate.descriptor, member.descriptor);
      return d < best.d ? { d, member } : best;
    },
    { d: Infinity, member: null },
  );
  if (nearest.d < 0.01) return false; // behavioral duplicate
  if (population.length < knobs.populationCap) {
    population.push(candidate);
    return true;
  }
  // Full house: the most redundant resident (smallest mean distance to the
  // rest) makes way, but only if the newcomer is less redundant than they are.
  let mostRedundant = null, lowestMean = Infinity;
  for (const member of population) {
    let mean = 0;
    for (const other of population) mean += distance(member.descriptor, other.descriptor);
    mean /= population.length - 1;
    if (mean < lowestMean) { lowestMean = mean; mostRedundant = member; }
  }
  let candidateMean = 0;
  for (const member of population) candidateMean += distance(candidate.descriptor, member.descriptor);
  candidateMean /= population.length;
  if (candidateMean <= lowestMean) return false;
  population[population.indexOf(mostRedundant)] = candidate;
  return true;
}

function save() {
  writeFileSync(popPath, JSON.stringify(population.map((member) => ({
    bytes: Buffer.from(encode(member.genome)).toString("base64"),
    hash: member.hash, descriptor: member.descriptor, born: member.born, origin: member.origin,
  }))));
}

// ── the tick ────────────────────────────────────────────────────────────────

let tick = 0, tried = 0, kept = 0;

function latestDigest() {
  const files = readdirSync(resolve(STATE, "digests")).filter((f) => f.endsWith(".md")).sort();
  return files.length ? resolve(STATE, "digests", files[files.length - 1]) : undefined;
}

function writeStatus() {
  writeFileSync(resolve(STATE, "status.json"), JSON.stringify({
    host: hostname().replace(/\.local$/, ""), population: population.length, queue: 0,
    tick, tried, kept, births: kept, lastTick: Date.now() / 1000, digest: latestDigest(),
  }, null, 2));
}

function step() {
  tick += 1;
  for (let n = 0; n < knobs.triesPerTick; n += 1) {
    tried += 1;
    let genome, origin, parents;
    const move = rng();
    if (move < knobs.immigrantRate || population.length === 0) {
      genome = randomGenome(); origin = "immigrant"; parents = [];
    } else if (move < knobs.immigrantRate + 0.5 && population.length >= 2) {
      const a = population[Math.floor(rng() * population.length)];
      const b = population[Math.floor(rng() * population.length)];
      genome = mutate(crossover(a.genome, b.genome)); origin = "cross"; parents = [a.hash, b.hash];
    } else {
      const parent = population[Math.floor(rng() * population.length)];
      genome = mutate(parent.genome); origin = "mutant"; parents = [parent.hash];
    }
    const result = probe(genome);
    if (!result.pass) continue;
    const candidate = { genome, hash: result.hash, descriptor: result.descriptor, born: Date.now(), origin };
    if (admit(candidate)) {
      kept += 1;
      appendFileSync(resolve(STATE, "lineage.jsonl"), JSON.stringify({
        t: Date.now(), hash: result.hash, origin, parents,
        descriptor: result.descriptor.map((v) => Number(v.toFixed(4))),
        size: encode(genome).length,
      }) + "\n");
      console.log(`birth ${result.hash.toString(16).padStart(8, "0")} (${origin}) pop ${population.length}`);
    }
  }
  writeStatus();
  if (tick % 20 === 0) save();
  if (tick % 50 === 0) {
    console.log(`tick ${tick}: ${kept}/${tried} kept, population ${population.length}`);
  }
}

for (const signal of ["SIGTERM", "SIGINT"]) {
  process.on(signal, () => {
    save(); writeStatus();
    console.log(`${signal}: population saved at tick ${tick}`);
    process.exit(0);
  });
}

console.log(`neofarm daemon on ${hostname()} · state ${STATE} · tick ${knobs.tickMs}ms × ${knobs.triesPerTick} tries`);
writeStatus();
setInterval(step, knobs.tickMs);
