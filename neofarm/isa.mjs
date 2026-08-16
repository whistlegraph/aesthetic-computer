// isa.mjs — the Neofarm machine, normative.
//
// An organism is a bytecode program for this VM, not source text. The design
// property everything rests on: ANY byte string decodes to a runnable genome.
// Unknown opcodes become NOP, register fields wrap, arithmetic is protected,
// non-finite results collapse to 0. Mutation and crossover are therefore
// total — the mechanical loop can never produce a syntax error.
//
// Three sections, three rates:
//   SETUP — once per life; its registers become the globals g0..g15.
//   PIXEL — per pixel over the field, per frame (a shader). r0,r1,r2 → RGB.
//   BEAT  — per musical eighth; AEV emits bounded audio events.
//
// No jumps, no loops, no heap. Cost = instruction count × section rate.
// The only randomness is RND over a PRNG seeded from the genome header —
// same genome, same field, same events, same hash, on any machine.

export const ISA_VERSION = 1;
export const INSTR_BYTES = 8; // op:u8 dst:u8 a:u8 b:u8 imm:f32le
export const REG_COUNT = 16; // writable file; operands 16..31 read inputs
export const MAX_SETUP = 64;
export const MAX_PIXEL = 96;
export const MAX_BEAT = 64;
export const EVENT_SLOTS = 8; // AEV slots per beat
const TAU = Math.PI * 2;

// op name, then how dst is computed from operands A, B and immediate F.
export const OPS = [
  ["nop", () => null],
  ["const", (A, B, F) => F],
  ["mov", (A) => A],
  ["add", (A, B) => A + B],
  ["sub", (A, B) => A - B],
  ["mul", (A, B) => A * B],
  ["div", (A, B) => (B === 0 ? 0 : A / B)],
  ["mod", (A, B) => (B === 0 ? 0 : A - Math.floor(A / B) * B)],
  ["min", (A, B) => Math.min(A, B)],
  ["max", (A, B) => Math.max(A, B)],
  ["abs", (A) => Math.abs(A)],
  ["floor", (A) => Math.floor(A)],
  ["fract", (A) => A - Math.floor(A)],
  ["clamp", (A) => (A < 0 ? 0 : A > 1 ? 1 : A)],
  ["mix", (A, B, F) => A + (B - A) * (F < 0 ? 0 : F > 1 ? 1 : F)],
  ["sel", (A, B, F) => (A > 0 ? B : F)],
  ["sin", (A) => Math.sin(A * TAU)],
  ["cos", (A) => Math.cos(A * TAU)],
  ["tanh", (A) => Math.tanh(A)],
  ["sqrt", (A) => Math.sqrt(Math.abs(A))],
  ["pow", (A, B) => Math.pow(Math.abs(A), B)],
  ["cmplt", (A, B) => (A < B ? 1 : 0)],
  ["cmpgt", (A, B) => (A > B ? 1 : 0)],
  ["rnd", (A, B, F, rnd) => rnd()],
  ["aev", () => null], // audio event; interpreted by the BEAT loop, NOP elsewhere
  ["scale", (A, B, F) => A * F],
  ["off", (A, B, F) => A + F],
];
const OP_AEV = OPS.findIndex(([name]) => name === "aev");

// Input names are per-section so disassembly reads like what it means.
export const INPUTS = {
  setup: ["sf", "z1", "z2", "z3", "z4", "z5", "z6", "z7",
          "z8", "z9", "za", "zb", "zc", "zd", "ze", "zf"],
  pixel: ["x", "y", "beat", "frame", "pr", "pg", "pb", "cd",
          "g0", "g1", "g2", "g3", "g4", "g5", "g6", "g7"],
  beat: ["bi", "bar", "lum", "var", "en", "z1", "z2", "z3",
         "g0", "g1", "g2", "g3", "g4", "g5", "g6", "g7"],
};

// ── genome: header + three instruction lists ────────────────────────────────

const MAGIC = [0x4e, 0x46, 0x30, 0x31]; // "NF01"

export function decode(bytes) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let ok = bytes.length >= 14;
  for (let i = 0; ok && i < 4; i += 1) ok = bytes[i] === MAGIC[i];
  // A byte string that isn't even a genome still becomes one: seed from
  // whatever bytes exist, instructions from the remainder. Totality > ceremony.
  const seed = ok ? view.getUint32(4, true) : fnv(bytes);
  const counts = ok
    ? [view.getUint16(8, true), view.getUint16(10, true), view.getUint16(12, true)]
    : [MAX_SETUP, MAX_PIXEL, MAX_BEAT];
  const body = bytes.subarray(ok ? 14 : 0);
  const caps = [MAX_SETUP, MAX_PIXEL, MAX_BEAT];
  const sections = [];
  let cursor = 0;
  for (let s = 0; s < 3; s += 1) {
    const want = Math.min(counts[s], caps[s]);
    const have = Math.min(want, Math.floor((body.length - cursor) / INSTR_BYTES));
    const list = [];
    for (let i = 0; i < have; i += 1) {
      const at = cursor + i * INSTR_BYTES;
      let op = body[at] % OPS.length;
      const instr = {
        op,
        dst: body[at + 1] % REG_COUNT,
        a: body[at + 2] % 32,
        b: body[at + 3] % 32,
        imm: Math.fround(new DataView(body.buffer, body.byteOffset + at + 4, 4).getFloat32(0, true)),
      };
      if (!Number.isFinite(instr.imm) || instr.imm === 0) instr.imm = 0; // NaN/Inf and -0 normalize
      if (instr.op === 0) { instr.dst = 0; instr.a = 0; instr.b = 0; instr.imm = 0; }
      list.push(instr);
    }
    cursor += have * INSTR_BYTES;
    sections.push(list);
  }
  return { version: ISA_VERSION, seed: seed >>> 0, setup: sections[0], pixel: sections[1], beat: sections[2] };
}

export function encode(genome) {
  const lists = [genome.setup, genome.pixel, genome.beat];
  const total = lists.reduce((n, l) => n + l.length, 0);
  const bytes = new Uint8Array(14 + total * INSTR_BYTES);
  const view = new DataView(bytes.buffer);
  MAGIC.forEach((byte, i) => { bytes[i] = byte; });
  view.setUint32(4, genome.seed >>> 0, true);
  view.setUint16(8, lists[0].length, true);
  view.setUint16(10, lists[1].length, true);
  view.setUint16(12, lists[2].length, true);
  let at = 14;
  for (const list of lists) {
    for (const instr of list) {
      bytes[at] = instr.op; bytes[at + 1] = instr.dst;
      bytes[at + 2] = instr.a; bytes[at + 3] = instr.b;
      view.setFloat32(at + 4, instr.imm, true);
      at += INSTR_BYTES;
    }
  }
  return bytes;
}

// ── deterministic PRNG streams ──────────────────────────────────────────────
// Each section invocation gets its own stream derived from (seed, section,
// invocation index), so results never depend on execution order.

function hash32(x) {
  x = Math.imul(x ^ (x >>> 16), 0x45d9f3b);
  x = Math.imul(x ^ (x >>> 16), 0x45d9f3b);
  return (x ^ (x >>> 16)) >>> 0;
}

function stream(seed, section, invocation) {
  let state = hash32(seed ^ Math.imul(section + 1, 0x9e3779b9) ^ Math.imul(invocation + 1, 0x85ebca6b)) || 1;
  return () => {
    state ^= state << 13; state ^= state >>> 17; state ^= state << 5;
    state >>>= 0;
    return state / 4294967296;
  };
}

function fnv(bytes) {
  let hash = 0x811c9dc5;
  for (const byte of bytes) hash = Math.imul(hash ^ byte, 0x01000193);
  return hash >>> 0;
}

// ── the reference interpreter (the proof oracle) ────────────────────────────

function runSection(list, regs, inputs, rnd, events) {
  for (const instr of list) {
    if (instr.op === OP_AEV) {
      if (events) {
        events.push({
          slot: instr.dst % EVENT_SLOTS,
          freq: read(regs, inputs, instr.a),
          amp: read(regs, inputs, instr.b),
          wave: Math.abs(Math.round(instr.imm)) % 4, // sine square saw noise
        });
      }
      continue;
    }
    const fn = OPS[instr.op][1];
    if (instr.op === 0) continue;
    const value = fn(read(regs, inputs, instr.a), read(regs, inputs, instr.b), instr.imm, rnd);
    regs[instr.dst] = Number.isFinite(value) ? value : 0;
  }
}

function read(regs, inputs, index) {
  return index < REG_COUNT ? regs[index] : inputs[index - REG_COUNT];
}

// Execute a genome for `frames` frames and `beats` eighth-notes on a
// width×height field. Small by default: this is the gate's microscope,
// not a display. Returns the final field, events, stats, and replay hash.
export function execute(genome, { width = 48, height = 48, frames = 8, beats = 8 } = {}) {
  const globals = new Float32Array(REG_COUNT);
  const setupInputs = new Float32Array(16);
  setupInputs[0] = (genome.seed % 1000) / 1000; // sf
  runSection(genome.setup, globals, setupInputs, stream(genome.seed, 0, 0), null);

  let field = new Float32Array(width * height * 3);
  let prev = new Float32Array(width * height * 3);
  const regs = new Float32Array(REG_COUNT);
  const inputs = new Float32Array(16);
  for (let i = 0; i < 8; i += 1) inputs[8 + i] = globals[i];

  let temporalDelta = 0;
  for (let frame = 0; frame < frames; frame += 1) {
    [field, prev] = [prev, field];
    for (let y = 0; y < height; y += 1) {
      for (let x = 0; x < width; x += 1) {
        const invocation = (frame * height + y) * width + x;
        const at = (y * width + x) * 3;
        inputs[0] = x / width;
        inputs[1] = y / height;
        inputs[2] = (frame % 2) / 2 + 0.25; // beat phase at 60 BPM, 2 frames/beat
        inputs[3] = frame / frames;
        inputs[4] = prev[at]; inputs[5] = prev[at + 1]; inputs[6] = prev[at + 2];
        inputs[7] = Math.hypot(inputs[0] - 0.5, inputs[1] - 0.5) * 2;
        regs.fill(0);
        runSection(genome.pixel, regs, inputs, stream(genome.seed, 1, invocation), null);
        for (let c = 0; c < 3; c += 1) {
          const v = regs[c];
          field[at + c] = v < 0 ? 0 : v > 1 ? 1 : v;
        }
      }
    }
    if (frame > 0) {
      let delta = 0;
      for (let i = 0; i < field.length; i += 1) delta += Math.abs(field[i] - prev[i]);
      temporalDelta += delta / field.length;
    }
  }
  temporalDelta /= Math.max(1, frames - 1);

  let lumMean = 0;
  for (let i = 0; i < field.length; i += 3) {
    lumMean += (field[i] + field[i + 1] + field[i + 2]) / 3;
  }
  lumMean /= width * height;
  let lumVar = 0;
  for (let i = 0; i < field.length; i += 3) {
    const lum = (field[i] + field[i + 1] + field[i + 2]) / 3;
    lumVar += (lum - lumMean) ** 2;
  }
  lumVar /= width * height;

  const events = [];
  const beatRegs = new Float32Array(REG_COUNT);
  const beatInputs = new Float32Array(16);
  for (let i = 0; i < 8; i += 1) beatInputs[8 + i] = globals[i];
  let energy = 0;
  for (let beat = 0; beat < beats; beat += 1) {
    beatInputs[0] = beat / 16;
    beatInputs[1] = (beat % 8) / 8;
    beatInputs[2] = lumMean;
    beatInputs[3] = lumVar;
    beatInputs[4] = energy;
    beatRegs.fill(0);
    const before = events.length;
    runSection(genome.beat, beatRegs, beatInputs, stream(genome.seed, 2, beat), events);
    if (events.length - before > EVENT_SLOTS) events.length = before + EVENT_SLOTS;
    energy = events.slice(before).reduce((sum, event) => sum + Math.abs(event.amp), 0);
  }

  const quantized = new Uint8Array(field.length + events.length * 4);
  field.forEach((v, i) => { quantized[i] = Math.round(v * 255); });
  events.forEach((event, i) => {
    const at = field.length + i * 4;
    quantized[at] = event.slot;
    quantized[at + 1] = Math.round(Math.abs(Math.tanh(event.freq)) * 255);
    quantized[at + 2] = Math.round(Math.abs(Math.tanh(event.amp)) * 255);
    quantized[at + 3] = event.wave;
  });

  return {
    field, width, height, events,
    stats: { lumMean, lumVar, temporalDelta, eventCount: events.length },
    hash: fnv(quantized),
  };
}

// ── readable Lisp form (round-trips) ────────────────────────────────────────

function operandName(section, index) {
  return index < REG_COUNT ? `r${index}` : INPUTS[section][index - REG_COUNT];
}

export function disassemble(genome) {
  const lines = [`(neofarm ${ISA_VERSION} :seed ${genome.seed}`];
  for (const section of ["setup", "pixel", "beat"]) {
    lines.push(`  (${section}`);
    for (const instr of genome[section]) {
      const [name] = OPS[instr.op];
      if (instr.op === 0) { lines.push("    (nop)"); continue; }
      const parts = [name, `r${instr.dst}`,
        operandName(section, instr.a), operandName(section, instr.b), String(instr.imm)];
      lines.push(`    (${parts.join(" ")})`);
    }
    lines.push("  )");
  }
  lines.push(")");
  return lines.join("\n");
}

export function assemble(text) {
  const tokens = text.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);
  let at = 0;
  function form() {
    if (tokens[at] !== "(") return tokens[at++];
    at += 1;
    const items = [];
    while (tokens[at] !== ")") items.push(form());
    at += 1;
    return items;
  }
  const root = form();
  if (root[0] !== "neofarm") throw new Error("not a neofarm form");
  const genome = { version: ISA_VERSION, seed: 0, setup: [], pixel: [], beat: [] };
  const seedAt = root.indexOf(":seed");
  if (seedAt !== -1) genome.seed = Number(root[seedAt + 1]) >>> 0;
  for (const item of root) {
    if (!Array.isArray(item) || !INPUTS[item[0]]) continue;
    const section = item[0];
    const names = INPUTS[section];
    for (const instr of item.slice(1)) {
      const op = OPS.findIndex(([name]) => name === instr[0]);
      if (op <= 0) { genome[section].push({ op: 0, dst: 0, a: 0, b: 0, imm: 0 }); continue; }
      const operand = (token) => {
        if (token?.startsWith("r")) return Number(token.slice(1)) % 32;
        const named = names.indexOf(token);
        return named === -1 ? 0 : REG_COUNT + named;
      };
      genome[section].push({
        op,
        dst: Number(instr[1]?.slice(1) || 0) % REG_COUNT,
        a: operand(instr[2]),
        b: operand(instr[3]),
        imm: Math.fround(Number(instr[4] || 0)) || 0,
      });
    }
  }
  return genome;
}
