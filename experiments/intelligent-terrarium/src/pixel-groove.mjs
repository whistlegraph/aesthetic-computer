const SIDE = 160;
const FIELD_SIDE = 128;
const MARGIN = 16;
const PIXELS = SIDE * SIDE - FIELD_SIDE * FIELD_SIDE;
const BYTES = PIXELS * 3;

export const GROOVE_LAYOUT = Object.freeze({
  version: 1,
  side: SIDE,
  fieldSide: FIELD_SIDE,
  margin: MARGIN,
  pixels: PIXELS,
  bytes: BYTES,
  tracks: Object.freeze({
    header: Object.freeze({ base: 0, pixels: 64, protected: true }),
    sequence: Object.freeze({ base: 64, pixels: 64, protected: true }),
    functions: Object.freeze({ base: 128, pixels: 64, protected: true }),
    bodies: Object.freeze({ base: 192, pixels: 288, protected: true }),
    projection: Object.freeze({ base: 480, pixels: 48, protected: true }),
    lifecycle: Object.freeze({ base: 528, pixels: 128, protected: true }),
    state: Object.freeze({ base: 656, pixels: 64, protected: false }),
    sprites: Object.freeze({ base: 720, pixels: 4100, protected: false }),
    proposals: Object.freeze({ base: 4820, pixels: 288, protected: false }),
    source: Object.freeze({ base: 5108, pixels: 192, protected: true }),
    fringe: Object.freeze({ base: 5300, pixels: PIXELS - 5300, protected: false }),
  }),
});

export const GROOVE_HEADER = Object.freeze({
  magic: 0,
  version: 4,
  headerBytes: 6,
  totalBytes: 8,
  protectedHash: 12,
  generation: 16,
  instructionCount: 20,
  entrypoint: 22,
  initialPc: 24,
  functionCount: 26,
  lifecycleCount: 28,
  sourceLength: 30,
  id: 32,
  parent: 56,
  createdIteration: 80,
  layoutEnd: 84,
  authorityUtcLow: 88,
  authorityUtcHigh: 92,
  entropySource: 96,
  entropySeed: 100,
  fieldResolution: 104,
  fieldBytes: 108,
  profileCode: 112,
});

export const HARDWARE_PROFILES = Object.freeze({
  quarter: Object.freeze({ name: "quarter", label: "Q", code: 1, resolution: 32, scale: .25 }),
  half: Object.freeze({ name: "half", label: "H", code: 2, resolution: 64, scale: .5 }),
  standard: Object.freeze({ name: "standard", label: "1X", code: 3, resolution: 128, scale: 1 }),
  double: Object.freeze({ name: "double", label: "2X", code: 4, resolution: 256, scale: 2 }),
});

export function hardwareProfile(value = "standard") {
  const profile = typeof value === "number"
    ? Object.values(HARDWARE_PROFILES).find((entry) => entry.resolution === value || entry.code === value)
    : HARDWARE_PROFILES[String(value).toLowerCase()];
  if (!profile) throw new TypeError("hardware profile must be quarter, half, standard, or double");
  return profile;
}

export const GROOVE_STATE = Object.freeze({
  tag: 0,
  pc: 1,
  lifecycleVector: 3,
  probeEpoch: 4,
  failedReprobes: 8,
  sequencePasses: 12,
  projectionGeneration: 16,
  needlePixel: 20,
  flags: 24,
});

export const LIFECYCLE_VECTORS = Object.freeze({
  boot: 0,
  tick: 1,
  reprobe: 2,
  organized: 3,
  graft: 4,
  terminal: 5,
  zero: 6,
  start: 7,
});

export const LIFECYCLE_OPS = Object.freeze({
  return: 0,
  resetPc: 1,
  executeSequence: 2,
  synthesize: 3,
  advanceFringe: 4,
  incrementProbe: 5,
  selectMode: 6,
  applySeedMode: 7,
  resetLife: 8,
  applyOrganized: 9,
  applyGraft: 10,
  incrementFailed: 11,
  callVector: 12,
  zeroRuntime: 13,
});

export const MICRO_OPS = Object.freeze({
  self: 1, arg: 2, x: 3, y: 4, depth: 5, time: 6, energy: 7, constant: 8,
  add: 9, sub: 10, mul: 11, div: 12, xor: 13, and: 14, or: 15,
  sin: 16, cos: 17, tanh: 18, abs: 19, min: 20, max: 21, solar: 22, return: 23,
});

const FUNCTION_NAMES = Object.freeze([
  "add", "xor", "shift", "mix", "solarize", "blur", "edges", "rotate", "mirror",
  "channels", "and", "or", "line", "triangle", "flood", "box", "copy", "paste",
  "cellular",
]);

function assertBuffer(value, length = BYTES) {
  if (!(value instanceof Uint8Array) || value.length !== length) {
    throw new TypeError(`PixelGroove must be exactly ${length} bytes`);
  }
  return value;
}

function byteOffset(track, pixel = 0) {
  const region = GROOVE_LAYOUT.tracks[track];
  if (!region || pixel < 0 || pixel >= region.pixels) throw new RangeError(`invalid groove address ${track}:${pixel}`);
  return (region.base + pixel) * 3;
}

function writeU16(bytes, offset, value) {
  bytes[offset] = value & 255;
  bytes[offset + 1] = (value >>> 8) & 255;
}

function readU16(bytes, offset) {
  return bytes[offset] | (bytes[offset + 1] << 8);
}

function writeU32(bytes, offset, value) {
  bytes[offset] = value & 255;
  bytes[offset + 1] = (value >>> 8) & 255;
  bytes[offset + 2] = (value >>> 16) & 255;
  bytes[offset + 3] = (value >>> 24) & 255;
}

function readU32(bytes, offset) {
  return (bytes[offset] | (bytes[offset + 1] << 8) | (bytes[offset + 2] << 16) | (bytes[offset + 3] << 24)) >>> 0;
}

function writeText(bytes, offset, capacity, value) {
  const encoded = Buffer.from(String(value || ""), "utf8").subarray(0, capacity);
  bytes.fill(0, offset, offset + capacity);
  bytes.set(encoded, offset);
  return encoded.length;
}

function readText(bytes, offset, capacity) {
  const end = bytes.indexOf(0, offset);
  return Buffer.from(bytes.subarray(offset, end < 0 || end > offset + capacity ? offset + capacity : end)).toString("utf8");
}

export function grooveCoordinates() {
  const coordinates = [];
  for (let ring = 0; ring < MARGIN; ring += 1) {
    const low = ring, high = SIDE - 1 - ring;
    for (let x = low; x <= high; x += 1) coordinates.push([x, low]);
    for (let y = low + 1; y <= high; y += 1) coordinates.push([high, y]);
    for (let x = high - 1; x >= low; x -= 1) coordinates.push([x, high]);
    for (let y = high - 1; y > low; y -= 1) coordinates.push([low, y]);
  }
  if (coordinates.length !== PIXELS) throw new Error(`invalid groove geometry: ${coordinates.length}`);
  return coordinates;
}

const COORDINATES = grooveCoordinates();

function protectedHash(bytes) {
  let value = 2166136261;
  const headerEnd = GROOVE_LAYOUT.tracks.header.pixels * 3;
  for (let offset = 0; offset < headerEnd; offset += 1) {
    const byte = offset >= GROOVE_HEADER.protectedHash && offset < GROOVE_HEADER.protectedHash + 4 ? 0 : bytes[offset];
    value ^= byte;
    value = Math.imul(value, 16777619) >>> 0;
  }
  for (const [name, track] of Object.entries(GROOVE_LAYOUT.tracks)) {
    if (!track.protected || name === "header") continue;
    const begin = track.base * 3, end = begin + track.pixels * 3;
    for (let offset = begin; offset < end; offset += 1) {
      value ^= bytes[offset];
      value = Math.imul(value, 16777619) >>> 0;
    }
  }
  return value >>> 0;
}

function writeMicroBody(bytes, bodyIndex, operations) {
  if (bodyIndex < 0 || bodyIndex >= 18) throw new RangeError("invalid function body");
  for (let index = 0; index < 16; index += 1) {
    const at = byteOffset("bodies", bodyIndex * 16 + index);
    const cell = operations[index] || { op: MICRO_OPS.return, literal: 0 };
    bytes[at] = 0x90;
    bytes[at + 1] = cell.op;
    bytes[at + 2] = Math.max(-128, Math.min(127, Math.round((cell.literal || 0) * 64))) & 255;
  }
}

function writeProjectionBody(bytes, axis, operations) {
  for (let index = 0; index < 16; index += 1) {
    const at = byteOffset("projection", axis * 16 + index);
    const cell = operations[index] || { op: MICRO_OPS.return, literal: 0 };
    bytes[at] = 0x90;
    bytes[at + 1] = cell.op;
    bytes[at + 2] = Math.max(-128, Math.min(127, Math.round((cell.literal || 0) * 64))) & 255;
  }
}

function op(...operations) {
  return operations.map((entry) => typeof entry === "number" ? { op: entry } : entry);
}

function seedFunctions(bytes) {
  const resident = new Map([
    [1, op(MICRO_OPS.self, MICRO_OPS.arg, MICRO_OPS.add, MICRO_OPS.return)],
    [2, op(MICRO_OPS.self, MICRO_OPS.arg, MICRO_OPS.xor, MICRO_OPS.return)],
    [5, op(MICRO_OPS.self, MICRO_OPS.arg, MICRO_OPS.solar, MICRO_OPS.return)],
    [11, op(MICRO_OPS.self, MICRO_OPS.arg, MICRO_OPS.and, MICRO_OPS.return)],
    [12, op(MICRO_OPS.self, MICRO_OPS.arg, MICRO_OPS.or, MICRO_OPS.return)],
  ]);
  for (let opcode = 1; opcode <= FUNCTION_NAMES.length; opcode += 1) {
    const entry = byteOffset("functions", (opcode - 1) * 3);
    const body = opcode <= 18 ? GROOVE_LAYOUT.tracks.bodies.base + (opcode - 1) * 16 : 0;
    bytes.set([0x70, resident.has(opcode) ? 0x05 : 0x01, opcode === 5 ? 2 : 4], entry);
    bytes[entry + 3] = opcode;
    writeU16(bytes, entry + 4, body);
    bytes[entry + 6] = resident.has(opcode) ? resident.get(opcode).length : 0;
    bytes[entry + 7] = opcode;
    bytes[entry + 8] = 0;
    if (opcode <= 18) writeMicroBody(bytes, opcode - 1, resident.get(opcode) || op(MICRO_OPS.return));
  }
}

function seedProjection(bytes) {
  writeProjectionBody(bytes, 0, op(MICRO_OPS.time, MICRO_OPS.sin, MICRO_OPS.energy, MICRO_OPS.mul, MICRO_OPS.tanh, MICRO_OPS.return));
  writeProjectionBody(bytes, 1, op(MICRO_OPS.time, MICRO_OPS.cos, MICRO_OPS.energy, MICRO_OPS.mul, MICRO_OPS.tanh, MICRO_OPS.return));
  writeProjectionBody(bytes, 2, op(MICRO_OPS.depth, MICRO_OPS.energy, MICRO_OPS.sub, MICRO_OPS.tanh, MICRO_OPS.return));
}

function seedLifecycle(bytes) {
  const O = LIFECYCLE_OPS;
  const V = LIFECYCLE_VECTORS;
  const programs = [
    [O.resetPc, O.return],
    [O.executeSequence, O.synthesize, O.advanceFringe, O.return],
    [O.incrementProbe, O.selectMode, O.applySeedMode, O.resetLife, O.return],
    [O.applyOrganized, O.resetLife, O.return],
    [O.applyGraft, O.resetLife, O.return],
    [O.incrementFailed, { op: O.callVector, a: V.reprobe }, O.return],
    [O.zeroRuntime, O.resetLife, O.return],
    [O.resetPc, { op: O.callVector, a: V.boot }, O.return],
  ];
  programs.forEach((program, vector) => {
    for (let index = 0; index < 16; index += 1) {
      const at = byteOffset("lifecycle", vector * 16 + index);
      const instruction = program[index] ?? O.return;
      bytes[at] = typeof instruction === "number" ? instruction : instruction.op;
      bytes[at + 1] = typeof instruction === "number" ? 0 : instruction.a || 0;
      bytes[at + 2] = typeof instruction === "number" ? 0 : instruction.b || 0;
    }
  });
}

function initializeState(bytes, pc = 0) {
  const state = byteOffset("state");
  bytes[state + GROOVE_STATE.tag] = 0xa0;
  writeU16(bytes, state + GROOVE_STATE.pc, pc);
  bytes[state + GROOVE_STATE.lifecycleVector] = LIFECYCLE_VECTORS.start;
  writeU32(bytes, state + GROOVE_STATE.needlePixel, GROOVE_LAYOUT.tracks.sequence.base + pc * 8);
}

export function zeroPixelGroove({ id = "blank", parent = "", generation = 0, iteration = 0,
  authorityUtcMs = 0, entropySource = "memory", entropySeed = 0, profile = "standard" } = {}) {
  const bytes = new Uint8Array(BYTES);
  const machine = hardwareProfile(profile);
  const header = byteOffset("header");
  bytes.set(Buffer.from("PGR1"), header + GROOVE_HEADER.magic);
  writeU16(bytes, header + GROOVE_HEADER.version, GROOVE_LAYOUT.version);
  writeU16(bytes, header + GROOVE_HEADER.headerBytes, GROOVE_LAYOUT.tracks.header.pixels * 3);
  writeU32(bytes, header + GROOVE_HEADER.totalBytes, BYTES);
  writeU32(bytes, header + GROOVE_HEADER.generation, generation >>> 0);
  writeU16(bytes, header + GROOVE_HEADER.instructionCount, 0);
  writeU16(bytes, header + GROOVE_HEADER.entrypoint, 0);
  writeU16(bytes, header + GROOVE_HEADER.initialPc, 0);
  writeU16(bytes, header + GROOVE_HEADER.functionCount, FUNCTION_NAMES.length);
  writeU16(bytes, header + GROOVE_HEADER.lifecycleCount, Object.keys(LIFECYCLE_VECTORS).length);
  writeText(bytes, header + GROOVE_HEADER.id, 24, id);
  writeText(bytes, header + GROOVE_HEADER.parent, 24, parent);
  writeU32(bytes, header + GROOVE_HEADER.createdIteration, iteration >>> 0);
  writeU32(bytes, header + GROOVE_HEADER.layoutEnd, GROOVE_LAYOUT.tracks.fringe.base);
  const authority = Math.max(0, Math.min(Number.MAX_SAFE_INTEGER, Number(authorityUtcMs) || 0));
  writeU32(bytes, header + GROOVE_HEADER.authorityUtcLow, authority >>> 0);
  writeU32(bytes, header + GROOVE_HEADER.authorityUtcHigh, Math.floor(authority / 0x100000000));
  bytes[header + GROOVE_HEADER.entropySource] = entropySource === "ac-utc+memory" ? 2 : 1;
  writeU32(bytes, header + GROOVE_HEADER.entropySeed, Number(entropySeed) >>> 0);
  writeU16(bytes, header + GROOVE_HEADER.fieldResolution, machine.resolution);
  writeU32(bytes, header + GROOVE_HEADER.fieldBytes, machine.resolution * machine.resolution * 3);
  bytes[header + GROOVE_HEADER.profileCode] = machine.code;
  seedFunctions(bytes);
  seedProjection(bytes);
  seedLifecycle(bytes);
  initializeState(bytes);
  writeU32(bytes, header + GROOVE_HEADER.protectedHash, protectedHash(bytes));
  return bytes;
}

export function startPixelGroove({ id, parent = "", generation = 0, iteration = 0, source, bytecode,
  authorityUtcMs = 0, entropySource = "memory", entropySeed = 0, profile = "standard" }) {
  const code = typeof bytecode === "string" ? Buffer.from(bytecode, "hex") : Buffer.from(bytecode || []);
  if (!id || !/^[a-zA-Z0-9._:-]{1,23}$/.test(id)) throw new TypeError("PixelGroove requires a bounded id");
  if (!code.length || code.length % 24 !== 0 || code.length > GROOVE_LAYOUT.tracks.sequence.pixels * 3) {
    throw new TypeError("PixelGroove sequence must contain 1..8 complete 24-byte instructions");
  }
  const bytes = zeroPixelGroove({ id, parent, generation, iteration, authorityUtcMs, entropySource, entropySeed, profile });
  bytes.set(code, byteOffset("sequence"));
  const sourceBytes = Buffer.from(String(source || ""), "utf8");
  if (sourceBytes.length > GROOVE_LAYOUT.tracks.source.pixels * 3) throw new TypeError("PixelGroove source exceeds source track");
  bytes.set(sourceBytes, byteOffset("source"));
  const header = byteOffset("header");
  writeU16(bytes, header + GROOVE_HEADER.instructionCount, code.length / 24);
  writeU16(bytes, header + GROOVE_HEADER.sourceLength, sourceBytes.length);
  writeU32(bytes, header + GROOVE_HEADER.protectedHash, protectedHash(bytes));
  return bytes;
}

export function inspectPixelGroove(value) {
  const bytes = assertBuffer(typeof value === "string" ? new Uint8Array(Buffer.from(value, "hex")) : value);
  const header = byteOffset("header"), state = byteOffset("state");
  const magic = Buffer.from(bytes.subarray(header, header + 4)).toString("ascii");
  const expectedHash = readU32(bytes, header + GROOVE_HEADER.protectedHash);
  const actualHash = protectedHash(bytes);
  const instructionCount = readU16(bytes, header + GROOVE_HEADER.instructionCount);
  const sourceLength = readU16(bytes, header + GROOVE_HEADER.sourceLength);
  const fieldResolution = readU16(bytes, header + GROOVE_HEADER.fieldResolution);
  const profile = Object.values(HARDWARE_PROFILES).find((entry) => entry.code === bytes[header + GROOVE_HEADER.profileCode]);
  const errors = [];
  if (magic !== "PGR1") errors.push("bad magic");
  if (readU16(bytes, header + GROOVE_HEADER.version) !== GROOVE_LAYOUT.version) errors.push("unsupported version");
  if (readU32(bytes, header + GROOVE_HEADER.totalBytes) !== BYTES) errors.push("bad extent");
  if (instructionCount > 8) errors.push("sequence exceeds track");
  if (sourceLength > GROOVE_LAYOUT.tracks.source.pixels * 3) errors.push("source exceeds track");
  if (!profile || profile.resolution !== fieldResolution || readU32(bytes, header + GROOVE_HEADER.fieldBytes) !== fieldResolution * fieldResolution * 3) {
    errors.push("invalid hardware profile");
  }
  if (expectedHash !== actualHash) errors.push("protected hash mismatch");
  const densityFor = (base, pixels) => {
    let occupiedBytes = 0, occupiedPixels = 0;
    for (let pixel = base; pixel < base + pixels; pixel += 1) {
      const at = pixel * 3;
      if (bytes[at] || bytes[at + 1] || bytes[at + 2]) occupiedPixels += 1;
      occupiedBytes += Number(bytes[at] !== 0) + Number(bytes[at + 1] !== 0) + Number(bytes[at + 2] !== 0);
    }
    return Object.freeze({ occupiedPixels, pixels, pixelFill: occupiedPixels / pixels,
      occupiedBytes, bytes: pixels * 3, byteFill: occupiedBytes / (pixels * 3) });
  };
  const trackDensity = Object.freeze(Object.fromEntries(Object.entries(GROOVE_LAYOUT.tracks)
    .map(([name, track]) => [name, densityFor(track.base, track.pixels)])));
  return Object.freeze({
    valid: errors.length === 0,
    errors: Object.freeze(errors),
    version: readU16(bytes, header + GROOVE_HEADER.version),
    id: readText(bytes, header + GROOVE_HEADER.id, 24),
    parent: readText(bytes, header + GROOVE_HEADER.parent, 24),
    generation: readU32(bytes, header + GROOVE_HEADER.generation),
    iteration: readU32(bytes, header + GROOVE_HEADER.createdIteration),
    authorityUtcMs: readU32(bytes, header + GROOVE_HEADER.authorityUtcLow) +
      readU32(bytes, header + GROOVE_HEADER.authorityUtcHigh) * 0x100000000,
    entropySource: bytes[header + GROOVE_HEADER.entropySource] === 2 ? "ac-utc+memory" : "memory",
    entropySeed: readU32(bytes, header + GROOVE_HEADER.entropySeed),
    hardware: profile ? Object.freeze({ ...profile, fieldBytes: fieldResolution * fieldResolution * 3 }) : null,
    instructionCount,
    entrypoint: readU16(bytes, header + GROOVE_HEADER.entrypoint),
    pc: readU16(bytes, state + GROOVE_STATE.pc),
    needlePixel: readU32(bytes, state + GROOVE_STATE.needlePixel),
    source: Buffer.from(bytes.subarray(byteOffset("source"), byteOffset("source") + sourceLength)).toString("utf8"),
    protectedHash: expectedHash.toString(16).padStart(8, "0"),
    density: Object.freeze({ overall: densityFor(0, GROOVE_LAYOUT.pixels), tracks: trackDensity }),
    tracks: Object.freeze(Object.fromEntries(Object.entries(GROOVE_LAYOUT.tracks).map(([name, track]) => [name, Object.freeze({ ...track, bytes: track.pixels * 3 })]))),
  });
}

export function readGrooveInstruction(value, instruction) {
  const bytes = assertBuffer(value);
  const record = inspectPixelGroove(bytes);
  if (!record.valid || instruction < 0 || instruction >= record.instructionCount) return null;
  const begin = byteOffset("sequence", instruction * 8);
  return bytes.slice(begin, begin + 24);
}

export function stepPixelGroove(value) {
  const bytes = assertBuffer(value).slice();
  const record = inspectPixelGroove(bytes);
  if (!record.valid || !record.instructionCount) return { groove: bytes, instruction: null, record };
  const pc = record.pc % record.instructionCount;
  const instruction = readGrooveInstruction(bytes, pc);
  const nextPc = (pc + 1) % record.instructionCount;
  const state = byteOffset("state");
  writeU16(bytes, state + GROOVE_STATE.pc, nextPc);
  writeU32(bytes, state + GROOVE_STATE.sequencePasses,
    readU32(bytes, state + GROOVE_STATE.sequencePasses) + (nextPc === 0 ? 1 : 0));
  writeU32(bytes, state + GROOVE_STATE.needlePixel, GROOVE_LAYOUT.tracks.sequence.base + nextPc * 8);
  return { groove: bytes, instruction, record: inspectPixelGroove(bytes) };
}

export function printPixelGroove(value, { field = null } = {}) {
  const bytes = assertBuffer(value);
  const rgb = new Uint8Array(SIDE * SIDE * 3);
  for (let pixel = 0; pixel < COORDINATES.length; pixel += 1) {
    const [x, y] = COORDINATES[pixel], destination = (y * SIDE + x) * 3;
    rgb.set(bytes.subarray(pixel * 3, pixel * 3 + 3), destination);
  }
  if (field) {
    const input = typeof field === "string" ? Buffer.from(field, "hex") : field;
    const sourceSide = inspectPixelGroove(bytes).hardware?.resolution || FIELD_SIDE;
    if (!(input instanceof Uint8Array) || input.length !== sourceSide * sourceSide * 3) {
      throw new TypeError(`field must match the record's ${sourceSide}x${sourceSide} RGB profile`);
    }
    for (let y = 0; y < FIELD_SIDE; y += 1) {
      for (let x = 0; x < FIELD_SIDE; x += 1) {
        const sourceX = Math.min(sourceSide - 1, Math.floor(x * sourceSide / FIELD_SIDE));
        const sourceY = Math.min(sourceSide - 1, Math.floor(y * sourceSide / FIELD_SIDE));
        const source = (sourceY * sourceSide + sourceX) * 3;
        const destination = ((y + MARGIN) * SIDE + x + MARGIN) * 3;
        rgb[destination] = input[source]; rgb[destination + 1] = input[source + 1]; rgb[destination + 2] = input[source + 2];
      }
    }
  }
  return Buffer.concat([Buffer.from(`P6\n${SIDE} ${SIDE}\n255\n`), Buffer.from(rgb)]);
}

export function grooveHex(value) {
  return Buffer.from(assertBuffer(value)).toString("hex");
}
