import { createHash } from "node:crypto";

export const PIECE_VM = Object.freeze({
  version: 4,
  instructionBytes: 8,
  maxInstructions: 512,
  registers: 32,
  dataBytes: 16 * 1024,
  maxCallDepth: 32,
  maxTransformDepth: 32,
  defaultFuel: 1_000_000,
  fixedOne: 65_536,
  senses: 8,
  maxFunctionArgs: 4,
});

export const PIECE_SENSES = Object.freeze({
  beat: 0, bar: 1, health: 2, actual: 3,
  potential: 4, spatial: 5, coherence: 6, fringe: 7,
});

export const PIECE_OPS = Object.freeze({
  halt: 0, constant: 1, move: 2, add: 3, sub: 4, mul: 5, div: 6, less: 7,
  jump: 8, jumpIf: 9, call: 10, return: 11,
  clear: 12, pixel: 13, glyph: 14, swap: 15,
  identity: 16, pushTransform: 17, popTransform: 18, translate: 19,
  rotateX: 20, rotateY: 21, rotateZ: 22, perspective: 23, point3: 24,
  line3: 25, triangle3: 26, push: 27, pop: 28,
  load8: 29, store8: 30, sense8: 31,
  readRegion8: 32, writeRegion8: 33,
});

const OPCODE_NAMES = Object.freeze(Object.fromEntries(Object.entries(PIECE_OPS).map(([name, code]) => [code, name])));
const BUFFER = Object.freeze({ front: 0, back: 1 });

// A deliberately supplied machine alphabet. Discovery begins with composition,
// spacing, hierarchy, motion, and use—not the astronomically unlikely recovery
// of recognizable glyph topology from random RGB arithmetic.
const GLYPH_PATTERNS = Object.freeze({
  "0":"01110/10001/10011/10101/11001/10001/01110", "1":"00100/01100/00100/00100/00100/00100/01110",
  "2":"01110/10001/00001/00010/00100/01000/11111", "3":"11110/00001/00001/01110/00001/00001/11110",
  "4":"00010/00110/01010/10010/11111/00010/00010", "5":"11111/10000/10000/11110/00001/00001/11110",
  "6":"01110/10000/10000/11110/10001/10001/01110", "7":"11111/00001/00010/00100/01000/01000/01000",
  "8":"01110/10001/10001/01110/10001/10001/01110", "9":"01110/10001/10001/01111/00001/00001/01110",
  A:"01110/10001/10001/11111/10001/10001/10001", B:"11110/10001/10001/11110/10001/10001/11110",
  C:"01111/10000/10000/10000/10000/10000/01111", D:"11110/10001/10001/10001/10001/10001/11110",
  E:"11111/10000/10000/11110/10000/10000/11111", F:"11111/10000/10000/11110/10000/10000/10000",
  G:"01111/10000/10000/10111/10001/10001/01111", H:"10001/10001/10001/11111/10001/10001/10001",
  I:"01110/00100/00100/00100/00100/00100/01110", J:"00001/00001/00001/00001/10001/10001/01110",
  K:"10001/10010/10100/11000/10100/10010/10001", L:"10000/10000/10000/10000/10000/10000/11111",
  M:"10001/11011/10101/10101/10001/10001/10001", N:"10001/11001/10101/10011/10001/10001/10001",
  O:"01110/10001/10001/10001/10001/10001/01110", P:"11110/10001/10001/11110/10000/10000/10000",
  Q:"01110/10001/10001/10001/10101/10010/01101", R:"11110/10001/10001/11110/10100/10010/10001",
  S:"01111/10000/10000/01110/00001/00001/11110", T:"11111/00100/00100/00100/00100/00100/00100",
  U:"10001/10001/10001/10001/10001/10001/01110", V:"10001/10001/10001/10001/10001/01010/00100",
  W:"10001/10001/10001/10101/10101/10101/01010", X:"10001/10001/01010/00100/01010/10001/10001",
  Y:"10001/10001/01010/00100/00100/00100/00100", Z:"11111/00001/00010/00100/01000/10000/11111",
  " ":"00000/00000/00000/00000/00000/00000/00000",
});

export const PIECE_GLYPH_ROM = Object.freeze(Object.fromEntries(Object.entries(GLYPH_PATTERNS)
  .map(([character, pattern]) => [character.charCodeAt(0), Object.freeze(pattern.split("/").map((row) => parseInt(row, 2)))])));

function hash(value) {
  return createHash("sha256").update(value).digest("hex");
}

function fixed(value) {
  if (Number.isInteger(value)) return Math.max(-0x80000000, Math.min(0x7fffffff, value * PIECE_VM.fixedOne)) | 0;
  if (Array.isArray(value) && value[0] === "ratio" && value.length === 3 && Number.isInteger(value[1]) && Number.isInteger(value[2]) && value[2] !== 0) {
    return Math.round(value[1] / value[2] * PIECE_VM.fixedOne) | 0;
  }
  throw new TypeError("fixed value must be an integer or (ratio numerator denominator)");
}

function color(args, offset = 0) {
  const values = args.slice(offset, offset + 3);
  if (values.length !== 3 || !values.every((value) => Number.isInteger(value) && value >= 0 && value <= 255)) {
    throw new TypeError("RGB values must be integers in 0..255");
  }
  return (values[0] << 16) | (values[1] << 8) | values[2];
}

function buffer(value) {
  if (!(value in BUFFER)) throw new TypeError("buffer must be front or back");
  return BUFFER[value];
}

function instruction(opcode, a = 0, b = 0, c = 0, immediate = 0) {
  return { opcode, a, b, c, immediate: immediate | 0 };
}

function printPieceForm(value) {
  return Array.isArray(value) ? `(${value.map(printPieceForm).join(" ")})` : String(value);
}

export function readPieceLisp(source) {
  const text = String(source || "").trim();
  if (!text || text.length > 16_384) throw new TypeError("PieceVM Lisp source must be 1..16384 characters");
  const tokens = text.match(/[()]|[^\s()]+/g) || [];
  if (tokens.join("").length !== text.replace(/\s/g, "").length) throw new TypeError("unreadable PieceVM Lisp source");
  let cursor = 0;
  const read = () => {
    const token = tokens[cursor++];
    if (token === undefined) throw new TypeError("unexpected end of PieceVM Lisp source");
    if (token === ")") throw new TypeError("unexpected ) in PieceVM Lisp source");
    if (token !== "(") {
      if (/^-?\d+$/.test(token)) return Number(token);
      if (!/^[a-z][a-z0-9._/+:-]*$/i.test(token)) throw new TypeError(`invalid PieceVM Lisp symbol: ${token}`);
      return token.toLowerCase();
    }
    const form = [];
    while (tokens[cursor] !== ")") {
      if (cursor >= tokens.length) throw new TypeError("unclosed PieceVM Lisp form");
      form.push(read());
    }
    cursor += 1;
    return form;
  };
  const form = read();
  if (cursor !== tokens.length) throw new TypeError("PieceVM Lisp source must contain one form");
  return form;
}

export function compilePieceLisp(source, { resolution = 32 } = {}) {
  if (![32, 64, 128, 256].includes(resolution)) throw new TypeError("PieceVM resolution must be 32, 64, 128, or 256");
  const form = readPieceLisp(source);
  if (!Array.isArray(form) || form[0] !== "piece" || form.length < 2) throw new TypeError("PieceVM source must be (piece instruction...)");
  const sourceForms = form.slice(1), labels = new Map(), registers = new Map(), vectors = new Map();
  const functions = new Map(), dataRegions = new Map(), output = [], fixups = [];
  const validName = (name) => typeof name === "string" && /^[a-z][a-z0-9._-]*$/.test(name);
  const allocateRegister = (name) => {
    if (typeof name !== "string" || !/^[a-z][a-z0-9._-]*$/.test(name)) throw new TypeError(`invalid register: ${name}`);
    if (!registers.has(name)) {
      if (registers.size >= PIECE_VM.registers) throw new RangeError("PieceVM register budget exceeded");
      registers.set(name, registers.size);
    }
    return registers.get(name);
  };
  const reg = (name, scope = null) => scope?.parameterRegisters?.get(name) ?? allocateRegister(name);
  const declareVector = (name) => {
    if (!validName(name) || vectors.has(name) || registers.has(name)) {
      throw new TypeError(`invalid or duplicate vector: ${name}`);
    }
    if (registers.size + 3 > PIECE_VM.registers) throw new RangeError("PieceVM vector exceeds register budget");
    const components = [`${name}.x`, `${name}.y`, `${name}.z`];
    if (components.some((component) => registers.has(component))) throw new TypeError(`vector components already allocated: ${name}`);
    const base = registers.size;
    components.forEach((component, index) => registers.set(component, base + index));
    vectors.set(name, base);
  };
  const vector = (name) => {
    if (typeof name !== "string" || !vectors.has(name)) throw new TypeError(`unknown PieceVM vector: ${name}`);
    return vectors.get(name);
  };
  let dataOffset = 0;
  for (const declaration of sourceForms) {
    if (!Array.isArray(declaration)) continue;
    if (declaration[0] === "data") {
      const [, name, length] = declaration;
      if (declaration.length !== 3 || !validName(name) || !Number.isInteger(length) || length < 1 ||
          dataRegions.has(name) || dataOffset + length > PIECE_VM.dataBytes) {
        throw new TypeError(`invalid PieceVM data region: ${printPieceForm(declaration)}`);
      }
      dataRegions.set(name, Object.freeze({ offset: dataOffset, length }));
      dataOffset += length;
    } else if (declaration[0] === "function") {
      const [, name, parameters, ...body] = declaration;
      if (!validName(name) || functions.has(name) || !Array.isArray(parameters) || parameters.length < 1 ||
          parameters.length > PIECE_VM.maxFunctionArgs || new Set(parameters).size !== parameters.length ||
          parameters.some((parameter) => !validName(parameter)) || !body.length ||
          !Array.isArray(body.at(-1)) || body.at(-1)[0] !== "return") {
        throw new TypeError(`invalid PieceVM function: ${printPieceForm(declaration)}`);
      }
      if (body.some((entry) => !Array.isArray(entry) || ["function", "data", "label", "vec3"].includes(entry[0]))) {
        throw new TypeError(`invalid nested declaration in PieceVM function: ${name}`);
      }
      functions.set(name, { name, parameters: [...parameters], body, parameterRegisters: new Map() });
    }
  }
  for (const declaration of sourceForms) {
    if (Array.isArray(declaration) && declaration[0] === "vec3") {
      if (declaration.length !== 5) throw new TypeError("vec3 requires a name and three components");
      declareVector(declaration[1]);
    }
  }
  for (const fn of functions.values()) for (const parameter of fn.parameters) {
    fn.parameterRegisters.set(parameter, allocateRegister(`${fn.name}.${parameter}`));
  }
  const programForms = [];
  for (const entry of sourceForms) {
    if (entry?.[0] === "data") continue;
    if (entry?.[0] === "function") {
      const fn = functions.get(entry[1]);
      programForms.push({ form: ["label", fn.name], scope: fn });
      for (const bodyForm of fn.body) programForms.push({ form: bodyForm, scope: fn });
    } else programForms.push({ form: entry, scope: null });
  }
  const emit = (value) => {
    output.push(value);
    if (output.length > PIECE_VM.maxInstructions) throw new RangeError("PieceVM instruction budget exceeded");
    return output.length - 1;
  };
  for (const entry of programForms) {
    const formInstruction = entry.form, scope = entry.scope;
    if (!Array.isArray(formInstruction) || typeof formInstruction[0] !== "string") throw new TypeError("PieceVM instructions must be lists");
    const [name, ...args] = formInstruction;
    if (name === "label") {
      if (args.length !== 1 || typeof args[0] !== "string" || labels.has(args[0])) throw new TypeError("invalid or duplicate PieceVM label");
      labels.set(args[0], output.length); continue;
    }
    if (name === "vec3" && args.length === 4) {
      const base = vector(args[0]);
      args.slice(1).forEach((component, index) => emit(instruction(PIECE_OPS.constant, base + index, 0, 0, fixed(component))));
      continue;
    }
    let value;
    if (name === "halt" && args.length === 0) value = instruction(PIECE_OPS.halt);
    else if (name === "constant" && args.length === 2) value = instruction(PIECE_OPS.constant, reg(args[0], scope), 0, 0, fixed(args[1]));
    else if (name === "move" && args.length === 2) value = instruction(PIECE_OPS.move, reg(args[0], scope), reg(args[1], scope));
    else if (["add", "sub", "mul", "div", "less"].includes(name) && args.length === 3)
      value = instruction(PIECE_OPS[name], reg(args[0], scope), reg(args[1], scope), reg(args[2], scope));
    else if (name === "jump" && args.length === 1 && typeof args[0] === "string") {
      value = instruction(PIECE_OPS.jump); fixups.push({ at: output.length, label: args[0] });
    } else if (name === "call" && args.length >= 1 && typeof args[0] === "string") {
      const target = args[0], supplied = args.slice(1), fn = functions.get(target);
      if (!fn && supplied.length) throw new TypeError(`PieceVM label has no argument signature: ${target}`);
      if (fn && supplied.length !== fn.parameters.length) {
        throw new TypeError(`PieceVM function ${target} expects ${fn.parameters.length} arguments, received ${supplied.length}`);
      }
      if (fn) {
        const parameters = fn.parameters.map((parameter) => fn.parameterRegisters.get(parameter));
        const argumentsAtCall = supplied.map((argument) => reg(argument, scope));
        parameters.forEach((parameter) => emit(instruction(PIECE_OPS.push, parameter)));
        argumentsAtCall.forEach((argument) => emit(instruction(PIECE_OPS.push, argument)));
        for (let index = parameters.length - 1; index >= 0; index -= 1) emit(instruction(PIECE_OPS.pop, parameters[index]));
        const at = emit(instruction(PIECE_OPS.call)); fixups.push({ at, label: target });
        for (let index = parameters.length - 1; index >= 0; index -= 1) emit(instruction(PIECE_OPS.pop, parameters[index]));
        continue;
      }
      value = instruction(PIECE_OPS.call); fixups.push({ at: output.length, label: target });
    } else if (name === "jump-if" && args.length === 2 && typeof args[1] === "string") {
      value = instruction(PIECE_OPS.jumpIf, reg(args[0], scope)); fixups.push({ at: output.length, label: args[1] });
    } else if (name === "return" && args.length === 0) value = instruction(PIECE_OPS.return);
    else if (name === "clear" && args.length === 4) value = instruction(PIECE_OPS.clear, buffer(args[0]), 0, 0, color(args, 1));
    else if (name === "pixel" && args.length === 6) value = instruction(PIECE_OPS.pixel, buffer(args[0]), reg(args[1], scope), reg(args[2], scope), color(args, 3));
    else if (name === "glyph" && args.length === 7 && Number.isInteger(args[1]) && args[1] >= 32 && args[1] <= 126)
      value = instruction(PIECE_OPS.glyph, buffer(args[0]), reg(args[2], scope), reg(args[3], scope), ((args[1] & 255) << 24) | color(args, 4));
    else if (name === "swap" && args.length === 0) value = instruction(PIECE_OPS.swap);
    else if (name === "identity" && args.length === 0) value = instruction(PIECE_OPS.identity);
    else if (name === "push-transform" && args.length === 0) value = instruction(PIECE_OPS.pushTransform);
    else if (name === "pop-transform" && args.length === 0) value = instruction(PIECE_OPS.popTransform);
    else if (name === "translate" && args.length === 3) value = instruction(PIECE_OPS.translate, reg(args[0], scope), reg(args[1], scope), reg(args[2], scope));
    else if (["rotate-x", "rotate-y", "rotate-z"].includes(name) && args.length === 1)
      value = instruction(PIECE_OPS[name.replace(/-([xyz])$/, (_, axis) => axis.toUpperCase())], reg(args[0], scope));
    else if (name === "perspective" && args.length === 1) value = instruction(PIECE_OPS.perspective, reg(args[0], scope));
    else if (name === "point3" && args.length === 7)
      value = instruction(PIECE_OPS.point3, buffer(args[0]), reg(args[1], scope), reg(args[2], scope), (reg(args[3], scope) << 24) | color(args, 4));
    else if (name === "line3" && args.length === 6)
      value = instruction(PIECE_OPS.line3, buffer(args[0]), vector(args[1]), vector(args[2]), color(args, 3));
    else if (name === "triangle3" && args.length === 7)
      value = instruction(PIECE_OPS.triangle3, buffer(args[0]), vector(args[1]), vector(args[2]), (vector(args[3]) << 24) | color(args, 4));
    else if (name === "push" && args.length === 1) value = instruction(PIECE_OPS.push, reg(args[0], scope));
    else if (name === "pop" && args.length === 1) value = instruction(PIECE_OPS.pop, reg(args[0], scope));
    else if (name === "load8" && args.length === 2) value = instruction(PIECE_OPS.load8, reg(args[0], scope), reg(args[1], scope));
    else if (name === "store8" && args.length === 2) value = instruction(PIECE_OPS.store8, reg(args[0], scope), reg(args[1], scope));
    else if (name === "read8" && args.length === 3 && dataRegions.has(args[1])) {
      const region = dataRegions.get(args[1]);
      value = instruction(PIECE_OPS.readRegion8, reg(args[0], scope), reg(args[2], scope), 0,
        (region.length << 16) | region.offset);
    } else if (name === "write8" && args.length === 3 && dataRegions.has(args[0])) {
      const region = dataRegions.get(args[0]);
      value = instruction(PIECE_OPS.writeRegion8, reg(args[1], scope), reg(args[2], scope), 0,
        (region.length << 16) | region.offset);
    }
    else if (name === "sense8" && args.length === 2 && typeof args[1] === "string" && args[1] in PIECE_SENSES)
      value = instruction(PIECE_OPS.sense8, reg(args[0], scope), 0, 0, PIECE_SENSES[args[1]]);
    else throw new TypeError(`invalid PieceVM instruction: ${JSON.stringify(formInstruction)}`);
    emit(value);
  }
  for (const fixup of fixups) {
    if (!labels.has(fixup.label)) throw new TypeError(`unknown PieceVM label: ${fixup.label}`);
    output[fixup.at].immediate = labels.get(fixup.label);
  }
  const swaps = output.reduce((count, value) => count + (value.opcode === PIECE_OPS.swap ? 1 : 0), 0);
  if (swaps !== 1) throw new TypeError("PieceVM frame must contain exactly one swap");
  if (output.at(-1)?.opcode !== PIECE_OPS.halt) throw new TypeError("PieceVM frame must end in halt");
  const bytecode = Buffer.alloc(output.length * PIECE_VM.instructionBytes);
  output.forEach((value, index) => {
    const at = index * PIECE_VM.instructionBytes;
    bytecode[at] = value.opcode; bytecode[at + 1] = value.a; bytecode[at + 2] = value.b; bytecode[at + 3] = value.c;
    bytecode.writeInt32LE(value.immediate | 0, at + 4);
  });
  const canonical = printPieceForm(form);
  const functionMetadata = Object.fromEntries([...functions].map(([name, fn]) => [name, Object.freeze({
    parameters: Object.freeze([...fn.parameters]),
    registers: Object.freeze(fn.parameters.map((parameter) => fn.parameterRegisters.get(parameter))),
    label: labels.get(name),
  })]));
  return Object.freeze({ version: PIECE_VM.version, source: canonical, resolution,
    instructionCount: output.length, registerCount: registers.size,
    registers: Object.freeze(Object.fromEntries(registers)), vectors: Object.freeze(Object.fromEntries(vectors)),
    labels: Object.freeze(Object.fromEntries(labels)), functions: Object.freeze(functionMetadata),
    data: Object.freeze(Object.fromEntries(dataRegions)),
    bytecode: bytecode.toString("hex"), bytecodeHash: hash(bytecode) });
}

function decode(program) {
  const bytes = Buffer.from(program.bytecode, "hex");
  if (bytes.length !== program.instructionCount * PIECE_VM.instructionBytes || bytes.length > PIECE_VM.maxInstructions * PIECE_VM.instructionBytes) {
    throw new TypeError("invalid PieceVM bytecode extent");
  }
  return Array.from({ length: program.instructionCount }, (_, index) => {
    const at = index * PIECE_VM.instructionBytes;
    const value = instruction(bytes[at], bytes[at + 1], bytes[at + 2], bytes[at + 3], bytes.readInt32LE(at + 4));
    if (!(value.opcode in OPCODE_NAMES)) throw new TypeError(`unknown PieceVM opcode: ${value.opcode}`);
    if ([value.a, value.b, value.c].some((register) => register >= PIECE_VM.registers)) throw new TypeError("PieceVM register address exceeds machine");
    if ([PIECE_OPS.jump, PIECE_OPS.jumpIf, PIECE_OPS.call].includes(value.opcode) && (value.immediate < 0 || value.immediate >= program.instructionCount)) {
      throw new TypeError("PieceVM control target exceeds code");
    }
    if (value.opcode === PIECE_OPS.sense8 && (value.immediate < 0 || value.immediate >= PIECE_VM.senses)) {
      throw new TypeError("PieceVM sense address exceeds membrane");
    }
    if ([PIECE_OPS.readRegion8, PIECE_OPS.writeRegion8].includes(value.opcode)) {
      const offset = value.immediate & 0xffff, length = (value.immediate >>> 16) & 0xffff;
      if (!length || offset + length > PIECE_VM.dataBytes) throw new TypeError("PieceVM data region exceeds memory");
    }
    if ([PIECE_OPS.clear, PIECE_OPS.pixel, PIECE_OPS.glyph, PIECE_OPS.point3, PIECE_OPS.line3, PIECE_OPS.triangle3].includes(value.opcode) && value.a > 1) {
      throw new TypeError("PieceVM buffer selector exceeds machine");
    }
    if (value.opcode === PIECE_OPS.point3 && ((value.immediate >>> 24) & 255) >= PIECE_VM.registers) {
      throw new TypeError("PieceVM point vector exceeds registers");
    }
    if (value.opcode === PIECE_OPS.line3 && (value.b + 2 >= PIECE_VM.registers || value.c + 2 >= PIECE_VM.registers)) {
      throw new TypeError("PieceVM line vector exceeds registers");
    }
    if (value.opcode === PIECE_OPS.triangle3 && (value.b + 2 >= PIECE_VM.registers || value.c + 2 >= PIECE_VM.registers || ((value.immediate >>> 24) & 255) + 2 >= PIECE_VM.registers)) {
      throw new TypeError("PieceVM triangle vector exceeds registers");
    }
    return value;
  });
}

function identityMatrix() {
  return [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
}

function matrixMultiply(a, b) {
  const output = new Array(16).fill(0);
  for (let row = 0; row < 4; row += 1) for (let column = 0; column < 4; column += 1)
    for (let k = 0; k < 4; k += 1) output[row * 4 + column] += a[row * 4 + k] * b[k * 4 + column];
  return output;
}

function transformPoint(matrix, x, y, z) {
  const point = [x, y, z, 1], output = [0, 0, 0, 0];
  for (let row = 0; row < 4; row += 1) for (let k = 0; k < 4; k += 1) output[row] += matrix[row * 4 + k] * point[k];
  return output;
}

function projectPoint(state, matrix, registers, base) {
  const point = transformPoint(matrix, registerNumber(registers, base), registerNumber(registers, base + 1), registerNumber(registers, base + 2));
  const depth = Math.max(.1, state.perspective + point[2]);
  return [Math.round(state.resolution / 2 + point[0] / depth * state.resolution / 2),
    Math.round(state.resolution / 2 - point[1] / depth * state.resolution / 2)];
}

function clipLine(size, start, end) {
  let [x0, y0] = start, [x1, y1] = end;
  const code = (x, y) => (x < 0 ? 1 : x >= size ? 2 : 0) | (y < 0 ? 4 : y >= size ? 8 : 0);
  for (let guard = 0; guard < 8; guard += 1) {
    const c0 = code(x0, y0), c1 = code(x1, y1);
    if (!(c0 | c1)) return [Math.round(x0), Math.round(y0), Math.round(x1), Math.round(y1)];
    if (c0 & c1) return null;
    const outside = c0 || c1;
    let x, y;
    if (outside & 8) { x = x0 + (x1 - x0) * (size - 1 - y0) / (y1 - y0); y = size - 1; }
    else if (outside & 4) { x = x0 + (x1 - x0) * -y0 / (y1 - y0); y = 0; }
    else if (outside & 2) { y = y0 + (y1 - y0) * (size - 1 - x0) / (x1 - x0); x = size - 1; }
    else { y = y0 + (y1 - y0) * -x0 / (x1 - x0); x = 0; }
    if (outside === c0) { x0 = x; y0 = y; } else { x1 = x; y1 = y; }
  }
  return null;
}

function paintLine(state, selector, start, end, immediate, consume) {
  const clipped = clipLine(state.resolution, start, end);
  if (!clipped) return;
  let [x0, y0, x1, y1] = clipped;
  const dx = Math.abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
  const dy = -Math.abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
  let error = dx + dy;
  for (let guard = 0; guard <= state.resolution * 2; guard += 1) {
    consume(); paint(state, selector, x0, y0, immediate);
    if (x0 === x1 && y0 === y1) return;
    const twice = 2 * error;
    if (twice >= dy) { error += dy; x0 += sx; }
    if (twice <= dx) { error += dx; y0 += sy; }
  }
  throw new Error("PieceVM clipped line exceeded raster bound");
}

export function createPieceVmState(resolution = 32) {
  if (![32, 64, 128, 256].includes(resolution)) throw new TypeError("invalid PieceVM state resolution");
  return {
    version: PIECE_VM.version, resolution,
    registers: new Int32Array(PIECE_VM.registers), data: new Uint8Array(PIECE_VM.dataBytes),
    buffers: [new Uint8Array(resolution * resolution * 3), new Uint8Array(resolution * resolution * 3)],
    front: 0, back: 1, frame: 0, faults: 0, lastFault: null,
    perspective: 1, lastTraceHash: null,
  };
}

function rgb(immediate) {
  return [(immediate >>> 16) & 255, (immediate >>> 8) & 255, immediate & 255];
}

function logicalBuffer(state, selector) {
  return state.buffers[selector === BUFFER.front ? state.front : state.back];
}

function paint(state, selector, x, y, immediate) {
  if (x < 0 || x >= state.resolution || y < 0 || y >= state.resolution) return false;
  const at = (y * state.resolution + x) * 3, values = rgb(immediate), target = logicalBuffer(state, selector);
  target[at] = values[0]; target[at + 1] = values[1]; target[at + 2] = values[2];
  return true;
}

function registerNumber(registers, index) { return registers[index] / PIECE_VM.fixedOne; }

function safeFixed(value) {
  if (!Number.isFinite(value)) throw new Error("non-finite PieceVM arithmetic");
  return Math.max(-0x80000000, Math.min(0x7fffffff, Math.round(value))) | 0;
}

export function defaultPieceSenses(frame = 0) {
  const tick = Math.max(0, Math.floor(Number(frame) || 0));
  return Uint8Array.of((tick * 7) & 255, (Math.floor(tick / 16) % 4) * 85,
    255, 128, 192, 96, 160, (tick * 29 + 17) & 255);
}

function pieceSenses(value, frame) {
  if (value === null || value === undefined) return defaultPieceSenses(frame);
  if (!ArrayBuffer.isView(value) && !Array.isArray(value)) throw new TypeError("PieceVM senses must be an eight-byte vector");
  if (value.length !== PIECE_VM.senses) throw new TypeError("PieceVM senses must contain exactly eight channels");
  const output = new Uint8Array(PIECE_VM.senses);
  for (let index = 0; index < output.length; index += 1) {
    if (!Number.isInteger(value[index]) || value[index] < 0 || value[index] > 255) throw new TypeError("PieceVM sense values must be bytes");
    output[index] = value[index];
  }
  return output;
}

export function runPieceVm(program, { state = null, fuel = PIECE_VM.defaultFuel, senses = null } = {}) {
  const instructions = decode(program);
  const vm = state || createPieceVmState(program.resolution);
  if (vm.resolution !== program.resolution) throw new TypeError("PieceVM program/state profile mismatch");
  const sensed = pieceSenses(senses, vm.frame);
  const callStack = [], operandStack = [], transforms = [identityMatrix()], trace = [];
  let pc = 0, remaining = Math.max(1, Math.floor(fuel)), swapped = false, halted = false, fault = null;
  const consume = (amount = 1) => {
    remaining -= amount;
    if (remaining < 0) throw new Error("PieceVM fuel exhausted");
  };
  try {
    while (!halted) {
      consume();
      const value = instructions[pc];
      if (!value) throw new Error("PieceVM pc escaped code");
      trace.push([pc, value.opcode, remaining]);
      let advance = true;
      const r = vm.registers;
      if (value.opcode === PIECE_OPS.halt) halted = true;
      else if (value.opcode === PIECE_OPS.constant) r[value.a] = value.immediate;
      else if (value.opcode === PIECE_OPS.move) r[value.a] = r[value.b];
      else if (value.opcode === PIECE_OPS.add) r[value.a] = safeFixed(r[value.b] + r[value.c]);
      else if (value.opcode === PIECE_OPS.sub) r[value.a] = safeFixed(r[value.b] - r[value.c]);
      else if (value.opcode === PIECE_OPS.mul) r[value.a] = safeFixed(r[value.b] * r[value.c] / PIECE_VM.fixedOne);
      else if (value.opcode === PIECE_OPS.div) {
        if (!r[value.c]) throw new Error("PieceVM division by zero");
        r[value.a] = safeFixed(r[value.b] * PIECE_VM.fixedOne / r[value.c]);
      } else if (value.opcode === PIECE_OPS.less) r[value.a] = r[value.b] < r[value.c] ? PIECE_VM.fixedOne : 0;
      else if (value.opcode === PIECE_OPS.jump) { pc = value.immediate; advance = false; }
      else if (value.opcode === PIECE_OPS.jumpIf && r[value.a]) { pc = value.immediate; advance = false; }
      else if (value.opcode === PIECE_OPS.call) {
        if (callStack.length >= PIECE_VM.maxCallDepth) throw new Error("PieceVM call stack exceeded");
        callStack.push(pc + 1); pc = value.immediate; advance = false;
      } else if (value.opcode === PIECE_OPS.return) {
        if (!callStack.length) throw new Error("PieceVM return stack underflow");
        pc = callStack.pop(); advance = false;
      } else if (value.opcode === PIECE_OPS.clear) {
        const target = logicalBuffer(vm, value.a), values = rgb(value.immediate);
        consume(vm.resolution * vm.resolution);
        for (let at = 0; at < target.length; at += 3) { target[at] = values[0]; target[at + 1] = values[1]; target[at + 2] = values[2]; }
      } else if (value.opcode === PIECE_OPS.pixel) {
        paint(vm, value.a, Math.trunc(registerNumber(r, value.b)), Math.trunc(registerNumber(r, value.c)), value.immediate);
      } else if (value.opcode === PIECE_OPS.glyph) {
        const character = (value.immediate >>> 24) & 255, rows = PIECE_GLYPH_ROM[character] || PIECE_GLYPH_ROM[32];
        const startX = Math.trunc(registerNumber(r, value.b)), startY = Math.trunc(registerNumber(r, value.c));
        consume(35);
        for (let y = 0; y < 7; y += 1) for (let x = 0; x < 5; x += 1)
          if (rows[y] & (1 << (4 - x))) paint(vm, value.a, startX + x, startY + y, value.immediate & 0xffffff);
      } else if (value.opcode === PIECE_OPS.swap) {
        if (swapped) throw new Error("PieceVM swapped more than once");
        swapped = true;
      } else if (value.opcode === PIECE_OPS.identity) transforms[transforms.length - 1] = identityMatrix();
      else if (value.opcode === PIECE_OPS.pushTransform) {
        if (transforms.length >= PIECE_VM.maxTransformDepth) throw new Error("PieceVM transform stack exceeded");
        transforms.push([...transforms.at(-1)]);
      } else if (value.opcode === PIECE_OPS.popTransform) {
        if (transforms.length <= 1) throw new Error("PieceVM transform stack underflow");
        transforms.pop();
      } else if (value.opcode === PIECE_OPS.translate) {
        const matrix = identityMatrix(); matrix[3] = registerNumber(r, value.a); matrix[7] = registerNumber(r, value.b); matrix[11] = registerNumber(r, value.c);
        transforms[transforms.length - 1] = matrixMultiply(transforms.at(-1), matrix);
      } else if ([PIECE_OPS.rotateX, PIECE_OPS.rotateY, PIECE_OPS.rotateZ].includes(value.opcode)) {
        const angle = registerNumber(r, value.a) * Math.PI * 2, sine = Math.sin(angle), cosine = Math.cos(angle), matrix = identityMatrix();
        if (value.opcode === PIECE_OPS.rotateX) { matrix[5] = cosine; matrix[6] = -sine; matrix[9] = sine; matrix[10] = cosine; }
        else if (value.opcode === PIECE_OPS.rotateY) { matrix[0] = cosine; matrix[2] = sine; matrix[8] = -sine; matrix[10] = cosine; }
        else { matrix[0] = cosine; matrix[1] = -sine; matrix[4] = sine; matrix[5] = cosine; }
        transforms[transforms.length - 1] = matrixMultiply(transforms.at(-1), matrix);
      } else if (value.opcode === PIECE_OPS.perspective) vm.perspective = Math.max(.05, Math.abs(registerNumber(r, value.a)));
      else if (value.opcode === PIECE_OPS.point3) {
        const zRegister = (value.immediate >>> 24) & 255;
        const point = transformPoint(transforms.at(-1), registerNumber(r, value.b), registerNumber(r, value.c), registerNumber(r, zRegister));
        const depth = Math.max(.1, vm.perspective + point[2]);
        const x = Math.round(vm.resolution / 2 + point[0] / depth * vm.resolution / 2);
        const y = Math.round(vm.resolution / 2 - point[1] / depth * vm.resolution / 2);
        paint(vm, value.a, x, y, value.immediate & 0xffffff);
      } else if (value.opcode === PIECE_OPS.line3) {
        paintLine(vm, value.a, projectPoint(vm, transforms.at(-1), r, value.b), projectPoint(vm, transforms.at(-1), r, value.c), value.immediate, consume);
      } else if (value.opcode === PIECE_OPS.triangle3) {
        const third = (value.immediate >>> 24) & 255, ink = value.immediate & 0xffffff;
        const points = [projectPoint(vm, transforms.at(-1), r, value.b), projectPoint(vm, transforms.at(-1), r, value.c), projectPoint(vm, transforms.at(-1), r, third)];
        paintLine(vm, value.a, points[0], points[1], ink, consume);
        paintLine(vm, value.a, points[1], points[2], ink, consume);
        paintLine(vm, value.a, points[2], points[0], ink, consume);
      } else if (value.opcode === PIECE_OPS.push) {
        if (operandStack.length >= 256) throw new Error("PieceVM operand stack exceeded");
        operandStack.push(r[value.a]);
      } else if (value.opcode === PIECE_OPS.pop) {
        if (!operandStack.length) throw new Error("PieceVM operand stack underflow");
        r[value.a] = operandStack.pop();
      } else if (value.opcode === PIECE_OPS.load8) {
        const address = Math.trunc(registerNumber(r, value.b));
        if (address < 0 || address >= vm.data.length) throw new Error("PieceVM load8 address exceeds data");
        r[value.a] = vm.data[address] * PIECE_VM.fixedOne;
      } else if (value.opcode === PIECE_OPS.store8) {
        const address = Math.trunc(registerNumber(r, value.a));
        if (address < 0 || address >= vm.data.length) throw new Error("PieceVM store8 address exceeds data");
        vm.data[address] = Math.trunc(registerNumber(r, value.b)) & 255;
      } else if (value.opcode === PIECE_OPS.sense8) {
        r[value.a] = Math.round(sensed[value.immediate] * PIECE_VM.fixedOne / 255);
      } else if (value.opcode === PIECE_OPS.readRegion8) {
        const offset = value.immediate & 0xffff, length = (value.immediate >>> 16) & 0xffff;
        const index = Math.trunc(registerNumber(r, value.b));
        if (index < 0 || index >= length) throw new Error("PieceVM read8 index exceeds data region");
        r[value.a] = vm.data[offset + index] * PIECE_VM.fixedOne;
      } else if (value.opcode === PIECE_OPS.writeRegion8) {
        const offset = value.immediate & 0xffff, length = (value.immediate >>> 16) & 0xffff;
        const index = Math.trunc(registerNumber(r, value.a));
        if (index < 0 || index >= length) throw new Error("PieceVM write8 index exceeds data region");
        vm.data[offset + index] = Math.trunc(registerNumber(r, value.b)) & 255;
      }
      if (advance) pc += 1;
    }
    if (!swapped) throw new Error("PieceVM halted without publishing a frame");
    if (callStack.length) throw new Error("PieceVM halted with live call frames");
    if (operandStack.length) throw new Error("PieceVM halted with live operand values");
    if (transforms.length !== 1) throw new Error("PieceVM halted with live transform frames");
    [vm.front, vm.back] = [vm.back, vm.front];
  } catch (error) {
    fault = String(error?.message || error);
    vm.faults += 1; vm.lastFault = fault;
  }
  if (!fault) { vm.frame += 1; vm.lastFault = null; }
  vm.lastTraceHash = hash(JSON.stringify(trace));
  return { state: vm, halted, swapped, fault, fuelUsed: fuel - remaining,
    frontHash: hash(vm.buffers[vm.front]), traceHash: vm.lastTraceHash, trace };
}

export function verifyPieceProgram(program, { fuel = PIECE_VM.defaultFuel } = {}) {
  const first = runPieceVm(program, { fuel });
  if (first.fault || !first.swapped) return Object.freeze({ valid: false, error: first.fault || "no swap" });
  const second = runPieceVm(program, { state: first.state, fuel });
  if (second.fault || !second.swapped) return Object.freeze({ valid: false, error: second.fault || "no second swap" });
  return Object.freeze({ valid: true, version: PIECE_VM.version, bytecodeHash: program.bytecodeHash,
    instructionCount: program.instructionCount, registerCount: program.registerCount,
    resolution: program.resolution, frames: 2, frontHashes: Object.freeze([first.frontHash, second.frontHash]),
    traceHashes: Object.freeze([first.traceHash, second.traceHash]), fuel: Object.freeze([first.fuelUsed, second.fuelUsed]) });
}
