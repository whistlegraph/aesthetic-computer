// A small 6502 assembler for NES cartridges, in the asm6 dialect most NES
// tutorials are written in. One source file in, the bytes of a .nes file out;
// no compiler to install. Official opcodes only, no macros or includes.

const OPS = {
  adc: { imm: 0x69, zp: 0x65, zpx: 0x75, abs: 0x6d, absx: 0x7d, absy: 0x79, indx: 0x61, indy: 0x71 },
  and: { imm: 0x29, zp: 0x25, zpx: 0x35, abs: 0x2d, absx: 0x3d, absy: 0x39, indx: 0x21, indy: 0x31 },
  asl: { acc: 0x0a, zp: 0x06, zpx: 0x16, abs: 0x0e, absx: 0x1e },
  bcc: { rel: 0x90 }, bcs: { rel: 0xb0 }, beq: { rel: 0xf0 }, bmi: { rel: 0x30 },
  bne: { rel: 0xd0 }, bpl: { rel: 0x10 }, bvc: { rel: 0x50 }, bvs: { rel: 0x70 },
  bit: { zp: 0x24, abs: 0x2c },
  brk: { imp: 0x00 }, clc: { imp: 0x18 }, cld: { imp: 0xd8 }, cli: { imp: 0x58 }, clv: { imp: 0xb8 },
  cmp: { imm: 0xc9, zp: 0xc5, zpx: 0xd5, abs: 0xcd, absx: 0xdd, absy: 0xd9, indx: 0xc1, indy: 0xd1 },
  cpx: { imm: 0xe0, zp: 0xe4, abs: 0xec }, cpy: { imm: 0xc0, zp: 0xc4, abs: 0xcc },
  dec: { zp: 0xc6, zpx: 0xd6, abs: 0xce, absx: 0xde }, dex: { imp: 0xca }, dey: { imp: 0x88 },
  eor: { imm: 0x49, zp: 0x45, zpx: 0x55, abs: 0x4d, absx: 0x5d, absy: 0x59, indx: 0x41, indy: 0x51 },
  inc: { zp: 0xe6, zpx: 0xf6, abs: 0xee, absx: 0xfe }, inx: { imp: 0xe8 }, iny: { imp: 0xc8 },
  jmp: { abs: 0x4c, ind: 0x6c }, jsr: { abs: 0x20 },
  lda: { imm: 0xa9, zp: 0xa5, zpx: 0xb5, abs: 0xad, absx: 0xbd, absy: 0xb9, indx: 0xa1, indy: 0xb1 },
  ldx: { imm: 0xa2, zp: 0xa6, zpy: 0xb6, abs: 0xae, absy: 0xbe },
  ldy: { imm: 0xa0, zp: 0xa4, zpx: 0xb4, abs: 0xac, absx: 0xbc },
  lsr: { acc: 0x4a, zp: 0x46, zpx: 0x56, abs: 0x4e, absx: 0x5e },
  nop: { imp: 0xea },
  ora: { imm: 0x09, zp: 0x05, zpx: 0x15, abs: 0x0d, absx: 0x1d, absy: 0x19, indx: 0x01, indy: 0x11 },
  pha: { imp: 0x48 }, php: { imp: 0x08 }, pla: { imp: 0x68 }, plp: { imp: 0x28 },
  rol: { acc: 0x2a, zp: 0x26, zpx: 0x36, abs: 0x2e, absx: 0x3e },
  ror: { acc: 0x6a, zp: 0x66, zpx: 0x76, abs: 0x6e, absx: 0x7e },
  rti: { imp: 0x40 }, rts: { imp: 0x60 },
  sbc: { imm: 0xe9, zp: 0xe5, zpx: 0xf5, abs: 0xed, absx: 0xfd, absy: 0xf9, indx: 0xe1, indy: 0xf1 },
  sec: { imp: 0x38 }, sed: { imp: 0xf8 }, sei: { imp: 0x78 },
  sta: { zp: 0x85, zpx: 0x95, abs: 0x8d, absx: 0x9d, absy: 0x99, indx: 0x81, indy: 0x91 },
  stx: { zp: 0x86, zpy: 0x96, abs: 0x8e }, sty: { zp: 0x84, zpx: 0x94, abs: 0x8c },
  tax: { imp: 0xaa }, tay: { imp: 0xa8 }, tsx: { imp: 0xba }, txa: { imp: 0x8a }, txs: { imp: 0x9a }, tya: { imp: 0x98 },
};
const DIRECTIVES = new Set(["org", "base", "pad", "align", "fillvalue", "db", "byte", "dw", "word", "dsb", "res", "hex"]);
const MAX_OUTPUT = 8 * 1024 * 1024;

// Split on top-level commas, leaving strings, character literals and parentheses whole.
function split(text) {
  const parts = []; let depth = 0, quote = "", start = 0;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (quote) { if (c === quote) quote = ""; }
    else if (c === '"' || c === "'") quote = c;
    else if (c === "(") depth++;
    else if (c === ")") depth--;
    else if (c === "," && !depth) { parts.push(text.slice(start, i).trim()); start = i + 1; }
  }
  parts.push(text.slice(start).trim());
  return parts;
}
function uncomment(line) {
  let quote = "";
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (quote) { if (c === quote) quote = ""; }
    else if (c === '"' || c === "'") quote = c;
    else if (c === ";") return line.slice(0, i);
  }
  return line;
}

// An expression's value, or null while a name in it is not yet known.
function evaluate(text, scope) {
  let at = 0; const src = text;
  const fail = (message) => { throw new Error(`${message} in "${src.trim()}"`); };
  const space = () => { while (src[at] === " " || src[at] === "\t") at++; };
  const both = (a, b, f) => (a === null || b === null ? null : f(a, b));
  function atom() {
    space();
    const rest = src.slice(at);
    let m;
    if ((m = /^\$([0-9a-fA-F]+)/.exec(rest))) { at += m[0].length; return parseInt(m[1], 16); }
    if ((m = /^%([01]+)/.exec(rest))) { at += m[0].length; return parseInt(m[1], 2); }
    if ((m = /^([0-9a-fA-F]+)h\b/.exec(rest)) && /^[0-9]/.test(m[1])) { at += m[0].length; return parseInt(m[1], 16); }
    if ((m = /^[0-9]+/.exec(rest))) { at += m[0].length; return parseInt(m[0], 10); }
    if ((m = /^'(.)'/.exec(rest))) { at += 3; return m[1].charCodeAt(0); }
    if (rest[0] === "(") { at++; const value = low(); space(); if (src[at] !== ")") fail("Missing )"); at++; return value; }
    if (rest[0] === "$" || rest[0] === "*") { at++; return scope.pc; }
    if ((m = /^[A-Za-z_@][\w@.]*/.exec(rest))) { at += m[0].length; return scope.lookup(m[0]); }
    return fail("Expected a value");
  }
  function unary() {
    space();
    const c = src[at];
    if (c === "<") { at++; const v = unary(); return v === null ? null : v & 255; }
    if (c === ">") { at++; const v = unary(); return v === null ? null : (v >> 8) & 255; }
    if (c === "-") { at++; const v = unary(); return v === null ? null : -v; }
    if (c === "~") { at++; const v = unary(); return v === null ? null : ~v; }
    if (c === "+") { at++; return unary(); }
    return atom();
  }
  const level = (next, table) => () => {
    let value = next();
    for (;;) {
      space();
      const op = Object.keys(table).find((key) => src.startsWith(key, at) && !(key.length === 1 && "<>".includes(key) && src[at + 1] === key));
      if (!op) return value;
      at += op.length;
      const right = next();
      if ((op === "/" || op === "%") && right === 0) fail("Division by zero");
      value = both(value, right, table[op]);
    }
  };
  const product = level(unary, { "*": (a, b) => a * b, "/": (a, b) => Math.trunc(a / b), "%": (a, b) => a % b });
  const sum = level(product, { "+": (a, b) => a + b, "-": (a, b) => a - b });
  const shift = level(sum, { "<<": (a, b) => a << b, ">>": (a, b) => a >> b });
  const and = level(shift, { "&": (a, b) => a & b });
  const xor = level(and, { "^": (a, b) => a ^ b });
  const low = level(xor, { "|": (a, b) => a | b });
  const value = low();
  space();
  if (at < src.length) fail("Unexpected text");
  return value;
}

// The addressing mode an operand is written in, with its expression.
function operand(text, modes) {
  const t = text.trim();
  if (!t) return { mode: modes.imp !== undefined ? "imp" : "acc" };
  if (/^a$/i.test(t) && modes.acc !== undefined) return { mode: "acc" };
  if (t[0] === "#") return { mode: "imm", expr: t.slice(1) };
  let m;
  if ((m = /^\((.+),\s*x\s*\)$/i.exec(t))) return { mode: "indx", expr: m[1] };
  if ((m = /^\((.+)\)\s*,\s*y$/i.exec(t))) return { mode: "indy", expr: m[1] };
  if (modes.ind !== undefined && (m = /^\((.+)\)$/.exec(t))) return { mode: "ind", expr: m[1] };
  if (modes.rel !== undefined) return { mode: "rel", expr: t };
  if ((m = /^(.+),\s*x$/i.exec(t))) return { mode: "x", expr: m[1] };
  if ((m = /^(.+),\s*y$/i.exec(t))) return { mode: "y", expr: m[1] };
  return { mode: "plain", expr: t };
}

/** Assemble asm6-style 6502 source. Returns the output bytes and the symbol table; throws with line numbers. */
export function assemble(source) {
  if (typeof source !== "string") throw new Error("Assembly source must be text.");
  const lines = source.split(/\r?\n/);
  const symbols = new Map(), errors = [];
  let scopeName = "";
  const full = (name) => (name[0] === "@" ? `${scopeName}${name}` : name).toLowerCase();
  // First pass fixes every instruction's size; the second writes bytes.
  const sizes = new Map();
  let out, pc, fill, started, final;
  const scope = { get pc() { return pc; }, lookup(name) {
    const key = full(name);
    if (symbols.has(key)) return symbols.get(key);
    if (final) throw new Error(`Unknown name ${name}`);
    return null;
  } };
  const emit = (...bytes) => {
    for (const b of bytes) {
      if (out.length >= MAX_OUTPUT) throw new Error("Output exceeds 8 MB");
      out.push(b & 255);
    }
    pc += bytes.length; started = true;
  };
  const known = (text, what) => {
    const value = evaluate(text, scope);
    if (value === null) throw new Error(`${what} must use names defined above it`);
    return value;
  };
  const byte = (value, what) => {
    if (value === null) return 0;
    if (value < -128 || value > 255) throw new Error(`${what} ${value} does not fit in a byte`);
    return value;
  };
  const padTo = (target, value) => {
    if (target < pc) throw new Error(`Cannot move back from $${pc.toString(16)} to $${target.toString(16)}`);
    if (target - pc > MAX_OUTPUT) throw new Error("Output exceeds 8 MB");
    while (pc < target) emit(value);
  };

  function directive(name, rest) {
    const args = rest.trim() ? split(rest) : [];
    if (name === "org" || name === "base") {
      const target = known(args[0] || "", "Address");
      // asm6: the first ORG only names the address; later ones fill up to it.
      if (name === "org" && started) padTo(target, args[1] ? byte(known(args[1], "Fill"), "Fill") : fill);
      else pc = target;
    } else if (name === "pad") padTo(known(args[0] || "", "Address"), args[1] ? byte(known(args[1], "Fill"), "Fill") : fill);
    else if (name === "align") {
      const size = known(args[0] || "", "Alignment");
      if (size < 1) throw new Error("Alignment must be positive");
      padTo(Math.ceil(pc / size) * size, args[1] ? byte(known(args[1], "Fill"), "Fill") : fill);
    } else if (name === "fillvalue") fill = byte(known(args[0] || "", "Fill"), "Fill");
    else if (name === "dsb" || name === "res") {
      const count = known(args[0] || "", "Count");
      if (count < 0 || count > MAX_OUTPUT) throw new Error("Count out of range");
      const value = args[1] ? byte(evaluate(args[1], scope), "Fill") : fill;
      for (let i = 0; i < count; i++) emit(value);
    } else if (name === "db" || name === "byte") {
      if (!args.length) throw new Error("No bytes given");
      for (const arg of args) {
        if (/^"[^"]*"$/.test(arg)) emit(...Buffer.from(arg.slice(1, -1), "latin1"));
        else emit(byte(evaluate(arg, scope), "Value"));
      }
    } else if (name === "dw" || name === "word") {
      if (!args.length) throw new Error("No words given");
      for (const arg of args) {
        const value = evaluate(arg, scope) ?? 0;
        if (value < -32768 || value > 65535) throw new Error(`Value ${value} does not fit in a word`);
        emit(value, value >> 8);
      }
    } else if (name === "hex") {
      const digits = rest.replace(/\s+/g, "");
      if (!/^([0-9a-fA-F]{2})+$/.test(digits)) throw new Error("HEX takes pairs of hex digits");
      emit(...Buffer.from(digits, "hex"));
    }
  }

  function instruction(index, mnemonic, rest) {
    const modes = OPS[mnemonic];
    const { mode, expr } = operand(rest, modes);
    if (mode === "imp" || mode === "acc") {
      if (modes[mode] === undefined) throw new Error(`${mnemonic.toUpperCase()} needs an operand`);
      return emit(modes[mode]);
    }
    const value = evaluate(expr, scope);
    if (mode === "rel") {
      const delta = value === null ? 0 : value - (pc + 2);
      if (final && (delta < -128 || delta > 127)) throw new Error(`Branch too far (${delta}); use JMP`);
      return emit(modes.rel, delta);
    }
    if (mode === "imm" || mode === "indx" || mode === "indy") {
      if (modes[mode] === undefined) throw new Error(`${mnemonic.toUpperCase()} has no such addressing mode`);
      return emit(modes[mode], byte(value, mode === "imm" ? "Immediate" : "Zero-page address"));
    }
    if (mode === "ind") return emit(modes.ind, value ?? 0, (value ?? 0) >> 8);
    const [short, long] = mode === "x" ? ["zpx", "absx"] : mode === "y" ? ["zpy", "absy"] : ["zp", "abs"];
    // A name not yet defined is assumed to be a full address, and stays one.
    if (!final) sizes.set(index, value !== null && value >= 0 && value < 256 && modes[short] !== undefined ? short : long);
    let chosen = sizes.get(index);
    if (modes[chosen] === undefined) chosen = modes[long] !== undefined ? long : short;
    if (modes[chosen] === undefined) throw new Error(`${mnemonic.toUpperCase()} has no such addressing mode`);
    if (chosen === short) return emit(modes[short], byte(value, "Zero-page address"));
    if (final && (value < 0 || value > 65535)) throw new Error(`Address ${value} out of range`);
    return emit(modes[chosen], value ?? 0, (value ?? 0) >> 8);
  }

  for (const pass of [1, 2]) {
    final = pass === 2; out = []; pc = 0; fill = 0; started = false; scopeName = "";
    lines.forEach((raw, index) => {
      try {
        let line = uncomment(raw).trim();
        if (!line) return;
        let m;
        if ((m = /^([A-Za-z_@][\w@.]*)\s*(?:=|\bequ\b)\s*(.+)$/i.exec(line)) && !OPS[m[1].toLowerCase()]) {
          const value = evaluate(m[2], scope);
          if (!final && symbols.has(full(m[1])) ) throw new Error(`${m[1]} is defined twice`);
          if (value !== null) symbols.set(full(m[1]), value);
          else if (final) throw new Error(`Cannot resolve ${m[1]}`);
          return;
        }
        if ((m = /^([A-Za-z_@][\w@.]*):\s*(.*)$/.exec(line))) {
          const key = full(m[1]);
          if (!final && symbols.has(key)) throw new Error(`${m[1]} is defined twice`);
          if (final && symbols.get(key) !== pc) throw new Error(`${m[1]} moved between passes`);
          symbols.set(key, pc);
          if (m[1][0] !== "@") scopeName = m[1];
          line = m[2].trim();
          if (!line) return;
        }
        m = /^(\.?)([A-Za-z][A-Za-z0-9]*)\b\s*(.*)$/.exec(line);
        if (!m) throw new Error("Not an instruction, directive or label:");
        const word = m[2].toLowerCase();
        if (DIRECTIVES.has(word)) directive(word, m[3]);
        else if (!m[1] && OPS[word]) instruction(index, word, m[3]);
        else throw new Error(`Unknown ${m[1] ? "directive" : "instruction"} ${m[1]}${m[2]}`);
      } catch (error) {
        if (final || !/Unknown name/.test(error.message)) errors.push(`line ${index + 1}: ${error.message}`);
      }
    });
  }
  if (errors.length) throw new Error(`Assembly failed:\n${[...new Set(errors)].slice(0, 20).join("\n")}`);
  return { bytes: Buffer.from(out), symbols: Object.fromEntries(symbols) };
}
