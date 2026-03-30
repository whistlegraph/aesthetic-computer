// KidLisp → WASM Compiler
// Compiles KidLisp source directly to WebAssembly binary.

// ─── WASM Binary Encoding ───────────────────────────────────────────

function uleb128(value) {
  const bytes = [];
  do {
    let byte = value & 0x7f;
    value >>>= 7;
    if (value !== 0) byte |= 0x80;
    bytes.push(byte);
  } while (value !== 0);
  return bytes;
}

function sleb128(value) {
  const bytes = [];
  let more = true;
  while (more) {
    let byte = value & 0x7f;
    value >>= 7;
    if (
      (value === 0 && (byte & 0x40) === 0) ||
      (value === -1 && (byte & 0x40) !== 0)
    ) {
      more = false;
    } else {
      byte |= 0x80;
    }
    bytes.push(byte);
  }
  return bytes;
}

function f32Bytes(value) {
  const buf = new ArrayBuffer(4);
  new Float32Array(buf)[0] = value;
  return [...new Uint8Array(buf)];
}

function encodeString(str) {
  const bytes = new TextEncoder().encode(str);
  return [...uleb128(bytes.length), ...bytes];
}

function section(id, contents) {
  return [id, ...uleb128(contents.length), ...contents];
}

function vec(items) {
  return [...uleb128(items.length), ...items.flat()];
}

// ─── Color Map ──────────────────────────────────────────────────────

const COLORS = {
  red: [255, 0, 0],
  green: [0, 128, 0],
  blue: [0, 0, 255],
  white: [255, 255, 255],
  black: [0, 0, 0],
  yellow: [255, 255, 0],
  cyan: [0, 255, 255],
  magenta: [255, 0, 255],
  orange: [255, 165, 0],
  purple: [128, 0, 128],
  pink: [255, 192, 203],
  gray: [128, 128, 128],
  grey: [128, 128, 128],
  lime: [0, 255, 0],
};

// ─── Parser ─────────────────────────────────────────────────────────

function tokenize(source) {
  const tokens = [];
  let i = 0;
  while (i < source.length) {
    const ch = source[i];
    if (ch === "(") {
      tokens.push({ type: "lparen" });
      i++;
    } else if (ch === ")") {
      tokens.push({ type: "rparen" });
      i++;
    } else if (ch === "\n") {
      tokens.push({ type: "newline" });
      i++;
    } else if (/\s/.test(ch)) {
      i++;
    } else if (ch === ";") {
      while (i < source.length && source[i] !== "\n") i++;
    } else {
      let start = i;
      while (i < source.length && !/[\s()]/.test(source[i])) i++;
      const atom = source.slice(start, i);
      const num = Number(atom);
      if (!isNaN(num) && atom !== "") {
        tokens.push({ type: "number", value: num });
      } else {
        tokens.push({ type: "symbol", value: atom });
      }
    }
  }
  return tokens;
}

function parse(tokens) {
  const lines = [];
  let currentLine = [];
  let pos = 0;

  function parseExpr() {
    if (pos >= tokens.length) return null;
    const tok = tokens[pos];
    if (tok.type === "lparen") {
      pos++;
      const items = [];
      while (pos < tokens.length && tokens[pos].type !== "rparen") {
        if (tokens[pos].type === "newline") {
          pos++;
          continue;
        }
        const expr = parseExpr();
        if (expr) items.push(expr);
      }
      if (pos < tokens.length) pos++; // skip )
      return { type: "list", items };
    } else if (tok.type === "number") {
      pos++;
      return { type: "number", value: tok.value };
    } else if (tok.type === "symbol") {
      pos++;
      return { type: "symbol", value: tok.value };
    }
    return null;
  }

  while (pos < tokens.length) {
    if (tokens[pos].type === "newline") {
      if (currentLine.length > 0) {
        lines.push(currentLine);
        currentLine = [];
      }
      pos++;
      continue;
    }
    const expr = parseExpr();
    if (expr) currentLine.push(expr);
  }
  if (currentLine.length > 0) lines.push(currentLine);

  // Wrap bare lines as function calls:
  // `ink 255 0 0` → `(ink 255 0 0)`
  const result = [];
  for (const line of lines) {
    if (line.length === 1) {
      result.push(line[0]);
    } else if (line.length > 1 && line[0].type === "symbol") {
      result.push({ type: "list", items: line });
    } else {
      for (const expr of line) result.push(expr);
    }
  }
  return result;
}

// ─── WASM Opcodes ───────────────────────────────────────────────────

const OP = {
  LOCAL_GET: 0x20,
  LOCAL_SET: 0x21,
  GLOBAL_GET: 0x23,
  GLOBAL_SET: 0x24,
  CALL: 0x10,
  F32_CONST: 0x43,
  F32_ADD: 0x92,
  F32_SUB: 0x93,
  F32_MUL: 0x94,
  F32_DIV: 0x95,
  F32_SQRT: 0x91,
  F32_ABS: 0x8b,
  F32_NEG: 0x8c,
  F32_FLOOR: 0x8e,
  F32_CEIL: 0x8d,
  I32_CONST: 0x41,
  I32_ADD: 0x6a,
  DROP: 0x1a,
  END: 0x0b,
};

const F32 = 0x7d;

// ─── Compiler ───────────────────────────────────────────────────────

export class Compiler {
  constructor() {
    this.types = [];
    this.typeMap = new Map();
    this.imports = [];
    this.importCount = 0;
    this.code = [];
    this.setupImports();
  }

  addType(params, results) {
    const key = `${params.join(",")}->${results.join(",")}`;
    if (this.typeMap.has(key)) return this.typeMap.get(key);
    const idx = this.types.length;
    this.types.push({ params, results });
    this.typeMap.set(key, idx);
    return idx;
  }

  addImport(module, name, paramCount, hasReturn = false) {
    const params = Array(paramCount).fill(F32);
    const results = hasReturn ? [F32] : [];
    const typeIdx = this.addType(params, results);
    const funcIdx = this.importCount++;
    this.imports.push({ module, name, typeIdx });
    return funcIdx;
  }

  setupImports() {
    this.funcs = {};
    // Drawing primitives — all f32 params
    this.funcs.wipe = this.addImport("env", "wipe", 3);
    this.funcs.ink = this.addImport("env", "ink", 3);
    this.funcs.line = this.addImport("env", "line", 4);
    this.funcs.box = this.addImport("env", "box", 4);
    this.funcs.circle = this.addImport("env", "circle", 3);
    this.funcs.plot = this.addImport("env", "plot", 2);
    this.funcs.tri = this.addImport("env", "tri", 6);

    // paint(w, h, frame) → ()
    this.paintTypeIdx = this.addType([F32, F32, F32], []);
  }

  // Emit bytecode that pushes a value onto the WASM stack.
  compileExpr(expr) {
    if (expr.type === "number") {
      this.code.push(OP.F32_CONST, ...f32Bytes(expr.value));
    } else if (expr.type === "symbol") {
      this.compileSymbol(expr.value);
    } else if (expr.type === "list") {
      this.compileCall(expr);
    }
  }

  compileSymbol(name) {
    // paint params: 0=w, 1=h, 2=frame
    if (name === "w") {
      this.code.push(OP.LOCAL_GET, ...uleb128(0));
      return;
    }
    if (name === "h") {
      this.code.push(OP.LOCAL_GET, ...uleb128(1));
      return;
    }
    if (name === "frame" || name === "f") {
      this.code.push(OP.LOCAL_GET, ...uleb128(2));
      return;
    }

    // Division shorthand: w/2, h/3, etc.
    const divMatch = name.match(/^(\w+)\/(\d+(?:\.\d+)?)$/);
    if (divMatch) {
      this.compileSymbol(divMatch[1]);
      this.code.push(OP.F32_CONST, ...f32Bytes(parseFloat(divMatch[2])));
      this.code.push(OP.F32_DIV);
      return;
    }

    // Color names → push 3 f32 values (r, g, b)
    if (COLORS[name]) {
      const [r, g, b] = COLORS[name];
      this.code.push(OP.F32_CONST, ...f32Bytes(r));
      this.code.push(OP.F32_CONST, ...f32Bytes(g));
      this.code.push(OP.F32_CONST, ...f32Bytes(b));
      return;
    }

    throw new Error(`Unknown symbol: ${name}`);
  }

  compileCall(expr) {
    if (expr.items.length === 0) return;
    const head = expr.items[0];
    if (head.type !== "symbol") {
      throw new Error(`Expected function name, got ${JSON.stringify(head)}`);
    }

    const name = head.value;
    const args = expr.items.slice(1);

    // Arithmetic
    const arithOp = { "+": OP.F32_ADD, "-": OP.F32_SUB, "*": OP.F32_MUL, "/": OP.F32_DIV };
    if (arithOp[name]) {
      this.compileExpr(args[0]);
      this.compileExpr(args[1]);
      this.code.push(arithOp[name]);
      return;
    }

    // Math builtins
    if (name === "sqrt") {
      this.compileExpr(args[0]);
      this.code.push(OP.F32_SQRT);
      return;
    }
    if (name === "abs") {
      this.compileExpr(args[0]);
      this.code.push(OP.F32_ABS);
      return;
    }
    if (name === "neg") {
      this.compileExpr(args[0]);
      this.code.push(OP.F32_NEG);
      return;
    }
    if (name === "floor") {
      this.compileExpr(args[0]);
      this.code.push(OP.F32_FLOOR);
      return;
    }

    // Drawing functions
    if (this.funcs[name] !== undefined) {
      for (const arg of args) this.compileExpr(arg);
      this.code.push(OP.CALL, ...uleb128(this.funcs[name]));
      return;
    }

    throw new Error(`Unknown function: ${name}`);
  }

  compile(source) {
    const tokens = tokenize(source);
    const ast = parse(tokens);

    for (const expr of ast) {
      if (expr.type === "list") {
        this.compileCall(expr);
      } else if (expr.type === "symbol" && COLORS[expr.value]) {
        // Bare color name on a line → wipe with that color
        const [r, g, b] = COLORS[expr.value];
        this.code.push(OP.F32_CONST, ...f32Bytes(r));
        this.code.push(OP.F32_CONST, ...f32Bytes(g));
        this.code.push(OP.F32_CONST, ...f32Bytes(b));
        this.code.push(OP.CALL, ...uleb128(this.funcs.wipe));
      }
    }

    return this.emit();
  }

  emit() {
    const bytes = [];

    // Magic + version
    bytes.push(0x00, 0x61, 0x73, 0x6d); // \0asm
    bytes.push(0x01, 0x00, 0x00, 0x00); // version 1

    // ── Type section (1) ──
    const typeEntries = this.types.map((t) => [
      0x60,
      ...uleb128(t.params.length),
      ...t.params,
      ...uleb128(t.results.length),
      ...t.results,
    ]);
    bytes.push(...section(1, vec(typeEntries)));

    // ── Import section (2) ──
    const importEntries = this.imports.map((imp) => [
      ...encodeString(imp.module),
      ...encodeString(imp.name),
      0x00, // func
      ...uleb128(imp.typeIdx),
    ]);
    bytes.push(...section(2, vec(importEntries)));

    // ── Function section (3) — declare paint ──
    bytes.push(...section(3, vec([[...uleb128(this.paintTypeIdx)]])));

    // ── Export section (7) ──
    const paintIdx = this.importCount; // first non-import func
    const exportEntries = [
      [...encodeString("paint"), 0x00, ...uleb128(paintIdx)],
    ];
    bytes.push(...section(7, vec(exportEntries)));

    // ── Code section (10) ──
    const body = [
      0x00, // 0 local declarations
      ...this.code,
      OP.END,
    ];
    const codeEntry = [...uleb128(body.length), ...body];
    bytes.push(...section(10, vec([codeEntry])));

    return new Uint8Array(bytes);
  }
}

// ─── CLI ────────────────────────────────────────────────────────────

import { readFileSync, writeFileSync } from "fs";
import { fileURLToPath } from "url";

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const input = process.argv[2];
  if (!input) {
    console.error("Usage: node compiler.mjs <input.lisp> [output.wasm]");
    process.exit(1);
  }
  const output = process.argv[3] || input.replace(/\.lisp$/, ".wasm");
  const source = readFileSync(input, "utf-8");
  const compiler = new Compiler();
  const wasm = compiler.compile(source);
  writeFileSync(output, wasm);
  console.log(`Compiled ${input} → ${output} (${wasm.length} bytes)`);
}
