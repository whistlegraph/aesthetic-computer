// KidLisp → WASM Compiler (Self-Contained Renderer)
//
// Emits a single .wasm module that contains:
//   - Linear memory with RGBA pixel buffer
//   - All rasterization algorithms (line, circle, box, etc.)
//   - The compiled piece code
//
// The host only reads memory — no rendering imports.
// Same binary → same pixels → verifiable visual compute.

// ─── WASM Encoding ──────────────────────────────────────────────────

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

function vecOf(items) {
  return [...uleb128(items.length), ...items.flat()];
}

// ─── Bytecode Emitter ───────────────────────────────────────────────

class E {
  constructor() {
    this.b = [];
  }
  // Constants
  i32c(v) { this.b.push(0x41, ...sleb128(v)); return this; }
  f32c(v) { this.b.push(0x43, ...f32Bytes(v)); return this; }
  // Locals & globals
  lg(i) { this.b.push(0x20, ...uleb128(i)); return this; }  // local.get
  ls(i) { this.b.push(0x21, ...uleb128(i)); return this; }  // local.set
  lt(i) { this.b.push(0x22, ...uleb128(i)); return this; }  // local.tee
  gg(i) { this.b.push(0x23, ...uleb128(i)); return this; }  // global.get
  gs(i) { this.b.push(0x24, ...uleb128(i)); return this; }  // global.set
  // i32 arithmetic
  iadd() { this.b.push(0x6a); return this; }
  isub() { this.b.push(0x6b); return this; }
  imul() { this.b.push(0x6c); return this; }
  idiv() { this.b.push(0x6d); return this; }
  irem() { this.b.push(0x6f); return this; }
  iand() { this.b.push(0x71); return this; }
  ior()  { this.b.push(0x72); return this; }
  // i32 comparison
  ieqz() { this.b.push(0x45); return this; }
  ieq()  { this.b.push(0x46); return this; }
  ine()  { this.b.push(0x47); return this; }
  ilt()  { this.b.push(0x48); return this; }
  igt()  { this.b.push(0x4a); return this; }
  ile()  { this.b.push(0x4c); return this; }
  ige()  { this.b.push(0x4e); return this; }
  // f32 arithmetic
  fadd() { this.b.push(0x92); return this; }
  fsub() { this.b.push(0x93); return this; }
  fmul() { this.b.push(0x94); return this; }
  fdiv() { this.b.push(0x95); return this; }
  fabs() { this.b.push(0x8b); return this; }
  fneg() { this.b.push(0x8c); return this; }
  fsqrt(){ this.b.push(0x91); return this; }
  ffloor(){this.b.push(0x8e); return this; }
  // f32 comparison
  flt()  { this.b.push(0x5b); return this; }
  fgt()  { this.b.push(0x5d); return this; }
  fle()  { this.b.push(0x5f); return this; }
  fge()  { this.b.push(0x5e); return this; }
  // Conversion
  i2f()  { this.b.push(0xb2); return this; } // i32 → f32
  f2i()  { this.b.push(0xa8); return this; } // f32 → i32 (trunc)
  // Memory
  st8(off = 0) { this.b.push(0x3a, 0x00, ...uleb128(off)); return this; }
  ld8u(off = 0){ this.b.push(0x2d, 0x00, ...uleb128(off)); return this; }
  // Control
  if_()  { this.b.push(0x04, 0x40); return this; }
  else_(){ this.b.push(0x05); return this; }
  end()  { this.b.push(0x0b); return this; }
  block(){ this.b.push(0x02, 0x40); return this; }
  loop() { this.b.push(0x03, 0x40); return this; }
  br(d)  { this.b.push(0x0c, ...uleb128(d)); return this; }
  brif(d){ this.b.push(0x0d, ...uleb128(d)); return this; }
  ret()  { this.b.push(0x0f); return this; }
  call(i){ this.b.push(0x10, ...uleb128(i)); return this; }
  drop() { this.b.push(0x1a); return this; }
  // Get raw bytes
  bytes() { return this.b; }
}

// ─── Colors ─────────────────────────────────────────────────────────

const COLORS = {
  red: [255, 0, 0], green: [0, 128, 0], blue: [0, 0, 255],
  white: [255, 255, 255], black: [0, 0, 0],
  yellow: [255, 255, 0], cyan: [0, 255, 255], magenta: [255, 0, 255],
  orange: [255, 165, 0], purple: [128, 0, 128],
  pink: [255, 192, 203], gray: [128, 128, 128], grey: [128, 128, 128],
  lime: [0, 255, 0],
};

// ─── Parser ─────────────────────────────────────────────────────────

function tokenize(source) {
  const tokens = [];
  let i = 0;
  while (i < source.length) {
    const ch = source[i];
    if (ch === "(") { tokens.push({ type: "lp" }); i++; }
    else if (ch === ")") { tokens.push({ type: "rp" }); i++; }
    else if (ch === "\n") { tokens.push({ type: "nl" }); i++; }
    else if (/\s/.test(ch)) { i++; }
    else if (ch === ";") { while (i < source.length && source[i] !== "\n") i++; }
    else {
      let start = i;
      while (i < source.length && !/[\s()]/.test(source[i])) i++;
      const atom = source.slice(start, i);
      const num = Number(atom);
      if (!isNaN(num) && atom !== "") {
        tokens.push({ type: "num", value: num });
      } else {
        tokens.push({ type: "sym", value: atom });
      }
    }
  }
  return tokens;
}

function parse(tokens) {
  const lines = [];
  let cur = [];
  let pos = 0;

  function expr() {
    if (pos >= tokens.length) return null;
    const t = tokens[pos];
    if (t.type === "lp") {
      pos++;
      const items = [];
      while (pos < tokens.length && tokens[pos].type !== "rp") {
        if (tokens[pos].type === "nl") { pos++; continue; }
        const e = expr();
        if (e) items.push(e);
      }
      if (pos < tokens.length) pos++;
      return { t: "list", items };
    } else if (t.type === "num") { pos++; return { t: "num", v: t.value }; }
    else if (t.type === "sym") { pos++; return { t: "sym", v: t.value }; }
    return null;
  }

  while (pos < tokens.length) {
    if (tokens[pos].type === "nl") {
      if (cur.length > 0) { lines.push(cur); cur = []; }
      pos++;
      continue;
    }
    const e = expr();
    if (e) cur.push(e);
  }
  if (cur.length > 0) lines.push(cur);

  const result = [];
  for (const line of lines) {
    if (line.length === 1) result.push(line[0]);
    else if (line.length > 1 && line[0].t === "sym") {
      result.push({ t: "list", items: line });
    } else {
      for (const e of line) result.push(e);
    }
  }
  return result;
}

// ─── Globals ────────────────────────────────────────────────────────

const G_W = 0, G_H = 1, G_IR = 2, G_IG = 3, G_IB = 4;
const I32 = 0x7f, F32 = 0x7d;

// ─── Function Indices ───────────────────────────────────────────────
// No imports — all functions are internal.

const F_SET_PIXEL = 0; // (i32, i32) → ()
const F_WIPE = 1;      // (f32, f32, f32) → ()
const F_INK = 2;       // (f32, f32, f32) → ()
const F_PLOT = 3;      // (f32, f32) → ()
const F_LINE = 4;      // (f32, f32, f32, f32) → ()
const F_BOX = 5;       // (f32, f32, f32, f32) → ()
const F_CIRCLE = 6;    // (f32, f32, f32) → ()
const F_TRI = 7;       // (f32, f32, f32, f32, f32, f32) → ()
const F_PAINT = 8;     // (f32, f32, f32) → ()

// ─── Runtime Function Emitters ──────────────────────────────────────

// $set_pixel(x: i32, y: i32)
// Writes a pixel at (x,y) using current ink color.
function emitSetPixel() {
  const e = new E();
  // params: 0=x, 1=y | locals: 2=offset
  // Bounds check
  e.lg(0).i32c(0).ilt().if_().ret().end();
  e.lg(0).gg(G_W).ige().if_().ret().end();
  e.lg(1).i32c(0).ilt().if_().ret().end();
  e.lg(1).gg(G_H).ige().if_().ret().end();
  // offset = (y * width + x) * 4
  e.lg(1).gg(G_W).imul().lg(0).iadd().i32c(4).imul().ls(2);
  // store RGBA
  e.lg(2).gg(G_IR).st8();
  e.lg(2).i32c(1).iadd().gg(G_IG).st8();
  e.lg(2).i32c(2).iadd().gg(G_IB).st8();
  e.lg(2).i32c(3).iadd().i32c(255).st8();
  e.end();
  return { locals: [[1, I32]], code: e.bytes() }; // 1 i32 local (offset)
}

// $wipe(r: f32, g: f32, b: f32)
function emitWipe() {
  const e = new E();
  // params: 0=r, 1=g, 2=b | locals: 3=i, 4=total, 5=ri, 6=gi, 7=bi
  e.lg(0).f2i().ls(5);
  e.lg(1).f2i().ls(6);
  e.lg(2).f2i().ls(7);
  // total = w * h * 4
  e.gg(G_W).gg(G_H).imul().i32c(4).imul().ls(4);
  // i = 0
  e.i32c(0).ls(3);
  // loop
  e.block().loop();
  e.lg(3).lg(4).ige().brif(1);
  e.lg(3).lg(5).st8();
  e.lg(3).i32c(1).iadd().lg(6).st8();
  e.lg(3).i32c(2).iadd().lg(7).st8();
  e.lg(3).i32c(3).iadd().i32c(255).st8();
  e.lg(3).i32c(4).iadd().ls(3);
  e.br(0);
  e.end().end(); // loop, block
  e.end();
  return { locals: [[5, I32]], code: e.bytes() };
}

// $ink(r: f32, g: f32, b: f32)
function emitInk() {
  const e = new E();
  e.lg(0).f2i().gs(G_IR);
  e.lg(1).f2i().gs(G_IG);
  e.lg(2).f2i().gs(G_IB);
  e.end();
  return { locals: [], code: e.bytes() };
}

// $plot(x: f32, y: f32)
function emitPlot() {
  const e = new E();
  e.lg(0).f2i();
  e.lg(1).f2i();
  e.call(F_SET_PIXEL);
  e.end();
  return { locals: [], code: e.bytes() };
}

// $line(x0: f32, y0: f32, x1: f32, y1: f32) — Bresenham
function emitLine() {
  const e = new E();
  // params: 0=x0, 1=y0, 2=x1, 3=y1
  // locals: 4=ix0, 5=iy0, 6=ix1, 7=iy1, 8=dx, 9=dy, 10=sx, 11=sy, 12=err, 13=e2
  e.lg(0).f2i().ls(4);
  e.lg(1).f2i().ls(5);
  e.lg(2).f2i().ls(6);
  e.lg(3).f2i().ls(7);

  // dx = abs(ix1 - ix0)
  e.lg(6).lg(4).isub().ls(8);
  e.lg(8).i32c(0).ilt().if_();
  e.i32c(0).lg(8).isub().ls(8);
  e.end();

  // dy = abs(iy1 - iy0)
  e.lg(7).lg(5).isub().ls(9);
  e.lg(9).i32c(0).ilt().if_();
  e.i32c(0).lg(9).isub().ls(9);
  e.end();

  // sx = ix0 < ix1 ? 1 : -1
  e.lg(4).lg(6).ilt().if_();
  e.i32c(1).ls(10);
  e.else_();
  e.i32c(-1).ls(10);
  e.end();

  // sy = iy0 < iy1 ? 1 : -1
  e.lg(5).lg(7).ilt().if_();
  e.i32c(1).ls(11);
  e.else_();
  e.i32c(-1).ls(11);
  e.end();

  // err = dx - dy
  e.lg(8).lg(9).isub().ls(12);

  // Main loop
  e.block().loop();

  // plot(ix0, iy0)
  e.lg(4).lg(5).call(F_SET_PIXEL);

  // if ix0 == ix1 && iy0 == iy1: break
  e.lg(4).lg(6).ieq();
  e.lg(5).lg(7).ieq();
  e.iand().brif(1);

  // e2 = 2 * err
  e.lg(12).i32c(2).imul().ls(13);

  // if e2 > -dy: err -= dy; ix0 += sx
  e.lg(13).i32c(0).lg(9).isub().igt().if_();
  e.lg(12).lg(9).isub().ls(12);
  e.lg(4).lg(10).iadd().ls(4);
  e.end();

  // if e2 < dx: err += dx; iy0 += sy
  e.lg(13).lg(8).ilt().if_();
  e.lg(12).lg(8).iadd().ls(12);
  e.lg(5).lg(11).iadd().ls(5);
  e.end();

  e.br(0);
  e.end().end(); // loop, block
  e.end();
  return { locals: [[10, I32]], code: e.bytes() };
}

// $box(x: f32, y: f32, w: f32, h: f32)
function emitBox() {
  const e = new E();
  // params: 0=x, 1=y, 2=w, 3=h
  // locals: 4=ix, 5=iy, 6=ex, 7=ey, 8=py, 9=px
  e.lg(0).f2i().ls(4); // ix
  e.lg(1).f2i().ls(5); // iy
  e.lg(0).lg(2).fadd().f2i().ls(6); // ex = x + w
  e.lg(1).lg(3).fadd().f2i().ls(7); // ey = y + h
  // py = iy
  e.lg(5).ls(8);
  // outer loop (rows)
  e.block().loop();
  e.lg(8).lg(7).ige().brif(1);
  // px = ix
  e.lg(4).ls(9);
  // inner loop (cols)
  e.block().loop();
  e.lg(9).lg(6).ige().brif(1);
  e.lg(9).lg(8).call(F_SET_PIXEL);
  e.lg(9).i32c(1).iadd().ls(9);
  e.br(0);
  e.end().end(); // inner loop, inner block
  e.lg(8).i32c(1).iadd().ls(8);
  e.br(0);
  e.end().end(); // outer loop, outer block
  e.end();
  return { locals: [[6, I32]], code: e.bytes() };
}

// $circle(cx: f32, cy: f32, r: f32) — brute force filled
function emitCircle() {
  const e = new E();
  // params: 0=cx, 1=cy, 2=r
  // locals: 3=icx, 4=icy, 5=ir, 6=dy, 7=dx, 8=r2
  e.lg(0).f2i().ls(3);
  e.lg(1).f2i().ls(4);
  e.lg(2).f2i().ls(5);
  // r2 = ir * ir
  e.lg(5).lg(5).imul().ls(8);
  // dy = -ir
  e.i32c(0).lg(5).isub().ls(6);
  // outer loop
  e.block().loop();
  e.lg(6).lg(5).igt().brif(1);
  // dx = -ir
  e.i32c(0).lg(5).isub().ls(7);
  // inner loop
  e.block().loop();
  e.lg(7).lg(5).igt().brif(1);
  // if dx*dx + dy*dy <= r2
  e.lg(7).lg(7).imul().lg(6).lg(6).imul().iadd();
  e.lg(8).ile().if_();
  e.lg(3).lg(7).iadd(); // cx + dx
  e.lg(4).lg(6).iadd(); // cy + dy
  e.call(F_SET_PIXEL);
  e.end();
  e.lg(7).i32c(1).iadd().ls(7);
  e.br(0);
  e.end().end(); // inner
  e.lg(6).i32c(1).iadd().ls(6);
  e.br(0);
  e.end().end(); // outer
  e.end();
  return { locals: [[6, I32]], code: e.bytes() };
}

// $tri(x0,y0,x1,y1,x2,y2) — scanline fill
function emitTri() {
  const e = new E();
  // params: 0=x0,1=y0,2=x1,3=y1,4=x2,5=y2
  // locals: 6=iy0,7=iy1,8=iy2,9=minY,10=maxY,11=y,12=minX,13=maxX,14=x
  // 15=ix0,16=ix1,17=ix2,18=tmp

  // Convert to int
  e.lg(0).f2i().ls(15);
  e.lg(1).f2i().ls(6);
  e.lg(2).f2i().ls(16);
  e.lg(3).f2i().ls(7);
  e.lg(4).f2i().ls(17);
  e.lg(5).f2i().ls(8);

  // minY = min(iy0, iy1, iy2), clamped to 0
  e.lg(6).ls(9);
  e.lg(7).lg(9).ilt().if_().lg(7).ls(9).end();
  e.lg(8).lg(9).ilt().if_().lg(8).ls(9).end();
  e.lg(9).i32c(0).ilt().if_().i32c(0).ls(9).end();

  // maxY = max(iy0, iy1, iy2), clamped to height-1
  e.lg(6).ls(10);
  e.lg(7).lg(10).igt().if_().lg(7).ls(10).end();
  e.lg(8).lg(10).igt().if_().lg(8).ls(10).end();
  e.gg(G_H).i32c(1).isub().ls(18);
  e.lg(10).lg(18).igt().if_().lg(18).ls(10).end();

  // y = minY
  e.lg(9).ls(11);
  e.block().loop();
  e.lg(11).lg(10).igt().brif(1);

  // Reset scan extents
  e.gg(G_W).ls(12); // minX = width (will be narrowed)
  e.i32c(0).ls(13); // maxX = 0

  // Check each of 3 edges — inlined to avoid extra functions
  // Edge 0→1
  e.lg(6).lg(11).ile().lg(7).lg(11).igt().iand()
   .lg(7).lg(11).ile().lg(6).lg(11).igt().iand()
   .ior().if_();
  // x = ix0 + (y - iy0) * (ix1 - ix0) / (iy1 - iy0)
  e.lg(11).lg(6).isub().lg(16).lg(15).isub().imul();
  e.lg(7).lg(6).isub();
  // avoid div by zero
  e.ls(18);
  e.lg(18).ieqz().if_().i32c(1).ls(18).end();
  e.lg(18).idiv();
  e.lg(15).iadd().ls(14);
  e.lg(14).lg(12).ilt().if_().lg(14).ls(12).end();
  e.lg(14).lg(13).igt().if_().lg(14).ls(13).end();
  e.end();

  // Edge 1→2
  e.lg(7).lg(11).ile().lg(8).lg(11).igt().iand()
   .lg(8).lg(11).ile().lg(7).lg(11).igt().iand()
   .ior().if_();
  e.lg(11).lg(7).isub().lg(17).lg(16).isub().imul();
  e.lg(8).lg(7).isub();
  e.ls(18);
  e.lg(18).ieqz().if_().i32c(1).ls(18).end();
  e.lg(18).idiv();
  e.lg(16).iadd().ls(14);
  e.lg(14).lg(12).ilt().if_().lg(14).ls(12).end();
  e.lg(14).lg(13).igt().if_().lg(14).ls(13).end();
  e.end();

  // Edge 2→0
  e.lg(8).lg(11).ile().lg(6).lg(11).igt().iand()
   .lg(6).lg(11).ile().lg(8).lg(11).igt().iand()
   .ior().if_();
  e.lg(11).lg(8).isub().lg(15).lg(17).isub().imul();
  e.lg(6).lg(8).isub();
  e.ls(18);
  e.lg(18).ieqz().if_().i32c(1).ls(18).end();
  e.lg(18).idiv();
  e.lg(17).iadd().ls(14);
  e.lg(14).lg(12).ilt().if_().lg(14).ls(12).end();
  e.lg(14).lg(13).igt().if_().lg(14).ls(13).end();
  e.end();

  // Clamp and fill scanline
  e.lg(12).i32c(0).ilt().if_().i32c(0).ls(12).end();
  e.gg(G_W).i32c(1).isub().ls(18);
  e.lg(13).lg(18).igt().if_().lg(18).ls(13).end();

  // Fill from minX to maxX
  e.lg(12).ls(14);
  e.block().loop();
  e.lg(14).lg(13).igt().brif(1);
  e.lg(14).lg(11).call(F_SET_PIXEL);
  e.lg(14).i32c(1).iadd().ls(14);
  e.br(0);
  e.end().end();

  e.lg(11).i32c(1).iadd().ls(11);
  e.br(0);
  e.end().end(); // y loop
  e.end();
  return { locals: [[13, I32]], code: e.bytes() };
}

// ─── Compiler ───────────────────────────────────────────────────────

export class Compiler {
  constructor() {
    this.code = new E(); // bytecode for paint body
  }

  // Emit piece expression
  compileExpr(node) {
    if (node.t === "num") {
      this.code.f32c(node.v);
    } else if (node.t === "sym") {
      this.compileSym(node.v);
    } else if (node.t === "list") {
      this.compileCall(node);
    }
  }

  compileSym(name) {
    if (name === "w") { this.code.lg(0); return; }
    if (name === "h") { this.code.lg(1); return; }
    if (name === "frame" || name === "f") { this.code.lg(2); return; }

    // Division shorthand: w/2, h/3, etc.
    const dm = name.match(/^(\w+)\/(\d+(?:\.\d+)?)$/);
    if (dm) {
      this.compileSym(dm[1]);
      this.code.f32c(parseFloat(dm[2]));
      this.code.fdiv();
      return;
    }

    // Multiplication shorthand: w*2, h*3, etc.
    const mm = name.match(/^(\w+)\*(\d+(?:\.\d+)?)$/);
    if (mm) {
      this.compileSym(mm[1]);
      this.code.f32c(parseFloat(mm[2]));
      this.code.fmul();
      return;
    }

    // Color name → 3 f32 values
    if (COLORS[name]) {
      const [r, g, b] = COLORS[name];
      this.code.f32c(r).f32c(g).f32c(b);
      return;
    }

    throw new Error(`Unknown symbol: ${name}`);
  }

  compileCall(node) {
    if (node.items.length === 0) return;
    const head = node.items[0];
    if (head.t !== "sym") throw new Error(`Expected function name, got ${JSON.stringify(head)}`);

    const name = head.v;
    const args = node.items.slice(1);

    // Arithmetic
    const arith = { "+": "fadd", "-": "fsub", "*": "fmul", "/": "fdiv" };
    if (arith[name]) {
      this.compileExpr(args[0]);
      this.compileExpr(args[1]);
      this.code[arith[name]]();
      return;
    }

    // Math builtins
    if (name === "sqrt") { this.compileExpr(args[0]); this.code.fsqrt(); return; }
    if (name === "abs") { this.compileExpr(args[0]); this.code.fabs(); return; }
    if (name === "neg") { this.compileExpr(args[0]); this.code.fneg(); return; }
    if (name === "floor"){ this.compileExpr(args[0]); this.code.ffloor(); return; }

    // Drawing functions → internal function calls
    const funcMap = {
      wipe: F_WIPE, ink: F_INK, plot: F_PLOT,
      line: F_LINE, box: F_BOX, circle: F_CIRCLE, tri: F_TRI,
    };
    if (funcMap[name] !== undefined) {
      for (const arg of args) this.compileExpr(arg);
      this.code.call(funcMap[name]);
      return;
    }

    throw new Error(`Unknown function: ${name}`);
  }

  compile(source) {
    const ast = parse(tokenize(source));

    // Compile piece code into paint body
    // Paint starts by setting globals from params
    this.code.lg(0).f2i().gs(G_W);  // width = param 0
    this.code.lg(1).f2i().gs(G_H);  // height = param 1

    for (const node of ast) {
      if (node.t === "list") {
        this.compileCall(node);
      } else if (node.t === "sym" && COLORS[node.v]) {
        // Bare color name → wipe
        const [r, g, b] = COLORS[node.v];
        this.code.f32c(r).f32c(g).f32c(b).call(F_WIPE);
      }
    }

    return this.buildModule();
  }

  buildModule() {
    const out = [];

    // Magic + version
    out.push(0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00);

    // ── Types ──
    const types = [
      [0x60, 2, I32, I32, 0],                            // 0: (i32,i32)→()
      [0x60, 3, F32, F32, F32, 0],                       // 1: (f32,f32,f32)→()
      [0x60, 2, F32, F32, 0],                             // 2: (f32,f32)→()
      [0x60, 4, F32, F32, F32, F32, 0],                   // 3: (f32,f32,f32,f32)→()
      [0x60, 6, F32, F32, F32, F32, F32, F32, 0],         // 4: (f32,f32,f32,f32,f32,f32)→()
    ];
    // Format: 0x60 paramcount paramtypes... resultcount resulttypes...
    const typeEntries = types.map(t => {
      const tag = t[0];
      const paramCount = t[1];
      const params = t.slice(2, 2 + paramCount);
      const resultCount = t[2 + paramCount];
      const results = t.slice(3 + paramCount);
      return [tag, ...uleb128(paramCount), ...params, ...uleb128(resultCount), ...results];
    });
    out.push(...section(1, vecOf(typeEntries)));

    // ── Functions (no imports!) ──
    // Map each function to its type index
    const funcTypes = [
      0, // set_pixel: (i32,i32)→()
      1, // wipe: (f32,f32,f32)→()
      1, // ink: (f32,f32,f32)→()
      2, // plot: (f32,f32)→()
      3, // line: (f32,f32,f32,f32)→()
      3, // box: (f32,f32,f32,f32)→()
      1, // circle: (f32,f32,f32)→()
      4, // tri: (f32,f32,f32,f32,f32,f32)→()
      1, // paint: (f32,f32,f32)→()
    ];
    out.push(...section(3, vecOf(funcTypes.map(t => [...uleb128(t)]))));

    // ── Memory ──
    // 16 pages = 1MB, enough for 512x512 RGBA
    out.push(...section(5, vecOf([[0x00, ...uleb128(16)]])));

    // ── Globals ──
    const globals = [
      [I32, 0x01, 0x41, ...sleb128(0), 0x0b], // width: i32 mut = 0
      [I32, 0x01, 0x41, ...sleb128(0), 0x0b], // height: i32 mut = 0
      [I32, 0x01, 0x41, ...sleb128(255), 0x0b], // ink_r: i32 mut = 255
      [I32, 0x01, 0x41, ...sleb128(255), 0x0b], // ink_g: i32 mut = 255
      [I32, 0x01, 0x41, ...sleb128(255), 0x0b], // ink_b: i32 mut = 255
    ];
    out.push(...section(6, vecOf(globals)));

    // ── Exports ──
    const exports = [
      [...encodeString("paint"), 0x00, ...uleb128(F_PAINT)],
      [...encodeString("memory"), 0x02, ...uleb128(0)],
    ];
    out.push(...section(7, vecOf(exports)));

    // ── Code ──
    const runtimeFuncs = [
      emitSetPixel(),  // 0
      emitWipe(),      // 1
      emitInk(),       // 2
      emitPlot(),      // 3
      emitLine(),      // 4
      emitBox(),       // 5
      emitCircle(),    // 6
      emitTri(),       // 7
    ];

    // Paint function body
    const paintBody = { locals: [], code: [...this.code.bytes(), 0x0b] };

    const allFuncs = [...runtimeFuncs, paintBody];

    const codeBodies = allFuncs.map(fn => {
      // Local declarations: groups of (count, type)
      const localDecl = fn.locals.length > 0
        ? [...uleb128(fn.locals.length), ...fn.locals.flatMap(([count, type]) => [...uleb128(count), type])]
        : [0x00];
      const body = [...localDecl, ...fn.code];
      return [...uleb128(body.length), ...body];
    });

    out.push(...section(10, vecOf(codeBodies)));

    return new Uint8Array(out);
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
