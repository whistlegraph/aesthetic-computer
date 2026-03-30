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
  fmin() { this.b.push(0x96); return this; }
  fmax() { this.b.push(0x97); return this; }
  fceil(){ this.b.push(0x8d); return this; }
  // f32 comparison
  feq()  { this.b.push(0x5b); return this; }
  fne()  { this.b.push(0x5c); return this; }
  flt()  { this.b.push(0x5d); return this; }
  fgt()  { this.b.push(0x5e); return this; }
  fle()  { this.b.push(0x5f); return this; }
  fge()  { this.b.push(0x60); return this; }
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
  lime: [0, 255, 0], brown: [139, 69, 19], beige: [245, 245, 220],
  indigo: [75, 0, 130], violet: [238, 130, 238],
  orange2: [255, 200, 0], // alias for ink 255 200 0
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

const G_W = 0, G_H = 1, G_IR = 2, G_IG = 3, G_IB = 4, G_IA = 5;
const G_SAX = 6, G_SAY = 7; // scroll accumulators (f32)
const I32 = 0x7f, F32 = 0x7d;

// ─── Constants ──────────────────────────────────────────────────────
const FPS = 30; // frames per second for time-based calculations

// ─── Function Indices ───────────────────────────────────────────────
// Imports come first in WASM function index space.
const NUM_IMPORTS = 3;
const F_SIN = 0;         // imported: (f32) → f32
const F_COS = 1;         // imported: (f32) → f32
const F_RANDOM = 2;      // imported: () → f32  [returns 0..1)
// Internal functions (offset by NUM_IMPORTS)
const F_SET_PIXEL = 3;   // (i32, i32) → ()
const F_WIPE = 4;        // (f32, f32, f32) → ()
const F_INK = 5;         // (f32, f32, f32) → ()
const F_PLOT = 6;        // (f32, f32) → ()
const F_LINE = 7;        // (f32, f32, f32, f32) → ()
const F_BOX = 8;         // (f32, f32, f32, f32) → ()
const F_CIRCLE = 9;      // (f32, f32, f32) → ()
const F_TRI = 10;        // (f32, f32, f32, f32, f32, f32) → ()
const F_SCROLL = 11;     // (f32, f32) → ()
const F_SPIN = 12;       // (f32) → ()
const F_ZOOM = 13;       // (f32) → ()
const F_CONTRAST = 14;   // (f32) → ()
const F_PAINT = 15;      // (f32, f32, f32) → ()

// ─── Runtime Function Emitters ──────────────────────────────────────

// $set_pixel(x: i32, y: i32)
// Writes a pixel at (x,y) using current ink color, alpha-blended via G_IA.
function emitSetPixel() {
  const e = new E();
  // params: 0=x, 1=y | locals: 2=offset, 3=alpha, 4=invAlpha, 5=old
  // Bounds check
  e.lg(0).i32c(0).ilt().if_().ret().end();
  e.lg(0).gg(G_W).ige().if_().ret().end();
  e.lg(1).i32c(0).ilt().if_().ret().end();
  e.lg(1).gg(G_H).ige().if_().ret().end();
  // offset = (y * width + x) * 4
  e.lg(1).gg(G_W).imul().lg(0).iadd().i32c(4).imul().ls(2);

  // alpha = G_IA
  e.gg(G_IA).ls(3);
  // Skip if alpha == 0
  e.lg(3).ieqz().if_().ret().end();

  // Fast path: alpha == 255 → direct write
  e.lg(3).i32c(255).ieq().if_();
    e.lg(2).gg(G_IR).st8();
    e.lg(2).i32c(1).iadd().gg(G_IG).st8();
    e.lg(2).i32c(2).iadd().gg(G_IB).st8();
    e.lg(2).i32c(3).iadd().i32c(255).st8();
  e.else_();
    // Alpha blend: new = (old * (255 - alpha) + ink * alpha) / 255
    e.i32c(255).lg(3).isub().ls(4); // invAlpha = 255 - alpha
    // R
    e.lg(2).ld8u().ls(5);
    e.lg(2); e.lg(5).lg(4).imul().gg(G_IR).lg(3).imul().iadd().i32c(255).idiv(); e.st8();
    // G
    e.lg(2).i32c(1).iadd().ld8u().ls(5);
    e.lg(2).i32c(1).iadd(); e.lg(5).lg(4).imul().gg(G_IG).lg(3).imul().iadd().i32c(255).idiv(); e.st8();
    // B
    e.lg(2).i32c(2).iadd().ld8u().ls(5);
    e.lg(2).i32c(2).iadd(); e.lg(5).lg(4).imul().gg(G_IB).lg(3).imul().iadd().i32c(255).idiv(); e.st8();
    // A stays 255
    e.lg(2).i32c(3).iadd().i32c(255).st8();
  e.end();

  e.end();
  return { locals: [[4, I32]], code: e.bytes() };
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

// $scroll(dx: f32, dy: f32) — shift pixels with wrapping + fractional accumulation
function emitScroll() {
  const e = new E();
  // params: 0=dx, 1=dy
  // locals: 2=idx, 3=idy, 4=total, 5=i, 6=src, 7=sx, 8=sy, 9=x, 10=y, 11=tmpOff

  // Accumulate fractional scroll: G_SAX += dx, G_SAY += dy
  e.gg(G_SAX).lg(0).fadd().gs(G_SAX);
  e.gg(G_SAY).lg(1).fadd().gs(G_SAY);
  // Extract integer parts (trunc toward zero)
  e.gg(G_SAX).f2i().ls(2); // idx
  e.gg(G_SAY).f2i().ls(3); // idy
  // Keep fractional remainders
  e.gg(G_SAX).lg(2).i2f().fsub().gs(G_SAX);
  e.gg(G_SAY).lg(3).i2f().fsub().gs(G_SAY);
  // Skip if no integer scroll
  e.lg(2).ieqz().lg(3).ieqz().iand().if_().ret().end();
  e.gg(G_W).gg(G_H).imul().ls(4); // total pixels
  // Copy current buffer to temp area (at offset total*4)
  e.i32c(0).ls(5); // i = 0
  e.gg(G_W).gg(G_H).imul().i32c(4).imul().ls(11); // tmpOff = total * 4
  e.block().loop();
  e.lg(5).lg(11).ige().brif(1);
  // mem[tmpOff + i] = mem[i]
  e.lg(5).lg(11).iadd();
  e.lg(5).ld8u();
  e.st8();
  e.lg(5).i32c(1).iadd().ls(5);
  e.br(0);
  e.end().end();

  // For each pixel (x,y), read from (x-dx, y-dy) in temp buffer with wrapping
  e.i32c(0).ls(10); // y = 0
  e.block().loop();
  e.lg(10).gg(G_H).ige().brif(1);
  e.i32c(0).ls(9); // x = 0
  e.block().loop();
  e.lg(9).gg(G_W).ige().brif(1);

  // sx = ((x - idx) % w + w) % w
  e.lg(9).lg(2).isub().gg(G_W).irem().gg(G_W).iadd().gg(G_W).irem().ls(7);
  // sy = ((y - idy) % h + h) % h
  e.lg(10).lg(3).isub().gg(G_H).irem().gg(G_H).iadd().gg(G_H).irem().ls(8);

  // src = tmpOff + (sy * w + sx) * 4
  e.lg(11).lg(8).gg(G_W).imul().lg(7).iadd().i32c(4).imul().iadd().ls(6);
  // dst = (y * w + x) * 4
  e.lg(10).gg(G_W).imul().lg(9).iadd().i32c(4).imul().ls(5);
  // copy 4 bytes
  e.lg(5).lg(6).ld8u().st8();
  e.lg(5).i32c(1).iadd().lg(6).i32c(1).iadd().ld8u().st8();
  e.lg(5).i32c(2).iadd().lg(6).i32c(2).iadd().ld8u().st8();
  e.lg(5).i32c(3).iadd().lg(6).i32c(3).iadd().ld8u().st8();

  e.lg(9).i32c(1).iadd().ls(9);
  e.br(0);
  e.end().end(); // x loop
  e.lg(10).i32c(1).iadd().ls(10);
  e.br(0);
  e.end().end(); // y loop
  e.end();
  return { locals: [[10, I32]], code: e.bytes() };
}

// ─── Rainbow Colors ─────────────────────────────────────────────────

const RAINBOW = [
  [255, 0, 0],     // red
  [255, 165, 0],   // orange
  [255, 255, 0],   // yellow
  [0, 128, 0],     // green
  [0, 0, 255],     // blue
  [75, 0, 130],    // indigo
  [238, 130, 238], // violet
];

// $spin(steps: f32) — vortex/swirl: angleChange = steps/distance from center
// Pixels near center rotate more, creating a spiral effect.
function emitSpin() {
  const e = new E();
  // param: 0=steps (f32)
  // f32 locals: 1=dx, 2=dy, 3=distSq, 4=dist, 5=angleChange, 6=sinA, 7=cosA, 8=fsx, 9=fsy, 10=fcx, 11=fcy
  // i32 locals: 12=total, 13=tmpOff, 14=x, 15=y,
  //             16=sx, 17=sy, 18=srcOff, 19=dstOff, 20=i

  // fcx = (w - 1) / 2.0, fcy = (h - 1) / 2.0
  e.gg(G_W).i2f().f32c(1).fsub().f32c(2).fdiv().ls(10);
  e.gg(G_H).i2f().f32c(1).fsub().f32c(2).fdiv().ls(11);

  // total = w * h, tmpOff = total * 4
  e.gg(G_W).gg(G_H).imul().ls(12);
  e.lg(12).i32c(4).imul().ls(13);

  // Copy buffer to temp
  e.i32c(0).ls(20);
  e.block().loop();
  e.lg(20).lg(13).ige().brif(1);
  e.lg(20).lg(13).iadd().lg(20).ld8u().st8();
  e.lg(20).i32c(1).iadd().ls(20);
  e.br(0);
  e.end().end();

  // For each (x, y)
  e.i32c(0).ls(15); // y = 0
  e.block().loop();
  e.lg(15).gg(G_H).ige().brif(1);

  e.i32c(0).ls(14); // x = 0
  e.block().loop();
  e.lg(14).gg(G_W).ige().brif(1);

  // dx = x - fcx, dy = y - fcy (all f32)
  e.lg(14).i2f().lg(10).fsub().ls(1);
  e.lg(15).i2f().lg(11).fsub().ls(2);

  // distSq = dx*dx + dy*dy
  e.lg(1).lg(1).fmul().lg(2).lg(2).fmul().fadd().ls(3);

  // dstOff = (y * w + x) * 4
  e.lg(15).gg(G_W).imul().lg(14).iadd().i32c(4).imul().ls(19);

  // if distSq < 1.0: copy directly (center pixel)
  e.lg(3).f32c(1).flt().if_();
    // srcOff in temp = tmpOff + dstOff
    e.lg(19).lg(13).iadd().ls(18);
    e.lg(19).lg(18).ld8u().st8();
    e.lg(19).i32c(1).iadd().lg(18).i32c(1).iadd().ld8u().st8();
    e.lg(19).i32c(2).iadd().lg(18).i32c(2).iadd().ld8u().st8();
    e.lg(19).i32c(3).iadd().lg(18).i32c(3).iadd().ld8u().st8();
  e.else_();
    // Vortex: angleChange = steps / distance
    e.lg(3).fsqrt().ls(4);
    e.lg(0).lg(4).fdiv().ls(5);

    // sinA = sin(angleChange), cosA = cos(angleChange)
    e.lg(5).call(F_SIN).ls(6);
    e.lg(5).call(F_COS).ls(7);

    // Rotate (dx, dy) by -angleChange using rotation matrix:
    // fsx = fcx + dx*cosA + dy*sinA
    e.lg(10).lg(1).lg(7).fmul().fadd().lg(2).lg(6).fmul().fadd().ls(8);
    // fsy = fcy - dx*sinA + dy*cosA
    e.lg(11).lg(1).lg(6).fmul().fsub().lg(2).lg(7).fmul().fadd().ls(9);

    // Nearest neighbor + wrap
    // Round to nearest (floor(x + 0.5)) for symmetric sampling
    e.lg(8).f32c(0.5).fadd().ffloor().f2i().ls(16);
    e.lg(9).f32c(0.5).fadd().ffloor().f2i().ls(17);
    e.lg(16).gg(G_W).irem().gg(G_W).iadd().gg(G_W).irem().ls(16);
    e.lg(17).gg(G_H).irem().gg(G_H).iadd().gg(G_H).irem().ls(17);

    // srcOff = tmpOff + (sy * w + sx) * 4
    e.lg(13).lg(17).gg(G_W).imul().lg(16).iadd().i32c(4).imul().iadd().ls(18);

    // Copy 4 bytes
    e.lg(19).lg(18).ld8u().st8();
    e.lg(19).i32c(1).iadd().lg(18).i32c(1).iadd().ld8u().st8();
    e.lg(19).i32c(2).iadd().lg(18).i32c(2).iadd().ld8u().st8();
    e.lg(19).i32c(3).iadd().lg(18).i32c(3).iadd().ld8u().st8();
  e.end();

  e.lg(14).i32c(1).iadd().ls(14);
  e.br(0);
  e.end().end(); // x loop
  e.lg(15).i32c(1).iadd().ls(15);
  e.br(0);
  e.end().end(); // y loop
  e.end();
  return { locals: [[11, F32], [9, I32]], code: e.bytes() };
}

// $zoom(factor: f32) — scale pixel buffer from center (nearest-neighbor)
function emitZoom() {
  const e = new E();
  // param: 0=factor (f32)
  // f32 locals: 1=inv, 2=fcx, 3=fcy, 4=fsx, 5=fsy
  // i32 locals: 6=total, 7=tmpOff, 8=x, 9=y, 10=sx, 11=sy,
  //             12=srcOff, 13=dstOff, 14=i

  e.f32c(1).lg(0).fdiv().ls(1);
  e.gg(G_W).i2f().f32c(1).fsub().f32c(2).fdiv().ls(2);
  e.gg(G_H).i2f().f32c(1).fsub().f32c(2).fdiv().ls(3);

  e.gg(G_W).gg(G_H).imul().ls(6);
  e.lg(6).i32c(4).imul().ls(7);

  // Copy buffer to temp
  e.i32c(0).ls(14);
  e.block().loop();
  e.lg(14).lg(7).ige().brif(1);
  e.lg(14).lg(7).iadd().lg(14).ld8u().st8();
  e.lg(14).i32c(1).iadd().ls(14);
  e.br(0);
  e.end().end();

  e.i32c(0).ls(9); // y = 0
  e.block().loop();
  e.lg(9).gg(G_H).ige().brif(1);

  e.i32c(0).ls(8); // x = 0
  e.block().loop();
  e.lg(8).gg(G_W).ige().brif(1);

  // fsx = fcx + (x - fcx) * inv
  e.lg(2).lg(8).i2f().lg(2).fsub().lg(1).fmul().fadd().ls(4);
  // fsy = fcy + (y - fcy) * inv
  e.lg(3).lg(9).i2f().lg(3).fsub().lg(1).fmul().fadd().ls(5);

  // Round to nearest for symmetric centering, then wrap
  e.lg(4).f32c(0.5).fadd().ffloor().f2i().ls(10);
  e.lg(5).f32c(0.5).fadd().ffloor().f2i().ls(11);
  e.lg(10).gg(G_W).irem().gg(G_W).iadd().gg(G_W).irem().ls(10);
  e.lg(11).gg(G_H).irem().gg(G_H).iadd().gg(G_H).irem().ls(11);

  // srcOff = tmpOff + (sy * w + sx) * 4
  e.lg(7).lg(11).gg(G_W).imul().lg(10).iadd().i32c(4).imul().iadd().ls(12);
  // dstOff = (y * w + x) * 4
  e.lg(9).gg(G_W).imul().lg(8).iadd().i32c(4).imul().ls(13);

  // Copy 4 bytes
  e.lg(13).lg(12).ld8u().st8();
  e.lg(13).i32c(1).iadd().lg(12).i32c(1).iadd().ld8u().st8();
  e.lg(13).i32c(2).iadd().lg(12).i32c(2).iadd().ld8u().st8();
  e.lg(13).i32c(3).iadd().lg(12).i32c(3).iadd().ld8u().st8();

  e.lg(8).i32c(1).iadd().ls(8);
  e.br(0);
  e.end().end(); // x loop
  e.lg(9).i32c(1).iadd().ls(9);
  e.br(0);
  e.end().end(); // y loop
  e.end();
  return { locals: [[5, F32], [9, I32]], code: e.bytes() };
}

// $contrast(factor: f32) — adjust pixel contrast around midpoint 128
function emitContrast() {
  const e = new E();
  // param: 0=factor (f32)
  // i32 locals: 1=total, 2=i, 3=off, 4=v
  // f32 locals: 5=result

  e.gg(G_W).gg(G_H).imul().i32c(4).imul().ls(1); // total bytes
  e.i32c(0).ls(2); // i = 0

  e.block().loop();
  e.lg(2).lg(1).ige().brif(1);

  // Process R, G, B (skip A at offset +3)
  // For each channel c in {0,1,2}:
  //   val = mem[i+c]
  //   result = 128 + (val - 128) * factor
  //   clamp to 0..255
  //   mem[i+c] = result
  for (let c = 0; c < 3; c++) {
    e.lg(2).i32c(c).iadd().ls(3); // off = i + c
    e.lg(3).ld8u().ls(4);          // v = mem[off]

    // result = 128 + (v - 128) * factor
    e.f32c(128).lg(4).i2f().f32c(128).fsub().lg(0).fmul().fadd().ls(5);

    // clamp: result = max(0, min(255, result))
    e.lg(5).f32c(0).fmax().f32c(255).fmin().ls(5);

    // store
    e.lg(3).lg(5).f2i().st8();
  }

  e.lg(2).i32c(4).iadd().ls(2); // i += 4
  e.br(0);
  e.end().end();
  e.end();
  return { locals: [[4, I32], [1, F32]], code: e.bytes() };
}

// ─── Compiler ───────────────────────────────────────────────────────

export class Compiler {
  constructor() {
    this.code = new E();
    this.nextLocal = 3;    // params: 0=w, 1=h, 2=frame
    this.paintLocals = []; // [[count, type], ...]
  }

  // Allocate a local in the paint function, returns its index
  allocLocal(type = F32) {
    const idx = this.nextLocal++;
    const last = this.paintLocals[this.paintLocals.length - 1];
    if (last && last[1] === type) last[0]++;
    else this.paintLocals.push([1, type]);
    return idx;
  }

  // How many f32 values does this expression push?
  exprArity(node) {
    if (node.t === "num") return 1;
    if (node.t === "sym") {
      if (COLORS[node.v] || node.v === "rainbow") return 3;
      return 1;
    }
    if (node.t === "list") {
      const head = node.items[0];
      if (head.t === "sym") {
        if (head.v === "?") return node.items.length > 1 ? this.exprArity(node.items[1]) : 1;
        if (head.v.match(/^\d+\.?\d*s\.\.\.$/)) return 1;
      }
    }
    return 1;
  }

  compileExpr(node) {
    if (node.t === "num") { this.code.f32c(node.v); }
    else if (node.t === "sym") { this.compileSym(node.v); }
    else if (node.t === "list") { this.compileCall(node); }
  }

  compileSym(name) {
    if (name === "w") { this.code.lg(0); return; }
    if (name === "h") { this.code.lg(1); return; }
    if (name === "frame" || name === "f") { this.code.lg(2); return; }

    // Rainbow → frame-dependent ROYGBIV color
    if (name === "rainbow") { this.compileRainbow(); return; }

    // Division shorthand: w/2, h/3, etc.
    const dm = name.match(/^(\w+)\/(\d+(?:\.\d+)?)$/);
    if (dm) { this.compileSym(dm[1]); this.code.f32c(parseFloat(dm[2])).fdiv(); return; }

    // Multiplication shorthand: w*2, h*3, etc.
    const mm = name.match(/^(\w+)\*(\d+(?:\.\d+)?)$/);
    if (mm) { this.compileSym(mm[1]); this.code.f32c(parseFloat(mm[2])).fmul(); return; }

    // Color name → 3 f32 values
    if (COLORS[name]) {
      const [r, g, b] = COLORS[name];
      this.code.f32c(r).f32c(g).f32c(b);
      return;
    }

    throw new Error(`Unknown symbol: ${name}`);
  }

  // rainbow → frame % 7 selects from ROYGBIV, pushes 3 f32s
  compileRainbow() {
    const idxL = this.allocLocal(I32);
    const rL = this.allocLocal(F32);
    const gL = this.allocLocal(F32);
    const bL = this.allocLocal(F32);

    this.code.lg(2).f2i().i32c(7).irem().ls(idxL);

    // Default to first color
    this.code.f32c(RAINBOW[0][0]).ls(rL);
    this.code.f32c(RAINBOW[0][1]).ls(gL);
    this.code.f32c(RAINBOW[0][2]).ls(bL);

    for (let i = 1; i < RAINBOW.length; i++) {
      this.code.lg(idxL).i32c(i).ieq().if_();
      this.code.f32c(RAINBOW[i][0]).ls(rL);
      this.code.f32c(RAINBOW[i][1]).ls(gL);
      this.code.f32c(RAINBOW[i][2]).ls(bL);
      this.code.end();
    }

    this.code.lg(rL).lg(gL).lg(bL);
  }

  // (? a b c) → random choice, pushes 1 or 3 f32s depending on arg type
  compileRandom(args) {
    const n = args.length;
    if (n === 0) { this.code.f32c(0); return; }
    if (n === 1) { this.compileExpr(args[0]); return; }

    const arity = this.exprArity(args[0]);
    const idxL = this.allocLocal(I32);

    // idx = floor(random() * n)
    this.code.call(F_RANDOM).f32c(n).fmul().ffloor().f2i().ls(idxL);

    if (arity === 3) {
      // Color mode
      const rL = this.allocLocal(F32);
      const gL = this.allocLocal(F32);
      const bL = this.allocLocal(F32);

      this.compileColorValue(args[0]);
      this.code.ls(bL).ls(gL).ls(rL);

      for (let i = 1; i < n; i++) {
        this.code.lg(idxL).i32c(i).ieq().if_();
        this.compileColorValue(args[i]);
        this.code.ls(bL).ls(gL).ls(rL);
        this.code.end();
      }
      this.code.lg(rL).lg(gL).lg(bL);
    } else {
      // Scalar mode
      const vL = this.allocLocal(F32);

      this.compileExpr(args[0]);
      this.code.ls(vL);

      for (let i = 1; i < n; i++) {
        this.code.lg(idxL).i32c(i).ieq().if_();
        this.compileExpr(args[i]);
        this.code.ls(vL);
        this.code.end();
      }
      this.code.lg(vL);
    }
  }

  // Emit 3 f32s (r, g, b) for a color-producing node
  compileColorValue(node) {
    if (node.t === "sym") {
      if (node.v === "rainbow") { this.compileRainbow(); return; }
      if (COLORS[node.v]) {
        const [r, g, b] = COLORS[node.v];
        this.code.f32c(r).f32c(g).f32c(b);
        return;
      }
    }
    if (node.t === "num") {
      // Treat number as grayscale
      this.code.f32c(node.v).f32c(node.v).f32c(node.v);
      return;
    }
    if (node.t === "list") {
      this.compileCall(node);
      return;
    }
  }

  // (Ns... a b) → smooth cosine oscillation between a and b over N seconds
  compileTimeInterp(periodStr, args) {
    const period = parseFloat(periodStr);
    const frames = Math.max(2, Math.round(period * FPS));

    if (args.length >= 2) {
      const aL = this.allocLocal(F32);
      this.compileExpr(args[0]);
      this.code.ls(aL);

      // result = a + (b - a) * 0.5 * (1 - cos(2π * frame / frames))
      this.compileExpr(args[1]);
      this.code.lg(aL).fsub();      // b - a
      this.code.f32c(0.5).fmul();   // (b-a)/2

      this.code.f32c(1);            // 1.0
      this.code.lg(2);              // frame (f32)
      this.code.f32c(2 * Math.PI / frames).fmul(); // angle
      this.code.call(F_COS);        // cos(angle)
      this.code.fsub();             // 1 - cos(angle)

      this.code.fmul();             // (b-a)/2 * (1 - cos)
      this.code.lg(aL).fadd();      // a + ...
    } else if (args.length === 1) {
      this.compileExpr(args[0]);
    }
  }

  // Inline fade: gradient fill (horizontal, compile-time color stops)
  compileFadeInline(colorNames, isFirstLine) {
    const names = colorNames.split("-");
    const colors = names.map(n => {
      const c = COLORS[n.trim()];
      if (!c) throw new Error(`Unknown fade color: ${n}`);
      return c;
    });
    const numStops = colors.length;

    const yL = this.allocLocal(I32);
    const xL = this.allocLocal(I32);
    const tL = this.allocLocal(F32);
    const segL = this.allocLocal(F32);
    const idxL = this.allocLocal(I32);
    const fracL = this.allocLocal(F32);
    const rL = this.allocLocal(F32);
    const gL = this.allocLocal(F32);
    const bL = this.allocLocal(F32);
    const offL = this.allocLocal(I32);

    if (isFirstLine) this.code.lg(2).f32c(0).fle().if_();

    // for y = 0..h
    this.code.i32c(0).ls(yL);
    this.code.block().loop();
    this.code.lg(yL).gg(G_H).ige().brif(1);

    // for x = 0..w
    this.code.i32c(0).ls(xL);
    this.code.block().loop();
    this.code.lg(xL).gg(G_W).ige().brif(1);

    // t = x / max(w - 1, 1)
    this.code.gg(G_W).i32c(1).isub().ls(offL);
    this.code.lg(offL).ieqz().if_();
    this.code.f32c(0).ls(tL);
    this.code.else_();
    this.code.lg(xL).i2f().lg(offL).i2f().fdiv().ls(tL);
    this.code.end();

    // segment = t * (numStops - 1), idx = floor, frac = segment - idx
    this.code.lg(tL).f32c(numStops - 1).fmul().ls(segL);
    this.code.lg(segL).ffloor().f2i().ls(idxL);
    this.code.lg(segL).lg(idxL).i2f().fsub().ls(fracL);
    // Clamp idx
    this.code.lg(idxL).i32c(numStops - 2).igt().if_();
    this.code.i32c(numStops - 2).ls(idxL);
    this.code.f32c(1).ls(fracL);
    this.code.end();

    // Branch on idx for color lerp
    this.code.f32c(colors[0][0]).ls(rL);
    this.code.f32c(colors[0][1]).ls(gL);
    this.code.f32c(colors[0][2]).ls(bL);

    for (let i = 0; i < numStops - 1; i++) {
      const c0 = colors[i], c1 = colors[i + 1];
      this.code.lg(idxL).i32c(i).ieq().if_();
      this.code.f32c(c0[0]).f32c(c1[0]).f32c(c0[0]).fsub().lg(fracL).fmul().fadd().ls(rL);
      this.code.f32c(c0[1]).f32c(c1[1]).f32c(c0[1]).fsub().lg(fracL).fmul().fadd().ls(gL);
      this.code.f32c(c0[2]).f32c(c1[2]).f32c(c0[2]).fsub().lg(fracL).fmul().fadd().ls(bL);
      this.code.end();
    }

    // Write pixel
    this.code.lg(yL).gg(G_W).imul().lg(xL).iadd().i32c(4).imul().ls(offL);
    this.code.lg(offL).lg(rL).f2i().st8();
    this.code.lg(offL).i32c(1).iadd().lg(gL).f2i().st8();
    this.code.lg(offL).i32c(2).iadd().lg(bL).f2i().st8();
    this.code.lg(offL).i32c(3).iadd().i32c(255).st8();

    this.code.lg(xL).i32c(1).iadd().ls(xL);
    this.code.br(0);
    this.code.end().end(); // x loop
    this.code.lg(yL).i32c(1).iadd().ls(yL);
    this.code.br(0);
    this.code.end().end(); // y loop

    if (isFirstLine) this.code.end();
  }

  compileCall(node) {
    if (node.items.length === 0) return;
    const head = node.items[0];
    if (head.t !== "sym") throw new Error(`Expected function name, got ${JSON.stringify(head)}`);

    const name = head.v;
    const args = node.items.slice(1);

    // Random choice: (? a b c)
    if (name === "?") { this.compileRandom(args); return; }

    // Time interpolation: (2s... a b) → lerp
    const interpMatch = name.match(/^(\d+\.?\d*)s\.\.\.$/);
    if (interpMatch) { this.compileTimeInterp(interpMatch[1], args); return; }

    // Periodic execution: (0.5s expr) → run every N seconds
    const periodicMatch = name.match(/^(\d+\.?\d*)s$/);
    if (periodicMatch) {
      const frames = Math.max(1, Math.round(parseFloat(periodicMatch[1]) * FPS));
      this.code.lg(2).f2i().i32c(frames).irem().ieqz().if_();
      for (const arg of args) {
        if (arg.t === "list") this.compileCall(arg);
        else this.compileExpr(arg);
      }
      this.code.end();
      return;
    }

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
    if (name === "sin") { this.compileExpr(args[0]); this.code.call(F_SIN); return; }
    if (name === "cos") { this.compileExpr(args[0]); this.code.call(F_COS); return; }

    // ink — special: color expressions can push 3 values + optional alpha
    if (name === "ink") {
      let totalArity = 0;
      for (const arg of args) {
        const a = this.exprArity(arg);
        this.compileExpr(arg);
        totalArity += a;
      }
      if (totalArity >= 4) {
        // Stack: [r, g, b, alpha, ...extras]. Drop extras, pop alpha → G_IA
        while (totalArity > 4) { this.code.drop(); totalArity--; }
        this.code.f2i().gs(G_IA);
      } else {
        while (totalArity > 3) { this.code.drop(); totalArity--; }
        this.code.i32c(255).gs(G_IA); // no alpha → fully opaque
      }
      this.code.call(F_INK);
      return;
    }

    // Drawing + transform functions
    const funcMap = {
      wipe: F_WIPE, plot: F_PLOT,
      line: F_LINE, box: F_BOX, circle: F_CIRCLE, tri: F_TRI,
      scroll: F_SCROLL, spin: F_SPIN, zoom: F_ZOOM, contrast: F_CONTRAST,
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

    // Paint params: 0=w, 1=h, 2=frame
    this.code.lg(0).f2i().gs(G_W);
    this.code.lg(1).f2i().gs(G_H);

    for (let i = 0; i < ast.length; i++) {
      const node = ast[i];
      const isFirstLine = i === 0;

      // fade: directive
      if (node.t === "sym" && node.v.startsWith("fade:")) {
        const parts = node.v.slice(5).split(":");
        this.compileFadeInline(parts[0], isFirstLine);
        continue;
      }

      // Bare color name → wipe
      if (node.t === "sym" && COLORS[node.v]) {
        const [r, g, b] = COLORS[node.v];
        if (isFirstLine) {
          this.code.lg(2).f32c(0).fle().if_();
          this.code.f32c(r).f32c(g).f32c(b).call(F_WIPE);
          this.code.end();
        } else {
          this.code.f32c(r).f32c(g).f32c(b).call(F_WIPE);
        }
        continue;
      }

      if (node.t === "list") {
        this.compileCall(node);
      }
    }

    return this.buildModule();
  }

  buildModule() {
    const out = [];
    out.push(0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00);

    // ── Types ──
    const T_I32_I32 = 0;       // (i32,i32)→()
    const T_F32_F32_F32 = 1;   // (f32,f32,f32)→()
    const T_F32_F32 = 2;       // (f32,f32)→()
    const T_F32x4 = 3;         // (f32,f32,f32,f32)→()
    const T_F32x6 = 4;         // (f32,f32,f32,f32,f32,f32)→()
    const T_F32_RET = 5;       // (f32)→f32  [sin, cos]
    const T_VOID_F32 = 6;      // ()→f32     [random]
    const T_F32_VOID = 7;      // (f32)→()   [spin, zoom, contrast]

    const types = [
      [0x60, 2, I32, I32, 0],
      [0x60, 3, F32, F32, F32, 0],
      [0x60, 2, F32, F32, 0],
      [0x60, 4, F32, F32, F32, F32, 0],
      [0x60, 6, F32, F32, F32, F32, F32, F32, 0],
      [0x60, 1, F32, 1, F32],
      [0x60, 0, 1, F32],
      [0x60, 1, F32, 0],
    ];
    const typeEntries = types.map(t => {
      const tag = t[0];
      const paramCount = t[1];
      const params = t.slice(2, 2 + paramCount);
      const resultCount = t[2 + paramCount];
      const results = t.slice(3 + paramCount);
      return [tag, ...uleb128(paramCount), ...params, ...uleb128(resultCount), ...results];
    });
    out.push(...section(1, vecOf(typeEntries)));

    // ── Imports ──
    const importEntries = [
      [...encodeString("math"), ...encodeString("sin"), 0x00, ...uleb128(T_F32_RET)],
      [...encodeString("math"), ...encodeString("cos"), 0x00, ...uleb128(T_F32_RET)],
      [...encodeString("math"), ...encodeString("random"), 0x00, ...uleb128(T_VOID_F32)],
    ];
    out.push(...section(2, vecOf(importEntries)));

    // ── Functions ──
    const funcTypes = [
      T_I32_I32,       // set_pixel
      T_F32_F32_F32,   // wipe
      T_F32_F32_F32,   // ink
      T_F32_F32,       // plot
      T_F32x4,         // line
      T_F32x4,         // box
      T_F32_F32_F32,   // circle
      T_F32x6,         // tri
      T_F32_F32,       // scroll
      T_F32_VOID,      // spin
      T_F32_VOID,      // zoom
      T_F32_VOID,      // contrast
      T_F32_F32_F32,   // paint
    ];
    out.push(...section(3, vecOf(funcTypes.map(t => [...uleb128(t)]))));

    // ── Memory ──
    out.push(...section(5, vecOf([[0x00, ...uleb128(32)]])));

    // ── Globals ──
    const globals = [
      [I32, 0x01, 0x41, ...sleb128(0), 0x0b],     // G_W: width
      [I32, 0x01, 0x41, ...sleb128(0), 0x0b],     // G_H: height
      [I32, 0x01, 0x41, ...sleb128(255), 0x0b],   // G_IR: ink red
      [I32, 0x01, 0x41, ...sleb128(255), 0x0b],   // G_IG: ink green
      [I32, 0x01, 0x41, ...sleb128(255), 0x0b],   // G_IB: ink blue
      [I32, 0x01, 0x41, ...sleb128(255), 0x0b],   // G_IA: ink alpha
      [F32, 0x01, 0x43, ...f32Bytes(0), 0x0b],    // G_SAX: scroll accum X
      [F32, 0x01, 0x43, ...f32Bytes(0), 0x0b],    // G_SAY: scroll accum Y
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
      emitSetPixel(),
      emitWipe(),
      emitInk(),
      emitPlot(),
      emitLine(),
      emitBox(),
      emitCircle(),
      emitTri(),
      emitScroll(),
      emitSpin(),
      emitZoom(),
      emitContrast(),
    ];

    const paintBody = { locals: this.paintLocals, code: [...this.code.bytes(), 0x0b] };
    const allFuncs = [...runtimeFuncs, paintBody];

    const codeBodies = allFuncs.map(fn => {
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
