// A frame of drawing as one buffer (kidlisp/PIECE-IL.md §9).
//
// A compiled KidLisp piece on the GPU path does not call the software
// rasterizer; it records its drawing into this builder, one Float32Array a
// frame, and the worker hands the buffer to bios in a single message. The
// renderer (gpu-frame-renderer.mjs) turns it into one vertex buffer and
// one draw. The format is the contract, so a native host can consume it
// as easily as the browser can.
//
// Op layout (floats): op, then its fields. Colours are 0..255 RGBA.
//   CLEAR  r g b a
//   LINE   x1 y1 x2 y2 thickness r g b a
//   BOX    x y w h fill r g b a
//   OVAL   cx cy rx ry fill r g b a
//   TRI    x1 y1 x2 y2 x3 y3 fill r g b a
//   SHAPE  n fill r g b a  x1 y1 … xn yn
export const OP = Object.freeze({ CLEAR: 1, LINE: 2, BOX: 3, OVAL: 4, TRI: 5, SHAPE: 6 });

export class GpuFrame {
  constructor(capacity = 1 << 16) {
    this.buffer = new Float32Array(capacity);
    this.length = 0;
    this.r = 255; this.g = 255; this.b = 255; this.a = 255;
    this.overlay = false;      // the CPU buffer has something to composite on top (text, pastes)
    this.ops = 0;
  }
  reset() { this.length = 0; this.ops = 0; this.overlay = false; }
  grow(extra) {
    if (this.length + extra <= this.buffer.length) return;
    const next = new Float32Array(Math.max(this.buffer.length * 2, this.length + extra));
    next.set(this.buffer.subarray(0, this.length)); this.buffer = next;
  }
  ink(r, g, b, a = 255) { this.r = r; this.g = g; this.b = b; this.a = a; }
  push(...values) { this.grow(values.length); for (let i = 0; i < values.length; i++) this.buffer[this.length++] = values[i]; this.ops++; }
  clear(r = this.r, g = this.g, b = this.b, a = 255) { this.push(OP.CLEAR, r, g, b, a); }
  line(x1, y1, x2, y2, thickness = 1) { this.push(OP.LINE, x1, y1, x2, y2, thickness, this.r, this.g, this.b, this.a); }
  box(x, y, w, h, fill = 1) { this.push(OP.BOX, x, y, w, h, fill, this.r, this.g, this.b, this.a); }
  oval(cx, cy, rx, ry, fill = 1) { this.push(OP.OVAL, cx, cy, rx, ry, fill, this.r, this.g, this.b, this.a); }
  circle(cx, cy, r, fill = 1) { this.push(OP.OVAL, cx, cy, r, r, fill, this.r, this.g, this.b, this.a); }
  tri(x1, y1, x2, y2, x3, y3, fill = 1) { this.push(OP.TRI, x1, y1, x2, y2, x3, y3, fill, this.r, this.g, this.b, this.a); }
  shape(points, fill = 1) {
    const n = points.length >> 1; if (n < 2) return;
    this.grow(7 + n * 2);
    const b = this.buffer; let i = this.length;
    b[i++] = OP.SHAPE; b[i++] = n; b[i++] = fill; b[i++] = this.r; b[i++] = this.g; b[i++] = this.b; b[i++] = this.a;
    for (let k = 0; k < n * 2; k++) b[i++] = points[k];
    this.length = i; this.ops++;
  }
  // A copy of this frame's commands, ready to transfer.
  take() { return this.buffer.slice(0, this.length); }
}

// Walk a buffer, calling back per op. Shared by the renderer and any test.
export function readFrame(buffer, visit) {
  let i = 0;
  const n = buffer.length;
  while (i < n) {
    const op = buffer[i++];
    if (op === OP.CLEAR) { visit.clear?.(buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3]); i += 4; }
    else if (op === OP.LINE) { visit.line?.(buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3], buffer[i + 4], buffer[i + 5], buffer[i + 6], buffer[i + 7], buffer[i + 8]); i += 9; }
    else if (op === OP.BOX) { visit.box?.(buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3], buffer[i + 4], buffer[i + 5], buffer[i + 6], buffer[i + 7], buffer[i + 8]); i += 9; }
    else if (op === OP.OVAL) { visit.oval?.(buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3], buffer[i + 4], buffer[i + 5], buffer[i + 6], buffer[i + 7], buffer[i + 8]); i += 9; }
    else if (op === OP.TRI) { visit.tri?.(buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3], buffer[i + 4], buffer[i + 5], buffer[i + 6], buffer[i + 7], buffer[i + 8], buffer[i + 9], buffer[i + 10]); i += 11; }
    else if (op === OP.SHAPE) { const count = buffer[i]; const fill = buffer[i + 1]; const r = buffer[i + 2], g = buffer[i + 3], b = buffer[i + 4], a = buffer[i + 5]; i += 6; visit.shape?.(buffer.subarray(i, i + count * 2), fill, r, g, b, a); i += count * 2; }
    else return i; // unknown op: stop
  }
  return i;
}
