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
//   CAMERA x y z yaw pitch fov near              (the 3D layer, kidlisp-mesh.mjs)
//   PLACE  mesh x y z yaw pitch roll scale a     mesh = an id in frame.meshes
//   LIGHT  x y z                                 the sun's direction (towards which it shines)
export const OP = Object.freeze({ CLEAR: 1, LINE: 2, BOX: 3, OVAL: 4, TRI: 5, SHAPE: 6, CAMERA: 7, PLACE: 8, LIGHT: 9 });

export class GpuFrame {
  constructor(capacity = 1 << 16) {
    this.buffer = new Float32Array(capacity);
    this.length = 0;
    this.r = 255; this.g = 255; this.b = 255; this.a = 255;
    this.overlay = false;      // the CPU buffer has something to composite on top (text, pastes)
    this.ops = 0;
    this.meshes = new Map();   // id → { verts, faces } (kidlisp-mesh.mjs layout), defined once, drawn by PLACE
    this.meshVersion = 0;
  }
  defineMesh(id, mesh) { this.meshes.set(id, mesh); this.meshVersion++; }
  camera(x, y, z, yaw, pitch, fov, near = 1) { this.grow(8); const o = this.buffer; let i = this.length; o[i++] = OP.CAMERA; o[i++] = x; o[i++] = y; o[i++] = z; o[i++] = yaw; o[i++] = pitch; o[i++] = fov; o[i++] = near; this.length = i; this.ops++; }
  light(x, y, z) { this.grow(4); const o = this.buffer; let i = this.length; o[i++] = OP.LIGHT; o[i++] = x; o[i++] = y; o[i++] = z; this.length = i; this.ops++; }
  place(mesh, x, y, z, yaw = 0, pitch = 0, roll = 0, scale = 1) { this.grow(10); const o = this.buffer; let i = this.length; o[i++] = OP.PLACE; o[i++] = mesh; o[i++] = x; o[i++] = y; o[i++] = z; o[i++] = yaw; o[i++] = pitch; o[i++] = roll; o[i++] = scale; o[i++] = this.a; this.length = i; this.ops++; }
  reset() { this.length = 0; this.ops = 0; this.overlay = false; }
  grow(extra) {
    if (this.length + extra <= this.buffer.length) return;
    const next = new Float32Array(Math.max(this.buffer.length * 2, this.length + extra));
    next.set(this.buffer.subarray(0, this.length)); this.buffer = next;
  }
  ink(r, g, b, a = 255) { this.r = r; this.g = g; this.b = b; this.a = a; }
  // Fixed-arity writes (no rest arguments): an engine without a JIT allocates
  // an array per spread, and a frame is thousands of these.
  clear(r = this.r, g = this.g, b = this.b, a = 255) { this.grow(5); const o = this.buffer; let i = this.length; o[i++] = OP.CLEAR; o[i++] = r; o[i++] = g; o[i++] = b; o[i++] = a; this.length = i; this.ops++; }
  line(x1, y1, x2, y2, thickness = 1) { this.grow(10); const o = this.buffer; let i = this.length; o[i++] = OP.LINE; o[i++] = x1; o[i++] = y1; o[i++] = x2; o[i++] = y2; o[i++] = thickness; o[i++] = this.r; o[i++] = this.g; o[i++] = this.b; o[i++] = this.a; this.length = i; this.ops++; }
  box(x, y, w, h, fill = 1) { this.grow(10); const o = this.buffer; let i = this.length; o[i++] = OP.BOX; o[i++] = x; o[i++] = y; o[i++] = w; o[i++] = h; o[i++] = fill; o[i++] = this.r; o[i++] = this.g; o[i++] = this.b; o[i++] = this.a; this.length = i; this.ops++; }
  oval(cx, cy, rx, ry, fill = 1) { this.grow(10); const o = this.buffer; let i = this.length; o[i++] = OP.OVAL; o[i++] = cx; o[i++] = cy; o[i++] = rx; o[i++] = ry; o[i++] = fill; o[i++] = this.r; o[i++] = this.g; o[i++] = this.b; o[i++] = this.a; this.length = i; this.ops++; }
  circle(cx, cy, r, fill = 1) { this.oval(cx, cy, r, r, fill); }
  tri(x1, y1, x2, y2, x3, y3, fill = 1) { this.grow(12); const o = this.buffer; let i = this.length; o[i++] = OP.TRI; o[i++] = x1; o[i++] = y1; o[i++] = x2; o[i++] = y2; o[i++] = x3; o[i++] = y3; o[i++] = fill; o[i++] = this.r; o[i++] = this.g; o[i++] = this.b; o[i++] = this.a; this.length = i; this.ops++; }
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
    else if (op === OP.CAMERA) { visit.camera?.(buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3], buffer[i + 4], buffer[i + 5], buffer[i + 6]); i += 7; }
    else if (op === OP.PLACE) { visit.place?.(buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3], buffer[i + 4], buffer[i + 5], buffer[i + 6], buffer[i + 7], buffer[i + 8]); i += 9; }
    else if (op === OP.LIGHT) { visit.light?.(buffer[i], buffer[i + 1], buffer[i + 2]); i += 3; }
    else return i; // unknown op: stop
  }
  return i;
}
