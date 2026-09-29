// The web's interpreter for the oskiewar frame program.
//
// The game hands the shell one Float32Array per paint — a program of
// scene-level ops — and this runs it against the shell's own drawing
// functions. It is one reader of the format; the game's immediate path and
// the console's native interpreter are the others. Keep the op table equal
// to the one at the head of the frame-program section in oskiewar.js:
//
//   1 VIEW     clipX clipY clipW clipH        NaN clipX = the whole stage
//   2 FACE     x1 y1 z1 x2 y2 z2 x3 y3 z3 r g b
//   3 DISC     x y depth radius r g b
//   4 CAPSULE  x1 y1 x2 y2 depth width r g b
//   5 TEXT     font x y size r g b string     font 0 comic 1 ywft 2 system 3 block
//   6 BOX      x y w h r g b a
//   7 LINE     x1 y1 x2 y2 width r g b
//   8 WIPE     r g b
//   9 CAMERA   position(3) right(3) up(3) forward(3) centerX centerY
//              orthoScale focal perspective depthSlope depthBase near
//              bandMinX bandMaxX bandMinY bandMaxY
//  10 WORLD    x1 y1 z1 x2 y2 z2 x3 y3 z3 r g b   a world-space face
//  11 DEPTH    mode value   WORLD depth from here on: 0 projected, 1 flat at
//              value, 2 projected plus value
//  12 ASSET    handle vertexCount faceCount, vertices (x y z), faces (four
//              ids, r g b, normal x y z)   a retained mesh, kept by handle
//  13 MESH     handle lightX lightY lightZ   draw a retained mesh, each quad
//              lit .72 + .28 * max(0, -normal·light), as two WORLD faces
//  14 MODEL    radius handle0 handle1 handle2 · origin(3) x(3) y(3) z(3) ·
//              lightX lightY lightZ   a retained mesh placed by a matrix, at
//              one of three baked levels: 0 while `radius` projects to 56 px
//              or more, 1 down to 20 px, 2 below. Vertices go to origin + x·X
//              + y·Y + z·Z; a normal goes by the cofactor matrix (exact under
//              any scale or mirror) and lights as MESH does, except a zero
//              normal, which is unlit. A face whose fourth id repeats its
//              third is one triangle. Nothing is tessellated here: the levels
//              are meshes the game baked (xbox/live/object-lisp.mjs).
//  15 ELLIPSE  x y depth · ax ay bx by · r g b   a projected circle: the
//              points x + a·cos t + b·sin t, a and b conjugate half-axes
//  16 PLATE    n · n×(x y) · depth · r g b   a flat convex polygon, n 3–16
//  17 OUTLINE  width r g b   from here on, every DISC, CAPSULE, ELLIPSE and
//              PLATE is drawn first in this ink, `width` px bigger, a hair
//              behind its own depth; width 0 stops. Flat objects draw so.
//  18 SHAPES   handle count length · records   an object's flat shapes, kept
//              by handle: per record kind (1 ball, 2 limb, 3 ring, 4 plate,
//              5 drum) · outline width (world units) and its ink rgb ·
//              nudge (world units back) · rgb · facing xyz (zero = both
//              sides), then object-space anchors: ball x y z radius; limb
//              two ends and a radius; ring centre and two radius vectors;
//              plate n and n points; drum centre, two radius vectors and
//              half its length as a vector
//  19 SKETCH   handle · origin(3) x(3) y(3) z(3)   draw kept shapes under a
//              placement: anchors projected here, filled flat, outlined,
//              one-sided ones skipped when turned away, ellipse sides picked
//              by projected size. Nothing else is expanded here.
//  20 FIGURE   sketch look pin · 16 joints (x y z)   a flat figure: the kept
//              shapes of `sketch` whose records are figure kinds (11 ball,
//              12 limb, 13 ring, 14 plate), each anchor a joint and an offset
//              — in the head's frame and head radii on joint 0 (the frame
//              from joint 1, where the head looks), in the chest's frame on
//              the body's joints (2 on: up the spine from joint 3, facing as
//              the head does, across from the right hip to the left; world
//              units), in world units on joint 1.
//              Colours below zero are slots of `look`. The joints are the
//              pose; nothing about the figure is expanded here. A joint the
//              body lacks is NaN, and what hangs on it isn't drawn. `pin`,
//              unless NaN, holds the figure at one depth (as the arenas
//              stand a fighter at the floor's near edge): each shape keeps a
//              tenth of its depth from the pelvis, enough to layer the limbs.
//  21 LOOK     handle · 10 × (r g b)   a figure's palette, kept by handle
//
// A WORLD face is taken to the current CAMERA here: to camera space, cut at
// the near plane (Sutherland-Hodgman, before the divide), projected with the
// ortho-to-perspective blend, cut to the band, and fanned. That is the one
// clip rule for world geometry; the game no longer projects it.
// Discs and capsules arrive whole and are fanned here; a face inside a second
// view (the P1 inset) is scissored to that view's rectangle here. The host's
// own coordinate limit is honoured the way the console's is: a face that
// asks for a coordinate past ±32200 is dropped, not drawn.

export const FRAME_VIEW = 1, FRAME_FACE = 2, FRAME_DISC = 3, FRAME_CAPSULE = 4,
  FRAME_TEXT = 5, FRAME_BOX = 6, FRAME_LINE = 7, FRAME_WIPE = 8,
  FRAME_CAMERA = 9, FRAME_WORLD = 10, FRAME_DEPTH = 11, FRAME_ASSET = 12,
  FRAME_MESH = 13, FRAME_MODEL = 14, FRAME_ELLIPSE = 15, FRAME_PLATE = 16,
  FRAME_OUTLINE = 17, FRAME_SHAPES = 18, FRAME_SKETCH = 19, FRAME_FIGURE = 20,
  FRAME_LOOK = 21;
// Fixed sizes; ASSET is variable and measured from its own header.
const opSize = [0, 5, 13, 8, 10, 9, 9, 9, 4, 25, 13, 3, 0, 5, 20, 11, 0, 5, 0, 14, 52, 32];
const sizeAt = (p, at) => p[at] === FRAME_ASSET ? 4 + p[at + 2] * 3 + p[at + 3] * 10
  : p[at] === FRAME_PLATE ? 6 + p[at + 1] * 2 : p[at] === FRAME_SHAPES ? 4 + p[at + 3] : opSize[p[at]] || 0;
// An outline sits this far behind its own shape, so the shape covers it and
// anything nearer covers both.
const outlineBehind = 2e-6;

// Ring tables by radius bucket, as the game fans them: a small disc is a
// hexagon, a large one thirty-two sides.
const discRings = [6, 8, 12, 16, 24, 32].map((sides) => {
  const ring = [];
  for (let side = 0; side < sides; side++) {
    const a = side * Math.PI * 2 / sides;
    ring.push(Math.cos(a), Math.sin(a));
  }
  return ring;
});
const discRingFor = (radius) => discRings[
  radius < 6 ? 0 : radius < 13 ? 1 : radius < 26 ? 2
    : radius < 52 ? 3 : radius < 110 ? 4 : 5];
const capsuleArcs = Object.fromEntries([3, 4, 6, 8, 12].map((n) => [n,
  Array.from({ length: n + 1 }, (_, i) =>
    [Math.cos(i * Math.PI / n), Math.sin(i * Math.PI / n)]).flat()]));

const safe = (value) => value > -32200 && value < 32200;

export function createFrameVm(host) {
  const { triangle3d, triangle, box, line, wipe, write, systemWrite,
    comicWrite, ywftWrite } = host;
  const fonts = [
    comicWrite || ywftWrite || systemWrite,
    ywftWrite || systemWrite,
    systemWrite,
    write || systemWrite,
  ];
  // A face with its own depth per vertex, or, on a host without a depth
  // buffer, the flat triangle the game drew before it had one.
  const face = triangle3d
    ? triangle3d
    : (x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b) =>
      triangle(x1, y1, x2, y2, x3, y3, r, g, b);
  let clip = null;
  const scissorPoly = new Float64Array(28), scissorNext = new Float64Array(28);

  function flatFace(x1, y1, x2, y2, x3, y3, depth, r, g, b) {
    if (!(safe(x1) && safe(y1) && safe(x2) && safe(y2) && safe(x3) && safe(y3))) return;
    if (!clip) { face(x1, y1, depth, x2, y2, depth, x3, y3, depth, r, g, b); return; }
    const left = clip.x, right = clip.x + clip.w, top = clip.y, bottom = clip.y + clip.h;
    const inside = (x, y) => x >= left && x <= right && y >= top && y <= bottom;
    if (inside(x1, y1) && inside(x2, y2) && inside(x3, y3)) {
      face(x1, y1, depth, x2, y2, depth, x3, y3, depth, r, g, b);
      return;
    }
    if ((x1 < left && x2 < left && x3 < left) || (x1 > right && x2 > right && x3 > right) ||
        (y1 < top && y2 < top && y3 < top) || (y1 > bottom && y2 > bottom && y3 > bottom)) return;
    let poly = scissorPoly, next = scissorNext, n = 3;
    poly[0] = x1; poly[1] = y1; poly[2] = x2; poly[3] = y2; poly[4] = x3; poly[5] = y3;
    for (let edge = 0; edge < 4; edge++) {
      const axis = edge < 2 ? 0 : 1;
      const bound = edge === 0 ? left : edge === 1 ? right : edge === 2 ? top : bottom;
      const sign = edge === 0 || edge === 2 ? 1 : -1;
      let m = 0;
      for (let i = 0; i < n; i++) {
        const j = (i + 1) % n;
        const ax = poly[i * 2], ay = poly[i * 2 + 1], bx = poly[j * 2], by = poly[j * 2 + 1];
        const da = ((axis ? ay : ax) - bound) * sign, db = ((axis ? by : bx) - bound) * sign;
        if (da >= 0) { next[m * 2] = ax; next[m * 2 + 1] = ay; m++; }
        if ((da >= 0) !== (db >= 0)) {
          const t = da / (da - db);
          next[m * 2] = ax + (bx - ax) * t; next[m * 2 + 1] = ay + (by - ay) * t; m++;
        }
      }
      const swap = poly; poly = next; next = swap; n = m;
      if (n < 3) return;
    }
    for (let i = 1; i + 1 < n; i++)
      face(poly[0], poly[1], depth, poly[i * 2], poly[i * 2 + 1], depth,
        poly[i * 2 + 2], poly[i * 2 + 3], depth, r, g, b);
  }

  // A FACE op carries a depth per vertex; scissoring flattens it to the first
  // vertex's depth, which is what the game's own scissor did for the inset.
  function fullFace(x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b) {
    if (!clip) {
      if (safe(x1) && safe(y1) && safe(z1) && safe(x2) && safe(y2) && safe(z2) &&
          safe(x3) && safe(y3) && safe(z3))
        face(x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b);
      return;
    }
    flatFace(x1, y1, x2, y2, x3, y3, z1, r, g, b);
  }

  function disc(x, y, depth, radius, r, g, b) {
    const ring = discRingFor(radius);
    const originX = x + ring[0] * radius, originY = y + ring[1] * radius;
    let lastX = x + ring[2] * radius, lastY = y + ring[3] * radius;
    for (let side = 4; side < ring.length; side += 2) {
      const nextX = x + ring[side] * radius;
      const nextY = y + ring[side + 1] * radius;
      flatFace(originX, originY, lastX, lastY, nextX, nextY, depth, r, g, b);
      lastX = nextX;
      lastY = nextY;
    }
  }

  function capsule(x1, y1, x2, y2, depth, width, r, g, b) {
    const dx = x2 - x1, dy = y2 - y1;
    const length = Math.hypot(dx, dy);
    const radius = width / 2;
    if (length < .001) { disc(x1, y1, depth, radius, r, g, b); return; }
    const nx = -dy / length * radius, ny = dx / length * radius;
    flatFace(x1 + nx, y1 + ny, x1 - nx, y1 - ny, x2 + nx, y2 + ny, depth, r, g, b);
    flatFace(x1 - nx, y1 - ny, x2 - nx, y2 - ny, x2 + nx, y2 + ny, depth, r, g, b);
    const steps = radius < 6 ? 3 : radius < 13 ? 4 : radius < 26 ? 6 : radius < 52 ? 8 : 12;
    const ring = capsuleArcs[steps], ux = dx / length, uy = dy / length;
    for (const end of [-1, 1]) {
      const cx = end < 0 ? x1 : x2, cy = end < 0 ? y1 : y2;
      let ax = cx + nx, ay = cy + ny;
      for (let i = 1; i <= steps; i++) {
        const side = ring[i * 2], along = ring[i * 2 + 1] * end * radius;
        const bx = cx + nx * side + ux * along, by = cy + ny * side + uy * along;
        flatFace(cx, cy, ax, ay, bx, by, depth, r, g, b);
        ax = bx; ay = by;
      }
    }
  }

  // The camera WORLD faces are seen through; filled by a CAMERA op.
  const cam = new Float64Array(24);
  let hasCamera = false;
  const lerp = (a, b, t) => a + (b - a) * t;
  const toView = (x, y, z) => {
    const dx = x - cam[0], dy = y - cam[1], dz = z - cam[2];
    return { x: dx * cam[3] + dy * cam[4] + dz * cam[5],
      y: dx * cam[6] + dy * cam[7] + dz * cam[8],
      z: dx * cam[9] + dy * cam[10] + dz * cam[11] };
  };
  const project = (v) => {
    const depth = Math.max(cam[19], v.z);
    const perspective = cam[16];
    const orthoX = cam[12] + v.x * cam[14], orthoY = cam[13] - v.y * cam[14];
    const perspectiveX = cam[12] + v.x * cam[15] / depth;
    const perspectiveY = cam[13] - v.y * cam[15] / depth;
    const z = depth * cam[17] + cam[18];
    return { x: lerp(orthoX, perspectiveX, perspective),
      y: lerp(orthoY, perspectiveY, perspective),
      z: z < -1.499 ? -1.499 : z > 1.4 ? 1.4 : z };
  };
  const mixVertex = (a, b, t) => ({ x: lerp(a.x, b.x, t), y: lerp(a.y, b.y, t),
    z: lerp(a.z, b.z, t) });
  function clipPolygon(polygon, distance) {
    const kept = [];
    for (let i = 0; i < polygon.length; i++) {
      const current = polygon[i], next = polygon[(i + 1) % polygon.length];
      const here = distance(current), there = distance(next);
      if (here >= 0) kept.push(current);
      if ((here >= 0) !== (there >= 0)) kept.push(mixVertex(current, next, here / (here - there)));
    }
    return kept;
  }
  const bandEdges = [
    (v) => v.x - cam[20], (v) => cam[21] - v.x,
    (v) => v.y - cam[22], (v) => cam[23] - v.y,
  ];
  const inBand = (v) => v.x >= cam[20] && v.x <= cam[21] && v.y >= cam[22] && v.y <= cam[23];
  let depthMode = 0, depthValue = 0;
  function emitProjected(a, b, c, r, g, bl) {
    if (!(safe(a.x) && safe(a.y) && safe(a.z) && safe(b.x) && safe(b.y) && safe(b.z) &&
        safe(c.x) && safe(c.y) && safe(c.z))) return;
    if (depthMode === 0) { face(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z, r, g, bl); return; }
    const flat = depthMode === 1;
    face(a.x, a.y, flat ? depthValue : a.z + depthValue,
      b.x, b.y, flat ? depthValue : b.z + depthValue,
      c.x, c.y, flat ? depthValue : c.z + depthValue, r, g, bl);
  }
  function worldFace(ax, ay, az, bx, by, bz, cx, cy, cz, r, g, b) {
    if (!hasCamera) return;
    const va = toView(ax, ay, az);
    const vb = toView(bx, by, bz);
    const vc = toView(cx, cy, cz);
    const near = cam[19];
    let polygon;
    if (va.z >= near && vb.z >= near && vc.z >= near) {
      const pa = project(va), pb = project(vb), pc = project(vc);
      if (inBand(pa) && inBand(pb) && inBand(pc)) { emitProjected(pa, pb, pc, r, g, b); return; }
      polygon = [pa, pb, pc];
    } else {
      const cut = clipPolygon([va, vb, vc], (v) => v.z - near);
      if (cut.length < 3) return;
      polygon = cut.map(project);
      if (polygon.some((v) => !Number.isFinite(v.x) || !Number.isFinite(v.y))) return;
    }
    for (const edge of bandEdges) {
      if (polygon.length < 3) return;
      polygon = clipPolygon(polygon, edge);
    }
    for (let corner = 2; corner < polygon.length; corner++)
      emitProjected(polygon[0], polygon[corner - 1], polygon[corner], r, g, b);
  }

  // Retained meshes, by the handle the game gave them. They live as long as
  // this interpreter does; a page has one.
  const meshes = new Map();
  function storeMesh(p, at) {
    const handle = p[at + 1], vc = p[at + 2], fc = p[at + 3];
    const vertices = Float64Array.from(p.subarray(at + 4, at + 4 + vc * 3));
    const faces = Float64Array.from(p.subarray(at + 4 + vc * 3, at + 4 + vc * 3 + fc * 10));
    meshes.set(handle, { vertices, faces, count: fc });
  }
  function drawMesh(handle, lx, ly, lz) {
    const mesh = meshes.get(handle);
    if (!mesh) return;
    const v = mesh.vertices, f = mesh.faces;
    for (let i = 0; i < mesh.count; i++) {
      const o = i * 10;
      const a = f[o] * 3, b = f[o + 1] * 3, c = f[o + 2] * 3, d = f[o + 3] * 3;
      const k = .72 + Math.max(0, -f[o + 7] * lx - f[o + 8] * ly - f[o + 9] * lz) * .28;
      const r = Math.round(f[o + 4] * k), g = Math.round(f[o + 5] * k), bl = Math.round(f[o + 6] * k);
      worldFace(v[a], v[a + 1], v[a + 2], v[b], v[b + 1], v[b + 2], v[c], v[c + 1], v[c + 2], r, g, bl);
      worldFace(v[a], v[a + 1], v[a + 2], v[c], v[c + 1], v[c + 2], v[d], v[d + 1], v[d + 2], r, g, bl);
    }
  }

  // The level a MODEL draws at, from how big its radius looks here.
  function levelOf(p, at) {
    if (!hasCamera) return 0;
    const v = toView(p[at + 5], p[at + 6], p[at + 7]);
    if (v.z <= cam[19]) return 0;
    const px = p[at + 1] * lerp(cam[14], cam[15] / v.z, cam[16]);
    return px >= 56 ? 0 : px >= 20 ? 1 : 2;
  }
  const placed = new Float64Array(12);
  function drawModel(p, at) {
    const mesh = meshes.get(p[at + 2 + levelOf(p, at)]);
    if (!mesh) return;
    const ox = p[at + 5], oy = p[at + 6], oz = p[at + 7];
    const xx = p[at + 8], xy = p[at + 9], xz = p[at + 10];
    const yx = p[at + 11], yy = p[at + 12], yz = p[at + 13];
    const zx = p[at + 14], zy = p[at + 15], zz = p[at + 16];
    const lx = p[at + 17], ly = p[at + 18], lz = p[at + 19];
    // Cofactor columns (Y×Z, Z×X, X×Y) carry normals; a mirror flips them
    // back out and swaps the winding, as the object's own faces do.
    const ax = yy * zz - yz * zy, ay = yz * zx - yx * zz, az = yx * zy - yy * zx;
    const bx = zy * xz - zz * xy, by = zz * xx - zx * xz, bz = zx * xy - zy * xx;
    const cx = xy * yz - xz * yy, cy = xz * yx - xx * yz, cz = xx * yy - xy * yx;
    const flip = xx * ax + xy * ay + xz * az < 0 ? -1 : 1;
    const v = mesh.vertices, f = mesh.faces, w = placed;
    const put = (o, id) => {
      const x = v[id * 3], y = v[id * 3 + 1], z = v[id * 3 + 2];
      w[o] = ox + x * xx + y * yx + z * zx;
      w[o + 1] = oy + x * xy + y * yy + z * zy;
      w[o + 2] = oz + x * xz + y * yz + z * zz;
    };
    for (let i = 0; i < mesh.count; i++) {
      const o = i * 10, tri = f[o + 3] === f[o + 2];
      put(0, f[o]);
      if (flip < 0) { put(3, f[o + (tri ? 2 : 3)]); put(6, f[o + (tri ? 1 : 2)]); put(9, f[o + 1]); }
      else { put(3, f[o + 1]); put(6, f[o + 2]); put(9, f[o + 3]); }
      const nx = f[o + 7], ny = f[o + 8], nz = f[o + 9];
      let k = 1;
      if (nx || ny || nz) {
        const wx = (nx * ax + ny * bx + nz * cx) * flip, wy = (nx * ay + ny * by + nz * cy) * flip;
        const wz = (nx * az + ny * bz + nz * cz) * flip;
        const toward = -(wx * lx + wy * ly + wz * lz) / (Math.hypot(wx, wy, wz) || 1);
        k = .72 + (toward > 0 ? toward : 0) * .28;
      }
      const r = Math.round(f[o + 4] * k), g = Math.round(f[o + 5] * k), bl = Math.round(f[o + 6] * k);
      worldFace(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8], r, g, bl);
      if (!tri) worldFace(w[0], w[1], w[2], w[6], w[7], w[8], w[9], w[10], w[11], r, g, bl);
    }
  }

  // Flat shapes. An ellipse gets as many sides as keep its chords within
  // 2 px of the curve (4 to 24): a lamp far off is a diamond, a near wheel
  // round. One under a pixel isn't drawn at all.
  const sidesFor = (radius) => radius < 3 ? 4 : radius < 6 ? 5
    : Math.max(6, Math.min(24, Math.ceil(Math.PI / Math.acos(Math.max(-1, 1 - 2 / radius)))));
  // `half`: only the half from angle `from` round to `from + π`, as a fan
  // from the centre — a drum's far end, whose other half the band covers.
  function ellipse(x, y, depth, ax, ay, bx, by, r, g, b, grow = 0, half = false, from = 0) {
    if (grow) {
      const ka = 1 + grow / (Math.hypot(ax, ay) || 1), kb = 1 + grow / (Math.hypot(bx, by) || 1);
      ax *= ka; ay *= ka; bx *= kb; by *= kb;
    }
    const reach = Math.max(Math.hypot(ax, ay), Math.hypot(bx, by));
    if (reach < .75) return;
    // Sides by its mean radius, so a long thin ellipse isn't paid for as a circle.
    const n = sidesFor(Math.max(Math.sqrt(Math.abs(ax * by - ay * bx)), reach * .35));
    if (half) {
      const steps = Math.max(2, Math.ceil(n / 2));
      let lx = x + ax * Math.cos(from) + bx * Math.sin(from), ly = y + ay * Math.cos(from) + by * Math.sin(from);
      for (let i = 1; i <= steps; i++) {
        const t = from + i / steps * Math.PI, nx = x + ax * Math.cos(t) + bx * Math.sin(t), ny = y + ay * Math.cos(t) + by * Math.sin(t);
        flatFace(x, y, lx, ly, nx, ny, depth, r, g, b);
        lx = nx; ly = ny;
      }
      return;
    }
    const ox = x + ax, oy = y + ay;
    let lx = x + ax * Math.cos(Math.PI * 2 / n) + bx * Math.sin(Math.PI * 2 / n);
    let ly = y + ay * Math.cos(Math.PI * 2 / n) + by * Math.sin(Math.PI * 2 / n);
    for (let i = 2; i < n; i++) {
      const c = Math.cos(i / n * Math.PI * 2), s = Math.sin(i / n * Math.PI * 2);
      const nx = x + ax * c + bx * s, ny = y + ay * c + by * s;
      flatFace(ox, oy, lx, ly, nx, ny, depth, r, g, b);
      lx = nx; ly = ny;
    }
  }
  // A flat shape's stadium: round ends with as many steps as the 2 px chord
  // rule gives, where the game's CAPSULE keeps its own finer table.
  function stadium(x1, y1, x2, y2, depth, width, r, g, b) {
    const dx = x2 - x1, dy = y2 - y1, length = Math.hypot(dx, dy), radius = width / 2;
    if (radius < .5) return;
    const steps = Math.max(2, Math.ceil(sidesFor(radius) / 2));
    const ux = length > .001 ? dx / length : 1, uy = length > .001 ? dy / length : 0;
    const nx = -uy * radius, ny = ux * radius;
    if (length > .001) {
      flatFace(x1 + nx, y1 + ny, x1 - nx, y1 - ny, x2 + nx, y2 + ny, depth, r, g, b);
      flatFace(x1 - nx, y1 - ny, x2 - nx, y2 - ny, x2 + nx, y2 + ny, depth, r, g, b);
    }
    // Each end sweeps from the +n side, round its tip, to the −n side.
    for (const end of [-1, 1]) {
      const cx = end < 0 ? x1 : x2, cy = end < 0 ? y1 : y2, tx = ux * radius * end, ty = uy * radius * end;
      let ax = cx + nx, ay = cy + ny;
      for (let i = 1; i <= steps; i++) {
        const t = i / steps * Math.PI, c = Math.cos(t), sn = Math.sin(t);
        const bx = cx + nx * c + tx * sn, by = cy + ny * c + ty * sn;
        flatFace(cx, cy, ax, ay, bx, by, depth, r, g, b);
        ax = bx; ay = by;
      }
    }
  }
  // A convex polygon, fanned; `grow` pushes each corner out from the middle.
  function plate(p, at, n, depth, r, g, b, grow = 0) {
    let cx = 0, cy = 0;
    if (grow) { for (let i = 0; i < n; i++) { cx += p[at + i * 2] / n; cy += p[at + i * 2 + 1] / n; } }
    const x = (i) => { const v = p[at + i * 2]; if (!grow) return v; const dx = v - cx, dy = p[at + i * 2 + 1] - cy; return v + dx / (Math.hypot(dx, dy) || 1) * grow; };
    const y = (i) => { const v = p[at + i * 2 + 1]; if (!grow) return v; const dx = p[at + i * 2] - cx, dy = v - cy; return v + dy / (Math.hypot(dx, dy) || 1) * grow; };
    for (let i = 2; i < n; i++) flatFace(x(0), y(0), x(i - 1), y(i - 1), x(i), y(i), depth, r, g, b);
  }
  const ink = { width: 0, r: 0, g: 0, b: 0 };
  function outlined(op, p, at) {
    const w = ink.width, d = outlineBehind;
    if (op === FRAME_DISC) disc(p[at + 1], p[at + 2], p[at + 3] + d, p[at + 4] + w, ink.r, ink.g, ink.b);
    else if (op === FRAME_CAPSULE) capsule(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5] + d, p[at + 6] + w * 2, ink.r, ink.g, ink.b);
    else if (op === FRAME_ELLIPSE)
      ellipse(p[at + 1], p[at + 2], p[at + 3] + d, p[at + 4], p[at + 5], p[at + 6], p[at + 7], ink.r, ink.g, ink.b, w);
    else plate(p, at + 2, p[at + 1], p[at + 2 + p[at + 1] * 2] + d, ink.r, ink.g, ink.b, w);
  }

  // Sketches: an object's flat shapes, kept by handle (SHAPES) and drawn under
  // a placement (SKETCH). Each record: kind · outline width, ink rgb · nudge ·
  // rgb · facing (3; zero = both sides), then its anchors in object space.
  const sketches = new Map();
  function storeShapes(p, at) {
    sketches.set(p[at + 1], { count: p[at + 2], records: Float64Array.from(p.subarray(at + 4, at + 4 + p[at + 3])) });
  }
  const seen = new Float64Array(4 * 4), flatPoly = new Float64Array(16);
  // A placed point to the screen: x, y, view depth, px per world unit.
  function seePlaced(m, x, y, z, o) {
    const wx = m[0] + x * m[3] + y * m[6] + z * m[9], wy = m[1] + x * m[4] + y * m[7] + z * m[10];
    const wz = m[2] + x * m[5] + y * m[8] + z * m[11];
    const dx = wx - cam[0], dy = wy - cam[1], dz = wz - cam[2];
    const vz = dx * cam[9] + dy * cam[10] + dz * cam[11];
    if (!(vz >= cam[19])) return false;   // behind the lens, or a joint that isn't there
    const k = cam[14] + (cam[15] / vz - cam[14]) * cam[16];
    seen[o] = cam[12] + (dx * cam[3] + dy * cam[4] + dz * cam[5]) * k;
    seen[o + 1] = cam[13] - (dx * cam[6] + dy * cam[7] + dz * cam[8]) * k;
    seen[o + 2] = vz; seen[o + 3] = k;
    return true;
  }
  const flatDepth = (vz, nudge) => {
    const z = (vz + nudge) * cam[17] + cam[18];
    return z < -1.499 ? -1.499 : z > 1.4 ? 1.4 : z;
  };
  // Record lengths: sketch kinds 1–5, figure kinds 11–14 (anchors carry a joint).
  const shapeSize = (R, i) => R[i] >= 11 ? 12 + [5, 9, 10, 1 + 4 * R[i + 12]][R[i] - 11]
    : 12 + [0, 4, 7, 9, 1 + 3 * R[i + 12], 12][R[i]];
  const placedModel = new Float64Array(12);
  function drawSketch(p, at) {
    const sketch = sketches.get(p[at + 1]);
    if (!sketch || !hasCamera) return;
    const m = placedModel;
    for (let k = 0; k < 12; k++) m[k] = p[at + 2 + k];
    const unit = Math.cbrt(Math.abs(m[3] * (m[7] * m[11] - m[8] * m[10]) - m[4] * (m[6] * m[11] - m[8] * m[9]) +
      m[5] * (m[6] * m[10] - m[7] * m[9])));
    const R = sketch.records;
    for (let n = 0, i = 0; n < sketch.count; n++, i += shapeSize(R, i)) {
      const kind = R[i], line = R[i + 1], nudge = R[i + 5], r = R[i + 6], g = R[i + 7], b = R[i + 8], a = i + 12;
      if (kind >= 11) continue;   // a figure's: drawn by FIGURE, on its joints
      const ax = kind === 4 ? R[a + 1] : R[a], ay = kind === 4 ? R[a + 2] : R[a + 1], az = kind === 4 ? R[a + 3] : R[a + 2];
      // One-sided: skip it when its facing turns from the camera.
      const fx = R[i + 9], fy = R[i + 10], fz = R[i + 11];
      if (fx || fy || fz) {
        const wx = fx * m[3] + fy * m[6] + fz * m[9], wy = fx * m[4] + fy * m[7] + fz * m[10], wz = fx * m[5] + fy * m[8] + fz * m[11];
        const px = m[0] + ax * m[3] + ay * m[6] + az * m[9], py = m[1] + ax * m[4] + ay * m[7] + az * m[10];
        const pz = m[2] + ax * m[5] + ay * m[8] + az * m[11];
        if (wx * (px - cam[0]) + wy * (py - cam[1]) + wz * (pz - cam[2]) > 0) continue;
      }
      const shape = (grow, depthShift, cr, cg, cb) => {
        if (kind === 1) {
          if (!seePlaced(m, R[a], R[a + 1], R[a + 2], 0)) return;
          const rad = R[a + 3] * seen[3] * unit;
          ellipse(seen[0], seen[1], flatDepth(seen[2], nudge) + depthShift, rad, 0, 0, rad, cr, cg, cb, grow);
        } else if (kind === 2) {
          if (!seePlaced(m, R[a], R[a + 1], R[a + 2], 0) || !seePlaced(m, R[a + 3], R[a + 4], R[a + 5], 4)) return;
          stadium(seen[0], seen[1], seen[4], seen[5], flatDepth((seen[2] + seen[6]) / 2, nudge) + depthShift,
            R[a + 6] * (seen[3] + seen[7]) * unit + grow * 2, cr, cg, cb);
        } else if (kind === 3) {
          if (!seePlaced(m, R[a], R[a + 1], R[a + 2], 0) || !seePlaced(m, R[a] + R[a + 3], R[a + 1] + R[a + 4], R[a + 2] + R[a + 5], 4) ||
            !seePlaced(m, R[a] + R[a + 6], R[a + 1] + R[a + 7], R[a + 2] + R[a + 8], 8)) return;
          ellipse(seen[0], seen[1], flatDepth(seen[2], nudge) + depthShift, seen[4] - seen[0], seen[5] - seen[1],
            seen[8] - seen[0], seen[9] - seen[1], cr, cg, cb, grow);
        } else if (kind === 4) {
          const count = R[a];
          let vz = 0;
          for (let k = 0; k < count; k++) {
            if (!seePlaced(m, R[a + 1 + k * 3], R[a + 2 + k * 3], R[a + 3 + k * 3], 0)) return;
            flatPoly[k * 2] = seen[0]; flatPoly[k * 2 + 1] = seen[1]; vz += seen[2];
          }
          plate(flatPoly, 0, count, flatDepth(vz / count, nudge) + depthShift, cr, cg, cb, grow);
        } else drum(m, R, a, nudge, grow, depthShift, cr, cg, cb);
      };
      // The ink edge is world units wide, sized where the shape is anchored.
      // Under half a pixel it is left off: a far object keeps its fills.
      if (line && seePlaced(m, ax, ay, az, 12) && line * seen[15] * unit >= .5)
        shape(line * seen[15] * unit, outlineBehind, R[i + 2], R[i + 3], R[i + 4]);
      shape(0, 0, r, g, b);
    }
  }
  // Figures: records that hang on joints. An anchor is joint j plus an
  // offset — on the head, in its frame (right, up, where it looks) and in
  // head radii. Resolved to world, then projected and filled as a sketch is.
  const looks = new Map();
  const figureSize = shapeSize;
  const J = new Float64Array(48), headFrame = new Float64Array(10), chestFrame = new Float64Array(10);
  const anchor = new Float64Array(3);
  // A joint's frame: the head's (in head radii); the body's joints turn
  // offsets with the chest, so a hem flares to the body's sides.
  const frameOf = (j) => j === 0 ? headFrame : j >= 2 ? chestFrame : null;
  function jointPoint(j, x, y, z, out = anchor) {
    const f = frameOf(j);
    if (f) {
      const r = f[9];
      for (let k = 0; k < 3; k++) out[k] = J[j * 3 + k] + r * (x * f[k] + y * f[3 + k] + z * f[6 + k]);
    } else { out[0] = J[j * 3] + x; out[1] = J[j * 3 + 1] + y; out[2] = J[j * 3 + 2] + z; }
    return out;
  }
  const seeWorld = (x, y, z, o) => seePlaced(identityPlace, x, y, z, o);
  const identityPlace = new Float64Array([0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]);
  function drawFigure(p, at) {
    const sketch = sketches.get(p[at + 1]), look = looks.get(p[at + 2]);
    if (!sketch || !hasCamera) return;
    for (let k = 0; k < 48; k++) J[k] = p[at + 4 + k];
    // Pinned, the figure's depths gather round the pin, a tenth as far apart.
    const pin = p[at + 3];
    let centre = 0;
    if (pin === pin) {
      const c = Number.isNaN(J[9]) ? 0 : 3;
      centre = (J[c * 3] - cam[0]) * cam[9] + (J[c * 3 + 1] - cam[1]) * cam[10] + (J[c * 3 + 2] - cam[2]) * cam[11];
    }
    const depthAt = (vz, nudge) => pin === pin ? pin + (flatDepth(vz, nudge) - flatDepth(centre, 0)) * .1 : flatDepth(vz, nudge);
    // The head's frame: forward to where it looks, right and up square to it.
    let fx = J[3] - J[0], fy = J[4] - J[1], fz = J[5] - J[2];
    const r = Math.hypot(fx, fy, fz) || 1;
    fx /= r; fy /= r; fz /= r;
    let rx = fz, ry = 0, rz = -fx;   // forward × world up (0, -1, 0)
    const rl = Math.hypot(rx, rz);
    if (rl < 1e-6) { rx = 1; rz = 0; } else { rx /= rl; rz /= rl; }
    const ux = ry * fz - rz * fy, uy = rz * fx - rx * fz, uz = rx * fy - ry * fx;
    headFrame.set([rx, ry, rz, ux, uy, uz, fx, fy, fz, r]);
    // The chest: up the spine, forward as the head faces, flattened square to it.
    let cux = J[6] - J[9], cuy = J[7] - J[10], cuz = J[8] - J[11];
    const cl = Math.hypot(cux, cuy, cuz) || 1;
    cux /= cl; cuy /= cl; cuz /= cl;
    const along = fx * cux + fy * cuy + fz * cuz;
    let cfx = fx - cux * along, cfy = fy - cuy * along, cfz = fz - cuz * along;
    const cfl = Math.hypot(cfx, cfy, cfz) || 1;
    cfx /= cfl; cfy /= cfl; cfz /= cfl;
    // Its x runs from the right hip to the left, whichever way the body faces
    // (the rig keeps its left hip on one side as it turns), so a hem flares out.
    let sx = J[30] - J[33], sy = J[31] - J[34], sz = J[32] - J[35];
    const side = sx * cux + sy * cuy + sz * cuz;
    sx -= cux * side; sy -= cuy * side; sz -= cuz * side;
    const sl = Math.hypot(sx, sy, sz);
    if (sl > 1e-6) { sx /= sl; sy /= sl; sz /= sl; }
    else { sx = cfy * cuz - cfz * cuy; sy = cfz * cux - cfx * cuz; sz = cfx * cuy - cfy * cux; }
    chestFrame.set([sx, sy, sz, cux, cuy, cuz, cfx, cfy, cfz, 1]);
    const R = sketch.records, pal = look || null;
    const paint = (c, o) => c[o] >= 0 || !pal ? [c[o], c[o + 1], c[o + 2]]
      : [pal[(-1 - c[o]) * 3], pal[(-1 - c[o]) * 3 + 1], pal[(-1 - c[o]) * 3 + 2]];
    for (let n = 0, i = 0; n < sketch.count; n++, i += figureSize(R, i)) {
      const kind = R[i];
      if (kind < 11) continue;
      const a = i + 12, j0 = kind === 14 ? R[a + 1] : R[a];
      const frame = frameOf(j0), scale = frame ? frame[9] : 1, nudge = R[i + 5];
      const [cr, cg, cb] = paint(R, i + 6);
      const [er, eg, eb] = paint(R, i + 2);
      const first = kind === 14 ? jointPoint(R[a + 1], R[a + 2], R[a + 3], R[a + 4]) : jointPoint(R[a], R[a + 1], R[a + 2], R[a + 3]);
      const px = first[0], py = first[1], pz = first[2];
      // One-sided: its facing, turned with the head if it hangs on the head.
      let nx = R[i + 9], ny = R[i + 10], nz = R[i + 11];
      if (nx || ny || nz) {
        if (frame) {
          const wx = nx * frame[0] + ny * frame[3] + nz * frame[6];
          const wy = nx * frame[1] + ny * frame[4] + nz * frame[7];
          const wz = nx * frame[2] + ny * frame[5] + nz * frame[8];
          nx = wx; ny = wy; nz = wz;
        }
        if (nx * (px - cam[0]) + ny * (py - cam[1]) + nz * (pz - cam[2]) > 0) continue;
      }
      const shape = (grow, shift, ir, ig, ib) => {
        if (kind === 11) {
          if (!seeWorld(px, py, pz, 0)) return;
          const rad = R[a + 4] * scale * seen[3];
          ellipse(seen[0], seen[1], depthAt(seen[2], nudge) + shift, rad, 0, 0, rad, ir, ig, ib, grow);
        } else if (kind === 12) {
          const b = jointPoint(R[a + 4], R[a + 5], R[a + 6], R[a + 7], figureEnd);
          if (!seeWorld(px, py, pz, 0) || !seeWorld(b[0], b[1], b[2], 4)) return;
          stadium(seen[0], seen[1], seen[4], seen[5], depthAt((seen[2] + seen[6]) / 2, nudge) + shift,
            R[a + 8] * scale * (seen[3] + seen[7]) + grow * 2, ir, ig, ib);
        } else if (kind === 13) {
          const ax = R[a + 4] * scale, ay = R[a + 5] * scale, az = R[a + 6] * scale;
          const bx = R[a + 7] * scale, by = R[a + 8] * scale, bz = R[a + 9] * scale;
          const turn = (x, y, z, o) => frame
            ? seeWorld(px + x * frame[0] + y * frame[3] + z * frame[6],
              py + x * frame[1] + y * frame[4] + z * frame[7],
              pz + x * frame[2] + y * frame[5] + z * frame[8], o)
            : seeWorld(px + x, py + y, pz + z, o);
          if (!seeWorld(px, py, pz, 0) || !turn(ax, ay, az, 4) || !turn(bx, by, bz, 8)) return;
          ellipse(seen[0], seen[1], depthAt(seen[2], nudge) + shift, seen[4] - seen[0], seen[5] - seen[1],
            seen[8] - seen[0], seen[9] - seen[1], ir, ig, ib, grow);
        } else {
          const count = R[a];
          let vz = 0;
          for (let k = 0; k < count; k++) {
            const q = jointPoint(R[a + 1 + k * 4], R[a + 2 + k * 4], R[a + 3 + k * 4], R[a + 4 + k * 4], figureEnd);
            if (!seeWorld(q[0], q[1], q[2], 0)) return;
            flatPoly[k * 2] = seen[0]; flatPoly[k * 2 + 1] = seen[1]; vz += seen[2];
          }
          plate(flatPoly, 0, count, depthAt(vz / count, nudge) + shift, ir, ig, ib, grow);
        }
      };
      const line = R[i + 1];
      if (line && seeWorld(px, py, pz, 12) && line * scale * seen[15] >= .5)
        shape(line * scale * seen[15], outlineBehind, er, eg, eb);
      shape(0, 0, cr, cg, cb);
    }
  }
  const figureEnd = new Float64Array(3);

  // A drum (a cylinder: centre, two radius vectors, half its length along the
  // axis): the far end's ellipse, the band between the ends' tangent points,
  // the near end's ellipse, each at its own depth.
  const drumEnds = [new Float64Array(7), new Float64Array(7)];
  function drum(m, R, a, nudge, grow, depthShift, cr, cg, cb) {
    for (const side of [0, 1]) {
      const s = side ? 1 : -1, e = drumEnds[side];
      const cx = R[a] + R[a + 9] * s, cy = R[a + 1] + R[a + 10] * s, cz = R[a + 2] + R[a + 11] * s;
      if (!seePlaced(m, cx, cy, cz, 0) || !seePlaced(m, cx + R[a + 3], cy + R[a + 4], cz + R[a + 5], 4) ||
        !seePlaced(m, cx + R[a + 6], cy + R[a + 7], cz + R[a + 8], 8)) return;
      e[0] = seen[0]; e[1] = seen[1]; e[2] = seen[2];
      e[3] = seen[4] - seen[0]; e[4] = seen[5] - seen[1]; e[5] = seen[8] - seen[0]; e[6] = seen[9] - seen[1];
    }
    const [far, near] = drumEnds[0][2] > drumEnds[1][2] ? drumEnds : [drumEnds[1], drumEnds[0]];
    const cap = (e, half, from) => ellipse(e[0], e[1], flatDepth(e[2], nudge) + depthShift,
      e[3], e[4], e[5], e[6], cr, cg, cb, grow, half, from);
    const dx = near[0] - far[0], dy = near[1] - far[1];
    if (Math.hypot(dx, dy) <= .5) cap(far);
    else {
      const t = (e) => Math.atan2(e[5] * dy - e[6] * dx, e[3] * dy - e[4] * dx);
      const tf = t(far), tn = t(near);
      // Only the far end's outer half shows past the band.
      const mid = tf + Math.PI / 2;
      const outward = (far[3] * Math.cos(mid) + far[5] * Math.sin(mid)) * dx + (far[4] * Math.cos(mid) + far[6] * Math.sin(mid)) * dy < 0;
      cap(far, true, outward ? tf : tf + Math.PI);
      const fx = far[3] * Math.cos(tf) + far[5] * Math.sin(tf), fy = far[4] * Math.cos(tf) + far[6] * Math.sin(tf);
      const nx = near[3] * Math.cos(tn) + near[5] * Math.sin(tn), ny = near[4] * Math.cos(tn) + near[6] * Math.sin(tn);
      flatPoly[0] = far[0] + fx; flatPoly[1] = far[1] + fy; flatPoly[2] = near[0] + nx; flatPoly[3] = near[1] + ny;
      flatPoly[4] = near[0] - nx; flatPoly[5] = near[1] - ny; flatPoly[6] = far[0] - fx; flatPoly[7] = far[1] - fy;
      plate(flatPoly, 0, 4, flatDepth((far[2] + near[2]) / 2, nudge) + depthShift, cr, cg, cb, grow);
    }
    cap(near);
  }

  const clipRect = { x: 0, y: 0, w: 0, h: 0 };
  function run(p, length, strings) {
    clip = null;
    hasCamera = false;
    depthMode = 0;
    depthValue = 0;
    ink.width = 0;
    let at = 0;
    while (at < length) {
      const op = p[at];
      switch (op) {
        case FRAME_VIEW:
          if (Number.isNaN(p[at + 1])) clip = null;
          else {
            clipRect.x = p[at + 1]; clipRect.y = p[at + 2];
            clipRect.w = p[at + 3]; clipRect.h = p[at + 4];
            clip = clipRect;
          }
          at += 5; break;
        case FRAME_FACE:
          fullFace(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6],
            p[at + 7], p[at + 8], p[at + 9], p[at + 10], p[at + 11], p[at + 12]);
          at += 13; break;
        case FRAME_DISC:
          if (ink.width) outlined(op, p, at);
          disc(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7]);
          at += 8; break;
        case FRAME_CAPSULE:
          if (ink.width) outlined(op, p, at);
          capsule(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6],
            p[at + 7], p[at + 8], p[at + 9]);
          at += 10; break;
        case FRAME_TEXT:
          fonts[p[at + 1]](strings[p[at + 8]], p[at + 2], p[at + 3], p[at + 4],
            p[at + 5], p[at + 6], p[at + 7]);
          at += 9; break;
        case FRAME_BOX:
          if (p[at + 8] >= 255) box(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7]);
          else box(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7], p[at + 8]);
          at += 9; break;
        case FRAME_LINE:
          line(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7], p[at + 8]);
          at += 9; break;
        case FRAME_WIPE:
          wipe(p[at + 1], p[at + 2], p[at + 3]);
          at += 4; break;
        case FRAME_CAMERA:
          for (let i = 0; i < 24; i++) cam[i] = p[at + 1 + i];
          hasCamera = true;
          at += 25; break;
        case FRAME_WORLD:
          worldFace(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6],
            p[at + 7], p[at + 8], p[at + 9], p[at + 10], p[at + 11], p[at + 12]);
          at += 13; break;
        case FRAME_DEPTH:
          depthMode = p[at + 1];
          depthValue = p[at + 2];
          at += 3; break;
        case FRAME_ASSET:
          storeMesh(p, at);
          at += sizeAt(p, at); break;
        case FRAME_MESH:
          drawMesh(p[at + 1], p[at + 2], p[at + 3], p[at + 4]);
          at += 5; break;
        case FRAME_MODEL:
          drawModel(p, at);
          at += 20; break;
        case FRAME_ELLIPSE:
          if (ink.width) outlined(op, p, at);
          ellipse(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7],
            p[at + 8], p[at + 9], p[at + 10]);
          at += 11; break;
        case FRAME_PLATE: {
          const n = p[at + 1];
          if (ink.width) outlined(op, p, at);
          plate(p, at + 2, n, p[at + 2 + n * 2], p[at + 3 + n * 2], p[at + 4 + n * 2], p[at + 5 + n * 2]);
          at += 6 + n * 2; break;
        }
        case FRAME_SHAPES:
          storeShapes(p, at);
          at += 4 + p[at + 3]; break;
        case FRAME_FIGURE:
          drawFigure(p, at);
          at += 52; break;
        case FRAME_LOOK:
          looks.set(p[at + 1], Float64Array.from(p.subarray(at + 2, at + 32)));
          at += 32; break;
        case FRAME_SKETCH:
          drawSketch(p, at);
          at += 14; break;
        case FRAME_OUTLINE:
          ink.width = p[at + 1]; ink.r = p[at + 2]; ink.g = p[at + 3]; ink.b = p[at + 4];
          at += 5; break;
        default:
          // An op this interpreter does not know ends the program rather than
          // walking into its arguments as if they were ops.
          throw new Error(`frame program: unknown op ${op} at ${at}`);
      }
    }
  }

  // Decode a program into plain objects, for tests and recorders.
  function decode(p, length, strings) {
    const ops = [];
    let at = 0;
    while (at < length) {
      const op = p[at];
      const size = sizeAt(p, at);
      if (!size) throw new Error(`frame program: unknown op ${op} at ${at}`);
      const args = Array.from(p.subarray(at + 1, at + size));
      if (op === FRAME_TEXT) args[7] = strings[args[7]];
      ops.push({ op, args });
      at += size;
    }
    return ops;
  }

  return { run, decode };
}

export default createFrameVm;
