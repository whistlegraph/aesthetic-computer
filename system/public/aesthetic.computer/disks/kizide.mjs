// kizide, 26.09.11
// A ring of textured cubes, spun by hand.

const count = 8;
const reach = 2.4; // a cube's worst-case projected half-extent, in `size`s
const dist = 3; // camera distance, in cube halves
const light = [-0.39, -0.49, -0.78]; // unit, over the viewer's left shoulder

const corners = [];
for (let i = 0; i < 8; i += 1)
  corners.push([i & 1 ? 1 : -1, i & 2 ? 1 : -1, i & 4 ? 1 : -1]);

// a face is an axis, a side, and the four corners that agree on that bit —
// walked in ring order, so the uv corners below line up with them.
const uvs = [
  [0, 0],
  [0, 1],
  [1, 1],
  [1, 0],
];

const faces = [];
for (let a = 0; a < 3; a += 1)
  for (const s of [-1, 1]) {
    const b = (a + 1) % 3,
      c = (a + 2) % 3;
    const ring = uvs.map(([u, v]) => {
      let i = s > 0 ? 1 << a : 0;
      if (u) i |= 1 << b;
      if (v) i |= 1 << c;
      return i;
    });
    faces.push([a, s, ring]);
  }

// the texture: a warm frame, magenta diagonals, a woven indigo ground. the
// frame is why the cubes still read as edged without any wireframe pass.
const texSize = 16;
const tex = new Uint8ClampedArray(texSize * texSize * 3);
for (let y = 0; y < texSize; y += 1)
  for (let x = 0; x < texSize; x += 1) {
    const framed = x === 0 || y === 0 || x === texSize - 1 || y === texSize - 1;
    const c = framed
      ? [255, 240, 200]
      : (x + y) % 7 < 2
        ? [225, 70, 195]
        : (x ^ y) & 2
          ? [54, 40, 92]
          : [32, 24, 62];
    const o = (y * texSize + x) * 3;
    tex[o] = c[0];
    tex[o + 1] = c[1];
    tex[o + 2] = c[2];
  }

// one cube gets rasterized into this and pasted. writing straight into
// `screen.pixels` renders nothing here — the writes land in a valid buffer
// and are silently dropped, while `ink` in the same frame draws fine. `paste`
// goes through `graph`, which is the path that actually reaches the screen.
const tile = { width: 0, height: 0, pixels: null };

let yaw = 0,
  pitch = 0,
  orbit = 0;

function sim() {
  yaw += 0.02;
  pitch += 0.013;
  orbit += 0.003;
}

function rot(x, y, z, cyw, syw, cp, sp) {
  const x1 = x * cyw - z * syw,
    z1 = x * syw + z * cyw;
  return [x1, y * cp - z1 * sp, y * sp + z1 * cp];
}

// one triangle, barycentric, perspective-correct. a vertex is
// [x, y, iw, u * iw, v * iw] — all three interpolate linearly on screen.
function tri(px, W, H, p0, p1, p2, shade) {
  const d =
    (p1[1] - p2[1]) * (p0[0] - p2[0]) + (p2[0] - p1[0]) * (p0[1] - p2[1]);
  if (d === 0) return;
  const id = 1 / d;

  const lo = Math.max(0, Math.floor(Math.min(p0[0], p1[0], p2[0]))),
    hi = Math.min(W - 1, Math.ceil(Math.max(p0[0], p1[0], p2[0]))),
    top = Math.max(0, Math.floor(Math.min(p0[1], p1[1], p2[1]))),
    bot = Math.min(H - 1, Math.ceil(Math.max(p0[1], p1[1], p2[1])));

  for (let y = top; y <= bot; y += 1)
    for (let x = lo; x <= hi; x += 1) {
      const sx = x + 0.5,
        sy = y + 0.5;
      const w0 =
          ((p1[1] - p2[1]) * (sx - p2[0]) + (p2[0] - p1[0]) * (sy - p2[1])) *
          id,
        w1 =
          ((p2[1] - p0[1]) * (sx - p2[0]) + (p0[0] - p2[0]) * (sy - p2[1])) * id,
        w2 = 1 - w0 - w1;
      if (w0 < 0 || w1 < 0 || w2 < 0) continue;

      const iw = w0 * p0[2] + w1 * p1[2] + w2 * p2[2];
      const u = (w0 * p0[3] + w1 * p1[3] + w2 * p2[3]) / iw,
        v = (w0 * p0[4] + w1 * p1[4] + w2 * p2[4]) / iw;

      const tx = Math.min(texSize - 1, Math.max(0, (u * texSize) | 0)),
        ty = Math.min(texSize - 1, Math.max(0, (v * texSize) | 0));
      const t = (ty * texSize + tx) * 3,
        o = (y * W + x) * 4;
      px[o] = tex[t] * shade;
      px[o + 1] = tex[t + 1] * shade;
      px[o + 2] = tex[t + 2] * shade;
      px[o + 3] = 255;
    }
}

function cube(px, W, H, cx, cy, size, yaw, pitch) {
  const cyw = Math.cos(yaw),
    syw = Math.sin(yaw),
    cp = Math.cos(pitch),
    sp = Math.sin(pitch);

  const flat = corners.map(([x, y, z]) => {
    const p = rot(x, y, z, cyw, syw, cp, sp);
    const iw = dist / (dist + p[2]);
    return [cx + p[0] * size * iw, cy + p[1] * size * iw, iw];
  });

  faces.forEach(([a, s, ring]) => {
    const n = rot(
      a === 0 ? s : 0,
      a === 1 ? s : 0,
      a === 2 ? s : 0,
      cyw,
      syw,
      cp,
      sp,
    );
    if (n[2] >= 0) return; // facing away — a cube is convex, so that's all
    // the depth sorting it needs.

    const lam = n[0] * light[0] + n[1] * light[1] + n[2] * light[2];
    const shade = 0.35 + 0.65 * (0.5 + 0.5 * lam); // half lambert; nothing
    // goes fully black at this size.

    const q = ring.map((i, k) => {
      const f = flat[i];
      return [f[0], f[1], f[2], uvs[k][0] * f[2], uvs[k][1] * f[2]];
    });
    tri(px, W, H, q[0], q[1], q[2], shade);
    tri(px, W, H, q[0], q[2], q[3], shade);
  });
}

function paint({ wipe, paste, screen }) {
  wipe(12, 10, 24);

  const W = screen.width,
    H = screen.height;

  // the ring wants room for itself plus one cube's reach; neighbours want a
  // gap of the same reach. both fall out of one radius.
  const room = Math.min(W, H) / 2 - 6,
    gap = Math.sin(Math.PI / count),
    ring = room / (1 + gap),
    size = (ring * gap) / reach;

  const span = Math.ceil(size * reach * 2) + 2;
  if (tile.width !== span) {
    tile.width = tile.height = span;
    tile.pixels = new Uint8ClampedArray(span * span * 4);
  }

  for (let i = 0; i < count; i += 1) {
    const a = orbit + (i / count) * Math.PI * 2;
    const cx = W / 2 + Math.cos(a) * ring,
      cy = H / 2 + Math.sin(a) * ring;
    const ox = Math.floor(cx - span / 2),
      oy = Math.floor(cy - span / 2);

    tile.pixels.fill(0); // transparent — `paste` skips those, so the wipe
    // shows between the cubes.
    cube(
      tile.pixels,
      span,
      span,
      cx - ox, // keep the subpixel remainder, so the orbit stays smooth
      cy - oy,
      size,
      // its own rate, not just its own phase — a cube has 90° symmetry, so
      // equal-step offsets keep re-syncing and the ring goes flat-on at once.
      yaw * (1 + i * 0.07) + i * 2.39996,
      pitch * (1 + i * 0.11) + i * 1.0472,
    );
    paste(tile, ox, oy);
  }
}

export { sim, paint };
