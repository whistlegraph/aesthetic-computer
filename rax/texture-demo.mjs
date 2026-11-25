// 🧊 Texture demos: a cube wearing a texture on each face, and a few pictures
// drawn by functions (`fn:`), so a bench needs nothing from the network.
// Used by the texture bench (test/texture-bench.mjs) and shown off in rax.
import { model } from './paint.mjs';
import { keyOf } from './textures.mjs';

// Six live KidLisp pieces from AC's store, one a face.
export const CUBE_FACES = ['$roz', '$bop', '$7zt', '$8an', '$oyo', '$4ba'];

// A cube `size` metres a side, its middle at the origin, each face a texture
// (names as textures.mjs reads them) over `tint` (white: as it is; an array
// of four colours: a gradient over it, corner by corner).
export function texturedCube(faces = CUBE_FACES, size = 1, tint = 0xffffff) {
  const h = size / 2, keys = faces.map(keyOf);
  const P = (x, y, z) => [x * h, y * h, z * h];
  // Each face's corners: top-left, top-right, bottom-right, bottom-left, seen from outside.
  const sides = [
    [P(-1, 1, 1), P(1, 1, 1), P(1, -1, 1), P(-1, -1, 1)], // +z
    [P(1, 1, -1), P(-1, 1, -1), P(-1, -1, -1), P(1, -1, -1)], // -z
    [P(1, 1, 1), P(1, 1, -1), P(1, -1, -1), P(1, -1, 1)], // +x
    [P(-1, 1, -1), P(-1, 1, 1), P(-1, -1, 1), P(-1, -1, -1)], // -x
    [P(-1, 1, -1), P(1, 1, -1), P(1, 1, 1), P(-1, 1, 1)], // +y
    [P(-1, -1, 1), P(1, -1, 1), P(1, -1, -1), P(-1, -1, -1)], // -y
  ];
  const made = model(({ quad }) => sides.forEach((c, i) => quad(c[0], c[1], c[2], c[3], tint, { texture: keys[i % keys.length] })));
  return { model: made, textures: keys };
}

// 🎨 Pictures drawn by functions: fill RGBA `pixels`, w × h, at time t (s).
export const PROCEDURES = {
  checker(p, w, h, t) {
    const n = 8, shift = Math.floor(t * 2);
    for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) {
      const on = (Math.floor((x * n) / w) + Math.floor((y * n) / h) + shift) & 1, i = (y * w + x) * 4;
      p[i] = on ? 255 : 30; p[i + 1] = on ? 120 : 30; p[i + 2] = on ? 180 : 60; p[i + 3] = 255;
    }
  },
  plasma(p, w, h, t) {
    for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) {
      const u = x / w, v = y / h, i = (y * w + x) * 4;
      const k = Math.sin(u * 10 + t) + Math.sin(v * 12 - t * 1.3) + Math.sin((u + v) * 8 + t * 0.7);
      p[i] = 128 + 127 * Math.sin(k); p[i + 1] = 128 + 127 * Math.sin(k + 2.1); p[i + 2] = 128 + 127 * Math.sin(k + 4.2); p[i + 3] = 255;
    }
  },
  // Rings with a clear middle: shows holes cut where a picture is clear.
  rings(p, w, h, t) {
    for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) {
      const d = Math.hypot(x / w - 0.5, y / h - 0.5), i = (y * w + x) * 4, on = Math.sin(d * 40 - t * 4) > 0;
      p[i] = on ? 255 : 0; p[i + 1] = on ? 220 : 0; p[i + 2] = 60; p[i + 3] = d < 0.18 ? 0 : 255;
    }
  },
};
export const registerProcedures = (textures) => {
  for (const [name, fn] of Object.entries(PROCEDURES)) textures.register(name, fn);
};
