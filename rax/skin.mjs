// 🖼️ Skins: which triangles of a model wear a texture, and where on it each
// corner lies. Seven numbers a triangle: the texture's key (textures.mjs
// `keyOf`; -1 for none) and u, v for each corner (0 … 1 across the texture,
// v down). Kept beside the model (paint.mjs makes them, project.mjs reads
// them), so the model itself stays eighteen numbers a triangle.
const SKINS = new WeakMap();
export const SKIN_FLOATS = 7;
export const skinOf = (made) => SKINS.get(made) || null;
export const setSkin = (made, skin) => SKINS.set(made, skin);

// 🧵 A material over a whole model: every triangle wears `texture`, laid flat on whichever way the triangle faces most, `tile` metres of the model to one width of the picture. The two halves of a box's face lie on the same patch (they share the face's own bounds), and each face takes a patch of its own somewhere in the picture, so no two faces show the same square of it. (A picture is not repeated past its edge: a face larger than a tile takes the whole picture, stretched.)
export function wearAll(made, texture, tile = 0.5) {
  const tris = Math.floor(made.length / 18), skin = new Float32Array(tris * SKIN_FLOATS);
  for (let t = 0; t < tris; t++) {
    const o = t * 18, p = [0, 1, 2].map((k) => [made[o + k * 6], made[o + k * 6 + 1], made[o + k * 6 + 2]]);
    const e1 = p[1].map((v, i) => v - p[0][i]), e2 = p[2].map((v, i) => v - p[0][i]);
    const n = [Math.abs(e1[1] * e2[2] - e1[2] * e2[1]), Math.abs(e1[2] * e2[0] - e1[0] * e2[2]), Math.abs(e1[0] * e2[1] - e1[1] * e2[0])];
    const axis = n[0] >= n[1] && n[0] >= n[2] ? 0 : n[1] >= n[2] ? 1 : 2, [ua, va] = axis === 0 ? [2, 1] : axis === 1 ? [0, 2] : [0, 1];
    const lo = [Math.min(p[0][ua], p[1][ua], p[2][ua]), Math.min(p[0][va], p[1][va], p[2][va])];
    const span = Math.max(Math.max(p[0][ua], p[1][ua], p[2][ua]) - lo[0], Math.max(p[0][va], p[1][va], p[2][va]) - lo[1], 1e-6);
    const part = Math.min(1, span / tile), room = 1 - part;
    const h = (a, b) => { const s = Math.sin(a * 127.1 + b * 311.7 + axis * 74.7) * 43758.5453; return s - Math.floor(s); };
    const ou = h(lo[0], lo[1]) * room, ov = h(lo[1] + 3.1, lo[0] + 1.7) * room;
    skin[t * SKIN_FLOATS] = texture;
    for (let k = 0; k < 3; k++) {
      skin[t * SKIN_FLOATS + 1 + k * 2] = ou + Math.min(part, ((p[k][ua] - lo[0]) / span) * part);
      skin[t * SKIN_FLOATS + 2 + k * 2] = ov + Math.min(part, (1 - (p[k][va] - lo[1]) / span) * part);
    }
  }
  SKINS.set(made, skin);
  return made;
}
