// 🖼️ Skins: which triangles of a model wear a texture, and where on it each
// corner lies. Seven numbers a triangle: the texture's key (textures.mjs
// `keyOf`; -1 for none) and u, v for each corner (0 … 1 across the texture,
// v down). Kept beside the model (paint.mjs makes them, project.mjs reads
// them), so the model itself stays eighteen numbers a triangle.
const SKINS = new WeakMap();
export const SKIN_FLOATS = 7;
export const skinOf = (made) => SKINS.get(made) || null;
export const setSkin = (made, skin) => SKINS.set(made, skin);
