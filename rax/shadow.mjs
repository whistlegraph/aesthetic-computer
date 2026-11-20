// 🌑 Shadows from the sun, as stencil volumes. Every box that stands in the
// world sweeps a closed solid away from the light: the faces toward the sun
// start it just behind the box's own lit skin, the faces away from the sun
// end it far along the light, and a wall joins the two wherever a lit face
// meets an unlit one. The renderer counts those solids in its stencil
// buffer and darkens what lies inside them (scene3d-webgpu.mjs).
//
// A box's corners: bit 0 is +x, bit 1 +y, bit 2 +z. Each face lists its
// corners counter-clockwise seen from outside.
const FACES = [[1, 3, 7, 5], [0, 4, 6, 2], [2, 6, 7, 3], [0, 1, 5, 4], [4, 5, 7, 6], [0, 2, 3, 1]];
// The face on the other side of each directed edge.
const ACROSS = new Map();
FACES.forEach((loop, f) => loop.forEach((a, k) => ACROSS.set(loop[(k + 1) % 4] * 8 + a, f)));
export const VOLUME_TRIANGLES = 24; // the most one box can need
const FLOATS = 18; // a triangle in the scene's stream: three of x, y, z, r, g, b

const corners = new Float64Array(24), lit = new Array(6);

// Write one box's shadow volume into `out` from float `at`; returns the next
// free float. `box` is { x, y, z, w, h, d } about its middle in its model;
// the model stands at (px, py, pz) turned `yaw` and scaled, as the view
// draws it. `light` is the unit direction the light travels, `near` how far
// behind the lit skin the volume begins, `far` how far it runs.
export function shadowVolume(out, at, box, px, py, pz, yaw, scale, light, near, far) {
  const cy = Math.cos(yaw), sy = Math.sin(yaw);
  // A box given a negative size is the same solid: without this its volume
  // would be inside out and cancel the shadows it overlaps.
  const w = Math.abs(box.w), h = Math.abs(box.h), d = Math.abs(box.d);
  scale = Math.abs(scale);
  for (let i = 0; i < 8; i++) {
    const lx = (box.x + (i & 1 ? w : -w) / 2) * scale;
    const ly = (box.y + (i & 2 ? h : -h) / 2) * scale;
    const lz = (box.z + (i & 4 ? d : -d) / 2) * scale;
    corners[i * 3] = lx * cy + lz * sy + px;
    corners[i * 3 + 1] = ly + py;
    corners[i * 3 + 2] = -lx * sy + lz * cy + pz;
  }
  for (let f = 0; f < 6; f++) {
    const [a, b, c] = FACES[f];
    const ux = corners[b * 3] - corners[a * 3], uy = corners[b * 3 + 1] - corners[a * 3 + 1], uz = corners[b * 3 + 2] - corners[a * 3 + 2];
    const vx = corners[c * 3] - corners[b * 3], vy = corners[c * 3 + 1] - corners[b * 3 + 1], vz = corners[c * 3 + 2] - corners[b * 3 + 2];
    lit[f] = (uy * vz - uz * vy) * light.x + (uz * vx - ux * vz) * light.y + (ux * vy - uy * vx) * light.z < 0;
  }
  const put = (corner, along) => {
    out[at] = corners[corner * 3] + light.x * along;
    out[at + 1] = corners[corner * 3 + 1] + light.y * along;
    out[at + 2] = corners[corner * 3 + 2] + light.z * along;
    out[at + 3] = out[at + 4] = out[at + 5] = 0;
    at += 6;
  };
  const quad = (a, ta, b, tb, c, tc, d, td) => {
    put(a, ta); put(b, tb); put(c, tc);
    put(a, ta); put(c, tc); put(d, td);
  };
  for (let f = 0; f < 6; f++) {
    const loop = FACES[f], t = lit[f] ? near : far;
    quad(loop[0], t, loop[1], t, loop[2], t, loop[3], t);
    if (!lit[f]) continue;
    for (let k = 0; k < 4; k++) {
      const a = loop[k], b = loop[(k + 1) % 4];
      if (!lit[ACROSS.get(a * 8 + b)]) quad(b, near, a, near, a, far, b, far);
    }
  }
  return at;
}
export const trianglesIn = (floats) => floats / FLOATS;

// How far a box's shadow must run to pass below the ground, held to `most`.
export const reach = (top, light, most = 70) => Math.min(most, (top + 2) / Math.max(0.05, -light.y));

// How dark the shadows are for a sun at this height (its unit y): none near
// the horizon, full once it is well up. Returns null when nothing is cast.
export function shadeFor(sunY, full = [0.6, 0.63, 0.74]) {
  if (sunY < 0.08) return null;
  const k = Math.min(1, (sunY - 0.08) / 0.25);
  return full.map((c) => 1 - (1 - c) * k);
}

// 🌑 A box's shadow on its own ground, as a low closed slab. The long solid
// above (shadowVolume) grazes the ground at a shallow angle, and wherever two
// surfaces nearly lie in one another the depth test flips from pixel to
// pixel: stripes. This one never grazes anything: the box's eight corners
// are slid along the light down to the ground it stands on, the hull of
// where they land is the shadow's outline, and that outline is raised into
// a slab from a metre under the ground to a hand above it. Its walls are
// upright, so they cut the ground square on; its lid and floor lie well
// clear of the ground. What the renderer counts inside it is the ground
// under the outline (and the first few centimetres of whatever stands
// there). `ground` is the height of the ground the box stands on; `far` the
// most a corner may slide (a low sun's shadows are held to that).
export const SLAB_TRIANGLES = 28; // an outline of eight corners at the most: two fans of six and sixteen of wall
const LID = 0.12, FLOOR = -1;
const landed = new Float64Array(16), hull = new Int8Array(18);
export function groundShadow(out, at, box, px, py, pz, yaw, scale, light, ground, far) {
  if (!(light.y < -0.01)) return at;
  const cy = Math.cos(yaw), sy = Math.sin(yaw);
  const w = Math.abs(box.w), h = Math.abs(box.h), d = Math.abs(box.d);
  scale = Math.abs(scale);
  const flat = Math.hypot(light.x, light.z);
  for (let i = 0; i < 8; i++) {
    // (The foot's corners are drawn in a little: the slab's wall on the lit
    // side would lie in the box's own wall, and two surfaces in one plane
    // flicker; a finger's width inside, it is hidden in the box.)
    const tuck = i & 2 ? 0 : Math.min(0.03, Math.min(w, d) * scale * 0.2) / scale;
    const lx = (box.x + (i & 1 ? 1 : -1) * (w / 2 - tuck)) * scale, ly = (box.y + (i & 2 ? h : -h) / 2) * scale, lz = (box.z + (i & 4 ? 1 : -1) * (d / 2 - tuck)) * scale;
    const x = lx * cy + lz * sy + px, y = ly + py, z = -lx * sy + lz * cy + pz;
    // Down the light to the ground; no farther along it than `far`.
    let t = Math.max(0, (y - ground) / -light.y);
    if (flat * t > far) t = far / flat;
    landed[i * 2] = x + light.x * t;
    landed[i * 2 + 1] = z + light.z * t;
  }
  // The hull of the eight (Andrew's chain: sorted by x then z, lower and upper runs).
  const order = [0, 1, 2, 3, 4, 5, 6, 7].sort((a, b) => landed[a * 2] - landed[b * 2] || landed[a * 2 + 1] - landed[b * 2 + 1]);
  const turn = (a, b, c) => (landed[b * 2] - landed[a * 2]) * (landed[c * 2 + 1] - landed[a * 2 + 1]) - (landed[b * 2 + 1] - landed[a * 2 + 1]) * (landed[c * 2] - landed[a * 2]);
  let n = 0;
  for (const p of order) { while (n >= 2 && turn(hull[n - 2], hull[n - 1], p) <= 1e-9) n--; hull[n++] = p; }
  const low = n + 1;
  for (let k = 6; k >= 0; k--) { const p = order[k]; while (n >= low && turn(hull[n - 2], hull[n - 1], p) <= 1e-9) n--; hull[n++] = p; }
  n--; // (the first corner comes round again at the end)
  if (n < 3) return at;
  const put = (k, y) => { out[at] = landed[hull[k] * 2]; out[at + 1] = ground + y; out[at + 2] = landed[hull[k] * 2 + 1]; out[at + 3] = out[at + 4] = out[at + 5] = 0; at += 6; };
  for (let k = 1; k + 1 < n; k++) { put(0, LID); put(k, LID); put(k + 1, LID); }
  for (let k = 1; k + 1 < n; k++) { put(0, FLOOR); put(k + 1, FLOOR); put(k, FLOOR); }
  for (let k = 0; k < n; k++) {
    const j = (k + 1) % n;
    put(k, FLOOR); put(j, FLOOR); put(j, LID);
    put(k, FLOOR); put(j, LID); put(k, LID);
  }
  return at;
}
