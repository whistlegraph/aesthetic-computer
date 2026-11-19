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
