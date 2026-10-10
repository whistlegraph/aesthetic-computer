// Meshes, a camera, and a projector: the 3D layer of the frame contract
// (kidlisp/PIECE-IL.md §11). A piece describes geometry once (`mesh`), a
// camera and placements per frame (`camera`, `place`), and never touches a
// vertex. A host with retained meshes (the Xbox native bios: meshUpload /
// meshDraw with its 27-float camera) draws them natively; every other host
// runs the projector here, which is a port of the Xbox's SceneMesh, so the
// pictures agree.
//
// Mesh layout, the Xbox's: verts = Float32Array x y z per vertex; faces =
// Float32Array of 10 per quad: i0 i1 i2 i3 r g b nx ny nz. A triangle is a
// quad with its last index repeated.
//
// Camera, 27 floats, the Xbox's: 0-2 position; 3-5 right, 6-8 up, 9-11
// forward (rows of the view rotation); 12-13 screen centre; 14 ortho scale,
// 15 focal length in pixels, 16 perspective (1); 17 near; 18-21 viewport;
// 22-23 depth base and slope; 24-26 light direction.

export const DEPTH_BASE = -1.4, DEPTH_SLOPE = 2.8 / 16000, NEAR = 1;
export const LIGHT = normalize([-0.42, 1, -0.28]);
function normalize(v) { const l = Math.hypot(v[0], v[1], v[2]) || 1; return [v[0] / l, v[1] / l, v[2] / l]; }

// ---- building ---------------------------------------------------------------
// forms: [["cube", w, h, d, r, g, b] | ["cube", x, y, z, w, h, d, r, g, b] |
//         ["face", x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4, r, g, b] |
//         ["tri", x1,y1,z1, x2,y2,z2, x3,y3,z3, r, g, b]]
// Numbers only: the piece's mesh is data, built once.
export function buildMesh(forms) {
  const verts = [], faces = [];
  const vertex = (x, y, z) => { verts.push(x, y, z); return verts.length / 3 - 1; };
  const quad = (a, b, c, d, r, g, b2) => {
    const n = faceNormal(verts, a, b, c);
    faces.push(a, b, c, d, r, g, b2, n[0], n[1], n[2]);
  };
  for (const form of forms) {
    if (!Array.isArray(form)) continue;
    const [head, ...a] = form;
    if (a.some((v) => typeof v !== "number" || !Number.isFinite(v))) throw new Error("mesh " + head + ": numbers only");
    if (head === "cube") {
      const [x, y, z, w, h, d, r, g, b] = a.length >= 9 ? a : [0, 0, 0, ...a];
      const hx = w / 2, hy = h / 2, hz = d / 2;
      const p = [[-hx, -hy, -hz], [hx, -hy, -hz], [hx, hy, -hz], [-hx, hy, -hz], [-hx, -hy, hz], [hx, -hy, hz], [hx, hy, hz], [-hx, hy, hz]].map(([px, py, pz]) => vertex(x + px, y + py, z + pz));
      // outward faces, counter-clockwise seen from outside (y up, z forward)
      quad(p[0], p[3], p[2], p[1], r, g, b);   // back (−z)
      quad(p[4], p[5], p[6], p[7], r, g, b);   // front (+z)
      quad(p[0], p[1], p[5], p[4], r, g, b);   // bottom
      quad(p[3], p[7], p[6], p[2], r, g, b);   // top
      quad(p[0], p[4], p[7], p[3], r, g, b);   // left
      quad(p[1], p[2], p[6], p[5], r, g, b);   // right
    } else if (head === "face" && a.length >= 15) {
      quad(vertex(a[0], a[1], a[2]), vertex(a[3], a[4], a[5]), vertex(a[6], a[7], a[8]), vertex(a[9], a[10], a[11]), a[12], a[13], a[14]);
    } else if (head === "tri" && a.length >= 12) {
      const c = vertex(a[6], a[7], a[8]);
      quad(vertex(a[0], a[1], a[2]), vertex(a[3], a[4], a[5]), c, c, a[9], a[10], a[11]);
    } else throw new Error("mesh: unknown part " + head);
  }
  return { verts: new Float32Array(verts), faces: new Float32Array(faces) };
}
function faceNormal(verts, a, b, c) {
  const ax = verts[a * 3], ay = verts[a * 3 + 1], az = verts[a * 3 + 2];
  const ux = verts[b * 3] - ax, uy = verts[b * 3 + 1] - ay, uz = verts[b * 3 + 2] - az;
  const vx = verts[c * 3] - ax, vy = verts[c * 3 + 1] - ay, vz = verts[c * 3 + 2] - az;
  return normalize([uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx]);
}

// ---- the camera -------------------------------------------------------------
// yaw turns about y (0 looks down +z), pitch about x; fov vertical, degrees.
export function sceneCamera(x, y, z, yaw, pitch, fov, width, height, out = new Float32Array(27), near = NEAR, left = 0, top = 0) {
  const cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch);
  // forward = R_y(yaw) R_x(pitch) (0,0,1); right = (cy, 0, -sy); up = right × forward
  const fx = sy * cp, fy = -sp, fz = cy * cp;
  const rx = cy, ry = 0, rz = -sy;
  const ux = ry * fz - rz * fy, uy = rz * fx - rx * fz, uz = rx * fy - ry * fx;
  out[0] = x; out[1] = y; out[2] = z;
  out[3] = rx; out[4] = ry; out[5] = rz; out[6] = ux; out[7] = uy; out[8] = uz; out[9] = fx; out[10] = fy; out[11] = fz;
  out[12] = left + width / 2; out[13] = top + height / 2;
  out[14] = 0; out[15] = (height / 2) / Math.tan(((fov || 60) * Math.PI) / 360); out[16] = 1;
  out[17] = near;
  out[18] = left; out[19] = top; out[20] = left + width; out[21] = top + height;
  out[22] = DEPTH_BASE; out[23] = DEPTH_SLOPE;
  out[24] = LIGHT[0]; out[25] = LIGHT[1]; out[26] = LIGHT[2];
  return out;
}

// ---- placing ----------------------------------------------------------------
// A placement: position, yaw/pitch/roll (radians), uniform scale. Returns a
// world-space copy of the mesh (verts and rotated normals), cached per
// mesh for the identity placement.
export function placeMesh(mesh, px, py, pz, yaw = 0, pitch = 0, roll = 0, scale = 1) {
  const cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch), cr = Math.cos(roll), sr = Math.sin(roll);
  // R = R_y(yaw) · R_x(pitch) · R_z(roll)
  const m00 = cy * cr + sy * sp * sr, m01 = -cy * sr + sy * sp * cr, m02 = sy * cp;
  const m10 = cp * sr, m11 = cp * cr, m12 = -sp;
  const m20 = -sy * cr + cy * sp * sr, m21 = sy * sr + cy * sp * cr, m22 = cy * cp;
  const src = mesh.verts, n = src.length / 3, verts = new Float32Array(src.length);
  for (let i = 0; i < n; i++) {
    const x = src[i * 3] * scale, y = src[i * 3 + 1] * scale, z = src[i * 3 + 2] * scale;
    verts[i * 3] = px + m00 * x + m01 * y + m02 * z;
    verts[i * 3 + 1] = py + m10 * x + m11 * y + m12 * z;
    verts[i * 3 + 2] = pz + m20 * x + m21 * y + m22 * z;
  }
  const faces = new Float32Array(mesh.faces);
  for (let f = 0; f < faces.length; f += 10) {
    const x = faces[f + 7], y = faces[f + 8], z = faces[f + 9];
    faces[f + 7] = m00 * x + m01 * y + m02 * z; faces[f + 8] = m10 * x + m11 * y + m12 * z; faces[f + 9] = m20 * x + m21 * y + m22 * z;
  }
  return { verts, faces };
}

// ---- projecting (the Xbox's SceneMesh, in JavaScript) ------------------------
// Emits each visible face as screen-space triangles with depth and the lit
// colour: emit(x1, y1, x2, y2, x3, y3, depth, r, g, b). Faces come out
// far-to-near so a host without a depth buffer can paint in order.
export function projectMesh(m, world, emit, alpha = 255) {
  const verts = world.verts, faces = world.faces, n = verts.length / 3;
  const view = new Float64Array(n * 3);
  for (let i = 0; i < n; i++) {
    const x = verts[i * 3] - m[0], y = verts[i * 3 + 1] - m[1], z = verts[i * 3 + 2] - m[2];
    view[i * 3] = x * m[3] + y * m[4] + z * m[5];
    view[i * 3 + 1] = x * m[6] + y * m[7] + z * m[8];
    view[i * 3 + 2] = x * m[9] + y * m[10] + z * m[11];
  }
  const near = m[17], out = [];
  for (let f = 0; f < faces.length; f += 10) {
    const ids = [faces[f], faces[f + 1], faces[f + 2], faces[f + 3]];
    // back faces: the lit normal points away when its dot with the view ray is positive
    const nx = faces[f + 7], ny = faces[f + 8], nz = faces[f + 9];
    const i0 = ids[0] * 3, cx = verts[i0] - m[0], cy = verts[i0 + 1] - m[1], cz = verts[i0 + 2] - m[2];
    if (nx * cx + ny * cy + nz * cz > 0) continue;
    let poly = [];
    for (const id of ids) { const p = [view[id * 3], view[id * 3 + 1], view[id * 3 + 2]]; if (!poly.length || poly[poly.length - 1][0] !== p[0] || poly[poly.length - 1][1] !== p[1] || poly[poly.length - 1][2] !== p[2]) poly.push(p); }
    if (poly.length < 3) continue;
    poly = clipNear(poly, near);
    if (poly.length < 3) continue;
    const light = 0.72 + Math.max(0, -(nx * m[24] + ny * m[25] + nz * m[26])) * 0.28;
    const r = Math.round(faces[f + 4] * light), g = Math.round(faces[f + 5] * light), b = Math.round(faces[f + 6] * light);
    let depthSum = 0;
    const screen = poly.map((p) => { const k = m[14] + (m[15] / p[2] - m[14]) * m[16]; depthSum += p[2]; return [m[12] + p[0] * k, m[13] - p[1] * k, Math.max(-1.499, Math.min(1.4, m[22] + p[2] * m[23]))]; });
    out.push({ z: depthSum / poly.length, screen, r, g, b });
  }
  out.sort((a, b) => b.z - a.z);
  for (const face of out) {
    const s = face.screen;
    for (let k = 1; k + 1 < s.length; k++) emit(s[0][0], s[0][1], s[k][0], s[k][1], s[k + 1][0], s[k + 1][1], (s[0][2] + s[k][2] + s[k + 1][2]) / 3, face.r, face.g, face.b, alpha);
  }
  return out.length;
}
function clipNear(poly, near) {
  const out = [];
  for (let i = 0; i < poly.length; i++) {
    const a = poly[i], b = poly[(i + 1) % poly.length], da = a[2] - near, db = b[2] - near;
    if (da >= 0) out.push(a);
    if ((da >= 0) !== (db >= 0)) { const t = da / (da - db); out.push([a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, a[2] + (b[2] - a[2]) * t]); }
  }
  return out;
}
