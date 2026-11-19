// Project/clamp at the camera near plane, then write directly into Oskiewar's
// reusable scene buffer. Every face and letter participates in the depth test.
export class Projector {
  constructor(scene) {
    this.scene = scene;
    this.input = new Float64Array(12);
    this.clipped = new Float64Array(15);
  }
  overlay(vertices, width, height) {
    for (let i = 0; i < vertices.length; i += 18) {
      if (this.scene.triangleCount >= this.scene.maxTriangles) return;
      let at = this.scene.triangleCount++ * 18;
      for (let j = 0; j < 3; j++) {
        const k = i + j * 6;
        this.scene.vertices[at++] = (-vertices[k] * 2) / width;
        this.scene.vertices[at++] = (vertices[k + 1] * 2) / height;
        this.scene.vertices[at++] = 0.00001 + vertices[k + 2];
        for (let c = 3; c < 6; c++) this.scene.vertices[at++] = vertices[k + c];
      }
    }
  }
  camera(eye, target, aspect, roll = 0) {
    this.eye = eye;
    let x = target.x - eye.x,
      y = target.y - eye.y,
      z = target.z - eye.z;
    const length = Math.hypot(x, y, z);
    x /= length;
    y /= length;
    z /= length;
    const horizontal = Math.hypot(x, z) || 1;
    this.forward = [x, y, z];
    this.right = [-z / horizontal, 0, x / horizontal];
    this.up = [(-x * y) / horizontal, horizontal, (-z * y) / horizontal];
    if (roll) {
      // A tilt of the head: right and up turn together about the forward axis.
      const c = Math.cos(roll), s = Math.sin(roll), r = this.right, u = this.up;
      this.right = [r[0] * c + u[0] * s, r[1] * c + u[1] * s, r[2] * c + u[2] * s];
      this.up = [u[0] * c - r[0] * s, u[1] * c - r[1] * s, u[2] * c - r[2] * s];
    }
    const tan = aspect < 1 ? Math.min(Math.sqrt(3), 1 / aspect) : 1;
    this.sx = 1 / (tan * aspect);
    this.sy = 1 / tan;
  }
  // 👻 Looking through: `points` are places in the world the eye must be able
  // to see (the body the camera follows). Until `look()` with nothing, a face
  // that stands between the eye and any of them is not drawn, so a wall or a
  // ceiling behind the player opens instead of pushing the camera in. Each
  // sight line stops `short` of its point, so the body itself is never cut.
  look(points = null, short = 0.45) {
    if (!points?.length) return (this.cut = null);
    const e = this.eye, [r, u, f] = [this.right, this.up, this.forward];
    this.cut = [];
    for (const p of points) {
      const dx = p.x - e.x, dy = p.y - e.y, dz = p.z - e.z, d = Math.hypot(dx, dy, dz);
      const k = d > short ? (d - short) / d : 0;
      if (k) this.cut.push((dx * r[0] + dy * r[1] + dz * r[2]) * k, (dx * u[0] + dy * u[1] + dz * u[2]) * k, (dx * f[0] + dy * f[1] + dz * f[2]) * k);
    }
    if (!this.cut.length) this.cut = null;
  }
  // Does this face (three points in view space, flat in `a`) cross a sight line?
  veils(a) {
    const e1x = a[3] - a[0], e1y = a[4] - a[1], e1z = a[5] - a[2],
      e2x = a[6] - a[0], e2y = a[7] - a[1], e2z = a[8] - a[2];
    for (let i = 0; i < this.cut.length; i += 3) {
      const dx = this.cut[i], dy = this.cut[i + 1], dz = this.cut[i + 2];
      // Möller–Trumbore from the eye (the origin of view space) along d.
      const px = dy * e2z - dz * e2y, py = dz * e2x - dx * e2z, pz = dx * e2y - dy * e2x;
      const det = e1x * px + e1y * py + e1z * pz;
      if (det > -1e-9 && det < 1e-9) continue;
      const inv = 1 / det, tx = -a[0], ty = -a[1], tz = -a[2];
      const s = (tx * px + ty * py + tz * pz) * inv;
      if (s < 0 || s > 1) continue;
      const qx = ty * e1z - tz * e1y, qy = tz * e1x - tx * e1z, qz = tx * e1y - ty * e1x;
      const v = (dx * qx + dy * qy + dz * qz) * inv;
      if (v < 0 || s + v > 1) continue;
      const t = (e2x * qx + e2y * qy + e2z * qz) * inv;
      if (t > 0 && t < 1) return true;
    }
    return false;
  }
  // A world point to screen space: x right and y up in -1..1, or null behind.
  project(x, y, z) {
    const e = this.eye,
      dx = x - e.x,
      dy = y - e.y,
      dz = z - e.z;
    const [r, u, f] = [this.right, this.up, this.forward];
    const depth = dx * f[0] + dy * f[1] + dz * f[2];
    if (depth < 0.1) return null;
    const px = (dx * r[0] + dy * r[1] + dz * r[2]) / depth, py = (dx * u[0] + dy * u[1] + dz * u[2]) / depth, k = this.squeeze(px, py);
    return { x: px * k * this.sx + (this.shiftX || 0), y: py * k * this.sy + (this.shiftY || 0), depth };
  }
  // 🔭 The lens (0: none, a flat picture as a pinhole makes it; 1: all of
  // it). A flat picture of a wide view stretches whatever is near its edges.
  // The lens keeps the middle exactly as the flat picture has it and draws
  // the edges in toward it, as a wide lens does (the stereographic one: a
  // thing at angle a from the middle lies 2 tan(a/2) out, not tan(a)), so
  // the same screen holds more of the world to either side and nothing at
  // the edge is pulled long. How much a point at (x, y) over its depth moves in:
  squeeze(x, y) {
    return this.lens ? 1 + (2 / (1 + Math.sqrt(1 + x * x + y * y)) - 1) * this.lens : 1;
  }
  // One triangle, in the eye's own space (right, up, ahead), put on the
  // screen in its colour. Under the lens a straight edge is a curve, so an
  // edge that is long on the screen is halved, and its halves again (to
  // `depth` times): by the edge alone, so two triangles that share one
  // halve it at the same place and no gap opens between them.
  put(x0, y0, z0, x1, y1, z1, x2, y2, z2, cr, cg, cb, depth) {
    const near = 0.05, far = 2400, sx = this.sx, sy = this.sy, ox = this.shiftX || 0, oy = this.shiftY || 0;
    const k0 = this.squeeze(x0 / z0, y0 / z0), k1 = this.squeeze(x1 / z1, y1 / z1), k2 = this.squeeze(x2 / z2, y2 / z2);
    const ax = (x0 * k0 * sx) / z0 + ox, ay = (y0 * k0 * sy) / z0 + oy, bx = (x1 * k1 * sx) / z1 + ox, by = (y1 * k1 * sy) / z1 + oy,
      cx = (x2 * k2 * sx) / z2 + ox, cy = (y2 * k2 * sy) / z2 + oy;
    // Wholly off one side of the picture: not drawn.
    if ((ax < -1 && bx < -1 && cx < -1) || (ax > 1 && bx > 1 && cx > 1) || (ay < -1 && by < -1 && cy < -1) || (ay > 1 && by > 1 && cy > 1)) return;
    if (depth > 0) {
      const LONG = 0.12 * 0.12; // (a sixteenth of the picture's width: short enough that two flat things a few millimetres apart keep their order)
      const e0 = (ax - bx) ** 2 + (ay - by) ** 2 > LONG, e1 = (bx - cx) ** 2 + (by - cy) ** 2 > LONG, e2 = (cx - ax) ** 2 + (cy - ay) ** 2 > LONG;
      if (e0 || e1 || e2) {
        const mx0 = (x0 + x1) / 2, my0 = (y0 + y1) / 2, mz0 = (z0 + z1) / 2, mx1 = (x1 + x2) / 2, my1 = (y1 + y2) / 2, mz1 = (z1 + z2) / 2,
          mx2 = (x2 + x0) / 2, my2 = (y2 + y0) / 2, mz2 = (z2 + z0) / 2, d = depth - 1;
        if (e0 && e1 && e2) {
          this.put(x0, y0, z0, mx0, my0, mz0, mx2, my2, mz2, cr, cg, cb, d);
          this.put(mx0, my0, mz0, x1, y1, z1, mx1, my1, mz1, cr, cg, cb, d);
          this.put(mx2, my2, mz2, mx1, my1, mz1, x2, y2, z2, cr, cg, cb, d);
          this.put(mx0, my0, mz0, mx1, my1, mz1, mx2, my2, mz2, cr, cg, cb, d);
        } else if (e0 && e1) {
          this.put(mx0, my0, mz0, x1, y1, z1, mx1, my1, mz1, cr, cg, cb, d);
          this.put(x0, y0, z0, mx0, my0, mz0, mx1, my1, mz1, cr, cg, cb, d);
          this.put(x0, y0, z0, mx1, my1, mz1, x2, y2, z2, cr, cg, cb, d);
        } else if (e1 && e2) {
          this.put(mx1, my1, mz1, x2, y2, z2, mx2, my2, mz2, cr, cg, cb, d);
          this.put(x1, y1, z1, mx1, my1, mz1, mx2, my2, mz2, cr, cg, cb, d);
          this.put(x1, y1, z1, mx2, my2, mz2, x0, y0, z0, cr, cg, cb, d);
        } else if (e2 && e0) {
          this.put(mx2, my2, mz2, x0, y0, z0, mx0, my0, mz0, cr, cg, cb, d);
          this.put(x2, y2, z2, mx2, my2, mz2, mx0, my0, mz0, cr, cg, cb, d);
          this.put(x2, y2, z2, mx0, my0, mz0, x1, y1, z1, cr, cg, cb, d);
        } else if (e0) {
          this.put(x0, y0, z0, mx0, my0, mz0, x2, y2, z2, cr, cg, cb, d);
          this.put(mx0, my0, mz0, x1, y1, z1, x2, y2, z2, cr, cg, cb, d);
        } else if (e1) {
          this.put(x1, y1, z1, mx1, my1, mz1, x0, y0, z0, cr, cg, cb, d);
          this.put(mx1, my1, mz1, x2, y2, z2, x0, y0, z0, cr, cg, cb, d);
        } else {
          this.put(x2, y2, z2, mx2, my2, mz2, x1, y1, z1, cr, cg, cb, d);
          this.put(mx2, my2, mz2, x0, y0, z0, x1, y1, z1, cr, cg, cb, d);
        }
        return;
      }
    }
    // Over budget: drop the rest of this frame's geometry, never throw
    // (a throw here stops the frame loop and freezes the game).
    if (this.scene.triangleCount >= this.scene.maxTriangles) {
      this.overflow = true;
      return;
    }
    const v = this.scene.vertices, q = far / (far - near), w = (far * near) / (far - near);
    let at = this.scene.triangleCount++ * 18;
    v[at++] = ax; v[at++] = ay; v[at++] = q - w / z0; v[at++] = cr; v[at++] = cg; v[at++] = cb;
    v[at++] = bx; v[at++] = by; v[at++] = q - w / z1; v[at++] = cr; v[at++] = cg; v[at++] = cb;
    v[at++] = cx; v[at++] = cy; v[at++] = q - w / z2; v[at++] = cr; v[at++] = cg; v[at++] = cb;
  }
  draw(vertices, x = 0, y = 0, z = 0, yaw = 0, pitch = 0, scale = 1, roll = 0) {
    const cy = Math.cos(yaw),
      sy = Math.sin(yaw),
      cp = Math.cos(pitch),
      sp = Math.sin(pitch),
      cr = Math.cos(roll),
      sr = Math.sin(roll);
    const f = this.forward,
      r = this.right,
      u = this.up,
      eye = this.eye,
      a = this.input,
      b = this.clipped;
    const near = 0.05,
      far = 2400; // far enough for the horizon and the curtain's overhead view
    for (let i = 0; i < vertices.length; i += 18) {
      for (let j = 0; j < 3; j++) {
        const k = i + j * 6,
          lx = vertices[k] * scale,
          ly = vertices[k + 1] * scale,
          lz = vertices[k + 2] * scale;
        const rx = lx * cr - ly * sr,
          ry = lx * sr + ly * cr;
        const py = ry * cp - lz * sp,
          pz = ry * sp + lz * cp;
        const wx = rx * cy + pz * sy + x - eye.x,
          wy = py + y - eye.y,
          wz = -rx * sy + pz * cy + z - eye.z;
        a[j * 3] = wx * r[0] + wy * r[1] + wz * r[2];
        a[j * 3 + 1] = wx * u[0] + wy * u[1] + wz * u[2];
        a[j * 3 + 2] = wx * f[0] + wy * f[1] + wz * f[2];
      }
      if (this.cut && this.veils(a)) continue;
      let count = 0;
      for (let j = 0; j < 3; j++) {
        const k = j * 3,
          prev = ((j + 2) % 3) * 3,
          inside = a[k + 2] >= near,
          was = a[prev + 2] >= near;
        if (inside !== was) {
          const t = (near - a[prev + 2]) / (a[k + 2] - a[prev + 2]);
          b[count++] = a[prev] + (a[k] - a[prev]) * t;
          b[count++] = a[prev + 1] + (a[k + 1] - a[prev + 1]) * t;
          b[count++] = near;
        }
        if (inside) {
          b[count++] = a[k];
          b[count++] = a[k + 1];
          b[count++] = a[k + 2];
        }
      }
      // What is left of the triangle, as a fan, each part put on the screen.
      for (let j = 3; j < count - 3; j += 3) {
        this.put(b[0], b[1], b[2], b[j], b[j + 1], b[j + 2], b[j + 3], b[j + 4], b[j + 5], vertices[i + 3], vertices[i + 4], vertices[i + 5], this.lens ? 5 : 0);
        if (this.overflow) return;
      }
    }
  }
}
