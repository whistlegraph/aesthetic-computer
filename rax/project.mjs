// Project/clamp at the camera near plane, then write directly into Oskiewar's
// reusable scene buffer. Every face and letter participates in the depth test.
import { bend, sight, hidden, depthRange, widest, ballOf, NEAR, SPLIT, SHARE } from './planet.mjs';
// 📺 Screens: models whose pixels are a picture shown, not a thing lit. The
// lens (lens-host.mjs) glazes them as the Prompt's paper is glazed.
const SCREENS = new WeakSet();
export const screen = (made) => (SCREENS.add(made), made);
const isScreen = (made) => SCREENS.has(made);
const BOW = 0.006; // how far a straight edge may cut under the ball's round, in the picture (about a pixel and a half)
// 🧻 What lies flat on the land (the grass, a path a few millimetres over it,
// paint, the far land a little under it) is one thin layer on another. A
// straight edge cuts under the ball's round, and no two layers are cut into
// the same edges: so far off one dips through the other in stripes. Two
// things keep them in order. An edge of such a shape is halved until it cuts
// under by no more than THIN of how far the eye stands off the land there
// (so along the eye's line it is out by no more than THIN of the distance).
// And each corner within LAYER of the land is written a little nearer or
// farther than it is, by its height: a layer five millimetres up is nearer
// by twice THIN of its distance. Only depth moves; the picture does not.
const LAYER = 0.03, THIN = 0.002, PER_METRE = (2 * THIN) / 0.005;
// How much nearer (as a share of its distance) a corner `h` metres over the land is written: none from 4 cm up.
export const layerBias = (h) => (h < -LAYER || h > 0.04 ? 0 : h <= 0.02 ? PER_METRE * h : PER_METRE * (0.04 - h));
const SIZED = 8e-4; // twice the area in the picture over which a shape counts as something near (a mote does not)
export class Projector {
  constructor(scene) {
    this.scene = scene;
    this.input = new Float64Array(12);
    this.clipped = new Float64Array(15);
    this.cutout = new Float64Array(12);
    this.bends = true; // (false while the sky is drawn: it is not wrapped round the ball)
    this.ground = 0; // the height the land lies at, in the room's own metres
    this.range = depthRange(0);
    this.nearest = 0;
  }
  // Once a frame, when the eye, the lens and the planet are set and before
  // anything is drawn. Depth is shared out by how near the nearest thing of
  // any size was last frame (planet.mjs); a frame that showed none keeps
  // what it had.
  frame() {
    if (this.nearest < Infinity) this.range = depthRange(this.nearest);
    this.nearest = Infinity;
    this.horizon = this.planet ? sight(this.planet, this.eye.y - this.ground) : Infinity;
  }
  // The widest angle off straight ahead still in the picture, kept until the picture changes.
  wide() {
    const wider = this.card ? this.stretch || 1 : 1; // (the flat picture drawn for the card's lens reaches farther out)
    const reach = Math.hypot((1 + Math.abs(this.shiftX || 0)) / (this.sx * wider), (1 + Math.abs(this.shiftY || 0)) / (this.sy * wider)), lens = this.card ? 0 : this.lens || 0;
    if (this.wideFor !== reach || this.wideLens !== lens) { this.wideFor = reach; this.wideLens = lens; this.widest = widest(reach, lens); }
    return this.widest;
  }
  // Is all of this model out of the picture: over the horizon, past the far
  // side of the ball, or off to one side of where the eye looks? (By the
  // ball round its corners, before any corner is worked on.)
  unseen(vertices, x, y, z, yaw, pitch, scale, roll) {
    const b = ballOf(vertices), e = this.eye, f = this.forward;
    const lx = b.x * scale, ly = b.y * scale, lz = b.z * scale;
    const cr = Math.cos(roll), sr = Math.sin(roll), cp = Math.cos(pitch), sp = Math.sin(pitch), cy = Math.cos(yaw), sy = Math.sin(yaw);
    const rx = lx * cr - ly * sr, ry = lx * sr + ly * cr, py = ry * cp - lz * sp, pz = ry * sp + lz * cp;
    let wx = rx * cy + pz * sy + x - e.x, wy = py + y - e.y, wz = -rx * sy + pz * cy + z - e.z, r = b.r * Math.abs(scale);
    if (this.planet && this.bends) {
      const R = this.planet, E = e.y - this.ground, up = wy + E;
      // 🕳️ From inside the planet nothing that stands wholly over the land is seen: the land is in the way.
      if (E < 0 && !pitch && !roll && y + b.low * scale - this.ground > 0.05 && scale > 0) return true;
      if (this.horizon < Infinity && hidden(R, E, Math.hypot(wx, wz), up, r)) return true; // (the view puts the horizon off at infinity for the shadows' solids: one is counted where it lies behind the land too)
      [wx, wy, wz] = bend(wx, up, wz, R, E);
      if (up + r > 0) r *= 1 + (up + r) / R; // (the ball spreads what stands high on it)
    }
    const far = Math.hypot(wx, wy, wz);
    if (far <= r) return false;
    const a = this.wide() + Math.asin(r / far);
    return a < Math.PI && (wx * f[0] + wy * f[1] + wz * f[2]) / far < Math.cos(a);
  }
  overlay(vertices, width, height) {
    for (let i = 0; i < vertices.length; i += 18) {
      if (this.scene.triangleCount >= this.scene.maxTriangles) return;
      let at = this.scene.triangleCount++ * 18;
      for (let j = 0; j < 3; j++) {
        const k = i + j * 6;
        this.scene.vertices[at++] = (-vertices[k] * 2) / width;
        this.scene.vertices[at++] = (vertices[k + 1] * 2) / height;
        this.scene.vertices[at++] = 0.00001 + vertices[k + 2] - (this.under || 0); // (under: a whole unit nearer than the world, so the card knows the HUD from it)
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
    // (On the planet: round the ball, as the shapes are.)
    let wx = dx, wy = dy, wz = dz;
    if (this.planet) [wx, wy, wz] = bend(dx, y - this.ground, dz, this.planet, e.y - this.ground);
    const depth = wx * f[0] + wy * f[1] + wz * f[2];
    if (depth < 0.1) return null;
    const px = (wx * r[0] + wy * r[1] + wz * r[2]) / depth, py = (wx * u[0] + wy * u[1] + wz * u[2]) / depth, k = this.squeeze(px, py);
    return { x: px * k * this.sx + (this.shiftX || 0), y: py * k * this.sy + (this.shiftY || 0), depth };
  }
  // 🔭 The lens (0: none, a flat picture as a pinhole makes it; 1: all of
  // it). A flat picture of a wide view stretches whatever is near its edges.
  // The lens keeps the middle exactly as the flat picture has it and draws
  // the edges in toward it, as a wide lens does (the stereographic one: a
  // thing at angle a from the middle lies 2 tan(a/2) out, not tan(a)), so
  // the same screen holds more of the world to either side and nothing at
  // the edge is pulled long. How much a point at (x, y) over its depth moves in:
  // 🔭 When the card bends the finished picture (lens-host.mjs) no corner is moved here.
  bend(x, y) {
    return this.card ? 1 : this.squeeze(x, y);
  }
  squeeze(x, y) {
    return this.lens ? 1 + (2 / (1 + Math.sqrt(1 + x * x + y * y)) - 1) * this.lens : 1;
  }
  // One triangle, in the eye's own space (right, up, ahead), put on the
  // screen in its colour. Under the lens a straight edge is a curve, so an
  // edge that is long on the screen is halved, and its halves again (to
  // `depth` times): by the edge alone, so two triangles that share one
  // halve it at the same place and no gap opens between them.
  put(x0, y0, z0, x1, y1, z1, x2, y2, z2, cr, cg, cb, depth) {
    // (On the card the picture is drawn flat and wider, to be bent after: card, stretch.)
    const near = NEAR, sx = this.sx * (this.card ? this.stretch || 1 : 1), sy = this.sy * (this.card ? this.stretch || 1 : 1), ox = this.shiftX || 0, oy = this.shiftY || 0;
    // 🌍 The land is a small planet's, wrapped round a ball of radius
    // `planet`: a thing a way off along the ground stands that far round the
    // ball, upright on it (so a tower far off leans away), and what is
    // farther than half way round is at the far side. The sky, which is
    // drawn far off about the eye, is left where it is (`bends` is off for
    // it). (The corners passed on to the halves are the flat ones: each is
    // bent here.)
    const fx0 = x0, fy0 = y0, fz0 = z0, fx1 = x1, fy1 = y1, fz1 = z1, fx2 = x2, fy2 = y2, fz2 = z2;
    let bow = 0, r0 = 0, r1 = 0, r2 = 0; // (each corner's own distance from the ball's middle)
    let flat = 0, g0 = 0, g1 = 0, g2 = 0; // lying on the land: how far the eye stands off the land at each corner
    let d0 = 0, d1 = 0, d2 = 0; // how much nearer each corner is written than it is (a share of its distance)
    let w0 = false, w1 = false, w2 = false; // edges that run too far round the ball to be judged by their ends
    if (this.planet && this.bends) {
      const R = this.planet, E = this.eye.y - this.ground, ux = this.right[1], uy = this.up[1], uz = this.forward[1];
      let gone = 0; // corners past the far side of the ball: a shape that is all there is not drawn
      let nearby = Infinity, reach = 0, top = -Infinity, low = Infinity; // the corners least and most far off along the ground, the highest and the lowest
      for (let c = 0; c < 3; c++) {
        const x = c === 0 ? x0 : c === 1 ? x1 : x2, y = c === 0 ? y0 : c === 1 ? y1 : y2, z = c === 0 ? z0 : c === 1 ? z1 : z2;
        const h = x * ux + y * uy + z * uz, d = Math.sqrt(Math.max(0, x * x + y * y + z * z - h * h));
        if (d > Math.PI * R) gone++;
        if (d < nearby) nearby = d;
        if (d > reach) reach = d;
        if (h + E > top) top = h + E;
        if (h + E < low) low = h + E;
        const turn = Math.min(Math.PI, d / R), out = R + h + E;
        const along = d > 1e-6 ? (out * Math.sin(turn)) / d : out / R, lift = out * Math.cos(turn) - R - E;
        // (Across the ground: scaled; up: where the ball puts it.)
        const nx = (x - h * ux) * along + ux * lift, ny = (y - h * uy) * along + uy * lift, nz = (z - h * uz) * along + uz * lift;
        if (c === 0) { x0 = nx; y0 = ny; z0 = nz; r0 = out; } else if (c === 1) { x1 = nx; y1 = ny; z1 = nz; r1 = out; } else { x2 = nx; y2 = ny; z2 = nz; r2 = out; }
      }
      // 🌄 Every corner over the horizon, or past the far side: is all of it?
      // (By the circle round it on the ground: a big shape, a great square
      // of the far land, may still reach back over to the eye.)
      if (gone === 3 || nearby > this.horizon) {
        const mx = (fx0 + fx1 + fx2) / 3, my = (fy0 + fy1 + fy2) / 3, mz = (fz0 + fz1 + fz2) / 3, mh = mx * ux + my * uy + mz * uz;
        let wide = 0;
        for (let c = 0; c < 3; c++) {
          const vx = (c === 0 ? fx0 : c === 1 ? fx1 : fx2) - mx, vy = (c === 0 ? fy0 : c === 1 ? fy1 : fy2) - my, vz = (c === 0 ? fz0 : c === 1 ? fz1 : fz2) - mz, vh = vx * ux + vy * uy + vz * uz;
          wide = Math.max(wide, vx * vx + vy * vy + vz * vz - vh * vh);
        }
        const off = Math.sqrt(Math.max(0, mx * mx + my * my + mz * mz - mh * mh)) - Math.sqrt(wide);
        if (off > Math.min(Math.PI * R, this.horizon < Infinity ? this.horizon + (top > 0 ? Math.sqrt(2 * top * R) : 0) : Infinity)) return;
      }
      // 🪢 An edge that runs a long way round the ball (over half its radius)
      // is not to be judged by its two ends: both may be out of the picture,
      // or behind the eye, with its middle in plain sight. It is halved first.
      if (reach > R / 4 && depth > 0) {
        const most = (R * R) / 4;
        w0 = (fx0 - fx1) * (fx0 - fx1) + (fy0 - fy1) * (fy0 - fy1) + (fz0 - fz1) * (fz0 - fz1) - (r0 - r1) * (r0 - r1) > most;
        w1 = (fx1 - fx2) * (fx1 - fx2) + (fy1 - fy2) * (fy1 - fy2) + (fz1 - fz2) * (fz1 - fz2) - (r1 - r2) * (r1 - r2) > most;
        w2 = (fx2 - fx0) * (fx2 - fx0) + (fy2 - fy0) * (fy2 - fy0) + (fz2 - fz0) * (fz2 - fz0) - (r2 - r0) * (r2 - r0) > most;
      }
      bow = (8 * R * BOW) / sy;
      // 🧻 (Few shapes have a corner in the thin layer on the land: only they pay for it.)
      if (low < 0.04 && top > -LAYER) {
        d0 = layerBias(r0 - R); d1 = layerBias(r1 - R); d2 = layerBias(r2 - R);
        if (top <= LAYER && low >= -LAYER) {
          // How far the eye stands off the land at each corner (off the flat that touches the ball there).
          const C = R + E;
          flat = 8 * R * THIN;
          g0 = Math.abs(x0 * x0 + y0 * y0 + z0 * z0 + C * (x0 * ux + y0 * uy + z0 * uz)) / r0;
          g1 = Math.abs(x1 * x1 + y1 * y1 + z1 * z1 + C * (x1 * ux + y1 * uy + z1 * uz)) / r1;
          g2 = Math.abs(x2 * x2 + y2 * y2 + z2 * z2 + C * (x2 * ux + y2 * uy + z2 * uz)) / r2;
        }
      }
    }
    // 🔪 A corner behind the eye (the ball can put one there; inside the
    // ball `draw` leaves every such cut to here, after the bending): what is
    // ahead of the near plane is kept, as three corners or four.
    const b0 = z0 < near, b1 = z1 < near, b2 = z2 < near, behind = b0 || b1 || b2, q = this.cutout, wide = w0 || w1 || w2;
    let ax = 0, ay = 0, bx = 0, by = 0, cx = 0, cy = 0, kept = 3;
    if (!behind) {
      const k0 = this.bend(x0 / z0, y0 / z0), k1 = this.bend(x1 / z1, y1 / z1), k2 = this.bend(x2 / z2, y2 / z2);
      ax = (x0 * k0 * sx) / z0 + ox; ay = (y0 * k0 * sy) / z0 + oy; bx = (x1 * k1 * sx) / z1 + ox; by = (y1 * k1 * sy) / z1 + oy;
      cx = (x2 * k2 * sx) / z2 + ox; cy = (y2 * k2 * sy) / z2 + oy;
      // Wholly off one side of the picture: not drawn.
      if (!wide && ((ax < -1 && bx < -1 && cx < -1) || (ax > 1 && bx > 1 && cx > 1) || (ay < -1 && by < -1 && cy < -1) || (ay > 1 && by > 1 && cy > 1))) return;
    } else {
      if (!wide && b0 && b1 && b2) return;
      if (!b0) { const k = this.bend(x0 / z0, y0 / z0); ax = (x0 * k * sx) / z0 + ox; ay = (y0 * k * sy) / z0 + oy; }
      if (!b1) { const k = this.bend(x1 / z1, y1 / z1); bx = (x1 * k * sx) / z1 + ox; by = (y1 * k * sy) / z1 + oy; }
      if (!b2) { const k = this.bend(x2 / z2, y2 / z2); cx = (x2 * k * sx) / z2 + ox; cy = (y2 * k * sy) / z2 + oy; }
      kept = 0;
      for (let j = 0; j < 3 && !wide; j++) {
        const x = j === 0 ? x0 : j === 1 ? x1 : x2, y = j === 0 ? y0 : j === 1 ? y1 : y2, z = j === 0 ? z0 : j === 1 ? z1 : z2;
        const px = j === 0 ? x2 : j === 1 ? x0 : x1, py = j === 0 ? y2 : j === 1 ? y0 : y1, pz = j === 0 ? z2 : j === 1 ? z0 : z1;
        if (z >= near !== pz >= near) {
          const t = (near - pz) / (z - pz);
          q[kept++] = px + (x - px) * t; q[kept++] = py + (y - py) * t; q[kept++] = near;
        }
        if (z >= near) { q[kept++] = x; q[kept++] = y; q[kept++] = z; }
      }
      kept /= 3;
      let left = 0, right = 0, low = 0, high = 0;
      for (let j = 0; j < kept; j++) {
        const z = q[j * 3 + 2], k = this.bend(q[j * 3] / z, q[j * 3 + 1] / z);
        const px = (q[j * 3] = (q[j * 3] * k * sx) / z + ox), py = (q[j * 3 + 1] = (q[j * 3 + 1] * k * sy) / z + oy);
        if (px < -1) left++; else if (px > 1) right++;
        if (py < -1) low++; else if (py > 1) high++;
      }
      if (!wide && (left === kept || right === kept || low === kept || high === kept)) return;
    }
    if (depth > 0) {
      // An edge is halved when it is long in the picture, when it would cut
      // too far under the ball's round (by how far its ends are apart round
      // the ball, and how near the nearer is), and when it passes behind the
      // eye: each by the edge alone, so both shapes on it halve it alike.
      const fine = this.fine || 0.12, LONG = fine * fine; // (0.12 is a sixteenth of the picture's width: short enough that two flat things a few millimetres apart keep their order)
      let e0 = w0 || b0 !== b1, e1 = w1 || b1 !== b2, e2 = w2 || b2 !== b0;
      if (!e0 && !b0) {
        const u = ax - bx, v = ay - by;
        if (u * u + v * v > LONG) e0 = true;
        else if (bow > 0) { const p = x0 - x1, s = y0 - y1, t = z0 - z1, o = r0 - r1, z = z0 < z1 ? z0 : z1; e0 = p * p + s * s + t * t - o * o > (flat ? flat * Math.max(g0 < g1 ? g0 : g1, 0.03 * z) : bow * z); }
      }
      if (!e1 && !b1) {
        const u = bx - cx, v = by - cy;
        if (u * u + v * v > LONG) e1 = true;
        else if (bow > 0) { const p = x1 - x2, s = y1 - y2, t = z1 - z2, o = r1 - r2, z = z1 < z2 ? z1 : z2; e1 = p * p + s * s + t * t - o * o > (flat ? flat * Math.max(g1 < g2 ? g1 : g2, 0.03 * z) : bow * z); }
      }
      if (!e2 && !b2) {
        const u = cx - ax, v = cy - ay;
        if (u * u + v * v > LONG) e2 = true;
        else if (bow > 0) { const p = x2 - x0, s = y2 - y0, t = z2 - z0, o = r2 - r0, z = z2 < z0 ? z2 : z0; e2 = p * p + s * s + t * t - o * o > (flat ? flat * Math.max(g2 < g0 ? g2 : g0, 0.03 * z) : bow * z); }
      }
      if (e0 || e1 || e2) {
        x0 = fx0; y0 = fy0; z0 = fz0; x1 = fx1; y1 = fy1; z1 = fz1; x2 = fx2; y2 = fy2; z2 = fz2;
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
    if (r0 && this.orbitLight) {
      const axis = this.orbitLight, center = 3 * (this.planet + this.eye.y - this.ground);
      const nx = x0 + x1 + x2 + center * this.right[1];
      const ny = y0 + y1 + y2 + center * this.up[1];
      const nz = z0 + z1 + z2 + center * this.forward[1];
      const diffuse = Math.max(0, (nx*axis[0] + ny*axis[1] + nz*axis[2]) / (r0 + r1 + r2));
      const light = 1 - this.orbitShade * (1 - diffuse);
      cr *= light; cg *= light; cb *= light;
    }
    if (!behind) return this.emit(ax, ay, z0 * (1 - d0), bx, by, z1 * (1 - d1), cx, cy, z2 * (1 - d2), cr, cg, cb);
    for (let j = 1; j + 1 < kept; j++) this.emit(q[0], q[1], q[2], q[j * 3], q[j * 3 + 1], q[j * 3 + 2], q[j * 3 + 3], q[j * 3 + 4], q[j * 3 + 5], cr, cg, cb);
  }
  // One triangle as it lies in the picture (x and y there, z how far ahead), written for the card.
  emit(ax, ay, z0, bx, by, z1, cx, cy, z2, cr, cg, cb) {
    // Over budget: drop the rest of this frame's geometry, never throw
    // (a throw here stops the frame loop and freezes the game).
    if (this.scene.triangleCount >= this.scene.maxTriangles) {
      this.overflow = true;
      return;
    }
    const v = this.scene.vertices, range = this.range, split = range.split, open = range.open;
    // 📏 How near the nearest thing of any size is, past the eye's own few metres: the next frame's depth goes by it (`frame`).
    const least = z0 < z1 ? (z0 < z2 ? z0 : z2) : z1 < z2 ? z1 : z2;
    if (least < this.nearest && (z0 > SPLIT || z1 > SPLIT || z2 > SPLIT) && Math.abs((bx - ax) * (cy - ay) - (cx - ax) * (by - ay)) > SIZED)
      this.nearest = least > SPLIT ? least : SPLIT;
    // 📉 Coming apart (decay, 0 to 1, when the server has gone): each shape
    // has its own number, and by it some drop out, the rest snap to a
    // coarser and coarser grid and slip sideways in bands, some turn to the
    // green of a terminal, and at the end they fall.
    if (this.decay > 0) {
      const g = this.decay, tick = Math.floor((this.decayAt || 0) / 90);
      let h = (Math.imul((ax * 4096) | 0, 73856093) ^ Math.imul((ay * 4096) | 0, 19349663) ^ Math.imul(tick + ((z0 * 16) | 0), 83492791)) >>> 0;
      const roll = () => ((h = (Math.imul(h, 1664525) + 1013904223) >>> 0) / 4294967296);
      if (roll() < g * 0.55) return; // gone
      const cell = 1 / (160 - 150 * g), snap = (n) => Math.round(n / cell) * cell;
      const slip = roll() < g * 0.6 ? (roll() - 0.5) * 0.5 * g : 0, drop = g > 0.6 ? (g - 0.6) * 2.5 * roll() : 0;
      ax = snap(ax) + slip; bx = snap(bx) + slip; cx = snap(cx) + slip;
      ay = snap(ay) - drop; by = snap(by) - drop; cy = snap(cy) - drop;
      if (roll() < g * 0.45) { const lit = 0.25 + 0.75 * roll(); cr = 0.05 * lit; cg = lit; cb = 0.3 * lit; }
      else if (roll() < g * 0.3) { const t = cr; cr = cb; cb = cg; cg = t; } // the colours' wires crossed
    }
    // Depth (planet.mjs depthAt): all one part, or the eye's own few metres and then the far.
    const a = range.a, b = range.b;
    let d0, d1, d2;
    if (split === Infinity) { d0 = a - b / z0; d1 = a - b / z1; d2 = a - b / z2; }
    else {
      const c = range.c, d = range.d;
      d0 = z0 <= split ? a - b / z0 : z0 >= open ? c - d / z0 : SHARE;
      d1 = z1 <= split ? a - b / z1 : z1 >= open ? c - d / z1 : SHARE;
      d2 = z2 <= split ? a - b / z2 : z2 >= open ? c - d / z2 : SHARE;
    }
    const at = this.scene.triangleCount++ * 18;
    v[at] = ax; v[at + 1] = ay; v[at + 2] = d0; v[at + 3] = cr; v[at + 4] = cg; v[at + 5] = cb;
    v[at + 6] = bx; v[at + 7] = by; v[at + 8] = d1; v[at + 9] = cr; v[at + 10] = cg; v[at + 11] = cb;
    v[at + 12] = cx; v[at + 13] = cy; v[at + 14] = d2; v[at + 15] = cr; v[at + 16] = cg; v[at + 17] = cb;
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
    const near = NEAR;
    // 📺 A screen's green is lifted by 4 so the lens knows its pixels (marks: on for the WebGPU host only).
    const lift = this.marks && isScreen(vertices) ? 4 : 0;
    if (vertices.length >= 216 && this.unseen(vertices, x, y, z, yaw, pitch, scale, roll)) return; // (a dozen triangles and up: fewer are as soon drawn as asked about)
    // 🕳️ Inside the ball the land is overhead and all round: what is behind
    // the eye in the flat world may be ahead of it on the ball, so nothing
    // is cut away here, before the bending (`put` cuts after it).
    const whole = this.planet && this.bends && eye.y < this.ground;
    // How many times a shape may be halved: seven, or as many as the view asks for the far land's great squares.
    const halvings = (this.lens && !this.card) || this.planet ? this.halvings || 7 : 0; // (the ball needs its edges halved whether or not the lens is on)
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
      if (whole) {
        if (this.scene.triangleCount >= this.scene.maxTriangles) { this.overflow = true; return; }
        this.put(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], vertices[i + 3], vertices[i + 4] + lift, vertices[i + 5], halvings);
        continue;
      }
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
        // (How short an edge must be is one rule for the whole frame, `fine`,
        // so two shapes that share an edge always halve it alike and no crack
        // opens; the view sets it from how full the last frame was.)
        if (this.scene.triangleCount >= this.scene.maxTriangles) { this.overflow = true; return; } // (by the count, never by the flag: that is only lowered where shadows are cast)
        this.put(b[0], b[1], b[2], b[j], b[j + 1], b[j + 2], b[j + 3], b[j + 4], b[j + 5], vertices[i + 3], vertices[i + 4] + lift, vertices[i + 5], halvings);
      }
    }
  }
}
