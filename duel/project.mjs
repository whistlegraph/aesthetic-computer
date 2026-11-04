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
  // A world point to screen space: x right and y up in -1..1, or null behind.
  project(x, y, z) {
    const e = this.eye,
      dx = x - e.x,
      dy = y - e.y,
      dz = z - e.z;
    const [r, u, f] = [this.right, this.up, this.forward];
    const depth = dx * f[0] + dy * f[1] + dz * f[2];
    if (depth < 0.1) return null;
    return {
      x: ((dx * r[0] + dy * r[1] + dz * r[2]) * this.sx) / depth + (this.shiftX || 0),
      y: ((dx * u[0] + dy * u[1] + dz * u[2]) * this.sy) / depth + (this.shiftY || 0),
      depth,
    };
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
      for (let j = 0; j < count; j += 3) {
        const depth = b[j + 2];
        b[j] = (b[j] * this.sx) / depth + (this.shiftX || 0);
        b[j + 1] = (b[j + 1] * this.sy) / depth + (this.shiftY || 0);
        b[j + 2] = far / (far - near) - (far * near) / (depth * (far - near));
      }
      for (let j = 3; j < count - 3; j += 3) {
        if (
          [0, 1, 2].some((axis) => {
            const lo = axis === 2 ? 0 : -1;
            return (
              (b[axis] < lo && b[j + axis] < lo && b[j + 3 + axis] < lo) ||
              (b[axis] > 1 && b[j + axis] > 1 && b[j + 3 + axis] > 1)
            );
          })
        )
          continue;
        // Over budget: drop the rest of this frame's geometry, never throw
        // (a throw here stops the frame loop and freezes the game).
        if (this.scene.triangleCount >= this.scene.maxTriangles) {
          this.overflow = true;
          return;
        }
        let at = this.scene.triangleCount++ * 18;
        for (const k of [0, j, j + 3]) {
          this.scene.vertices[at++] = b[k];
          this.scene.vertices[at++] = b[k + 1];
          this.scene.vertices[at++] = b[k + 2];
          this.scene.vertices[at++] = vertices[i + 3];
          this.scene.vertices[at++] = vertices[i + 4];
          this.scene.vertices[at++] = vertices[i + 5];
        }
      }
    }
  }
}
