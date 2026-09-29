// Oskiewar scene contract shared by browser and native GPU hosts.
// Geometry arrives in logical screen space after the canonical JS camera has
// projected and clipped it. Z remains authoritative and must reach a depth
// buffer; hosts must never discard it or substitute painter ordering.

export const OSKIEWAR_SCENE_VERSION = 1;
export const OSKIEWAR_MAX_TRIANGLES = 8192;
export const OSKIEWAR_VERTEX_FLOATS = 6; // clip x/y/z + linear r/g/b
export const OSKIEWAR_TRIANGLE_FLOATS = OSKIEWAR_VERTEX_FLOATS * 3;

export function clipDepth(z) {
  if (!Number.isFinite(z)) throw new RangeError("scene depth must be finite");
  // This is the mapping already shipped by the Xbox D3D renderer. Keeping it
  // here makes WebGL/Metal/D3D compare the same normalized depth values.
  return Math.max(0, Math.min(1, (z + 1.5) / 3));
}
export function clipPoint(x, y, z, width, height) {
  if (![x, y, width, height].every(Number.isFinite) || width <= 0 || height <= 0)
    throw new RangeError("invalid scene viewport or vertex");
  return [x / (width / 2) - 1, 1 - y / (height / 2), clipDepth(z)];
}

export class OskiewarScene3D {
  constructor({ maxTriangles = OSKIEWAR_MAX_TRIANGLES } = {}) {
    this.maxTriangles = maxTriangles;
    this.vertices = new Float32Array(maxTriangles * OSKIEWAR_TRIANGLE_FLOATS);
    this.triangleCount = 0;
  }

  beginFrame() { this.triangleCount = 0; }

  triangle(x1, y1, z1, x2, y2, z2, x3, y3, z3,
    r = 255, g = 255, b = 255, width = 1920, height = 1080) {
    if (this.triangleCount >= this.maxTriangles) return false;
    const points = [clipPoint(x1, y1, z1, width, height),
      clipPoint(x2, y2, z2, width, height),
      clipPoint(x3, y3, z3, width, height)];
    const color = [r, g, b].map((channel) =>
      Math.max(0, Math.min(255, Number(channel) || 0)) / 255);
    let at = this.triangleCount * OSKIEWAR_TRIANGLE_FLOATS;
    for (const point of points) {
      this.vertices.set([...point, ...color], at);
      at += OSKIEWAR_VERTEX_FLOATS;
    }
    this.triangleCount++;
    return true;
  }

  frameVertices() {
    return this.vertices.subarray(0,
      this.triangleCount * OSKIEWAR_TRIANGLE_FLOATS);
  }
}
