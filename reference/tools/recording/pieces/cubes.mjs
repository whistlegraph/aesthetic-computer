// Raytraced cubes with pixel manipulation
// 2025.09.25.23.02.20

// Vector 3D class for ray tracing math
class Vec3 {
  constructor(x = 0, y = 0, z = 0) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  add(v) { return new Vec3(this.x + v.x, this.y + v.y, this.z + v.z); }
  sub(v) { return new Vec3(this.x - v.x, this.y - v.y, this.z - v.z); }
  mul(s) { return new Vec3(this.x * s, this.y * s, this.z * s); }
  dot(v) { return this.x * v.x + this.y * v.y + this.z * v.z; }
  length() { return Math.sqrt(this.dot(this)); }
  normalize() { const l = this.length(); return l > 0 ? this.mul(1/l) : this; }
}

// Ray class
class Ray {
  constructor(origin, direction) {
    this.origin = origin;
    this.direction = direction.normalize();
  }
  
  at(t) {
    return this.origin.add(this.direction.mul(t));
  }
}

// Axis-aligned bounding box (cube)
class Cube {
  constructor(min, max, color) {
    this.min = min; // minimum corner
    this.max = max; // maximum corner
    this.color = color;
  }
  
  // Ray-AABB intersection using slab method
  intersect(ray) {
    const invDir = new Vec3(
      1.0 / ray.direction.x,
      1.0 / ray.direction.y,
      1.0 / ray.direction.z
    );
    
    let t1 = (this.min.x - ray.origin.x) * invDir.x;
    let t2 = (this.max.x - ray.origin.x) * invDir.x;
    if (t1 > t2) [t1, t2] = [t2, t1];
    
    let t3 = (this.min.y - ray.origin.y) * invDir.y;
    let t4 = (this.max.y - ray.origin.y) * invDir.y;
    if (t3 > t4) [t3, t4] = [t4, t3];
    
    let t5 = (this.min.z - ray.origin.z) * invDir.z;
    let t6 = (this.max.z - ray.origin.z) * invDir.z;
    if (t5 > t6) [t5, t6] = [t6, t5];
    
    const tmin = Math.max(t1, Math.max(t3, t5));
    const tmax = Math.min(t2, Math.min(t4, t6));
    
    if (tmax < 0 || tmin > tmax) return null;
    
    const t = tmin > 0 ? tmin : tmax;
    if (t < 0) return null;
    
    const point = ray.at(t);
    const normal = this.getNormal(point);
    
    return { t, point, normal };
  }
  
  // Get surface normal at a point
  getNormal(point) {
    const center = this.min.add(this.max).mul(0.5);
    const size = this.max.sub(this.min).mul(0.5);
    const local = point.sub(center);
    
    // Find which face we hit by finding the largest component
    const absX = Math.abs(local.x / size.x);
    const absY = Math.abs(local.y / size.y);
    const absZ = Math.abs(local.z / size.z);
    
    if (absX > absY && absX > absZ) {
      return new Vec3(local.x > 0 ? 1 : -1, 0, 0);
    } else if (absY > absZ) {
      return new Vec3(0, local.y > 0 ? 1 : -1, 0);
    } else {
      return new Vec3(0, 0, local.z > 0 ? 1 : -1);
    }
  }
}

// HSV to RGB conversion
function hsvToRgb(h, s, v) {
  const c = v * s;
  const x = c * (1 - Math.abs((h / 60) % 2 - 1));
  const m = v - c;
  
  let r, g, b;
  if (h < 60) [r, g, b] = [c, x, 0];
  else if (h < 120) [r, g, b] = [x, c, 0];
  else if (h < 180) [r, g, b] = [0, c, x];
  else if (h < 240) [r, g, b] = [0, x, c];
  else if (h < 300) [r, g, b] = [x, 0, c];
  else [r, g, b] = [c, 0, x];
  
  return [
    Math.round((r + m) * 255),
    Math.round((g + m) * 255),
    Math.round((b + m) * 255)
  ];
}

let cubes = [];

function initCubes() {
  cubes = [];
  
  // Create 20 animated cubes
  for (let i = 0; i < 20; i++) {
    const hue = (i * 360 / 20) % 360;
    const [r, g, b] = hsvToRgb(hue, 0.8, 0.9);
    
    // Random position and size
    const centerX = (Math.random() - 0.5) * 8;
    const centerY = (Math.random() - 0.5) * 6;
    const centerZ = (Math.random() - 0.5) * 4 + 5;
    const size = 0.3 + Math.random() * 0.7;
    
    const cube = new Cube(
      new Vec3(centerX - size, centerY - size, centerZ - size),
      new Vec3(centerX + size, centerY + size, centerZ + size),
      { r, g, b }
    );
    
    // Add animation properties
    cube.originalCenter = new Vec3(centerX, centerY, centerZ);
    cube.rotationSpeed = (Math.random() - 0.5) * 0.02;
    cube.floatSpeed = (Math.random() - 0.5) * 0.01;
    cube.floatAmplitude = 0.5 + Math.random() * 1.0;
    
    cubes.push(cube);
  }
}

function updateCubes(frameTime) {
  cubes.forEach((cube, i) => {
    // Floating animation
    const floatOffset = Math.sin(frameTime * 0.003 + i) * cube.floatAmplitude;
    
    // Rotation animation (simulate by moving cubes in a circle)
    const rotationOffset = frameTime * cube.rotationSpeed;
    const radius = 0.5;
    const rotX = Math.cos(rotationOffset + i) * radius;
    const rotZ = Math.sin(rotationOffset + i) * radius;
    
    const newCenter = new Vec3(
      cube.originalCenter.x + rotX,
      cube.originalCenter.y + floatOffset,
      cube.originalCenter.z + rotZ
    );
    
    const size = cube.max.sub(cube.min).mul(0.5);
    cube.min = newCenter.sub(size);
    cube.max = newCenter.add(size);
  });
}

function castRay(ray) {
  let closest = null;
  let minT = Infinity;
  
  for (const cube of cubes) {
    const hit = cube.intersect(ray);
    if (hit && hit.t < minT) {
      minT = hit.t;
      closest = { ...hit, cube };
    }
  }
  
  return closest;
}

function shade(hit, ray) {
  const light = new Vec3(2, 4, 1).normalize();
  const lightDot = Math.max(0, hit.normal.dot(light));
  
  // Add some ambient lighting
  const ambient = 0.3;
  const diffuse = lightDot * 0.7;
  const intensity = ambient + diffuse;
  
  return {
    r: Math.round(hit.cube.color.r * intensity),
    g: Math.round(hit.cube.color.g * intensity),
    b: Math.round(hit.cube.color.b * intensity)
  };
}

function renderPixel(x, y, width, height) {
  // Camera setup
  const aspect = width / height;
  const fov = Math.PI / 3; // 60 degrees
  const camera = new Vec3(0, 0, 0);
  
  // Convert screen coordinates to world coordinates
  const u = (2 * x / width - 1) * aspect * Math.tan(fov / 2);
  const v = (1 - 2 * y / height) * Math.tan(fov / 2);
  
  const direction = new Vec3(u, v, 1).normalize();
  const ray = new Ray(camera, direction);
  
  const hit = castRay(ray);
  
  if (hit) {
    return shade(hit, ray);
  } else {
    // Background gradient
    const t = y / height;
    const r = Math.round(50 + t * 30);
    const g = Math.round(60 + t * 40);
    const b = Math.round(100 + t * 80);
    return { r, g, b };
  }
}

function paint({ api, frameTime }) {
  const { screen } = api;
  
  // Initialize cubes on first frame
  if (cubes.length === 0) {
    initCubes();
  }
  
  // Update cube animations
  updateCubes(frameTime);
  
  // Render each pixel
  for (let y = 0; y < screen.height; y++) {
    for (let x = 0; x < screen.width; x++) {
      const color = renderPixel(x, y, screen.width, screen.height);
      const index = (y * screen.width + x) * 4;
      
      screen.pixels[index] = color.r;     // Red
      screen.pixels[index + 1] = color.g; // Green
      screen.pixels[index + 2] = color.b; // Blue
      screen.pixels[index + 3] = 255;     // Alpha
    }
  }
}

export { paint };