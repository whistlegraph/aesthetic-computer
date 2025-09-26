// Mixed raytraced cubes and spheres with slow, graceful movement
// 2025.09.25.23.05.41

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

// Sphere class
class Sphere {
  constructor(center, radius, color) {
    this.center = center;
    this.radius = radius;
    this.color = color;
  }
  
  intersect(ray) {
    const oc = ray.origin.sub(this.center);
    const a = ray.direction.dot(ray.direction);
    const b = 2.0 * oc.dot(ray.direction);
    const c = oc.dot(oc) - this.radius * this.radius;
    const discriminant = b * b - 4 * a * c;
    
    if (discriminant < 0) return null;
    
    const t = (-b - Math.sqrt(discriminant)) / (2 * a);
    if (t < 0.001) return null;
    
    const point = ray.at(t);
    const normal = point.sub(this.center).normalize();
    return { t, point, normal };
  }
}

// Cube class
class Cube {
  constructor(min, max, color) {
    this.min = min;
    this.max = max;
    this.color = color;
  }
  
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
    if (t < 0.001) return null;
    
    const point = ray.at(t);
    const normal = this.getNormal(point);
    
    return { t, point, normal };
  }
  
  getNormal(point) {
    const center = this.min.add(this.max).mul(0.5);
    const size = this.max.sub(this.min).mul(0.5);
    const local = point.sub(center);
    
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

let objects = [];

function initObjects() {
  objects = [];
  
  // Create 8 spheres
  for (let i = 0; i < 8; i++) {
    const hue = (i * 360 / 16) % 360; // Spread colors across both objects
    const [r, g, b] = hsvToRgb(hue, 0.7, 0.9);
    
    const centerX = (Math.random() - 0.5) * 6;
    const centerY = (Math.random() - 0.5) * 4;
    const centerZ = 3 + Math.random() * 4;
    const radius = 0.3 + Math.random() * 0.4;
    
    const sphere = new Sphere(
      new Vec3(centerX, centerY, centerZ),
      radius,
      { r, g, b }
    );
    
    // Slow, graceful animation properties
    sphere.originalCenter = new Vec3(centerX, centerY, centerZ);
    sphere.rotationSpeed = (Math.random() - 0.5) * 0.003; // Much slower
    sphere.floatSpeed = (Math.random() - 0.5) * 0.002;    // Much slower
    sphere.floatAmplitude = 0.3 + Math.random() * 0.4;    // Smaller range
    sphere.type = 'sphere';
    
    objects.push(sphere);
  }
  
  // Create 8 cubes
  for (let i = 0; i < 8; i++) {
    const hue = ((i + 8) * 360 / 16) % 360; // Offset colors
    const [r, g, b] = hsvToRgb(hue, 0.8, 0.85);
    
    const centerX = (Math.random() - 0.5) * 6;
    const centerY = (Math.random() - 0.5) * 4;
    const centerZ = 3 + Math.random() * 4;
    const size = 0.25 + Math.random() * 0.3;
    
    const cube = new Cube(
      new Vec3(centerX - size, centerY - size, centerZ - size),
      new Vec3(centerX + size, centerY + size, centerZ + size),
      { r, g, b }
    );
    
    // Slow, graceful animation properties
    cube.originalCenter = new Vec3(centerX, centerY, centerZ);
    cube.rotationSpeed = (Math.random() - 0.5) * 0.002;  // Much slower
    cube.floatSpeed = (Math.random() - 0.5) * 0.0015;    // Much slower
    cube.floatAmplitude = 0.2 + Math.random() * 0.3;     // Smaller range
    cube.type = 'cube';
    
    objects.push(cube);
  }
}

function updateObjects(frameTime) {
  objects.forEach((obj, i) => {
    // Gentle floating animation - much slower
    const floatOffset = Math.sin(frameTime * 0.0008 + i * 0.5) * obj.floatAmplitude;
    
    // Gentle orbital motion - much slower
    const rotationOffset = frameTime * obj.rotationSpeed;
    const radius = 0.3; // Smaller orbit
    const rotX = Math.cos(rotationOffset + i * 0.8) * radius;
    const rotZ = Math.sin(rotationOffset + i * 0.8) * radius;
    
    const newCenter = new Vec3(
      obj.originalCenter.x + rotX,
      obj.originalCenter.y + floatOffset,
      obj.originalCenter.z + rotZ
    );
    
    if (obj.type === 'sphere') {
      obj.center = newCenter;
    } else if (obj.type === 'cube') {
      const size = obj.max.sub(obj.min).mul(0.5);
      obj.min = newCenter.sub(size);
      obj.max = newCenter.add(size);
    }
  });
}

function castRay(ray) {
  let closest = null;
  let minT = Infinity;
  
  for (const obj of objects) {
    const hit = obj.intersect(ray);
    if (hit && hit.t < minT) {
      minT = hit.t;
      closest = { ...hit, object: obj };
    }
  }
  
  return closest;
}

function shade(hit, ray) {
  // Soft, warm lighting
  const light = new Vec3(1, 2, 0.5).normalize();
  const lightDot = Math.max(0, hit.normal.dot(light));
  
  // More ambient light for softer look
  const ambient = 0.4;
  const diffuse = lightDot * 0.6;
  const intensity = ambient + diffuse;
  
  return {
    r: Math.round(hit.object.color.r * intensity),
    g: Math.round(hit.object.color.g * intensity),
    b: Math.round(hit.object.color.b * intensity)
  };
}

function renderPixel(x, y, width, height) {
  const aspect = width / height;
  const fov = Math.PI / 4; // Slightly wider field of view
  const camera = new Vec3(0, 0, 0);
  
  const u = (2 * x / width - 1) * aspect * Math.tan(fov / 2);
  const v = (1 - 2 * y / height) * Math.tan(fov / 2);
  
  const direction = new Vec3(u, v, 1).normalize();
  const ray = new Ray(camera, direction);
  
  const hit = castRay(ray);
  
  if (hit) {
    return shade(hit, ray);
  } else {
    // Soft gradient background
    const t = y / height;
    const r = Math.round(40 + t * 25);
    const g = Math.round(50 + t * 35);
    const b = Math.round(80 + t * 60);
    return { r, g, b };
  }
}

function paint({ api, frameTime }) {
  const { screen } = api;
  
  if (objects.length === 0) {
    initObjects();
  }
  
  updateObjects(frameTime);
  
  // Render each pixel
  for (let y = 0; y < screen.height; y++) {
    for (let x = 0; x < screen.width; x++) {
      const color = renderPixel(x, y, screen.width, screen.height);
      const index = (y * screen.width + x) * 4;
      
      screen.pixels[index] = color.r;
      screen.pixels[index + 1] = color.g;
      screen.pixels[index + 2] = color.b;
      screen.pixels[index + 3] = 255;
    }
  }
}

export { paint };