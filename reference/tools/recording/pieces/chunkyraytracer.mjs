// Chunky Ray Tracer - Pixel-perfect ray tracing with nearest-neighbor upscaling
// No blurry bilinear filtering - pure crispy pixels!

// Vector3 operations (optimized)
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
  normalize() {
    const len = this.length();
    return len > 0 ? this.mul(1 / len) : new Vec3();
  }
  reflect(normal) { return this.sub(normal.mul(2 * this.dot(normal))); }
}

// Ray class
class Ray {
  constructor(origin, direction) {
    this.origin = origin;
    this.direction = direction.normalize();
  }
  at(t) { return this.origin.add(this.direction.mul(t)); }
}

// Sphere object
class Sphere {
  constructor(center, radius, color, reflectivity = 0.2) {
    this.center = center;
    this.radius = radius;
    this.color = color;
    this.reflectivity = reflectivity;
  }

  intersect(ray) {
    const oc = ray.origin.sub(this.center);
    const a = ray.direction.dot(ray.direction);
    const b = 2 * oc.dot(ray.direction);
    const c = oc.dot(oc) - this.radius * this.radius;
    const discriminant = b * b - 4 * a * c;

    if (discriminant < 0) return null;

    const t1 = (-b - Math.sqrt(discriminant)) / (2 * a);
    const t2 = (-b + Math.sqrt(discriminant)) / (2 * a);
    const t = t1 > 0.001 ? t1 : t2 > 0.001 ? t2 : null;
    
    if (t === null) return null;

    const point = ray.at(t);
    const normal = point.sub(this.center).normalize();
    return { t, point, normal, object: this };
  }
}

// Scene setup
let scene = null;
let camera = null;
let renderBuffer = null;
const RENDER_SCALE = 0.2; // Even chunkier! 1/5 resolution for bigger pixels

function initScene() {
  scene = {
    objects: [
      new Sphere(new Vec3(0, 0, -5), 1, new Vec3(1, 0.3, 0.3), 0.3),
      new Sphere(new Vec3(-2, -0.5, -4), 0.8, new Vec3(0.3, 1, 0.3), 0.4),
      new Sphere(new Vec3(2, -0.3, -6), 1.2, new Vec3(0.3, 0.3, 1), 0.2),
      new Sphere(new Vec3(0, 1.5, -7), 0.6, new Vec3(1, 1, 0.3), 0.5),
    ],
    light: new Vec3(-2, 3, -1), // Single light for speed
    ambientLight: new Vec3(0.2, 0.2, 0.3)
  };

  camera = { position: new Vec3(0, 0, 0), fov: 60 };
}

function findClosestIntersection(ray, objects) {
  let closest = null;
  let minT = Infinity;

  for (const obj of objects) {
    const hit = obj.intersect(ray);
    if (hit && hit.t < minT) {
      minT = hit.t;
      closest = hit;
    }
  }
  return closest;
}

function traceRay(ray, objects, depth = 0) {
  if (depth > 2) return new Vec3(0.1, 0.2, 0.4); // Sky color

  const hit = findClosestIntersection(ray, objects);
  if (!hit) return new Vec3(0.1, 0.2, 0.4); // Sky color

  // Simple lighting
  const lightDir = scene.light.sub(hit.point).normalize();
  const lightIntensity = Math.max(0, hit.normal.dot(lightDir));
  
  // Shadow check (simplified)
  const shadowRay = new Ray(hit.point.add(hit.normal.mul(0.001)), lightDir);
  const shadowHit = findClosestIntersection(shadowRay, objects);
  const inShadow = shadowHit && shadowHit.t < scene.light.sub(hit.point).length();
  
  let color = hit.object.color.mul(lightIntensity * (inShadow ? 0.3 : 1.0))
    .add(scene.ambientLight);

  // Simple reflection
  if (hit.object.reflectivity > 0) {
    const reflectDir = ray.direction.reflect(hit.normal);
    const reflectRay = new Ray(hit.point.add(hit.normal.mul(0.001)), reflectDir);
    const reflectColor = traceRay(reflectRay, objects, depth + 1);
    color = color.mul(1 - hit.object.reflectivity).add(
      reflectColor.mul(hit.object.reflectivity)
    );
  }

  return color;
}

function getRay(x, y, width, height) {
  const aspect = width / height;
  const fov = camera.fov * Math.PI / 180;
  const scale = Math.tan(fov / 2);
  const px = (2 * (x + 0.5) / width - 1) * scale * aspect;
  const py = (1 - 2 * (y + 0.5) / height) * scale;
  const direction = new Vec3(px, py, -1).normalize();
  return new Ray(camera.position, direction);
}

// Nearest-neighbor sampling for chunky pixel aesthetic - NO BLURRING!
function nearestSample(buffer, x, y, width, height) {
  const fx = Math.floor(x * width);
  const fy = Math.floor(y * height);
  const sx = Math.min(fx, width - 1);
  const sy = Math.min(fy, height - 1);
  return buffer[sy * width + sx];
}

let time = 0;

function paint({ api, frameIndex, frameTime, simCount }) {
  if (!scene) initScene();
  
  time = frameTime * 0.001;
  
  // Animate camera
  const radius = 1.5;
  camera.position = new Vec3(
    Math.sin(time * 0.3) * radius,
    Math.sin(time * 0.2) * 0.5,
    Math.cos(time * 0.3) * radius
  );

  // Animate spheres
  scene.objects[0].center = new Vec3(
    Math.sin(time * 0.5) * 0.5,
    Math.sin(time * 0.7) * 0.3,
    -5 + Math.sin(time * 0.4) * 0.5
  );

  scene.objects[1].center = new Vec3(
    -2 + Math.sin(time * 0.6) * 0.3,
    -0.5 + Math.cos(time * 0.8) * 0.2,
    -4 + Math.cos(time * 0.5) * 0.4
  );

  // Animate light
  scene.light = new Vec3(
    -2 + Math.sin(time * 0.4) * 2,
    3 + Math.cos(time * 0.3) * 1,
    -1 + Math.sin(time * 0.6) * 1
  );

  const { screen } = api;
  const width = screen.width;
  const height = screen.height;
  const pixels = screen.pixels;

  // Render at low resolution for CHUNKY PIXELS
  const renderWidth = Math.floor(width * RENDER_SCALE);
  const renderHeight = Math.floor(height * RENDER_SCALE);
  
  if (!renderBuffer || renderBuffer.length !== renderWidth * renderHeight) {
    renderBuffer = new Array(renderWidth * renderHeight);
  }

  // Ray trace at super low resolution (approx 200x200 for 1024x1024)
  for (let y = 0; y < renderHeight; y++) {
    for (let x = 0; x < renderWidth; x++) {
      const ray = getRay(x, y, renderWidth, renderHeight);
      const color = traceRay(ray, scene.objects);
      renderBuffer[y * renderWidth + x] = color;
    }
  }

  // Upscale to full resolution with nearest-neighbor (CHUNKY PIXELS!)
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const u = x / width;
      const v = y / height;
      const color = nearestSample(renderBuffer, u, v, renderWidth, renderHeight);

      // Gamma correction - keep those colors crisp!
      const r = Math.min(255, Math.pow(Math.max(0, color.x), 0.45) * 255);
      const g = Math.min(255, Math.pow(Math.max(0, color.y), 0.45) * 255);
      const b = Math.min(255, Math.pow(Math.max(0, color.z), 0.45) * 255);

      const index = (y * width + x) * 4;
      pixels[index] = r;
      pixels[index + 1] = g;
      pixels[index + 2] = b;
      pixels[index + 3] = 255;
    }
  }
}

export { paint };