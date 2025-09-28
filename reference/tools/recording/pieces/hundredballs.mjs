// Full Resolution Ray Tracer - 100 balls at native screen resolution
// No downsampling - pure pixel-perfect ray tracing!

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
  constructor(center, radius, color, reflectivity = 0.2, velocity = new Vec3()) {
    this.center = center;
    this.radius = radius;
    this.color = color;
    this.reflectivity = reflectivity;
    this.velocity = velocity;
    this.initialCenter = center;
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

  update(time, bounds) {
    // Physics simulation with bouncing
    this.center = this.initialCenter.add(new Vec3(
      Math.sin(time * this.velocity.x) * bounds.x,
      Math.sin(time * this.velocity.y) * bounds.y,
      Math.sin(time * this.velocity.z) * bounds.z
    ));
  }
}

// Scene setup
let scene = null;
let camera = null;

function initScene() {
  const balls = [];
  
  // Create 100 random balls with different properties
  for (let i = 0; i < 100; i++) {
    const hue = (i / 100) * 360; // Rainbow distribution
    const sat = 0.7 + Math.random() * 0.3;
    const brightness = 0.6 + Math.random() * 0.4;
    
    // HSV to RGB conversion
    const c = brightness * sat;
    const x = c * (1 - Math.abs(((hue / 60) % 2) - 1));
    const m = brightness - c;
    let r, g, b;
    
    if (hue < 60) { r = c; g = x; b = 0; }
    else if (hue < 120) { r = x; g = c; b = 0; }
    else if (hue < 180) { r = 0; g = c; b = x; }
    else if (hue < 240) { r = 0; g = x; b = c; }
    else if (hue < 300) { r = x; g = 0; b = c; }
    else { r = c; g = 0; b = x; }
    
    const color = new Vec3(r + m, g + m, b + m);
    
    // Random position in 3D space
    const center = new Vec3(
      (Math.random() - 0.5) * 8,
      (Math.random() - 0.5) * 6,
      -3 - Math.random() * 8
    );
    
    // Random size
    const radius = 0.1 + Math.random() * 0.4;
    
    // Random velocity for animation
    const velocity = new Vec3(
      0.2 + Math.random() * 0.8,
      0.3 + Math.random() * 0.7,
      0.1 + Math.random() * 0.5
    );
    
    // Random reflectivity
    const reflectivity = Math.random() * 0.6;
    
    balls.push(new Sphere(center, radius, color, reflectivity, velocity));
  }

  scene = {
    objects: balls,
    light: new Vec3(-3, 4, -1),
    ambientLight: new Vec3(0.15, 0.15, 0.2)
  };

  camera = { position: new Vec3(0, 0, 0), fov: 70 };
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
  if (depth > 2) return new Vec3(0.05, 0.1, 0.2); // Dark sky

  const hit = findClosestIntersection(ray, objects);
  if (!hit) {
    // Gradient sky
    const t = 0.5 * (ray.direction.y + 1);
    return new Vec3(0.1, 0.2, 0.4).mul(1 - t).add(new Vec3(0.7, 0.8, 1.0).mul(t * 0.3));
  }

  // Simple lighting
  const lightDir = scene.light.sub(hit.point).normalize();
  const lightIntensity = Math.max(0, hit.normal.dot(lightDir));
  
  // Basic shadow check (simplified for speed with 100 balls)
  const shadowRay = new Ray(hit.point.add(hit.normal.mul(0.001)), lightDir);
  const shadowHit = findClosestIntersection(shadowRay, objects);
  const inShadow = shadowHit && shadowHit.t < scene.light.sub(hit.point).length();
  
  let color = hit.object.color.mul(lightIntensity * (inShadow ? 0.4 : 1.0))
    .add(scene.ambientLight);

  // Reflection
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

let time = 0;

function paint({ api, frameIndex, frameTime, simCount }) {
  if (!scene) initScene();
  
  time = frameTime * 0.001;
  
  // Animate camera in a smooth orbit
  const radius = 2;
  camera.position = new Vec3(
    Math.sin(time * 0.2) * radius,
    Math.sin(time * 0.1) * 0.8,
    Math.cos(time * 0.2) * radius
  );

  // Update all 100 balls
  const bounds = new Vec3(1.5, 1.2, 1.0);
  for (const ball of scene.objects) {
    ball.update(time, bounds);
  }

  // Animate light
  scene.light = new Vec3(
    -3 + Math.sin(time * 0.3) * 2,
    4 + Math.cos(time * 0.25) * 1.5,
    -1 + Math.sin(time * 0.4) * 2
  );

  const { screen } = api;
  const width = screen.width;
  const height = screen.height;
  const pixels = screen.pixels;

  // Full resolution ray tracing - no downsampling!
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const ray = getRay(x, y, width, height);
      const color = traceRay(ray, scene.objects);

      // Gamma correction
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