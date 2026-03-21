// Ray Tracer - Real-time ray tracing with pixel manipulation
// Renders spheres, planes, lighting, shadows, and reflections

// Vector3 operations
class Vec3 {
  constructor(x = 0, y = 0, z = 0) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  add(v) {
    return new Vec3(this.x + v.x, this.y + v.y, this.z + v.z);
  }

  sub(v) {
    return new Vec3(this.x - v.x, this.y - v.y, this.z - v.z);
  }

  mul(s) {
    return new Vec3(this.x * s, this.y * s, this.z * s);
  }

  dot(v) {
    return this.x * v.x + this.y * v.y + this.z * v.z;
  }

  length() {
    return Math.sqrt(this.dot(this));
  }

  normalize() {
    const len = this.length();
    return len > 0 ? this.mul(1 / len) : new Vec3();
  }

  reflect(normal) {
    return this.sub(normal.mul(2 * this.dot(normal)));
  }
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

// Sphere object
class Sphere {
  constructor(center, radius, color, material = { reflectivity: 0, shininess: 50 }) {
    this.center = center;
    this.radius = radius;
    this.color = color;
    this.material = material;
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

// Plane object
class Plane {
  constructor(point, normal, color, material = { reflectivity: 0.3, shininess: 10 }) {
    this.point = point;
    this.normal = normal.normalize();
    this.color = color;
    this.material = material;
  }

  intersect(ray) {
    const denom = this.normal.dot(ray.direction);
    if (Math.abs(denom) < 0.001) return null;

    const t = this.point.sub(ray.origin).dot(this.normal) / denom;
    if (t < 0.001) return null;

    const point = ray.at(t);
    return { t, point, normal: this.normal, object: this };
  }
}

// Light source
class Light {
  constructor(position, color, intensity = 1) {
    this.position = position;
    this.color = color;
    this.intensity = intensity;
  }
}

// Scene setup
let scene = null;
let camera = null;

function initScene() {
  scene = {
    objects: [
      // Colorful spheres
      new Sphere(new Vec3(0, 0, -5), 1, new Vec3(1, 0.2, 0.2), { reflectivity: 0.3, shininess: 100 }),
      new Sphere(new Vec3(-2.5, -0.5, -4), 0.8, new Vec3(0.2, 1, 0.2), { reflectivity: 0.4, shininess: 80 }),
      new Sphere(new Vec3(2.2, -0.3, -6), 1.2, new Vec3(0.2, 0.2, 1), { reflectivity: 0.5, shininess: 120 }),
      new Sphere(new Vec3(0, 2, -7), 0.6, new Vec3(1, 1, 0.2), { reflectivity: 0.6, shininess: 150 }),

      // Reflective floor plane
      new Plane(new Vec3(0, -2, 0), new Vec3(0, 1, 0), new Vec3(0.7, 0.7, 0.7), { reflectivity: 0.4, shininess: 20 })
    ],
    lights: [
      new Light(new Vec3(-3, 4, -2), new Vec3(1, 1, 1), 0.8),
      new Light(new Vec3(3, 2, -1), new Vec3(0.8, 0.9, 1), 0.6),
      new Light(new Vec3(0, 6, -4), new Vec3(1, 0.9, 0.8), 0.4)
    ],
    ambientLight: new Vec3(0.1, 0.1, 0.15)
  };

  camera = {
    position: new Vec3(0, 0, 0),
    fov: 60,
    aspect: 1
  };
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

function isInShadow(point, light, objects) {
  const lightDir = light.position.sub(point).normalize();
  const shadowRay = new Ray(point.add(lightDir.mul(0.001)), lightDir);
  const lightDistance = light.position.sub(point).length();

  const hit = findClosestIntersection(shadowRay, objects);
  return hit && hit.t < lightDistance;
}

function calculateLighting(hit, ray, lights, objects) {
  let color = scene.ambientLight.mul(0.2);

  for (const light of lights) {
    if (isInShadow(hit.point, light, objects)) continue;

    const lightDir = light.position.sub(hit.point).normalize();
    const distance = light.position.sub(hit.point).length();
    const attenuation = 1 / (1 + 0.1 * distance + 0.01 * distance * distance);

    // Diffuse lighting
    const diffuse = Math.max(0, hit.normal.dot(lightDir));
    const diffuseColor = light.color.mul(diffuse * light.intensity * attenuation);

    // Specular lighting
    const viewDir = ray.direction.mul(-1);
    const reflectDir = lightDir.mul(-1).reflect(hit.normal);
    const specular = Math.pow(Math.max(0, viewDir.dot(reflectDir)), hit.object.material.shininess);
    const specularColor = light.color.mul(specular * light.intensity * attenuation * 0.5);

    color = color.add(diffuseColor.add(specularColor));
  }

  return color;
}

function traceRay(ray, objects, lights, depth = 0) {
  if (depth > 3) return new Vec3(0, 0, 0);

  const hit = findClosestIntersection(ray, objects);
  if (!hit) {
    // Sky gradient
    const t = 0.5 * (ray.direction.y + 1);
    return new Vec3(0.5, 0.7, 1.0).mul(1 - t).add(new Vec3(1, 1, 1).mul(t)).mul(0.3);
  }

  // Base color with lighting
  let color = hit.object.color;
  const lighting = calculateLighting(hit, ray, lights, objects);
  color = new Vec3(
    color.x * lighting.x,
    color.y * lighting.y,
    color.z * lighting.z
  );

  // Reflection
  if (hit.object.material.reflectivity > 0) {
    const reflectDir = ray.direction.reflect(hit.normal);
    const reflectRay = new Ray(hit.point.add(hit.normal.mul(0.001)), reflectDir);
    const reflectColor = traceRay(reflectRay, objects, lights, depth + 1);
    color = color.mul(1 - hit.object.material.reflectivity).add(
      reflectColor.mul(hit.object.material.reflectivity)
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

// Animation parameters
let time = 0;

function paint({ api, frameIndex, frameTime, simCount }) {
  if (!scene) initScene();
  
  time = frameTime * 0.001; // Convert to seconds
  
  // Animate camera rotation
  const radius = 1;
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
    -2.5 + Math.sin(time * 0.6) * 0.3,
    -0.5 + Math.cos(time * 0.8) * 0.2,
    -4 + Math.cos(time * 0.5) * 0.4
  );

  scene.objects[3].center = new Vec3(
    Math.cos(time * 0.4) * 0.8,
    2 + Math.sin(time * 0.9) * 0.3,
    -7 + Math.sin(time * 0.3) * 0.6
  );

  // Animate lights
  scene.lights[0].position = new Vec3(
    -3 + Math.sin(time * 0.4) * 1.5,
    4 + Math.cos(time * 0.3) * 1,
    -2 + Math.sin(time * 0.6) * 1
  );

  const { screen } = api;
  const width = screen.width;
  const height = screen.height;
  const pixels = screen.pixels;

  // Ray trace each pixel
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const ray = getRay(x, y, width, height);
      const color = traceRay(ray, scene.objects, scene.lights);

      // Gamma correction and tone mapping
      const gamma = 1.0 / 2.2;
      const r = Math.min(255, Math.pow(Math.max(0, color.x), gamma) * 255);
      const g = Math.min(255, Math.pow(Math.max(0, color.y), gamma) * 255);
      const b = Math.min(255, Math.pow(Math.max(0, color.z), gamma) * 255);

      const index = (y * width + x) * 4;
      pixels[index] = r;     // Red
      pixels[index + 1] = g; // Green
      pixels[index + 2] = b; // Blue
      pixels[index + 3] = 255; // Alpha
    }
  }
}

// Export the functions that AC expects
export { paint };