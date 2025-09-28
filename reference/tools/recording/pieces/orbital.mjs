// Simple orbital system: 1 cube + 1 sphere orbiting each other with translucency
// 2025.09.25.23.30.47

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
  cross(v) { return new Vec3(this.y * v.z - this.z * v.y, this.z * v.x - this.x * v.z, this.x * v.y - this.y * v.x); }
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
  constructor(center, radius, color, alpha = 0.7) {
    this.center = center;
    this.radius = radius;
    this.color = color;
    this.alpha = alpha; // translucency
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
    return { t, point, normal, alpha: this.alpha };
  }
}

// Cube class
class Cube {
  constructor(min, max, color, alpha = 0.7) {
    this.min = min;
    this.max = max;
    this.color = color;
    this.alpha = alpha; // translucency
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
    
    return { t, point, normal, alpha: this.alpha };
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

let balls = []; // Array of bouncing balls
let cube;
let orbitPhase, cubePhase; // Global phase variables for texture access

// Texture function for cube - cool procedural pattern with proper perspective
function getCubeTexture(point, normal, cubeCenter) {
  // Convert world point to local cube space
  const localPoint = point.sub(cubeCenter);
  
  // Get UV coordinates based on which face we're on, using proper face mapping
  let u, v, faceId;
  
  if (Math.abs(normal.x) > 0.9) {
    // X face (left/right)
    u = (localPoint.z / 0.5 + 1) * 0.5; // -0.5 to 0.5 -> 0 to 1
    v = (localPoint.y / 0.5 + 1) * 0.5;
    faceId = normal.x > 0 ? 0 : 1;
  } else if (Math.abs(normal.y) > 0.9) {
    // Y face (top/bottom)
    u = (localPoint.x / 0.5 + 1) * 0.5;
    v = (localPoint.z / 0.5 + 1) * 0.5;
    faceId = normal.y > 0 ? 2 : 3;
  } else {
    // Z face (front/back)
    u = (localPoint.x / 0.5 + 1) * 0.5;
    v = (localPoint.y / 0.5 + 1) * 0.5;
    faceId = normal.z > 0 ? 4 : 5;
  }
  
  // Clamp UV to ensure they're in 0-1 range
  u = Math.max(0, Math.min(1, u));
  v = Math.max(0, Math.min(1, v));
  
  // Scale UV for more detail and add face-specific offset
  const scale = 6;
  u = u * scale + faceId * 0.1;
  v = v * scale + faceId * 0.1;
  
  // Create cool pattern: circuit board with face-specific colors
  const gridU = Math.floor(u) % 3;
  const gridV = Math.floor(v) % 3;
  const fracU = u - Math.floor(u);
  const fracV = v - Math.floor(v);
  
  // Circuit lines with varying thickness
  const lineThickness = 0.15;
  const thinLines = 0.05;
  const isThickLine = (fracU < lineThickness || fracU > 1 - lineThickness ||
                       fracV < lineThickness || fracV > 1 - lineThickness);
  const isThinLine = !isThickLine && (
    Math.abs(fracU - 0.5) < thinLines || Math.abs(fracV - 0.5) < thinLines
  );
  
  // Gentle animated effect with face variation - much slower to avoid flickering
  const time = Date.now() * 0.001;
  const pulse = Math.sin(time * 0.5 + u * 0.2 + v * 0.2 + faceId) * 0.3 + 0.7;
  const slowPulse = Math.sin(time * 0.3 + u * 0.1 + v * 0.1) * 0.2 + 0.8;
  
  // Face-specific base colors
  const faceColors = [
    { r: 0.2, g: 0.8, b: 1.0 }, // Right face - cyan
    { r: 1.0, g: 0.3, b: 0.2 }, // Left face - red
    { r: 0.3, g: 1.0, b: 0.3 }, // Top face - green
    { r: 1.0, g: 0.8, b: 0.1 }, // Bottom face - yellow
    { r: 0.8, g: 0.2, b: 1.0 }, // Front face - magenta
    { r: 1.0, g: 0.6, b: 0.1 }  // Back face - orange
  ];
  
  const faceColor = faceColors[faceId];
  
  if (isThickLine) {
    // Bright glowing circuit lines
    return {
      r: Math.floor((faceColor.r * 150 + 100) * pulse),
      g: Math.floor((faceColor.g * 150 + 100) * pulse),
      b: Math.floor((faceColor.b * 150 + 100) * pulse)
    };
  } else if (isThinLine) {
    // Dimmer secondary lines with slow pulse
    return {
      r: Math.floor(faceColor.r * 80 * slowPulse + 40),
      g: Math.floor(faceColor.g * 80 * slowPulse + 40),
      b: Math.floor(faceColor.b * 80 * slowPulse + 40)
    };
  } else if ((gridU + gridV) % 2 === 0) {
    // Dark squares with face tint
    return { 
      r: Math.floor(faceColor.r * 30 + 10), 
      g: Math.floor(faceColor.g * 30 + 10), 
      b: Math.floor(faceColor.b * 30 + 10) 
    };
  } else {
    // Medium squares with face color variation
    const variation = Math.sin(u * 0.7) * Math.cos(v * 0.9) * 0.3 + 0.7;
    return { 
      r: Math.floor(faceColor.r * 60 * variation + 20), 
      g: Math.floor(faceColor.g * 60 * variation + 20), 
      b: Math.floor(faceColor.b * 60 * variation + 20) 
    };
  }
}

function initObjects() {
  // Create multiple bouncing balls with different colors and properties
  balls = [];
  const numBalls = 6;
  const colors = [
    { r: 100, g: 150, b: 255 }, // Blue
    { r: 255, g: 100, b: 150 }, // Pink
    { r: 150, g: 255, b: 100 }, // Green
    { r: 255, g: 200, b: 100 }, // Orange
    { r: 200, g: 100, b: 255 }, // Purple
    { r: 100, g: 255, b: 200 }  // Cyan
  ];
  
  for (let i = 0; i < numBalls; i++) {
    balls.push({
      sphere: new Sphere(
        new Vec3(
          (Math.random() - 0.5) * 4, // Random X position
          (Math.random() - 0.5) * 4, // Random Y position
          4 + Math.random() * 2      // Z between 4-6
        ),
        0.2 + Math.random() * 0.15, // Random radius 0.2-0.35
        colors[i % colors.length],
        0.7 // Alpha
      ),
      velocity: new Vec3(
        (Math.random() - 0.5) * 2, // Random X velocity
        (Math.random() - 0.5) * 2, // Random Y velocity  
        (Math.random() - 0.5) * 1  // Random Z velocity
      ),
      originalColor: colors[i % colors.length]
    });
  }
  
  // Create translucent cube in center
  cube = new Cube(
    new Vec3(-0.7, -0.7, 4.3),
    new Vec3(0.7, 0.7, 5.7),
    { r: 255, g: 100, b: 150 },
    0.7
  );
}

function updateObjects(frameTime) {
  const dt = 16.67 / 1000 * 0.2; // Much slower movement - 20% speed
  const boundsMin = new Vec3(-2.5, -2.5, 3);
  const boundsMax = new Vec3(2.5, 2.5, 7);
  
  // Update bouncing balls with physics
  balls.forEach(ball => {
    // Apply velocity (much slower)
    ball.sphere.center = ball.sphere.center.add(ball.velocity.mul(dt));
    
    // Bounce off boundaries with high energy retention
    if (ball.sphere.center.x - ball.sphere.radius < boundsMin.x || 
        ball.sphere.center.x + ball.sphere.radius > boundsMax.x) {
      ball.velocity.x *= -0.95; // Less energy loss
      ball.sphere.center.x = Math.max(boundsMin.x + ball.sphere.radius, 
                                     Math.min(boundsMax.x - ball.sphere.radius, ball.sphere.center.x));
    }
    
    if (ball.sphere.center.y - ball.sphere.radius < boundsMin.y || 
        ball.sphere.center.y + ball.sphere.radius > boundsMax.y) {
      ball.velocity.y *= -0.95;
      ball.sphere.center.y = Math.max(boundsMin.y + ball.sphere.radius, 
                                     Math.min(boundsMax.y - ball.sphere.radius, ball.sphere.center.y));
    }
    
    if (ball.sphere.center.z - ball.sphere.radius < boundsMin.z || 
        ball.sphere.center.z + ball.sphere.radius > boundsMax.z) {
      ball.velocity.z *= -0.95;
      ball.sphere.center.z = Math.max(boundsMin.z + ball.sphere.radius, 
                                     Math.min(boundsMax.z - ball.sphere.radius, ball.sphere.center.z));
    }
    
    // Much lighter gravity effect
    ball.velocity.y -= 0.1 * dt;
  });
  
  // Rotate the cube slowly
  const cubePeriodMs = 8000;
  cubePhase = (frameTime / cubePeriodMs) * 2 * Math.PI;
  
  const cubeCenter = new Vec3(0, 0, 5);
  const size = 0.7;
  cube.min = cubeCenter.sub(new Vec3(size, size, size));
  cube.max = cubeCenter.add(new Vec3(size, size, size));
}

function castRay(ray) {
  const hits = [];
  
  // Check all bouncing balls
  balls.forEach(ball => {
    const sphereHit = ball.sphere.intersect(ray);
    if (sphereHit) {
      hits.push({ ...sphereHit, object: ball.sphere });
    }
  });
  
  // Check cube
  const cubeHit = cube.intersect(ray);
  if (cubeHit) {
    hits.push({ ...cubeHit, object: cube });
  }
  
  // Sort by distance (closest first)
  hits.sort((a, b) => a.t - b.t);
  
  return hits;
}

function shade(hit, ray) {
  const light = new Vec3(1, 1, 0.5).normalize();
  const lightDot = Math.max(0, hit.normal.dot(light));
  
  // Get base color - use texture for cube, noise for balls
  let baseColor;
  if (hit.object === cube) {
    // Apply texture mapping to cube with proper cube center
    const cubeCenter = new Vec3(0, 0, 5);
    baseColor = getCubeTexture(hit.point, hit.normal, cubeCenter);
    
    // Standard shading for cube
    const ambient = 0.3;
    const diffuse = lightDot * 0.7;
    const intensity = ambient + diffuse;
    
    return {
      r: Math.round(baseColor.r * intensity),
      g: Math.round(baseColor.g * intensity),
      b: Math.round(baseColor.b * intensity),
      alpha: hit.alpha
    };
  } else {
    // Super reflective balls with noise texture
    const noiseScale = 8.0;
    const noise = (Math.sin(hit.point.x * noiseScale) * Math.sin(hit.point.y * noiseScale) * Math.sin(hit.point.z * noiseScale) + 1) / 2;
    const noiseFactor = 0.8 + noise * 0.4; // Noise between 0.8 and 1.2
    
    // High reflectivity with specular highlight
    const viewDir = ray.direction.mul(-1);
    const reflectDir = ray.direction.sub(hit.normal.mul(2 * ray.direction.dot(hit.normal)));
    const specular = Math.pow(Math.max(0, viewDir.dot(reflectDir)), 32) * 0.8;
    
    const ambient = 0.4;
    const diffuse = lightDot * 0.4;  // Less diffuse for more metallic look
    const intensity = ambient + diffuse + specular;
    
    baseColor = hit.object.color;
    
    return {
      r: Math.round(baseColor.r * intensity * noiseFactor),
      g: Math.round(baseColor.g * intensity * noiseFactor),
      b: Math.round(baseColor.b * intensity * noiseFactor),
      alpha: hit.alpha
    };
  }
}

function blendColors(front, back) {
  const alpha = front.alpha;
  const invAlpha = 1 - alpha;
  
  return {
    r: Math.round(front.r * alpha + back.r * invAlpha),
    g: Math.round(front.g * alpha + back.g * invAlpha),
    b: Math.round(front.b * alpha + back.b * invAlpha)
  };
}

function renderPixel(x, y, width, height, cameraPos, cameraTarget, cameraUp) {
  const aspect = width / height;
  const fov = Math.PI / 4;
  
  // Create camera coordinate system
  const forward = cameraTarget.sub(cameraPos).normalize();
  const right = forward.cross(cameraUp).normalize();
  const up = right.cross(forward).normalize();
  
  // Calculate ray direction in world space
  const u = (2 * x / width - 1) * aspect * Math.tan(fov / 2);
  const v = (1 - 2 * y / height) * Math.tan(fov / 2);
  
  const direction = forward.add(right.mul(u)).add(up.mul(v)).normalize();
  const ray = new Ray(cameraPos, direction);
  
  const hits = castRay(ray);
  
  if (hits.length === 0) {
    // Background gradient
    const t = y / height;
    const r = Math.round(20 + t * 30);
    const g = Math.round(30 + t * 40);
    const b = Math.round(50 + t * 60);
    return { r, g, b };
  }
  
  // Handle translucency blending
  if (hits.length === 1) {
    // Only one object hit
    const color = shade(hits[0], ray);
    const t = y / height;
    const bgR = Math.round(20 + t * 30);
    const bgG = Math.round(30 + t * 40);
    const bgB = Math.round(50 + t * 60);
    
    return blendColors(color, { r: bgR, g: bgG, b: bgB });
  } else {
    // Two objects hit - blend them
    const frontColor = shade(hits[0], ray);
    const backColor = shade(hits[1], ray);
    
    // Blend back with background first
    const t = y / height;
    const bgR = Math.round(20 + t * 30);
    const bgG = Math.round(30 + t * 40);
    const bgB = Math.round(50 + t * 60);
    
    const backBlended = blendColors(backColor, { r: bgR, g: bgG, b: bgB });
    
    // Then blend front with that result
    return blendColors(frontColor, backBlended);
  }
}

function paint({ api, frameTime }) {
  const { screen } = api;
  
  if (balls.length === 0 || !cube) {
    initObjects();
  }
  
  updateObjects(frameTime);
  
  // Calculate rotating camera position
  const cameraRadius = 3.5; // Distance from scene center
  const cameraHeight = 1.0; // Height above/below scene center
  const sceneCenter = new Vec3(0, 0, 5.0); // Where the action is happening
  
  // Camera orbits slower than objects for smoother viewing
  const cameraPeriodMs = 8000; // 8 second camera orbit
  const cameraPhase = (frameTime / cameraPeriodMs) * 2 * Math.PI;
  
  // Camera position orbiting around scene center
  const cameraPos = new Vec3(
    Math.cos(cameraPhase) * cameraRadius,
    Math.sin(cameraPhase * 0.3) * cameraHeight, // gentle vertical movement
    sceneCenter.z + Math.sin(cameraPhase) * cameraRadius
  );
  
  const cameraTarget = sceneCenter; // Always look at scene center
  const cameraUp = new Vec3(0, 1, 0); // World up vector
  
  // Render each pixel
  for (let y = 0; y < screen.height; y++) {
    for (let x = 0; x < screen.width; x++) {
      const color = renderPixel(x, y, screen.width, screen.height, cameraPos, cameraTarget, cameraUp);
      const index = (y * screen.width + x) * 4;
      
      screen.pixels[index] = color.r;
      screen.pixels[index + 1] = color.g;
      screen.pixels[index + 2] = color.b;
      screen.pixels[index + 3] = 255;
    }
  }
}

export { paint };