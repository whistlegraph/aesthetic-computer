// The builder inserts the KidLisp plan's rayKernel before this host renderer.
struct Controls { width: u32, height: u32, bounces: u32, unused: u32, greenX: f32, pad0: f32, pad1: f32, pad2: f32 }
@group(0) @binding(0) var<uniform> controls: Controls;
@group(0) @binding(1) var outputImage: texture_storage_2d<rgba8unorm, write>;
struct Hit { found: bool, position: vec3f, normal: vec3f, color: vec3f, mirror: f32 }
fn intersect(origin: vec3f, direction: vec3f, limit: f32) -> Hit {
  let centers = array<vec3f, 3>(vec3f(-1.12, -0.12, 2.3), vec3f(1.0, -0.35, 1.8), vec3f(controls.greenX, 0.05, 4.15));
  let radii = array<f32, 3>(0.88, 0.65, 1.05);
  let colors = array<vec3f, 3>(vec3f(0.78, 0.075, 0.055), vec3f(0.48, 0.56, 0.66), vec3f(0.045, 0.48, 0.31));
  let mirrors = array<f32, 3>(0.30, 0.82, 0.20);
  var nearest = limit;
  var hit: Hit;
  for (var i = 0u; i < 3u; i++) {
    let r = origin - centers[i];
    let discriminant = rayKernel(r.x, r.y, r.z, direction.x, direction.y, direction.z, radii[i]);
    if (discriminant < 0.0) { continue; }
    let b = dot(r, direction);
    let root = sqrt(discriminant);
    var t = -b - root;
    if (t < 0.0001) { t = -b + root; }
    if (t > 0.0001 && t < nearest) {
      nearest = t;
      let position = origin + direction * t;
      hit = Hit(true, position, (position - centers[i]) / radii[i], colors[i], mirrors[i]);
    }
  }
  if (abs(direction.y) > 1e-9) {
    let t = (-1.0 - origin.y) / direction.y;
    if (t > 0.0001 && t < nearest) {
      let position = origin + direction * t;
      let parity = floor(position.x) + floor(position.z);
      let checker = parity - 2.0 * floor(parity / 2.0);
      hit = Hit(true, position, vec3f(0.0, 1.0, 0.0), select(vec3f(0.09, 0.105, 0.13), vec3f(0.36, 0.34, 0.30), checker != 0.0), 0.18);
    }
  }
  return hit;
}
fn sky(direction: vec3f) -> vec3f {
  let t = clamp(0.5 + direction.y * 0.5, 0.0, 1.0);
  return vec3f(0.12, 0.17, 0.25) + t * vec3f(0.37, 0.49, 0.65);
}
fn trace(initialDirection: vec3f) -> vec3f {
  var origin = vec3f(0.0, 0.45, -4.2);
  var direction = initialDirection;
  var localColors: array<vec3f, 3>;
  var mirrors: array<f32, 3>;
  var depth = 0u;
  var color: vec3f;
  // At most three reflections plus the terminal ray. Reverse blending keeps
  // the reference evaluator's nesting rather than reassociating the sum.
  for (var bounce = 0u; bounce < 4u; bounce++) {
    let hit = intersect(origin, direction, 1e30);
    if (!hit.found) { color = sky(direction); break; }
    let toLight = vec3f(-3.5, 5.5, -1.5) - hit.position;
    let lightDistance = length(toLight);
    let lightDirection = toLight / lightDistance;
    let offset = hit.position + hit.normal * 0.001;
    let shadowed = intersect(offset, lightDirection, lightDistance).found;
    let diffuse = max(0.0, dot(hit.normal, lightDirection)) * select(0.95, 0.08, shadowed);
    let halfDirection = normalize(lightDirection - direction);
    let specular = select(pow(max(0.0, dot(hit.normal, halfDirection)), 80.0) * 0.8, 0.0, shadowed);
    color = hit.color * (0.16 + diffuse) + vec3f(specular);
    if (bounce == controls.bounces) { break; }
    localColors[depth] = color;
    mirrors[depth] = hit.mirror;
    depth++;
    direction = normalize(direction - 2.0 * dot(direction, hit.normal) * hit.normal);
    origin = offset;
  }
  for (var i = depth; i > 0u; i--) { color = localColors[i - 1u] * (1.0 - mirrors[i - 1u]) + color * mirrors[i - 1u]; }
  return color;
}
@compute @workgroup_size(8, 8)
fn main(@builtin(global_invocation_id) id: vec3u) {
  if (id.x >= controls.width || id.y >= controls.height) { return; }
  let w = f32(controls.width); let h = f32(controls.height);
  let direction = normalize(vec3f(((f32(id.x) + 0.5) / w * 2.0 - 1.0) * w / h * 0.52, (1.0 - (f32(id.y) + 0.5) / h * 2.0) * 0.52 - 0.09, 1.0));
  let color = sqrt(clamp(trace(direction), vec3f(0.0), vec3f(1.0)));
  textureStore(outputImage, id.xy, vec4f(floor(color * 255.0 + 0.5) / 255.0, 1.0));
}
