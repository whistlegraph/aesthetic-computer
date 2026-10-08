// Experimental benchmark host. KidLisp owns the ray/sphere numeric kernel;
// JS currently owns scene traversal, shading, reflections, and the pixel loop.
import { KidLisp } from "../../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution } from "../../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { compileNumeric, runNumeric } from "../../system/public/aesthetic.computer/lib/kidlisp-plan.mjs";
import { instantiateNumericWasm } from "../../system/public/aesthetic.computer/lib/kidlisp-plan-wasm.mjs";

export const RAY_BINDINGS = ["ox", "oy", "oz", "dx", "dy", "dz", "radius"];
export function rayKernel(source, backend = "wasm") {
  const execution = new KidLispExecution({ seed: 0, maxSteps: 100000000 });
  const lisp = new KidLisp({ execution });
  const forms = lisp.parse(source);
  if (forms.length !== 1) throw new TypeError("Ray kernel needs one numeric expression");
  const ast = forms[0];
  const plan = compileNumeric(ast, { bindings: RAY_BINDINGS });
  const api = { screen: { width: 1, height: 1 } };
  let evaluate, bytes = 0;
  if (backend === "reference") evaluate = values => {
    RAY_BINDINGS.forEach((name, i) => { lisp.localEnv[name] = values[i]; });
    return lisp.evaluate(ast, api);
  };
  else if (backend === "plan") evaluate = values => runNumeric(plan, values);
  else if (backend === "wasm") {
    const wasm = instantiateNumericWasm(plan);
    evaluate = wasm.run;
    bytes = wasm.bytes.length;
  } else if (backend === "direct-js") evaluate = ([ox, oy, oz, dx, dy, dz, radius]) => {
    const b = 0 + ox * dx + oy * dy + oz * dz;
    return b * b - ((0 + ox * ox + oy * oy + oz * oz) - radius * radius);
  };
  else throw new TypeError(`Unknown ray backend: ${backend}`);
  return { backend, bytes, plan, evaluate, beginFrame: frame => execution.beginFrame(frame) };
}

const normalize = (x, y, z) => { const length = Math.sqrt(x * x + y * y + z * z); return [x / length, y / length, z / length]; };
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
const sky = d => {
  const t = Math.max(0, Math.min(1, 0.5 + d[1] * 0.5));
  return [0.12 + t * 0.37, 0.17 + t * 0.49, 0.25 + t * 0.65];
};

export function renderRayFrame(kernel, { width = 96, height = 54, frame = 0, bounces = 2 } = {}) {
  if (![width, height].every(n => Number.isInteger(n) && n > 0 && n <= 512) || !Number.isInteger(frame) || frame < 0 || frame > 1000000 || !Number.isInteger(bounces) || bounces < 0 || bounces > 3) throw new RangeError("Invalid ray frame controls");
  kernel.beginFrame(frame);
  const rgba = new Uint8ClampedArray(width * height * 4);
  const spheres = [
    { center: [-1.12, -0.12, 2.3], radius: 0.88, color: [0.78, 0.075, 0.055], mirror: 0.30 },
    { center: [1.0, -0.35, 1.8], radius: 0.65, color: [0.48, 0.56, 0.66], mirror: 0.82 },
    { center: [0.05 + Math.sin(frame * 0.04) * 0.28, 0.05, 4.15], radius: 1.05, color: [0.045, 0.48, 0.31], mirror: 0.20 },
  ];
  const light = [-3.5, 5.5, -1.5];
  let rays = 0, intersections = 0;
  const intersect = (origin, direction, limit = Infinity) => {
    rays++;
    let nearest = limit, hit = null;
    for (const sphere of spheres) {
      const relative = origin.map((v, i) => v - sphere.center[i]);
      intersections++;
      const discriminant = kernel.evaluate([...relative, ...direction, sphere.radius]);
      if (discriminant < 0) continue;
      const b = dot(relative, direction), root = Math.sqrt(discriminant);
      let t = -b - root;
      if (t < 0.0001) t = -b + root;
      if (t > 0.0001 && t < nearest) {
        nearest = t;
        const position = origin.map((v, i) => v + direction[i] * t);
        hit = { position, normal: position.map((v, i) => (v - sphere.center[i]) / sphere.radius), ...sphere };
      }
    }
    if (Math.abs(direction[1]) > 1e-9) {
      const t = (-1 - origin[1]) / direction[1];
      if (t > 0.0001 && t < nearest) {
        const position = origin.map((v, i) => v + direction[i] * t);
        const checker = ((Math.floor(position[0]) + Math.floor(position[2])) % 2 + 2) % 2;
        hit = { position, normal: [0, 1, 0], color: checker ? [0.36, 0.34, 0.30] : [0.09, 0.105, 0.13], mirror: 0.18 };
      }
    }
    return hit;
  };
  const trace = (origin, direction, depth) => {
    const hit = intersect(origin, direction);
    if (!hit) return sky(direction);
    const toLight = light.map((v, i) => v - hit.position[i]);
    const lightDistance = Math.sqrt(dot(toLight, toLight));
    const lightDirection = toLight.map(v => v / lightDistance);
    const offset = hit.position.map((v, i) => v + hit.normal[i] * 0.001);
    const shadowed = intersect(offset, lightDirection, lightDistance) !== null;
    const diffuse = Math.max(0, dot(hit.normal, lightDirection)) * (shadowed ? 0.08 : 0.95);
    const half = normalize(lightDirection[0] - direction[0], lightDirection[1] - direction[1], lightDirection[2] - direction[2]);
    const specular = shadowed ? 0 : Math.pow(Math.max(0, dot(hit.normal, half)), 80) * 0.8;
    const local = hit.color.map(c => c * (0.16 + diffuse) + specular);
    if (depth === 0) return local;
    const incidence = 2 * dot(direction, hit.normal);
    const reflection = normalize(...direction.map((v, i) => v - incidence * hit.normal[i]));
    const reflected = trace(offset, reflection, depth - 1);
    return local.map((c, i) => c * (1 - hit.mirror) + reflected[i] * hit.mirror);
  };
  for (let y = 0; y < height; y++) for (let x = 0; x < width; x++) {
    const direction = normalize(((x + 0.5) / width * 2 - 1) * width / height * 0.52, (1 - (y + 0.5) / height * 2) * 0.52 - 0.09, 1);
    const color = trace([0, 0.45, -4.2], direction, bounces);
    const offset = (x + y * width) * 4;
    for (let c = 0; c < 3; c++) rgba[offset + c] = Math.round(Math.sqrt(Math.max(0, Math.min(1, color[c]))) * 255);
    rgba[offset + 3] = 255;
  }
  return { width, height, frame, rgba, rays, intersections };
}
