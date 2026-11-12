// scene-api-4-demo — a 1 km streamed landscape on sceneApi 4 (GPU meshes,
// native-bios/SCENE-API-4.md). Every new call runs here: chunked terrain
// uploaded and freed as the camera flies, a lightmapped plinth, a textured
// cube, 2,000 instanced trees, a skinned tentacle, 32 point lights, a sky
// dome, fog, and a HUD on the old triangle list over the top of it.
// Pad: left stick steers, right trigger runs. Untouched, it flies itself.

const CELL = 2, CH = 32, CHUNKS = 16, WORLD = CELL * CH * CHUNKS;  // 1024 m square
const RADIUS = 5;                     // chunks kept around the camera
const SPAN = 700;                     // view metres across the depth range
let W = 1920, H = 1080, ready = false, why = "";
let t = 0, camX = 0, camZ = 0, heading = 0, flying = true, frames = 0;
const cam = new Float32Array(27), skyCam = new Float32Array(27);
const chunks = new Map();             // "i,j" → mesh handle
let grass = 0, checker = 0, glow = 0, pixel = 0;
let mGround = 0, mCube = 0, mPlinth = 0, mTree = 0, mSky = 0, mTentacle = 0, mLamp = 0;
let cube = 0, plinth = 0, tree = 0, sky = 0, tentacle = 0, lamp = 0;
let trees = null, lamps = null, lampInstances = null, bones = null, checks = [];
const model = new Float32Array(12);

// ── the land ─────────────────────────────────────────────────────────────
function height(x, z) {
  return Math.sin(x * 0.013) * 9 + Math.cos(z * 0.011) * 11 + Math.sin((x + z) * 0.041) * 2.5 +
    Math.cos(x * 0.07 - z * 0.05) * 0.8;
}
const verts = (CH + 1) * (CH + 1);
const P = new Float32Array(verts * 3), C = new Uint8Array(verts * 4), UV = new Float32Array(verts * 2);
const IDX = new Uint16Array(CH * CH * 6);
for (let j = 0, k = 0; j < CH; j++) for (let i = 0; i < CH; i++) {
  const a = j * (CH + 1) + i, b = a + 1, c = a + CH + 1, d = c + 1;
  IDX.set([a, c, b, b, c, d], k); k += 6;
}
function uploadChunk(ci, cj) {
  const x0 = -WORLD / 2 + ci * CH * CELL, z0 = -WORLD / 2 + cj * CH * CELL;
  for (let j = 0, v = 0; j <= CH; j++) for (let i = 0; i <= CH; i++, v++) {
    const x = x0 + i * CELL, z = z0 + j * CELL, y = height(x, z);
    P[v * 3] = x; P[v * 3 + 1] = y; P[v * 3 + 2] = z;
    const g = Math.max(0, Math.min(1, (y + 12) / 26));
    C[v * 4] = 70 + g * 90; C[v * 4 + 1] = 120 + g * 70; C[v * 4 + 2] = 60 + g * 40; C[v * 4 + 3] = 255;
    UV[v * 2] = x / 8; UV[v * 2 + 1] = z / 8;
  }
  // no normals: the host smooths them from the faces
  return gpuMesh({ positions: P, indices: IDX, colors: C, uvs: UV });
}
function stream() {
  const ci = Math.floor((camX + WORLD / 2) / (CH * CELL)), cj = Math.floor((camZ + WORLD / 2) / (CH * CELL));
  for (const [key, handle] of chunks) {
    const [i, j] = key.split(",").map(Number);
    if (Math.abs(i - ci) > RADIUS + 1 || Math.abs(j - cj) > RADIUS + 1) { gpuMeshFree(handle); chunks.delete(key); }
  }
  let budget = 2;                     // uploads a frame, so a turn never hitches
  for (let r = 0; r <= RADIUS && budget; r++)
    for (let j = cj - r; j <= cj + r && budget; j++) for (let i = ci - r; i <= ci + r && budget; i++) {
      if (i < 0 || j < 0 || i >= CHUNKS || j >= CHUNKS || chunks.has(i + "," + j)) continue;
      const h = uploadChunk(i, j);
      if (h > 0) chunks.set(i + "," + j, h);
      budget--;
    }
}

// ── small meshes ─────────────────────────────────────────────────────────
function texture(size, fn, flags) {
  const px = new Uint8Array(size * size * 4);
  for (let y = 0; y < size; y++) for (let x = 0; x < size; x++) px.set(fn(x, y), (y * size + x) * 4);
  return gpuTexture(size, size, px, flags);
}
function box(sx, sy, sz, lightmapped) {   // 24 vertices, hard edges
  const p = [], n = [], uv = [], uv2 = [], idx = [];
  const faces = [[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1]];
  for (const f of faces) {
    const u = f[1] ? [1, 0, 0] : [0, 1, 0], w = [f[1] * u[2] - f[2] * u[1], f[2] * u[0] - f[0] * u[2], f[0] * u[1] - f[1] * u[0]];
    const base = p.length / 3;
    for (const [a, b] of [[-1, -1], [1, -1], [1, 1], [-1, 1]]) {
      p.push((f[0] + u[0] * a + w[0] * b) * sx, (f[1] + u[1] * a + w[1] * b) * sy, (f[2] + u[2] * a + w[2] * b) * sz);
      n.push(...f); uv.push((a + 1) / 2, (b + 1) / 2); uv2.push((a + 1) / 2, (b + 1) / 2);
    }
    idx.push(base, base + 1, base + 2, base, base + 2, base + 3);
  }
  const desc = { positions: new Float32Array(p), normals: new Float32Array(n), uvs: new Float32Array(uv), indices: new Uint16Array(idx) };
  if (lightmapped) desc.uvs2 = new Float32Array(uv2);
  return gpuMesh(desc);
}
function treeMesh() {                // a trunk and a cone, coloured per vertex
  const p = [], c = [], idx = [], S = 7;
  const ring = (y, r, rgb) => { const b = p.length / 3; for (let k = 0; k < S; k++) { const a = k / S * Math.PI * 2; p.push(Math.cos(a) * r, y, Math.sin(a) * r); c.push(...rgb, 255); } return b; };
  const tip = (y, rgb) => { p.push(0, y, 0); c.push(...rgb, 255); return p.length / 3 - 1; };
  const t0 = ring(0, 0.25, [90, 60, 40]), t1 = ring(2, 0.2, [90, 60, 40]);
  for (let k = 0; k < S; k++) { const a = k, b = (k + 1) % S; idx.push(t0 + a, t1 + a, t0 + b, t0 + b, t1 + a, t1 + b); }
  const c0 = ring(1.5, 2, [40, 110, 50]), top = tip(7, [120, 200, 110]);
  for (let k = 0; k < S; k++) idx.push(c0 + k, top, c0 + (k + 1) % S);
  return gpuMesh({ positions: new Float32Array(p), colors: new Uint8Array(c), indices: new Uint16Array(idx) });
}
function skyMesh() {                 // an inside-out dome, gradient baked in
  const p = [], c = [], idx = [], S = 24, R = 12;
  for (let r = 0; r <= R; r++) for (let s = 0; s <= S; s++) {
    const el = (r / R) * Math.PI / 2 - 0.15, az = s / S * Math.PI * 2;
    p.push(Math.cos(el) * Math.cos(az) * 100, Math.sin(el) * 100, Math.cos(el) * Math.sin(az) * 100);
    const k = r / R; c.push(200 - k * 150, 170 - k * 100, 190 - k * 40, 255);
  }
  for (let r = 0; r < R; r++) for (let s = 0; s < S; s++) {
    const a = r * (S + 1) + s, b = a + 1, d = a + S + 1, e = d + 1;
    idx.push(a, b, d, b, e, d);
  }
  return gpuMesh({ positions: new Float32Array(p), colors: new Uint8Array(c), indices: new Uint16Array(idx) });
}
function tentacleMesh() {            // a tube 4 m tall over four bones
  const p = [], j = [], w = [], idx = [], S = 10, R = 16;
  for (let r = 0; r <= R; r++) {
    const y = r / R * 4, bone = Math.min(2, Math.floor(y)), f = Math.min(1, y - bone);
    for (let s = 0; s < S; s++) {
      const a = s / S * Math.PI * 2, rad = 0.45 * (1 - y / 5);
      p.push(Math.cos(a) * rad, y, Math.sin(a) * rad);
      j.push(bone, bone + 1, 0, 0); w.push(1 - f, f, 0, 0);
    }
  }
  for (let r = 0; r < R; r++) for (let s = 0; s < S; s++) {
    const a = r * S + s, b = r * S + (s + 1) % S;
    idx.push(a, a + S, b, b, a + S, b + S);
  }
  return gpuMesh({ positions: new Float32Array(p), joints: new Uint8Array(j), weights: new Float32Array(w),
    indices: new Uint32Array(idx), bounds: new Float32Array([-5, -1, -5, 5, 6, 5]) });
}
function expectThrow(name, fn) { try { fn(); checks.push(name + ":silent"); } catch (e) { checks.push(name + ":ok"); } }

function boot() {
  if (typeof gpuDraw !== "function") { why = "no sceneApi 4 on this host"; return; }
  grass = texture(64, (x, y) => { const n = ((x * 7 + y * 13) ^ (x * y)) & 31; return [200 + n, 210 + n, 190 + n, 255]; }, 1);
  checker = texture(64, (x, y) => ((x >> 3) + (y >> 3)) & 1 ? [235, 225, 200, 255] : [150, 60, 60, 255], 1);
  glow = texture(32, (x, y) => { const d = Math.hypot(x - 15.5, y - 15.5) / 16; const v = 255 * Math.max(0.15, 1 - d); return [v, v * 0.9, v * 0.6, 255]; }, 2);
  pixel = texture(8, (x, y) => (x === 0 || y === 0 || x === 7 || y === 7) ? [255, 255, 255, 255] : [255, 120, 40, 255], 4);
  mGround = gpuMaterial({ texture: grass });
  mCube = gpuMaterial({ texture: checker, cull: "back" });
  mPlinth = gpuMaterial({ texture: pixel, lightmap: glow, lightmapScale: 1.6 });
  mTree = gpuMaterial({ cull: "none" });
  mSky = gpuMaterial({ unlit: true, fog: false, depthWrite: false });
  mTentacle = gpuMaterial({ color: [0.7, 0.3, 0.9, 1], emissive: [0.05, 0, 0.1] });
  mLamp = gpuMaterial({ unlit: true, emissive: [0.2, 0.2, 0.2] });
  cube = box(3, 3, 3, false); plinth = box(6, 0.6, 6, true); lamp = box(0.3, 0.3, 0.3, false);
  tree = treeMesh(); sky = skyMesh(); tentacle = tentacleMesh();
  // 2,000 trees, each 12 floats of placement and an rgba tint.
  const N = 2000; trees = new Float32Array(N * 16);
  let seed = 7; const rnd = () => (seed = (seed * 16807) % 2147483647) / 2147483647;
  for (let i = 0; i < N; i++) {
    const x = (rnd() - 0.5) * WORLD * 0.95, z = (rnd() - 0.5) * WORLD * 0.95, s = 0.7 + rnd() * 0.8, a = rnd() * 6.28;
    trees.set([Math.cos(a) * s, 0, Math.sin(a) * s, 0, s, 0, -Math.sin(a) * s, 0, Math.cos(a) * s, x, height(x, z) - 0.2, z,
      0.8 + rnd() * 0.3, 0.85 + rnd() * 0.3, 0.8 + rnd() * 0.2, 1], i * 16);
  }
  lamps = new Float32Array(32 * 8); lampInstances = new Float32Array(32 * 16); bones = new Float32Array(4 * 12);
  gpuEnvironment(new Float32Array([0.32, 0.3, 0.42, 0.85, 0.75, 0.6, 0.62, 0.56, 0.66, 120, SPAN - 40, 1, SPAN]));
  // The contract's refusals, recorded rather than fatal.
  expectThrow("indices", () => gpuMesh({ positions: new Float32Array(9), indices: new Uint16Array([0, 1, 5]) }));
  expectThrow("pow2", () => gpuTexture(3, 3, new Uint8Array(36)));
  expectThrow("cull", () => gpuMaterial({ cull: "sideways" }));
  expectThrow("camera", () => gpuDraw(cube, mCube, new Float32Array(20)));
  expectThrow("bones", () => gpuDraw(tentacle, mTentacle, cam));
  const spare = box(1, 1, 1, false), spareTex = texture(4, () => [0, 0, 0, 255], 0), spareMat = gpuMaterial();
  checks.push("free:" + (gpuMeshFree(spare) && gpuTextureFree(spareTex) && gpuMaterialFree(spareMat) && !gpuMeshFree(spare)));
  ready = cube > 0 && plinth > 0 && tree > 0 && sky > 0 && tentacle > 0;
  if (!ready) why = "an upload was refused";
  telemetry("SCENE4_BOOT", JSON.stringify({ ready, limits: gpuLimits, checks, sceneApi: capabilities().sceneApi }));
}

function sim() {
  t += 1 / 60;
  const pad = typeof gamepad === "function" ? gamepad() : null;
  const steer = pad ? pad.leftX || 0 : 0, run = pad ? pad.rightTrigger || 0 : 0;
  if (Math.abs(steer) > 0.2 || run > 0.1) flying = false;
  heading += flying ? 0.0025 : steer * 0.03;
  const speed = flying ? 9 : 4 + run * 30;
  camX += Math.sin(heading) * speed / 60; camZ -= Math.cos(heading) * speed / 60;
  const edge = WORLD / 2 - 60;
  if (Math.abs(camX) > edge || Math.abs(camZ) > edge) heading += 0.02;
}

function setCamera(m, x, y, z, fx, fy, fz, sky) {
  const l = Math.hypot(fx, fy, fz); fx /= l; fy /= l; fz /= l;
  let rx = -fz, rz = fx; const rl = Math.hypot(rx, rz); rx /= rl; rz /= rl;
  const ux = -rz * fy, uy = rz * fx - rx * fz, uz = rx * fy;
  m.set([x, y, z, rx, 0, rz, ux, uy, uz, fx, fy, fz, W / 2, H / 2, 1, H * 0.9, 1, 0.1, 0, 0, W, H,
    sky ? 1.395 : -1.4, sky ? 0 : 2.8 / SPAN, -0.4, -0.8, -0.45]);
}

function columns(out, x, y, z, yaw, s = 1) {
  const c = Math.cos(yaw) * s, n = Math.sin(yaw) * s;
  out.set([c, 0, -n, 0, s, 0, n, 0, c, x, y, z]);
  return out;
}

function hud(lines) {
  const z = -1.47, x = 24, y = 24, w = 820, h = 34 + lines.length * 30;
  triangles3d(new Float32Array([x, y, z, x + w, y, z, x + w, y + h, z, 10, 12, 24, x, y, z, x + w, y + h, z, x, y + h, z, 10, 12, 24]), 2);
  lines.forEach((line, i) => systemWrite(line, x + 18, y + 14 + i * 30, 24, 230, 236, 250));
}

function paint() {
  const caps = capabilities();
  W = caps.width || 1920; H = caps.height || 1080;
  wipe(150, 140, 170);
  if (!ready) { hud(["sceneApi 4 demo", why || "booting"]); return; }
  frames++;
  stream();
  const camY = height(camX, camZ) + 14 + Math.sin(t * 0.3) * 4;
  const fx = Math.sin(heading), fz = -Math.cos(heading);
  setCamera(cam, camX, camY, camZ, fx, -0.28, fz, false);
  setCamera(skyCam, 0, 0, 0, fx, -0.28, fz, true);
  // 32 lamps circling just ahead of the camera, a light and a glowing box each.
  const ox = camX + fx * 40, oz = camZ + fz * 40;
  for (let i = 0; i < 32; i++) {
    const a = i / 32 * Math.PI * 2 + t * 0.4, r = 18 + (i % 4) * 7;
    const x = ox + Math.cos(a) * r, z = oz + Math.sin(a) * r, y = height(x, z) + 2.5;
    const hue = i / 32 * 6.28;
    const rgb = [0.6 + 0.6 * Math.cos(hue), 0.6 + 0.6 * Math.cos(hue - 2.1), 0.6 + 0.6 * Math.cos(hue + 2.1)];
    lamps.set([x, y, z, 12, rgb[0] * 1.4, rgb[1] * 1.4, rgb[2] * 1.4, 0], i * 8);
    lampInstances.set([1, 0, 0, 0, 1, 0, 0, 0, 1, x, y, z, rgb[0], rgb[1], rgb[2], 1], i * 16);
  }
  gpuLights(lamps, 32);
  let tris = gpuDraw(sky, mSky, skyCam);
  for (const handle of chunks.values()) tris += gpuDraw(handle, mGround, cam);
  tris += gpuDrawInstances(tree, mTree, cam, trees, trees.length / 16);
  tris += gpuDrawInstances(lamp, mLamp, cam, lampInstances, 32);
  const px = ox + fx * 20, pz = oz + fz * 20, py = height(px, pz);
  tris += gpuDraw(plinth, mPlinth, cam, columns(model, px, py + 0.6, pz, 0));
  tris += gpuDraw(cube, mCube, cam, columns(model, px, py + 4.2 + Math.sin(t) * 0.5, pz, t * 0.5));
  // The tentacle: four bones bending a little more each joint.
  const bend = Math.sin(t * 1.7) * 0.45;
  let jx = 0, jy = 0;
  for (let k = 0; k < 4; k++) {
    const a = bend * k, c = Math.cos(a), s = Math.sin(a);
    bones.set([c, s, 0, -s, c, 0, 0, 0, 1, jx + s * k, jy - c * k, 0], k * 12);
    jx += -Math.sin(a); jy += Math.cos(a);
  }
  tris += gpuDraw(tentacle, mTentacle, cam, columns(model, px + 9, py, pz, t * 0.2, 1.5), bones);
  const s = gpuStats();
  hud([`sceneApi 4 gpu meshes · host level ${caps.sceneApi || "n/a"}`,
    `chunks ${chunks.size} · meshes ${s.meshes} · ${(s.meshBytes / 1048576).toFixed(1)} MB`,
    `draws ${s.draws} · culled ${s.culled} · triangles ${s.triangles}`,
    `queued now ${tris} · textures ${s.textures}`, `checks ${checks.join(" ")}`]);
  if (frames % 120 === 1) telemetry("SCENE4", JSON.stringify({ frames, stats: s, tris, cam: [camX | 0, camZ | 0] }));
}

function act() {}
function leave() {}
