// fur.mjs — the pup in fur, for WebGL2 hosts. (xbox/FIAPUP.md → Fur)
//
// The flat interpreter draws the pup's balls and limbs as inked flat
// shapes. This draws the same shapes, from the same SKETCH ops, as furry
// solids in the same WebGL context and depth buffer, so the flat yard and
// the furry pup cover each other correctly.
//
// - Shell fur, alpha-tested, no blending: one instanced draw.
//   `gl_InstanceID` walks primitive × layer; layer 0 is the opaque base,
//   layers 1…N are shells at pow(i/N, 1.3) of the fur length.
// - Strands come from a hash over cells in object-space octahedral
//   coordinates, so there are no poles or seams, and they move with the part.
// - A toon ramp on half-Lambert, a root-to-tip AO gradient, tinted tips and
//   a rim sheen, and an inverted-hull ink line whose extrusion is jittered
//   by the same clump noise, so the silhouette is hairy.
// - Combing: a 256² RGBA8 atlas with one 16² octahedral tile per primitive
//   holds direction and flatness. Strokes splat into it on the CPU; it holds
//   for 0.3 s, then springs back (τ ≈ 1 s, ζ ≈ 0.6), overshooting a little,
//   which is the fur fluffing back up.
// - Ruffle and lag: running raises a per-primitive ruffle that decays over
//   ~8 s. A damped spring per part leaves the tips trailing behind motion.
//
// Primitives arrive as { kind: 1 ball | 2 limb, a, b, r, rgb, len, x, y }
// in world units; `a` = `b` for a ball, and x/y are the part's axes.

const MAX_PRIMS = 96, TILE = 16, ATLAS = 256, PER_ROW = ATLAS / TILE;
const PRIM_TEXELS = 6;

// Octahedral coordinates of a direction, and the comb texel there for a
// primitive's tile. Shared by both shaders.
const octaSource = `
vec2 octa(vec3 n) {
  n /= abs(n.x) + abs(n.y) + abs(n.z);
  return n.y >= 0.0 ? n.xz : (1.0 - abs(n.zx)) * vec2(n.x >= 0.0 ? 1.0 : -1.0, n.z >= 0.0 ? 1.0 : -1.0);
}
vec4 combAt(int tile, vec3 local) {
  vec2 o = octa(normalize(local));
  vec2 tileAt = vec2(float(tile % ${PER_ROW}), float(tile / ${PER_ROW})) * ${TILE}.0;
  return texture(comb, (tileAt + (o * .5 + .5) * ${TILE - 1}.0 + .5) / ${ATLAS}.0);
}`;

const vertexSource = `#version 300 es
precision highp float;
precision highp sampler2D;
layout(location = 0) in vec3 unit;          // a point of the unit sphere
uniform sampler2D prims;                    // PRIM_TEXELS texels per primitive
uniform sampler2D comb;
uniform int layers;                         // shells + 1 (0 = base); 1 for ink
uniform float shells;
uniform float ink;                          // 0 fur, > 0 the ink hull's width
uniform vec3 camPos, camRight, camUp, camFwd;
uniform vec4 lens;                          // centerX centerY focal (perspective = 1)
uniform vec2 depthMap;                      // slope base
uniform vec2 stage;                         // logical width, height
uniform float time;
out vec3 vNormal;
out vec3 vLocal;
out float vH;
out float vAlong;
flat out int vPrim;
flat out float vRadius;
flat out float vLimb;
float hash(vec3 p) { return fract(sin(dot(p, vec3(12.9898, 78.233, 37.719))) * 43758.5453); }
vec4 texel(int prim, int k) { return texelFetch(prims, ivec2(k, prim), 0); }
${octaSource}
void main() {
  int prim = gl_InstanceID / layers, layer = gl_InstanceID - prim * layers;
  vec4 t0 = texel(prim, 0), t1 = texel(prim, 1), t3 = texel(prim, 3), t4 = texel(prim, 4), t5 = texel(prim, 5);
  vec3 a = t0.xyz, b = t1.xyz;
  float r = t0.w, len = t1.w;
  vec3 axis = b - a;
  vec3 Y = length(axis) > 1e-4 ? normalize(axis) : normalize(t5.xyz);
  vec3 X = normalize(t4.xyz - dot(t4.xyz, Y) * Y);
  vec3 Z = cross(X, Y);
  vec3 n = normalize(X * unit.x + Y * unit.y + Z * unit.z);
  vec3 root = (unit.y >= 0.0 ? b : a) + n * r;
  float h = ink > 0.0 ? 1.0 : pow(float(layer) / shells, 1.3);
  float flatten = combAt(int(t3.w), unit).z;
  float ruffle = texel(prim, 2).a;
  float reach = len * h * (1.0 - .6 * flatten) * (1.0 + .4 * ruffle);
  if (ink > 0.0 && len == 0.0 && r < 2.6) { gl_Position = vec4(0.0); return; }   // no ring round an eye
  // A decal (an eye, the nose) shows only while its face is toward the camera.
  if (t4.w > .5 && dot(t5.xyz, camFwd) > .1) { gl_Position = vec4(0.0); return; }
  if (ink > 0.0) {
    // The ink hull: past the fur's tips, notched by the clump noise.
    float clump = hash(floor(unit * 7.0));
    reach = len * (.55 + .45 * clump) + ink;
  }
  // The tips trail motion, and droop a touch.
  vec3 lag = t3.xyz * pow(h, 2.0) - vec3(0.0, .25 * len * h * h, 0.0);
  vec3 world = root + n * reach + lag;
  vec3 d = world - camPos;
  float vz = dot(d, camFwd);
  vec2 screen = vec2(lens.x + dot(d, camRight) * lens.z / vz, lens.y - dot(d, camUp) * lens.z / vz);
  vec2 ndc = vec2(screen.x / (stage.x * .5) - 1.0, 1.0 - screen.y / (stage.y * .5));
  float depth = clamp((vz * depthMap.x + depthMap.y + 1.5) / 3.0, 0.0, 1.0) * 2.0 - 1.0;
  gl_Position = vec4(ndc * vz, depth * vz, vz);
  vNormal = n; vLocal = unit; vH = float(layer) / shells; vPrim = prim;
  float L = length(axis);
  vAlong = (unit.y >= 0.0 ? L : 0.0) + unit.y * r;
  vRadius = r;
  vLimb = L > 1e-4 ? 1.0 : 0.0;
}`;

const fragmentSource = `#version 300 es
precision highp float;
precision highp sampler2D;
uniform sampler2D prims;
uniform sampler2D comb;
uniform float ink;
uniform float density;
${octaSource}
uniform vec3 inkColor;
uniform vec3 light;
uniform vec3 camFwd;
uniform float time;
in vec3 vNormal;
in vec3 vLocal;
in float vH;
in float vAlong;
flat in int vPrim;
flat in float vRadius;
flat in float vLimb;
out vec4 pixel;
vec4 texel(int k) { return texelFetch(prims, ivec2(k, vPrim), 0); }
uint hashu(uvec2 v) {
  v = v * 1664525u + 1013904223u;
  v.x += v.y * 1664525u; v.y += v.x * 1664525u;
  v ^= v >> 16u;
  v.x += v.y * 1664525u; v.y += v.x * 1664525u;
  v ^= v >> 16u;
  return v.x;
}
float hash(vec2 cell) { return float(hashu(uvec2(ivec2(cell) + 4096))) / 4294967295.0; }
void main() {
  if (ink > 0.0) { pixel = vec4(inkColor, 1.0); return; }
  vec4 t1 = texel(1), t2 = texel(2), t3 = texel(3);
  vec3 base = t2.rgb;
  float len = t1.w, ruffle = t2.a;
  vec2 o = octa(normalize(vLocal));
  vec4 c = combAt(int(t3.w), vLocal);
  vec2 dir = c.xy * 2.0 - 1.0;
  float flatten = c.z;
  if (vH > 0.0 && len > 0.0) {
    // Strands: a cell grid in octahedral space, leaning with the comb and
    // tousled by the ruffle; each cell's strand ends at its own height.
    float cell = 10.0 / density;   // world units per strand cell
    vec3 u = normalize(vLocal);
    vec2 uv = vLimb > .5
      ? vec2((atan(u.z, u.x) / 6.2832 + .5) * 6.2832 * vRadius, vAlong) / cell
      : (o * .5 + .5) * 2.4 * vRadius / cell;
    uv -= dir * pow(vH, 1.2) * 2.6 * (.4 + flatten);
    vec2 clumpAt = floor(uv / 3.0);
    float lean = hash(clumpAt + 31.0) * 6.2832;
    uv += ruffle * vec2(cos(lean), sin(lean)) * vH * 3.2;
    float tall = hash(floor(uv));
    float clump = hash(floor(uv / 3.0) + 97.0);
    tall = mix(tall, 1.0, .3) * mix(.7, 1.0, clump);
    vec2 f = fract(uv) * 2.0 - 1.0;
    float thick = 1.5 * (tall - vH) + .12;
    if (vH > tall || length(f) > thick + fwidth(uv.x) * .5) discard;
  }
  // Toon ramp on half-Lambert, root shade to tip light, a rim sheen.
  float lambert = dot(normalize(vNormal), -light) * .5 + .5;
  float band = lambert > .62 ? 1.0 : lambert > .34 ? .86 : .74;
  float ao = len > 0.0 ? mix(.84, 1.0, vH) : 1.0;
  vec3 color = base * band * ao;
  color = mix(color, min(vec3(1.0), base * 1.18 + .05), vH * .55);
  color += flatten * .13;   // combed fur lies smooth and catches the light
  float rim = pow(1.0 - abs(dot(normalize(vNormal), camFwd)), 3.0);
  color += rim * .12 * (len > 0.0 ? 1.0 : .3);
  pixel = vec4(color, 1.0);
}`;

function compile(gl, type, source) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS))
    throw new Error(gl.getShaderInfoLog(shader) || "fur shader failed");
  return shader;
}

// A unit sphere, subdivided from an octahedron: `level` 3 is 512 faces.
function sphere(level = 3) {
  let faces = [];
  const v = [[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1]];
  const tri = [[0, 2, 4], [4, 2, 1], [1, 2, 5], [5, 2, 0], [0, 4, 3], [4, 1, 3], [1, 5, 3], [5, 0, 3]];
  faces = tri.map((t) => t.map((i) => v[i]));
  const mid = (a, b) => { const m = [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2], l = Math.hypot(...m); return m.map((x) => x / l); };
  for (let k = 0; k < level; k++) {
    const next = [];
    for (const [a, b, c] of faces) {
      const ab = mid(a, b), bc = mid(b, c), ca = mid(c, a);
      next.push([a, ab, ca], [ab, b, bc], [ca, bc, c], [ab, bc, ca]);
    }
    faces = next;
  }
  return new Float32Array(faces.flat(2));
}

// Octahedral coordinates of a unit vector, as the shader has them.
export function octa([x, y, z]) {
  const s = Math.abs(x) + Math.abs(y) + Math.abs(z);
  x /= s; y /= s; z /= s;
  return y >= 0 ? [x, z] : [(1 - Math.abs(z)) * Math.sign(x || 1), (1 - Math.abs(x)) * Math.sign(z || 1)];
}

export function createFur(gl, { shells = 16, density = 26 } = {}) {
  const program = gl.createProgram();
  gl.attachShader(program, compile(gl, gl.VERTEX_SHADER, vertexSource));
  gl.attachShader(program, compile(gl, gl.FRAGMENT_SHADER, fragmentSource));
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
  const at = (name) => gl.getUniformLocation(program, name);
  const u = Object.fromEntries(["prims", "comb", "layers", "shells", "ink", "camPos", "camRight", "camUp", "camFwd",
    "lens", "depthMap", "stage", "time", "density", "inkColor", "light"].map((n) => [n, at(n)]));

  const unit = sphere(3), vertexCount = unit.length / 3;
  const vao = gl.createVertexArray(), buffer = gl.createBuffer();
  gl.bindVertexArray(vao);
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.bufferData(gl.ARRAY_BUFFER, unit, gl.STATIC_DRAW);
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
  gl.bindVertexArray(null);

  const primData = new Float32Array(MAX_PRIMS * PRIM_TEXELS * 4);
  const primTex = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, primTex);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, PRIM_TEXELS, MAX_PRIMS, 0, gl.RGBA, gl.FLOAT, primData);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

  // The comb: per texel direction (x, y), flatness, and their velocities,
  // simulated on the CPU, uploaded as RGBA8.
  const field = new Float32Array(ATLAS * ATLAS * 6);
  const combBytes = new Uint8Array(ATLAS * ATLAS * 4);
  for (let i = 0; i < ATLAS * ATLAS; i++) { combBytes[i * 4] = 128; combBytes[i * 4 + 1] = 128; }
  const touched = new Float32Array(MAX_PRIMS).fill(-1e9);  // last stroke time per tile
  const combTex = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, combTex);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, ATLAS, ATLAS, 0, gl.RGBA, gl.UNSIGNED_BYTE, combBytes);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  const live = new Set();   // tiles still moving

  const state = { shells, density, prims: [], time: 0, stats: { prims: 0, faces: 0 } };

  // Splat a stroke into a primitive's tile: at octahedral point `o`, lean
  // toward `dir` (tile units), flatten by `amount`.
  function comb(prim, o, dir, amount = 1, now = state.time) {
    if (!(prim >= 0 && prim < MAX_PRIMS)) return;
    const tx = (prim % PER_ROW) * TILE, ty = Math.floor(prim / PER_ROW) * TILE;
    const cx = (o[0] * .5 + .5) * (TILE - 1), cy = (o[1] * .5 + .5) * (TILE - 1), sigma = 2.2;
    const m = Math.hypot(dir[0], dir[1]) || 1;
    for (let y = 0; y < TILE; y++) for (let x = 0; x < TILE; x++) {
      const w = Math.exp(-((x - cx) ** 2 + (y - cy) ** 2) / (2 * sigma * sigma)) * amount;
      if (w < .02) continue;
      const i = ((ty + y) * ATLAS + tx + x) * 6;
      field[i] += (dir[0] / m - field[i]) * w;
      field[i + 1] += (dir[1] / m - field[i + 1]) * w;
      field[i + 2] += (1 - field[i + 2]) * w;
      field[i + 3] = field[i + 4] = field[i + 5] = 0;
    }
    touched[prim] = now;
    live.add(prim);
  }

  // Relax every live tile: hold, then a damped spring back to rest.
  function relax(dt, now) {
    const omega = 1.4, zeta = .6;
    for (const prim of [...live]) {
      const tx = (prim % PER_ROW) * TILE, ty = Math.floor(prim / PER_ROW) * TILE;
      const holding = now - touched[prim] < 1.2;
      let moving = holding;
      for (let y = 0; y < TILE; y++) for (let x = 0; x < TILE; x++) {
        const i = ((ty + y) * ATLAS + tx + x) * 6, b = ((ty + y) * ATLAS + tx + x) * 4;
        if (!holding) for (let k = 0; k < 3; k++) {
          const v = field[i + k], w = field[i + 3 + k];
          const acc = -omega * omega * v - 2 * zeta * omega * w;
          field[i + 3 + k] = w + acc * dt;
          field[i + k] = v + field[i + 3 + k] * dt;
          if (Math.abs(field[i + k]) > .004 || Math.abs(field[i + 3 + k]) > .004) moving = true;
        }
        combBytes[b] = Math.max(0, Math.min(255, 128 + field[i] * 127));
        combBytes[b + 1] = Math.max(0, Math.min(255, 128 + field[i + 1] * 127));
        combBytes[b + 2] = Math.max(0, Math.min(255, Math.max(0, field[i + 2]) * 255));
      }
      if (!moving) live.delete(prim);
      gl.bindTexture(gl.TEXTURE_2D, combTex);
      gl.texSubImage2D(gl.TEXTURE_2D, 0, tx, ty, TILE, TILE, gl.RGBA, gl.UNSIGNED_BYTE,
        tileBytes(tx, ty));
    }
  }
  const tileBuffer = new Uint8Array(TILE * TILE * 4);
  function tileBytes(tx, ty) {
    for (let y = 0; y < TILE; y++)
      tileBuffer.set(combBytes.subarray(((ty + y) * ATLAS + tx) * 4, ((ty + y) * ATLAS + tx + TILE) * 4), y * TILE * 4);
    return tileBuffer;
  }

  // One frame: primitives, the frame's CAMERA (24 numbers), the stage size.
  function draw(prims, cam, width, height, dt) {
    state.time += dt;
    relax(dt, state.time);
    const n = Math.min(prims.length, MAX_PRIMS);
    state.prims = prims;
    for (let p = 0; p < n; p++) {
      const q = prims[p], o = p * PRIM_TEXELS * 4;
      primData.set([q.a[0], q.a[1], q.a[2], q.r, q.b[0], q.b[1], q.b[2], q.len,
        q.rgb[0] / 255, q.rgb[1] / 255, q.rgb[2] / 255, q.ruffle || 0,
        q.lag?.[0] || 0, q.lag?.[1] || 0, q.lag?.[2] || 0, q.tile ?? p,
        q.x[0], q.x[1], q.x[2], q.decal ? 1 : 0, ...(q.decal || q.y), 0], o);
    }
    gl.bindTexture(gl.TEXTURE_2D, primTex);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, PRIM_TEXELS, Math.max(1, n), gl.RGBA, gl.FLOAT, primData);

    gl.useProgram(program);
    gl.bindVertexArray(vao);
    gl.activeTexture(gl.TEXTURE0); gl.bindTexture(gl.TEXTURE_2D, primTex); gl.uniform1i(u.prims, 0);
    gl.activeTexture(gl.TEXTURE1); gl.bindTexture(gl.TEXTURE_2D, combTex); gl.uniform1i(u.comb, 1);
    gl.activeTexture(gl.TEXTURE0);
    gl.uniform3f(u.camPos, cam[0], cam[1], cam[2]);
    gl.uniform3f(u.camRight, cam[3], cam[4], cam[5]);
    gl.uniform3f(u.camUp, cam[6], cam[7], cam[8]);
    gl.uniform3f(u.camFwd, cam[9], cam[10], cam[11]);
    gl.uniform4f(u.lens, cam[12], cam[13], cam[15], 0);
    gl.uniform2f(u.depthMap, cam[17], cam[18]);
    gl.uniform2f(u.stage, width, height);
    gl.uniform1f(u.time, state.time);
    gl.uniform1f(u.shells, state.shells);
    gl.uniform1f(u.density, state.density);
    gl.uniform3f(u.inkColor, 74 / 255, 44 / 255, 30 / 255);
    gl.uniform3f(u.light, .38, -.9, .25);
    gl.enable(gl.DEPTH_TEST);

    // The ink hull first, back faces only; then the fur over it.
    gl.enable(gl.CULL_FACE);
    gl.cullFace(gl.FRONT);
    gl.frontFace(gl.CCW);
    gl.uniform1f(u.ink, .55);
    gl.uniform1i(u.layers, 1);
    gl.drawArraysInstanced(gl.TRIANGLES, 0, vertexCount, n);
    gl.cullFace(gl.BACK);
    gl.uniform1f(u.ink, 0);
    gl.uniform1i(u.layers, state.shells + 1);
    gl.drawArraysInstanced(gl.TRIANGLES, 0, vertexCount, n * (state.shells + 1));
    gl.disable(gl.CULL_FACE);
    gl.bindVertexArray(null);
    state.stats.prims = n;
    state.stats.faces = vertexCount / 3 * n * (state.shells + 2);
  }

  return { draw, comb, state, setShells: (k) => { state.shells = Math.max(2, Math.min(32, k | 0)); },
    setDensity: (d) => { state.density = d; } };
}

// ——— the pup's SKETCH ops, as fur primitives ———
// Records (frame-vm.mjs op 18): kind · outline w rgb · nudge · rgb · facing(3)
// · anchors. Balls are kind 1 (x y z r), limbs kind 2 (two ends and r).
const recordSize = (R, i) => 12 + [0, 4, 7, 9, 1 + 3 * R[i + 12], 12][R[i]];
const cream = (c) => c[0] > 240 && c[1] > 225;

// Fur length by what the shape is: none on eyes, nose, tongue, collar, tag
// and paws; long on ears and the tail; short on the body.
function lengthOf(kind, r, rgb) {
  if (rgb[0] > 200 && rgb[1] < 120) return 0;           // tongue, collar
  if (rgb[0] < 70) return 0;                            // nose, eyes
  if (kind === 1 && r < 2.4) return 0;                  // highlights, the tag
  if (kind === 1 && r < 3.8 && cream(rgb)) return 0;    // paws
  // Ears (the saddle brown, as limbs) and the tail (the thin limb) are long.
  const ear = kind === 2 && rgb[0] < 180 && rgb[1] < 120, tail = kind === 2 && r < 2.6;
  return tail ? 1.1 : ear ? r * .2 : r * (kind === 2 ? .1 : .075);
}

// A shape with no fur inside a furry ball (an eye, the nose, the tag) is
// moved out to that ball's surface, past its fur, so it shows where it faces
// the camera and hides where it doesn't: a decal, by depth.
export function surface(prims) {
  for (const q of prims) {
    if (q.len || q.kind !== 1) continue;
    let host = null, best = Infinity;
    for (const h of prims) {
      if (h === q || !h.len || h.kind !== 1) continue;
      const d = Math.hypot(q.a[0] - h.a[0], q.a[1] - h.a[1], q.a[2] - h.a[2]);
      if (d < h.r && d < best) { best = d; host = h; }
    }
    if (!host || best < 1e-4) continue;
    const k = (host.r + host.len * .6 + q.r * .2) / best;
    const c = host.a.map((x, i) => x + (q.a[i] - x) * k);
    q.a = c; q.b = c;
    q.decal = c.map((x, i) => (x - host.a[i]) / (best * k));
  }
  return prims;
}

export function sketchPrims(records, count, m, part, out = []) {
  const unit = Math.cbrt(Math.abs(m[3] * (m[7] * m[11] - m[8] * m[10]) - m[4] * (m[6] * m[11] - m[8] * m[9]) +
    m[5] * (m[6] * m[10] - m[7] * m[9])));
  const place = (x, y, z) => [m[0] + x * m[3] + y * m[6] + z * m[9], m[1] + x * m[4] + y * m[7] + z * m[10],
    m[2] + x * m[5] + y * m[8] + z * m[11]];
  const X = [m[3], m[4], m[5]], Y = [m[6], m[7], m[8]];
  for (let n = 0, i = 0; n < count; n++, i += recordSize(records, i)) {
    const kind = records[i], a = i + 12, rgb = [records[i + 6], records[i + 7], records[i + 8]];
    if (kind === 1) {
      const c = place(records[a], records[a + 1], records[a + 2]), r = records[a + 3] * unit;
      out.push({ kind, a: c, b: c, r, rgb, len: lengthOf(kind, r, rgb), x: X, y: Y, part });
    } else if (kind === 2) {
      const r = records[a + 6] * unit;
      out.push({ kind, a: place(records[a], records[a + 1], records[a + 2]),
        b: place(records[a + 3], records[a + 4], records[a + 5]), r, rgb, len: lengthOf(kind, r, rgb), x: X, y: Y, part });
    }
  }
  return out;
}

// Whether a sketch is all balls and limbs (so fur can take it whole).
export function furry(records, count) {
  for (let n = 0, i = 0; n < count; n++, i += recordSize(records, i)) if (records[i] !== 1 && records[i] !== 2) return false;
  return count > 0;
}

// A ray from the camera through a screen point, and the nearest primitive
// it meets: its index and the hit's direction from the primitive's axis, in
// the primitive's own frame (for the comb).
export function pickPrim(prims, cam, sx, sy) {
  const u = (sx - cam[12]) / cam[15], v = (cam[13] - sy) / cam[15];
  const d = [cam[9] + cam[3] * u + cam[6] * v, cam[10] + cam[4] * u + cam[7] * v, cam[11] + cam[5] * u + cam[8] * v];
  const dl = Math.hypot(...d); d[0] /= dl; d[1] /= dl; d[2] /= dl;
  const o = [cam[0], cam[1], cam[2]];
  let best = null;
  prims.forEach((q, index) => {
    if (!q.len) return;
    // closest approach of the ray to the segment a–b, then a sphere there
    const ab = [q.b[0] - q.a[0], q.b[1] - q.a[1], q.b[2] - q.a[2]], L = Math.hypot(...ab);
    let c = q.a;
    for (let k = 0; k < 2; k++) {
      const oc = [o[0] - c[0], o[1] - c[1], o[2] - c[2]];
      const bq = oc[0] * d[0] + oc[1] * d[1] + oc[2] * d[2], cq = oc[0] ** 2 + oc[1] ** 2 + oc[2] ** 2 - (q.r + q.len) ** 2;
      const disc = bq * bq - cq;
      if (disc < 0) { if (L < 1e-4) return; }
      const t = -bq - Math.sqrt(Math.max(0, disc));
      const p = [o[0] + d[0] * t, o[1] + d[1] * t, o[2] + d[2] * t];
      if (L > 1e-4 && k === 0) {
        const s = Math.max(0, Math.min(1, ((p[0] - q.a[0]) * ab[0] + (p[1] - q.a[1]) * ab[1] + (p[2] - q.a[2]) * ab[2]) / (L * L)));
        c = [q.a[0] + ab[0] * s, q.a[1] + ab[1] * s, q.a[2] + ab[2] * s];
        continue;
      }
      if (disc < 0 || t <= 0) return;
      if (!best || t < best.t) best = { t, index, point: p, center: c };
    }
  });
  if (!best) return null;
  const q = prims[best.index];
  const n = [best.point[0] - best.center[0], best.point[1] - best.center[1], best.point[2] - best.center[2]];
  // into the primitive's frame, as the shader builds it
  const axis = [q.b[0] - q.a[0], q.b[1] - q.a[1], q.b[2] - q.a[2]];
  const norm = (w) => { const l = Math.hypot(...w) || 1; return w.map((x) => x / l); };
  const Y = Math.hypot(...axis) > 1e-4 ? norm(axis) : norm(q.y);
  const dotY = q.x[0] * Y[0] + q.x[1] * Y[1] + q.x[2] * Y[2];
  const X = norm([q.x[0] - dotY * Y[0], q.x[1] - dotY * Y[1], q.x[2] - dotY * Y[2]]);
  const Z = [X[1] * Y[2] - X[2] * Y[1], X[2] * Y[0] - X[0] * Y[2], X[0] * Y[1] - X[1] * Y[0]];
  const local = norm([n[0] * X[0] + n[1] * X[1] + n[2] * X[2], n[0] * Y[0] + n[1] * Y[1] + n[2] * Y[2],
    n[0] * Z[0] + n[1] * Z[1] + n[2] * Z[2]]);
  return { index: best.index, local, point: best.point };
}
