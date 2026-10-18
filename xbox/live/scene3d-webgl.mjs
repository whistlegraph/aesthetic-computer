import { OskiewarScene3D } from "./scene3d.mjs";

// clipDepth without the finite check: a NaN here comes from geometry the game
// already culls, and a compare on the hot path is all the guard it needs.
const depthOf = (z) => {
  const d = (z + 1.5) / 3;
  return d < 0 ? 0 : d > 1 ? 1 : d;
};

const vertexSource = `#version 300 es
precision highp float;
layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
out vec3 ink;
void main() {
  gl_Position = vec4(position, 1.0);
  ink = color;
}`;

const fragmentSource = `#version 300 es
precision highp float;
in vec3 ink;
out vec4 pixel;
void main() { pixel = vec4(ink, 1.0); }`;

// Raycast sprites: an object the game would rather have shaded per pixel
// than built from faces hands the host a kind and a few numbers, and the
// host casts a ray through every pixel of a screen square for it. The sphere
// is exact (its normal and its depth come from the hit, so it sits in the
// depth buffer like any face), and its edge is smoothed over one pixel.
// Kinds are fragment functions; the beach ball is the first. 26.10.15
const raycastVertexSource = `#version 300 es
precision highp float;
layout(location = 0) in vec2 corner;
uniform vec2 centre;
uniform vec2 extent;
out vec2 local;
void main() {
  local = corner;
  gl_Position = vec4(centre + corner * extent, 0.0, 1.0);
}`;

const raycastFragmentSource = `#version 300 es
precision highp float;
in vec2 local;
uniform float depth;
uniform float depthRange;
uniform float roll;
uniform float tumble;
uniform int kind;
out vec4 pixel;
const vec3 beach[8] = vec3[8](
  vec3(.24,.47,.95), vec3(.66,.33,.88), vec3(.93,.26,.42), vec3(1.,.53,.22),
  vec3(1.,.84,.24), vec3(.36,.82,.44), vec3(.2,.78,.82), vec3(.24,.47,.95));
vec3 rotateX(vec3 v, float a) { float c = cos(a), s = sin(a);
  return vec3(v.x, c * v.y - s * v.z, s * v.y + c * v.z); }
vec3 rotateZ(vec3 v, float a) { float c = cos(a), s = sin(a);
  return vec3(c * v.x - s * v.y, s * v.x + c * v.y, v.z); }
void main() {
  float r = length(local);
  if (kind == 1) {
    vec2 q = local;
    float c = cos(roll), s = sin(roll);
    q = mat2(c, -s, s, c) * q;
    float wisps = .72 + .18 * sin(q.x * 7.0 + sin(q.y * 5.0 + roll))
      + .1 * sin(q.y * 11.0 - q.x * 4.0 + roll * 2.0);
    float density = exp(-dot(q * vec2(1.1, .85), q * vec2(1.1, .85)) * 3.2);
    float alpha = tumble * density * wisps * (1.0 - smoothstep(.55, 1.0, r));
    if (alpha < .002) discard;
    gl_FragDepth = .5 + depth * .5;
    pixel = vec4(vec3(.79, .81, .84) + density * .08, alpha);
    return;
  }
  float aa = fwidth(r);
  float cover = 1.0 - smoothstep(1.0 - aa, 1.0 + aa * .25, r);
  if (cover <= 0.0) discard;
  float rr = min(r * r, 1.0);
  vec3 n = vec3(local, sqrt(1.0 - rr));
  // Object space: the ball rolls in the picture plane and is tipped toward
  // the lens so its pole and gores read as a sphere, not a pinwheel.
  vec3 o = rotateX(rotateZ(n, -roll), -.62 + tumble);
  float lon = atan(o.x, o.z) / 6.2831853 + .5;
  float gore = lon * 8.0;
  vec3 base = beach[int(floor(gore)) & 7];
  // A fine welded seam where two gores meet.
  float edge = min(fract(gore), 1.0 - fract(gore));
  float seamWidth = fwidth(gore) * 1.2 + .015;
  base *= mix(.8, 1.0, smoothstep(seamWidth * .5, seamWidth, edge));
  // White pole caps with a valve on one.
  float cap = smoothstep(.83, .86, abs(o.y));
  base = mix(base, vec3(.97, .96, .92), cap);
  float valve = smoothstep(.985, .99, o.y);
  base = mix(base, vec3(.82, .8, .76), valve);
  vec3 light = normalize(vec3(-.45, .62, .64));
  float diffuse = max(dot(n, light), 0.0);
  float wrap = max(dot(n, light) * .5 + .5, 0.0);
  vec3 halfway = normalize(light + vec3(0.0, 0.0, 1.0));
  float spec = pow(max(dot(n, halfway), 0.0), 90.0) * .85;
  float sheen = pow(max(dot(n, halfway), 0.0), 9.0) * .18;
  float rim = pow(1.0 - n.z, 3.0);
  vec3 colour = base * (.28 + .52 * diffuse + .3 * wrap);
  colour += vec3(1.0) * (spec + sheen);
  colour += vec3(.75, .86, 1.0) * rim * .45;
  // Ambient occlusion toward the floor side.
  colour *= mix(.78, 1.0, smoothstep(-1.0, .1, n.y));
  gl_FragDepth = clamp(depth - depthRange * n.z, 0.0, 1.0);
  pixel = vec4(min(colour, vec3(1.0)), cover);
}`;

const postVertexSource = `#version 300 es
precision highp float;
out vec2 uv;
void main() {
  vec2 p = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
  uv = p;
  gl_Position = vec4(p * 2.0 - 1.0, 0.0, 1.0);
}`;
const postFragmentSource = `#version 300 es
precision highp float;
in vec2 uv;
uniform sampler2D image;
uniform sampler2D sceneDepth;
uniform vec2 logicalSize;
uniform vec4 focus; // y, half-band, feather, blur radius in logical pixels
uniform vec4 motion; // camera dx/dy, heart pulse, speed
out vec4 pixel;
void main() {
  vec4 center = texture(image, uv);
  float depth = texture(sceneDepth, uv).r;
  // HUD triangles sit at -1.48 before the shared depth mapping; text is a
  // separate full-density canvas and never enters this pass.
  if (depth < .505) { pixel = center; return; }
  float edge = smoothstep(.18, .62, length(uv - .5));
  float defocus = smoothstep(focus.y, focus.y + max(.01, focus.z), abs(1.0 - uv.y - focus.x));
  vec2 radial = (uv - .5) * (motion.w * 9.0 + motion.z * 5.0) * edge;
  vec2 travel = vec2(motion.x, -motion.y) + radial;
  vec2 soft = vec2(focus.w * defocus, focus.w * defocus * .35);
  vec2 stepUV = (travel + soft) / logicalSize;
  vec4 total = center * 2.0;
  float weight = 2.0;
  for (int i = -3; i <= 3; i++) {
    if (i == 0) continue;
    vec2 at = clamp(uv + stepUV * (float(i) / 3.0), vec2(.001), vec2(.999));
    float otherDepth = texture(sceneDepth, at).r;
    // Never smear HUD ink into the world or a foreground silhouette into sky.
    if (otherDepth < .505 || abs(otherDepth - depth) > .06) continue;
    float w = 1.0 - abs(float(i)) * .15;
    total += texture(image, at) * w;
    weight += w;
  }
  pixel = total / weight;
}`;

function compile(gl, type, source) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const message = gl.getShaderInfoLog(shader) || "shader compilation failed";
    gl.deleteShader(shader);
    throw new Error(message);
  }
  return shader;
}

function program(gl, vertexText = vertexSource, fragmentText = fragmentSource) {
  const vertex = compile(gl, gl.VERTEX_SHADER, vertexText);
  const fragment = compile(gl, gl.FRAGMENT_SHADER, fragmentText);
  const result = gl.createProgram();
  gl.attachShader(result, vertex);
  gl.attachShader(result, fragment);
  gl.linkProgram(result);
  gl.deleteShader(vertex);
  gl.deleteShader(fragment);
  if (!gl.getProgramParameter(result, gl.LINK_STATUS)) {
    const message = gl.getProgramInfoLog(result) || "scene program link failed";
    gl.deleteProgram(result);
    throw new Error(message);
  }
  return result;
}

export class WebGLOskiewarScene3D {
  constructor(canvas, options = {}) {
    const gl = canvas.getContext("webgl2", {
      alpha: true, antialias: true, depth: true, stencil: false,
      premultipliedAlpha: false, preserveDrawingBuffer: false,
    });
    if (!gl) throw new Error("WebGL 2 is unavailable");
    this.canvas = canvas;
    this.gl = gl;
    this.scene = new OskiewarScene3D(options);
    this.program = program(gl);
    this.array = gl.createVertexArray();
    this.buffer = gl.createBuffer();
    gl.bindVertexArray(this.array);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);
    gl.bufferData(gl.ARRAY_BUFFER, this.scene.vertices.byteLength,
      gl.DYNAMIC_DRAW);
    const stride = 6 * Float32Array.BYTES_PER_ELEMENT;
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 3, gl.FLOAT, false, stride, 0);
    gl.enableVertexAttribArray(1);
    gl.vertexAttribPointer(1, 3, gl.FLOAT, false, stride,
      3 * Float32Array.BYTES_PER_ELEMENT);
    gl.bindVertexArray(null);
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);
    gl.disable(gl.CULL_FACE);
    // The shell's logical stage (1080 tall, width by aspect). The fast path
    // below maps logical coordinates to clip space with these, so the game
    // hands over the same numbers it hands every other host.
    this.logicalWidth = 1920;
    this.logicalHeight = 1080;
    this.halfWidth = 960;
    this.halfHeight = 540;
    // Raycast sprites, queued per frame and drawn after the faces. A sprite
    // program that fails to build leaves the queue off and the game on its
    // flat drawing; the faces never depend on it.
    this.sprites = [];
    this.effects = { focus: [.5, 1, .2, 0], motion: [0, 0, 0, 0] };
    this.postProgram = program(gl, postVertexSource, postFragmentSource);
    this.postUniforms = Object.fromEntries(["image", "sceneDepth", "logicalSize", "focus", "motion"]
      .map(name => [name, gl.getUniformLocation(this.postProgram, name)]));
    this.postArray = gl.createVertexArray();
    this.targets = null;
    try {
      this.raycastProgram = program(gl, raycastVertexSource, raycastFragmentSource);
      this.raycastUniforms = Object.fromEntries(["centre", "extent", "depth",
        "depthRange", "roll", "tumble", "kind"].map((name) =>
        [name, gl.getUniformLocation(this.raycastProgram, name)]));
      this.quadArray = gl.createVertexArray();
      this.quadBuffer = gl.createBuffer();
      gl.bindVertexArray(this.quadArray);
      gl.bindBuffer(gl.ARRAY_BUFFER, this.quadBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]),
        gl.STATIC_DRAW);
      gl.enableVertexAttribArray(0);
      gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0);
      gl.bindVertexArray(null);
    } catch (error) {
      console.warn("[scene] raycast sprites unavailable:", error?.message || error);
      this.raycastProgram = null;
    }
  }

  // One raycast sprite: logical centre and radius (the stage's 1080-tall
  // units, like every face), the centre's depth and the depth its nearest
  // surface reaches (the shared z), and the kind's own numbers. Returns
  // whether the host took it, so the game can fall back to flat drawing.
  raycast(kind, x, y, radius, z, zFront, roll = 0, tumble = 0) {
    if (!this.raycastProgram || !["beach", "smoke"].includes(kind)) return false;
    if (!(radius > .25) || !Number.isFinite(x + y + z + zFront)) return false;
    this.sprites.push({ kind, x, y, radius, z, zFront, roll, tumble });
    return true;
  }

  postEffects(y = .5, band = 1, feather = .2, blur = 0, dx = 0, dy = 0, pulse = 0, speed = 0) {
    const finite = (n, fallback = 0) => Number.isFinite(n) ? n : fallback;
    this.effects.focus = [finite(y, .5), finite(band, 1), Math.max(.01, finite(feather, .2)), Math.max(0, Math.min(6, finite(blur)))];
    this.effects.motion = [Math.max(-12, Math.min(12, finite(dx))), Math.max(-12, Math.min(12, finite(dy))), Math.max(0, Math.min(1, finite(pulse))), Math.max(0, Math.min(1, finite(speed)))];
  }

  releaseTargets() {
    const gl = this.gl, t = this.targets;
    if (!t) return;
    for (const f of [t.draw, t.resolve]) gl.deleteFramebuffer(f);
    for (const b of [t.colorBuffer, t.depthBuffer]) gl.deleteRenderbuffer(b);
    for (const texture of [t.color, t.depth]) gl.deleteTexture(texture);
    this.targets = null;
  }

  ensureTargets() {
    const gl = this.gl, width = this.canvas.width, height = this.canvas.height;
    if (this.targets?.width === width && this.targets.height === height) return;
    this.releaseTargets();
    const t = this.targets = { width, height, draw: gl.createFramebuffer(), resolve: gl.createFramebuffer() };
    gl.bindFramebuffer(gl.FRAMEBUFFER, t.resolve);
    for (const [key, format, attachment] of [["color", gl.RGBA8, gl.COLOR_ATTACHMENT0], ["depth", gl.DEPTH_COMPONENT24, gl.DEPTH_ATTACHMENT]]) {
      t[key] = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, t[key]);
      gl.texStorage2D(gl.TEXTURE_2D, 1, format, width, height);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, key === "color" ? gl.LINEAR : gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, key === "color" ? gl.LINEAR : gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, attachment, gl.TEXTURE_2D, t[key], 0);
    }
    if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE) throw new Error("post resolve framebuffer incomplete");
    gl.bindFramebuffer(gl.FRAMEBUFFER, t.draw);
    const samples = Math.min(4, gl.getParameter(gl.MAX_SAMPLES));
    for (const [key, format, attachment] of [["colorBuffer", gl.RGBA8, gl.COLOR_ATTACHMENT0], ["depthBuffer", gl.DEPTH_COMPONENT24, gl.DEPTH_ATTACHMENT]]) {
      t[key] = gl.createRenderbuffer();
      gl.bindRenderbuffer(gl.RENDERBUFFER, t[key]);
      gl.renderbufferStorageMultisample(gl.RENDERBUFFER, samples, format, width, height);
      gl.framebufferRenderbuffer(gl.FRAMEBUFFER, attachment, gl.RENDERBUFFER, t[key]);
    }
    if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE) throw new Error("post scene framebuffer incomplete");
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  }

  presentPost() {
    const gl = this.gl, t = this.targets, u = this.postUniforms;
    gl.bindFramebuffer(gl.READ_FRAMEBUFFER, t.draw);
    gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, t.resolve);
    gl.blitFramebuffer(0, 0, t.width, t.height, 0, 0, t.width, t.height, gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT, gl.NEAREST);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.disable(gl.DEPTH_TEST);
    gl.useProgram(this.postProgram);
    gl.bindVertexArray(this.postArray);
    gl.activeTexture(gl.TEXTURE0); gl.bindTexture(gl.TEXTURE_2D, t.color); gl.uniform1i(u.image, 0);
    gl.activeTexture(gl.TEXTURE1); gl.bindTexture(gl.TEXTURE_2D, t.depth); gl.uniform1i(u.sceneDepth, 1);
    gl.uniform2f(u.logicalSize, this.logicalWidth, this.logicalHeight);
    gl.uniform4fv(u.focus, this.effects.focus); gl.uniform4fv(u.motion, this.effects.motion);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    gl.bindVertexArray(null);
    gl.activeTexture(gl.TEXTURE0);
    gl.enable(gl.DEPTH_TEST);
  }

  setLogicalSize(width, height) {
    this.logicalWidth = width;
    this.logicalHeight = height;
    this.halfWidth = width / 2;
    this.halfHeight = height / 2;
  }

  // The host face sink: twelve positional numbers, no allocation, the same
  // signature as the console's `triangle3d`. Depth uses the shared
  // (z + 1.5) / 3 mapping so the depth test agrees with D3D and Metal. A
  // frame that outgrows the buffer doubles it (and the GPU buffer with it)
  // rather than dropping faces; the park draws its meshes as JS faces here.
  triangle3d(x1, y1, z1, x2, y2, z2, x3, y3, z3, r = 255, g = 255, b = 255) {
    const scene = this.scene;
    if (scene.triangleCount >= scene.maxTriangles) this.grow();
    const v = scene.vertices;
    let at = scene.triangleCount * 18;
    const hw = this.halfWidth, hh = this.halfHeight;
    const cr = (r < 0 ? 0 : r > 255 ? 255 : r) / 255;
    const cg = (g < 0 ? 0 : g > 255 ? 255 : g) / 255;
    const cb = (b < 0 ? 0 : b > 255 ? 255 : b) / 255;
    v[at++] = x1 / hw - 1; v[at++] = 1 - y1 / hh; v[at++] = depthOf(z1);
    v[at++] = cr; v[at++] = cg; v[at++] = cb;
    v[at++] = x2 / hw - 1; v[at++] = 1 - y2 / hh; v[at++] = depthOf(z2);
    v[at++] = cr; v[at++] = cg; v[at++] = cb;
    v[at++] = x3 / hw - 1; v[at++] = 1 - y3 / hh; v[at++] = depthOf(z3);
    v[at++] = cr; v[at++] = cg; v[at++] = cb;
    scene.triangleCount++;
  }

  grow() {
    const scene = this.scene;
    const next = new Float32Array(scene.vertices.length * 2);
    next.set(scene.vertices);
    scene.vertices = next;
    scene.maxTriangles *= 2;
    const gl = this.gl;
    gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);
    gl.bufferData(gl.ARRAY_BUFFER, next.byteLength, gl.DYNAMIC_DRAW);
  }

  resize(pixelWidth, pixelHeight) {
    if (this.canvas.width !== pixelWidth) this.canvas.width = pixelWidth;
    if (this.canvas.height !== pixelHeight) this.canvas.height = pixelHeight;
  }

  beginFrame() { this.scene.beginFrame(); this.sprites.length = 0; }

  triangle(...values) { return this.scene.triangle(...values); }

  present({ clear = [0, 0, 0, 0] } = {}) {
    const gl = this.gl;
    const fx = this.effects;
    let post = fx.focus[3] > 0 || fx.motion.some(value => value !== 0);
    if (post) {
      try { this.ensureTargets(); }
      catch (error) {
        console.warn("[scene] post effects unavailable:", error.message);
        this.releaseTargets();
        post = false;
      }
    }
    gl.bindFramebuffer(gl.FRAMEBUFFER, post ? this.targets.draw : null);
    gl.depthMask(true);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    gl.clearColor(...clear);
    gl.clearDepth(1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    if (this.scene.triangleCount) {
      gl.useProgram(this.program);
      gl.bindVertexArray(this.array);
      gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);
      gl.bufferSubData(gl.ARRAY_BUFFER, 0, this.scene.frameVertices());
      gl.drawArrays(gl.TRIANGLES, 0, this.scene.triangleCount * 3);
      gl.bindVertexArray(null);
    }
    if (this.sprites.length) this.presentSprites();
    if (post) this.presentPost();
  }

  presentSprites() {
    const gl = this.gl, u = this.raycastUniforms;
    gl.useProgram(this.raycastProgram);
    gl.bindVertexArray(this.quadArray);
    gl.enable(gl.BLEND);
    gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
    // Opaque balls establish depth first; wisps blend back-to-front without
    // writing depth, so their transparent margins never punch holes in smoke.
    this.sprites.sort((a, b) => (a.kind === "smoke") - (b.kind === "smoke") || b.z - a.z);
    for (const sprite of this.sprites) {
      gl.depthMask(sprite.kind !== "smoke");
      const centre = depthOf(sprite.z), front = depthOf(sprite.zFront);
      gl.uniform2f(u.centre, sprite.x / this.halfWidth - 1, 1 - sprite.y / this.halfHeight);
      gl.uniform2f(u.extent, sprite.radius / this.halfWidth, sprite.radius / this.halfHeight);
      gl.uniform1f(u.depth, centre);
      gl.uniform1f(u.depthRange, Math.max(0, centre - front));
      gl.uniform1f(u.roll, sprite.roll);
      gl.uniform1f(u.tumble, sprite.tumble);
      gl.uniform1i(u.kind, sprite.kind === "smoke" ? 1 : 0);
      gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    }
    gl.depthMask(true);
    gl.disable(gl.BLEND);
    gl.bindVertexArray(null);
  }

  destroy() {
    const gl = this.gl;
    this.releaseTargets();
    gl.deleteProgram(this.postProgram);
    gl.deleteVertexArray(this.postArray);
    gl.deleteBuffer(this.buffer);
    gl.deleteVertexArray(this.array);
    gl.deleteProgram(this.program);
    if (this.raycastProgram) {
      gl.deleteProgram(this.raycastProgram);
      gl.deleteBuffer(this.quadBuffer);
      gl.deleteVertexArray(this.quadArray);
    }
  }
}

export default WebGLOskiewarScene3D;
