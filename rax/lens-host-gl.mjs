// 🔭 The lens host again, for a card with no WebGPU (Xbox Edge, older phones).
// The same two passes as lens-host.mjs, in WebGL 2, from the same triangle
// stream: the world drawn flat and wide into a picture (with the screens'
// mask beside it, textures from one array, the sun's shadows counted in the
// stencil), then one triangle through the lens to the screen, then the HUD.
// Whatever lens-host.mjs learns to do, this learns too: view.mjs asks the
// same things of both and does not know which it has.
//
// What differs from the WebGPU one, and why the picture does not:
//   • a picture drawn into has its first row at the bottom here, so the lens
//     looks it up with y the other way (the pictures laid on shapes are sent
//     row for row as they are there, and need nothing);
//   • one blend serves both of a pass's pictures, so the mask is written
//     with a whole alpha and comes out as it is written;
//   • a pixel's place on the screen counts up from the bottom, and is turned
//     over for the grain and the lines, which are counted from the top.
import { OskiewarScene3D } from './engine/scene3d.mjs';
import { TEXTURE_SIZE, TEXTURE_LAYERS } from './lens-host.mjs';

const SHADOW_NEAR = 0.002; // (scene3d-webgpu.mjs: nearer than this is the HUD's, never darkened)

const WORLD_VERTEX = `#version 300 es
layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
layout(location = 2) in vec4 tex;
uniform float zoom;
out vec3 vColor;
out vec4 vTex;
void main() {
  gl_Position = vec4(position.xy * zoom, position.z, 1.);
  if (position.z < -.5) gl_Position = vec4(2., 2., 2., 1.); // the HUD's: not in the world's picture
  vColor = color;
  vTex = tex;
}`;
// (The marks in a colour are lens-host.mjs's: red over 1.5 is seen through,
// green over 3.5 is a screen's pixel, blue over 7.5 wears a texture.)
const WORLD_FRAGMENT = `#version 300 es
precision highp float;
precision highp sampler2DArray;
in vec3 vColor;
in vec4 vTex;
uniform sampler2DArray pictures;
uniform vec2 scales[${TEXTURE_LAYERS}];
layout(location = 0) out vec4 outColor;
layout(location = 1) out vec4 outScreen;
void main() {
  vec3 c = vColor;
  float screen = 0.;
  bool textured = c.b > 7.5;
  if (textured) {
    c.b -= 8.;
    vec2 uv = vTex.xy / max(vTex.z, 1e-6);
    int at = clamp(int(vTex.w + .5), 0, ${TEXTURE_LAYERS - 1});
    // (0 … 1 is the picture once, edges included; past it, it repeats.)
    vec2 t = uv;
    if (any(lessThan(uv, vec2(-.001))) || any(greaterThan(uv, vec2(1.001)))) t = fract(uv);
    t = clamp(t, vec2(0.), vec2(1.));
    vec4 texel = textureLod(pictures, vec3(t * scales[at], float(at)), 0.);
    if (texel.a < .5) discard;
    c *= texel.rgb;
  }
  if (c.g > 3.5) { c.g -= 4.; screen = 1.; }
  outScreen = vec4(screen, 0., 0., 1.);
  outColor = vec4(c, 1.);
  if (c.r > 1.5) outColor = vec4(c.r - 2., c.gb, .42);
}`;
// 🌑 The shade: where the stencil's count is not zero, the picture's fourth number is put out.
const MARK_VERTEX = `#version 300 es
layout(location = 0) in vec3 position;
void main() { gl_Position = vec4(position, 1.); }`;
const MARK_FRAGMENT = `#version 300 es
precision highp float;
layout(location = 0) out vec4 outColor;
void main() { outColor = vec4(0.); }`;
const HUD_VERTEX = `#version 300 es
layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
out vec3 vColor;
void main() {
  gl_Position = vec4(position.xy, position.z + 1., 1.);
  if (position.z >= -.5) gl_Position = vec4(2., 2., 2., 1.); // the world's: already on the screen
  vColor = color;
}`;
const HUD_FRAGMENT = `#version 300 es
precision highp float;
in vec3 vColor;
out vec4 outColor;
void main() { outColor = vec4(vColor.r, vColor.g > 3.5 ? vColor.g - 4. : vColor.g, vColor.b, 1.); }`;
const LENS_VERTEX = `#version 300 es
out vec2 at;
void main() {
  vec2 corner = vec2(float((gl_VertexID << 1) & 2), float(gl_VertexID & 2)) * 2. - 1.;
  gl_Position = vec4(corner, 0., 1.);
  at = corner; // the screen, -1 to 1, y up
}`;
// (lens-host.mjs LENS, line for line; the names there are the names here.)
const LENS_FRAGMENT = `#version 300 es
precision highp float;
in vec2 at;
uniform float sx, sy, wide, lens, fovea, idle, dither, time, glaze, height;
uniform vec3 shade;
uniform sampler2D picture;     // the whole picture, read as the screen wants it (smooth when it is drawn finer)
uniform sampler2D middle;      // its middle, larger
uniform sampler2D pictureLit;  // the same two, read one texel at a time for the shadows' mark
uniform sampler2D middleLit;
uniform sampler2D farMask;     // 📺 where the screens are
uniform sampler2D middleMask;
out vec4 outColor;
vec2 turned(vec2 p) { return vec2(p.x, 1. - p.y); } // a picture's first row is its bottom one here
float lit(bool within, vec2 p) { return within ? textureLod(middleLit, turned(p), 0.).a : textureLod(pictureLit, turned(p), 0.).a; }
vec3 look(vec2 p, bool within) { return within ? textureLod(middle, turned(p), 0.).rgb : textureLod(picture, turned(p), 0.).rgb; }
float masked(vec2 p, bool within) { return within ? textureLod(middleMask, turned(p), 0.).r : textureLod(farMask, turned(p), 0.).r; }
float hash(vec2 p) {
  vec3 v = fract(vec3(p.xyx) * .1031);
  v += dot(v, v.yzx + 33.33);
  return fract((v.x + v.y) * v.z);
}
vec3 glazed(vec2 p, vec2 px, bool within, vec2 pos, vec3 here) {
  float band = floor(pos.y / 3.);
  float slip = (hash(vec2(floor(time * 30.), band)) - .5) * .35 * px.x;
  vec2 a = p + vec2(slip, 0.);
  vec2 reach = vec2(.8 * px.x, 0.);
  vec3 smear = (2. * look(a, within) + look(a - reach, within) + look(a + reach, within)) * .25;
  vec2 bleed = vec2(.7 * px.x, 0.);
  vec3 c = mix(smear, vec3(look(a - bleed, within).r, smear.g, look(a + bleed, within).b), .65);
  c = mix(vec3(dot(c, vec3(.299, .587, .114))), c, 1.25);
  c = mix(c, c * c * (3. - 2. * c), .45);
  c *= 1. - .07 * step(.5, fract(pos.y / 3.));
  c += (hash(pos + fract(time * 7.) * 289.) - .5) * .05;
  return clamp(c, vec3(0.), vec3(1.));
}
vec3 spill(vec2 p, vec2 px, bool within) {
  vec3 sum = vec3(0.);
  for (int i = 0; i < 8; i++) {
    float angle = float(i) * .785398 + .3;
    vec2 d = vec2(cos(angle), sin(angle)) * px * (i % 2 == 1 ? 6. : 3.);
    sum += look(p + d, within) * masked(p + d, within);
  }
  return sum / 8.;
}
vec3 graded(vec3 c, vec2 p) {
  float y = dot(c, vec3(.299, .587, .114));
  vec3 g = mix(vec3(y), c, .88);
  g = g * mix(vec3(.96, .99, 1.05), vec3(1.04, 1.0, .95), smoothstep(.15, .85, y));
  g = .035 + g * .965;
  float r = length(p * vec2(.9, 1.));
  g *= 1. - .22 * smoothstep(.55, 1.45, r);
  return clamp(g, vec3(0.), vec3(1.));
}
const int bayer[16] = int[16](0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5);
void main() {
  vec2 pos = vec2(gl_FragCoord.x, height - gl_FragCoord.y); // counted from the top, as the lines and the grain are
  // Where this pixel is, as a tangent off the middle of the lensed picture.
  vec2 q = at / vec2(sx, sy);
  float r = length(q);
  // How far out the flat picture has the same thing: t, where r = t k(t).
  float t = r;
  if (lens > 0. && r > 0.) {
    for (int n = 0; n < 6; n++) {
      float s = sqrt(1. + t * t);
      float k = 1. + (2. / (1. + s) - 1.) * lens;
      float dk = (-2. * t) / (s * (1. + s) * (1. + s)) * lens;
      t = t - (t * k - r) / (k + t * dk);
    }
  }
  vec2 straight = r > 0. ? q * (t / r) : q; // (the flat picture's place)
  // The texture's picture is the screen's flat one, at wide times the scale.
  vec2 ndc = straight * vec2(sx, sy) * wide;
  vec2 farAt = clamp(vec2(ndc.x * .5 + .5, .5 - ndc.y * .5), vec2(0.), vec2(1.));
  vec4 far = textureLod(picture, turned(farAt), 0.);
  // 👁️ Near the middle the sharp texture has it (a texel in from its edge, so none of its border is read).
  vec2 inner = ndc / max(fovea, .0001);
  vec2 sharpAt = clamp(vec2(inner.x * .5 + .5, .5 - inner.y * .5), vec2(0.), vec2(1.));
  vec4 sharp = textureLod(middle, turned(sharpAt), 0.);
  bool within = fovea > 0. && max(abs(inner.x), abs(inner.y)) < .985;
  float light = lit(within, within ? sharpAt : farAt);
  vec3 rgb = within ? sharp.rgb : far.rgb;
  // (Taken outside the branches: a slope asked for inside one is not promised to be right.)
  vec2 farWidth = fwidth(farAt);
  if (glaze > 0.) {
    // 📺 The screens and the world, glazed apart.
    vec2 p = within ? sharpAt : farAt;
    vec2 px = farWidth * (within ? 1. / max(fovea, .0001) : 1.);
    float screen = within ? textureLod(middleMask, turned(sharpAt), 0.).r : textureLod(farMask, turned(farAt), 0.).r;
    if (screen > .5) {
      rgb = mix(rgb, glazed(p, px, within, pos, rgb), glaze);
    } else {
      rgb = rgb * mix(shade, vec3(1.), light);
      rgb = mix(rgb, graded(rgb, at), glaze);
    }
    // The screens' light falls a little way out over what is round them.
    rgb += spill(p, px, within) * .3 * glaze;
  } else {
    rgb *= mix(shade, vec3(1.), light);
  }
  if (idle > 0.) {
    vec2 p = within ? sharpAt : farAt;
    vec2 px = within ? 1. / vec2(textureSize(middle, 0)) : 1. / vec2(textureSize(picture, 0));
    vec3 neighbors = (look(p + vec2(px.x, 0.), within) + look(p - vec2(px.x, 0.), within)
      + look(p + vec2(0., px.y), within) + look(p - vec2(0., px.y), within)) * .25;
    vec3 detail = clamp((within ? sharp.rgb : far.rgb) - neighbors, vec3(-.1), vec3(.1));
    float edgeShade = 1. - .075 * smoothstep(.2, 1.3, length(at));
    rgb = mix(rgb, (rgb + .32 * detail) * edgeShade, idle);
  }
  // Quantize only mixed edge pixels. The ordered pattern stays on the paper grid.
  ivec2 cell = ivec2(pos) % 4;
  float threshold = (float(bayer[cell.y * 4 + cell.x]) + 0.5) / 16.;
  float edge = clamp(length(fwidth(rgb)) * 6., 0., 1.);
  outColor = vec4(mix(rgb, floor(rgb * 31. + threshold) / 31., edge * dither), 1.);
}`;

function program(gl, vertex, fragment, names) {
  const made = gl.createProgram();
  for (const [type, source] of [[gl.VERTEX_SHADER, vertex], [gl.FRAGMENT_SHADER, fragment]]) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) throw new Error(`WebGL 2 shader: ${gl.getShaderInfoLog(shader)}`);
    gl.attachShader(made, shader);
    gl.deleteShader(shader);
  }
  gl.linkProgram(made);
  if (!gl.getProgramParameter(made, gl.LINK_STATUS)) throw new Error(`WebGL 2 program: ${gl.getProgramInfoLog(made)}`);
  return { made, at: Object.fromEntries(names.map((name) => [name, gl.getUniformLocation(made, name)])) };
}

export class LensScene3DGL {
  static async create(canvas, options = {}) {
    // (No antialias: the lens does its own. No alpha: the page behind never shows.)
    const gl = canvas.getContext('webgl2', { alpha: false, antialias: false, depth: true, stencil: false, powerPreference: 'high-performance', preserveDrawingBuffer: false });
    if (!gl) throw new Error('WebGL 2 unavailable');
    return new LensScene3DGL(canvas, gl, options);
  }
  constructor(canvas, gl, options = {}) {
    this.canvas = canvas;
    this.gl = gl;
    this.scene = new OskiewarScene3D(options);
    this.shadows = !!options.shadows;
    const stored = (key) => { try { return localStorage.getItem(key); } catch { return null; } };
    this.glaze = stored('rax:glaze') === 'off' ? 0 : 1;
    this.max = gl.getParameter(gl.MAX_TEXTURE_SIZE);
    this.worldProgram = program(gl, WORLD_VERTEX, WORLD_FRAGMENT, ['zoom', 'pictures', 'scales']);
    this.markProgram = program(gl, MARK_VERTEX, MARK_FRAGMENT, []);
    this.hudProgram = program(gl, HUD_VERTEX, HUD_FRAGMENT, []);
    this.lensProgram = program(gl, LENS_VERTEX, LENS_FRAGMENT, ['sx', 'sy', 'wide', 'lens', 'fovea', 'idle', 'dither', 'time', 'glaze', 'height', 'shade',
      'picture', 'middle', 'pictureLit', 'middleLit', 'farMask', 'middleMask']);
    // The stream (place and colour) and, for the world's shapes, where each corner lies on its texture.
    this.scene.uvs = new Float32Array(this.scene.maxTriangles * 12);
    this.buffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);
    gl.bufferData(gl.ARRAY_BUFFER, this.scene.vertices.byteLength, gl.DYNAMIC_DRAW);
    this.uvBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, this.uvBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, this.scene.uvs.byteLength, gl.DYNAMIC_DRAW);
    this.cover = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, this.cover);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, SHADOW_NEAR, 3, -1, SHADOW_NEAR, -1, 3, SHADOW_NEAR]), gl.STATIC_DRAW);
    const stream = (withUvs) => {
      const vao = gl.createVertexArray();
      gl.bindVertexArray(vao);
      gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);
      gl.enableVertexAttribArray(0); gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 24, 0);
      gl.enableVertexAttribArray(1); gl.vertexAttribPointer(1, 3, gl.FLOAT, false, 24, 12);
      if (withUvs) { gl.bindBuffer(gl.ARRAY_BUFFER, this.uvBuffer); gl.enableVertexAttribArray(2); gl.vertexAttribPointer(2, 4, gl.FLOAT, false, 16, 0); }
      return vao;
    };
    this.worldArray = stream(true);
    this.hudArray = stream(false);
    this.coverArray = gl.createVertexArray();
    gl.bindVertexArray(this.coverArray);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.cover);
    gl.enableVertexAttribArray(0); gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 12, 0);
    this.noArray = gl.createVertexArray(); // (the lens's triangle is made from its corners' numbers)
    gl.bindVertexArray(null);
    // 🖼️ The pictures laid on shapes: one array of square layers.
    this.pictures = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, this.pictures);
    gl.texStorage3D(gl.TEXTURE_2D_ARRAY, 1, gl.RGBA8, TEXTURE_SIZE, TEXTURE_SIZE, TEXTURE_LAYERS);
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_MAG_FILTER, gl.NEAREST); // (pixels stay pixels up close)
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    this.scaleData = new Float32Array(TEXTURE_LAYERS * 2).fill(1);
    this.scalesDirty = true;
    const sampler = (filter) => {
      const made = gl.createSampler();
      gl.samplerParameteri(made, gl.TEXTURE_MAG_FILTER, filter); gl.samplerParameteri(made, gl.TEXTURE_MIN_FILTER, filter);
      gl.samplerParameteri(made, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE); gl.samplerParameteri(made, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      return made;
    };
    this.sampler = sampler(gl.NEAREST);
    this.smooth = sampler(gl.LINEAR); // for a picture drawn finer than it is shown (aa)
    // Two pictures of the world: the whole of it, and its middle larger.
    this.layers = [{}, {}];
    // 🧊 The card's side of a frame, as view.mjs asks it of the WebGPU one.
    this.fences = [];
    this.device = { queue: { onSubmittedWorkDone: () => this.settled() } };
    this.lost = (event) => { event.preventDefault(); options.onDeviceLost?.({ reason: 'unknown', message: 'WebGL 2 context lost' }); };
    canvas.addEventListener('webglcontextlost', this.lost);
  }
  // A promise kept when the card has finished all it has been handed so far.
  settled() {
    const { gl } = this;
    const sync = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
    gl.flush();
    return new Promise((resolve) => { this.fences.push({ sync, resolve }); this.watch(); });
  }
  watch() {
    if (this.watching) return;
    const { gl } = this;
    const look = () => {
      this.watching = null;
      this.fences = this.fences.filter(({ sync, resolve }) => {
        const state = gl.isContextLost() ? gl.ALREADY_SIGNALED : gl.clientWaitSync(sync, 0, 0);
        if (state === gl.TIMEOUT_EXPIRED) return true;
        gl.deleteSync(sync); resolve();
        return false;
      });
      if (this.fences.length) this.watching = setTimeout(look, 2);
    };
    this.watching = setTimeout(look, 0);
  }
  resize(width, height) {
    width = Math.min(this.max, Math.max(1, Math.round(width)));
    height = Math.min(this.max, Math.max(1, Math.round(height)));
    if (this.canvas.width === width && this.canvas.height === height) return;
    this.canvas.width = width;
    this.canvas.height = height;
  }
  beginFrame() { this.scene.beginFrame(); }
  triangle(...values) { return this.scene.triangle(...values); }
  // The pictures the world is drawn into, each with its own depth: made again when a size changes.
  room(sizes) {
    const { gl } = this;
    this.layers.forEach((layer, n) => {
      const w = Math.min(this.max, Math.max(1, Math.round(this.canvas.width * sizes[n]))), h = Math.min(this.max, Math.max(1, Math.round(this.canvas.height * sizes[n])));
      if (layer.frame && layer.width === w && layer.height === h) return;
      gl.deleteTexture(layer.picture || null); gl.deleteTexture(layer.mask || null); gl.deleteRenderbuffer(layer.depth || null); gl.deleteFramebuffer(layer.frame || null);
      layer.width = w; layer.height = h;
      const flat = (format) => {
        const made = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, made);
        gl.texStorage2D(gl.TEXTURE_2D, 1, format, w, h);
        return made;
      };
      layer.picture = flat(gl.RGBA8);
      layer.mask = flat(gl.R8); // 📺 where the screens are
      layer.depth = gl.createRenderbuffer();
      gl.bindRenderbuffer(gl.RENDERBUFFER, layer.depth);
      gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH24_STENCIL8, w, h);
      layer.frame = gl.createFramebuffer();
      gl.bindFramebuffer(gl.FRAMEBUFFER, layer.frame);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, layer.picture, 0);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT1, gl.TEXTURE_2D, layer.mask, 0);
      gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_STENCIL_ATTACHMENT, gl.RENDERBUFFER, layer.depth);
      if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE && !gl.isContextLost()) throw new Error('WebGL 2 picture incomplete');
    });
  }
  // (lens-host.mjs `present`: the same asks, the same answers.)
  present({ clear = [0, 0, 0, 1], shadow = null, lens = null } = {}) {
    const { gl, scene } = this;
    if (gl.isContextLost()) return;
    const l = lens || { sx: 1, sy: 1, wide: 1, lens: 0, over: 1, fovea: 0 };
    const aa = Math.max(1, Math.min(3, Math.round(l.aa || 1)));
    const over = Math.max(1, Math.min(2, l.over || 1)) * aa, fovea = l.fovea > 0 && l.fovea < 1 ? l.fovea : 0;
    this.room(fovea ? [over * 0.5, over * fovea] : [over, 1 / Math.max(this.canvas.width, this.canvas.height)]);
    if (scene.triangleCount) {
      gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);
      gl.bufferSubData(gl.ARRAY_BUFFER, 0, scene.frameVertices());
      gl.bindBuffer(gl.ARRAY_BUFFER, this.uvBuffer);
      gl.bufferSubData(gl.ARRAY_BUFFER, 0, scene.uvs, 0, scene.triangleCount * 12);
    }
    const cast = this.shadows && shadow?.count > 0 && shadow.first + shadow.count === scene.triangleCount ? shadow : null;
    const drawn = cast ? cast.first : scene.triangleCount;
    gl.disable(gl.CULL_FACE); gl.disable(gl.SCISSOR_TEST); gl.disable(gl.DITHER);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindSampler(0, null);
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, this.pictures);
    gl.useProgram(this.worldProgram.made);
    gl.uniform1i(this.worldProgram.at.pictures, 0);
    if (this.scalesDirty) { gl.uniform2fv(this.worldProgram.at.scales, this.scaleData); this.scalesDirty = false; }
    // 1. The world, flat and wide, with its shadows: once whole, once its middle.
    const zooms = [1, fovea ? 1 / fovea : 1];
    (fovea ? this.layers : [this.layers[0]]).forEach((layer, n) => {
      gl.bindFramebuffer(gl.FRAMEBUFFER, layer.frame);
      gl.viewport(0, 0, layer.width, layer.height);
      gl.drawBuffers([gl.COLOR_ATTACHMENT0, gl.COLOR_ATTACHMENT1]);
      gl.colorMask(true, true, true, true); gl.depthMask(true); gl.stencilMask(0xff);
      gl.clearBufferfv(gl.COLOR, 0, clear);
      gl.clearBufferfv(gl.COLOR, 1, [0, 0, 0, 0]);
      gl.clearBufferfi(gl.DEPTH_STENCIL, 0, 1, 0);
      gl.enable(gl.DEPTH_TEST); gl.depthFunc(gl.LEQUAL); gl.disable(gl.STENCIL_TEST);
      gl.enable(gl.BLEND); gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
      gl.useProgram(this.worldProgram.made);
      gl.uniform1f(this.worldProgram.at.zoom, zooms[n]);
      gl.bindVertexArray(this.worldArray);
      if (drawn) gl.drawArrays(gl.TRIANGLES, 0, drawn * 3);
      if (cast) {
        // The shadows' solids: no colour, no depth written; a face behind the scene counts up or down by which way it faces.
        gl.disable(gl.BLEND);
        gl.colorMask(false, false, false, false); gl.depthMask(false); gl.depthFunc(gl.LESS);
        gl.enable(gl.STENCIL_TEST); gl.stencilFunc(gl.ALWAYS, 0, 0xff);
        gl.stencilOpSeparate(gl.FRONT, gl.KEEP, gl.DECR_WRAP, gl.KEEP);
        gl.stencilOpSeparate(gl.BACK, gl.KEEP, gl.INCR_WRAP, gl.KEEP);
        gl.drawArrays(gl.TRIANGLES, cast.first * 3, cast.count * 3);
        // The shade: one triangle over the picture, marking it wherever the count is not zero.
        gl.drawBuffers([gl.COLOR_ATTACHMENT0, gl.NONE]);
        gl.colorMask(false, false, false, true); gl.stencilMask(0);
        gl.stencilFunc(gl.NOTEQUAL, 0, 0xff); gl.stencilOp(gl.KEEP, gl.KEEP, gl.KEEP);
        gl.useProgram(this.markProgram.made);
        gl.bindVertexArray(this.coverArray);
        gl.drawArrays(gl.TRIANGLES, 0, 3);
        gl.disable(gl.STENCIL_TEST); gl.stencilMask(0xff);
      }
    });
    // 2. Through the lens to the screen, then the HUD over it, straight.
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    gl.colorMask(true, true, true, true); gl.depthMask(true);
    gl.clearColor(clear[0], clear[1], clear[2], 1); gl.clearDepth(1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.disable(gl.BLEND); gl.disable(gl.DEPTH_TEST);
    const u = this.lensProgram.at;
    gl.useProgram(this.lensProgram.made);
    gl.uniform1f(u.sx, l.sx); gl.uniform1f(u.sy, l.sy); gl.uniform1f(u.wide, l.wide); gl.uniform1f(u.lens, l.lens);
    gl.uniform1f(u.fovea, fovea); gl.uniform1f(u.idle, l.idle || 0); gl.uniform1f(u.dither, l.dither || 0);
    gl.uniform1f(u.time, (performance.now() / 1000) % 1000); gl.uniform1f(u.glaze, l.glaze ?? this.glaze);
    gl.uniform1f(u.height, this.canvas.height);
    gl.uniform3fv(u.shade, shadow?.shade || [1, 1, 1]);
    const shown = aa > 1 ? this.smooth : this.sampler;
    [[u.picture, this.layers[0].picture, shown], [u.middle, this.layers[1].picture, shown], [u.pictureLit, this.layers[0].picture, this.sampler],
      [u.middleLit, this.layers[1].picture, this.sampler], [u.farMask, this.layers[0].mask, shown], [u.middleMask, this.layers[1].mask, shown]].forEach(([at, texture, sampler], unit) => {
      gl.activeTexture(gl.TEXTURE0 + unit);
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.bindSampler(unit, sampler);
      gl.uniform1i(at, unit);
    });
    gl.bindVertexArray(this.noArray);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    for (let unit = 0; unit < 6; unit++) { gl.activeTexture(gl.TEXTURE0 + unit); gl.bindTexture(gl.TEXTURE_2D, null); gl.bindSampler(unit, null); }
    if (drawn) {
      gl.enable(gl.DEPTH_TEST); gl.depthFunc(gl.LEQUAL);
      gl.useProgram(this.hudProgram.made);
      gl.bindVertexArray(this.hudArray);
      gl.drawArrays(gl.TRIANGLES, 0, drawn * 3);
    }
    gl.bindVertexArray(null);
  }
  // 🖼️ Lay a picture on texture layer `layer` (lens-host.mjs `texture`): its first row is the layer's first row, as there.
  texture(layer, picture) {
    const { gl } = this;
    if (!(layer >= 0 && layer < TEXTURE_LAYERS) || !picture) return 0;
    const width = Math.min(TEXTURE_SIZE, picture.width | 0), height = Math.min(TEXTURE_SIZE, picture.height | 0);
    if (!width || !height) return 0;
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, this.pictures);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false); gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, false);
    gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
    if (picture.pixels) {
      const bytes = picture.pixels instanceof Uint8Array ? picture.pixels : new Uint8Array(picture.pixels.buffer, picture.pixels.byteOffset, picture.pixels.byteLength);
      gl.pixelStorei(gl.UNPACK_ROW_LENGTH, picture.width | 0);
      gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, 0, 0, 0, layer, width, height, 1, gl.RGBA, gl.UNSIGNED_BYTE, bytes);
      gl.pixelStorei(gl.UNPACK_ROW_LENGTH, 0);
    } else {
      gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, 0, 0, 0, layer, width, height, 1, gl.RGBA, gl.UNSIGNED_BYTE, picture);
    }
    const at = layer * 2;
    if (this.scaleData[at] !== width / TEXTURE_SIZE || this.scaleData[at + 1] !== height / TEXTURE_SIZE) {
      this.scaleData[at] = width / TEXTURE_SIZE; this.scaleData[at + 1] = height / TEXTURE_SIZE; this.scalesDirty = true;
    }
    return width * height * 4;
  }
  destroy() {
    const { gl } = this;
    this.canvas.removeEventListener('webglcontextlost', this.lost);
    clearTimeout(this.watching);
    for (const { resolve } of this.fences) resolve();
    this.fences = [];
    for (const layer of this.layers) { gl.deleteTexture(layer.picture || null); gl.deleteTexture(layer.mask || null); gl.deleteRenderbuffer(layer.depth || null); gl.deleteFramebuffer(layer.frame || null); }
    gl.deleteTexture(this.pictures);
    for (const buffer of [this.buffer, this.uvBuffer, this.cover]) gl.deleteBuffer(buffer);
    gl.getExtension('WEBGL_lose_context')?.loseContext();
  }
}
