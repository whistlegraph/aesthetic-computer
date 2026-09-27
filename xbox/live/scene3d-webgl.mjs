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

function program(gl) {
  const vertex = compile(gl, gl.VERTEX_SHADER, vertexSource);
  const fragment = compile(gl, gl.FRAGMENT_SHADER, fragmentSource);
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

  beginFrame() { this.scene.beginFrame(); }

  triangle(...values) { return this.scene.triangle(...values); }

  present({ clear = [0, 0, 0, 0] } = {}) {
    const gl = this.gl;
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    gl.clearColor(...clear);
    gl.clearDepth(1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    if (!this.scene.triangleCount) return;
    gl.useProgram(this.program);
    gl.bindVertexArray(this.array);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer);
    gl.bufferSubData(gl.ARRAY_BUFFER, 0, this.scene.frameVertices());
    gl.drawArrays(gl.TRIANGLES, 0, this.scene.triangleCount * 3);
    gl.bindVertexArray(null);
  }

  destroy() {
    const gl = this.gl;
    gl.deleteBuffer(this.buffer);
    gl.deleteVertexArray(this.array);
    gl.deleteProgram(this.program);
  }
}

export default WebGLOskiewarScene3D;
