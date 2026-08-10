import { OskiewarScene3D } from "./scene3d.mjs";

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
