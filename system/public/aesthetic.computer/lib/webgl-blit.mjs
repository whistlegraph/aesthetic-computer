// 🎨 WebGL Blitter
// Uploads RGBA pixel buffers into a WebGL2 texture and renders a full-screen quad

import { log } from "./logs.mjs";

export function createWebGLBlitter(canvas) {
  let gl = null;
  let program = null;
  let vao = null;
  let vbo = null;
  let texture = null;
  let uResolutionLoc = null;
  let uTextureLoc = null;
  let currentWidth = 0;
  let currentHeight = 0;
  let ready = false;

  const vertexSource = `#version 300 es
    precision highp float;
    in vec2 a_position;
    out vec2 v_uv;
    void main() {
      v_uv = (a_position + 1.0) * 0.5;
      gl_Position = vec4(a_position, 0.0, 1.0);
    }
  `;

  const fragmentSource = `#version 300 es
    precision highp float;
    in vec2 v_uv;
    uniform sampler2D u_texture;
    out vec4 outColor;
    void main() {
      outColor = texture(u_texture, vec2(v_uv.x, 1.0 - v_uv.y));
    }
  `;

  function compileShader(source, type) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      const err = gl.getShaderInfoLog(shader);
      gl.deleteShader(shader);
      throw new Error(err || "Shader compile failed");
    }
    return shader;
  }

  function createProgram() {
    const vs = compileShader(vertexSource, gl.VERTEX_SHADER);
    const fs = compileShader(fragmentSource, gl.FRAGMENT_SHADER);
    const prog = gl.createProgram();
    gl.attachShader(prog, vs);
    gl.attachShader(prog, fs);
    gl.linkProgram(prog);
    gl.deleteShader(vs);
    gl.deleteShader(fs);
    if (!gl.getProgramParameter(prog, gl.LINK_STATUS)) {
      const err = gl.getProgramInfoLog(prog);
      gl.deleteProgram(prog);
      throw new Error(err || "Program link failed");
    }
    return prog;
  }

  function ensureTextureSize(width, height) {
    if (!texture) {
      texture = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    }

    if (width !== currentWidth || height !== currentHeight) {
      currentWidth = width;
      currentHeight = height;
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texImage2D(
        gl.TEXTURE_2D,
        0,
        gl.RGBA,
        width,
        height,
        0,
        gl.RGBA,
        gl.UNSIGNED_BYTE,
        null,
      );
    }
  }

  return {
    init() {
      gl = canvas.getContext("webgl2", {
        alpha: true, // Enable transparency for compositing with 3D canvas underneath
        antialias: false,
        depth: false,
        stencil: false,
        premultipliedAlpha: false,
        preserveDrawingBuffer: true, // Keep previous frame to prevent black flash during resize
        powerPreference: "high-performance",
      });

      if (!gl) {
        log.gpu.warn?.("WebGL Blitter: WebGL2 not supported");
        return false;
      }

      try {
        program = createProgram();
        vao = gl.createVertexArray();
        vbo = gl.createBuffer();

        gl.bindVertexArray(vao);
        gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
        gl.bufferData(
          gl.ARRAY_BUFFER,
          new Float32Array([
            -1, -1,
             1, -1,
            -1,  1,
            -1,  1,
             1, -1,
             1,  1,
          ]),
          gl.STATIC_DRAW,
        );

        const posLoc = gl.getAttribLocation(program, "a_position");
        gl.enableVertexAttribArray(posLoc);
        gl.vertexAttribPointer(posLoc, 2, gl.FLOAT, false, 0, 0);
        gl.bindVertexArray(null);

        uTextureLoc = gl.getUniformLocation(program, "u_texture");
        uResolutionLoc = gl.getUniformLocation(program, "u_resolution");

        gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
        gl.disable(gl.BLEND);

        ready = true;
        log.gpu.success?.("WebGL Blitter initialized");
        return true;
      } catch (err) {
        log.gpu.error?.("WebGL Blitter init failed:", err);
        ready = false;
        return false;
      }
    },

    isReady() {
      return ready;
    },

    render(imageData) {
      if (!ready || !imageData || !imageData.data) return false;

      const width = imageData.width;
      const height = imageData.height;
      if (!width || !height) return false;

      ensureTextureSize(width, height);

      gl.viewport(0, 0, width, height);
      gl.useProgram(program);
      gl.bindVertexArray(vao);
      gl.activeTexture(gl.TEXTURE0);
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texSubImage2D(
        gl.TEXTURE_2D,
        0,
        0,
        0,
        width,
        height,
        gl.RGBA,
        gl.UNSIGNED_BYTE,
        imageData.data,
      );
      gl.uniform1i(uTextureLoc, 0);
      if (uResolutionLoc) gl.uniform2f(uResolutionLoc, width, height);
      gl.drawArrays(gl.TRIANGLES, 0, 6);
      gl.bindVertexArray(null);
      return true;
    },

    destroy() {
      if (!gl) return;
      if (texture) gl.deleteTexture(texture);
      if (vbo) gl.deleteBuffer(vbo);
      if (vao) gl.deleteVertexArray(vao);
      if (program) gl.deleteProgram(program);
      gl = null;
      texture = null;
      vbo = null;
      vao = null;
      program = null;
      ready = false;
    },
  };
}
