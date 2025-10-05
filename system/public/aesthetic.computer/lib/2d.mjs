// 2D (GPU)
// Render the aesthetic.computer 2D display system on the GPU
// (WebGL2)

import { createShader, createProgram, preloadShaders } from "./gl.mjs";
import * as vec2 from "../dep/gl-matrix/vec2.mjs";
import { font1 } from "../disks/common/fonts.mjs";

// let send; // Send messages pack to aeshetic.computer.
let gl;
let packed = [];
let shaders;
let currentBlendMode = null;

// Starts the renderer.
async function initialize(wrapper /*, sendToPiece*/) {
  // send = sendToPiece;

  // Initialize WebGL2.
  const can = document.createElement("canvas");
  can.dataset.type = "2d";

  gl = can.getContext("webgl2", {
    alpha: true,
    depth: false,
    stencil: false,
    desynchronized: true,
    antialias: false,
  });

  gl.enable(gl.BLEND);
  gl.blendEquation(gl.FUNC_ADD);
  gl.blendFuncSeparate(
    gl.SRC_ALPHA,
    gl.ONE_MINUS_SRC_ALPHA,
    gl.SRC_ALPHA,
    gl.ONE_MINUS_SRC_ALPHA,
  );
  currentBlendMode = "draw";

  // Load all the shaders for the renderer.
  const sources = await preloadShaders([
    "./shaders/pass-vert",
    "./shaders/pass-frag",
  ]);

  // Compile all the shader programs for our pipeline.
  const pvert = createShader(gl, gl.VERTEX_SHADER, sources["pass-vert"]);
  const pfrag = createShader(gl, gl.FRAGMENT_SHADER, sources["pass-frag"]);
  const program = createProgram(gl, pvert, pfrag);

  shaders = {
    line: {
      program,
      attributes: {},
      uniforms: {},
      buffers: {},
      other: {}, // For holding stuff like `vao` or other extra state.
    },
  };

  const line = shaders.line;

  line.attributes.pos = gl.getAttribLocation(line.program, "pos");
  line.attributes.color = gl.getAttribLocation(line.program, "color");
  line.uniforms.res = gl.getUniformLocation(line.program, "res");

  // Set up the data buffer.
  line.buffers.data = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, line.buffers.data);

  line.other.vao = gl.createVertexArray();
  gl.bindVertexArray(line.other.vao);

  // Helper for setting up interleaved attribute pointers.
  function pointer({ attribute, size, type, normalize, stride, offset }) {
    gl.enableVertexAttribArray(attribute);
    const bytes = Float32Array.BYTES_PER_ELEMENT;
    stride *= bytes;
    offset *= bytes;
    gl.vertexAttribPointer(attribute, size, type, normalize, stride, offset); // Array buffer has been bound.
  }

  pointer({
    attribute: line.attributes.pos,
    size: 2,
    type: gl.FLOAT,
    normalize: false,
    stride: 6,
    offset: 0,
  });

  pointer({
    attribute: line.attributes.color,
    size: 4,
    type: gl.FLOAT,
    normalize: false,
    stride: 6,
    offset: 2,
  });
}

let ink = {
  color: [0, 0, 0, 1],
  erase: false,
};
let ink2 = null; // Secondary color.

function clamp01(value) {
  if (value <= 0) return 0;
  if (value >= 1) return 1;
  return value;
}

function channelToUnit(value, fallback) {
  if (value === undefined) return fallback;
  const num = Number(value);
  if (!Number.isFinite(num)) return fallback;
  return clamp01(num / 255);
}

function normalizeColor(color) {
  const array = Array.isArray(color) ? color : [color];
  if (array.length === 0) {
    return [0, 0, 0, 1];
  }

  const r = channelToUnit(array[0], 0);
  const g = channelToUnit(array.length > 1 ? array[1] : array[0], r);
  const b = channelToUnit(array.length > 2 ? array[2] : array[0], r);
  const a = channelToUnit(array.length > 3 ? array[3] : 255, 1);

  return [r, g, b, a];
}

function parseInk(rawParams) {
  const array = Array.isArray(rawParams) ? rawParams : [rawParams];
  const sample = array.slice(0, 3).map((channel) => Number(channel));
  const hasThreeChannels = sample.length === 3 && sample.every(Number.isFinite);
  const isErase = hasThreeChannels && sample.every((channel) => channel <= 0);

  return {
    color: normalizeColor(array),
    erase: isErase,
  };
}

function setBlendMode(mode) {
  if (!gl || mode === currentBlendMode) {
    return;
  }

  if (mode === "erase") {
    gl.blendFuncSeparate(
      gl.ZERO,
      gl.ONE_MINUS_SRC_ALPHA,
      gl.ZERO,
      gl.ONE_MINUS_SRC_ALPHA,
    );
  } else {
    gl.blendFuncSeparate(
      gl.SRC_ALPHA,
      gl.ONE_MINUS_SRC_ALPHA,
      gl.SRC_ALPHA,
      gl.ONE_MINUS_SRC_ALPHA,
    );
  }

  currentBlendMode = mode;
}

// Packs a frame with data.
// (Interpreting a list of paint commands from a piece, per render frame.)
function pack(content) {
  content.code.forEach((statement) => {
    const name = statement[0];
    const params = statement.slice(1);
    if (name === "wipe") {
      packed.push({
        wipe: normalizeColor(params),
      });
    } else if (name === "ink") {
      ink = parseInk(params);
      if (ink.erase) {
        ink2 = null;
      }
    } else if (name === "ink2") {
      ink2 = normalizeColor(params);
    } else if (name === "line") {
      const p1 = params.slice(0, 2);
      const p2 = params.slice(2, 4);
      const primaryColor = ink.color;
      const secondaryColor = !ink.erase && ink2 ? ink2 : primaryColor;
      packed.push({
        line: new Float32Array([...p1, ...primaryColor, ...p2, ...secondaryColor]),
        erase: ink.erase,
      });
    } else if (name === "point") {
      const p = params.slice(0, 2);
      packed.push({
        point: new Float32Array([...p, ...ink.color]),
        erase: ink.erase,
      });
    } else if (name === "text") {
      const [x, y, text] = params;
      // Draw text using lines
      const size = 20; // Text size
      const width = size * 0.6; // Character width
      text
        .toString()
        .split("")
        .forEach((char, i) => {
          const cx = x + i * width;
        });
        // TODO: Map the character data in the glyph json.
    }
  });
}

// Makes draw calls based on a packed frame.
function render() {
  if (!packed || !shaders || !gl) return;

  // Render lines (and points).
  const line = shaders.line;
  gl.useProgram(line.program);
  gl.bindVertexArray(line.other.vao);

  packed.forEach((packItem) => {
    if (packItem.wipe) {
      setBlendMode("draw");
      gl.clearColor(...packItem.wipe);
      gl.clear(gl.COLOR_BUFFER_BIT);
    }

    // Lines and Points
    if (packItem.line || packItem.point) {
      setBlendMode(packItem.erase ? "erase" : "draw");

      const primitive = packItem.line || packItem.point;
      gl.bindBuffer(gl.ARRAY_BUFFER, line.buffers.data);
      gl.bufferData(gl.ARRAY_BUFFER, primitive, gl.DYNAMIC_DRAW);
      gl.uniform2f(line.uniforms.res, gl.canvas.width, gl.canvas.height);
      const primitiveType = packItem.line ? gl.LINES : gl.POINTS;
      const offset = 0;
      const count = primitive.length / 6;
      gl.drawArrays(primitiveType, offset, count);
    }

    // Rectangles
    // ...

    // Polygons / Triangles
    // ...

    // Bitmaps
    // ...
  });

  packed.length = 0;
  setBlendMode("draw");
}

// Resizes the textures & re-initializes the necessary components for a
// resolution (res) change.
// See also: `frame` via window.resize in `bios.js`.
function frame(w, h, wrapper) {
  gl.canvas.width = w;
  gl.canvas.height = h;
  gl.canvas.style.width = wrapper.style.width;
  gl.canvas.style.height = wrapper.style.height;
  gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
  gl.clearColor(0.0, 0.0, 0.0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);
  if (!wrapper.contains(gl.canvas)) wrapper.append(gl.canvas);
}

export { initialize, frame, pack, render };
