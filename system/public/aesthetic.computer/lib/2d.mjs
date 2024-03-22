// 2D (GPU)
// Render the aesthetic.computer 2D display system on the GPU
// (WebGL2)

import { createShader, createProgram, preloadShaders } from "./gl.mjs";

// let send; // Send messages pack to aeshetic.computer.
let gl;
let packed;
let shaders;

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
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

  // Load all the shaders for the renderer.
  const sources = await preloadShaders([
    "./shaders/pass-vert",
    "./shaders/pass-frag",
  ]);

  // Compile all the shader programs for our pipeline.
  const pvert = createShader(gl, gl.VERTEX_SHADER, sources["pass-vert"]);
  const pfrag = createShader(gl, gl.FRAGMENT_SHADER, sources["pass-frag"]);
  const pprogram = createProgram(gl, pvert, pfrag);

  shaders = {
    line: {
      program: pprogram,
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

  // Set up another buffer as needed...
  // shaders.screen.buffers.color = gl.createBuffer();
  // gl.bindBuffer(gl.ARRAY_BUFFER, shaders.screen.buffers.color);
}

let ink = [0, 0, 0, 1.0];
let ink2 = ink;

function convertColor(color) {
  let out;
  if (Array.isArray(color)) {
    out = color.map((c) => c / 255);
  } else {
    // Assume a single integer.
    const c = color / 255;
    out = [c, c, c, 1];
  }

  return out;
}

// Packs a frame with data.
// (Interpreting a list of paint commands from a piece.)
// TBD: Cache textures, etc. make special cases as needed.

function pack(content) {
  packed = [];
  content.code.forEach((statement) => {
    const name = statement[0];
    const params = statement.slice(1);
    if (name === "wipe") {
      packed.push({
        wipe: convertColor(params[0]),
      });
    } else if (name === "ink") {
      ink = convertColor(params[0]);
    } else if (name === "ink2") {
      ink2 = params;
    } else if (name === "line") {
      const p1 = params.slice(0, 2);
      const p2 = params.slice(2, 4);
      packed.push({
        line: new Float32Array([...p1, ...ink, ...p2, ...(ink2 || ink)]),
      });
    }
  });
  ink2 = null;
}

// Makes draw calls.
function render() {
  if (!packed || !shaders) return;

  // packed.forEach((command, index) => {
    // const name = command[0];
    // if (command[0])
  // });

  // Render lines.
  const line = shaders.line;
  gl.useProgram(line.program);
  gl.bindVertexArray(line.other.vao);

  if (packed[0]?.line) {
    gl.bindBuffer(gl.ARRAY_BUFFER, line.buffers.data);
    gl.bufferData(gl.ARRAY_BUFFER, packed[0].line, gl.DYNAMIC_DRAW);
    gl.uniform2f(line.uniforms.res, gl.canvas.width, gl.canvas.height);

    const primitiveType = gl.LINES;
    const offset = 0;
    const count = packed[0].line.length / 6;
    gl.drawArrays(primitiveType, offset, count);
  }

  packed = null;
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
  gl.clearColor(1.0, 0.0, 0.0, 0.25);
  gl.clear(gl.COLOR_BUFFER_BIT);
  if (!wrapper.contains(gl.canvas)) wrapper.append(gl.canvas);
}

// Receive events from aesthetic.computer
// function handleEvent() {}

export { initialize, frame, pack, render };
