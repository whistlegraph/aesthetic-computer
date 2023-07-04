// 2D (GPU)
// Render the aesthetic.computer 2D display system on the GPU
// (WebGL2)

// let send; // Send messages pack to aeshetic.computer.
let gl;

// Starts the renderer.
function initialize(wrapper /*, sendToPiece*/) {
  // send = sendToPiece;

  // Initialize WebGL2.
  const can = document.createElement("canvas");

  gl = can.getContext("webgl2", {
    alpha: false,
    depth: false,
    stencil: false,
    desynchronized: true,
    antialias: false,
  });

  gl.enable(gl.BLEND);
  gl.blendEquation(gl.FUNC_ADD);
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

  wrapper.append(can); // And add the canvas to the DOM.
}

// Resizes the textures & re-initializes the necessary components for a resolution change.
// See also: `frame` via window.resize in `bios.js`.
function frame(w, h, rect, nativeWidth, nativeHeight, wrapper) {}

// Kills the renderer.
function kill() {}

// Packs a frame with data.
function pack() {
  // Batch together a list of draw calls.
  // Cache textures, etc. make special cases as needed.
}

// Makes draw calls.
function render() {
  // Loop through a batched list and make distinct draw calls as needed.
}

// Receive events from aesthetic.computer
function handleEvent() {}

export { initialize, frame, kill, pack, render, handleEvent };
