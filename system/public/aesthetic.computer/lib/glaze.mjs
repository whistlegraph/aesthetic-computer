// ✨ Glaze 2022.02.06.15.09
// This creates a nice webgl2 rendering layer
// over the scaled software rasterizer.

// TODO: Rename the pipeline stages from frag->compute->display to something
//       that makes the most sense for @mxsage.

// TODO: Change `setCustomUniforms` to `setCustomFragUniforms` after renaming
//       frag to something else.

const { keys } = Object;

import glazes from "./glazes/uniforms.js";
import { createShader, createProgram, preloadShaders } from "./gl.mjs";
import { pathEnd, wrapNotArray } from "./helpers.mjs";

class Glaze {
  w;
  h;
  rect;

  loaded = false;
  shadersLoaded = false;
  uniformNames;
  frag;
  type;

  #uniforms;

  constructor(type = "prompt", w, h, rect) {
    this.w = w;
    this.h = h;
    this.rect = rect;
    this.type = type;
    this.#uniforms = glazes[type];
    this.uniformNames = keys(this.#uniforms).map((id) => id.split(":")[1]);
  }

  async load(callback) {
    // Before any glaze loads, make sure that the passthrough shader is loaded.
    // TODO: Technically this should load with the other shaders in parallel,
    //       and it would be faster.
    const names = [
      `./glazes/${this.type}/${this.type}-frag`,
      `./glazes/${this.type}/${this.type}-compute`,
      `./glazes/${this.type}/${this.type}-display`,
      `./glazes/passthrough-vert`,
    ];
    const shaders = await preloadShaders(names);
    this.frag = shaders[pathEnd(names[0])];
    this.compute = shaders[pathEnd(names[1])];
    this.display = shaders[pathEnd(names[2])];
    passthrough = shaders["passthrough-vert"];
    this.shadersLoaded = true;
    callback();
  }

  // Export a list of clean uniform names... everything after the ":".
  setCustomUniforms(locations, gl) {
    // Parse every key in custom uniforms, then apply the uniform values.
    keys(this.#uniforms).forEach((uniformIdentifier) => {
      const [type, name] = uniformIdentifier.split(":");
      gl[`uniform${type}`](
        locations[name],
        ...wrapNotArray(this.#uniforms[uniformIdentifier]),
      );
    });
  }
}

let gl, canvas;
let glaze;
let passthrough;

export function init(wrapper) {
  canvas = document.createElement("canvas");
  canvas.dataset.type = "glaze";
  canvas.classList.add("first-glaze");
  canvas.style.opacity = 0;

  gl = canvas.getContext("webgl2", {
    alpha: false,
    depth: false,
    stencil: false,
    desynchronized: true,
    antialias: false,
  });

  // gl = null;
  // canvas = null;

  if (gl) {
    // Blending & Culling
    gl.enable(gl.BLEND);
    gl.blendEquation(gl.FUNC_ADD);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    // Make sure that this gets added before the uiCanvas.
    wrapper.append(canvas);
  } else {
    off();
  }
}

let customProgram, computeProgram, displayProgram;
let fb; // Frame-buffer.
let texSurf, texFbSurfA, texFbSurfB; // Original aesthetic.computer surface texture,
// in addition to a double-buffer.
let texSurfWidth, texSurfHeight;
let vao;

const defaultUniformNames = [
  "iTexture",
  "iTexturePost",
  "iTime",
  "iMouse",
  "iResolution",
];

let customUniformLocations = {};

const displayUniformNames = defaultUniformNames;
const displayUniformLocations = {};

let offed = false;

// TODO: This is run on every resize... but some of this can move into init() above.
// Resizes the textures & re-initializes the necessary components for a resolution change.
// See also: `frame` via window.resize in `bios.js`.
export function frame(w, h, rect, nativeWidth, nativeHeight, wrapper) {
  if (glaze.shadersLoaded === false) return;

  // Run `init` if the canvas does not exist.
  // Note: Should `init` just be here?
  if (canvas === undefined) {
    init(wrapper);
  }

  if (offed) return; // If glaze could not initialize then don't run a frame.

  // Set the native canvas width and height.
  canvas.width = nativeWidth * window.devicePixelRatio;
  canvas.height = nativeHeight * window.devicePixelRatio;

  canvas.style.width = rect.width + "px";
  canvas.style.height = rect.height + "px";

  // Create custom shader program.
  const customVert = createShader(gl, gl.VERTEX_SHADER, passthrough);
  const customFrag = createShader(gl, gl.FRAGMENT_SHADER, glaze.frag);
  customProgram = createProgram(gl, customVert, customFrag);

  // Create compute shader program.
  const computeVert = createShader(gl, gl.VERTEX_SHADER, passthrough);
  const computeFrag = createShader(gl, gl.FRAGMENT_SHADER, glaze.compute);
  computeProgram = createProgram(gl, computeVert, computeFrag);

  // Create display shader program.
  const displayVert = createShader(gl, gl.VERTEX_SHADER, passthrough);
  const displayFrag = createShader(gl, gl.FRAGMENT_SHADER, glaze.display);
  displayProgram = createProgram(gl, displayVert, displayFrag);

  // Make surface texture.
  texSurf = gl.createTexture();

  texSurfWidth = w;
  texSurfHeight = h;

  // Temporarily fill texture with random pixels.
  const buffer = new Uint8Array(4 * w * h);

  for (let i = 0; i < buffer.length; i += 4) {
    buffer[i] = (255 * i) / buffer.length;
    buffer[i + 1] = (255 * i) / buffer.length;
    buffer[i + 2] = (255 * i) / buffer.length;
    buffer[i + 3] = 255;
  }

  gl.bindTexture(gl.TEXTURE_2D, texSurf);
  gl.texImage2D(
    gl.TEXTURE_2D,
    0,
    gl.RGBA,
    w,
    h,
    0,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    buffer,
  );

  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

  // Make fb texture.
  texFbSurfA = gl.createTexture();

  // Temporarily fill texture with random pixels.
  const buffer2 = new Uint8Array(4 * w * h);
  buffer2.fill(0);

  // Make post texture.
  texFbSurfB = gl.createTexture();

  gl.bindTexture(gl.TEXTURE_2D, texFbSurfB);
  gl.texImage2D(
    gl.TEXTURE_2D,
    0,
    gl.RGBA,
    w,
    h,
    0,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    buffer2,
  );

  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

  gl.bindTexture(gl.TEXTURE_2D, texFbSurfA);
  gl.texImage2D(
    gl.TEXTURE_2D,
    0,
    gl.RGBA,
    w,
    h,
    0,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    buffer2,
  );

  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

  // Make frame buffer.
  fb = gl.createFramebuffer();

  // Make vertex array object.
  vao = gl.createVertexArray();
  gl.bindVertexArray(vao);

  // Position Attribute
  const positionAttributeLocation = gl.getAttribLocation(
    customProgram,
    "a_position",
  );
  const positionBuffer = gl.createBuffer();

  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

  const positions = [-1, 1, -1, -1, 1, -1, 1, 1];

  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

  gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);
  gl.vertexAttribDivisor(positionAttributeLocation, 0);
  gl.enableVertexAttribArray(positionAttributeLocation);

  // Texture Coordinate Attribute
  const texCoordAttributeLocation = gl.getAttribLocation(
    customProgram,
    "a_texc",
  );
  const texCoordBuffer = gl.createBuffer();

  gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);

  const texCoords = [0, 0, 0, 1, 1, 1, 1, 0];

  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(texCoords), gl.STATIC_DRAW);

  gl.vertexAttribPointer(texCoordAttributeLocation, 2, gl.FLOAT, false, 0, 0);
  gl.vertexAttribDivisor(texCoordAttributeLocation, 0);
  gl.enableVertexAttribArray(texCoordAttributeLocation);

  // Vertex Attribute Index
  const indices = [
    0,
    1,
    2, // first triangle, bottom left - top left - top right
    0,
    2,
    3, // second triangle, bottom left - top right, bottom right
  ];

  const indicesBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indicesBuffer);
  gl.bufferData(
    gl.ELEMENT_ARRAY_BUFFER,
    new Uint16Array(indices),
    gl.STATIC_DRAW,
  );

  // Display Uniforms (Just the defaults.)
  defaultUniformNames.forEach(function (item, index) {
    displayUniformLocations[item] = gl.getUniformLocation(displayProgram, item);
  });

  // Custom Effect Uniforms (All the defaults, plus custom ones!)
  customUniformLocations = {};

  glaze.uniformNames
    .concat(defaultUniformNames)
    .forEach(function (item, index) {
      customUniformLocations[item] = gl.getUniformLocation(customProgram, item);
    });

  glaze.loaded = true;
}

// Turn glaze off if it has already been turned on.
export function off() {
  if (offed && canvas) {
    canvas.classList.remove("first-glaze");
    canvas.style.opacity = 0;
  }
  offed = true;
}

export function getCan() {
  return canvas;
}

// Turn glaze on if it has already been turned off.
export async function on(
  w,
  h,
  rect,
  nativeWidth,
  nativeHeight,
  wrapper,
  type,
  loaded,
) {
  if (
    glaze &&
    (glaze.type === type || type === undefined) &&
    glaze.w === w &&
    glaze.h === h &&
    rect.width === glaze.rect.width &&
    rect.height === glaze.rect.height
  ) {
    // Don't reload glaze from scratch if the same one has already been loaded.

    //console.log("Keeping glaze...", rect, glaze.rect);

    // TODO: Re-implement this... it causes a flicker rn. 22.09.19.23.43
    // Reframe glaze only if necessary...
    // if (glaze.w !== w || glaze.h !== h) {
    //  frame(w, h, rect, nativeWidth, nativeHeight, wrapper);
    // }
    return glaze;
  } else {
    glaze = new Glaze(type, w, h, rect);

    await glaze.load(() => {
      offed = false;
      frame(w, h, rect, nativeWidth, nativeHeight, wrapper);
      loaded();
    });
    return glaze;
  }
}

// Update the texture either in whole or in part based on a dirtyRect from `bios`.
export function update(texture, x = 0, y = 0) {
  if (glaze === undefined || glaze.loaded === false) return;

  gl.bindTexture(gl.TEXTURE_2D, texSurf);

  // TODO: I could pass in a subrectangle and do texSubImage2D here.
  // texSubImage2D(target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, type: GLenum, pixels: ArrayBufferView | null): void;

  gl.texSubImage2D(
    gl.TEXTURE_2D,
    0,
    x,
    y,
    texture.width,
    texture.height,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    texture, // Note: passing in canvasTexture did not work in Safari 15.2 so I am passing in imageData instead.
  );
}

// Draw the current output to a scaled freeze frame if the system is changing resolutions and neeeds a hold.
export function freeze(fCtx) {
  fCtx.drawImage(canvas, 0, 0, fCtx.canvas.width, fCtx.canvas.height);
  clear();
  canvas.style.opacity = 0;
}

export function unfreeze() {
  if (canvas) canvas.style.removeProperty("opacity");
}

export function render(time, mouse) {
  if (glaze === undefined || glaze.loaded === false) return;

  // 🅰️ Render Surface
  gl.useProgram(customProgram);

  gl.bindFramebuffer(gl.FRAMEBUFFER, fb);

  gl.framebufferTexture2D(
    gl.FRAMEBUFFER,
    gl.COLOR_ATTACHMENT0,
    gl.TEXTURE_2D,
    texFbSurfA,
    0,
  );

  // Resolution of custom filter.
  // TODO: Add the option to switch to full "native" resolution mode. 2022.04.11.03.48
  gl.viewport(0, 0, texSurfWidth, texSurfHeight);

  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, texSurf);
  gl.activeTexture(gl.TEXTURE1);
  gl.bindTexture(gl.TEXTURE_2D, texFbSurfB);

  gl.uniform1i(customUniformLocations.iTexture, 0);
  gl.uniform1i(customUniformLocations.iTexturePost, 1);
  gl.uniform1f(customUniformLocations.iTime, time);
  gl.uniform2f(customUniformLocations.iMouse, mouse.x, mouse.y);
  gl.uniform2f(customUniformLocations.iResolution, texSurfWidth, texSurfHeight);

  glaze.setCustomUniforms(customUniformLocations, gl);

  gl.bindVertexArray(vao);
  gl.drawElementsInstanced(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0, 1);

  // 🅱️ Compute Surface
  gl.useProgram(computeProgram);

  gl.bindFramebuffer(gl.FRAMEBUFFER, fb);

  gl.framebufferTexture2D(
    gl.FRAMEBUFFER,
    gl.COLOR_ATTACHMENT0,
    gl.TEXTURE_2D,
    texFbSurfB,
    0,
  );

  gl.viewport(0, 0, texSurfWidth, texSurfHeight);

  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, texSurf);
  gl.activeTexture(gl.TEXTURE1);
  gl.bindTexture(gl.TEXTURE_2D, texFbSurfA);

  gl.uniform1i(gl.getUniformLocation(computeProgram, "iTexture"), 0);
  gl.uniform1i(gl.getUniformLocation(computeProgram, "iTexturePost"), 1);
  gl.uniform1f(gl.getUniformLocation(computeProgram, "iTime"), time);
  gl.uniform2f(
    gl.getUniformLocation(computeProgram, "iMouse"),
    mouse.x,
    mouse.y,
  );
  gl.uniform2f(
    gl.getUniformLocation(computeProgram, "iResolution"),
    texSurfWidth,
    texSurfHeight,
  );

  //glaze.setComputeUniforms(computeUniformLocations, gl);

  gl.bindVertexArray(vao);
  gl.drawElementsInstanced(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0, 1);

  //©️ Display Surface
  gl.useProgram(displayProgram);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);

  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, texSurf);

  gl.activeTexture(gl.TEXTURE1);
  gl.bindTexture(gl.TEXTURE_2D, texFbSurfB);

  gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);

  gl.uniform1i(displayUniformLocations.iTexture, 0);
  gl.uniform1i(displayUniformLocations.iTexturePost, 1);
  gl.uniform2f(displayUniformLocations.iMouse, mouse.x, mouse.y);
  gl.uniform2f(
    displayUniformLocations.iResolution,
    gl.canvas.width,
    gl.canvas.height,
  );
  gl.uniform1f(displayUniformLocations.iTime, time);
  gl.drawElementsInstanced(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0, 1);
}

export function clear(r = 0, g = 0, b = 0) {
  gl?.clearColor(r, g, b, 0);
  gl?.clear(gl.COLOR_BUFFER_BIT);
}

/*
function getSample(point, w = 1, h = 1) {
  const x = Math.floor(point[0] * gl.canvas.width);
  const y = Math.floor(point[1] * gl.canvas.height);
  const sample = new Uint8Array(3 * w * h); // A 1 pixel, RGB sample
  gl.readPixels(x, y, w, h, gl.RGB, gl.UNSIGNED_BYTE, sample);
  console.log("Glaze sample:", sample);
  return sample;
}
*/
