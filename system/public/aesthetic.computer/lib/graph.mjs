// 📜 TODO: Sometimes it seems like the state in `scroll` breaks. 25.07.09.12.38
import {
  p2,
  number,
  wrap,
  randInt,
  byteInterval17,
  even,
  radians,
  lerp,
  randIntRange,
  randIntArr,
  clamp,
  signedCeil,
  rainbow,
  isHexString,
  hexToRgb,
  shiftRGB,
  cssColors,
} from "./num.mjs";

import * as mat4 from "../dep/gl-matrix/mat4.mjs";
import * as vec2 from "../dep/gl-matrix/vec2.mjs";
import * as vec3 from "../dep/gl-matrix/vec3.mjs";
import * as vec4 from "../dep/gl-matrix/vec4.mjs";

import { any, repeat, nonvalue, flip } from "./help.mjs";
import { Box } from "./geo.mjs";
import { nanoid } from "../dep/nanoid/nanoid.js";

const { round, sign, abs, ceil, floor, sin, cos, min, max, sqrt, PI } = Math;

let width, height, pixels;
const depthBuffer = [];
const writeBuffer = [];
const c = [255, 255, 255, 255];
let c2 = null; // Alternate / secondary color support.
const panTranslation = { x: 0, y: 0 }; // For 2d shifting using `pan` and `unpan`.
let activeMask; // A box for totally masking the renderer.
//                 This should work everywhere.
const skips = [];

let debug = false;
export function setDebug(newDebug) {
  debug = newDebug;
}

let twoDCommands;
export function twoD(ref) {
  twoDCommands = ref;
}

// 1. Configuration & State
function makeBuffer(width, height, fillProcess, painting, api) {
  if (!width || !height) return;

  const imageData = new ImageData(width, height);

  const buffer = {
    pixels: imageData.data,
    width: imageData.width,
    height: imageData.height,
  };

  buffer.api = api;

  if (typeof fillProcess === "function") {
    // Remember the current buffer and color.
    const savedBuffer = getBuffer();
    const rc = c; // Remember color.
    setBuffer(buffer);
    api.screen.pixels = buffer.pixels; // Set the API's pixel buffer.
    fillProcess(api); // Every fill process gets a painting API.
    painting.paint(true);
    // Restore old buffer and color.
    setBuffer(savedBuffer);
    color(...rc);
  }

  return buffer;
}

// Returns a cloned pixel buffer.
function cloneBuffer(buffer) {
  return {
    width: buffer.width,
    height: buffer.height,
    pixels: new Uint8ClampedArray(buffer.pixels),
  };
}

function getBuffer() {
  return { width, height, pixels };
}

function setBuffer(buffer) {
  ({ width, height, pixels } = buffer);
}

function changePixels(changer) {
  changer(pixels, width, height);
}

// Return a pixel from the main buffer or from a specified buffer.
function pixel(x, y, painting = { width, height, pixels }) {
  const buffer = painting.pixels;
  if (x >= painting.width || y >= painting.height || x < 0 || y < 0) {
    return [0, 0, 0, 0];
  }
  const i = (floor(x) + floor(y) * painting.width) * 4;
  return [buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3]];
}

// Helper function to check if two colors are the same
function colorsMatch(color1, color2) {
  if (!color1) return false;
  return (
    color1[0] === color2[0] &&
    color1[1] === color2[1] &&
    color1[2] === color2[2] &&
    color1[3] === color2[3]
  );
}

// Fill pixels with a color using a flood fill technique.
function flood(x, y, fillColor = c) {
  // Get the target color of the pixel at (x, y)
  const targetColor = pixel(x, y);
  if (targetColor[3] === 0) {
    // If the target pixel is transparent, return
    return {
      color: targetColor,
      area: 0,
    };
  }

  let count = 0;
  const visited = new Set();
  const stack = [[x, y]];

  color(...findColor(fillColor));
  const oldColor = c;
  while (stack.length) {
    const [cx, cy] = stack.pop();
    const key = `${cx},${cy}`;

    if (visited.has(key)) continue;
    visited.add(key);

    const currentColor = pixel(cx, cy);
    if (colorsMatch(currentColor, targetColor)) {
      count++;

      plot(cx, cy);

      stack.push([cx + 1, cy]); // Push neighbors to stack.
      stack.push([cx - 1, cy]);
      stack.push([cx, cy + 1]);
      stack.push([cx, cy - 1]);
    }
  }

  color(...oldColor);

  return {
    color: targetColor,
    area: count,
  };
}

// Parse a color from a variety of inputs..
function findColor() {
  let args = [...arguments];

  if (args.length === 1 && args[0] !== undefined) {
    const isNumber = () => typeof args[0] === "number";
    const isArray = () => Array.isArray(args[0]);
    const isString = () => typeof args[0] === "string";
    const isBool = typeof args[0] === "boolean";

    if (isBool) {
      return args[0] ? [255, 255, 255, 255] : [0, 0, 0, 255];
    }

    // If it's not a Number or Array or String, then assume it's an object,
    // randomly pick a key & re-run.
    if (!isNumber() && !isArray() && !isString())
      return findColor(any(args[0]));

    // Single number argument.
    if (isNumber()) {
      // Treat as raw hex if we hit a certain limit.
      if (args[0] > 255) {
        args = hexToRgb(args[0]);
      } else {
        // Otherwise, replicate the first number across all three fields.
        args = Array.from(args);
        args.push(args[0], args[0]);
      }
    } else if (isArray()) {
      // Or if it's an array, then spread it out and re-ink.
      // args = args[0];
      return findColor(...args[0]);
    } else if (isString()) {
      // See if it's a hex.
      const cleanedHex = args[0]
        .replace("#", "")
        .replace("0x", "")
        .toUpperCase();
      if (isHexString(cleanedHex) === true) {
        args = hexToRgb(cleanedHex);
      } else if (args[0] === "erase") {
        // TODO: Add better alpha support here... 23.09.11.22.10
        //       ^ See `parseColor` in `num`.
        // let alpha = 255;
        // if (args[1]) alpha = parseFloat(args[1]);
        args = [-1, -1, -1];
        if (args[1]) args.push(computeAlpha(args[1]));
      } else if (args[0] === "rainbow") {
        args = rainbow(); // Cycle through roygbiv in a linear sequence.
      } else {
        args = cssColors[args[0]]; // Try to match it to a table.
        if (!args) {
          args = randIntArr(255, 3);
          args.push(255);
        }
      }
      // TODO: Add an error message here. 22.08.29.13.03
    }
  } else if (args.length === 2) {
    // rainbow, alpha
    if (args[0] === "rainbow") {
      args = [...rainbow(), computeAlpha(args[1])];
    } else if (typeof args[0] === "string") {
      args = [...cssColors[args[0]], computeAlpha(args[1])];
    } else if (Array.isArray(args[0])) {
      args = [...args[0], args[1]];
    } else {
      // rgb, a
      args = [args[0], args[0], args[0], args[1]];
    }
  } else if (
    args.length === 0 ||
    (args.length === 1 && args[0] === undefined)
  ) {
    args = randIntArr(255, 3);
    args.push(255); // Generate random values here, always leave alpha 255.
  }

  if (args.length === 3) args = [...args, 255]; // Always be sure we have alpha.

  // Randomized any undefined or null values across all 4 arguments.
  args.forEach((a, i) => {
    if (isNaN(args[i])) args[i] = randInt(255);
  });

  return args;
}

// TODO: How could I convert from 0->1 and 0->255 with no side effects?
// TODO: What's a good way for ink to accept different ranges of alpha?
//       24.08.20.20.12
// Current solution:
// Any number from 0-><1 will use 0-> alpha.
//  - 1 and above will use 0->255.
//  - 0 always bottoms out
// Edge case near the 1 is manageable.
function computeAlpha(alpha) {
  if (alpha > 0 && alpha < 1) alpha = round(alpha * 255);
  return alpha;
}

// Set global color.
// Send 0 arguements to retrieve the current one.
function color(r, g, b, a = 255) {
  if (arguments.length === 0) return c.slice();
  c[0] = floor(r);
  c[1] = floor(g);
  c[2] = floor(b);
  c[3] = floor(a);
  return c.slice();
}

// Support for secondary color with the ability to clear the color by
// passing null or undefined as the first parameter.
function color2(r, g, b, a = 255) {
  if (arguments.length === 0) return c2.slice();
  if (r === undefined || r === null) {
    c2 = null;
    return;
  }
  if (!c2) c2 = [];
  c2[0] = floor(r);
  c2[1] = floor(g);
  c2[2] = floor(b);
  c2[3] = floor(a);
  return c2.slice();
}

export {
  makeBuffer,
  cloneBuffer,
  setBuffer,
  getBuffer,
  changePixels,
  depthBuffer,
  writeBuffer,
  color,
  color2,
  findColor,
  c, // currentColor
  pixel,
};

// 2. 2D Drawing
function clear() {
  // Note: I believe this would be the fastest method but would have to test it.
  // Would have to copy up by doubling until we hit the length!
  // pixels[0] = 255;
  // pixels[1] = 255;
  // pixels[2] = 255;
  // pixels[3] = 255;
  // pixels.copyWithin(4, 0);  // Determine the area to clear (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = activeMask.x;
    minY = activeMask.y;
    maxX = activeMask.x + activeMask.width;
    maxY = activeMask.y + activeMask.height;
  }

  if (activeMask) {
    // Clear only the masked area
    for (let y = minY; y < maxY; y++) {
      for (let x = minX; x < maxX; x++) {
        const i = (y * width + x) * 4;
        pixels[i] = c[0]; // r
        pixels[i + 1] = c[1]; // g
        pixels[i + 2] = c[2]; // b
        pixels[i + 3] = c[3]; // alpha
      }
    }
  } else {
    // Clear the entire screen (original behavior)
    for (let i = 0; i < pixels.length; i += 4) {
      pixels[i] = c[0]; // r
      pixels[i + 1] = c[1]; // g
      pixels[i + 2] = c[2]; // b
      pixels[i + 3] = c[3]; // alpha
    }
  }
}

/**
 * Plot a single pixel using (x, y) or {x, y} if only x is given.
 * (1) {x, y}
 * (2) (x, y)
 */
// Where a pixel is a region in which we draw from the upper left corner. (2D)
function plot(x, y) {
  x = floor(x);
  y = floor(y);
  // Skip pixels that are offscreen and/or found in the `skips` list.
  if (x < 0 || x >= width || y < 0 || y >= height) return;

  // Check pixels in the active mask.
  if (activeMask) {
    // Don't apply pan translation to mask bounds when checking plot coordinates
    // The mask coordinates are already set relative to the current pan position
    if (
      y < activeMask.y ||
      y >= activeMask.y + activeMask.height ||
      x >= activeMask.x + activeMask.width ||
      x < activeMask.x
    )
      return;
  }

  for (const s of skips) if (x === s.x && y === s.y) return;
  // Plot our pixel.
  const i = (x + y * width) * 4;
  
  // Additional safety check for array bounds
  if (i < 0 || i >= pixels.length - 3) return;
  
  const alpha = c[3];

  // Erasing
  if (c[0] === -1 && c[1] === -1 && c[2] === -1) {
    erase(pixels, i, 1 - c[3] / 255);
  } else if (alpha === 255) {
    // No alpha blending, just copy.
    pixels.set(c, i);
  } else if (alpha !== 0) {
    blend(pixels, c, 0, i);
  }
}

// Adds a point to the skip list which ignores these points from being drawn
// in `plot`. Passing `null` will clear the skip list.
// TODO: Should the skip list clear automatically on every paint? 2022.02.03.01.16
// TODO: Leaving skip blank will default to a random skipping algorithm?
//       Writing a function will allow you to dynamically skip pixels.
function skip(...args) {
  if (args[0] === null) skips.length = 0;
  else
    args.forEach((p) => {
      skips.push({
        x: floor(p.x || p[0]) + panTranslation.x,
        y: floor(p.y || p[1]) + panTranslation.y,
      });
    });
}

// Plots a single pixel within the panned coordinate space.
// Basically a wrapper over plot, which should ultimately be renamed to set?
// Accepts x, y or {x, y}
function point(...args) {
  let x, y;

  if (args.length === 1) {
    if (args[0].length >= 2) {
      // Array
      x = args[0][0];
      y = args[0][1];
    } else {
      // Object
      x = args[0].x;
      y = args[0].y;
    }
  } else if (args.length >= 2) {
    // Multiple arguments
    x = args[0];
    y = args[1];
  } else {
    x = randInt(width);
    y = randInt(height);
  }

  // TODO: Add support for {x, y} single argument. 2022.02.02.20.39
  x += panTranslation.x;
  y += panTranslation.y;
  // TODO: Eventually add rotation and scale etc.

  plot(x, y);
  return [x, y];
}

// Run a callback function to shade, then plot an array of pixel coordinates.
// This runs within `pline` and `pixelPerfectPolyline`.
function shadePixels(points, shader, shaderArgs = []) {
  points.forEach((p) => {
    // TODO: - [] Send current pixel under p? This can be used for cool position
    //            reading color effects. 23.01.05.01.23

    // Clip
    if (p.x < 0) return;
    if (p.x >= width) return;
    if (p.y < 0) return;
    if (p.y >= height) return;

    shader.position?.(p, ...shaderArgs); // Compute position.

    // Clip again
    if (p.x < 0) return;
    if (p.x >= width) return;
    if (p.y < 0) return;
    if (p.y >= height) return;

    p.x = floor(p.x); // Floor position and check bounds.
    p.y = floor(p.y);

    const n = p.x + p.y * width;

    if (writeBuffer[n] !== 1) {
      writeBuffer[n] = 1; // Remember this point in the frame's writeBuffer.
      // 🪄 Pixel Shader
      const i = floor(p.x + p.y * width) * 4; // Get current pixel under p.
      const pixel = pixels.subarray(i, i + 4);
      // TODO: Put p.color into shaderArgs or some other automated thing?
      shader.color({ x: p.x, y: p.y }, pixel, c, p.color); // Modify color.
    }
  }); // Paint each filtered pixel.
}

function pan(x, y) {
  if (typeof x === "object") {
    x = x.x;
    y = x.y;
  }
  if (y === undefined) y = x;
  panTranslation.x += floor(x);
  panTranslation.y += floor(y);
}

function unpan() {
  panTranslation.x = 0;
  panTranslation.y = 0;
  savedPanTranslation = null; // Also clear saved pan state
  // console.log("🔧 unpan() called - cleared current and saved pan state");
}

let savedPanTranslation;

// Save the local transform.
function savepan() {
  savedPanTranslation = { ...panTranslation };
}

// Restore it.
function loadpan() {
  if (savedPanTranslation) {
    panTranslation.x = savedPanTranslation.x;
    panTranslation.y = savedPanTranslation.y;
  }
}

// 😷 Mask off pixels.
function mask(box) {
  activeMask = box;
}

// 😄 Unmask pixels.
function unmask() {
  activeMask = null;
}

function copy(destX, destY, srcX, srcY, src, alpha = 1.0) {
  destX = floor(destX);
  destY = floor(destY);
  srcX = floor(srcX);
  srcY = floor(srcY);

  // Skip pixels that are offscreen or outside the src buffer.
  // Used in `paste`.
  if (
    destX < 0 ||
    destX >= width ||
    destY < 0 ||
    destY >= height ||
    srcX < 0 ||
    srcX >= src.width ||
    srcY < 0 ||
    srcY >= src.height
  ) {
    return;
  }

  const di = (destX + destY * width) * 4;
  const si = (srcX + srcY * src.width) * 4;

  blend(pixels, src.pixels, si, di, alpha);
}

/*
function copyRow(destX, destY, srcX, srcY, src) {
  destX = Math.round(destX);
  destY = Math.round(destY);
  srcX = Math.round(srcX);
  srcY = Math.round(srcY);

  const destIndex = (destX + destY * width) * 4;
  const srcIndex = (srcX + srcY * src.width) * 4;
  const rowLength = src.width * 4 - destX * 4;

  let srcStart = srcIndex;
  let srcEnd = srcIndex + src.width * 4;

  const sub = src.pixels.subarray(srcStart, srcEnd);

  pixels.set(sub, destIndex);
}
 */

// Resize a bitmap to a new with and height, returning a new
// bitmap, using nearest neighbor scaling.
// Bitmaps are {pixels: uint8array, width: int, height, int}
function resize(bitmap, width, height) {
  const ratioX = bitmap.width / width;
  const ratioY = bitmap.height / height;
  const pixels = new Uint8ClampedArray(width * height * 4);

  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      const index = (y * width + x) * 4;
      const srcX = floor(x * ratioX);
      const srcY = floor(y * ratioY);
      const srcIndex = (srcY * bitmap.width + srcX) * 4;

      pixels[index] = bitmap.pixels[srcIndex];
      pixels[index + 1] = bitmap.pixels[srcIndex + 1];
      pixels[index + 2] = bitmap.pixels[srcIndex + 2];
      pixels[index + 3] = bitmap.pixels[srcIndex + 3];
    }
  }
  return { pixels, width, height };
}

// Apply a blur effect to the current pixel buffer using smooth weighted blur
// radius: The blur radius (supports fractional values like 0.5, 1.5, etc.)
function blur(radius = 1) {
  if (radius <= 0) return;

  // Clamp radius to reasonable values for performance
  radius = Math.min(radius, 20);

  // Determine the area to blur (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, Math.min(width, activeMask.x));
    maxX = Math.max(0, Math.min(width, activeMask.x + activeMask.width));
    minY = Math.max(0, Math.min(height, activeMask.y));
    maxY = Math.max(0, Math.min(height, activeMask.y + activeMask.height));
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;

  // Early exit if bounds are invalid
  if (workingWidth <= 0 || workingHeight <= 0) return;

  // Create a copy of the current pixels to read from
  const sourcePixels = new Uint8ClampedArray(pixels);

  // Apply horizontal blur pass (only within mask bounds)
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      let r = 0,
        g = 0,
        b = 0,
        a = 0;
      let totalWeight = 0;

      // Always sample at least immediate neighbors for visible blur effect
      const sampleRadius = Math.max(1, Math.ceil(radius));

      for (let i = -sampleRadius; i <= sampleRadius; i++) {
        const distance = Math.abs(i);
        // Calculate weight using a smoother function that works well for fractional values
        let weight;
        if (distance === 0) {
          // Center pixel gets base weight
          weight = 1.0;
        } else {
          // Neighbors get weight based on radius
          // For radius < 1, this gives fractional weights to immediate neighbors
          // For radius >= 1, this gives decreasing weights with distance
          weight = Math.max(0, radius - distance + 1) * radius;
        }

        // Clamp sampling to mask bounds for proper edge handling
        const sampleX = Math.max(minX, Math.min(maxX - 1, x + i));
        const index = (y * width + sampleX) * 4;

        r += sourcePixels[index] * weight;
        g += sourcePixels[index + 1] * weight;
        b += sourcePixels[index + 2] * weight;
        a += sourcePixels[index + 3] * weight;
        totalWeight += weight;
      }

      // Write weighted average back to main buffer
      const index = (y * width + x) * 4;
      if (totalWeight > 0) {
        pixels[index] = r / totalWeight;
        pixels[index + 1] = g / totalWeight;
        pixels[index + 2] = b / totalWeight;
        pixels[index + 3] = a / totalWeight;
      }
    }
  }

  // Copy result for vertical pass (only the working area)
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const index = (y * width + x) * 4;
      sourcePixels[index] = pixels[index];
      sourcePixels[index + 1] = pixels[index + 1];
      sourcePixels[index + 2] = pixels[index + 2];
      sourcePixels[index + 3] = pixels[index + 3];
    }
  }

  // Apply vertical blur pass (only within mask bounds)
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      let r = 0,
        g = 0,
        b = 0,
        a = 0;
      let totalWeight = 0;

      // Always sample at least immediate neighbors for visible blur effect
      const sampleRadius = Math.max(1, Math.ceil(radius));

      for (let i = -sampleRadius; i <= sampleRadius; i++) {
        const distance = Math.abs(i);
        // Calculate weight using a smoother function that works well for fractional values
        let weight;
        if (distance === 0) {
          // Center pixel gets base weight
          weight = 1.0;
        } else {
          // Neighbors get weight based on radius
          // For radius < 1, this gives fractional weights to immediate neighbors
          // For radius >= 1, this gives decreasing weights with distance
          weight = Math.max(0, radius - distance + 1) * radius;
        }

        // Clamp sampling to mask bounds for proper edge handling
        const sampleY = Math.max(minY, Math.min(maxY - 1, y + i));
        const index = (sampleY * width + x) * 4;

        r += sourcePixels[index] * weight;
        g += sourcePixels[index + 1] * weight;
        b += sourcePixels[index + 2] * weight;
        a += sourcePixels[index + 3] * weight;
        totalWeight += weight;
      }

      // Write weighted average back to main buffer
      const index = (y * width + x) * 4;
      if (totalWeight > 0) {
        pixels[index] = r / totalWeight;
        pixels[index + 1] = g / totalWeight;
        pixels[index + 2] = b / totalWeight;
        pixels[index + 3] = a / totalWeight;
      }
    }
  }
}

// Copies pixels from a source buffer to the active buffer and returns
// the source buffer.
// TODO: Add dirty rectangle support here...
//       - What would the best parameter set be?
// `from` - can either be
// TODO: Replace with more generic algorithm?

// Notes:
// Scale can also be a transform object: { scale, angle }
// Blit only works with a scale of 1.
function paste(from, destX = 0, destY = 0, scale = 1, blit = false) {
  if (!from) {
    return;
  }

  destX += panTranslation.x;
  destY += panTranslation.y;

  if (scale !== 1) {
    let angle = 0;
    let anchor;
    let width, height;

    if (typeof scale === "object") {
      angle = scale.angle;
      width = scale.width;
      height = scale.height;
      anchor = scale.anchor;
      // ^ Pull properties out of the scale object.
      scale = scale.scale; // And then redefine scale.
    }

    // Fast path for simple integer scaling (no rotation, no custom dimensions)
    if (
      !angle &&
      !width &&
      !height &&
      !anchor &&
      typeof scale === "number" &&
      scale > 0 &&
      scale === ~~scale &&
      scale <= 8
    ) {
      // Integer scale up to 8x for safety

      // Ultra-fast nearest-neighbor scaling using direct pixel manipulation
      const srcWidth = from.width;
      const srcHeight = from.height;
      const srcPixels = from.pixels;
      const scaleInt = ~~scale; // Convert to integer

      // Pre-calculate destination bounds
      const destWidth = srcWidth * scaleInt;
      const destHeight = srcHeight * scaleInt;

      // Boundary check
      if (
        destX >= 0 &&
        destY >= 0 &&
        destX + destWidth <= width &&
        destY + destHeight <= height
      ) {
        // Direct pixel buffer manipulation for maximum speed
        for (let srcY = 0; srcY < srcHeight; srcY += 1) {
          for (let srcX = 0; srcX < srcWidth; srcX += 1) {
            const srcIndex = (srcX + srcY * srcWidth) << 2; // Fast * 4

            if (srcIndex < srcPixels.length) {
              // Extract color data directly
              const r = srcPixels[srcIndex];
              const g = srcPixels[srcIndex + 1];
              const b = srcPixels[srcIndex + 2];
              const a = srcPixels[srcIndex + 3];

              // Skip transparent pixels for efficiency
              if (a > 0) {
                const baseDestX = destX + srcX * scaleInt;
                const baseDestY = destY + srcY * scaleInt;

                // Set color once and draw scaled block
                color(r, g, b, a);

                // Use box for efficiency when scale > 1
                if (scaleInt > 1) {
                  box(baseDestX, baseDestY, scaleInt, scaleInt, "fill");
                } else {
                  plot(baseDestX, baseDestY);
                }
              }
            }
          }
        }
        return; // Exit early for fast path
      }
    }

    // Fall back to general grid-based scaling for complex cases
    grid(
      {
        box: { x: destX, y: destY, w: from.width, h: from.height },
        transform: { scale, angle, width, height, anchor },
      },
      from,
    );

    return;
  }

  // Note: Angle does not work here yet... 23.04.29.23.38

  // TODO: See if from has a dirtyBox attribute.
  if (from.crop) {
    // A cropped copy - optimize with row-wise operations where possible.
    const cropW = from.crop.w;
    const cropH = from.crop.h;

    // Check if we can do efficient row copying
    if (
      cropW > 8 &&
      destX >= 0 &&
      destY >= 0 &&
      destX + cropW <= width &&
      destY + cropH <= height
    ) {
      // Row-wise copying for better cache efficiency
      for (let y = 0; y < cropH; y += 1) {
        const srcY = from.crop.y + y;
        const destRowY = destY + y;

        // Copy entire row when possible
        for (let x = 0; x < cropW; x += 1) {
          copy(destX + x, destRowY, from.crop.x + x, srcY, from.painting);
        }
      }
    } else {
      // Fall back to original implementation
      for (let x = 0; x < cropW; x += 1) {
        for (let y = 0; y < cropH; y += 1) {
          copy(
            destX + x,
            destY + y,
            from.crop.x + x,
            from.crop.y + y,
            from.painting,
          );
        }
      }
    }
  } else {
    // Check to see if we can perform a full copy here,
    // with no alpha blending.
    if (blit) {
      pixels.set(from.pixels, 0);
    } else {
      // Optimize pixel-by-pixel copy with better access patterns
      const srcWidth = from.width;
      const srcHeight = from.height;

      // Check if we can do efficient bulk operations
      if (
        srcWidth > 4 &&
        srcHeight > 4 &&
        destX >= 0 &&
        destY >= 0 &&
        destX + srcWidth <= width &&
        destY + srcHeight <= height
      ) {
        // Row-major order for better cache performance
        for (let y = 0; y < srcHeight; y += 1) {
          for (let x = 0; x < srcWidth; x += 1) {
            copy(destX + x, destY + y, x, y, from);
          }
        }
      } else {
        // Original implementation for edge cases
        for (let x = 0; x < srcWidth; x += 1) {
          for (let y = 0; y < srcHeight; y += 1) {
            copy(destX + x, destY + y, x, y, from);
          }
        }
      }
    }
  }
}

// Similar to `paste` but always centered.
function stamp(from, x, y, scale = 1, angle = 0) {
  if (!from) return;
  
  // Handle transform object for scale/angle  
  let transform = scale;
  if (typeof scale !== "object") {
    transform = { scale, angle };
  }
  
  // The issue is that when paste uses grid for scaling, the center calculation
  // gets messed up. We need to manually calculate the correct position.
  
  if (transform.scale === 1 && (transform.angle === 0 || transform.angle === undefined)) {
    // Simple case - no transformation
    paste(from, x - from.width / 2, y - from.height / 2);
  } else {
    // Complex case - use grid directly with proper setup
    // We want the center of the ORIGINAL image to end up at x,y
    const scaleValue = typeof transform.scale === 'number' ? transform.scale : 1;
    
    // Calculate the top-left position of the scaled image such that 
    // its center aligns with x,y
    const scaledWidth = from.width * scaleValue;
    const scaledHeight = from.height * scaleValue;
    
    const topLeftX = x - scaledWidth / 2;
    const topLeftY = y - scaledHeight / 2;
    
    grid(
      {
        box: { x: topLeftX, y: topLeftY, w: from.width, h: from.height },
        transform: transform,
      },
      from,
    );
  }
}

let blendingMode = "blend";
function blendMode(mode = "blend") {
  blendingMode = mode;
}

// let stipple = 0;
// A fast alpha blending function that looks into a pixel array.
// Transcribed from C++: https://stackoverflow.com/a/12016968
function blend(dst, src, si, di, alphaIn = 1) {
  //stipple += 1;
  //if (stipple < 4) { return; }
  //stipple = 0;

  if (blendingMode === "erase") {
    const normalAlpha = 1 - src[si + 3] / 255;
    dst[di + 3] *= normalAlpha;
    if (dst[di + 3] === 0) {
      // If the alpha is zero then wipe the data.
      dst[di] = 32;
      dst[di + 1] = 32;
      dst[di + 2] = 32;
    }
    return;
  }

  if (src[si + 3] === 0) return; // Return early if src is invalid.

  // Just do a straight up copy if we are in "blit" mode.
  if (blendingMode === "blit") {
    for (let i = 0; i < 4; i++) {
      if (i != 3) {
        dst[di + i] = src[si + i]; // For R, G, B channels
      } else {
        dst[di + i] = src[si + i] * alphaIn; // For the Alpha channel
      }
    }
    return;
  }

  // A. Blend over transparent pixels.
  if (dst[di + 3] < 255 && src[si + 3] > 0) {
    const epsilon = 1e-10; // Small number to prevent division by zero
    const alphaSrc = (src[si + 3] * alphaIn) / 255;
    const alphaDst = dst[di + 3] / 255;
    const combinedAlpha = alphaSrc + (1.0 - alphaSrc) * alphaDst;
    if (combinedAlpha > epsilon) {
      // Check against a very small number instead of 0
      for (let offset = 0; offset < 3; offset++) {
        // Iterate over R, G, B channels
        dst[di + offset] =
          (src[si + offset] * alphaSrc +
            dst[di + offset] * (1.0 - alphaSrc) * alphaDst) /
          (combinedAlpha + epsilon); // Add epsilon to prevent division instability
      }
      dst[di + 3] = combinedAlpha * 255;
    }
  } else {
    // B. Blend over opaque pixels.
    const alpha = src[si + 3] * alphaIn + 1;
    const invAlpha = 256 - alpha;
    dst[di] = (alpha * src[si + 0] + invAlpha * dst[di + 0]) >> 8;
    dst[di + 1] = (alpha * src[si + 1] + invAlpha * dst[di + 1]) >> 8;
    dst[di + 2] = (alpha * src[si + 2] + invAlpha * dst[di + 2]) >> 8;
    dst[di + 3] = dst[di + 3] + alpha;
  }
}

// Blends the alpha channel only / erases pixels.
function erase(pixels, i, normalizedAlpha) {
  pixels[i + 3] *= normalizedAlpha;
}

// Draws a horizontal line. (Should be very fast...)
function lineh(x0, x1, y) {
  x0 = floor(x0);
  x1 = floor(x1);
  y = floor(y);
  if (y < 0 || y >= height || x0 >= width || x1 < 0) return; // Check if the entire line is outside the mask
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    if (
      y < activeMask.y ||
      y >= activeMask.y + activeMask.height ||
      x1 < activeMask.x ||
      x0 >= activeMask.x + activeMask.width
    )
      return;
  }

  // Clamp to screen bounds first
  x0 = clamp(x0, 0, width - 1);
  x1 = clamp(x1, 0, width - 1); // Then clamp to mask bounds if mask is active
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    x0 = clamp(x0, activeMask.x, activeMask.x + activeMask.width - 1);
    x1 = clamp(x1, activeMask.x, activeMask.x + activeMask.width - 1);
  }

  const firstIndex = (x0 + y * width) * 4;
  const secondIndex = (x1 + y * width) * 4;

  let startIndex, endIndex;

  // Sort indices so we can always draw from left to right.
  if (firstIndex > secondIndex) {
    startIndex = secondIndex;
    endIndex = firstIndex;
  } else {
    startIndex = firstIndex;
    endIndex = secondIndex;
  }

  // Erasing.
  if (blendMode !== "erase" && c[0] === -1 && c[1] === -1 && c[2] === -1) {
    const normalAlpha = 1 - c[3] / 255;
    for (let i = startIndex; i <= endIndex; i += 4) {
      erase(pixels, i, normalAlpha);
    }
    // No alpha.
  } else if (c[3] === 255) {
    for (let i = startIndex; i <= endIndex; i += 4) {
      pixels[i] = c[0];
      pixels[i + 1] = c[1];
      pixels[i + 2] = c[2];
      pixels[i + 3] = 255;
    }
    // Alpha blending.
  } else if (c[3] !== 0) {
    for (let i = startIndex; i <= endIndex; i += 4) {
      blend(pixels, c, 0, i);
    }
  }
}

// Draws a line
// (2) p1, p2: pairs of {x, y} or [x, y]
// (4) x0, y0, x1, y1
// TODO: Automatically use lineh if possible. 22.10.05.18.27
function line() {
  let x0, y0, x1, y1;
  if (arguments.length === 1) {
    // Safely access properties on the first argument
    const arg0 = arguments[0];
    if (arg0 && typeof arg0 === "object") {
      x0 = arg0.x0; // Assume an object { x0, y0, x1, y1 }
      y0 = arg0.y0;
      x1 = arg0.x1;
      y1 = arg0.y1;
    }
  } else if (arguments.length === 4) {
    x0 = arguments[0]; // Set all `undefined` or `null` values to 0.
    y0 = arguments[1];
    x1 = arguments[2];
    y1 = arguments[3];
  } else if (arguments.length === 2) {
    const arg0 = arguments[0];
    const arg1 = arguments[1];
    if (Array.isArray(arg0) && Array.isArray(arg1)) {
      // assume [x, y], [x, y]
      x0 = arg0[0];
      y0 = arg0[1];
      x1 = arg1[0];
      y1 = arg1[1];
    } else if (
      arg0 &&
      typeof arg0 === "object" &&
      arg1 &&
      typeof arg1 === "object"
    ) {
      // assume {x, y}, {x, y}
      x0 = arg0.x;
      y0 = arg0.y;
      x1 = arg1.x;
      y1 = arg1.y;
    }
  } else {
    // if (debug) {
    //   console.warn(
    //     "Line did not use the correct number of arguments:",
    //     arguments
    //   );
    // }
  }

  // Set all untruthy values like null, or undefined to a random value.
  if (x0 == null) x0 = randIntRange(0, width);
  if (y0 == null) y0 = randIntRange(0, height);
  if (x1 == null) x1 = randIntRange(0, width);
  if (y1 == null) y1 = randIntRange(0, height);

  if (isNaN(x0) || isNaN(y0) || isNaN(x1) || isNaN(y1)) {
    console.error("Invalid line arguments:", x0, y0, x1, y1);
    return;
  }

  // console.log("Line in:", x0, y0, x1, y1);

  // Add any panTranslations.
  x0 += panTranslation.x;
  y0 += panTranslation.y;
  x1 += panTranslation.x;
  y1 += panTranslation.y;

  // Lerp from primary to secondary color as needed.
  const cachedInk = c.slice(0);

  // Check if line is perfectly horizontal and no gradient is present, otherwise run bresenham.
  if (y0 === y1 && !c2) {
    lineh(x0, x1, y0);
  } else {
    bresenham(x0, y0, x1, y1).forEach((p) => {
      if (c2) {
        const step = sqrt(p.x * p.x + p.y * p.y) / 255; // Gradient step.
        color(...shiftRGB(c, c2, step));
        plot(p.x, p.y);
      } else {
        plot(p.x, p.y);
      }
    });
    if (c2) color(...cachedInk);
  }

  const out = [x0, y0, x1, y1];
  // console.log("Line out:", out);
  twoDCommands?.push(["line", ...out]); // Forward this call to the GPU.
  return out;
}

// Takes an array of pixel coords `{x, y}` and filters out L shapes.
// Note: It checks the previous, current, and next pixel and requires a minimum
//        set of 3 before it removes anything.
// Draws a regular `line` if only two pixels are provided.
// Transcribed from: https://rickyhan.com/jekyll/update/2018/11/22/pixel-art-algorithm-pixel-perfect.html
function pixelPerfectPolyline(points, shader) {
  if (points.length < 2) return; // Require 2 or more points.
  const pixels = [];
  //points.reverse(); // Draw from front to back.

  let last = points[0];

  // TODO: Draw everything front to back similar to pline? 23.02.01.13.12
  //for (let i = coords.length - 2; i >= 0; i -= 1) {

  points.forEach((cur) => {
    // Clip offscreen segments.
    const xMin = min(last.x, cur.x);
    const xMax = max(last.x, cur.x);
    const yMin = min(last.y, cur.y);
    const yMax = max(last.y, cur.y);
    if (xMin >= width || xMax < 0 || yMin >= height || yMax < 0) {
      last = cur;
      return;
    }

    //if (cur.color === "rainbow") color(rainbow());

    const rb = last.color === "rainbow";

    // Compute bresen pixels, filtering out duplicates.
    bresenham(last.x, last.y, cur.x, cur.y).forEach((p, i) => {
      if (i > 0 || pixels.length < 2) {
        pixels.push({ ...p, color: rb ? rainbow() : last.color }); // Add color for each pixel.
      }
    });
    last = cur;
  });

  const filtered = [];
  let c = 0;

  while (c < pixels.length) {
    if (
      c > 0 &&
      c + 1 < pixels.length &&
      (pixels[c - 1].x === pixels[c].x || pixels[c - 1].y === pixels[c].y) && // check left and up
      (pixels[c + 1].x === pixels[c].x || pixels[c + 1].y === pixels[c].y) && // check right and down
      pixels[c - 1].x !== pixels[c + 1].x && // check left and right of prev and next
      pixels[c - 1].y !== pixels[c + 1].y
    ) {
      // check top and bottom of prev and next
      c += 1;
    }
    filtered.push(pixels[c]);
    c += 1;
  }

  if (shader) {
    shadePixels(filtered, shader);
  } else {
    filtered.forEach((p) => point(p));
  }
}

// Draws a line from a point at a distance... with an angle in degrees.
function lineAngle(x1, y1, dist, degrees) {
  const x2 = x1 + dist * cos(radians(degrees));
  const y2 = y1 + dist * sin(radians(degrees));
  return line(x1, y1, x2, y2);
}

// Take two vertices and plot a 3d line with depth information.
function line3d(a, b, lineColor, gradients) {
  const aColor = a.color;
  const bColor = b.color;

  a = a.transform(screenMatrix);
  b = b.transform(screenMatrix);

  const aZ = a.pos[Z];
  const bZ = a.pos[Z];

  a = a.perspectiveDivide();
  b = b.perspectiveDivide();

  const [x0, y0, z0] = a.pos;
  const [x1, y1, z1] = b.pos;
  const points = bresenham(x0, y0, x1, y1);

  const saveColor = c.slice();

  color(...lineColor); // Set color from lineColor or default to global color.
  // color(randInt(255), randInt(255), randInt(255)); // Random colors.

  points.forEach((p, i) => {
    const progress = i / points.length;
    //const z = lerp(z0, z1, progress);

    // Lerp from a.color to b.color if gradients are enabled.
    if (gradients && aColor && bColor) {
      const R = lerp(aColor[0], bColor[0], progress);
      const G = lerp(aColor[1], bColor[1], progress);
      const B = lerp(aColor[2], bColor[2], progress);
      color(R, G, B);
    }

    const newZ = lerp(aZ, bZ, progress);
    const stretchedDepth = 1 - min(1, abs(newZ) / 2.5);

    /*
    color(
      c[0] * stretchedDepth,
      c[1] * stretchedDepth,
      c[2] * stretchedDepth,
    ); // Set color from lineColor or default to global color.
    */

    const depth = newZ * -1;
    if (depthBuffer) {
      const index = p.x + p.y * width;
      if (depth > depthBuffer[index]) {
      } else {
        depthBuffer[index] = depth;
        plot(p.x, p.y);
      }
    } else {
      plot(p.x, p.y);
    }
  });

  color(...saveColor); // Restore color.
}

// TODO: Implement a nice filled option here...
//       Something pixel-perfect with the outline... like a flood?
function circle(x0, y0, radius, filled = false, thickness, precision) {
  if (filled || thickness > 1) {
    oval(x0, y0, radius, radius, filled, thickness, precision);
    return;
  }

  // Circle
  x0 = floor(x0);
  y0 = floor(y0);
  radius = floor(radius);

  let f = 1 - radius,
    ddF_x = 0,
    ddF_y = -2 * radius,
    x = 0,
    y = radius;

  point(x0, y0 + radius);
  point(x0, y0 - radius);
  point(x0 + radius, y0);
  point(x0 - radius, y0);

  while (x < y) {
    if (f >= 0) {
      y -= 1;
      ddF_y += 2;
      f += ddF_y;
    }
    x += 1;
    ddF_x += 2;
    f += ddF_x + 1;
    point(x0 + x, y0 + y);
    point(x0 - x, y0 + y);
    point(x0 + x, y0 - y);
    point(x0 - x, y0 - y);
    point(x0 + y, y0 + x);
    point(x0 - y, y0 + x);
    point(x0 + y, y0 - x);
    point(x0 - y, y0 - x);
  }
}

// TODO: Generate sampled points around a circle then use
function oval(
  x0,
  y0,
  radiusX,
  radiusY,
  filled = false,
  thickness = 1,
  precision,
) {
  const points = generateEllipsePoints(x0, y0, radiusX, radiusY, precision);
  shape({ points, filled, thickness });
}

// TODO: How can I relate precision to the circumference and avoid this little
//       sharp point on the top?
// TODO: This function can have radians etc. / be more terse. 23.02.13.19.42
function generateEllipsePoints(x0, y0, radiusX, radiusY, precision = 20) {
  const points = [];
  for (let i = 0; i < 360; i += precision) {
    const angle = radians(i);
    const x = x0 + radiusX * cos(angle);
    const y = y0 + radiusY * sin(angle);
    points.push([x, y]);
  }
  return points;
}

// Draws a series of 1px lines without overlapping / overdrawing points.
// TODO: Add closed mode? Example: ink(handPalette.w).poly([...w, w[0]]);
function poly(coords) {
  let last = coords[0];
  for (let i = 1; i < coords.length; i++) {
    const cur = coords[i];
    line(last, cur);
    last = cur;
  }
}

// Rasterize an Npx thick poly line with rounded end-caps.
/* TODO
 - [😇] Clip coords to inside of the screen.
 + Later
 - [] Perhaps if thickness === 1 then this can be combined with
     `pixelPerfectPolyline` ?
 - [] Render a third triangle from mid point to last point to next quad
      point?
 - [] Rounded half-circle endcaps.
 - [] Filled circle if coords.length === 1.
 - [] Texture / special FX.
 + Done
 - [x] Triangle rasterization of segment.
 - [x] Optimize performance.
   - [x] Run the profiler.
*/
function pline(coords, thickness, shader) {
  // 1️⃣ Generate geometry.
  if (coords.length < 2) return; // Require at least two coordinates.

  let points = [], // Raster grids.
    lines = [],
    tris = [];

  // 🎴 Draw everything from front to back!

  let last = coords[coords.length - 1]; // Keep the last drawn element.

  let lpar, ldir; // Store last parallel points / prepopulate if supplied.

  for (let i = coords.length - 2; i >= 0; i -= 1) {
    const cur = coords[i];

    // 1. Two points on both sides of last and cur via line dir.
    const lp = [last.x || last[0], last.y || last[1]],
      cp = [cur.x || cur[0], cur.y || cur[1]]; // Convert last and cur to vec2.

    const dir = vec2.normalize([], vec2.subtract([], cp, lp)); // Line direction.
    if (!ldir) ldir = dir;

    const rot = vec2.rotate([], dir, [0, 0], PI / 2); // Rotated by 90d

    const offset1 = vec2.scale([], rot, thickness / 2); // Parallel offsets.
    const offset2 = vec2.scale([], rot, -thickness / 2);

    let c1, c2;
    if (!lpar) {
      c1 = vec2.add([], lp, offset1); // Compute both sets of points.
      c2 = vec2.add([], lp, offset2);
      lpar = [c1, c2];
    } else {
      [c1, c2] = lpar;
    }

    [c1, c2, lp, cp].forEach((v) => vec2.floor(v, v)); // Floor everything.

    // 2. Plotting

    const dot = vec2.dot(dir, ldir); // Get the dot product of cur and last dir.

    let trig; // Triangle geometry.

    if (dot > 0) {
      // Vertex order for forward direction.
      trig = [
        [c1, c2, lp],
        [c2, lp, cp],
      ];
      lines.push(...bresenham(...c1, ...lp)); // Par line 1
      lines.push(...bresenham(...c2, ...cp)); // Par line 2
    } else {
      // Vertex order for backward direction.
      trig = [
        [c2, lp, c1],
        [c1, cp, lp],
      ];
      lines.push(...bresenham(...c2, ...lp)); // Par line 1
      lines.push(...bresenham(...c1, ...cp)); // Par line 2
    }

    // Partial outside clipping.
    // const clippedTris = trig.filter((triangle) =>
    //   triangle.every(
    //     (v) => v[0] >= 0 && v[0] < width && v[1] >= 0 && v[1] < height
    //   )
    // );

    // Full outside clipping.
    // Clip triangles that are *fully* offscreen.
    // Take into account panning here...
    const clippedTris = trig.filter((triangle) =>
      triangle.some((v) => {
        const tv = v.slice();
        tv[0] += panTranslation.x;
        tv[1] += panTranslation.y;
        return tv[0] >= 0 && tv[0] < width && tv[1] >= 0 && tv[1] < height;
      }),
    );

    clippedTris.forEach((tri) => fillTri(tri, tris)); // Fill quad.
    //trig.forEach((t) => fillTri(t, tris)); // Fill quad.

    ldir = dir;
    lines.push(...bresenham(...lp, ...cp));

    if (i === coords.length - 2)
      points.push({ x: c1[0], y: c1[1] }, { x: c2[0], y: c2[1] });

    points.push({ x: lp[0], y: lp[1] }, { x: cp[0], y: cp[1] }); // Add points.

    // Paint each triangle.
    if (cur.color === "rainbow") color(...rainbow());
    else if (cur.color) color(...cur.color);

    if (shader) {
      const progress = 1 - i / (coords.length - 2);
      shadePixels(tris, shader, [progress]);
    } else {
      tris.forEach((p) => point(p));
    }

    tris.length = 0;

    last = cur; // Update the last point.
    lpar = [c1, c2]; // ... and last parallel points.
  }

  // 3️⃣ Painting

  // color(0, 255, 0); // Paint vertex points.
  // points.forEach((p) => point(p));

  // color(0, 0, 255); // Paint wireframe lines.
  // lines.forEach((p) => point(p));

  return lpar;
}

// 🔺 Rasterizes a tri. See also: https://www.youtube.com/watch?v=SbB5taqJsS4
function fillTri(v3, pix) {
  const scan = []; // Scan buffer of left->right down Y axis.
  const [min, mid, max] = v3.sort((va, vb) => va[1] - vb[1]); // Sort Y.

  const v1 = [max[0] - min[0], max[1] - min[1]], // Area / cross-product.
    v2 = [mid[0] - min[0], mid[1] - min[1]],
    cp = v1[0] * v2[1] - v2[0] * v1[1];

  const handedness = cp > 0 ? 1 : cp < 0 ? 0 : -1; // O->L, 1->R, -1->️🚫

  // Scan across each edge.
  [
    [min, max, 0 + handedness], // Min and Max for each edge with handedness.
    [min, mid, 1 - handedness],
    [mid, max, 1 - handedness],
  ].forEach(function scanEdge(v) {
    const yStart = v[0][Y],
      yEnd = v[1][Y],
      xStart = v[0][X],
      xEnd = v[1][X];

    const yDist = yEnd - yStart;
    const xDist = xEnd - xStart;
    if (yDist <= 0) return; // Don't convert if we have no columns.
    const xStep = xDist / yDist;

    let x = xStart;
    for (let y = yStart; y < yEnd; y += 1) {
      scan[y * 2 + v[2]] = floor(x);
      x += xStep;
    }
  });

  // Rasterize all by scanning horizontally.
  for (let y = min[Y]; y < max[Y]; y += 1) {
    for (let x = scan[y * 2]; x < scan[y * 2 + 1]; x += 1) {
      pix.push({ x, y });
    }
  }
}

/**
 * Bresenham's Line Algorithm
 * @description - Returns an array of integer points that make up an aliased line from {x0, y0} to {x1, y1}.
 * - This function is "abstract" and does not render anything... but outputs points.
 * @param x0
 * @param y0
 * @param x1
 * @param y1
 * @returns {*[]}
 */
function bresenham(x0, y0, x1, y1) {
  const points = [];

  // Make sure everything is floor'd.
  x0 = floor(x0) || 0;
  y0 = floor(y0) || 0;
  x1 = floor(x1) || 0;
  y1 = floor(y1) || 0;
  // Bresenham's Algorithm
  const dx = abs(x1 - x0);
  const dy = -abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx + dy;

  while (true) {
    points.push({ x: x0, y: y0 });

    if (x0 === x1 && y0 === y1) break;
    const e2 = 2 * err;
    if (e2 >= dy) {
      err += dy;
      x0 += sx;
    }
    if (e2 <= dx) {
      err += dx;
      y0 += sy;
    }
  }
  return points;
}

// Takes in x, y, width and height and draws an
// outline, inline (1px) or filled rectangle, optionally
// from the center by inputting eg: "inline*center" in mode.
const BOX_CENTER = "center";
// Parameters
// (1) box (any object with {x, y, w, h} properties) (1)
// (2) box, mode (2)
// (3) x, y, size (3)
// (4) x, y, w, h (4)
// (4) x, y, size, mode:string (4)
// (5) x, y, w, h, mode (5)
// TODO: ♾️ If width or height (size) is infinity then make sure the box covers
//          the entire pixel buffer width. 25.05.11.16.30
function box() {
  let x,
    y,
    w,
    h,
    mode = "fill";

  // Apply the TODO: If any argument is NaN then just make it 'undefined'
  for (let i = 0; i < arguments.length; i++) {
    if (Number.isNaN(arguments[i])) {
      arguments[i] = undefined;
    }
  }

  if (arguments.length === 1 || arguments.length === 2) {
    // Array(4)
    if (Array.isArray(arguments[0])) {
      x = arguments[0][0];
      y = arguments[0][1];
      w = arguments[0][2];
      h = arguments[0][3];
    } else if (arguments[0]) {
      // Object {x, y, w, h}
      // Note: Also works with anything that has width and height properties.

      // Allow long names.
      if (!isNaN(arguments[0].width)) arguments[0].w = arguments[0].width;
      if (!isNaN(arguments[0].height)) arguments[0].h = arguments[0].height;

      x = arguments[0].x || 0;
      y = arguments[0].y || 0;
      w = arguments[0].w || 0;

      if (isNaN(arguments[0].h)) {
        h = w;
      } else {
        h = arguments[0].h;
      }

      if (x === undefined || y === undefined || w === undefined) {
        return console.error(
          "Could not make a box {x,y,w,h} from:",
          arguments[0],
        );
      }
    }
    if (arguments[1]) mode = arguments[1];
  } else if (arguments.length === 3) {
    // x, y, size
    x = arguments[0];
    y = arguments[1];
    w = arguments[2];
    h = arguments[2];
  } else if (arguments.length === 4) {
    if (typeof arguments[3] === "number") {
      // x, y, w, h
      x = arguments[0];
      y = arguments[1];
      w = arguments[2];
      h = arguments[3];
    } else {
      // x, y, size, mode
      x = arguments[0];
      y = arguments[1];
      w = arguments[2];
      h = arguments[2];
      mode = arguments[3];
    }
  } else if (arguments.length === 5) {
    // x, y, w, h, mode
    x = arguments[0];
    y = arguments[1];
    w = arguments[2];
    h = arguments[3];
    mode = arguments[4];
  } else {
    //return console.error("Invalid box call.");
  }

  if (w === 0 || h === 0 || isNaN(w) || isNaN(h)) return; // Silently quit if the box has no volume.
  if (w === Infinity) w = width;
  if (h === Infinity) h = height;

  // Random parameters if undefined, null, or NaN.
  if (nonvalue(x)) x = randInt(width);
  if (nonvalue(y)) y = randInt(height);
  if (nonvalue(w)) w = randInt(width);
  if (nonvalue(h)) h = randInt(height);

  // Abs / normalize the parameters.
  if (mode === undefined || mode === "") mode = "fill";

  // Check for "Center" at the end of mode.
  if (mode.endsWith(BOX_CENTER)) {
    x -= w / 2;
    y -= h / 2;
    mode = mode.slice(0, -BOX_CENTER.length - 1); // Clear through separator: *
  }

  x = floor(x);
  y = floor(y);
  w = signedCeil(w);
  h = signedCeil(h);

  ({ x, y, w, h } = Box.from([x, y, w, h]).abs);

  // Apply any global pan translations.
  // x += panTranslation.x; // Note: Already processed in `line`.
  // y += panTranslation.y;

  const thickness = parseInt(mode.split(":")[1]) || 1;
  mode = mode.split(":")[0];

  if (mode === "outline" || mode === "out") {
    if (thickness === 1) {
      line(x - 1, y - 1, x + w, y - 1); // Top
      line(x - 1, y + h, x + w, y + h); // Bottom
      line(x - 1, y, x - 1, y + h - 1); // Left
      line(x + w, y, x + w, y + h - 1); // Right
    } else {
      const leftX = x - thickness;
      const topY = y - thickness;
      const rightX = x + w + thickness;
      const bottomY = y + h + thickness;
      const boxHeight = h + thickness * 2;
      const boxWidth = w + thickness * 2;
      box(leftX, topY, boxWidth, thickness); // Top box
      box(leftX, bottomY - thickness, boxWidth, thickness); // Bottom box
      box(leftX, topY + thickness, thickness, boxHeight - thickness * 2); // Left box
      box(
        rightX - thickness,
        topY + thickness,
        thickness,
        boxHeight - thickness * 2,
      ); // Right box
    }
  } else if (mode === "inline" || mode === "in") {
    if (thickness === 1) {
      line(x, y, x + w - 1, y); // Top
      line(x, y + h - 1, x + w - 1, y + h - 1); // Bottom
      line(x, y + 1, x, y + h - 2); // Left
      line(x + w - 1, y + 1, x + w - 1, y + h - 2); // Right
    } else {
      if (thickness * 2 <= w && thickness * 2 <= h) {
        box(x, y, w, thickness); // Top
        box(x, y + h - thickness, w, thickness); // Bottom
        box(x, y + thickness, thickness, h - thickness * 2); // Left
        box(x + w - thickness, y + thickness, thickness, h - thickness * 2); // Right
      } else {
        box(x, y, w, h); // Just fill the box if the inline is too big.
      }
    }
  } else if (mode === "fill" || mode === "") {
    // TODO: The boxes could be cropped to always fit inside the screen here.
    w -= 1;
    if (sign(height) === 1) {
      for (let row = 0; row < h; row += 1) line(x, y + row, x + w, y + row);
    } else {
      for (let row = 0; row > h; row -= 1) line(x, y + row, x + w, y + row);
    }
  }
}

// Rasterizes an outlined or filled shape from pairs of points.
// Accepts: (x, y, x, y, x, y, ...)
//      Or: ([[x, y], [x, y], ...])
//      Or: { points: ", filled: false }
function shape() {
  let argPoints;
  let points;
  let filled = true;
  let thickness = 1; // Used if unfilled.

  if (arguments.length === 1 && !Array.isArray(arguments[0])) {
    // Assume an object {points, filled}
    argPoints = arguments[0].points;
    filled = arguments[0].filled;
    thickness = arguments[0].thickness || thickness;
  } else {
    argPoints = arguments[0];
  }

  if (!Array.isArray(argPoints[0])) {
    // Assume a flat list of coordinates to convert into pairs.
    points = [];

    for (let p = 0; p < argPoints.length; p += 2) {
      points.push([argPoints[p], argPoints[p + 1]]);
    }
  } else {
    points = argPoints; // Assume array of pairs was passed.
  }

  if (filled) {
    fillShape(points); // Fill the shape in with the chosen color.
  } else {
    // Make lines from 1->2->3->...->1
    if (thickness === 1) {
      points.forEach((p, i) => {
        const lastPoint = i < points.length - 1 ? points[i + 1] : points[0];
        line(...p, ...lastPoint);
      });
    } else {
      // Thicker outline using pline.
      points.push(points[0]);
      pline(
        points.map((p) => p2.of(...p)),
        thickness,
      );
    }
  }
}

// Note: This may not be very fast. It was written by ChatGPT. 23.02.11.12.51
// Note: The most efficient algorithm in C I could find as an alternate:
//       https://gist.github.com/ideasman42/983738130f754ef58ffa66bcdbbab892
// Fills a shape using the scan line algorithm, hitting every pixel via `point`.
function fillShape(points) {
  // Find the minimum and maximum y-coordinates of the points
  let minY = Infinity;
  let maxY = -Infinity;
  for (let i = 0; i < points.length; i++) {
    minY = min(minY, points[i][1]);
    maxY = max(maxY, points[i][1]);
  }

  // For each scan line from minY to maxY
  for (let y = minY; y <= maxY; y++) {
    // Find the intersections of the scan line with the edges of the polygon
    let intersections = [];
    for (let i = 0; i < points.length; i++) {
      let p1 = points[i];
      let p2 = points[(i + 1) % points.length];
      if ((p1[1] <= y && y < p2[1]) || (p2[1] <= y && y < p1[1])) {
        let x = ((y - p1[1]) * (p2[0] - p1[0])) / (p2[1] - p1[1]) + p1[0];
        intersections.push(x);
      }
    }

    // Sort the intersections in increasing order
    intersections.sort((a, b) => a - b);

    // Fill in the pixels between each pair of intersections
    for (let i = 0; i < intersections.length; i += 2) {
      let x1 = floor(intersections[i]);
      let x2 = ceil(intersections[i + 1]);
      for (let x = x1; x < x2; x++) {
        point(x, y);
      }
    }
  }
}

// Renders a square grid at x, y given cols, rows, and scale.
// Buffer is optional, and if present will render the pixels at scale starting
// from the top left corner of the buffer, repeating if needed to fill the grid.

function grid(
  {
    box: { x, y, w: cols, h: rows },
    transform: { scale, angle, width: twidth, height: theight, anchor },
    centers = [],
  },
  buffer,
) {
  const oc = c.slice(); // Remember the original color.

  let w, h;

  // Note: `transform` is a bit overloaded here and looks for the presence
  //        of certain properties to determine the behavior.
  //        See `function paste` here in `graph` for an example usage.
  if (scale !== undefined) {
    if (number(scale)) scale = { x: scale, y: scale };
    w = cols * scale.x;
    h = rows * scale.y;

    //       ❤️‍🔥
    // TODO: Allow for scaleX and scaleY values on transform / let scale
    //       be an input array.
  } else if (twidth !== undefined && theight !== undefined) {
    w = twidth;
    h = theight;
    scale = { x: w / cols, y: h / rows };
    //      ^ Give scale a separate width and height.
  }

  const colPix = w / cols,
    rowPix = h / rows;

  if (scale.x < 0) x -= w + 1;
  if (scale.y < 0) y -= h + 1;

  angle = wrap(angle, 360); // Keep angle positive.

  // Always make sure we are at the mid-point of the pixel we rotate around.
  // given the image resolution's even / oddness on each axis.
  // (Make some off by 1 adjustments for specific angles.)

  let xmod = 0,
    ymod = 0;

  if (angle) {
    // Odd width.
    if (w % 2 !== 0 && h % 2 === 0) {
      if (x % 1 !== 0 && angle === 90) xmod += 0.5;

      // if (angle === 90 || angle === 270) ymod += 1;
      if (angle === 270) {
        xmod += x % 1 !== 0 ? 1.0 : 0.5;
        ymod += 0.5;
      }

      if (angle === 180) {
        ymod += 0.5;
        if (x % 1 === 0) xmod += 0.5;
      }
    }
    if (h % 2 !== 0 && w % 2 === 0) {
      // Odd height.
      if (y % 1 !== 0 && angle === 90) ymod += 0.5;

      if (angle === 270) {
        xmod += 0.5;
        ymod += y % 1 !== 0 ? 1.0 : 0.5;
      }

      if (angle === 180) {
        xmod += 0.5;
        if (y % 1 === 0) ymod += 0.5;
      }
    } else if (w % 2 === 0 && h % 2 === 0) {
      // Both even...
      xmod += 0.5;
      ymod += 0.5;
    } else if (w % 2 !== 0 && h % 2 !== 0) {
      // Both odd...
      xmod += 0.5;
      ymod += 0.5;
    }
  }

  // Make off by 1 adjustments for specific scale inverstions.
  // (This is kind of hacky. 23.07.20.13.44)
  if (scale.x < 0 && scale.y > 0) {
    if (angle >= 90 && angle < 270) ymod += 1 * sign(-scale.y);
    if (angle >= 180 && angle <= 270) xmod += 1 * sign(-scale.x);
  } else if (scale.x > 0 && scale.y > 0) {
    if (angle >= 90 && angle < 270) xmod += 1 * sign(-scale.x);
    if (angle >= 180 && angle <= 270) ymod += 1 * sign(-scale.y);
  } else if (scale.y < 0 && scale.x > 0) {
    if (angle >= 90 && angle < 270) ymod += 1 * sign(-scale.y);
    if (angle >= 180 && angle <= 270) xmod += 1 * sign(-scale.x);
  } else if (scale.y < 0 && scale.x < 0) {
    if (angle >= 90 && angle < 270) xmod += 1 * sign(-scale.x);
    if (angle >= 180 && angle <= 270) ymod += 1 * sign(-scale.y);
  }

  x += xmod;
  y += ymod;

  // Rotate around the anchor, or center.
  let centerX, centerY;
  if (anchor) {
    centerX = anchor.x + x;
    centerY = anchor.y + y;
  } else {
    centerX = x + w / 2;
    centerY = y + h / 2;
  }

  angle = radians(angle); // Sets angle to 0 if it was undefined.
  // Draw a scaled image if the buffer is present.
  if (buffer) {
    const bufWidth = buffer.width;
    const bufHeight = buffer.height;
    const bufPixels = buffer.pixels;

    // Use fast integer conversions and pre-calculate values
    const scaleXAbs = ~~abs(scale.x); // Fast float-to-int conversion
    const scaleYAbs = ~~abs(scale.y);
    const isAngleZero = angle === 0;

    // Pre-calculate trigonometric values only if needed
    let cosValue, sinValue;
    if (!isAngleZero) {
      cosValue = cos(angle);
      sinValue = sin(angle);
    }

    // Pre-calculate integer boundaries and scales
    const bufferWidth = scaleXAbs;
    const bufferHeight = scaleYAbs;
    const halfBoxWidth = bufferWidth >> 1; // Fast division by 2
    const halfBoxHeight = bufferHeight >> 1;
    const adjustedBufferWidth = bufferWidth + (halfBoxWidth << 1); // Fast multiplication by 2
    const adjustedBufferHeight = bufferHeight + (halfBoxHeight << 1);

    // Pre-calculate row and column pixel increments
    const colPixInt = ~~(w / cols);
    const rowPixInt = ~~(h / rows);

    // Fast path for simple scaling (no rotation, integer scales)
    if (
      isAngleZero &&
      scale.x === scaleXAbs &&
      scale.y === scaleYAbs &&
      scale.x > 0 &&
      scale.y > 0
    ) {
      // Ultra-fast nearest-neighbor scaling with direct buffer operations
      for (let j = 0; j < rows; j += 1) {
        const srcY = j % bufHeight;
        const destStartY = ~~(y + j * rowPixInt);
        const destEndY = ~~(y + (j + 1) * rowPixInt);
        const pixelHeight = destEndY - destStartY;

        if (pixelHeight > 0 && destStartY >= 0 && destEndY <= height) {
          for (let i = 0; i < cols; i += 1) {
            const srcX = i % bufWidth;            const srcIndex = (srcX + srcY * bufWidth) << 2; // Fast * 4
            if (srcIndex < bufPixels.length) {
              // Extract color data directly
              const r = bufPixels[srcIndex];
              const g = bufPixels[srcIndex + 1];
              const b = bufPixels[srcIndex + 2];
              const a = bufPixels[srcIndex + 3];

              const destStartX = ~~(x + i * colPixInt);
              const destEndX = ~~(x + (i + 1) * colPixInt);
              const pixelWidth = destEndX - destStartX;

              if (pixelWidth > 0 && destStartX >= 0 && destEndX <= width) {
                // Set color once and draw block efficiently
                color(r, g, b, a);

                // Use box for rectangular fills when possible (faster than multiple lineh calls)
                if (pixelWidth > 1 && pixelHeight > 1) {
                  box(destStartX, destStartY, pixelWidth, pixelHeight, "fill");
                } else if (pixelHeight === 1) {
                  // Single horizontal line
                  lineh(destStartX, destEndX - 1, destStartY);
                } else {
                  // Vertical line or single pixel
                  for (let dy = 0; dy < pixelHeight; dy += 1) {
                    lineh(destStartX, destEndX - 1, destStartY + dy);
                  }
                }
              }
            }
          }
        }
      }
    } else {
      // Complex path for rotation and non-integer scaling (preserve existing functionality)
      for (let j = 0; j < rows; j += 1) {
        const plotY = y + rowPix * j;
        const repeatY = j % bufHeight;

        for (let i = 0; i < cols; i += 1) {
          const plotX = x + colPix * i;

          let finalX, finalY;

          if (isAngleZero) {
            finalX = plotX;
            finalY = plotY;
          } else {
            // Rotate the plot coordinates around the center of the grid
            const dx = plotX - centerX;
            const dy = plotY - centerY;
            finalX = dx * cosValue - dy * sinValue + centerX;
            finalY = dx * sinValue + dy * cosValue + centerY;
          }

          // Adjusted boundary checks
          if (
            finalX < -adjustedBufferWidth ||
            finalX > width + adjustedBufferWidth ||
            finalY < -adjustedBufferHeight ||
            finalY > height + adjustedBufferHeight
          ) {
            continue; // Skip drawing this box
          }

          // Find the proper color
          const repeatX = i % bufWidth;
          const pixIndex = (repeatX + bufWidth * repeatY) << 2; // Fast multiplication by 4

          if (pixIndex < bufPixels.length) {
            const colorData = [
              bufPixels[pixIndex],
              bufPixels[pixIndex + 1],
              bufPixels[pixIndex + 2],
              bufPixels[pixIndex + 3],
            ];
            color(...colorData);

            // Calculate destination pixel ranges ensuring no gaps
            const scaleX = abs(scale.x);
            const scaleY = abs(scale.y);

            const destStartX = ~~(x + i * scaleX); // Fast floor conversion
            const destEndX = ~~(x + (i + 1) * scaleX);
            const destStartY = ~~(y + j * scaleY);
            const destEndY = ~~(y + (j + 1) * scaleY);

            const pixelWidth = destEndX - destStartX;
            const pixelHeight = destEndY - destStartY;

            // Only draw if there's actually area to fill
            if (pixelWidth > 0 && pixelHeight > 0) {
              // Apply rotation if needed
              if (isAngleZero) {
                // Optimized no-rotation path
                if (pixelWidth > 1 && pixelHeight > 1) {
                  // Use box for efficiency when drawing rectangles
                  box(destStartX, destStartY, pixelWidth, pixelHeight, "fill");
                } else {
                  // Fall back to line drawing for thin regions
                  for (let dy = 0; dy < pixelHeight; dy += 1) {
                    lineh(destStartX, destEndX - 1, destStartY + dy);
                  }
                }
              } else {
                // Rotation path - preserve existing pixel-by-pixel approach for accuracy
                for (let dy = 0; dy < pixelHeight; dy += 1) {
                  for (let dx = 0; dx < pixelWidth; dx += 1) {
                    const px = destStartX + dx;
                    const py = destStartY + dy;

                    // Rotate around center
                    const relX = px - centerX;
                    const relY = py - centerY;
                    const rotX = relX * cosValue - relY * sinValue + centerX;
                    const rotY = relX * sinValue + relY * cosValue + centerY;

                    plot(~~rotX, ~~rotY); // Fast floor conversion
                  }
                }
              }
            }
          }
        }
      }
    }
  } else {
    // Draw a debug / blueprint grid if no buffer is present.

    // Plot a point in each of the four corners.
    const right = x + w - 1,
      bottom = y + h - 1;

    color(64, 64, 64);
    plot(x, y);
    plot(right, y);
    plot(x, bottom);
    plot(right, bottom);
    color(...oc);

    // Draw each grid square, with optional center points.
    for (let i = 0; i < cols; i += 1) {
      const plotX = x + colPix * i;
      for (let j = 0; j < rows; j += 1) {
        const plotY = y + rowPix * j;

        // Lightly shade this grid square, alternating tint on evens and odds.
        const alphaMod = oc[3] / 255;
        color(oc[0], oc[1], oc[2], even(i + j) ? 50 * alphaMod : 75 * alphaMod);
        box(plotX, plotY, scale.x, scale.y);

        // Color in the centers of each grid square.
        centers.forEach((p) => {
          color(oc[0], oc[1], oc[2], 100);
          plot(plotX + p.x, plotY + p.y);
        });
      }
    }

    color(...oc); // Restore color.
  }
}

// Rendering stored drawings.
// Silently fails if `drawing` is left `undefined`.
// Params:
//   drawing, x, y, scale = 1, angle = 0, thickness = 1
//   drawing, position, scale = 1, angle = 0, thickness = 1
function draw() {
  const args = arguments;
  let drawing = args[0],
    x,
    y,
    scale = 1,
    angle = 0,
    thickness = 1;
  if (typeof args[1] === "number") {
    x = args[1];
    y = args[2];
    scale = args[3] || scale;
    angle = args[4] || angle;
    thickness = args[5] || thickness;
  } else if (typeof args[1] === "object") {
    drawing = args[0];
    if (Array.isArray(args[1])) {
      x = args[1][0];
      y = args[1][1];
    } else {
      x = args[1].x;
      y = args[1].y;
    }
    scale = args[2] || scale;
    angle = args[3] || angle;
    thickness = args[4] || thickness;
  }

  if (drawing === undefined) return;

  // TODO: Eventually make this the call: rotatePoint(args[0], args[1], 0, 0);
  angle = radians(angle);
  const s = sin(angle);
  const c = cos(angle);

  x = floor(x);
  y = floor(y);

  pan(x, y);

  // 🍎 Build a poly line out of subsequent points sharing start & end points.
  // 🧠 And plot other commands.
  const gesture = []; // Keep track of continuous lines.

  function paintGesture() {
    // Draw each gesture path and then kill it.
    thickness === 1 ? poly(gesture) : pline(gesture, thickness);
    gesture.length = 0;
  }

  drawing?.commands?.forEach(({ name, args }, i) => {
    args = args.map((a) => a * scale);

    // console.log(name, i, drawing.commands.length, drawing);

    if (name === "line") {
      let x1 = args[0]; // x1
      let y1 = args[1]; // y1
      let x2 = args[2]; // x2
      let y2 = args[3]; // y2
      let nx1 = x1 * c - y1 * s;
      let ny1 = x1 * s + y1 * c;
      let nx2 = x2 * c - y2 * s;
      let ny2 = x2 * s + y2 * c;

      if (nx1 !== nx2 || ny1 !== ny2) {
        if (thickness === 1) {
          gesture.push([nx1, ny1], [nx2, ny2]);
        } else {
          gesture.push({ x: nx1, y: ny1 }, { x: nx2, y: ny2 });
        }
      }

      const nextCommand = drawing.commands[i + 1];
      if (nextCommand && nextCommand.name === "line") {
        const nextArgs = nextCommand.args.map((a) => a * scale);
        if (args[2] !== nextArgs[0] || args[3] !== nextArgs[1]) {
          // If the last point of cur line and 1st point of next line are diff.
          // Then we can paint!
          paintGesture();
        } else {
          // Otherwise we should pop off the last point to avoid repeats.
          gesture.pop();
        }
      } else {
        paintGesture();
      }
    } else if (name === "point") {
      thickness === 1
        ? point(...args)
        : circle(args[0], args[1], thickness / 2, true);
    }
  });

  pan(-x, -y);
}

// Write out a line of text.
// TODO: Add directionality using a bresenham algorithm.
//       - Must know about height.
// TODO: Abstract this to another level, similar to 'draw' above.
//       - I would need to get the final drawing API and pass that to
//         a module that builds on it, then also has functions that
//         get added back to it. This would be *graph: layer 2*.
function printLine(
  text,
  font,
  startX,
  startY,
  blockWidth = 6,
  scale = 1,
  xOffset = 0,
  thickness = 1,
  rotation = 0,
) {
  if (!text) return;

  // Early culling for mask bounds
  if (activeMask) {
    const lineHeight = font?.A?.box?.height || 10; // Estimate line height
    const scaledHeight = lineHeight * scale;

    // Check if the entire line is outside the mask bounds
    if (
      startY >= activeMask.y + activeMask.height ||
      startY + scaledHeight <= activeMask.y ||
      startX >= activeMask.x + activeMask.width ||
      startX + text.length * blockWidth * scale <= activeMask.x
    ) {
      return; // Don't render if completely outside mask
    }
  }

  [...text.toString()].forEach((char, i) => {
    draw(
      font[char],
      startX + blockWidth * scale * i + xOffset,
      startY,
      scale,
      rotation,
      thickness,
    );
  });
}

function noise16() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      pixels[i] = byteInterval17(randInt(16)); // r
      pixels[i + 1] = byteInterval17(randInt(16)); // g
      pixels[i + 2] = byteInterval17(randInt(16)); // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noise16DIGITPAIN() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      pixels[i] = byteInterval17(randInt(16)) * 0.6; // r
      pixels[i + 1] = byteInterval17(randInt(16)) * 0.15; // g
      pixels[i + 2] = byteInterval17(randInt(16)) * 0.55; // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noise16Aesthetic() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      pixels[i] = byteInterval17(randInt(16)) * 0.4; // r
      pixels[i + 1] = byteInterval17(randInt(16)) * 0.15; // g
      pixels[i + 2] = byteInterval17(randInt(16)) * 0.8; // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noise16Sotce() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      if (flip()) pixels[i] = byteInterval17(14 + randInt(2)); // r
      if (flip()) pixels[i + 1] = byteInterval17(8 + randInt(2)) * 0.9; // g
      if (flip()) pixels[i + 2] = byteInterval17(8 + randInt(2)) * 0.9; // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noiseTinted(tint, amount, saturation) {
  // console.log("Tinting:", tint, amount, saturation);
  tint = findColor(tint);
  for (let i = 0; i < pixels.length; i += 4) {
    const grayscale = randInt(255);
    pixels[i] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[0],
      amount,
    ); // r
    pixels[i + 1] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[1],
      amount,
    ); // g
    pixels[i + 2] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[2],
      amount,
    ); // b
    pixels[i + 3] = 255; // a
  }
}

// Accumulated fractional steps for smooth fractional spinning
let spinAccumulator = 0;

// Accumulated fractional zoom for smooth zooming
let zoomAccumulator = 1.0;

// Accumulated fractional scroll for smooth scrolling
let scrollAccumulatorX = 0;
let scrollAccumulatorY = 0;

// Accumulated fractional shear for smooth shearing
let shearAccumulatorX = 0;
let shearAccumulatorY = 0;

// Per-pixel shear accumulation for ensuring all pixels eventually move
let pixelShearAccumX = null;
let pixelShearAccumY = null;

// Scroll the entire pixel buffer by x and/or y pixels with wrapping
function scroll(dx = 0, dy = 0) {
  if (dx === 0 && dy === 0) return; // No change needed

  // Accumulate fractional scroll amounts
  scrollAccumulatorX += dx;
  scrollAccumulatorY += dy;

  // Extract integer parts for actual scrolling
  const integerDx = Math.floor(scrollAccumulatorX);
  const integerDy = Math.floor(scrollAccumulatorY);

  // Keep fractional remainders
  scrollAccumulatorX -= integerDx;
  scrollAccumulatorY -= integerDy;

  // Only proceed if we have integer pixels to scroll
  if (integerDx === 0 && integerDy === 0) return;

  // Determine the area to scroll (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Apply pan translation to mask bounds and ensure they're within screen bounds
    minX = Math.max(0, Math.min(width, activeMask.x + panTranslation.x));
    maxX = Math.max(
      0,
      Math.min(width, activeMask.x + activeMask.width + panTranslation.x),
    );
    minY = Math.max(0, Math.min(height, activeMask.y + panTranslation.y));
    maxY = Math.max(
      0,
      Math.min(height, activeMask.y + activeMask.height + panTranslation.y),
    );
  }

  const boundsWidth = maxX - minX;
  const boundsHeight = maxY - minY;

  // Early exit if bounds are invalid
  if (boundsWidth <= 0 || boundsHeight <= 0) return;

  // Use integer scroll amounts
  let finalDx = ((integerDx % boundsWidth) + boundsWidth) % boundsWidth;
  let finalDy = ((integerDy % boundsHeight) + boundsHeight) % boundsHeight;

  if (finalDx === 0 && finalDy === 0) return; // No effective shift after normalization

  // Create a complete copy of the working area for safe reading
  const tempPixels = new Uint8ClampedArray(pixels);

  // General case: pixel-by-pixel with proper bounds checking
  for (let y = 0; y < boundsHeight; y++) {
    for (let x = 0; x < boundsWidth; x++) {
      // Calculate source coordinates with wrapping within bounds
      const srcX = minX + ((x + boundsWidth - finalDx) % boundsWidth);
      const srcY = minY + ((y + boundsHeight - finalDy) % boundsHeight);

      // Calculate destination coordinates
      const destX = minX + x;
      const destY = minY + y;

      // Ensure coordinates are within valid bounds
      if (
        srcX >= minX &&
        srcX < maxX &&
        srcY >= minY &&
        srcY < maxY &&
        destX >= minX &&
        destX < maxX &&
        destY >= minY &&
        destY < maxY
      ) {
        const srcOffset = (srcY * width + srcX) * 4;
        const destOffset = (destY * width + destX) * 4;

        // Copy RGBA values
        pixels[destOffset] = tempPixels[srcOffset];
        pixels[destOffset + 1] = tempPixels[srcOffset + 1];
        pixels[destOffset + 2] = tempPixels[srcOffset + 2];
        pixels[destOffset + 3] = tempPixels[srcOffset + 3];
      }
    }
  }
}

// Rotates pixels in concentric rings around a specified anchor point
// steps: positive for clockwise, negative for counterclockwise
// anchorX, anchorY: optional anchor point (defaults to center of working area)
// Each ring rotates by exactly 'steps' pixels, preserving all data
// Supports fractional steps by accumulating them over time
function spin(steps = 0, anchorX = null, anchorY = null) {
  if (steps === 0) return;

  // Handle fractional steps by accumulating them
  spinAccumulator += steps;
  const integerSteps = floor(spinAccumulator);
  spinAccumulator -= integerSteps; // Keep the fractional remainder
  if (integerSteps === 0) return; // No integer steps to process yet

  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Apply pan translation to mask bounds
    minX = activeMask.x + panTranslation.x;
    minY = activeMask.y + panTranslation.y;
    maxX = activeMask.x + activeMask.width + panTranslation.x;
    maxY = activeMask.y + activeMask.height + panTranslation.y;
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;
  // Use provided anchor point or default to center of working area
  const centerX = anchorX !== null ? anchorX : minX + floor(workingWidth / 2);
  const centerY = anchorY !== null ? anchorY : minY + floor(workingHeight / 2);

  // Calculate maximum ring radius to reach the furthest corner
  const maxRadius =
    floor(
      sqrt(
        max(
          (centerX - minX) * (centerX - minX) +
            (centerY - minY) * (centerY - minY),
          (maxX - 1 - centerX) * (maxX - 1 - centerX) +
            (centerY - minY) * (centerY - minY),
          (centerX - minX) * (centerX - minX) +
            (maxY - 1 - centerY) * (maxY - 1 - centerY),
          (maxX - 1 - centerX) * (maxX - 1 - centerX) +
            (maxY - 1 - centerY) * (maxY - 1 - centerY),
        ),
      ),
    ) + 1;

  if (maxRadius < 1) return;

  // Create a copy of the pixels to read from
  const tempPixels = new Uint8ClampedArray(pixels);

  // Group pixels by radius in a single pass - much faster!
  const ringsByRadius = new Array(maxRadius + 1);
  for (let i = 0; i <= maxRadius; i++) {
    ringsByRadius[i] = [];
  }

  // Single pass to collect all pixels and group by radius
  for (let y = minY; y < maxY; y++) {
    const dy = y - centerY;
    const dy2 = dy * dy; // Cache dy squared

    for (let x = minX; x < maxX; x++) {
      const dx = x - centerX;
      const distanceSquared = dx * dx + dy2;
      const radius = floor(sqrt(distanceSquared) + 0.5);

      if (radius >= 1 && radius <= maxRadius) {
        const idx = (y * width + x) * 4;
        const pixel = [
          tempPixels[idx],
          tempPixels[idx + 1],
          tempPixels[idx + 2],
          tempPixels[idx + 3],
        ];

        // Pre-calculate angle for sorting
        const angle = Math.atan2(dy, dx);
        const normalizedAngle = angle < 0 ? angle + 2 * PI : angle;

        ringsByRadius[radius].push({
          x,
          y,
          pixel,
          angle: normalizedAngle,
        });
      }
    }
  }

  // Process each ring that has pixels
  for (let radius = 1; radius <= maxRadius; radius++) {
    const ringData = ringsByRadius[radius];
    if (ringData.length === 0) continue;

    // Sort once by pre-calculated angles - much faster than calculating during sort
    ringData.sort((a, b) => a.angle - b.angle);

    const ringSize = ringData.length;
    const effectiveSteps = ((integerSteps % ringSize) + ringSize) % ringSize;

    if (effectiveSteps === 0) continue; // No rotation needed

    // Direct pixel copying without intermediate arrays
    for (let i = 0; i < ringSize; i++) {
      const sourceIndex = i;
      const targetIndex = (i + effectiveSteps) % ringSize;

      const sourceData = ringData[sourceIndex];
      const targetPos = ringData[targetIndex];

      const idx = (targetPos.y * width + targetPos.x) * 4;
      const sourcePixel = sourceData.pixel;

      pixels[idx] = sourcePixel[0];
      pixels[idx + 1] = sourcePixel[1];
      pixels[idx + 2] = sourcePixel[2];
      pixels[idx + 3] = sourcePixel[3];
    }
  }
}

// Zoom the entire pixel buffer with 1.0 as neutral (no change)
// level < 1.0 zooms out, level > 1.0 zooms in, level = 1.0 does nothing
// anchorX, anchorY: 0.0 = top/left, 0.5 = center, 1.0 = bottom/right
// Uses bilinear sampling with hard-edge thresholding for smooth scaling with crisp output
function zoom(level = 1, anchorX = 0.5, anchorY = 0.5) {
  if (level === 1.0) return; // No change needed - neutral zoom
  // Accumulate zoom level for smooth fractional zoom support
  zoomAccumulator *= level;

  // Use smaller threshold for smoother transitions
  const threshold = 0.001; // Much smaller threshold for smoother motion
  if (Math.abs(Math.log(zoomAccumulator)) < threshold) return;

  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    minX = activeMask.x;
    minY = activeMask.y;
    maxX = activeMask.x + activeMask.width;
    maxY = activeMask.y + activeMask.height;
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;

  // Calculate anchor point in pixel coordinates within working area
  const anchorPixelX = minX + workingWidth * anchorX;
  const anchorPixelY = minY + workingHeight * anchorY;

  // Create a copy of the current pixels to read from
  const tempPixels = new Uint8ClampedArray(pixels);

  const scale = zoomAccumulator;
  const invScale = 1.0 / scale;
  // Pre-calculate constants for performance
  const widthTimes4 = width * 4;

  // Bilinear math for smooth positioning, nearest neighbor sampling for crisp output
  for (let destY = minY; destY < maxY; destY++) {
    const destRowOffset = destY * widthTimes4;

    for (let destX = minX; destX < maxX; destX++) {
      // Convert destination to texture coordinates relative to anchor (bilinear math)
      const texX = (destX - anchorPixelX) * invScale + anchorPixelX;
      const texY = (destY - anchorPixelY) * invScale + anchorPixelY;

      // Use bilinear interpolation math to find the ideal sampling position
      const x1 = Math.floor(texX);
      const y1 = Math.floor(texY);
      const fx = texX - x1;
      const fy = texY - y1;

      // Calculate bilinear weights to determine sampling bias
      const w00 = (1 - fx) * (1 - fy); // top-left
      const w01 = fx * (1 - fy); // top-right
      const w10 = (1 - fx) * fy; // bottom-left
      const w11 = fx * fy; // bottom-right

      // Find which quadrant has the most influence
      const weights = [w00, w01, w10, w11];
      const offsets = [
        [0, 0],
        [1, 0],
        [0, 1],
        [1, 1],
      ];
      let maxWeight = 0;
      let bestOffset = [0, 0];

      for (let i = 0; i < 4; i++) {
        if (weights[i] > maxWeight) {
          maxWeight = weights[i];
          bestOffset = offsets[i];
        }
      }

      // Apply nearest neighbor sampling at the bilinear-determined position
      const srcX = x1 + bestOffset[0];
      const srcY = y1 + bestOffset[1];

      // Wrap source coordinates within working bounds
      let wrappedSrcX = ((srcX - minX) % workingWidth) + workingWidth;
      let wrappedSrcY = ((srcY - minY) % workingHeight) + workingHeight;

      wrappedSrcX =
        wrappedSrcX >= workingWidth ? wrappedSrcX - workingWidth : wrappedSrcX;
      wrappedSrcY =
        wrappedSrcY >= workingHeight
          ? wrappedSrcY - workingHeight
          : wrappedSrcY;

      // Convert back to absolute coordinates
      const finalSrcX = minX + wrappedSrcX;
      const finalSrcY = minY + wrappedSrcY;

      // Ensure coordinates are within valid bounds
      if (
        finalSrcX >= minX &&
        finalSrcX < maxX &&
        finalSrcY >= minY &&
        finalSrcY < maxY &&
        destX >= minX &&
        destX < maxX &&
        destY >= minY &&
        destY < maxY
      ) {
        const srcIdx = (finalSrcY * width + finalSrcX) * 4;
        const destIdx = destRowOffset + destX * 4;

        // Copy RGBA values
        pixels[destIdx] = tempPixels[srcIdx];
        pixels[destIdx + 1] = tempPixels[srcIdx + 1];
        pixels[destIdx + 2] = tempPixels[srcIdx + 2];
        pixels[destIdx + 3] = tempPixels[srcIdx + 3];
      }
    }
  }

  // Reset zoom accumulator after applying
  zoomAccumulator = 1.0;
}

// Sort pixels by color within the masked area (or entire screen if no mask)
// Sorts by luminance (brightness) - darker pixels first, lighter pixels last
function sort() {
  // Determine the area to sort (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Apply pan translation to mask bounds
    const maskX = activeMask.x + panTranslation.x;
    const maskY = activeMask.y + panTranslation.y;
    minX = Math.max(0, Math.floor(maskX));
    minY = Math.max(0, Math.floor(maskY));
    maxX = Math.min(width, Math.floor(maskX + activeMask.width));
    maxY = Math.min(height, Math.floor(maskY + activeMask.height));
  }

  // Collect all pixels in the area
  const pixelsToSort = [];
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const index = (y * width + x) * 4;
      const r = pixels[index];
      const g = pixels[index + 1];
      const b = pixels[index + 2];
      const a = pixels[index + 3];

      // Calculate luminance for sorting (standard formula)
      const luminance = 0.299 * r + 0.587 * g + 0.114 * b;

      pixelsToSort.push({
        r,
        g,
        b,
        a,
        luminance,
      });
    }
  }

  // Sort by luminance (darker to lighter)
  pixelsToSort.sort((a, b) => a.luminance - b.luminance);

  // Write sorted pixels back to their positions
  let sortedIndex = 0;
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const index = (y * width + x) * 4;
      const sortedPixel = pixelsToSort[sortedIndex];

      pixels[index] = sortedPixel.r;
      pixels[index + 1] = sortedPixel.g;
      pixels[index + 2] = sortedPixel.b;
      pixels[index + 3] = sortedPixel.a;

      sortedIndex++;
    }
  }
}

// Copy a region from the current screen buffer into a new buffer
// Returns a buffer object that can be used with paste()
function copyRegion(x, y, w, h) {
  x = Math.floor(x);
  y = Math.floor(y);
  w = Math.floor(w);
  h = Math.floor(h);

  // Apply pan translation to coordinates
  x += panTranslation.x;
  y += panTranslation.y;

  // Clamp to screen bounds
  x = Math.max(0, Math.min(x, width));
  y = Math.max(0, Math.min(y, height));
  w = Math.max(0, Math.min(w, width - x));
  h = Math.max(0, Math.min(h, height - y));

  if (w <= 0 || h <= 0) {
    return null;
  }

  // Create new buffer using ImageData like makeBuffer does
  const imageData = new ImageData(w, h);
  const buffer = {
    pixels: imageData.data,
    width: imageData.width,
    height: imageData.height,
  };

  // Copy pixels from screen buffer to new buffer
  for (let srcY = 0; srcY < h; srcY++) {
    for (let srcX = 0; srcX < w; srcX++) {
      const srcIndex = ((y + srcY) * width + (x + srcX)) * 4;
      const destIndex = (srcY * w + srcX) * 4;

      if (srcIndex >= 0 && srcIndex < pixels.length - 3) {
        buffer.pixels[destIndex] = pixels[srcIndex];
        buffer.pixels[destIndex + 1] = pixels[srcIndex + 1];
        buffer.pixels[destIndex + 2] = pixels[srcIndex + 2];
        buffer.pixels[destIndex + 3] = pixels[srcIndex + 3];
      }
    }
  }

  return buffer;
}

let stolen;

// TODO: Steal should run copyregion and keep the buffer in a global 'stolen' variable.
function steal(x, y, width, height) {
  stolen = copyRegion(x, y, width, height);
  return stolen;
}

// Paste the stolen buffer at the specified coordinates with optional scaling
function putback(x, y, scale = 1) {
  if (!stolen) return;
  const result = paste(stolen, x, y, scale);
  return result;
}

// KidPix-style shear function
// shearX: horizontal shear amount (positive = right lean, negative = left lean)
// Simple shear by moving entire rows/columns
// shearX: horizontal shear factor (positive = right lean, negative = left lean)
// shearY: vertical shear factor (positive = down lean, negative = up lean)
function shear(shearX = 0, shearY = 0) {
  if (shearX === 0 && shearY === 0) return;

  // Accumulate fractional shear amounts
  shearAccumulatorX += shearX;
  shearAccumulatorY += shearY;

  const finalShearX = shearAccumulatorX;
  const finalShearY = shearAccumulatorY;

  shearAccumulatorX = 0;
  shearAccumulatorY = 0;

  // Work area bounds
  let minX = 0,
    maxX = width,
    minY = 0,
    maxY = height;
  if (activeMask) {
    minX = Math.max(0, Math.min(width, activeMask.x + panTranslation.x));
    maxX = Math.max(
      0,
      Math.min(width, activeMask.x + activeMask.width + panTranslation.x),
    );
    minY = Math.max(0, Math.min(height, activeMask.y + panTranslation.y));
    maxY = Math.max(
      0,
      Math.min(height, activeMask.y + activeMask.height + panTranslation.y),
    );
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;
  if (workingWidth <= 0 || workingHeight <= 0) return;

  const tempPixels = new Uint8ClampedArray(pixels);
  const centerY = workingHeight / 2;
  const centerX = workingWidth / 2; // Horizontal shear: each row shifts more based on distance from center
  if (finalShearX !== 0) {
    for (let y = 0; y < workingHeight; y++) {
      const distFromCenter = y - workingHeight / 2;
      const rowShift = Math.round(finalShearX * distFromCenter);

      for (let x = 0; x < workingWidth; x++) {
        let srcX = x - rowShift;
        srcX = ((srcX % workingWidth) + workingWidth) % workingWidth;

        const srcOffset = ((minY + y) * width + (minX + srcX)) * 4;
        const destOffset = ((minY + y) * width + (minX + x)) * 4;

        pixels[destOffset] = tempPixels[srcOffset];
        pixels[destOffset + 1] = tempPixels[srcOffset + 1];
        pixels[destOffset + 2] = tempPixels[srcOffset + 2];
        pixels[destOffset + 3] = tempPixels[srcOffset + 3];
      }
    }
    tempPixels.set(pixels);
  } // Vertical shear: each column shifts more based on distance from center
  if (finalShearY !== 0) {
    for (let x = 0; x < workingWidth; x++) {
      const distFromCenter = x - workingWidth / 2;
      const colShift = Math.round(finalShearY * distFromCenter);

      for (let y = 0; y < workingHeight; y++) {
        let srcY = y - colShift;
        srcY = ((srcY % workingHeight) + workingHeight) % workingHeight;

        const srcOffset = ((minY + srcY) * width + (minX + x)) * 4;
        const destOffset = ((minY + y) * width + (minX + x)) * 4;

        pixels[destOffset] = tempPixels[srcOffset];
        pixels[destOffset + 1] = tempPixels[srcOffset + 1];
        pixels[destOffset + 2] = tempPixels[srcOffset + 2];
        pixels[destOffset + 3] = tempPixels[srcOffset + 3];
      }
    }
  }
}

// b. Geometric Abstractions

// For producing a projection matrix.
// For matrix & linear algebra help: https://www.youtube.com/playlist?list=PLZHQObOWTQDPD3MizzM2xVFitgF8hE_ab
class Camera {
  type = "perspective";
  matrix;
  #x = 0;
  #rotX = 0;
  #y = 0;
  #rotY = 0;
  #z = 0;
  #rotZ = 0;
  fov;

  near = 0.001;
  far = 1000;

  position = [0, 0, 0, 1];
  rotation = [0, 0, 0];
  scale = [1, 1, 1];

  // centerCached; // Saved after each call to `center()`.

  perspectiveMatrix;
  #transformMatrix;

  // Takes x, y, z position and an optional scale (xyz) array.
  constructor(fov = 80, { x, y, z, scale } = { x: 0, y: 0, z: 0, scale: 1 }) {
    this.fov = fov;

    this.x = x;
    this.y = y;
    this.z = z;

    if (scale) this.scale = scale;

    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
  }

  set rotX(n) {
    this.#rotX = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.rotation[0] = n;
  }

  get rotX() {
    return this.#rotX;
  }

  set rotY(n) {
    this.#rotY = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.rotation[1] = n;
  }

  get rotY() {
    return this.#rotY;
  }

  set rotZ(n) {
    this.#rotZ = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.rotation[2] = n;
  }

  get rotZ() {
    return this.#rotZ;
  }

  // Returns the rotation of the camera in radians.
  get rot() {
    return [this.#rotX, this.#rotY, this.#rotZ];
  }

  set x(n = 0) {
    this.#x = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.position[0] = n;
  }

  get x() {
    return this.#x;
  }

  set y(n = 0) {
    this.#y = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.position[1] = n;
  }

  get y() {
    return this.#y;
  }

  set z(n = 0) {
    this.#z = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.position[2] = n;
  }

  get z() {
    return this.#z;
  }

  forward(n) {
    this.#z -= n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
  }

  #perspective(fov) {
    const zNear = this.near;
    const zFar = this.far;

    this.perspectiveMatrix = mat4.perspective(
      mat4.create(),
      radians(fov),
      width / height,
      zNear,
      zFar,
    );

    // See: https://github.com/BennyQBD/3DSoftwareRenderer/blob/641f59125351d9565e744a90ad86256c3970a724/src/Matrix4f.java#L89
    // And compare it with: https://glmatrix.net/docs/mat4.js.html#line1508

    const zRange = zNear - zFar;
    const ten = (-zNear - zFar) / zRange;
    const fourteen = (2 * zFar * zNear) / zRange;

    this.perspectiveMatrix[10] = ten; // Zero the Z component.
    this.perspectiveMatrix[14] = fourteen;
    this.perspectiveMatrix[11] = 1; // Flip the Y so we see things rightside up.
  }

  get perspective() {
    return this.perspectiveMatrix;
  }

  // Recalculate the camera matrix for a new display constraint.
  // TODO: Eventually this should be redundant for custom cameras
  //       that don't hook into the `screen`. 24.02.21.15.26
  resize() {
    this.forward(0);
  }

  // Get an XYZ position on a plane at a given depth,
  // relative to screen coordinates.
  ray(X = width / 2, Y = height / 2, depth = 1, flippedY = false) {
    this.#perspective(this.fov);

    // 1. Camera World Space
    const pos = [...this.position];

    if (flippedY) pos[1] *= -1; // TODO: This is a little janky now, both CPU
    //                                   and GPU should have the same Y.

    const rotX = mat4.fromXRotation(mat4.create(), radians(this.#rotX));
    const rotY = mat4.fromYRotation(mat4.create(), radians(this.#rotY));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.#rotZ));

    const rotatedX = mat4.multiply(mat4.create(), rotX, mat4.create());
    const rotatedY = mat4.multiply(mat4.create(), rotY, rotatedX);
    const rotatedZ = mat4.multiply(mat4.create(), rotatedY, rotZ);

    const scaled = mat4.scale(mat4.create(), rotatedZ, this.scale);

    const world = scaled;

    // Camera World Space -> Inverted Perspective Projection
    const invertedProjection = mat4.invert(
      mat4.create(),
      this.perspectiveMatrix,
    );
    const invWorldPersProj = mat4.mul(mat4.create(), world, invertedProjection);

    // 2. Screen Point -> Inverted World Perspective Projection

    // Normalize from screen coordinates.
    X = 1 - X / width;
    Y = 1 - Y / height;

    // 2. -> Normalized Device Space (NDS)
    let x = 2.0 * X - 1;
    let y = 2.0 * Y - 1;

    // NDS -> Homogeneous Space
    // (flipped Z, because we are in a left-handed coordinate system.)
    const screenPos = vec4.fromValues(x, -y, 1, 1);

    // Adjust Z depth of plane... (scale screen position)
    const shiftedScreenPos = vec4.scale(vec4.create(), screenPos, depth);

    // Get near plane.
    const xyz = vec4.transformMat4(
      vec4.create(),
      shiftedScreenPos,
      invWorldPersProj,
    );

    // Subtract transformed point from camera position.
    const worldPos = vec4.sub(vec4.create(), pos, xyz);

    return worldPos;
  }

  #transform() {
    // TODO: Why does this and the FPS camera control need to be inverted?
    //       Can't I just somehow invert the matrix to avoid all the swapping?
    //       Maybe it has something to do with rotation order?
    //       The default in three.js is XYZ, but here I'm using YXZ which I
    //       had to override there. 22.10.09.20.31

    // Translation.
    const panned = mat4.translate(mat4.create(), mat4.create(), [
      this.#x,
      this.#y,
      this.#z,
    ]);

    // Rotation
    const rotY = mat4.fromYRotation(mat4.create(), radians(-this.#rotY)); // FLIPPED
    const rotX = mat4.fromXRotation(mat4.create(), radians(this.#rotX));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.#rotZ));
    const rotatedY = mat4.multiply(mat4.create(), rotY, panned);
    const rotatedX = mat4.multiply(mat4.create(), rotX, rotatedY);
    const rotatedZ = mat4.multiply(mat4.create(), rotZ, rotatedX);

    // Scale
    // TODO: Add support for camera scaling.
    const scaled = mat4.scale(mat4.create(), rotatedZ, this.scale);

    // Perspective
    this.#transformMatrix = mat4.multiply(
      mat4.create(),
      this.perspectiveMatrix,
      scaled,
    );
  }
}

// For moving a camera round over time.
// TODO: Add a track? (Rollercoaster / coast)
// TODO: Only supports sideways movement right now.
class Dolly {
  camera;

  xVel = 0;
  yVel = 0;
  zVel = 0;
  dec = 0.9;

  constructor(camera) {
    this.camera = camera;
  }

  sim() {
    this.xVel *= this.dec;
    this.yVel *= this.dec;
    this.zVel *= this.dec;

    if (abs(this.xVel) > 0) this.camera.x += this.xVel;
    if (abs(this.yVel) > 0) this.camera.y += this.yVel;
    if (abs(this.zVel) > 0) this.camera.z += this.zVel;
  }

  push({ x, y, z }) {
    // Strafe x and z.
    const xz = vec2.rotate(
      vec2.create(),
      vec2.fromValues(x, z),
      vec2.fromValues(0, 0),
      radians(-this.camera.rotY), // Take the camera Y axis for strafing.
    );

    this.xVel += xz[0] || 0;
    this.yVel += y || 0;
    this.zVel += xz[1] || 0;
  }
}

let formId = 0;

// Mesh
class Form {
  primitive = "triangle";
  type = "triangle";

  limiter = 0; // Only enabled on CPU rendered `line` at the moment. 22.11.06.18.19

  uid; // = nanoid(4); // An id to keep across threads. Takes ~4 milliseconds. 😢

  tag; // Gets sent to the GPU as a named / marked tag.
  // Currently only available on `buffered` types in `3d.mjs` 23.02.07.09.46

  // Model
  vertices = [];
  indices = [];

  // TODO: Texture and color should be optional, and perhaps based on type.
  // TODO: Should this use a parameter called shader?
  texture; // = makeBuffer(32, 32);
  color;
  colorModifier;

  // GPU Specific Params & Buffers
  gpuVerticesSent = 0;
  gpuReset = false; // Assumes this object is being recreated on the GPU.
  gpuKeep = true;
  gpuConvertColors = true;
  gpuTransformed = false;
  gpuRecolored = false;
  MAX_POINTS = 100000; // Some buffered geometry gpu calls may use this hint.
  uvs = [];

  #gradientColors = [
    [1.0, 0.0, 0.0, 1.0],
    [0.0, 1.0, 0.0, 1.0],
    [0.0, 0.0, 1.0, 1.0],
  ];

  /* I haven't needed support for this yet so it's left commented. 22.10.13.23.12
  #texCoords = [
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 1.0, 0.0, 0.0],
    [1.0, 1.0, 0.0, 0.0],
  ];
  */

  // Transform
  position = [0, 0, 0];
  rotation = [0, 0, 0];
  scale = [1, 1, 1];

  gradients = true;

  // Blending
  alpha = 1.0;

  constructor(
    // Model
    // `type` can be "triangle", or "line" or "line:buffered"
    // `positions` and colors can be sent and then verticies will be generated
    {
      type,
      vertices,
      uvs = [],
      positions,
      colors,
      gradients,
      indices,
      keep = true,
    },
    fill,
    // Transform
    transform,
  ) {
    this.gradients = gradients; // A flag to decide if we use gradients or not. Only for line3d right now. 22.11.06.02.00

    // Give an incremental id per session.
    this.uid = formId;
    formId += 1;

    // Set the primitive type.
    this.primitive = type;
    this.type = type;

    // Decide whether to throw this away after being drawn once
    this.gpuKeep = keep;

    // Take into account form -> primitive relationships.
    if (type === "quad") this.primitive = "triangle";
    if (type === "triangle:buffered") this.primitive = "triangle";
    if (type === "line:buffered") this.primitive = "line";

    this.indices = indices || repeat(positions?.length, (i) => i);

    // 🌩️ Ingest positions and turn them into vertices.
    // ("Import" a model...)

    // Switch fill to transform if the was skipped.
    if (fill?.pos || fill?.rot || fill?.scale) {
      transform = fill;
      fill = undefined;
    }

    // Assign texture or color.
    if (fill?.tex) this.texture = fill.tex;
    if (fill?.color) this.color = fill.color || c.slice();
    if (fill?.alpha) this.alpha = fill.alpha;

    // TODO: There is no maxed out notice here.
    if (positions?.length > 0)
      this.addPoints({ positions, colors }, this.indices);

    // Or just set vertices directly.
    if (vertices?.length > 0) {
      this.vertices = vertices;
      this.uvs = uvs;
    }

    this.position = transform?.pos || [0, 0, 0];
    this.rotation = transform?.rot || [0, 0, 0];

    if (typeof transform.scale === "number") {
      this.scale = [transform.scale, transform.scale, transform.scale];
    } else {
      this.scale = transform?.scale || [1, 1, 1];
    }
  }

  // TODO: This needs to support color (and eventually N vertex attributes).

  resetUID() {
    this.uid = nanoid(4);
    //this.uid = formId;
    //formId += 1;
  }

  // Clears vertex and index attributes to prepare for replacement geometry.
  clear() {
    this.uvs = [];
    this.vertices = [];
    this.indices = [];
    this.gpuReset = true;
    this.gpuVerticesSent = 0;
  }

  // How close we are to being beyond the max points allotted by the GPU for
  // buffer geometries.
  maxProgress() {
    return this.vertices.length / (this.MAX_POINTS + 1);
  }

  addPoints(attributes, indices) {
    const incomingLength = attributes.positions.length;
    const verticesLength = this.vertices.length;
    const pointsAvailable = this.MAX_POINTS - verticesLength;

    let end = incomingLength;
    let maxedOut = false;

    /* Left for debugging. 22.10.30.18.30
    if (this.MAX_POINTS === 256) {
      console.log(
        "Incoming:", incomingLength, "Current:", verticesLength,
        "Max:", this.MAX_POINTS
      );
    }
    */

    if (pointsAvailable < incomingLength) {
      end = pointsAvailable;
      maxedOut = true;
      if (debug)
        console.warn(
          "Max. cutoff in GPU form!",
          this,
          incomingLength,
          pointsAvailable,
        );
    }

    // Create new vertices from incoming positions.
    for (let i = 0; i < end; i++) {
      // Generate texCoord from position instead of loading.
      // (Vertex / 2) + 0.5 // Vertex to UV
      // See also: (Vertex - 0.5) * 2 // UV to Vertex
      // TODO: This only works for quads right now.
      const texCoord = [
        attributes.positions[i][X] / 2 + 0.5,
        attributes.positions[i][Y] / 2 + 0.5,
        //0, //positions[i][Z] / 2 + 0.5; // TODO: Is this necessary to calculate for UV?
        //0,
      ];

      // 🔥
      // TODO:
      // Wrap based on MAX_POINTS.

      this.uvs.push(...texCoord); // For sending to the GPU.

      // Optionally put color through a special function here.
      if (attributes.colors?.[i] && typeof this.colorModifier === "function") {
        attributes.colors[i] = this.colorModifier(attributes.colors[i]);
      }

      this.vertices.push(
        // For sending to the CPU.
        new Vertex(
          attributes.positions[i],
          attributes.colors?.[i],
          // this.#gradientColors[i % 3],
          texCoord, //this.#texCoords[i % 3] // Replace to enable bespoke texture coordinates.
          attributes.normals?.[i],
        ),
      );

      // TODO: This may need to be turned back on for the GPU?
      //       What was with the -1 here?  22.11.06.17.42
      // if (!indices) this.indices.push(verticesLength - 1 + i);
      if (!indices) this.indices.push(verticesLength + i);
      // console.log(indices, !indices, i, verticesLength);
    }

    if (indices) this.indices = indices;

    // Create indices from pre-indexed positions or generate
    // a linear set of indices based on length.

    // TODO: How inefficient is this? 22.10.30.17.30
    // this.indices = indices || repeat(this.vertices.length, (i) => i);

    return maxedOut;
  }

  // Get the world position of this form's local vertex.
  transformVertex(vertex) {
    // Build a matrix to represent this form's position, rotation and scale.
    const panned = mat4.fromTranslation(mat4.create(), [
      this.position[X],
      this.position[Y],
      this.position[Z],
    ]);

    const rotX = mat4.fromXRotation(mat4.create(), radians(this.rotation[X]));
    const rotY = mat4.fromYRotation(mat4.create(), radians(this.rotation[Y]));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.rotation[Z]));

    const rotatedX = mat4.mul(mat4.create(), panned, rotX);
    const rotatedY = mat4.mul(mat4.create(), rotatedX, rotY);
    const rotatedZ = mat4.mul(mat4.create(), rotatedY, rotZ);

    const matrix = rotatedZ;

    //mat4.translate(matrix, matrix, this.position);

    // Apply scale.
    mat4.scale(matrix, matrix, this.scale);

    // Apply the world matrix.
    //matrix = mat4.mul(mat4.create(), worldMatrix, matrix);

    // const transformedVertices = [];
    // Transform each vertex by the matrix.
    //this.vertices.forEach((vertex) => {
    return vertex.transformWorld(matrix);
    //});
  }
  graph({ matrix: cameraMatrix }) {
    // Build a matrix to represent this form's position, rotation and scale.
    const panned = mat4.fromTranslation(mat4.create(), [
      this.position[X] * -1,
      this.position[Y],
      this.position[Z] * -1,
    ]);

    const rotX = mat4.fromXRotation(mat4.create(), radians(this.rotation[X]));
    const rotY = mat4.fromYRotation(mat4.create(), radians(this.rotation[Y]));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.rotation[Z]));

    const rotatedX = mat4.multiply(mat4.create(), rotX, panned);
    const rotatedY = mat4.multiply(mat4.create(), rotY, rotatedX);
    const rotatedZ = mat4.multiply(mat4.create(), rotZ, rotatedY);

    // Scale
    const scaled = mat4.scale(mat4.create(), rotatedZ, this.scale); // Render wireframe lines for line type forms using untransformed vertices
    if (this.type === "line" && this.vertices.length > 0) {
      const lineColor = this.color || [255, 0, 0, 255]; // Default to red

      // Use the full combined matrix for line3d
      const fullMatrix = mat4.multiply(mat4.create(), cameraMatrix, scaled);

      // Render lines between pairs of vertices using original vertices
      for (let i = 0; i < this.vertices.length; i += 2) {
        if (i + 1 < this.vertices.length) {
          const a = this.vertices[i];
          const b = this.vertices[i + 1];
          // Create temporary transformed vertices for line3d
          const transformedA = a.transform(fullMatrix);
          const transformedB = b.transform(fullMatrix);

          // Skip drawing if either vertex is behind the camera (negative Z)
          if (transformedA.pos[2] <= 0 || transformedB.pos[2] <= 0) {
            continue;
          }

          // Apply perspective divide and screen space transformation
          const perspA = perspectiveDivide(transformedA);
          const perspB = perspectiveDivide(transformedB);
          const screenA = toScreenSpace(perspA);
          const screenB = toScreenSpace(perspB);

          // Draw simple 2D line
          line(screenA.pos[0], screenA.pos[1], screenB.pos[0], screenB.pos[1]);
        }
      }
    }

    // Still return transformed vertices for compatibility
    const transformedVertices = [];
    const matrix = mat4.multiply(mat4.create(), cameraMatrix, scaled);
    this.vertices.forEach((vertex) => {
      transformedVertices.push(vertex.transform(matrix));
    });

    return transformedVertices;
  }
}

// Constants for accessing Vector / Position components.
const X = 0;
const Y = 1;
const Z = 2;
const W = 3;

// A single point in space with color and texture coordinate information.
class Vertex {
  static X = 0;
  static Y = 1;
  static Z = 2;
  static W = 3;

  pos;
  color;
  texCoords;
  normal;
  constructor(
    position = [0, 0, 0, 1],
    color = [1, 1, 1, 1],
    textureCoordinates = [0, 0],
    normal = null,
  ) {
    this.pos = position;
    this.color = color;
    this.texCoords = textureCoordinates;

    if (normal !== null) {
      this.normal = vec3.fromValues(...normal);
    } else {
      this.normal = vec3.fromValues(0, 0, 1); // Default normal pointing up
    }
  }

  // TODO: Optimize this function for large vertex counts. 22.10.13.00.14
  transform(matrix) {
    // Camera
    const vert = new Vertex(
      vec4.transformMat4(
        vec4.create(),
        [
          this.pos[X] * -1, // FLIPPED
          this.pos[Y],
          this.pos[Z] * -1, // FLIPPED
          this.pos[W],
        ],
        matrix,
      ),
      this.color,
      this.texCoords,
    );
    // console.log(matrix, vert);
    return vert;
  }

  transformWorld(matrix) {
    return new Vertex(
      vec4.transformMat4(vec4.create(), this.pos, matrix),
      this.color,
      this.texCoords,
    );
  }
}

// Sutherland-Hodgman clipping algorithm
function clip(vertices, clippingBoundary) {
  let clipped = [];

  function inside(p, edge) {
    switch (edge) {
      case "left":
        return p[X] >= -1;
      case "right":
        return p[X] <= 1;
      case "bottom":
        return p[Y] >= -1;
      case "top":
        return p[Y] <= 1;
      case "near":
        return p[Z] >= 0;
      case "far":
        return p[Z] <= 1;
    }
  }

  function computeIntersection(p1, p2, edge) {
    let t;
    switch (edge) {
      case "left":
        t = (-1 - p1[X]) / (p2[X] - p1[X]);
        break;
      case "right":
        t = (1 - p1[X]) / (p2[X] - p1[X]);
        break;
      case "bottom":
        t = (-1 - p1[Y]) / (p2[Y] - p1[Y]);
        break;
      case "top":
        t = (1 - p1[Y]) / (p2[Y] - p1[Y]);
        break;
      case "near":
        t = (0 - p1[Z]) / (p2[Z] - p1[Z]);
        break;
      case "far":
        t = (1 - p1[Z]) / (p2[Z] - p1[Z]);
        break;
    }

    return vec4.lerp(vec4.create(), p1, p2, t);
  }

  for (const edge of clippingBoundary) {
    const input = clipped.length > 0 ? clipped : vertices;
    clipped = [];

    if (input.length === 0) break;

    let prevVertex = input[input.length - 1];

    for (let i = 0; i < input.length; i++) {
      const curVertex = input[i];

      if (inside(curVertex.pos, edge)) {
        if (!inside(prevVertex.pos, edge)) {
          const intersection = computeIntersection(
            prevVertex.pos,
            curVertex.pos,
            edge,
          );
          clipped.push(new Vertex(intersection, curVertex.color));
        }
        clipped.push(curVertex);
      } else if (inside(prevVertex.pos, edge)) {
        const intersection = computeIntersection(
          prevVertex.pos,
          curVertex.pos,
          edge,
        );
        clipped.push(new Vertex(intersection, curVertex.color));
      }

      prevVertex = curVertex;
    }
  }

  return clipped;
}

// for 3d line clipping
function clipLineToFrustum(v1, v2) {
  const clippingBoundary = ["left", "right", "bottom", "top", "near", "far"];

  function inside(p, edge) {
    switch (edge) {
      case "left":
        return p[X] >= -p[W];
      case "right":
        return p[X] <= p[W];
      case "bottom":
        return p[Y] >= -p[W];
      case "top":
        return p[Y] <= p[W];
      case "near":
        return p[Z] >= 0;
      case "far":
        return p[Z] <= p[W];
    }
  }

  function computeIntersection(p1, p2, edge) {
    let t;
    switch (edge) {
      case "left":
        t = (-p1[W] - p1[X]) / (p2[X] - p1[X] + p2[W] - p1[W]);
        break;
      case "right":
        t = (p1[W] - p1[X]) / (p2[X] - p1[X] - p2[W] + p1[W]);
        break;
      case "bottom":
        t = (-p1[W] - p1[Y]) / (p2[Y] - p1[Y] + p2[W] - p1[W]);
        break;
      case "top":
        t = (p1[W] - p1[Y]) / (p2[Y] - p1[Y] - p2[W] + p1[W]);
        break;
      case "near":
        t = (0 - p1[Z]) / (p2[Z] - p1[Z]);
        break;
      case "far":
        t = (p1[W] - p1[Z]) / (p2[Z] - p1[Z] - p2[W] + p1[W]);
        break;
    }

    return [
      p1[X] + t * (p2[X] - p1[X]),
      p1[Y] + t * (p2[Y] - p1[Y]),
      p1[Z] + t * (p2[Z] - p1[Z]),
      p1[W] + t * (p2[W] - p1[W]),
    ];
  }

  let clippedVertices = [v1, v2];

  for (const edge of clippingBoundary) {
    if (clippedVertices.length < 2) break;

    const [p1, p2] = clippedVertices;
    const p1Inside = inside(p1.pos, edge);
    const p2Inside = inside(p2.pos, edge);

    if (p1Inside && p2Inside) {
      // Both inside, keep both
      continue;
    } else if (!p1Inside && !p2Inside) {
      // Both outside, discard both
      clippedVertices = [];
      break;
    } else {
      // One inside, one outside
      const intersection = computeIntersection(p1.pos, p2.pos, edge);
      const intersectionVertex = new Vertex(intersection, p1.color);

      if (p1Inside) {
        // p1 inside, p2 outside
        clippedVertices = [p1, intersectionVertex];
      } else {
        // p1 outside, p2 inside
        clippedVertices = [intersectionVertex, p2];
      }
    }
  }

  return clippedVertices;
}

function perspectiveDivide(vertex) {
  const vert = new Vertex([
    vertex.pos[X] / vertex.pos[W],
    vertex.pos[Y] / vertex.pos[W],
    vertex.pos[Z] / vertex.pos[W],
    vertex.pos[W],
  ]);

  vert.color = vertex.color;
  vert.texCoords = vertex.texCoords;

  return vert;
}

function toScreenSpace(vertex) {
  // Flip Y.
  const x = vertex.pos[X];
  const y = -vertex.pos[Y];
  const z = vertex.pos[Z];

  const sX = ((x + 1.0) / 2.0) * width;
  const sY = ((y + 1.0) / 2.0) * height;

  const vert = new Vertex([sX, sY, z, vertex.pos[W]]);
  vert.color = vertex.color;
  vert.texCoords = vertex.texCoords;
  return vert;
}

function zeroLineClip(vertices) {
  const clipped = [];

  let prevVertex = vertices[vertices.length - 1]; // Start with the last vertex
  let prevComponent = prevVertex.pos[Z];
  let prevInside = prevComponent >= 0;

  for (const curVertex of vertices) {
    const curComponent = curVertex.pos[Z];
    const curInside = curComponent >= 0;

    if (curInside !== prevInside) {
      // Edge crosses the z=0 plane, find intersection
      const t = prevComponent / (prevComponent - curComponent);
      const intersectionPos = [
        prevVertex.pos[X] + t * (curVertex.pos[X] - prevVertex.pos[X]),
        prevVertex.pos[Y] + t * (curVertex.pos[Y] - prevVertex.pos[Y]),
        0, // z = 0
        prevVertex.pos[W] + t * (curVertex.pos[W] - prevVertex.pos[W]),
      ];

      // Interpolate color as well
      const intersectionColor = [
        prevVertex.color[0] + t * (curVertex.color[0] - prevVertex.color[0]),
        prevVertex.color[1] + t * (curVertex.color[1] - prevVertex.color[1]),
        prevVertex.color[2] + t * (curVertex.color[2] - prevVertex.color[2]),
        prevVertex.color[3] + t * (curVertex.color[3] - prevVertex.color[3]),
      ];

      clipped.push(new Vertex(intersectionPos, intersectionColor));
    }

    if (curInside) {
      clipped.push(curVertex);
    }

    prevVertex = curVertex;
    prevComponent = curComponent;
    prevInside = curInside;
  }
}

export {
  clear,
  point,
  plot,
  flood,
  colorsMatch,
  pan,
  unpan,
  savepan,
  loadpan,
  mask,
  unmask,
  skip,
  copy,
  resize,
  blur,
  paste,
  stamp,
  steal,
  putback,
  line,
  pline,
  pixelPerfectPolyline,
  lineAngle,
  circle,
  oval,
  poly,
  box,
  shape,
  grid,
  draw,
  noise16,
  noise16DIGITPAIN,
  noise16Aesthetic,
  noise16Sotce,
  noiseTinted,
  printLine,
  blendMode,
  scroll,
  spin,
  zoom,
  sort,
  shear,
  Camera,
  Form,
  Dolly,
};
