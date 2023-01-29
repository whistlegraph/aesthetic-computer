import {
  randInt,
  byteInterval17,
  vec2,
  vec3,
  vec4,
  mat4,
  even,
  radians,
  lerp,
  map,
  randIntRange,
  clamp,
} from "./num.mjs";

import { repeat } from "./help.mjs";
import { nanoid } from "../dep/nanoid/nanoid.js";

const { abs, sign, ceil, floor, sin, cos, tan, min, max } = Math;

let width, height, pixels;
const depthBuffer = [];
const writeBuffer = [];
const c = [255, 255, 255, 255];
const panTranslation = { x: 0, y: 0 }; // For 2d shifting using `pan` and `unpan`.
const skips = [];

let debug = false;
export function setDebug(newDebug) {
  debug = newDebug;
}

// 1. Configuration & State
function makeBuffer(width, height, fillProcess, painting) {
  const imageData = new ImageData(width, height);

  const buffer = {
    pixels: imageData.data,
    width: imageData.width,
    height: imageData.height,
  };

  if (typeof fillProcess === "function") {
    // Remember the current buffer and color.
    const savedBuffer = getBuffer();
    const rc = c; // Remember color.
    setBuffer(buffer);
    const api = { width, height, pixels };
    Object.assign(api, painting.api);
    fillProcess(api); // Every fill process gets a painting API.
    painting.paint(true);
    // Restore old buffer and color.
    setBuffer(savedBuffer);
    color(...rc);
  }

  return buffer;
}

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

// Return a pixel from the main buffer.
function pixel(x, y) {
  const i = (floor(x) + floor(y) * width) * 4;
  return [pixels[i], pixels[i + 1], pixels[i + 2], pixels[i + 3]];
}

// Set global color.
// Send 0 arguements to retrieve the current one.
function color(r, g, b, a = 255) {
  if (arguments.length === 0) return c.slice();

  c[0] = floor(r);
  c[1] = floor(g);
  c[2] = floor(b);
  c[3] = floor(a);
}

export {
  makeBuffer,
  cloneBuffer,
  setBuffer,
  changePixels,
  depthBuffer,
  writeBuffer,
  color,
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
  // pixels.copyWithin(4, 0);

  for (let i = 0; i < pixels.length; i += 4) {
    pixels[i] = c[0]; // r
    pixels[i + 1] = c[1]; // g
    pixels[i + 2] = c[2]; // b
    pixels[i + 3] = c[3]; // alpha
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
  //if (x < 0 || x >= width || y < 0 || y >= height) return;
  if (x < 0) return;
  if (x >= width) return;
  if (y < 0) return;
  if (y >= height) return;

  for (const s of skips) if (x === s.x && y === s.y) return;

  // Plot our pixel.
  const i = (x + y * width) * 4;
  const alpha = c[3];

  if (alpha === 255) {
    // No alpha blending, just copy.
    pixels.set(c, i);
  } else if (alpha !== 0) {
    blend(c, pixels, 0, i);
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
        x: floor(p[0]) + panTranslation.x,
        y: floor(p[1]) + panTranslation.y,
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

    shader.position(p); // Compute position.

    // Clip again
    if (p.x < 0) return;
    if (p.x >= width) return;
    if (p.y < 0) return;
    if (p.y >= height) return;

    p.x = floor(p.x); // Floor position and check bounds.
    p.y = floor(p.y);

    const n = p.y * width + p.x;

    if (writeBuffer[n] === 0) {
      writeBuffer[n] = 1; // Remember this point in the frame's writeBuffer.
      // 🪄 Pixel Shader
      const i = floor((p.x + p.y * width) * 4); // Get current pixel under p.
      const pixel = pixels.subarray(i, i + 4);
      shader.color({ x: p.x, y: p.y }, pixel, c, p.color); // Modify color.
    }
  }); // Paint each filtered pixel.
}

// TODO: Implement panTranslation for primitives other than line?
function pan(x, y) {
  if (y === undefined) y = x;
  panTranslation.x += floor(x);
  panTranslation.y += floor(y);
}

function unpan() {
  panTranslation.x = 0;
  panTranslation.y = 0;
}

function copy(destX, destY, srcX, srcY, src, alpha = 1.0) {
  destX = Math.floor(destX);
  destY = Math.floor(destY);
  srcX = Math.floor(srcX);
  srcY = Math.floor(srcY);

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

  blend(src.pixels, pixels, si, di, alpha);
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

// Copies pixels from a source buffer to the active buffer and returns
// the source buffer.
// TODO: Add dirty rectangle support here...
//       - What would the best parameter set be?
// `from` - can either be

// TODO: Some of these routes are incompatible. 22.10.01.11.57
// TODO: Replace with more generic algorithm?
function paste(from, destX = 0, destY = 0, scale = 1, blit = false) {
  if (!from) return;

  if (scale !== 1) {
    grid(
      {
        box: {
          x: destX,
          y: destY,
          w: from.width,
          h: from.height,
        },
        scale,
      },
      from
    );
    return;
  }

  // TODO: See if from has a dirtyBox attribute.
  if (from.crop) {
    // A cropped copy.
    // TODO: This could be sped up quite a bit by going row by row.
    for (let x = 0; x < from.crop.w; x += 1) {
      for (let y = 0; y < from.crop.h; y += 1) {
        copy(
          destX + x,
          destY + y,
          from.crop.x + x,
          from.crop.y + y,
          from.painting
        );
      }
    }
  } else {
    // Check to see if we can perform a full copy here,
    // with no alpha blending.
    if (blit) {
      pixels.set(from.pixels, 0);
    } else {
      // Or go pixel by pixel, with blending.
      for (let x = 0; x < from.width; x += 1) {
        for (let y = 0; y < from.height; y += 1) {
          copy(destX + x, destY + y, x, y, from);
        }
      }
    }
  }
}

//let stipple = 0;

// A fast alpha blending function.
// Transcribed from C++: https://stackoverflow.com/a/12016968
function blend(src, dst, si, di, alphaIn = 1) {
  //stipple += 1;
  //if (stipple < 4) { return; }
  //stipple = 0;
  if (src[si + 3] === 0) return;
  const alpha = src[si + 3] * alphaIn + 1;
  const invAlpha = 256 - alpha;
  dst[di] = (alpha * src[si + 0] + invAlpha * dst[di + 0]) >> 8;
  dst[di + 1] = (alpha * src[si + 1] + invAlpha * dst[di + 1]) >> 8;
  dst[di + 2] = (alpha * src[si + 2] + invAlpha * dst[di + 2]) >> 8;
  dst[di + 3] = dst[di + 3] + alpha;
}

// Draws a horizontal line. (Should be very fast...)
function lineh(x0, x1, y) {
  x0 = floor(x0);
  x1 = floor(x1);
  y = floor(y);

  if (y < 0 || y >= height) return;

  x0 = clamp(x0, 0, width);
  x1 = clamp(x1, 0, width);

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

  // Only use alpha blending if necessary.
  if (c[3] === 255) {
    for (let i = startIndex; i <= endIndex; i += 4) {
      pixels[i] = c[0];
      pixels[i + 1] = c[1];
      pixels[i + 2] = c[2];
      pixels[i + 3] = 255;
    }
  } else if (c[3] !== 0) {
    for (let i = startIndex; i <= endIndex; i += 4) {
      blend(c, pixels, 0, i);
    }
  }
}

// Draws a line
// (2) p1, p2: pairs of {x, y} or [x, y]
// (4) x0, y0, x1, y1
// TODO: Automatically use lineh if possible. 22.10.05.18.27
function line() {
  let x0, y0, x1, y1;

  if (arguments.length === 4) {
    x0 = arguments[0]; // Set all `undefined` or `null` values to 0.
    y0 = arguments[1];
    x1 = arguments[2];
    y1 = arguments[3];
  } else if (arguments.length === 2) {
    if (Array.isArray(arguments[0])) {
      // assume [x, y], [x, y]
      x0 = arguments[0][0];
      y0 = arguments[0][1];
      x1 = arguments[1][0];
      y1 = arguments[1][1];
    } else {
      // assume {x, y}, {x, y}
      x0 = arguments[0].x;
      y0 = arguments[0].y;
      x1 = arguments[1].x;
      y1 = arguments[1].y;
    }
  } else {
    console.warn(
      "Line did not use the correct number of arguments:",
      arguments
    );
  }

  // Set all untruthy values like null, or undefined to a random value.
  if (x0 == null) x0 = randIntRange(0, width);
  if (y0 == null) y0 = randIntRange(0, height);
  if (x1 == null) x1 = randIntRange(0, width);
  if (y1 == null) y1 = randIntRange(0, height);

  if (isNaN(x0) || isNaN(y0) || isNaN(x1) || isNaN(y1)) {
    return console.error("Invalid line arguments:", x0, y0, x1, y1);
  }

  // Add any panTranslations.
  x0 += panTranslation.x;
  y0 += panTranslation.y;
  x1 += panTranslation.x;
  y1 += panTranslation.y;

  // Check if line is perfectly horizontal, otherwise run bresenham.
  if (y0 === y1) {
    lineh(x0, x1, y0);
  } else {
    bresenham(x0, y0, x1, y1).forEach((p) => plot(p.x, p.y));
  }
}

// Takes an array of pixel coords `{x, y}` and filters out L shapes.
// Note: It checks the previous, current, and next pixel and requires a minimum
//        set of 3 before it removes anything.
// Draws a regular `line` if only two pixels are provided.
// Transcribed from: https://rickyhan.com/jekyll/update/2018/11/22/pixel-art-algorithm-pixel-perfect.html
function pixelPerfectPolyline(points, shader) {
  if (points.length < 2) return; // Require 2 or more points.

  const pixels = [];
  let last = points[0];

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

    // Compute bresen pixels, filtering out duplicates.
    bresenham(last.x, last.y, cur.x, cur.y).forEach((p, i) => {
      if (i > 0 || pixels.length < 2) {
        pixels.push({ ...p, color: cur.color }); // Add color for each pixel.
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
  const x2 = x1 + dist * Math.cos(radians(degrees));
  const y2 = y1 + dist * Math.sin(radians(degrees));
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

function circle(x0, y0, radius, filled = false) {
  x0 = floor(x0);
  y0 = floor(y0);
  radius = floor(radius);

  let f = 1 - radius,
    ddF_x = 0,
    ddF_y = -2 * radius,
    x = 0,
    y = radius;

  plot(x0, y0 + radius);
  plot(x0, y0 - radius);
  plot(x0 + radius, y0);
  plot(x0 - radius, y0);

  while (x < y) {
    if (f >= 0) {
      y -= 1;
      ddF_y += 2;
      f += ddF_y;
    }
    x += 1;
    ddF_x += 2;
    f += ddF_x + 1;
    plot(x0 + x, y0 + y);
    plot(x0 - x, y0 + y);
    plot(x0 + x, y0 - y);
    plot(x0 - x, y0 - y);
    plot(x0 + y, y0 + x);
    plot(x0 - y, y0 + x);
    plot(x0 + y, y0 - x);
    plot(x0 - y, y0 - x);
  }
  if (filled) {
    console.log("Draw a filled circle!");
    // Flood fill algorithm
    //floodFill(x0, y0);
  }
}

/*
function floodFill(x, y) {
  if (getPixelColor(x, y) !== "black") return;
  setPixelColor(x, y, "white");
  floodFill(x + 1, y);
  floodFill(x - 1, y);
  floodFill(x, y + 1);
  floodFill(x, y - 1);
}
*/

// Draws a 1px aliased circle: http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#C
// function circle(
//   x0 = randIntRange(0, width),
//   y0 = randIntRange(0, height),
//   radius
// ) {
//   x0 = floor(x0);
//   y0 = floor(y0);
//   radius = floor(radius);

//   let f = 1 - radius,
//     ddF_x = 0,
//     ddF_y = -2 * radius,
//     x = 0,
//     y = radius;

//   plot(x0, y0 + radius);
//   plot(x0, y0 - radius);
//   plot(x0 + radius, y0);
//   plot(x0 - radius, y0);

//   while (x < y) {
//     if (f >= 0) {
//       y -= 1;
//       ddF_y += 2;
//       f += ddF_y;
//     }
//     x += 1;
//     ddF_x += 2;
//     f += ddF_x + 1;
//     plot(x0 + x, y0 + y);
//     plot(x0 - x, y0 + y);
//     plot(x0 + x, y0 - y);
//     plot(x0 - x, y0 - y);
//     plot(x0 + y, y0 + x);
//     plot(x0 - y, y0 + x);
//     plot(x0 + y, y0 - x);
//     plot(x0 - y, y0 - x);
//   }
// }

// Draws a series of 1px lines without overlapping / overdrawing points.
function poly(coords) {
  let last = coords[0];
  coords.forEach((cur, i) => {
    if (i < coords.length - 1) skip(cur);
    line(last, cur);
    skip(null);
    last = cur;
  });
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
    const lp = [last.x, last.y],
      cp = [cur.x, cur.y]; // Convert last and cur to vec2.

    const dir = vec2.normalize([], vec2.subtract([], cp, lp)); // Line direction.
    if (!ldir) ldir = dir;

    const rot = vec2.rotate([], dir, [0, 0], Math.PI / 2); // Rotated by 90d

    const offset1 = vec2.scale([], rot, thickness / 2); // Parallel offsets.
    const offset2 = vec2.scale([], rot, -thickness / 2);

    let c1 = vec2.add([], cp, offset1);
    let c2 = vec2.add([], cp, offset2);

    let l1, l2;
    if (!lpar) {
      l1 = vec2.add([], lp, offset1); // Compute both sets of points.
      l2 = vec2.add([], lp, offset2);
      lpar = [l1, l2];
    } else {
      [l1, l2] = lpar;
    }

    [l1, l2, c1, c2].forEach((v) => vec2.floor(v, v)); // Floor everything.

    last = cur; // Update the last point.
    lpar = [c1, c2]; // ... and last parallel points.

    // 2. Plotting

    const dot = vec2.dot(dir, ldir); // Get the dot product of cur and last dir.

    let trig; // Triangle geometry.

    if (dot > 0) {
      // Vertex order for forward direction.
      trig = [
        [l1, l2, c1],
        [l2, c1, c2],
      ];
      lines.push(...bresenham(...l1, ...c1)); // Par line 1
      lines.push(...bresenham(...l2, ...c2)); // Par line 2
    } else {
      // Vertex order for backward direction.
      trig = [
        [l2, c1, l1],
        [l1, c2, c1],
      ];
      lines.push(...bresenham(...l2, ...c1)); // Par line 1
      lines.push(...bresenham(...l1, ...c2)); // Par line 2
    }

    // Partial outside clipping.
    // const clippedTris = trig.filter((triangle) =>
    //   triangle.every(
    //     (v) => v[0] >= 0 && v[0] < width && v[1] >= 0 && v[1] < height
    //   )
    // );

    // Full outside clipping.
    // Clip triangles that are *fully* offscreen.
    const clippedTris = trig.filter((triangle) =>
      triangle.some(
        (v) => v[0] >= 0 && v[0] < width && v[1] >= 0 && v[1] < height
      )
    );

    clippedTris.forEach((tri) => fillTri(tri, tris)); // Fill quad.
    //trig.forEach((t) => fillTri(t, tris)); // Fill quad.

    ldir = dir;
    lines.push(...bresenham(...lp, ...cp));

    if (i === coords.length - 2)
      points.push({ x: l1[0], y: l1[1] }, { x: l2[0], y: l2[1] });

    points.push({ x: c1[0], y: c1[1] }, { x: c2[0], y: c2[1] }); // Add points.

    // Paint each triangle.
    if (cur.color) color(...cur.color);

    if (shader) {
      shadePixels(tris, shader, [cur.color]);
    } else {
      tris.forEach((p) => point(p));
    }

    tris.length = 0;
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
  const dy = abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx - dy;

  while (true) {
    points.push({ x: x0, y: y0 });

    if (x0 === x1 && y0 === y1) break;
    const e2 = 2 * err;
    if (e2 > -dy) {
      err -= dy;
      x0 += sx;
    }
    if (e2 < dx) {
      err += dx;
      y0 += sy;
    }
  }

  return points;
}

// Takes in x, y, width and height and draws an
// outline, inline (1px) or filled rectangle, optionally
// from the center by inputting eg: "inline*center" in mode.
const BOX_CENTER = "*center";
// Parameters
// (1) box (any object with {x, y, w, h} properties) (1)
// (2) box, mode (2)
// (3) x, y, size (3)
// (4) x, y, w, h (4)
// (4) x, y, size, mode:string (4)
// (5) x, y, w, h, mode (5)
function box() {
  let x,
    y,
    w,
    h,
    mode = "fill";

  if (arguments.length === 1) {
    // Array(4)
    if (Array.isArray(arguments[0])) {
      x = arguments[0][0];
      y = arguments[0][1];
      w = arguments[0][2];
      h = arguments[0][3];
    } else {
      // Object {x, y, w, h}
      // Note: Also works with anything that has width and height properties.
      x = arguments[0].x || 0;
      y = arguments[0].y || 0;
      w = arguments[0].w || arguments[0].width;
      h =
        arguments[0].h ||
        arguments[0].height ||
        arguments[0].w ||
        arguments[0].width;
      if (x === undefined || y === undefined || w === undefined) {
        return console.error(
          "Could not make a box {x,y,w,h} from:",
          arguments[0]
        );
      }
    }
  } else if (arguments.length === 2) {
    // box, mode
    x = arguments[0].x;
    y = arguments[0].y;
    w = arguments[0].w;
    h = arguments[0].h;
    mode = arguments[1];
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

  // Random parameters if undefined.
  if (x === undefined) x = randInt(width);
  if (y === undefined) y = randInt(height);
  if (w === undefined) w = randInt(width);
  if (h === undefined) h = randInt(height);
  if (mode === undefined) mode = "fill"; // TODO: Add chooser here.

  // Check for "Center" at the end of mode.
  if (mode.endsWith(BOX_CENTER)) {
    x -= w / 2;
    y -= h / 2;

    mode = mode.slice(0, -BOX_CENTER.length); // Remove it.
  }

  if (mode === "outline" || mode === "out") {
    line(x - 1, y - 1, x + w, y - 1); // Top
    line(x - 1, y + h, x + w, y + h); // Bottom
    line(x - 1, y, x - 1, y + h - 1); // Left
    line(x + w, y, x + w, y + h - 1); // Right
  } else if (mode === "inline" || mode === "in") {
    line(x, y, x + w - 1, y); // Top
    line(x, y + h - 1, x + w - 1, y + h - 1); // Bottom
    line(x, y + 1, x, y + h - 2); // Left
    line(x + w - 1, y + 1, x + w - 1, y + h - 2); // Right
  } else if (mode === "fill") {
    w -= 1;
    if (sign(height) === 1) {
      for (let row = 0; row < h; row += 1) line(x, y + row, x + w, y + row);
    } else {
      for (let row = 0; row > h; row -= 1) line(x, y + row, x + w, y + row);
    }
  }
}

// TODO: The most efficient algorithm I could find for filling:
//       https://gist.github.com/ideasman42/983738130f754ef58ffa66bcdbbab892
function shape() {
  if (arguments % 2 !== 0) {
    // Split arguments into points.
    let points = [];

    for (let p = 0; p < arguments.length; p += 2) {
      points.push([arguments[p], arguments[p + 1]]);
    }

    // Make lines from 1->2->3->...->1
    // Draw white points for each.
    points.forEach((p, i) => {
      color(0, 255, 0, 100);
      const lastPoint = i < points.length - 1 ? points[i + 1] : points[0];
      line(...p, ...lastPoint);
      color(255, 255, 255);
      point(...p);
    });
  } else {
    console.error("Shape requires an even number of arguments: x,y,x,y...");
  }
}

// Renders a square grid at x, y given cols, rows, and scale.
// Buffer is optional, and if present will render the pixels at scale starting
// from the top left corner of the buffer, repeating if needed to fill the grid.
function grid(
  { box: { x, y, w: cols, h: rows }, scale, centers = [] },
  buffer
) {
  const oc = c.slice(); // Remember the original color.

  const w = cols * scale;
  const h = rows * scale;

  const colPix = w / cols,
    rowPix = h / rows;

  // Draw a scaled image if the buffer is present.
  // Technically, this allows us to scale any bitmap. 22.08.21.21.13
  if (buffer) {
    for (let j = 0; j < rows; j += 1) {
      const plotY = y + rowPix * j;
      for (let i = 0; i < cols; i += 1) {
        const plotX = x + colPix * i;

        // Repeat (tile) the source over X and Y if we run out of pixels.
        const repeatX = i % buffer.width;
        const repeatY = j % buffer.height;
        const repeatCols = buffer.width;

        // Loop over the buffer and find the proper color.
        const pixIndex = (repeatX + repeatCols * repeatY) * 4;

        if (pixIndex < buffer.pixels.length) {
          color(...buffer.pixels.subarray(pixIndex, pixIndex + 4));
          box(plotX, plotY, scale);
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
        box(plotX, plotY, scale);

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

// Loading & rendering stored drawings. TODO: Store this on another layer of
//                                            abstraction? 2021.12.13.22.04
// Silently fails if `drawing` is left `undefined`.
function draw(drawing, x, y, scale = 1, angle = 0) {
  if (drawing === undefined) return;

  // TODO: Eventually make this the call: rotatePoint(args[0], args[1], 0, 0);
  angle = radians(angle);
  const s = sin(angle);
  const c = cos(angle);

  pan(x, y);
  drawing.commands.forEach(({ name, args }) => {
    args = args.map((a) => a * scale); // TODO: Add scale in addition to pan.

    if (name === "line") {
      let x1 = args[0]; // x1
      let y1 = args[1]; // y1

      let x2 = args[2]; // x2
      let y2 = args[3]; // y2

      let nx1 = x1 * c - y1 * s;
      let ny1 = x1 * s + y1 * c;

      let nx2 = x2 * c - y2 * s;
      let ny2 = x2 * s + y2 * c;

      line(nx1, ny1, nx2, ny2);
    } else if (name === "point") point(...args);
  });
  unpan();
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
  xOffset = 0
) {
  if (!text) return;
  [...text.toString()].forEach((char, i) => {
    draw(font[char], startX + blockWidth * scale * i + xOffset, startY, scale);
  });
}

function noise16() {
  for (let i = 0; i < pixels.length; i += 4) {
    pixels[i] = byteInterval17(randInt(16)); // r
    pixels[i + 1] = byteInterval17(randInt(16)); // g
    pixels[i + 2] = byteInterval17(randInt(16)); // b
    pixels[i + 3] = 255; // a
  }
}

function noise16DIGITPAIN() {
  for (let i = 0; i < pixels.length; i += 4) {
    pixels[i] = byteInterval17(randInt(16)) * 0.6; // r
    pixels[i + 1] = byteInterval17(randInt(16)) * 0.15; // g
    pixels[i + 2] = byteInterval17(randInt(16)) * 0.55; // b
    pixels[i + 3] = 255; // a
  }
}

function noiseTinted(tint, amount, saturation) {
  // console.log("Tinting:", tint, amount, saturation);
  for (let i = 0; i < pixels.length; i += 4) {
    const grayscale = randInt(255);
    pixels[i] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[0],
      amount
    ); // r
    pixels[i + 1] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[1],
      amount
    ); // g
    pixels[i + 2] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[2],
      amount
    ); // b
    pixels[i + 3] = 255; // a
  }
}

export {
  clear,
  point,
  plot,
  pan,
  unpan,
  skip,
  copy,
  paste,
  line,
  pline,
  pixelPerfectPolyline,
  lineAngle,
  circle,
  poly,
  bresenham, // This function is under "abstract" because it doesn't render.
  box,
  shape,
  grid,
  draw,
  noise16,
  noise16DIGITPAIN,
  noiseTinted,
  printLine,
};

// 3. 3D Drawing (Kinda mixed with some 2D)

// a. Globals

const X = 0;
const Y = 1;
const Z = 2;
const W = 3;
let screenMatrix;

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

  centerCached; // Saved after each call to `center()`.

  perspectiveMatrix;
  #transformMatrix;

  // Takes x, y, z position and an optional scale (xyz) array.
  constructor(fov = 80, { x, y, z, scale }) {
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
      zFar
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
    const rotatedZ = mat4.multiply(mat4.create(), rotZ, rotatedY);

    const scaled = mat4.scale(mat4.create(), rotatedZ, this.scale);

    const world = scaled;

    // Camera World Space -> Inverted Perspective Projection
    const invertedProjection = mat4.invert(
      mat4.create(),
      this.perspectiveMatrix
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
      invWorldPersProj
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
      scaled
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
      radians(-this.camera.rotY) // Take the camera Y axis for strafing.
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
    transform
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

    // TODO: There is no maxed out notice here.
    if (positions?.length > 0)
      this.addPoints({ positions, colors }, this.indices);

    // Or just set vertices directly.
    if (vertices?.length > 0) {
      this.vertices = vertices;
      this.uvs = uvs;
    }

    // Switch fill to transform if the was skipped.
    if (fill?.pos || fill?.rot || fill?.scale) {
      transform = fill;
      fill = undefined;
    }

    // Assign texture or color.
    if (fill?.tex) this.texture = fill.tex;
    if (fill?.color) this.color = fill.color;
    if (fill?.alpha) this.alpha = fill.alpha;

    this.position = transform?.pos || [0, 0, 0];
    this.rotation = transform?.rot || [0, 0, 0];
    this.scale = transform?.scale || [1, 1, 1];
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
          pointsAvailable
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
        //0, //positions[i][Z] / 2 + 0.5, // TODO: Is this necessary to calculate for UV?
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
          attributes.normals?.[i]
        )
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

    const rotY = mat4.fromYRotation(mat4.create(), radians(this.rotation[Y]));

    const rotX = mat4.fromXRotation(
      mat4.create(),
      radians(this.rotation[X] * -1) // FLIPPED
    );

    const rotZ = mat4.fromZRotation(
      mat4.create(),
      radians(this.rotation[Z] * -1) // FLIPPED
    );

    const rotatedX = mat4.mul(mat4.create(), panned, rotX);
    const rotatedY = mat4.mul(mat4.create(), rotatedX, rotY);
    const rotatedZ = mat4.mul(mat4.create(), rotatedY, rotZ);

    let matrix = rotatedZ;

    // Apply scale.
    mat4.scale(matrix, matrix, this.scale);

    // Apply the camera matrix.
    matrix = mat4.mul(mat4.create(), cameraMatrix, matrix);

    const transformedVertices = [];

    // Transform each vertex by the matrix.
    this.vertices.forEach((vertex) => {
      transformedVertices.push(vertex.transform(matrix));
    });

    screenMatrix = initScreenSpaceTransformMatrix(width / 2, height / 2);

    // *** Choose a render primitive. ***

    // TODO: Add indexed drawing. Right now only length is used.
    //       22.10.11.20.21

    if (this.primitive === "triangle") {
      //if (this.indices.length % 3 !== 0) return; // Since it's triangles, make sure we always draw have a multiple of 3 indices.

      let posLimitMax = this.indices.length / 3;
      let posLimit = this.limiter % (posLimitMax + 1);

      // Loop indices list to draw each triangle.
      for (let i = 0; i < this.indices.length - posLimit * 3; i += 3) {
        if (i + 1 > this.indices.length || i + 3 > this.indices.length) return;

        // Draw each triangle by applying the screen transform &
        // perspective divide (with clipping).
        drawTriangle(
          transformedVertices[this.indices[i]],
          transformedVertices[this.indices[i + 1]],
          transformedVertices[this.indices[i + 2]],
          // Eventually pass in a "shader" function instead of texture or alpha..
          this.texture,
          this.alpha
        );
      }
    }

    if (this.primitive === "line") {
      // Limit positions and colors in order to know their drawing order...
      let posLimitMax = this.indices.length / 2;
      let posLimit = this.limiter % (posLimitMax + 1);
      // let posLimit = posLimitMax - ((limiter % posLimitMax) + 1); // Reverse the order.

      // Loop indices list to draw each triangle.
      for (let i = 0; i < this.indices.length - posLimit * 2; i += 2) {
        // Draw each line by applying the screen transform &
        // perspective divide (with clipping).
        drawLine3d(
          transformedVertices[this.indices[i]],
          transformedVertices[this.indices[i + 1]],
          transformedVertices[this.indices[i]].color || this.color,
          this.gradients
        );
      }
    }
  }

  angle(x, y, z) {
    this.rotation[X] = x;
    this.rotation[Y] = y;
    this.rotation[Z] = z;

    this.gpuTransformed = true;
  }

  turn({ x, y, z }) {
    this.rotation[X] = (this.rotation[X] + (x || 0)) % 360;
    this.rotation[Y] = (this.rotation[Y] + (y || 0)) % 360;
    this.rotation[Z] = (this.rotation[Z] + (z || 0)) % 360;

    this.gpuTransformed = true;
  }
}

/*
class Model {
  positions;
  texCoords;

  constructor(positions, texCoords) {
    this.positions = positions;
    this.texCoords = texCoords;
  }
}
*/

class Vertex {
  pos; // vec4
  color; // vec4
  texCoords; // vec4
  normal; // vec3 (gpu only for now)

  get x() {
    return this.pos[X];
  }

  get y() {
    return this.pos[Y];
  }

  get z() {
    return this.pos[Z];
  }

  get w() {
    return this.pos[W];
  }

  get color24bit() {
    // 0-255
    return this.color.map((c) => floor(c * 255));
  }

  constructor(
    pos = [0, 0, 0, 1],
    color = [...c, 1.0],
    texCoords = [0, 0, 0, 0],
    normal = [0, 0, 0]
  ) {
    this.pos = vec4.fromValues(...pos);
    this.color = vec4.fromValues(...color);
    this.texCoords = vec4.fromValues(...texCoords);
    this.normal = vec3.fromValues(...normal);
  }

  // TODO: Optimize this function for large vertex counts. 22.10.13.00.14
  transform(matrix) {
    // Camera
    return new Vertex(
      vec4.transformMat4(
        vec4.create(),
        [
          this.pos[X] * -1, // FLIPPED
          this.pos[Y],
          this.pos[Z] * -1, // FLIPPED
          this.pos[W],
        ],
        matrix
      ),
      this.color,
      this.texCoords
    );
  }

  transformWorld(matrix) {
    return new Vertex(
      vec4.transformMat4(
        vec4.create(),
        [
          this.pos[X], // FLIPPED
          this.pos[Y],
          this.pos[Z], // FLIPPED
          this.pos[W],
        ],
        matrix
      ),
      this.color,
      this.texCoords
    );
  }

  perspectiveDivide() {
    return new Vertex(
      vec4.fromValues(
        this.pos[X] / this.pos[W],
        this.pos[Y] / this.pos[W],
        this.pos[Z] / this.pos[W],
        this.pos[W]
      ),
      this.color,
      this.texCoords
    );
  }

  lerp(other, lerpAmt) {
    const pos = vec4.lerp(vec4.create(), this.pos, other.pos, lerpAmt);
    const col = vec4.lerp(vec4.create(), this.color, other.color, lerpAmt);
    const texCoords = vec4.lerp(
      vec4.create(),
      this.texCoords,
      other.texCoords,
      lerpAmt
    );
    return new Vertex(pos, col, texCoords);
  }
}

function initScreenSpaceTransformMatrix(halfWidth, halfHeight) {
  const m = mat4.create();
  mat4.translate(m, m, [halfWidth - 0.5, halfHeight - 0.5, 0]);
  mat4.scale(m, m, [halfWidth, -halfHeight, 1]);
  return m;
}

function isInsideViewFrustum(v4) {
  return (
    abs(v4[X]) <= abs(v4[W]) &&
    abs(v4[Y]) <= abs(v4[W]) &&
    abs(v4[Z]) <= abs(v4[W])
  );
}

// c. Rendering Procedures

class Edge {
  #x;
  #yStart;
  #yEnd;

  color;
  #colorStep;

  texCoordX;
  #texCoordXStep;
  texCoordY;
  #texCoordYStep;

  oneOverZ;
  #oneOverZStep;

  depth;
  #depthStep;

  get x() {
    return this.#x;
  }

  get yStart() {
    return this.#yStart;
  }

  get yEnd() {
    return this.#yEnd;
  }

  #xStep;

  constructor(gradients, minYVert, maxYVert, minYVertIndex) {
    this.#yStart = ceil(minYVert.y);
    this.#yEnd = ceil(maxYVert.y);

    const yDist = maxYVert.y - minYVert.y;
    const xDist = maxYVert.x - minYVert.x;

    const yPrestep = this.#yStart - minYVert.y;

    this.#xStep = xDist / yDist;

    this.#x = minYVert.x + yPrestep * this.#xStep;

    const xPrestep = this.#x - minYVert.x;

    // Texture

    this.texCoordX =
      gradients.texCoordX[minYVertIndex] +
      gradients.texCoordXXStep * xPrestep +
      gradients.texCoordXYStep * yPrestep;

    this.#texCoordXStep =
      gradients.texCoordXYStep + gradients.texCoordXXStep * this.#xStep;

    this.texCoordY =
      gradients.texCoordY[minYVertIndex] +
      gradients.texCoordYXStep * xPrestep +
      gradients.texCoordYYStep * yPrestep;

    this.#texCoordYStep =
      gradients.texCoordYYStep + gradients.texCoordYXStep * this.#xStep;

    this.oneOverZ =
      gradients.oneOverZ[minYVertIndex] +
      gradients.oneOverZXStep * xPrestep +
      gradients.oneOverZYStep * yPrestep;

    this.#oneOverZStep =
      gradients.oneOverZYStep + gradients.oneOverZXStep * this.#xStep;

    this.depth =
      gradients.depth[minYVertIndex] +
      gradients.depthXStep * xPrestep +
      gradients.depthYStep * yPrestep;

    this.#depthStep = gradients.depthYStep + gradients.depthXStep * this.#xStep;

    // Color
    {
      const vec = gradients.color[minYVertIndex].slice();
      vec4.add(
        vec,
        vec,
        vec4.scale(vec4.create(), gradients.colorYStep, yPrestep)
      );
      vec4.add(
        vec,
        vec,
        vec4.scale(vec4.create(), gradients.colorXStep, xPrestep)
      );
      this.color = vec;
    }

    {
      const vec = gradients.colorYStep.slice();
      const scaled = vec4.scale(
        vec4.create(),
        gradients.colorXStep,
        this.#xStep
      );
      vec4.add(vec, vec, scaled);
      this.#colorStep = vec;
    }
  }

  step() {
    this.#x += this.#xStep; // add xStep

    vec4.add(this.color, this.color, this.#colorStep); // add colorStep

    this.texCoordX += this.#texCoordXStep;
    this.texCoordY += this.#texCoordYStep;
    this.oneOverZ += this.#oneOverZStep;
    this.depth += this.#depthStep;

    // this.#lighting += this.#lightingStep // TODO: Add lighting.
  }
}

class Gradients {
  // See also: https://github.com/BennyQBD/3DSoftwareRenderer/blob/8f196cd3d9811c47638d102e08988162afffc04e/src/Gradients.java.
  // https://youtu.be/4sSL0kGMjMQ?t=1016

  oneOverZ;
  texCoordX;
  texCoordY;
  depth;

  texCoordXXStep;
  texCoordXYStep;
  texCoordYXStep;
  texCoordYYStep;

  oneOverZXStep;
  oneOverZYStep;

  depthXStep;
  depthYStep;

  color;
  colorYStep;
  colorXStep;

  constructor(minYVert, midYVert, maxYVert) {
    this.color = [minYVert.color, midYVert.color, maxYVert.color];

    const oneOverdX =
      1 /
      ((midYVert.x - maxYVert.x) * (minYVert.y - maxYVert.y) -
        (minYVert.x - maxYVert.x) * (midYVert.y - maxYVert.y));

    const oneOverdY = -oneOverdX;

    // Texture

    this.oneOverZ = [
      1 / minYVert.pos[W],
      1 / midYVert.pos[W],
      1 / maxYVert.pos[W],
    ];

    this.texCoordX = [
      minYVert.texCoords[X] * this.oneOverZ[0],
      midYVert.texCoords[X] * this.oneOverZ[1],
      maxYVert.texCoords[X] * this.oneOverZ[2],
    ];

    this.texCoordY = [
      minYVert.texCoords[Y] * this.oneOverZ[0],
      midYVert.texCoords[Y] * this.oneOverZ[1],
      maxYVert.texCoords[Y] * this.oneOverZ[2],
    ];

    this.depth = [minYVert.pos[Z], midYVert.pos[Z], maxYVert.pos[Z]];

    // Note that the W component is the perspective Z value;
    // The Z component is the occlusion Z value
    this.texCoordXXStep = Gradients.calcXStep(
      this.texCoordX,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdX
    );

    this.texCoordXYStep = Gradients.calcYStep(
      this.texCoordX,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdY
    );

    this.texCoordYXStep = Gradients.calcXStep(
      this.texCoordY,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdX
    );

    this.texCoordYYStep = Gradients.calcYStep(
      this.texCoordY,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdY
    );

    this.oneOverZXStep = Gradients.calcXStep(
      this.oneOverZ,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdX
    );

    this.oneOverZYStep = Gradients.calcYStep(
      this.oneOverZ,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdY
    );

    this.depthXStep = Gradients.calcXStep(
      this.depth,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdX
    );

    this.depthYStep = Gradients.calcYStep(
      this.depth,
      minYVert,
      midYVert,
      maxYVert,
      oneOverdY
    );

    // Color

    // (c1 - c2) * (y0 - y2) - (c0 - c2) * (y1 - y2)
    // a           b           c           d
    {
      const a = vec4.sub(vec4.create(), this.color[1], this.color[2]);
      const b = minYVert.y - maxYVert.y;

      const c = vec4.sub(vec4.create(), this.color[0], this.color[2]);
      const d = midYVert.y - maxYVert.y;

      const left = vec4.scale(vec4.create(), a, b);
      const right = vec4.scale(vec4.create(), c, d);

      const sub = vec4.sub(vec4.create(), left, right);

      this.colorXStep = vec4.scale(vec4.create(), sub, oneOverdX);
    }

    // (c1 - c2) * (x0 - x2) - (c0 - c2) * (x1 - x2)
    // a           b           c           d
    {
      const a = vec4.sub(vec4.create(), this.color[1], this.color[2]);
      const b = minYVert.x - maxYVert.x;

      const c = vec4.sub(vec4.create(), this.color[0], this.color[2]);
      const d = midYVert.x - maxYVert.x;

      const left = vec4.scale(vec4.create(), a, b);
      const right = vec4.scale(vec4.create(), c, d);

      const sub = vec4.sub(vec4.create(), left, right);

      this.colorYStep = vec4.scale(vec4.create(), sub, oneOverdY);
    }
  }

  static calcXStep(values, minYVert, midYVert, maxYVert, oneOverdX) {
    return (
      ((values[1] - values[2]) * (minYVert.y - maxYVert.y) -
        (values[0] - values[2]) * (midYVert.y - maxYVert.y)) *
      oneOverdX
    );
  }

  static calcYStep(values, minYVert, midYVert, maxYVert, oneOverdY) {
    return (
      ((values[1] - values[2]) * (minYVert.x - maxYVert.x) -
        (values[0] - values[2]) * (midYVert.x - maxYVert.x)) *
      oneOverdY
    );
  }
}

// ?. Line Rendering

function drawLine3d(a, b, color = c, gradients) {
  const aInside = isInsideViewFrustum(a.pos);
  const bInside = isInsideViewFrustum(b.pos);

  if (aInside && bInside) {
    line3d(a, b, color, gradients);
    return;
  }

  // Don't draw anything if we are completely outside.
  //if (!aInside && !bInside) return;

  const vertices = [a, b];
  const auxillaryList = [];

  if (
    clipPolygonAxis(vertices, auxillaryList, 0) &&
    clipPolygonAxis(vertices, auxillaryList, 1) &&
    clipPolygonAxis(vertices, auxillaryList, 2)
  ) {
    const initialVertex = vertices[0];
    for (let i = 1; i < vertices.length - 1; i += 1) {
      line3d(initialVertex, vertices[i], color, gradients);
    }
  }
}

// d. Triangle Rendering

function drawTriangle(v1, v2, v3, texture, alpha) {
  const v1Inside = isInsideViewFrustum(v1.pos);
  const v2Inside = isInsideViewFrustum(v2.pos);
  const v3Inside = isInsideViewFrustum(v3.pos);

  if (v1Inside && v2Inside && v3Inside) {
    fillTriangle(v1, v2, v3, texture, alpha);
    return;
  }

  // Don't draw anyhing if we are completely outside.
  //if (!v1Inside && !v2Inside && !v3Inside) return;

  const vertices = [v1, v2, v3];
  const auxillaryList = [];

  if (
    clipPolygonAxis(vertices, auxillaryList, 0) &&
    clipPolygonAxis(vertices, auxillaryList, 1) &&
    clipPolygonAxis(vertices, auxillaryList, 2)
  ) {
    const initialVertex = vertices[0];
    for (let i = 1; i < vertices.length - 1; i += 1) {
      fillTriangle(initialVertex, vertices[i], vertices[i + 1], texture, alpha);
    }
  }
}

function fillTriangle(minYVert, midYVert, maxYVert, texture, alpha) {
  minYVert = minYVert.transform(screenMatrix).perspectiveDivide();
  midYVert = midYVert.transform(screenMatrix).perspectiveDivide();
  maxYVert = maxYVert.transform(screenMatrix).perspectiveDivide();

  // Backface culling by checking if Z normal is negative.

  // TODO: Add normal to vertex (for basic lighting) here?
  //if (triangleAreaDouble(minYVert, maxYVert, midYVert) >= 0) {
  //  return;
  //}

  if (maxYVert.y < midYVert.y) {
    const temp = maxYVert;
    maxYVert = midYVert;
    midYVert = temp;
  }

  if (midYVert.y < minYVert.y) {
    const temp = midYVert;
    midYVert = minYVert;
    minYVert = temp;
  }

  if (maxYVert.y < midYVert.y) {
    const temp = maxYVert;
    maxYVert = midYVert;
    midYVert = temp;
  }

  const handedness = triangleAreaDouble(minYVert, maxYVert, midYVert) >= 0;

  scanTriangle(minYVert, midYVert, maxYVert, handedness, texture, alpha);

  // Debug / Wireframes
  // TODO: How to accurately outline a triangle?
  // in drawScanLine: Add border at xMin and xMax and also use j to know if we are at the bottom.

  const tempColor = c.slice();
  color(127, 127, 127);

  line(minYVert.x, minYVert.y, midYVert.x, midYVert.y);
  line(midYVert.x, midYVert.y, maxYVert.x, maxYVert.y);
  line(minYVert.x, minYVert.y, maxYVert.x, maxYVert.y);

  color(...minYVert.color);
  plot(minYVert.x, minYVert.y);

  color(...midYVert.color);
  plot(midYVert.x, midYVert.y);

  color(...maxYVert.color);
  plot(maxYVert.x, maxYVert.y);

  color(...tempColor);
}

function triangleAreaDouble(a, b, c) {
  const x1 = b.x - a.x;
  const y1 = b.y - a.y;
  const x2 = c.x - a.x;
  const y2 = c.y - a.y;
  return x1 * y2 - x2 * y1;
}

function scanTriangle(
  minYVert,
  midYVert,
  maxYVert,
  handedness,
  texture,
  alpha
) {
  const gradients = new Gradients(minYVert, midYVert, maxYVert);
  const topToBottom = new Edge(gradients, minYVert, maxYVert, 0);
  const topToMiddle = new Edge(gradients, minYVert, midYVert, 0);
  const middleToBottom = new Edge(gradients, midYVert, maxYVert, 1);

  scanEdges(gradients, topToBottom, topToMiddle, handedness, texture, alpha);
  scanEdges(gradients, topToBottom, middleToBottom, handedness, texture, alpha);
}

function scanEdges(gradients, a, b, handedness, texture, alpha, render = true) {
  let left = a;
  let right = b;
  if (handedness) {
    let temp = left;
    left = right;
    right = temp;
  }

  const yStart = b.yStart;
  const yEnd = b.yEnd;

  //console.log(yStart, yEnd)
  for (let i = yStart; i < yEnd; i += 1) {
    if (render) {
      drawScanLine(gradients, left, right, i, texture, alpha, render);
    }
    left.step();
    right.step();
  }
}

function drawScanLine(
  gradients,
  left,
  right,
  j,
  texture,
  alpha,
  render = true
) {
  const xMin = ceil(left.x);
  const xMax = ceil(right.x);

  const xPrestep = xMin - left.x;

  // Texture
  const xDist = right.x - left.x;
  const texCoordXXStep = (right.texCoordX - left.texCoordX) / xDist;
  const texCoordYXStep = (right.texCoordY - left.texCoordY) / xDist;

  let texCoordX = left.texCoordX + texCoordXXStep * xPrestep;
  let texCoordY = left.texCoordY + texCoordYXStep * xPrestep;

  // Depth
  const depthXStep = (right.depth - left.depth) / xDist;
  const oneOverZXStep = (right.oneOverZ - left.oneOverZ) / xDist;
  let oneOverZ = left.oneOverZ + oneOverZXStep * xPrestep;
  let depth = left.depth + depthXStep * xPrestep;

  // Color
  const gradientColor = vec4.add(
    vec4.create(),
    left.color,
    vec4.scale(vec4.create(), gradients.colorXStep, xPrestep)
  );

  //console.log(xMin, xMax, j)

  for (let i = xMin; i < xMax; i += 1) {
    const index = i + j * width;

    //if (index < depthBuffer.length && depth < depthBuffer[index]) {
    //if (depth < depthBuffer[index]) {
    //  depthBuffer[index] = depth;

    // TODO: Add color and fog.
    // const stretchedDepth = 1 - (depth - 0.9) * 10;
    // console.log(stretchedDepth);
    // const r = Math.floor(gradientColor[X] * 255 + 0.5);
    // const g = Math.floor(gradientColor[Y] * 255 + 0.5);
    // const b = Math.floor(gradientColor[Z] * 255 + 0.5);
    // color(255 * stretchedDepth, 255 * stretchedDepth, 255 * stretchedDepth);
    // plot(i, j);

    const z = 1 / oneOverZ;

    if (texture) {
      const srcX = texCoordX * z * (texture.width - 1) + 0.5;
      const srcY = texCoordY * z * (texture.height - 1) + 0.5;

      if (render) {
        copy(i, j, srcX, srcY, texture, alpha); // TODO: Eventually remove alpha from here.
        //plot(i, j);
      }

      texCoordX += texCoordXXStep;
      texCoordY += texCoordYXStep;
    } else {
      vec4.add(gradientColor, gradientColor, gradients.colorXStep);
      color(...gradientColor);
      plot(i, j);
    }

    // Depth
    oneOverZ += oneOverZXStep;
    depth += depthXStep;
  }
}

function clipPolygonAxis(vertices, auxillaryList, componentIndex) {
  clipPolygonComponent(vertices, componentIndex, 1.0, auxillaryList);
  vertices.length = 0;

  if (auxillaryList.length === 0) {
    return false;
  }

  clipPolygonComponent(auxillaryList, componentIndex, -1.0, vertices);
  auxillaryList.length = 0;

  return !(vertices.length === 0);
}

function clipPolygonComponent(
  vertices,
  componentIndex,
  componentFactor,
  result
) {
  let prevVertex = vertices[vertices.length - 1];
  let prevComponent = prevVertex.pos[componentIndex] * componentFactor;
  let prevInside = prevComponent <= prevVertex.w;

  for (let i = 0; i < vertices.length; i += 1) {
    const curVertex = vertices[i];
    const curComponent = curVertex.pos[componentIndex] * componentFactor;

    const curInside = curComponent <= curVertex.w;

    if (curInside ^ prevInside) {
      const lerpAmount =
        (prevVertex.w - prevComponent) /
        (prevVertex.w - prevComponent - (curVertex.w - curComponent));
      result.push(prevVertex.lerp(curVertex, lerpAmount));
    }

    if (curInside) result.push(curVertex);

    prevVertex = curVertex;
    prevComponent = curComponent;
    prevInside = curInside;
  }
}

export { Camera, Form, Dolly };

// e. Utilities

let graphicLogCount = 0;
const graphicLogMax = 5;

/*
function graphicLog(log) {
  graphicLogCount = Math.min(graphicLogCount + 1, graphicLogMax);
  if (graphicLogCount < graphicLogMax) {
    console.log(log);
  }
}
*/
