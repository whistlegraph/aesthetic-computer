// 🧮 Numbers
import * as quat from "../dep/gl-matrix/quat.mjs";
import * as mat3 from "../dep/gl-matrix/mat3.mjs";
import * as mat4 from "../dep/gl-matrix/mat4.mjs";
import * as vec2 from "../dep/gl-matrix/vec2.mjs";
import * as vec3 from "../dep/gl-matrix/vec3.mjs";
import * as vec4 from "../dep/gl-matrix/vec4.mjs";

export { vec2, vec3, vec4, mat3, mat4, quat };

const { round, min, max, sqrt } = Math;

// Returns true if the number is even, and false otherwise.
export function even(n) {
  return n % 2 === 0;
}

export function odd(n) {
  return !even(n);
}

// Accepts integer from 0–16
// Yields 17 different values between 0–255.
export function byteInterval17(i16) {
  return Math.min(i16 * 16, 255);
}

// Generates an integer from 0-n (inclusive)
export function randInt(n) {
  return Math.floor(Math.random() * (n + 1));
}

// Generates an array of random integers from 0-n (inclusive)
// TODO: How could this be made more generic? 22.1.5
// TODO: How to make this account for range? 2022.01.17.00.33
export function randIntArr(n, count) {
  return Array(count).fill(n).map(randInt);
}

// Generates an integer from low-high (inclusive)
export function randIntRange(low, high) {
  return low + randInt(high - low);
}

// Multiplies one or more [] operands by n and returns a Number or Array.
export function multiply(operands, n) {
  if (Array.isArray(operands)) {
    return operands.map((o) => o * n);
  } else {
    return operands * n;
  }
}

// Gets the distance between two points.
// (4) x1, y1, x2, y1
// (2) {x, y}, {x, y}
export function dist() {
  let x1, y1, x2, y2;

  if (arguments.length === 4) {
    x1 = arguments[0];
    y1 = arguments[1];
    x2 = arguments[2];
    y2 = arguments[3];
  } else if (arguments.length === 2) {
    x1 = arguments[0].x;
    y1 = arguments[0].y;
    x2 = arguments[1].x;
    y2 = arguments[1].y;
  }

  const dx = x2 - x1;
  const dy = y2 - y1;
  return sqrt(dx * dx + dy * dy);
}

// TODO: Would 6 decimal places work? (Do some research) 22.11.08.00.21
export function dist3d(p1, p2) {
  // Convert everything to 4 decimal places.
  // Drop some precision here so vec3.dist (Math.hypot) doesn't go crazy
  // on my Float32Array values.
  if (p1.buffer) p1 = p1.map((p) => p.toPrecision(4));
  if (p2.buffer) p2 = p2.map((p) => p.toPrecision(4));
  return Number(vec3.dist(p1, p2).toPrecision(4));
}

// Converts degrees to radians.
export function radians(deg) {
  return deg * (Math.PI / 180);
}

export function degrees(rad) {
  return rad * (180 / Math.PI);
}

// Keeps a value between min and max.
export function clamp(value, min, max) {
  return Math.min(Math.max(value, min), max);
}

// Slides a number between a and by a normalized amount.
export function lerp(a, b, amount) {
  return a + (b - a) * clamp(amount, 0, 1);
}

// Maps a number within a range to a new range.
// https://stackoverflow.com/a/23202637/8146077
export function map(num, inMin, inMax, outMin, outMax) {
  return ((num - inMin) * (outMax - outMin)) / (inMax - inMin) + outMin;
}

// Returns a string of numbers based on local system time. YYYY.MM.DD.HH.MM.SS
export function timestamp() {
  const d = new Date();
  const pad = (n) => n.toString().padStart(2, "0");
  return `
    ${d.getFullYear()}.
    ${d.getMonth() + 1}.
    ${pad(d.getDate())}.
    ${pad(d.getHours())}.
    ${pad(d.getMinutes())}.
    ${pad(d.getSeconds())}`.replace(/\s/g, "");
}

// A. Lerps over a single value (from->to) via `progress` (0->1).
// B. Quantizes over an array of individual `values` via `progress` (0->1).
// TODO: Allow `progress` to be 0->N which would map to an index in `values`.
export class Track {
  #values;
  #result;
  #quantize;

  constructor(values, result) {
    this.#values = values;
    this.#result = result;
    this.#quantize = Array.isArray(values);
  }

  step(progress) {
    if (this.#quantize) {
      const index = Math.min(
        Math.floor(progress * this.#values.length),
        this.#values.length - 1
      );
      this.#result(this.#values[index]);
    } else {
      this.#result(lerp(this.#values.from, this.#values.to, progress));
    }
  }
}

function cleanHexString(h) {
  return h.replace("#", "").replace("0x", "");
}

// Determines if a string is a valid hex value.
export function isHexString(h) {
  h = cleanHexString(h);
  const a = parseInt(h, 16);
  return a.toString(16) === h.toLowerCase();
}

// 🌈 Colors

// Lerp two RGBA arrays, skipping alpha and rounding the output.
// (Assumes 0-255)
export function shiftRGB(a, b, step, mode = "lerp", range = 255) {
  const low = range === 255 ? [1, 10] : [0.01, 0.05];
  const high = range === 255 ? [245, 250] : [0.92, 0.95];
  if (mode === "add" || mode == "subtract") {
    if (mode === "subtract") step *= -1;
    const shifted = [
      clamp(a[0] + a[0] * step, randIntRange(...low), randIntRange(...high)),
      clamp(a[1] + a[1] * step, randIntRange(...low), randIntRange(...high)),
      clamp(a[2] + a[2] * step, randIntRange(...low), randIntRange(...high)),
      range,
    ];
    if (range === 255) return shifted.map((v) => round(v));
    else return shifted;
  } else {
    const shifted = [
      lerp(a[0], b[0], step),
      lerp(a[1], b[1], step),
      lerp(a[2], b[2], step),
      range,
    ];
    if (range === 255) return shifted.map((v) => round(v));
    else return shifted;
  }
}

// Convert separate rgb values to a single integer.
export function rgbToHex(r, g, b) {
  return (1 << 24) + (r << 16) + (g << 8) + b;
}

// The same, as a string. (Via: https://stackoverflow.com/a/5624139)
// Optionally add a prefix like "#" or "0x"
export function rgbToHexStr(r, g, b, prefix = "") {
  return prefix + ((1 << 24) | (r << 16) | (g << 8) | b).toString(16).slice(1);
}

// Takes either a string hex or a number hex and outputs and [RGB] array.
// TODO: Take in alpha.
export function hexToRgb(h) {
  const int = typeof h === "string" ? parseInt(cleanHexString(h), 16) : h;
  return [(int >> 16) & 255, (int >> 8) & 255, int & 255];
}

// The below was adapted from: https://codepen.io/Elliotclyde/pen/MWyRezZ

// TODO: The two functions below could be refactored and combined pretty easily. 22.11.19.00.30
export function saturate(rgb, amount = 1) {
  const hadAlpha = rgb.length === 4;
  const alpha = rgb[3];
  rgb = rgb.slice(0, 3);

  const grey = lightness(rgb) * 255;

  const [low, mid, high] = getLowestMiddleHighest(rgb);

  if (low.val === high.val) return rgb; // Return grey if we are there already.

  const saturationRange = round(min(255 - grey, grey));
  const maxChange = min(255 - high.val, low.val);
  const changeAmount = min(saturationRange * amount, maxChange);
  const middleValueRatio = (grey - mid.val) / (grey - high.val);

  const out = [];
  out[high.index] = round(high.val + changeAmount);
  out[low.index] = round(low.val - changeAmount);
  out[mid.index] = round(grey + (out[high.index] - grey) * middleValueRatio);
  if (hadAlpha) out[3] = alpha; // Add alpha channel back if it exists.
  return out;
}

export function desaturate(rgb, amount = 1) {
  const hadAlpha = rgb.length === 4;
  const alpha = rgb[3];
  rgb = rgb.slice(0, 3);

  const [low, mid, high] = getLowestMiddleHighest(rgb);
  const grey = lightness(rgb) * 255;

  if (low.val === high.val) return rgb; // Return grey if we are there already.

  const saturationRange = round(min(255 - grey, grey));
  const maxChange = grey - low.val;
  const changeAmount = min(saturationRange * amount, maxChange);
  const middleValueRatio = (grey - mid.val) / (grey - high.val);

  const out = [];
  out[high.index] = round(high.val - changeAmount);
  out[low.index] = round(low.val + changeAmount);
  out[mid.index] = round(grey + (out[high.index] - grey) * middleValueRatio);
  if (hadAlpha) out[3] = alpha; // Add alpha channel back if it exists.
  return out;
}

// These support `saturate` and `desaturate` above.

function lightness(rgb) {
  // Get the highest and lowest out of red green and blue
  const highest = max(...rgb);
  const lowest = min(...rgb);
  return (highest + lowest) / 2 / 255; // Return the average divided by 255
}

function getLowestMiddleHighest(rgb) {
  let high = { val: 0, index: -1 };
  let low = { val: Infinity, index: -1 };

  rgb.map((val, index) => {
    if (val > high.val) {
      high = { val: val, index: index };
    }
    if (val < low.val) {
      low = { val: val, index: index };
    }
  });

  const mid = { index: 3 - high.index - low.index };
  mid.val = rgb[mid.index];
  return [low, mid, high];
}
