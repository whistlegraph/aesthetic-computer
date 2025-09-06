// 🧮 Numbers
// import * as quat from "../dep/gl-matrix/quat.mjs";
// import * as mat3 from "../dep/gl-matrix/mat3.mjs";
// import * as mat4 from "../dep/gl-matrix/mat4.mjs";
// import * as vec2 from "../dep/gl-matrix/vec2.mjs";
// import * as vec3 from "../dep/gl-matrix/vec3.mjs";
// import * as vec4 from "../dep/gl-matrix/vec4.mjs";
import { anyKey } from "./help.mjs";

// export { vec2, vec3, vec4, mat3, mat4, quat };

const {
  abs,
  round,
  floor,
  ceil,
  random,
  PI,
  min,
  max,
  sqrt,
  pow,
  atan2,
  sin,
  cos,
} = Math;

// Utilities for modifying {x, y} points.
export const p2 = {
  // Turn two values into an {x, y} point.
  of: function (x, y) {
    return { x, y };
  },
  // Get the length of the point as a vector.
  len: function (pA) {
    return Math.hypot(pA.x, pA.y);
  },
  // Normalize a vector to have a length of 1
  norm: function (p) {
    let len = this.len(p);
    return len === 0 ? { x: 0, y: 0 } : { x: p.x / len, y: p.y / len };
  },
  // Check for the equality of two points.
  eq: function (p1, p2) {
    return p1.x === p2.x && p1.y === p2.y;
  },
  // Mutably add P->in to P->out.
  inc: function (pout, pin) {
    pout.x += pin.x;
    pout.y += pin.y;
    return pout;
  },
  // Mutably scale P->out by P->in.
  scl: function (pout, pin) {
    pout.x *= pin.x || pin;
    pout.y *= pin.y || pin;
    return pout;
  },
  // Immutably add pA + pB.
  add: function (pA, pB) {
    return {
      x: pA.x + pB.x,
      y: pA.y + pB.y,
    };
  },
  // Immutably sub pA - pB.
  sub: function (pA, pB) {
    return {
      x: pA.x - pB.x,
      y: pA.y - pB.y,
    };
  },
  // Immutably rotate p by angle in radians.
  rot(p, angle) {
    return {
      x: p.x * cos(angle) - p.y * sin(angle),
      y: p.x * sin(angle) + p.y * cos(angle),
    };
  },
  // Immutably multiply pA * pB.
  mul: function (pA, pB) {
    return {
      x: pA.x * (pB.x || pB),
      y: pA.y * (pB.y || pB),
    };
  },
  // Immutably divide pA / pB.
  // If pA is a single number then this function expands it to an `{x, y}`.
  // Note: Other library functions here could do the same. 2023.1.19
  div: function (pA, pB) {
    if (typeof pA === "number") pA = { x: pA, y: pA };
    return {
      x: pA.x / pB.x,
      y: pA.y / pB.y,
    };
  },
  mid: function (pA, pB) {
    return {
      x: (pA.x + pB.x) / 2,
      y: (pA.y + pB.y) / 2,
    };
  },
  dist: function (pA, pB) {
    // Note: There is also a more generic distance function in `num`.
    return sqrt(pow(pB.x - pA.x, 2) + pow(pB.y - pA.y, 2));
  },
  angle: function (pA, pB) {
    return atan2(pB.y - pA.y, pB.x - pA.x);
  },
  dot: function (pA, pB) {
    return pA.x * pB.x + pA.y * pB.y;
  },
  floor: function (p) {
    return { x: floor(p.x), y: floor(p.y) };
  },
};

// Add all numbers in an array or all parameters.
export function add(...args) {
  let numbers;
  if (Array.isArray(args[0])) {
    numbers = args[0];
  } else {
    numbers = args;
  }
  return numbers.reduce((acc, curr) => acc + curr, 0);
}

// Find the midpoint between two [x, y] coordinates.
export function midp(a, b) {
  return [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2];
}

// Determine if the value is a number or not.
export function number(maybeNumber) {
  return typeof maybeNumber === "number";
}

// Line Intersection
// https://stackoverflow.com/a/24392281
// accepts 8 arguments or 2 {x0, y0, x1, y1} describing 2 lines
// returns true if the line from (a,b)->(c,d) intersects with (p,q)->(r,s)
export function intersects() {
  let a, b, c, d, p, q, r, s;
  if (
    arguments.length === 2 &&
    typeof arguments[0] === "object" &&
    typeof arguments[1] === "object"
  ) {
    a = arguments[0].x0;
    b = arguments[0].y0;
    c = arguments[0].x1;
    d = arguments[0].y1;
    p = arguments[1].x0;
    q = arguments[1].y0;
    r = arguments[1].x1;
    s = arguments[1].y1;
  } else if (arguments.length === 8) {
    [a, b, c, d, p, q, r, s] = arguments;
  } else {
    console.warn("Invalid intersection input.");
    return false;
  }

  let det, lambda, gamma;
  det = (c - a) * (s - q) - (r - p) * (d - b);
  if (det === 0) {
    return false;
  } else {
    lambda = ((s - q) * (r - a) + (p - r) * (s - b)) / det;
    gamma = ((b - d) * (r - a) + (c - a) * (s - b)) / det;
    return 0 < lambda && lambda < 1 && 0 < gamma && gamma < 1;
  }
}

// Ceil an integer (ignoring its sign)
// export function signedCeil(n) {
//   return n < 0 ? floor(n) : ceil(n);
// }

// // Floor an integer (ignoring its sign)
// export function signedFloor(val) {
//   let adjustment = 0;
//   if (val < 0) adjustment = -1;
//   return Math.floor(val) + adjustment;
// };

// Ceil a number away from 0
export function signedCeil(n) {
  return n < 0 ? Math.floor(n) : Math.ceil(n);
}

// Floor a number towards 0
export function signedFloor(val) {
  return val < 0 ? Math.ceil(val) : Math.floor(val);
}

// Wraps a number around 0 through a max.
export function wrap(n, to) {
  // n %= to; // This should work the same.
  // if (n < 0) n = to + n;
  return (n / to - floor(n / to)) * to;
}

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
  return min(i16 * 16, 255);
}

// Return a random float from 0->1.
export function rand() {
  return random();
}

// Generates an integer from 0-n (inclusive)
export function randInt(n) {
  return floor(rand() * (n + 1));
}

// Generates a random index from an array.
export function randInd(arr) {
  return randInt(arr.length - 1);
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

// Converts an array of strings formatted like "1-100" into an array of
// random integer ranges.
// 📓 Used for parsing ranged params (usually colors) inside of pieces.
export function rangedInts(ints) {
  if (ints[0] === undefined) return;
  return ints.map((str) => {
    if (str.match(/^\d+-\d+$/)) {
      const range = str.split("-");
      return randIntRange(parseInt(range[0]), parseInt(range[1]));
    } else {
      if (str === "?") return randInt(255);
      return parseInt(str);
    }
  });
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
// (4) x1, y1, x2, y2
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

  //get dist directly by using p1.x - p2.x, p1.y - p2.y, p1.z - p2.z
  const dx = p1[0] - p2[0];
  const dy = p1[1] - p2[1];
  const dz = p1[2] - p2[2];
  return sqrt(dx * dx + dy * dy + dz * dz);
  // return Number(vec3.dist(p1, p2).toPrecision(4));
}

// Converts degrees to radians.
export function radians(deg = 0) {
  if (isNaN(deg)) deg = 0;
  return deg * (PI / 180);
}

// Converts radians to degrees.
export function degrees(rad) {
  return rad * (180 / PI);
}

// Keeps a value between min and max.
export function clamp(value, low, high) {
  return min(max(value, low), high);
}

export function within(range, a, b) {
  return abs(a - b) < range;
}

// Slides a number between a and b by a normalized amount.
export function lerp(a, b, amount) {
  return a + (b - a) * clamp(amount, 0, 1);
}

// Maps a number within a range to a new range.
// https://stackoverflow.com/a/23202637/8146077
export function map(num, inMin, inMax, outMin, outMax) {
  return ((num - inMin) * (outMax - outMin)) / (inMax - inMin) + outMin;
}

// Perlin noise in 2D.
export function perlin(x, y) {
  var grad3 = [
    [1, 1, 0],
    [-1, 1, 0],
    [1, -1, 0],
    [-1, -1, 0],
    [1, 0, 1],
    [-1, 0, 1],
    [1, 0, -1],
    [-1, 0, -1],
    [0, 1, 1],
    [0, -1, 1],
    [0, 1, -1],
    [0, -1, -1],
  ];
  var p = [];
  for (var i = 0; i < 256; i++) {
    p[i] = Math.floor(Math.random() * 256);
  }
  var perm = [];
  for (var i = 0; i < 512; i++) {
    perm[i] = p[i & 255];
  }

  function dot(g, x, y) {
    return g[0] * x + g[1] * y;
  }

  var F2 = 0.5 * (Math.sqrt(3.0) - 1.0);
  var s = (x + y) * F2;
  var i = Math.floor(x + s);
  var j = Math.floor(y + s);
  var G2 = (3.0 - Math.sqrt(3.0)) / 6.0;
  var t = (i + j) * G2;
  var X0 = i - t;
  var Y0 = j - t;
  var x0 = x - X0;
  var y0 = y - Y0;

  var i1, j1;
  if (x0 > y0) {
    i1 = 1;
    j1 = 0;
  } else {
    i1 = 0;
    j1 = 1;
  }

  var x1 = x0 - i1 + G2;
  var y1 = y0 - j1 + G2;
  var x2 = x0 - 1.0 + 2.0 * G2;
  var y2 = y0 - 1.0 + 2.0 * G2;

  var ii = i & 255;
  var jj = j & 255;
  var gi0 = perm[ii + perm[jj]] % 12;
  var gi1 = perm[ii + i1 + perm[jj + j1]] % 12;
  var gi2 = perm[ii + 1 + perm[jj + 1]] % 12;

  var t0 = 0.5 - x0 * x0 - y0 * y0;
  var n0, n1, n2;
  if (t0 < 0) n0 = 0.0;
  else {
    t0 *= t0;
    n0 = t0 * t0 * dot(grad3[gi0], x0, y0);
  }

  var t1 = 0.5 - x1 * x1 - y1 * y1;
  if (t1 < 0) n1 = 0.0;
  else {
    t1 *= t1;
    n1 = t1 * t1 * dot(grad3[gi1], x1, y1);
  }

  var t2 = 0.5 - x2 * x2 - y2 * y2;
  if (t2 < 0) n2 = 0.0;
  else {
    t2 *= t2;
    n2 = t2 * t2 * dot(grad3[gi2], x2, y2);
  }

  return 70.0 * (n0 + n1 + n2);
}

// Return the maximum number in an array.
export function arrMax(arr) {
  return arr.reduce((top, current) => max(top, current), -Infinity);
}

// Return a new array with every n index missing.
export function arrCompress(arr, n) {
  return arr.filter((_, index) => (index + 1) % n === 0);
}

// Returns a string of numbers based on local system time. YYYY.MM.DD.HH.MM.SS:MS
export function timestamp() {
  const d = new Date();
  const pad = (n, digits = 2) => n.toString().padStart(digits, "0");
  return `
    ${d.getFullYear()}.
    ${pad(d.getMonth() + 1)}.
    ${pad(d.getDate())}.
    ${pad(d.getHours())}.
    ${pad(d.getMinutes())}.
    ${pad(d.getSeconds())}.
    ${pad(d.getMilliseconds(), 3)}`.replace(/\s/g, "");
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
      const index = min(
        floor(progress * this.#values.length),
        this.#values.length - 1,
      );
      this.#result(this.#values[index]);
    } else {
      this.#result(lerp(this.#values.from, this.#values.to, progress));
    }
  }
}

function cleanHexString(h) {
  return h.replace("#", "").replace("0x", "").toUpperCase();
}

// Determines if a string is a valid hex value.
export function isHexString(h) {
  h = cleanHexString(h);
  const a = parseInt(h, 16);
  return a.toString(16) === h.toLowerCase();
}

// 🌈 Colors
// Parses a color from piece params.
// Including numeric ranges (0-255) and string matching.
export function parseColor(params) {
  if (params.length === 0) return params;
  const int = parseInt(params[0]);

  if (!isNaN(int) || params.length > 2) {
    // Could be "? ? ?" here hence the length check.
    // Assume all params can be parsed into integers (excepting alpha)
    if (params.length === 2 || params.length === 4) {
      const alpha = calculateAlpha(params[params.length - 1]);
      params[params.length - 1] = alpha.toString();
    }

    return rangedInts(params);
  } else {
    let name = params[0].toLowerCase(); // Assume a css color string.

    let alpha = calculateAlpha(params[1]); // Calculate alpha param.

    if (name === "?") name = anyKey(cssColors); // Pick a name if `?` is passed.

    // Check for fade syntax first (new feature)
    if (name.startsWith("fade:")) {
      return {
        type: "fade",
        fadeString: name,
        alpha: alpha,
        originalParams: params
      };
    }

    // Check for color index format like "c151"
    const indexColor = parseColorIndex(name);
    if (indexColor) {
      return [...indexColor, alpha];
    }

    if (name in cssColors) {
      return [...cssColors[name], alpha];
    } else if (name === "erase") {
      return [-1, -1, -1, alpha];
    } else if (name === "rainbow") {
      return ["rainbow", alpha]; // Cycle through the "rainbow" colors here.
    } else if (name === "zebra") {
      return ["zebra", alpha]; // Cycle through black/white zebra colors.
    } else {
      return [0, 0, 0, alpha]; // Default to black if color name is not found
    }
  }
}

// Just used in parseColor above.
// TODO: Float has no range support.
function calculateAlpha(alphaParam) {
  if (alphaParam === "?") return randIntRange(5, 255); // Always show a bit.
  let alpha = parseFloat(alphaParam);
  if (alpha >= 0 && alpha <= 1) {
    alpha = round(alpha * 255); // Convert a float alpha from 0->1.
  } else {
    alpha = rangedInts([alphaParam]) || 255;
  }
  return alpha;
}

// Helper function to check if parseColor result is a fade
export function isFadeColor(colorResult) {
  return colorResult && 
         typeof colorResult === "object" && 
         !Array.isArray(colorResult) && 
         colorResult.type === "fade";
}

/*
export function parseColor(params) {
  const int = parseInt(params[0]);
  if (!isNaN(int)) {
    // assume all params can be parsed into integers

    // TODO:
    // Check to see if params length is 2
    // and check the alpha of the 2nd param similar to below.
    // Check to see if params length is 4 and do the same.
    // Wrap this alpha calculation in a function.

    return rangedInts(params);
  } else {
    // assume params represents a css color string
    const name = params[0].toLowerCase();

    // Calculate additional alpha param for named colors.
    let alpha = parseFloat(params[1]);
    if (alpha >= 0 && alpha <= 1) {
      alpha = round(alpha * 255); // Convert a float alpha from 0->1.
    } else {
      alpha = parseInt(params[1]) || 255;
    }

    // Check for color index format like "c151"
    const indexColor = parseColorIndex(name);
    if (indexColor) {
      return [...indexColor, alpha];
    }

    if (name in cssColors) {
      return [...cssColors[name], alpha];
    } else {
      return [0, 0, 0, alpha]; // default to black if color name is not found
    }
  }
}
*/

// All 147 named css colors.
export const cssColors = {
  aliceblue: [240, 248, 255],
  antiquewhite: [250, 235, 215],
  aqua: [0, 255, 255],
  aquamarine: [127, 255, 212],
  azure: [240, 255, 255],
  beige: [245, 245, 220],
  bisque: [255, 228, 196],
  black: [0, 0, 0],
  blanchedalmond: [255, 235, 205],
  blue: [0, 0, 255],
  blueviolet: [138, 43, 226],
  brown: [165, 42, 42],
  burlywood: [222, 184, 135],
  cadetblue: [95, 158, 160],
  chartreuse: [127, 255, 0],
  chocolate: [210, 105, 30],
  coral: [255, 127, 80],
  cornflowerblue: [100, 149, 237],
  cornsilk: [255, 248, 220],
  crimson: [220, 20, 60],
  cyan: [0, 255, 255],
  darkblue: [0, 0, 139],
  darkcyan: [0, 139, 139],
  darkgoldenrod: [184, 134, 11],
  darkgray: [169, 169, 169],
  darkgrey: [169, 169, 169],
  darkgreen: [0, 100, 0],
  darkkhaki: [189, 183, 107],
  darkmagenta: [139, 0, 139],
  darkolivegreen: [85, 107, 47],
  darkorange: [255, 140, 0],
  darkorchid: [153, 50, 204],
  darkred: [139, 0, 0],
  darksalmon: [233, 150, 122],
  darkseagreen: [143, 188, 143],
  darkslateblue: [72, 61, 139],
  darkslategray: [47, 79, 79],
  darkslategrey: [47, 79, 79],
  darkturquoise: [0, 206, 209],
  darkviolet: [148, 0, 211],
  deeppink: [255, 20, 147],
  deepskyblue: [0, 191, 255],
  dimgray: [105, 105, 105],
  dimgrey: [105, 105, 105],
  dodgerblue: [30, 144, 255],
  firebrick: [178, 34, 34],
  floralwhite: [255, 250, 240],
  forestgreen: [34, 139, 34],
  fuchsia: [255, 0, 255],
  gainsboro: [220, 220, 220],
  ghostwhite: [248, 248, 255],
  gold: [255, 215, 0],
  goldenrod: [218, 165, 32],
  gray: [128, 128, 128],
  grey: [128, 128, 128],
  green: [0, 128, 0],
  greenyellow: [173, 255, 47],
  honeydew: [240, 255, 240],
  hotpink: [255, 105, 180],
  indianred: [205, 92, 92],
  indigo: [75, 0, 130],
  ivory: [255, 255, 240],
  khaki: [240, 230, 140],
  lavender: [230, 230, 250],
  lavenderblush: [255, 240, 245],
  lawngreen: [124, 252, 0],
  lemonchiffon: [255, 250, 205],
  lightblue: [173, 216, 230],
  lightcoral: [240, 128, 128],
  lightcyan: [224, 255, 255],
  lightgoldenrodyellow: [250, 250, 210],
  lightgray: [211, 211, 211],
  lightgrey: [211, 211, 211],
  lightgreen: [144, 238, 144],
  lightpink: [255, 182, 193],
  lightsalmon: [255, 160, 122],
  lightseagreen: [32, 178, 170],
  lightskyblue: [135, 206, 250],
  lightslategray: [119, 136, 153],
  lightslategrey: [119, 136, 153],
  lightsteelblue: [176, 196, 222],
  lightyellow: [255, 255, 224],
  lime: [0, 255, 0],
  limegreen: [50, 205, 50],
  linen: [250, 240, 230],
  magenta: [255, 0, 255],
  maroon: [128, 0, 0],
  mediumaquamarine: [102, 205, 170],
  mediumblue: [0, 0, 205],
  mediumorchid: [186, 85, 211],
  mediumpurple: [147, 112, 219],
  mediumseagreen: [60, 179, 113],
  mediumslateblue: [123, 104, 238],
  mediumspringgreen: [0, 250, 154],
  mediumturquoise: [72, 209, 204],
  mediumvioletred: [199, 21, 133],
  midnightblue: [25, 25, 112],
  mintcream: [245, 255, 250],
  mistyrose: [255, 228, 225],
  moccasin: [255, 228, 181],
  navajowhite: [255, 222, 173],
  navy: [0, 0, 128],
  oldlace: [253, 245, 230],
  olive: [128, 128, 0],
  olivedrab: [107, 142, 35],
  orange: [255, 165, 0],
  orangered: [255, 69, 0],
  orchid: [218, 112, 214],
  palegoldenrod: [238, 232, 170],
  palegreen: [152, 251, 152],
  paleturquoise: [175, 238, 238],
  palevioletred: [219, 112, 147],
  papayawhip: [255, 239, 213],
  peachpuff: [255, 218, 185],
  peru: [205, 133, 63],
  pink: [255, 192, 203],
  plum: [221, 160, 221],
  powderblue: [176, 224, 230],
  purple: [128, 0, 128],
  rebeccapurple: [102, 51, 153],
  red: [255, 0, 0],
  rosybrown: [188, 143, 143],
  royalblue: [65, 105, 225],
  saddlebrown: [139, 69, 19],
  salmon: [250, 128, 114],
  sandybrown: [244, 164, 96],
  seagreen: [46, 139, 87],
  seashell: [255, 245, 238],
  sienna: [160, 82, 45],
  silver: [192, 192, 192],
  skyblue: [135, 206, 235],
  slateblue: [106, 90, 205],
  slategray: [112, 128, 144],
  slategrey: [112, 128, 144],
  snow: [255, 250, 250],
  springgreen: [0, 255, 127],
  steelblue: [70, 130, 180],
  tan: [210, 180, 140],
  teal: [0, 128, 128],
  thistle: [216, 191, 216],
  tomato: [255, 99, 71],
  turquoise: [64, 224, 208],
  violet: [238, 130, 238],
  wheat: [245, 222, 179],
  white: [255, 255, 255],
  whitesmoke: [245, 245, 245],
  yellow: [255, 255, 0],
  yellowgreen: [154, 205, 50],
  // Custom brown colors for AC
  darkbrown: [101, 67, 33],
  darkerbrown: [62, 39, 35],
  darksienna: [139, 90, 43],
};

// 🌈 Rainbow color cycling.
let currentRainbowIndex = 0;

const rainbowColors = [
  cssColors.red,
  cssColors.orange,
  cssColors.yellow,
  cssColors.green,
  cssColors.blue,
  cssColors.indigo,
  cssColors.violet,
];

export function rainbow() {
  const color = rainbowColors[currentRainbowIndex];
  currentRainbowIndex = (currentRainbowIndex + 1) % rainbowColors.length;
  return color.slice();
}

// 🦓 Zebra color cycling (black/white alternating).
let currentZebraIndex = 0;
let frameZebraColor = null; // Cache the zebra color for this frame
let zebraFrameAdvanced = false; // Track if we've advanced this frame

const zebraColors = [
  cssColors.black,   // [0, 0, 0]
  cssColors.white,   // [255, 255, 255]
];

export function zebra(offset = 0) {
  // Advance the zebra index once per frame on the first call
  if (!zebraFrameAdvanced) {
    currentZebraIndex = (currentZebraIndex + 1) % zebraColors.length;
    frameZebraColor = zebraColors[currentZebraIndex].slice();
    zebraFrameAdvanced = true;
  }
  
  // Calculate the final index with offset from the current frame's base
  const finalIndex = (currentZebraIndex + offset) % zebraColors.length;
  const result = zebraColors[finalIndex];
  return result.slice();
}

// Reset zebra frame cache - should be called once per frame/execution
export function resetZebraCache() {
  zebraFrameAdvanced = false;
  frameZebraColor = null;
}

// Find a color inside of `cssColors` by value.
export function findColor(rgb) {
  for (let name in cssColors) {
    if (
      cssColors[name][0] === rgb[0] &&
      cssColors[name][1] === rgb[1] &&
      cssColors[name][2] === rgb[2]
    ) {
      return name;
    }
  }
}

// Create organized color index system
// First 16: Standard web colors
// Then organized by color families (reds, oranges, yellows, greens, blues, purples, etc.)
export const organizedColorIndex = [
  // 0-15: Standard 16 web colors (c0=black, c1=white)
  "black", "white", "red", "lime", "blue", "yellow", "cyan", "magenta", 
  "silver", "gray", "maroon", "olive", "green", "purple", "teal", "navy",
  
  // 17-32: Additional reds and pinks
  "crimson", "darkred", "firebrick", "indianred", "lightcoral", "salmon",
  "darksalmon", "lightsalmon", "pink", "lightpink", "hotpink", "deeppink",
  "palevioletred", "mediumvioletred", "coral", "tomato",
  
  // 33-48: Oranges
  "orange", "darkorange", "orangered", "chocolate", "saddlebrown", "sienna",
  "brown", "rosybrown", "sandybrown", "goldenrod", "darkgoldenrod", "peru",
  "burlywood", "tan", "navajowhite", "bisque",
  
  // 49-64: Yellows and golds
  "gold", "palegoldenrod", "khaki", "darkkhaki", "moccasin", "wheat",
  "lemonchiffon", "lightgoldenrodyellow", "lightyellow", "beige", "cornsilk",
  "blanchedalmond", "papayawhip", "antiquewhite", "linen", "oldlace",
  
  // 65-80: Greens
  "forestgreen", "darkgreen", "darkolivegreen", "darkseagreen", "limegreen",
  "seagreen", "mediumseagreen", "springgreen", "mediumspringgreen", "palegreen",
  "lightgreen", "lawngreen", "chartreuse", "greenyellow", "yellowgreen", "olivedrab",
  
  // 81-96: Blues and cyans
  "aqua", "darkturquoise", "turquoise", "mediumturquoise", "paleturquoise",
  "lightcyan", "cadetblue", "steelblue", "lightsteelblue", "powderblue",
  "lightblue", "skyblue", "lightskyblue", "deepskyblue", "dodgerblue", "cornflowerblue",
  
  // 97-112: More blues
  "royalblue", "mediumblue", "darkblue", "midnightblue", "slateblue",
  "darkslateblue", "mediumslateblue", "mediumpurple", "blueviolet", "indigo",
  "darkorchid", "darkviolet", "mediumorchid", "thistle", "plum", "violet",
  
  // 113-128: Purples and magentas
  "orchid", "fuchsia", "darkmagenta", "mediumvioletred", "lavenderblush",
  "mistyrose", "lavender", "ghostwhite", "azure", "aliceblue", "mintcream",
  "honeydew", "seashell", "ivory", "floralwhite", "snow",
  
  // 129-144: Grays and remaining colors
  "gainsboro", "lightgray", "lightgrey", "darkgray", "darkgrey", "dimgray",
  "dimgrey", "lightslategray", "lightslategrey", "slategray", "slategrey",
  "darkslategray", "darkslategrey", "whitesmoke", "rebeccapurple"
];

// Add any remaining CSS colors that weren't included above
const remainingColors = Object.keys(cssColors).filter(color => 
  !organizedColorIndex.includes(color)
);
export const completeColorIndex = [...organizedColorIndex, ...remainingColors];

// Get color by CSS index (c0, c1, etc.)
export function getColorByIndex(index) {
  // Use 0-based indexing (c0 = first color)
  if (index >= 0 && index < completeColorIndex.length) {
    const colorName = completeColorIndex[index];
    return {
      name: colorName,
      rgb: cssColors[colorName]
    };
  }
  return null;
}

// Palette colors (p0 = rainbow, etc.)
export const paletteColors = {
  0: "rainbow", // p0 = rainbow
  1: "zebra",   // p1 = zebra
  // Add more palette colors here as needed
};

// Get palette color by index (p1, p2, etc.)
export function getPaletteByIndex(index) {
  if (index in paletteColors) {
    return paletteColors[index];
  }
  return null;
}

// Static color index mapping - STANDARDIZED and PERMANENT for aesthetic.computer
export const staticColorMap = {
  // 0-15: Core web colors (never change these)
  0: [0, 0, 0],         // c0 = black
  1: [255, 255, 255],   // c1 = white  
  2: [255, 0, 0],       // c2 = red
  3: [0, 255, 0],       // c3 = lime
  4: [0, 0, 255],       // c4 = blue
  5: [255, 255, 0],     // c5 = yellow
  6: [0, 255, 255],     // c6 = cyan
  7: [255, 0, 255],     // c7 = magenta
  8: [192, 192, 192],   // c8 = silver
  9: [128, 128, 128],   // c9 = gray
  10: [128, 0, 0],      // c10 = maroon
  11: [128, 128, 0],    // c11 = olive
  12: [0, 128, 0],      // c12 = green
  13: [128, 0, 128],    // c13 = purple
  14: [0, 128, 128],    // c14 = teal
  15: [0, 0, 128],      // c15 = navy
  
  // 16-31: Red spectrum
  16: [220, 20, 60],    // c16 = crimson
  17: [139, 0, 0],      // c17 = darkred
  18: [178, 34, 34],    // c18 = firebrick
  19: [205, 92, 92],    // c19 = indianred
  20: [240, 128, 128],  // c20 = lightcoral
  21: [250, 128, 114],  // c21 = salmon
  22: [233, 150, 122],  // c22 = darksalmon
  23: [255, 160, 122],  // c23 = lightsalmon
  24: [255, 192, 203],  // c24 = pink
  25: [255, 182, 193],  // c25 = lightpink
  26: [255, 105, 180],  // c26 = hotpink
  27: [255, 20, 147],   // c27 = deeppink
  28: [219, 112, 147],  // c28 = palevioletred
  29: [199, 21, 133],   // c29 = mediumvioletred
  30: [255, 127, 80],   // c30 = coral
  31: [255, 99, 71],    // c31 = tomato
  
  // 32-47: Orange spectrum
  32: [255, 69, 0],     // c32 = orangered
  33: [255, 140, 0],    // c33 = darkorange
  34: [255, 165, 0],    // c34 = orange
  35: [255, 215, 0],    // c35 = gold
  36: [255, 218, 185],  // c36 = peachpuff
  37: [255, 228, 196],  // c37 = bisque
  38: [255, 239, 213],  // c38 = papayawhip
  39: [255, 228, 181],  // c39 = moccasin
  40: [255, 222, 173],  // c40 = navajowhite
  41: [245, 222, 179],  // c41 = wheat
  42: [222, 184, 135],  // c42 = burlywood
  43: [210, 180, 140],  // c43 = tan
  44: [188, 143, 143],  // c44 = rosybrown
  45: [205, 133, 63],   // c45 = peru
  46: [244, 164, 96],   // c46 = sandybrown
  47: [160, 82, 45],    // c47 = saddlebrown
  
  // 48-63: Yellow/Green spectrum
  48: [255, 248, 220],  // c48 = cornsilk
  49: [255, 255, 240],  // c49 = ivory
  50: [255, 250, 205],  // c50 = lemonchiffon
  51: [250, 250, 210],  // c51 = lightgoldenrodyellow
  52: [240, 230, 140],  // c52 = khaki
  53: [238, 232, 170],  // c53 = palegoldenrod
  54: [189, 183, 107],  // c54 = darkkhaki
  55: [154, 205, 50],   // c55 = yellowgreen
  56: [124, 252, 0],    // c56 = lawngreen
  57: [127, 255, 0],    // c57 = chartreuse
  58: [173, 255, 47],   // c58 = greenyellow
  59: [50, 205, 50],    // c59 = limegreen
  60: [152, 251, 152],  // c60 = palegreen
  61: [144, 238, 144],  // c61 = lightgreen
  62: [0, 250, 154],    // c62 = mediumspringgreen
  63: [0, 255, 127],    // c63 = springgreen
  
  // 64-79: Green spectrum
  64: [46, 125, 50],    // c64 = forestgreen
  65: [34, 139, 34],    // c65 = forestgreen
  66: [0, 100, 0],      // c66 = darkgreen
  67: [85, 107, 47],    // c67 = darkolivegreen
  68: [107, 142, 35],   // c68 = olivedrab
  69: [102, 205, 170],  // c69 = mediumaquamarine
  70: [127, 255, 212],  // c70 = aquamarine
  71: [176, 196, 222],  // c71 = lightsteelblue
  72: [175, 238, 238],  // c72 = paleturquoise
  73: [0, 206, 209],    // c73 = darkturquoise
  74: [72, 209, 204],   // c74 = mediumturquoise
  75: [64, 224, 208],   // c75 = turquoise
  76: [0, 139, 139],    // c76 = darkcyan
  77: [95, 158, 160],   // c77 = cadetblue
  78: [70, 130, 180],   // c78 = steelblue
  79: [176, 224, 230],  // c79 = powderblue
  
  // 80-95: Blue spectrum  
  80: [173, 216, 230],  // c80 = lightblue
  81: [135, 206, 235],  // c81 = skyblue
  82: [135, 206, 250],  // c82 = lightskyblue
  83: [0, 191, 255],    // c83 = deepskyblue
  84: [30, 144, 255],   // c84 = dodgerblue
  85: [100, 149, 237],  // c85 = cornflowerblue
  86: [123, 104, 238],  // c86 = mediumslateblue
  87: [106, 90, 205],   // c87 = slateblue
  88: [72, 61, 139],    // c88 = darkslateblue
  89: [25, 25, 112],    // c89 = midnightblue
  90: [0, 0, 139],      // c90 = darkblue
  91: [0, 0, 205],      // c91 = mediumblue
  92: [65, 105, 225],   // c92 = royalblue
  93: [138, 43, 226],   // c93 = blueviolet
  94: [75, 0, 130],     // c94 = indigo
  95: [72, 0, 72],      // c95 = darkmagenta
  
  // 96-111: Purple/Violet spectrum
  96: [153, 50, 204],   // c96 = darkorchid
  97: [186, 85, 211],   // c97 = mediumorchid
  98: [218, 112, 214],  // c98 = orchid
  99: [221, 160, 221],  // c99 = plum
  100: [238, 130, 238], // c100 = violet
  101: [255, 0, 255],   // c101 = fuchsia (same as magenta)
  102: [208, 32, 144],  // c102 = violetred
  103: [199, 21, 133],  // c103 = mediumvioletred
  104: [219, 112, 147], // c104 = palevioletred
  105: [255, 105, 180], // c105 = hotpink
  106: [255, 20, 147],  // c106 = deeppink
  107: [220, 20, 60],   // c107 = crimson
  108: [139, 69, 19],   // c108 = saddlebrown
  109: [160, 82, 45],   // c109 = saddlebrown
  110: [205, 133, 63],  // c110 = peru
  111: [222, 184, 135], // c111 = burlywood
  
  // 112-127: Browns and earth tones
  112: [245, 245, 220], // c112 = beige
  113: [255, 248, 220], // c113 = cornsilk
  114: [255, 235, 205], // c114 = blanchedalmond
  115: [245, 222, 179], // c115 = wheat
  116: [255, 228, 181], // c116 = moccasin
  117: [255, 218, 185], // c117 = peachpuff
  118: [210, 180, 140], // c118 = tan
  119: [188, 143, 143], // c119 = rosybrown
  120: [244, 164, 96],  // c120 = sandybrown
  121: [205, 133, 63],  // c121 = peru
  122: [160, 82, 45],   // c122 = saddlebrown
  123: [139, 69, 19],   // c123 = saddlebrown
  124: [101, 67, 33],   // c124 = darkbrown
  125: [62, 39, 35],    // c125 = darkerbrown
  126: [139, 90, 43],   // c126 = darksienna
  127: [165, 42, 42],   // c127 = brown
};

// Parse color index string like "c0", "c1", "p0" and return color
export function parseColorIndex(indexString) {
  if (typeof indexString === 'string') {
    if (indexString.startsWith('c')) {
      const index = parseInt(indexString.substring(1), 10);
      
      // Use standardized static color map
      if (!isNaN(index) && staticColorMap[index]) {
        const color = staticColorMap[index];
        // Validate color array
        if (Array.isArray(color) && color.length >= 3 && 
            color[0] !== undefined && color[1] !== undefined && color[2] !== undefined) {
          return color;
        }
      }
    } else if (indexString.startsWith('p')) {
      const index = parseInt(indexString.substring(1), 10);
      if (!isNaN(index)) {
        const palette = getPaletteByIndex(index);
        if (palette === "rainbow") {
          return ["rainbow"]; // Special marker for rainbow
        } else if (palette === "zebra") {
          return ["zebra"]; // Special marker for zebra
        }
      }
    }
  }
  return null;
}

// Alpha blends two colors, mutating and returning `dst`.
// Transcribed from C++: https://stackoverflow.com/a/12016968
export function blend(dst, src, alphaIn = 1) {
  if (src[3] === 0) return; // Return early if src is invalid.

  // Assume we are erasing if first channel is negative.
  if (src[0] === -1) {
    // (All three should be negative for an `erase`.)
    const normalizedAlpha = 1 - src[3] / 255;
    dst[3] *= normalizedAlpha;
    return;
  }

  // Otherwise continue to blend.
  if (src[3] === undefined) src[3] = 255; // Max alpha if it's not present.
  const alpha = src[3] * alphaIn + 1;
  const invAlpha = 256 - alpha;
  dst[0] = (alpha * src[0] + invAlpha * dst[0]) >> 8;
  dst[1] = (alpha * src[1] + invAlpha * dst[1]) >> 8;
  dst[2] = (alpha * src[2] + invAlpha * dst[2]) >> 8;
  dst[3] = dst[3] + alpha;
  return dst;
}

// Lerp two RGBA arrays, skipping alpha and rounding the output.
// (Assumes 0-255)
export function shiftRGB(a, b, step, mode = "lerp", range = 255) {
  const low = range === 255 ? [1, 10] : [0.01, 0.05];
  const high = range === 255 ? [245, 250] : [0.92, 0.95];

  if (mode === "add" || mode === "subtract") {
    if (mode === "subtract") step *= -1;
    const shifted = [
      clamp(a[0] + a[0] * step, randIntRange(...low), randIntRange(...high)),
      clamp(a[1] + a[1] * step, randIntRange(...low), randIntRange(...high)),
      clamp(a[2] + a[2] * step, randIntRange(...low), randIntRange(...high)),
      range,
    ];
    //if (range === 255) return shifted.map((v) => round(v));
    cc
  } else if (mode === "step") {
    const shifted = [
      towards(a[0], b[0], step),
      towards(a[1], b[1], step),
      towards(a[2], b[2], step),
      range,
    ];
    console.log(a[0], b[0], step);
    //if (range === 255) return shifted.map((v) => round(v));
    return shifted;
  } else {
    const shifted = [
      lerp(a[0], b[0], step),
      lerp(a[1], b[1], step),
      lerp(a[2], b[2], step),
      range,
    ];
    // if (range === 255) return shifted.map((v) => round(v));
    return shifted;
  }
}

// Move towards a target definitely and linearly. (Not quite lerping)
function towards(from, to, by) {
  return from < to ? min(from + by, to) : max(from - by, to);
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
  const out = [(int >> 16) & 255, (int >> 8) & 255, int & 255];
  return out;
}

// The below was adapted from: https://codepen.io/Elliotclyde/pen/MWyRezZ

// TODO: The two functions below could be refactored and combined pretty easily. 22.11.19.00.30
// Saturate a color by `amount`
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
// Desaturate a color by `amount`
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

// Convert rgb to hsl (360, 100, 100).
export function rgbToHsl(r, g, b) {
  (r /= 255), (g /= 255), (b /= 255);
  const max = Math.max(r, g, b),
    min = Math.min(r, g, b);
  let h,
    s,
    l = (max + min) / 2;

  if (max == min) {
    h = s = 0;
  } else {
    const d = max - min;
    s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
    switch (max) {
      case r:
        h = (g - b) / d + (g < b ? 6 : 0);
        break;
      case g:
        h = (b - r) / d + 2;
        break;
      case b:
        h = (r - g) / d + 4;
        break;
    }
    h /= 6;
  }
  return [h * 360, s * 100, l * 100];
}

// Convert hsl (360, 100, 100) to rgb.
export function hslToRgb(h, s, l) {
  s /= 100;
  l /= 100;

  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = l - c / 2;

  let r = 0,
    g = 0,
    b = 0;

  if (0 <= h && h < 60) {
    r = c;
    g = x;
    b = 0;
  } else if (60 <= h && h < 120) {
    r = x;
    g = c;
    b = 0;
  } else if (120 <= h && h < 180) {
    r = 0;
    g = c;
    b = x;
  } else if (180 <= h && h < 240) {
    r = 0;
    g = x;
    b = c;
  } else if (240 <= h && h < 300) {
    r = x;
    g = 0;
    b = c;
  } else if (300 <= h && h < 360) {
    r = c;
    g = 0;
    b = x;
  }

  r = Math.round((r + m) * 255);
  g = Math.round((g + m) * 255);
  b = Math.round((b + m) * 255);

  return [r, g, b];
}
