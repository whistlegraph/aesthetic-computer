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
  var grad3 = [[1,1,0],[-1,1,0],[1,-1,0],[-1,-1,0],[1,0,1],[-1,0,1],[1,0,-1],[-1,0,-1],[0,1,1],[0,-1,1],[0,1,-1],[0,-1,-1]];
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
  if (x0 > y0) { i1 = 1; j1 = 0; }
  else { i1 = 0; j1 = 1; }

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
    ${d.getMonth() + 1}.
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

    if (name in cssColors) {
      return [...cssColors[name], alpha];
    } else if (name === "erase") {
      return [-1, -1, -1, alpha];
    } else if (name === "rainbow") {
      return ["rainbow", alpha]; // Cycle through the "rainbow" colors here.
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

// Convert rgb to hsl.
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
