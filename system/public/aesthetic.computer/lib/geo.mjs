// 🧮 Geometry

const { abs, cos, sin, floor, sqrt } = Math;
import * as vec4 from "../dep/gl-matrix/vec4.mjs";
import { dist, radians, randIntRange } from "./num.mjs";

// A generic circle model for algorithmic use.
export class Circle {
  x;
  y;
  radius;

  constructor(x, y, radius = 8) {
    this.x = x;
    this.y = y;
    this.radius = radius;
  }

  //  Determines whether the circle is intersecting the given line.
  // function ( line, )

  // Returns a random (x, y) point within the circle by recursively generating
  // random points within a bounding box and checking to see if they are within
  // the radius.
  random() {
    const sq = [-this.radius, this.radius];
    const np = {
      x: this.x + randIntRange(...sq),
      y: this.y + randIntRange(...sq),
    };

    if (dist(this.x, this.y, np.x, np.y) < this.radius) {
      return np;
    } else {
      return this.random(this.radius);
    }
  }
}

// A dynamic box defined by x, y, w, h with methods that mutate the state.
export class Box {
  x = 0;
  y = 0;
  w = 1;
  h = 1;

  // Params:
  // x, y, w, h
  // x, y, s
  // {x, y}, s
  constructor() {
    if (arguments.length === 4) {
      this.x = arguments[0];
      this.y = arguments[1];
      this.w = arguments[2];
      this.h = arguments[3];
    } else if (arguments.length === 3) {
      this.x = arguments[0];
      this.y = arguments[1];
      this.w = arguments[2];
      this.h = this.w;
    } else if (arguments.length === 2) {
      this.x = arguments[0].x;
      this.y = arguments[0].y;
      this.w = arguments[1];
      this.h = this.w;
    }

    if (this.w === 0) this.w = 1;
    if (this.h === 0) this.h = 1;
  }

  // Yields a new box that is a copy of an existing old one.
  static from(box) {
    if (Array.isArray(box)) {
      return new Box(...box);
    } else {
      return new Box(box.x, box.y, box.w || box.width, box.h || box.height);
    }
  }

  // Compute area of box.
  get area() {
    return abs(this.w * this.h);
  }

  // Compute center point of box.
  get center() {
    return {
      x: this.x + this.w / 2,
      y: this.y + this.h / 2,
    };
  }

  // Yields a box where x, y is at the top left and w, h are positive.
  get abs() {
    let { x, y, w, h } = this;

    if (w < 0) {
      x += w + 1;
      w = Math.abs(w);
    }

    if (h < 0) {
      y += h + 1;
      h = Math.abs(h);
    }

    return new Box(x, y, w, h);
  }

  // The top side of the box. (same as y)
  get top() {
    return this.y;
  }

  // Calculates a y value, representing the bottom of the box.
  // Note: Returns y if the height is 1.
  get bottom() {
    return this.h === 1 ? this.y : this.y + this.h;
    //return this.y + this.h;
  }

  // The left side of the box. (same as x)
  get left() {
    return this.x;
  }

  // Calculates an x value, representing the right of the box.
  // Note: Returns x if the width is 1.
  get right() {
    return this.w === 1 ? this.x : this.x + this.w;
  }

  // All four corners.
  get topLeft() {
    return { x: this.left, y: this.top };
  }

  get topRight() {
    return { x: this.right, y: this.top };
  }

  get bottomLeft() {
    return { x: this.left, y: this.bottom };
  }

  get bottomRight() {
    return { x: this.right, y: this.bottom };
  }

  // Scales a box and returns a copy.
  scale(n) {
    return new Box(this.x, this.y, this.w * n, this.h * n);
  }

  // Crops one box to another.
  crop(toX, toY, toW, toH) {
    let { x, y, w, h } = this;

    if (x >= toW || y >= toH) return; // Return `undefined` if x or y is out of bounds.

    // Crop left side.
    if (x < toX) {
      w += x;
      x = toX;
    }
    // Crop right side.
    if (x + w > toW) w = toW - x;
    // Crop top side.
    if (y < toY) {
      h += y;
      y = toY;
    }
    // Crop bottom side.
    if (y + h > toH) h = toH - y;

    return new Box(x, y, w, h);
  }

  // Moves the box by x and y.
  move({ x, y }) {
    this.x += x;
    this.y += y;
  }

  // Returns true if this box contains the point {x, y}.
  contains(point = { x: undefined, y: undefined }) {
    const { x, y } = point;
    return (
      this.x <= x && x <= this.x + this.w && this.y <= y && y <= this.y + this.h
    );
  }

  // Returns true if this box contains NO points in `arr`.
  containsNone(arr) {
    return arr.every((o) => this.contains(o) === false);
  }

  // Returns true if this box contains the point `xy` from `arr`.
  onlyContains(index, arr) {
    // Cut index out of the original arr.
    const newArr = arr.slice(0, index).concat(arr.slice(index + 1));
    // If our index is in the box, but it contains no other points, return true.
    return this.contains(arr[index]) && this.containsNone(newArr);
  }

  // The opposite of contains.
  misses(o) {
    return !this.contains(o);
  }

  // Grow a box from the center by `n` units on every side.
  // And returns a copy.
  grow(n) {
    return new Box(this.x - n, this.y - n, this.w + n * 2, this.h + n * 2);
  }

  // Checks whether two boxes are making contact and / or intersecting.
  // 📓 This might be a little unperforant with tons of boxes. 23.06.29.12.48
  contact(box) {
    return (
      box &&
      (this.contains(box.topLeft) ||
        this.contains(box.topRight) ||
        this.contains(box.bottomRight) ||
        this.contains(box.bottomLeft))
    );
  }

  // Returns true if the boxes match exactly.
  equal(box) {
    return (
      this.x === box.x &&
      this.y === box.y &&
      this.w === box.w &&
      this.h === box.h
    );
  }
}

// High level behavior for points: {x, y} (See also: num.vec2)
export class Point {
  static equals(p1, p2) {
    return p1.x === p2.x && p1.y === p2.y;
  }
}

// A 2 dimensional uniform grid, using a box as the frame (with scaling).
export class Grid {
  box;
  scale;
  // TODO: Could rotation eventually be added here? 2021.12.08.10.51
  transform;

  #halfScale;
  centerOffset;

  constructor(x, y, w, h, s = 1) {
    // Takes the same arguments as box.
    this.box = new Box(x, y, w, h);
    this.scale = s;
    this.transform = { scale: this.scale };
    this.#halfScale = this.scale / 2;
    this.centerOffset = floor(this.#halfScale);
  }

  // Loop through every point in the grid, starting from the top left, and
  // applying a callback.
  each(fun) {
    for (let x = 0; x < this.box.w; x += 1) {
      for (let y = 0; y < this.box.h; y += 1) {
        fun(x, y, x + y * this.box.w); // Send back the coords and an index.
      }
    }
  }

  // Returns unscaled point `{x, y}` in `grid` for given display coordinate
  // `pos`, or `false` if `pos` is outside of `grid`.
  under({ x, y }, cb) {
    const { scale, box } = this;

    // Get original (unscaled) grid position.
    const gx = floor((x - box.x) / scale);
    const gy = floor((y - box.y) / scale);

    // Generate display (x, y) box and grid (gx, gy) position,
    // and whether we are in the grid or not.
    const gridSquare = {
      x: box.x + gx * scale,
      y: box.y + gy * scale,
      w: scale,
      h: scale,
      gx,
      gy,
      in: this.scaled.contains({ x, y }),
    };

    if (gridSquare.in && cb) cb(gridSquare);
    return gridSquare;
  }

  // Returns floored display coordinates from local, untransformed ones.
  get(x, y) {
    return [
      floor(this.box.x + x * this.scale),
      floor(this.box.y + y * this.scale),
    ];
  }

  // Yields the grid's transformed bounding box according to `scale`.
  get scaled() {
    return new Box(
      this.box.x,
      this.box.y,
      this.box.w * this.scale,
      this.box.h * this.scale
    );
  }

  // Yields the projected "center" x, y point of the whole grid.
  middle() {
    return this.get(floor(this.box.w / 2), floor(this.box.h / 2));
  }

  center(x, y) {
    const scaled = this.get(x, y);
    // TODO: This can be replaced with a vec2.add
    scaled[0] += Math.floor(this.#halfScale);
    scaled[1] += Math.floor(this.#halfScale);
    return scaled;
  }

  // Yields an array of offset points that can be plotted to mark the center of
  // each grid square. (Useful for editors, development and debugging.)
  // Tries to find the exact center point, but if that doesn't exist then
  // this function produces a 2x2 grid of pixels in the center.
  get centers() {
    const o = this.centerOffset;

    let points = [];

    // Find exact center point of grid square if possible.
    if (this.#halfScale % 1 === 0.5 && this.#halfScale > 0.5) {
      // We have a perfect middle square.
      points.push({ x: o, y: o });
    } else if (this.scale >= 4) {
      // We can assume we are even here, so we return 4 pixels to mark
      // the center.
      points.push(
        { x: o, y: o },
        { x: o - 1, y: o - 1 },
        { x: o - 1, y: o },
        { x: o, y: o - 1 }
      );
    }
    return points;
  }
}

// This box model uses `soil` to build a dirty rectangle out of points
// in order to optimize rendering.
export class DirtyBox {
  box;
  #left;
  #top;
  #right;
  #bottom;
  soiled = false;

  constructor() {
    this.box = new Box(0, 0, 0); // Note: I probably don't need all the features of `box` here.
  }

  soil({ x, y }) {
    if (this.#left === undefined) {
      this.#left = x;
      this.#right = this.#left;
    }
    if (this.#top === undefined) {
      this.#top = y;
      this.#bottom = this.#top;
    }

    if (x < this.#left) this.#left = x;
    if (y < this.#top) this.#top = y;

    if (x > this.#right) this.#right = x;
    if (y > this.#bottom) this.#bottom = y;

    this.box.x = this.#left;
    this.box.y = this.#top;

    this.box.w = this.#right - this.#left + 1;
    this.box.h = this.#bottom - this.#top + 1;

    this.soiled = true;
  }

  // Crops pixels from an image and returns the new one.
  // - `image` has { width, height, pixels }
  crop(image) {
    const b = this.croppedBox(image);
    const p = image.pixels;
    const newP = new Uint8ClampedArray(b.w * b.h * 4);

    // Copy rows from `p` -> `newP`
    for (let row = 0; row < b.h; row += 1) {
      const index = (b.x + (b.y + row) * image.width) * 4;
      newP.set(p.subarray(index, index + b.w * 4), row * b.w * 4);
    }

    return newP;
  }

  croppedBox(image) {
    return this.box.crop(0, 0, image.width, image.height);
  }
}

// Project a line outwards from an origin point at dist, and degrees to get the other point.
export function linePointsFromAngle(x1, y1, dist, degrees) {
  const x2 = x1 + dist * cos(radians(degrees));
  const y2 = y1 + dist * sin(radians(degrees));
  return [x1, y1, x2, y2];
}

// Project outwards from a point at an `angle` and `dist` and get the resulting point.
export function pointFrom(x, y, angle, dist) {
  return [x + dist * cos(radians(angle)), y + dist * sin(radians(angle))];
}

// Follows a point over time.
// TODO: Also keep track of a target rotation?
// Usage:
// const race = new Race();
export class Race {
  pos;
  step;
  goal;

  last;
  speed;
  dist = 0;

  quantizer;

  constructor(opts = { quantized: true }) {
    this.speed = opts.speed || 20;
    this.step = opts.step || 0.005;
    if (opts.quantized) this.quantizer = new Quantizer({ step: this.step });
  }

  // Should also reset.
  start(point) {
    if (point.length === 2) point.push(0, 0); // Make sure point is a vec4.

    this.quantizer?.start(point);
    this.pos = vec4.clone(point);
    this.goal = vec4.clone(point);
    this.last = vec4.clone(point);
    this.dist = 0;
  }

  to(point = this.goal) {
    if (!point) return; // No goal.
    if (point.length === 2) point.push(0, 0); // Make sure point is a vec4.
    this.goal = point;

    if (!this.pos) return false;
    let out;

    // Quantization ON (regulated segments)
    if (this.quantizer) {
      const newPos = vec4.lerp(
        vec4.create(),
        this.pos,
        point,
        0.01 * this.speed
      );
      out = this.quantizer.to(newPos);
      this.pos = newPos;
    } else {
      // Quantization OFF (longer, less regulated segments)
      const newPos = vec4.lerp(
        vec4.create(),
        this.pos,
        point,
        0.01 * this.speed
      );
      this.dist += vec4.dist(this.pos, newPos);
      this.pos = newPos;

      if (this.dist >= this.step) {
        out = {
          last: vec4.clone(this.last),
          current: this.pos,
          out: [this.last, this.pos],
        };
        this.dist -= this.step; // Hold on for better normalization!
        this.last = this.pos;
      } else {
        out = { last: this.last, current: this.pos };
      }
    }

    return out;
  }
}

// A simple model for lazy following.
// Originally programmed for `3dline`.
// TODO: Generalize the output so only individual points
//       (not line segment vertices with repeated points)
//       can be returned. 22.10.18.11.10
export class Quantizer {
  pos;
  step;
  dist = 0;

  constructor(opts) {
    this.step = opts.step;
  }

  // Returns an array of [lastPoint, nextPoint...]
  to(point) {
    if (!this.pos) return false;

    let out = [];
    this.dist = vec4.dist(this.pos, point);
    let lastPoint = this.pos;

    while (this.dist >= this.step) {
      const nextPoint = vec4.lerp(
        vec4.create(),
        lastPoint,
        point,
        this.step / this.dist
      );
      out.push(lastPoint, nextPoint);
      lastPoint = nextPoint;
      this.dist -= this.step; // Hold on for better normalization!
    }

    this.pos = lastPoint;
    return { last: this.pos, current: point, out };
  }

  start(point) {
    this.pos = vec4.clone(point);
  }
}
