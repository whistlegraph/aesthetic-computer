// Starfield, 2021
// A classic starfield effect.

let starfield;
let nowipe = false;
const { tan, floor, random } = Math;

export function boot($, options) {
  starfield = new Starfield($, options?.stars);
  if (options?.speed) starfield.speed = options.speed;
}

// 🧮 Sim(ulate)
export function sim($) {
  starfield.update($);
}

// 🎨 Paint
// `backdrop: false` plots only the stars, over whatever is already there.
export function paint($, options) {
  if (options?.backdrop !== false) {
    if (!nowipe) $.wipe(0);
    if (nowipe) $.ink(0, 32).box(0, 0, $.screen.width, $.screen.height);
  }
  starfield.paint($, options);
}

export function wipe(state) {
  nowipe = !state;
}

// 📚 Library

class Starfield {
  numStars = 128;
  spread = 40;
  speed = 2.1;

  stars = {
    x: Array(this.numStars),
    y: Array(this.numStars),
    z: Array(this.numStars),
  };

  constructor($, numStars) {
    this.numStars = numStars || this.numStars;
    for (let i = 0; i < this.numStars; i += 1) {
      this.reset(i);
    }
  }

  update($) {
    for (let i = 0; i < this.numStars; i += 1) {
      this.stars.z[i] -= 0.01 * this.speed;

      if (this.stars.z[i] <= 0) {
        this.reset(i);
      }

      const p = this.projection($, i);
      const x = p[0];
      const y = p[1];

      if (x < 0 || x >= $.screen.width || y < 0 || y >= $.screen.height) {
        this.reset(i);
      }
    }
  }

  // `bold: true` draws the nearer third of the stars as 2x2 blocks.
  paint($, options) {
    const r = $.num.randInt;
    for (let i = 0; i < this.numStars; i += 1) {
      $.ink(
        options?.color || [r(255), r(255), r(255)],
        floor((options?.alpha || 1) * 255),
      );
      const [x, y] = this.projection($, i);
      if (options?.bold && this.stars.z[i] < this.spread / 3) $.box(x, y, 2, 2);
      else $.plot(x, y);
    }
  }

  reset(i) {
    this.stars.x[i] = 2 * (random() - 0.5) * this.spread;
    this.stars.y[i] = 2 * (random() - 0.5) * this.spread;
    this.stars.z[i] = (random() + 0.00001) * this.spread;
  }

  projection($, i) {
    const fov = 105;
    const tanHalfFov = tan($.num.radians(fov / 2));

    const halfWidth = $.screen.width / 2;
    const halfHeight = $.screen.height / 2;
    return [
      floor(
        (this.stars.x[i] / (this.stars.z[i] * tanHalfFov)) * halfWidth +
          halfWidth,
      ),
      floor(
        (this.stars.y[i] / (this.stars.z[i] * tanHalfFov)) * halfHeight +
          halfHeight,
      ),
    ];
  }
}
