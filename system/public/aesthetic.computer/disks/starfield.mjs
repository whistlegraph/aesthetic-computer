let width, height;

export function boot ($) {
  $.resize({gap: 0});
}

// 🧮 Sim(ulate)
export function sim({ screen }) {
  ({ width, height } = screen);
  starfield.update();
}

// 🎨 Paint
export function paint($) {
  $.wipe(0);
  starfield.paint($);
}

// 📚 Library

// TODO: Use radians from num. 2022.01.17.02.37
function radians(deg) {
  return deg * (Math.PI / 180);
}

class Starfield {
  numStars = 128;
  spread = 40;
  speed = 2.1;

  stars = {
    x: Array(this.numStars),
    y: Array(this.numStars),
    z: Array(this.numStars),
  };

  constructor() {
    for (let i = 0; i < this.numStars; i += 1) {
      this.reset(i);
    }
  }

  update() {
    for (let i = 0; i < this.numStars; i += 1) {
      this.stars.z[i] -= 0.01 * this.speed;

      if (this.stars.z[i] <= 0) {
        this.reset(i);
      }

      const p = this.projection(i);
      const x = p[0];
      const y = p[1];

      if (x < 0 || x >= width || y < 0 || y >= height) {
        this.reset(i);
      }
    }
  }

  paint({ ink, num: { randInt: r }, plot }) {
    for (let i = 0; i < this.numStars; i += 1) {
      ink(r(255), r(255), r(255)).plot(...this.projection(i));
    }
  }

  reset(i) {
    this.stars.x[i] = 2 * (Math.random() - 0.5) * this.spread;
    this.stars.y[i] = 2 * (Math.random() - 0.5) * this.spread;
    this.stars.z[i] = (Math.random() + 0.00001) * this.spread;
  }

  projection(i) {
    const fov = 105;
    const tanHalfFov = Math.tan(radians(fov / 2));

    const halfWidth = width / 2;
    const halfHeight = height / 2;
    return [
      Math.floor(
        (this.stars.x[i] / (this.stars.z[i] * tanHalfFov)) * halfWidth +
          halfWidth
      ),
      Math.floor(
        (this.stars.y[i] / (this.stars.z[i] * tanHalfFov)) * halfHeight +
          halfHeight
      ),
    ];
  }
}

const starfield = new Starfield();

// 💗 Beat
//export function beat($api) {
//  const { num, help, sound } = $api;
//}