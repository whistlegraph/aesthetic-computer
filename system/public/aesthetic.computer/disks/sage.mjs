// Sage, 2022.01.31.19.14 [Sage @mxsage + Jeffrey]
// A basic demo of a little walker.

// TODO: Bake `wrap` and `pixel` into the api. 2022.02.01.02.46 (JAS)
// TODO: Implement something via `sim` and be able to change the framerate.

const { pow, floor, sin, cos, random } = Math;
const { assign } = Object;

let loc,
  lastLoc = {};
let theta = 0;
const threshold = 100;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize, fps, wipe, screen, cursor }) {
  // fps(30); // Slow things down if you need to.
  // resize(64, 64);
  // cursor("none");
  wipe(0, 0, 0);
  loc = { x: screen.width / 2, y: screen.height / 2 };
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ ink, line, paintCount, screen }) {
  const selfColor = pixel(loc, screen)[0] / threshold + 1;

  theta += random() - 0.5; // Choose a random angle.

  // Rotate and move 🐢 by theta, scaled by underlying color.
  loc.x += cos(theta) * selfColor;
  loc.y += sin(theta) * selfColor;

  loc = wrap(loc, screen.width, screen.height); // Wrap 🐢 to the screen.
  assign(lastLoc, loc);

  ink(25 + sin(0.01 * paintCount) * 128 + 128); // Oscillate color over time.

  if (pow(lastLoc.x - loc.x, 2) + pow(lastLoc.y - loc.y, 2) < threshold) {
    line(lastLoc.x, lastLoc.y, loc.x, loc.y);
  }
}

// ✒ Act (Runs once per user interaction)
// function act({ event: e }) {
// if (e.is("draw")) {}
// }

// 💗 Beat (Runs once per bpm)
// function beat($api) { /* Play a sound here! */ }

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
// function sim($api) { /* Simulate something here! */ }

export { boot, paint };

// 📚 Library (Useful classes & functions used throughout the piece)
function wrap({ x, y }, w, h) {
  x = (x / w - floor(x / w)) * w;
  y = (y / h - floor(y / h)) * h;
  return { x, y };
}

function pixel({ x, y }, screen) {
  const i = (screen.width * floor(y) + floor(x)) * 4;
  return screen.pixels.subarray(i, i + 4);
}
