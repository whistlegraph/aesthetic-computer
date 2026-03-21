// 🪝 Spline, 2022.01.24.02.41
// An, interactive line algorithm with curves.

// TODO: Use `spline` within `nail`.

import { Mark } from "../lib/gesture.mjs";
const { min, max } = Math;

let mark,
  freq = 10,
  scale;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ cursor }) {
  cursor("tiny");
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, point, pan, unpan, help, screen }) {
  wipe(128); // Paint a backdrop.

  // Generate a little periodic data sample.
  const points = [];
  scale = { x: screen.width, y: screen.height / 8 };

  help.repeat(freq, (i) => {
    points.push({
      x: (i / freq) * scale.x,
      y: (i % 2 === 0 ? 1 : -1) * scale.y,
    });
  });

  mark = new Mark(); // Make a mark and add some test points.
  mark.input(points);

  pan(0, screen.height / 2);
  ink(255, 100).poly(mark.spline()); // Draw all curve points.

  ink(255, 0, 0, 100);
  points.forEach((p) => point(p)); // Draw every sample point.
  unpan();

  return false; // Freeze until `needsPaint()` is called elsewhere.
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, needsPaint }) {
  if (e.is("move")) {
    freq = min(max(1, freq + e.delta.x / 4), 100);
    needsPaint();
  }
}

// 📚 Library (Useful functions used throughout the program)

export { boot, paint, act };

// ♻️ Recycle Bin

/**
 * Extract the necessary fields from the event object to produce a point sample.
 * @param e
 * @returns {{x, y, pressure}}
 */
// function point(e) {
//  return (({ x, y, pressure }) => ({ x, y, pressure }))(e);
//}

// 💗 Beat (Runs once per bpm)
// function beat($api) {}

// 🧮 Simulate (Runs once per logic frame (120fps)).
// function sim($api) {}
