// Boxes, 22.08.25.10.54
// A test pattern for the `box` primitive.
// Also eventually to be used for prototyping `blend`.

// TODO: Eliminate / remove the flicker that occurs on resize.
// TODO: Add `blend` API to graph, where all ink colors lerp towards a
//       particular value. 22.08.29.13.17

const { floor, ceil } = Math;
const maxZoom = 32;
let zooms = 0;
let zoomStep = 1;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize }) {
  resize(3);
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim({ simCount, resize, screen }) {
  // TODO: Move a ball here!
  if (simCount % 8n === 0n) {
    resize(screen.width + zoomStep);
    zooms += 1;
    if (zooms > maxZoom) {
      zooms = 0;
      zoomStep *= -1;
    }
  }
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, screen: { width: w, height: h } }) {
  wipe(128); // Grey background.

  // Three 1 pixel boxes.
  //ink(`red`).box(0, 0, 1);
  //ink(`green`).box(1, 1, 1);
  //ink(`blue`).box(2, 2, 1);

  // Lerps this to whatever the ink color is before drawing
  // blend([255, 255, 255], 0.75);

  // Paint a box at the center of the display, rendering from its
  // center point.
  ink(`red`).box(1, 1, w - 2, "fill");
  ink(255, 255, 255, 128).box(w / 2, h / 2, w - 2, "fill*center");
  ink(`blue`).plot(w / 2, h / 2); // Draw a dot at the center of display.

  return true; // Only once.
}

// ✒ Act (Runs once per user interaction)
function act({ event }) {}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat };
