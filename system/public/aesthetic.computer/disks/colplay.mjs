// Colplay, 2023.10.09.11.26.39.362
// Use a painting as a tonal keyboard.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add multi-touch support.
#endregion */

const clones = {};
let lastColor;
const { keys } = Object;

// 🎨 Paint
function paint({ paste, system }) {
  const np = system.nopaint;
  keys(clones).forEach((k) => {
    paste(clones[k], np.translation.x, np.translation.y, np.zoomLevel);
  });
}

// 🎪 Act
function act({
  event: e,
  system,
  pixel,
  flood,
  page,
  screen,
  delay,
  clonePixels,
  needsPaint,
  colorsMatch,
  sound,
  num: { rgbToHsl, map },
}) {
  if (e.is("lift")) lastColor = null;

  if (e.is("touch") || e.is("draw")) {
    //  Get the color pixel from the painting under the cursor.
    const { x, y } = system.nopaint.pointToPainting({ system, pen: e });

    const color = pixel(x, y, system.painting);
    // Make sure the pixel isn't transparent.
    // And the color is different from lastColor.
    if (color[3] > 0 && (!lastColor || !colorsMatch(lastColor, color))) {
      lastColor = color;
      const clone = clonePixels(system.painting);
      const label = performance.now();
      clones[label] = clone;
      page(clone);
      flood(x, y, [255, 255, 255, 127]);
      page(screen);
      const hsl = rgbToHsl(...color);
      const pan = 1 - (x / screen.width) * 2;

      console.log("Pan:", pan);
      console.log("HSL:", hsl);

      sound.synth({
        type: "square",
        tone: map(hsl[0], 0, 360, 200, 1000),
        attack: 0.001,
        decay: 0.98,
        volume: 0.75, // + hsl[2] / 100 / 2,
        pan,
        duration: 0.5,
      });

      delay(() => {
        delete clones[label];
        needsPaint();
      }, 12);
    }
  }
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Colplay",
    desc: "Use a painting as a tonal keyboard.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export const system = "nopaint";
export { paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
