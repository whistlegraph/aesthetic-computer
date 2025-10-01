// Colplay, 2023.10.09.11.26.39.362
// Use a painting as a tonal keyboard.

/* #region 📚 README 
  Maps rainbow/ROYGBIV colors to musical notes (c, d, e, f, g, a, b)
  Paint with rainbow colors then tap to play notes!
#endregion */

/* #region 🏁 TODO 
  - [] Add multi-touch support.
  - [] Add octave shifting.
#endregion */

const clones = {};
let lastColor;
const activeSounds = {}; // Track active sounds for proper release
const { keys } = Object;

// ROYGBIV color mapping to notes (matching clock.mjs and notepat)
const noteColorMap = {
  c: [255, 0, 0],      // Red
  d: [255, 127, 0],    // Orange  
  e: [255, 255, 0],    // Yellow
  f: [0, 255, 0],      // Green
  g: [0, 128, 255],    // Blue
  a: [75, 0, 130],     // Indigo
  b: [148, 0, 211],    // Violet
};

// Musical note frequencies (4th octave)
const noteFrequencies = {
  c: 261.63,  // C4
  d: 293.66,  // D4
  e: 329.63,  // E4
  f: 349.23,  // F4
  g: 392.00,  // G4
  a: 440.00,  // A4
  b: 493.88,  // B4
};

// Find the closest matching note from a color
function findNoteFromColor(rgb) {
  let closestNote = 'c';
  let smallestDiff = Infinity;
  
  for (const [note, noteRgb] of Object.entries(noteColorMap)) {
    // Calculate color distance
    const diff = Math.sqrt(
      Math.pow(rgb[0] - noteRgb[0], 2) +
      Math.pow(rgb[1] - noteRgb[1], 2) +
      Math.pow(rgb[2] - noteRgb[2], 2)
    );
    
    if (diff < smallestDiff) {
      smallestDiff = diff;
      closestNote = note;
    }
  }
  
  return closestNote;
}

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
  pens,
}) {
  // Always kill sounds on any lift event
  if (e.is("lift")) {
    lastColor = null;
    keys(activeSounds).forEach((key) => {
      activeSounds[key]?.kill?.(0.15); // Quick fade out like notepat
      delete activeSounds[key];
    });
  }

  if (e.is("touch") || e.is("draw")) {
    //  Get the color pixel from the painting under the cursor.
    const { x, y } = system.nopaint.pointToPainting({ system, pen: e });

    const color = pixel(x, y, system.painting);
    // Make sure the pixel isn't transparent.
    // And the color is different from lastColor.
    if (color[3] > 0 && (!lastColor || !colorsMatch(lastColor, color))) {
      // Kill previous sound if it exists with quick fade
      if (lastColor) {
        const prevNote = findNoteFromColor(lastColor);
        activeSounds[prevNote]?.kill?.(0.05); // Fast fade when switching notes
        delete activeSounds[prevNote];
      }
      
      lastColor = color;
      const clone = clonePixels(system.painting);
      const label = performance.now();
      clones[label] = clone;
      page(clone);
      flood(x, y, [255, 255, 255, 127]);
      page(screen);
      
      // Find matching note from color
      const note = findNoteFromColor(color);
      const frequency = noteFrequencies[note];
      const pan = 1 - (x / screen.width) * 2;

      console.log("Note:", note, "Frequency:", frequency, "Pan:", pan);

      // Create sound with soft attack but sustains until released (like notepat)
      const snd = sound.synth({
        type: "sine",
        tone: frequency,
        attack: 0.08,      // Soft attack for gentle onset
        decay: 0.9999,     // Very slow decay - sustains while held
        volume: 0.75,      // Slightly lower volume for softer feel
        pan,
        duration: "🔁",    // Sustain until released
      });
      
      activeSounds[note] = snd;

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
    desc: "Rainbow keyboard. Paint with ROYGBIV colors, tap to play notes (c, d, e, f, g, a, b).",
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
