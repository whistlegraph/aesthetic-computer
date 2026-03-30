// hash-color, 2025.3.30
// Per-character color identity for git commit hashes.
// Each hex digit (0-f) maps to a distinct hue, producing a
// colorful "fingerprint" that makes hashes visually recognizable.

// 16 hand-tuned colors for hex digits 0-f, ensuring good contrast
// on both dark and light backgrounds.
const HEX_COLORS = {
  "0": [255, 100, 100], // red
  "1": [255, 160, 80],  // orange
  "2": [255, 210, 70],  // gold
  "3": [200, 230, 80],  // lime
  "4": [100, 220, 100], // green
  "5": [80, 210, 180],  // teal
  "6": [80, 200, 230],  // sky
  "7": [100, 160, 255], // blue
  "8": [140, 130, 255], // indigo
  "9": [180, 110, 255], // violet
  a: [220, 100, 240],   // purple
  b: [255, 100, 200],   // magenta
  c: [255, 130, 150],   // rose
  d: [200, 180, 140],   // tan
  e: [160, 210, 200],   // sage
  f: [220, 200, 255],   // lavender
};

// Returns a color-code string for MatrixChunky8/write: \r,g,b\char
// e.g. "a1b" → "\220,100,240\a\255,160,80\1\255,100,200\b"
function colorizeHash(hash) {
  let out = "";
  for (const ch of hash) {
    const c = HEX_COLORS[ch.toLowerCase()];
    if (c) {
      out += `\\${c[0]},${c[1]},${c[2]}\\${ch}`;
    } else {
      out += ch;
    }
  }
  return out;
}

// Returns the RGB array for a single hex digit.
function hashCharColor(ch) {
  return HEX_COLORS[ch.toLowerCase()] || [200, 200, 200];
}

export { colorizeHash, hashCharColor, HEX_COLORS };
