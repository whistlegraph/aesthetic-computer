// Shared note color helpers for music pieces (clock, notepat, abc123)

// Base octave (4): Standard Crayola/CSS/ANSI-style colors
const NOTE_COLOR_MAP_BASE = {
  c: [205, 38, 38],    // Firebrick red (Crayola Red)
  d: [255, 140, 0],    // Dark orange (CSS)
  e: [255, 215, 0],    // Gold (CSS)
  f: [34, 139, 34],    // Forest green (CSS)
  g: [30, 90, 180],    // Steel blue
  a: [75, 0, 130],     // Indigo (CSS)
  b: [138, 43, 226],   // Blue violet (CSS)
};

// Upper octave (+1): Dayglo/neon palette
const NOTE_COLOR_MAP_DAYGLO = {
  c: [255, 65, 85],    // Neon red/coral
  d: [255, 165, 50],   // Neon orange
  e: [255, 255, 80],   // Neon yellow
  f: [65, 255, 120],   // Neon green
  g: [80, 180, 255],   // Neon blue
  a: [160, 80, 255],   // Neon purple
  b: [220, 100, 255],  // Neon violet/magenta
};

// Lower octave (-1): Muted/darker tones
const NOTE_COLOR_MAP_MUTED = {
  c: [139, 26, 26],    // Dark red
  d: [180, 100, 0],    // Brown orange
  e: [180, 150, 0],    // Dark gold
  f: [20, 90, 20],     // Dark green
  g: [20, 60, 120],    // Dark blue
  a: [50, 0, 90],      // Dark indigo
  b: [90, 30, 150],    // Dark violet
};

// Sharps/flats (black keys) - all black
const NOTE_COLOR_BLACK = [0, 0, 0];

// Legacy map for backwards compatibility
const NOTE_COLOR_MAP = {
  ...NOTE_COLOR_MAP_BASE,
  // Sharps/flats are all black
  "c#": NOTE_COLOR_BLACK,
  cs: NOTE_COLOR_BLACK,
  db: NOTE_COLOR_BLACK,
  "d#": NOTE_COLOR_BLACK,
  ds: NOTE_COLOR_BLACK,
  eb: NOTE_COLOR_BLACK,
  "f#": NOTE_COLOR_BLACK,
  fs: NOTE_COLOR_BLACK,
  gb: NOTE_COLOR_BLACK,
  "g#": NOTE_COLOR_BLACK,
  gs: NOTE_COLOR_BLACK,
  ab: NOTE_COLOR_BLACK,
  "a#": NOTE_COLOR_BLACK,
  as: NOTE_COLOR_BLACK,
  bb: NOTE_COLOR_BLACK,
};

function clamp(value, min = 0, max = 255) {
  return Math.max(min, Math.min(max, value));
}

export function normalizeNoteName(noteName) {
  if (!noteName) return null;
  if (noteName === "rest") return null;
  if (noteName === "speech") return "speech";
  const clean = noteName.toLowerCase().replace(/[0-9]/g, "");
  return clean;
}

export function isSharpOrFlat(noteName) {
  if (!noteName) return false;
  const clean = normalizeNoteName(noteName);
  return clean && (clean.includes("#") || clean.length === 2);
}

export function getNoteColor(noteName) {
  if (!noteName || noteName === "rest") return [102, 102, 102];
  if (noteName === "speech") return [255, 200, 100];
  const cleanNote = normalizeNoteName(noteName);
  return NOTE_COLOR_MAP[cleanNote] || [255, 255, 255];
}

export function getNoteColorForOctave(noteName, octave, baseOctave = 4) {
  if (!noteName || noteName === "rest") return [102, 102, 102];
  if (noteName === "speech") return [255, 200, 100];
  
  const cleanNote = normalizeNoteName(noteName);
  if (!cleanNote) return [255, 255, 255];
  
  // Sharps/flats are always black regardless of octave
  if (isSharpOrFlat(cleanNote)) {
    return NOTE_COLOR_BLACK;
  }
  
  // Get base note (first character)
  const baseNote = cleanNote[0];
  
  const delta = octave - baseOctave;
  
  if (delta >= 1) {
    // Upper octave: dayglo palette
    return NOTE_COLOR_MAP_DAYGLO[baseNote] || NOTE_COLOR_MAP_BASE[baseNote] || [255, 255, 255];
  } else if (delta <= -1) {
    // Lower octave: muted palette
    return NOTE_COLOR_MAP_MUTED[baseNote] || NOTE_COLOR_MAP_BASE[baseNote] || [255, 255, 255];
  } else {
    // Base octave: standard palette
    return NOTE_COLOR_MAP_BASE[baseNote] || [255, 255, 255];
  }
}

export function applyOctaveAccent(rgb, octave, { baseOctave = 4, step = 25 } = {}) {
  if (!Array.isArray(rgb) || rgb.length < 3 || !Number.isFinite(octave)) {
    return rgb;
  }
  const delta = octave - baseOctave;
  if (delta === 0) return rgb;
  const adjust = step * delta;
  return [
    clamp(rgb[0] + adjust),
    clamp(rgb[1] + adjust),
    clamp(rgb[2] + adjust),
  ];
}

export function getNoteColorWithOctave(
  noteName,
  octave,
  { baseOctave = 4, accentStep = 25 } = {},
) {
  // Use the new octave-specific palettes instead of just adjusting brightness
  return getNoteColorForOctave(noteName, octave, baseOctave);
}

export function toRgbCss(rgb) {
  if (!Array.isArray(rgb) || rgb.length < 3) return "rgb(255, 255, 255)";
  return `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
}

export function parseNotepatNote(note, baseOctave = 4) {
  if (!note) return { noteName: null, octave: baseOctave };
  let octave = Number.isFinite(baseOctave) ? baseOctave : 4;
  let noteName = note;

  if (noteName.startsWith("++")) {
    octave += 2;
    noteName = noteName.slice(2);
  } else if (noteName.startsWith("+")) {
    octave += 1;
    noteName = noteName.slice(1);
  } else if (noteName.startsWith("-")) {
    octave -= 1;
    noteName = noteName.slice(1);
  }

  return { noteName, octave };
}

export { NOTE_COLOR_MAP, NOTE_COLOR_MAP_BASE, NOTE_COLOR_MAP_DAYGLO, NOTE_COLOR_MAP_MUTED, NOTE_COLOR_BLACK, isSharpOrFlat as isBlackKey };
