// Shared note color helpers for music pieces (clock, notepat, abc123)

const NOTE_COLOR_MAP = {
  c: [255, 0, 0], // Red
  d: [255, 127, 0], // Orange
  e: [255, 255, 0], // Yellow
  f: [0, 255, 0], // Green
  g: [0, 128, 255], // Blue
  a: [75, 0, 130], // Indigo
  b: [148, 0, 211], // Violet
  "c#": [255, 68, 68],
  cs: [255, 68, 68],
  db: [255, 68, 68],
  "d#": [255, 153, 68],
  ds: [255, 153, 68],
  eb: [255, 153, 68],
  "f#": [68, 255, 68],
  fs: [68, 255, 68],
  gb: [68, 255, 68],
  "g#": [68, 153, 255],
  gs: [68, 153, 255],
  ab: [68, 153, 255],
  "a#": [119, 68, 187],
  as: [119, 68, 187],
  bb: [119, 68, 187],
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

export function getNoteColor(noteName) {
  if (!noteName || noteName === "rest") return [102, 102, 102];
  if (noteName === "speech") return [255, 200, 100];
  const cleanNote = normalizeNoteName(noteName);
  return NOTE_COLOR_MAP[cleanNote] || [255, 255, 255];
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
  const base = getNoteColor(noteName);
  return applyOctaveAccent(base, octave, { baseOctave, step: accentStep });
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

export { NOTE_COLOR_MAP };
