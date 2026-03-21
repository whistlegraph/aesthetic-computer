// ABC123 - Shared library for alphabet and number learning pieces
// Coordinates visual themes, musical notes, and speech across all 36 pieces

import {
  getNoteColorWithOctave,
  parseNotepatNote,
} from "./note-colors.mjs";

// 🎹 Musical mapping based on notepat QWERTY layout
// Letters a-g are natural notes, h-n are second octave, others are sharps
export const letterToNote = {
  a: "a",    b: "b",    c: "c",    d: "d",    e: "e",    f: "f",    g: "g",
  h: "+c",   i: "+d",   j: "+e",   k: "+f",   l: "+g",   m: "+a",   n: "+b",
  o: "+g#",  p: "+a#",  q: "a#",   r: "g#",   s: "d#",   t: "+c#",
  u: "+f#",  v: "c#",   w: "f#",   x: null,   y: "+d#",  z: null,
};

// 🔢 Number to note mapping (scale degrees)
export const numberToNote = {
  0: null,   // Rest/silence
  1: "c",    // Root
  2: "d",    // Second
  3: "e",    // Third
  4: "f",    // Fourth
  5: "g",    // Fifth
  6: "a",    // Sixth
  7: "b",    // Seventh
  8: "+c",   // Octave
  9: "+d",   // Ninth
};

// 🎨 Letter themes with colors, emojis, and words
export const letterThemes = {
  a: { color: [220, 60, 60], emoji: "🍎", words: ["apple", "ant", "airplane", "alligator", "astronaut"] },
  b: { color: [65, 105, 225], emoji: "🏀", words: ["ball", "bear", "banana", "butterfly", "boat"] },
  c: { color: [255, 140, 0], emoji: "🐱", words: ["cat", "car", "cake", "cloud", "cow"] },
  d: { color: [138, 43, 226], emoji: "🐕", words: ["dog", "duck", "dinosaur", "door", "drum"] },
  e: { color: [34, 139, 34], emoji: "🐘", words: ["elephant", "egg", "ear", "eye", "earth"] },
  f: { color: [255, 20, 147], emoji: "🐟", words: ["fish", "frog", "flower", "fire", "flag"] },
  g: { color: [50, 205, 50], emoji: "🍇", words: ["grapes", "giraffe", "guitar", "grass", "gift"] },
  h: { color: [30, 144, 255], emoji: "🏠", words: ["house", "hat", "heart", "horse", "hand"] },
  i: { color: [255, 182, 193], emoji: "🍦", words: ["ice cream", "igloo", "insect", "island", "iron"] },
  j: { color: [147, 112, 219], emoji: "🪼", words: ["jellyfish", "jungle", "juice", "jump", "jar"] },
  k: { color: [255, 215, 0], emoji: "🪁", words: ["kite", "king", "kangaroo", "key", "kitchen"] },
  l: { color: [255, 127, 80], emoji: "🦁", words: ["lion", "lemon", "leaf", "lamp", "ladder"] },
  m: { color: [192, 192, 192], emoji: "🌙", words: ["moon", "mouse", "milk", "mountain", "music"] },
  n: { color: [139, 90, 43], emoji: "🪹", words: ["nest", "nose", "nut", "night", "nine"] },
  o: { color: [255, 165, 0], emoji: "🍊", words: ["orange", "owl", "ocean", "octopus", "oven"] },
  p: { color: [128, 0, 128], emoji: "🐷", words: ["pig", "pizza", "penguin", "pencil", "piano"] },
  q: { color: [218, 165, 32], emoji: "👑", words: ["queen", "quilt", "question", "quiet", "quack"] },
  r: { color: [220, 20, 60], emoji: "🌈", words: ["rainbow", "rabbit", "rain", "robot", "rocket"] },
  s: { color: [255, 255, 0], emoji: "⭐", words: ["star", "sun", "snake", "spider", "ship"] },
  t: { color: [0, 128, 128], emoji: "🐢", words: ["turtle", "tree", "train", "tiger", "tooth"] },
  u: { color: [75, 0, 130], emoji: "☂️", words: ["umbrella", "unicorn", "up", "under", "ukulele"] },
  v: { color: [238, 130, 238], emoji: "🎻", words: ["violin", "van", "volcano", "vegetable", "vase"] },
  w: { color: [0, 191, 255], emoji: "🐋", words: ["whale", "water", "wind", "window", "wolf"] },
  x: { color: [128, 128, 128], emoji: "🩻", words: ["x-ray", "xylophone", "box", "fox", "mix"] },
  y: { color: [255, 255, 100], emoji: "🪀", words: ["yo-yo", "yellow", "yak", "yarn", "yawn"] },
  z: { color: [64, 64, 64], emoji: "🦓", words: ["zebra", "zoo", "zero", "zipper", "zoom"] },
};

// 🔢 Number themes
export const numberThemes = {
  0: { color: [20, 20, 30], emoji: "🕳️", words: ["zero", "nothing", "empty", "none", "void"], shape: "void" },
  1: { color: [255, 0, 0], emoji: "☝️", words: ["one", "first", "single", "solo", "alone"], shape: "dot" },
  2: { color: [255, 127, 0], emoji: "✌️", words: ["two", "pair", "double", "both", "twins"], shape: "line" },
  3: { color: [255, 255, 0], emoji: "🔺", words: ["three", "triple", "triangle", "trio", "third"], shape: "triangle" },
  4: { color: [0, 255, 0], emoji: "🍀", words: ["four", "square", "quad", "fourth", "quarter"], shape: "square" },
  5: { color: [0, 127, 255], emoji: "🖐️", words: ["five", "hand", "fifth", "star", "pentagon"], shape: "pentagon" },
  6: { color: [75, 0, 130], emoji: "🎲", words: ["six", "dice", "hexagon", "sixth", "cube"], shape: "hexagon" },
  7: { color: [148, 0, 211], emoji: "🌈", words: ["seven", "rainbow", "week", "lucky", "seventh"], shape: "heptagon" },
  8: { color: [255, 20, 147], emoji: "🎱", words: ["eight", "octopus", "spider", "octave", "infinity"], shape: "octagon" },
  9: { color: [0, 255, 255], emoji: "🎳", words: ["nine", "cloud", "ninth", "baseball", "lives"], shape: "nonagon" },
};

function getThemeColorFromNote(note, baseOctave = 4) {
  if (!note) return null;
  const { noteName, octave } = parseNotepatNote(note, baseOctave);
  return getNoteColorWithOctave(noteName, octave, { baseOctave, accentStep: 25 });
}

export function getLetterNoteColor(letter, baseOctave = 4) {
  const note = letterToNote[letter?.toLowerCase?.()];
  return getThemeColorFromNote(note, baseOctave);
}

export function getNumberNoteColor(num, baseOctave = 4) {
  const note = numberToNote[num];
  return getThemeColorFromNote(note, baseOctave);
}

// Note frequencies for synth (octave 4 base)
const NOTE_FREQUENCIES = {
  "c": 261.63, "c#": 277.18, "d": 293.66, "d#": 311.13,
  "e": 329.63, "f": 349.23, "f#": 369.99, "g": 392.00,
  "g#": 415.30, "a": 440.00, "a#": 466.16, "b": 493.88,
  "+c": 523.25, "+c#": 554.37, "+d": 587.33, "+d#": 622.25,
  "+e": 659.25, "+f": 698.46, "+f#": 739.99, "+g": 783.99,
  "+g#": 830.61, "+a": 880.00, "+a#": 932.33, "+b": 987.77,
};

// 🔊 Play the musical note for a letter
export function playLetterSound(letter, sound, opts = {}) {
  const note = letterToNote[letter.toLowerCase()];
  if (!note || !sound?.synth) return null;
  
  const freq = NOTE_FREQUENCIES[note];
  if (!freq) return null;
  
  return sound.synth({
    type: opts.wave || "triangle",
    tone: freq,
    attack: 0.01,
    decay: 0.95,
    duration: opts.duration || 0.5,
    volume: opts.volume || 0.7,
  });
}

// 🔢 Play the musical note for a number
export function playNumberSound(num, sound, opts = {}) {
  const note = numberToNote[num];
  if (!note || !sound?.synth) return null;
  
  const freq = NOTE_FREQUENCIES[note];
  if (!freq) return null;
  
  return sound.synth({
    type: opts.wave || "sine",
    tone: freq,
    attack: 0.01,
    decay: 0.95,
    duration: opts.duration || 0.5,
    volume: opts.volume || 0.7,
  });
}

// 🗣️ Speak the letter and optionally a word
export function speakLetter(letter, speak, includeWord = true) {
  if (!speak) return;
  const theme = letterThemes[letter.toLowerCase()];
  const word = theme?.words?.[0] || "";
  const text = includeWord && word ? `${letter.toUpperCase()}. ${word}` : letter.toUpperCase();
  speak(text);
}

// 🗣️ Speak the number and optionally a word
export function speakNumber(num, speak, includeWord = true) {
  if (!speak) return;
  const theme = numberThemes[num];
  const word = theme?.words?.[0] || "";
  const text = includeWord && word ? `${word}` : `${num}`;
  speak(text);
}

// 🎲 Get a random word for a letter
export function getRandomWord(letter) {
  const theme = letterThemes[letter.toLowerCase()];
  if (!theme?.words?.length) return "";
  return theme.words[Math.floor(Math.random() * theme.words.length)];
}

// 🎲 Get a random word for a number
export function getRandomNumberWord(num) {
  const theme = numberThemes[num];
  if (!theme?.words?.length) return "";
  return theme.words[Math.floor(Math.random() * theme.words.length)];
}

// 🎨 Available fonts for display
export const fonts = [null, "unifont", "MatrixChunky8"]; // null = system default
export const fontNames = ["system", "unifont", "MatrixChunky8"];

// Helper: get contrasting text color for legibility
function getContrastColor(bgColor) {
  // Calculate luminance
  const [r, g, b] = bgColor;
  const lum = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
  return lum > 0.5 ? [20, 20, 30] : [255, 255, 255];
}

// 🎨 Draw a fullscreen letter display showing ALL fonts at native scale with animation
// frame: animation frame counter for wobble effects
export function drawLetter(letter, { wipe, ink, screen, write, text, box }, theme, frame = 0) {
  const th = theme || letterThemes[letter.toLowerCase()];
  if (!th) return;
  const noteColor = getLetterNoteColor(letter);
  const bgColor = noteColor || th.color;

  wipe(bgColor);
  
  const displayChar = letter.toUpperCase();
  const centerX = screen.width / 2;
  const textColor = getContrastColor(bgColor);
  const shadowColor = textColor[0] > 128 ? [255, 255, 255, 40] : [0, 0, 0, 80];
  
  // Calculate vertical spacing - divide screen into sections for each font
  const bottomSpace = 24;
  const sectionHeight = (screen.height - bottomSpace) / fonts.length;
  
  // Draw each font at native scale with animation
  fonts.forEach((font, i) => {
    const baseY = sectionHeight * (i + 0.5);
    // Staggered wave animation - each font bounces at different phase
    const wobble = Math.sin((frame * 0.08) + i * 2.1) * 2;
    const y = baseY + wobble;
    const fontName = fontNames[i];
    
    // Dark/light strip behind for contrast
    const stripAlpha = textColor[0] > 128 ? 30 : 50;
    ink(textColor[0], textColor[1], textColor[2], stripAlpha).box(
      0, baseY - sectionHeight * 0.4,
      screen.width, sectionHeight * 0.8
    );
    
    // Shadow with slight offset
    ink(...shadowColor).write(displayChar, {
      x: centerX + 1,
      y: y + 1,
      center: "xy",
      size: 1, // Native scale
    }, undefined, undefined, false, font);
    
    // Main letter
    ink(...textColor).write(displayChar, {
      x: centerX,
      y: y,
      center: "xy",
      size: 1, // Native scale
    }, undefined, undefined, false, font);
    
    // Font label to the right with subtle animation
    const labelX = centerX + 16 + Math.sin((frame * 0.05) + i) * 1;
    ink(textColor[0], textColor[1], textColor[2], 120).write(fontName, {
      x: labelX,
      y: y,
      size: 1,
    }, undefined, undefined, false, font);
  });
  
  // Word at bottom with emoji
  const word = th.words?.[0] || "";
  const wordText = `${th.emoji} ${word}`;
  const wordY = screen.height - 12;
  
  // Measure text to center it
  const measured = text?.box(wordText, { x: 0, y: wordY }, undefined, 1);
  const wordWidth = measured?.box?.width || 0;
  const wordX = Math.floor((screen.width - wordWidth) / 2);
  
  ink(textColor[0], textColor[1], textColor[2], 200).write(wordText, {
    x: wordX,
    y: wordY,
    size: 1,
  });
}

// 🔢 Draw a fullscreen number display showing ALL fonts at native scale with animation
// frame: animation frame counter for wobble effects
export function drawNumber(num, { wipe, ink, screen, write, box, line, text }, frame = 0) {
  const th = numberThemes[num];
  if (!th) return;
  const noteColor = getNumberNoteColor(num);
  const bgColor = noteColor || th.color;

  wipe(bgColor);
  
  const displayChar = `${num}`;
  const centerX = screen.width / 2;
  const textColor = getContrastColor(bgColor);
  const shadowColor = textColor[0] > 128 ? [255, 255, 255, 40] : [0, 0, 0, 80];
  
  // Draw counting dots at the top with animation
  const dotSize = 4;
  const dotSpacing = dotSize * 2.5;
  const totalWidth = num * dotSpacing - dotSpacing;
  const startX = screen.width / 2 - totalWidth / 2;
  const dotY = 10;
  
  for (let i = 0; i < num; i++) {
    const dotWobble = Math.sin((frame * 0.1) + i * 0.5) * 1;
    ink(textColor[0], textColor[1], textColor[2], 180).box(
      startX + i * dotSpacing - dotSize / 2,
      dotY - dotSize / 2 + dotWobble,
      dotSize,
      dotSize,
      "round"
    );
  }
  
  // Calculate vertical spacing - divide remaining screen into sections for each font
  const topOffset = 24; // Space for dots
  const bottomOffset = 18; // Space for word
  const availableHeight = screen.height - topOffset - bottomOffset;
  const sectionHeight = availableHeight / fonts.length;
  
  // Draw each font at native scale with animation
  fonts.forEach((font, i) => {
    const baseY = topOffset + sectionHeight * (i + 0.5);
    // Staggered wave animation
    const wobble = Math.sin((frame * 0.08) + i * 2.1) * 2;
    const y = baseY + wobble;
    const fontName = fontNames[i];
    
    // Dark/light strip behind for contrast
    const stripAlpha = textColor[0] > 128 ? 30 : 50;
    ink(textColor[0], textColor[1], textColor[2], stripAlpha).box(
      0, baseY - sectionHeight * 0.4,
      screen.width, sectionHeight * 0.8
    );
    
    // Shadow
    ink(...shadowColor).write(displayChar, {
      x: centerX + 1,
      y: y + 1,
      center: "xy",
      size: 1, // Native scale
    }, undefined, undefined, false, font);
    
    // Main number
    ink(...textColor).write(displayChar, {
      x: centerX,
      y: y,
      center: "xy",
      size: 1, // Native scale
    }, undefined, undefined, false, font);
    
    // Font label to the right with animation
    const labelX = centerX + 16 + Math.sin((frame * 0.05) + i) * 1;
    ink(textColor[0], textColor[1], textColor[2], 120).write(fontName, {
      x: labelX,
      y: y,
      size: 1,
    }, undefined, undefined, false, font);
  });
  
  // Word at bottom
  const word = th.words?.[0] || "";
  const wordText = `${th.emoji} ${word}`;
  const wordY = screen.height - 10;
  
  // Measure text to center it
  const measured = text?.box(wordText, { x: 0, y: wordY }, undefined, 1);
  const wordWidth = measured?.box?.width || 0;
  const wordX = Math.floor((screen.width - wordWidth) / 2);
  
  ink(textColor[0], textColor[1], textColor[2], 200).write(wordText, {
    x: wordX,
    y: wordY,
    size: 1,
  });
}

// 🧭 Handle navigation between pieces
export function handleNavigation(e, jump) {
  if (!e || !jump) return false;
  
  // Letter keys jump to letter pieces
  if (e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (/^[a-z]$/.test(key)) {
      jump(key);
      return true;
    }
    if (/^[0-9]$/.test(key)) {
      jump(key);
      return true;
    }
  }
  return false;
}
