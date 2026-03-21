// Chord Detection for Western Music
// Identifies chord names from a set of active notes

// Note name to semitone offset from C (chromatic scale)
const NOTE_TO_SEMITONE = {
  c: 0,
  "c#": 1,
  db: 1,
  d: 2,
  "d#": 3,
  eb: 3,
  e: 4,
  f: 5,
  "f#": 6,
  gb: 6,
  g: 7,
  "g#": 8,
  ab: 8,
  a: 9,
  "a#": 10,
  bb: 10,
  b: 11,
};

const SEMITONE_TO_NOTE = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];

// Chord interval patterns (relative to root, in semitones)
// Each pattern maps to [intervals, suffix]
const CHORD_PATTERNS = [
  // Triads
  { intervals: [0, 4, 7], name: "", type: "major" },          // Major: C
  { intervals: [0, 3, 7], name: "m", type: "minor" },         // Minor: Cm
  { intervals: [0, 3, 6], name: "dim", type: "diminished" },  // Diminished: Cdim
  { intervals: [0, 4, 8], name: "aug", type: "augmented" },   // Augmented: Caug
  { intervals: [0, 5, 7], name: "sus4", type: "suspended" },  // Sus4: Csus4
  { intervals: [0, 2, 7], name: "sus2", type: "suspended" },  // Sus2: Csus2
  
  // Seventh chords
  { intervals: [0, 4, 7, 11], name: "maj7", type: "major7" },     // Major 7: Cmaj7
  { intervals: [0, 4, 7, 10], name: "7", type: "dominant7" },     // Dominant 7: C7
  { intervals: [0, 3, 7, 10], name: "m7", type: "minor7" },       // Minor 7: Cm7
  { intervals: [0, 3, 6, 10], name: "m7b5", type: "half-dim" },   // Half-diminished: Cm7b5
  { intervals: [0, 3, 6, 9], name: "dim7", type: "diminished7" }, // Diminished 7: Cdim7
  { intervals: [0, 4, 8, 10], name: "7#5", type: "augmented7" },  // Augmented 7: C7#5
  { intervals: [0, 3, 7, 11], name: "mMaj7", type: "minor-major7" }, // Minor-major 7
  
  // Extended chords (common voicings)
  { intervals: [0, 4, 7, 10, 14], name: "9", type: "dominant9" },      // Dominant 9
  { intervals: [0, 3, 7, 10, 14], name: "m9", type: "minor9" },        // Minor 9
  { intervals: [0, 4, 7, 11, 14], name: "maj9", type: "major9" },      // Major 9
  
  // Add chords
  { intervals: [0, 4, 7, 14], name: "add9", type: "add9" },   // Add 9
  { intervals: [0, 3, 7, 14], name: "madd9", type: "minor-add9" }, // Minor add 9
  { intervals: [0, 4, 7, 9], name: "6", type: "major6" },     // Major 6
  { intervals: [0, 3, 7, 9], name: "m6", type: "minor6" },    // Minor 6
  
  // Power chord
  { intervals: [0, 7], name: "5", type: "power" },            // Power chord: C5
];

/**
 * Normalize a note name to lowercase base note with optional sharp
 * Handles formats like: "C", "c", "C#", "Db", "+c", "+c#", "++d", "-a#"
 */
export function normalizeNote(note) {
  if (!note || typeof note !== "string") return null;
  
  // Remove octave prefix indicators (+, -, ++)
  let clean = note.toLowerCase().replace(/^[+\-]+/, "");
  
  // Convert flats to sharps for consistency
  if (clean.length === 2 && clean[1] === "b") {
    const flatToSharp = {
      db: "c#",
      eb: "d#",
      gb: "f#",
      ab: "g#",
      bb: "a#",
    };
    clean = flatToSharp[clean] || clean;
  }
  
  return clean;
}

/**
 * Get semitone value (0-11) for a note
 */
export function noteToSemitone(note) {
  const normalized = normalizeNote(note);
  return normalized ? NOTE_TO_SEMITONE[normalized] : null;
}

/**
 * Get pitch classes (0-11) from an array of note names
 * Removes duplicates (octave equivalents)
 */
export function notesToPitchClasses(notes) {
  const pitchClasses = new Set();
  for (const note of notes) {
    const semitone = noteToSemitone(note);
    if (semitone !== null) {
      pitchClasses.add(semitone);
    }
  }
  return Array.from(pitchClasses).sort((a, b) => a - b);
}

/**
 * Check if a set of intervals matches a chord pattern
 */
function matchesPattern(intervals, pattern) {
  if (intervals.length !== pattern.length) return false;
  for (let i = 0; i < intervals.length; i++) {
    if (intervals[i] !== pattern[i]) return false;
  }
  return true;
}

/**
 * Get intervals relative to a root note
 */
function getIntervalsFromRoot(pitchClasses, root) {
  return pitchClasses.map(pc => (pc - root + 12) % 12).sort((a, b) => a - b);
}

/**
 * Detect chord from an array of note names
 * Returns { name: "Cmaj7", root: "C", type: "major7", notes: [...] } or null
 */
export function detectChord(notes) {
  if (!notes || !Array.isArray(notes) || notes.length < 2) return null;
  
  const pitchClasses = notesToPitchClasses(notes);
  if (pitchClasses.length < 2) return null;
  
  // Try each pitch class as a potential root
  let bestMatch = null;
  let bestPriority = Infinity;
  
  for (const root of pitchClasses) {
    const intervals = getIntervalsFromRoot(pitchClasses, root);
    
    // Check against all chord patterns
    for (let i = 0; i < CHORD_PATTERNS.length; i++) {
      const pattern = CHORD_PATTERNS[i];
      if (matchesPattern(intervals, pattern.intervals)) {
        // Prefer the first matching pattern (they're ordered by preference)
        // Also prefer when the lowest note is the root
        const isRootLowest = pitchClasses[0] === root;
        const priority = i + (isRootLowest ? 0 : 100);
        
        if (priority < bestPriority) {
          bestPriority = priority;
          bestMatch = {
            name: SEMITONE_TO_NOTE[root] + pattern.name,
            root: SEMITONE_TO_NOTE[root],
            suffix: pattern.name,
            type: pattern.type,
            notes: notes,
          };
        }
      }
    }
  }
  
  return bestMatch;
}

/**
 * Get a short chord name suitable for small displays
 * E.g., "Cmaj7" -> "Cmaj7", but could be abbreviated if needed
 */
export function getShortChordName(chord) {
  if (!chord) return null;
  return chord.name;
}

/**
 * Get a formatted chord name with proper typography
 * Uses unicode characters for better display
 */
export function getFormattedChordName(chord) {
  if (!chord) return null;
  
  let name = chord.name;
  
  // Replace common suffixes with proper formatting
  // Note: This could use unicode characters like ° for dim, + for aug
  // but we'll keep it ASCII-friendly for now
  
  return name;
}

// Export for testing
export const _internals = {
  NOTE_TO_SEMITONE,
  SEMITONE_TO_NOTE,
  CHORD_PATTERNS,
  matchesPattern,
  getIntervalsFromRoot,
};
