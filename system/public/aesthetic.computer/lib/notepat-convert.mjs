// Utility helpers for converting between notepat keyboard notation and
// the extended melody syntax used by disks like clock.
//
// Exported separately so we can share logic between runtime and tests.

export function convertNotepatNotation(melodyString) {
  if (!melodyString) return melodyString;

  // Create a character-by-character conversion
  let result = "";
  let i = 0;
  let currentOctave = 4; // Track the current base octave for conversion
  let braceDepth = 0; // Track when we're inside { ... }
  let inQuote = false; // Track when we're inside " ... "

  while (i < melodyString.length) {
    const char = melodyString[i];

    // Toggle quote state and pass through characters inside speech unchanged
    if (char === '"') {
      inQuote = !inQuote;
      result += char;
      i++;
      continue;
    }

    // Track brace depth (waveform/volume/Hz specifiers) and pass through contents unchanged
    if (!inQuote && char === '{') {
      braceDepth += 1;
      result += char;
      i++;
      continue;
    }

    if (!inQuote && braceDepth > 0 && char === '}') {
      braceDepth = Math.max(0, braceDepth - 1);
      result += char;
      i++;
      continue;
    }

    // If we're inside a brace block or quoted string, leave the content untouched
    if (braceDepth > 0 || inQuote) {
      result += char;
      i++;
      continue;
    }

    // Check if this is an octave number that we should track
    if (/[0-9]/.test(char)) {
      currentOctave = parseInt(char);
      result += char;
    }
    // First octave sharps (vswrq → cs ds fs gs as)
    else if (char === 'v') {
      result += `${currentOctave}cs`;
    } else if (char === 's') {
      result += `${currentOctave}ds`;
    } else if (char === 'w') {
      result += `${currentOctave}fs`;
    } else if (char === 'r') {
      result += `${currentOctave}gs`;
    } else if (char === 'q') {
      result += `${currentOctave}as`;
    }
    // Second octave white keys (hijklmn → next octave c d e f g a b)
    else if (char === 'h') {
      result += `${currentOctave + 1}c`;
    } else if (char === 'i') {
      result += `${currentOctave + 1}d`;
    } else if (char === 'j') {
      result += `${currentOctave + 1}e`;
    } else if (char === 'k') {
      result += `${currentOctave + 1}f`;
    } else if (char === 'l') {
      result += `${currentOctave + 1}g`;
    } else if (char === 'm') {
      result += `${currentOctave + 1}a`;
    } else if (char === 'n') {
      result += `${currentOctave + 1}b`;
    }
    // Second octave sharps (tyuop → next octave cs ds fs gs as)
    else if (char === 't') {
      result += `${currentOctave + 1}cs`;
    } else if (char === 'y') {
      result += `${currentOctave + 1}ds`;
    } else if (char === 'u') {
      result += `${currentOctave + 1}fs`;
    } else if (char === 'o') {
      result += `${currentOctave + 1}gs`;
    } else if (char === 'p') {
      result += `${currentOctave + 1}as`;
    }
    // All other characters pass through unchanged
    else {
      result += char;
    }

    i++;
  }

  return result;
}
