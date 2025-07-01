// Melody Parser, 2025.6.28.05.20
// A shared melody parser for Aesthetic Computer that can parse melody strings
// and return structured note data. Used by both kidlisp.mjs and clock.mjs.
//
// 🎵 NEW: Octave Persistence Feature!
// You can now write "5cdefg" instead of "5c5d5e5f5g" - the octave persists until changed.
// Examples:
//   5cdefg      -> 5C 5D 5E 5F 5G (octave 5 persists)  
//   4cd5ef6g    -> 4C 4D 5E 5F 6G (octave changes persist)
//   5d.d.edgf#- -> 5D 5D 5E 5D 5G 5F# (with rhythm)
// 
// 🎵 NEW: Relative Octave Modifiers!
// Use + and - for relative octave changes that accumulate:
// Examples:
//   c +c +c +c  -> 4C 5C 6C 7C (each + goes one octave higher)
//   c +d +e -f  -> 4C 5D 6E 5F (+ goes up, - goes down from current)
//   c +d 5e +f  -> 4C 5D 5E 6F (absolute octave resets the base)
//   ++c --d     -> 6C 2D (multiple modifiers for bigger jumps)
// 
// Character savings: 25-43% fewer characters for typical melodies!

/**
 * Parses a melody string into an array of note objects.
 * 
 * Supports multiple notations:
 * - Octave-first: 4c, 5g#, 6d
 * - Note-first: c4, g#5, d6
 * - Default octave (4): c, g#, d
 * - Octave persistence: 5cdefg (octave 5 persists until changed)
 * - Octave changes: 4cd5ef6g (octave changes persist)
 * - Relative octave modifiers: +c, -b, ++d, --a (relative to base octave)
 * 
 * Modifiers:
 * - Sharp: # or s (4c# or 4cs)
 * - Flat: b (4db)
 * - Duration dots: . (shorter) .. (even shorter) ... (shortest)
 * - Duration commas: , (longer) ,, (even longer) ,,, (longest)
 * - Global duration modifiers: {...} {..} {.} {,} {,,} - applies to all following notes without local modifiers
 * - Swing prefixes: < (early/rushed) > (late/laid back)
 * - Multiple swing: << (more early) >>> (much later) - multiples the effect
 * - Swing timing: Each symbol = 1/16 beat offset (small, musical adjustments)
 * - Relative octave: + (one octave up) - (one octave down) ++ (two up) -- (two down)
 * - Waveform types: {sine} {sawtooth} {square} {triangle} - persists until changed
 * - Volume control: {0.5} (volume only) or {square:0.3} (type and volume) - persists until changed
 * - Rests: _ (explicit rest) or standalone - (context dependent)
 * - Separators: spaces (ignored), | (measure bars, ignored)
 * - Measure separators: | (ignored, visual only)
 * 
 * @param {string} melodyString - The melody string to parse
 * @param {number} [startingOctave=4] - The default octave to use if none specified
 * @returns {Array} Array of note objects with { note, octave, duration, waveType, volume, swing?, swingAmount? } properties
 */
export function parseMelody(melodyString, startingOctave = 4) {
  const notes = [];
  let i = 0;
  let currentOctave = startingOctave; // Use provided starting octave that persists across notes
  let baseOctave = startingOctave; // Base octave for relative modifiers
  let relativeOffset = 0; // Current relative offset from base octave
  let currentWaveType = "sine"; // Default waveform type that persists across notes
  let currentVolume = 0.8; // Default volume that persists across notes
  let globalDurationModifier = null; // Global duration modifier (e.g., "..." or "," or null)

  // Helper function to apply global duration modifier if no local modifier is present
  function applyGlobalDurationModifier(baseDuration, localModifier) {
    if (localModifier) {
      // Local modifier takes precedence, return as is
      return baseDuration;
    }
    
    if (!globalDurationModifier) {
      // No global modifier, return base duration
      return baseDuration;
    }
    
    // Apply global duration modifier
    if (globalDurationModifier.startsWith('.')) {
      const dots = globalDurationModifier.length;
      return 2 / Math.pow(2, dots); // Divide by powers of 2
    } else if (globalDurationModifier.startsWith(',')) {
      const commas = globalDurationModifier.length;
      return 2 * Math.pow(2, commas); // Multiply by powers of 2
    }
    
    return baseDuration;
  }
  
  while (i < melodyString.length) {
    let char = melodyString[i];
    
    // Handle waveform type and volume syntax {sine}, {sawtooth}, {square:0.5}, {0.3}
    // Also handle global duration modifiers like {...}, {..}, {.}, {,}, {,,}
    if (char === '{') {
      const endBrace = melodyString.indexOf('}', i);
      if (endBrace !== -1) {
        const content = melodyString.substring(i + 1, endBrace);
        const contentLower = content.toLowerCase();
        
        // Check for global duration modifier syntax like {...}, {..}, {.}, {,}, {,,}
        if (/^[.,]+$/.test(content)) {
          globalDurationModifier = content;
        }
        // Check for type:volume syntax like {square:0.5}
        else if (contentLower.includes(':')) {
          const [waveType, volumeStr] = contentLower.split(':');
          if (['sine', 'sawtooth', 'square', 'triangle'].includes(waveType)) {
            currentWaveType = waveType;
          }
          const volume = parseFloat(volumeStr);
          if (!isNaN(volume) && volume >= 0 && volume <= 1) {
            currentVolume = volume;
          }
        }
        // Check for volume-only syntax like {0.5}
        else if (/^\d*\.?\d+$/.test(contentLower)) {
          const volume = parseFloat(contentLower);
          if (!isNaN(volume) && volume >= 0 && volume <= 1) {
            currentVolume = volume;
          }
        }
        // Check for waveform-only syntax like {sine}
        else if (['sine', 'sawtooth', 'square', 'triangle'].includes(contentLower)) {
          currentWaveType = contentLower;
        }
        
        i = endBrace + 1;
        continue; // Skip to next character after processing waveform/volume/duration
      }
      // If not a valid syntax, just skip the character
      i++;
      continue;
    }
    
    // Handle octave-first notation (4c, 5g#, etc) or standalone octave changes
    if (/[0-9]/.test(char)) {
      const octave = parseInt(char);
      currentOctave = octave; // Update persistent octave
      baseOctave = octave; // Set as new base octave for relative modifiers
      relativeOffset = 0; // Reset relative offset when absolute octave is specified
      i++;
      
      if (i < melodyString.length) {
        const noteChar = melodyString[i].toLowerCase();
        if (/[a-g]/.test(noteChar)) {
          let note = noteChar;
          i++;
          
          // Check for sharp or flat modifiers
          if (i < melodyString.length) {
            const nextChar = melodyString[i];
            if (nextChar === 's' || nextChar === '#') {
              note += '#'; // Convert both 's' and '#' to # internally
              i++;
            } else if (nextChar === 'b' && i + 1 < melodyString.length && !/[a-g]/i.test(melodyString[i + 1])) {
              // Only treat 'b' as flat if it's not followed by another note letter
              note += 'b';
              i++;
            }
          }
          
          // Default duration is 2 (half note = 1 second with default 500ms baseTempo)
          let duration = 2;
          let hasLocalModifier = false;
          
          // Check for duration modifiers using dots or dashes (optional)
          if (i < melodyString.length) {
            const nextChar = melodyString[i];
            // Use dots for divisions: no dots = quarter, . = eighth, .. = sixteenth, ... = thirty-second, etc.
            if (nextChar === '.') {
              hasLocalModifier = true;
              let dots = 0;
              while (i < melodyString.length && melodyString[i] === '.') {
                dots++;
                i++;
              }
              // Dots divide the default duration: . = default/2, .. = default/4, ... = default/8, etc.
              duration = 2 / Math.pow(2, dots);
            }
            // Use commas for longer notes: , = half, ,, = whole, ,,, = double whole, etc.
            else if (nextChar === ',') {
              hasLocalModifier = true;
              let commas = 0;
              while (i < melodyString.length && melodyString[i] === ',') {
                commas++;
                i++;
              }
              // , = double (4.0), ,, = quadruple (8.0), ,,, = octuple (16.0), etc.
              duration = 2 * Math.pow(2, commas);
            }
          }
          
          // Apply global duration modifier if no local modifier was found
          if (!hasLocalModifier) {
            duration = applyGlobalDurationModifier(duration, false);
          }
          
          notes.push({ note, octave: currentOctave, duration, waveType: currentWaveType, volume: currentVolume });
        } else {
          // Just an octave change without a note - currentOctave is already updated
          // This allows for patterns like "5defg" or "4c5d6e" where octave persists
        }
      }
    }
    // Handle swing prefixes (< for early, > for late) with multiple counts
    else if (char === '<' || char === '>') {
      const swingType = char; // '<' or '>'
      let swingCount = 0;
      
      // Count consecutive swing symbols
      while (i < melodyString.length && melodyString[i] === swingType) {
        swingCount++;
        i++;
      }
      
      // Now parse the note that follows
      if (i < melodyString.length) {
        const noteChar = melodyString[i].toLowerCase();
        if (/[a-g]/.test(noteChar)) {
          let note = noteChar;
          i++;
          
          // Check for sharp or flat modifiers
          if (i < melodyString.length) {
            const nextChar = melodyString[i];
            if (nextChar === 's' || nextChar === '#') {
              note += '#'; // Convert both 's' and '#' to # internally
              i++;
            } else if (nextChar === 'b' && i + 1 < melodyString.length && !/[a-g]/i.test(melodyString[i + 1])) {
              // Only treat 'b' as flat if it's not followed by another note letter
              note += 'b';
              i++;
            }
          }

          let octave = currentOctave; // Use persistent octave
          
          // Default duration is 2 (half note = 1 second with default 500ms baseTempo) with swing timing
          let duration = 2;
          let swing = swingType === '<' ? 'early' : 'late'; // early or late
          let swingAmount = swingCount; // How many symbols (1, 2, 3, etc.)
          let hasLocalModifier = false;
          
          // Check for duration modifiers using dots or dashes (optional)
          if (i < melodyString.length) {
            const nextChar = melodyString[i];
            // Use dots for divisions: no dots = quarter, . = eighth, .. = sixteenth, ... = thirty-second, etc.
            if (nextChar === '.') {
              hasLocalModifier = true;
              let dots = 0;
              while (i < melodyString.length && melodyString[i] === '.') {
                dots++;
                i++;
              }
              // Dots divide the default duration: . = default/2, .. = default/4, ... = default/8, etc.
              duration = 2 / Math.pow(2, dots);
            }
            // Use commas for longer notes: , = half, ,, = whole, ,,, = double whole, etc.
            else if (nextChar === ',') {
              hasLocalModifier = true;
              let commas = 0;
              while (i < melodyString.length && melodyString[i] === ',') {
                commas++;
                i++;
              }
              // , = double (4.0), ,, = quadruple (8.0), ,,, = octuple (16.0), etc.
              duration = 2 * Math.pow(2, commas);
            }
          }
          
          // Apply global duration modifier if no local modifier was found
          if (!hasLocalModifier) {
            duration = applyGlobalDurationModifier(duration, false);
          }
          
          notes.push({ note, octave, duration, swing, swingAmount, waveType: currentWaveType, volume: currentVolume });
        }
      }
    }
    // Handle note-first notation (c4, d#5, etc) or relative octave modifiers (+c, -b, ++d)
    else if (/[a-g+-]/.test(char.toLowerCase())) {
      let relativeModifier = '';
      
      // Check for relative octave modifiers at the start
      if (char === '+' || char === '-') {
        relativeModifier = char;
        i++;
        
        // Handle multiple modifiers (++, --, etc.)
        while (i < melodyString.length && melodyString[i] === char) {
          relativeModifier += char;
          i++;
        }
        
        // Now we should have a note
        if (i >= melodyString.length || !/[a-g]/.test(melodyString[i].toLowerCase())) {
          // No note following the modifier - for '-', this might be a standalone dash for rest
          if (char === '-') {
            // Handle as standalone rest dash - use comma logic now
            let commas = relativeModifier.length;
            let duration = 2 * Math.pow(2, commas); // - = half (2.0), -- = whole (4.0), etc.
            notes.push({ note: 'rest', octave: null, duration, waveType: null, volume: null });
            continue;
          } else {
            // Invalid syntax, skip
            continue;
          }
        }
        
        char = melodyString[i];
      }
      
      let note = char.toLowerCase();
      i++;
      
      // Check for sharp or flat modifiers
      if (i < melodyString.length) {
        const nextChar = melodyString[i];
        if (nextChar === 's' || nextChar === '#') {
          note += '#'; // Convert both 's' and '#' to # internally
          i++;
        } else if (nextChar === 'b' && i + 1 < melodyString.length && !/[a-g]/i.test(melodyString[i + 1])) {
          // Only treat 'b' as flat if it's not followed by another note letter
          note += 'b';
          i++;
        }
      }
      
      // For octave persistence, don't check for octave after note
      // The octave should be specified before the note (octave-first)
      // This allows patterns like 5cdefg to work correctly
      let octave;
      
      // Handle relative octave modifiers
      if (relativeModifier) {
        let modifierOffset = 0;
        
        if (relativeModifier.startsWith('+')) {
          modifierOffset = relativeModifier.length; // +1 for +, +2 for ++, etc.
          relativeOffset += modifierOffset; // Accumulate the offset
        } else if (relativeModifier.startsWith('-')) {
          modifierOffset = -relativeModifier.length; // -1 for -, -2 for --, etc.
          relativeOffset += modifierOffset; // Accumulate the offset
        }
        
        octave = baseOctave + relativeOffset;
        currentOctave = octave; // Update persistent octave to the new level
      } else {
        octave = currentOctave; // Use persistent octave
      }
      
      // Default duration is 2 (half note = 1 second with default 500ms baseTempo)
      let duration = 2;
      // No swing for normal notes
      let hasLocalModifier = false;
      
      // Check for duration modifiers using dots or dashes (optional)
      if (i < melodyString.length) {
        const nextChar = melodyString[i];
        // Use dots for divisions: no dots = quarter, . = eighth, .. = sixteenth, ... = thirty-second, etc.
        if (nextChar === '.') {
          hasLocalModifier = true;
          let dots = 0;
          while (i < melodyString.length && melodyString[i] === '.') {
            dots++;
            i++;
          }
          // Dots divide the default duration: . = default/2, .. = default/4, ... = default/8, etc.
          duration = 2 / Math.pow(2, dots);
        }
        // Use commas for longer notes: , = half, ,, = whole, ,,, = double whole, etc.
        else if (nextChar === ',') {
          hasLocalModifier = true;
          let commas = 0;
          while (i < melodyString.length && melodyString[i] === ',') {
            commas++;
            i++;
          }
          // , = double (4.0), ,, = quadruple (8.0), ,,, = octuple (16.0), etc.
          duration = 2 * Math.pow(2, commas);
        }
      }
      
      // Apply global duration modifier if no local modifier was found
      if (!hasLocalModifier) {
        duration = applyGlobalDurationModifier(duration, false);
      }
      
      notes.push({ note, octave, duration, waveType: currentWaveType, volume: currentVolume });
    }
    // Handle rests (only _ and explicit standalone -, not spaces which are separators)
    else if (char === '_') {
      let duration = 2; // Use same default as notes
      let hasLocalModifier = false;
      i++;
      
      // Check for duration modifiers using dots or commas on rests (consistent with notes)
      if (i < melodyString.length) {
        const nextChar = melodyString[i];
        // Use dots for divisions: . = default/2, .. = default/4, ... = default/8, etc.
        if (nextChar === '.') {
          hasLocalModifier = true;
          let dots = 0;
          while (i < melodyString.length && melodyString[i] === '.') {
            dots++;
            i++;
          }
          // Dots divide the default duration: . = default/2, .. = default/4, ... = default/8, etc.
          duration = 2 / Math.pow(2, dots);
        }
        // Use commas for longer rests: , = double, ,, = quadruple, ,,, = octuple, etc.
        else if (nextChar === ',') {
          hasLocalModifier = true;
          let commas = 0;
          while (i < melodyString.length && melodyString[i] === ',') {
            commas++;
            i++;
          }
          // Commas multiply the default duration: , = default*2, ,, = default*4, ,,, = default*8, etc.
          duration = 2 * Math.pow(2, commas);
        }
      }
      
      // Apply global duration modifier if no local modifier was found
      if (!hasLocalModifier) {
        duration = applyGlobalDurationModifier(duration, false);
      }
      
      notes.push({ note: 'rest', octave: null, duration, waveType: null, volume: null });
    }
    // Handle spaces as separators (ignore them, don't treat as rests)
    else if (char === ' ') {
      i++; // Skip spaces - they're just separators
    }
    // Handle standalone duration dashes (not preceded by a note - these become rests)
    else if (char === '-') {
      // Check if this is a standalone rest dash or part of a sequence
      let dashes = 0;
      let tempI = i;
      while (tempI < melodyString.length && melodyString[tempI] === '-') {
        dashes++;
        tempI++;
      }
      
      // Treat standalone dashes as comma logic: - = 2*2=4, -- = 2*4=8, etc.
      let duration = 2 * Math.pow(2, dashes);
      i = tempI; // Move index past all the dashes
      
      notes.push({ note: 'rest', octave: null, duration, waveType: null, volume: null });
    }
    // Handle measure separators (optional, for readability)
    else if (char === '|') {
      // Measure bars are just visual separators, skip them
      i++;
    } else {
      i++;
    }
  }
  
  return notes;
}

/**
 * Converts a note and octave to the tone format expected by the Aesthetic Computer synth.
 * 
 * @param {string} note - The note name (c, c#, d, etc.)
 * @param {number} octave - The octave number (4 = middle octave)
 * @returns {string} The tone string in the format expected by the synth (e.g., "4C", "5G#")
 */
export function noteToTone(note, octave = 4) {
  // Handle both simple notes and notes with octave/modifiers
  const noteMap = {
    'c': 'C',
    'c#': 'C#',
    'db': 'C#',
    'd': 'D',
    'd#': 'D#',
    'eb': 'D#',
    'e': 'E',
    'f': 'F',
    'f#': 'F#',
    'gb': 'F#',
    'g': 'G',
    'g#': 'G#',
    'ab': 'G#',
    'a': 'A',
    'a#': 'A#',
    'bb': 'A#',
    'b': 'B'
  };
  
  const normalizedNote = note.toLowerCase();
  const baseNote = noteMap[normalizedNote] || 'C';
  
  // Use specified octave, or default to 4
  const finalOctave = octave !== null ? octave : 4;
  
  return `${finalOctave}${baseNote}`;
}

/**
 * Extracts just the note names from a parsed melody, suitable for simple note sequences.
 * Rests are converted to a default note or skipped based on options.
 * 
 * @param {Array} parsedMelody - Array of note objects from parseMelody()
 * @param {Object} options - Options for extraction
 * @param {boolean} options.skipRests - Whether to skip rests (default: true)
 * @param {string} options.restNote - Note to use for rests if not skipping (default: 'G')
 * @returns {Array} Array of note strings
 */
export function extractNoteNames(parsedMelody, options = {}) {
  const { skipRests = true, restNote = 'G' } = options;
  
  return parsedMelody
    .filter(noteData => !skipRests || noteData.note !== 'rest')
    .map(noteData => noteData.note === 'rest' ? restNote : noteData.note);
}

/**
 * Extracts tone strings from a parsed melody, suitable for direct use with the synth.
 * 
 * @param {Array} parsedMelody - Array of note objects from parseMelody()
 * @param {Object} options - Options for extraction
 * @param {boolean} options.skipRests - Whether to skip rests (default: true)
 * @param {string} options.restTone - Tone to use for rests if not skipping (default: '4G')
 * @returns {Array} Array of tone strings
 */
export function extractTones(parsedMelody, options = {}) {
  const { skipRests = true, restTone = '4G' } = options;
  
  return parsedMelody
    .filter(noteData => !skipRests || noteData.note !== 'rest')
    .map(noteData => noteData.note === 'rest' ? restTone : noteToTone(noteData.note, noteData.octave));
}

/**
 * Parses a melody string that may contain simultaneous tracks enclosed in parentheses.
 * 
 * Examples:
 * - "(ceg) (dfa)" - Two simultaneous 3-note chords
 * - "(cdefg) (gfedc)" - Two simultaneous melodic lines
 * - "c d (eg) f" - Mix of single notes and simultaneous notes
 * 
 * @param {string} melodyString - The melody string with possible parenthesized tracks
 * @param {number} [startingOctave=4] - The default octave to use if none specified
 * @returns {Object} Object with { tracks: Array[], isSingleTrack: boolean }
 */
export function parseSimultaneousMelody(melodyString, startingOctave = 4) {
  // First, extract any global waveform types that appear outside parentheses
  let globalWaveType = "sine"; // Default
  let processedMelodyString = melodyString;
  
  // Find waveform types outside of parentheses
  const globalWaveTypePattern = /{(sine|sawtooth|square|triangle)}/g;
  let match;
  const waveTypes = [];
  
  while ((match = globalWaveTypePattern.exec(melodyString)) !== null) {
    const start = match.index;
    const end = match.index + match[0].length;
    
    // Check if this waveform type is inside parentheses
    const beforeMatch = melodyString.substring(0, start);
    const afterMatch = melodyString.substring(end);
    
    // Count parentheses before and after to see if we're inside a group
    const openParensBefore = (beforeMatch.match(/\(/g) || []).length;
    const closeParensBefore = (beforeMatch.match(/\)/g) || []).length;
    
    // If open parens == close parens before this position, we're outside any group
    if (openParensBefore === closeParensBefore) {
      globalWaveType = match[1].toLowerCase();
      waveTypes.push({ position: start, length: match[0].length, type: match[1] });
    }
  }
  
  // Remove global waveform types from the string (outside parentheses only)
  waveTypes.reverse().forEach(waveInfo => {
    // Double-check this is outside parentheses
    const beforeMatch = processedMelodyString.substring(0, waveInfo.position);
    const openParensBefore = (beforeMatch.match(/\(/g) || []).length;
    const closeParensBefore = (beforeMatch.match(/\)/g) || []).length;
    
    if (openParensBefore === closeParensBefore) {
      processedMelodyString = processedMelodyString.substring(0, waveInfo.position) + 
                             processedMelodyString.substring(waveInfo.position + waveInfo.length);
    }
  });
  
  // If no parentheses found, treat as single track
  if (!processedMelodyString.includes('(') && !processedMelodyString.includes(')')) {
    // Apply global wave type to the single track if one was specified
    const hasLocalWaveType = /{(sine|sawtooth|square|triangle)}/.test(processedMelodyString);
    const contentWithGlobalWave = hasLocalWaveType ? processedMelodyString : 
                                   (globalWaveType !== "sine" ? `{${globalWaveType}} ${processedMelodyString}` : processedMelodyString);
    
    return {
      tracks: [parseMelody(contentWithGlobalWave, startingOctave)],
      isSingleTrack: true,
      type: 'single'
    };
  }
  
  // Extract all parentheses groups and check for x() disable syntax
  const parenGroups = [];
  const disabledTracks = []; // Track which tracks are disabled by x()
  const parenPattern = /x?\(([^)]+)\)/g;
  let groupMatch;
  
  while ((groupMatch = parenPattern.exec(processedMelodyString)) !== null) {
    const fullMatch = groupMatch[0]; // e.g., "x(ceg)" or "(ceg)"
    const groupContent = groupMatch[1].trim(); // e.g., "ceg"
    const isDisabled = fullMatch.startsWith('x(');
    
    parenGroups.push(groupContent);
    disabledTracks.push(isDisabled);
  }
  
  // If we found parentheses groups, create parallel tracks
  if (parenGroups.length > 0) {
    const parallelTracks = parenGroups.map((groupContent, index) => {
      // Prepend the global wave type to each group if it doesn't have its own
      const hasLocalWaveType = /{(sine|sawtooth|square|triangle)}/.test(groupContent);
      const contentWithGlobalWave = hasLocalWaveType ? groupContent : `{${globalWaveType}} ${groupContent}`;
      const parsedTrack = parseMelody(contentWithGlobalWave, startingOctave);
      
      // Add disabled flag to the track
      parsedTrack.isDisabled = disabledTracks[index];
      
      return parsedTrack;
    });
    
    return {
      tracks: parallelTracks,
      isSingleTrack: false,
      type: 'parallel',
      trackCount: parallelTracks.length,
      maxLength: Math.max(...parallelTracks.map(track => track.length))
    };
  }
  
  // Fallback to single track if something went wrong
  return {
    tracks: [parseMelody(processedMelodyString, startingOctave)],
    isSingleTrack: true,
    type: 'single'
  };
}
