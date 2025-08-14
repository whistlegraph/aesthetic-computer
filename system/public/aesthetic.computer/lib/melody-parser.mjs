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
// 🎵 NEW: Space-Based Parallel Tracks!
// Use spaces to separate groups for parallel playback:
// Examples:
//   ceg         -> Single track with 3 notes
//   ceg dfa     -> Two parallel tracks
//   ceg* dfa    -> First track with mutation, second without
//   ceg* dfa*   -> Both tracks with mutation
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
 * - Duration dots: . (shorter) .. (even shorter) ... (shortest) - STICKY: applies to following notes until changed
 * - Duration commas: , (longer) ,, (even longer) ,,, (longest) - STICKY: applies to following notes until changed
 * - Sonic extension: ' (longer sound) '' (even longer sound) ''' (longest sound) - extends note sound without affecting timeline
 * - Swing prefixes: < (early/rushed) > (late/laid back)
 * - Multiple swing: << (more early) >>> (much later) - multiples the effect
 * - Swing timing: Each symbol = 1/16 beat offset (small, musical adjustments)
 * - Relative octave: + (one octave up) - (one octave down) ++ (two up) -- (two down)
 * - Struck mode: ^ (toggle between held and struck notes) - STICKY: applies to following notes until changed
 * - Waveform types: {sine} {sawtooth} {square} {triangle} {noise-white} {sample} {custom} - persists until changed
 * - Volume control: {0.5} (volume only) or {square:0.3} (type and volume) - persists until changed
 * - Hz shift: {100hz} {-50hz} {50hz&} (frequency shift in Hz, & for cumulative) - persists until changed
 * - Speech synthesis: "text" (quoted text for speech synthesis with random voice selection)
 * - Rests: _ (explicit rest) or standalone - (context dependent)
 * - Separators: spaces (ignored), | (measure bars, ignored)
 * - Measure separators: | (ignored, visual only)
 * 
 * Duration modifier examples:
 *   c... defg    -> c is very short (1/8), d,e,f,g are also very short until changed
 *   c... d. efg  -> c is very short (1/8), d is short (1/2), e,f,g are short (1/2)
 *   c,, defg     -> c is very long (8.0), d,e,f,g are also very long until changed
 * 
 * Sonic extension examples:
 *   c.de'f       -> c short, d normal, e' long sound but normal timeline, f normal
 *   cde''f       -> c normal, d normal, e'' very long sound but normal timeline, f normal
 *   c'd'e'f      -> all notes have extended sound duration but keep normal timeline spacing
 * 
 * Struck mode examples:
 *   ^cdefg       -> All notes are struck (finite duration with natural decay)
 *   c^defg       -> c is held, d,e,f,g are struck  
 *   ^cd^efg      -> c,d are struck, e,f,g are held again
 * 
 * Speech synthesis examples:
 *   cde"hi"gab   -> Plays c, d, e, speaks "hi", then plays g, a, b
 *   "hello"defg  -> Speaks "hello", then plays d, e, f, g
 *   c"one"d"two" -> Plays c, speaks "one", plays d, speaks "two"
 * 
 * @param {string} melodyString - The melody string to parse
 * @param {number} [startingOctave=4] - The default octave to use if none specified
 * @returns {Array} Array of note objects with { note, octave, duration, sonicDuration?, waveType, volume, swing?, swingAmount?, struck?, toneShift? } properties
 */
export function parseMelody(melodyString, startingOctave = 4) {
  const notes = [];
  let i = 0;
  let currentOctave = startingOctave; // Use provided starting octave that persists across notes
  let baseOctave = startingOctave; // Base octave for relative modifiers
  let relativeOffset = 0; // Current relative offset from base octave
  let currentWaveType = "sine"; // Default waveform type that persists across notes
  let currentVolume = 0.8; // Default volume that persists across notes
  let currentToneShift = 0; // Default Hz shift that persists across notes
  let globalDurationModifier = null; // Sticky duration modifier (e.g., "..." or "," or null)
  let isStruck = false; // Sticky struck mode flag (when ^ is used)

  // Helper function to apply sticky duration modifier if no local modifier is present
  function applyStickyDurationModifier(baseDuration, hasLocalModifier) {
    if (hasLocalModifier) {
      // Local modifier takes precedence, return as is
      return baseDuration;
    }
    
    if (!globalDurationModifier) {
      // No sticky modifier, return base duration
      return baseDuration;
    }
    
    // Apply sticky duration modifier
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
    
    // Handle quoted text for speech synthesis
    if (char === '"') {
      // Find the closing quote
      let quotedText = '';
      let quoteStartIndex = i;
      i++; // Move past opening quote
      
      while (i < melodyString.length && melodyString[i] !== '"') {
        quotedText += melodyString[i];
        i++;
      }
      
      if (i < melodyString.length && melodyString[i] === '"') {
        // Found closing quote, create a speech note
        const speechNote = {
          note: "speech",
          text: quotedText,
          octave: currentOctave,
          duration: 2, // Default duration for speech
          waveType: currentWaveType,
          volume: currentVolume,
          struck: isStruck,
          toneShift: currentToneShift,
          isSpeech: true
        };
        
        notes.push(speechNote);
        i++; // Move past closing quote
        continue;
      } else {
        // No closing quote found, treat as regular character
        i = quoteStartIndex; // Reset position
      }
    }
    
    // Handle waveform type, volume, and Hz shift syntax {sine}, {sawtooth}, {square:0.5}, {0.3}, {100hz}, {-50hz}
    if (char === '{') {
      const endBrace = melodyString.indexOf('}', i);
      if (endBrace !== -1) {
        const content = melodyString.substring(i + 1, endBrace);
        const contentLower = content.toLowerCase();
        
        // Check for Hz shift syntax like {100hz} or {-50hz} or {50hz&} for cumulative
        if (contentLower.endsWith('hz&') || contentLower.endsWith('hz')) {
          const isCumulative = contentLower.endsWith('hz&');
          const hzPart = isCumulative ? contentLower.slice(0, -3) : contentLower.slice(0, -2);
          const hzValue = parseFloat(hzPart);
          
          if (!isNaN(hzValue)) {
            if (isCumulative) {
              // Store the cumulative step value for later use in the clock
              currentToneShift = { value: hzValue, cumulative: true, step: hzValue };
              console.log(`🎵 Parsed cumulative Hz shift: ${hzValue}Hz&`);
            } else {
              currentToneShift = hzValue;
              console.log(`🎵 Parsed Hz shift: ${hzValue}Hz`);
            }
          } else {
            console.log(`🎵 PARSER ERROR: Failed to parse Hz value: {${content}}`);
          }
        }
        // Check for type:volume syntax like {square:0.5}
        else if (contentLower.includes(':')) {
          const [waveType, volumeStr] = contentLower.split(':');
          if (['sine', 'sawtooth', 'square', 'triangle', 'noise-white', 'sample', 'custom'].includes(waveType)) {
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
        else if (['sine', 'sawtooth', 'square', 'triangle', 'noise-white', 'sample', 'custom'].includes(contentLower)) {
          currentWaveType = contentLower;
        }
        
        i = endBrace + 1;
        continue; // Skip to next character after processing waveform/volume/toneShift
      }
      // If not a valid syntax, just skip the character
      i++;
      continue;
    }
    
    // Handle struck note toggle (^)
    if (char === '^') {
      isStruck = !isStruck; // Toggle struck mode
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
              note += 's'; // Convert both 's' and '#' to s internally
              i++;
            } else if (nextChar === 'b' && i + 1 < melodyString.length && !/[a-g]/i.test(melodyString[i + 1])) {
              // Only treat 'b' as flat if it's not followed by another note letter
              note += 'b';
              i++;
            }
          }
          
          // Check for sonic extension modifier (') before duration modifiers
          let sonicExtension = 0;
          if (i < melodyString.length && melodyString[i] === "'") {
            while (i < melodyString.length && melodyString[i] === "'") {
              sonicExtension++;
              i++;
            }
          }
          
          // Default duration is 2 (half note = 1 second with default 500ms baseTempo)
          let duration = 2;
          let sonicDuration = duration; // Start with timeline duration
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
              // Make this duration modifier sticky for subsequent notes
              globalDurationModifier = '.'.repeat(dots);
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
              // Make this duration modifier sticky for subsequent notes
              globalDurationModifier = ','.repeat(commas);
            }
          }
          
          // Apply sticky duration modifier if no local modifier was found
          if (!hasLocalModifier) {
            duration = applyStickyDurationModifier(duration, false);
          }
          
          // Apply sonic extension if present
          if (sonicExtension > 0) {
            // Each apostrophe doubles the sonic duration: ' = 2x, '' = 4x, ''' = 8x
            sonicDuration = duration * Math.pow(2, sonicExtension);
          } else {
            sonicDuration = duration; // No extension, sonic matches timeline
          }
          
          const noteObj = { 
            note, 
            octave: currentOctave, 
            duration, 
            waveType: currentWaveType, 
            volume: currentVolume, 
            struck: isStruck, 
            toneShift: currentToneShift 
          };
          
          // Only add sonicDuration if it's different from duration
          if (sonicDuration !== duration) {
            noteObj.sonicDuration = sonicDuration;
          }
          
          notes.push(noteObj);
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
              note += 's'; // Convert both 's' and '#' to s internally
              i++;
            } else if (nextChar === 'b' && i + 1 < melodyString.length && !/[a-g]/i.test(melodyString[i + 1])) {
              // Only treat 'b' as flat if it's not followed by another note letter
              note += 'b';
              i++;
            }
          }
          
          // Check for sonic extension modifier (') before duration modifiers
          let sonicExtension = 0;
          if (i < melodyString.length && melodyString[i] === "'") {
            while (i < melodyString.length && melodyString[i] === "'") {
              sonicExtension++;
              i++;
            }
          }

          let octave = currentOctave; // Use persistent octave
          
          // Default duration is 2 (half note = 1 second with default 500ms baseTempo) with swing timing
          let duration = 2;
          let sonicDuration = duration; // Start with timeline duration
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
            duration = applyStickyDurationModifier(duration, false);
          }
          
          // Apply sonic extension if present
          if (sonicExtension > 0) {
            // Each apostrophe doubles the sonic duration: ' = 2x, '' = 4x, ''' = 8x
            sonicDuration = duration * Math.pow(2, sonicExtension);
          } else {
            sonicDuration = duration; // No extension, sonic matches timeline
          }
          
          const noteObj = { 
            note, 
            octave, 
            duration, 
            swing, 
            swingAmount, 
            waveType: currentWaveType, 
            volume: currentVolume, 
            struck: isStruck, 
            toneShift: currentToneShift 
          };
          
          // Only add sonicDuration if it's different from duration
          if (sonicDuration !== duration) {
            noteObj.sonicDuration = sonicDuration;
          }
          
          notes.push(noteObj);
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
          note += 's'; // Convert both 's' and '#' to s internally
          i++;
        } else if (nextChar === 'b' && i + 1 < melodyString.length && !/[a-g]/i.test(melodyString[i + 1])) {
          // Only treat 'b' as flat if it's not followed by another note letter
          note += 'b';
          i++;
        }
      }
      
      // Check for sonic extension modifier (') before duration modifiers
      let sonicExtension = 0;
      if (i < melodyString.length && melodyString[i] === "'") {
        while (i < melodyString.length && melodyString[i] === "'") {
          sonicExtension++;
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
      let sonicDuration = duration; // Start with timeline duration
      // No swing for normal notes
      let hasLocalModifier = false;
      
      // Check for duration modifiers using dots or dashes (optional)
      if (i < melodyString.length) {
        const nextChar = melodyString[i];
        // Use dots for divisions: no dots = quarter, . = eighth, .. = sixteenth, ... = default/8, etc.
        if (nextChar === '.') {
          hasLocalModifier = true;
          let dots = 0;
          while (i < melodyString.length && melodyString[i] === '.') {
            dots++;
            i++;
          }
          // Dots divide the default duration: . = default/2, .. = default/4, ... = default/8, etc.
          duration = 2 / Math.pow(2, dots);
          // Make this duration modifier sticky for subsequent notes
          globalDurationModifier = '.'.repeat(dots);
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
          // Make this duration modifier sticky for subsequent notes
          globalDurationModifier = ','.repeat(commas);
        }
      }
      
      // Apply sticky duration modifier if no local modifier was found
      if (!hasLocalModifier) {
        duration = applyStickyDurationModifier(duration, false);
      }
      
      // Apply sonic extension if present
      if (sonicExtension > 0) {
        // Each apostrophe doubles the sonic duration: ' = 2x, '' = 4x, ''' = 8x
        sonicDuration = duration * Math.pow(2, sonicExtension);
      } else {
        sonicDuration = duration; // No extension, sonic matches timeline
      }
      
      const noteObj = { 
        note, 
        octave, 
        duration, 
        waveType: currentWaveType, 
        volume: currentVolume, 
        struck: isStruck, 
        toneShift: currentToneShift 
      };
      
      // Only add sonicDuration if it's different from duration
      if (sonicDuration !== duration) {
        noteObj.sonicDuration = sonicDuration;
      }
      
      notes.push(noteObj);
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
          // Make this duration modifier sticky for subsequent notes
          globalDurationModifier = '.'.repeat(dots);
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
          // Make this duration modifier sticky for subsequent notes
          globalDurationModifier = ','.repeat(commas);
        }
      }
      
      // Apply sticky duration modifier if no local modifier was found
      if (!hasLocalModifier) {
        duration = applyStickyDurationModifier(duration, false);
      }
      
      notes.push({ note: 'rest', octave: currentOctave, duration, waveType: null, volume: null });
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
      
      notes.push({ note: 'rest', octave: currentOctave, duration, waveType: null, volume: null });
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
    'cs': 'C#',
    'c#': 'C#',
    'db': 'C#',
    'd': 'D',
    'ds': 'D#',
    'd#': 'D#',
    'eb': 'D#',
    'e': 'E',
    'f': 'F',
    'fs': 'F#',
    'f#': 'F#',
    'gb': 'F#',
    'g': 'G',
    'gs': 'G#',
    'g#': 'G#',
    'ab': 'G#',
    'a': 'A',
    'as': 'A#',
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
 * Parses a melody string that may contain simultaneous tracks separated by spaces.
 * 
 * Examples:
 * - "ceg" - Single track with 3 notes
 * - "ceg dfa" - Two simultaneous tracks (parallel)
 * - "5cdefg 4gfedc" - Two simultaneous melodic lines
 * - "ceg* dfa" - First track with mutation, second without
 * - "ceg* dfa*" - Both tracks with mutation
 * - "xceg dfa" - First track disabled (x prefix), only second plays
 * 
 * @param {string} melodyString - The melody string with space-separated groups
 * @param {number} [startingOctave=4] - The default octave to use if none specified
 * @returns {Object} Object with { tracks: Array[], isSingleTrack: boolean }
 */
export function parseSimultaneousMelody(melodyString, startingOctave = 4) {
  // Extract any global waveform types that appear at the beginning
  let globalWaveType = "sine"; // Default
  let processedMelodyString = melodyString.trim();
  
  // Find waveform types at the beginning of the string
  const globalWaveTypePattern = /^{(sine|sawtooth|square|triangle|noise-white|sample|custom)}/;
  const waveMatch = globalWaveTypePattern.exec(processedMelodyString);
  
  if (waveMatch) {
    globalWaveType = waveMatch[1].toLowerCase();
    processedMelodyString = processedMelodyString.substring(waveMatch[0].length).trim();
  }
  
  // Split the melody string by whitespace to get individual groups
  const groups = processedMelodyString.split(/\s+/).filter(group => group.length > 0);
  
  // If no groups or only one group, treat as single track
  if (groups.length <= 1) {
    const singleGroup = groups[0] || '';
    
    // Check for * anywhere in the string (support multiple * markers for mutation zones)
    const asteriskIndices = [];
    for (let i = 0; i < singleGroup.length; i++) {
      if (singleGroup[i] === '*') {
        asteriskIndices.push(i);
      }
    }
    const hasMutation = asteriskIndices.length > 0;
    let contentForParsing = singleGroup;
    let mutationTriggerPositions = [];
    
    if (hasMutation) {
      // Remove all * characters from the content to parse
      contentForParsing = singleGroup.replace(/\*/g, '');
      
      // Calculate the positions in the parsed notes where mutations should trigger
      // For each *, count actual note characters before it in the processed string
      mutationTriggerPositions = asteriskIndices.map(asteriskIndex => {
        let noteCount = 0;
        for (let i = 0; i < asteriskIndex; i++) {
          const char = singleGroup[i];
          // Count note letters, rests, and skip over waveform/duration modifiers
          if (char === '{') {
            // Skip over entire {waveform} or {duration} block
            while (i < singleGroup.length && singleGroup[i] !== '}') {
              i++;
            }
            if (i < singleGroup.length && singleGroup[i] === '}') {
              i++; // Skip the closing brace
            }
            i--; // Adjust for the loop increment
          } else if (/[a-g_-]/.test(char) || 
              (char.match(/[0-9]/) && i + 1 < singleGroup.length && /[a-g]/.test(singleGroup[i + 1]))) {
            noteCount++;
          }
        }
        return noteCount;
      });
    }
    
    // Apply global wave type to the single track if one was specified
    const hasLocalWaveType = /{(sine|sawtooth|square|triangle|noise-white|sample|custom)}/.test(contentForParsing);
    const contentWithGlobalWave = hasLocalWaveType ? contentForParsing : 
                                   (globalWaveType !== "sine" ? `{${globalWaveType}} ${contentForParsing}` : contentForParsing);
    
    const parsedTrack = parseMelody(contentWithGlobalWave, startingOctave);
    
    // Add mutation flag to the single track if * was present
    if (hasMutation) {
      parsedTrack.hasMutation = true;
      parsedTrack.originalContent = contentForParsing;
      parsedTrack.mutationCount = 0;
      parsedTrack.mutationTriggerPositions = mutationTriggerPositions; // Array of positions where mutations should trigger
      parsedTrack.currentMutationZone = 0; // Track which mutation zone we're currently in
      parsedTrack.mutationType = 'within-group'; // Notes to the left of asterisks
      
      // Only set legacy single position for backward compatibility if we have exactly one trigger
      // This prevents conflicts between multiple zones and legacy systems
      if (mutationTriggerPositions.length === 1) {
        parsedTrack.mutationTriggerPosition = mutationTriggerPositions[0];
      }
    }
    
    return {
      tracks: [parsedTrack],
      isSingleTrack: true,
      type: 'single'
    };
  }
  
  // Multiple groups - create parallel tracks
  const parallelTracks = groups.map((groupContent, index) => {
    // Check if group starts with 'x' to disable it
    const isDisabled = groupContent.startsWith('x');
    let processingContent = isDisabled ? groupContent.slice(1) : groupContent; // Remove 'x' if present
    
    // Handle asterisks within the group content
    let contentForParsing = processingContent;
    let mutationTriggerPositions = [];
    let hasMutation = false;
    
    // Find asterisks within this group and create mutation zones
    const asteriskIndices = [];
    for (let i = 0; i < processingContent.length; i++) {
      if (processingContent[i] === '*') {
        asteriskIndices.push(i);
      }
    }
    
    if (asteriskIndices.length > 0) {
      hasMutation = true;
      // Remove asterisks from content for parsing
      contentForParsing = processingContent.replace(/\*/g, '');
      
      // Calculate mutation trigger positions (notes to the left of each asterisk)
      mutationTriggerPositions = asteriskIndices.map(asteriskIndex => {
        let noteCount = 0;
        for (let i = 0; i < asteriskIndex; i++) {
          const char = processingContent[i];
          // Count note letters, rests, and skip over waveform/duration modifiers
          if (char === '{') {
            // Skip over entire {waveform} or {duration} block
            while (i < processingContent.length && processingContent[i] !== '}') {
              i++;
            }
            if (i < processingContent.length && processingContent[i] === '}') {
              i++; // Skip the closing brace
            }
            i--; // Adjust for the loop increment
          } else if (/[a-g_-]/.test(char) || 
              (char.match(/[0-9]/) && i + 1 < processingContent.length && /[a-g]/.test(processingContent[i + 1]))) {
            noteCount++;
          }
        }
        return noteCount;
      });
    }
    
    // Prepend the global wave type to each group if it doesn't have its own
    const hasLocalWaveType = /{(sine|sawtooth|square|triangle|noise-white|sample|custom)}/.test(contentForParsing);
    const contentWithGlobalWave = hasLocalWaveType ? contentForParsing : `{${globalWaveType}} ${contentForParsing}`;
    
    const parsedTrack = parseMelody(contentWithGlobalWave, startingOctave);
    
    // Add disabled flag to the track
    parsedTrack.isDisabled = isDisabled;
    
    // Add mutation flag to the track
    parsedTrack.hasMutation = hasMutation;
    if (parsedTrack.hasMutation) {
      // Store the original track content for mutation reference
      parsedTrack.originalContent = contentForParsing;
      parsedTrack.mutationCount = 0; // Track how many times we've mutated
      
      // Set up mutation zones for asterisks within groups
      if (mutationTriggerPositions.length > 0) {
        parsedTrack.mutationTriggerPositions = mutationTriggerPositions;
        parsedTrack.currentMutationZone = 0;
        parsedTrack.mutationType = 'within-group'; // Notes to the left of asterisks
        
        // Legacy compatibility
        if (mutationTriggerPositions.length === 1) {
          parsedTrack.mutationTriggerPosition = mutationTriggerPositions[0];
        }
      }
    }
    
    return parsedTrack;
  });
  
  // Filter out disabled tracks (x prefix syntax)
  const enabledTracks = parallelTracks.filter(track => !track.isDisabled);
  
  return {
    tracks: enabledTracks,
    isSingleTrack: false,
    type: 'parallel',
    trackCount: enabledTracks.length,
    maxLength: Math.max(...enabledTracks.map(track => track.length))
  };
}

/**
 * Mutates a melody track by randomly changing one note to a different note/sharp
 * @param {Array} originalTrack - The original parsed track
 * @param {string} originalContent - The original string content
 * @param {number} startingOctave - The starting octave
 * @returns {Array} - A new mutated track
 */
export function mutateMelodyTrack(originalTrack, originalContent, startingOctave = 4) {
  if (!originalTrack || originalTrack.length === 0) return originalTrack;
  
  // Check the mutation type to determine behavior
  const mutationType = originalTrack.mutationType || 'legacy';
  let mutatableIndices = [];
  
  if (mutationType === 'within-group') {
    // NEW: Asterisks within groups - mutate any note from the start (or previous asterisk) up to the current asterisk
    const hasMultipleZones = originalTrack.mutationTriggerPositions && originalTrack.mutationTriggerPositions.length > 0;
    
    if (hasMultipleZones) {
      let currentZone = originalTrack.currentMutationZone || 0;
      const triggerPositions = originalTrack.mutationTriggerPositions;
      
      // Loop back to the beginning if we've exhausted all zones
      if (currentZone >= triggerPositions.length) {
        currentZone = 0;
      }
      
      // For within-group asterisks: mutate any note from the zone start up to (but not including) the current asterisk position
      const asteriskPosition = triggerPositions[currentZone];
      
      // Determine the start of the current zone
      const zoneStart = currentZone === 0 ? 0 : triggerPositions[currentZone - 1];
      const zoneEnd = asteriskPosition; // Up to but not including the asterisk position
      
      // Find all notes in the range from zoneStart to zoneEnd
      // Prioritize unchanged notes, then previously mutated ones
      const unchangedIndices = [];
      const mutatedIndices = [];
      
      for (let i = zoneStart; i < Math.min(zoneEnd, originalTrack.length); i++) {
        const note = originalTrack[i];
        if (note.note !== '-') { // Allow 'rest', '_', and notes to mutate, but not standalone dashes
          if (note.isMutation) {
            mutatedIndices.push(i);
          } else {
            unchangedIndices.push(i);
          }
        }
      }
      
      // Prefer unchanged notes first, then fall back to mutated ones
      mutatableIndices = unchangedIndices.length > 0 ? unchangedIndices : mutatedIndices;
    } else {
      // If no zones defined, prioritize unchanged notes in the entire track
      const unchangedIndices = [];
      const mutatedIndices = [];
      
      originalTrack.forEach((note, index) => {
        if (note.note !== '-') { // Allow 'rest', '_', and notes to mutate
          if (note.isMutation) {
            mutatedIndices.push(index);
          } else {
            unchangedIndices.push(index);
          }
        }
      });
      
      // Prefer unchanged notes first, then fall back to mutated ones
      mutatableIndices = unchangedIndices.length > 0 ? unchangedIndices : mutatedIndices;
    }
  } else {
    // LEGACY: Original behavior for backwards compatibility
    const hasMultipleZones = originalTrack.mutationTriggerPositions && originalTrack.mutationTriggerPositions.length > 0;
    const hasSingleTrigger = originalTrack.mutationTriggerPosition !== undefined;
    
    if (hasMultipleZones) {
      // Legacy system: Support multiple mutation zones with looping
      let currentZone = originalTrack.currentMutationZone || 0;
      const triggerPositions = originalTrack.mutationTriggerPositions;
      
      // Loop back to the beginning if we've exhausted all zones
      if (currentZone >= triggerPositions.length) {
        currentZone = 0;
      }
      
      // Determine the range for the current mutation zone
      const zoneStart = currentZone === 0 ? 0 : triggerPositions[currentZone - 1];
      const zoneEnd = triggerPositions[currentZone];
      
      // Only mutate notes within the current zone
      for (let i = zoneStart; i < Math.min(zoneEnd, originalTrack.length); i++) {
        const note = originalTrack[i];
        if (note.note !== '-') { // Allow 'rest', '_', and notes to mutate, but not standalone dashes
          mutatableIndices.push(i);
        }
      }
    } else if (hasSingleTrigger && originalTrack.mutationTriggerPosition > 0) {
      // Legacy system: Only mutate notes/rests that appear before the trigger position
      for (let i = 0; i < Math.min(originalTrack.mutationTriggerPosition, originalTrack.length); i++) {
        const note = originalTrack[i];
        if (note.note !== '-') { // Allow 'rest', '_', and notes to mutate, but not standalone dashes
          mutatableIndices.push(i);
        }
      }
    } else {
      // Original behavior: find all notes and rests in the track (exclude only other types of rests like '-')
      originalTrack.forEach((note, index) => {
        if (note.note !== '-') { // Allow 'rest', '_', and notes to mutate
          mutatableIndices.push(index);
        }
      });
    }
  }
  
  if (mutatableIndices.length === 0) return originalTrack; // No notes or rests to mutate
  
  // Create a new track with mutations
  const mutatedTrack = [...originalTrack];
  
  // Mutate one random note from the available options
  if (mutatableIndices.length > 0) {
    const randomIndex = mutatableIndices[Math.floor(Math.random() * mutatableIndices.length)];
    const itemToMutate = originalTrack[randomIndex];
    const mutatedNote = createMutatedNote(itemToMutate, startingOctave, originalTrack);
    mutatedTrack[randomIndex] = mutatedNote;
  }
  
  // Preserve track-level metadata
  preserveTrackMetadata(mutatedTrack, originalTrack);
  
  return mutatedTrack;
}

// Helper function to create a mutated note
function createMutatedNote(itemToMutate, startingOctave, originalTrack) {
  // Define possible note mutations (natural notes + rest)
  const noteNames = ['C', 'D', 'E', 'F', 'G', 'A', 'B'];
  const mutationOptions = [...noteNames, '_']; // Include rest as a mutation option
  
  // Handle rest-to-note mutation
  if (itemToMutate.note === 'rest' || itemToMutate.note === '_') {
    // Rest mutating to a note - pick a random note (never another rest)
    const randomNoteIndex = Math.floor(Math.random() * noteNames.length);
    const newNoteName = noteNames[randomNoteIndex].toLowerCase();
    
    // Try to preserve original octave if available
    let octaveToUse = itemToMutate.originalOctave || itemToMutate.octave || startingOctave || 4;
    
    // If no preserved octave, look for other notes in the track to get a reasonable octave
    if (!itemToMutate.originalOctave && !itemToMutate.octave && originalTrack.length > 1) {
      const otherNotes = originalTrack.filter(n => n.note !== 'rest' && n.note !== '_' && n.octave);
      if (otherNotes.length > 0) {
        // Use the most common octave from other notes
        const octaves = otherNotes.map(n => n.octave);
        const octaveCount = {};
        octaves.forEach(oct => octaveCount[oct] = (octaveCount[oct] || 0) + 1);
        octaveToUse = parseInt(Object.keys(octaveCount).reduce((a, b) => octaveCount[a] > octaveCount[b] ? a : b));
      }
    }
    
    // Create a new note mutation, preserving ALL modifier properties
    return {
      note: newNoteName,
      octave: octaveToUse,
      duration: itemToMutate.duration, // Keep the same duration
      waveType: itemToMutate.waveType || 'sine', // Preserve or use default
      volume: itemToMutate.volume || 0.8, // Preserve or use default
      tone: noteToTone(newNoteName, octaveToUse),
      isMutation: true, // Mark this as a mutation for potential visual feedback
      // Preserve ALL modifier properties from original note
      struck: itemToMutate.struck, // Preserve struck flag
      swing: itemToMutate.swing, // Preserve swing timing
      swingAmount: itemToMutate.swingAmount, // Preserve swing amount
      toneShift: itemToMutate.toneShift, // Preserve Hz shift
      sonicDuration: itemToMutate.sonicDuration, // Preserve sonic extension
      isSpeech: itemToMutate.isSpeech, // Preserve speech flag (if any)
      text: itemToMutate.text, // Preserve speech text (if any)
      // Store original note info for potential restoration
      originalNote: itemToMutate.note,
      originalOctave: itemToMutate.octave
    };
  }
  
  // Handle note-to-something mutation (existing logic)
  // Get the current note in a normalized form for comparison
  const currentNote = itemToMutate.note.toLowerCase();
  
  // Create all possible mutations (notes + rest) but exclude the current note
  const allPossibleMutations = [];
  
  // Add all natural notes and valid sharps (excluding E# and B# which don't exist)
  noteNames.forEach(noteName => {
    const natural = noteName.toLowerCase();
    const sharp = noteName.toLowerCase() + 's';
    
    // Only add if different from current note
    if (natural !== currentNote) {
      allPossibleMutations.push(natural);
    }
    
    // Only add sharps for notes that have valid sharps (exclude E# and B# which don't exist)
    if (sharp !== currentNote && noteName !== 'E' && noteName !== 'B') {
      allPossibleMutations.push(sharp);
    }
  });
  
  // Add rest option if current note is not a rest
  if (currentNote !== 'rest' && currentNote !== '_') {
    allPossibleMutations.push('_');
  }
  
  if (allPossibleMutations.length === 0) {
    // Fallback: if somehow no options available, return original note
    // This should never happen in practice
    return itemToMutate;
  }
  
  // Pick a random mutation from available options
  const randomMutationIndex = Math.floor(Math.random() * allPossibleMutations.length);
  const newMutation = allPossibleMutations[randomMutationIndex];
  
  // Handle note-to-rest mutation
  if (newMutation === '_') {
    return {
      note: 'rest', // Use 'rest' to match parser format
      octave: itemToMutate.octave, // Preserve octave for potential back-mutation
      duration: itemToMutate.duration, // Keep the same duration
      waveType: itemToMutate.waveType, // Preserve wave type
      volume: itemToMutate.volume, // Preserve volume
      tone: null, // Rests don't have tones
      isMutation: true, // Mark this as a mutation for potential visual feedback
      // Preserve ALL modifier properties from original note
      struck: itemToMutate.struck, // Preserve struck flag
      swing: itemToMutate.swing, // Preserve swing timing
      swingAmount: itemToMutate.swingAmount, // Preserve swing amount
      toneShift: itemToMutate.toneShift, // Preserve Hz shift
      sonicDuration: itemToMutate.sonicDuration, // Preserve sonic extension
      isSpeech: itemToMutate.isSpeech, // Preserve speech flag (if any)
      text: itemToMutate.text, // Preserve speech text (if any)
      // Store original note info for potential restoration
      originalNote: itemToMutate.note,
      originalOctave: itemToMutate.octave
    };
  }
  
  // Handle note-to-note mutation
  const newNoteName = newMutation;
  
  return {
    note: newNoteName,
    octave: itemToMutate.octave || startingOctave,
    duration: itemToMutate.duration, // Keep the same duration
    waveType: itemToMutate.waveType || 'sine', // Preserve or use default
    volume: itemToMutate.volume || 0.8, // Preserve or use default
    tone: noteToTone(newNoteName, itemToMutate.octave),
    isMutation: true, // Mark this as a mutation for potential visual feedback
    // Preserve ALL modifier properties from original note
    struck: itemToMutate.struck, // Preserve struck flag
    swing: itemToMutate.swing, // Preserve swing timing
    swingAmount: itemToMutate.swingAmount, // Preserve swing amount
    toneShift: itemToMutate.toneShift, // Preserve Hz shift
    sonicDuration: itemToMutate.sonicDuration, // Preserve sonic extension
    isSpeech: itemToMutate.isSpeech, // Preserve speech flag (if any)
    text: itemToMutate.text, // Preserve speech text (if any)
    // Store original note info for potential restoration
    originalNote: itemToMutate.note,
    originalOctave: itemToMutate.octave
  };
}

// Helper function to preserve track metadata
function preserveTrackMetadata(mutatedTrack, originalTrack) {
  if (originalTrack.mutationTriggerPosition !== undefined) {
    mutatedTrack.mutationTriggerPosition = originalTrack.mutationTriggerPosition;
  }
  if (originalTrack.mutationTriggerPositions !== undefined) {
    mutatedTrack.mutationTriggerPositions = originalTrack.mutationTriggerPositions;
  }
  if (originalTrack.currentMutationZone !== undefined) {
    mutatedTrack.currentMutationZone = originalTrack.currentMutationZone;
  }
  if (originalTrack.hasMutation !== undefined) {
    mutatedTrack.hasMutation = originalTrack.hasMutation;
  }
  if (originalTrack.originalContent !== undefined) {
    mutatedTrack.originalContent = originalTrack.originalContent;
  }
  if (originalTrack.mutationCount !== undefined) {
    mutatedTrack.mutationCount = originalTrack.mutationCount;
  }
  if (originalTrack.mutationType !== undefined) {
    mutatedTrack.mutationType = originalTrack.mutationType;
  }
}
