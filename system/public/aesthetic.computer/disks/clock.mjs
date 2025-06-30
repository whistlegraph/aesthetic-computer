// Clock, 2025.6.27.12.00
// Just a standard clock with melody support.

/* 🐢 TODO
  - [✅] Use kidlisp melody syntax for the sequence parameter.
  - [✅] Respect duration information from melody parser (dots, dashes, rests).
  - [✅] Add continuous melody timing mode that respects note durations.
  - [✅] Add sync mode that plays melody notes on UTC clock ticks.
  - [✅] Respect colon parameters for starting octave and base timing.
  - [] Allow multiple tracks / sequences to be played at once.
  - [] Incorporate sample playback and potential rewriting of the sequence
          while it is being read.
  - [] Add BPM parameter to control melody tempo.
  - [] Add time signature support for different feels.

  Usage:
    clock/melody_string                    - Continuous melody playback with 1s timing
    clock/melody_string/sync               - Melody synced to UTC clock ticks
    clock/melody_string:divisor            - Set timing divisor:
      :1     - 1 second per whole note (default) 
      :0.5   - 0.5 seconds per whole note
      :2     - 2 seconds per whole note
      :0.25  - 0.25 seconds per whole note
      (any positive number works as a time divisor)

  Base octave is 4 unless first note specifies octave (e.g., 5cdefg uses octave 5)
  Use Up/Down arrow keys to change base octave during playback
  
  🎵 NEW: Parallel Tracks Support!
  Use parentheses to create parallel melody lines that play simultaneously:
    (ceg) (dfa) - Two parallel melodies: c→e→g and d→f→a
                  Plays: c+d, then e+f, then g+a
    (cd) (ef) - Two short parallel lines: c→d and e→f  
                Plays: c+e, then d+f
    (cde) (f) - Different length tracks (longer track cycles)
                Plays: c+f, then d+f, then e+f
    (c) (d) (e) - Three single-note parallel tracks
                  Plays: c+d+e simultaneously

  🎵 NEW: Waveform Type Support!
  Use {type} syntax to set the waveform type:
    {sine} cdefg - Sine wave (default)
    {square} cdefg - Square wave
    {sawtooth} cdefg - Sawtooth wave  
    {triangle} cdefg - Triangle wave
    {square} (ceg) (dfa) - Square wave for both parallel tracks
    (ceg) ({triangle} dfa) - Sine for first track, triangle for second

  Examples:
    clock/cdefg               - Play in octave 4 with 1s timing
    clock/5cdefg              - Play in octave 5 (determined by first note)
    clock/cdefg:0.25          - Fast playback at 0.25s timing in octave 4
    clock/5d.d.edgf#-:2       - Play at 2s timing with octave 5
    clock/c<defg<<ab:0.5      - Play with swing timing at 0.5s
    clock/{square} cdefg      - Square wave melody
    clock/(ceg) (dfa)         - Parallel melodies: c+d, e+f, g+a
    clock/{triangle} (cd) (ef) (ga)  - Three parallel tracks with triangle wave
    clock/{square} (ceg) ({sawtooth} dfa)  - Mixed waveforms

  - [] Figure out what the timeslice should be.
  - [-] Add a button to 'learn' the clock.
  - [✅] Automatically make make the font scale 1 if the text is too wide for screen.width.  
       (each character is 6 pixels wide, so if the text is longer than screen.width / 8, scale it down)
 */

import { parseMelody, extractTones, parseSimultaneousMelody } from "../lib/melody-parser.mjs";

// Utility function for padding numbers with zeros
function pad(num, size = 2) {
  num = num.toString();
  while (num.length < size) num = "0" + num;
  return num;
}

let synced = false;
let sequence = [],
  sequenceIndex = 0;
let parsedMelody = [];
let octave = 4;
let originalMelodyString = ""; // Store the original melody string for reparsing

// Enhanced melody state for simultaneous tracks
let melodyTracks = null; // Will store parsed simultaneous tracks
let melodyState = null;
let baseTempo = 500; // Default 500ms per quarter note (120 BPM)
let melodyMode = "continuous"; // "continuous" or "sync" - how to time the melody
let targetTempo = 500; // Target tempo for smooth transitions
let isLerpingToSync = false; // Whether we're lerping back to sync
let originalTimeDivisor = 1.0; // Store the original time divisor for reference
let currentTimeDivisor = 1.0; // Current effective time divisor (changes with slider)
let targetTimeDivisor = 1.0; // Target divisor for lerping back to original
let utcTriggerDivisor = 1.0; // Divisor used for UTC triggering (stable during lerping)
let lastNoteTime = 0; // Time when the last note was triggered
let animationProgress = 0; // Progress towards next note (0 to 1)
let specialCharFlashTime = 0; // Time when special character was processed (for green flash)

function boot({ ui, clock, params, colon }) {
  // Get the UTC offset from /api/clock and use that to set the clock.
  clock.resync();
  
  // Set default octave to 4 (removed from colon parameter)
  octave = 4;
  
  // Set base tempo using colon[0] as a divisor (e.g., :1 = 1 second, :0.5 = 0.5 seconds)
  // Since default note duration is 2, we need to divide by 2 to get the correct whole note timing
  const timeDivisor = parseFloat(colon[0]) || 1.0;  // Default to 1 second if no colon param
  originalTimeDivisor = timeDivisor; // Store for later reference
  currentTimeDivisor = timeDivisor; // Initialize current divisor to original
  targetTimeDivisor = timeDivisor; // Initialize target divisor to original
  utcTriggerDivisor = timeDivisor; // Initialize UTC trigger divisor to original
  baseTempo = (timeDivisor * 1000) / 2;  // Divide by 2 because default duration is 2
  targetTempo = baseTempo; // Set target tempo to initial value
  isLerpingToSync = false; // Reset lerping state
  
  if (params[0]) {
    // Concatenate all params to handle cases like clock/(ceg) (dfa) where
    // (ceg) is in params[0] and (dfa) is in params[1]
    originalMelodyString = params.join(' ');
    
    // Parse the melody string for simultaneous tracks
    melodyTracks = parseSimultaneousMelody(originalMelodyString, octave);
    
    if (melodyTracks.isSingleTrack) {
      // Single track - use existing logic
      parsedMelody = melodyTracks.tracks[0];
      
      // Check if the first note has an explicit octave, if so use that as the base octave
      if (parsedMelody.length > 0 && parsedMelody[0].octave !== octave) {
        octave = parsedMelody[0].octave;
      }
      
      // Extract tone strings for the sequence, using the determined octave as default
      sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${octave}G` });
      
      // Initialize melody timing state
      melodyState = {
        notes: parsedMelody,
        index: 0,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        type: 'single'
      };
    } else {
      // Multiple tracks - create state for parallel playback with independent timing
      melodyState = {
        tracks: melodyTracks.tracks,
        index: 0,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        type: melodyTracks.type, // 'parallel' or 'multi'
        maxLength: melodyTracks.maxLength || 0,
        cycleStartTime: 0, // Shared cycle timing for parallel tracks
        // Independent timing state for each parallel track
        trackStates: melodyTracks.tracks.map((track, trackIndex) => ({
          trackIndex: trackIndex,
          noteIndex: 0,
          track: track,
          nextNoteTargetTime: 0 // Each track has its own target time
        }))
      };
      
      // For backward compatibility, set parsedMelody to the first track
      parsedMelody = melodyTracks.tracks[0];
      sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${octave}G` });
    }
  }
  
  // Check for melody mode in params
  if (params[1] === "sync") {
    melodyMode = "sync"; // Sync melody to UTC clock
  } else {
    melodyMode = "continuous"; // Play melody continuously based on timing mode
  }
}

function paint({ wipe, ink, write, clock, screen, sound, api, help }) {
  const syncedDate = clock.time(); // Get time once at the beginning
  let bgColor;
  let currentSeconds; // To store seconds if syncedDate is valid

  if (syncedDate) {
    currentSeconds = syncedDate.getSeconds(); // Get seconds if time is valid
  }

  if (synced) {
    bgColor = "cyan";
  } else {
    if (syncedDate) {
      const lastDigitOfSecond = currentSeconds % 10;
      // Define colors based on ROYGBIV spectrum plus black, white, and gray
      const colors = [
        "red", // Second ends in 0
        "orange", // Second ends in 1
        "yellow", // Second ends in 2
        "green", // Second ends in 3
        "blue", // Second ends in 4
        "indigo", // Second ends in 5
        "violet", // Second ends in 6
        "black", // Second ends in 7
        "white", // Second ends in 8
        "gray", // Second ends in 9
      ];
      bgColor = colors[lastDigitOfSecond];
    } else {
      bgColor = "purple"; // Fallback if syncedDate is not available
    }
  }

  wipe(bgColor);

  if (synced) {
    synced = false; // Reset synced flag after using it for wipe color.
  }

  const availableWidth = screen.width;
  const availableHeight = screen.height;

  sound.paint.bars(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 32),
    0,
    0,
    availableWidth,
    availableHeight,
    [255, 255, 0, 255],
    {
      noamp: true,
      nobounds: true,
      primaryColor: [255, 255, 0, 128],
      secondaryColor: [0, 0, 0, 128],
    },
  );

  sound.paint.waveform(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 32),
    0,
    0,
    availableWidth,
    availableHeight,
    [255, 255, 0, 255],
  );

  // Making a digital clock display.
  if (syncedDate) {
    const morning = syncedDate.getHours() < 12;
    let hours = syncedDate.getHours();
    hours = hours % 12;
    hours = hours ? hours : 12; // the hour '0' should be '12'
    const minutes = pad(syncedDate.getMinutes());
    const displaySeconds = pad(currentSeconds); // Use currentSeconds from above and pad it
    const millis = pad(syncedDate.getMilliseconds(), 3);
    const ampm = morning ? "AM" : "PM";

    const timeString =
      hours + ":" + minutes + ":" + displaySeconds + ":" + millis + " " + ampm;

    let fontSize = 1; // Default size

    // if (timeString.length * 6 < screen.width) {
    //   fontSize = 1;
    // } else if (timeString.length * 6 < screen.width * 0.8) {
    // }

    ink("red").line(0, screen.height / 2, 8 + 5, screen.height / 2);
    ink("red").line(
      screen.width - 9,
      screen.height / 2,
      screen.width,
      screen.height / 2,
    );
    ink("white").write(timeString, { center: "xy", size: fontSize }, "black");
    
    // Display the whole note time (baseTempo) in bottom left corner
    const wholeNoteTime = (baseTempo * 2) / 1000; // Convert to seconds (multiply by 2 since default duration is 2)
    const wholeText = screen.width < 200 ? `W:${wholeNoteTime.toFixed(2)}s` : `WHOLE: ${wholeNoteTime.toFixed(3)}s`;
    ink("cyan").write(wholeText, { 
      x: 8, // Position from left edge
      y: screen.height - 16,  // Position from bottom edge
      scale: 1
    });
    
    // Display the base octave in red at bottom right
    const octaveText = screen.width < 200 ? `O${octave}` : `OCT ${octave}`;
    const octaveX = screen.width < 200 ? screen.width - 24 : screen.width - 48;
    ink("red").write(octaveText, { 
      x: octaveX, // Position from right edge
      y: screen.height - 16,  // Position from bottom edge
      scale: 1
    });
    
    // Draw melody timeline if we have a melody
    if (hasMelodyContent(melodyState)) {
      drawMelodyTimeline(ink, write, screen, melodyState);
    }
    
    // Draw timing graph on the right side
    drawTimingGraph(ink, write, screen);
  } else {
    ink("red").write("SYNCING...", { center: "xy", size: 2 });
    
    // Display the whole note time even when syncing
    const wholeNoteTime = (baseTempo * 2) / 1000;
    const wholeText = screen.width < 200 ? `W:${wholeNoteTime.toFixed(2)}s` : `WHOLE: ${wholeNoteTime.toFixed(3)}s`;
    ink("cyan").write(wholeText, { 
      x: 8,
      y: screen.height - 16,
      scale: 1
    });
    
    // Display the base octave even when syncing
    const octaveText = screen.width < 200 ? `O${octave}` : `OCT ${octave}`;
    const octaveX = screen.width < 200 ? screen.width - 24 : screen.width - 48;
    ink("red").write(octaveText, { 
      x: octaveX,
      y: screen.height - 16,
      scale: 1
    });
    
    // Draw melody timeline even when syncing if we have a melody
    if (hasMelodyContent(melodyState)) {
      drawMelodyTimeline(ink, write, screen, melodyState);
    }
    
    // Draw timing graph even when syncing
    drawTimingGraph(ink, write, screen);
  }
}

// Draw a single version of the melody string with the active note highlighted
function drawMelodyTimeline(ink, write, screen, melodyState) {
  const timelineY = screen.height - 40; // More space above info text
  const timelineStartX = 16;
  const timelineEndX = screen.width - 16;
  const timelineWidth = timelineEndX - timelineStartX;
  
  // Get the melody string to display (use original input)
  let melodyString = "";
  let currentNoteIndex = 0;
  
  if (melodyState && originalMelodyString) {
    // Use the original melody string exactly as typed
    melodyString = originalMelodyString;
    
    // Calculate current position based on melody type
    if (melodyState.type === 'single') {
      const totalNotes = melodyState.notes.length;
      // Since melodyState.index is incremented after playing, we need the previous note
      currentNoteIndex = (melodyState.index - 1 + totalNotes) % totalNotes;
    } else if (melodyState.type === 'parallel') {
      // For parallel tracks, use the main melody index
      const totalBeats = melodyState.maxLength;
      currentNoteIndex = melodyState.index % totalBeats;
    }
  } else {
    // No melody - show a default pattern
    melodyString = "cdefgab";
    currentNoteIndex = 0;
  }
  
  // Center the melody string horizontally
  const stringWidth = melodyString.length * 6; // Each character is 6 pixels wide
  const startX = Math.max(timelineStartX, (screen.width - stringWidth) / 2);
  
  // Build the melody string with inline color codes
  let coloredMelodyString = "";
  
  // Check if we should flash green for special characters
  const now = performance.now();
  const flashDuration = 200; // 200ms flash
  const shouldFlashGreen = (now - specialCharFlashTime) < flashDuration;
  
  if (melodyState && originalMelodyString) {
    // Create a mapping from string positions to notes for proper highlighting
    let noteCharPositions = []; // Array of {charIndex, noteIndex} for note characters
    let charIndex = 0;
    let noteIndex = 0;
    
    // First pass: identify all note character positions and their duration modifiers
    // Handle both single tracks and parallel tracks with groups
    let inGroup = false;
    let currentTrackIndex = 0;
    let noteIndexInCurrentTrack = 0;
    
    for (let i = 0; i < melodyString.length; i++) {
      const char = melodyString[i];
      
      if (char === '(') {
        inGroup = true;
        continue;
      } else if (char === ')') {
        inGroup = false;
        currentTrackIndex++;
        noteIndexInCurrentTrack = 0;
        continue;
      } else if (char === ' ') {
        // Skip spaces - track index is managed by parentheses
        continue;
      }
      
      if (/[a-g#b-]/i.test(char)) {
        // This is a note character - mark it and any following duration modifiers
        let noteEnd = i;
        
        // Look ahead for duration modifiers (dots, dashes)
        while (noteEnd + 1 < melodyString.length) {
          const nextChar = melodyString[noteEnd + 1];
          if (nextChar === '.' || nextChar === '-' || nextChar === '<' || nextChar === '>') {
            noteEnd++;
          } else {
            break;
          }
        }
        
        // Mark all characters from i to noteEnd as part of this note
        // For parallel tracks, use the note index within the current track
        // For single tracks, use the global note index
        const noteIndexToUse = melodyState.type === 'parallel' ? noteIndexInCurrentTrack : noteIndex;
        
        for (let j = i; j <= noteEnd; j++) {
          noteCharPositions.push({ 
            charIndex: j, 
            noteIndex: noteIndexToUse,
            trackIndex: currentTrackIndex // Store track info for parallel tracks
          });
        }
        
        noteIndex++; // Global note counter
        noteIndexInCurrentTrack++; // Track-specific note counter
        i = noteEnd; // Skip ahead to avoid reprocessing duration modifiers
      }
    }
    
    // Color the original melody string character by character
    for (let i = 0; i < melodyString.length; i++) {
      const char = melodyString[i];
      let color = "yellow"; // Default color
      
      // Check for special characters that should flash green
      if (shouldFlashGreen && (char === '+' || char === '{' || char === '}')) {
        color = "green";
      }
      // Check if this is a note character that should be highlighted red
      else if (melodyState.type === 'single') {
        // Find if this character position corresponds to a note
        const noteCharData = noteCharPositions.find(ncp => ncp.charIndex === i);
        if (noteCharData) {
          // This is a note character, check if it's the current one
          if (noteCharData.noteIndex === currentNoteIndex) {
            color = "red";
          }
        }
      }
      // For parallel tracks, highlight based on current beat position
      else if (melodyState.type === 'parallel') {
        // For parallel tracks, check if this character is part of the currently playing note
        const noteCharData = noteCharPositions.find(ncp => ncp.charIndex === i);
        if (noteCharData && noteCharData.trackIndex < melodyState.trackStates.length) {
          const trackState = melodyState.trackStates[noteCharData.trackIndex];
          // Check if this note is the currently playing note in this track
          // With the new cycle-based timing, trackState.noteIndex directly points to the current note
          if (noteCharData.noteIndex === trackState.noteIndex) {
            color = "red";
          }
        }
      }
      
      coloredMelodyString += `\\${color}\\${char}`;
    }
  } else {
    // For default melody or no melody state
    for (let i = 0; i < melodyString.length; i++) {
      const char = melodyString[i];
      
      // Highlight the current playing note in red, others in yellow
      if (i === currentNoteIndex) {
        coloredMelodyString += `\\red\\${char}`;
      } else {
        coloredMelodyString += `\\yellow\\${char}`;
      }
    }
  }
  
  // Draw the entire melody string with inline colors in one call
  write(coloredMelodyString, {
    x: startX,
    y: timelineY,
    scale: 1
  });
  
  // Draw timing info below the melody string
  const infoY = timelineY + 12;
  
  // Show current timing divisor
  const timeDivisor = currentTimeDivisor;
  const timingText = screen.width < 300 ? 
    `${timeDivisor.toFixed(1)}s` :
    `${timeDivisor.toFixed(1)}s timing`;
  
  ink("cyan").write(timingText, { 
    x: timelineStartX, 
    y: infoY,
    scale: 1
  });
}

// Helper function to determine if a note character is playing in parallel tracks
function isNotePlayingInParallelTrack(melodyString, charIndex, trackStates) {
  // Parse the parallel structure to find which track this character belongs to
  let currentTrackIndex = 0;
  let noteIndexInTrack = 0;
  let inGroup = false;
  
  // Find which track the character at charIndex belongs to
  for (let i = 0; i < melodyString.length; i++) {
    const char = melodyString[i];
    
    if (char === '(') {
      inGroup = true;
      continue;
    } else if (char === ')') {
      inGroup = false;
      currentTrackIndex++;
      noteIndexInTrack = 0;
      continue;
    } else if (char === ' ') {
      // Skip spaces - track index is managed by parentheses
      continue;
    }
    
    if (i === charIndex) {
      // Found the character we're checking
      if (currentTrackIndex < trackStates.length) {
        const trackState = trackStates[currentTrackIndex];
        // Check if this note is the currently playing note in this track
        // Since trackState.noteIndex points to the next note to play after incrementing,
        // we need to check against the previous note (with proper wrapping)
        const currentPlayingIndex = (trackState.noteIndex - 1 + trackState.track.length) % trackState.track.length;
        return noteIndexInTrack === currentPlayingIndex;
      }
      return false;
    }
    
    // Count notes, treating duration-modified notes as single units
    if (/[a-g#b-]/i.test(char)) {
      // Skip ahead past any duration modifiers
      let j = i;
      while (j + 1 < melodyString.length) {
        const nextChar = melodyString[j + 1];
        if (nextChar === '.' || nextChar === '-' || nextChar === '<' || nextChar === '>') {
          j++;
        } else {
          break;
        }
      }
      
      // If we're checking a character within this note unit, it belongs to the current note
      if (charIndex >= i && charIndex <= j) {
        if (currentTrackIndex < trackStates.length) {
          const trackState = trackStates[currentTrackIndex];
          const currentPlayingIndex = (trackState.noteIndex - 1 + trackState.track.length) % trackState.track.length;
          return noteIndexInTrack === currentPlayingIndex;
        }
        return false;
      }
      
      noteIndexInTrack++;
      i = j; // Skip ahead to avoid reprocessing duration modifiers
    }
  }
  
  return false;
}

// Draw a timing graph on the right side with measurement lines
function drawTimingGraph(ink, write, screen) {
  const graphWidth = 40;
  const graphStartX = screen.width - graphWidth - 8;
  const graphEndX = screen.width - 8;
  const graphStartY = 60;
  const graphEndY = screen.height - 60;
  const graphHeight = graphEndY - graphStartY;
  
  // Draw the main vertical line
  ink("gray").line(graphStartX, graphStartY, graphStartX, graphEndY);
  
  // Define the range of timing values (0.1s to 5.0s for better visibility)
  const minTiming = 0.1;
  const maxTiming = 5.0;
  const range = maxTiming - minTiming;
  
  // Draw measurement lines and labels for each 0.1 increment
  for (let timing = minTiming; timing <= maxTiming; timing += 0.1) {
    const normalizedPos = (timing - minTiming) / range;
    const yPos = graphEndY - (normalizedPos * graphHeight);
    
    // Draw tick marks
    let tickLength = 4; // Small ticks for 0.1 increments
    let color = "darkgray";
    
    // Larger ticks and brighter color for major marks (whole seconds)
    if (Math.abs(timing % 1.0) < 0.05) {
      tickLength = 8;
      color = "white";
    }
    // Medium ticks for half seconds
    else if (Math.abs(timing % 0.5) < 0.05) {
      tickLength = 6;
      color = "lightgray";
    }
    
    ink(color).line(graphStartX, yPos, graphStartX + tickLength, yPos);
    
    // Draw labels for major marks only
    if (Math.abs(timing % 1.0) < 0.05) {
      const label = timing.toFixed(0);
      ink("white").write(label, {
        x: graphStartX + tickLength + 2,
        y: yPos - 4,
        scale: 1
      });
    }
  }
  
  // Draw current timing indicator
  const currentNormalizedPos = Math.max(0, Math.min(1, (currentTimeDivisor - minTiming) / range));
  const currentYPos = graphEndY - (currentNormalizedPos * graphHeight);
  
  // Draw a bright indicator line for current timing
  ink("cyan").line(graphStartX - 4, currentYPos, graphStartX + 12, currentYPos);
  
  // Draw current timing value
  const currentLabel = currentTimeDivisor.toFixed(1);
  ink("cyan").write(currentLabel, {
    x: graphStartX + 14,
    y: currentYPos - 4,
    scale: 1
  });
  
  // Highlight 1.0s timing with special color
  if (Math.abs(currentTimeDivisor - 1.0) < 0.05) {
    ink("yellow").line(graphStartX - 4, currentYPos, graphStartX + 12, currentYPos);
    ink("yellow").write(currentLabel, {
      x: graphStartX + 14,
      y: currentYPos - 4,
      scale: 1
    });
  }
  
  // Draw graph title
  ink("white").write("TIMING", {
    x: graphStartX - 6,
    y: graphStartY - 12,
    scale: 1
  });
}

// 📚 Library

// Helper function to check if melody state has content
function hasMelodyContent(melodyState) {
  if (!melodyState) return false;
  
  if (melodyState.type === 'single') {
    return melodyState.notes && melodyState.notes.length > 0;
  } else if (melodyState.type === 'parallel') {
    return melodyState.tracks && melodyState.tracks.length > 0;
  } else if (melodyState.type === 'multi') {
    return melodyState.tracks && melodyState.tracks.length > 0;
  }
  
  return false;
}

function reparseMelodyWithNewOctave(newOctave) {
  if (originalMelodyString) {
    // Reparse the melody with the new base octave
    melodyTracks = parseSimultaneousMelody(originalMelodyString, newOctave);
    
    if (melodyTracks.isSingleTrack) {
      // Single track
      parsedMelody = melodyTracks.tracks[0];
      sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${newOctave}G` });
      
      // Update melody state if it exists, preserving timing and position
      if (melodyState && melodyState.type === 'single') {
        // Store current timing state
        const currentIndex = melodyState.index;
        
        // Update with new notes
        melodyState.notes = parsedMelody;
        
        // Preserve timing - don't reset position
        melodyState.index = currentIndex;
        
        // Important: Don't reset nextNoteTargetTime - let playback continue seamlessly
      }
    } else {
      // Multiple tracks
      if (melodyState && (melodyState.type === 'multi' || melodyState.type === 'parallel')) {
        // Store current timing state for the main melody index
        const currentIndex = melodyState.index;
        
        // Store current track states if they exist, including timing
        const currentTrackStates = melodyState.trackStates ? 
          melodyState.trackStates.map(ts => ({
            noteIndex: ts.noteIndex,
            nextNoteTargetTime: ts.nextNoteTargetTime // Preserve timing!
          })) : null;
        
        // Update with new parsed tracks
        melodyState.tracks = melodyTracks.tracks;
        melodyState.type = melodyTracks.type;
        melodyState.maxLength = melodyTracks.maxLength || 0;
        
        // Update or create track states for parallel timing
        if (melodyState.type === 'parallel') {
          melodyState.trackStates = melodyTracks.tracks.map((track, trackIndex) => {
            // Try to preserve timing from existing track state
            const existingState = currentTrackStates && currentTrackStates[trackIndex];
            return {
              trackIndex: trackIndex,
              noteIndex: existingState ? existingState.noteIndex : 0,
              track: track,
              nextNoteTargetTime: existingState ? existingState.nextNoteTargetTime : 0 // Preserve timing!
            };
          });
        }
        
        // Preserve global timing
        melodyState.index = currentIndex;
        
        // Update parsedMelody for backward compatibility
        parsedMelody = melodyTracks.tracks[0];
        sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${newOctave}G` });
      }
    }
  }
}

// Simple timing adjustment - no longer needed with direct UTC checking
function realignMelodyTiming(currentSyncedTime) {
  // No-op - we now check UTC time directly
}

function act({ event: e, clock, sound: { synth } }) {
  // Handle mouse/touch interaction for timing adjustment
  if (e.is("draw")) {
    // Direct pixel-by-pixel adjustment
    // Negative delta.y (moving up) decreases divisor (faster timing)
    // Positive delta.y (moving down) increases divisor (slower timing)
    const pixelMovement = e.delta.y * 0.01; // More sensitive pixel-to-timing conversion
    
    // Apply movement directly to current divisor
    const newDivisor = currentTimeDivisor + pixelMovement;
    
    // Clamp to reasonable values (0.1s to 10.0s)
    const clampedDivisor = Math.max(0.1, Math.min(10.0, newDivisor));
    
    // Update if there's any change (pixel-by-pixel)
    if (Math.abs(clampedDivisor - currentTimeDivisor) > 0.001) {
      // Update the divisor values immediately
      currentTimeDivisor = clampedDivisor;
      targetTimeDivisor = clampedDivisor; // Update target to match - no lerping back
      utcTriggerDivisor = clampedDivisor; // Update UTC trigger immediately
      
      // Update timing
      baseTempo = (currentTimeDivisor * 1000) / 2;
      
      // Play a subtle tick sound for each pixel movement
      synth({
        type: "square",
        tone: currentTimeDivisor === 1.0 ? "7C" : "6G", // Higher pitch for feedback
        duration: 0.03, // Very short tick
        attack: 0.005,
        volume: 0.15 // Quieter since it plays more frequently
      });
      
      // Update melody state timing if it exists
      if (melodyState) {
        melodyState.baseTempo = baseTempo;
      }
    }
    
    // Stop any ongoing lerping when actively dragging
    isLerpingToSync = false;
  }
  
  // Reset any drag state when user stops dragging
  if (e.is("lift")) {
    // No need to reset accumulated movement since we're doing direct pixel adjustment
  }
  // Remove the lerp-back behavior - timing stays where user sets it

  // Handle keyboard input for octave changes
  if (e.is("keyboard:down:arrowup")) {
    // Increase octave (max 8)
    if (octave < 8) {
      octave++;
      
      // Reparse melody with new octave
      reparseMelodyWithNewOctave(octave);
      
      // Play a preview note at the new octave
      // synth({
      //   type: "sine",
      //   tone: `${octave}C`,
      //   duration: 0.1,
      //   volume: 0.5
      // });
    }
  }

  if (e.is("keyboard:down:arrowdown")) {
    // Decrease octave (min 1)
    if (octave > 1) {
      octave--;
      
      // Reparse melody with new octave
      reparseMelodyWithNewOctave(octave);
      
      // Play a preview note at the new octave
      // synth({
      //   type: "sine",
      //   tone: `${octave}C`,
      //   duration: 0.1,
      //   volume: 0.5
      // });
    }
  }
  
  // Respond to user input here.
  if (e.is("touch")) {
    // clock.resync();
    // synth();
  }
}

let lastNoteStartTime = 0; // When the last note started playing (UTC time)
let lastNoteDuration = 0; // Duration of the last note in milliseconds
let nextNoteTargetTime = 0; // When the next note should play (UTC time)
let lastResyncTime = 0; // Track when we last resynced
const RESYNC_INTERVAL = 3000; // Resync every 3 seconds

function sim({ sound, beep, clock, num, help, params, colon }) {
  sound.speaker?.poll();

  // Get the current synced time
  const syncedDate = clock.time();
  
  // Periodically resync the clock to stay accurate
  const now = performance.now();
  if (now - lastResyncTime > RESYNC_INTERVAL) {
    try {
      clock.resync();
      lastResyncTime = now;
      
      // Realign melody timing with the newly synced clock
      if (melodyState && syncedDate) {
        const newSyncedTime = syncedDate.getTime();
        realignMelodyTiming(newSyncedTime);
      }
    } catch (error) {
      console.log('%cclock unsynced', 'color: orange; background: black; padding: 2px;');
      lastResyncTime = now; // Still update to avoid spamming the error
    }
  }
  
  // Update melody playback if we have a melody and synced time is available
  // NOTE: Melody timing is now handled by the UTC-aligned system below for perfect sync

  // Runs once per logic frame. (120fps locked.)
  // Get the current time and beep at different intervals
  const time = clock.time();

  if (!time) return;

  const seconds = time.getSeconds();
  const milliseconds = time.getMilliseconds();

  function bleep(syncedTime) {
    if (!melodyState) {
      // Fallback to simple sequence if no melody state
      const fallbackTone = sequence?.[sequenceIndex] || `${octave}${help.choose("G", "B", "D")}`;
      
      try {
        sound.synth({
          type: "sine",
          tone: fallbackTone,
          duration: 0.1,
          attack: 0.01,
          decay: 0.995,
          volume: 0.8
        });
      } catch (error) {
        console.error("🎵 Error playing fallback tone:", fallbackTone, "Error:", error);
      }
      
      sequenceIndex = (sequenceIndex + 1) % (sequence?.length || 1);
      return;
    }

    const currentTime = syncedTime ? syncedTime.getTime() : performance.now();

    // Only handle single track melodies here - parallel tracks are handled in sim()
    if (melodyState.type === 'single') {
      if (!melodyState.notes || melodyState.notes.length === 0) return;
      
      const noteData = melodyState.notes[melodyState.index];
      if (!noteData) return;
      
      const { note, octave: noteOctave, duration, swing, swingAmount, waveType } = noteData;
      
      // Play the note
      if (note !== 'rest') {
        let tone = noteOctave ? `${noteOctave}${note.toUpperCase()}` : `${octave}${note.toUpperCase()}`;
        
        try {
          sound.synth({
            type: waveType || "sine",
            tone: tone,
            duration: (duration * melodyState.baseTempo) / 1000,
            attack: 0.01,
            decay: 0.995,
            volume: 0.8
          });
          console.log(`%c♪ ${tone} ${waveType || 'sine'} ${duration}`, 'color: #00ff88; background: black; font-weight: bold; padding: 2px;');
        } catch (error) {
          console.error(`%c✗ ${tone} - ${error}`, 'color: red; background: black; font-weight: bold; padding: 2px;');
        }
      }
      
      if (waveType || swing) {
        specialCharFlashTime = performance.now();
      }
      
      // Update timing tracking
      lastNoteStartTime = currentTime;
      lastNoteDuration = duration * melodyState.baseTempo;
      
      // Set target time for next note
      nextNoteTargetTime = currentTime + lastNoteDuration;
      
      // Advance to next note
      melodyState.index = (melodyState.index + 1) % melodyState.notes.length;
    }
  }

  // Super simple melody timing - check against target time
  if (time) {
    const currentTimeMs = time.getTime();
    
    if (hasMelodyContent(melodyState)) {
      if (melodyState.type === 'single') {
        // Single track timing
        if (nextNoteTargetTime === 0) {
          // First note - target next UTC second boundary
          nextNoteTargetTime = Math.ceil(currentTimeMs / 1000) * 1000;
        }
        
        // Check if it's time to play the next note
        if (currentTimeMs >= nextNoteTargetTime) {
          bleep(time);
          synced = true;
        }
      } else if (melodyState.type === 'parallel') {
        // Parallel tracks - use shared cycle timing to prevent drift
        let anyTrackPlayed = false;
        
        // Initialize shared cycle start time if not set
        if (!melodyState.cycleStartTime) {
          melodyState.cycleStartTime = Math.ceil(currentTimeMs / 1000) * 1000;
        }
        
        // Calculate shared cycle position
        const timeSinceCycleStart = currentTimeMs - melodyState.cycleStartTime;
        const cycleDurationMs = melodyState.baseTempo; // One beat duration
        
        melodyState.trackStates.forEach((trackState) => {
          const track = trackState.track;
          if (!track || track.length === 0) return;
          
          // Calculate total duration of this track in beats
          const trackTotalDuration = track.reduce((sum, note) => sum + note.duration, 0);
          const trackCycleDurationMs = trackTotalDuration * melodyState.baseTempo;
          
          // Calculate where we are in this track's cycle
          const trackCycleTime = timeSinceCycleStart % trackCycleDurationMs;
          
          // Find which note should be playing based on cycle position
          let accumulatedTime = 0;
          let currentNoteIndex = 0;
          let noteStartTime = 0;
          
          for (let i = 0; i < track.length; i++) {
            const noteDuration = track[i].duration * melodyState.baseTempo;
            if (trackCycleTime >= accumulatedTime && trackCycleTime < accumulatedTime + noteDuration) {
              currentNoteIndex = i;
              noteStartTime = accumulatedTime;
              break;
            }
            accumulatedTime += noteDuration;
          }
          
          // Check if we need to play this note (just started)
          const noteData = track[currentNoteIndex];
          const timeSinceNoteStart = trackCycleTime - noteStartTime;
          
          // Play if we just started this note (within a small window)
          if (timeSinceNoteStart < 50 && trackState.noteIndex !== currentNoteIndex) {
            if (noteData && noteData.note !== 'rest') {
              const { note, octave: noteOctave, duration, waveType } = noteData;
              let tone = noteOctave ? `${noteOctave}${note.toUpperCase()}` : `${octave}${note.toUpperCase()}`;
              
              try {
                sound.synth({
                  type: waveType || "sine",
                  tone: tone,
                  duration: (duration * melodyState.baseTempo) / 1000,
                  attack: 0.01,
                  decay: 0.995,
                  volume: 0.7 / melodyState.tracks.length
                });
                console.log(`%c♪ Track${trackState.trackIndex}: ${tone} ${waveType || 'sine'} ${duration}`, 'color: #00ff88; background: black; font-weight: bold; padding: 2px;');
              } catch (error) {
                console.error(`%c✗ Track${trackState.trackIndex}: ${tone} - ${error}`, 'color: red; background: black; font-weight: bold; padding: 2px;');
              }
              
              if (waveType) {
                specialCharFlashTime = performance.now();
              }
            }
            
            anyTrackPlayed = true;
          }
          
          // Update track state
          trackState.noteIndex = currentNoteIndex;
        });
        
        if (anyTrackPlayed) {
          synced = true;
        }
      }
    } else {
      // No melody - use UTC boundaries for simple bleep
      const timeDivisor = utcTriggerDivisor;
      const intervalMs = timeDivisor * 1000;
      const utcBeatIndex = Math.floor(currentTimeMs / intervalMs);
      const timeSinceLastBeat = currentTimeMs - (utcBeatIndex * intervalMs);
      
      // Trigger if we just crossed a beat boundary
      if (timeSinceLastBeat < 50 && (lastNoteStartTime === 0 || 
          currentTimeMs > lastNoteStartTime + 100)) {
        lastNoteStartTime = currentTimeMs;
        bleep(time);
        synced = true;
      }
    }
  }
}

// function beat() {
//   // Runs once per system metronome (BPM) tick.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }


