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
let accumulatedMovement = 0; // Accumulated pixel movement between quantized values

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
        nextNoteTime: 0,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        idealBeatTime: 0,
        actualPlayTime: 0,
        type: 'single'
      };
    } else {
      // Multiple tracks - create state for parallel playback with independent timing
      melodyState = {
        tracks: melodyTracks.tracks,
        index: 0,
        nextNoteTime: 0,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        idealBeatTime: 0,
        actualPlayTime: 0,
        type: melodyTracks.type, // 'parallel' or 'multi'
        maxLength: melodyTracks.maxLength || 0,
        // Independent timing state for each parallel track
        trackStates: melodyTracks.tracks.map((track, trackIndex) => ({
          trackIndex: trackIndex,
          noteIndex: 0,
          nextNoteTime: 0,
          idealBeatTime: 0,
          actualPlayTime: 0,
          track: track
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
  
  // Get the melody string to display (one complete cycle)
  let melodyString = "";
  let currentNoteIndex = 0;
  
  if (melodyState && melodyState.notes && melodyState.notes.length > 0) {
    currentNoteIndex = melodyState.index || 0;
    
    if (melodyState.type === 'single') {
      // Single track - build string from all notes in the melody
      melodyState.notes.forEach((noteData) => {
        if (noteData.note === 'rest') {
          melodyString += '-';
        } else {
          melodyString += noteData.note.toLowerCase();
        }
      });
    } else if (melodyState.type === 'parallel') {
      // Parallel tracks - show combined notation like "(ce)"
      for (let i = 0; i < melodyState.maxLength; i++) {
        let parallelNotes = [];
        
        melodyState.trackStates.forEach((trackState) => {
          const track = trackState.track;
          if (track && track.length > 0) {
            const trackIndex = i % track.length;
            const noteData = track[trackIndex];
            if (noteData && noteData.note !== 'rest') {
              parallelNotes.push(noteData.note.toLowerCase());
            }
          }
        });
        
        if (parallelNotes.length > 1) {
          melodyString += `(${parallelNotes.join('')})`;
        } else if (parallelNotes.length === 1) {
          melodyString += parallelNotes[0];
        } else {
          melodyString += '-';
        }
      }
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
  for (let i = 0; i < melodyString.length; i++) {
    const char = melodyString[i];
    
    // Highlight the current playing note in red, others in yellow
    if (i === currentNoteIndex) {
      coloredMelodyString += `\\red\\${char}`;
    } else {
      coloredMelodyString += `\\yellow\\${char}`;
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
        const currentNextNoteTime = melodyState.nextNoteTime;
        const currentIdealBeatTime = melodyState.idealBeatTime;
        const currentActualPlayTime = melodyState.actualPlayTime;
        
        // Update with new notes
        melodyState.notes = parsedMelody;
        
        // Preserve timing - don't reset position or timing
        melodyState.index = currentIndex;
        melodyState.nextNoteTime = currentNextNoteTime;
        melodyState.idealBeatTime = currentIdealBeatTime;
        melodyState.actualPlayTime = currentActualPlayTime;
      }
    } else {
      // Multiple tracks
      if (melodyState && (melodyState.type === 'multi' || melodyState.type === 'parallel')) {
        // Store current timing state for the main melody index
        const currentIndex = melodyState.index;
        const currentNextNoteTime = melodyState.nextNoteTime;
        const currentIdealBeatTime = melodyState.idealBeatTime;
        const currentActualPlayTime = melodyState.actualPlayTime;
        
        // Store current track states if they exist
        const currentTrackStates = melodyState.trackStates ? 
          melodyState.trackStates.map(ts => ({
            noteIndex: ts.noteIndex,
            nextNoteTime: ts.nextNoteTime,
            idealBeatTime: ts.idealBeatTime,
            actualPlayTime: ts.actualPlayTime
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
              nextNoteTime: existingState ? existingState.nextNoteTime : 0,
              idealBeatTime: existingState ? existingState.idealBeatTime : 0,
              actualPlayTime: existingState ? existingState.actualPlayTime : 0,
              track: track
            };
          });
        }
        
        // Preserve global timing
        melodyState.index = currentIndex;
        melodyState.nextNoteTime = currentNextNoteTime;
        melodyState.idealBeatTime = currentIdealBeatTime;
        melodyState.actualPlayTime = currentActualPlayTime;
        
        // Update parsedMelody for backward compatibility
        parsedMelody = melodyTracks.tracks[0];
        sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${newOctave}G` });
      }
    }
  }
}

// Realign melody timing with clock resyncs for better synchronization
function realignMelodyTiming(currentSyncedTime) {
  if (!melodyState) return;
  
  // Calculate how far we are from the next expected note
  const timeDiff = currentSyncedTime - melodyState.nextNoteTime;
  
  // If the time difference is significant (more than half a beat), realign
  const halfBeat = melodyState.baseTempo / 2;
  
  if (Math.abs(timeDiff) > halfBeat) {
    // Realign to the current synced time
    melodyState.idealBeatTime = currentSyncedTime;
    melodyState.actualPlayTime = currentSyncedTime;
    melodyState.nextNoteTime = currentSyncedTime;
    
    // For parallel tracks, realign each track's timing
    if (melodyState.type === 'parallel' && melodyState.trackStates) {
      melodyState.trackStates.forEach(trackState => {
        trackState.idealBeatTime = currentSyncedTime;
        trackState.actualPlayTime = currentSyncedTime;
        trackState.nextNoteTime = currentSyncedTime;
      });
    }
  } else if (!isLerpingToSync) {
    // Only do small adjustments if we're not in the middle of lerping
    // Small adjustment - just shift the timing slightly to stay aligned
    const adjustment = timeDiff / 4; // Gentle adjustment
    melodyState.nextNoteTime -= adjustment;
    melodyState.idealBeatTime -= adjustment;
    
    // Adjust parallel tracks as well
    if (melodyState.type === 'parallel' && melodyState.trackStates) {
      melodyState.trackStates.forEach(trackState => {
        trackState.nextNoteTime -= adjustment;
        trackState.idealBeatTime -= adjustment;
      });
    }
  }
}

function act({ event: e, clock, sound: { synth } }) {
  // Handle mouse/touch interaction for timing adjustment
  if (e.is("draw")) {
    // Accumulate pixel movement for dead zone logic
    // Negative delta.y (moving up) decreases divisor (faster timing)
    // Positive delta.y (moving down) increases divisor (slower timing)
    const pixelMovement = e.delta.y * 0.005; // Convert pixels to timing units
    accumulatedMovement += pixelMovement;
    
    // Calculate what the new divisor would be if we applied all accumulated movement
    const potentialNewDivisor = currentTimeDivisor + accumulatedMovement;
    
    // Snap to 0.1 increments
    const snappedDivisor = Math.round(potentialNewDivisor * 10) / 10;
    
    // Clamp to reasonable values (0.1s to 10.0s)
    const clampedDivisor = Math.max(0.1, Math.min(10.0, snappedDivisor));
    
    // Only update if we've crossed a 0.1 boundary (dead zone logic)
    if (Math.abs(clampedDivisor - currentTimeDivisor) >= 0.1) {
      // Calculate how much movement we actually consumed
      const actualChange = clampedDivisor - currentTimeDivisor;
      
      // Update the divisor values
      currentTimeDivisor = clampedDivisor;
      targetTimeDivisor = clampedDivisor; // Update target to match - no lerping back
      utcTriggerDivisor = clampedDivisor; // Update UTC trigger immediately
      
      // Reset accumulated movement, keeping any leftover precision
      accumulatedMovement -= actualChange;
      
      // Update timing
      baseTempo = (currentTimeDivisor * 1000) / 2;
      
      // Play a tick sound for feedback
      synth({
        type: "square",
        tone: currentTimeDivisor === 1.0 ? "6C" : "5G", // Higher pitch for 1.0, lower for others
        duration: 0.05,
        attack: 0.01,
        volume: 0.3
      });
      
      // Update melody state timing if it exists
      if (melodyState) {
        melodyState.baseTempo = baseTempo;
      }
    }
    
    // Stop any ongoing lerping when actively dragging
    isLerpingToSync = false;
  }
  
  // Reset accumulated movement when user stops dragging
  if (e.is("lift")) {
    accumulatedMovement = 0;
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

let lastTriggerTime = null; // Track the last trigger time for UTC-aligned timing
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
          decay: 0.995, // Maximum sustain - notes ring out fully
          volume: 0.8
        });
      } catch (error) {
        console.error("🎵 Error playing fallback tone:", fallbackTone, "Error:", error);
      }
      
      sequenceIndex = (sequenceIndex + 1) % (sequence?.length || 1);
      return;
    }

    if (melodyState.type === 'single') {
      // Handle single track melody (existing logic)
      if (!melodyState.notes || melodyState.notes.length === 0) {
        return;
      }
      
      const noteData = melodyState.notes[melodyState.index];
      if (!noteData) {
        return;
      }
      
      const { note, octave: noteOctave, duration, swing, swingAmount, waveType } = noteData;
      
      // Update animation timing
      lastNoteTime = performance.now();
      
      const currentTime = syncedTime ? syncedTime.getTime() : performance.now(); // Use synced time if available
      
      // Initialize timing aligned with UTC boundaries for perfect sync
      if (melodyState.idealBeatTime === 0) {
        // Calculate next UTC-aligned beat boundary based on the current tempo parameter
        const timeDivisor = currentTimeDivisor;
        const timeDivisorMs = timeDivisor * 1000;
        
        // Find the next UTC boundary for this tempo
        const currentSec = Math.floor(currentTime / 1000) * 1000;
        const currentMs = currentTime % 1000;
        const beatIndex = Math.floor(currentMs / timeDivisorMs);
        const nextBeatTime = currentSec + ((beatIndex + 1) * timeDivisorMs);
        
        melodyState.idealBeatTime = nextBeatTime;
        melodyState.actualPlayTime = nextBeatTime;
        melodyState.nextNoteTime = nextBeatTime;
        melodyState.utcBeatInterval = timeDivisorMs; // Store for future calculations
      }
      
      // Calculate note duration with proper scaling
      const baseDuration = duration * melodyState.baseTempo;
      let noteDuration = baseDuration;
      
      // Apply swing timing if present
      let playTime = melodyState.actualPlayTime;
      if (swing && swingAmount) {
        // Each swing symbol = 1/16 beat offset (62.5ms for 1-second timing)
        const swingOffset = (swingAmount * melodyState.baseTempo) / 16;
        if (swing === 'early') {
          playTime -= swingOffset;
        } else if (swing === 'late') {
          playTime += swingOffset;
        }
      }
      
      // Only play actual notes, not rests
      if (note !== 'rest') {
        // Format tone properly for the synth
        let tone;
        if (noteOctave) {
          const noteUpper = note.toUpperCase();
          tone = `${noteOctave}${noteUpper}`;
        } else {
          const noteUpper = note.toUpperCase();
          tone = `${octave}${noteUpper}`;
        }
        
        // Use consistent attack for all notes
        const attack = 0.01;
        
        try {
          sound.synth({
            type: waveType || "sine", // Use parsed waveform type or default to sine
            tone: tone,
            duration: (noteDuration / 1000), // Convert to seconds
            attack: attack,
            decay: 0.995, // Maximum sustain - notes ring out fully
            volume: 0.8
          });
          // Single clean log per note
          console.log(`%c♪ ${tone} ${waveType || 'sine'} ${(noteDuration/1000).toFixed(2)}s`, 'color: #00ff88; background: black; font-weight: bold; padding: 2px;');
        } catch (error) {
          console.error(`%c✗ ${tone} - ${error}`, 'color: red; background: black; font-weight: bold; padding: 2px;');
          // Continue without stopping the melody
        }
      }
      
      // Hybrid timing: UTC-aligned beats but note durations still affect playback
      // The beat grid stays locked to UTC, but notes can have different durations
      const nextIndex = (melodyState.index + 1) % melodyState.notes.length;
      melodyState.index = nextIndex;
      
      // Calculate next beat time based on UTC boundaries and current tempo divisor
      // This ensures tempo changes maintain sync with the UTC clock
      const timeDivisor = currentTimeDivisor;
      const timeDivisorMs = timeDivisor * 1000;
      // Reuse currentTime from above - no need to redeclare
      
      // Always calculate the next UTC-aligned beat time
      if (timeDivisor === 1.0) {
        // 1-second timing: find the next second boundary
        const nextSecBoundary = Math.ceil(currentTime / 1000) * 1000;
        melodyState.idealBeatTime = nextSecBoundary;
        melodyState.actualPlayTime = nextSecBoundary;
        melodyState.nextNoteTime = nextSecBoundary;
      } else {
        // Non-1-second timing: find the next divisor boundary
        const currentSec = Math.floor(currentTime / 1000) * 1000;
        const currentMs = currentTime % 1000;
        const nextBeatIndex = Math.floor(currentMs / timeDivisorMs) + 1;
        let nextBeatTime = currentSec + (nextBeatIndex * timeDivisorMs);
        
        // If we've passed the second boundary, start from the next second
        if (nextBeatTime >= currentSec + 1000) {
          nextBeatTime = currentSec + 1000;
        }
        
        melodyState.idealBeatTime = nextBeatTime;
        melodyState.actualPlayTime = nextBeatTime;
        melodyState.nextNoteTime = nextBeatTime;
      }
      
    } else if (melodyState.type === 'parallel') {
      // Handle parallel tracks - synchronized with main beat timing like single tracks
      const currentTime = syncedTime ? syncedTime.getTime() : performance.now(); // Use synced time if available
      
      // Update animation timing for parallel tracks
      lastNoteTime = performance.now();
      
      // Initialize timing aligned with UTC boundaries for perfect sync (same as single tracks)
      if (melodyState.idealBeatTime === 0) {
        // Calculate next UTC-aligned beat boundary based on the current tempo parameter
        const timeDivisor = currentTimeDivisor;
        const timeDivisorMs = timeDivisor * 1000;
        
        // Find the next UTC boundary for this tempo
        const currentSec = Math.floor(currentTime / 1000) * 1000;
        const currentMs = currentTime % 1000;
        const beatIndex = Math.floor(currentMs / timeDivisorMs);
        const nextBeatTime = currentSec + ((beatIndex + 1) * timeDivisorMs);
        
        melodyState.idealBeatTime = nextBeatTime;
        melodyState.actualPlayTime = nextBeatTime;
        melodyState.nextNoteTime = nextBeatTime;
        melodyState.utcBeatInterval = timeDivisorMs; // Store for future calculations
        
        // Initialize each track's timing to the same aligned time
        melodyState.trackStates.forEach((trackState, index) => {
          trackState.idealBeatTime = nextBeatTime;
          trackState.actualPlayTime = nextBeatTime;
          trackState.nextNoteTime = nextBeatTime;
        });
      }
      
      // Play all parallel tracks simultaneously, respecting individual note durations
      let maxNoteDuration = 0;
      let playingTones = []; // Collect tones for single log
      
      melodyState.trackStates.forEach((trackState, trackIndex) => {
        const track = trackState.track;
        
        if (!track || track.length === 0) return;
        
        // Get the current note for this track
        const noteData = track[trackState.noteIndex];
        
        if (noteData && noteData.note !== 'rest') {
          const { note, octave: noteOctave, duration, swing, swingAmount, waveType } = noteData;
          
          // Calculate note duration with proper scaling
          let noteDuration = duration * melodyState.baseTempo;
          
          // Apply swing timing if present
          let trackPlayTime = melodyState.actualPlayTime;
          if (swing && swingAmount) {
            // Each swing symbol = 1/16 beat offset (62.5ms for 1-second timing)
            const swingOffset = (swingAmount * melodyState.baseTempo) / 16;
            if (swing === 'early') {
              trackPlayTime -= swingOffset;
            } else if (swing === 'late') {
              trackPlayTime += swingOffset;
            }
          }
          
          // Track the longest note duration for timing the next beat
          if (noteDuration > maxNoteDuration) {
            maxNoteDuration = noteDuration;
          }
          
          let tone;
          if (noteOctave) {
            const noteUpper = note.toUpperCase();
            tone = `${noteOctave}${noteUpper}`;
          } else {
            const noteUpper = note.toUpperCase();
            tone = `${octave}${noteUpper}`;
          }
          
          // Use consistent attack for all notes
          const attack = 0.01;
          
          // Adjust volume for parallel tracks to prevent clipping
          const parallelVolume = 0.7 / melodyState.tracks.length;
          
          sound.synth({
            type: waveType || "sine", // Use parsed waveform type or default to sine
            tone: tone,
            duration: (noteDuration / 1000), // Convert to seconds - use actual note duration
            attack: attack,
            decay: 0.995, // Maximum sustain - notes ring out fully
            volume: parallelVolume
          });
          
          // Collect tone for logging
          playingTones.push(`${tone}(${waveType || 'sine'})`);
        } else if (noteData) {
          // Handle rests - still need to track duration for timing
          const noteDuration = noteData.duration * melodyState.baseTempo;
          if (noteDuration > maxNoteDuration) {
            maxNoteDuration = noteDuration;
          }
        }
        
        // Advance to next note in this track
        trackState.noteIndex = (trackState.noteIndex + 1) % track.length;
      });
      
      // Single log for all parallel tracks
      if (playingTones.length > 0) {
        console.log(`%c♪♪ ${playingTones.join('+')} ${(maxNoteDuration/1000).toFixed(2)}s`, 'color: #00ff88; background: black; font-weight: bold; padding: 2px;');
      }
      
      // Advance the main melody index for consistency
      const nextIndex = (melodyState.index + 1) % melodyState.maxLength;
      melodyState.index = nextIndex;
      
      // Calculate next beat time based on UTC boundaries and current tempo divisor (same as single tracks)
      // This ensures tempo changes maintain sync with the UTC clock
      const timeDivisor = currentTimeDivisor;
      const timeDivisorMs = timeDivisor * 1000;
      // Reuse currentTime from above - no need to redeclare
      
      // Always calculate the next UTC-aligned beat time
      if (timeDivisor === 1.0) {
        // 1-second timing: find the next second boundary
        const nextSecBoundary = Math.ceil(currentTime / 1000) * 1000;
        melodyState.idealBeatTime = nextSecBoundary;
        melodyState.actualPlayTime = nextSecBoundary;
        melodyState.nextNoteTime = nextSecBoundary;
      } else {
        // Non-1-second timing: find the next divisor boundary
        const currentSec = Math.floor(currentTime / 1000) * 1000;
        const currentMs = currentTime % 1000;
        const nextBeatIndex = Math.floor(currentMs / timeDivisorMs) + 1;
        let nextBeatTime = currentSec + (nextBeatIndex * timeDivisorMs);
        
        // If we've passed the second boundary, start from the next second
        if (nextBeatTime >= currentSec + 1000) {
          nextBeatTime = currentSec + 1000;
        }
        
        melodyState.idealBeatTime = nextBeatTime;
        melodyState.actualPlayTime = nextBeatTime;
        melodyState.nextNoteTime = nextBeatTime;
      }
      
    } else if (melodyState.type === 'multi') {
      // Handle multiple simultaneous tracks
      const currentTime = syncedTime ? syncedTime.getTime() : performance.now(); // Use synced time if available
      
      // Update animation timing for multi tracks
      lastNoteTime = performance.now();
      
      // Initialize timing if not set
      if (melodyState.idealBeatTime === 0) {
        melodyState.idealBeatTime = currentTime;
        melodyState.actualPlayTime = currentTime;
      }
      
      // Get the current track group to play
      const currentTrack = melodyState.tracks[melodyState.index];
      if (!currentTrack) {
        return;
      }
      
      if (currentTrack.type === 'simultaneous') {
        // Play all simultaneous tracks at once
        currentTrack.tracks.forEach((track, trackIndex) => {
          if (!track || track.length === 0) {
            return;
          }
          
          // For simultaneous tracks, each track is an array of notes
          // We need to play the first note of each track in the group
          const noteIndex = 0; // Always use first note for simultaneous groups
          const noteData = track[noteIndex];
          if (!noteData) {
            return;
          }
          
          const { note, octave: noteOctave, duration, swing, swingAmount, waveType } = noteData;
          
          // Calculate note duration with proper scaling
          let noteDuration = duration * melodyState.baseTempo;
          
          // Apply swing timing if present
          let trackPlayTime = melodyState.actualPlayTime;
          if (swing && swingAmount) {
            // Each swing symbol = 1/16 beat offset (62.5ms for 1-second timing)
            const swingOffset = (swingAmount * melodyState.baseTempo) / 16;
            if (swing === 'early') {
              trackPlayTime -= swingOffset;
            } else if (swing === 'late') {
              trackPlayTime += swingOffset;
            }
          }
          
          // Only play actual notes, not rests
          if (note !== 'rest') {
            let tone;
            if (noteOctave) {
              const noteUpper = note.toUpperCase();
              tone = `${noteOctave}${noteUpper}`;
            } else {
              const noteUpper = note.toUpperCase();
              tone = `${octave}${noteUpper}`;
            }
            
            // Use consistent attack for all notes
            const attack = 0.01;
            
            // Adjust volume for simultaneous notes to prevent clipping
            const simultaneousVolume = 0.6 / currentTrack.tracks.length;
            
            sound.synth({
              type: waveType || "sine", // Use parsed waveform type or default to sine
              tone: tone,
              duration: (noteDuration / 1000), // Convert to seconds - full duration
              attack: attack,
              decay: 0.995, // Maximum sustain - notes ring out fully
              volume: simultaneousVolume
            });
          }
        });
        
        // Update timing for next simultaneous group
        const baseDuration = melodyState.baseTempo; // Use base tempo for timing
        melodyState.idealBeatTime += baseDuration;
        melodyState.nextNoteTime = melodyState.idealBeatTime;
        melodyState.actualPlayTime = melodyState.idealBeatTime;
        
        // Advance to next track group
        const oldIndex = melodyState.index;
        melodyState.index = (melodyState.index + 1) % melodyState.tracks.length;
        
      } else {
        // Sequential track within multi-track structure
        const track = currentTrack.tracks[0];
        if (!track || track.length === 0) return;
        
        const noteIndex = melodyState.index % track.length;
        const noteData = track[noteIndex];
        if (!noteData) return;
        
        const { note, octave: noteOctave, duration, swing, swingAmount, waveType } = noteData;
        
        // Calculate note duration with proper scaling
        let noteDuration = duration * melodyState.baseTempo;
        
        // Apply swing timing if present
        let seqPlayTime = melodyState.actualPlayTime;
        if (swing && swingAmount) {
          // Each swing symbol = 1/16 beat offset (62.5ms for 1-second timing)
          const swingOffset = (swingAmount * melodyState.baseTempo) / 16;
          if (swing === 'early') {
            seqPlayTime -= swingOffset;
          } else if (swing === 'late') {
            seqPlayTime += swingOffset;
          }
        }
        
        // Only play actual notes, not rests
        if (note !== 'rest') {
          let tone;
          if (noteOctave) {
            const noteUpper = note.toUpperCase();
            tone = `${noteOctave}${noteUpper}`;
          } else {
            const noteUpper = note.toUpperCase();
            tone = `${octave}${noteUpper}`;
          }
          
          // Use consistent attack for all notes
          const attack = 0.01;
          
          sound.synth({
            type: waveType || "sine", // Use parsed waveform type or default to sine
            tone: tone,
            duration: (noteDuration / 1000), // Convert to seconds - full duration
            attack: attack,
            decay: 0.995, // Maximum sustain - notes ring out fully
            volume: 0.8
          });
        }
        
        // Update timing using actual note duration (respecting dots, commas, etc.)
        melodyState.idealBeatTime += noteDuration;
        melodyState.nextNoteTime = melodyState.idealBeatTime;
        melodyState.actualPlayTime = melodyState.idealBeatTime;
        
        // Advance to next track group
        melodyState.index = (melodyState.index + 1) % melodyState.tracks.length;
      }
    }
  }

  // UTC-aligned melody timing for both single and parallel tracks
  // This ensures perfect alignment with the painted clock time
  if (time) {
    const currentTimeMs = time.getTime();
    
    // Use the UTC trigger divisor for stable beat boundaries (not affected by lerping)
    const timeDivisor = utcTriggerDivisor;
    const intervalMs = timeDivisor * 1000; // Interval between notes in milliseconds
    
    // Calculate the current UTC-aligned beat position
    // For perfect synchronization, we align to absolute UTC time boundaries
    const utcBeatIndex = Math.floor(currentTimeMs / intervalMs);
    const nextBeatTime = (utcBeatIndex + 1) * intervalMs;
    const timeSinceLastBeat = currentTimeMs - (utcBeatIndex * intervalMs);
    
    // Trigger if we just crossed a beat boundary (within 100ms tolerance)
    const justCrossedBeat = timeSinceLastBeat < 100;
    const currentBeatTime = utcBeatIndex * intervalMs;
    
    // Only trigger if this is a new beat boundary
    if (justCrossedBeat && (lastTriggerTime === null || currentBeatTime > lastTriggerTime)) {
      lastTriggerTime = currentBeatTime;
      
      // Always trigger melody playback when we cross a timing boundary
      if (hasMelodyContent(melodyState)) {
        bleep(time);
      } else {
        bleep(time); // Fallback to simple bleep
      }
      
      synced = true;
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


