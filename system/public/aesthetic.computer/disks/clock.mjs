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
    x(ceg) (dfa) - Disable first track with x() - only second track plays

  🎵 NEW: Waveform Type & Volume Support!
  Use {type} or {type:volume} or {volume} syntax:
    {sine} cdefg - Sine wave (default)
    {square} cdefg - Square wave
    {sawtooth} cdefg - Sawtooth wave  
    {triangle} cdefg - Triangle wave
    {0.5} cdefg - Set volume to 50% (keeps current waveform)
    {square:0.3} cdefg - Square wave at 30% volume
    {0.8} c{0.2}d{1.0}e - Volume changes during melody
    {square} (ceg) (dfa) - Square wave for both parallel tracks
    (ceg) ({triangle:0.6} dfa) - Sine for first track, triangle at 60% for second
  
  🎵 NEW: Global Duration Modifiers!
  Use {...} {..} {.} {,} {,,} to set default durations for all following notes:
    {...} cdefg - All notes become very short (thirty-second notes)
    {..} cdefg - All notes become short (sixteenth notes)  
    {.} cdefg - All notes become eighth notes
    {,} cdefg - All notes become half notes (longer)
    {,,} cdefg - All notes become whole notes (very long)
    {..} c d. e f,, - Global applies to c and e; d and f use local modifiers

  Examples:
    clock/cdefg               - Play in octave 4 with 1s timing
    clock/5cdefg              - Play in octave 5 (determined by first note)
    clock/cdefg:0.25          - Fast playback at 0.25s timing in octave 4
    clock/5d.d.edgf#-:2       - Play at 2s timing with octave 5
    clock/c<defg<<ab:0.5      - Play with swing timing at 0.5s
    clock/{square} cdefg      - Square wave melody
    clock/{...} cdefg         - Very short staccato notes
    clock/{,,} cdefg          - Long sustained notes
    clock/(ceg) (dfa)         - Parallel melodies: c+d, e+f, g+a
    clock/{triangle} (cd) (ef) (ga)  - Three parallel tracks with triangle wave
    clock/{square} (ceg) ({sawtooth} dfa)  - Mixed waveforms

  - [] Figure out what the timeslice should be.
  - [-] Add a button to 'learn' the clock.
  - [✅] Automatically make make the font scale 1 if the text is too wide for screen.width.  
       (each character is 6 pixels wide, so if the text is longer than screen.width / 8, scale it down)
 */

import { parseMelody, extractTones, parseSimultaneousMelody, mutateMelodyTrack } from "../lib/melody-parser.mjs";

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
let mutationFlashTime = 0; // Time when a mutation occurred (for asterisk flash)
let currentNoteStartTime = 0; // When the current red note started playing
let currentNoteDuration = 0; // Duration of the current red note
let hasFirstSynthFired = false; // Flag to track when first synth plays (for syntax coloring)

// Sound management similar to notepat
const activeSounds = {}; // Track active synth instances for proper timing control
const scheduledNotes = []; // Queue of notes to be released at specific times

function boot({ ui, clock, params, colon, hud }) {
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
    } else if (melodyTracks.tracks.length === 1 && melodyTracks.tracks[0].length === 1) {
      // Special case: single note in parentheses like (c) - treat as single track
      parsedMelody = melodyTracks.tracks[0];
      
      // Check if the first note has an explicit octave, if so use that as the base octave
      if (parsedMelody.length > 0 && parsedMelody[0].octave !== octave) {
        octave = parsedMelody[0].octave;
      }
      
      // Extract tone strings for the sequence, using the determined octave as default
      sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${octave}G` });
      
      // Initialize melody timing state as single track, preserving mutation metadata
      melodyState = {
        notes: parsedMelody,
        index: 0,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        type: 'single'
      };
      
      // Preserve mutation metadata if present
      if (parsedMelody.hasMutation) {
        melodyState.hasMutation = true;
        melodyState.originalContent = parsedMelody.originalContent;
      }
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
        // Independent timing state for each parallel track
        trackStates: melodyTracks.tracks.map((track, trackIndex) => ({
          trackIndex: trackIndex,
          noteIndex: 0,
          track: track,
          nextNoteTargetTime: 0, // When this track should play its next note
          startTime: 0, // UTC-aligned start time for this track (set when first note plays)
          totalElapsedBeats: 0 // Total musical time elapsed for this track
        }))
      };
      
      // For backward compatibility, set parsedMelody to the first track
      parsedMelody = melodyTracks.tracks[0];
      sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${octave}G` });
    }
  } else {
    // No melody provided - set up fallback notes early in boot
    // Use a simple pentatonic scale pattern that sounds pleasant
    originalMelodyString = "cdefgab";
    
    // Parse the fallback melody using the same system as regular melodies
    melodyTracks = parseSimultaneousMelody(originalMelodyString, octave);
    parsedMelody = melodyTracks.tracks[0];
    
    // Extract tone strings for the fallback sequence
    sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${octave}G` });
    
    // Initialize fallback melody timing state (same structure as regular melodies)
    melodyState = {
      notes: parsedMelody,
      index: 0,
      baseTempo: baseTempo,
      isPlaying: false,
      startTime: performance.now(),
      timingMode: parseFloat(colon[0]) || 1.0,
      type: 'single',
      isFallback: true // Mark as fallback for special yellow coloring
    };
  }
  
  // Check for melody mode in params
  if (params[1] === "sync") {
    melodyMode = "sync"; // Sync melody to UTC clock
  } else {
    melodyMode = "continuous"; // Play melody continuously based on timing mode
  }

  // --- Set the HUD preview clock label immediately on boot ---
  if (typeof hud !== 'undefined' && hud.label) {
    // Don't set any color - let disk.mjs handle all HUD coloring automatically
    // Set a placeholder time string until paint runs
    hud.label("--:--:--:---", undefined, 0);
  }
}

function paint({ wipe, ink, write, clock, screen, sound, api, help, hud }) {
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
    help.resampleArray(sound.speaker.waveforms.left, 8),
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

  // sound.paint.waveform(
  //   api,
  //   sound.speaker.amplitudes.left,
  //   help.resampleArray(sound.speaker.waveforms.left, 8),
  //   0,
  //   0,
  //   availableWidth,
  //   availableHeight,
  //   [255, 255, 0, 255],
  // );

  // --- Digital Clock Render ---
  let clockDrawn = false;
  if (syncedDate) {
    // Draw melody timeline if we have a melody
    let coloredMelodyStringForTimeline = null;
    if (hasMelodyContent(melodyState)) {
      // Build the colored melody string once, use for both timeline and HUD
      const currentMelodyString = buildCurrentMelodyString(originalMelodyString, melodyState);
      coloredMelodyStringForTimeline = buildColoredMelodyStringUnified(currentMelodyString, melodyState);
      // Commented out - HUD label preview replaces center timeline display
      // drawMelodyTimeline(ink, write, screen, melodyState, coloredMelodyStringForTimeline);
    }
    // --- Scaled Clock Output ---
    if (!paint.scaledClockAnchor) {
      paint.scaledClockAnchor = {
        utc: syncedDate.getTime(),
        perf: performance.now(),
        divisor: currentTimeDivisor
      };
    }
    if (paint.scaledClockAnchor.divisor !== currentTimeDivisor) {
      const nowPerf = performance.now();
      const elapsed = (nowPerf - paint.scaledClockAnchor.perf) * (1 / paint.scaledClockAnchor.divisor);
      const newAnchorUtc = paint.scaledClockAnchor.utc + elapsed;
      paint.scaledClockAnchor = {
        utc: newAnchorUtc,
        perf: nowPerf,
        divisor: currentTimeDivisor
      };
    }
    const nowPerf = performance.now();
    const elapsed = (nowPerf - paint.scaledClockAnchor.perf) * (1 / currentTimeDivisor);
    const scaledTime = new Date(paint.scaledClockAnchor.utc + elapsed);

    const morning = scaledTime.getHours() < 12;
    let hours = scaledTime.getHours();
    hours = hours % 12;
    hours = hours ? hours : 12;
    const minutes = pad(scaledTime.getMinutes());
    const displaySeconds = pad(scaledTime.getSeconds());
    const millis = pad(scaledTime.getMilliseconds(), 3);
    const ampm = morning ? "AM" : "PM";
    const timeString = hours + ":" + minutes + ":" + displaySeconds + ":" + millis + " " + ampm;
    let fontSize = 1;

    // --- Draw the cyan timing line first, then the clock text centered on it ---
    // Calculate the Y position of the cyan line (same as in drawTimingGraph)
    const meterStartY = 0;
    const meterEndY = screen.height;
    const meterHeight = meterEndY - meterStartY;
    const minTiming = 0.05;
    const maxTiming = 3.0;
    const centerY = Math.round(screen.height / 2);
    const scaleY = meterHeight / (maxTiming - minTiming);
    const currentYPos = Math.round(centerY + (currentTimeDivisor - 1.0) * scaleY);
    // Draw the cyan line first so it appears behind the text
    ink("cyan").line(0, currentYPos, screen.width, currentYPos);

    // --- Draw the 1s label on the cyan line ---
    // The 1s timing is always at centerY
    const oneSLabel = "1s";
    const oneSBox = (typeof text !== 'undefined' && text.box) ? text.box(oneSLabel, { scale: 1 }) : { width: 12, height: 12 };
    const oneSX = 8; // Padding from left edge
    const oneSY = centerY - Math.round(oneSBox.height / 2);
    // Draw a black background for contrast
    if (typeof text !== 'undefined' && text.box && text.box.draw) {
      text.box.draw(oneSLabel, {
        x: oneSX,
        y: oneSY,
        scale: 1,
        color: "black"
      });
    } else {
      if (ink("black").fillRect) {
        ink("black").fillRect(oneSX - 4, oneSY - 2, oneSBox.width + 8, oneSBox.height + 4);
      }
    }
    ink("white").write(oneSLabel, {
      x: oneSX,
      y: oneSY,
      scale: 1
    }, "black");
    // Draw the clock text centered on the line (in front), with white text on a colored background
    const box = (typeof text !== 'undefined' && text.box) ? text.box(timeString, { scale: fontSize }) : { width: timeString.length * 6, height: 12 };
    const textX = Math.round((screen.width - box.width) / 2);
    // Center the text vertically on the line
    const textY = currentYPos - Math.round(box.height / 2);
    // Determine background color: black if exactly 1s, green if below, red if above
    let timerBgColor = "black";
    if (Math.abs(currentTimeDivisor - 1.0) < 0.01) {
      timerBgColor = "black";
    } else if (currentTimeDivisor > 1.0) {
      timerBgColor = "red";
    } else if (currentTimeDivisor < 1.0) {
      timerBgColor = "green";
    }
    // Draw the background rectangle using text.box if available
    if (typeof text !== 'undefined' && text.box && text.box.draw) {
      text.box.draw(timeString, {
        x: textX,
        y: textY,
        scale: fontSize,
        color: timerBgColor
      });
    } else {
      // Fallback: draw a filled rectangle if possible (legacy or custom ink API)
      if (ink(timerBgColor).fillRect) {
        ink(timerBgColor).fillRect(textX - 4, textY - 2, box.width + 8, box.height + 4);
      }
      // If no fillRect, just skip background
    }
    // Draw the time string in white, centered on the line
    // Always use white text for the main clock display to ensure good contrast
    // against the colored timing backgrounds (red/green/black)
    ink("white").write(timeString, {
      x: textX,
      y: textY,
      scale: fontSize
    }, timerBgColor);
    clockDrawn = true;

    // Display the base octave in red at bottom right
    const octaveText = screen.width < 200 ? `O${octave}` : `OCT ${octave}`;
    const octaveX = screen.width < 200 ? screen.width - 24 : screen.width - 48;
    ink("red").write(octaveText, { 
      x: octaveX, // Position from right edge
      y: screen.height - 16,  // Position from bottom edge
      scale: 1
    });

    // --- HUD preview clock label in the corner ---
    if (typeof hud !== 'undefined' && hud.label) {
      // Check if melody timing has started (based on whether first synth has fired)
      const melodyTimingStarted = hasFirstSynthFired;
      
      console.log("🕐 Clock HUD Debug:", {
        melodyTimingStarted,
        hasFirstSynthFired,
        nextNoteTargetTime,
        lastNoteTime,
        hasTrackStates: !!(melodyState && melodyState.trackStates),
        trackStatesStarted: melodyState && melodyState.trackStates ? melodyState.trackStates.some(ts => ts.startTime > 0) : false,
        melodyStateIndex: melodyState ? melodyState.index : null,
        melodyStateType: melodyState ? melodyState.type : null
      });
      
      // Provide "clock" prefix plus melody content - let disk.mjs handle all coloring
      let previewStringDecorative = '';
      let previewStringPlain = '';
      if (originalMelodyString && originalMelodyString.trim().length > 0) {
        if (melodyTimingStarted) {
          // Melody timing has started - use colored version with highlighting
          const currentMelodyString = buildCurrentMelodyString(originalMelodyString, melodyState);
          const coloredMelody = coloredMelodyStringForTimeline || buildColoredMelodyStringUnified(currentMelodyString, melodyState);
          console.log("🎨 Using colored melody:", coloredMelody.substring(0, 50) + "...");
          // Ensure the space after "clock" doesn't get colored by the melody colors
          previewStringDecorative = `\\white\\clock \\white\\` + coloredMelody;
        } else {
          // Melody timing hasn't started yet - show plain uncolored text
          console.log("🎨 Using plain melody (timing not started)");
          previewStringDecorative = `\\white\\clock ${originalMelodyString}`;
        }
        previewStringPlain = `clock ${originalMelodyString}`;
      } else {
        // No melody content - just show "clock" with explicit neutral color
        console.log("🎨 No melody content - just clock");
        previewStringDecorative = `\\white\\clock`;
        previewStringPlain = `clock`;
      }
      
      // To ensure the shadow is always black and not colored by inline codes, filter color codes from the decorative string
      function stripColorCodes(str) {
        // Remove all \\color\\ sequences
        return str.replace(/\\[a-zA-Z]+\\/g, '');
      }
      const shadowString = stripColorCodes(previewStringDecorative);
      // Draw the shadow label first, with all color codes stripped, in black
      if (hud.label && typeof hud.label === 'function') {
        // Always pass the logical (colorless) string as the 4th argument to hud.label
        // Let disk.mjs handle all HUD coloring automatically - no explicit color override
        hud.label(shadowString, undefined, 0, previewStringPlain); // shadow
        hud.supportsInlineColor = true;
        hud.label(previewStringDecorative, undefined, 0, previewStringPlain); // colored
      }
      // If the HUD system supports setting the actual prompt content, set it to the plain string (no color codes)
      if (hud.setPromptContent) {
        hud.setPromptContent(previewStringPlain);
      }
    }

// Unified: Build colored melody string for both timeline and HUD label
function buildColoredMelodyStringUnified(melodyString, melodyState) {
  // This is the same logic as in drawMelodyTimeline, but returns the colored string
  if (!melodyString) return "";
  let coloredMelodyString = "";
  let noteCharPositions = [];
  let charIndex = 0;
  let noteIndex = 0;
  let inGroup = false;
  let inWaveform = false;
  let currentTrackIndex = 0;
  let noteIndexInCurrentTrack = 0;
  let now = performance.now();
  let currentNoteIndex = 0;
  
  // Check if timing has actually started before applying current note highlighting
  const timingHasStarted = hasFirstSynthFired;
  
  console.log("🎵 Melody Color Debug:", {
    timingHasStarted,
    hasFirstSynthFired,
    nextNoteTargetTime,
    lastNoteTime,
    hasTrackStates: !!(melodyState && melodyState.trackStates),
    melodyStateIndex: melodyState ? melodyState.index : null
  });
  
  if (timingHasStarted && melodyState && melodyState.type === 'single' && melodyState.notes) {
    const totalNotes = melodyState.notes.length;
    currentNoteIndex = (melodyState.index - 1 + totalNotes) % totalNotes;
  } else if (timingHasStarted && melodyState && melodyState.type === 'parallel') {
    // For parallel, handled below
  } else {
    // If timing hasn't started, set currentNoteIndex to -1 so no note is highlighted
    currentNoteIndex = -1;
  }
  // First pass: identify all note character positions and their duration modifiers
  for (let i = 0; i < melodyString.length; i++) {
    const char = melodyString[i];
    if (char === '{') {
      inWaveform = true;
      continue;
    } else if (char === '}') {
      inWaveform = false;
      continue;
    } else if (inWaveform) {
      continue;
    } else if (char === '(') {
      inGroup = true;
      continue;
    } else if (char === ')') {
      inGroup = false;
      currentTrackIndex++;
      noteIndexInCurrentTrack = 0;
      continue;
    } else if (char === ' ') {
      continue;
    }
    
    // Handle octave-first notation (5f, 4c#, etc.) - octave number should be part of the note unit
    if (/[0-9]/.test(char) && i + 1 < melodyString.length && /[a-g]/i.test(melodyString[i + 1])) {
      let noteStart = i; // Start with the octave number
      let noteEnd = i + 1; // Move to the note letter
      
      // Check for sharp modifiers
      if (noteEnd + 1 < melodyString.length) {
        const nextChar = melodyString[noteEnd + 1];
        if (nextChar === 's' || nextChar === '#') {
          noteEnd++;
        }
      }
      
      // Check for duration modifiers
      while (noteEnd + 1 < melodyString.length) {
        const nextChar = melodyString[noteEnd + 1];
        if (nextChar === '.' || nextChar === '-' || nextChar === '<' || nextChar === '>' || nextChar === ',') {
          noteEnd++;
        } else {
          break;
        }
      }
      
      const noteIndexToUse = melodyState && melodyState.type === 'parallel' ? noteIndexInCurrentTrack : noteIndex;
      for (let j = noteStart; j <= noteEnd; j++) {
        noteCharPositions.push({
          charIndex: j,
          noteIndex: noteIndexToUse,
          trackIndex: currentTrackIndex
        });
      }
      noteIndex++;
      noteIndexInCurrentTrack++;
      i = noteEnd;
    }
    // Handle regular note letters (without octave prefix) or relative octave modifiers (+c, -f, etc.)
    else if (/[a-g#_+-]/i.test(char)) {
      let noteStart = i;
      let noteEnd = i;
      
      // Handle relative octave modifiers (++, --, etc.)
      if (char === '+' || char === '-') {
        while (noteEnd + 1 < melodyString.length && melodyString[noteEnd + 1] === char) {
          noteEnd++;
        }
        // Now we should have a note letter
        if (noteEnd + 1 < melodyString.length && /[a-g]/i.test(melodyString[noteEnd + 1])) {
          noteEnd++;
        } else {
          // Handle standalone dash as rest (like ---)
          if (char === '-') {
            const noteIndexToUse = melodyState && melodyState.type === 'parallel' ? noteIndexInCurrentTrack : noteIndex;
            for (let j = noteStart; j <= noteEnd; j++) {
              noteCharPositions.push({
                charIndex: j,
                noteIndex: noteIndexToUse,
                trackIndex: currentTrackIndex
              });
            }
            noteIndex++;
            noteIndexInCurrentTrack++;
            i = noteEnd;
            continue;
          } else {
            // Invalid + without note, skip
            continue;
          }
        }
      }
      
      // Check for sharp modifiers (for regular notes or after relative modifiers)
      if (noteEnd + 1 < melodyString.length) {
        const nextChar = melodyString[noteEnd + 1];
        if (nextChar === 's' || nextChar === '#') {
          noteEnd++;
        }
      }
      
      // Check for duration modifiers
      while (noteEnd + 1 < melodyString.length) {
        const nextChar = melodyString[noteEnd + 1];
        if (nextChar === '.' || nextChar === '-' || nextChar === '<' || nextChar === '>' || nextChar === ',') {
          noteEnd++;
        } else {
          break;
        }
      }
      
      const noteIndexToUse = melodyState && melodyState.type === 'parallel' ? noteIndexInCurrentTrack : noteIndex;
      for (let j = noteStart; j <= noteEnd; j++) {
        noteCharPositions.push({
          charIndex: j,
          noteIndex: noteIndexToUse,
          trackIndex: currentTrackIndex
        });
      }
      noteIndex++;
      noteIndexInCurrentTrack++;
      i = noteEnd;
    }
  }
  // Color the original melody string character by character
  let inWaveformForColoring = false;
  // For fade logic - red at start, fading to orange as note ends
  function getRedNoteColor() {
    if (currentNoteStartTime === 0 || currentNoteDuration === 0) {
      return "red"; // Default to red if timing not set
    }
    
    const elapsedTime = now - currentNoteStartTime;
    const progress = Math.min(1, elapsedTime / currentNoteDuration);
    
    // Fade from red to orange over the note duration
    if (progress < 0.5) {
      return "red"; // First half stays red
    } else {
      return "orange"; // Second half turns orange
    }
  }
  
  // Helper function to check if a note is mutated
  function isNoteMutated(noteCharData) {
    if (!noteCharData) return false;
    
    if (melodyState && melodyState.type === 'single') {
      const currentNote = melodyState.notes[noteCharData.noteIndex];
      return currentNote && currentNote.isMutation;
    } else if (melodyState && melodyState.type === 'parallel' && melodyState.trackStates && noteCharData.trackIndex < melodyState.trackStates.length) {
      const track = melodyState.tracks[noteCharData.trackIndex];
      const currentNote = track[noteCharData.noteIndex];
      return currentNote && currentNote.isMutation;
    }
    
    return false;
  }
  
  // Get color for mutated notes (magenta for mutations)
  function getMutatedNoteColor() {
    if (currentNoteStartTime === 0 || currentNoteDuration === 0) {
      return "magenta"; // Default to magenta if timing not set
    }
    
    const elapsedTime = now - currentNoteStartTime;
    const progress = Math.min(1, elapsedTime / currentNoteDuration);
    
    // Fade from magenta to purple over the note duration
    if (progress < 0.5) {
      return "magenta"; // First half stays magenta
    } else {
      return "purple"; // Second half turns purple
    }
  }
  const flashDuration = 200;
  const mutationFlashDuration = 300; // Longer flash for mutations
  const shouldFlashGreen = (now - (typeof specialCharFlashTime !== 'undefined' ? specialCharFlashTime : 0)) < flashDuration;
  const shouldFlashMutation = (now - (typeof mutationFlashTime !== 'undefined' ? mutationFlashTime : 0)) < mutationFlashDuration;
  for (let i = 0; i < melodyString.length; i++) {
    const char = melodyString[i];
    let color = "yellow";
    
    // Check if this character is part of the currently playing note
    const noteCharData = noteCharPositions.find(ncp => ncp.charIndex === i);
    let isCurrentlyPlayingNote = false;
    
    if (noteCharData) {
      if (melodyState && melodyState.type === 'single') {
        isCurrentlyPlayingNote = (noteCharData.noteIndex === currentNoteIndex);
      } else if (melodyState && melodyState.type === 'parallel' && melodyState.trackStates && noteCharData.trackIndex < melodyState.trackStates.length) {
        const trackState = melodyState.trackStates[noteCharData.trackIndex];
        const currentPlayingIndex = (trackState.noteIndex - 1 + trackState.track.length) % trackState.track.length;
        isCurrentlyPlayingNote = (noteCharData.noteIndex === currentPlayingIndex);
      }
    }
    
    if (char === '{') {
      inWaveformForColoring = true;
      color = shouldFlashGreen ? "green" : (timingHasStarted ? "yellow" : "gray");
    } else if (char === '}') {
      inWaveformForColoring = false;
      color = shouldFlashGreen ? "green" : (timingHasStarted ? "yellow" : "gray");
    } else if (inWaveformForColoring) {
      color = timingHasStarted ? "cyan" : "gray";
    } else if (char === '*') {
      // Special handling for mutation asterisk
      if (shouldFlashMutation) {
        color = "white"; // Flash white when mutation occurs
      } else {
        color = timingHasStarted ? "purple" : "gray"; // Purple when timing started, gray before
      }
    } else if (melodyState && melodyState.isFallback) {
      if (noteCharData) {
        if (isCurrentlyPlayingNote) {
          // Check if this note is mutated and use different coloring
          const isMutated = isNoteMutated(noteCharData);
          
          // Check if this is a duration punctuation character within the current note
          if (/[.,]/i.test(char)) {
            color = isMutated ? getMutatedNoteColor() : getRedNoteColor(); // Duration punctuation uses mutation or normal fade
          } else if (/[s]/i.test(char)) {
            color = isMutated ? getMutatedNoteColor() : getRedNoteColor(); // Sharp 's' uses same color as note letters
          } else if (/[0-9+\-#<>]/i.test(char)) {
            color = "green"; // Other special characters in current note flash green
          } else {
            color = isMutated ? getMutatedNoteColor() : getRedNoteColor(); // Note letters use mutation or normal fade
          }
        } else {
          // Use gray if timing hasn't started, yellow if it has
          color = timingHasStarted ? "yellow" : "gray";
        }
      } else {
        color = timingHasStarted ? "yellow" : "gray";
      }
    } else {
      if (noteCharData) {
        if (isCurrentlyPlayingNote) {
          // Check if this note is mutated and use different coloring
          const isMutated = isNoteMutated(noteCharData);
          
          // Check if this is a duration punctuation character within the current note
          if (/[.,]/i.test(char)) {
            color = isMutated ? getMutatedNoteColor() : getRedNoteColor(); // Duration punctuation uses mutation or normal fade
          } else if (/[s]/i.test(char)) {
            color = isMutated ? getMutatedNoteColor() : getRedNoteColor(); // Sharp 's' uses same color as note letters
          } else if (/[0-9+\-#<>]/i.test(char)) {
            color = "green"; // Other special characters in current note flash green
          } else {
            color = isMutated ? getMutatedNoteColor() : getRedNoteColor(); // Note letters use mutation or normal fade
          }
        } else {
          // Use gray if timing hasn't started, yellow if it has
          color = timingHasStarted ? "yellow" : "gray";
        }
      } else {
        // For non-note characters, use gray until timing starts
        color = timingHasStarted ? "yellow" : "gray";
      }
    }
    coloredMelodyString += `\\${color}\\${char}`;
  }
  return coloredMelodyString;
}
    // (Remove any other preview label drawing from the center)
  } else {
    ink("red").write("SYNCING...", { center: "xy", size: 2 });
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
    // Draw timing graph even when syncing (intentionally omitted)
    // drawTimingGraph(ink, write, screen);
  }
}

// Draw a single version of the melody string with the active note highlighted
function drawMelodyTimeline(ink, write, screen, melodyState, coloredMelodyStringOverride) {
  const timelineY = screen.height - 40; // More space above info text
  const timelineStartX = 16;
  const timelineEndX = screen.width - 16;
  const timelineWidth = timelineEndX - timelineStartX;
  
  // Get the melody string to display (use original input)
  let melodyString = "";
  if (melodyState && originalMelodyString) {
    melodyString = originalMelodyString;
  } else {
    melodyString = "cdefgab";
  }
  // Center the melody string horizontally
  const stringWidth = melodyString.length * 6;
  const startX = Math.max(timelineStartX, (screen.width - stringWidth) / 2);
  // Use the provided colored string if available
  let coloredMelodyString = coloredMelodyStringOverride || buildColoredMelodyStringUnified(buildCurrentMelodyString(melodyString, melodyState), melodyState);
  write(coloredMelodyString, {
    x: startX,
    y: timelineY,
    scale: 1
  });
  // Draw timing info below the melody string
  const infoY = timelineY + 12;
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

// Build the current melody string showing actual mutated notes
function buildCurrentMelodyString(originalMelodyString, melodyState) {
  if (!melodyState || !originalMelodyString) return originalMelodyString;
  
  // For tracks without mutations, return original string
  if (melodyState.type === 'single' && !melodyState.hasMutation) {
    return originalMelodyString;
  }
  
  if (melodyState.type === 'parallel' && melodyState.tracks) {
    const hasMutations = melodyState.tracks.some(track => track.hasMutation);
    if (!hasMutations) return originalMelodyString;
  }
  
  // For tracks with mutations, we need to replace mutated notes in the original string
  let modifiedString = originalMelodyString;
  
  if (melodyState.type === 'single' && melodyState.hasMutation && melodyState.notes) {
    // Find all note positions in the original string and map them to track notes
    const notePositions = [];
    let noteIndex = 0;
    
    for (let i = 0; i < originalMelodyString.length; i++) {
      const char = originalMelodyString[i];
      
      // Skip waveform specifiers
      if (char === '{') {
        while (i < originalMelodyString.length && originalMelodyString[i] !== '}') i++;
        continue;
      }
      
      // Skip parentheses and spaces
      if (char === '(' || char === ')' || char === ' ' || char === '*') continue;
      
      // Check if this is a note letter (main note character)
      if (/[a-g]/i.test(char)) {
        const trackNote = melodyState.notes[noteIndex];
        if (trackNote && trackNote.isMutation) {
          // This note was mutated, record its position for replacement
          notePositions.push({
            stringIndex: i,
            trackIndex: noteIndex,
            originalChar: char,
            newNote: trackNote.note.toLowerCase()
          });
        }
        noteIndex++;
      }
    }
    
    // Replace mutated notes in the string (in reverse order to preserve indices)
    notePositions.reverse().forEach(pos => {
      modifiedString = modifiedString.substring(0, pos.stringIndex) + 
                     pos.newNote + 
                     modifiedString.substring(pos.stringIndex + 1);
    });
  }
  
  // For parallel tracks with mutations
  if (melodyState.type === 'parallel' && melodyState.tracks) {
    const notePositions = [];
    let currentTrackIndex = 0;
    let noteIndexInTrack = 0;
    let inGroup = false;
    
    for (let i = 0; i < originalMelodyString.length; i++) {
      const char = originalMelodyString[i];
      
      // Skip waveform specifiers
      if (char === '{') {
        while (i < originalMelodyString.length && originalMelodyString[i] !== '}') i++;
        continue;
      }
      
      if (char === '(') {
        inGroup = true;
        noteIndexInTrack = 0;
        continue;
      } else if (char === ')') {
        inGroup = false;
        currentTrackIndex++;
        continue;
      } else if (char === ' ' || char === '*') {
        continue;
      }
      
      // Check if this is a note letter within a group
      if (inGroup && /[a-g]/i.test(char)) {
        const track = melodyState.tracks[currentTrackIndex];
        if (track && track[noteIndexInTrack] && track[noteIndexInTrack].isMutation) {
          // This note was mutated, record its position for replacement
          notePositions.push({
            stringIndex: i,
            trackIndex: currentTrackIndex,
            noteIndex: noteIndexInTrack,
            originalChar: char,
            newNote: track[noteIndexInTrack].note.toLowerCase()
          });
        }
        noteIndexInTrack++;
      }
    }
    
    // Replace mutated notes in the string (in reverse order to preserve indices)
    notePositions.reverse().forEach(pos => {
      modifiedString = modifiedString.substring(0, pos.stringIndex) + 
                     pos.newNote + 
                     modifiedString.substring(pos.stringIndex + 1);
    });
  }
  
  return modifiedString;
}

// Helper function to determine if a note character is playing in parallel tracks
function isNotePlayingInParallelTrack(melodyString, charIndex, trackStates) {
  // Parse the parallel structure to find which track this character belongs to
  let currentTrackIndex = 0;
  let noteIndexInTrack = 0;
  let inGroup = false;
  let inWaveform = false; // Track if we're inside a {waveform} specifier
  
  // Find which track the character at charIndex belongs to
  for (let i = 0; i < melodyString.length; i++) {
    const char = melodyString[i];
    
    if (char === '{') {
      inWaveform = true;
      continue;
    } else if (char === '}') {
      inWaveform = false;
      continue;
    } else if (inWaveform) {
      // Skip all characters inside {waveform} specifiers
      continue;
    } else if (char === '(') {
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
    if (/[a-g#_]/i.test(char)) {
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

// Create a managed synth sound with proper lifecycle control (similar to notepat)
function createManagedSound(sound, tone, waveType, duration, volume = 0.8, isFallback = false) {
  // Add a proportional gap that scales inversely with duration - shorter notes need much bigger gaps
  let gapRatio;
  
  if (duration <= 100) {
    // Very short notes (like c....) need massive gaps to be audible - 90%
    gapRatio = 0.90;
  } else if (duration <= 200) {
    // Short notes (like c...) need huge gaps - 85%
    gapRatio = 0.85;
  } else if (duration <= 400) {
    // Medium-short notes (like c.) need big gaps - 70%
    gapRatio = 0.70;
  } else if (duration <= 800) {
    // Regular notes need moderate gaps - 30%
    gapRatio = 0.30;
  } else {
    // Long notes need minimal gaps - 15%
    gapRatio = 0.15;
  }
  
  const noteGap = duration * gapRatio;
  const actualDuration = Math.max(50, duration - noteGap); // Ensure minimum 50ms duration
  
  // Calculate fade time proportional to note duration
  let fadeTime;
  if (actualDuration <= 100) {
    // Very short notes need very fast fade - 5ms
    fadeTime = 0.005;
  } else if (actualDuration <= 300) {
    // Short notes need fast fade - 10ms
    fadeTime = 0.010;
  } else if (actualDuration <= 600) {
    // Medium notes need moderate fade - 20ms
    fadeTime = 0.020;
  } else {
    // Long notes can have normal fade - 50ms
    fadeTime = 0.050;
  }
  
  const synthInstance = sound.synth({
    type: waveType || "sine",
    tone: tone,
    duration: "🔁", // Infinite duration - we'll manage the lifecycle manually
    attack: 0.01,
    decay: 0.995,
    volume: volume
  });
  
  // Set flag to indicate first synth has fired (for syntax coloring)
  hasFirstSynthFired = true;
  
  // Calculate when this note should be released (using the shorter duration)
  const releaseTime = performance.now() + actualDuration;
  
  // Store the sound for lifecycle management (use performance.now() + random for unique IDs)
  const soundId = `${tone}_${performance.now()}_${Math.random().toString(36).substr(2, 9)}`;
  activeSounds[soundId] = {
    synth: synthInstance,
    tone: tone,
    waveType: waveType,
    startTime: performance.now(),
    releaseTime: releaseTime,
    fadeTime: fadeTime, // Store the calculated fade time
    isFallback: isFallback
  };
  
  // Schedule the note to be released
  scheduledNotes.push({
    soundId: soundId,
    releaseTime: releaseTime,
    fadeTime: fadeTime // Store fade time with the scheduled release
  });
  
  // Sort scheduled notes by release time for efficient processing
  scheduledNotes.sort((a, b) => a.releaseTime - b.releaseTime);
  
  // Log with appropriate styling
  if (isFallback) {
    // console.log(`%c♪ ${tone} ${waveType || 'sine'} ${(duration / 1000).toFixed(1)}s (fallback, managed)`, 'color: #ffff00; background: black; font-weight: bold; padding: 2px;');
  } else {
    // console.log(`%c♪ ${tone} ${waveType || 'sine'} ${(duration / 1000).toFixed(1)}s (managed)`, 'color: #00ff88; background: black; font-weight: bold; padding: 2px;');
  }
  
  return soundId;
}

// Process scheduled note releases
function processScheduledReleases() {
  const now = performance.now();
  const overdueThreshold = 5000; // 5 seconds - notes this overdue should be force-killed
  
  // Release notes that have reached their scheduled release time
  while (scheduledNotes.length > 0 && scheduledNotes[0].releaseTime <= now) {
    const noteToRelease = scheduledNotes.shift();
    const sound = activeSounds[noteToRelease.soundId];
    
    if (sound && sound.synth) {
      // Use the fade time that was calculated for this specific note
      const fadeTime = noteToRelease.fadeTime || sound.fadeTime || 0.05; // Fallback to 50ms if not set
      
      // Check if note is extremely overdue (likely from standby)
      const overdueBy = now - noteToRelease.releaseTime;
      if (overdueBy > overdueThreshold) {
        // console.log(`%cForce-killing overdue note (${(overdueBy/1000).toFixed(1)}s late)`, 'color: red; background: black; padding: 2px;');
        sound.synth.kill(0.001); // Very fast kill for overdue notes
      } else {
        sound.synth.kill(fadeTime);
      }
      delete activeSounds[noteToRelease.soundId];
    }
  }
  
  // Also check for any orphaned active sounds that don't have scheduled releases
  const activeIds = Object.keys(activeSounds);
  const scheduledIds = new Set(scheduledNotes.map(note => note.soundId));
  
  activeIds.forEach(soundId => {
    if (!scheduledIds.has(soundId)) {
      const sound = activeSounds[soundId];
      if (sound) {
        const age = now - sound.startTime;
        // If a sound has been active for more than 10 seconds without a release schedule, kill it
        if (age > 10000) {
          // console.log(`%cCleaning up orphaned sound: ${sound.tone}`, 'color: yellow; background: black; padding: 2px;');
          if (sound.synth) sound.synth.kill(0.1);
          delete activeSounds[soundId];
        }
      }
    }
  });
}

// Clear all active sounds (useful for cleanup or emergency stop)
function clearAllSounds() {
  const fadeTime = 0.02; // Quick fade for cleanup
  
  // Kill all active sounds
  Object.keys(activeSounds).forEach(soundId => {
    const sound = activeSounds[soundId];
    if (sound && sound.synth) {
      sound.synth.kill(fadeTime);
    }
  });
  
  // Clear all tracking arrays
  Object.keys(activeSounds).forEach(key => delete activeSounds[key]);
  scheduledNotes.length = 0;
}

// Detect and handle standby/resume situations
let lastFrameTime = 0;
const STANDBY_THRESHOLD = 1000; // If more than 1 second has passed, assume standby

function handleStandbyResume() {
  const now = performance.now();
  
  if (lastFrameTime > 0) {
    const timeDelta = now - lastFrameTime;
    
    // If a large time gap is detected, we likely returned from standby
    if (timeDelta > STANDBY_THRESHOLD) {
      // console.log(`%cStandby detected (${(timeDelta/1000).toFixed(1)}s gap) - cleaning up sounds`, 'color: orange; background: black; padding: 2px;');
      
      // Clear all hanging sounds
      clearAllSounds();
      
      // Reset timing to prevent multiple rapid fires
      nextNoteTargetTime = 0;
      
      // Reset all track timing states
      if (melodyState && melodyState.trackStates) {
        melodyState.trackStates.forEach(trackState => {
          trackState.nextNoteTargetTime = 0;
          trackState.startTime = 0;
          // Keep noteIndex and totalElapsedBeats to preserve musical position
        });
      }
      
      return true; // Indicate that standby cleanup occurred
    }
  }
  
  lastFrameTime = now;
  return false;
}

// Slide existing active sounds to a new octave (similar to notepat's slide function)
function slideActiveSoundsToNewOctave(oldOctave, newOctave) {
  const octaveChange = newOctave - oldOctave;
  
  // Iterate through all active sounds and update their tones
  Object.keys(activeSounds).forEach(soundId => {
    const sound = activeSounds[soundId];
    if (sound && sound.synth && sound.tone) {
      // Parse the current tone to extract note and octave
      const toneMatch = sound.tone.match(/^(\d+)([A-G]#?)$/);
      if (toneMatch) {
        const currentOctave = parseInt(toneMatch[1]);
        const note = toneMatch[2];
        
        // Calculate new octave, clamping to valid range (1-9)
        const targetOctave = Math.max(1, Math.min(9, currentOctave + octaveChange));
        const newTone = `${targetOctave}${note}`;
        
        try {
          // Update the synth to slide to the new tone
          sound.synth.update({
            tone: newTone,
            duration: 0.05 // Very fast slide duration - minimal delay/skip
          });
          
          // Update our tracking information
          sound.tone = newTone;
          
          // Log the slide
          if (sound.isFallback) {
            // console.log(`%c↗ ${sound.tone} → ${newTone} (slide, fallback)`, 'color: #ffff88; background: black; font-weight: bold; padding: 2px;');
          } else {
            // console.log(`%c↗ ${sound.tone} → ${newTone} (slide)`, 'color: #88ff88; background: black; font-weight: bold; padding: 2px;');
          }
        } catch (error) {
          console.warn(`Failed to slide ${sound.tone} to ${newTone}:`, error);
          // If sliding fails, gracefully fade out the sound
          sound.synth.kill(0.1);
          delete activeSounds[soundId];
        }
      }
    }
  });
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
        const isFallback = melodyState.isFallback;
        
        // Update with new notes
        melodyState.notes = parsedMelody;
        
        // Preserve timing - don't reset position
        melodyState.index = currentIndex;
        melodyState.isFallback = isFallback; // Preserve fallback status
      }
    } else {
      // Multiple tracks
      if (melodyState && (melodyState.type === 'multi' || melodyState.type === 'parallel')) {
        // Store current timing state for the main melody index
        const currentIndex = melodyState.index;
        const isFallback = melodyState.isFallback;
        
        // Store current track states if they exist
        const currentTrackStates = melodyState.trackStates ? 
          melodyState.trackStates.map(ts => ({
            noteIndex: ts.noteIndex,
            startTime: ts.startTime,
            totalElapsedBeats: ts.totalElapsedBeats
          })) : null;
        
        // Update with new parsed tracks
        melodyState.tracks = melodyTracks.tracks;
        melodyState.type = melodyTracks.type;
        melodyState.maxLength = melodyTracks.maxLength || 0;
        melodyState.isFallback = isFallback; // Preserve fallback status
        
        // Update or create track states for parallel timing
        if (melodyState.type === 'parallel') {
          melodyState.trackStates = melodyTracks.tracks.map((track, trackIndex) => {
            // Try to preserve timing from existing track state
            const existingState = currentTrackStates && currentTrackStates[trackIndex];
            return {
              trackIndex: trackIndex,
              noteIndex: existingState ? existingState.noteIndex : 0,
              track: track,
              nextNoteTargetTime: 0, // Reset target time for new track
              startTime: existingState ? existingState.startTime : 0, // Preserve start time
              totalElapsedBeats: existingState ? existingState.totalElapsedBeats : 0 // Preserve elapsed time
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
  } else if (melodyState && melodyState.isFallback) {
    // Handle fallback notes - reparse with new octave
    originalMelodyString = "cdefgab"; // Use the same fallback pattern
    melodyTracks = parseSimultaneousMelody(originalMelodyString, newOctave);
    parsedMelody = melodyTracks.tracks[0];
    sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${newOctave}G` });
    
    // Update fallback melody state with new octave
    const currentIndex = melodyState.index;
    melodyState.notes = parsedMelody;
    melodyState.index = currentIndex; // Preserve timing
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
      const oldOctave = octave;
      octave++;
      
      // Slide existing sounds to new octave instead of cutting them off
      slideActiveSoundsToNewOctave(oldOctave, octave);
      
      // Reparse melody with new octave
      reparseMelodyWithNewOctave(octave);
    }
  }

  if (e.is("keyboard:down:arrowdown")) {
    // Decrease octave (min 1)
    if (octave > 1) {
      const oldOctave = octave;
      octave--;
      
      // Slide existing sounds to new octave instead of cutting them off
      slideActiveSoundsToNewOctave(oldOctave, octave);
      
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

  // Check for standby/resume before processing anything else
  const wasInStandby = handleStandbyResume();
  
  // Process scheduled note releases for managed sound system
  processScheduledReleases();

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
      // console.log('%cclock unsynced', 'color: orange; background: black; padding: 2px;');
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
      // This should not happen since fallback notes are now set up in boot()
      console.warn("🎵 No melody state available - this should not happen");
      return;
    }

    const currentTime = syncedTime ? syncedTime.getTime() : performance.now();

    // Only handle single track melodies here - parallel tracks are handled in sim()
    if (melodyState.type === 'single') {
      if (!melodyState.notes || melodyState.notes.length === 0) return;
      
      const noteData = melodyState.notes[melodyState.index];
      if (!noteData) return;
      
      const { note, octave: noteOctave, duration, swing, swingAmount, waveType, volume } = noteData;
      
      // Play the note using managed sound system
      if (note !== 'rest') {
        let tone = noteOctave ? `${noteOctave}${note.toUpperCase()}` : `${octave}${note.toUpperCase()}`;
        const noteDuration = duration * melodyState.baseTempo;
        
        try {
          // Create managed sound that will be automatically released
          createManagedSound(
            sound, 
            tone, 
            waveType, 
            noteDuration, 
            volume || 0.8, // Use note volume or default to 0.8
            melodyState.isFallback
          );
        } catch (error) {
          console.error(`%c✗ ${tone} - ${error}`, 'color: red; background: black; font-weight: bold; padding: 2px;');
        }
      }
      
      if (waveType || swing) {
        specialCharFlashTime = performance.now();
      }
      
      // Update timing tracking for single tracks
      lastNoteStartTime = currentTime;
      lastNoteDuration = duration * melodyState.baseTempo;
      
      // Update current note timing for red highlight fade
      currentNoteStartTime = performance.now();
      currentNoteDuration = duration * melodyState.baseTempo;
      
      // Don't set nextNoteTargetTime here - it's handled in the main timing loop
      // to ensure UTC alignment
      
      // Advance to next note
      const oldIndex = melodyState.index;
      melodyState.index = (melodyState.index + 1) % melodyState.notes.length;
      
      // Check if we've looped back to the beginning and this track has mutation enabled
      if (oldIndex === melodyState.notes.length - 1 && melodyState.index === 0 && melodyState.hasMutation) {
        // Apply mutation to this single track
        console.log(`🎲 Mutating single track (${melodyState.originalContent})`);
        const mutatedTrack = mutateMelodyTrack(parsedMelody, melodyState.originalContent, octave);
        
        // Set mutation flash time for asterisk animation
        mutationFlashTime = performance.now();
        
        // Update the parsed melody
        parsedMelody = mutatedTrack;
        melodyState.notes = mutatedTrack;
        
        // Preserve mutation metadata
        parsedMelody.hasMutation = true;
        parsedMelody.originalContent = parsedMelody.originalContent;
        parsedMelody.mutationCount = (parsedMelody.mutationCount || 0) + 1;
        melodyState.notes.hasMutation = true;
        melodyState.notes.originalContent = parsedMelody.originalContent;
        melodyState.notes.mutationCount = parsedMelody.mutationCount;
        
        // Update the sequence for backward compatibility
        sequence = extractTones(parsedMelody, { skipRests: false, restTone: `${octave}G` });
        
        console.log(`🎲 Single track mutated (mutation #${parsedMelody.mutationCount})`);
      }
    }
  }

  // Super simple melody timing - check against target time
  if (time && !wasInStandby) { // Skip timing checks on standby frames to prevent rapid-fire
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
          
          // After playing, set next note time based on the note's actual duration
          if (melodyState && melodyState.notes && melodyState.notes.length > 0) {
            const currentNoteData = melodyState.notes[(melodyState.index - 1 + melodyState.notes.length) % melodyState.notes.length];
            if (currentNoteData) {
              const noteDuration = currentNoteData.duration * melodyState.baseTempo;
              nextNoteTargetTime = currentTimeMs + noteDuration;
            }
          }
        }
      } else if (melodyState.type === 'parallel') {
        // Parallel tracks - each track follows its own musical logic but stays UTC-aligned
        let anyTrackPlayed = false;
        
        melodyState.trackStates.forEach((trackState, trackIndex) => {
          const track = trackState.track;
          if (!track || track.length === 0) return;
          
          // Skip disabled tracks (marked with x() syntax)
          if (track.isDisabled) {
            return;
          }
          
          // Set start time for this track's first note if not set
          if (trackState.startTime === 0) {
            // Start each track at the next UTC boundary based on timing divisor
            const timeDivisor = utcTriggerDivisor;
            const intervalMs = timeDivisor * 1000;
            trackState.startTime = Math.ceil(currentTimeMs / intervalMs) * intervalMs;
            trackState.nextNoteTargetTime = trackState.startTime;
            trackState.totalElapsedBeats = 0;
          }
          
          // Check if it's time for this track to play its next note
          if (currentTimeMs >= trackState.nextNoteTargetTime) {
            const noteData = track[trackState.noteIndex];
            if (noteData) {
              // Play this track's note using managed sound system
              if (noteData.note !== 'rest') {
                const { note, octave: noteOctave, duration, waveType, volume } = noteData;
                let tone = noteOctave ? `${noteOctave}${note.toUpperCase()}` : `${octave}${note.toUpperCase()}`;
                const noteDuration = duration * melodyState.baseTempo;
                
                try {
                  // Create managed sound with reduced volume for parallel tracks
                  createManagedSound(
                    sound, 
                    tone, 
                    waveType, 
                    noteDuration, 
                    (volume || 0.7) / melodyState.tracks.length, // Use note volume or default, adjust for track count
                    melodyState.isFallback
                  );
                } catch (error) {
                  console.error(`%c✗ Track${trackIndex}: ${tone} - ${error}`, 'color: red; background: black; font-weight: bold; padding: 2px;');
                }
                
                if (waveType) {
                  specialCharFlashTime = performance.now();
                }
                
                // Update current note timing for red highlight fade
                currentNoteStartTime = performance.now();
                currentNoteDuration = noteData.duration * melodyState.baseTempo;
              }
              
              // Calculate next note time from UTC-aligned start time to prevent drift
              // Add this note's duration to the total elapsed time
              const noteDuration = noteData.duration * melodyState.baseTempo;
              trackState.totalElapsedBeats += noteDuration;
              
              // Calculate next target time from the original start time + total elapsed
              trackState.nextNoteTargetTime = trackState.startTime + trackState.totalElapsedBeats;
              
              // Advance this track to its next note
              const oldNoteIndex = trackState.noteIndex;
              trackState.noteIndex = (trackState.noteIndex + 1) % track.length;
              
              // Check if we've looped back to the beginning and this track has mutation enabled
              if (oldNoteIndex === track.length - 1 && trackState.noteIndex === 0 && track.hasMutation) {
                // Apply mutation to this track
                console.log(`🎲 Mutating track ${trackIndex} (${track.originalContent})`);
                const mutatedTrack = mutateMelodyTrack(track, track.originalContent, octave);
                
                // Set mutation flash time for asterisk animation
                mutationFlashTime = performance.now();
                
                // Update the track in place
                melodyState.tracks[trackIndex] = mutatedTrack;
                
                // Preserve mutation metadata
                melodyState.tracks[trackIndex].hasMutation = true;
                melodyState.tracks[trackIndex].originalContent = track.originalContent;
                melodyState.tracks[trackIndex].mutationCount = (track.mutationCount || 0) + 1;
                
                // Update the track reference in trackState for consistency
                trackState.track = melodyState.tracks[trackIndex];
                
                console.log(`🎲 Track ${trackIndex} mutated (mutation #${melodyState.tracks[trackIndex].mutationCount})`);
              }
              
              anyTrackPlayed = true;
            }
          }
        });
        
        if (anyTrackPlayed) {
          synced = true;
        }
      }
    } else {
      // No melody - this case should not occur since fallback notes are set up in boot()
      // But if it does, use UTC boundaries for simple bleep
      console.warn("🎵 No melody state in UTC timing - this should not happen");
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

function leave() {
  // Runs once before the piece is unloaded.
  // Clear all active sounds to prevent lingering beeps
  clearAllSounds();
}

function preview({ ink, wipe, write }) {
  // Render a custom thumbnail image showing the fallback melody
  wipe("black");
  
  // Test colored text with wrapping to debug the off-by-one issue
  write("\\yellow\\hello \\red\\world \\green\\this \\blue\\is \\cyan\\a \\magenta\\very \\orange\\long \\white\\melody \\gray\\test", {
    x: 6,
    y: 6,
    bounds: 60,  // Force wrapping
    scale: 1
  });
  
  // Add a small clock icon below
  ink("cyan");
  write("🕐", {
    x: 20,
    y: 40,
    scale: 1
  });
}

// function icon() {
// Render an application icon, aka favicon.
// }


