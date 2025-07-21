// Clock, 2025.6.27.12.00
// Just a standard clock with melody support and UTC sync.

// TODO: - [] Reduce code size.
//       - [] Adjust swipe syncing and check utc syncing. 

/* 🐢 About
  Usage:
    clock melody_string               - Continuous melody playback with 1s timing
    clock melody_string               - Melody synced to UTC clock ticks
    clock melody_string

  Base octave is 4 unless first note specifies octave (e.g., 5cdefg uses octave 5)
  Use Up/Down arrow keys to change base octave during playback
  
  🎵 NEW: Parallel Tracks Support!
  Use spaces to separate groups for parallel playback:
    ceg dfa - Two parallel melodies: c→e→g and d→f→a
              Plays: c+d, then e+f, then g+a
    cd ef - Two short parallel lines: c→d and e→f  
            Plays: c+e, then d+f
    cde f - Different length tracks (longer track cycles)
            Plays: c+f, then d+f, then e+f
    c d e - Three single-note parallel tracks
            Plays: c+d+e simultaneously
    xceg dfa - First track disabled (x prefix), only second plays

  🎵 NEW: Waveform Type & Volume Support!
  Use {type} or {type:volume} or {volume} syntax:
    {sine} cdefg - Sine wave (default)
    {square} cdefg - Square wave
    {sawtooth} cdefg - Sawtooth wave  
    {triangle} cdefg - Triangle wave
    {noise-white} cdefg - White noise
    {sample} cdefg - Sample playback
    {custom} cdefg - Custom waveform generation
    {0.5} cdefg - Set volume to 50% (keeps current waveform)
    {square:0.3} cdefg - Square wave at 30% volume
    {0.8} c{0.2}d{1.0}e - Volume changes during melody
    {square} cdefg - Square wave for all tracks
    ceg {triangle:0.6} dfa - Sine for first track, triangle at 60% for second
  
  🎵 NEW: Sticky Duration Modifiers!
  Use suffix notation for duration that applies to all following notes:
    c... defg - c is very short, and d,e,f,g are all also very short
    c... d. efg - c is very short, d is short, then e,f,g are all short  
    c,, defg - c is very long, and d,e,f,g are all also very long
    {...} cdefg - All notes become very short (thirty-second notes)
    {..} cdefg - All notes become short (sixteenth notes)  
    {.} cdefg - All notes become eighth notes
    {,} cdefg - All notes become half notes (longer)
    {,,} cdefg - All notes become whole notes (very long)
    {..} c d. e f,, - Global applies to c and e; d and f use local modifiers

  🎵 NEW: Struck Note Mode!
  Use ^ to toggle between held and struck note timing:
    ^cdefg - All notes are struck (finite duration with natural decay)
    c^defg - c is held, defg are struck
    ^cd^efg - cd are struck, efg are held
    The ^ character is sticky and persists until changed, similar to + and - octave

  Examples:
    clock cdefg               - Play in octave 4 with 1s timing
    clock 5cdefg              - Play in octave 5 (determined by first note)
    clock cdefg:0.25          - Fast playback at 0.25s timing in octave 4
    clock 5d.d.edgf#-:2       - Play at 2s timing with octave 5
    clock c<defg<<ab:0.5      - Play with swing timing at 0.5s
    clock {square} cdefg      - Square wave melody
    clock c... defg           - Very short staccato notes (sticky duration)
    clock c,, defg            - Long sustained notes (sticky duration)
    clock ^cdefg              - Struck notes with natural decay (piano-like)
    clock (ceg) (dfa)         - Parallel melodies: c+d, e+f, g+a
    clock {triangle} (cd) (ef) (ga)  - Three parallel tracks with triangle wave
    clock {square} (ceg) ({sawtooth} dfa)  - Mixed waveforms
*/

import {
  parseMelody,
  extractTones,
  parseSimultaneousMelody,
  mutateMelodyTrack,
  noteToTone,
} from "../lib/melody-parser.mjs";

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
let octaveFlashTime = 0; // Track when octave changed for flash effect

// Enhanced melody state for simultaneous tracks
let melodyTracks = null; // Will store parsed simultaneous tracks
let melodyState = null;
let baseTempo = 500; // Default 500ms per quarter note (120 BPM)

// History buffer system for visual timeline
let historyBuffer = []; // Array to store all played notes with timestamps
const MAX_HISTORY_ITEMS = 1000; // Limit history to prevent memory issues

// History item structure:
// {
//   note: "c",
//   octave: 4,
//   startTime: timestampMs,
//   endTime: timestampMs,
//   trackIndex: 0,
//   waveType: "sine",
//   volume: 0.8,
//   isMutation: false,
//   struck: false
// }
let tempoJustChanged = false; // Flag to track immediate tempo changes for overlapping sounds
let lastTempoChangeTime = 0; // Track when tempo last changed to add tolerance
let tempoChangeTolerance = 150; // Milliseconds to wait before allowing another tempo change restart
let nowLineY = 0; // Y position of the NOW line (draggable, defaults to screen center)
let isDraggingNowLine = false; // Whether user is currently dragging the NOW line

// Rainbow color mapping for notes (ROYGBIV) using RGB arrays
const noteColorMap = {
  c: [255, 0, 0], // Red
  d: [255, 127, 0], // Orange
  e: [255, 255, 0], // Yellow
  f: [0, 255, 0], // Green
  g: [0, 128, 255], // Blue
  a: [75, 0, 130], // Indigo
  b: [148, 0, 211], // Violet
  // Handle sharps/flats with similar colors
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

// Helper function to get note color
function getNoteColor(noteName) {
  if (!noteName || noteName === "rest") return [102, 102, 102]; // Gray for rests
  const cleanNote = noteName.toLowerCase().replace(/[0-9]/g, ""); // Remove octave numbers
  return noteColorMap[cleanNote] || [255, 255, 255]; // Default to white if not found
}

// Helper function to make a color brighter for active notes
function getBrighterColor(rgbArray) {
  const [r, g, b] = rgbArray;

  // Make brighter by adding to each component (max 255)
  const brighterR = Math.min(255, r + 80);
  const brighterG = Math.min(255, g + 80);
  const brighterB = Math.min(255, b + 80);

  return [brighterR, brighterG, brighterB];
}

// Helper function to make a color darker and more gray for history notes
function getDarkerColor(rgbArray) {
  const [r, g, b] = rgbArray;

  // Calculate luminance for grayscale conversion
  const luminance = 0.299 * r + 0.587 * g + 0.114 * b;

  // Mix with gray (50% saturation reduction) and darken
  const grayR = Math.round(r * 0.5 + luminance * 0.5);
  const grayG = Math.round(g * 0.5 + luminance * 0.5);
  const grayB = Math.round(b * 0.5 + luminance * 0.5);

  // Then darken the desaturated color
  const darkerR = Math.max(0, Math.round(grayR * 0.6));
  const darkerG = Math.max(0, Math.round(grayG * 0.6));
  const darkerB = Math.max(0, Math.round(grayB * 0.6));

  return [darkerR, darkerG, darkerB];
}
let melodyMode = "continuous"; // "continuous" or "sync" - how to timing the melody
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
let mutatedNotePositions = []; // Positions of notes that were mutated (for rainbow flash)
let triggeredAsteriskPositions = []; // Positions of asterisks that were just triggered (for white flash)
let recentlyMutatedNoteIndex = -1; // Index of the most recently mutated note (for rainbow flash)
let recentlyMutatedTrackIndex = -1; // Track index of the most recently mutated note (for parallel tracks)
let currentNoteStartTime = 0; // When the current red note started playing
let currentNoteDuration = 0; // Duration of the current red note
let hasFirstSynthFired = false; // Flag to track when first synth plays (for syntax coloring)
let noteFlowMode = "stop"; // "stop" or "flow" - whether red note stops at cyan line or flows past it
let totalNotesPlayed = 0; // Track total notes played across all repetitions for persistent white note history
let completedSequences = 0; // Track how many complete melody sequences have been played

// Sound management similar to notepat
const activeSounds = {}; // Track active synth instances for proper timing control
const scheduledNotes = []; // Queue of notes to be released at specific times
let isLeavingPiece = false; // Flag to prevent new sounds from being created after leave() is called

// UI Buttons for octave control
let octaveMinusBtn, octavePlusBtn;

// Visual logging throttling
let lastLogTime = 0;
let lastLoggedNoteCount = 0;
const LOG_THROTTLE_MS = 1000; // Only log once per second maximum

// Helper function to get current note index for synchronization
function getCurrentNoteIndex(melodyState, trackIndex = 0) {
  if (!hasFirstSynthFired || !melodyState) {
    return -1; // No note is currently playing
  }

  if (melodyState.type === "single" && melodyState.notes) {
    const totalNotes = melodyState.notes.length;
    return (melodyState.index - 1 + totalNotes) % totalNotes;
  } else if (melodyState.type === "parallel" && melodyState.trackStates) {
    const trackState = melodyState.trackStates[trackIndex];
    if (trackState && trackState.track) {
      const totalNotes = trackState.track.length;
      return (trackState.noteIndex - 1 + totalNotes) % totalNotes;
    }
  }

  return -1;
}

function boot({ ui, clock, params, colon, hud, screen, typeface, api }) {
  // Reset leaving flag when piece starts
  isLeavingPiece = false;

  // Reset total notes played counter for fresh start
  totalNotesPlayed = 0;

  // Reset completed sequences counter
  completedSequences = 0;

  // Get the UTC offset from /api/clock and use that to set the clock.
  clock.resync();

  // Set default octave to 4 (removed from colon parameter)
  octave = 4;

  // Set base tempo using colon[0] as a divisor (e.g., :1 = 1 second, :0.5 = 0.5 seconds)
  // Since default note duration is 2, we need to divide by 2 to get the correct whole note timing
  const timeDivisor = parseFloat(colon[0]) || 1.0; // Default to 1 second if no colon param
  originalTimeDivisor = timeDivisor; // Store for later reference
  currentTimeDivisor = timeDivisor; // Initialize current divisor to original
  targetTimeDivisor = timeDivisor; // Initialize target divisor to original
  utcTriggerDivisor = timeDivisor; // Initialize UTC trigger divisor to original
  baseTempo = (timeDivisor * 1000) / 2; // Divide by 2 because default duration is 2
  targetTempo = baseTempo; // Set target tempo to initial value
  isLerpingToSync = false; // Reset lerping state

  if (params[0]) {
    // Concatenate all params to handle cases like clock/(ceg) (dfa) where
    // (ceg) is in params[0] and (dfa) is in params[1]
    originalMelodyString = params.join(" ");

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
      sequence = extractTones(parsedMelody, {
        skipRests: false,
        restTone: `${octave}G`,
      });

      // Initialize melody timing state
      melodyState = {
        notes: parsedMelody,
        index: 0,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        type: "single",
      };

      // Preserve mutation metadata if present
      if (parsedMelody.hasMutation) {
        melodyState.hasMutation = true;
        melodyState.originalContent = parsedMelody.originalContent;
        melodyState.mutationTriggerPosition =
          parsedMelody.mutationTriggerPosition;

        // Copy multiple mutation zones properties
        if (parsedMelody.mutationTriggerPositions) {
          melodyState.mutationTriggerPositions =
            parsedMelody.mutationTriggerPositions;
          melodyState.currentMutationZone =
            parsedMelody.currentMutationZone || 0;
        }
      }
    } else if (melodyTracks.tracks.length === 1) {
      // Special case: single parallel group like (c), (ccc), ({...}ccc*) - treat as single track
      parsedMelody = melodyTracks.tracks[0];

      // Check if the first note has an explicit octave, if so use that as the base octave
      if (parsedMelody.length > 0 && parsedMelody[0].octave !== octave) {
        octave = parsedMelody[0].octave;
      }

      // Extract tone strings for the sequence, using the determined octave as default
      sequence = extractTones(parsedMelody, {
        skipRests: false,
        restTone: `${octave}G`,
      });

      // Initialize melody timing state as single track, preserving mutation metadata
      melodyState = {
        notes: parsedMelody,
        index: 0,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        type: "single",
      };

      // Preserve mutation metadata if present
      if (parsedMelody.hasMutation) {
        melodyState.hasMutation = true;
        melodyState.originalContent = parsedMelody.originalContent;
        melodyState.mutationTriggerPosition =
          parsedMelody.mutationTriggerPosition;

        // Copy multiple mutation zones properties
        if (parsedMelody.mutationTriggerPositions) {
          melodyState.mutationTriggerPositions =
            parsedMelody.mutationTriggerPositions;
          melodyState.currentMutationZone =
            parsedMelody.currentMutationZone || 0;
        }
      }
    } else {
      // Multiple tracks - create state for parallel playback with independent timing

      // Debug logging for parallel tracks
      // Parse parallel track melody format

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
          totalElapsedBeats: 0, // Total musical time elapsed for this track
          lastMutationTriggered: false, // Flag to prevent rapid re-triggering of mutations
        })),
      };

      // For backward compatibility, set parsedMelody to the first track
      parsedMelody = melodyTracks.tracks[0];
      sequence = extractTones(parsedMelody, {
        skipRests: false,
        restTone: `${octave}G`,
      });
    }
  } else {
    // No melody provided - set up fallback notes early in boot
    // Use a simple pentatonic scale pattern that sounds pleasant
    originalMelodyString = "cdefgab";

    // Parse the fallback melody using the same system as regular melodies
    melodyTracks = parseSimultaneousMelody(originalMelodyString, octave);
    parsedMelody = melodyTracks.tracks[0];

    // Extract tone strings for the fallback sequence
    sequence = extractTones(parsedMelody, {
      skipRests: false,
      restTone: `${octave}G`,
    });

    // Initialize fallback melody timing state (same structure as regular melodies)
    melodyState = {
      notes: parsedMelody,
      index: 0,
      baseTempo: baseTempo,
      isPlaying: false,
      startTime: performance.now(),
      timingMode: parseFloat(colon[0]) || 1.0,
      type: "single",
      isFallback: true, // Mark as fallback for special yellow coloring
    };
  }

  // Check for melody mode in params
  if (params[1] === "sync") {
    melodyMode = "sync"; // Sync melody to UTC clock
  } else {
    melodyMode = "continuous"; // Play melody continuously based on timing mode
  }

  // --- Set the HUD preview clock label immediately on boot ---
  if (typeof hud !== "undefined" && hud.label) {
    // Don't set any color - let disk.mjs handle all HUD coloring automatically
    // Set a placeholder time string until paint runs
    hud.label("--:--:--:---", undefined, 0);
  }

  // Build octave control buttons
  buildOctaveButtons(api);
}

function paint({
  wipe,
  ink,
  write,
  clock,
  screen,
  sound,
  api,
  help,
  hud,
  typeface,
}) {
  const syncedDate = clock.time(); // Get time once at the beginning

  // Simple gray background instead of rainbow colors
  wipe("gray");

  const availableWidth = screen.width;
  const availableHeight = screen.height;

  // Temporarily disable waveform painting to see timeline clearly
  // sound.paint.bars(
  //   api,
  //   sound.speaker.amplitudes.left,
  //   help.resampleArray(sound.speaker.waveforms.left, 8),
  //   0,
  //   0,
  //   availableWidth,
  //   availableHeight,
  //   [255, 255, 0, 255],
  //   {
  //     noamp: true,
  //     nobounds: true,
  //     primaryColor: [255, 255, 0, 128],
  //     secondaryColor: [0, 0, 0, 128],
  //   },
  // );

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
    // Build colored melody string for HUD label only (no timeline on main screen)
    let coloredMelodyStringForTimeline = null;
    if (hasMelodyContent(melodyState)) {
      // Build the colored melody string for HUD use only
      const currentMelodyString = buildCurrentMelodyString(
        originalMelodyString,
        melodyState,
      );
      coloredMelodyStringForTimeline = buildColoredMelodyStringUnified(
        currentMelodyString,
        melodyState,
      );
      // Note: No longer drawing melody timeline on main screen
    }
    // --- Scaled Clock Output ---
    if (!paint.scaledClockAnchor) {
      paint.scaledClockAnchor = {
        utc: syncedDate.getTime(),
        perf: performance.now(),
        divisor: currentTimeDivisor,
      };
    }
    if (paint.scaledClockAnchor.divisor !== currentTimeDivisor) {
      const nowPerf = performance.now();
      const elapsed =
        (nowPerf - paint.scaledClockAnchor.perf) *
        (1 / paint.scaledClockAnchor.divisor);
      const newAnchorUtc = paint.scaledClockAnchor.utc + elapsed;
      paint.scaledClockAnchor = {
        utc: newAnchorUtc,
        perf: nowPerf,
        divisor: currentTimeDivisor,
      };
    }
    const nowPerf = performance.now();
    const elapsed =
      (nowPerf - paint.scaledClockAnchor.perf) * (1 / currentTimeDivisor);
    const scaledTime = new Date(paint.scaledClockAnchor.utc + elapsed);

    const morning = scaledTime.getHours() < 12;
    let hours = scaledTime.getHours();
    hours = hours % 12;
    hours = hours ? hours : 12;
    const minutes = pad(scaledTime.getMinutes());
    const displaySeconds = pad(scaledTime.getSeconds());
    const millis = pad(scaledTime.getMilliseconds(), 3);
    const ampm = morning ? "AM" : "PM";
    const timeString =
      hours + ":" + minutes + ":" + displaySeconds + ":" + millis + " " + ampm;
    let fontSize = 1;

    // --- Initialize NOW line position based on timing divisor ---
    // Calculate the Y position of the cyan line based on timing divisor
    const meterStartY = 0;
    const meterEndY = screen.height;
    const meterHeight = meterEndY - meterStartY;
    const minTiming = 0.05;
    const maxTiming = 3.0;
    const centerY = Math.round(screen.height / 2);
    const scaleY = meterHeight / (maxTiming - minTiming);
    const currentYPos = Math.round(
      centerY + (currentTimeDivisor - 1.0) * scaleY,
    );

    // Initialize NOW line position to match the timing line if not set
    if (nowLineY === 0) {
      nowLineY = currentYPos; // Start at timing position, then becomes draggable
    }

    clockDrawn = true;

    // Paint octave control buttons
    const octaveText = `${octave}`;
    const glyphWidth = typeface.glyphs["0"].resolution[0];
    const padding = 4; // Padding on left and right of octave number
    const octaveTextWidth = octaveText.length * glyphWidth + padding * 2;
    const buttonWidth = 16;
    const buttonHeight = 16;
    const totalWidth = buttonWidth + octaveTextWidth + buttonWidth;
    const startX = screen.width - totalWidth;
    const startY = screen.height - buttonHeight;

    // Paint minus button
    octaveMinusBtn?.paint((btn) => {
      const disabled = octave <= 1;
      const isPressed = btn.down;
      const buttonColor = disabled ? "gray" : isPressed ? "white" : "red";
      const textColor = disabled ? "darkgray" : isPressed ? "red" : "white";

      ink(buttonColor).box(btn.box.x, btn.box.y, btn.box.w, btn.box.h);
      ink(textColor).write("-", {
        x: btn.box.x + btn.box.w / 2 - 3,
        y: btn.box.y + btn.box.h / 2 - 4,
        scale: 1,
      });
    });

    // Paint octave text in the middle with background
    const flashDuration = 150; // Flash for 150ms (faster)
    const isFlashing = performance.now() - octaveFlashTime < flashDuration;
    const octaveBgColor = isFlashing ? "white" : "blue";
    const octaveTextColor = isFlashing ? "blue" : "white";

    ink(octaveBgColor).box(
      startX + buttonWidth,
      startY,
      octaveTextWidth,
      buttonHeight,
    );
    ink(octaveTextColor).write(octaveText, {
      x: startX + buttonWidth + padding,
      y: startY + 3,
      scale: 1,
    });

    // Paint plus button
    octavePlusBtn?.paint((btn) => {
      const disabled = octave >= 8;
      const isPressed = btn.down;
      const buttonColor = disabled ? "gray" : isPressed ? "white" : "red";
      const textColor = disabled ? "darkgray" : isPressed ? "red" : "white";

      ink(buttonColor).box(btn.box.x, btn.box.y, btn.box.w, btn.box.h);
      ink(textColor).write("+", {
        x: btn.box.x + btn.box.w / 2 - 3,
        y: btn.box.y + btn.box.h / 2 - 4,
        scale: 1,
      });
    });

    // --- HUD preview clock label in the corner ---
    if (typeof hud !== "undefined" && hud.label) {
      // Check if melody timing has started (based on whether first synth has fired)
      const melodyTimingStarted = hasFirstSynthFired;

      // Provide "clock" prefix plus melody content - let disk.mjs handle all coloring
      let previewStringDecorative = "";
      let previewStringPlain = "";
      if (originalMelodyString && originalMelodyString.trim().length > 0) {
        if (melodyTimingStarted) {
          // Melody timing has started - use colored version with highlighting
          const currentMelodyString = buildCurrentMelodyString(
            originalMelodyString,
            melodyState,
          );
          const coloredMelody =
            coloredMelodyStringForTimeline ||
            buildColoredMelodyStringUnified(currentMelodyString, melodyState);

          // Ensure the space after "clock" doesn't get colored by the melody colors
          previewStringDecorative = `\\white\\clock \\white\\` + coloredMelody;
        } else {
          // Melody timing hasn't started yet - show plain uncolored text

          previewStringDecorative = `\\white\\clock ${originalMelodyString}`;
        }
        previewStringPlain = `clock ${originalMelodyString}`;
      } else {
        // No melody content - just show "clock" with explicit neutral color

        previewStringDecorative = `\\white\\clock`;
        previewStringPlain = `clock`;
      }

      // To ensure the shadow is always black and not colored by inline codes, filter color codes from the decorative string
      function stripColorCodes(str) {
        // Remove all \\color\\ sequences including RGB values like \\255,20,147\\
        return str.replace(
          /\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+)\\/g,
          "",
        );
      }
      const shadowString = stripColorCodes(previewStringDecorative);
      // Draw the shadow label first, with all color codes stripped, in black
      if (hud.label && typeof hud.label === "function") {
        // Always pass the logical (colorless) string as the 4th argument to hud.label
        // Let disk.mjs handle all HUD coloring automatically - no explicit color override
        hud.label(shadowString, undefined, 0, previewStringPlain); // shadow
        hud.supportsInlineColor = true;
        hud.disablePieceNameColoring = true; // Let our custom colors override piece name coloring
        // Force cache invalidation for animated color changes by updating frame hash
        hud.frameHash = performance.now(); 
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
      let inWaveform = false;
      let currentTrackIndex = 0;
      let noteIndexInCurrentTrack = 0;
      let now = performance.now();
      let currentNoteIndex = 0;

      // Check if timing has actually started before applying current note highlighting
      const timingHasStarted = hasFirstSynthFired;

      // Use the unified function to get current note index
      if (timingHasStarted && melodyState && melodyState.type === "single") {
        currentNoteIndex = getCurrentNoteIndex(melodyState);
      } else if (
        timingHasStarted &&
        melodyState &&
        melodyState.type === "parallel"
      ) {
        // For parallel, handled below per track
      } else {
        // If timing hasn't started, set currentNoteIndex to -1 so no note is highlighted
        currentNoteIndex = -1;
      }

      // Split by spaces to get groups, then process each group
      const groups = melodyString.trim().split(/\s+/);
      let globalCharIndex = 0;
      let groupStartPositions = [];

      // Calculate the starting position of each group in the original string
      let searchStart = 0;
      for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
        const group = groups[groupIdx];
        const groupStart = melodyString.indexOf(group, searchStart);
        groupStartPositions.push(groupStart);
        searchStart = groupStart + group.length;
      }

      // First pass: identify all note character positions and their duration modifiers
      for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
        const group = groups[groupIdx];
        const groupStartChar = groupStartPositions[groupIdx];
        noteIndex = 0; // Reset note index for each group

        for (let i = 0; i < group.length; i++) {
          const char = group[i];
          const absoluteCharIndex = groupStartChar + i;

          if (char === "{") {
            inWaveform = true;
            continue;
          } else if (char === "}") {
            inWaveform = false;
            continue;
          } else if (inWaveform) {
            continue;
          }

          // Handle octave-first notation (5f, 4c#, etc.) - octave number should be part of the note unit
          if (
            /[0-9]/.test(char) &&
            i + 1 < group.length &&
            /[a-g]/i.test(group[i + 1])
          ) {
            let noteStart = i; // Start with the octave number
            let noteEnd = i + 1; // Move to the note letter

            // Check for sharp modifiers
            if (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (nextChar === "s" || nextChar === "#") {
                noteEnd++;
              }
            }

            // Check for duration modifiers and asterisks
            while (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (
                nextChar === "." ||
                nextChar === "-" ||
                nextChar === "<" ||
                nextChar === ">" ||
                nextChar === "," ||
                nextChar === "*"
              ) {
                noteEnd++;
              } else {
                break;
              }
            }

            const noteIndexToUse =
              melodyState && melodyState.type === "parallel"
                ? noteIndex
                : noteIndex;
            for (let j = noteStart; j <= noteEnd; j++) {
              noteCharPositions.push({
                charIndex: groupStartChar + j,
                noteIndex: noteIndexToUse,
                trackIndex: groupIdx,
              });
            }
            noteIndex++;
            i = noteEnd;
          }
          // Handle regular note letters (without octave prefix) or relative octave modifiers (+c, -f, etc.)
          else if (/[a-g#_+-]/i.test(char)) {
            let noteStart = i;
            let noteEnd = i;

            // Handle relative octave modifiers (++, --, etc.)
            if (char === "+" || char === "-") {
              while (
                noteEnd + 1 < group.length &&
                group[noteEnd + 1] === char
              ) {
                noteEnd++;
              }
              // Now we should have a note letter
              if (
                noteEnd + 1 < group.length &&
                /[a-g]/i.test(group[noteEnd + 1])
              ) {
                noteEnd++;
              } else {
                // Handle standalone dash as rest (like ---)
                if (char === "-") {
                  const noteIndexToUse =
                    melodyState && melodyState.type === "parallel"
                      ? noteIndex
                      : noteIndex;
                  for (let j = noteStart; j <= noteEnd; j++) {
                    noteCharPositions.push({
                      charIndex: groupStartChar + j,
                      noteIndex: noteIndexToUse,
                      trackIndex: groupIdx,
                    });
                  }
                  noteIndex++;
                  i = noteEnd;
                  continue;
                } else {
                  // Invalid + without note, skip
                  continue;
                }
              }
            }

            // Check for sharp modifiers (for regular notes or after relative modifiers)
            if (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (nextChar === "s" || nextChar === "#") {
                noteEnd++;
              }
            }

            // Check for duration modifiers and asterisks
            while (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (
                nextChar === "." ||
                nextChar === "-" ||
                nextChar === "<" ||
                nextChar === ">" ||
                nextChar === "," ||
                nextChar === "*"
              ) {
                noteEnd++;
              } else {
                break;
              }
            }

            const noteIndexToUse =
              melodyState && melodyState.type === "parallel"
                ? noteIndex
                : noteIndex;
            for (let j = noteStart; j <= noteEnd; j++) {
              noteCharPositions.push({
                charIndex: groupStartChar + j,
                noteIndex: noteIndexToUse,
                trackIndex: groupIdx,
              });
            }
            noteIndex++;
            i = noteEnd;
          }
        }
      }
      // Color the original melody string character by character
      let inWaveformForColoring = false;

      // For fade logic - red at start, fading to orange as note ends
      function getRedNoteColor() {
        // Always return red for currently playing notes - no orange fade
        return "red";
      }

      // Helper function to check if a note is mutated
      function isNoteMutated(noteCharData) {
        if (!noteCharData) return false;

        if (melodyState && melodyState.type === "single") {
          const currentNote = melodyState.notes[noteCharData.noteIndex];
          return currentNote && currentNote.isMutation;
        } else if (
          melodyState &&
          melodyState.type === "parallel" &&
          melodyState.trackStates &&
          noteCharData.trackIndex < melodyState.trackStates.length
        ) {
          const track = melodyState.tracks[noteCharData.trackIndex];
          const currentNote = track[noteCharData.noteIndex];
          return currentNote && currentNote.isMutation;
        }

        return false;
      }

      // Get color for mutated notes (goldenrod for non-active mutations, red-orange fade for active)
      function getMutatedNoteColor(
        isCurrentlyPlaying = false,
        noteCharData = null,
      ) {
        // Check if this is the specific note that was just mutated (rainbow flash)
        const isRecentlyMutatedNote =
          noteCharData &&
          noteCharData.noteIndex === recentlyMutatedNoteIndex &&
          noteCharData.trackIndex === recentlyMutatedTrackIndex;

        // Check if this note should flash rainbow during mutation
        const shouldFlashForThisNote =
          shouldFlashMutation && isRecentlyMutatedNote;

        if (shouldFlashForThisNote) {
          // Rainbow flash effect - same as asterisk
          const time = now * 0.005; // Slow down the cycle
          const hue = (time % 1) * 360; // 0-360 degrees

          // Convert HSV to RGB for rainbow effect
          function hsvToRgb(h, s, v) {
            const c = v * s;
            const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
            const m = v - c;
            let r, g, b;

            if (h >= 0 && h < 60) {
              r = c;
              g = x;
              b = 0;
            } else if (h >= 60 && h < 120) {
              r = x;
              g = c;
              b = 0;
            } else if (h >= 120 && h < 180) {
              r = 0;
              g = c;
              b = x;
            } else if (h >= 180 && h < 240) {
              r = 0;
              g = x;
              b = c;
            } else if (h >= 240 && h < 300) {
              r = x;
              g = 0;
              b = c;
            } else {
              r = c;
              g = 0;
              b = x;
            }

            return [
              Math.round((r + m) * 255),
              Math.round((g + m) * 255),
              Math.round((b + m) * 255),
            ];
          }

          return hsvToRgb(hue, 1, 1); // Full saturation and brightness for vivid rainbow
        }

        if (isCurrentlyPlaying) {
          // When actively playing, mutated notes still get the red-to-orange fade
          return getRedNoteColor();
        } else {
          // When not playing, mutated notes stay goldenrod
          return "goldenrod";
        }
      }
      const flashDuration = 200;
      const mutationFlashDuration = 300; // Longer flash for mutations
      const shouldFlashGreen =
        now -
          (typeof specialCharFlashTime !== "undefined"
            ? specialCharFlashTime
            : 0) <
        flashDuration;
      const shouldFlashMutation =
        now -
          (typeof mutationFlashTime !== "undefined" ? mutationFlashTime : 0) <
        mutationFlashDuration;

      // Reset recently mutated tracking when flash period expires
      if (!shouldFlashMutation) {
        recentlyMutatedNoteIndex = -1;
        recentlyMutatedTrackIndex = -1;
      }

      let currentTrackForColoring = 0; // Track which parallel track we're processing for coloring

      for (let i = 0; i < melodyString.length; i++) {
        const char = melodyString[i];
        let color = "yellow";

        // Handle disabled group detection for x prefix syntax
        if (char === " ") {
          // Space separates groups
          currentTrackForColoring++;
          color = timingHasStarted ? "yellow" : "gray";
        }
        // Check if this character is part of a disabled group (starts with 'x')
        else if (char === "x") {
          // Check if this 'x' is at the start of a group
          let isGroupPrefix = false;
          if (i === 0) {
            // First character of the string
            isGroupPrefix = true;
          } else {
            // Check if preceded by space(s)
            let j = i - 1;
            while (j >= 0 && melodyString[j] === " ") {
              j--;
            }
            isGroupPrefix = j < 0 || melodyString[j] === " ";
          }

          if (isGroupPrefix) {
            color = "brown"; // Render 'x' prefix in brown
          } else {
            // Regular 'x' character, handle normally
            const noteCharData = noteCharPositions.find(
              (ncp) => ncp.charIndex === i,
            );
            if (noteCharData) {
              color = timingHasStarted ? "yellow" : "gray";
            } else {
              color = timingHasStarted ? "yellow" : "gray";
            }
          }
        }
        // Check if we're inside a disabled group (group that starts with 'x')
        else {
          // Find which group this character belongs to
          let currentGroupIndex = 0;
          let charCount = 0;
          const groups = melodyString.trim().split(/\s+/);
          let isInDisabledGroup = false;

          // Find which group contains this character
          for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
            const group = groups[groupIdx];
            const groupStart = melodyString.indexOf(group, charCount);
            const groupEnd = groupStart + group.length;

            if (i >= groupStart && i < groupEnd) {
              currentGroupIndex = groupIdx;
              isInDisabledGroup = group.startsWith("x");
              break;
            }
            charCount = groupEnd;
          }

          if (isInDisabledGroup) {
            // We're inside a disabled group - render everything in gray
            color = "gray";
          }
          // If we're not in a disabled group, continue with normal logic
          else {
            // Check if this character is part of the currently playing note
            const noteCharData = noteCharPositions.find(
              (ncp) => ncp.charIndex === i,
            );
            let isCurrentlyPlayingNote = false;

            if (noteCharData) {
              if (melodyState && melodyState.type === "single") {
                isCurrentlyPlayingNote =
                  noteCharData.noteIndex === currentNoteIndex;
              } else if (
                melodyState &&
                melodyState.type === "parallel" &&
                melodyState.trackStates &&
                noteCharData.trackIndex < melodyState.trackStates.length
              ) {
                // Use the unified function to get current note index for this track
                const currentPlayingIndex = getCurrentNoteIndex(
                  melodyState,
                  noteCharData.trackIndex,
                );
                isCurrentlyPlayingNote =
                  noteCharData.noteIndex === currentPlayingIndex;
              }
            }

            if (char === "{") {
              inWaveformForColoring = true;
              color = shouldFlashGreen
                ? "green"
                : timingHasStarted
                  ? "yellow"
                  : "gray";
            } else if (char === "}") {
              inWaveformForColoring = false;
              color = shouldFlashGreen
                ? "green"
                : timingHasStarted
                  ? "yellow"
                  : "gray";
            } else if (inWaveformForColoring) {
              color = timingHasStarted ? "cyan" : "gray";
            } else if (char === "*") {
              // Special handling for mutation asterisk - white flash when triggered, rainbow otherwise
              if (
                shouldFlashMutation &&
                (triggeredAsteriskPositions.includes(i) ||
                  triggeredAsteriskPositions.includes("*"))
              ) {
                // White flash when asterisk is triggered
                color = "white";
              } else {
                // Create a time-based rainbow that cycles through hues
                const time = now * 0.005; // Slow down the cycle
                const hue = (time % 1) * 360; // 0-360 degrees

                // Convert HSV to RGB for rainbow effect
                function hsvToRgb(h, s, v) {
                  const c = v * s;
                  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
                  const m = v - c;
                  let r, g, b;

                  if (h >= 0 && h < 60) {
                    r = c;
                    g = x;
                    b = 0;
                  } else if (h >= 60 && h < 120) {
                    r = x;
                    g = c;
                    b = 0;
                  } else if (h >= 120 && h < 180) {
                    r = 0;
                    g = c;
                    b = x;
                  } else if (h >= 180 && h < 240) {
                    r = 0;
                    g = x;
                    b = c;
                  } else if (h >= 240 && h < 300) {
                    r = x;
                    g = 0;
                    b = c;
                  } else {
                    r = c;
                    g = 0;
                    b = x;
                  }

                  return [
                    Math.round((r + m) * 255),
                    Math.round((g + m) * 255),
                    Math.round((b + m) * 255),
                  ];
                }

                color = hsvToRgb(hue, 1, 1); // Full saturation and brightness for vivid rainbow
              }
            } else if (melodyState && melodyState.isFallback) {
              if (noteCharData) {
                if (isCurrentlyPlayingNote) {
                  // Check if this note is mutated and use different coloring
                  const isMutated = isNoteMutated(noteCharData);

                  // Check if this is a duration punctuation character within the current note
                  if (/[.,]/i.test(char)) {
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Duration punctuation uses mutation or normal fade
                  } else if (/[s]/i.test(char)) {
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Sharp 's' uses same color as note letters
                  } else if (char === "_") {
                    // Rest character - should flash red like regular notes
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Rests use same color as note letters
                  } else if (/[0-9+\-#<>]/i.test(char)) {
                    color = "green"; // Other special characters in current note flash green
                  } else {
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Note letters use mutation or normal fade
                  }
                } else {
                  // Not currently playing - check if it's a mutated note
                  const isMutated = isNoteMutated(noteCharData);
                  if (isMutated) {
                    color = getMutatedNoteColor(false, noteCharData); // Goldenrod for non-active mutated notes
                  } else {
                    // Use gray if timing hasn't started, yellow if it has
                    color = timingHasStarted ? "yellow" : "gray";
                  }
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
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Duration punctuation uses mutation or normal fade
                  } else if (/[s]/i.test(char)) {
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Sharp 's' uses same color as note letters
                  } else if (char === "_") {
                    // Rest character - should flash red like regular notes
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Rests use same color as note letters
                  } else if (/[0-9+\-#<>]/i.test(char)) {
                    color = "green"; // Other special characters in current note flash green
                  } else {
                    color = isMutated
                      ? getMutatedNoteColor(true, noteCharData)
                      : getRedNoteColor(); // Note letters use mutation or normal fade
                  }
                } else {
                  // Not currently playing - check if it's a mutated note
                  const isMutated = isNoteMutated(noteCharData);
                  if (isMutated) {
                    color = getMutatedNoteColor(false, noteCharData); // Goldenrod for non-active mutated notes
                  } else {
                    // Use gray if timing hasn't started, yellow if it has
                    color = timingHasStarted ? "yellow" : "gray";
                  }
                }
              } else {
                // For non-note characters, use gray until timing starts
                color = timingHasStarted ? "yellow" : "gray";
              }
            }
          }
        }

        // Handle RGB arrays specially - convert to simple RGB escape code format
        if (Array.isArray(color)) {
          const rgbString = `\\${color[0]},${color[1]},${color[2]}\\${char}`;
          coloredMelodyString += rgbString;
        } else {
          coloredMelodyString += `\\${color}\\${char}`;
        }
      }

      return coloredMelodyString;
    }
    // (Remove any other preview label drawing from the center)
  } else {
    ink("red").write("SYNCING...", { center: "xy", size: 2 });
    // Note: Octave buttons are now painted after timeline universally
  }

  // Draw flowing note visualization last so it appears on top of everything
  // Always draw timeline - use current time if syncedDate not available
  const timeForTimeline = syncedDate || new Date();
  drawFlowingNotes(ink, write, screen, melodyState, timeForTimeline);

  // Paint octave control buttons after timeline so they appear on top
  const octaveText = `${octave}`;
  const glyphWidth = typeface.glyphs["0"].resolution[0];
  const padding = 4; // Padding on left and right of octave number
  const octaveTextWidth = octaveText.length * glyphWidth + padding * 2;
  const buttonWidth = 16;
  const buttonHeight = 16;
  const totalWidth = buttonWidth + octaveTextWidth + buttonWidth;
  const startX = screen.width - totalWidth;
  const startY = screen.height - buttonHeight;

  // Paint minus button with proper state handling
  octaveMinusBtn?.paint((btn) => {
    const disabled = octave <= 1;
    const isPressed = btn.down;
    const buttonColor = disabled ? "gray" : isPressed ? "white" : "red";
    const textColor = disabled ? "darkgray" : isPressed ? "red" : "white";

    ink(buttonColor).box(btn.box.x, btn.box.y, btn.box.w, btn.box.h);
    ink(textColor).write("-", {
      x: btn.box.x + btn.box.w / 2 - 3,
      y: btn.box.y + btn.box.h / 2 - 4,
      scale: 1,
    });
  });

  // Paint octave text in the middle with background
  const flashDuration = 150; // Flash for 150ms (faster)
  const isFlashing = performance.now() - octaveFlashTime < flashDuration;
  const octaveBgColor = isFlashing ? "white" : "blue";
  const octaveTextColor = isFlashing ? "blue" : "white";

  ink(octaveBgColor).box(
    startX + buttonWidth,
    startY,
    octaveTextWidth,
    buttonHeight,
  );
  ink(octaveTextColor).write(octaveText, {
    x: startX + buttonWidth + padding,
    y: startY + 3,
    scale: 1,
  });

  // Paint plus button with proper state handling
  octavePlusBtn?.paint((btn) => {
    const disabled = octave >= 8;
    const isPressed = btn.down;
    const buttonColor = disabled ? "gray" : isPressed ? "white" : "green";
    const textColor = disabled ? "darkgray" : isPressed ? "green" : "white";

    ink(buttonColor).box(btn.box.x, btn.box.y, btn.box.w, btn.box.h);
    ink(textColor).write("+", {
      x: btn.box.x + btn.box.w / 2 - 3,
      y: btn.box.y + btn.box.h / 2 - 4,
      scale: 1,
    });
  });

  // No separate UTC time display - consolidated into main clock

  // Draw the horizontal bar and main clock display on top of everything
  if (syncedDate) {
    // Use the same color logic as the timeline - match the NOW line colors
    let nowLineColor;
    if (isDraggingNowLine) {
      nowLineColor = "yellow"; // Yellow when dragging
    } else if (currentTimeDivisor < 1.0) {
      nowLineColor = "lime"; // Green for faster
    } else if (currentTimeDivisor > 1.0) {
      nowLineColor = "red"; // Red for slower
    } else {
      nowLineColor = "white"; // White for normal
    }

    // Draw crosshair style NOW line - left and right segments only
    const crosshairLength = 30; // Length of each crosshair segment
    
    // Draw shadow first (flush left with main line, 1px offset down)
    // Left crosshair shadow
    ink("black").line(0, nowLineY + 1, crosshairLength, nowLineY + 1);
    // Right crosshair shadow
    ink("black").line(screen.width - crosshairLength, nowLineY + 1, screen.width, nowLineY + 1);
    
    // Draw main crosshair lines
    // Left crosshair
    ink(nowLineColor).line(0, nowLineY, crosshairLength, nowLineY);
    // Right crosshair
    ink(nowLineColor).line(screen.width - crosshairLength, nowLineY, screen.width, nowLineY);

    // Create current note display above the NOW line
    let currentNotesText = "";
    if (hasFirstSynthFired && melodyState) {
      const currentNotes = [];
      
      if (melodyState.type === "single") {
        const currentNoteIndex = getCurrentNoteIndex(melodyState);
        if (currentNoteIndex >= 0 && melodyState.notes && melodyState.notes[currentNoteIndex]) {
          const currentNote = melodyState.notes[currentNoteIndex];
          currentNotes.push(currentNote.note.toUpperCase());
        }
      } else if (melodyState.type === "parallel" && melodyState.trackStates) {
        melodyState.trackStates.forEach((trackState, trackIndex) => {
          if (trackState && trackState.track && trackState.track.length > 0) {
            const currentNoteIndex = getCurrentNoteIndex(melodyState, trackIndex);
            if (currentNoteIndex >= 0 && currentNoteIndex < trackState.track.length) {
              const currentNote = trackState.track[currentNoteIndex];
              if (currentNote && currentNote.note !== "rest") {
                currentNotes.push(currentNote.note.toUpperCase());
              }
            }
          }
        });
      }
      
      if (currentNotes.length > 0) {
        currentNotesText = currentNotes.join(" + ");
      }
    }

    // Create main time display with timing prefix
    const scaledTime = new Date(syncedDate.getTime());
    const morning = scaledTime.getHours() < 12;
    let hours = scaledTime.getHours() % 12;
    hours = hours ? hours : 12;
    const minutes = pad(scaledTime.getMinutes());
    const displaySeconds = pad(scaledTime.getSeconds());
    const millis = pad(scaledTime.getMilliseconds(), 3);
    const ampm = morning ? "AM" : "PM";
    
    // Add timing prefix based on speed
    let timingPrefix = "";
    let clockColor = "white";
    
    if (currentTimeDivisor < 1.0) {
      // Faster than real-time - lime text with + prefix
      timingPrefix = "+";
      clockColor = "lime";
    } else if (currentTimeDivisor > 1.0) {
      // Slower than real-time - red text with - prefix
      timingPrefix = "-";
      clockColor = "red";
    } else {
      // Normal time (1.0) - white text with space prefix
      timingPrefix = " ";
      clockColor = "white";
    }
    
    // Main time string with timing prefix
    const mainTimeString = timingPrefix + hours + ":" + minutes + ":" + displaySeconds + ":" + millis + " " + ampm;
    
    // Responsive font sizing - check if scale 2 fits, otherwise use scale 1
    let fontSize = 2;
    const textWidth = mainTimeString.length * 6 * fontSize; // Estimate text width
    if (textWidth > screen.width - 20) { // Leave 20px margin
      fontSize = 1;
    }



    // Draw clock display with color matching the NOW line
    const shadowOffset = 1;
    
    // Time string using actual UTC time
    const displayHours = syncedDate.getHours() % 12;
    const finalDisplayHours = displayHours ? displayHours : 12;
    const displayMinutes = pad(syncedDate.getMinutes());
    const finalDisplaySeconds = pad(syncedDate.getSeconds());
    const displayMillis = pad(syncedDate.getMilliseconds(), 3);
    const displayAmpm = syncedDate.getHours() < 12 ? "AM" : "PM";
    const timeString = finalDisplayHours + ":" + displayMinutes + ":" + finalDisplaySeconds + ":" + displayMillis + " " + displayAmpm;
    
    // Responsive font sizing - prefer size 2 if there's enough space
    let timeDisplayFontSize = 2;
    const minVerticalSpace = 25; // Minimum space needed above NOW line for size 2
    const availableVerticalSpace = nowLineY - shadowOffset;
    
    // Check if we have enough vertical space and horizontal space for size 2
    if (availableVerticalSpace < minVerticalSpace || screen.width < 250) {
      timeDisplayFontSize = 1;
    }
    
    // Calculate Y position so text midpoint aligns with NOW line
    const textHeight = 8 * timeDisplayFontSize; // Approximate text height
    const centerTimeY = nowLineY - (textHeight / 2);
    
    // Draw shadow first using center: "x" for proper horizontal centering
    ink("black").write(timeString, {
      center: "x",
      y: centerTimeY + shadowOffset,
      size: timeDisplayFontSize,
    });
    
    // Draw main text using NOW line color for visual consistency
    ink(nowLineColor).write(timeString, {
      center: "x",
      y: centerTimeY,
      size: timeDisplayFontSize,
    });
  }
}

// Draw a single version of the melody string with the active note highlighted
function drawMelodyTimeline(
  ink,
  write,
  screen,
  melodyState,
  coloredMelodyStringOverride,
) {
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
  let coloredMelodyString =
    coloredMelodyStringOverride ||
    buildColoredMelodyStringUnified(
      buildCurrentMelodyString(melodyString, melodyState),
      melodyState,
    );

  write(coloredMelodyString, {
    x: startX,
    y: timelineY,
    scale: 1,
  });

  // Draw timing info below the melody string
  const infoY = timelineY + 12;
  const timeDivisor = currentTimeDivisor;
  const timingText =
    screen.width < 250
      ? `${timeDivisor.toFixed(1)}s`
      : `${timeDivisor.toFixed(1)}s timing`;
  ink("cyan").write(timingText, {
    x: timelineStartX,
    y: infoY,
    scale: 1,
  });
}

// Build the current melody string showing actual mutated notes
function buildCurrentMelodyString(originalMelodyString, melodyState) {
  if (!melodyState || !originalMelodyString) return originalMelodyString;

  // For tracks without mutations, return original string
  if (melodyState.type === "single" && !melodyState.hasMutation) {
    return originalMelodyString;
  }

  if (melodyState.type === "parallel" && melodyState.tracks) {
    const hasMutations = melodyState.tracks.some((track) => track.hasMutation);
    if (!hasMutations) return originalMelodyString;
  }

  // For tracks with mutations, we need to replace mutated notes in the original string
  let modifiedString = originalMelodyString;

  if (
    melodyState.type === "single" &&
    melodyState.hasMutation &&
    melodyState.notes
  ) {
    // Find all note positions in the original string and map them to track notes
    const notePositions = [];
    let noteIndex = 0;

    for (let i = 0; i < originalMelodyString.length; i++) {
      const char = originalMelodyString[i];

      // Skip waveform specifiers
      if (char === "{") {
        while (
          i < originalMelodyString.length &&
          originalMelodyString[i] !== "}"
        )
          i++;
        continue;
      }

      // Skip asterisks and spaces
      if (char === "*" || char === " ") continue;

      // Check if this is a note letter (main note character) or underscore rest
      if (/[a-g_]/i.test(char)) {
        const trackNote = melodyState.notes[noteIndex];
        if (trackNote && trackNote.isMutation) {
          // This note was mutated, record its position for replacement
          const displayNote =
            trackNote.note === "rest" ? "_" : trackNote.note.toLowerCase();
          notePositions.push({
            stringIndex: i,
            trackIndex: noteIndex,
            originalChar: char,
            newNote: displayNote,
          });
        }
        noteIndex++;
      }
    }

    // Replace mutated notes in the string (in reverse order to preserve indices)
    notePositions.reverse().forEach((pos) => {
      modifiedString =
        modifiedString.substring(0, pos.stringIndex) +
        pos.newNote +
        modifiedString.substring(pos.stringIndex + 1);
    });
  }

  // For parallel tracks with mutations (space-based groups)
  if (melodyState.type === "parallel" && melodyState.tracks) {
    const notePositions = [];

    // Split by spaces to get groups, then process each group
    const groups = originalMelodyString.trim().split(/\s+/);
    let globalCharIndex = 0;
    let groupStartPositions = [];

    // Calculate the starting position of each group in the original string
    let searchStart = 0;
    for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
      const group = groups[groupIdx];
      const groupStart = originalMelodyString.indexOf(group, searchStart);
      groupStartPositions.push(groupStart);
      searchStart = groupStart + group.length;
    }

    // Process each group and find mutated notes
    for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
      const group = groups[groupIdx];
      const groupStartChar = groupStartPositions[groupIdx];
      const track = melodyState.tracks[groupIdx];

      if (!track) continue; // Skip if no corresponding track

      let noteIndexInTrack = 0;
      let inWaveform = false;

      for (let i = 0; i < group.length; i++) {
        const char = group[i];
        const absoluteCharIndex = groupStartChar + i;

        // Skip waveform specifiers
        if (char === "{") {
          inWaveform = true;
          continue;
        } else if (char === "}") {
          inWaveform = false;
          continue;
        } else if (inWaveform) {
          continue;
        }

        // Skip asterisks and x prefix
        if (char === "*" || char === "x") continue;

        // Check if this is a note letter or underscore rest
        if (/[a-g_]/i.test(char)) {
          const trackNote = track[noteIndexInTrack];
          if (trackNote && trackNote.isMutation) {
            // This note was mutated, record its position for replacement
            const displayNote =
              trackNote.note === "rest" ? "_" : trackNote.note.toLowerCase();
            notePositions.push({
              stringIndex: absoluteCharIndex,
              trackIndex: groupIdx,
              noteIndex: noteIndexInTrack,
              originalChar: char,
              newNote: displayNote,
            });
          }
          noteIndexInTrack++;
        }
      }
    }

    // Replace mutated notes in the string (in reverse order to preserve indices)
    notePositions.reverse().forEach((pos) => {
      modifiedString =
        modifiedString.substring(0, pos.stringIndex) +
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

    if (char === "{") {
      inWaveform = true;
      continue;
    } else if (char === "}") {
      inWaveform = false;
      continue;
    } else if (inWaveform) {
      // Skip all characters inside {waveform} specifiers
      continue;
    } else if (char === "(") {
      inGroup = true;
      continue;
    } else if (char === ")") {
      inGroup = false;
      currentTrackIndex++;
      noteIndexInTrack = 0;
      continue;
    } else if (char === " ") {
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
        const currentPlayingIndex =
          (trackState.noteIndex - 1 + trackState.track.length) %
          trackState.track.length;
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
        if (
          nextChar === "." ||
          nextChar === "-" ||
          nextChar === "<" ||
          nextChar === ">"
        ) {
          j++;
        } else {
          break;
        }
      }

      // If we're checking a character within this note unit, it belongs to the current note
      if (charIndex >= i && charIndex <= j) {
        if (currentTrackIndex < trackStates.length) {
          const trackState = trackStates[currentTrackIndex];
          const currentPlayingIndex =
            (trackState.noteIndex - 1 + trackState.track.length) %
            trackState.track.length;
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
    const yPos = graphEndY - normalizedPos * graphHeight;

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
        scale: 1,
      });
    }
  }

  // Draw current timing indicator
  const currentNormalizedPos = Math.max(
    0,
    Math.min(1, (currentTimeDivisor - minTiming) / range),
  );
  const currentYPos = graphEndY - currentNormalizedPos * graphHeight;

  // Draw a bright indicator line for current timing
  ink("cyan").line(graphStartX - 4, currentYPos, graphStartX + 12, currentYPos);

  // Draw current timing value
  const currentLabel = currentTimeDivisor.toFixed(1);
  ink("cyan").write(currentLabel, {
    x: graphStartX + 14,
    y: currentYPos - 4,
    scale: 1,
  });

  // Draw flow mode indicator
  const flowModeText = noteFlowMode === "stop" ? "🛑" : "🌊";
  ink("white").write(flowModeText, {
    x: graphStartX + 50,
    y: currentYPos - 4,
    scale: 1,
  });

  // Highlight 1.0s timing with special color
  if (Math.abs(currentTimeDivisor - 1.0) < 0.05) {
    ink("yellow").line(
      graphStartX - 4,
      currentYPos,
      graphStartX + 12,
      currentYPos,
    );
    ink("yellow").write(currentLabel, {
      x: graphStartX + 14,
      y: currentYPos - 4,
      scale: 1,
    });
    // Keep flow mode indicator visible even when yellow
    ink("white").write(flowModeText, {
      x: graphStartX + 50,
      y: currentYPos - 4,
      scale: 1,
    });
  }

  // Draw graph title
  ink("white").write("TIMING", {
    x: graphStartX - 6,
    y: graphStartY - 12,
    scale: 1,
  });
}

// Draw flowing notes visualization - vertical timeline using history buffer
function drawFlowingNotes(ink, write, screen, melodyState, syncedDate) {


  if (!melodyState || !hasMelodyContent(melodyState) || !syncedDate) {
    return;
  }

  // Initialize NOW line position if not set
  if (nowLineY === 0) {
    nowLineY = screen.height / 2;
  }

  // Get current time for calculations
  const currentTimeMs = syncedDate.getTime();

  // VERTICAL Timeline configuration - full screen width
  const timelineWidth = screen.width; // Use full screen width
  const timelineX = Math.round(screen.width / 2); // Center point
  const timelineStartX = 0; // Start at left edge of screen
  const timelineEndX = screen.width; // End at right edge of screen

  // Calculate timeline parameters - extend from top to bottom of screen
  const zoomFactor = Math.max(0.3, currentTimeDivisor); // Smart zoom based on timing
  const scaledPixelsPerSecond = Math.round(40 * zoomFactor); // Ensure integer pixels per second

  // Calculate time window to show future notes
  const futureTimeWindowSeconds = 10; // Show 10 seconds of future notes
  const futureTimeWindowMs = futureTimeWindowSeconds * 1000;

  // Determine number of tracks for width calculation
  let trackCount = 1;
  if (melodyState.type === "parallel" && melodyState.trackStates) {
    trackCount = melodyState.trackStates.length;
  }

  // First draw simplified timeline background (background layer)
  drawTimelineBackground(
    ink,
    write,
    screen,
    currentTimeMs,
    scaledPixelsPerSecond,
    futureTimeWindowSeconds,
    timelineStartX,
    timelineEndX,
    nowLineY,
  );

  // Draw the "NOW" line BEFORE notes so notes can blink on top of it
  // Use same color logic as the clock
  let nowLineColor;
  if (isDraggingNowLine) {
    nowLineColor = "yellow"; // Yellow when dragging
  } else if (currentTimeDivisor < 1.0) {
    nowLineColor = "lime"; // Green for faster
  } else if (currentTimeDivisor > 1.0) {
    nowLineColor = "red"; // Red for slower
  } else {
    nowLineColor = "white"; // White for normal
  }
  
  // Draw crosshair style - left and right segments only
  const crosshairLength = 30; // Length of each crosshair segment
  
  // Draw shadow first (flush left with main line, 1px offset down)
  // Left crosshair shadow
  ink("black").line(timelineStartX, nowLineY + 1, timelineStartX + crosshairLength, nowLineY + 1);
  // Right crosshair shadow
  ink("black").line(timelineEndX - crosshairLength, nowLineY + 1, timelineEndX, nowLineY + 1);
  
  // Draw main crosshair lines
  // Left crosshair
  ink(nowLineColor).line(timelineStartX, nowLineY, timelineStartX + crosshairLength, nowLineY);
  // Right crosshair
  ink(nowLineColor).line(timelineEndX - crosshairLength, nowLineY, timelineEndX, nowLineY);

  // ENABLED: Get ALL history items that would be visible from top to bottom of screen
  const extendedHistoryItems = getExtendedHistoryItems(
    currentTimeMs,
    futureTimeWindowMs,
    trackCount,
    screen.height,
    scaledPixelsPerSecond,
  );

  // ENABLED: Get future notes that should be precomputed and displayed up to the top of screen
  const futureNotes = getFutureNotes(currentTimeMs, screen.height, scaledPixelsPerSecond, melodyState, nowLineY);



  // ENABLED: Combine history, current, and future notes for complete timeline visualization
  const allTimelineItems = [...extendedHistoryItems, ...futureNotes];



  // Only show currently active notes - no history or future notes
  // const allTimelineItems = [];
  // Add only the currently playing note(s) if timing has started
  if (hasFirstSynthFired && melodyState) {
    // Unified logic for both single and parallel tracks
    if (melodyState.type === "single") {
      // Single track - add the currently playing note
      const currentNoteIndex = getCurrentNoteIndex(melodyState);
      if (
        currentNoteIndex >= 0 &&
        melodyState.notes &&
        melodyState.notes[currentNoteIndex]
      ) {
        const currentNote = melodyState.notes[currentNoteIndex];
        // Use the tracked timing variables if available, otherwise estimate
        let noteStartTime, noteEndTime;
        if (lastNoteStartTime && lastNoteDuration) {
          noteStartTime = lastNoteStartTime;
          noteEndTime = lastNoteStartTime + lastNoteDuration;
        } else if (currentNoteStartTime && currentNoteDuration) {
          noteStartTime = currentNoteStartTime;
          noteEndTime = currentNoteStartTime + currentNoteDuration;
        } else {
          // Fallback timing estimation
          noteStartTime = currentTimeMs;
          noteEndTime = currentTimeMs + baseTempo;
        }

        const activeNoteItem = {
          note: currentNote.note,
          octave: currentNote.octave,
          startTime: noteStartTime,
          endTime: noteEndTime,
          trackIndex: 0,
          waveType: currentNote.waveType || "sine",
          volume: currentNote.volume || 0.8,
          isMutation: currentNote.isMutation || false,
          struck: currentNote.struck || false,
        };
        allTimelineItems.push(activeNoteItem);
      }
    } else if (melodyState.type === "parallel" && melodyState.trackStates) {
      // Parallel tracks - add currently playing notes from each track
      // Use the same robust timing logic as single tracks
      melodyState.trackStates.forEach((trackState, trackIndex) => {
        if (trackState && trackState.track && trackState.track.length > 0) {
          // Get the current note index for this specific track using getCurrentNoteIndex
          const currentNoteIndex = getCurrentNoteIndex(melodyState, trackIndex);
          
          if (currentNoteIndex >= 0 && currentNoteIndex < trackState.track.length) {
            const currentNote = trackState.track[currentNoteIndex];
            
            if (currentNote) {
              // For parallel tracks, each track should use its own timing
              // Use track-specific timing first, then fall back to global timing
              let noteStartTime, noteEndTime;
              
              if (trackState.lastNoteStartTime && trackState.lastNoteDuration) {
                // Use track-specific timing - each track maintains its own timing
                noteStartTime = trackState.lastNoteStartTime;
                noteEndTime = trackState.lastNoteStartTime + trackState.lastNoteDuration;
              } else if (lastNoteStartTime && lastNoteDuration) {
                // Fallback to global timing if track-specific timing not available
                noteStartTime = lastNoteStartTime;
                noteEndTime = lastNoteStartTime + lastNoteDuration;
              } else if (currentNoteStartTime && currentNoteDuration) {
                // Fallback to legacy timing variables
                noteStartTime = currentNoteStartTime;
                noteEndTime = currentNoteStartTime + currentNoteDuration;
              } else {
                // Final fallback - estimate based on current time
                noteStartTime = currentTimeMs;
                noteEndTime = currentTimeMs + (currentNote.duration * melodyState.baseTempo);
              }

              const activeNoteItem = {
                note: currentNote.note,
                octave: currentNote.octave || octave,
                startTime: noteStartTime,
                endTime: noteEndTime,
                trackIndex: trackIndex,
                waveType: currentNote.waveType || "sine",
                volume: currentNote.volume || 0.8,
                isMutation: currentNote.isMutation || false,
                struck: currentNote.struck || false,
              };
              allTimelineItems.push(activeNoteItem);
            }
          }
        }
      });
    }
  }

  // Calculate the musical timing reference once for all notes
  // Use current time as the reference point for timeline visualization
  const musicalTimeReference = currentTimeMs;

  // Draw all the note bars from combined history and future notes
  // Calculate track positions to prevent overlapping
  const trackPositions = [];
  const trackWidths = [];
  
  // Distribute tracks evenly across the timeline width with no gaps or overlaps
  for (let i = 0; i < trackCount; i++) {
    const startPos = Math.round((timelineWidth * i) / trackCount);
    const endPos = Math.round((timelineWidth * (i + 1)) / trackCount);
    trackPositions.push(timelineStartX + startPos);
    trackWidths.push(endPos - startPos);
  }

  // Sort timeline items by startTime to ensure proper order and remove duplicates
  const sortedTimelineItems = allTimelineItems
    .filter((item, index, arr) => {
      // Remove duplicates based on note, startTime, endTime, and trackIndex
      return arr.findIndex(other => 
        other.note === item.note && 
        Math.abs(other.startTime - item.startTime) < 10 && 
        Math.abs(other.endTime - item.endTime) < 10 && 
        other.trackIndex === item.trackIndex
      ) === index;
    })
    .sort((a, b) => a.startTime - b.startTime);

  // Pre-calculate positions for seamless history note flow
  // History notes should connect end-to-start regardless of individual timing
  const historyNotes = sortedTimelineItems.filter(item => {
    // Use more stable criteria for history detection to prevent flickering
    // For parallel tracks, use track-specific timing; for single tracks, use global timing
    let isCurrentlyPlayingByTiming = false;
    if (melodyState && melodyState.type === "parallel" && melodyState.trackStates && item.trackIndex < melodyState.trackStates.length) {
      // For parallel tracks, use track-specific timing
      const trackState = melodyState.trackStates[item.trackIndex];
      if (trackState.lastNoteStartTime > 0) {
        isCurrentlyPlayingByTiming = Math.abs(item.startTime - trackState.lastNoteStartTime) < 50;
      }
    } else {
      // For single tracks, use global timing
      isCurrentlyPlayingByTiming = lastNoteStartTime > 0 && Math.abs(item.startTime - lastNoteStartTime) < 50;
    }
    
    const isCurrentlyPlaying = (musicalTimeReference >= item.startTime && musicalTimeReference <= item.endTime) ||
                              isCurrentlyPlayingByTiming;
    const isHistory = item.endTime < musicalTimeReference && !isCurrentlyPlaying;
    return isHistory;
  });

  // Calculate connected positions for history notes (most recent first) - STABLE positioning
  const historyPositions = new Map();
  
  // Track positions for seamless rendering of ALL notes (history, current, future)
  const allPositions = new Map();
  
  // Pre-calculate pixel-perfect positions for all notes in each track to eliminate gaps
  for (let trackIdx = 0; trackIdx < trackCount; trackIdx++) {
    // Get all notes for this track, sorted chronologically (future → current → history)
    const trackNotes = sortedTimelineItems
      .filter(item => item.trackIndex === trackIdx)
      .sort((a, b) => a.startTime - b.startTime);
    
    if (trackNotes.length === 0) continue;
    
    // Find the currently playing note for this track as the reference point
    const currentNoteIndex = trackNotes.findIndex(item => {
      const timingTolerance = 50;
      let isCurrentlyPlayingByTiming = false;
      
      if (melodyState && melodyState.type === "parallel" && melodyState.trackStates && trackIdx < melodyState.trackStates.length) {
        const trackState = melodyState.trackStates[trackIdx];
        if (trackState.lastNoteStartTime > 0) {
          isCurrentlyPlayingByTiming = Math.abs(item.startTime - trackState.lastNoteStartTime) < timingTolerance;
        }
      } else {
        isCurrentlyPlayingByTiming = lastNoteStartTime > 0 && Math.abs(item.startTime - lastNoteStartTime) < timingTolerance;
      }
      
      return (musicalTimeReference >= item.startTime && musicalTimeReference <= item.endTime) || isCurrentlyPlayingByTiming;
    });
    
    // Calculate position for current note (or use NOW line if no current note)
    let referenceY = nowLineY;
    if (currentNoteIndex >= 0) {
      const currentNote = trackNotes[currentNoteIndex];
      const noteDurationMs = currentNote.endTime - currentNote.startTime;
      const noteHeightPixels = Math.max(1, (noteDurationMs / 1000) * scaledPixelsPerSecond);
      const noteProgress = (musicalTimeReference - currentNote.startTime) / noteDurationMs;
      const noteProgressClamped = Math.max(0, Math.min(1, noteProgress));
      
      // Current note position (smooth movement)
      const currentNoteTopY = nowLineY - noteHeightPixels + noteProgressClamped * noteHeightPixels;
      referenceY = Math.round(currentNoteTopY + noteHeightPixels); // End of current note
      
      // Store current note position
      allPositions.set(currentNote, {
        barStartY: Math.round(currentNoteTopY),
        barEndY: Math.round(currentNoteTopY + noteHeightPixels)
      });
    }
    
    // Calculate future notes (above current/NOW line) - working upward
    if (currentNoteIndex >= 0) {
      let currentStartY = allPositions.get(trackNotes[currentNoteIndex]).barStartY;
      for (let i = currentNoteIndex + 1; i < trackNotes.length; i++) {
        const note = trackNotes[i];
        const noteDurationMs = note.endTime - note.startTime;
        const noteHeightPixels = Math.max(1, (noteDurationMs / 1000) * scaledPixelsPerSecond);
        
        const barHeight = Math.max(1, Math.round(noteHeightPixels));
        const barEndY = Math.round(currentStartY);
        const barStartY = barEndY - barHeight;
        
        allPositions.set(note, { barStartY, barEndY });
        currentStartY = barStartY;
      }
    } else {
      // No current note, start future notes from NOW line going upward
      let currentStartY = nowLineY;
      for (let i = 0; i < trackNotes.length; i++) {
        const note = trackNotes[i];
        const noteDurationMs = note.endTime - note.startTime;
        const noteHeightPixels = Math.max(1, (noteDurationMs / 1000) * scaledPixelsPerSecond);
        
        const barHeight = Math.max(1, Math.round(noteHeightPixels));
        const barEndY = Math.round(currentStartY);
        const barStartY = barEndY - barHeight;
        
        allPositions.set(note, { barStartY, barEndY });
        currentStartY = barStartY;
      }
    }
    
    // Calculate history notes (below current/NOW line) - working downward
    if (currentNoteIndex >= 0) {
      let currentEndY = allPositions.get(trackNotes[currentNoteIndex]).barEndY;
      for (let i = currentNoteIndex - 1; i >= 0; i--) {
        const note = trackNotes[i];
        const noteDurationMs = note.endTime - note.startTime;
        const noteHeightPixels = Math.max(1, (noteDurationMs / 1000) * scaledPixelsPerSecond);
        
        const barStartY = Math.round(currentEndY);
        const barHeight = Math.max(1, Math.round(noteHeightPixels));
        const barEndY = barStartY + barHeight;
        
        allPositions.set(note, { barStartY, barEndY });
        currentEndY = barEndY;
      }
    }
  }
  
  // Legacy history positioning system (keeping for compatibility)
  if (historyNotes.length > 0) {
    // For parallel tracks, we need to calculate positions per track
    // Group history notes by track index
    const historyByTrack = new Map();
    
    for (let trackIdx = 0; trackIdx < trackCount; trackIdx++) {
      const trackHistoryNotes = historyNotes.filter(item => item.trackIndex === trackIdx);
      if (trackHistoryNotes.length > 0) {
        historyByTrack.set(trackIdx, trackHistoryNotes.sort((a, b) => b.endTime - a.endTime));
      }
    }
    
    // Calculate positions for each track independently
    historyByTrack.forEach((trackHistoryNotes, trackIdx) => {
      // Start from the bottom of the active note for this track, or NOW line if no active note
      let currentEndY = nowLineY;
      
      // Find the active note for this specific track using stable timing detection
      const activeNoteForTrack = sortedTimelineItems.find(item => {
        if (item.trackIndex !== trackIdx) return false;
        // Use consistent timing detection to prevent flickering - track-specific for parallel tracks
        const timingTolerance = 50; // Consistent with main detection logic
        
        let isCurrentlyPlayingByTiming = false;
        if (melodyState && melodyState.type === "parallel" && melodyState.trackStates && trackIdx < melodyState.trackStates.length) {
          // For parallel tracks, use track-specific timing
          const trackState = melodyState.trackStates[trackIdx];
          if (trackState.lastNoteStartTime > 0) {
            isCurrentlyPlayingByTiming = Math.abs(item.startTime - trackState.lastNoteStartTime) < timingTolerance;
          }
        } else {
          // For single tracks, use global timing
          isCurrentlyPlayingByTiming = lastNoteStartTime > 0 && Math.abs(item.startTime - lastNoteStartTime) < timingTolerance;
        }
        
        const isCurrentlyPlaying = (musicalTimeReference >= item.startTime && musicalTimeReference <= item.endTime) ||
                                   isCurrentlyPlayingByTiming;
        return isCurrentlyPlaying;
      });
      
      if (activeNoteForTrack) {
        // Calculate where the active note ends for this track using its actual timing
        const activeNoteDurationMs = activeNoteForTrack.endTime - activeNoteForTrack.startTime;
        const activeNoteHeightPixels = Math.max(1, Math.round((activeNoteDurationMs / 1000) * scaledPixelsPerSecond));
        // Use the note's actual timing for progress calculation with smooth positioning
        const noteProgress = (musicalTimeReference - activeNoteForTrack.startTime) / activeNoteDurationMs;
        const noteProgressClamped = Math.max(0, Math.min(1, noteProgress));
        // For smooth movement, keep as floating point until final render
        const adjustedNoteTopY = nowLineY - activeNoteHeightPixels + noteProgressClamped * activeNoteHeightPixels;
        const activeNoteStartY = adjustedNoteTopY; // Keep as float for smooth positioning
        const activeNoteEndY = activeNoteStartY + activeNoteHeightPixels;
        currentEndY = activeNoteEndY; // Start history notes from the bottom of the active note
      }
      
      // Position history notes for this track
      for (const historyNote of trackHistoryNotes) {
        const noteDurationMs = historyNote.endTime - historyNote.startTime;
        const noteHeightPixels = Math.max(1, (noteDurationMs / 1000) * scaledPixelsPerSecond); // Keep as float
        
        // Position this note so it starts at currentEndY (seamless connection)
        // Round the start position to ensure pixel-perfect alignment
        const barStartY = Math.round(currentEndY);
        const barHeightRounded = Math.max(1, Math.round(noteHeightPixels));
        const barEndY = barStartY + barHeightRounded;
        
        historyPositions.set(historyNote, { barStartY, barEndY });
        
        // Next note (older) should start exactly where this note ends (use rounded end position)
        currentEndY = barEndY;
      }
    });
  }

  sortedTimelineItems.forEach((historyItem) => {
    const {
      note,
      octave: noteOctave,
      startTime,
      endTime,
      trackIndex,
      waveType,
      volume,
      isMutation,
      struck,
    } = historyItem;

    // Skip if note is out of bounds for track count
    if (trackIndex >= trackCount) return;

    // Calculate track horizontal position using pre-calculated positions
    const trackX = trackPositions[trackIndex];
    const trackWidth = trackWidths[trackIndex];
    const trackCenterX = Math.round(trackX + trackWidth / 2);

    // Convert time to VERTICAL screen position using the draggable NOW line
    // Future notes (startTime > musicalTimeReference) appear above the NOW line (smaller Y values)
    // Current notes (startTime <= musicalTimeReference <= endTime) cross the NOW line
    // History notes (endTime < musicalTimeReference) appear below the NOW line (larger Y values)
    const timeDifferenceStart = (startTime - musicalTimeReference) / 1000; // Convert to seconds
    const timeDifferenceEnd = (endTime - musicalTimeReference) / 1000;

  // Calculate consistent note height based on duration FIRST
  const noteDurationMs = endTime - startTime;
  const noteDurationSeconds = noteDurationMs / 1000;
  // Keep as floating point for smooth scaling with varying durations
  const noteHeightPixels = Math.max(1, (noteDurationSeconds * scaledPixelsPerSecond));
  
  // DEBUG: Log note duration calculations for varying durations - THROTTLED with sticky duration tracking
  if (note !== "rest" && trackIndex === 0) {
    const now = performance.now();
    if (now - lastLogTime > LOG_THROTTLE_MS || lastLoggedNoteCount !== allTimelineItems.length) {
      // Calculate what the expected duration should be based on the original melody string
      let expectedDurationInfo = "";
      if (originalMelodyString && melodyState && melodyState.notes) {
        const noteIndex = melodyState.notes.findIndex(n => 
          Math.abs(n.startTime - startTime) < 50 && n.note === note
        );
        if (noteIndex >= 0 && noteIndex < melodyState.notes.length) {
          const melodyNote = melodyState.notes[noteIndex];
          expectedDurationInfo = `, expectedDur=${melodyNote.duration}x${melodyState.baseTempo}=${melodyNote.duration * melodyState.baseTempo}ms`;
        }
      }
      

      lastLogTime = now;
      lastLoggedNoteCount = allTimelineItems.length;
    }
  }
    
    // Determine colors and positioning
    let barColor, textColor, hasBackgroundBlink = false;
    let isCurrentlyPlaying, isHistory, isFuture;
    let barStartY, barEndY;

    // Determine note type based on timing - FIXED: For parallel tracks, use note-specific timing
    // Check if this specific note is currently playing (more robust for parallel tracks)
    const noteStartTime = startTime;
    const noteEndTime = endTime;
    const noteActualDurationMs = noteEndTime - noteStartTime;
    
    // For notes with duration modifiers, use note-specific timing for smooth animation
    // This prevents jank when notes have different durations (like c.e..f.g..e.b...)
    const timingTolerance = 25; // Tighter tolerance for more precise timing with duration modifiers
    
    // Primary timing detection uses the note's actual start/end times
    const isWithinNoteTiming = (musicalTimeReference >= noteStartTime && musicalTimeReference <= noteEndTime);
    
    // Secondary detection for timing synchronization - use track-specific timing for parallel tracks
    let isNearTrackTiming = false;
    if (melodyState && melodyState.type === "parallel" && melodyState.trackStates && trackIndex < melodyState.trackStates.length) {
      // For parallel tracks, use track-specific timing
      const trackState = melodyState.trackStates[trackIndex];
      if (trackState.lastNoteStartTime > 0) {
        isNearTrackTiming = Math.abs(startTime - trackState.lastNoteStartTime) < timingTolerance;
      }
    } else {
      // For single tracks, use global timing
      isNearTrackTiming = lastNoteStartTime > 0 && Math.abs(startTime - lastNoteStartTime) < timingTolerance;
    }
    
  isCurrentlyPlaying = isWithinNoteTiming || (isNearTrackTiming && !isWithinNoteTiming);
  
  // More precise history/future detection to prevent state flickering
  isHistory = endTime < musicalTimeReference && !isCurrentlyPlaying;
  isFuture = startTime > musicalTimeReference && !isCurrentlyPlaying;

  // DEBUG: Log timing state for debugging skipping - THROTTLED with sequence tracking
  if (note !== "rest" && trackIndex === 0) {
    const now = performance.now();
    if (now - lastLogTime > LOG_THROTTLE_MS) {
      const timeDiff = musicalTimeReference - noteStartTime;
      
      // Calculate the time gap from the previous note for sequence analysis
      let gapInfo = "";
      if (sortedTimelineItems.length > 1) {
        const currentIndex = sortedTimelineItems.findIndex(item => 
          item.startTime === startTime && item.note === note && item.trackIndex === trackIndex
        );
        if (currentIndex > 0) {
          const previousNote = sortedTimelineItems[currentIndex - 1];
          const gapMs = startTime - previousNote.endTime;
          gapInfo = `, gap=${gapMs.toFixed(1)}ms from ${previousNote.note}`;
        }
      }
      
      console.log(`⏰ TIMING ${note}: within=${isWithinNoteTiming}, near=${isNearTrackTiming}, playing=${isCurrentlyPlaying}, history=${isHistory}, future=${isFuture}, timeDiff=${timeDiff.toFixed(1)}ms${gapInfo}`);
    }
  }

    // Use pre-calculated pixel-perfect positions for ALL notes to eliminate gaps
    const preCalculatedPosition = allPositions.get(historyItem);
    if (preCalculatedPosition) {
      barStartY = preCalculatedPosition.barStartY;
      barEndY = preCalculatedPosition.barEndY;
    } else if (isCurrentlyPlaying) {
      // Currently playing notes flow through the NOW line during playback
      // Use the actual note's timing for smooth animation with varying durations
      const actualNoteEndTime = noteEndTime;
      const actualNoteDuration = actualNoteEndTime - noteStartTime;
      const noteProgress = (musicalTimeReference - noteStartTime) / actualNoteDuration;
      const noteProgressClamped = Math.max(0, Math.min(1, noteProgress));
      
      // For smooth pixel-by-pixel movement, use floating point positioning
      // The note should start above the NOW line and move down through it
      const noteTopYWhenStarting = nowLineY - noteHeightPixels;
      const noteBottomYWhenEnding = nowLineY;
      
      // Smooth interpolation based on actual note progress
      const currentNoteTopY = noteTopYWhenStarting + (noteProgressClamped * noteHeightPixels);
      
      // DEBUG: Log current note movement for debugging - THROTTLED
      if (note !== "rest" && trackIndex === 0) {
        const now = performance.now();
        if (now - lastLogTime > LOG_THROTTLE_MS / 4) { // More frequent for current notes
          console.log(`🎵 CURRENT NOTE ${note}: progress=${noteProgress.toFixed(4)}, topY=${currentNoteTopY.toFixed(2)}, height=${noteHeightPixels.toFixed(2)}, duration=${actualNoteDuration}ms`);
        }
      }
      
      // Use floating point for smooth movement, only round for final rendering
      barStartY = currentNoteTopY;
      barEndY = barStartY + noteHeightPixels;

    } else if (isFuture) {
      // Future notes flow upward toward cyan line - use precise floating point positioning
      const timeToStart = (startTime - musicalTimeReference) / 1000;
      const pixelOffset = timeToStart * scaledPixelsPerSecond; // Keep as float for smooth movement
      barStartY = nowLineY - pixelOffset - noteHeightPixels;
      barEndY = barStartY + noteHeightPixels;

      // DEBUG: Log future note positioning - MINIMAL
      if (note !== "rest" && trackIndex === 0 && allTimelineItems.length < 3) {
        console.log(`🔮 FUTURE NOTE ${note}: timeToStart=${timeToStart.toFixed(3)}s, pixelOffset=${pixelOffset.toFixed(2)}, startY=${barStartY.toFixed(2)}, height=${noteHeightPixels.toFixed(2)}`);
      }

    } else if (isHistory) {
      // Use legacy history positioning as fallback
      const preCalculatedPosition = historyPositions.get(historyItem);
      if (preCalculatedPosition) {
        barStartY = preCalculatedPosition.barStartY;
        barEndY = preCalculatedPosition.barEndY;
      } else {
        // Fallback to original logic if position not found
        const timeWhenEnded = (musicalTimeReference - endTime) / 1000;
        const pixelOffset = timeWhenEnded * scaledPixelsPerSecond; // Keep as float for smooth movement
        barEndY = nowLineY + pixelOffset;
        barStartY = barEndY - noteHeightPixels;
      }
    }

    // For smooth transitions, calculate how much the note has moved across the NOW line
    const crossingProgress = isCurrentlyPlaying
      ? Math.min(1, (musicalTimeReference - startTime) / (endTime - startTime))
      : isFuture
        ? 0
        : 1;

    if (note === "rest") {
      // Rests - gray with different shades for time periods
      if (isHistory) {
        barColor = [60, 60, 60]; // Darker gray for history rests
      } else if (isFuture) {
        barColor = [140, 140, 140]; // Lighter gray for future rests
      } else {
        barColor = [102, 102, 102]; // Medium gray for current rests
      }
      textColor = "white";
    } else {
      // Use rainbow colors for notes with smooth transitions
      const baseNoteColor = getNoteColor(note);

      if (isHistory) {
        // Past/history notes - smoothly fade to much darker as they get older
        const ageMs = musicalTimeReference - endTime;
        const fadeAmount = Math.min(1, ageMs / 2000); // Fade over 2 seconds
        const darkFactor = 0.15 + 0.25 * (1 - fadeAmount); // From 0.4 to 0.15 (much darker)
        barColor = baseNoteColor.map((c) => Math.round(c * darkFactor));
        textColor = [200, 200, 200]; // Light gray text for better readability on dark history notes
      } else if (isCurrentlyPlaying) {
        // Currently playing note - bright with strong pulsing background blink
        const brightColor = getBrighterColor(baseNoteColor);

        // Add stronger pulsing background blink effect for active notes
        const pulseSpeed = 8; // Pulse speed
        const pulsePhase =
          (performance.now() / 1000) * pulseSpeed * 2 * Math.PI;
        const pulseIntensity = (Math.sin(pulsePhase) + 1) / 2; // 0 to 1

        // Blend between bright color and super-bright for strong pulsing effect
        const superBrightColor = brightColor.map((c) =>
          Math.min(255, Math.round(c * 1.3)),
        ); // 30% brighter
        barColor = brightColor.map((c, i) =>
          Math.round(c + (superBrightColor[i] - c) * pulseIntensity),
        );

        hasBackgroundBlink = true;
        textColor = "white";
      } else if (isFuture) {
        // Future notes - normal color with slight approach brightening
        const approachDistance = startTime - currentTimeMs;
        const approachBrightening = Math.max(0, 1 - approachDistance / 5000); // Brighten as we approach
        const brightness = 0.85 + 0.15 * approachBrightening;
        barColor = baseNoteColor.map((c) => Math.round(c * brightness));
        textColor = "black";
      } else {
        // Fallback - normal note color
        barColor = baseNoteColor;
        textColor = "black";
      }
    }

    // Only render notes that are visible - always draw boxes even if 1px high
    // Note labels are skipped for very small notes to avoid visual clutter
    if (barEndY >= -noteHeightPixels && barStartY <= screen.height + noteHeightPixels) {
      // Draw note bar (skip rests visual for now, focus on notes)
      if (note !== "rest") {
        const barWidth = trackWidth; // Use exact track width to prevent overlaps
        const barX = trackX; // Already calculated above

        // No spacing - notes should flow seamlessly together with consistent positioning
        let actualBarStartY = barStartY;
        let actualBarHeight = noteHeightPixels;

        // Allow notes to draw naturally past screen edges for smooth scrolling
        let renderStartY = actualBarStartY;
        let renderHeight = actualBarHeight;

        // Only round coordinates at the final rendering step for smooth sub-pixel movement
        // Use pixel-perfect positioning to eliminate gaps between consecutive notes
        renderHeight = Math.max(1, Math.round(renderHeight));
        renderStartY = Math.round(renderStartY);

        // DEBUG: Log rendering coordinates for debugging skipping issues - THROTTLED
        if (note !== "rest" && trackIndex === 0 && isCurrentlyPlaying) {
          const now = performance.now();
          if (now - lastLogTime > LOG_THROTTLE_MS / 4) {

          }
        }

        // Draw notes even if they extend past screen edges - let them scroll off naturally
        if (renderHeight >= 1) {
          // Ensure barColor is properly formatted for ink()
          let inkColor;
          if (Array.isArray(barColor)) {
            // Convert RGB array to ink-compatible format - use the RGB array directly
            inkColor = barColor;
          } else {
            inkColor = barColor;
          }

          // Draw the main note bar - use RGB array directly for ink() with integer coordinates
          // Notes are drawn with no borders and designed to overlap seamlessly
          ink(inkColor).box(barX, renderStartY, barWidth, renderHeight);

          // Unified note label rendering logic for all note types (history, active, future)
          // Show labels when note height >= 15px and track width >= 15px for consistent readability
          if (noteHeightPixels >= 15 && barWidth >= 15) {
            // For all notes (including active notes), center the label within the track
            // Use floating point position for smooth label movement, round at final step
            const labelY = Math.round(actualBarStartY + actualBarHeight / 2 - 4);
            // Use trackCenterX to ensure proper centering for parallel tracks
            const labelX = Math.round(trackCenterX - 3);

            // Keep label visible as long as any part of the note is on screen
            // Only hide when the label itself would be completely off-screen
            if (labelY >= -4 && labelY <= screen.height + 4) {
              // Check if label overlaps with clock display area and reduce opacity if so
              let labelColor = textColor;
              
              // Calculate clock display area bounds (replicate clock sizing logic)
              const minVerticalSpace = 25;
              const shadowOffset = 1;
              const availableVerticalSpace = nowLineY - shadowOffset;
              const timeDisplayFontSize = (availableVerticalSpace >= minVerticalSpace && screen.width >= 250) ? 2 : 1;
              
              const clockTextHeight = 8 * timeDisplayFontSize;
              const clockCenterY = nowLineY - (clockTextHeight / 2);
              
              // Scale padding based on font size - larger fonts need more clearance
              const basePadding = timeDisplayFontSize === 2 ? 12 : 6; // Double padding for size 2
              const clockTopY = clockCenterY - basePadding;
              const clockBottomY = clockCenterY + clockTextHeight + basePadding;
              
              // Estimate clock width with scaled padding
              const timeStringLength = 15; // Approximate length of "12:34:56:789 AM"
              const clockWidth = timeStringLength * 6 * timeDisplayFontSize;
              const horizontalPadding = timeDisplayFontSize === 2 ? 20 : 10; // Double horizontal padding for size 2
              const clockLeftX = (screen.width - clockWidth) / 2 - horizontalPadding;
              const clockRightX = clockLeftX + clockWidth + (horizontalPadding * 2);
              
              // Check if label overlaps with clock area
              const labelOverlapsClockVertically = labelY >= clockTopY && labelY <= clockBottomY;
              const labelOverlapsClockHorizontally = labelX >= clockLeftX && labelX <= clockRightX;
              
              if (labelOverlapsClockVertically && labelOverlapsClockHorizontally) {
                // Reduce opacity when overlapping with clock
                if (Array.isArray(labelColor)) {
                  // For RGB array colors, reduce alpha
                  labelColor = [...labelColor, 64]; // 25% opacity
                } else {
                  // For named colors, use a dimmed version or convert to RGB with alpha
                  if (labelColor === "white") {
                    labelColor = [255, 255, 255, 64];
                  } else if (labelColor === "red") {
                    labelColor = [255, 0, 0, 64];
                  } else if (labelColor === "yellow") {
                    labelColor = [255, 255, 0, 64];
                  } else if (labelColor === "green") {
                    labelColor = [0, 255, 0, 64];
                  } else if (labelColor === "blue") {
                    labelColor = [0, 0, 255, 64];
                  } else if (labelColor === "cyan") {
                    labelColor = [0, 255, 255, 64];
                  } else if (labelColor === "magenta") {
                    labelColor = [255, 0, 255, 64];
                  } else {
                    // Default to semi-transparent white for unknown colors
                    labelColor = [255, 255, 255, 64];
                  }
                }
              }
              
              ink(labelColor).write(note.toUpperCase(), {
                x: labelX,
                y: labelY,
                scale: 0.8,
              });
            }
          }


        }
      }
    } else {
      // Note completely off-screen - don't render
    }
  });

  // 📊 VISUAL GEOMETRY LOG - REMOVED for cleaner console output
}

// Draw simplified timeline background for vertical layout
function drawTimelineBackground(
  ink,
  write,
  screen,
  currentTimeMs,
  pixelsPerSecond,
  timeWindowSeconds,
  startX,
  endX,
  centerY,
) {
  // Draw minimal clean background - no division lines, measure markers, or background color
  // Completely transparent background - no visual elements drawn
}

// 📚 Library

// Add a note to the history buffer
function addNoteToHistory(
  note,
  octave,
  startTime,
  duration,
  trackIndex = 0,
  waveType = "sine",
  volume = 0.8,
  isMutation = false,
  struck = false,
) {
  const historyItem = {
    note: note,
    octave: octave,
    startTime: startTime,
    endTime: startTime + duration,
    trackIndex: trackIndex,
    waveType: waveType,
    volume: volume,
    isMutation: isMutation,
    struck: struck,
  };

  historyBuffer.push(historyItem);

  // Keep history buffer size manageable
  if (historyBuffer.length > MAX_HISTORY_ITEMS) {
    historyBuffer.shift(); // Remove oldest item
  }
}

// Get history items that should be visible on the timeline
function getVisibleHistoryItems(currentTimeMs, timeWindowMs, trackCount = 1) {
  const startTime = currentTimeMs - timeWindowMs;
  const endTime = currentTimeMs + timeWindowMs;

  return historyBuffer
    .filter((item) => {
      // Include items that overlap with the visible time window
      return item.endTime >= startTime && item.startTime <= endTime;
    })
    .sort((a, b) => a.startTime - b.startTime); // Sort by start time
}

// Get extended history items that fill the entire screen from top to bottom
function getExtendedHistoryItems(
  currentTimeMs,
  timeWindowMs,
  trackCount = 1,
  screenHeight,
  pixelsPerSecond,
) {
  // Calculate how far back in time we need to go to fill the screen from top to NOW line
  const timeToTopOfScreen = (screenHeight / pixelsPerSecond) * 1000; // Convert to milliseconds
  const extendedStartTime = currentTimeMs - timeToTopOfScreen;
  const endTime = currentTimeMs + timeWindowMs;

  const filteredItems = historyBuffer
    .filter((item) => {
      return (
        item.endTime >= extendedStartTime &&
        item.startTime <= endTime &&
        item.trackIndex < trackCount
      );
    })
    .sort((a, b) => a.startTime - b.startTime); // Sort by start time
    
  return filteredItems;
}

// Get future notes that should be visible extending up from NOW line to top of screen
function getFutureNotes(
  currentTimeMs,
  screenHeight,
  pixelsPerSecond,
  melodyState,
  nowLineY,
) {
  const futureNotes = [];

  if (!melodyState || !hasMelodyContent(melodyState)) {
    return futureNotes;
  }

  // Calculate how much future time we need to fill from NOW line to top of screen
  const timeToTopOfScreen = (nowLineY / pixelsPerSecond) * 1000; // Convert to milliseconds
  const futureEndTime = currentTimeMs + timeToTopOfScreen;

  if (melodyState.type === "single") {
    // Single track - predict future notes starting from where the current note ends
    // This ensures seamless visual connection between active note and future notes
    let predictTime;
    let noteIndex;
    
    if (lastNoteStartTime > 0 && lastNoteDuration > 0) {
      // Start future notes exactly where current note ends for seamless flow
      predictTime = lastNoteStartTime + lastNoteDuration;
      
      // FIXED: Find the ACTUAL currently playing note by matching with lastNoteStartTime
      // This ensures we use the correct note index regardless of melodyState.index sync issues
      let actualCurrentNoteIndex = melodyState.index;
      
      // Try to find the actual current note by looking for the note that matches our timing
      // This helps when melodyState.index gets out of sync with actual playback
      for (let i = 0; i < melodyState.notes.length; i++) {
        const testNoteStart = lastNoteStartTime;
        const testNoteEnd = lastNoteStartTime + lastNoteDuration;
        // Use a simple approach - just use melodyState.index but validate it
        // The real issue is that melodyState.index advances before lastNoteStartTime updates
        // So melodyState.index is likely pointing to the NEXT note, not the current one
      }
      
      // CORRECTED: If melodyState.index is ahead, use the previous note index
      // When a note starts playing, melodyState.index advances but lastNoteStartTime reflects the current note
      const possibleCurrentIndex = (melodyState.index - 1 + melodyState.notes.length) % melodyState.notes.length;
      const currentNote = melodyState.notes[possibleCurrentIndex];
      const melodyStateNote = melodyState.notes[melodyState.index];
      
      // Check if melodyState.index is pointing to the next note (most likely scenario)
      // If melodyState shows 'g' but we're playing 'e', then melodyState.index is 1 ahead
      actualCurrentNoteIndex = possibleCurrentIndex;
      noteIndex = melodyState.index; // Next note is what melodyState.index is pointing to
      
      // DEBUG: Enhanced debugging to show the synchronization issue
      const nextNote = melodyState.notes[noteIndex];
    } else {
      // Fallback to nextNoteTargetTime if no current note info
      predictTime = nextNoteTargetTime || currentTimeMs;
      noteIndex = melodyState.index;
    }

    while (predictTime < futureEndTime && futureNotes.length < 100) {
      // Limit to prevent infinite loops
      const noteData = melodyState.notes[noteIndex];
      if (!noteData) break;

      const noteDuration = noteData.duration * melodyState.baseTempo;

      futureNotes.push({
        note: noteData.note,
        octave: noteData.octave || octave,
        startTime: predictTime,
        endTime: predictTime + noteDuration,
        trackIndex: 0,
        waveType: noteData.waveType || "sine",
        volume: noteData.volume || 0.8,
        isMutation: false,
        struck: noteData.struck || false,
      });

      predictTime += noteDuration;
      noteIndex = (noteIndex + 1) % melodyState.notes.length;
    }
  } else if (melodyState.type === "parallel" && melodyState.trackStates) {
    // Parallel tracks - predict future notes for each track using precise timing
    melodyState.trackStates.forEach((trackState, trackIndex) => {
      if (!trackState.track || trackState.track.length === 0) return;

      // Start future notes exactly where current note ends for seamless visual flow
      let predictTime;
      let noteIndex;
      
      if (trackState.nextNoteTargetTime > 0) {
        // For parallel tracks, start future notes from the nextNoteTargetTime
        // This is when the next note will actually play
        predictTime = trackState.nextNoteTargetTime;
        noteIndex = trackState.noteIndex; // This is the next note that will play
      } else {
        // Fallback if no timing established yet
        predictTime = currentTimeMs;
        noteIndex = trackState.noteIndex || 0;
      }

      while (predictTime < futureEndTime && futureNotes.length < 500) {
        // Higher limit for multiple tracks
        const noteData = trackState.track[noteIndex];
        if (!noteData) break;

        const noteDuration = noteData.duration * melodyState.baseTempo;

        futureNotes.push({
          note: noteData.note,
          octave: noteData.octave || octave,
          startTime: predictTime,
          endTime: predictTime + noteDuration,
          trackIndex: trackIndex,
          waveType: noteData.waveType || "sine",
          volume: noteData.volume || 0.8,
          isMutation: false,
          struck: noteData.struck || false,
        });

        predictTime += noteDuration;
        noteIndex = (noteIndex + 1) % trackState.track.length;
      }
    });
  }

  return futureNotes.sort((a, b) => a.startTime - b.startTime);
}

// Clear history buffer (useful for reset)
function clearHistory() {
  historyBuffer.length = 0;
}

// 📚 Library

// Helper function to check if melody state has content
function hasMelodyContent(melodyState) {
  if (!melodyState) return false;

  if (melodyState.type === "single") {
    return melodyState.notes && melodyState.notes.length > 0;
  } else if (melodyState.type === "parallel") {
    return melodyState.trackStates && melodyState.trackStates.length > 0;
  } else if (melodyState.type === "multi") {
    return melodyState.tracks && melodyState.tracks.length > 0;
  }

  return false;
}

// Create a managed synth sound with proper lifecycle control (similar to notepat)
function createManagedSound(
  sound,
  tone,
  waveType,
  duration,
  volume = 0.8,
  isFallback = false,
  struck = false, // New parameter for struck notes
  isLastNoteInSequence = false, // New parameter to eliminate gap for last note before loop
) {
  // Prevent new sounds from being created after leave() is called
  if (isLeavingPiece) {
    console.log(`🔇 Prevented sound creation after leave(): ${tone}`);
    return null;
  }

  // Add a proportional gap that scales inversely with duration - MINIMAL gaps for tight timing
  // EXCEPTION: No gap for the last note in a sequence to prevent loop timing issues
  let gapRatio = 0;

  if (!isLastNoteInSequence) {
    if (duration <= 100) {
      // Very short notes (like c....) need tiny gaps to maintain rhythm - 5%
      gapRatio = 0.05;
    } else if (duration <= 200) {
      // Short notes (like c...) need almost no gaps - 3%
      gapRatio = 0.03;
    } else if (duration <= 400) {
      // Medium-short notes (like c.) need minimal gaps - 2%
      gapRatio = 0.02;
    } else if (duration <= 800) {
      // Regular notes need very small gaps - 1%
      gapRatio = 0.01;
    } else {
      // Long notes need almost no gaps - 0.5%
      gapRatio = 0.005;
    }
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
    fadeTime = 0.01;
  } else if (actualDuration <= 600) {
    // Medium notes need moderate fade - 20ms
    fadeTime = 0.02;
  } else {
    // Long notes can have normal fade - 50ms
    fadeTime = 0.05;
  }

  // For struck notes, use finite duration with longer fade; for held notes, use infinite duration
  let synthDuration, synthFadeTime;

  if (struck) {
    // Struck notes: use full duration with decay that fades over the entire note length
    synthDuration = duration / 1000; // Convert ms to seconds for full duration
    synthFadeTime = Math.max(0.001, (duration * 0.01) / 1000); // Very fast fade, converted to seconds
  } else {
    // Held notes: use infinite duration with manual lifecycle management
    synthDuration = "🔁";
    synthFadeTime = fadeTime;
  }

  const synthInstance = sound.synth({
    type: waveType || "sine",
    tone: tone,
    duration: synthDuration,
    attack: struck ? 0.0 : 0.01, // Very short attack for struck notes
    decay: struck ? 1 : 0.1, // Fast decay for struck notes - will fade over the full duration
    volume: volume,
  });

  // Set flag to indicate first synth has fired (for syntax coloring)
  hasFirstSynthFired = true;

  // Calculate when this note should be released (using the shorter duration)
  const releaseTime = performance.now() + actualDuration;

  // Store the sound for lifecycle management (use performance.now() + random for unique IDs)
  const soundId = `${struck ? "struck_" : ""}${tone}_${performance.now()}_${Math.random().toString(36).substr(2, 9)}`;

  // For struck notes, we still need to track them for timing synchronization
  if (struck) {
    // Struck notes: track for timing but don't schedule manual release (they self-terminate)
    activeSounds[soundId] = {
      synth: synthInstance,
      tone: tone,
      waveType: waveType,
      startTime: performance.now(),
      releaseTime: releaseTime, // When the note should naturally end
      fadeTime: synthFadeTime,
      isFallback: isFallback,
      isStruck: true, // Flag to identify struck notes
    };

    // Don't schedule manual release for struck notes - they manage their own lifecycle
    // But we still return the ID for tracking
    return soundId;
  }

  // For held notes, continue with normal lifecycle management
  activeSounds[soundId] = {
    synth: synthInstance,
    tone: tone,
    waveType: waveType,
    startTime: performance.now(),
    releaseTime: releaseTime,
    fadeTime: fadeTime, // Store the calculated fade time
    isFallback: isFallback,
    isStruck: false, // Flag to identify held notes
  };

  // Schedule the note to be released
  scheduledNotes.push({
    soundId: soundId,
    releaseTime: releaseTime,
    fadeTime: fadeTime, // Store fade time with the scheduled release
  });

  // Sort scheduled notes by release time for efficient processing
  scheduledNotes.sort((a, b) => a.releaseTime - b.releaseTime);

  // Log with appropriate styling
  if (isFallback) {
  } else {
  }

  return soundId;
}

// Process scheduled note releases
function processScheduledReleases() {
  // Skip processing if we're leaving the piece
  if (isLeavingPiece) {
    return;
  }

  const now = performance.now();
  const overdueThreshold = 5000; // 5 seconds - notes this overdue should be force-killed
  // Release notes that have reached their scheduled release time
  while (scheduledNotes.length > 0 && scheduledNotes[0].releaseTime <= now) {
    const noteToRelease = scheduledNotes.shift();
    const sound = activeSounds[noteToRelease.soundId];

    if (sound && sound.synth) {
      // Skip manual release for struck notes - they manage their own lifecycle
      if (sound.isStruck) {
        // Just remove from tracking, don't manually kill
        delete activeSounds[noteToRelease.soundId];
        continue;
      }

      // Use the fade time that was calculated for this specific note
      const fadeTime = noteToRelease.fadeTime || sound.fadeTime || 0.05; // Fallback to 50ms if not set

      // Check if note is extremely overdue (likely from standby)
      const overdueBy = now - noteToRelease.releaseTime;
      if (overdueBy > overdueThreshold) {
        sound.synth.kill(0.001); // Very fast kill for overdue notes
      } else {
        sound.synth.kill(fadeTime);
      }
      delete activeSounds[noteToRelease.soundId];
    }
  }
  // Also check for any orphaned active sounds that don't have scheduled releases
  const activeIds = Object.keys(activeSounds);
  const scheduledIds = new Set(scheduledNotes.map((note) => note.soundId));

  activeIds.forEach((soundId) => {
    if (!scheduledIds.has(soundId)) {
      const sound = activeSounds[soundId];
      if (sound) {
        const age = now - sound.startTime;

        // For struck notes, clean up based on their expected duration
        if (sound.isStruck) {
          const expectedDuration = sound.releaseTime - sound.startTime;
          // Clean up struck notes that have exceeded their expected duration by a significant margin
          if (age > expectedDuration + 1000) {
            // 1 second grace period
            delete activeSounds[soundId];
          }
        } else {
          // For held notes, if a sound has been active for more than 10 seconds without a release schedule, kill it
          if (age > 10000) {
            if (sound.synth) sound.synth.kill(0.1);
            delete activeSounds[soundId];
          }
        }
      }
    }
  });
}

// Clear all active sounds (useful for cleanup or emergency stop)
function clearAllSounds() {
  const fadeTime = 0.02; // Quick fade for cleanup

  // Kill all active sounds
  Object.keys(activeSounds).forEach((soundId) => {
    const sound = activeSounds[soundId];
    if (sound && sound.synth) {
      try {
        sound.synth.kill(fadeTime);
      } catch (error) {
        console.warn(`Failed to kill sound ${soundId}:`, error);
        // Try alternative cleanup methods
        try {
          sound.synth.kill(0);
        } catch (secondError) {
          console.warn(`Failed to force-kill sound ${soundId}:`, secondError);
        }
      }
    }
  });

  // Clear all tracking arrays
  Object.keys(activeSounds).forEach((key) => delete activeSounds[key]);
  scheduledNotes.length = 0;

  console.log("🔇 All sounds cleared and tracking arrays reset");
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
      // Clear all hanging sounds
      clearAllSounds();

      // Reset timing to prevent multiple rapid fires
      nextNoteTargetTime = 0;

      // Reset all track timing states
      if (melodyState && melodyState.trackStates) {
        melodyState.trackStates.forEach((trackState) => {
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
  Object.keys(activeSounds).forEach((soundId) => {
    const sound = activeSounds[soundId];
    if (sound && sound.synth && sound.tone) {
      // Parse the current tone to extract note and octave
      const toneMatch = sound.tone.match(/^(\d+)([A-G]#?)$/);
      if (toneMatch) {
        const currentOctave = parseInt(toneMatch[1]);
        const note = toneMatch[2];

        // Calculate new octave, clamping to valid range (1-9)
        const targetOctave = Math.max(
          1,
          Math.min(9, currentOctave + octaveChange),
        );
        const newTone = `${targetOctave}${note}`;

        try {
          // Update the synth to slide to the new tone
          sound.synth.update({
            tone: newTone,
            duration: 0.05, // Very fast slide duration - minimal delay/skip
          });

          // Update our tracking information
          sound.tone = newTone;

          // Log the slide
          if (sound.isFallback) {
          } else {
          }
        } catch (error) {
          console.warn(`Failed to slide ${sound.tone} to ${newTone}:`, error);
          // If sliding fails, handle differently for struck vs held notes
          if (sound.isStruck) {
            // For struck notes, just remove from tracking - don't kill since they're self-managing
            delete activeSounds[soundId];
          } else {
            // For held notes, gracefully fade out the sound
            sound.synth.kill(0.1);
            delete activeSounds[soundId];
          }
        }
      }
    }
  });
}

// Preserve mutation state from existing tracks
function preserveMutationState() {
  const state = {
    singleTrack: null,
    tracks: [],
  };

  // Preserve single track state
  if (parsedMelody && parsedMelody.hasMutation) {
    state.singleTrack = {
      hasMutation: parsedMelody.hasMutation,
      mutationCount: parsedMelody.mutationCount,
      currentMutationZone: parsedMelody.currentMutationZone,
      mutationType: parsedMelody.mutationType,
      originalContent: parsedMelody.originalContent,
      mutationTriggerPositions: parsedMelody.mutationTriggerPositions,
      mutationTriggerPosition: parsedMelody.mutationTriggerPosition,
      mutatedNotes: parsedMelody.map((note) => ({
        isMutation: note.isMutation,
        originalNote: note.originalNote,
        originalOctave: note.originalOctave,
        // Preserve the actual current mutated note content
        currentNote: note.note,
        currentOctave: note.octave,
        duration: note.duration,
        waveType: note.waveType,
        volume: note.volume,
        tone: note.tone,
      })),
    };
  }

  // Preserve parallel tracks state
  if (melodyState && melodyState.tracks) {
    state.tracks = melodyState.tracks.map((track) => {
      if (!track.hasMutation) return null;

      return {
        hasMutation: track.hasMutation,
        mutationCount: track.mutationCount,
        currentMutationZone: track.currentMutationZone,
        mutationType: track.mutationType,
        originalContent: track.originalContent,
        mutationTriggerPositions: track.mutationTriggerPositions,
        mutationTriggerPosition: track.mutationTriggerPosition,
        mutatedNotes: track.map((note) => ({
          isMutation: note.isMutation,
          originalNote: note.originalNote,
          originalOctave: note.originalOctave,
          // Preserve the actual current mutated note content
          currentNote: note.note,
          currentOctave: note.octave,
          duration: note.duration,
          waveType: note.waveType,
          volume: note.volume,
          tone: note.tone,
        })),
      };
    });
  }

  return state;
}

// Restore mutation state to a newly parsed track
function restoreMutationState(newTrack, oldState) {
  if (!oldState) return;

  // Restore track-level mutation metadata
  newTrack.hasMutation = oldState.hasMutation;
  newTrack.mutationCount = oldState.mutationCount;
  newTrack.currentMutationZone = oldState.currentMutationZone;
  newTrack.mutationType = oldState.mutationType;
  newTrack.originalContent = oldState.originalContent;
  newTrack.mutationTriggerPositions = oldState.mutationTriggerPositions;
  newTrack.mutationTriggerPosition = oldState.mutationTriggerPosition;

  // Restore note-level mutation data
  if (oldState.mutatedNotes) {
    for (
      let i = 0;
      i < Math.min(newTrack.length, oldState.mutatedNotes.length);
      i++
    ) {
      const oldNoteState = oldState.mutatedNotes[i];
      if (oldNoteState.isMutation) {
        // This note was previously mutated, restore the full mutation with octave adjustment
        newTrack[i].isMutation = true;
        newTrack[i].originalNote = oldNoteState.originalNote;
        newTrack[i].originalOctave = oldNoteState.originalOctave;

        // Restore the actual mutated note content
        newTrack[i].note = oldNoteState.currentNote;

        // Apply octave adjustment: if the old octave was different from original,
        // maintain the same relative difference in the new octave
        if (oldNoteState.currentOctave && oldNoteState.originalOctave) {
          const octaveDifference =
            oldNoteState.currentOctave - oldNoteState.originalOctave;
          newTrack[i].octave = newTrack[i].octave + octaveDifference;
        } else if (oldNoteState.currentOctave) {
          // Use the preserved current octave adjusted to the new base
          const oldBaseOctave = oldNoteState.originalOctave || 4;
          const newBaseOctave = newTrack[i].octave || 4;
          const octaveShift = newBaseOctave - oldBaseOctave;
          newTrack[i].octave = oldNoteState.currentOctave + octaveShift;
        }

        // Restore other mutation properties
        if (oldNoteState.duration !== undefined)
          newTrack[i].duration = oldNoteState.duration;
        if (oldNoteState.waveType !== undefined)
          newTrack[i].waveType = oldNoteState.waveType;
        if (oldNoteState.volume !== undefined)
          newTrack[i].volume = oldNoteState.volume;

        // Update tone to match the mutated note and adjusted octave
        if (newTrack[i].note !== "rest") {
          // Use the already imported noteToTone function
          newTrack[i].tone = noteToTone(newTrack[i].note, newTrack[i].octave);
        } else {
          newTrack[i].tone = null;
        }
      }
    }
  }
}

function reparseMelodyWithNewOctave(newOctave) {
  if (originalMelodyString) {
    // Store current mutation state before reparsing
    const oldMutationState = preserveMutationState();

    // Reparse the melody with the new base octave
    melodyTracks = parseSimultaneousMelody(originalMelodyString, newOctave);

    if (melodyTracks.isSingleTrack) {
      // Single track
      parsedMelody = melodyTracks.tracks[0];

      // Restore mutation state to the newly parsed track
      restoreMutationState(parsedMelody, oldMutationState.singleTrack);

      sequence = extractTones(parsedMelody, {
        skipRests: false,
        restTone: `${newOctave}G`,
      });

      // Update melody state if it exists, preserving timing and position
      if (melodyState && melodyState.type === "single") {
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
      if (
        melodyState &&
        (melodyState.type === "multi" || melodyState.type === "parallel")
      ) {
        // Store current timing state for the main melody index
        const currentIndex = melodyState.index;
        const isFallback = melodyState.isFallback;

        // Store current track states if they exist
        const currentTrackStates = melodyState.trackStates
          ? melodyState.trackStates.map((ts) => ({
              noteIndex: ts.noteIndex,
              startTime: ts.startTime,
              totalElapsedBeats: ts.totalElapsedBeats,
              nextNoteTargetTime: ts.nextNoteTargetTime, // Preserve target time to prevent timing discontinuity
            }))
          : null;

        // Update with new parsed tracks
        melodyState.tracks = melodyTracks.tracks;
        melodyState.type = melodyTracks.type;
        melodyState.maxLength = melodyTracks.maxLength || 0;
        melodyState.isFallback = isFallback; // Preserve fallback status

        // Restore mutation state to each track
        melodyTracks.tracks.forEach((track, index) => {
          if (oldMutationState.tracks && oldMutationState.tracks[index]) {
            restoreMutationState(track, oldMutationState.tracks[index]);
          }
        });

        // Update or create track states for parallel timing
        if (melodyState.type === "parallel") {
          melodyState.trackStates = melodyTracks.tracks.map(
            (track, trackIndex) => {
              // Try to preserve timing from existing track state
              const existingState =
                currentTrackStates && currentTrackStates[trackIndex];
              return {
                trackIndex: trackIndex,
                noteIndex: existingState ? existingState.noteIndex : 0,
                track: track,
                nextNoteTargetTime: existingState
                  ? existingState.nextNoteTargetTime
                  : 0, // Preserve target time for seamless continuity
                startTime: existingState ? existingState.startTime : 0, // Preserve start time
                totalElapsedBeats: existingState
                  ? existingState.totalElapsedBeats
                  : 0, // Preserve elapsed time
                lastMutationTriggered: existingState
                  ? existingState.lastMutationTriggered
                  : false, // Preserve mutation flag
              };
            },
          );
        }

        // Preserve global timing
        melodyState.index = currentIndex;

        // Update parsedMelody for backward compatibility
        parsedMelody = melodyTracks.tracks[0];
        sequence = extractTones(parsedMelody, {
          skipRests: false,
          restTone: `${newOctave}G`,
        });
      }
    }
  } else if (melodyState && melodyState.isFallback) {
    // Handle fallback notes - reparse with new octave
    originalMelodyString = "cdefgab"; // Use the same fallback pattern
    melodyTracks = parseSimultaneousMelody(originalMelodyString, newOctave);
    parsedMelody = melodyTracks.tracks[0];
    sequence = extractTones(parsedMelody, {
      skipRests: false,
      restTone: `${newOctave}G`,
    });

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

function act({ event: e, clock, sound: { synth }, screen, ui, typeface, api }) {
  // Initialize NOW line position if not set
  if (nowLineY === 0) {
    nowLineY = screen.height / 2;
  }

  // Timeline area bounds for NOW line dragging
  const timelineX = Math.round(screen.width / 4);
  const timelineWidth = 150;
  const timelineStartX = timelineX - timelineWidth / 2;
  const timelineEndX = timelineX + timelineWidth / 2;

  // Handle unified dragging: cyan line position AND timing adjustment in one action
  if (e.is("touch")) {
    // Allow dragging from anywhere on screen - no position restrictions
    // Store initial position for relative dragging
    isDraggingNowLine = true;
  }

  if (e.is("draw") && isDraggingNowLine) {
    // UNIFIED ACTION: Update both NOW line position AND timing simultaneously

    // 1. Update NOW line position RELATIVELY based on drag delta (not absolute position)
    const newY = nowLineY + e.delta.y;
    nowLineY = Math.max(50, Math.min(screen.height - 50, newY)); // Keep within bounds

    // 2. ALSO adjust timing based on the same drag movement
    // Negative delta.y (moving up) decreases divisor (faster timing)
    // Positive delta.y (moving down) increases divisor (slower timing)
    const pixelMovement = e.delta.y * 0.01; // More sensitive pixel-to-timing conversion

    // Apply movement directly to current divisor
    const newDivisor = currentTimeDivisor + pixelMovement;

    // Clamp to reasonable values (0.1s to 10.0s)
    const clampedDivisor = Math.max(0.1, Math.min(10.0, newDivisor));

    // Update if there's any change (pixel-by-pixel)
    if (Math.abs(clampedDivisor - currentTimeDivisor) > 0.001) {
      // Only trigger tempo change if enough time has passed since last change
      const now = performance.now();
      if (now - lastTempoChangeTime > tempoChangeTolerance) {
        lastTempoChangeTime = now;
        tempoJustChanged = true; // Flag for immediate timing adjustment
      }

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

  // Stop dragging when touch ends
  if (e.is("lift")) {
    isDraggingNowLine = false;
  }

  // Handle screen resize/reframe - rebuild octave buttons
  if (e.is("reframed")) {
    buildOctaveButtons(api);
  }

  // Handle keyboard input for octave changes
  if (e.is("keyboard:down:arrowup")) {
    // Increase octave (max 8)
    if (octave < 8) {
      const oldOctave = octave;
      octave++;
      octaveFlashTime = performance.now(); // Trigger flash effect

      // Rebuild buttons with new octave value
      buildOctaveButtons(api);

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
      octaveFlashTime = performance.now(); // Trigger flash effect

      // Rebuild buttons with new octave value
      buildOctaveButtons(api);

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

  // Toggle note flow mode with 'f' key
  if (e.is("keyboard:down:f")) {
    noteFlowMode = noteFlowMode === "stop" ? "flow" : "stop";
  }

  // Handle octave button interactions
  octavePlusBtn?.act(e, {
    down: () => {
      // No feedback sound on down to keep it simple
    },
    push: (btn) => {
      // Increase octave (max 8)
      if (octave < 8) {
        const oldOctave = octave;
        octave++;
        octaveFlashTime = performance.now(); // Trigger flash effect

        // Rebuild buttons with new octave value
        buildOctaveButtons(api);

        // Slide existing sounds to new octave instead of cutting them off
        slideActiveSoundsToNewOctave(oldOctave, octave);

        // Reparse melody with new octave
        reparseMelodyWithNewOctave(octave);
      }
    },
  });

  octaveMinusBtn?.act(e, {
    down: () => {
      // No feedback sound on down to keep it simple
    },
    push: (btn) => {
      // Decrease octave (min 1)
      if (octave > 1) {
        const oldOctave = octave;
        octave--;
        octaveFlashTime = performance.now(); // Trigger flash effect

        // Rebuild buttons with new octave value
        buildOctaveButtons(api);

        // Slide existing sounds to new octave instead of cutting them off
        slideActiveSoundsToNewOctave(oldOctave, octave);

        // Reparse melody with new octave
        reparseMelodyWithNewOctave(octave);
      }
    },
  });

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

// Simple musical state - no complex timeline needed
let musicalState = {
  lastSyncTime: 0,
  isInitialized: false,
};

function sim({ sound, beep, clock, num, help, params, colon, screen }) {
  sound.speaker?.poll();

  // Get current time for timing calculations
  const now = performance.now();

  // Early exit if we're leaving the piece to prevent new sounds
  if (isLeavingPiece) {
    return;
  }

  // Check for standby/resume before processing anything else
  const wasInStandby = handleStandbyResume();

  // Process scheduled note releases for managed sound system
  processScheduledReleases();

  // Get the current synced time
  const syncedDate = clock.time();

  // Simple musical state initialization
  if (!musicalState.isInitialized && syncedDate) {
    musicalState.lastSyncTime = syncedDate.getTime();
    musicalState.isInitialized = true;
  }

  // Periodically resync the clock to stay accurate
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
    if (melodyState.type === "single") {
      if (!melodyState.notes || melodyState.notes.length === 0) return;

      const noteData = melodyState.notes[melodyState.index];
      if (!noteData) return;

      const {
        note,
        octave: noteOctave,
        duration,
        swing,
        swingAmount,
        waveType,
        volume,
        struck,
      } = noteData;

      // Play the note using managed sound system
      if (note !== "rest") {
        let tone = noteOctave
          ? `${noteOctave}${note.toUpperCase()}`
          : `${octave}${note.toUpperCase()}`;
        const noteDuration = duration * melodyState.baseTempo;

        try {
          // Check if this is the last note in the sequence to eliminate loop gap
          const isLastNote = (melodyState.index === melodyState.notes.length - 1);
          
          // Create managed sound that will be automatically released
          createManagedSound(
            sound,
            tone,
            waveType,
            noteDuration,
            volume || 0.8, // Use note volume or default to 0.8
            melodyState.isFallback,
            struck, // Pass the struck flag for note timing behavior
            isLastNote, // Eliminate gap for last note to prevent loop timing issues
          );

          // Add note to history buffer for visual timeline
          addNoteToHistory(
            note,
            noteOctave || octave,
            currentTime,
            noteDuration,
            0, // Track 0 for single track
            waveType || "sine",
            volume || 0.8,
            false, // Not a mutation (mutations are added separately)
            struck,
          );

          // Increment total notes played for persistent white note history
          totalNotesPlayed++;
        } catch (error) {
          console.error(
            `%c✗ ${tone} - ${error}`,
            "color: red; background: black; font-weight: bold; padding: 2px;",
          );
        }
      } else {
        // Log rest notes too for complete timing tracking - but less verbosely
        const restDuration = duration * melodyState.baseTempo;

        // Add rest to history buffer too for complete timeline
        addNoteToHistory(
          "rest",
          noteOctave || octave,
          currentTime,
          restDuration,
          0, // Track 0 for single track
          waveType || "sine",
          volume || 0.8,
          false,
          struck,
        );
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

      // Track completed sequences for white note history
      if (
        oldIndex === melodyState.notes.length - 1 &&
        melodyState.index === 0
      ) {
        completedSequences++;
      }

      // Reset mutation zones when melody loops back to beginning
      if (
        oldIndex === melodyState.notes.length - 1 &&
        melodyState.index === 0
      ) {
        // Check for end-of-track mutation before resetting the flag
        if (melodyState.hasMutation && !melodyState.lastMutationTriggered) {
          // Store the original track before mutation to compare changes
          const originalTrack = [...parsedMelody];

          // Apply mutation to this single track
          const mutatedTrack = mutateMelodyTrack(
            parsedMelody,
            melodyState.originalContent,
            octave,
          );

          // Find which notes actually changed (not just marked as mutations)
          const mutationIndices = [];
          let recentlyMutatedIndex = -1;
          for (
            let i = 0;
            i < Math.min(mutatedTrack.length, originalTrack.length);
            i++
          ) {
            const originalNote = originalTrack[i];
            const mutatedNote = mutatedTrack[i];

            // Check if the note actually changed
            if (
              originalNote &&
              mutatedNote &&
              (originalNote.note !== mutatedNote.note ||
                originalNote.octave !== mutatedNote.octave)
            ) {
              mutationIndices.push(i);
              recentlyMutatedIndex = i; // This is the note that just changed
            }
          }
          mutatedNotePositions = mutationIndices;
          recentlyMutatedNoteIndex = recentlyMutatedIndex;
          recentlyMutatedTrackIndex = 0; // Single track is always track 0

          // Set mutation flash time for asterisk and note animation
          mutationFlashTime = performance.now();

          // For now, trigger all asterisks to flash white (simplified approach)
          triggeredAsteriskPositions = ["*"]; // Special marker to flash all asterisks

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
          sequence = extractTones(parsedMelody, {
            skipRests: false,
            restTone: `${octave}G`,
          });
        }

        // Reset the mutation trigger flag when looping back to allow next mutation
        melodyState.lastMutationTriggered = false;

        if (
          melodyState.mutationTriggerPositions &&
          melodyState.mutationTriggerPositions.length > 0
        ) {
          melodyState.currentMutationZone = 0;
          parsedMelody.currentMutationZone = 0;
          melodyState.notes.currentMutationZone = 0;
        }
      }

      // Mutation logic moved to timing gap checking - no longer trigger here immediately
    }
  }

  // Super simple melody timing - use direct UTC time
  if (time && !wasInStandby && musicalState.isInitialized) {
    // Skip timing checks on standby frames to prevent rapid-fire
    const currentTimeMs = time.getTime(); // Use direct UTC time for musical sync
    let anyTrackPlayed = false; // Track whether any track played in this frame

    // Handle immediate tempo changes for overlapping sounds
    if (tempoJustChanged) {
      tempoJustChanged = false; // Reset the flag

      // IMMEDIATE TEMPO RESPONSE: Allow overlapping sounds by adjusting timing immediately
      if (
        melodyState &&
        melodyState.type === "single" &&
        nextNoteTargetTime > 0
      ) {
        // Set next note to play very soon, creating overlap instead of waiting
        nextNoteTargetTime = Math.min(nextNoteTargetTime, currentTimeMs + 50); // Max 50ms delay
      } else if (
        melodyState &&
        melodyState.type === "parallel" &&
        melodyState.trackStates
      ) {
        // Update all parallel tracks for immediate tempo response
        melodyState.trackStates.forEach((trackState) => {
          if (trackState.nextNoteTargetTime > 0) {
            trackState.nextNoteTargetTime = Math.min(
              trackState.nextNoteTargetTime,
              currentTimeMs + 50,
            );
          }
        });
      }
    }

    if (hasMelodyContent(melodyState)) {
      if (melodyState.type === "single") {
        // Single track timing - use simple direct timing
        if (nextNoteTargetTime === 0) {
          // Check if melody starts with rest notes - if so, skip ahead to first audible note
          let firstAudibleIndex = 0;
          let cumulativeRestDuration = 0;

          // Find the first non-rest note
          while (
            firstAudibleIndex < melodyState.notes.length &&
            melodyState.notes[firstAudibleIndex].note === "rest"
          ) {
            cumulativeRestDuration +=
              melodyState.notes[firstAudibleIndex].duration *
              melodyState.baseTempo;
            firstAudibleIndex++;
          }

          if (
            firstAudibleIndex > 0 &&
            firstAudibleIndex < melodyState.notes.length
          ) {
            // Melody starts with rest notes but has audible notes later - skip ahead to first audible note
            melodyState.index = firstAudibleIndex;
            // Start immediately instead of waiting for UTC boundary
            nextNoteTargetTime = currentTimeMs;
          } else if (
            firstAudibleIndex >= melodyState.notes.length &&
            melodyState.hasMutation
          ) {
            // All notes are rests AND we have mutation enabled - trigger immediate mutation
            melodyState.index = 0;
            nextNoteTargetTime = currentTimeMs;

            // Immediately trigger the first mutation to create audible content
            // Store the original track before mutation to compare changes
            const originalTrack = [...parsedMelody];

            const mutatedTrack = mutateMelodyTrack(
              parsedMelody,
              melodyState.originalContent,
              octave,
            );

            // Find which notes actually changed (not just marked as mutations)
            const mutationIndices = [];
            let recentlyMutatedIndex = -1;
            for (
              let i = 0;
              i < Math.min(mutatedTrack.length, originalTrack.length);
              i++
            ) {
              const originalNote = originalTrack[i];
              const mutatedNote = mutatedTrack[i];

              // Check if the note actually changed
              if (
                originalNote &&
                mutatedNote &&
                (originalNote.note !== mutatedNote.note ||
                  originalNote.octave !== mutatedNote.octave)
              ) {
                mutationIndices.push(i);
                recentlyMutatedIndex = i; // This is the note that just changed
              }
            }
            mutatedNotePositions = mutationIndices;
            recentlyMutatedNoteIndex = recentlyMutatedIndex;
            recentlyMutatedTrackIndex = 0; // Single track is always track 0

            // Set mutation flash time for asterisk and note animation
            mutationFlashTime = performance.now();

            // For now, trigger all asterisks to flash white (simplified approach)
            // In the future, this could be made more specific to the triggered asterisk
            triggeredAsteriskPositions = ["*"]; // Special marker to flash all asterisks

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
            sequence = extractTones(parsedMelody, {
              skipRests: false,
              restTone: `${octave}G`,
            });

            // Now actually play the first note after mutation
            bleep(time);
            anyTrackPlayed = true;
          } else if (firstAudibleIndex >= melodyState.notes.length) {
            // All notes are rests but no mutation - start immediately anyway
            melodyState.index = 0;
            nextNoteTargetTime = currentTimeMs;
          } else {
            // First note is audible - use direct timing for normal timing
            nextNoteTargetTime = Math.ceil(currentTimeMs / 1000) * 1000;
          }
        }

        // Check if it's time to play the next note using direct timing
        if (nextNoteTargetTime > 0 && currentTimeMs >= nextNoteTargetTime) {
          const timingGap = currentTimeMs - nextNoteTargetTime;

          // CRITICAL: Get the current note data BEFORE calling bleep (which advances the index)
          const currentNoteData = melodyState.notes[melodyState.index];

          bleep(time);
          anyTrackPlayed = true;

          // Calculate next note target time using the note we just played (not the next note)
          if (currentNoteData) {
            const noteDuration =
              currentNoteData.duration * (melodyState.baseTempo || baseTempo); // Use consistent tempo
            const oldTarget = nextNoteTargetTime;
            
            // CRITICAL FIX: Use sequential timing like parallel tracks
            // Each note should start exactly when the previous note ends, not based on current time
            nextNoteTargetTime = nextNoteTargetTime + noteDuration;
          }
        }

        // Check for mutations during the gap between notes (using direct timing)
        if (
          nextNoteTargetTime > 0 &&
          melodyState &&
          melodyState.notes &&
          melodyState.notes.length > 0
        ) {
          // Calculate when the current note should have finished using direct timing
          const currentNoteData =
            melodyState.notes[
              (melodyState.index - 1 + melodyState.notes.length) %
                melodyState.notes.length
            ];

          if (currentNoteData && lastNoteStartTime > 0) {
            const currentNoteEndTime =
              lastNoteStartTime +
              currentNoteData.duration * (melodyState.baseTempo || baseTempo); // Use consistent tempo

            // Check if we're in the timing gap: current note has finished but next note hasn't started yet
            if (
              (currentTimeMs >= currentNoteEndTime &&
                currentTimeMs < nextNoteTargetTime) ||
              (currentTimeMs >= currentNoteEndTime &&
                currentNoteEndTime === nextNoteTargetTime)
            ) {
              // Check for mutation triggers at the note position that just finished playing
              let shouldMutate = false;
              let isEndOfTrackMutation = false;

              // Calculate the previous note position (the one that just finished)
              const previousNoteIndex =
                (melodyState.index - 1 + melodyState.notes.length) %
                melodyState.notes.length;

              // Check for end-of-track asterisk mutation (legacy) - prioritize this
              if (
                previousNoteIndex === melodyState.notes.length - 1 &&
                melodyState.hasMutation &&
                !melodyState.lastMutationTriggered
              ) {
                // We just finished the last note, trigger end-of-track mutation
                shouldMutate = true;
                isEndOfTrackMutation = true;
                melodyState.lastMutationTriggered = true; // Prevent rapid re-triggering
              }
              // Check if the note that just finished was a mutation trigger position (multiple zones)
              else if (
                melodyState.hasMutation &&
                melodyState.mutationTriggerPositions &&
                melodyState.mutationTriggerPositions.length > 0
              ) {
                // Multiple mutation zones system
                const currentZone = melodyState.currentMutationZone || 0;
                const triggerPosition =
                  melodyState.mutationTriggerPositions[currentZone];

                // Check if the note that just finished was the trigger
                // For end-of-track asterisks, trigger when the last note finishes
                const isDirectTrigger = previousNoteIndex === triggerPosition;
                const isEndOfTrackTrigger =
                  triggerPosition >= melodyState.notes.length &&
                  previousNoteIndex === melodyState.notes.length - 1;
                const shouldTrigger =
                  (isDirectTrigger || isEndOfTrackTrigger) &&
                  !melodyState.lastMutationTriggered;

                if (
                  currentZone < melodyState.mutationTriggerPositions.length &&
                  shouldTrigger
                ) {
                  shouldMutate = true;
                  melodyState.lastMutationTriggered = true; // Prevent rapid re-triggering
                }
              }

              if (shouldMutate) {
                // Store the original track before mutation to compare changes
                const originalTrack = [...parsedMelody];

                // Apply mutation to this single track
                const mutatedTrack = mutateMelodyTrack(
                  parsedMelody,
                  melodyState.originalContent,
                  octave,
                );

                // Find which notes were actually changed (not just marked as mutations)
                let mutationIndices = [];
                let recentlyMutatedIndex = -1;

                if (
                  melodyState.mutationTriggerPositions &&
                  melodyState.mutationTriggerPositions.length > 0
                ) {
                  // Multiple zones: find only the newly changed notes in the zone that was just processed
                  const triggerPositions = melodyState.mutationTriggerPositions;
                  const currentZone = melodyState.currentMutationZone || 0;

                  // The zone that was just mutated is the zone we're currently in (before advancing)
                  const mutatedZoneIndex = currentZone;

                  // Determine the range for the zone that was just mutated
                  const zoneStart =
                    mutatedZoneIndex === 0
                      ? 0
                      : triggerPositions[mutatedZoneIndex - 1];
                  const zoneEnd =
                    mutatedZoneIndex < triggerPositions.length
                      ? triggerPositions[mutatedZoneIndex]
                      : mutatedTrack.length;

                  // Find notes that actually changed in this zone by comparing original vs mutated
                  for (
                    let i = zoneStart;
                    i <
                    Math.min(
                      zoneEnd,
                      mutatedTrack.length,
                      originalTrack.length,
                    );
                    i++
                  ) {
                    const originalNote = originalTrack[i];
                    const mutatedNote = mutatedTrack[i];

                    // Check if the note actually changed (not just marked as mutation)
                    if (
                      originalNote &&
                      mutatedNote &&
                      (originalNote.note !== mutatedNote.note ||
                        originalNote.octave !== mutatedNote.octave)
                    ) {
                      mutationIndices.push(i);
                      recentlyMutatedIndex = i; // This is the note that just changed
                    }
                  }
                } else {
                  // Single zone or legacy: find all notes that actually changed
                  for (
                    let i = 0;
                    i < Math.min(mutatedTrack.length, originalTrack.length);
                    i++
                  ) {
                    const originalNote = originalTrack[i];
                    const mutatedNote = mutatedTrack[i];

                    // Check if the note actually changed
                    if (
                      originalNote &&
                      mutatedNote &&
                      (originalNote.note !== mutatedNote.note ||
                        originalNote.octave !== mutatedNote.octave)
                    ) {
                      mutationIndices.push(i);
                      recentlyMutatedIndex = i; // This is the note that just changed
                    }
                  }
                }

                mutatedNotePositions = mutationIndices;
                recentlyMutatedNoteIndex = recentlyMutatedIndex;
                recentlyMutatedTrackIndex = 0; // Single track is always track 0

                // Set mutation flash time for asterisk and note animation
                mutationFlashTime = performance.now();

                // For now, trigger all asterisks to flash white (simplified approach)
                // In the future, this could be made more specific to the triggered asterisk
                triggeredAsteriskPositions = ["*"]; // Special marker to flash all asterisks

                // Update the parsed melody
                parsedMelody = mutatedTrack;
                melodyState.notes = mutatedTrack;

                // Preserve mutation metadata
                parsedMelody.hasMutation = true;
                parsedMelody.originalContent = parsedMelody.originalContent;
                parsedMelody.mutationCount =
                  (parsedMelody.mutationCount || 0) + 1;
                melodyState.notes.hasMutation = true;
                melodyState.notes.originalContent =
                  parsedMelody.originalContent;
                melodyState.notes.mutationCount = parsedMelody.mutationCount;

                // If using multiple mutation zones, advance to next zone after mutation
                if (
                  melodyState.mutationTriggerPositions &&
                  melodyState.mutationTriggerPositions.length > 0 &&
                  !isEndOfTrackMutation
                ) {
                  const currentZone = melodyState.currentMutationZone || 0;
                  const nextZone = currentZone + 1;

                  if (nextZone < melodyState.mutationTriggerPositions.length) {
                    // Advance to next zone - update ALL references
                    melodyState.currentMutationZone = nextZone;
                    parsedMelody.currentMutationZone = nextZone;
                    melodyState.notes.currentMutationZone = nextZone;

                    // Also update the mutated track metadata
                    mutatedTrack.currentMutationZone = nextZone;
                  }
                }

                // Update the sequence for backward compatibility
                sequence = extractTones(parsedMelody, {
                  skipRests: false,
                  restTone: `${octave}G`,
                });
              }
            }
          }
        }

        if (anyTrackPlayed) {
          synced = true;
        }
      } else if (melodyState.type === "parallel" && melodyState.trackStates) {
        // PERFECT DIVISIONAL TIMING: Each track maintains mathematical precision
        melodyState.trackStates.forEach((trackState, trackIndex) => {
          if (!trackState.track || trackState.track.length === 0) return;

          // Initialize timing for this track if not set
          if (trackState.nextNoteTargetTime === 0) {
            // All tracks start from the same precise UTC second boundary
            const utcStartTime = Math.ceil(currentTimeMs / 1000) * 1000;
            trackState.nextNoteTargetTime = utcStartTime;

            // Find first audible note in this track
            let firstAudibleIndex = 0;
            while (
              firstAudibleIndex < trackState.track.length &&
              trackState.track[firstAudibleIndex].note === "rest"
            ) {
              firstAudibleIndex++;
            }

            if (
              firstAudibleIndex > 0 &&
              firstAudibleIndex < trackState.track.length
            ) {
              trackState.noteIndex = firstAudibleIndex;
            } else if (firstAudibleIndex >= trackState.track.length) {
              trackState.noteIndex = 0;
            }
          }

          // Check if it's time to play the next note in this track
          if (
            trackState.nextNoteTargetTime > 0 &&
            currentTimeMs >= trackState.nextNoteTargetTime
          ) {
            const noteData = trackState.track[trackState.noteIndex];
            if (noteData) {
              // Play the note
              const {
                note,
                octave: noteOctave,
                duration,
                waveType,
                volume,
                struck,
              } = noteData;

              if (note !== "rest") {
                let tone = noteOctave
                  ? `${noteOctave}${note.toUpperCase()}`
                  : `${octave}${note.toUpperCase()}`;
                const noteDuration = duration * melodyState.baseTempo;

                // DEBUG: Log note playback with duration details for sticky modifier tracking
                if (trackIndex === 0) {

                }

                try {
                  // Check if this is the last note in this track's sequence to eliminate loop gap
                  const isLastNoteInTrack = (trackState.index === trackState.track.length - 1);
                  
                  createManagedSound(
                    sound,
                    tone,
                    waveType,
                    noteDuration,
                    volume || 0.8,
                    melodyState.isFallback,
                    struck,
                    isLastNoteInTrack, // Eliminate gap for last note to prevent loop timing issues
                  );

                  // Add note to history buffer for visual timeline
                  addNoteToHistory(
                    note,
                    noteOctave || octave,
                    currentTimeMs,
                    noteDuration,
                    trackIndex,
                    waveType || "sine",
                    volume || 0.8,
                    false, // Not a mutation (mutations are added separately)
                    struck,
                  );

                  totalNotesPlayed++;
                } catch (error) {
                  console.error(
                    `%c✗ Track ${trackIndex + 1} ${tone} - ${error}`,
                    "color: red; background: black; font-weight: bold; padding: 2px;",
                  );
                }
              } else {
                // Add rest to history buffer for complete timeline
                const restDuration = duration * melodyState.baseTempo;
                addNoteToHistory(
                  "rest",
                  noteOctave || octave,
                  currentTimeMs,
                  restDuration,
                  trackIndex,
                  waveType || "sine",
                  volume || 0.8,
                  false,
                  struck,
                );
              }

              anyTrackPlayed = true;

              // Update timing tracking for this track
              trackState.startTime = performance.now();

              // Update global timing for flowing note animation AND timeline visualization
              currentNoteStartTime = performance.now();
              currentNoteDuration = duration * melodyState.baseTempo;
              
              // CRITICAL: For parallel tracks, use a more sophisticated approach to global timing
              // Instead of overwriting global timing with each track, use the track that started most recently
              // This prevents animation jitter when tracks have different durations
              if (!lastNoteStartTime || currentTimeMs >= lastNoteStartTime) {
                lastNoteStartTime = currentTimeMs; // Use currentTimeMs for consistency with single track
                lastNoteDuration = duration * melodyState.baseTempo;
              }
              
              // Also store track-specific timing for this track
              trackState.lastNoteStartTime = currentTimeMs;
              trackState.lastNoteDuration = duration * melodyState.baseTempo;

              // Advance to next note in this track
              const oldIndex = trackState.noteIndex;
              trackState.noteIndex =
                (trackState.noteIndex + 1) % trackState.track.length;

              // Check for mutations when this track loops back to beginning
              if (
                oldIndex === trackState.track.length - 1 &&
                trackState.noteIndex === 0
              ) {
                // Check if this track has mutations
                if (trackState.track.hasMutation) {
                  const originalTrack = [...trackState.track];
                  const mutatedTrack = mutateMelodyTrack(
                    trackState.track,
                    trackState.track.originalContent || trackState.track,
                    octave,
                  );

                  const mutationIndices = [];
                  let recentlyMutatedIndex = -1;
                  for (
                    let i = 0;
                    i < Math.min(mutatedTrack.length, originalTrack.length);
                    i++
                  ) {
                    const originalNote = originalTrack[i];
                    const mutatedNote = mutatedTrack[i];

                    if (
                      originalNote &&
                      mutatedNote &&
                      (originalNote.note !== mutatedNote.note ||
                        originalNote.octave !== mutatedNote.octave)
                    ) {
                      mutationIndices.push(i);
                      recentlyMutatedIndex = i;
                    }
                  }

                  trackState.track = mutatedTrack;
                  trackState.track.hasMutation = true;
                  trackState.track.originalContent =
                    originalTrack.originalContent || originalTrack;
                  trackState.track.mutationCount =
                    (trackState.track.mutationCount || 0) + 1;

                  melodyState.tracks[trackIndex] = trackState.track;

                  mutationFlashTime = performance.now();
                  triggeredAsteriskPositions = ["*"];

                  if (mutationIndices.length > 0) {
                    mutatedNotePositions = mutationIndices;
                    recentlyMutatedNoteIndex = recentlyMutatedIndex;
                    recentlyMutatedTrackIndex = trackIndex;
                  }
                }
              }

              // PERFECT MATHEMATICAL TIMING: Calculate exact next note time with no drift
              // For single-track melodies parsed as parallel (like c.defg..ef), ensure proper sequential timing
              const noteDuration = noteData.duration * melodyState.baseTempo;
              
              // CRITICAL FIX: For sticky duration inheritance, ensure each note waits for the full duration
              // of the previous note, respecting inherited duration modifiers like c.defg..ef
              trackState.nextNoteTargetTime = trackState.nextNoteTargetTime + noteDuration;
              
              // DEBUG: Log timing calculation for sticky duration debugging
              if (trackIndex === 0 && note !== "rest") {

              }
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
      const timeSinceLastBeat = currentTimeMs - utcBeatIndex * intervalMs;

      // Trigger if we just crossed a beat boundary
      if (
        timeSinceLastBeat < 50 &&
        (lastNoteStartTime === 0 || currentTimeMs > lastNoteStartTime + 100)
      ) {
        lastNoteStartTime = currentTimeMs;
        bleep(time);
        anyTrackPlayed = true;
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
  // Set flag to prevent new sounds from being created
  isLeavingPiece = true;

  // Clear all active sounds to prevent lingering beeps
  clearAllSounds();

  // Double-check: force clear again after a brief delay to catch any stragglers
  setTimeout(() => {
    if (Object.keys(activeSounds).length > 0) {
      console.warn(
        `🔇 Force-clearing ${Object.keys(activeSounds).length} lingering sounds`,
      );
      clearAllSounds();
    }
  }, 10);
}

function preview({ ink, wipe, write }) {
  // Render a custom thumbnail image showing the fallback melody
  wipe("black");

  // Test colored text with wrapping to debug the off-by-one issue
  write(
    "\\\\yellow\\\\hello \\\\red\\\\world \\\\green\\\\this \\\\blue\\\\is \\\\cyan\\\\a \\\\magenta\\\\very \\\\orange\\\\long \\\\white\\\\melody \\\\gray\\\\test",
    {
      x: 6,
      y: 6,
      bounds: 60, // Force wrapping
      scale: 1,
    },
  );

  // Add a small clock icon below
  ink("cyan");
  write("🕐", {
    x: 20,
    y: 40,
    scale: 1,
  });
}

// function icon() {
// Render an application icon, aka favicon.
// }

// Build octave control buttons (+ and - buttons with octave number in between)
function buildOctaveButtons({ screen, ui, typeface }) {
  const glyphWidth = typeface.glyphs["0"].resolution[0];
  const buttonHeight = 16;
  const buttonWidth = 16;
  const spacing = 0; // No gaps between buttons
  const padding = 4; // Padding on left and right of octave number

  // Calculate total width needed for the three-button layout: - 4 +
  const octaveText = `${octave}`;
  const octaveTextWidth = octaveText.length * glyphWidth + padding * 2;
  const totalWidth = buttonWidth + octaveTextWidth + buttonWidth;

  // Position flush to bottom right corner
  const startX = screen.width - totalWidth;
  const startY = screen.height - buttonHeight;

  // Create minus button
  octaveMinusBtn = new ui.Button(startX, startY, buttonWidth, buttonHeight);

  // Create plus button (after text)
  octavePlusBtn = new ui.Button(
    startX + buttonWidth + octaveTextWidth,
    startY,
    buttonWidth,
    buttonHeight,
  );
}
