// Clock, 2025.6.27.12.00
// Just a standard clock with melody support and UTC sync.

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

// Animation stability for flowing notes during drag operations
let flowingNotesCache = {
  lastTimeDivisor: 1.0,
  cachedVisualSpeed: null,
  cachedTimeSpan: null,
  cacheValid: false,
  isDragging: false
};

// Smooth animation system for flowing notes
let flowingNotesAnimation = {
  targetTimingLineY: null,
  currentTimingLineY: null,
  targetVisualSpeed: null,
  currentVisualSpeed: null,
  lerpFactor: 0.15, // How fast to lerp (0.05 = slow, 0.3 = fast)
  isInitialized: false
};

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

    // --- Draw the cyan timing line first, then the clock text centered on it ---
    // Calculate the Y position of the cyan line (same as in drawTimingGraph)
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
    // Draw the cyan line first so it appears behind the text
    ink("cyan").line(0, currentYPos, screen.width, currentYPos);

    // --- Draw the 'now' label on the cyan line ---
    // The 1s timing is always at centerY
    const nowLabel = "now";
    const nowBox =
      typeof text !== "undefined" && text.box
        ? text.box(nowLabel, { scale: 1 })
        : { width: 18, height: 12 };
    const nowX = 8; // Padding from left edge
    const nowY = centerY - Math.round(nowBox.height / 2);
    // Draw a green background for contrast
    if (typeof text !== "undefined" && text.box && text.box.draw) {
      text.box.draw(nowLabel, {
        x: nowX,
        y: nowY,
        scale: 1,
        color: "green",
      });
    } else {
      if (ink("green").fillRect) {
        ink("green").fillRect(
          nowX - 4,
          nowY - 2,
          nowBox.width + 8,
          nowBox.height + 4,
        );
      }
    }
    ink("white").write(
      nowLabel,
      {
        x: nowX,
        y: nowY,
        scale: 1,
      },
      "green",
    );
    // Draw the clock text centered on the line (in front), with white text on a colored background
    const box =
      typeof text !== "undefined" && text.box
        ? text.box(timeString, { scale: fontSize })
        : { width: timeString.length * 6, height: 12 };
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
    if (typeof text !== "undefined" && text.box && text.box.draw) {
      text.box.draw(timeString, {
        x: textX,
        y: textY,
        scale: fontSize,
        color: timerBgColor,
      });
    } else {
      // Fallback: draw a filled rectangle if possible (legacy or custom ink API)
      if (ink(timerBgColor).fillRect) {
        ink(timerBgColor).fillRect(
          textX - 4,
          textY - 2,
          box.width + 8,
          box.height + 4,
        );
      }
      // If no fillRect, just skip background
    }
    // Draw the time string in white, centered on the line
    // Always use white text for the main clock display to ensure good contrast
    // against the colored timing backgrounds (red/green/black)
    ink("white").write(
      timeString,
      {
        x: textX,
        y: textY,
        scale: fontSize,
      },
      timerBgColor,
    );
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
    // Paint octave control buttons even when syncing
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
    // Note: No longer drawing melody timeline on main screen when syncing
    // Draw timing graph even when syncing (intentionally omitted)
    // drawTimingGraph(ink, write, screen);
  }

  // Draw flowing note visualization last so it appears on top of everything
  if (syncedDate) {
    drawFlowingNotes(ink, write, screen, melodyState, syncedDate);
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
    screen.width < 300
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

// Draw flowing notes visualization - simple red note drop animation
function drawFlowingNotes(ink, write, screen, melodyState, syncedDate) {
  if (!melodyState || !hasMelodyContent(melodyState) || !syncedDate) {
    return;
  }

  // Use smoothly interpolated values from updateFlowingNotesAnimation() for buttery smooth animation
  let actualTimingLineY, visualSpeed, smoothCurrentTimeDivisor;
  
  if (flowingNotesAnimation.isInitialized) {
    // Use lerped values for smooth animation
    actualTimingLineY = Math.round(flowingNotesAnimation.currentTimingLineY);
    visualSpeed = flowingNotesAnimation.currentVisualSpeed;
    
    // Calculate a smooth time divisor based on the lerped timing line position
    // This ensures note duration calculations also use smoothed values
    const centerY = Math.round(screen.height / 2);
    const meterHeight = screen.height;
    const minTiming = 0.05;
    const maxTiming = 3.0;
    const scaleY = meterHeight / (maxTiming - minTiming);
    const normalizedPos = (actualTimingLineY - centerY) / scaleY;
    smoothCurrentTimeDivisor = Math.max(minTiming, Math.min(maxTiming, 1.0 + normalizedPos));
  } else {
    // Fallback to direct calculation if updateFlowingNotesAnimation hasn't run yet
    const centerY = Math.round(screen.height / 2);
    const meterStartY = 0;
    const meterEndY = screen.height;
    const meterHeight = meterEndY - meterStartY;
    const minTiming = 0.05;
    const maxTiming = 3.0;
    const scaleY = meterHeight / (maxTiming - minTiming);
    actualTimingLineY = Math.round(
      centerY + (currentTimeDivisor - 1.0) * scaleY,
    );
    smoothCurrentTimeDivisor = currentTimeDivisor;
    
    // Quick fallback visual speed calculation
    const availableSpaceBelow = screen.height - actualTimingLineY;
    const availableSpaceAbove = actualTimingLineY;
    const avgNoteDuration = 2 * baseTempo;
    const minTimeSpan = 1000 * Math.max(0.5, currentTimeDivisor);
    const requiredTimeSpanBelow = availableSpaceBelow / (avgNoteDuration * 0.5);
    const requiredTimeSpanAbove = availableSpaceAbove / (avgNoteDuration * 0.5);
    const requiredTimeSpan = Math.max(requiredTimeSpanBelow, requiredTimeSpanAbove) * avgNoteDuration;
    const timeSpan = Math.max(requiredTimeSpan, minTimeSpan);
    visualSpeed = Math.max(availableSpaceBelow, availableSpaceAbove) / timeSpan;
  }

  // UNIFIED APPROACH: Convert everything to tracks array format
  let tracks = [];

  if (melodyState.type === "single" && melodyState.notes) {
    tracks = [melodyState.notes];
  } else if (melodyState.type === "parallel" && melodyState.tracks) {
    tracks = melodyState.tracks;
  } else {
    return;
  }

  // Calculate track spacing and positioning
  let trackSpacing, startX;
  
  if (tracks.length === 1) {
    trackSpacing = 0;
    startX = screen.width / 4;
  } else {
    const availableWidth = screen.width * 0.9;
    const marginX = screen.width * 0.05;
    
    if (tracks.length === 2) {
      trackSpacing = availableWidth / 2;
      startX = marginX + (availableWidth / 4);
    } else {
      trackSpacing = availableWidth / tracks.length;
      startX = marginX + (trackSpacing / 2);
    }
  }

  // Constants for note flow - ensure gray notes reach bottom and black notes reach top
  const availableSpaceBelow = screen.height - actualTimingLineY;
  const availableSpaceAbove = actualTimingLineY;
  
  // Calculate timeSpan for note range calculation using smooth values
  let avgNoteDuration = 0;
  if (tracks.length > 0) {
    const firstTrack = tracks[0];
    if (firstTrack && firstTrack.length > 0) {
      const totalDuration = firstTrack.reduce((sum, note) => sum + (note.duration || 2), 0);
      // Use smooth time divisor for consistent animation
      const smoothBaseTempo = (smoothCurrentTimeDivisor * 1000) / 2;
      avgNoteDuration = (totalDuration / firstTrack.length) * smoothBaseTempo;
    }
  }
  
  // Ensure we have a reasonable fallback duration using smooth values
  if (avgNoteDuration === 0) {
    const smoothBaseTempo = (smoothCurrentTimeDivisor * 1000) / 2;
    avgNoteDuration = 2 * smoothBaseTempo; // Default to 2 units * smoothBaseTempo
  }
  
  // Calculate timeSpan for determining how many notes to show using smooth divisor
  const requiredTimeSpanBelow = availableSpaceBelow / (avgNoteDuration * 0.5);
  const requiredTimeSpanAbove = availableSpaceAbove / (avgNoteDuration * 0.5);
  const requiredTimeSpan = Math.max(requiredTimeSpanBelow, requiredTimeSpanAbove) * avgNoteDuration;
  const minTimeSpan = 1000 * Math.max(0.5, smoothCurrentTimeDivisor);
  const timeSpan = Math.max(requiredTimeSpan, minTimeSpan);
  
  // For backwards compatibility, still calculate dropDistance
  const dropDistance = availableSpaceBelow;
  const currentTimeMs = performance.now();

  // Draw flowing notes for each track
  tracks.forEach((track, trackIndex) => {
    if (!track || track.length === 0) return;

    const trackX = tracks.length === 1 ? startX : startX + trackIndex * trackSpacing;

    // Get current note index
    let currentNoteIndex = -1;
    if (hasFirstSynthFired) {
      if (melodyState.type === "single") {
        currentNoteIndex = getCurrentNoteIndex(melodyState);
      } else if (melodyState.type === "parallel" && melodyState.trackStates && melodyState.trackStates[trackIndex]) {
        currentNoteIndex = getCurrentNoteIndex(melodyState, trackIndex);
      }
    }

    if (currentNoteIndex < 0) return;

    // Calculate current note timing
    let noteStartTime = 0;
    let noteDuration = 0;
    let noteProgress = 0;

    if (melodyState.type === "single") {
      noteStartTime = currentNoteStartTime;
      noteDuration = currentNoteDuration;
      if (noteStartTime > 0 && noteDuration > 0) {
        const elapsed = currentTimeMs - noteStartTime;
        noteProgress = Math.max(0, Math.min(1, elapsed / noteDuration));
      }
    } else if (melodyState.type === "parallel" && melodyState.trackStates && melodyState.trackStates[trackIndex]) {
      const trackState = melodyState.trackStates[trackIndex];
      if (trackState && trackState.startTime > 0) {
        const currentNote = track[currentNoteIndex];
        if (currentNote && currentNote.duration) {
          noteStartTime = trackState.startTime;
          noteDuration = currentNote.duration * melodyState.baseTempo;
          const elapsed = currentTimeMs - noteStartTime;
          noteProgress = Math.max(0, Math.min(1, elapsed / noteDuration));
        }
      }
    }

    // Draw notes in a range around current position
    // Calculate how many notes we need to show based on timeSpan and average note duration
    const notesToShowBehind = Math.max(20, Math.ceil(timeSpan / avgNoteDuration) + 10); // Dynamic based on time span
    const notesToShowAhead = Math.max(20, Math.ceil(timeSpan / avgNoteDuration) + 10); // Dynamic based on time span

    // Track gray notes for debugging
    const grayNotes = [];

    for (let i = -notesToShowBehind; i <= notesToShowAhead; i++) {
      const noteIndex = (currentNoteIndex + i + track.length) % track.length;
      const note = track[noteIndex];
      if (!note || note.note === "rest") continue;

      let noteY, boxColor, textColor;

      if (i < 0) {
        // Past notes (below cyan line) - calculate cumulative duration from current note backwards
        let cumulativeDuration = noteProgress * noteDuration; // Time already elapsed in current note
        
        // Add durations of notes between current and this past note using smooth tempo
        for (let j = -1; j >= i; j--) {
          const pastNoteIndex = (currentNoteIndex + j + track.length) % track.length;
          const pastNote = track[pastNoteIndex];
          if (pastNote) {
            const smoothBaseTempo = (smoothCurrentTimeDivisor * 1000) / 2;
            cumulativeDuration += (pastNote.duration || 2) * smoothBaseTempo;
          }
        }
        
        noteY = actualTimingLineY + (cumulativeDuration * visualSpeed);
        boxColor = "gray";
        textColor = "brown";
        
        // Track gray note for debugging
        grayNotes.push({
          note: note.note,
          i: i,
          noteY: noteY,
          cumulativeDuration: cumulativeDuration,
          visualSpeed: visualSpeed,
          actualTimingLineY: actualTimingLineY,
          screenHeight: screen.height,
          isVisible: noteY >= -50 && noteY <= screen.height + 50,
          shouldReachBottom: cumulativeDuration >= timeSpan
        });
      } else if (i === 0) {
        // Current note (at/near cyan line with envelope)
        // Use the actual note progress and duration for consistent speed
        const currentNoteDistance = noteProgress * (note.duration * melodyState.baseTempo) * visualSpeed;
        noteY = actualTimingLineY + currentNoteDistance;
        
        // Red with envelope fading to gray
        if (noteProgress < 0.7) {
          boxColor = "red";
          textColor = "white";
        } else {
          // Fade from red to gray in the last 30% of the note
          boxColor = "gray";
          textColor = "brown";
        }
      } else {
        // Future notes (above cyan line) - calculate cumulative duration from current note forwards
        let cumulativeDuration = (1 - noteProgress) * noteDuration; // Remaining time in current note
        
        // Add durations of notes between current and this future note using smooth tempo
        for (let j = 1; j < i; j++) {
          const futureNoteIndex = (currentNoteIndex + j) % track.length;
          const futureNote = track[futureNoteIndex];
          if (futureNote) {
            const smoothBaseTempo = (smoothCurrentTimeDivisor * 1000) / 2;
            cumulativeDuration += (futureNote.duration || 2) * smoothBaseTempo;
          }
        }
        
        noteY = actualTimingLineY - (cumulativeDuration * visualSpeed);
        boxColor = "black";
        textColor = "white";
      }

      // Only draw if visible on screen
      if (noteY >= -50 && noteY <= screen.height + 50) {
        const noteBoxWidth = 20;
        const noteBoxHeight = 16;

        // Draw note box
        ink(boxColor).box(
          trackX - noteBoxWidth/2, 
          noteY - noteBoxHeight/2, 
          noteBoxWidth, 
          noteBoxHeight
        );

        // Draw note text
        const noteText = note.note.toUpperCase();
        ink(textColor).write(noteText, {
          x: trackX - 3,
          y: noteY - 4,
          scale: 1.2,
        });
      }
    }

    // Removed debug logging for production
  });
}

// 📚 Library

// Helper function to check if melody state has content
function hasMelodyContent(melodyState) {
  if (!melodyState) return false;

  if (melodyState.type === "single") {
    return melodyState.notes && melodyState.notes.length > 0;
  } else if (melodyState.type === "parallel") {
    return melodyState.tracks && melodyState.tracks.length > 0;
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
) {
  // Prevent new sounds from being created after leave() is called
  if (isLeavingPiece) {
    console.log(`🔇 Prevented sound creation after leave(): ${tone}`);
    return null;
  }

  // Add a proportional gap that scales inversely with duration - shorter notes need much bigger gaps
  let gapRatio;

  if (duration <= 100) {
    // Very short notes (like c....) need massive gaps to be audible - 90%
    gapRatio = 0.9;
  } else if (duration <= 200) {
    // Short notes (like c...) need huge gaps - 85%
    gapRatio = 0.85;
  } else if (duration <= 400) {
    // Medium-short notes (like c.) need big gaps - 70%
    gapRatio = 0.7;
  } else if (duration <= 800) {
    // Regular notes need moderate gaps - 30%
    gapRatio = 0.3;
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

// Smooth animation system - lerps flowing note positions for buttery smoothness
function updateFlowingNotesAnimation({ screen }) {
  // Calculate the actual target cyan timing line position
  const centerY = Math.round(screen.height / 2);
  const meterStartY = 0;
  const meterEndY = screen.height;
  const meterHeight = meterEndY - meterStartY;
  const minTiming = 0.05;
  const maxTiming = 3.0;
  const scaleY = meterHeight / (maxTiming - minTiming);
  const targetTimingLineY = Math.round(
    centerY + (currentTimeDivisor - 1.0) * scaleY,
  );

  // Calculate target visual speed
  const availableSpaceBelow = screen.height - targetTimingLineY;
  const availableSpaceAbove = targetTimingLineY;
  let avgNoteDuration = 2 * baseTempo; // Default fallback duration
  
  // Use cached values for stability during drag operations
  const minTimeSpan = 1000 * Math.max(0.5, currentTimeDivisor);
  let targetTimeSpan;
  
  if (flowingNotesCache.isDragging && flowingNotesCache.cacheValid) {
    targetTimeSpan = flowingNotesCache.cachedTimeSpan;
  } else {
    const requiredTimeSpanBelow = availableSpaceBelow / (avgNoteDuration * 0.5);
    const requiredTimeSpanAbove = availableSpaceAbove / (avgNoteDuration * 0.5);
    const requiredTimeSpan = Math.max(requiredTimeSpanBelow, requiredTimeSpanAbove) * avgNoteDuration;
    targetTimeSpan = Math.max(requiredTimeSpan, minTimeSpan);
  }
  
  const targetVisualSpeed = Math.max(availableSpaceBelow, availableSpaceAbove) / targetTimeSpan;

  // Initialize animation values on first run
  if (!flowingNotesAnimation.isInitialized) {
    flowingNotesAnimation.currentTimingLineY = targetTimingLineY;
    flowingNotesAnimation.currentVisualSpeed = targetVisualSpeed;
    flowingNotesAnimation.isInitialized = true;
  }

  // Smooth lerp to target values
  const lerpFactor = flowingNotesCache.isDragging 
    ? flowingNotesAnimation.lerpFactor * 0.5  // Slower lerp during dragging for stability
    : flowingNotesAnimation.lerpFactor;       // Normal lerp speed
    
  flowingNotesAnimation.currentTimingLineY = 
    flowingNotesAnimation.currentTimingLineY + 
    (targetTimingLineY - flowingNotesAnimation.currentTimingLineY) * lerpFactor;
    
  flowingNotesAnimation.currentVisualSpeed = 
    flowingNotesAnimation.currentVisualSpeed + 
    (targetVisualSpeed - flowingNotesAnimation.currentVisualSpeed) * lerpFactor;

  // Store lerped values for use in drawFlowingNotes
  flowingNotesAnimation.targetTimingLineY = targetTimingLineY;
  flowingNotesAnimation.targetVisualSpeed = targetVisualSpeed;
}

function act({ event: e, clock, sound: { synth }, screen, ui, typeface, api }) {
  // Handle mouse/touch interaction for timing adjustment
  if (e.is("draw")) {
    // Mark that we're dragging for animation stability
    flowingNotesCache.isDragging = true;
    
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
      const oldDivisor = currentTimeDivisor;

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

      // Recalculate nextNoteTargetTime to maintain flowing notes sync when timing changes
      if (nextNoteTargetTime > 0 && oldDivisor !== currentTimeDivisor) {
        const currentTimeMs = clock.time().getTime();
        const timeSinceLastTarget = currentTimeMs - nextNoteTargetTime;
        const oldBeatDuration = oldDivisor * 1000;
        const newBeatDuration = currentTimeDivisor * 1000;

        // Adjust the next note target time based on the new timing
        const beatProgress = timeSinceLastTarget / oldBeatDuration;
        nextNoteTargetTime = currentTimeMs - beatProgress * newBeatDuration;
      }

      // Also update parallel track timing if it exists
      if (
        melodyState &&
        melodyState.type === "parallel" &&
        melodyState.trackStates
      ) {
        melodyState.trackStates.forEach((trackState) => {
          if (trackState.nextNoteTargetTime > 0) {
            const currentTimeMs = clock.time().getTime();
            const timeSinceLastTarget =
              currentTimeMs - trackState.nextNoteTargetTime;
            const oldBeatDuration = oldDivisor * 1000;
            const newBeatDuration = currentTimeDivisor * 1000;

            // Adjust the next note target time based on the new timing
            const beatProgress = timeSinceLastTarget / oldBeatDuration;
            trackState.nextNoteTargetTime =
              currentTimeMs - beatProgress * newBeatDuration;
          }
        });
      }
    }

    // Stop any ongoing lerping when actively dragging
    isLerpingToSync = false;
  }

  // Reset any drag state when user stops dragging
  if (e.is("lift")) {
    // Mark dragging as finished and invalidate cache for smooth transition
    flowingNotesCache.isDragging = false;
    flowingNotesCache.cacheValid = false;
  }
  // Remove the lerp-back behavior - timing stays where user sets it

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

function sim({ sound, beep, clock, num, help, params, colon, screen }) {
  sound.speaker?.poll();

  // Update smooth animation system for flowing notes
  updateFlowingNotesAnimation({ screen });

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
          // Create managed sound that will be automatically released
          createManagedSound(
            sound,
            tone,
            waveType,
            noteDuration,
            volume || 0.8, // Use note volume or default to 0.8
            melodyState.isFallback,
            struck, // Pass the struck flag for note timing behavior
          );
          
          // Increment total notes played for persistent white note history
          totalNotesPlayed++;
        } catch (error) {
          console.error(
            `%c✗ ${tone} - ${error}`,
            "color: red; background: black; font-weight: bold; padding: 2px;",
          );
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

      // Track completed sequences for white note history
      if (oldIndex === melodyState.notes.length - 1 && melodyState.index === 0) {
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

  // Super simple melody timing - check against target time
  if (time && !wasInStandby) {
    // Skip timing checks on standby frames to prevent rapid-fire
    const currentTimeMs = time.getTime();
    let anyTrackPlayed = false; // Track whether any track played in this frame

    if (hasMelodyContent(melodyState)) {
      if (melodyState.type === "single") {
        // Single track timing
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
            // First note is audible - use UTC boundary for normal timing
            nextNoteTargetTime = Math.ceil(currentTimeMs / 1000) * 1000;
          }
        }

        // Check if it's time to play the next note
        if (nextNoteTargetTime > 0 && currentTimeMs >= nextNoteTargetTime) {
          bleep(time);
          anyTrackPlayed = true;

          // Calculate next note target time
          const currentNoteData = melodyState.notes[melodyState.index];
          if (currentNoteData) {
            const noteDuration =
              currentNoteData.duration * melodyState.baseTempo;
            nextNoteTargetTime = currentTimeMs + noteDuration;
          }
        }

        // Check for mutations during the gap between notes (after current note finishes, before next note starts)
        if (
          nextNoteTargetTime > 0 &&
          melodyState &&
          melodyState.notes &&
          melodyState.notes.length > 0
        ) {
          // Calculate when the current note should have finished
          const currentNoteData =
            melodyState.notes[
              (melodyState.index - 1 + melodyState.notes.length) %
                melodyState.notes.length
            ];

          if (currentNoteData && lastNoteStartTime > 0) {
            const currentNoteEndTime =
              lastNoteStartTime +
              currentNoteData.duration * melodyState.baseTempo;

            // (timing debug log removed)

            // Check if we're in the timing gap: current note has finished but next note hasn't started yet
            // Also check if we're exactly at the transition point (when noteEndTime === nextTargetTime)
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
                console.log(
                  `🎲 END-OF-TRACK MUTATION: previousNoteIndex=${previousNoteIndex}, lastNote=${melodyState.notes.length - 1}, lastMutationTriggered=${melodyState.lastMutationTriggered}`,
                );
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
        // Parallel tracks timing - handle each track independently
        melodyState.trackStates.forEach((trackState, trackIndex) => {
          if (!trackState.track || trackState.track.length === 0) return;

          // Initialize timing for this track if not set
          if (trackState.nextNoteTargetTime === 0) {
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
              // Skip ahead to first audible note
              trackState.noteIndex = firstAudibleIndex;
              trackState.nextNoteTargetTime = currentTimeMs;
            } else if (firstAudibleIndex >= trackState.track.length) {
              // All notes are rests - start immediately anyway
              trackState.noteIndex = 0;
              trackState.nextNoteTargetTime = currentTimeMs;
            } else {
              // First note is audible - use UTC boundary for normal timing
              trackState.nextNoteTargetTime =
                Math.ceil(currentTimeMs / 1000) * 1000;
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

                try {
                  createManagedSound(
                    sound,
                    tone,
                    waveType,
                    noteDuration,
                    volume || 0.8,
                    melodyState.isFallback,
                    struck, // Pass the struck flag for note timing behavior
                  );
                  
                  // Increment total notes played for persistent white note history
                  totalNotesPlayed++;
                } catch (error) {
                  console.error(
                    `%c✗ Track ${trackIndex + 1} ${tone} - ${error}`,
                    "color: red; background: black; font-weight: bold; padding: 2px;",
                  );
                }
              }

              anyTrackPlayed = true;
              
              // Update timing tracking for this track (for red note drop animation)
              trackState.startTime = performance.now();

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
                  // Apply mutation to this specific track
                  const originalTrack = [...trackState.track];
                  const mutatedTrack = mutateMelodyTrack(
                    trackState.track,
                    trackState.track.originalContent || trackState.track,
                    octave,
                  );

                  // Find which notes actually changed in this track
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
                      recentlyMutatedIndex = i; // This is the note that just changed
                    }
                  }

                  // Update this track with mutated notes
                  trackState.track = mutatedTrack;

                  // Preserve mutation metadata
                  trackState.track.hasMutation = true;
                  trackState.track.originalContent =
                    originalTrack.originalContent || originalTrack;
                  trackState.track.mutationCount =
                    (trackState.track.mutationCount || 0) + 1;

                  // Update the main melody state tracks array
                  melodyState.tracks[trackIndex] = trackState.track;

                  // Set mutation flash time for visual feedback (this should happen for any track mutation)
                  mutationFlashTime = performance.now();
                  triggeredAsteriskPositions = ["*"];

                  // Store mutated positions for this track (for potential visual feedback)
                  if (mutationIndices.length > 0) {
                    mutatedNotePositions = mutationIndices;
                    recentlyMutatedNoteIndex = recentlyMutatedIndex;
                    recentlyMutatedTrackIndex = trackIndex; // Make sure we track the correct track index
                  }
                }
              }

              // Calculate next note target time for this track
              const noteDuration = noteData.duration * melodyState.baseTempo;
              trackState.nextNoteTargetTime = currentTimeMs + noteDuration;
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
