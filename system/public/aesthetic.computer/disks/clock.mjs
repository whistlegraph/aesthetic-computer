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
let baseTempo = 500; // Default 500ms per quarter note (120 BPM) - used for visualization
let schedulingTempo = 500; // FIXED tempo for note scheduling - NEVER changes after boot
let playbackSpeedMultiplier = 1.0; // Speed multiplier for playback (1.0 = normal, 0.5 = half speed, 2.0 = double speed)
let melodyMode = "continuous"; // "continuous" or "sync" - how to time the melody
let targetTempo = 500; // Target tempo for smooth transitions
let isLerpingToSync = false; // Whether we're lerping back to sync
let originalTimeDivisor = 1.0; // Store the original time divisor for reference
let currentTimeDivisor = 1.0; // Current effective time divisor (changes with slider)
let targetTimeDivisor = 1.0; // Target divisor for lerping back to original
let utcTriggerDivisor = 1.0; // Divisor used for UTC triggering (stable during lerping)
let lerpSpeed = 0.25; // How fast to lerp timing changes (0.25 = 25% per frame for very fast response)
let isDraggingTiming = false; // Whether user is currently dragging the timing slider
let lastNoteTime = 0; // Time when the last note was triggered
let animationProgress = 0; // Progress towards next note (0 to 1)
let specialCharFlashTime = 0; // Time when special character was processed (for green flash)
let mutationFlashTime = 0; // Time when a mutation occurred (for asterisk flash)
let mutatedNotePositions = []; // Positions of notes that were mutated (for rainbow flash)
let triggeredAsteriskPositions = []; // Positions of asterisks that were just triggered (for white flash)
let recentlyMutatedNoteIndex = -1; // Index of the most recently mutated note (for rainbow flash)
let recentlyMutatedTrackIndex = -1; // Track index of the most recently mutated note (for parallel tracks)
let currentNoteStartTime = 0; // When the current red note started playing

// Advanced smoothing system for ultra-smooth animation
let smoothedVisualTimeDivisor = 1.0; // Smoothed version for visual calculations
let smoothedVisualTempo = 500; // Smoothed version for visual calculations
let smoothedVisualSpeed = 0.048; // Smoothed version of visual speed for consistent animation (48px / 1000ms)
let smoothedTimeSpan = 2750; // Increased to show 5.5 notes for 1s timing (5.5 * 500ms)

// Advanced smoothing state for critically damped spring system
let smoothingVelocity = 0; // Velocity of smoothing changes
let smoothingAcceleration = 0; // Acceleration for spring system
let previousTimeDivisor = 1.0; // Previous frame's divisor for velocity calculation
let lastSmoothingTime = 0; // Previous frame's time for delta calculation
let frameHistory = []; // Multi-frame averaging history
const MAX_FRAME_HISTORY = 8; // Number of frames to average for ultra-smooth motion
let momentumFactor = 0.95; // Momentum preservation factor (0.95 = 95% momentum retention)
let currentNoteDuration = 0; // Duration of the current red note
let hasFirstSynthFired = false; // Flag to track when first synth plays (for syntax coloring)
let noteFlowMode = "flow"; // "stop" or "flow" - whether red note stops at cyan line or flows past it
let totalNotesPlayed = 0; // Track total notes played across all repetitions for persistent white note history
let completedSequences = 0; // Track how many complete melody sequences have been played

// Sound management similar to notepat
const activeSounds = {}; // Track active synth instances for proper timing control
const scheduledNotes = []; // Queue of notes to be released at specific times
let isLeavingPiece = false; // Flag to prevent new sounds from being created after leave() is called

// UI Buttons for octave control
let octaveMinusBtn, octavePlusBtn;

// Helper function to check if melody state has content to display
function hasMelodyContent(melodyState) {
  if (!melodyState) return false;
  
  if (melodyState.type === "single") {
    return melodyState.notes && melodyState.notes.length > 0;
  } else if (melodyState.type === "parallel") {
    return melodyState.tracks && melodyState.tracks.length > 0 && 
           melodyState.tracks.some(track => track && track.length > 0);
  }
  
  return false;
}

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
  schedulingTempo = 500; // FIXED tempo - never changes after boot to prevent timing discontinuities
  playbackSpeedMultiplier = timeDivisor; // Use divisor as speed multiplier (1.0 = normal, 0.5 = half speed, etc.)
  targetTempo = baseTempo; // Set target tempo to initial value
  isLerpingToSync = false; // Reset lerping state
  
  // Initialize smoothed visual parameters
  smoothedVisualTimeDivisor = timeDivisor;
  smoothedVisualTempo = baseTempo;
  smoothedVisualSpeed = 0.048; // Start with reasonable visual speed (48px / 1000ms)
  smoothedTimeSpan = 2750; // Increased to show 5.5 notes for 1s timing (5.5 * 500ms)

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

      // Debug logging for melody state
      console.log(`🎵 Boot: Created single track melody state with ${parsedMelody.length} notes`);
      console.log(`🎵 Boot: First note:`, parsedMelody[0]);
      console.log(`🎵 Boot: Melody state:`, melodyState);

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
          // NEW: Absolute timing to prevent drift
          absoluteStartTime: 0, // Absolute UTC time when this track started
          beatCount: 0, // How many beats have elapsed since start
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

    // If user is dragging timing, also show the target timing line
    if (isDraggingTiming && Math.abs(targetTimeDivisor - currentTimeDivisor) > 0.01) {
      const targetYPos = Math.round(
        centerY + (targetTimeDivisor - 1.0) * scaleY,
      );
      // Draw target line in lighter cyan/white as preview
      ink("white").line(0, targetYPos, screen.width, targetYPos);
    }

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
      try {
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

      return noteCharPositions;
    } catch (error) {
      console.error("Error in calculateNotePositions:", error);
      return [];
    }
  }

  function drawFlowingNotes(melodyState) {
    if (!melodyState || !melodyState.tracks) {
      console.log("Notes on screen: 0");
      return;
    }

    // Convert single track to array format
    const tracks = Array.isArray(melodyState.tracks) ? melodyState.tracks : [melodyState.tracks];
    
    let notesOnScreen = 0;
    const currentTime = performance.now();
    
    // Draw notes for each track
    tracks.forEach((track, trackIndex) => {
      if (!track || !track.notes || !Array.isArray(track.notes)) return;
      
      // Calculate track color
      const trackColor = trackIndex === 0 ? [255, 255, 255] : 
                        trackIndex === 1 ? [255, 100, 100] :
                        trackIndex === 2 ? [100, 255, 100] :
                        [100, 100, 255];
      
      // Calculate playback position
      const playbackTime = melodyState.isPlaying ? 
        (currentTime - melodyState.startTime) * melodyState.playbackSpeed : 0;
      
      // Draw each note in the track
      track.notes.forEach((note, noteIndex) => {
        if (!note || typeof note.start !== 'number' || typeof note.duration !== 'number') return;
        
        // Calculate note timing
        const noteStart = note.start * 1000; // Convert to milliseconds
        const noteEnd = noteStart + (note.duration * 1000);
        
        // Check if note should be visible (within a time window)
        const lookAhead = 2000; // 2 seconds ahead
        const lookBehind = 1000; // 1 second behind
        
        if (noteStart > playbackTime + lookAhead || noteEnd < playbackTime - lookBehind) {
          return; // Note is outside visible time window
        }
        
        // Calculate note position
        const noteProgress = (playbackTime - noteStart) / (noteEnd - noteStart);
        const isActive = noteProgress >= 0 && noteProgress <= 1;
        
        // Calculate screen position
        const screenWidth = api.screen.width;
        const screenHeight = api.screen.height;
        
        // Note flows from right to left
        const noteX = screenWidth - ((playbackTime - noteStart + lookAhead) / (lookAhead + lookBehind)) * screenWidth;
        const noteY = screenHeight * 0.3 + (trackIndex * 40) + (noteIndex % 12) * 20;
        
        // Note dimensions
        const noteWidth = Math.max(20, (note.duration * 1000 / 200)); // Scale with duration
        const noteHeight = 15;
        
        // Set note color and opacity
        let noteR = trackColor[0];
        let noteG = trackColor[1];  
        let noteB = trackColor[2];
        let alpha = isActive ? 255 : 128;
        
        // Highlight currently playing note
        if (isActive) {
          noteR = Math.min(255, noteR + 50);
          noteG = Math.min(255, noteG + 50);
          noteB = Math.min(255, noteB + 50);
        }
        
        // Draw the note
        if (noteX + noteWidth > 0 && noteX < screenWidth) {
          api.ink([noteR, noteG, noteB, alpha]);
          api.box(noteX, noteY, noteWidth, noteHeight);
          
          // Draw note name if available
          if (note.note && noteWidth > 30) {
            api.ink([255, 255, 255, alpha]);
            api.write(note.note, noteX + 2, noteY + 2);
          }
          
          notesOnScreen++;
        }
      });
    });
    
    console.log("Notes on screen:", notesOnScreen);
  }
