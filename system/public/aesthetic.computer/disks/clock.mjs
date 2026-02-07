// Clock, 2025.6.27.12.00
// Just a standard clock with melody support, UTC sync, and live keyboard playing.

// TODO: - [] Reduce code size.
//       - [] Adjust swipe syncing and check utc syncing. 

/* 🐢 About
  Usage:
    clock melody_string               - Continuous melody playback with 1s timing
    clock melody_string               - Melody synced to UTC clock ticks
    clock melody_string

  Base octave is 4 unless first note specifies octave (e.g., 5cdefg uses octave 5)
  Use Up/Down arrow keys to change base octave during playback
  
  🎵 NEW: Notepat Keyboard Notation Support!
  Use notepat keyboard characters for compact sharp and octave notation:
    v s w r q - Sharp notes (c# d# f# g# a#) in current octave
    h i j k l m n - White notes (c d e f g a b) in next octave up
    t y u o p - Sharp notes (c# d# f# g# a#) in next octave up
  Examples:
    clock cvd - Plays C, C#, D (using v for C#)
    clock cdefghijk - Plays full chromatic scale across two octaves
    clock vswrq - Plays all sharps in current octave
  
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

  🎵 NEW: Sequential Sections (Song Structure)!
  Use > to separate sections that play in sequence:
    ceg dfa > abc def - Two sections: first plays ceg+dfa, then abc+def, loops
    cdefg 3> abcde 2> gabcd - Loop counts: section 1 plays 3x, section 2 plays 2x, section 3 plays 1x
    {square} ceg > {triangle} dfa - Different waveforms per section
  Examples:
    clock ceg fab > aba bag - First section has 2 parallel tracks, then second section
    clock intro 2> verse 4> chorus - Intro plays 2x, verse 4x, chorus 1x, then loops

  🎵 NEW: Waveform Type & Volume Support!
  Use {type} or {type:volume} or {volume} syntax:
    {sine} cdefg - Sine wave (default)
    {square} cdefg - Square wave
    {sawtooth} cdefg - Sawtooth wave  
    {triangle} cdefg - Triangle wave
    {noise-white} cdefg - White noise
    {sample} cdefg - Sample playback (alias for stample)
    {stample} cdefg - Stample playback (uses sample recorded in stample piece)
    {custom} cdefg - Custom waveform generation
    {bubble} cdefg - Physical bubble modeling (frequency controls bubble size)
    {0.5} cdefg - Set volume to 50% (keeps current waveform)
    {square:0.3} cdefg - Square wave at 30% volume
    {0.8} c{0.2}d{1.0}e - Volume changes during melody
    {square} cdefg - Square wave for all tracks
    ceg {triangle:0.6} dfa - Sine for first track, triangle at 60% for second

  🎵 NEW: Hz Pitch Shift Support!
  Use {hz} syntax for micro pitch adjustments:
    {100hz} cdefg - Shift all notes up by 100 Hz
    {-50hz} cdefg - Shift all notes down by 50 Hz
    c{100hz}d{-25hz}e{0hz}f - Individual note shifts
    {50hz} ceg {-30hz} dfa - Different shifts per track
    {50hz&} cdefg - Cumulative shift: +50Hz each cycle (+50, +100, +150...)
    The Hz shift is sticky and persists until changed
    Use & modifier for animated cumulative effects that build over time
  
  🎵 NEW: Visual Separators!
  Use ~ or | for visual organization without affecting playback:
    {noise-white} cdefg~abcd~efgh - Visual segments with preserved mode
    cdef|gabc|defg - Measure bars for visual organization
    Sticky values like {noise-white} persist across separators
  
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

  🎵 NEW: Swing Timing!
  Use [ for early/rushed and ] for late/laid back timing:
    c[defg[[ab - d is early, a b are very early
    c]d]]e - d is late, e is very late
    Each symbol = 1/16 beat offset (small, musical adjustments)

  🎵 NEW: Speech Synthesis Support!
  Use quoted text for speech synthesis with random voice selection:
    cde"hi"gab - Plays c, d, e, speaks "hi", then plays g, a, b
    "hello world"defg - Speaks "hello world", then plays d, e, f, g
    c"one"d"two"e - Plays c, speaks "one", plays d, speaks "two", plays e
    {square} c"beep"d - Square wave notes with speech in between
    Uses random male/female voices with prosody variation like textfence

  Examples:
    clock cdefg               - Play in octave 4 with 1s timing
    clock 5cdefg              - Play in octave 5 (determined by first note)
    clock cdefg:0.25          - Fast playback at 0.25s timing in octave 4
    clock 5d.d.edgf#-:2       - Play at 2s timing with octave 5
    clock c[defg[[ab:0.5      - Play with swing timing at 0.5s
    clock {square} cdefg      - Square wave melody
    clock c... defg           - Very short staccato notes (sticky duration)
    clock c,, defg            - Long sustained notes (sticky duration)
    clock ^cdefg              - Struck notes with natural decay (piano-like)
    clock {100hz}ceg{50hz}abag{0hz}bab - Hz pitch shift example
    clock ceg dfa             - Parallel melodies: c+d, e+f, g+a
    clock {triangle} cd ef ga - Three parallel tracks with triangle wave
    clock ceg dfa > abc def   - Sequential sections (first plays, then second, loops)
    clock verse 4> chorus 2>  - Verse plays 4x, chorus plays 2x, then loops
    clock {#abc123} cdefg     - Load painting code as stample sample
    clock {#abc123} ceg {#xyz789} dfa - Different stamples per parallel track
*/

console.log("🕰️ CLOCK MODULE LOADING at", Date.now());

import {
  parseMelody,
  extractTones,
  parseSimultaneousMelody,
  parseSequentialMelody,
  mutateMelodyTrack,
  noteToTone,
  extractStampleCodes,
  extractSayTexts,
} from "../lib/melody-parser.mjs";
import { convertNotepatNotation } from "../lib/notepat-convert.mjs";
import { getNoteColor } from "../lib/note-colors.mjs";

// Utility function for padding numbers with zeros
function pad(num, size = 2) {
  num = num.toString();
  while (num.length < size) num = "0" + num;
  return num;
}

// Convert notepat keyboard notation to standard notation
// Notepat conversion helper imported above.

// Convert internal note representation (like 'cs', 'ds') back to notepat notation
function convertToNotepatDisplay(note, octave, baseOctave = 4) {
  if (!note || note === 'rest') return '_';
  
  note = note.toLowerCase();
  
  // Determine if this is in the base octave or one octave higher
  const isHigherOctave = octave > baseOctave;
  
  // Map internal notes to notepat characters
  const notepatMap = {
    // Base octave
    'c': 'c',
    'cs': 'v',
    'd': 'd',
    'ds': 's',
    'e': 'e',
    'f': 'f',
    'fs': 'w',
    'g': 'g',
    'gs': 'r',
    'a': 'a',
    'as': 'q',
    'b': 'b',
    // Higher octave
    'c+': 'h',
    'cs+': 't',
    'd+': 'i',
    'ds+': 'y',
    'e+': 'j',
    'f+': 'k',
    'fs+': 'u',
    'g+': 'l',
    'gs+': 'o',
    'a+': 'm',
    'as+': 'p',
    'b+': 'n',
  };
  
  const key = isHigherOctave ? note + '+' : note;
  return notepatMap[key] || note; // Fallback to original if not in map
}

let synced = false;
let sequence = [],
  sequenceIndex = 0;
let parsedMelody = [];
let octave = 4;
let originalMelodyString = ""; // Store the original melody string for reparsing
let cachedClockCode = null; // Short code for QR display after caching to API
let cachedClockAuthor = null; // Author handle for byline display (e.g., "@handle" or null for anon)
let octaveFlashTime = 0; // Track when octave changed for flash effect

// Loading state system - wait for samples before playback
let loadingState = {
  isLoading: false,
  samplesNeeded: 0,
  samplesLoaded: 0,
  readyToPlay: true, // Start true for melodies without samples
  loadStartTime: 0,
  pendingPromises: [],
};

// Enhanced melody state for simultaneous tracks
let melodyTracks = null; // Will store parsed simultaneous tracks
let melodyState = null;
let baseTempo = 500; // Default 500ms per quarter note (120 BPM)
let maxTrackCountSeen = 1; // Track the highest track count across all sequences (for history persistence)

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

// Cumulative Hz shift tracking for animated notation
// Support for multiple cumulative states (per track)
let cumulativeHzStates = new Map(); // trackId -> cumulativeState
let globalCumulativeInterval = 0; // Shared timing interval for all cumulative effects

// Helper function to calculate final frequency with Hz shift and apply lower bound
function calculateFrequencyWithLowerBound(tone, hzShift = 0, sound, trackId = null) {
  try {
    // Get the base frequency for the note using the freq function
    const baseFrequency = sound.freq(tone);
    if (!baseFrequency) {
      console.warn(`🎵 Could not calculate frequency for tone: ${tone}`);
      return { frequency: null, wasReset: false, toneShift: hzShift };
    }
    
    // Debug logging for frequency calculation
    // console.log(`🎵 FREQ DEBUG: tone="${tone}" baseFreq=${baseFrequency.toFixed(2)}Hz hzShift=${hzShift}Hz`);
    
    // Apply Hz shift
    const shiftedFrequency = baseFrequency + hzShift;
    
    // console.log(`🎵 FREQ DEBUG: shiftedFreq=${shiftedFrequency.toFixed(2)}Hz`);
    
    // Check lower bound (20 Hz minimum)
    const minFrequency = 20;
    if (shiftedFrequency < minFrequency) {
      console.log(`🎵 Frequency ${shiftedFrequency.toFixed(2)}Hz below minimum (${minFrequency}Hz) for ${tone}, resetting to base frequency (${baseFrequency.toFixed(2)}Hz)`);
      
      // If this was caused by cumulative Hz shift, reset the cumulative state
      if (trackId !== null && hzShift !== 0) {
        const state = getCumulativeState(trackId);
        if (state.enabled && state.current === hzShift) {
          console.log(`🎵 Track ${trackId + 1}: Resetting cumulative Hz state due to frequency lower bound`);
          state.current = 0;
          state.cycles = 0;
          state.enabled = false;
        }
      }
      
      return { 
        frequency: baseFrequency, 
        wasReset: true, 
        toneShift: 0, // Reset the shift to 0 when frequency is reset
        originalShift: hzShift 
      };
    }
    
    return { 
      frequency: shiftedFrequency, 
      wasReset: false, 
      toneShift: hzShift 
    };
  } catch (error) {
    console.warn(`🎵 Error calculating frequency for ${tone}:`, error);
    return { frequency: null, wasReset: false, toneShift: hzShift };
  }
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

// Helper function to create speech utterance with SSML prosody (similar to textfence)
function createSpeechUtterance(text, voiceType = "female", num) {
  let rate, pitch;
  if (voiceType === "female") {
    rate = `${num.randIntRange(90, 105)}%`;
    pitch = `+${num.randIntRange(10, 35)}%`;
  } else {
    rate = `${num.randIntRange(80, 105)}%`;
    pitch = `${num.randIntRange(-15, 0)}%`;
  }
  return `
  <speak>
    <prosody rate="${rate}" pitch="${pitch}">${text}</prosody>
  </speak>
  `;
}

// Speak text with SFX system for instant playback (cached speech)
function speakText(text, voice = "female:18", options = {}, num, speak) {
  const speechLabel = `speech:${voice} - ${text}`;
  
  // Always use the speak function which handles caching internally
  // The speech system will use cached audio if available
  console.log(`🗣️ Playing speech: "${text}" with ${voice} voice`);
  
  const utterance = createSpeechUtterance(text, voice.split(":")[0], num);
  speak(utterance, voice, "cloud", {
    volume: options.volume || 1,
    pan: options.pan || 0,
    skipCompleted: true,
    ...options
  });
}

// Helper function to preload all speech in a melody for instant playback
function preloadMelodySpeech(melodyState, speak, num, help) {
  if (!melodyState || !speak) return;
  
  speechEnabled = false;
  speechCache.clear(); // Clear previous cache entries
  const speechToCache = [];
  
  // Collect all speech notes from all tracks
  if (melodyState.type === "single" && melodyState.notes) {
    melodyState.notes.forEach(note => {
      if (note.isSpeech && note.text) {
        speechToCache.push(note.text);
      }
    });
  } else if (melodyState.tracks) {
    melodyState.tracks.forEach(track => {
      if (track && track.length > 0) {
        track.forEach(note => {
          if (note.isSpeech && note.text) {
            speechToCache.push(note.text);
          }
        });
      }
    });
  }
  
  // Remove duplicates
  const uniqueSpeechTexts = [...new Set(speechToCache)];
  
  if (uniqueSpeechTexts.length === 0) {
    speechEnabled = false;
    return;
  }
  
  console.log(`🗣️ Preloading ${uniqueSpeechTexts.length} speech samples for instant playback...`);
  
  // Generate speech for both male and female voices for variety
  uniqueSpeechTexts.forEach(text => {
    ['female', 'male'].forEach(voiceType => {
      const voiceNumber = voiceType === "female" ? 18 : 22;
      const voiceString = `${voiceType}:${voiceNumber}`;
      const speechLabel = `speech:${voiceString} - ${text}`;
      
      // Pre-generate speech with silent playback to populate the cache
      console.log(`🗣️ Pre-generating speech: "${text}" (${voiceString})`);
      
      const utterance = createSpeechUtterance(text, voiceType, num);
      speak(utterance, voiceString, "cloud", {
        volume: 0, // Silent generation for caching only
        pan: 0,
        skipCompleted: true // Don't send completion events during caching
      });
      
      // Mark as cached in our tracking (the actual caching is handled by speech system)
      speechCache.add(speechLabel);
    });
  });
  
  // Enable speech immediately since we've requested all samples
  speechEnabled = true;
  console.log(`🗣️ Speech enabled! Requested ${uniqueSpeechTexts.length * 2} speech samples for caching.`);
}

// Preload say texts for {say} waveform type (pitched speech samples)
// Returns an array of promises that resolve when all samples are loaded
async function preloadSayTexts(melodyState, speak, num) {
  if (!melodyState || !speak) return [];
  
  const sayTexts = extractSayTexts(melodyState);
  
  if (sayTexts.size === 0) return [];
  
  console.log(`🗣️ Preloading ${sayTexts.size} say samples for {say} waveform...`);
  
  const loadPromises = [];
  
  sayTexts.forEach(text => {
    // Skip if already queued
    if (sayCacheQueue.has(text)) return;
    sayCacheQueue.add(text);
    
    // Use preloadOnly mode to get a promise that resolves when cached
    const loadPromise = speak(text, "female:18", "cloud", {
      preloadOnly: true, // Returns promise, resolves when cached, doesn't play
    });
    
    if (loadPromise) {
      // Wrap to update loading state when done
      const trackedPromise = loadPromise.then((label) => {
        loadingState.samplesLoaded++;
        console.log(`🗣️ Loaded say text: "${text}" (${loadingState.samplesLoaded}/${loadingState.samplesNeeded})`);
        return label;
      });
      loadPromises.push(trackedPromise);
      console.log(`🗣️ Preloading say text: "${text}"`);
    }
  });
  
  return loadPromises;
}

// Helper functions for managing per-track cumulative Hz states
function getCumulativeState(trackId) {
  if (!cumulativeHzStates.has(trackId)) {
    cumulativeHzStates.set(trackId, {
      step: 0,
      current: 0,
      cycles: 0,
      enabled: false,
      lastIncrementTime: 0
    });
  }
  return cumulativeHzStates.get(trackId);
}

function initializeCumulativeState(trackId, step, intervalMs) {
  const state = getCumulativeState(trackId);
  state.step = step;
  state.current = step; // Start with the first increment
  state.cycles = 1;
  state.enabled = true;
  state.lastIncrementTime = performance.now();
  globalCumulativeInterval = intervalMs; // All tracks share the same interval
  console.log(`🎵 Track ${trackId + 1}: Starting cumulative Hz mode: ${step}Hz& (interval: ${intervalMs}ms)`);
}

function updateCumulativeState(trackId, currentTime) {
  const state = getCumulativeState(trackId);
  if (!state.enabled || globalCumulativeInterval <= 0) return state.current;
  
  const timeSinceLastIncrement = currentTime - state.lastIncrementTime;
  if (timeSinceLastIncrement >= globalCumulativeInterval) {
    state.cycles++;
    state.current = state.step * state.cycles;
    
    // Lower bound check: if Hz goes below 0, reset to default (0)
    if (state.current < 0) {
      console.log(`🎵 Track ${trackId + 1}: Hz shift went below 0 (${state.current}Hz), resetting to default (0Hz)`);
      state.current = 0;
      state.cycles = 0;
      state.enabled = false; // Disable cumulative mode after reset
    }
    
    state.lastIncrementTime = currentTime;
    console.log(`🎵 Track ${trackId + 1} Cycle ${state.cycles}: Hz shift now ${state.current >= 0 ? '+' : ''}${state.current}Hz`);
  }
  
  return state.current;
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

// Stample sample support (like toss and notepat)
let stampleSampleId = null;
let stampleSampleData = null;
let stampleSampleRate = null;
let fallbackSfx = null; // Fallback sound if no stample recorded

// Stample code cache for {#code} painting references
// Maps painting code -> { sampleId, data, sampleRate, loaded, loading, error }
const stampleCache = new Map();

// Module-level references for lazy loading (set in boot)
let netPreload = null;
let soundRef = null;

// UI Buttons for octave control
let octaveMinusBtn, octavePlusBtn;

// Visual logging throttling
let lastLogTime = 0;
let lastLoggedNoteCount = 0;
const LOG_THROTTLE_MS = 1000; // Only log once per second maximum

// Floating Hz display system
let floatingHzDisplays = []; // Array of floating frequency displays
const FLOATING_HZ_DURATION = 2000; // How long each display lasts (2 seconds)
const FLOATING_HZ_SPEED = 30; // How fast they float up (pixels per second)
const FLOATING_HZ_FADE_START = 0.7; // When to start fading (70% through duration)

// Speech caching system for instant playback
let speechEnabled = false; // Flag to indicate if speech is available in the melody
let speechCache = new Set(); // Track which speech samples are cached as SFX

// Say waveform caching - tracks which say texts have been queued for caching
const sayCacheQueue = new Set();
// References needed for say waveform caching
let speakRef = null;
let numRef = null;

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

// Function to add a floating Hz display
function addFloatingHzDisplay(frequency, noteString, trackIndex = 0, screen) {
  const now = performance.now();
  
  // Calculate starting position - offset based on track index to avoid overlap
  const baseX = 10; // Start near left edge
  const offsetX = trackIndex * 80; // Spread tracks horizontally
  const startX = Math.min(baseX + offsetX, screen.width - 100); // Keep within screen bounds
  const startY = 30; // Start below HUD area
  
  const display = {
    frequency: Math.round(frequency * 10) / 10, // Round to 1 decimal place
    noteString: noteString,
    trackIndex: trackIndex,
    startTime: now,
    x: startX,
    y: startY,
    startY: startY,
    alpha: 1.0
  };
  
  floatingHzDisplays.push(display);
  
  // Limit number of concurrent displays to prevent overflow
  if (floatingHzDisplays.length > 20) {
    floatingHzDisplays.splice(0, floatingHzDisplays.length - 20);
  }
}

// Function to draw and update floating Hz displays
function drawFloatingHzDisplays(ink, write, screen) {
  const now = performance.now();
  
  // Update and render each floating display
  for (let i = floatingHzDisplays.length - 1; i >= 0; i--) {
    const display = floatingHzDisplays[i];
    const elapsed = now - display.startTime;
    const progress = elapsed / FLOATING_HZ_DURATION;
    
    // Remove expired displays
    if (progress >= 1.0) {
      floatingHzDisplays.splice(i, 1);
      continue;
    }
    
    // Calculate position (float upward)
    const floatDistance = elapsed * (FLOATING_HZ_SPEED / 1000); // Convert to pixels per ms
    display.y = display.startY - floatDistance;
    
    // Calculate alpha (fade out near the end)
    if (progress > FLOATING_HZ_FADE_START) {
      const fadeProgress = (progress - FLOATING_HZ_FADE_START) / (1.0 - FLOATING_HZ_FADE_START);
      display.alpha = 1.0 - fadeProgress;
    } else {
      display.alpha = 1.0;
    }
    
    // Create text string
    const hzText = `${display.noteString}: ${display.frequency}Hz`;
    
    // Calculate color based on track index
    const trackColors = [
      [255, 100, 100], // Red for track 1
      [100, 255, 100], // Green for track 2  
      [100, 100, 255], // Blue for track 3
      [255, 255, 100], // Yellow for track 4
      [255, 100, 255], // Magenta for track 5
      [100, 255, 255], // Cyan for track 6
    ];
    
    const baseColor = trackColors[display.trackIndex % trackColors.length];
    const alpha = Math.round(display.alpha * 255);
    const color = [
      Math.round(baseColor[0] * display.alpha),
      Math.round(baseColor[1] * display.alpha),
      Math.round(baseColor[2] * display.alpha),
      alpha
    ];
    
    // Skip rendering if too transparent
    if (display.alpha < 0.1) continue;
    
    // Draw shadow first (slightly offset, in black)
    ink([0, 0, 0, Math.round(alpha * 0.7)]).write(hzText, {
      x: display.x + 1,
      y: display.y + 1,
      scale: 1,
    });
    
    // Draw main text
    ink(color).write(hzText, {
      x: display.x,
      y: display.y,
      scale: 1,
    });
  }
}

// Cache melody to store-clock API and get pronounceable shortcode for QR
async function cacheMelody(melody) {
  if (!melody || melody === "cdefgab") return null; // Don't cache default/fallback
  
  try {
    const response = await fetch('/api/store-clock', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ source: melody }), // Use 'source' for consistency with KidLisp
      credentials: 'include' // Include auth cookies so logged-in user gets attributed
    });
    
    if (!response.ok) {
      console.warn('🎵 Failed to cache clock melody:', response.status);
      return null;
    }
    
    const data = await response.json();
    console.log(`🎵 Clock melody cached: *${data.code} (${data.cached ? 'existing' : 'new'})`);
    return data.code;
  } catch (err) {
    console.warn('🎵 Error caching clock melody:', err.message);
    return null;
  }
}

// RGB decoding: 3 samples per pixel (R, G, B channels) - adapted from stample.mjs
function decodeBitmapToSample(bitmap, meta) {
  if (!bitmap?.pixels?.length || !bitmap?.width || !bitmap?.height) return null;
  const totalPixels = bitmap.width * bitmap.height;
  const samplesPerPixel = 3; // R, G, B
  const maxSamples = totalPixels * samplesPerPixel;
  const sampleLength = Math.min(meta?.sampleLength || maxSamples, maxSamples);
  const data = new Array(sampleLength);

  for (let i = 0; i < sampleLength; i += 1) {
    const pixelIndex = Math.floor(i / samplesPerPixel);
    const channel = i % samplesPerPixel; // 0=R, 1=G, 2=B
    const byte = bitmap.pixels[pixelIndex * 4 + channel] || 0;
    data[i] = byte / 127.5 - 1;
  }

  return data;
}

// Convert an image to a pixel buffer - adapted from stample.mjs
async function imageToBuffer(image) {
  if (!image) return null;
  const source = image.img || image.bitmap || image;

  if (source?.pixels && source?.width && source?.height) {
    return {
      width: source.width,
      height: source.height,
      pixels: new Uint8ClampedArray(source.pixels),
    };
  }

  if (source?.data && source?.width && source?.height) {
    return {
      width: source.width,
      height: source.height,
      pixels: new Uint8ClampedArray(source.data),
    };
  }

  const width = source.width || source.naturalWidth || source.videoWidth;
  const height = source.height || source.naturalHeight || source.videoHeight;
  if (!width || !height) return null;

  let canvas;
  if (typeof OffscreenCanvas !== "undefined") {
    canvas = new OffscreenCanvas(width, height);
  } else if (typeof document !== "undefined") {
    canvas = document.createElement("canvas");
    canvas.width = width;
    canvas.height = height;
  }
  if (!canvas) return null;

  const ctx = canvas.getContext("2d");
  if (!ctx) return null;
  ctx.clearRect(0, 0, width, height);
  ctx.drawImage(source, 0, 0, width, height);
  const imageData = ctx.getImageData(0, 0, width, height);

  return {
    width,
    height,
    pixels: new Uint8ClampedArray(imageData.data),
  };
}

// Load a painting code as a stample sample (lazy loading)
async function loadStampleCode(code, { preload, sound }) {
  if (!code) return null;
  
  // Check if already loaded or loading
  const existing = stampleCache.get(code);
  if (existing?.loaded) {
    return existing;
  }
  if (existing?.loading) {
    // Wait for ongoing load to complete
    return new Promise((resolve) => {
      const checkInterval = setInterval(() => {
        const cached = stampleCache.get(code);
        if (cached && !cached.loading) {
          clearInterval(checkInterval);
          resolve(cached);
        }
      }, 50);
    });
  }
  
  // Mark as loading
  stampleCache.set(code, { loading: true, loaded: false });
  
  const baseUrl = typeof location !== "undefined" && location.origin
    ? location.origin
    : "https://aesthetic.computer";
  
  try {
    // Fast path: short code direct media endpoint
    const directUrl = `${baseUrl}/media/paintings/${code}.png?t=${Date.now()}`;
    console.log(`🎵 Clock: Loading stample #${code} from ${directUrl}`);
    
    const img = await preload(directUrl, true);
    const buffer = await imageToBuffer(img);
    
    if (!buffer?.pixels?.length) {
      throw new Error("Failed to decode painting image");
    }
    
    // Decode bitmap to audio
    const totalPixels = buffer.width * buffer.height;
    const meta = {
      sampleLength: totalPixels * 3, // RGB = 3 samples per pixel
      sampleRate: 48000,
    };
    
    const decoded = decodeBitmapToSample(buffer, meta);
    if (!decoded?.length) {
      throw new Error("Failed to decode bitmap to audio samples");
    }
    
    const sampleId = `stample:${code}`;
    sound?.registerSample?.(sampleId, decoded, meta.sampleRate);
    
    const result = {
      sampleId,
      data: decoded,
      sampleRate: meta.sampleRate,
      loaded: true,
      loading: false,
    };
    
    stampleCache.set(code, result);
    console.log(`🎵 Clock: Loaded stample #${code} (${decoded.length} samples)`);
    
    return result;
  } catch (err) {
    console.warn(`🎵 Clock: Failed to load stample #${code}:`, err.message);
    stampleCache.set(code, { loading: false, loaded: false, error: err.message });
    return null;
  }
}

// Get a stample by code (returns cached or triggers load)
function getStampleByCode(code, { preload, sound }) {
  const cached = stampleCache.get(code);
  if (cached?.loaded) {
    return cached.sampleId;
  }
  
  // Trigger lazy load if not already loading
  if (!cached?.loading) {
    loadStampleCode(code, { preload, sound });
  }
  
  return null; // Not yet loaded, will be available on next note
}

async function boot({ ui, clock, params, colon, hud, screen, typeface, api, speak, num, help, store, net, sound }) {
  try {
    console.log(`🎵 CLOCK BOOT STARTED - params:`, params, `colon:`, colon);
  
    // Reset leaving flag when piece starts
    isLeavingPiece = false;
    
    // Clear stample cache on new boot
    stampleCache.clear();
    
    // Store references for lazy loading
    netPreload = net?.preload;
    soundRef = sound;
    
    // Store speak and num references for say waveform caching
    speakRef = speak;
    numRef = num;

    // Preload fallback sound for stample mode (like toss and notepat do)
    if (net?.preload) {
      net.preload("startup")
        .then((sfx) => {
          fallbackSfx = sfx;
          console.log("🎵 Clock: Loaded fallback sfx:", sfx);
        })
        .catch((err) => console.warn("🎵 Clock: Failed to load fallback sfx:", err));
    }

    // Load stample sample from store (like toss and notepat do)
    stampleSampleId = null;
    stampleSampleData = null;
    stampleSampleRate = null;

    if (store?.retrieve) {
      (async () => {
        try {
          const storedSample =
            store["stample:sample"] ||
            (await store.retrieve("stample:sample", "local:db"));
          if (storedSample?.data?.length) {
            const storedId = storedSample.id || "stample";
            stampleSampleId = storedId;
            stampleSampleData = storedSample.data;
            stampleSampleRate = storedSample.sampleRate;
            sound?.registerSample?.(storedId, storedSample.data, storedSample.sampleRate);
            console.log("🎵 Clock loaded stample sample:", storedId, storedSample.data.length, "samples");
          } else {
            console.log("🎵 Clock: No stample sample found in store (record one in `stample` piece first)");
          }
        } catch (err) {
          console.warn("🎵 Clock: Failed to load stample sample:", err);
        }
      })();
    }

    // Reset cached clock code and author
    cachedClockCode = null;
    cachedClockAuthor = null;

  // Reset total notes played counter for fresh start
  totalNotesPlayed = 0;

  // Reset completed sequences counter
  completedSequences = 0;

  // Reset floating Hz displays
  floatingHzDisplays = [];

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

  // Reset cumulative Hz states when starting a new melody
  cumulativeHzStates.clear();
  
  // Clear speech preloading state for new melody
  speechEnabled = false;
  
  // Initialize an empty melody state immediately
  // This prevents "No melody state" warnings if sim() runs before async boot completes
  // Start with empty notes - will be populated from params/API if provided
  melodyState = {
    notes: [],
    index: 0,
    baseTempo: baseTempo,
    isPlaying: false,
    startTime: performance.now(),
    timingMode: parseFloat(colon[0]) || 1.0,
    type: "single",
    isFallback: true,
    isEmpty: true, // Track that this is an empty/silent clock
  };
  console.log("🎵 EARLY MELODY STATE SET (empty):", melodyState?.type, melodyState?.notes?.length, "notes");

  // Determine the melody string from params or API
  let isFallbackMelody = false;
  
  // Check if params[0] is a cached code (starts with *)
  // If so, fetch the melody from the store-clock API
  if (params[0] && params[0].startsWith("*")) {
    const code = params[0].slice(1); // Remove * prefix
    console.log(`🎵 Fetching cached melody for code: *${code}`);
    
    try {
      const response = await fetch(
        `/api/store-clock?code=${encodeURIComponent(code)}`
      );
      
      if (response.ok) {
        const data = await response.json();
        if (data.source) {
          console.log(`🎵 Loaded cached melody: ${data.source}`);
          // Set the code immediately since we already have it
          cachedClockCode = code;
          // Store the author handle for byline display
          cachedClockAuthor = data.handle || null;
          console.log(`🎵 Clock author: ${cachedClockAuthor || 'anon'}`);
          api.send({ type: "clock:cached", content: { code: cachedClockCode, author: cachedClockAuthor } });
          
          // Use the fetched melody as if it was passed directly
          originalMelodyString = data.source;
        } else {
          console.warn(`⚠️ Clock code *${code} not found in store`);
          originalMelodyString = "cdefgab"; // Default melody
          isFallbackMelody = true;
        }
      } else {
        console.warn(`⚠️ Failed to fetch clock code *${code}: ${response.status}`);
        originalMelodyString = "cdefgab"; // Default melody
        isFallbackMelody = true;
      }
    } catch (err) {
      console.error(`🔴 Error fetching clock code *${code}:`, err);
      originalMelodyString = "cdefgab"; // Default melody
      isFallbackMelody = true;
    }
  } else if (params[0]) {
    // Concatenate all params to handle cases like clock/(ceg) (dfa) where
    // (ceg) is in params[0] and (dfa) is in params[1]
    originalMelodyString = params.join(" ");
    console.log(`🎵 PARAMS BRANCH: originalMelodyString = "${originalMelodyString}"`);
  } else {
    // No melody provided - empty/silent clock
    originalMelodyString = "";
    isFallbackMelody = true;
    console.log(`🎵 EMPTY CLOCK: silent mode, no melody`);
  }

  console.log(`🎵 About to process melody: "${originalMelodyString}", isFallback: ${isFallbackMelody}`);
  
  // Handle empty clock - no melody, just black screen and silence
  if (!originalMelodyString || originalMelodyString.trim() === "") {
    console.log(`🎵 Empty clock - silent mode`);
    melodyState = {
      notes: [],
      index: 0,
      baseTempo: baseTempo,
      isPlaying: false,
      startTime: performance.now(),
      timingMode: parseFloat(colon[0]) || 1.0,
      type: "single",
      isFallback: true,
      isEmpty: true,
    };
    parsedMelody = [];
    sequence = [];
    // Skip all melody processing
  } else
  // Process the melody string (same for API-fetched, params, or fallback)
  {

    // Convert notepat notation to standard notation before parsing
    const convertedMelodyString = convertNotepatNotation(originalMelodyString, octave);
    console.log(`🎵 Original: "${originalMelodyString}"`);
    console.log(`🎵 Converted: "${convertedMelodyString}"`);
    
    // Parse the melody string - first try sequential (with > separators), falls back to simultaneous
    melodyTracks = parseSequentialMelody(convertedMelodyString, octave);
    console.log(`🎵 Parsed melodyTracks:`, melodyTracks?.type, `tracks:`, melodyTracks?.tracks?.length, `sequences:`, melodyTracks?.sequences?.length);

    // Handle sequential melodies (sections separated by >)
    if (melodyTracks.type === 'sequential') {
      console.log(`🎵 Sequential melody detected with ${melodyTracks.sequences.length} sections`);
      
      // Get the first sequence to start with
      const firstSequence = melodyTracks.sequences[0];
      
      // Initialize state for sequential playback
      melodyState = {
        type: 'sequential',
        sequences: melodyTracks.sequences,
        currentSequence: 0,
        totalSequences: melodyTracks.sequences.length,
        baseTempo: baseTempo,
        isPlaying: false,
        startTime: performance.now(),
        timingMode: parseFloat(colon[0]) || 1.0,
        isFallback: isFallbackMelody,
        // Current sequence state (will be updated as sequences advance)
        currentSequenceState: null, // Will be initialized when playback starts
      };
      
      // Initialize the first sequence's playback state
      initializeSequenceState(melodyState, 0, baseTempo, isFallbackMelody, colon);
      
      // For backward compatibility, set parsedMelody to the first track of the first sequence
      const firstSeqTracks = firstSequence.tracks || [firstSequence.notes];
      parsedMelody = firstSeqTracks[0] || [];
      sequence = extractTones(parsedMelody, {
        skipRests: false,
        restTone: `${octave}G`,
      });
      
      console.log(`🎵 Sequential melody initialized: ${melodyTracks.sequences.length} sequences, first sequence has ${firstSeqTracks.length} tracks`);
    }
    else if (melodyTracks.isSingleTrack) {
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
        isFallback: isFallbackMelody,
      };
      console.log(`🎵 MelodyState set (single track):`, melodyState.notes?.length, `notes, type:`, melodyState.type);

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
        isFallback: isFallbackMelody,
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
        isFallback: isFallbackMelody,
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
  
  // Preload speech for instant playback if speak function is available
  if (speak && melodyState) {
    preloadMelodySpeech(melodyState, speak, num, help);
    
    // Preload {say} waveform texts and wait for them to load
    const sayLoadPromises = await preloadSayTexts(melodyState, speak, num);
    
    if (sayLoadPromises.length > 0) {
      loadingState.isLoading = true;
      loadingState.readyToPlay = false;
      loadingState.samplesNeeded = sayLoadPromises.length;
      loadingState.samplesLoaded = 0;
      loadingState.loadStartTime = performance.now();
      loadingState.pendingPromises = sayLoadPromises;
      
      console.log(`🗣️ Waiting for ${sayLoadPromises.length} say samples to load...`);
      
      // Wait for all samples to finish loading
      await Promise.all(sayLoadPromises);
      
      loadingState.isLoading = false;
      loadingState.readyToPlay = true;
      console.log(`🗣️ All say samples loaded! Ready to play.`);
    }
  }

  // Cache melody to API for QR shortcode (fire and forget, don't block boot)
  // Only cache if:
  // 1. We have a real melody (not fallback)
  // 2. We didn't already get a code from API fetch (cachedClockCode would be set)
  if (!isFallbackMelody && !cachedClockCode && originalMelodyString) {
    cacheMelody(originalMelodyString).then(code => {
      if (code) {
        cachedClockCode = code;
        // Notify disk.mjs that we have a cached code for QR display
        if (typeof api?.send === 'function') {
          api.send({ type: "clock:cached", content: { code: cachedClockCode } });
        }
      }
    });
  }
  } catch (bootError) {
    console.error("🔴 CLOCK BOOT ERROR:", bootError);
    throw bootError; // Re-throw so the disk system knows
  }
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

  // Black background for empty clock, gray for active melody
  const isEmpty = melodyState?.isEmpty || (melodyState?.notes?.length === 0 && !originalMelodyString);
  wipe(isEmpty ? "black" : "gray");

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

    // Paint octave control buttons (only if typeface is loaded)
    if (typeface?.glyphs?.["0"]?.resolution) {
      const octaveText = `${octave}`;
      const glyphWidth = typeface.glyphs["0"].resolution[0];
      const padding = 4; // Padding on left and right of octave number
      const octaveTextWidth = octaveText.length * glyphWidth + padding * 2;
      const buttonWidth = 16;
      const buttonHeight = 16;
      const totalWidth = buttonWidth + octaveTextWidth + buttonWidth;
      const startX = 0; // Bottom-left corner (moved from right for QR code space)
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
    }

    // --- HUD preview clock label in the corner ---
    if (typeof hud !== "undefined" && hud.label) {
      // Check if melody timing has started (based on whether first synth has fired)
      const melodyTimingStarted = hasFirstSynthFired;

      // Show loading indicator if samples are still loading
      if (!loadingState.readyToPlay && loadingState.isLoading) {
        const loadProgress = loadingState.samplesNeeded > 0 
          ? `${loadingState.samplesLoaded}/${loadingState.samplesNeeded}`
          : "...";
        const loadingStr = `\\yellow\\clock \\white\\Loading samples ${loadProgress}`;
        hud.supportsInlineColor = true;
        hud.label(loadingStr, undefined, 0, `clock Loading samples ${loadProgress}`);
        return; // Skip the rest of the HUD rendering while loading
      }

      // Provide "clock" prefix plus melody content - let disk.mjs handle all coloring
      let previewStringDecorative = "";
      let previewStringPlain = "";
      if (originalMelodyString && originalMelodyString.trim().length > 0) {
        // Always show colored syntax highlighting when there's melody content
        const currentMelodyString = buildCurrentMelodyString(
          originalMelodyString,
          melodyState,
        );
        const coloredMelody =
          coloredMelodyStringForTimeline ||
          buildColoredMelodyStringUnified(currentMelodyString, melodyState);

          // Add cumulative Hz display for all active tracks
          let hzDisplay = "";
          if (cumulativeHzStates.size > 0) {
            const displays = [];
            for (const [trackIndex, state] of cumulativeHzStates) {
              if (state.enabled) {
                const hzColor = state.current >= 0 ? "green" : "red";
                const hzSign = state.current >= 0 ? "+" : "";
                displays.push(`\\${hzColor}\\[${hzSign}${state.current}Hz]`);
              }
            }
            if (displays.length > 0) {
              hzDisplay = ` ${displays.join(" ")}`;
            }
          }
          
          // Ensure the space after "clock" doesn't get colored by the melody colors
          // Note: Sequence indicator moved to footer (next to octave controls)
          previewStringDecorative = `\\white\\clock \\white\\` + coloredMelody + hzDisplay;
        
        // ALWAYS use original melody string for plain text (backspace preservation)
        // This ensures that backspace always returns to the original melody, not mutated state
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
        // First set shadow (stripped version)
        hud.label(shadowString, undefined, 0, previewStringPlain); // shadow
        hud.supportsInlineColor = true;
        hud.disablePieceNameColoring = true; // Let our custom colors override piece name coloring
        // Force cache invalidation for animated color changes by updating frame hash
        hud.frameHash = performance.now();
        // Then set colored version for live syntax highlighting
        hud.label(previewStringDecorative, undefined, 0, previewStringPlain); // colored
      }
      // The HUD label calls above pass previewStringPlain as 4th argument for backspace preservation
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

      // For sequential melodies, get the effective melodyState (currentSequenceState)
      let effectiveMelodyState = melodyState;
      let currentSequenceIndex = 0; // Default to first sequence
      let isSequentialMelody = melodyState && melodyState.type === "sequential";
      
      if (isSequentialMelody) {
        // Use currentSequence if set, otherwise default to 0
        currentSequenceIndex = melodyState.currentSequence ?? 0;
        if (melodyState.currentSequenceState) {
          effectiveMelodyState = melodyState.currentSequenceState;
        } else if (melodyState.sequences && melodyState.sequences.length > 0) {
          // Fall back to first sequence's parsed state
          effectiveMelodyState = melodyState.sequences[0].parsed;
        }
      }

      // Use the unified function to get current note index
      if (timingHasStarted && effectiveMelodyState && effectiveMelodyState.type === "single") {
        currentNoteIndex = getCurrentNoteIndex(effectiveMelodyState);
      } else if (
        timingHasStarted &&
        effectiveMelodyState &&
        effectiveMelodyState.type === "parallel"
      ) {
        // For parallel, handled below per track
      } else {
        // If timing hasn't started, set currentNoteIndex to -1 so no note is highlighted
        currentNoteIndex = -1;
      }

      // For sequential melodies, split by > first to identify sequences
      let sequences = [];
      let sequenceDelimiterPositions = []; // Track positions of > in the string
      
      if (isSequentialMelody) {
        // Find all > positions and split into sequences
        let lastSplitPos = 0;
        for (let i = 0; i < melodyString.length; i++) {
          if (melodyString[i] === '>') {
            sequences.push(melodyString.substring(lastSplitPos, i).trim());
            sequenceDelimiterPositions.push(i);
            lastSplitPos = i + 1;
          }
        }
        sequences.push(melodyString.substring(lastSplitPos).trim());
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

      // For sequential melodies, we need to map groups to tracks within the current sequence
      // Filter out sequence delimiter groups (just ">", "2>", "3>", etc.)
      let effectiveGroupToTrackMap = []; // Maps group index to { sequenceIndex, trackIndex }
      let currentSeqIdx = 0;
      let currentTrackInSeq = 0;
      
      if (isSequentialMelody) {
        for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
          const group = groups[groupIdx];
          // Check if this is a sequence delimiter (>, 2>, 3>, etc.)
          if (/^\d*>$/.test(group)) {
            // This is a delimiter, advance sequence
            currentSeqIdx++;
            currentTrackInSeq = 0;
            effectiveGroupToTrackMap.push({ isDelimiter: true, sequenceIndex: currentSeqIdx - 1 });
          } else {
            // This is a track in the current sequence
            effectiveGroupToTrackMap.push({ 
              isDelimiter: false, 
              sequenceIndex: currentSeqIdx, 
              trackIndex: currentTrackInSeq,
              isCurrentSequence: currentSeqIdx === currentSequenceIndex
            });
            currentTrackInSeq++;
          }
        }
      }

      // First pass: identify all note character positions and their duration modifiers
      for (let groupIdx = 0; groupIdx < groups.length; groupIdx++) {
        const group = groups[groupIdx];
        const groupStartChar = groupStartPositions[groupIdx];
        noteIndex = 0; // Reset note index for each group
        
        // For sequential melodies, get the track index mapping
        let trackIdxForColoring = groupIdx; // Default for non-sequential
        let isInCurrentSequence = true;
        
        if (isSequentialMelody && effectiveGroupToTrackMap[groupIdx]) {
          const mapping = effectiveGroupToTrackMap[groupIdx];
          if (mapping.isDelimiter) {
            // Skip delimiter groups - they have no notes
            continue;
          }
          trackIdxForColoring = mapping.trackIndex;
          isInCurrentSequence = mapping.isCurrentSequence;
        }

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
          // Also handle notepat characters after octave numbers
          if (
            /[0-9]/.test(char) &&
            i + 1 < group.length &&
            /[a-gvswrqhijklmntyuop]/i.test(group[i + 1])
          ) {
            let noteStart = i; // Start with the octave number
            let noteEnd = i + 1; // Move to the note letter

            // Check for sharp modifiers
            if (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (nextChar === "#") {
                noteEnd++;
              }
            }

            // Check for duration modifiers and asterisks
            while (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (
                nextChar === "." ||
                nextChar === "-" ||
                nextChar === "[" ||
                nextChar === "]" ||
                nextChar === "," ||
                nextChar === "*"
              ) {
                noteEnd++;
              } else {
                break;
              }
            }

            const noteIndexToUse = noteIndex;
            // For sequential melodies, use the mapped track index; for parallel, use group index; for single, use 0
            const trackIndexToUse = isSequentialMelody ? trackIdxForColoring :
              (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
            for (let j = noteStart; j <= noteEnd; j++) {
              noteCharPositions.push({
                charIndex: groupStartChar + j,
                noteIndex: noteIndexToUse,
                trackIndex: trackIndexToUse,
                isInCurrentSequence: isInCurrentSequence,
              });
            }
            noteIndex++;
            i = noteEnd;
          }
          // Handle struck mode toggle (^) - NOT a timing unit, just skip it
          else if (char === "^") {
            // Skip ^ characters, they don't count as notes or timing units
            continue;
          }
          // Handle standalone dashes (-) - NOT a timing unit for syntax highlighting
          // (Only - with a note like -c counts, standalone --- doesn't)
          else if (char === "-") {
            // Check if this is a series of dashes that leads to a note
            let dashEnd = i;
            while (dashEnd + 1 < group.length && group[dashEnd + 1] === "-") {
              dashEnd++;
            }
            // Check if followed by a note letter (relative octave modifier like ---c)
            if (
              dashEnd + 1 < group.length &&
              /[a-gvswrqhijklmntyuop]/i.test(group[dashEnd + 1])
            ) {
              // This is a relative octave modifier, treat as part of a note
              let noteStart = i;
              let noteEnd = dashEnd + 1;
              
              // Check for sharp modifiers
              if (noteEnd + 1 < group.length && group[noteEnd + 1] === "#") {
                noteEnd++;
              }
              
              // Check for duration modifiers
              while (noteEnd + 1 < group.length) {
                const nextChar = group[noteEnd + 1];
                if (nextChar === "." || nextChar === "," || nextChar === "*" || 
                    nextChar === "[" || nextChar === "]") {
                  noteEnd++;
                } else {
                  break;
                }
              }
              
              const noteIndexToUse = noteIndex;
              const trackIndexToUse = isSequentialMelody ? trackIdxForColoring :
                (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
              for (let j = noteStart; j <= noteEnd; j++) {
                noteCharPositions.push({
                  charIndex: groupStartChar + j,
                  noteIndex: noteIndexToUse,
                  trackIndex: trackIndexToUse,
                  isInCurrentSequence: isInCurrentSequence,
                });
              }
              noteIndex++;
              i = noteEnd;
            } else {
              // Standalone dashes - skip them, they're not timing units
              i = dashEnd;
              continue;
            }
          }
          // Handle + relative octave modifier (must be followed by a note)
          else if (char === "+") {
            let plusEnd = i;
            while (plusEnd + 1 < group.length && group[plusEnd + 1] === "+") {
              plusEnd++;
            }
            // Check if followed by a note letter
            if (
              plusEnd + 1 < group.length &&
              /[a-gvswrqhijklmntyuop]/i.test(group[plusEnd + 1])
            ) {
              // This is a relative octave modifier, treat as part of a note
              let noteStart = i;
              let noteEnd = plusEnd + 1;
              
              // Check for sharp modifiers
              if (noteEnd + 1 < group.length && group[noteEnd + 1] === "#") {
                noteEnd++;
              }
              
              // Check for duration modifiers
              while (noteEnd + 1 < group.length) {
                const nextChar = group[noteEnd + 1];
                if (nextChar === "." || nextChar === "," || nextChar === "*" || 
                    nextChar === "[" || nextChar === "]") {
                  noteEnd++;
                } else {
                  break;
                }
              }
              
              const noteIndexToUse = noteIndex;
              const trackIndexToUse = isSequentialMelody ? trackIdxForColoring :
                (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
              for (let j = noteStart; j <= noteEnd; j++) {
                noteCharPositions.push({
                  charIndex: groupStartChar + j,
                  noteIndex: noteIndexToUse,
                  trackIndex: trackIndexToUse,
                  isInCurrentSequence: isInCurrentSequence,
                });
              }
              noteIndex++;
              i = noteEnd;
            } else {
              // Standalone + without note - skip
              i = plusEnd;
              continue;
            }
          }
          // Handle regular note letters (without octave prefix)
          // Also include notepat characters: vswrq (sharps), hijklmn (white notes), tyuop (sharps in higher octave)
          else if (/[a-g#_vswrqhijklmntyuop]/i.test(char)) {
            let noteStart = i;
            let noteEnd = i;

            // Check for sharp modifiers (for regular notes or after relative modifiers)
            if (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (nextChar === "#") {
                noteEnd++;
              }
            }

            // Check for duration modifiers and asterisks
            while (noteEnd + 1 < group.length) {
              const nextChar = group[noteEnd + 1];
              if (
                nextChar === "." ||
                nextChar === "-" ||
                nextChar === "[" ||
                nextChar === "]" ||
                nextChar === "," ||
                nextChar === "*"
              ) {
                noteEnd++;
              } else {
                break;
              }
            }

            const noteIndexToUse = noteIndex;
            // For sequential melodies, use the mapped track index; for parallel, use group index; for single, use 0
            const trackIndexToUse = isSequentialMelody ? trackIdxForColoring :
              (effectiveMelodyState && effectiveMelodyState.type === "parallel" ? groupIdx : 0);
            for (let j = noteStart; j <= noteEnd; j++) {
              noteCharPositions.push({
                charIndex: groupStartChar + j,
                noteIndex: noteIndexToUse,
                trackIndex: trackIndexToUse,
                isInCurrentSequence: isInCurrentSequence,
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

      // Helper function to get note color for static display (before timing)
      function getStaticNoteColor(noteCharData) {
        if (!noteCharData) {
          return "gray";
        }
        
        // Use effectiveMelodyState for sequential melodies
        const stateToUse = effectiveMelodyState || melodyState;
        if (!stateToUse) return "gray";
        
        let note = null;
        if (stateToUse.type === "single" && stateToUse.notes) {
          note = stateToUse.notes[noteCharData.noteIndex];
        } else if (stateToUse.type === "parallel" && stateToUse.tracks) {
          const track = stateToUse.tracks[noteCharData.trackIndex];
          note = track && track[noteCharData.noteIndex];
        }
        
        if (!note) {
          return "gray";
        }
        
        // Get the RGB color for this note
        const rgb = getNoteColor(note.note);
        const colorString = `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
        return colorString;
      }

      // Helper function to check if a note is mutated
      function isNoteMutated(noteCharData) {
        if (!noteCharData) return false;

        // Use effectiveMelodyState for sequential melodies
        const stateToUse = effectiveMelodyState || melodyState;
        if (!stateToUse) return false;

        if (stateToUse.type === "single") {
          const currentNote = stateToUse.notes && stateToUse.notes[noteCharData.noteIndex];
          return currentNote && currentNote.isMutation;
        } else if (
          stateToUse.type === "parallel" &&
          stateToUse.trackStates &&
          noteCharData.trackIndex < stateToUse.trackStates.length
        ) {
          const track = stateToUse.tracks[noteCharData.trackIndex];
          const currentNote = track && track[noteCharData.noteIndex];
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
      let currentSequenceInString = 0; // Track which sequence we're in for dimming future sequences

      for (let i = 0; i < melodyString.length; i++) {
        const char = melodyString[i];
        let color = "yellow";
        
        // Handle sequence separator `>` for sequential melodies
        if (char === ">") {
          // Check if this is a sequence separator (not preceded by a digit which would make it part of a loop count)
          const prevChar = i > 0 ? melodyString[i - 1] : " ";
          const nextChar = i + 1 < melodyString.length ? melodyString[i + 1] : " ";
          
          // If preceded by a digit, it's a loop count like "3>" - color the > white
          // Otherwise it's a standalone > separator
          if (/\d/.test(prevChar)) {
            color = "white"; // Loop count indicator
          } else {
            color = "cyan"; // Sequence separator
          }
          currentSequenceInString++; // Advance to next sequence
          coloredMelodyString += `\\${color}\\${char}`;
          continue;
        }
        
        // Handle loop count digits before `>` (like "3>" meaning loop 3 times)
        if (/\d/.test(char)) {
          // Check if this digit is followed by > (loop count)
          let lookAhead = i + 1;
          while (lookAhead < melodyString.length && /\d/.test(melodyString[lookAhead])) {
            lookAhead++;
          }
          if (lookAhead < melodyString.length && melodyString[lookAhead] === ">") {
            // This is a loop count number - color it magenta
            color = "magenta";
            coloredMelodyString += `\\${color}\\${char}`;
            continue;
          }
        }

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
              } else if (
                melodyState &&
                melodyState.type === "sequential" &&
                melodyState.currentSequenceState
              ) {
                // For sequential melodies, only highlight notes in the current sequence
                // AND only if this note is the currently playing one
                if (noteCharData.isInCurrentSequence) {
                  const seqState = melodyState.currentSequenceState;
                  if (seqState.type === "single" && seqState.notes) {
                    // Single track within sequence
                    const totalNotes = seqState.notes.length;
                    const currentPlayingIndex = (seqState.index - 1 + totalNotes) % totalNotes;
                    isCurrentlyPlayingNote = noteCharData.noteIndex === currentPlayingIndex;
                  } else if (seqState.type === "parallel" && seqState.trackStates) {
                    // Parallel tracks within sequence
                    const trackState = seqState.trackStates[noteCharData.trackIndex];
                    if (trackState && trackState.track) {
                      const totalNotes = trackState.track.length;
                      const currentPlayingIndex = (trackState.noteIndex - 1 + totalNotes) % totalNotes;
                      isCurrentlyPlayingNote = noteCharData.noteIndex === currentPlayingIndex;
                    }
                  }
                }
              }
            }

            if (char === "{") {
              inWaveformForColoring = true;
              color = shouldFlashGreen ? "green" : "yellow"; // Always show yellow for braces
            } else if (char === "}") {
              inWaveformForColoring = false;
              color = shouldFlashGreen ? "green" : "yellow"; // Always show yellow for braces
            } else if (inWaveformForColoring) {
              // Check if we're inside a stample code block {#code}
              // Look back to find the opening { and check for #
              let lookBack = i - 1;
              let waveContent = "";
              while (lookBack >= 0 && melodyString[lookBack] !== "{") {
                waveContent = melodyString[lookBack] + waveContent;
                lookBack--;
              }
              waveContent = waveContent + char;
              
              // Check if this waveform starts with # (painting code)
              if (waveContent.startsWith("#") || (lookBack >= 0 && melodyString[lookBack + 1] === "#")) {
                // Extract the code (everything after #)
                const hashIdx = waveContent.indexOf("#");
                const code = waveContent.substring(hashIdx + 1).replace(/[^a-zA-Z0-9]/g, "");
                
                // Check cache status for coloring
                const cached = stampleCache.get(code);
                if (cached && cached.loaded) {
                  color = "lime"; // Loaded - bright green
                } else if (cached && cached.loading) {
                  color = "cyan"; // Loading - cyan
                } else if (cached && cached.error) {
                  color = [255, 100, 100]; // Error - light red
                } else {
                  color = "orange"; // Not started yet
                }
              } else {
                color = "cyan"; // Regular waveform content (sine, square, stample, etc.)
              }
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
                    // Use note-specific colors if timing hasn't started, yellow if it has
                    color = timingHasStarted ? "yellow" : getStaticNoteColor(noteCharData);
                  }
                }
              } else {
                // For characters not identified as notes, keep the basic color scheme  
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
                  } else if (/[0-9+\-#\[\]]/i.test(char)) {
                    color = "green"; // Other special characters in current note flash green (changed <> to [] for swing)
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
        
        // Apply dimming for notes not in current sequence (for sequential melodies)
        // Use noteCharData.isInCurrentSequence which is set during parsing
        const noteCharData2 = noteCharPositions.find(ncp => ncp.charIndex === i);
        const shouldDim = isSequentialMelody && noteCharData2 && noteCharData2.isInCurrentSequence === false;
        
        if (shouldDim && color !== "cyan" && color !== "magenta" && color !== "white") {
          // Dim the color to indicate non-current sequence
          if (typeof color === "string") {
            color = "gray"; // Dim all non-current sequence notes to gray
          } else if (Array.isArray(color)) {
            // Dim RGB values
            color = [Math.round(color[0] * 0.3), Math.round(color[1] * 0.3), Math.round(color[2] * 0.3)];
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

  // Draw floating Hz displays
  drawFloatingHzDisplays(ink, write, screen);

  // Paint octave control buttons after timeline so they appear on top
  if (typeface?.glyphs?.["0"]?.resolution) {
    const octaveText = `${octave}`;
    const glyphWidth = typeface.glyphs["0"].resolution[0];
    const padding = 4; // Padding on left and right of octave number
    const octaveTextWidth = octaveText.length * glyphWidth + padding * 2;
    const buttonWidth = 16;
    const buttonHeight = 16;
    const totalWidth = buttonWidth + octaveTextWidth + buttonWidth;
    const startX = 0; // Bottom-left corner (moved from right for QR code space)
    const tapingOffset = api.system?.taping ? 1 : 0; // Move up 1 pixel when taping
    const startY = screen.height - buttonHeight - tapingOffset;

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

    // Paint sequence indicator for sequential melodies (next to octave controls)
    if (melodyState && melodyState.type === "sequential" && melodyState.sequences) {
      const currentSeq = melodyState.sequences[melodyState.currentSequence];
      const seqIndex = melodyState.currentSequence;
      const totalSeqs = melodyState.sequences.length;
      const loopNum = (currentSeq?.currentLoop || 0);
      const totalLoops = currentSeq?.loopCount || 1;
      
      // Position to the right of the octave buttons
      const seqX = startX + totalWidth + 4;
      const seqY = startY;
      
      // Visual block-based indicator - SQUARE blocks
      const blockSize = buttonHeight; // Square: same width and height
      const blockGap = 2;
      
      // Calculate progress through current section
      // Get current note index and total notes in this section
      let currentNoteInSection = 0;
      let totalNotesInSection = 1;
      
      if (melodyState.currentSequenceState) {
        const seqState = melodyState.currentSequenceState;
        if (seqState.type === "single" && seqState.notes) {
          currentNoteInSection = seqState.index || 0;
          totalNotesInSection = seqState.notes.length || 1;
        } else if (seqState.type === "parallel" && seqState.trackStates) {
          // For parallel, use the longest track's progress
          const maxTrackLen = Math.max(...seqState.trackStates.map(ts => ts.track?.length || 0));
          const maxNoteIdx = Math.max(...seqState.trackStates.map(ts => ts.noteIndex || 0));
          currentNoteInSection = maxNoteIdx;
          totalNotesInSection = maxTrackLen || 1;
        }
      }
      
      // Calculate overall progress including loops
      // Progress = (completedLoops * totalNotes + currentNote) / (totalLoops * totalNotes)
      const totalProgress = (loopNum * totalNotesInSection + currentNoteInSection) / (totalLoops * totalNotesInSection);
      
      // Draw each sequence as a block
      for (let i = 0; i < totalSeqs; i++) {
        const blockX = seqX + i * (blockSize + blockGap);
        const isActive = i === seqIndex;
        const isPast = i < seqIndex;
        
        // Block background
        ink(isPast ? "gray" : (isActive ? "darkblue" : "darkgray")).box(blockX, seqY, blockSize, blockSize);
        
        // Fill for completed sections
        if (isPast) {
          ink("cyan").box(blockX, seqY, blockSize, blockSize);
        }
        
        // Progress fill for active section
        if (isActive) {
          const fillWidth = Math.round(totalProgress * blockSize);
          ink("cyan").box(blockX, seqY, fillWidth, blockSize);
        }
      }
    }
  }

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

  // Draw author byline after melody string (only for cached codes)
  if (cachedClockCode) {
    const bylineText = cachedClockAuthor ? `by ${cachedClockAuthor}` : "by anon";
    const melodyEndX = startX + stringWidth + 8; // 8px gap after melody
    ink(cachedClockAuthor ? [100, 180, 255] : [128, 128, 128]).write(bylineText, {
      x: melodyEndX,
      y: timelineY,
      scale: 1,
    });
  }

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
  
  // Show speech caching status if speech notes are present
  const hasSpeechNotes = (melodyState && 
    ((melodyState.type === "single" && melodyState.notes && melodyState.notes.some(note => note.isSpeech)) ||
     (melodyState.tracks && melodyState.tracks.some(track => track.some && track.some(note => note.isSpeech)))));
  
  if (hasSpeechNotes) {
    const speechStatusText = speechEnabled ? "🗣️ ready" : "🗣️ loading...";
    const speechStatusColor = speechEnabled ? "green" : "yellow";
    const speechStatusX = timelineStartX + timingText.length * 6 + 12; // Position after timing text
    
    ink(speechStatusColor).write(speechStatusText, {
      x: speechStatusX,
      y: infoY,
      scale: 1,
    });
  }
}

// Build the current melody string showing actual mutated notes
function buildCurrentMelodyString(originalMelodyString, melodyState) {
  if (!melodyState || !originalMelodyString) return originalMelodyString;

  // For sequential melodies, delegate to the currentSequenceState
  if (melodyState.type === "sequential" && melodyState.currentSequenceState) {
    // For sequential, we show the full original string but use currentSequenceState for highlighting
    return originalMelodyString;
  }

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
      // Also include notepat characters: vswrq (sharps), hijklmn (white notes), tyuop (sharps in higher octave)
      if (/[a-g_vswrqhijklmntyuop]/i.test(char)) {
        const trackNote = melodyState.notes[noteIndex];
        if (trackNote && trackNote.isMutation) {
          // This note was mutated, record its position for replacement
          // Convert to notepat notation for display
          const displayNote = trackNote.note === "rest" 
            ? "_" 
            : convertToNotepatDisplay(trackNote.note, trackNote.octave, octave);
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
        // Also include notepat characters
        if (/[a-g_vswrqhijklmntyuop]/i.test(char)) {
          const trackNote = track[noteIndexInTrack];
          if (trackNote && trackNote.isMutation) {
            // This note was mutated, record its position for replacement
            // Convert to notepat notation for display
            const displayNote = trackNote.note === "rest"
              ? "_"
              : convertToNotepatDisplay(trackNote.note, trackNote.octave, octave);
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
    // Include notepat characters
    if (/[a-g#_vswrqhijklmntyuop]/i.test(char)) {
      // Skip ahead past any duration modifiers
      let j = i;
      while (j + 1 < melodyString.length) {
        const nextChar = melodyString[j + 1];
        if (
          nextChar === "." ||
          nextChar === "-" ||
          nextChar === "[" ||
          nextChar === "]"
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
  // For sequential melodies, use currentSequenceState or fall back to first sequence
  let effectiveMelodyStateForViz = melodyState;
  if (melodyState && melodyState.type === "sequential") {
    if (melodyState.currentSequenceState) {
      effectiveMelodyStateForViz = melodyState.currentSequenceState;
    } else if (melodyState.sequences && melodyState.sequences.length > 0) {
      // Fall back to first sequence's parsed state
      effectiveMelodyStateForViz = melodyState.sequences[0].parsed;
    }
  }
  
  // Use CURRENT sequence's track count for layout
  // This makes the current sequence use full width, hiding history from non-existent tracks
  let trackCount = 1;
  if (effectiveMelodyStateForViz && effectiveMelodyStateForViz.type === "parallel" && effectiveMelodyStateForViz.trackStates) {
    trackCount = effectiveMelodyStateForViz.trackStates.length;
  } else if (melodyState && melodyState.type === 'sequential' && melodyState.currentSequenceState) {
    // For sequential melodies, use current sequence's track count
    const currentSeqState = melodyState.currentSequenceState;
    if (currentSeqState.type === 'parallel' && currentSeqState.trackStates) {
      trackCount = currentSeqState.trackStates.length;
    }
  }
  
  // Track the maximum track count we've seen for persistent history display
  if (trackCount > maxTrackCountSeen) {
    maxTrackCountSeen = trackCount;
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
  // Use maxTrackCountSeen to preserve history from sequences with more tracks
  const extendedHistoryItems = getExtendedHistoryItems(
    currentTimeMs,
    futureTimeWindowMs,
    maxTrackCountSeen, // Use max seen, not current, to preserve history across sequence changes
    screen.height,
    scaledPixelsPerSecond,
  );

  // ENABLED: Get future notes that should be precomputed and displayed up to the top of screen
  // For sequential melodies, pass the parent melodyState so we can predict across all sequences
  const melodyStateForFuture = (melodyState && melodyState.type === "sequential") ? melodyState : effectiveMelodyStateForViz;
  const futureNotes = getFutureNotes(currentTimeMs, screen.height, scaledPixelsPerSecond, melodyStateForFuture, nowLineY);



  // ENABLED: Combine history, current, and future notes for complete timeline visualization
  const allTimelineItems = [...extendedHistoryItems, ...futureNotes];



  // Only show currently active notes - no history or future notes
  // const allTimelineItems = [];
  // Add only the currently playing note(s) if timing has started
  if (hasFirstSynthFired && effectiveMelodyStateForViz) {
    // Unified logic for both single and parallel tracks
    if (effectiveMelodyStateForViz.type === "single") {
      // Single track - add the currently playing note
      const currentNoteIndex = getCurrentNoteIndex(effectiveMelodyStateForViz);
      if (
        currentNoteIndex >= 0 &&
        effectiveMelodyStateForViz.notes &&
        effectiveMelodyStateForViz.notes[currentNoteIndex]
      ) {
        const currentNote = effectiveMelodyStateForViz.notes[currentNoteIndex];
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
    } else if (effectiveMelodyStateForViz.type === "parallel" && effectiveMelodyStateForViz.trackStates) {
      // Parallel tracks - add currently playing notes from each track
      // Use the same robust timing logic as single tracks
      effectiveMelodyStateForViz.trackStates.forEach((trackState, trackIndex) => {
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
  // Use maxTrackCountSeen to include positions for history tracks that may not be in current section
  for (let i = 0; i < maxTrackCountSeen; i++) {
    const startPos = Math.round((timelineWidth * i) / maxTrackCountSeen);
    const endPos = Math.round((timelineWidth * (i + 1)) / maxTrackCountSeen);
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
    if (effectiveMelodyStateForViz && effectiveMelodyStateForViz.type === "parallel" && effectiveMelodyStateForViz.trackStates && item.trackIndex < effectiveMelodyStateForViz.trackStates.length) {
      // For parallel tracks, use track-specific timing
      const trackState = effectiveMelodyStateForViz.trackStates[item.trackIndex];
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
  for (let trackIdx = 0; trackIdx < maxTrackCountSeen; trackIdx++) {
    // Get all notes for this track, sorted chronologically (future → current → history)
    const trackNotes = sortedTimelineItems
      .filter(item => item.trackIndex === trackIdx)
      .sort((a, b) => a.startTime - b.startTime);
    
    if (trackNotes.length === 0) continue;
    
    // Find the currently playing note for this track as the reference point
    const currentNoteIndex = trackNotes.findIndex(item => {
      const timingTolerance = 50;
      let isCurrentlyPlayingByTiming = false;
      
      if (effectiveMelodyStateForViz && effectiveMelodyStateForViz.type === "parallel" && effectiveMelodyStateForViz.trackStates && trackIdx < effectiveMelodyStateForViz.trackStates.length) {
        const trackState = effectiveMelodyStateForViz.trackStates[trackIdx];
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
    
    for (let trackIdx = 0; trackIdx < maxTrackCountSeen; trackIdx++) {
      const trackHistoryNotes = historyNotes.filter(item => item.trackIndex === trackIdx);
      if (trackHistoryNotes.length > 0) {
        historyByTrack.set(trackIdx, trackHistoryNotes.sort((a, b) => b.endTime - a.endTime));
      }
    }
    
    // Calculate positions for each track independently
    historyByTrack.forEach((trackHistoryNotes, trackIdx) => {
      // Check if this track is active in the current sequence
      const trackActiveInCurrentSequence = effectiveMelodyStateForViz && 
        effectiveMelodyStateForViz.type === "parallel" && 
        effectiveMelodyStateForViz.trackStates && 
        trackIdx < effectiveMelodyStateForViz.trackStates.length;
      
      // Only look for active notes if this track is in the current sequence
      // For inactive tracks (from previous sections), always use time-based positioning
      let activeNoteForTrack = null;
      if (trackActiveInCurrentSequence) {
        // Find the active note for this specific track using stable timing detection
        activeNoteForTrack = sortedTimelineItems.find(item => {
          if (item.trackIndex !== trackIdx) return false;
          // Use consistent timing detection to prevent flickering - track-specific for parallel tracks
          const timingTolerance = 50; // Consistent with main detection logic
          
          let isCurrentlyPlayingByTiming = false;
          // For parallel tracks, use track-specific timing
          const trackState = effectiveMelodyStateForViz.trackStates[trackIdx];
          if (trackState.lastNoteStartTime > 0) {
            isCurrentlyPlayingByTiming = Math.abs(item.startTime - trackState.lastNoteStartTime) < timingTolerance;
          }
          
          const isCurrentlyPlaying = (musicalTimeReference >= item.startTime && musicalTimeReference <= item.endTime) ||
                                      isCurrentlyPlayingByTiming;
          return isCurrentlyPlaying;
        });
      }
      
      // Calculate starting position for history notes based on active note (if any)
      let currentEndY = nowLineY;
      if (activeNoteForTrack) {
        // Calculate where the active note ends for this track
        const activeNoteDurationMs = activeNoteForTrack.endTime - activeNoteForTrack.startTime;
        const activeNoteHeightPixels = Math.max(1, Math.round((activeNoteDurationMs / 1000) * scaledPixelsPerSecond));
        const noteProgress = (musicalTimeReference - activeNoteForTrack.startTime) / activeNoteDurationMs;
        const noteProgressClamped = Math.max(0, Math.min(1, noteProgress));
        const adjustedNoteTopY = nowLineY - activeNoteHeightPixels + noteProgressClamped * activeNoteHeightPixels;
        const activeNoteStartY = adjustedNoteTopY;
        const activeNoteEndY = activeNoteStartY + activeNoteHeightPixels;
        currentEndY = activeNoteEndY; // Start history notes from the bottom of the active note
      }
      
      // Check if this track has an active note (to determine positioning strategy)
      const hasActiveNote = !!activeNoteForTrack;
      
      // Position history notes for this track
      for (const historyNote of trackHistoryNotes) {
        const noteDurationMs = historyNote.endTime - historyNote.startTime;
        const noteHeightPixels = Math.max(1, (noteDurationMs / 1000) * scaledPixelsPerSecond); // Keep as float
        
        let barStartY, barEndY;
        
        if (hasActiveNote) {
          // Track has active note - chain history notes seamlessly from currentEndY
          barStartY = Math.round(currentEndY);
          const barHeightRounded = Math.max(1, Math.round(noteHeightPixels));
          barEndY = barStartY + barHeightRounded;
          // Next note (older) should start exactly where this note ends
          currentEndY = barEndY;
        } else {
          // Track has NO active note - use time-based positioning for each note
          // This ensures history scrolls smoothly down from NOW line based on actual timestamps
          const timeSinceNoteEnded = (musicalTimeReference - historyNote.endTime) / 1000;
          // The TOP of this note = nowLineY + time offset (history goes DOWN from NOW)
          barStartY = Math.round(nowLineY + timeSinceNoteEnded * scaledPixelsPerSecond);
          barEndY = barStartY + Math.max(1, Math.round(noteHeightPixels));
        }
        
        historyPositions.set(historyNote, { barStartY, barEndY });
      }
    });
  }

  // DEBUG: Dump geometry for all timeline items - THROTTLED
  const now = performance.now();
  if (now - (globalThis._lastGeomDump || 0) > 2000) { // Every 2 seconds
    globalThis._lastGeomDump = now;
    globalThis._collectGeom = true;
    globalThis._geomItems = [];
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
      sequenceTrackCount, // Track count for this note's sequence
      sayText, // Text for say waveform display
      sequenceIndex: noteSequenceIndex, // Which sequence this note belongs to
    } = historyItem;

    // Get current sequence index from melodyState for comparison
    const currentSeqIndex = melodyState?.currentSequence ?? null;
    
    // Determine if this is a history note (from a previous section)
    // History notes should use their OWN sequenceTrackCount for proper layout
    // Current/future notes use the current trackCount
    const isHistoryNote = endTime < musicalTimeReference;
    
    // A note is from a different section if:
    // 1. Its trackIndex exceeds the current section's track count, OR
    // 2. It's a sequential melody and the note's sequence index doesn't match current sequence
    const isFromDifferentTrackCount = trackIndex >= trackCount;
    const isFromDifferentSequence = (noteSequenceIndex !== null && currentSeqIndex !== null && noteSequenceIndex !== currentSeqIndex);
    const isFromDifferentSection = isFromDifferentTrackCount || isFromDifferentSequence;
    
    // For history notes OR notes from different sections, use their own track count
    // This shows how the previous section was laid out visually
    // Current/future notes use the current section's track count for proper full-width layout
    const effectiveTrackCount = ((isHistoryNote || isFromDifferentSection) && sequenceTrackCount)
      ? sequenceTrackCount  // History/different section: use the note's own section track count
      : trackCount;         // Current/future: use current section track count
    
    // Skip notes that can't be rendered properly (shouldn't happen but safety check)
    if (trackIndex >= effectiveTrackCount) {
      console.warn(`⚠️ Skipping note ${note}@trk${trackIndex} - trackIndex >= effectiveTrackCount (${effectiveTrackCount})`);
      return;
    }

    // Calculate track horizontal position using the note's own section track count
    // History notes from 2-track sections stay in their 2-track layout
    // Current notes use current section layout
    const noteTrackStartPos = Math.round((timelineWidth * trackIndex) / effectiveTrackCount);
    const noteTrackEndPos = Math.round((timelineWidth * (trackIndex + 1)) / effectiveTrackCount);
    const trackX = timelineStartX + noteTrackStartPos;
    const trackWidth = noteTrackEndPos - noteTrackStartPos;
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
    if (effectiveMelodyStateForViz && effectiveMelodyStateForViz.type === "parallel" && effectiveMelodyStateForViz.trackStates && trackIndex < effectiveMelodyStateForViz.trackStates.length) {
      // For parallel tracks, use track-specific timing
      const trackState = effectiveMelodyStateForViz.trackStates[trackIndex];
      if (trackState.lastNoteStartTime > 0) {
        isNearTrackTiming = Math.abs(startTime - trackState.lastNoteStartTime) < timingTolerance;
      }
    } else {
      // For single tracks, use global timing
      isNearTrackTiming = lastNoteStartTime > 0 && Math.abs(startTime - lastNoteStartTime) < timingTolerance;
    }
    
  // Notes from different sections (trackIndex >= current trackCount) should ALWAYS
  // be treated as history, not playing/future - they belong to a previous sequence
  if (isFromDifferentSection) {
    isCurrentlyPlaying = false;
    isHistory = true;
    isFuture = false;
  } else {
    isCurrentlyPlaying = isWithinNoteTiming || (isNearTrackTiming && !isWithinNoteTiming);
    
    // More precise history/future detection to prevent state flickering
    isHistory = endTime < musicalTimeReference && !isCurrentlyPlaying;
    isFuture = startTime > musicalTimeReference && !isCurrentlyPlaying;
  }

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

    // DEBUG: Collect render geometry for visualization
    if (globalThis._collectGeom) {
      globalThis._geomItems = globalThis._geomItems || [];
      globalThis._geomItems.push({
        note,
        trk: trackIndex,
        seqTrkCnt: sequenceTrackCount || '?',
        effTrkCnt: effectiveTrackCount,
        x: trackX,
        w: trackWidth,
        y: Math.round(barStartY),
        h: Math.round(barEndY - barStartY),
        type: isCurrentlyPlaying ? 'PLAY' : isFuture ? 'FUT' : 'HIST',
      });
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
              
              // Display sayText if present (for say waveform), otherwise show note name
              const displayText = sayText ? `"${sayText}"` : note.toUpperCase();
              
              // For sayText, use a smaller scale and center differently if text is long
              const textScale = sayText ? 0.6 : 0.8;
              const charWidth = 6 * textScale;
              const textWidth = displayText.length * charWidth;
              const textX = sayText 
                ? Math.round(trackCenterX - textWidth / 2) // Center sayText
                : labelX; // Original positioning for note names
              
              ink(labelColor).write(displayText, {
                x: textX,
                y: labelY,
                scale: textScale,
              });
            }
          }


        }
      }
    } else {
      // Note completely off-screen - don't render
    }
  });

  // DEBUG: Output collected geometry
  if (globalThis._collectGeom && globalThis._geomItems && globalThis._geomItems.length > 0) {
    console.log(`📐 RENDER GEOMETRY (${globalThis._geomItems.length} items) - timelineStartX=${timelineStartX}, timelineWidth=${timelineWidth}, nowLineY=${Math.round(nowLineY)}:`);
    console.table(globalThis._geomItems);
    
    // Also output as copyable JSON for analysis
    const overlaps = [];
    for (let i = 0; i < globalThis._geomItems.length; i++) {
      for (let j = i + 1; j < globalThis._geomItems.length; j++) {
        const a = globalThis._geomItems[i];
        const b = globalThis._geomItems[j];
        // Check if X ranges overlap
        const xOverlap = a.x < b.x + b.w && a.x + a.w > b.x;
        // Check if Y ranges overlap  
        const yOverlap = a.y < b.y + b.h && a.y + a.h > b.y;
        if (xOverlap && yOverlap) {
          overlaps.push({ a: `${a.note}@trk${a.trk}`, b: `${b.note}@trk${b.trk}`, ax: a.x, aw: a.w, bx: b.x, bw: b.w, ay: a.y, ah: a.h, by: b.y, bh: b.h });
        }
      }
    }
    if (overlaps.length > 0) {
      console.log(`⚠️ OVERLAPPING BOXES (${overlaps.length}):`);
      console.table(overlaps);
    } else {
      console.log(`✅ No overlapping boxes detected`);
    }
    
    globalThis._collectGeom = false;
  }

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
  sequenceTrackCount = null, // Track count for this note's sequence
  sayText = null, // Text for say waveform display
  sequenceIndex = null, // Which sequence (section) this note belongs to
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
    sequenceTrackCount: sequenceTrackCount, // Store track count for rendering
    sayText: sayText, // Store say text for display in note bars
    sequenceIndex: sequenceIndex, // Store sequence index for proper playing detection
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
  
  // Get baseTempo with fallback to global baseTempo
  const effectiveBaseTempo = melodyState.baseTempo || baseTempo || 1000;

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

      const noteDuration = noteData.duration * effectiveBaseTempo;

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

        const noteDuration = noteData.duration * effectiveBaseTempo;

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
  } else if (melodyState.type === "sequential") {
    // Sequential melodies - delegate to specialized function that predicts across sequences
    return getFutureNotesSequential(currentTimeMs, futureEndTime, melodyState, effectiveBaseTempo);
  }

  return futureNotes.sort((a, b) => a.startTime - b.startTime);
}

// Get future notes for sequential melodies - predicts across all sequences
function getFutureNotesSequential(
  currentTimeMs,
  futureEndTime,
  parentMelodyState,
  effectiveBaseTempo,
) {
  const futureNotes = [];
  if (!parentMelodyState || !parentMelodyState.sequences) return futureNotes;
  
  const sequences = parentMelodyState.sequences;
  const currentSeqIndex = parentMelodyState.currentSequence || 0;
  const currentSeq = sequences[currentSeqIndex];
  const currentState = parentMelodyState.currentSequenceState;
  
  if (!currentState) return futureNotes;
  
  // Calculate starting time and position within current sequence
  let predictTime = currentTimeMs;
  let seqIndex = currentSeqIndex;
  let loopsRemaining = currentSeq.loopCount - (currentSeq.currentLoop || 0);
  
  // Start from the next note in current sequence
  if (currentState.type === 'single') {
    let noteIndex = currentState.index || 0;
    const notes = currentState.notes;
    if (!notes || notes.length === 0) return futureNotes;
    
    // Only predict remaining notes in CURRENT sequence to avoid overlaps with different track counts
    const seqTracks = currentSeq.tracks || [currentSeq.notes];
    const seqTrackCount = seqTracks.length;
    const seqNotes = seqTracks[0];
    if (!seqNotes || seqNotes.length === 0) return futureNotes;
    
    // Predict remaining notes in this sequence
    for (let i = noteIndex; i < seqNotes.length && predictTime < futureEndTime; i++) {
      const noteData = seqNotes[i];
      if (!noteData) continue;
      
      const noteDuration = noteData.duration * effectiveBaseTempo;
      
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
        sequenceIndex: currentSeqIndex,
        sequenceTrackCount: seqTrackCount,
      });
      
      predictTime += noteDuration;
    }
  } else if (currentState.type === 'parallel' && currentState.trackStates) {
    // Only predict remaining notes in CURRENT sequence to avoid overlaps with different track counts
    const seqTracks = currentSeq.tracks || [currentSeq.notes];
    const seqTrackCount = seqTracks.length;
    if (!seqTracks || seqTracks.length === 0) return futureNotes;
    
    // Add future notes from all tracks in current sequence only
    seqTracks.forEach((track, trackIndex) => {
      if (!track) return;
      
      // Use each track's current position from trackStates
      let startNoteIndex = 0;
      let trackTime = currentTimeMs;
      
      if (currentState.trackStates && currentState.trackStates[trackIndex]) {
        const trackState = currentState.trackStates[trackIndex];
        startNoteIndex = trackState.noteIndex || 0;
        
        // Use nextNoteTargetTime if available for precise timing
        if (trackState.nextNoteTargetTime > 0) {
          trackTime = trackState.nextNoteTargetTime;
        }
      }
      
      for (let i = startNoteIndex; i < track.length && trackTime < futureEndTime; i++) {
        const noteData = track[i];
        if (!noteData) continue;
        
        const noteDuration = noteData.duration * effectiveBaseTempo;
        
        futureNotes.push({
          note: noteData.note,
          octave: noteData.octave || octave,
          startTime: trackTime,
          endTime: trackTime + noteDuration,
          trackIndex: trackIndex,
          waveType: noteData.waveType || "sine",
          volume: noteData.volume || 0.8,
          isMutation: false,
          struck: noteData.struck || false,
          sequenceIndex: currentSeqIndex,
          sequenceTrackCount: seqTrackCount,
        });
        
        trackTime += noteDuration;
      }
    });
  }
  
  return futureNotes;
}

// Clear history buffer (useful for reset)
function clearHistory() {
  historyBuffer.length = 0;
}

// 📚 Library

// Helper function to initialize the playback state for a specific sequence
// continuationTime: if provided, new tracks start from this time (for seamless sequence transitions)
function initializeSequenceState(melodyState, sequenceIndex, baseTempo, isFallback, colon, continuationTime = null) {
  if (!melodyState || melodyState.type !== 'sequential') return;
  if (sequenceIndex < 0 || sequenceIndex >= melodyState.sequences.length) return;
  
  const sequence = melodyState.sequences[sequenceIndex];
  melodyState.currentSequence = sequenceIndex;
  
  // Reset the sequence's loop counter if we're starting fresh
  sequence.currentLoop = 0;
  
  // Get tracks for this sequence
  const tracks = sequence.tracks || [sequence.notes];
  
  if (tracks.length === 1 || sequence.isSingleTrack) {
    // Single track in this sequence
    melodyState.currentSequenceState = {
      type: 'single',
      notes: tracks[0],
      index: 0,
      baseTempo: baseTempo,
      isPlaying: false,
      startTime: performance.now(),
      timingMode: parseFloat(colon?.[0]) || 1.0,
      isFallback: isFallback,
      // Use continuation time if provided (seamless transition from previous sequence)
      nextNoteTargetTime: continuationTime || 0,
    };
    
    // Preserve mutation metadata if present
    if (tracks[0]?.hasMutation) {
      melodyState.currentSequenceState.hasMutation = true;
      melodyState.currentSequenceState.originalContent = tracks[0].originalContent;
      if (tracks[0].mutationTriggerPositions) {
        melodyState.currentSequenceState.mutationTriggerPositions = tracks[0].mutationTriggerPositions;
        melodyState.currentSequenceState.currentMutationZone = 0;
      }
    }
  } else {
    // Multiple parallel tracks in this sequence
    melodyState.currentSequenceState = {
      type: 'parallel',
      tracks: tracks,
      index: 0,
      baseTempo: baseTempo,
      isPlaying: false,
      startTime: performance.now(),
      timingMode: parseFloat(colon?.[0]) || 1.0,
      isFallback: isFallback,
      maxLength: Math.max(...tracks.map(t => t?.length || 0)),
      trackStates: tracks.map((track, trackIndex) => ({
        trackIndex: trackIndex,
        noteIndex: 0,
        track: track,
        // Use continuation time if provided (seamless transition from previous sequence)
        nextNoteTargetTime: continuationTime || 0,
        startTime: 0,
        totalElapsedBeats: 0,
        lastMutationTriggered: false,
      })),
    };
  }
  
  console.log(`🎵 Initialized sequence ${sequenceIndex + 1}/${melodyState.sequences.length} (loop ${sequence.currentLoop + 1}/${sequence.loopCount}): ${tracks.length} tracks`);
}

// Helper function to advance to the next sequence (or loop current one)
function advanceSequence(melodyState, baseTempo, isFallback, colon) {
  if (!melodyState || melodyState.type !== 'sequential') return false;
  
  const currentSeq = melodyState.sequences[melodyState.currentSequence];
  currentSeq.currentLoop++;
  
  console.log(`🎵 Sequence ${melodyState.currentSequence + 1} completed loop ${currentSeq.currentLoop}/${currentSeq.loopCount}`);
  
  // Capture the continuation time from the current sequence's track(s)
  // This is the latest nextNoteTargetTime - where the new sequence should pick up
  let continuationTime = null;
  const seqState = melodyState.currentSequenceState;
  if (seqState) {
    if (seqState.type === 'single' && seqState.nextNoteTargetTime) {
      continuationTime = seqState.nextNoteTargetTime;
    } else if (seqState.type === 'parallel' && seqState.trackStates) {
      // Use the maximum nextNoteTargetTime from all tracks
      // This ensures the new sequence starts after the last note finishes
      continuationTime = Math.max(...seqState.trackStates.map(ts => ts.nextNoteTargetTime || 0));
    }
  }
  
  if (currentSeq.currentLoop >= currentSeq.loopCount) {
    // Move to next sequence
    const nextSequenceIndex = (melodyState.currentSequence + 1) % melodyState.sequences.length;
    
    // Reset the completed sequence's loop counter for next time
    currentSeq.currentLoop = 0;
    
    // Don't clear history - we want a continuous global timeline view
    // History notes will naturally age out as MAX_HISTORY_ITEMS is reached
    
    console.log(`🎵 Advancing to sequence ${nextSequenceIndex + 1}/${melodyState.sequences.length} (continuation time: ${continuationTime})`);
    initializeSequenceState(melodyState, nextSequenceIndex, baseTempo, isFallback, colon, continuationTime);
    return true; // Sequence changed
  } else {
    // Loop current sequence again
    console.log(`🎵 Looping sequence ${melodyState.currentSequence + 1} again (${currentSeq.currentLoop + 1}/${currentSeq.loopCount}, continuation time: ${continuationTime})`);
    initializeSequenceState(melodyState, melodyState.currentSequence, baseTempo, isFallback, colon, continuationTime);
    return false; // Same sequence, just looped
  }
}

// Helper function to check if all tracks in current sequence have completed one full cycle
function checkSequenceCompletion(melodyState) {
  if (!melodyState || melodyState.type !== 'sequential') return false;
  if (!melodyState.currentSequenceState) return false;
  
  const state = melodyState.currentSequenceState;
  
  if (state.type === 'single') {
    // Check if single track has looped back to start
    return state.index === 0 && state.hasCompletedOneCycle;
  } else if (state.type === 'parallel') {
    // Check if all parallel tracks have looped back to start
    if (!state.trackStates) return false;
    return state.trackStates.every(ts => ts.noteIndex === 0 && ts.hasCompletedOneCycle);
  }
  
  return false;
}

// Helper function to check if melody state has content
function hasMelodyContent(melodyState) {
  if (!melodyState) return false;

  if (melodyState.type === "single") {
    return melodyState.notes && melodyState.notes.length > 0;
  } else if (melodyState.type === "parallel") {
    return melodyState.trackStates && melodyState.trackStates.length > 0;
  } else if (melodyState.type === "multi") {
    return melodyState.tracks && melodyState.tracks.length > 0;
  } else if (melodyState.type === "sequential") {
    // Sequential melody has content if it has sequences, even if currentSequenceState isn't set yet
    return melodyState.sequences && melodyState.sequences.length > 0;
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
  toneShift = 0, // New parameter for Hz pitch shift
  screen = null, // Screen object for floating displays
  trackIndex = 0, // Track index for floating display positioning
  stampleCode = null, // Painting code for stample loading (e.g., "abc123" from {#abc123})
  sayText = null, // Text for say waveform (e.g., "hello" from {"hello"})
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

  let synthInstance;

  // Handle bubble waveform type specially using sound.bubble instead of sound.synth
  if (waveType === "bubble") {
    // Convert note to frequency for bubble radius calculation
    const baseFreq = sound.freq(tone); // Get base frequency for the note
    const finalFreq = baseFreq + (toneShift || 0); // Apply Hz shift
    
    // Calculate correct radius for the desired frequency using bubble physics
    // From bubble.mjs: phaseStep = (3.0 / radius) * timestep
    // And frequency = phaseStep * sampleRate / (2π)
    // Solving for radius: radius = 3.0 / (frequency * 2π) / 0.001
    const internalRadius = 3.0 / (finalFreq * 2 * Math.PI);
    const bubbleRadius = internalRadius / 0.001; // Undo the *0.001 scaling in bubble.mjs
    
    // Map to other bubble parameters
    const rise = 1.0; // Default buoyancy
    const pan = 0;    // Centered pan (could be enhanced later)
    
    synthInstance = sound.bubble({
      radius: bubbleRadius,
      rise: rise,
      volume: volume,
      pan: pan
    });

    // Handle struck vs held mode for bubbles
    if (struck) {
      // For struck notes, disable sustain so bubbles naturally decay
      if (synthInstance.disableSustain) {
        synthInstance.disableSustain();
        console.log(`🧋 STRUCK: Sustain disabled for ${tone}`);
      }
    } else {
      // For held notes, enable sustain so bubbles continue until manually stopped
      if (synthInstance.enableSustain) {
        synthInstance.enableSustain();
        console.log(`🧋 HELD: Sustain enabled for ${tone}`);
      }
    }

    // Debug bubble parameters with more detail
    console.log(`🧋 BUBBLE: ${tone} (${Math.round(finalFreq)}Hz) → radius:${bubbleRadius.toFixed(1)} ${struck ? 'struck' : 'held'} vol:${volume.toFixed(2)}`);
  } else if (waveType === "stample" || waveType === "sample") {
    // Handle stample/sample waveform type using sound.play() like toss and notepat do
    let sampleId = null;
    
    // Check for specific painting code first
    if (stampleCode) {
      const cached = stampleCache.get(stampleCode);
      if (cached?.loaded) {
        sampleId = cached.sampleId;
        console.log(`🎤 STAMPLE: Using cached painting #${stampleCode}`);
      } else if (cached?.loading) {
        // Sample is loading - stay silent until loaded
        console.log(`🎤 STAMPLE: #${stampleCode} still loading (silent)`);
        return null; // Don't play anything while loading
      } else if (!cached && netPreload && soundRef) {
        // Trigger lazy load for next time, stay silent for now
        loadStampleCode(stampleCode, { preload: netPreload, sound: soundRef });
        console.log(`🎤 STAMPLE: Started loading #${stampleCode} (silent until loaded)`);
        return null; // Don't play anything while loading
      } else if (cached?.error) {
        // Loading failed - use fallback
        console.log(`🎤 STAMPLE: #${stampleCode} failed to load, using fallback`);
        sampleId = stampleSampleId || fallbackSfx;
      }
    } else {
      // No painting code specified - use default stample
      sampleId = stampleSampleId || fallbackSfx;
    }
    
    if (sampleId) {
      // Get frequency for pitch adjustment
      const baseFreq = sound.freq(tone);
      const finalFreq = baseFreq + (toneShift || 0);
      
      synthInstance = sound.play(sampleId, {
        volume: volume,
        pitch: finalFreq, // Pitch in Hz (bios divides by basePitch 440)
        loop: !struck, // Loop for held notes, don't loop for struck notes
      });
      
      const codeInfo = stampleCode ? ` #${stampleCode}` : '';
      console.log(`🎤 STAMPLE${codeInfo}: ${tone} (${Math.round(finalFreq)}Hz) vol:${volume.toFixed(2)} ${struck ? 'struck' : 'held'}`);
    } else {
      // Fallback to sine if no sample available
      console.log(`🎤 STAMPLE: No sample available, falling back to sine for ${tone}`);
      synthInstance = sound.synth({
        type: "sine",
        tone: tone,
        duration: synthDuration,
        attack: struck ? 0.0 : 0.01,
        decay: struck ? 1 : 0.1,
        volume: volume,
        toneShift: toneShift,
      });
    }
  } else if (waveType === "say" && sayText) {
    // Handle say waveform type - plays cached speech at the note's pitch
    // Use PLAIN text (not SSML with random prosody) so we get consistent caching
    // Pitch is applied via playback speed, not TTS prosody
    
    // Get frequency for pitch adjustment
    const baseFreq = sound.freq(tone);
    const finalFreq = baseFreq + (toneShift || 0);
    
    // Use actualDuration (in ms) for time stretching - this is the real note length
    // Don't use synthDuration as it's in seconds for struck notes or infinite for held notes
    const sayTargetDuration = actualDuration; // Already in ms with minimum 50ms enforced
    
    console.log(`🗣️ SAY createManagedSound: waveType=${waveType}, sayText="${sayText}", tone=${tone}, freq=${Math.round(finalFreq)}Hz, targetDuration=${sayTargetDuration}ms`);
    
    // Use speak function with pitch option - this uses speakAPI.playSfx directly in bios
    // which has access to the speech cache (sound.play goes through worker and can't access it)
    if (speakRef) {
      speakRef(sayText, "female:18", "cloud", {
        volume: volume,
        pitch: finalFreq, // Pitch in Hz - speech.mjs will convert to speed
        targetDuration: sayTargetDuration, // Time stretch to fit note duration (ms), then pitch shift
        loop: false, // Speech samples should play once, not loop
        skipCompleted: true,
      });
    } else {
      console.error("🗣️ SAY ERROR: speakRef is not set!");
    }
    
    console.log(`🗣️ SAY: "${sayText}" @ ${tone} (${Math.round(finalFreq)}Hz) targetDuration:${sayTargetDuration}ms vol:${volume.toFixed(2)} ${struck ? 'struck' : 'held'}`);
  } else {
    // Use normal synth for all other waveform types
    synthInstance = sound.synth({
      type: waveType || "sine",
      tone: tone,
      duration: synthDuration,
      attack: struck ? 0.0 : 0.01, // Very short attack for struck notes
      decay: struck ? 1 : 0.1, // Fast decay for struck notes - will fade over the full duration
      volume: volume,
      toneShift: toneShift, // Pass through the Hz pitch shift
    });
  }

  // Debug: Log toneShift being passed to synth
  if (toneShift !== 0) {
    console.log(`🎵 CLOCK DEBUG: Creating sound for ${tone} with toneShift: ${toneShift}Hz`);
  }

  // Set flag to indicate first synth has fired (for syntax coloring)
  hasFirstSynthFired = true;

  // Calculate and display the actual frequency being played (only when using Hz shifts)
  if (screen && sound && sound.freq && toneShift !== 0) {
    try {
      // Calculate the actual frequency including Hz shift
      const baseFreq = sound.freq(tone);
      const finalFreq = baseFreq + toneShift;
      
      // Create floating display showing the frequency
      const noteDisplayString = tone.replace(/[0-9]/g, '').toUpperCase(); // Remove octave numbers for display
      addFloatingHzDisplay(finalFreq, noteDisplayString, trackIndex, screen);
      
      console.log(`🎵 FLOATING: ${noteDisplayString} at ${Math.round(finalFreq * 10) / 10}Hz (track ${trackIndex + 1})`);
    } catch (error) {
      console.warn(`❌ Could not calculate frequency for floating display: ${tone}`, error);
    }
  }

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

  // For bubble waveforms with sustain enabled, don't schedule automatic release
  // They should play indefinitely until manually stopped by the next note
  const isSustainedBubble = (waveType === "bubble" && !struck);
  
  if (!isSustainedBubble) {
    // Schedule the note to be released for non-bubble sounds or struck bubbles
    scheduledNotes.push({
      soundId: soundId,
      releaseTime: releaseTime,
      fadeTime: fadeTime, // Store fade time with the scheduled release
    });

    // Sort scheduled notes by release time for efficient processing
    scheduledNotes.sort((a, b) => a.releaseTime - b.releaseTime);
  }

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
function slideActiveSoundsToNewOctave(oldOctave, newOctave, soundAPI) {
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
          // Handle bubble waveforms differently - they use radius instead of tone
          if (sound.waveType === "bubble") {
            // Convert new tone to frequency and then to bubble radius using correct physics
            const newFreq = soundAPI.freq(newTone);
            
            // Calculate correct radius using bubble physics: radius = 3.0 / (frequency * 2π) / 0.001
            const internalRadius = 3.0 / (newFreq * 2 * Math.PI);
            const newBubbleRadius = internalRadius / 0.001;
            
            // Update the bubble with new radius
            sound.synth.update({
              radius: newBubbleRadius,
              duration: 0.05, // Fast transition
            });
            
            console.log(`🧋 OCTAVE: ${sound.tone} → ${newTone} (${Math.round(newFreq)}Hz, radius:${newBubbleRadius.toFixed(1)})`);
          } else {
            // Update the synth to slide to the new tone (for non-bubble sounds)
            sound.synth.update({
              tone: newTone,
              duration: 0.05, // Very fast slide duration - minimal delay/skip
            });
          }

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

    // Convert notepat notation with the new octave first
    const convertedMelodyString = convertNotepatNotation(originalMelodyString, newOctave);

    // Reparse the melody with the new base octave
    melodyTracks = parseSimultaneousMelody(convertedMelodyString, newOctave);

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

function act({ event: e, clock, sound, screen, ui, typeface, api }) {
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
      slideActiveSoundsToNewOctave(oldOctave, octave, sound);

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
      slideActiveSoundsToNewOctave(oldOctave, octave, sound);

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

  // Reset mutations with backspace key - restore original melody state
  if (e.is("keyboard:down:backspace")) {
    // Reset all mutation state and reparse from original melody string
    if (originalMelodyString && originalMelodyString.trim().length > 0) {
      // Clear mutation state
      mutatedNotePositions = [];
      recentlyMutatedNoteIndex = -1;
      recentlyMutatedTrackIndex = -1;
      mutationFlashTime = 0;
      triggeredAsteriskPositions = [];

      // Re-parse the original melody to reset all mutations
      melodyTracks = parseSimultaneousMelody(originalMelodyString, octave);
      
      // Reset melody state based on track structure (same logic as in boot())
      if (melodyTracks.isSingleTrack) {
        parsedMelody = melodyTracks.tracks[0];
        
        // Check if the first note has an explicit octave
        if (parsedMelody.length > 0 && parsedMelody[0].octave !== octave) {
          octave = parsedMelody[0].octave;
        }

        // Extract tone strings for the sequence
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
          timingMode: parseFloat(1.0) || 1.0, // Default timing
          type: "single",
        };
      } else if (melodyTracks.tracks.length === 1) {
        // Single parallel group - treat as single track
        parsedMelody = melodyTracks.tracks[0];

        if (parsedMelody.length > 0 && parsedMelody[0].octave !== octave) {
          octave = parsedMelody[0].octave;
        }

        sequence = extractTones(parsedMelody, {
          skipRests: false,
          restTone: `${octave}G`,
        });

        melodyState = {
          notes: parsedMelody,
          index: 0,
          baseTempo: baseTempo,
          isPlaying: false,
          startTime: performance.now(),
          timingMode: parseFloat(1.0) || 1.0,
          type: "single",
        };
      } else {
        // Multiple parallel tracks
        melodyState = {
          tracks: melodyTracks.tracks,
          trackStates: melodyTracks.tracks.map((track, trackIndex) => ({
            trackIndex: trackIndex,
            noteIndex: 0,
            track: track,
            nextNoteTargetTime: 0,
            startTime: 0,
            totalElapsedBeats: 0,
            lastMutationTriggered: false,
          })),
          baseTempo: baseTempo,
          isPlaying: false,
          startTime: performance.now(),
          timingMode: parseFloat(1.0) || 1.0,
          type: "parallel",
        };
      }

      // Clear history buffer to remove mutation traces
      historyBuffer = [];
      maxTrackCountSeen = 1; // Reset max track count on melody reset
      
      console.log("🔄 Mutations reset - melody restored to original state");
    }
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
        slideActiveSoundsToNewOctave(oldOctave, octave, sound);

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
        slideActiveSoundsToNewOctave(oldOctave, octave, sound);

        // Reparse melody with new octave
        reparseMelodyWithNewOctave(octave);
      }
    },
  });

  // Notepat-style keyboard mapping for live note playing
  // First octave (base octave): cdefgab + vswrq (sharps)
  // Second octave (base octave + 1): hijklmn + tyuop (sharps)
  
  // Helper function to play a note with current settings
  const playKeyboardNote = (noteName, noteOctave) => {
    const noteString = `${noteOctave}${noteName}`;
    
    // Use current melody settings if available
    const currentSettings = melodyState ? {
      waveform: melodyState.currentWaveform || "sine",
      volume: melodyState.currentVolume || 0.7,
      hzShift: melodyState.currentHzShift || 0
    } : {
      waveform: "sine", 
      volume: 0.7,
      hzShift: 0
    };
    
    // Play the note with current settings
    sound.synth({
      type: currentSettings.waveform,
      tone: noteString,
      duration: 0.5, // Short duration for live playing
      volume: currentSettings.volume,
      attack: 0.01,
      decay: 0.999
    });
    
    console.log(`🎹 Played: ${noteString} (${currentSettings.waveform})`);
  };

  // First octave white keys (c d e f g a b)
  if (e.is("keyboard:down:c") && !e.repeat) playKeyboardNote("c", octave);
  if (e.is("keyboard:down:d") && !e.repeat) playKeyboardNote("d", octave);
  if (e.is("keyboard:down:e") && !e.repeat) playKeyboardNote("e", octave);
  if (e.is("keyboard:down:f") && !e.repeat) playKeyboardNote("f", octave);
  if (e.is("keyboard:down:g") && !e.repeat) playKeyboardNote("g", octave);
  if (e.is("keyboard:down:a") && !e.repeat) playKeyboardNote("a", octave);
  if (e.is("keyboard:down:b") && !e.repeat) playKeyboardNote("b", octave);
  
  // First octave black keys (v s w r q → c# d# f# g# a#)
  if (e.is("keyboard:down:v") && !e.repeat) playKeyboardNote("c#", octave);
  if (e.is("keyboard:down:s") && !e.repeat) playKeyboardNote("d#", octave);
  if (e.is("keyboard:down:w") && !e.repeat) playKeyboardNote("f#", octave);
  if (e.is("keyboard:down:r") && !e.repeat) playKeyboardNote("g#", octave);
  if (e.is("keyboard:down:q") && !e.repeat) playKeyboardNote("a#", octave);
  
  // Second octave white keys (h i j k l m n → c d e f g a b)
  if (e.is("keyboard:down:h") && !e.repeat) playKeyboardNote("c", octave + 1);
  if (e.is("keyboard:down:i") && !e.repeat) playKeyboardNote("d", octave + 1);
  if (e.is("keyboard:down:j") && !e.repeat) playKeyboardNote("e", octave + 1);
  if (e.is("keyboard:down:k") && !e.repeat) playKeyboardNote("f", octave + 1);
  if (e.is("keyboard:down:l") && !e.repeat) playKeyboardNote("g", octave + 1);
  if (e.is("keyboard:down:m") && !e.repeat) playKeyboardNote("a", octave + 1);
  if (e.is("keyboard:down:n") && !e.repeat) playKeyboardNote("b", octave + 1);
  
  // Second octave black keys (t y u o p → c# d# f# g# a#)
  if (e.is("keyboard:down:t") && !e.repeat) playKeyboardNote("c#", octave + 1);
  if (e.is("keyboard:down:y") && !e.repeat) playKeyboardNote("d#", octave + 1);
  if (e.is("keyboard:down:u") && !e.repeat) playKeyboardNote("f#", octave + 1);
  if (e.is("keyboard:down:o") && !e.repeat) playKeyboardNote("g#", octave + 1);
  if (e.is("keyboard:down:p") && !e.repeat) playKeyboardNote("a#", octave + 1);

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
let simDebugLogged = false; // One-time debug flag

// Simple musical state - no complex timeline needed
let musicalState = {
  lastSyncTime: 0,
  isInitialized: false,
};

function sim({ sound, beep, clock, num, help, params, colon, screen, speak }) {
  if (!simDebugLogged) {
    simDebugLogged = true;
    console.log("🎵 SIM FIRST RUN - melodyState:", melodyState?.type, "notes:", melodyState?.notes?.length);
  }
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

  if (!time) {
    // Debug: clock not synced yet
    if (Math.random() < 0.01) console.log("🔴 SIM: clock.time() returned null - waiting for sync");
    return;
  }

  const seconds = time.getSeconds();
  const milliseconds = time.getMilliseconds();

  function bleep(syncedTime) {
    if (!melodyState) {
      // Boot hasn't completed yet - silently return
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
        sonicDuration,
        swing,
        swingAmount,
        waveType,
        volume,
        struck,
        toneShift,
        stampleCode,
        sayText,
      } = noteData;

      // Debug: Log toneShift value and handle cumulative Hz shifts
      let actualToneShift = toneShift;
      
      // Handle cumulative Hz shifts
      if (typeof toneShift === 'object' && toneShift.cumulative) {
        const trackId = 0; // Single track uses track ID 0
        const state = getCumulativeState(trackId);
        
        // Initialize cumulative state if this is the first time we see it
        if (!state.enabled) {
          // Calculate interval based on melody length and tempo for consistent timing
          const totalMelodyDuration = melodyState.notes.reduce((sum, note) => sum + note.duration, 0) * melodyState.baseTempo;
          initializeCumulativeState(trackId, toneShift.step, totalMelodyDuration);
        }
        actualToneShift = state.current;
      }

      // Handle speech synthesis for quoted text
      if (noteData.isSpeech && noteData.text) {
        const voiceType = help.flip() ? "female" : "male"; // Random voice selection
        const voiceNumber = voiceType === "female" ? 18 : 22;
        const voiceString = `${voiceType}:${voiceNumber}`;
        
        console.log(`🗣️ Playing speech: "${noteData.text}" with ${voiceType} voice`);
        
        // Use speakText function for cached speech playback
        speakText(noteData.text, voiceString, { 
          pan: 0, 
          volume: volume || 0.8 
        }, num, speak);
        
        // Add speech note to history buffer for visual timeline
        addNoteToHistory(
          "speech",
          noteOctave || octave,
          currentTime,
          duration * melodyState.baseTempo,
          0, // Track 0 for single track
          "speech", // Special waveType for speech
          volume || 0.8,
          false, // Not a mutation
          struck,
          1, // Single track section
          noteData.text, // Speech text for display
          null, // No sequence index for single track
        );
        
        // Flash green for special character (speech)
        specialCharFlashTime = performance.now();
      }
      // Play the note using managed sound system
      else if (note !== "rest") {
        let tone = noteOctave
          ? `${noteOctave}${note.toUpperCase()}`
          : `${octave}${note.toUpperCase()}`;
        const noteDuration = duration * melodyState.baseTempo;
        const synthDuration = (sonicDuration || duration) * melodyState.baseTempo;

        try {
          // Check if this is the last note in the sequence to eliminate loop gap
          const isLastNote = (melodyState.index === melodyState.notes.length - 1);
          
          // Calculate frequency with lower bound checking
          const frequencyResult = calculateFrequencyWithLowerBound(tone, actualToneShift || 0, sound, 0); // Track ID 0 for single track
          const finalToneShift = frequencyResult.toneShift;
          
          // Log if frequency was reset due to lower bound
          if (frequencyResult.wasReset) {
            console.log(`🎵 Note ${tone}: Frequency reset from ${(frequencyResult.frequency - frequencyResult.originalShift).toFixed(2)}Hz to ${frequencyResult.frequency.toFixed(2)}Hz (Hz shift: ${frequencyResult.originalShift} → 0)`);
          }
          
          // Create managed sound that will be automatically released
          
          // For sustained bubbles, kill any previous sustained bubbles before starting new one
          if (waveType === "bubble" && !struck) {
            Object.keys(activeSounds).forEach((soundId) => {
              const activeSound = activeSounds[soundId];
              if (activeSound && activeSound.waveType === "bubble" && !activeSound.isStruck) {
                // Kill previous sustained bubble with short fade
                if (activeSound.synth && activeSound.synth.kill) {
                  activeSound.synth.kill(0.05);
                }
                delete activeSounds[soundId];
              }
            });
          }
          
          createManagedSound(
            sound,
            tone,
            waveType,
            synthDuration,
            volume || 0.8, // Use note volume or default to 0.8
            melodyState.isFallback,
            struck, // Pass the struck flag for note timing behavior
            isLastNote, // Eliminate gap for last note to prevent loop timing issues
            finalToneShift, // Use the frequency-bounded Hz shift
            screen, // Pass screen for floating displays
            0, // Track index 0 for single track
            stampleCode, // Painting code for stample waveform
            sayText, // Text for say waveform
          );

          // Add note to history buffer for visual timeline
          addNoteToHistory(
            note,
            noteOctave || octave,
            currentTime,
            synthDuration,
            0, // Track 0 for single track
            waveType || "sine",
            volume || 0.8,
            false, // Not a mutation (mutations are added separately)
            struck,
            1, // Single track section
            sayText, // Text for say waveform display
            null, // No sequence index for single track
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
          1, // Single track section
          null, // No say text for rest
          null, // No sequence index for single track
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
        
        // Handle cumulative Hz shift increment when looping for single track
        const trackId = 0; // Single track uses track ID 0
        const state = getCumulativeState(trackId);
        if (state.enabled) {
          state.cycles++;
          state.current = state.step * state.cycles;
          
          // Lower bound check: if Hz goes below 0, reset to default (0)
          if (state.current < 0) {
            console.log(`🎵 Track ${trackId + 1}: Hz shift went below 0 (${state.current}Hz), resetting to default (0Hz)`);
            state.current = 0;
            state.cycles = 0;
            state.enabled = false; // Disable cumulative mode after reset
          }
          
          console.log(`🎵 Cycle ${state.cycles}: Hz shift now ${state.current >= 0 ? '+' : ''}${state.current}Hz`);
        }
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

    // Handle time-based cumulative Hz increments for all tracks
    if (cumulativeHzStates.size > 0 && globalCumulativeInterval > 0) {
      for (const [trackId, state] of cumulativeHzStates) {
        if (state.enabled) {
          updateCumulativeState(trackId, currentTimeMs);
        }
      }
    }

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

    const hasContent = hasMelodyContent(melodyState);
    if (!hasContent && melodyState) {
      // Debug: why is hasMelodyContent returning false?
      console.log("🔴 SIM: hasMelodyContent=false but melodyState exists:", 
        "type:", melodyState.type, 
        "notes:", melodyState.notes?.length,
        "trackStates:", melodyState.trackStates?.length,
        "tracks:", melodyState.tracks?.length);
    }
    
    // Block playback until samples are ready
    if (!loadingState.readyToPlay) {
      // Still loading samples - don't start playback yet
      return;
    }
    
    if (hasContent) {
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
          // CRITICAL: Don't play notes until all samples are loaded (for {say} waveform)
          if (!loadingState.readyToPlay) {
            // Only log once per second to avoid spam
            if (!loadingState.lastWaitLog || performance.now() - loadingState.lastWaitLog > 1000) {
              console.log("🗣️ Waiting for samples...", loadingState.samplesLoaded, "/", loadingState.samplesNeeded);
              loadingState.lastWaitLog = performance.now();
            }
            // Skip this note timing - defer until samples are ready
            // Adjust target time to prevent timing drift while waiting
            nextNoteTargetTime = currentTimeMs + 50; // Check again in 50ms
            return; // Exit early - don't play notes yet
          }
          
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
            // CRITICAL: Don't play notes until all samples are loaded (for {say} waveform)
            if (!loadingState.readyToPlay) {
              // Only log once per second to avoid spam
              if (!loadingState.lastWaitLogParallel || performance.now() - loadingState.lastWaitLogParallel > 1000) {
                console.log("🗣️ Parallel: Waiting for samples...", loadingState.samplesLoaded, "/", loadingState.samplesNeeded);
                loadingState.lastWaitLogParallel = performance.now();
              }
              // Skip this note timing - defer until samples are ready
              // Adjust target time to prevent timing drift while waiting
              trackState.nextNoteTargetTime = currentTimeMs + 50; // Check again in 50ms
              return; // Exit early - don't play notes yet
            }
            const noteData = trackState.track[trackState.noteIndex];
            if (noteData) {
              // Play the note
              const {
                note,
                octave: noteOctave,
                duration,
                sonicDuration,
                waveType,
                volume,
                struck,
                toneShift,
                stampleCode,
                sayText,
              } = noteData;

              // Handle cumulative Hz shifts for parallel tracks
              let actualToneShift = toneShift;
              if (typeof toneShift === 'object' && toneShift.cumulative) {
                const state = getCumulativeState(trackIndex);
                
                // Initialize cumulative state if this is the first time we see it for this track
                if (!state.enabled) {
                  // For parallel tracks: calculate interval based on shortest track duration
                  let shortestTrackDuration = Infinity;
                  melodyState.trackStates.forEach(trackState => {
                    if (trackState.track && trackState.track.length > 0) {
                      const trackDuration = trackState.track.reduce((sum, note) => sum + note.duration, 0) * melodyState.baseTempo;
                      shortestTrackDuration = Math.min(shortestTrackDuration, trackDuration);
                    }
                  });
                  initializeCumulativeState(trackIndex, toneShift.step, shortestTrackDuration);
                }
                // Use the current cumulative state for this specific track
                actualToneShift = state.current;
              }

              // Handle speech synthesis for quoted text in parallel tracks
              if (noteData.isSpeech && noteData.text) {
                const voiceType = help.flip() ? "female" : "male"; // Random voice selection
                const voiceNumber = voiceType === "female" ? 18 : 22;
                const voiceString = `${voiceType}:${voiceNumber}`;
                
                console.log(`🗣️ Track ${trackIndex + 1} Playing speech: "${noteData.text}" with ${voiceType} voice`);
                
                // Use speakText function for cached speech playback
                speakText(noteData.text, voiceString, { 
                  pan: 0, 
                  volume: volume || 0.8 
                }, num, speak);
                
                // Add speech note to history buffer for visual timeline
                addNoteToHistory(
                  "speech",
                  noteOctave || octave,
                  currentTimeMs,
                  duration * melodyState.baseTempo,
                  trackIndex,
                  "speech", // Special waveType for speech
                  volume || 0.8,
                  false, // Not a mutation
                  struck,
                  melodyState.trackStates.length, // Track count for this parallel section
                  noteData.text, // Speech text for display
                  null, // No sequence index for parallel tracks
                );
                
                // Flash green for special character (speech)
                specialCharFlashTime = performance.now();
              }
              else if (note !== "rest") {
                let tone = noteOctave
                  ? `${noteOctave}${note.toUpperCase()}`
                  : `${octave}${note.toUpperCase()}`;
                const noteDuration = duration * melodyState.baseTempo;
                const synthDuration = (sonicDuration || duration) * melodyState.baseTempo;

                // DEBUG: Log note playback with duration details for sticky modifier tracking
                if (trackIndex === 0) {

                }

                try {
                  // Check if this is the last note in this track's sequence to eliminate loop gap
                  const isLastNoteInTrack = (trackState.index === trackState.track.length - 1);
                  
                  // Calculate frequency with lower bound checking
                  const frequencyResult = calculateFrequencyWithLowerBound(tone, actualToneShift || 0, sound, trackIndex);
                  const finalToneShift = frequencyResult.toneShift;
                  
                  // Log if frequency was reset due to lower bound
                  if (frequencyResult.wasReset) {
                    console.log(`🎵 Track ${trackIndex + 1} Note ${tone}: Frequency reset from ${(frequencyResult.frequency - frequencyResult.originalShift).toFixed(2)}Hz to ${frequencyResult.frequency.toFixed(2)}Hz (Hz shift: ${frequencyResult.originalShift} → 0)`);
                  }
                  
                  // For sustained bubbles, kill any previous sustained bubbles on this track before starting new one
                  if (waveType === "bubble" && !struck) {
                    Object.keys(activeSounds).forEach((soundId) => {
                      const activeSound = activeSounds[soundId];
                      // Kill previous sustained bubbles on the same track or single tracks
                      if (activeSound && activeSound.waveType === "bubble" && !activeSound.isStruck) {
                        // Kill previous sustained bubble with short fade
                        if (activeSound.synth && activeSound.synth.kill) {
                          activeSound.synth.kill(0.05);
                        }
                        delete activeSounds[soundId];
                      }
                    });
                  }
                  
                  createManagedSound(
                    sound,
                    tone,
                    waveType,
                    synthDuration,
                    volume || 0.8,
                    melodyState.isFallback,
                    struck,
                    isLastNoteInTrack, // Eliminate gap for last note to prevent loop timing issues
                    finalToneShift, // Use the frequency-bounded Hz shift
                    screen, // Pass screen for floating displays
                    trackIndex, // Pass actual track index for positioning
                    stampleCode, // Painting code for stample waveform
                    sayText, // Text for say waveform
                  );

                  // Add note to history buffer for visual timeline
                  addNoteToHistory(
                    note,
                    noteOctave || octave,
                    currentTimeMs,
                    synthDuration,
                    trackIndex,
                    waveType || "sine",
                    volume || 0.8,
                    false, // Not a mutation (mutations are added separately)
                    struck,
                    melodyState.trackStates.length, // Track count for this parallel section
                    sayText, // Text for say waveform display
                    null, // No sequence index for parallel tracks
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
                  melodyState.trackStates.length, // Track count for this parallel section
                  null, // No say text for rest
                  null, // No sequence index for parallel tracks
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

              // DEBUG: Track loop detection
              if (trackIndex === 1) {
                console.log(`🎵 Track 2 played note ${oldIndex} (${note}), next=${trackState.noteIndex}`);
              }

              // Check for mutations when this track loops back to beginning
              if (
                oldIndex === trackState.track.length - 1 &&
                trackState.noteIndex === 0
              ) {
                console.log(`🔄 Track ${trackIndex + 1} LOOPED back to start!`);
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
      } else if (melodyState.type === "sequential" && melodyState.currentSequenceState) {
        // SEQUENTIAL TIMING: Delegate to current sequence's internal state
        const seqState = melodyState.currentSequenceState;
        
        if (seqState.type === "single") {
          // Handle single track within current sequence
          if (!seqState.notes || seqState.notes.length === 0) return;
          
          // Initialize timing if not set - start immediately for seamless sequence transitions
          if (!seqState.nextNoteTargetTime || seqState.nextNoteTargetTime === 0) {
            seqState.nextNoteTargetTime = currentTimeMs; // Start NOW, not at next second boundary
            seqState.hasCompletedOneCycle = false;
          }
          
          // Check if it's time to play next note
          if (seqState.nextNoteTargetTime > 0 && currentTimeMs >= seqState.nextNoteTargetTime) {
            const noteData = seqState.notes[seqState.index];
            if (noteData) {
              const { note, octave: noteOctave, duration, sonicDuration, waveType, volume, struck, toneShift, stampleCode, sayText } = noteData;
              
              if (note !== "rest") {
                let tone = noteOctave ? `${noteOctave}${note.toUpperCase()}` : `${octave}${note.toUpperCase()}`;
                const noteDuration = duration * melodyState.baseTempo;
                const synthDuration = (sonicDuration || duration) * melodyState.baseTempo;
                
                try {
                  const frequencyResult = calculateFrequencyWithLowerBound(tone, toneShift || 0, sound, 0);
                  
                  createManagedSound(
                    sound,
                    tone,
                    waveType,
                    synthDuration,
                    volume || 0.8,
                    melodyState.isFallback,
                    struck,
                    false,
                    frequencyResult.toneShift,
                    screen,
                    0,
                    stampleCode, // Painting code for stample waveform
                    sayText, // Text for say waveform
                  );
                  
                  // Track count is 1 for single-track sequence, include sequence index for proper playing detection
                  addNoteToHistory(note, noteOctave || octave, currentTimeMs, synthDuration, 0, waveType || "sine", volume || 0.8, false, struck, 1, sayText, melodyState.currentSequence);
                  totalNotesPlayed++;
                } catch (error) {
                  console.error(`✗ Sequential single ${tone} - ${error}`);
                }
              }
              
              anyTrackPlayed = true;
              currentNoteStartTime = performance.now();
              currentNoteDuration = duration * melodyState.baseTempo;
              
              // Advance to next note
              const oldIndex = seqState.index;
              seqState.index = (seqState.index + 1) % seqState.notes.length;
              
              // CRITICAL: Update nextNoteTargetTime BEFORE checking sequence advance
              // This ensures advanceSequence gets the correct continuation time
              seqState.nextNoteTargetTime = seqState.nextNoteTargetTime + (noteData.duration * melodyState.baseTempo);
              
              // Check if we completed a cycle
              if (oldIndex === seqState.notes.length - 1 && seqState.index === 0) {
                seqState.hasCompletedOneCycle = true;
                
                // Check if sequence should advance
                if (advanceSequence(melodyState, baseTempo, melodyState.isFallback, [melodyState.timingMode])) {
                  console.log(`🎵 Sequence advanced!`);
                }
                
                // Reset the cycle flag for the new sequence state
                if (melodyState.currentSequenceState) {
                  melodyState.currentSequenceState.hasCompletedOneCycle = false;
                }
              }
            }
          }
        } else if (seqState.type === "parallel" && seqState.trackStates) {
          // Handle parallel tracks within current sequence
          let allTracksCompleted = true;
          
          seqState.trackStates.forEach((trackState, trackIndex) => {
            if (!trackState.track || trackState.track.length === 0) return;
            
            // Initialize timing for this track if not set - start immediately for seamless sequence transitions
            if (trackState.nextNoteTargetTime === 0) {
              trackState.nextNoteTargetTime = currentTimeMs; // Start NOW, not at next second boundary
              trackState.hasCompletedOneCycle = false;
            }
            
            // Track if this track hasn't completed yet
            if (!trackState.hasCompletedOneCycle) {
              allTracksCompleted = false;
            }
            
            // Check if it's time to play next note
            if (trackState.nextNoteTargetTime > 0 && currentTimeMs >= trackState.nextNoteTargetTime) {
              const noteData = trackState.track[trackState.noteIndex];
              if (noteData) {
                const { note, octave: noteOctave, duration, sonicDuration, waveType, volume, struck, toneShift, stampleCode, sayText } = noteData;
                
                if (note !== "rest") {
                  let tone = noteOctave ? `${noteOctave}${note.toUpperCase()}` : `${octave}${note.toUpperCase()}`;
                  const noteDuration = duration * melodyState.baseTempo;
                  const synthDuration = (sonicDuration || duration) * melodyState.baseTempo;
                  
                  try {
                    const frequencyResult = calculateFrequencyWithLowerBound(tone, toneShift || 0, sound, trackIndex);
                    
                    createManagedSound(
                      sound,
                      tone,
                      waveType,
                      synthDuration,
                      volume || 0.8,
                      melodyState.isFallback,
                      struck,
                      false,
                      frequencyResult.toneShift,
                      screen,
                      trackIndex,
                      stampleCode, // Painting code for stample waveform
                      sayText, // Text for say waveform
                    );
                    
                    // Track count from parallel sequence state, include sequence index for proper playing detection
                    const seqTrackCount = seqState.trackStates.length;
                    addNoteToHistory(note, noteOctave || octave, currentTimeMs, synthDuration, trackIndex, waveType || "sine", volume || 0.8, false, struck, seqTrackCount, sayText, melodyState.currentSequence);
                    totalNotesPlayed++;
                  } catch (error) {
                    console.error(`✗ Sequential parallel track ${trackIndex + 1} ${tone} - ${error}`);
                  }
                }
                
                anyTrackPlayed = true;
                
                // Advance to next note in this track
                const oldIndex = trackState.noteIndex;
                trackState.noteIndex = (trackState.noteIndex + 1) % trackState.track.length;
                
                // Check if this track completed a cycle
                if (oldIndex === trackState.track.length - 1 && trackState.noteIndex === 0) {
                  trackState.hasCompletedOneCycle = true;
                }
                
                // Calculate next note time
                trackState.nextNoteTargetTime = trackState.nextNoteTargetTime + (noteData.duration * melodyState.baseTempo);
              }
            }
          });
          
          // Check if ALL tracks have completed at least one cycle
          const allCompleted = seqState.trackStates.every(ts => ts.hasCompletedOneCycle);
          if (allCompleted) {
            // Reset all tracks' completion flags
            seqState.trackStates.forEach(ts => { ts.hasCompletedOneCycle = false; });
            
            // Advance sequence
            if (advanceSequence(melodyState, baseTempo, melodyState.isFallback, [melodyState.timingMode])) {
              console.log(`🎵 Sequence advanced after parallel completion!`);
            }
          }
        }
        
        if (anyTrackPlayed) {
          synced = true;
        }
      }
    } else {
      // No melody state yet - boot hasn't completed, silently skip
      // This is normal during async boot initialization
      return;
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
function buildOctaveButtons({ screen, ui, typeface, system }) {
  // Use typeface glyph width if available, otherwise use a reasonable default (6px)
  const glyphWidth = typeface?.glyphs?.["0"]?.resolution?.[0] || 6;
  const buttonHeight = 16;
  const buttonWidth = 16;
  const spacing = 0; // No gaps between buttons
  const padding = 4; // Padding on left and right of octave number

  // Calculate total width needed for the three-button layout: - 4 +
  const octaveText = `${octave}`;
  const octaveTextWidth = octaveText.length * glyphWidth + padding * 2;
  const totalWidth = buttonWidth + octaveTextWidth + buttonWidth;

  // Position flush to bottom LEFT corner (moved from right to leave room for QR code)
  const startX = 0;
  const tapingOffset = system?.taping ? 1 : 0; // Move up 1 pixel when taping
  const startY = screen.height - buttonHeight - tapingOffset;

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

// Export piece functions for disk.mjs to find
const background = true; // ⏱️ Keep sim() running when page is hidden (lid closed)
export { boot, paint, sim, act, leave, background };
