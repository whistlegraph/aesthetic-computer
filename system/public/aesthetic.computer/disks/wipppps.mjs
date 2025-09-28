// Oskie - zzzZWAP

// Color history for decay effect (module-level since no window object in worker)
let colorHistory = [];

// Timeline zoom smoothing (module-level)
let lastPixelsPerSecond = 60;

// Last MIDI note color for needle theming
let lastMidiNoteColor = { r: 0, g: 255, b: 255 }; // Default to cyan

// Track previous locator to detect section changes
let previousLocatorName = null;

// Track PAUSE counter to cycle through different PAUSE sources
let pauseCounter = 0;

// Timeline visibility toggle
let timelineVisible = true;

// Frequency display toggle
let frequencyDisplayVisible = false; // Commented out to make timeline flush with top

// Audio sync logging variables
let stallWarningTime = null;
let progressErrorCount = 0;
let lastProgressError = null;

// KidLisp mode toggle - when true, use kidlisp() instead of TV bars
let kidlispMode = true;

// Frequency smoothing - track previous values for smooth transitions
let previousFrequencyValues = [];

// Pause overlay state for play button
let pauseOverlayVisible = false;
let pauseOverlayStartTime = 0;
let audioStarting = false; // NEW: Track when audio is starting but not yet playing
let tapeAutoPlayStarted = false; // Track if auto-play has been triggered for tape mode
let isCurrentlyRecording = false; // Track recording state for paint function

// Emergency escape mechanism for stuck detection loops
let emergencyTapCount = 0;
let lastEmergencyTap = null;

// Pause/resume functionality
let pausedAt = 0; // Position where we paused (0 to 1)

// Pause-aware timing for visual effects
let pauseStartTime = null; // When we paused
let totalPausedTime = 0; // Total time spent paused

// zzzZWAP LOCATOR-SPECIFIC COLOR PALETTES - DERIVED FROM KIDLISP PATTERNS
const ZZZWAP_PALETTES = {
  BEFORE_START: {
    name: "Before Start",
    timelineGradient: "fade:navy-navy-blue-black:vertical",
    colors: [
      { r: 20, g: 20, b: 40 },     // dark blue
      { r: 40, g: 40, b: 80 },     // medium blue
      { r: 60, g: 60, b: 120 },    // lighter blue
      { r: 80, g: 80, b: 160 },    // bright blue
    ]
  },
  START: {
    name: "Start",
    timelineGradient: "fade:lime-lime-green-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime
      { r: 255, g: 255, b: 0 },    // yellow
      { r: 0, g: 255, b: 0 },      // lime
      { r: 255, g: 0, b: 255 },    // magenta
    ]
  },
  PAUSE: {
    name: "Pause",
    timelineGradient: "fade:gray-gray-gray-black:vertical",
    colors: [
      { r: 255, g: 255, b: 255 },  // white (timeline backdrop)
      { r: 255, g: 0, b: 0 },      // red
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
    ]
  },
  ACT_I: {
    name: "Act I",
    timelineGradient: "fade:lime-lime-green-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 255, g: 192, b: 203 },  // pink
      { r: 255, g: 0, b: 255 },    // magenta
    ]
  },
  ACT_II: {
    name: "Act II", 
    timelineGradient: "fade:magenta-magenta-purple-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime
      { r: 255, g: 0, b: 255 },    // magenta (gradient)
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 255, g: 192, b: 203 },  // pink
    ]
  },
  ACT_III: {
    name: "Act III",
    timelineGradient: "fade:lime-lime-green-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 255, g: 192, b: 203 },  // pink
      { r: 255, g: 0, b: 255 },    // magenta
    ]
  },
  ACT_IIII: {
    name: "Act IIII",
    timelineGradient: "fade:magenta-magenta-violet-black:vertical",
    colors: [
      { r: 255, g: 0, b: 255 },    // magenta (gradient)
      { r: 0, g: 255, b: 0 },      // lime
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 255, g: 192, b: 203 },  // pink
    ]
  },
  ACT_V: {
    name: "Act V",
    timelineGradient: "fade:lime-lime-cyan-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 255, g: 192, b: 203 },  // pink
      { r: 255, g: 0, b: 255 },    // magenta
    ]
  },
  ACT_VI: {
    name: "Act VI",
    timelineGradient: "fade:lime-lime-teal-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime (gradient)
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 128, g: 128, b: 128 },  // gray
      { r: 255, g: 192, b: 203 },  // pink
      { r: 255, g: 0, b: 255 },    // magenta
    ]
  },
  SURPRISE: {
    name: "Surprise",
    timelineGradient: "fade:yellow-yellow-orange-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 255, g: 192, b: 203 },  // pink
      { r: 255, g: 0, b: 255 },    // magenta
    ]
  },
  PAUSE_2: {
    name: "Pause 2",
    timelineGradient: "fade:gray-gray-gray-black:vertical",
    colors: [
      { r: 255, g: 255, b: 255 },  // white (timeline backdrop)
      { r: 255, g: 0, b: 0 },      // red (gradient)
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
    ]
  },
  PAUSE_3: {
    name: "Pause 3", 
    timelineGradient: "fade:gray-gray-gray-black:vertical",
    colors: [
      { r: 255, g: 255, b: 255 },  // white (timeline backdrop)
      { r: 255, g: 0, b: 0 },      // red
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
    ]
  },
  END: {
    name: "End",
    timelineGradient: "fade:white-white-silver-black:vertical",
    colors: [
      { r: 0, g: 255, b: 0 },      // lime
      { r: 128, g: 128, b: 128 },  // gray
      { r: 16, g: 16, b: 16 },     // very dark gray (was black)
      { r: 255, g: 192, b: 203 },  // pink
      { r: 255, g: 0, b: 255 },    // magenta
    ]
  }
};

// Zoom on the kick....

// zzzZWAP LOCATOR-SPECIFIC KIDLISP SOURCES
const ZZZWAP_KIDLISP_SOURCES = {
  BEFORE_START: `black (paste https://assets.aesthetic.computer/wipppps/cow.png 0 0 0.6) (ink black 48) (write sound 7 7) (write on? 41 7) (ink red) (write sound 6 6) (write on? 40 6) (ink black 48) (tri (- (/ width 2) 19) (- (/ height 2) 19) (- (/ width 2) 19) (+ (/ height 2) 21) (+ (/ width 2) 21) (+ (/ height 2) 1)) (ink white 180) (tri (- (/ width 2) 20) (- (/ height 2) 20) (- (/ width 2) 20) (+ (/ height 2) 20) (+ (/ width 2) 20) (/ height 2))`,
  // BEFORE_START: `(fade:black-navy-black) (ink (? red blue navy 0) (+ 50 (* kick 100))) (ink (? red blue gray 0) (+ 10 (* (random) 30))) (circle (/ width 2) (/ height 2) (+ 2 (* kick 3))) (spin (5s... -0.5 0.5)) (zoom 1.125) (contrast 1.05) (ink rainbow (? 16 32 48 128 255)) (repeat 1024 point) (scroll (? 0 0 1 -1) (? 0 0 1 -1))`,
  START: `(fade:lime-yellow-lime) (ink (? lime magenta 0) (+ 100 (* kick 155))) (ink (? lime magenta 0) (+ 20 (* (random) 60))) (flood (/ width 2) (/ height 2)) (ink (? lime magenta 0) (+ 100 (* kick 155))) (circle (/ width 2) (/ height 2) (+ 3 (* kick 4))) (spin (2s... -1.125 1.125)) (zoom 1.2) (contrast 1.05) (ink yellow (? 32 64 96 255)) (repeat 32 point) (scroll (? 0 0 0 2 -2) (? 0 0 0 2 -2))`,
  PAUSE: `(fade:red-white-black-gray) (ink (? red white 0) (+ 110 (* kick 155))) (ink (? red white 0) (+ 20 (* (random) 60))) (flood (/ width 2) (/ height 2)) (ink (? red white 0) (+ 110 (* kick 155))) (box (/ width 2) (/ height 2) (+ 3 (* kick 4)) (+ 3 (* kick 4))) (spin (0.3s... -4.0 4.0)) (zoom (0.1s... 1.5 3.0)) (contrast 1.3) (ink (? red white 0) (+ 150 (* kick 100))) (repeat 64 point) (scroll (? 0 0 2 -2) (? 0 0 2 -2))`,
  ACT_I: `(fade:lime-gray-black-pink) (ink (? lime magenta 0) (+ 120 (* kick 155))) (ink (? lime magenta 0) (+ 25 (* (random) 65))) (flood (/ width 2) (/ height 2)) (ink (? lime magenta 0) (+ 120 (* kick 155))) (circle (/ width 2) (/ height 2) (+ 3 (* kick 4))) (spin (2s... -1.3 1.3)) (zoom 1.2) (contrast 1.1) (scroll (? 3 -3) (? 3 -3))`,
  ACT_II: `(fade:lime-gray-black-pink) (ink (? lime magenta 0) (+ 130 (* kick 155))) (ink (? lime magenta 0) (+ 30 (* (random) 70))) (gradient lime magenta 0 0 width height) (ink (? lime magenta 0) (+ 130 (* kick 155))) (line (/ width 2) 0 (/ width 2) height) (spin (2s... -1.5 1.5)) (zoom 1.2) (contrast 1.1) (scroll (? 4 -4) (? 3 -3))`,
  ACT_III: `(fade:lime-gray-black-pink) (ink (? lime magenta 0) (+ 140 (* kick 155))) (ink (? lime magenta 0) (+ 35 (* (random) 75))) (flood (/ width 2) (/ height 2)) (ink (? lime magenta 0) (+ 140 (* kick 155))) (line 0 (/ height 2) width (/ height 2)) (line (/ width 2) 0 (/ width 2) height) (spin (1.8s... -1.7 1.7)) (zoom 1.2) (contrast 1.1) (scroll (? 5 -5) (? 4 -4))`,
  ACT_IIII: `(fade:lime-yellow-lime) (ink (? lime magenta 0) (+ 100 (* kick 155))) (ink (? lime magenta 0) (+ 20 (* (random) 60))) (flood (/ width 2) (/ height 2)) (ink (? lime magenta 0) (+ 100 (* kick 155))) (circle (/ width 2) (/ height 2) (+ 3 (* kick 4))) (spin (2s... -1.125 1.125)) (zoom 1.2) (contrast 1.05) (ink yellow (? 32 64 96 255)) (repeat 32 point) (scroll (? 0 0 0 2 -2) (? 0 0 0 2 -2))`,
  ACT_V: `(fade:lime-gray-black-pink) (ink (? lime magenta 0) (+ 160 (* kick 155))) (ink (? lime magenta 0) (+ 45 (* (random) 85))) (flood (/ width 2) (/ height 2)) (ink (? lime magenta 0) (+ 160 (* kick 155))) (box (/ width 2) (/ height 2) (+ 3 (* kick 4)) (+ 3 (* kick 4))) (circle (/ width 2) (/ height 2) (+ 3 (* kick 4))) (spin (1.2s... -2.3 2.3)) (zoom 1.2) (contrast 1.1) (scroll (? 7 -7) (? 6 -6))`,
  ACT_VI: `(fade:lime-gray-black-pink) (ink (? lime magenta 0) (+ 170 (* kick 155))) (ink (? lime magenta 0) (+ 50 (* (random) 90))) (gradient lime black (/ width 2) 0 (/ width 2) height) (ink (? lime magenta 0) (+ 170 (* kick 155))) (point (/ width 3) (/ height 3)) (point (* width 0.66) (* height 0.66)) (line (/ width 2) 0 (/ width 2) height) (spin (1s... -2.5 2.5)) (zoom 1.2) (contrast 1.1) (scroll (? 8 -8) (? 7 -7))`,
  SURPRISE: `(fade:lime-gray-black-pink) (ink (? lime magenta 0) (+ 180 (* kick 175))) (ink (? lime magenta 0) (+ 60 (* (random) 100))) (flood (/ width 2) (/ height 2)) (ink (? lime magenta 0) (+ 180 (* kick 175))) (circle (/ width 2) (/ height 2) (+ 4 (* kick 5))) (poly (/ width 4) (/ height 4) (+ 2 (* kick 3)) 8) (poly (* width 0.75) (* height 0.75) (+ 2 (* kick 3)) 8) (box (/ width 2) (/ height 2) (+ 3 (* kick 4)) (+ 3 (* kick 4))) (spin (0.5s... -3.0 3.0)) (zoom 1.3) (contrast 1.2) (scroll (? 10 -10) (? 9 -9))`,
  PAUSE_2: `(fade:red-white-black-gray) (ink (? red white 0) (+ 160 (* kick 155))) (ink (? red white 0) (+ 45 (* (random) 85))) (gradient red black 0 0 width height) (ink (? red white 0) (+ 160 (* kick 155))) (line 0 0 width height) (line width 0 0 height) (spin (0.2s... -5.0 5.0)) (zoom (0.05s... 2.0 4.0)) (contrast 1.4) (ink (? red white 0) (+ 180 (* kick 120))) (repeat 96 point) (scroll (? 0 0 4 -4) (? 0 0 4 -4))`,
  PAUSE_3: `(fade:magenta-red-yellow-white-black) (ink (? magenta red yellow white) (+ 160 (* kick 95))) (def beat (* kick 12)) (def pulse (* amp 8)) (def cx (/ width 2)) (def cy (/ height 2)) (def t (* frame 0.08)) (ink (? yellow magenta white) (+ 120 (* amp 75))) (def x1 (+ cx (* beat (sin t)))) (def y1 (+ cy (* pulse (cos (* t 1.3))))) (def x2 (+ cx (* pulse (sin (* t 2))))) (def y2 (+ cy (* beat (cos (* t 0.7))))) (tri x1 y1 x2 y2 cx cy) (circle (+ cx (* 20 (sin (* t 3)))) (+ cy (* 15 (cos (* t 2)))) (+ 3 (* kick 5))) (box cx cy (+ beat pulse) (+ beat pulse)) (spin (0.05s... -10.0 10.0)) (zoom (0.01s... 2.8 5.5)) (contrast (+ 1.4 (* kick 0.5))) (repeat (+ 64 (* amp 96)) point) (scroll (* kick 5) (* amp -5))`,
  END: `(fade:lime-gray-black-pink) (ink (? lime magenta 0) (+ 80 (* kick 100))) (ink (? lime magenta 0) (+ 15 (* (random) 40))) (flood (/ width 2) (/ height 2)) (ink (? lime magenta 0) (+ 80 (* kick 100))) (point (/ width 2) (/ height 2)) (spin (3s... -0.5 0.5)) (zoom 1.1) (contrast 1.0) (scroll (? 1 -1) (? 1 -1))`,
  // NEW: Buffering/initialization visual
  BUFFERING: `(fade:orange-yellow-orange) (ink stateColor (+ 80 (* amp 120))) (ink stateColor (+ 30 (* (random) 80))) (flood (/ width 2) (/ height 2)) (ink stateColor 200) (circle (/ width 2) (/ height 2) (+ 8 (* amp 15))) (box (- (/ width 2) 10) (- (/ height 2) 10) 20 20) (spin (0.8s... -3.0 3.0)) (zoom (2s... 1.0 1.5)) (contrast 1.4) (ink stateColor (+ 120 (* amp 100))) (repeat 128 point) (scroll (? 0 0 2 -2) (? 0 0 2 -2))`
};

// Blank KidLisp for tape mode before audio starts
const DEFAULT_BLANK_KIDLISP = `black`;

// FUNCTION TO GET PALETTE FOR CURRENT LOCATOR
function getPaletteForLocator(locatorName) {
  if (!locatorName) return ZZZWAP_PALETTES.START;
  
  const name = locatorName.toUpperCase();
  if (name.includes('ACT VI')) return ZZZWAP_PALETTES.ACT_VI;
  if (name.includes('ACT V')) return ZZZWAP_PALETTES.ACT_V;
  if (name.includes('ACT IIII')) return ZZZWAP_PALETTES.ACT_IIII;
  if (name.includes('ACT III')) return ZZZWAP_PALETTES.ACT_III;
  if (name.includes('ACT II')) return ZZZWAP_PALETTES.ACT_II;
  if (name.includes('ACT I')) return ZZZWAP_PALETTES.ACT_I;
  if (name.includes('SURPRISE')) return ZZZWAP_PALETTES.SURPRISE;
  
  // Handle PAUSE variations with cycling
  if (name.includes('PAUSE')) {
    // First check for explicitly numbered PAUSE variations
    if (name.includes('PAUSE 3') || name.includes('PAUSE3')) return ZZZWAP_PALETTES.PAUSE_3;
    if (name.includes('PAUSE 2') || name.includes('PAUSE2')) return ZZZWAP_PALETTES.PAUSE_2;
    if (name.includes('PAUSE')) {
      // Generic PAUSE - cycle through palettes based on counter
      const pausePalettes = [ZZZWAP_PALETTES.PAUSE, ZZZWAP_PALETTES.PAUSE_2, ZZZWAP_PALETTES.PAUSE_3];
      const paletteIndex = Math.max(0, (pauseCounter - 1)) % pausePalettes.length;
      return pausePalettes[paletteIndex];
    }
  }
  
  if (name.includes('START')) return ZZZWAP_PALETTES.START;
  if (name.includes('END')) return ZZZWAP_PALETTES.END;
  
  return ZZZWAP_PALETTES.START; // Default fallback
}

// FUNCTION TO GET KIDLISP SOURCE FOR CURRENT LOCATOR
function getKidlispSourceForLocator(locatorName, isPlaying = false, audioInitializing = false, inTapeMode = false) {
  // Debug logging for tape mode issues
  if (inTapeMode) {
    console.log(`🎬 KIDLISP_TAPE_MODE: Skipping loading states, using content directly. locator=${locatorName}, isPlaying=${isPlaying}, audioInitializing=${audioInitializing}`);
  }

  // Handle section changes to reset/increment counters (SHARED LOGIC for both modes)
  if (locatorName && locatorName !== previousLocatorName) {
    const name = locatorName.toUpperCase();
    // If we're entering a new PAUSE section, increment the counter
    if (name.includes('PAUSE') && (!previousLocatorName || !previousLocatorName.toUpperCase().includes('PAUSE'))) {
      pauseCounter++;
    }
    // Reset pause counter if we're no longer in a PAUSE section
    if (!name.includes('PAUSE') && previousLocatorName && previousLocatorName.toUpperCase().includes('PAUSE')) {
      pauseCounter = 0;
    }
    previousLocatorName = locatorName;
  }

  // In tape mode, skip all loading/waiting states and go straight to playing content
  // This prevents any play button or loading states from appearing during recording
  if (inTapeMode) {
    // Show black screen until audio actually starts playing
    if (!isPlaying) {
      console.log(`🎬 KIDLISP_TAPE_MODE: Showing black screen until audio starts`);
      return DEFAULT_BLANK_KIDLISP;
    }
    
    // Once audio is playing, use normal locator content
    console.log(`🎬 KIDLISP_TAPE_MODE: Audio playing, using timeline content`);
    
    // If we have a locator, use it; otherwise use START
    if (!locatorName) return ZZZWAP_KIDLISP_SOURCES.START;
    
    const name = locatorName.toUpperCase();
    
    // Return appropriate content for the locator
    if (name.includes('PAUSE')) {
      // Cycle through different PAUSE sources based on counter
      const pauseSources = [
        ZZZWAP_KIDLISP_SOURCES.PAUSE,
        ZZZWAP_KIDLISP_SOURCES.PAUSE_2,
        ZZZWAP_KIDLISP_SOURCES.PAUSE_3
      ];
      const sourceIndex = Math.max(0, (pauseCounter - 1)) % pauseSources.length;
      return pauseSources[sourceIndex];
    }
    
    if (name.includes('ACT_I')) return ZZZWAP_KIDLISP_SOURCES.ACT_I;
    if (name.includes('ACT_II')) return ZZZWAP_KIDLISP_SOURCES.ACT_II;
    if (name.includes('ACT_III')) return ZZZWAP_KIDLISP_SOURCES.ACT_III;
    if (name.includes('INTERMISSION')) return ZZZWAP_KIDLISP_SOURCES.INTERMISSION;
    if (name.includes('END')) return ZZZWAP_KIDLISP_SOURCES.END;
    if (name.includes('START')) return ZZZWAP_KIDLISP_SOURCES.START;
    
    // Default fallback for tape mode
    return ZZZWAP_KIDLISP_SOURCES.START;
  }

  // Normal (non-tape) mode behavior
  // If audio is initializing, always use BUFFERING
  if (audioInitializing) {
    console.log(`🎬 KIDLISP_NORMAL_MODE: Using BUFFERING (audioInitializing=${audioInitializing})`);
    return ZZZWAP_KIDLISP_SOURCES.BUFFERING;
  }
  
  // If not playing, use BEFORE_START
  if (!isPlaying) {
    console.log(`🎬 KIDLISP_NORMAL_MODE: Using BEFORE_START (isPlaying=${isPlaying}, inTapeMode=${inTapeMode})`);
    return ZZZWAP_KIDLISP_SOURCES.BEFORE_START;
  }
  
  if (!locatorName) return ZZZWAP_KIDLISP_SOURCES.START;
  
  const name = locatorName.toUpperCase();
  
  if (name.includes('ACT VI')) return ZZZWAP_KIDLISP_SOURCES.ACT_VI;
  if (name.includes('ACT V')) return ZZZWAP_KIDLISP_SOURCES.ACT_V;
  if (name.includes('ACT IIII')) return ZZZWAP_KIDLISP_SOURCES.ACT_IIII;
  if (name.includes('ACT III')) return ZZZWAP_KIDLISP_SOURCES.ACT_III;
  if (name.includes('ACT II')) return ZZZWAP_KIDLISP_SOURCES.ACT_II;
  if (name.includes('ACT I')) return ZZZWAP_KIDLISP_SOURCES.ACT_I;
  if (name.includes('SURPRISE')) return ZZZWAP_KIDLISP_SOURCES.SURPRISE;
  
  // Handle PAUSE variations with cycling
  if (name.includes('PAUSE')) {
    // First check for explicitly numbered PAUSE variations
    if (name.match(/PAUSE\s*3/i) || name.match(/PAUSE3/i)) return ZZZWAP_KIDLISP_SOURCES.PAUSE_3;
    if (name.match(/PAUSE\s*2/i) || name.match(/PAUSE2/i)) return ZZZWAP_KIDLISP_SOURCES.PAUSE_2;
    if (name.match(/PAUSE(?!\s*\d)/i)) {
      // Generic PAUSE - cycle through sources based on counter
      const pauseSources = [ZZZWAP_KIDLISP_SOURCES.PAUSE, ZZZWAP_KIDLISP_SOURCES.PAUSE_2, ZZZWAP_KIDLISP_SOURCES.PAUSE_3];
      const sourceIndex = Math.max(0, (pauseCounter - 1)) % pauseSources.length;
      return pauseSources[sourceIndex];
    }
  }
  
  if (name.includes('START')) return ZZZWAP_KIDLISP_SOURCES.START;
  if (name.includes('END')) return ZZZWAP_KIDLISP_SOURCES.END;
  
  return ZZZWAP_KIDLISP_SOURCES.START; // Default fallback
}

// Loads specific Ableton project and audio files over network
// Shows only current and next locator during playback

export const nohud = true;

// ALS Project Parser Class - Enhanced with reference tools knowledge
class ALSProject {
  constructor(xmlData) {
    this.rawData = xmlData;
    this.tracks = [];
    this.scenes = [];
    this.locators = [];
    this.clips = [];
    this.devices = [];
    this.warpMarkers = [];
    this.tempo = 120;
    this.timeSignature = { numerator: 4, denominator: 4 };
    this.sampleRate = 44100;
    this.projectDuration = 0;
    
    this.parseXML(xmlData);
  }
  
  parseXML(xmlData) {
    this.parseGlobalSettings(xmlData);
    this.parseTempo(xmlData);
    this.parseTempoChanges(xmlData); // Add tempo changes parsing
    this.parseTracks(xmlData);
    this.parseScenes(xmlData);
    this.parseLocators(xmlData);
    this.parseWarpMarkers(xmlData);
    this.parseClips(xmlData);
    this.parseDevices(xmlData);
    this.projectDuration = this.calculateProjectDuration();
  }
  
  parseGlobalSettings(xmlData) {
    const sampleRateMatch = xmlData.match(/<SampleRate Value="(\d+)"/);
    if (sampleRateMatch) {
      this.sampleRate = parseInt(sampleRateMatch[1]);
    }
  }
  
  parseTempoChanges(xmlData) {
    console.log("Parsing tempo changes...");
    this.tempoChanges = [];
    
    // Look for tempo change events in the arrangement
    const tempoChangeRegex = /<FloatEvent[^>]*Time="([^"]*)"[^>]*Value="([^"]*)"[^>]*\/>/g;
    let match;
    
    while ((match = tempoChangeRegex.exec(xmlData)) !== null) {
      const time = parseFloat(match[1]);
      const tempo = parseFloat(match[2]);
      
      if (!isNaN(time) && !isNaN(tempo)) {
        this.tempoChanges.push({
          time: time,
          beat: this.alsTimeToBeat(time),
          tempo: tempo
        });
      }
    }
    
    // Sort tempo changes by time
    this.tempoChanges.sort((a, b) => a.time - b.time);
    
    // console.log(`Found ${this.tempoChanges.length} tempo changes:`, this.tempoChanges);
    
    // If no tempo changes found, create one at the beginning with the main tempo
    if (this.tempoChanges.length === 0) {
      this.tempoChanges.push({
        time: 0,
        beat: 0,
        tempo: this.tempo
      });
    }
  }
  
  // Get the tempo at a specific beat position
  getTempoAtBeat(beat) {
    let currentTempo = this.tempo;
    
    for (const change of this.tempoChanges) {
      if (change.beat <= beat) {
        currentTempo = change.tempo;
      } else {
        break;
      }
    }
    
    return currentTempo;
  }
  
  // Convert beats to seconds accounting for tempo changes
  beatsToSecondsWithTempoMap(beats) {
    if (this.tempoChanges.length <= 1) {
      // No tempo changes, use simple calculation
      return (beats * 60) / this.tempo;
    }
    
    let seconds = 0;
    let currentBeat = 0;
    
    for (let i = 0; i < this.tempoChanges.length; i++) {
      const change = this.tempoChanges[i];
      const nextChange = this.tempoChanges[i + 1];
      
      const segmentStart = Math.max(currentBeat, change.beat);
      const segmentEnd = nextChange ? Math.min(nextChange.beat, beats) : beats;
      
      if (segmentEnd <= segmentStart) continue;
      
      const segmentBeats = segmentEnd - segmentStart;
      const segmentSeconds = (segmentBeats * 60) / change.tempo;
      seconds += segmentSeconds;
      
      currentBeat = segmentEnd;
      
      if (currentBeat >= beats) break;
    }
    
    return seconds;
  }
  
  parseTempo(xmlData) {
    // Try multiple tempo patterns
    let tempoMatch = xmlData.match(/<Tempo>[\s\S]*?<Manual Value="(\d+\.?\d*)"[\s\S]*?<\/Tempo>/);
    if (!tempoMatch) {
      tempoMatch = xmlData.match(/<Tempo Value="(\d+\.?\d*)"/);
    }
    if (!tempoMatch) {
      tempoMatch = xmlData.match(/<Manual Value="(\d+\.?\d*)"/);
    }
    
    if (tempoMatch) {
      this.tempo = parseFloat(tempoMatch[1]);
      // console.log("Parsed tempo:", this.tempo, "BPM");
    } else {
      // console.log("No tempo found, using default 120 BPM");
      // console.log("Tempo search patterns failed in XML. First 1000 chars:", xmlData.substring(0, 1000));
    }
    
    const timeSignatureMatch = xmlData.match(/<TimeSignature>\s*<Numerator Value="(\d+)"\s*\/>\s*<Denominator Value="(\d+)"\s*\/>\s*<\/TimeSignature>/);
    if (timeSignatureMatch) {
      this.timeSignature = {
        numerator: parseInt(timeSignatureMatch[1]),
        denominator: parseInt(timeSignatureMatch[2])
      };
      console.log("Parsed time signature:", this.timeSignature);
    }
  }
  
  parseTempoChanges(xmlData) {
    // console.log("Parsing tempo changes...");
    this.tempoChanges = [];
    
    // For now, just create a single tempo change at the beginning
    // More complex tempo change parsing can be added later
    this.tempoChanges.push({
      time: 0,
      beat: 0,
      tempo: this.tempo
    });
    
    // console.log(`Using single tempo: ${this.tempo} BPM`);
  }
  
  parseTracks(xmlData) {
    // console.log("Starting enhanced track parsing...");
    
    const trackRegex = /<(MidiTrack|AudioTrack|ReturnTrack|MasterTrack|GroupTrack)[\s\S]*?<\/\1>/g;
    let match;
    let trackIndex = 0;
    
    while ((match = trackRegex.exec(xmlData)) !== null) {
      const trackContent = match[0];
      const trackType = match[1];
      
      const track = {
        index: trackIndex,
        type: trackType,
        name: this.extractTrackName(trackContent) || `${trackType} ${trackIndex + 1}`,
        color: this.extractTrackColor(trackContent),
        muted: this.extractTrackMuted(trackContent),
        solo: this.extractTrackSolo(trackContent),
        clips: []
      };
      
      // console.log(`Track ${trackIndex}: "${track.name}" (${track.type})`);
      this.tracks.push(track);
      trackIndex++;
    }
    
    // console.log(`Parsed ${this.tracks.length} tracks with names`);
  }
  
  parseScenes(xmlData) {
    const sceneRegex = /<Scene[\s\S]*?<\/Scene>/g;
    let match;
    
    while ((match = sceneRegex.exec(xmlData)) !== null) {
      const sceneContent = match[0];
      
      const scene = {
        name: this.extractSceneName(sceneContent) || `Scene ${this.scenes.length + 1}`,
        tempo: this.extractSceneTempo(sceneContent),
        timeSignature: this.extractSceneTimeSignature(sceneContent)
      };
      
      this.scenes.push(scene);
    }
  }
  
  // Parse locators (crucial for song structure visualization)
  parseLocators(xmlData) {
    // console.log("Starting locator parsing...");
    
    // First, let's examine the locators section more carefully
    const locatorsSection = xmlData.match(/<Locators>[\s\S]*?<\/Locators>/);
    if (locatorsSection) {
      // console.log("Found Locators section, length:", locatorsSection[0].length);
      // console.log("Locators section preview:", locatorsSection[0].substring(0, 500));
    }
    
    // Parse individual locator elements with their full content
    const locatorRegex = /<Locator[^>]*>[\s\S]*?<\/Locator>/g;
    let match;
    
    while ((match = locatorRegex.exec(xmlData)) !== null) {
      const locatorContent = match[0];
      // console.log("Processing full locator:", locatorContent.substring(0, 300));
      
      // Extract ID from the opening tag
      const idMatch = locatorContent.match(/<Locator[^>]*Id="([^"]*)"[^>]*>/);
      const locatorId = idMatch ? idMatch[1] : this.locators.length.toString();
      
      // Look for Time child element
      const timeMatch = locatorContent.match(/<Time[^>]*Value="([^"]*)"[^>]*\/>/);
      
      // Look for Name child element  
      const nameMatch = locatorContent.match(/<Name[^>]*Value="([^"]*)"[^>]*\/>/);
      
      if (timeMatch) {
        const timeValue = parseFloat(timeMatch[1]);
        
        // Debug raw locator timing
        // console.log(`Converting locator timing for "${nameMatch ? nameMatch[1] : 'Unnamed'}": ${timeValue}`);
        
        // Use smart conversion for locators too
        const locator = {
          id: locatorId,
          time: timeValue,
          beat: timeValue, // Keep original for reference
          name: nameMatch ? nameMatch[1] : `Locator ${locatorId}`,
          seconds: this.convertALSTimeToSeconds(timeValue, `locator "${nameMatch ? nameMatch[1] : locatorId}"`)
        };
        
        // console.log(`Locator ${locatorId}: ${locator.name} at ${timeValue} = ${locator.seconds.toFixed(2)}s`);
        this.locators.push(locator);
      } else {
        // console.log("No time found in locator:", locatorId);
      }
    }
    
    // console.log(`Total locators parsed: ${this.locators.length}`);
    
    // Sort locators by time
    this.locators.sort((a, b) => a.time - b.time);
    
    // If still no locators found, create some test locators
    if (this.locators.length === 0) {
      // console.log("No locators found, creating test locators...");
      this.locators = [
        { id: "0", time: 0, beat: 0, name: "START", seconds: 0 },
        { id: "1", time: 480, beat: 480, name: "Verse", seconds: this.beatsToSeconds(480) },
        { id: "2", time: 960, beat: 960, name: "Chorus", seconds: this.beatsToSeconds(960) },
        { id: "3", time: 1440, beat: 1440, name: "Bridge", seconds: this.beatsToSeconds(1440) },
        { id: "4", time: 1920, beat: 1920, name: "End", seconds: this.beatsToSeconds(1920) }
      ];
      // console.log("Created test locators:", this.locators);
    }
  }
  
  // Parse warp markers (audio time manipulation)
  parseWarpMarkers(xmlData) {
    const warpRegex = /<WarpMarker[\s\S]*?\/>/g;
    let match;
    
    while ((match = warpRegex.exec(xmlData)) !== null) {
      const warpContent = match[0];
      
      const timeMatch = warpContent.match(/SecTime="(\d+\.?\d*)"/);
      const beatTimeMatch = warpContent.match(/BeatTime="(\d+\.?\d*)"/);
      
      if (timeMatch && beatTimeMatch) {
        const warpMarker = {
          secTime: parseFloat(timeMatch[1]),
          beatTime: parseFloat(beatTimeMatch[1])
        };
        
        this.warpMarkers.push(warpMarker);
      }
    }
  }
  
  parseClips(xmlData) {
    this.parseArrangementClips(xmlData);
    this.parseMIDIClips(xmlData);
    this.parseAudioClips(xmlData);
    
    // Enhanced clip parsing for timeline visualization
    this.parseEnhancedClips(xmlData);
  }
  
  parseArrangementClips(xmlData) {
    const clipRegex = /<ClipSlot[\s\S]*?<\/ClipSlot>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipContent = match[0];
      
      if (clipContent.includes('<MidiClip') || clipContent.includes('<AudioClip')) {
        const clip = {
          type: clipContent.includes('<MidiClip') ? 'midi' : 'audio',
          time: this.extractClipTime(clipContent),
          duration: this.extractClipDuration(clipContent),
          name: this.extractClipName(clipContent),
          trackIndex: this.findTrackForClip(clipContent)
        };
        
        this.clips.push(clip);
      }
    }
  }
  
  // Enhanced clip parsing for better visualization
  parseEnhancedClips(xmlData) {
    // console.log("Parsing enhanced clips for visualization...");
    
    // Parse TakeLanes which contain arrangement clips
    const takeLaneRegex = /<TakeLane[\s\S]*?<\/TakeLane>/g;
    let takeLaneMatch;
    let trackIndex = 0;
    
    while ((takeLaneMatch = takeLaneRegex.exec(xmlData)) !== null) {
      const takeLaneContent = takeLaneMatch[0];
      
      // Find clips within this take lane
      const clipRegex = /<(MidiClip|AudioClip)[\s\S]*?<\/\1>/g;
      let clipMatch;
      
      while ((clipMatch = clipRegex.exec(takeLaneContent)) !== null) {
        const clipContent = clipMatch[0];
        const clipType = clipMatch[1];
        
        // Extract detailed clip information
        const clip = {
          id: this.extractClipId(clipContent),
          type: clipType.toLowerCase(),
          trackIndex: trackIndex,
          trackId: this.findTrackIdForClip(clipContent),
          currentStart: this.extractClipCurrentStart(clipContent),
          currentEnd: this.extractClipCurrentEnd(clipContent),
          loopStart: this.extractClipLoopStart(clipContent),
          loopEnd: this.extractClipLoopEnd(clipContent),
          startRelative: this.extractClipStartRelative(clipContent),
          time: this.extractClipTime(clipContent),
          name: this.extractClipName(clipContent) || `${clipType} Clip`,
          color: this.extractClipColor(clipContent),
          notes: clipType === 'MidiClip' ? this.extractMIDINotes(clipContent) : [],
          isLooping: this.extractClipIsLooping(clipContent),
          isActive: false, // Will be calculated based on current time
          velocity: clipType === 'MidiClip' ? this.extractClipVelocity(clipContent) : null
        };
        
        // Convert timing to seconds for visualization using smart conversion
        const rawStart = clip.currentStart || clip.time || 0;
        const rawEnd = clip.currentEnd || (clip.currentStart + (clip.loopEnd - clip.loopStart)) || 0;
        
        clip.startSeconds = this.convertALSTimeToSeconds(rawStart, `clip "${clip.name}" start`);
        clip.endSeconds = this.convertALSTimeToSeconds(rawEnd, `clip "${clip.name}" end`);
        clip.duration = clip.endSeconds - clip.startSeconds;
        
        this.clips.push(clip);
        // console.log(`Enhanced clip: ${clip.name} on track ${clip.trackIndex}, ${clip.startSeconds.toFixed(2)}s - ${clip.endSeconds.toFixed(2)}s`);
      }
      
      trackIndex++;
    }
    
    // console.log(`Parsed ${this.clips.length} enhanced clips across ${trackIndex} tracks`);
  }

  parseMIDIClips(xmlData) {
    const clipRegex = /<MidiClip[\s\S]*?<\/MidiClip>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipContent = match[0];
      
      const clip = {
        type: 'midi',
        time: this.extractClipTime(clipContent),
        duration: this.extractClipDuration(clipContent),
        name: this.extractClipName(clipContent),
        notes: this.extractMIDINotes(clipContent),
        trackIndex: this.findTrackForClip(clipContent)
      };
      
      this.clips.push(clip);
    }
  }
  
  parseAudioClips(xmlData) {
    const clipRegex = /<AudioClip[\s\S]*?<\/AudioClip>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipContent = match[0];
      
      const clip = {
        type: 'audio',
        time: this.extractClipTime(clipContent),
        duration: this.extractClipDuration(clipContent),
        name: this.extractClipName(clipContent),
        sampleRef: this.extractSampleRef(clipContent),
        warpMarkers: this.extractWarpMarkers(clipContent),
        trackIndex: this.findTrackForClip(clipContent)
      };
      
      this.clips.push(clip);
    }
  }
  
  parseDevices(xmlData) {
    const deviceRegex = /<(PluginDevice|InstrumentRack|Operator|Simpler|Impulse)[\s\S]*?<\/\1>/g;
    let match;
    
    while ((match = deviceRegex.exec(xmlData)) !== null) {
      const deviceContent = match[0];
      const deviceType = match[1];
      
      const device = {
        type: deviceType,
        enabled: this.extractDeviceEnabled(deviceContent),
        parameters: this.extractDeviceParameters(deviceContent)
      };
      
      this.devices.push(device);
    }
  }
  
  // Helper extraction methods
  extractTrackName(content) {
    const nameMatch = content.match(/<EffectiveName Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : null;
  }
  
  extractTrackColor(content) {
    const colorMatch = content.match(/<ColorIndex Value="(\d+)"/);
    return colorMatch ? parseInt(colorMatch[1]) : 0;
  }
  
  extractTrackMuted(content) {
    const mutedMatch = content.match(/<TrackIsMuted Value="(true|false)"/);
    return mutedMatch ? mutedMatch[1] === 'true' : false;
  }
  
  extractTrackSolo(content) {
    const soloMatch = content.match(/<TrackIsSoloed Value="(true|false)"/);
    return soloMatch ? soloMatch[1] === 'true' : false;
  }
  
  // Enhanced clip extraction methods
  extractClipId(content) {
    const idMatch = content.match(/<(MidiClip|AudioClip)[^>]*Id="([^"]*)"/);
    return idMatch ? idMatch[2] : null;
  }
  
  extractClipCurrentStart(content) {
    const startMatch = content.match(/<CurrentStart Value="([^"]*)"/);
    return startMatch ? parseFloat(startMatch[1]) : 0;
  }
  
  extractClipCurrentEnd(content) {
    const endMatch = content.match(/<CurrentEnd Value="([^"]*)"/);
    return endMatch ? parseFloat(endMatch[1]) : 0;
  }
  
  extractClipLoopStart(content) {
    const loopStartMatch = content.match(/<LoopStart Value="([^"]*)"/);
    return loopStartMatch ? parseFloat(loopStartMatch[1]) : 0;
  }
  
  extractClipLoopEnd(content) {
    const loopEndMatch = content.match(/<LoopEnd Value="([^"]*)"/);
    return loopEndMatch ? parseFloat(loopEndMatch[1]) : 0;
  }
  
  extractClipStartRelative(content) {
    const startRelMatch = content.match(/<StartRelative Value="([^"]*)"/);
    return startRelMatch ? parseFloat(startRelMatch[1]) : 0;
  }
  
  extractClipTime(content) {
    const timeMatch = content.match(/<CurrentStart Value="(\d+\.?\d*)"/);
    return timeMatch ? parseFloat(timeMatch[1]) : 0;
  }
  
  extractClipDuration(content) {
    const durationMatch = content.match(/<Length Value="(\d+\.?\d*)"/);
    return durationMatch ? parseFloat(durationMatch[1]) : 0;
  }
  
  extractClipName(content) {
    const nameMatch = content.match(/<Name Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : null;
  }
  
  extractClipColor(content) {
    const colorMatch = content.match(/<Color Value="(\d+)"/);
    return colorMatch ? parseInt(colorMatch[1]) : Math.floor(Math.random() * 64);
  }
  
  extractClipIsLooping(content) {
    const loopMatch = content.match(/<IsLooping Value="(true|false)"/);
    return loopMatch ? loopMatch[1] === 'true' : true;
  }
  
  extractClipVelocity(content) {
    // Extract average velocity from MIDI notes
    const notes = this.extractMIDINotes(content);
    if (notes.length > 0) {
      const avgVelocity = notes.reduce((sum, note) => sum + note.velocity, 0) / notes.length;
      return Math.round(avgVelocity);
    }
    return 100;
  }
  
  findTrackIdForClip(content) {
    // This would need more sophisticated logic based on XML structure
    return null;
  }
  
  extractMIDINotes(clipContent) {
    const notes = [];
    const keyTrackRegex = /<KeyTrack[\s\S]*?<\/KeyTrack>/g;
    let keyTrackMatch;
    
    while ((keyTrackMatch = keyTrackRegex.exec(clipContent)) !== null) {
      const keyTrackContent = keyTrackMatch[0];
      const noteRegex = /<MidiNoteEvent[\s\S]*?\/>/g;
      let noteMatch;
      
      while ((noteMatch = noteRegex.exec(keyTrackContent)) !== null) {
        const noteElement = noteMatch[0];
        
        const timeMatch = noteElement.match(/Time="(\d+\.?\d*)"/);
        const durationMatch = noteElement.match(/Duration="(\d+\.?\d*)"/);
        const velocityMatch = noteElement.match(/Velocity="(\d+)"/);
        
        if (timeMatch && durationMatch) {
          const note = {
            time: parseFloat(timeMatch[1]),
            duration: parseFloat(durationMatch[1]),
            velocity: velocityMatch ? parseInt(velocityMatch[1]) : 100,
            pitch: this.extractNotePitch(noteElement)
          };
          
          notes.push(note);
        }
      }
    }
    
    return notes;
  }
  
  extractNotePitch(noteElement) {
    const keyMatch = noteElement.match(/Key="(\d+)"/);
    return keyMatch ? parseInt(keyMatch[1]) : 60; // Default to middle C
  }
  
  findArrangementTrackForClip(takeLaneContent) {
    // This would need more complex logic to match clips to tracks
    return 0;
  }
  
  extractLoopData(content) {
    const loopMatch = content.match(/<Loop[\s\S]*?<\/Loop>/);
    if (loopMatch) {
      const startMatch = loopMatch[0].match(/<StartRelative Value="(\d+\.?\d*)"/);
      const endMatch = loopMatch[0].match(/<EndRelative Value="(\d+\.?\d*)"/);
      
      return {
        start: startMatch ? parseFloat(startMatch[1]) : 0,
        end: endMatch ? parseFloat(endMatch[1]) : 0
      };
    }
    return null;
  }
  
  extractSampleRef(content) {
    const sampleRefMatch = content.match(/<SampleRef[\s\S]*?<\/SampleRef>/);
    if (sampleRefMatch) {
      const fileRefMatch = sampleRefMatch[0].match(/<FileRef[\s\S]*?<\/FileRef>/);
      if (fileRefMatch) {
        const pathMatch = fileRefMatch[0].match(/<Path Value="([^"]*)"/);
        return pathMatch ? pathMatch[1] : null;
      }
    }
    return null;
  }
  
  extractWarpMarkers(content) {
    const markers = [];
    const markerRegex = /<WarpMarker[\s\S]*?\/>/g;
    let match;
    
    while ((match = markerRegex.exec(content)) !== null) {
      const markerElement = match[0];
      const secTimeMatch = markerElement.match(/SecTime="(\d+\.?\d*)"/);
      const beatTimeMatch = markerElement.match(/BeatTime="(\d+\.?\d*)"/);
      
      if (secTimeMatch && beatTimeMatch) {
        markers.push({
          secTime: parseFloat(secTimeMatch[1]),
          beatTime: parseFloat(beatTimeMatch[1])
        });
      }
    }
    
    return markers;
  }
  
  extractValueFromContent(content, tagName) {
    const regex = new RegExp(`<${tagName} Value="([^"]*)"`, 'i');
    const match = content.match(regex);
    return match ? match[1] : null;
  }
  
  extractDeviceEnabled(content) {
    const enabledMatch = content.match(/<On Value="(true|false)"/);
    return enabledMatch ? enabledMatch[1] === 'true' : true;
  }
  
  extractDeviceParameters(content) {
    const parameters = [];
    const paramRegex = /<Parameter[\s\S]*?<\/Parameter>/g;
    let match;
    
    while ((match = paramRegex.exec(content)) !== null) {
      const paramContent = match[0];
      const nameMatch = paramContent.match(/<Name Value="([^"]*)"/);
      const valueMatch = paramContent.match(/<UserValue Value="(\d+\.?\d*)"/);
      
      if (nameMatch && valueMatch) {
        parameters.push({
          name: nameMatch[1],
          value: parseFloat(valueMatch[1])
        });
      }
    }
    
    return parameters;
  }
  
  findArrangementTrackForClip(clipContent) {
    // Simplified - would need more complex logic in real implementation
    return Math.floor(Math.random() * this.tracks.length);
  }
  
  findTrackForClip(clipContent) {
    // Simplified - would need more complex logic in real implementation
    return Math.floor(Math.random() * this.tracks.length);
  }
  
  extractSceneName(content) {
    const nameMatch = content.match(/<Name Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : null;
  }
  
  extractSceneTempo(content) {
    const tempoMatch = content.match(/<Tempo Value="(\d+\.?\d*)"/);
    return tempoMatch ? parseFloat(tempoMatch[1]) : null;
  }
  
  extractSceneTimeSignature(content) {
    const timeSignatureMatch = content.match(/<TimeSignature>\s*<Numerator Value="(\d+)"\s*\/>\s*<Denominator Value="(\d+)"\s*\/>\s*<\/TimeSignature>/);
    if (timeSignatureMatch) {
      return {
        numerator: parseInt(timeSignatureMatch[1]),
        denominator: parseInt(timeSignatureMatch[2])
      };
    }
    return null;
  }
  
  getTrackColor(type) {
    const colors = {
      'MidiTrack': [100, 149, 237],
      'AudioTrack': [34, 139, 34],
      'ReturnTrack': [255, 140, 0],
      'MasterTrack': [220, 20, 60]
    };
    return colors[type] || [128, 128, 128];
  }
  
  // Calculate current beat position from audio time
  getCurrentBeat(audioTime) {
    return (audioTime / 60) * this.tempo;
  }
  
  // Calculate the actual project duration from clips and locators
  calculateProjectDuration() {
    let maxDuration = 0;
    
    // Check clips
    for (const clip of this.clips) {
      const clipEnd = clip.time + clip.duration;
      if (clipEnd > maxDuration) {
        maxDuration = clipEnd;
      }
    }
    
    // Check locators
    for (const locator of this.locators) {
      if (locator.time > maxDuration) {
        maxDuration = locator.time;
      }
    }
    
    return maxDuration;
  }
  
  // Convert ALS time units to beats (improved conversion)
  alsTimeToBeat(alsTime) {
    // In Ableton Live, different time values use different units:
    // - Locator Time values are typically in beats (quarter notes)  
    // - Clip CurrentStart/CurrentEnd are often in seconds or sample ticks
    // We need to normalize these. For now, try direct beats assumption.
    return alsTime;
  }

  // Convert beats to actual time in seconds
  beatsToSeconds(beats) {
    // Correct formula: seconds = (beats * 60) / BPM
    return (beats * 60) / this.tempo;
  }

  // Smart time conversion that handles different ALS time units
  convertALSTimeToSeconds(rawTime, context = 'clip') {
    if (rawTime === undefined || rawTime === null) return 0;
    
    // Apply different conversion strategies based on the magnitude of the value
    if (rawTime >= 10000) {
      // Very large values (>= 10000) are likely in ticks (480 ticks per beat in Live)
      const beats = rawTime / 480;
      const seconds = this.beatsToSeconds(beats);
      console.log(`  ${context}: ${rawTime} ticks → ${beats.toFixed(2)} beats → ${seconds.toFixed(2)} seconds`);
      return seconds;
    } else if (rawTime >= 500) {
      // Medium values (500-9999) might be in seconds already or high-resolution beats
      // Check if treating as seconds gives reasonable values
      if (rawTime < 1000) {
        // 500-1000 range, could be seconds
        // console.log(`  ${context}: ${rawTime} assumed as seconds`);
        return rawTime;
      } else {
        // > 1000, likely high-res beats or ticks
        const beats = rawTime / 480; // Try tick conversion
        const seconds = this.beatsToSeconds(beats);
        // console.log(`  ${context}: ${rawTime} treated as ticks → ${beats.toFixed(2)} beats → ${seconds.toFixed(2)} seconds`);
        return seconds;
      }
    } else {
      // Small values (< 500) are likely beats
      const seconds = this.beatsToSeconds(rawTime);
      // console.log(`  ${context}: ${rawTime} beats → ${seconds.toFixed(2)} seconds`);
      return seconds;
    }
  }  // Get clips that are active at the current time
  getActiveClips(currentTimeSeconds) {
    return this.clips.filter(clip => {
      if (!clip.startSeconds || !clip.endSeconds) return false;
      
      // Check if current time is within clip bounds
      const isWithinTime = currentTimeSeconds >= clip.startSeconds && currentTimeSeconds <= clip.endSeconds;
      
      // For looping clips, check if it's within the loop boundaries
      if (clip.isLooping && isWithinTime) {
        const clipProgress = (currentTimeSeconds - clip.startSeconds) / (clip.endSeconds - clip.startSeconds);
        const loopDuration = clip.loopEnd - clip.loopStart;
        const clipDuration = clip.endSeconds - clip.startSeconds;
        
        if (loopDuration > 0 && clipDuration > 0) {
          // Calculate position within the loop
          const loopProgress = (clipProgress * clipDuration) % loopDuration;
          return loopProgress >= 0 && loopProgress <= loopDuration;
        }
      }
      
      return isWithinTime;
    });
  }
  
  // Get MIDI notes that should be playing at the current time (SIMPLIFIED)
  getActiveNotes(currentTimeSeconds) {
    const activeNotes = [];
    
    this.clips.forEach(clip => {
      if (clip.type !== 'midiclip' || !clip.notes) return;
      
      // Skip clips with weird timing
      if (!clip.startSeconds && clip.startSeconds !== 0) return;
      
      clip.notes.forEach(note => {
        const noteStartTime = clip.startSeconds + this.beatsToSeconds(this.alsTimeToBeat(note.time || 0));
        const noteEndTime = noteStartTime + this.beatsToSeconds(this.alsTimeToBeat(note.duration || 0.25));
        
        // Check if note is currently playing
        if (currentTimeSeconds >= noteStartTime && currentTimeSeconds <= noteEndTime) {
          activeNotes.push({
            ...note,
            clipName: clip.name,
            trackIndex: clip.trackIndex,
            globalStartTime: noteStartTime,
            globalEndTime: noteEndTime,
            timeInNote: currentTimeSeconds - noteStartTime
          });
        }
      });
    });
    
    return activeNotes;
  }
  
  // Get active clips at a specific beat position
  getActiveClipsAtBeat(beatPosition) {
    return this.clips.filter(clip => {
      const clipStart = this.alsTimeToBeat(clip.time);
      const clipEnd = clipStart + this.alsTimeToBeat(clip.duration);
      return beatPosition >= clipStart && beatPosition < clipEnd;
    });
  }
  
  // Get active MIDI notes at a specific beat position
  getActiveNotesAtBeat(beatPosition) {
    const activeNotes = [];
    
    for (const clip of this.clips) {
      if (clip.type === 'midi' && clip.notes) {
        const clipStart = this.alsTimeToBeat(clip.time);
        
        for (const note of clip.notes) {
          const noteStart = clipStart + this.alsTimeToBeat(note.time);
          const noteEnd = noteStart + this.alsTimeToBeat(note.duration);
          
          if (beatPosition >= noteStart && beatPosition < noteEnd) {
            activeNotes.push({
              ...note,
              absoluteStart: noteStart,
              absoluteEnd: noteEnd,
              clipIndex: this.clips.indexOf(clip)
            });
          }
        }
      }
    }
    
    return activeNotes;
  }
  
  // Get current and next locator at a specific time position
  getCurrentLocator(currentTimeSeconds, actualDuration) {
    let currentLocator = null;
    let nextLocator = null;
    let currentIndex = -1;
    
    // Use locator times directly - no scaling needed since audio is at correct 143 BPM
    
    for (let i = 0; i < this.locators.length; i++) {
      const locator = this.locators[i];
      
      if (locator.seconds <= currentTimeSeconds) {
        currentLocator = locator;
        currentIndex = i;
        
        // Log when we pass a locator (only once per locator)
        if (!passedLocators.has(i)) {
          passedLocators.add(i);
          const timeDiff = currentTimeSeconds - locator.seconds;
          
          // Calculate what this timing should be based on pure audio progress
          const unscaledAudioTime = currentAudioProgress * actualDuration;
          const beatsBasedOnAudio = (unscaledAudioTime * this.tempo) / 60;
          
          // Calculate the beat offset between project and audio
          const beatOffset = locator.time - beatsBasedOnAudio;
          
          // console.log(`🎯 LOCATOR PASSED: "${locator.name}" 
          //   Expected: ${locator.seconds.toFixed(2)}s (no scaling)
          //   Actual: ${currentTimeSeconds.toFixed(2)}s
          //   Audio Time: ${unscaledAudioTime.toFixed(2)}s
          //   Audio Beats: ${beatsBasedOnAudio.toFixed(2)} vs Project Beats: ${locator.time.toFixed(2)}
          //   Beat Offset: ${beatOffset.toFixed(2)} beats (${(beatOffset * 60 / this.tempo).toFixed(2)}s)
          //   Drift: ${timeDiff >= 0 ? '+' : ''}${timeDiff.toFixed(2)}s 
          //   Beat: ${locator.time.toFixed(2)} @ ${this.tempo}BPM`);
        }
      } else if (!nextLocator && currentTimeSeconds < locator.seconds) {
        nextLocator = locator;
        break;
      }
    }
    
    return { current: currentLocator, next: nextLocator, currentIndex };
  }
}

// State
let alsProject = null;
let wavFile = null;
let preloadedAudio = null;
let isPlaying = false;
let audioInitializing = false; // NEW: Track when audio is decoding/worklet loading
let playingSfx = null;
let playStartTime = 0;
let audioStartTime = 0;
let message = "Loading zzzZWAP project...";
let netAPI = null;
let progress = 0;
let actualDuration = null;
let passedLocators = new Set(); // Track which locators we've passed
let currentAudioProgress = null; // Store the actual audio progress from speaker worklet
let timelineOffset = 2.0; // Look ahead by this many seconds (adjustable with +/-)
let isBuffering = false; // Track loading state

// Boot function to load files over network
export const boot = async ({ net }) => {
  netAPI = net;
  
  try {
    // Load the .als file
    // console.log("Loading zzzZWAP.als...");
    const alsResponse = await fetch("https://assets.aesthetic.computer/wipppps/zzzZWAP.als");
    const alsArrayBuffer = await alsResponse.arrayBuffer();
    
    // Try to decompress using native browser decompression (if supported)
    let alsText;
    try {
      // console.log("Attempting to decompress ALS file using DecompressionStream...");
      const stream = new Response(alsArrayBuffer).body;
      const decompressedStream = stream.pipeThrough(new DecompressionStream('gzip'));
      const decompressedResponse = new Response(decompressedStream);
      alsText = await decompressedResponse.text();
      // console.log("Successfully decompressed ALS file");
    } catch (decompressError) {
      console.log("Native decompression failed, trying as plain text:", decompressError);
      // Fallback: try as plain text
      alsText = new TextDecoder().decode(alsArrayBuffer);
    }
    
    // Parse the ALS project
    alsProject = new ALSProject(alsText);
    // console.log("ALS project loaded:", alsProject);
    // console.log("Locators found:", alsProject.locators.length);
    // console.log("First 1000 chars of ALS file:", alsText.substring(0, 1000));
    
    // Load the audio file with progress tracking for mobile
    console.log("🎵 Loading zzzZWAP.wav...");
    message = "Loading audio...";
    const audioLoadStartTime = performance.now();
    
    try {
      // Load with progress callback for better mobile UX
      preloadedAudio = await net.preload("https://assets.aesthetic.computer/wipppps/zzzZWAP.wav", true, (progress) => {
        if (progress && progress.loaded && progress.total) {
          const percent = Math.round((progress.loaded / progress.total) * 100);
          message = `Loading audio: ${percent}%`;
          console.log(`🎵 Audio download progress: ${percent}%`);
        }
      });
      
      const audioLoadTime = performance.now() - audioLoadStartTime;
      
      // 🎵 ENHANCED AUDIO VALIDATION FOR MOBILE
      if (!preloadedAudio) {
        throw new Error("Failed to load audio file");
      }
      
      console.log(`🎵 Audio preload completed in ${audioLoadTime.toFixed(0)}ms`);
      console.log("🎵 Preloaded audio object:", preloadedAudio);
      
      // Try to determine duration from various sources
      let durationSources = [];
      
      // Check for buffer property first (Web Audio API format)
      if (preloadedAudio.buffer && preloadedAudio.buffer.duration) {
        actualDuration = preloadedAudio.buffer.duration;
        durationSources.push(`buffer: ${actualDuration.toFixed(3)}s`);
      }
      // Check for direct duration property
      else if (preloadedAudio.duration) {
        actualDuration = preloadedAudio.duration;
        durationSources.push(`direct: ${actualDuration.toFixed(3)}s`);
      }
      // Check if it's just a string (URL) and we need to handle it differently
      else if (typeof preloadedAudio === 'string') {
        console.log("🎵 Audio returned as URL string:", preloadedAudio);
        // For now, use fallback duration - we'll get the real one when playing
        actualDuration = 183.8; // Known duration of zzzZWAP.wav
        durationSources.push(`fallback: ${actualDuration.toFixed(3)}s`);
      }
      else {
        console.warn("🎵 Could not determine duration from preloaded audio");
        console.log("🎵 Audio object properties:", Object.keys(preloadedAudio));
        actualDuration = 183.8; // Fallback duration
        durationSources.push(`unknown_fallback: ${actualDuration.toFixed(3)}s`);
      }
      
      console.log(`🎵 Duration sources: [${durationSources.join(', ')}] → final: ${actualDuration.toFixed(3)}s`);
      
      message = "Ready! Tap to play";
      console.log("🎵 All files loaded successfully!");
      
    } catch (audioError) {
      console.error("🎵 Audio loading failed:", audioError);
      message = "Audio loading failed - check connection";
      preloadedAudio = null;
    }
    
  } catch (error) {
    console.error("Error loading files:", error);
    message = "Error loading files";
  }
};

// Initialize
// console.log("🎵 VISUALIZER.MJS: zzzZWAP visualizer loaded and ready!");

// TV bars painting buffer - module-level to persist between frames
let tvBarsBuffer = null;
let lastScreenWidth = 0;
let lastScreenHeight = 0;

function paint({ wipe, ink, screen, sound, paintCount, clock, write, box, line, typeface, painting, page, paste, blur, zoom, scroll, poly, shape, kidlisp, flood, api }) {
  // Basic debug to confirm piece is running
  if (paintCount === 1) {
    console.log(`🎬 WIPPPPS_PIECE_LOADED: Paint function started for wipppps piece`);
  }

  // Check for tape recording mode early for KidLisp source selection
  // Use direct API access for immediate detection without frame delay
  const directRecordingState = api?.rec?.recording === true;
  const inTapeMode = directRecordingState || isCurrentlyRecording || tapeAutoPlayStarted;

  // Debug logging for tape mode detection (only log occasionally to avoid spam)
  if (paintCount % 60 === 0) { // Log every ~1 second at 60fps
    console.log(`🎬 TAPE_MODE_DEBUG: inTapeMode=${inTapeMode}, directRecordingState=${directRecordingState}, isCurrentlyRecording=${isCurrentlyRecording}, tapeAutoPlayStarted=${tapeAutoPlayStarted}`);
  }

  // In tape mode before audio starts, show completely black screen
  if (inTapeMode && !isPlaying) {
    wipe(0, 0, 0, 255); // Complete black screen
    return; // Skip all other rendering
  }

  // Initialize or recreate TV bars buffer if needed (first time or screen size changed)
  if (!tvBarsBuffer || screen.width !== lastScreenWidth || screen.height !== lastScreenHeight) {
    // TV bars buffer is now 24px shorter to fit under KidLisp
    tvBarsBuffer = painting(screen.width, 24, (api) => {
      // Initialize with transparent background
      api.wipe(0, 0, 0, 0);
    });
    lastScreenWidth = screen.width;
    lastScreenHeight = screen.height;
  }

  // Global TV color tracking
  let globalCurrentColor = null;
  
  // Function to generate unique color for each locator
  function getLocatorColor(locator, index) {
    // Add null safety check
    if (!locator || !locator.name) {
      // Return a default color if locator is invalid
      return { r: 100, g: 100, b: 100 };
    }
    
    // Use locator name hash + index for unique colors
    const nameHash = locator.name.split('').reduce((hash, char) => hash + char.charCodeAt(0), 0);
    const hue = ((nameHash * 37) + (index * 60)) % 360; // Spread colors across spectrum
    const saturation = 0.6; // Softer saturation for ambient colors
    const lightness = 0.4; // Medium brightness
    
    // Convert HSL to RGB
    const c = (1 - Math.abs(2 * lightness - 1)) * saturation;
    const x = c * (1 - Math.abs(((hue / 60) % 2) - 1));
    const m = lightness - c / 2;
    
    let r, g, b;
    if (hue < 60) { r = c; g = x; b = 0; }
    else if (hue < 120) { r = x; g = c; b = 0; }
    else if (hue < 180) { r = 0; g = c; b = x; }
    else if (hue < 240) { r = 0; g = x; b = c; }
    else if (hue < 300) { r = x; g = 0; b = c; }
    else { r = c; g = 0; b = x; }
    
    r = Math.floor((r + m) * 255);
    g = Math.floor((g + m) * 255);
    b = Math.floor((b + m) * 255);
    
    return {r, g, b, name: locator.name};
  }
  
  // If files aren't loaded yet, show loading message on black background
  if (!alsProject || !preloadedAudio) {
    wipe(0, 0, 0);
    ink(255, 255, 255);
    try {
      write(message, 20, screen.height / 2);
    } catch (e) {
      // Fallback if write fails - just continue without text
    }
    return;
  }
  
  // Update progress with audio-driven timing (audio is master clock)
  if (isPlaying && playingSfx && actualDuration) {
    // Use actual audio progress as the master timing source, but validate it first
    if (currentAudioProgress !== null && currentAudioProgress > 0) {
      // Check if audio progress seems reasonable for a resume scenario
      const expectedProgress = pausedAt; // Where we should be based on last pause
      const progressDiff = Math.abs(currentAudioProgress - expectedProgress);
      
      // Calculate what manual timing says we should be at
      const timeSincePlay = playStartTime ? (performance.now() - playStartTime) / 1000 : 0;
      const sessionElapsed = (performance.now() - playStartTime) / 1000;
      const manualProgress = Math.min((pausedAt * actualDuration + sessionElapsed) / actualDuration, 1);
      const manualAudioDiff = Math.abs(currentAudioProgress - manualProgress);
      
      // More aggressive protection: reject audio progress if it's inconsistent
      const isRecentResume = timeSincePlay < 10.0; // Extended to 10 seconds for more protection
      const isProgressSuspicious = (pausedAt > 0 && progressDiff > 0.01) || manualAudioDiff > 0.01; // Very tight 1% threshold
      
      if (isRecentResume && isProgressSuspicious) {
        // Audio progress seems wrong - use manual timing to maintain visual continuity
        progress = manualProgress;
        
        // Log the decision to override audio progress
        if (paintCount % 30 === 0) { // More frequent logging during overrides
          console.log(`🎵 VISUAL_CONTINUITY: Using manual=${manualProgress.toFixed(6)} instead of audio=${currentAudioProgress.toFixed(6)} (pauseDiff=${progressDiff.toFixed(6)}, manualDiff=${manualAudioDiff.toFixed(6)}, timeSincePlay=${timeSincePlay.toFixed(3)}s, pausedAt=${pausedAt.toFixed(6)})`);
        }
      } else {
        // Audio progress seems trustworthy - transition gradually
        const oldProgress = progress;
        
        // If this is a big jump, ease into it rather than jumping immediately
        if (Math.abs(oldProgress - currentAudioProgress) > 0.02) { // 2% threshold for easing
          // Ease towards audio progress instead of jumping
          const easeRate = 0.1; // 10% per frame
          progress = oldProgress + (currentAudioProgress - oldProgress) * easeRate;
          
          if (paintCount % 60 === 0) {
            console.log(`🎵 EASING_TO_AUDIO: from=${oldProgress.toFixed(6)} to=${currentAudioProgress.toFixed(6)} current=${progress.toFixed(6)} (easing)`);
          }
        } else {
          // Small difference, safe to use audio progress directly
          progress = currentAudioProgress;
          
          // Log when we switch to audio-driven mode
          if (Math.abs(oldProgress - progress) > 0.01 && paintCount % 60 === 0) {
            console.log(`🎵 AUDIO_DRIVEN: switched from manual=${oldProgress.toFixed(6)} to audio=${progress.toFixed(6)} (manualDiff=${manualAudioDiff.toFixed(6)}, timeSincePlay=${timeSincePlay.toFixed(3)}s)`);
          }
        }
      }
    } else {
      // Fallback to manual timing if audio progress isn't available yet
      const sessionElapsed = (performance.now() - playStartTime) / 1000;
      const currentTime = (pausedAt * actualDuration) + sessionElapsed;
      const newProgress = Math.min(currentTime / actualDuration, 1);
      
      // Only use manual timing if we don't have audio progress
      if (currentAudioProgress === null) {
        progress = newProgress;
        
        // Log when using manual timing
        if (paintCount % 300 === 0) {
          console.log(`🎵 MANUAL_TIMING: progress=${progress.toFixed(6)}, elapsed=${sessionElapsed.toFixed(3)}s, waiting for audio progress`);
        }
      }
    }
    
    // Log sync comparison for debugging
    if (currentAudioProgress !== null && paintCount % 300 === 0) {
      const sessionElapsed = (performance.now() - playStartTime) / 1000;
      const manualProgress = Math.min(((pausedAt * actualDuration) + sessionElapsed) / actualDuration, 1);
      const audioProgress = currentAudioProgress;
      const progressDrift = Math.abs(manualProgress - audioProgress);
      
      // console.log(`🎵 SYNC_COMPARISON: manual=${manualProgress.toFixed(6)}, audio=${audioProgress.toFixed(6)}, drift=${progressDrift.toFixed(6)}, using=${progress === audioProgress ? 'audio' : 'manual'}`);
    }
  }
  
  // Calculate current time in seconds based on project timeline, not audio duration
  let currentTimeSeconds;
  if (isPlaying && progress && alsProject) {
    // Use the actual audio timeline as the master - visuals follow audio completely
    // No offset needed since we're using locator times directly
    currentTimeSeconds = progress * actualDuration;
    
    // 🎵 ENHANCED TIMELINE SYNC LOGGING - Audio-driven timeline
    if (paintCount % 300 === 0 && currentTimeSeconds > 0) { // Every ~5 seconds at 60fps
      // console.log(`🎵 TIMELINE_SYNC: progress=${progress.toFixed(6)}, timeSeconds=${currentTimeSeconds.toFixed(3)}, duration=${actualDuration?.toFixed(2)}s`);
      
      if (currentAudioProgress !== null) {
        const audioTime = currentAudioProgress * actualDuration;
        const visualTime = progress * actualDuration;
        const timeDrift = Math.abs(visualTime - audioTime);
        // console.log(`🎵 AUDIO_SYNC: audioProgress=${currentAudioProgress.toFixed(6)}, audioTime=${audioTime.toFixed(3)}s, visualTime=${visualTime.toFixed(3)}s, drift=${timeDrift.toFixed(3)}s`);
        
        // Alert on significant drift (should be minimal now)
        if (timeDrift > 0.1) { // 100ms drift threshold  
          console.warn(`🎵 SYNC_DRIFT_ALERT: Unexpected drift! Visual=${visualTime.toFixed(3)}s, Audio=${audioTime.toFixed(3)}s, Drift=${timeDrift.toFixed(3)}s`);
        } else {
          // console.log(`🎵 SYNC_STATUS: ✅ Audio and visuals in sync (${timeDrift.toFixed(3)}s drift)`);
        }
      }
      
      // Log current locator for context
      const locatorInfo = alsProject.getCurrentLocator(currentTimeSeconds, actualDuration);
      if (locatorInfo.current) {
        // console.log(`🎵 LOCATOR_SYNC: current="${locatorInfo.current.name}", expected=${locatorInfo.current.seconds.toFixed(3)}s, actual=${currentTimeSeconds.toFixed(3)}s`);
      }
    }
  } else if (pausedAt > 0 && actualDuration) {
    // When paused, maintain the visual state at the pause position
    currentTimeSeconds = pausedAt * actualDuration;
    // Log pause state occasionally
    if (paintCount % 600 === 0) { // Every ~10 seconds when paused
      // console.log(`🎵 PAUSED_STATE: pausedAt=${pausedAt.toFixed(6)}, timeSeconds=${currentTimeSeconds.toFixed(3)}s`);
    }
  } else {
    currentTimeSeconds = 0;
  }

  // === PRIMARY TV BAR COMPOSITION (SEPARATE BUFFER) ===
  // Render TV bars to their own buffer for isolation from UI elements
  
  // Get pause-aware time for visual effects
  function getPauseAwareTime() {
    const rawTime = performance.now();
    if (isPlaying) {
      return rawTime - totalPausedTime;
    } else {
      // When paused, freeze time at the pause point
      if (pauseStartTime === null) {
        pauseStartTime = rawTime;
      }
      return rawTime - totalPausedTime - (rawTime - pauseStartTime);
    }
  }
  
  // Update pause timing
  if (!isPlaying && pauseStartTime !== null) {
    // We're paused - time is frozen
  } else if (isPlaying && pauseStartTime !== null) {
    // We just resumed - add the pause duration to total
    totalPausedTime += performance.now() - pauseStartTime;
    pauseStartTime = null;
  }
  
  // Clear and render to TV bars buffer
  page(tvBarsBuffer); // Switch to TV bars buffer

  wipe(0, 0, 0, 0); // Clear the buffer with transparent black each frame
  
  // The TV bar is the main visual element that fills the entire screen
  // All other elements are overlays on top of this base composition
  
  // ACTIVE ELEMENTS TRACKER - Collect all active/visible elements per frame
  const activeElements = {
    timestamp: currentTimeSeconds,
    currentLocators: [],
    nearbyLocators: [],
    visibleClips: [],
    activeNotes: [],
    visible3pxColors: [] // Track colors of currently playing MIDI notes
  };
  
  // Analyze current musical content to generate TV bar colors
  const centerX = screen.width / 2;
  
  // Get current locator to determine which palette to use
  const { current: currentLocator } = alsProject.getCurrentLocator(currentTimeSeconds, actualDuration);
  const currentPalette = getPaletteForLocator(currentLocator?.name);
  
  // Check for locator changes and clear decay on section transitions
  const currentLocatorName = currentLocator?.name || null;
  if (currentLocatorName !== previousLocatorName) {
    // Locator changed - clear all decaying colors for clean section transition
    colorHistory = [];
    previousLocatorName = currentLocatorName;
    // console.log(`🎬 Locator changed to: ${currentLocatorName} - cleared color decay`);
  }
  
  // Process MIDI notes for current time to collect active colors
  if (alsProject && alsProject.locators && alsProject.locators.length > 0) {
    for (let locatorIndex = 0; locatorIndex < alsProject.locators.length; locatorIndex++) {
      const locator = alsProject.locators[locatorIndex];
      const segmentStart = locator.seconds;
      const segmentEnd = alsProject.locators[locatorIndex + 1]?.seconds || actualDuration;
      
      // Check ALL segments for currently playing notes, not just the current segment
      // This allows multiple notes from different segments to play simultaneously
      
      if (alsProject.clips) {
        alsProject.clips.forEach((clip, clipIndex) => {
          if (clip.type === 'midiclip' && clip.notes && clip.notes.length > 0) {
            clip.notes.forEach((note, noteIndex) => {
              // Calculate note timing in seconds
              const noteStartTime = clip.startSeconds + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.time || 0));
              const noteEndTime = noteStartTime + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.duration || 0.25));
              
              // Check if note is currently playing (regardless of which segment it's in)
              if (currentTimeSeconds >= noteStartTime && currentTimeSeconds <= noteEndTime) {
                const velocity = note.velocity || 100;
                const pitch = note.key || 60;
                const noteDuration = noteEndTime - noteStartTime;
                
                // Only log occasionally to avoid spam
                // if (Math.random() < 0.01) {
                //   console.log(`🎵 Found playing note: pitch=${pitch}, velocity=${velocity}, start=${noteStartTime.toFixed(2)}s, end=${noteEndTime.toFixed(2)}s, clip=${clipIndex}`);
                // }
                
                // LOCATOR-SPECIFIC PALETTE COLOR SELECTION
                // Use current locator to determine palette, then pick color from that palette
                const colorIndex = (clipIndex * 3 + noteIndex * 5 + pitch * 2) % currentPalette.colors.length;
                const baseColor = currentPalette.colors[colorIndex];
                
                // Apply velocity-based brightness variation
                const velocityFactor = 0.7 + (velocity / 127) * 0.3; // 0.7 to 1.0
                
                let r = Math.floor(baseColor.r * velocityFactor);
                let g = Math.floor(baseColor.g * velocityFactor);
                let b = Math.floor(baseColor.b * velocityFactor);
                
                // Ensure minimum brightness
                r = Math.max(30, r);
                g = Math.max(30, g);
                b = Math.max(30, b);
                
                const colorInfo = {
                  r: r,
                  g: g,
                  b: b,
                  pitch: pitch,
                  velocity: velocity,
                  duration: noteDuration,
                  startTime: noteStartTime,
                  clipIndex: clipIndex,
                  noteIndex: noteIndex,
                  paletteIndex: colorIndex, // Track which palette color was used
                  paletteName: currentPalette.name // Track which palette was used
                };
                
                activeElements.visible3pxColors.push(colorInfo);
              }
            });
          }
        });
      }
      
      // Track current locator for fallback color (only for the current segment)
      const isCurrentSegment = currentTimeSeconds >= segmentStart && currentTimeSeconds < segmentEnd;
      if (isCurrentSegment) {
        const locatorColor = getLocatorColor(locator, locatorIndex);
        activeElements.currentLocators.push({
          name: locator.name,
          color: locatorColor,
          startTime: segmentStart,
          endTime: segmentEnd
        });
      }
    }
  }
  
  // RENDER FULL-SCREEN TV BAR COMPOSITION
  const playingColors = activeElements.visible3pxColors;
  
  // Debug: Log raw playing colors before deduplication
  // if (playingColors.length > 0 && Math.random() < 0.05) {
  //   console.log(`🎨 DEBUG: ${playingColors.length} raw colors before dedup:`, playingColors.map(c => `pitch=${c.pitch}, start=${c.startTime.toFixed(2)}, RGB=(${c.r},${c.g},${c.b})`));
  // }
  
  // Remove duplicates based on startTime AND pitch to allow multiple notes starting at same time
  const uniqueColors = [];
  const seenNotes = new Set();
  
  playingColors.forEach(colorInfo => {
    // Create unique key combining startTime, pitch, clipIndex, and noteIndex
    // This prevents duplicate notes from same clip and allows multiple different notes
    const noteKey = `${colorInfo.startTime}-${colorInfo.pitch}-${colorInfo.clipIndex}-${colorInfo.noteIndex}`;
    if (!seenNotes.has(noteKey)) {
      seenNotes.add(noteKey);
      uniqueColors.push(colorInfo);
    }
  });
  
  // Debug logging (reduced frequency)
  if (uniqueColors.length > 0 && Math.random() < 0.1) {
    const paletteName = uniqueColors[0]?.paletteName || "Unknown";
    // console.log(`🎨 TV Bar: ${uniqueColors.length} unique colors detected after dedup (Palette: ${paletteName})`);
    if (uniqueColors.length > 1) {
      // console.log(`🎨 Multiple colors:`, uniqueColors.map(c => `pitch=${c.pitch}, clip=${c.clipIndex}, note=${c.noteIndex}, ${c.paletteName}[${c.paletteIndex}], RGB=(${c.r},${c.g},${c.b})`));
    }
  }
  
  // COLOR DECAY SYSTEM - restore visual persistence of notes
  // First, identify which colors are truly NEW (not in history yet)
  const newColorKeys = new Set();
  
  uniqueColors.forEach(colorInfo => {
    // Make each note much more unique so we get rapid bar addition
    // Include more specific details to ensure almost every note gets its own bar
    const colorKey = `${colorInfo.r}-${colorInfo.g}-${colorInfo.b}-${colorInfo.pitch || 0}-${colorInfo.clipIndex || 0}-${colorInfo.noteIndex || 0}-${Math.floor((colorInfo.startTime || 0) * 10)}`;
    const existingIndex = colorHistory.findIndex(h => h.colorKey === colorKey);
    
    if (existingIndex >= 0) {
      // Update existing color - reset decay
      colorHistory[existingIndex].alpha = 1.0;
      colorHistory[existingIndex].lastSeen = getPauseAwareTime();
      colorHistory[existingIndex].isDecaying = false;
    } else {
      // Mark this as a new color that should flash
      newColorKeys.add(colorKey);
      
      // Add new color to history with unique key and better positioning
      // Create more spread-out positioning using full pitch range and stronger randomization
      const pitchNormalized = Math.min(127, Math.max(0, colorInfo.pitch || 60)) / 127; // Full MIDI range 0-127
      const timePosition = ((colorInfo.startTime || 0) % 8) / 8; // Longer time cycle for more spread
      const randomSpread = Math.random(); // Full 0-1 random for good distribution
      const preferredPosition = (pitchNormalized * 0.4 + timePosition * 0.2 + randomSpread * 0.4); // More randomness
      
      colorHistory.push({
        r: colorInfo.r,
        g: colorInfo.g,
        b: colorInfo.b,
        alpha: 1.0,
        lastSeen: getPauseAwareTime(),
        isDecaying: false,
        preferredPosition: preferredPosition,
        colorKey: colorKey,
        birthTime: getPauseAwareTime(), // Track when this bar was born for flash effect
        pitch: colorInfo.pitch || 60 // Store pitch for debugging
      });
    }
  });
  
  // Update decay for all colors in history - use pause-aware time
  const now = getPauseAwareTime();
  const decayDuration = 3000; // Reduced from 5 seconds to 3 seconds for faster clearing
  
  colorHistory.forEach(color => {
    const timeSinceLastSeen = now - color.lastSeen;
    if (timeSinceLastSeen > 100) { // Start decaying after 100ms (faster than before)
      color.isDecaying = true;
      color.alpha = Math.max(0, 1.0 - (timeSinceLastSeen - 100) / decayDuration);
    }
  });
  
  // Remove fully decayed colors
  colorHistory = colorHistory.filter(color => color.alpha > 0.05);
  
  // Combine active and decaying colors - add new bars to CENTER, alternating left/right
  // Only give birthTime to ACTUALLY new colors (using the newColorKeys set)
  const allActiveColors = uniqueColors.map(color => {
    // Check if this exact color was marked as new
    const colorKey = `${color.r}-${color.g}-${color.b}-${color.pitch || 0}-${color.clipIndex || 0}-${color.noteIndex || 0}-${Math.floor((color.startTime || 0) * 10)}`;
    const isNewColor = newColorKeys.has(colorKey);
    
    return {
      ...color,
      birthTime: isNewColor ? getPauseAwareTime() : null, // Only truly new colors get birthTime
      isNewBar: isNewColor // Mark as new only if it was just added
    };
  });
  
  const allDecayingColors = colorHistory.filter(h => h.isDecaying); // Decaying colors
  
  // SIMPLIFIED ARRANGEMENT: Just combine all colors and distribute evenly
  // This maintains better visual balance and prevents left-clustering
  const allColors = [...allActiveColors, ...allDecayingColors];
  
  // Sort by preferredPosition to maintain consistent placement
  allColors.sort((a, b) => (a.preferredPosition || 0) - (b.preferredPosition || 0));
  
  const activeSegments = allColors;
  
  if (activeSegments.length > 0) {
    // Show ALL colors - no arbitrary limits, let natural decay control the count
    const colorsToShow = activeSegments;
    
    // Use the simplified sorted arrangement
    const positionSortedColors = colorsToShow;
    
    // Equal width bars that divide the screen evenly - ensure no cut-off
    const totalBars = positionSortedColors.length;
    const barWidth = totalBars > 0 ? Math.floor(screen.width / totalBars) : screen.width;
    
    positionSortedColors.forEach((colorInfo, index) => {
      try {
        // Each bar gets an equal slice, positioned sequentially from left to right
        const startX = index * barWidth;
        // Ensure last bar doesn't go off screen
        const actualWidth = (index === totalBars - 1) ? (screen.width - startX) : barWidth;
        const clampedStartX = Math.min(startX, screen.width - actualWidth);
        
        // Handle white flash for new bars and fading for decaying bars
        const now = getPauseAwareTime();
        
        if (colorInfo.isDecaying) {
          // Fade to black for decaying colors
          const alpha = colorInfo.alpha;
          const fadedR = Math.floor(colorInfo.r * alpha);
          const fadedG = Math.floor(colorInfo.g * alpha);
          const fadedB = Math.floor(colorInfo.b * alpha);
          ink(fadedR, fadedG, fadedB);
        } else {
          // Normal RGB color (no flash)
          ink(colorInfo.r, colorInfo.g, colorInfo.b);
        }
        
        // Get audio data for this bar - now with frequency bands!
        const amplitude = sound.speaker?.amplitudes?.left || 0; // Single number for overall intensity
        const audioWaveform = sound.speaker?.waveforms?.left || []; // Array for per-bar variation
        const frequencyBands = sound.speaker?.frequencies?.left || []; // Array of frequency band objects
        
        // Calculate amplitude-based height with frequency band analysis
        let amplitudeHeight = 24; // Use TV bars buffer height instead of screen.height
        
        // Always use TV bars buffer height
        amplitudeHeight = 24;
        
        // Center the bar vertically within the TV bars buffer
        const barHeight = Math.floor(amplitudeHeight);
        const barY = 0; // Start from top of TV bars buffer
        
        // Draw the bar with full height
        box(clampedStartX, barY, actualWidth, barHeight);
        
        // CRITICAL: Reset fade mode by calling ink with a non-fade string
        // This clears the global fadeMode, fadeColors, fadeDirection variables
        ink("black"); // Forces findColor to reset fade state
      } catch (error) {
        console.error('Error rendering TV bar:', error);
        // Fallback: render in red to show something went wrong
        ink(255, 0, 0);
        box(10 + index * 20, 0, 15, 24); // Use TV bars buffer height
      }
    });
    
  } else {
    // Fallback: show current locator theme color across full screen with amplitude height
    if (currentPalette && currentPalette.colors && currentPalette.colors.length > 0) {
      // Use the same base color logic as timeline background and segments
      // Find the current locator index to match the segment color logic
      let currentLocatorIndex = 0;
      if (alsProject && alsProject.locators && currentLocator) {
        currentLocatorIndex = alsProject.locators.findIndex(loc => loc.name === currentLocator.name);
        if (currentLocatorIndex === -1) currentLocatorIndex = 0;
      }
      const colorIndex = currentLocatorIndex % currentPalette.colors.length;
      const bgColor = currentPalette.colors[colorIndex];
      ink(bgColor.r, bgColor.g, bgColor.b); // Solid palette color, no pulse
      
      // Always full height for fallback as well
      box(0, 0, screen.width, 24); // Use TV bars buffer height
    } else {
      // Default background when no content is playing - match timeline fallback
      ink(50, 50, 50);
      
      // Always full height for default as well
      box(0, 0, screen.width, 24); // Use TV bars buffer height
    }
  }

  // === WAVEFORM OVERLAY ON TV BARS ===
  // Draw waveform on top of the TV bars (inspired by whistle.mjs)
  const waveform = sound.speaker?.waveforms?.left || [];
  const amplitude = sound.speaker?.amplitudes?.left || 0;
  
  if (waveform.length > 0) {
    // Waveform as a divider with flood fills above and below
    const resampledWaveform = waveform.length > 128 
      ? waveform.filter((_, i) => i % Math.ceil(waveform.length / 128) === 0).slice(0, 128)
      : waveform;
    
    const xStep = screen.width / (resampledWaveform.length - 1);
    const yMid = 12; // Half of TV bars buffer height (24/2)
    const yMax = 8; // Amplitude range for the 24px buffer
    
    // Get weird colors for above and below flood fills
    const currentTime = performance.now();
    const colorCycle = (currentTime / 200) % 360; // Color cycle every 200ms
    
    // Weird color above the waveform (shifting through spectrum)
    const aboveHue = (colorCycle + 180) % 360;
    const aboveColor = hslToRgb(aboveHue / 360, 0.8, 0.4);
    
    // Weird color below the waveform (different hue)
    const belowHue = (colorCycle + 90) % 360;
    const belowColor = hslToRgb(belowHue / 360, 0.7, 0.3);
    
    // First draw the waveform line as fully opaque white
    ink(255, 255, 255, 255); // Fully opaque white
    poly(
      resampledWaveform.map((v, i) => [
        i * xStep,
        yMid + v * yMax
      ])
    );
    
    // Flood fill above the midline with weird color - COMMENTED OUT
    // ink(aboveColor.r, aboveColor.g, aboveColor.b, 30); // Very low opacity weird color
    // for (let x = 0; x < screen.width; x += 8) { // Sample every 8 pixels for performance
    //   flood(x, 6); // Flood from upper area of TV bars buffer
    // }
    
    // Flood fill below the midline with different weird color - COMMENTED OUT
    // ink(belowColor.r, belowColor.g, belowColor.b, 30); // Very low opacity weird color
    // for (let x = 0; x < screen.width; x += 8) { // Sample every 8 pixels for performance
    //   flood(x, 18); // Flood from lower area of TV bars buffer
    // }
  }
  
  // HSL to RGB conversion helper function
  function hslToRgb(h, s, l) {
    let r, g, b;
    if (s === 0) {
      r = g = b = l; // achromatic
    } else {
      const hue2rgb = (p, q, t) => {
        if (t < 0) t += 1;
        if (t > 1) t -= 1;
        if (t < 1/6) return p + (q - p) * 6 * t;
        if (t < 1/2) return q;
        if (t < 2/3) return p + (q - p) * (2/3 - t) * 6;
        return p;
      };
      const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
      const p = 2 * l - q;
      r = hue2rgb(p, q, h + 1/3);
      g = hue2rgb(p, q, h);
      b = hue2rgb(p, q, h - 1/3);
    }
    return {
      r: Math.round(r * 255),
      g: Math.round(g * 255),
      b: Math.round(b * 255)
    };
  }

  //oom(1.5).scroll(paintCount, paintCount).spin(3).zoom(0.15).blur(0);

  // === END TV BARS RENDERING ===
  // Switch back to main screen and paste the TV bars buffer with blur effect
  page(screen);
  
  // === KIDLISP AND TV BARS COMBINATION ===
  if (kidlispMode) {
    // Only show TV bars when playing, not when paused
    if (isPlaying) {
      // First paste the TV bars buffer at the bottom (24px tall)
      paste(tvBarsBuffer, 0, screen.height - 24);
    }
    
    // Then render KidLisp on top, dimensions adjusted based on pause state
    // Use pre-defined locator-specific KidLisp sources
    
    // Get current locator for context
    const { current: currentLocator } = alsProject ? alsProject.getCurrentLocator(currentTimeSeconds, actualDuration) : { current: null };
    const currentPalette = getPaletteForLocator(currentLocator?.name);
    
    // Get the pre-defined KidLisp source for this locator
    let kidlispCode = getKidlispSourceForLocator(currentLocator?.name, isPlaying, audioInitializing, inTapeMode);
    
    // Add defensive checks to prevent errors
    if (!kidlispCode) {
      kidlispCode = ZZZWAP_KIDLISP_SOURCES.START; // Fallback to START
    }
    
    // Execute the KidLisp theme code (amp is now available as a global variable)
    // Adjust KidLisp dimensions based on pause state
    if (!isPlaying) {
      // When paused: full screen KidLisp, only leave 3px at bottom for 2px progress bar
      kidlisp(0, 0, screen.width, screen.height - 3, kidlispCode);
    } else {
      // When playing: normal layout with space for timeline and TV bars
      kidlisp(0, 25, screen.width, screen.height - 24 - 24, kidlispCode);
    }
    
    // CRITICAL: Reset fade mode by calling ink with a non-fade string
    // This clears the global fadeMode, fadeColors, fadeDirection variables
    ink("black"); // Forces findColor to reset fade state
  } else {
    // Use traditional TV bars buffer (full screen)
    paste(tvBarsBuffer, 0, screen.height - 24);
  }
  
  // blur(1); // Apply blur to the TV bars buffer to test separation
  // zoom(0.5);

  // === DYNAMIC TIMELINE ZOOM BASED ON NOTE DENSITY ===
  
  // Function to calculate note density in a time window
  function calculateNoteDensity(startTime, endTime) {
    let noteCount = 0;
    const timeWindow = endTime - startTime;
    
    if (alsProject && alsProject.clips) {
      alsProject.clips.forEach(clip => {
        if (clip.type === 'midiclip' && clip.notes) {
          clip.notes.forEach(note => {
            const noteStartTime = clip.startSeconds + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.time || 0));
            if (noteStartTime >= startTime && noteStartTime <= endTime) {
              noteCount++;
            }
          });
        }
      });
    }
    
    return timeWindow > 0 ? noteCount / timeWindow : 0; // notes per second
  }
  
  // Fixed zoom level for consistent behavior (no dynamic scaling)
  const pixelsPerSecond = 80; // Increased from 40 to 80 px/sec for faster scrolling / more zoomed in view

  // === FREQUENCY DISPLAY OVERLAY ===
  // Frequency display commented out to make timeline flush with top
  if (false) { // Disabled frequency display code
    const freqDisplayHeight = 12; // Even more compact - nice and tight!
    const freqDisplayY = 0; // Position frequency display at very top
    
    // Divide the frequency area in half - left for freq bars, right for beat detection
    const totalFreqWidth = screen.width;
    const freqBarsWidth = Math.floor(totalFreqWidth * 0.5); // Left half for frequency bars
    const beatDisplayWidth = totalFreqWidth - freqBarsWidth; // Right half for beat detection
    const beatDisplayX = freqBarsWidth; // Start beat display after freq bars
    
    // Get audio data first to calculate layouts
    const amplitude = sound.speaker?.amplitudes?.left || 0;
    const frequencyBands = sound.speaker?.frequencies?.left || [];
    const waveform = sound.speaker?.waveforms?.left || [];
    const beatData = sound.speaker?.beat || { detected: false, strength: 0, timestamp: 0 };
    
    // Audio debug logging removed to reduce console spam
    
    if (frequencyBands.length > 0) {
      // Filter out frequency bands that have no significant data to avoid empty dark bands
      const activeBands = frequencyBands.filter((band, index) => {
        const bandAmplitude = band.amplitude || 0;
        const scaledAmplitude = Math.pow(bandAmplitude, 0.7);
        return scaledAmplitude >= 0.005; // Only include bands with some activity
      });
      
      // If we filtered out too many bands, keep at least the bands with the highest activity
      let bandsToShow = activeBands;
      if (activeBands.length < 8) {
        // Not enough active bands, so include the top bands by amplitude
        const sortedBands = frequencyBands
          .map((band, index) => ({ ...band, originalIndex: index }))
          .sort((a, b) => (b.amplitude || 0) - (a.amplitude || 0))
          .slice(0, Math.max(8, activeBands.length));
        bandsToShow = sortedBands;
      }
      
      if (bandsToShow.length === 0) {
        // Fallback: show all bands if none are active
        bandsToShow = frequencyBands;
      }
      
      // Reserve space for amplitude bar on the left - no gaps, pixel perfect
      const ampBarWidth = 20; // Amplitude bar width
      const freqStartX = ampBarWidth; // No gap between amp and freq
      const freqAreaWidth = freqBarsWidth - freqStartX; // Only use left half of screen
      const exactBandWidth = freqAreaWidth / bandsToShow.length;
      
      // First pass: Draw individual colored negative backdrops for each active frequency band
      bandsToShow.forEach((band, displayIndex) => {
        // Calculate pixel-perfect positioning
        const x = Math.floor(freqStartX + (displayIndex * exactBandWidth));
        const nextX = Math.floor(freqStartX + ((displayIndex + 1) * exactBandWidth));
        const barWidth = nextX - x; // Pixel-perfect width, no gaps
        
        // Use original index for color calculation to maintain consistent colors
        const originalIndex = band.originalIndex !== undefined ? band.originalIndex : 
                              frequencyBands.findIndex(b => b.name === band.name);
        const normalizedIndex = originalIndex / frequencyBands.length;
        let r, g, b;
        
        if (normalizedIndex < 0.22) {
          // Sub-bass and bass frequencies - deep red to red
          const t = normalizedIndex / 0.22;
          r = Math.floor(150 + t * 105); // 150-255
          g = Math.floor(t * 50); // 0-50
          b = 0;
        } else if (normalizedIndex < 0.44) {
          // Low-mid frequencies - red to yellow
          const t = (normalizedIndex - 0.22) / 0.22;
          r = 255;
          g = Math.floor(50 + t * 205); // 50-255
          b = 0;
        } else if (normalizedIndex < 0.67) {
          // Mid to high-mid frequencies - yellow to cyan
          const t = (normalizedIndex - 0.44) / 0.23;
          r = Math.floor(255 * (1 - t));
          g = 255;
          b = Math.floor(t * 255);
        } else {
          // Treble and ultra frequencies - cyan to blue to purple
          const t = (normalizedIndex - 0.67) / 0.33;
          r = Math.floor(t * 128); // Add some purple
          g = Math.floor(255 * (1 - t));
          b = 255;
        }
        
        // Create dark negative backdrop version (15% of original color)
        const backdropR = Math.floor(r * 0.15);
        const backdropG = Math.floor(g * 0.15);
        const backdropB = Math.floor(b * 0.15);
        
        ink(backdropR, backdropG, backdropB, 255); // Explicit alpha to prevent blending
        box(x, freqDisplayY, barWidth, freqDisplayHeight);
      });
      
      // Draw amplitude bar backdrop (more visible dark green) - explicit alpha
      ink(0, 40, 0, 255); // Explicit full alpha to prevent blending issues
      box(0, freqDisplayY, ampBarWidth, freqDisplayHeight);
      
      // Second pass: Draw actual amplitude and frequency bars on top
      
      // Draw amplitude level bar on the left in green - bottom aligned, no margins
      const ampHeight = Math.max(1, Math.floor(amplitude * freqDisplayHeight)); // Ensure at least 1px when active
      const ampY = freqDisplayY + freqDisplayHeight - ampHeight; // Bottom aligned
      
      // Log amplitude rendering
      if (false && paintCount % 60 === 0 && amplitude > 0.01) {
        console.log(`🟢 Amplitude rendering:
          Raw amplitude: ${amplitude.toFixed(4)}
          Bar height: ${ampHeight}px (max: ${freqDisplayHeight}px)
          Y position: ${ampY} (should be >= ${freqDisplayY})
          Height overflow: ${ampY < freqDisplayY ? 'YES - OVERFLOW!' : 'no'}`);
      }
      
      // Green amplitude bar with intensity based on level - explicit alpha to prevent bleed
      const greenIntensity = Math.floor(100 + amplitude * 155); // 100-255 green
      ink(0, greenIntensity, 0, 255); // Explicit full alpha to prevent blending issues
      box(0, ampY, ampBarWidth, ampHeight); // Pixel perfect, no margins
      
      // Draw frequency bands on the right side - bottom aligned, pixel perfect
      bandsToShow.forEach((band, displayIndex) => {
        let bandAmplitude = band.amplitude || 0;
        
        // Use the amplitude directly from speaker.mjs (it's already properly scaled there)
        // No additional compression here since speaker.mjs now handles it
        
        // Add frequency weighting - lower frequencies should feel heavier/more prominent
        const originalIndex = band.originalIndex !== undefined ? band.originalIndex : 
                              frequencyBands.findIndex(b => b.name === band.name);
        const normalizedIndex = originalIndex / frequencyBands.length;
        
        // Initialize smoothing array if needed
        if (previousFrequencyValues.length <= displayIndex) {
          previousFrequencyValues[displayIndex] = 0;
        }
        
        // Store original amplitude for logging
        const originalAmplitude = bandAmplitude;
        
        // Frequency weighting: gentle boost across all frequencies to help them reach full height
        let frequencyWeight = 1.0;
        if (normalizedIndex < 0.2) {
          // Bass: gentle boost for very low frequencies
          frequencyWeight = 1.0 + (0.2 - normalizedIndex) * 0.5; // Max 10% boost
        } else if (normalizedIndex >= 0.2 && normalizedIndex <= 0.8) {
          // Middle frequencies: give them a boost too so they can reach full height
          frequencyWeight = 1.2; // 20% boost for middle frequencies
        } else {
          // Very high frequencies: slight reduction
          frequencyWeight = 1.0 - (normalizedIndex - 0.8) * 0.25; // Max 5% reduction
        }
        
        bandAmplitude = bandAmplitude * frequencyWeight;
        
        // Allow bars to reach full height
        bandAmplitude = Math.min(1.0, bandAmplitude);
        
        // Apply lighter smoothing to reduce CPU overhead
        const smoothingFactor = 0.8; // Lighter smoothing for better performance
        const previousValue = previousFrequencyValues[displayIndex];
        bandAmplitude = previousValue * smoothingFactor + bandAmplitude * (1 - smoothingFactor);
        
        // Store smoothed value for next frame
        previousFrequencyValues[displayIndex] = bandAmplitude;
        
        // Log processing pipeline for first few bands every 60 frames
        if (false && paintCount % 60 === 0 && displayIndex < 3 && originalAmplitude > 0.01) {
          console.log(`🎚️ Band ${displayIndex} (idx:${originalIndex}) processing:
            Raw: ${originalAmplitude.toFixed(4)}
            Weight: ${frequencyWeight.toFixed(3)}x 
            After weight: ${(originalAmplitude * frequencyWeight).toFixed(4)}
            After clamp: ${Math.min(0.9, originalAmplitude * frequencyWeight).toFixed(4)}
            Before smooth: ${beforeSmoothing.toFixed(4)}
            After smooth: ${bandAmplitude.toFixed(4)}
            Previous: ${previousValue.toFixed(4)}`);
        }
        
        // Simple threshold for showing bands
        const threshold = 0.02; // 2% threshold
        
        // Calculate pixel-perfect positioning
        const x = Math.floor(freqStartX + (displayIndex * exactBandWidth));
        const nextX = Math.floor(freqStartX + ((displayIndex + 1) * exactBandWidth));
        const barWidth = nextX - x; // Pixel-perfect width, no gaps
        
        if (bandAmplitude >= threshold) {
          // Band has data - draw colored bar that can reach full height
          const barHeight = Math.max(2, Math.round(bandAmplitude * freqDisplayHeight)); // Use round instead of floor for more accurate height
          const y = freqDisplayY + freqDisplayHeight - barHeight; // Bottom aligned
          
          // Log final rendering values for first few bands
          if (false && paintCount % 60 === 0 && displayIndex < 3 && bandAmplitude > 0.01) {
            console.log(`🎨 Band ${displayIndex} rendering:
              Final amplitude: ${bandAmplitude.toFixed(4)}
              Bar height: ${barHeight}px (max: ${freqDisplayHeight}px)
              Y position: ${y} (should be >= ${freqDisplayY})
              Height overflow: ${y < freqDisplayY ? 'YES - OVERFLOW!' : 'no'}`);
          }
          
          // Use original index for consistent color mapping
          
          let r, g, b;
          if (normalizedIndex < 0.22) {
            // Sub-bass and bass frequencies - deep red to red
            const t = normalizedIndex / 0.22;
            r = Math.floor(150 + t * 105); // 150-255
            g = Math.floor(t * 50); // 0-50
            b = 0;
          } else if (normalizedIndex < 0.44) {
            // Low-mid frequencies - red to yellow
            const t = (normalizedIndex - 0.22) / 0.22;
            r = 255;
            g = Math.floor(50 + t * 205); // 50-255
            b = 0;
          } else if (normalizedIndex < 0.67) {
            // Mid to high-mid frequencies - yellow to cyan
            const t = (normalizedIndex - 0.44) / 0.23;
            r = Math.floor(255 * (1 - t));
            g = 255;
            b = Math.floor(t * 255);
          } else {
            // Treble and ultra frequencies - cyan to blue to purple
            const t = (normalizedIndex - 0.67) / 0.33;
            r = Math.floor(t * 128); // Add some purple
            g = Math.floor(255 * (1 - t));
            b = 255;
          }
          
          // Apply amplitude-based brightness with good range
          const brightness = 0.3 + (bandAmplitude * 0.7); // 30% to 100% brightness
          r = Math.floor(r * brightness);
          g = Math.floor(g * brightness);
          b = Math.floor(b * brightness);
          
          // Draw main frequency bar - ensure no alpha blending issues
          ink(r, g, b, 255); // Explicitly set full alpha to prevent blending issues
          box(x, y, barWidth, barHeight);
          
          // Optional: Add subtle "weight" effect for heavy frequencies
          // FIX: Disable glow effect temporarily to prevent alpha bleed issues
          /*
          if (normalizedIndex < 0.4 && bandAmplitude > 0.4) {
            // Add a subtle glow effect for heavy bass/low-mid frequencies
            const glowAlpha = Math.floor((bandAmplitude - 0.4) * 80); // 0-48 alpha
            ink(r, g, b, glowAlpha);
            // Draw a slightly wider bar for glow effect
            const glowWidth = Math.min(2, Math.floor(barWidth * 0.2));
            if (glowWidth > 0) {
              box(x - glowWidth, y, barWidth + (glowWidth * 2), barHeight);
            }
          }
          */
        }
        // No else needed - the backdrop is already drawn
      });
      
      // === BEAT DETECTION VISUALIZATION ===
      // Right half of the frequency area shows beat detection
      
      // Beat detection debug logs removed to reduce console spam
      
      // Draw beat detection backdrop
      ink(20, 20, 40, 255); // Dark blue backdrop for beat area
      box(beatDisplayX, freqDisplayY, beatDisplayWidth, freqDisplayHeight);
      
      // Beat detection visualization
      if (beatData.detected) {
        // Flash effect for detected beat
        const flashIntensity = Math.floor(255 * beatData.strength);
        ink(255, flashIntensity, flashIntensity, 255); // Red flash with intensity based on strength
        box(beatDisplayX, freqDisplayY, beatDisplayWidth, freqDisplayHeight);
        
        // Beat strength bar (vertical)
        const beatBarWidth = 6;
        const beatBarHeight = Math.floor(beatData.strength * freqDisplayHeight);
        const beatBarX = beatDisplayX + Math.floor((beatDisplayWidth - beatBarWidth) / 2);
        const beatBarY = freqDisplayY + freqDisplayHeight - beatBarHeight;
        
        ink(255, 255, 255, 255); // White beat strength indicator
        box(beatBarX, beatBarY, beatBarWidth, beatBarHeight);
      } else {
        // Show beat detection state when no beat
        ink(60, 60, 80, 255); // Dim indicator
        const indicatorSize = 4;
        const indicatorX = beatDisplayX + Math.floor((beatDisplayWidth - indicatorSize) / 2);
        const indicatorY = freqDisplayY + Math.floor((freqDisplayHeight - indicatorSize) / 2);
        box(indicatorX, indicatorY, indicatorSize, indicatorSize);
      }
    
    } else if (waveform.length > 0) {
      // Fallback: draw waveform as amplitude visualization - bottom aligned, no gaps
      const waveformStep = screen.width / waveform.length;
      const maxHeight = freqDisplayHeight;
      
      ink(0, 255, 255); // Cyan for waveform
      waveform.forEach((sample, index) => {
        const x = index * waveformStep;
        const barHeight = Math.abs(sample) * maxHeight;
        const y = freqDisplayY + freqDisplayHeight - barHeight; // Bottom aligned
        box(Math.floor(x), y, Math.floor(waveformStep), Math.floor(barHeight));
      });
    } else {
      // Simple amplitude bar if no detailed data - bottom aligned, full width
      const ampWidth = amplitude * screen.width;
      const ampHeight = freqDisplayHeight;
      const ampY = freqDisplayY;
      
      ink(255, 255, 0); // Yellow for amplitude
      box(0, ampY, Math.floor(ampWidth), ampHeight);
    }
  } // End of disabled frequency display code

  // === TIMELINE OVERLAY (MINIMAL & FAST) ===
  // The timeline is now an overlay element that sits on top of the TV bar composition
  // Hide timeline when paused to allow full-screen KidLisp
  
  if (timelineVisible && isPlaying) {
    // 🎵 TIMELINE RENDER LOGGING - Track when timeline is drawn and its sync status
    if (paintCount % 600 === 0) { // Every ~10 seconds at 60fps
      const locatorInfo = alsProject?.getCurrentLocator(currentTimeSeconds, actualDuration);
      // console.log(`🎵 TIMELINE_RENDER: visible=true, playing=true, currentTime=${currentTimeSeconds.toFixed(3)}s, locator="${locatorInfo?.current?.name || 'none'}"`);
      
      if (currentAudioProgress !== null) {
        const audioTime = currentAudioProgress * actualDuration;
        const visualAudioDrift = Math.abs(currentTimeSeconds - audioTime);
        console.log(`🎵 TIMELINE_SYNC_STATUS: visual=${currentTimeSeconds.toFixed(3)}s, audio=${audioTime.toFixed(3)}s, drift=${visualAudioDrift.toFixed(3)}s`);
      }
    }
    
    // Minimal timeline layout parameters
    const freqDisplayHeight = 12; // Define this here so timeline can reference it
    const timelineHeight = 25; // Reduced from 50 to 25 - much more minimal
    const timelineY = frequencyDisplayVisible ? freqDisplayHeight : 0; // Position below frequency display if it's visible
    const leadTimeSeconds = 2.0; // Show 2 seconds of future time
    const timelineWindowSeconds = screen.width / pixelsPerSecond;
    // Calculate current view window - align to start at the red center line
    const viewStartTime = currentTimeSeconds;
    const viewEndTime = Math.min(actualDuration, currentTimeSeconds + timelineWindowSeconds);
    
    // Themed timeline background - matches current TV color palette exactly
    if (currentPalette && currentPalette.colors && currentPalette.colors.length > 0) {
      let colorIndex = 0; // Default to first color
      
      // Use locator index for all palettes (including PAUSE)
      if (alsProject && alsProject.locators && currentLocator) {
        const currentLocatorIndex = alsProject.locators.findIndex(loc => loc.name === currentLocator.name);
        if (currentLocatorIndex !== -1) {
          colorIndex = currentLocatorIndex % currentPalette.colors.length;
        }
      }
      
      const bgColor = currentPalette.colors[colorIndex];
      
      // Use solid colors instead of gradients
      ink(bgColor.r, bgColor.g, bgColor.b);
    } else {
      ink(50, 50, 50); // Fallback to match TV default { r: 50, g: 50, b: 50 }
    }
    box(0, timelineY, screen.width, timelineHeight); // Simple strip at bottom
    
    // CRITICAL: Reset gradient/fade state after background
    ink("black");
  
  // Draw locator segments on timeline overlay
  if (alsProject && alsProject.locators) {
    for (let i = 0; i < alsProject.locators.length; i++) {
      const locator = alsProject.locators[i];
      const segmentStart = locator.seconds;
      const segmentEnd = alsProject.locators[i + 1]?.seconds || actualDuration;
      
      // Calculate screen coordinates for this segment relative to red center line
      const segmentStartX = centerX + (segmentStart - currentTimeSeconds) * pixelsPerSecond;
      const segmentEndX = centerX + (segmentEnd - currentTimeSeconds) * pixelsPerSecond;
      const segmentWidth = segmentEndX - segmentStartX;
      
      // Only draw if segment is visible on screen
      if (segmentEndX > -50 && segmentStartX < screen.width + 50 && segmentWidth > 0) {
        // Use the same palette-based color logic as TV bars instead of getLocatorColor
        const segmentPalette = getPaletteForLocator(locator.name);
        
        // Defensive check to prevent errors
        if (!segmentPalette || !segmentPalette.colors || segmentPalette.colors.length === 0) {
          console.warn(`⚠️ Invalid palette for locator: ${locator.name}`);
          continue; // Skip this segment
        }
        
        const colorIndex = i % segmentPalette.colors.length; // Simple index-based selection for timeline
        
        // Use normal color cycling for all segments including PAUSE
        const paletteColor = segmentPalette.colors[colorIndex];
        
        // DEBUG: Log segment color info for PAUSE segments (now using normal colors)
        const isPauseSegment = segmentPalette.name && segmentPalette.name.toLowerCase().includes('pause');
        if (isPauseSegment) {
          console.log(`🎬 Segment Overlay - Palette: "${segmentPalette.name}", ColorIndex: ${colorIndex}, RGB: (${paletteColor.r}, ${paletteColor.g}, ${paletteColor.b}), Locator: "${locator.name}"`);
        }
        
        const isCurrentTime = currentTimeSeconds >= segmentStart && currentTimeSeconds < segmentEnd;
        
        // Use solid colors for all segments (no special handling for PAUSE)
        ink(paletteColor.r, paletteColor.g, paletteColor.b);
        const clampedX = Math.max(0, segmentStartX);
        const effectiveWidth = segmentStartX < 0 ? segmentWidth + segmentStartX : segmentWidth;
        const clampedWidth = Math.min(effectiveWidth, screen.width - clampedX);
        if (clampedWidth > 0) {
          box(clampedX, timelineY, clampedWidth, timelineHeight);
          
          // CRITICAL: Reset gradient/fade state after each segment
          ink("black");
        }
      }
    }
  }
  
  // Draw MIDI notes overlay on timeline
  if (alsProject && alsProject.locators && alsProject.locators.length > 0) {
    for (let locatorIndex = 0; locatorIndex < alsProject.locators.length; locatorIndex++) {
      const locator = alsProject.locators[locatorIndex];
      const segmentStart = locator.seconds;
      const segmentEnd = alsProject.locators[locatorIndex + 1]?.seconds || actualDuration;
      
      // Calculate segment position in the scrolling view
      const segmentStartX = (segmentStart - viewStartTime) * pixelsPerSecond;
      const segmentEndX = (segmentEnd - viewStartTime) * pixelsPerSecond;
      const segmentWidth = segmentEndX - segmentStartX;
      
      // Always process segments to check for individual note visibility
      // Don't skip segments just because the segment boundary is offscreen
      if (segmentWidth > 1) {
        // Get all MIDI notes for this segment
        const segmentNotes = [];
        
        if (alsProject.clips) {
          alsProject.clips.forEach((clip, clipIndex) => {
            if (clip.type === 'midiclip' && clip.notes && clip.notes.length > 0) {
              clip.notes.forEach((note, noteIndex) => {
                const noteStartTime = clip.startSeconds + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.time || 0));
                const noteEndTime = noteStartTime + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.duration || 0.25));
                
                // Check if note belongs to this segment AND if the note itself is potentially visible
                if (noteStartTime >= segmentStart && noteStartTime < segmentEnd) {
                  const noteX = centerX + (noteStartTime - currentTimeSeconds) * pixelsPerSecond;
                  const noteDuration = noteEndTime - noteStartTime;
                  const noteWidth = Math.max(1, Math.floor(noteDuration * pixelsPerSecond));
                  
                  // Only include notes that are actually visible on screen
                  if (noteX + noteWidth >= -10 && noteX < screen.width + 10) {
                    segmentNotes.push({
                      ...note,
                      startTime: noteStartTime,
                      endTime: noteEndTime,
                      clipName: clip.name,
                      clipIndex: clipIndex, // Add clip index for color calculation
                      noteIndex: noteIndex, // Add note index for color calculation
                      locatorIndex: locatorIndex, // Add locator index for stable row calculation
                      trackIndex: clip.trackIndex
                    });
                  }
                }
              });
            }
          });
        }
        
        if (segmentNotes.length > 0) {
          // Draw MIDI notes as small rectangles with no gaps
          const notesAreaHeight = timelineHeight - 2; // Use almost full timeline height
          const notesAreaStartY = timelineY + 1; // Start 1px from top
          const noteHeight = Math.max(2, Math.floor(notesAreaHeight / 10)); // Fixed height, max 10 rows
          const maxRows = Math.floor(notesAreaHeight / noteHeight);
          
          segmentNotes.forEach((note, noteIndex) => {
            const noteX = centerX + (note.startTime - currentTimeSeconds) * pixelsPerSecond;
            const pitch = note.key || 60;
            // Use stable row calculation based on original note position
            const stableIndex = (note.locatorIndex * 100) + (note.clipIndex * 20) + note.noteIndex;
            const row = stableIndex % maxRows;
            const noteY = notesAreaStartY + row * noteHeight; // No spacing, notes touch
            
            // Note is already pre-filtered for visibility, so just draw it
            const velocity = note.velocity || 100;
            const noteDuration = note.endTime - note.startTime; // Single declaration for the whole block
            const velocityNorm = velocity / 127; // Move this outside color block so it's available everywhere
              
              // Generate themed color using the palette of the note's own locator section
              let r, g, b;
              // Get the palette for the locator that this note belongs to
              const noteLocator = alsProject.locators[note.locatorIndex];
              const notePalette = getPaletteForLocator(noteLocator?.name);
              
              if (notePalette && notePalette.colors && notePalette.colors.length > 0) {
                // Use the note's own locator palette, not the current active palette
                const colorIndex = (note.clipIndex * 3 + note.noteIndex * 5 + pitch * 2) % notePalette.colors.length;
                const baseColor = notePalette.colors[colorIndex];
                
                // Apply velocity-based brightness variation (same as TV bars)
                const velocityFactor = 0.7 + (velocityNorm * 0.3); // 0.7 to 1.0
                
                r = Math.floor(baseColor.r * velocityFactor);
                g = Math.floor(baseColor.g * velocityFactor);
                b = Math.floor(baseColor.b * velocityFactor);
                
                // Ensure minimum brightness (same as TV bars)
                r = Math.max(30, r);
                g = Math.max(30, g);
                b = Math.max(30, b);
              } else {
                // Fallback to original HSL method if no palette
                const hue = (pitch * 7 + row * 60) % 360;
                const saturation = 0.7 + (velocityNorm * 0.3);
                const lightness = 0.3 + (velocityNorm * 0.5);
                
                const c = (1 - Math.abs(2 * lightness - 1)) * saturation;
                const x = c * (1 - Math.abs(((hue / 60) % 2) - 1));
                const m = lightness - c / 2;
                
                if (hue < 60) { r = c; g = x; b = 0; }
                else if (hue < 120) { r = x; g = c; b = 0; }
                else if (hue < 180) { r = 0; g = c; b = x; }
                else if (hue < 240) { r = 0; g = x; b = c; }
                else if (hue < 300) { r = x; g = 0; b = c; }
                else { r = c; g = 0; b = x; }
                
                r = Math.floor((r + m) * 255);
                g = Math.floor((g + m) * 255);
                b = Math.floor((b + m) * 255);
              }
              
              // Simple note width based on duration only
              const minWidth = 1;
              const durationWidth = Math.max(minWidth, Math.floor(noteDuration * pixelsPerSecond));
              // Use the fixed noteHeight calculated above for consistent spacing
              const finalHeight = noteHeight;
              
              // Check playing state - simplified without decay
              const noteEnd = note.endTime || (note.startTime + 0.25);
              const isPlaying = currentTimeSeconds >= note.startTime && currentTimeSeconds <= noteEnd;
              
              // Simple note width based on duration
              const noteWidth = Math.max(1, Math.floor(noteDuration * pixelsPerSecond));
              
              // Draw the note
              let noteR = r, noteG = g, noteB = b;
              if (isPlaying) {
                // Capture the base color BEFORE brightening for needle theming
                lastMidiNoteColor = { r: r, g: g, b: b };
                
                // Note is actively playing - use white or red for active notes
                if (velocity > 100) {
                  // High velocity notes are red when active
                  noteR = 255;
                  noteG = 0;
                  noteB = 0;
                } else {
                  // Normal velocity notes are white when active
                  noteR = 255;
                  noteG = 255;
                  noteB = 255;
                }
              }
              ink(noteR, noteG, noteB);
              box(noteX, noteY, noteWidth, finalHeight);
          });
        }
      }
    }
  } // Close the segmentNotes.forEach or similar loop that was missing
    
    // Draw themed needle with MIDI note color
    const needleX = centerX; // Center perfectly
    
    // Use the exact color of the last played MIDI note for the needle
    const needleColor = {
      r: lastMidiNoteColor.r,
      g: lastMidiNoteColor.g,
      b: lastMidiNoteColor.b
    };
    
    // Draw single needle line matching MIDI note color exactly
    // timelineY and timelineHeight are already declared at the top of the timeline conditional
    ink(needleColor.r, needleColor.g, needleColor.b, 255); // Full opacity
    line(needleX, timelineY, needleX, timelineY + timelineHeight - 1); // Single centered line, 1px shorter
    
  } // Close timeline visibility conditional
  
  // === PAUSE OVERLAY WITH PLAY BUTTON ===
  // Show play button overlay when not playing (but not when audio is starting) OR when explicitly shown during playback
  // Also show during initialization for visual feedback
  // BUT: Don't show overlay when recording (tape mode) - just show black
  // Check for any indication that we're in tape recording mode
  const showOverlay = (!inTapeMode) && ((!isPlaying && !audioStarting) || (isPlaying && pauseOverlayVisible) || audioInitializing);
  
  if (showOverlay) {
    const overlayTime = performance.now();
    
    // Auto-hide overlay after 3 seconds if playing has started
    if (isPlaying && pauseOverlayVisible) {
      if (overlayTime - pauseOverlayStartTime > 3000) {
        pauseOverlayVisible = false;
      }
    }
    
    // Semi-transparent black overlay - more black and white vibes
    ink(0, 0, 0, 180); // Darker overlay (was 120, now 180)
    box(0, 0, screen.width, screen.height);
    
    // Different visual feedback based on state
    const centerX = screen.width / 2;
    const centerY = screen.height / 2;
    const triangleSize = 14; // Same size as old wipppps
    
    if (audioInitializing) {
      // During initialization, the KidLisp BUFFERING source will handle visuals
      // Just show a simple loading indicator here as backup
      ink("orange");
      const pulsePhase = (overlayTime / 300) % 2; // 300ms cycle
      const opacity = pulsePhase < 1 ? Math.floor(pulsePhase * 255) : Math.floor((2 - pulsePhase) * 255);
      ink(255, 165, 0, opacity); // Orange with pulsing opacity
      box(centerX - 2, centerY - 2, 4, 4);
      
    } else {
      // Show normal play triangle when not initializing
      
      // Draw black shadow for play triangle (offset by 1px)
      ink(0, 0, 0, 255);
      shape([
        [centerX - triangleSize / 2 + 1, centerY - triangleSize / 2 + 1],
        [centerX + triangleSize / 2 + 1, centerY + 1],
        [centerX - triangleSize / 2 + 1, centerY + triangleSize / 2 + 1],
      ]);

      // Draw white outline for play triangle (slightly larger)
      ink(255, 255, 255, 255);
      shape([
        [centerX - triangleSize / 2 - 1, centerY - triangleSize / 2 - 1],
        [centerX + triangleSize / 2 + 1, centerY],
        [centerX - triangleSize / 2 - 1, centerY + triangleSize / 2 + 1],
      ]);

      // Draw main white play triangle (black and white vibes - no rainbow)
      ink(255, 255, 255, 255);
      shape([
        [centerX - triangleSize / 2, centerY - triangleSize / 2],
        [centerX + triangleSize / 2, centerY],
        [centerX - triangleSize / 2, centerY + triangleSize / 2],
      ]);
    }
  }
  
  // === PROGRESS BAR AND TRACK INFO ===
  // Progress bar at the bottom - drawn AFTER all other overlays to appear on top
  // BUT: Hide progress bar during tape recording mode
  if (progress !== undefined && actualDuration && !inTapeMode) {
    const barHeight = 2; // 2px tall progress bar (thinner)
    const barY = screen.height - barHeight;

    // Background bar (dark) - full width
    ink(0, 0, 0, 255);
    box(0, barY, screen.width, barHeight);

    // Progress bar - red when playing or paused
    const progressWidth = progress * screen.width;
    if (progressWidth > 0) {
      ink(255, 0, 0, 255); // Red progress bar
      box(0, barY, progressWidth, barHeight);
    }
  }

  // Draw track timer in bottom right corner - COMMENTED OUT
  /*
  if (actualDuration) {
    // Use the same timing calculation as progress for consistency
    let currentTime = 0;
    if (isPlaying) {
      const sessionElapsed = (performance.now() - playStartTime) / 1000;
      currentTime = (pausedAt * actualDuration) + sessionElapsed;
    } else {
      // When paused, show the paused position
      currentTime = pausedAt * actualDuration;
    }

    // Format time as MM:SS
    const formatTime = (seconds) => {
      const mins = Math.floor(seconds / 60);
      const secs = Math.floor(seconds % 60);
      return `${mins}:${secs.toString().padStart(2, "0")}`;
    };

    const timeText = `${formatTime(currentTime)} / ${formatTime(actualDuration)}`;

    // Position in bottom right, accounting for text width
    const textX = screen.width - timeText.length * 6 - 6; // Approximate character width
    const textY = screen.height - 16; // Higher up to be above progress bar

    // Draw shadow (offset by 1px down and right)
    ink(32, 32, 32); // Set dark gray ink for shadow
    write(timeText, textX + 1, textY + 1);

    // Draw main text in white
    ink(255, 255, 255); // Set white ink for main text
    write(timeText, textX, textY);
  }
  */

  // Draw artist and track info in bottom left corner - COMMENTED OUT
  /*
  const artistText = "oskie - zzzZWAP";
  const artistX = 6; // Small margin from left edge
  const artistY = screen.height - 16; // Same height as timer

  // Draw shadow for artist text
  ink(32, 32, 32); // Dark gray shadow
  write(artistText, artistX + 1, artistY + 1);

  // Draw main artist text in white
  ink(255, 255, 255); // White text
  write(artistText, artistX, artistY);
  */
  
} // Close paint function

async function act({ event: e, sound, api }) {
  // Track recording state for paint function - only use api.rec.recording for actual recording
  const prevIsCurrentlyRecording = isCurrentlyRecording;
  isCurrentlyRecording = api?.rec?.recording === true;
  
  // Debug the actual API values
  console.log(`🎬 ACT_DEBUG: api.rec.recording=${api?.rec?.recording}, api.rec.loadCallback=${api?.rec?.loadCallback}, isCurrentlyRecording=${isCurrentlyRecording}`);
  
  // Debug logging when recording state changes
  if (isCurrentlyRecording !== prevIsCurrentlyRecording) {
    console.log(`🎬 RECORDING_STATE_CHANGED: isCurrentlyRecording=${isCurrentlyRecording} (was ${prevIsCurrentlyRecording}), api.rec.recording=${api?.rec?.recording}, api.rec.loadCallback=${api?.rec?.loadCallback !== null ? 'set' : 'null'}`);
  }
  
  // Reset auto-play flag when not recording to allow it to work again next time
  if (!api?.rec?.recording && !api?.rec?.loadCallback) {
    tapeAutoPlayStarted = false;
  }
  
  // 🎵 TAPE_MODE_AUTO_PLAY: Automatically start playback when in tape mode
  // Check for recording state in api.rec instead of api.system.taping
  // BUT: Only auto-play during actual recording, not during video playback of the tape
  const isRecording = api?.rec?.recording; // Only during active recording, not when loadCallback is set (playback)
  if (isRecording && !isPlaying && !audioStarting && preloadedAudio && !tapeAutoPlayStarted) {
    console.log("🎵 TAPE_MODE: Auto-starting playback for tape mode (detected via api.rec.recording)");
    tapeAutoPlayStarted = true; // Prevent multiple auto-starts
    
    // Force audio context to running state for tape mode
    try {
      if (sound.bios && sound.bios.audioContext) {
        console.log(`🎵 TAPE_MODE: Audio context state: ${sound.bios.audioContext.state}`);
        if (sound.bios.audioContext.state === 'suspended') {
          console.log("🎵 TAPE_MODE: Resuming suspended audio context");
          sound.bios.audioContext.resume();
        } else if (sound.bios.audioContext.state === 'running') {
          console.log("🎵 TAPE_MODE: Audio context already running - good for autoplay");
        }
      }
    } catch (error) {
      console.log("🎵 TAPE_MODE: Could not check audio context:", error);
    }
    
    // Try to start audio directly without synthetic events first
    try {
      if (preloadedAudio && typeof preloadedAudio.play === 'function') {
        console.log("🎵 TAPE_MODE: Attempting direct audio play");
        const playPromise = preloadedAudio.play();
        if (playPromise && playPromise.then) {
          playPromise.then(() => {
            console.log("🎵 TAPE_MODE: Direct audio play succeeded");
            // Set audio state variables to reflect that it's playing
            isPlaying = true;
            audioStarting = false;
            startAudioStartDetection();
          }).catch(err => {
            console.log("🎵 TAPE_MODE: Direct audio play failed, trying synthetic event:", err);
            // Fallback to synthetic event
            const syntheticTouchEvent = { is: (type) => type === "touch" };
            return act({ event: syntheticTouchEvent, sound, api });
          });
        }
        return; // Exit early if we tried direct play
      }
    } catch (error) {
      console.log("🎵 TAPE_MODE: Direct play error:", error);
    }
    
    // Fallback: use synthetic touch event
    const syntheticTouchEvent = {
      is: (type) => type === "touch"
    };
    
    // Temporarily override the event and recursively call act to trigger playback
    return act({ event: syntheticTouchEvent, sound, api });
  }

  // Toggle both timeline and frequency display with 'tab' key only
  if (e.is("keyboard:tab") || (e.is("keyboard:down:tab"))) {
    timelineVisible = !timelineVisible;
    frequencyDisplayVisible = !frequencyDisplayVisible;
    console.log("Timeline visibility toggled:", timelineVisible);
    console.log("Frequency display visibility toggled:", frequencyDisplayVisible);
  }
  
  // Toggle KidLisp mode with 'k' key
  if (e.is("keyboard:k")) {
    kidlispMode = !kidlispMode;
    console.log("KidLisp mode toggled:", kidlispMode);
  }
  
  // Simple play/pause control
  if (e.is("keyboard:p")) {
    timelineOffset += 0.5; // Look further ahead
  }
  
  if (e.is("keyboard:minus")) {
    timelineOffset = Math.max(0, timelineOffset - 0.5); // Look less ahead (minimum 0)
  }
  
  // Handle play/pause/resume
  if (e.is("touch") && preloadedAudio) {
    // console.log(`🎵 TOUCH_EVENT: isPlaying=${isPlaying}, audioStarting=${audioStarting}, playingSfx=${!!playingSfx}, killed=${playingSfx?.killed}`);
    
    if (isPlaying && playingSfx && !playingSfx.killed) {
      // Currently playing - pause the track
      // console.log("🎵 PAUSING_PLAYBACK: Touch detected while playing, pausing audio");
      
      if (actualDuration) {
        // Use the visual progress that we've been maintaining, not the potentially wrong audio progress
        let newPausedAt;
        
        // Always use the visual progress since we've been protecting it from bad audio progress
        newPausedAt = Math.min(progress, 1.0); // Use our carefully maintained visual progress
        console.log(`⏸️ Using visual progress for pause: ${(newPausedAt * 100).toFixed(1)}% (${(newPausedAt * actualDuration).toFixed(2)}s)`);
        
        // Log comparison for debugging
        if (currentAudioProgress !== null && currentAudioProgress > 0) {
          const audioPausedAt = Math.min(currentAudioProgress, 1.0);
          const progressDiff = Math.abs(newPausedAt - audioPausedAt);
          console.log(`⏸️ Progress comparison: visual=${(newPausedAt * 100).toFixed(1)}%, audio=${(audioPausedAt * 100).toFixed(1)}%, diff=${(progressDiff * 100).toFixed(1)}%`);
        }
        
        pausedAt = newPausedAt;
        
        // Set state before killing to prevent race condition in sim()
        isPlaying = false;
        audioStarting = false; // Clear any starting state
        audioInitializing = false; // Clear any initialization state
        pauseOverlayVisible = true; // Show pause overlay when paused
        
        playingSfx.kill();
        playingSfx = null;
      } else {
        isPlaying = false;
        audioStarting = false;
        audioInitializing = false; // Clear any initialization state
        pauseOverlayVisible = true; // Show pause overlay when paused  
      }
      console.log("🎵 PAUSED");
      return;
    }
    
    // Prevent multiple touches from starting multiple audio instances when not playing
    if (audioStarting || (playingSfx && !playingSfx.killed)) {
      console.log("🎵 TOUCH_IGNORED: Audio already starting or about to start");
      
      // EMERGENCY ESCAPE: If user taps multiple times while stuck in detection loop, force start
      const now = performance.now();
      if (!lastEmergencyTap) lastEmergencyTap = now;
      
      const timeSinceLastTap = now - lastEmergencyTap;
      if (timeSinceLastTap < 2000) { // Multiple taps within 2 seconds
        emergencyTapCount = (emergencyTapCount || 0) + 1;
        console.log(`🎵 EMERGENCY_TAP: Tap ${emergencyTapCount} detected while stuck in detection`);
        
        if (emergencyTapCount >= 3) { // 3 quick taps = emergency escape
          console.warn("🎵 EMERGENCY_ESCAPE: Force starting audio due to repeated taps during detection loop");
          
          // Force clear all blocking states
          audioStartDetected = true;
          audioStarting = false;
          audioInitializing = false;
          isPlaying = true;
          
          // Initialize visual progress
          if (pausedAt > 0) {
            progress = pausedAt;
          } else {
            progress = 0;
          }
          
          // Start visual timeline
          const resumeOffset = pausedAt * actualDuration;
          playStartTime = performance.now() - (resumeOffset * 1000);
          
          console.log("🎵 EMERGENCY_TIMELINE_START: Force started visual timeline");
          
          // Reset emergency counters
          emergencyTapCount = 0;
          lastEmergencyTap = null;
          
          return; // Don't process this as a normal touch
        }
      } else {
        // Reset counter if too much time has passed
        emergencyTapCount = 0;
      }
      
      lastEmergencyTap = now;
      return;
    }
    
    if (!isPlaying && !audioStarting) {
      // Not currently playing - start or resume playback
      
      // 🎵 MOBILE AUDIO CONTEXT ACTIVATION CHECK
      // Ensure audio context is running (required for mobile browsers)
      if (sound && sound.enabled && sound.enabled()) {
        console.log("🎵 Audio context is enabled and ready");
      } else {
        console.warn("🎵 Audio context not enabled - attempting activation");
        message = "Activating audio...";
        // The touch event should activate the audio context automatically
        // but let's add a small delay to ensure it's ready
        await new Promise(resolve => setTimeout(resolve, 200));
        
        // Check again after delay
        if (!sound || !sound.enabled || !sound.enabled()) {
          console.error("🎵 Audio context failed to activate!");
          message = "Audio context failed - try again";
          return;
        }
      }
      
      // 🎵 VERIFY AUDIO IS READY
      if (!preloadedAudio) {
        console.error("🎵 AUDIO_NOT_READY: No preloaded audio available");
        message = "Audio not loaded";
        return;
      }
      
      console.log(`🎵 AUDIO_READY: Audio object available for playback`);
      console.log("🎵 Audio object type:", typeof preloadedAudio);
      if (typeof preloadedAudio === 'object') {
        console.log("🎵 Audio object keys:", Object.keys(preloadedAudio));
      }
      
      // Reset tracking when starting playback
      if (pausedAt === 0) {
        passedLocators.clear();
        console.log("🎵 Starting from beginning - reset locator tracking");
      } else {
        console.log(`🎵 Resuming from ${(pausedAt * 100).toFixed(1)}% (${(pausedAt * actualDuration).toFixed(2)}s)`);
      }
      
      // Hide pause overlay immediately when starting playback
      pauseOverlayVisible = false;
      
      // Set initialization flag to provide feedback during audio loading
      audioInitializing = true;
      console.log("🎵 AUDIO_INITIALIZING: Set initialization flag for visual feedback");
      
      // Start playing from the paused position
      const playStartTimestamp = performance.now();
      const audioTimeBeforePlay = sound.time;
      
      // 🎵 COMPREHENSIVE AUDIO START LOGGING
      console.log(`🎵 AUDIO_START_ATTEMPT: pausedAt=${pausedAt.toFixed(6)}, duration=${actualDuration?.toFixed(3) || 'unknown'}s, audioTime=${audioTimeBeforePlay?.toFixed(6) || 'unknown'}s`);
      
      // Start playing from the correct position
      if (pausedAt > 0) {
        // Resume from a specific position
        playingSfx = sound.play(preloadedAudio, { speed: 1, from: pausedAt });
        console.log(`🎵 AUDIO_PLAY: Resuming from position ${(pausedAt * 100).toFixed(1)}%`);
      } else {
        // Start from beginning - don't use 'from' parameter
        playingSfx = sound.play(preloadedAudio, { speed: 1 });
        console.log(`🎵 AUDIO_PLAY: Starting from beginning`);
      }
      const playCallDuration = performance.now() - playStartTimestamp;
      const audioTimeAfterPlay = sound.time;
      
      if (playingSfx) {
        // 🎵 DON'T START VISUAL TIMELINE YET - wait for actual audio to begin
        // Set a flag that we're waiting for audio to start
        isPlaying = false; // Keep false until we detect actual audio
        audioStarting = true; // NEW: Hide overlay while audio is starting
        let audioStartDetected = false;
        playStartTime = null; // Will be set when audio actually starts
        
        // 🎵 DETAILED SUCCESS LOGGING
        console.log(`🎵 AUDIO_PLAY_CALLED: play() took ${playCallDuration.toFixed(3)}ms, audioTime: ${audioTimeBeforePlay?.toFixed(6) || 'N/A'}s → ${audioTimeAfterPlay?.toFixed(6) || 'N/A'}s`);
        console.log(`🎵 WAITING_FOR_AUDIO: Waiting for actual audio to start before beginning visual timeline`);
        
        // Validate playingSfx object structure for progress tracking
        console.log(`🎵 PLAY_OBJECT: hasProgress=${typeof playingSfx.progress === 'function'}, hasKill=${typeof playingSfx.kill === 'function'}, hasPlayResult=${!!playingSfx.playResult}`);
        
        // 🎵 FORCE SEEK AFTER PLAY - attempt to correct false starts
        if (pausedAt > 0 && typeof playingSfx.seek === 'function') {
          const seekTarget = pausedAt * actualDuration; // Convert to seconds
          console.log(`🎵 FORCE_SEEK: Attempting to seek to ${seekTarget.toFixed(3)}s (${(pausedAt * 100).toFixed(1)}%)`);
          try {
            playingSfx.seek(seekTarget);
            console.log(`🎵 FORCE_SEEK: Seek completed`);
          } catch (seekError) {
            console.warn(`🎵 FORCE_SEEK_FAILED: ${seekError.message}`);
          }
        } else if (pausedAt > 0) {
          console.log(`🎵 FORCE_SEEK: Skipped - seek method not available, but pausedAt=${pausedAt.toFixed(6)}`);
        } else {
          console.log(`🎵 FORCE_SEEK: Skipped - starting from beginning, pausedAt=${pausedAt}`);
        }
        
        // 🎵 AUDIO START DETECTION LOOP
        // Check for actual audio progress to confirm audio has started
        const checkAudioStart = async () => {
          let attempts = 0;
          const maxAttempts = 20; // Reduced from 100 to 20 (1 second timeout with 50ms intervals)
          let consecutiveFailures = 0; // Track consecutive progress() failures
          
          console.log(`🎵 DETECTION_START: Beginning audio start detection loop, maxAttempts=${maxAttempts}`);
          
          while (!audioStartDetected && attempts < maxAttempts && playingSfx && !playingSfx.killed) {
            attempts++;
            console.log(`🎵 DETECTION_ATTEMPT_${attempts}: Starting attempt ${attempts}/${maxAttempts}`);
            
            try {
              let progressCallSucceeded = false;
              
              // Check if we're getting actual audio progress > 0 (with timeout)
              if (typeof playingSfx.progress === 'function') {
                console.log(`🎵 DETECTION_ATTEMPT_${attempts}: Calling progress() with timeout`);
                
                // Add timeout to progress call to prevent hanging
                const progressPromise = playingSfx.progress();
                const timeoutPromise = new Promise((_, reject) => 
                  setTimeout(() => reject(new Error('Progress timeout')), 200) // Shorter timeout for responsiveness
                );
                
                try {
                  const progressData = await Promise.race([progressPromise, timeoutPromise]);
                  progressCallSucceeded = true;
                  consecutiveFailures = 0; // Reset failure counter
                  console.log(`🎵 DETECTION_ATTEMPT_${attempts}: progress=${progressData?.progress || 'null'}, killed=${playingSfx.killed}`);
                  
                  if (progressData && typeof progressData.progress === 'number' && progressData.progress >= 0) {
                    // Verify the progress is reasonably close to where we expect it
                    const expectedProgress = pausedAt;
                    const actualProgress = progressData.progress;
                    const progressDiff = Math.abs(actualProgress - expectedProgress);
                    
                    console.log(`🎵 PROGRESS_VERIFICATION: expected=${expectedProgress.toFixed(6)}, actual=${actualProgress.toFixed(6)}, diff=${progressDiff.toFixed(6)}`);
                    
                    // If progress is too far off, try seeking again
                    if (pausedAt > 0 && progressDiff > 0.05 && typeof playingSfx.seek === 'function') { // 5% tolerance
                      const seekTarget = pausedAt * actualDuration;
                      console.log(`🎵 RETRY_SEEK: Progress too far off, re-seeking to ${seekTarget.toFixed(3)}s`);
                      try {
                        playingSfx.seek(seekTarget);
                        // Continue the loop to check again
                        await new Promise(resolve => setTimeout(resolve, 100)); // Wait a bit for seek to take effect
                        continue;
                      } catch (seekError) {
                        console.warn(`🎵 RETRY_SEEK_FAILED: ${seekError.message}`);
                      }
                    }
                    
                    // Audio has actually started! (any progress >= 0 means it's running)
                    console.log(`🎵 AUDIO_START_DETECTED: Audio actually playing after ${attempts * 50}ms, progress=${progressData.progress.toFixed(6)}`);
                    audioStartDetected = true;
                    audioStarting = false; // Clear starting flag - this will hide the overlay
                    audioInitializing = false; // Clear initialization flag - audio is fully ready
                    isPlaying = true;
                    
                    // Initialize visual progress to maintain continuity
                    if (pausedAt > 0) {
                      // When resuming, start visual progress from where we paused
                      progress = pausedAt;
                      console.log(`🎵 VISUAL_INIT: Starting visual progress from pausedAt=${pausedAt.toFixed(6)}`);
                    } else {
                      // Starting fresh from beginning
                      progress = 0;
                      console.log(`🎵 VISUAL_INIT: Starting visual progress from beginning`);
                    }
                    
                    // More precise timing: account for the detection delay and maintain visual continuity
                    const audioCurrentTime = progressData.progress * actualDuration;
                    // When resuming from pause, maintain visual timeline continuity
                    const resumeOffset = pausedAt * actualDuration; // Where we should visually be
                    playStartTime = performance.now() - (resumeOffset * 1000); // Base on where we paused, not current audio
                    console.log(`🎵 VISUAL_TIMELINE_START: playStartTime=${playStartTime}, audioCurrentTime=${audioCurrentTime.toFixed(3)}s, resumeOffset=${resumeOffset.toFixed(3)}s, pausedAt=${pausedAt.toFixed(6)}`);
                    console.log(`🎵 INITIALIZATION_COMPLETE: audioInitializing=${audioInitializing}, ready for playback`);
                    break;
                  }
                } catch (progressError) {
                  consecutiveFailures++;
                  console.log(`🎵 DETECTION_ATTEMPT_${attempts}: Progress call failed: ${progressError.message} (consecutive failures: ${consecutiveFailures})`);
                  
                  // If we've had too many consecutive progress failures, give up and start anyway
                  if (consecutiveFailures >= 10) {
                    console.warn(`🎵 PROGRESS_FAILURE_LIMIT: Too many consecutive progress failures (${consecutiveFailures}), starting visual timeline anyway`);
                    break; // Exit the loop and start the fallback
                  }
                }
              } else {
                console.log(`🎵 DETECTION_ATTEMPT_${attempts}: progress function not available`);
              }
              
              // Always check the speaker for audio activity (more reliable)
              if (sound.speaker && sound.speaker.amplitudes) {
                const leftAmp = Number(sound.speaker.amplitudes.left) || 0;
                const rightAmp = Number(sound.speaker.amplitudes.right) || 0;
                console.log(`🎵 DETECTION_ATTEMPT_${attempts}: amplitudes L=${leftAmp.toFixed(4)}, R=${rightAmp.toFixed(4)}`);
                
                if (leftAmp > 0.001 || rightAmp > 0.001) { // Lower threshold for detection
                  console.log(`🎵 AUDIO_START_DETECTED: Speaker amplitude detected after ${attempts * 50}ms, L=${leftAmp.toFixed(3)}, R=${rightAmp.toFixed(3)}`);
                  audioStartDetected = true;
                  audioStarting = false; // Clear starting flag - this will hide the overlay
                  audioInitializing = false; // Clear initialization flag - audio is fully ready
                  isPlaying = true;
                  
                  // Initialize visual progress to maintain continuity
                  if (pausedAt > 0) {
                    // When resuming, start visual progress from where we paused
                    progress = pausedAt;
                    console.log(`🎵 VISUAL_INIT: Starting visual progress from pausedAt=${pausedAt.toFixed(6)}`);
                  } else {
                    // Starting fresh from beginning
                    progress = 0;
                    console.log(`🎵 VISUAL_INIT: Starting visual progress from beginning`);
                  }
                  
                  // When resuming from pause, maintain visual timeline continuity
                  const resumeOffset = pausedAt * actualDuration; // Where we should visually be
                  playStartTime = performance.now() - (resumeOffset * 1000); // Base on where we paused, not detection delay
                  console.log(`🎵 VISUAL_TIMELINE_START: playStartTime=${playStartTime} (amplitude-based detection), resumeOffset=${resumeOffset.toFixed(3)}s, pausedAt=${pausedAt.toFixed(6)}`);
                  console.log(`🎵 INITIALIZATION_COMPLETE: audioInitializing=${audioInitializing}, ready for playback`);
                  break;
                }
              } else {
                console.log(`🎵 DETECTION_ATTEMPT_${attempts}: speaker amplitudes not available`);
              }
              
            } catch (err) {
              console.warn(`🎵 AUDIO_START_CHECK_ERROR: attempt ${attempts}, ${err.message}`);
            }
            
            // Wait 50ms before checking again (more responsive)
            console.log(`🎵 DETECTION_ATTEMPT_${attempts}: Waiting 50ms before next attempt`);
            await new Promise(resolve => setTimeout(resolve, 50));
          }
          
          if (!audioStartDetected) {
            console.warn(`🎵 AUDIO_START_TIMEOUT: Failed to detect audio start after ${maxAttempts * 50}ms (${consecutiveFailures} consecutive failures), starting visual timeline anyway`);
            // Force clear all blocking states and start visual timeline
            audioStartDetected = true; // Set to true to prevent any further detection attempts
            audioStarting = false; // Clear starting flag
            audioInitializing = false; // Clear initialization flag - timeout reached
            isPlaying = true;
            
            // Initialize visual progress to maintain continuity
            if (pausedAt > 0) {
              // When resuming, start visual progress from where we paused
              progress = pausedAt;
              console.log(`🎵 VISUAL_INIT: Starting visual progress from pausedAt=${pausedAt.toFixed(6)} (fallback)`);
            } else {
              // Starting fresh from beginning
              progress = 0;
              console.log(`🎵 VISUAL_INIT: Starting visual progress from beginning (fallback)`);
            }
            
            // When resuming from pause, maintain visual timeline continuity even on timeout
            const resumeOffset = pausedAt * actualDuration; // Where we should visually be
            playStartTime = performance.now() - (resumeOffset * 1000); // Base on where we paused
            console.log(`🎵 VISUAL_TIMELINE_START: playStartTime=${playStartTime} (timeout fallback), resumeOffset=${resumeOffset.toFixed(3)}s, pausedAt=${pausedAt.toFixed(6)}`);
            console.log(`🎵 INITIALIZATION_COMPLETE: audioInitializing=${audioInitializing}, timeout fallback - forcing start`);
          }
          
          console.log(`🎵 DETECTION_END: audioStartDetected=${audioStartDetected}, isPlaying=${isPlaying}, audioStarting=${audioStarting}`);
        };
        
        // Start the audio detection process (non-blocking)
        console.log(`🎵 DETECTION_INIT: Starting checkAudioStart() function`);
        checkAudioStart().then(() => {
          console.log(`🎵 DETECTION_COMPLETE: checkAudioStart() completed successfully`);
        }).catch(err => {
          console.error(`🎵 AUDIO_START_DETECTION_ERROR: ${err.message}`);
          // Fallback: start visual timeline anyway
          audioStarting = false; // Clear starting flag
          isPlaying = true;
          
          // Initialize visual progress to maintain continuity
          if (pausedAt > 0) {
            // When resuming, start visual progress from where we paused
            progress = pausedAt;
            console.log(`🎵 VISUAL_INIT: Starting visual progress from pausedAt=${pausedAt.toFixed(6)}`);
          } else {
            // Starting fresh from beginning
            progress = 0;
            console.log(`🎵 VISUAL_INIT: Starting visual progress from beginning`);
          }
          
          // When resuming from pause, maintain visual timeline continuity even on error
          const resumeOffset = pausedAt * actualDuration; // Where we should visually be
          playStartTime = performance.now() - (resumeOffset * 1000); // Base on where we paused
          console.log(`🎵 VISUAL_TIMELINE_START: playStartTime=${playStartTime} (error fallback), resumeOffset=${resumeOffset.toFixed(3)}s, pausedAt=${pausedAt.toFixed(6)}`);
        });
        
        // Try multiple ways to get duration with enhanced logging
        let durationSources = [];
        
        // Method 1: From the BIOS sample info
        if (sound.speaker && sound.speaker.length && sound.speaker.sampleRate) {
          const speakerDuration = sound.speaker.length / sound.speaker.sampleRate;
          durationSources.push(`speaker: ${speakerDuration.toFixed(3)}s`);
          if (!actualDuration) actualDuration = speakerDuration;
        }
        
        // Method 2: From playResult
        if (playingSfx.playResult && playingSfx.playResult.buffer) {
          const playResultDuration = playingSfx.playResult.buffer.duration;
          durationSources.push(`playResult: ${playResultDuration.toFixed(3)}s`);
          if (!actualDuration) actualDuration = playResultDuration;
        }
        
        // Method 3: From the preloaded audio metadata
        if (preloadedAudio && preloadedAudio.buffer && preloadedAudio.buffer.duration) {
          const preloadedDuration = preloadedAudio.buffer.duration;
          durationSources.push(`preloaded: ${preloadedDuration.toFixed(3)}s`);
          if (!actualDuration) actualDuration = preloadedDuration;
        }
        
        // Fallback: calculate from sample info
        if (!actualDuration) {
          actualDuration = 8831329 / 48000; // About 184 seconds based on the console output
          durationSources.push(`fallback: ${actualDuration.toFixed(3)}s`);
        }
        
        console.log(`🎵 DURATION_SOURCES: [${durationSources.join(', ')}] → final: ${actualDuration.toFixed(3)}s`);
        
        // Test immediate progress to catch false starts
        if (typeof playingSfx.progress === 'function') {
          playingSfx.progress().then((initialProgress) => {
            console.log(`🎵 INITIAL_PROGRESS: ${initialProgress?.progress?.toFixed(6) || 'N/A'} (should be ~${pausedAt.toFixed(6)})`);
            
            // Detect false start or sync issues
            if (initialProgress?.progress !== undefined) {
              const progressDiff = Math.abs(initialProgress.progress - pausedAt);
              if (progressDiff > 0.01) { // 1% tolerance
                console.warn(`🎵 FALSE_START_DETECTED: expected=${pausedAt.toFixed(6)}, actual=${initialProgress.progress.toFixed(6)}, diff=${progressDiff.toFixed(6)}`);
              }
            }
          }).catch(err => {
            console.warn(`🎵 PROGRESS_TEST_FAILED: ${err.message}`);
          });
        }
        
      } else {
        console.error("🎵 AUDIO_START_FAILED: sound.play() returned null/undefined!");
        console.error(`🎵 START_CONTEXT: preloadedAudio=${!!preloadedAudio}, pausedAt=${pausedAt}, audioTime=${audioTimeBeforePlay}`);
      }
    }
  }
}

// Simple auto-stop when audio ends
function sim({ sound, updateKidLispAudio }) {
  // Update KidLisp global variables with current audio data
  if (sound && sound.speaker && updateKidLispAudio) {
    // Get current amplitude values
    const leftAmp = sound.speaker.amplitudes?.left || 0;
    const rightAmp = sound.speaker.amplitudes?.right || 0;
    const avgAmp = (leftAmp + rightAmp) / 2;
    
    // Scale amplitude to a reasonable range for scroll/visual effects (0-10)
    const scaledAmp = Math.round(avgAmp * 10);
    
    // Get beat detection data
    const beatDetected = sound.speaker.beat?.detected || false;
    const beatStrength = sound.speaker.beat?.strength || 0;
    
    // Update KidLisp globals using the proper API
    updateKidLispAudio({
      amp: scaledAmp,
      leftAmp: Math.round(leftAmp * 10),
      rightAmp: Math.round(rightAmp * 10),
      beat: beatDetected ? 1 : 0,
      kick: beatDetected ? 1 : 0,  // Same as beat for now - kick drum detection
      // Add state flags for visual feedback
      isPlaying: isPlaying ? 1 : 0,
      audioStarting: audioStarting ? 1 : 0,
      audioInitializing: audioInitializing ? 1 : 0,
      pauseOverlayVisible: pauseOverlayVisible ? 1 : 0,
      // Add simple color string based on state
      stateColor: audioInitializing ? "orange" : 
                  audioStarting ? "yellow" : 
                  isPlaying ? "lime" : 
                  pauseOverlayVisible ? "red" : "white"
    });
  }

  // Check for audio ending naturally (but not manual pause)
  if (isPlaying && playingSfx && playingSfx.killed) {
    console.log("🎵 TRACK ENDED: Auto-stopping playback");
    isPlaying = false;
    progress = 0;
    pausedAt = 0; // Reset pause position when track ends
    playingSfx = null;
  }
  
  // Get actual audio progress from speaker worklet and compare with our calculations
  if (isPlaying && playingSfx && playingSfx.progress && typeof playingSfx.progress === 'function') {
    playingSfx.progress().then((p) => {
      if (p && typeof p.progress === 'number') {
        const previousAudioProgress = currentAudioProgress;
        currentAudioProgress = p.progress;
        
        // 🎵 COMPREHENSIVE SYNC DRIFT DETECTION
        if (progress > 0 && currentAudioProgress > 0) {
          const progressDiff = Math.abs(progress - currentAudioProgress);
          const timeDiff = Math.abs(progress * actualDuration - currentAudioProgress * actualDuration);
          
          if (progressDiff > 0.05) { // 5% difference threshold
            console.warn(`🎵 SYNC_DRIFT: calculated=${progress.toFixed(6)}, actual=${currentAudioProgress.toFixed(6)}, diff=${progressDiff.toFixed(6)} (${timeDiff.toFixed(3)}s)`);
          }
          
          // Alert on major sync issues
          if (timeDiff > 1.0) { // 1 second drift
            console.error(`🎵 MAJOR_SYNC_ISSUE: Time drift of ${timeDiff.toFixed(3)}s detected! Visual timeline may be out of sync.`);
          }
        }
        
        // Detect if audio stopped unexpectedly
        if (previousAudioProgress > 0 && currentAudioProgress === 0 && isPlaying) {
          console.warn("🎵 AUDIO_STOPPED_UNEXPECTEDLY: progress reset to 0 while playing");
        }
        
        // Detect audio stalls (progress not advancing)
        if (previousAudioProgress !== null && Math.abs(currentAudioProgress - previousAudioProgress) < 0.001 && isPlaying) {
          // Only log if this is a consistent stall (not just occasional timing)
          if (!stallWarningTime || performance.now() - stallWarningTime > 5000) {
            console.warn(`🎵 AUDIO_STALL: Progress stuck at ${currentAudioProgress.toFixed(6)} (previous: ${previousAudioProgress.toFixed(6)})`);
            stallWarningTime = performance.now();
          }
        }
        
        // Log progress details every few seconds (less frequent than timeline logging)
        if (Math.floor(currentAudioProgress * 1000) % 50 === 0) { // Every ~5% of track
          console.log(`🎵 PROGRESS_UPDATE: ${(currentAudioProgress * 100).toFixed(2)}% (${(currentAudioProgress * actualDuration).toFixed(2)}s/${actualDuration.toFixed(1)}s)`);
        }
        
      } else {
        // Handle invalid progress responses
        if (isPlaying) {
          console.warn(`🎵 INVALID_PROGRESS: Received invalid progress data:`, p);
        }
      }
    }).catch(err => {
      // Monitor progress polling errors
      if (!progressErrorCount) progressErrorCount = 0;
      progressErrorCount++;
      
      if (progressErrorCount > 10 && (!lastProgressError || performance.now() - lastProgressError > 10000)) {
        console.warn(`🎵 PROGRESS_ERRORS: ${progressErrorCount} progress polling errors detected`);
        lastProgressError = performance.now();
        progressErrorCount = 0;
      }
    });
  }
  
  // Poll speaker for real-time analysis (following wipppps pattern)
  sound.speaker?.poll();
}

export { paint, act, sim };
