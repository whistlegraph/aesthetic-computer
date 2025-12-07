// Weather, 2025.12.06
// A passive Weather Channel style display.

/* #region 📚 README 
  A non-interactive, ambient weather display that auto-cycles through
  current conditions, forecast, and details - inspired by The Weather Channel
  "Local on the 8s" displays from the 90s.
  
  Usage:
    weather              - Weather for default location (New York)
    weather [location]   - Weather for a city name or coordinates
    
  Examples:
    weather
    weather los angeles
    weather tokyo
    weather 48.8567,2.3508
    
  This piece is fully passive - no keyboard input needed.
  Views cycle automatically every ~10 seconds.
#endregion */

/* #region 🏁 TODO 
  - [x] Basic weather display
  - [x] Location geocoding via Open-Meteo (free, no API key)
  - [x] Auto-cycling views
  - [x] Bottom crawl text
  - [x] Weather Channel aesthetic
  - [x] Time-of-day and weather-based color themes
  - [x] Mixed fonts (unifont, MatrixChunky8, default)
  - [x] Animated backdrops (stars, moon, sun, clouds, rain, snow, fog, lightning)
  - [x] Proper text.box measurements for time positioning
  - [] IP-based geolocation fallback
  - [] Smooth jazz background music
#endregion */

// WMO Weather Code Mapping - ASCII icons only for MatrixChunky8 compatibility
const WEATHER_CODES = {
  0: { text: "Clear", short: "CLEAR", color: [255, 220, 64], icon: "*", type: "clear" },
  1: { text: "Mostly Clear", short: "M.CLR", color: [255, 220, 100], icon: "*", type: "clear" },
  2: { text: "Partly Cloudy", short: "P.CLD", color: [200, 200, 200], icon: "~", type: "cloudy" },
  3: { text: "Overcast", short: "OVCST", color: [150, 150, 150], icon: "=", type: "cloudy" },
  45: { text: "Fog", short: "FOG", color: [180, 180, 180], icon: "~", type: "fog" },
  48: { text: "Freezing Fog", short: "F.FOG", color: [180, 200, 220], icon: "~", type: "fog" },
  51: { text: "Light Drizzle", short: "L.DRZ", color: [100, 150, 200], icon: "'", type: "rain" },
  53: { text: "Drizzle", short: "DRIZL", color: [80, 130, 180], icon: "'", type: "rain" },
  55: { text: "Heavy Drizzle", short: "H.DRZ", color: [60, 110, 160], icon: ",", type: "rain" },
  56: { text: "Freezing Drizzle", short: "F.DRZ", color: [100, 180, 220], icon: "'", type: "rain" },
  57: { text: "Heavy Freezing Drizzle", short: "HF.DZ", color: [80, 160, 200], icon: ",", type: "rain" },
  61: { text: "Light Rain", short: "L.RAN", color: [80, 140, 200], icon: "|", type: "rain" },
  63: { text: "Rain", short: "RAIN", color: [60, 120, 180], icon: "|", type: "rain" },
  65: { text: "Heavy Rain", short: "H.RAN", color: [40, 80, 140], icon: "#", type: "rain" },
  66: { text: "Freezing Rain", short: "F.RAN", color: [100, 180, 240], icon: "|", type: "rain" },
  67: { text: "Heavy Freezing Rain", short: "HF.RN", color: [80, 160, 220], icon: "#", type: "rain" },
  71: { text: "Light Snow", short: "L.SNW", color: [220, 230, 255], icon: ".", type: "snow" },
  73: { text: "Snow", short: "SNOW", color: [200, 210, 240], icon: "o", type: "snow" },
  75: { text: "Heavy Snow", short: "H.SNW", color: [180, 190, 220], icon: "O", type: "snow" },
  77: { text: "Snow Grains", short: "S.GRN", color: [210, 220, 240], icon: ".", type: "snow" },
  80: { text: "Light Showers", short: "L.SHW", color: [100, 160, 220], icon: "|", type: "rain" },
  81: { text: "Showers", short: "SHWRS", color: [80, 140, 200], icon: "|", type: "rain" },
  82: { text: "Heavy Showers", short: "H.SHW", color: [60, 100, 160], icon: "#", type: "rain" },
  85: { text: "Light Snow Showers", short: "LS.SH", color: [200, 210, 240], icon: "o", type: "snow" },
  86: { text: "Heavy Snow Showers", short: "HS.SH", color: [180, 190, 220], icon: "O", type: "snow" },
  95: { text: "Thunderstorm", short: "STORM", color: [180, 100, 200], icon: "!", type: "storm" },
  96: { text: "Thunderstorm w/ Hail", short: "STM+H", color: [200, 120, 220], icon: "!", type: "storm" },
  99: { text: "Severe Thunderstorm", short: "S.STM", color: [220, 80, 180], icon: "!", type: "storm" },
};

// Color themes based on time and weather
const THEMES = {
  dayClear: {
    bgTop: [50, 150, 255],
    bgBottom: [20, 80, 200],
    headerBg: [30, 120, 230],
    panelBg: [20, 90, 180, 220],
    text: [255, 255, 255],
    textDim: [180, 220, 255],
    accent: [255, 220, 0],
    accent2: [255, 150, 50],
    temp: [255, 255, 80],
    crawlBg: [255, 180, 0],
    crawlText: [40, 30, 0],
    divider: [100, 180, 255],
  },
  dayCloudy: {
    bgTop: [100, 120, 160],
    bgBottom: [60, 80, 120],
    headerBg: [80, 100, 140],
    panelBg: [50, 70, 110, 220],
    text: [255, 255, 255],
    textDim: [200, 210, 230],
    accent: [255, 220, 150],
    accent2: [180, 200, 255],
    temp: [255, 255, 255],
    crawlBg: [120, 140, 180],
    crawlText: [255, 255, 255],
    divider: [140, 160, 200],
  },
  dayRain: {
    bgTop: [30, 60, 120],
    bgBottom: [15, 35, 80],
    headerBg: [25, 50, 110],
    panelBg: [20, 45, 90, 220],
    text: [200, 220, 255],
    textDim: [140, 170, 220],
    accent: [80, 200, 255],
    accent2: [150, 180, 255],
    temp: [150, 220, 255],
    crawlBg: [60, 100, 180],
    crawlText: [220, 240, 255],
    divider: [80, 120, 180],
  },
  daySnow: {
    bgTop: [200, 220, 255],
    bgBottom: [160, 180, 220],
    headerBg: [180, 200, 240],
    panelBg: [170, 190, 230, 220],
    text: [30, 50, 90],
    textDim: [80, 100, 140],
    accent: [50, 150, 255],
    accent2: [100, 180, 255],
    temp: [40, 80, 150],
    crawlBg: [230, 240, 255],
    crawlText: [30, 50, 100],
    divider: [150, 180, 220],
  },
  dayStorm: {
    bgTop: [60, 40, 100],
    bgBottom: [30, 20, 60],
    headerBg: [80, 50, 130],
    panelBg: [50, 35, 90, 220],
    text: [240, 230, 255],
    textDim: [180, 160, 220],
    accent: [255, 200, 50],
    accent2: [255, 100, 255],
    temp: [255, 240, 100],
    crawlBg: [200, 80, 255],
    crawlText: [255, 255, 255],
    divider: [140, 100, 200],
  },
  night: {
    bgTop: [15, 20, 60],
    bgBottom: [5, 10, 35],
    headerBg: [20, 30, 80],
    panelBg: [15, 25, 60, 220],
    text: [180, 200, 255],
    textDim: [120, 150, 200],
    accent: [255, 220, 80],
    accent2: [180, 140, 255],
    temp: [180, 220, 255],
    crawlBg: [40, 50, 120],
    crawlText: [200, 210, 255],
    divider: [60, 80, 150],
  },
  nightClear: {
    bgTop: [10, 15, 50],
    bgBottom: [5, 8, 30],
    headerBg: [15, 25, 70],
    panelBg: [12, 20, 55, 220],
    text: [200, 220, 255],
    textDim: [130, 160, 210],
    accent: [255, 255, 150],
    accent2: [100, 200, 255],
    temp: [200, 230, 255],
    crawlBg: [30, 40, 100],
    crawlText: [200, 220, 255],
    divider: [50, 70, 140],
  },
  sunset: {
    bgTop: [255, 120, 50],
    bgBottom: [200, 60, 100],
    headerBg: [240, 90, 70],
    panelBg: [200, 70, 80, 220],
    text: [255, 255, 230],
    textDim: [255, 200, 180],
    accent: [255, 255, 100],
    accent2: [255, 150, 200],
    temp: [255, 255, 200],
    crawlBg: [255, 100, 80],
    crawlText: [255, 255, 230],
    divider: [255, 150, 120],
  },
  sunrise: {
    bgTop: [255, 200, 100],
    bgBottom: [255, 140, 120],
    headerBg: [255, 170, 90],
    panelBg: [255, 150, 100, 220],
    text: [80, 40, 20],
    textDim: [140, 80, 50],
    accent: [255, 80, 0],
    accent2: [255, 200, 50],
    temp: [120, 60, 20],
    crawlBg: [255, 220, 100],
    crawlText: [100, 50, 20],
    divider: [255, 180, 140],
  },
};

const DAYS = ["SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"];
const FULL_DAYS = ["SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY"];

// State
let weatherData = null;
let location = null;
let loading = true;
let error = null;
let locationTimezone = "UTC"; // Timezone of the location (from API)

// View cycling
let currentView = 0;
let viewTimer = 0;
const VIEW_DURATION = 900; // ~15 seconds per view at 60fps
const NUM_VIEWS = 5; // Added scrolling text forecast view

// Animation
let frameCount = 0;
let crawlX = 0;
let crawlAccum = 0;
let crawlText = "";

// Scrolling text forecast
let textForecastScroll = 0;
let textForecastLines = [];

// Animated backdrop particles (2D fallback)
let particles = [];
let stars = [];
let clouds = [];
let lightningTimer = 0;
let lightningFlash = false;

// 3D System
let cam, dolly;
let camRotY = 0;  // Camera Y rotation for world spin
let camBobPhase = 0;  // Camera bob for organic movement
let starForms = [];
let moonForm = null;
let sunForm = null;
let cloudForms = [];
let rainForms = [];
let snowForms = [];
let use3D = false;
let Form3D = null;

// Smooth galaxy fly-through with orbital visits
let targetStarIndex = 0;
let prevStarIndex = -1;
let phaseTimer = 0;
let cameraMode = "cruise";  // "cruise", "approach", "orbit", "depart"

// Timing for each phase (in frames, ~60fps)
const CRUISE_DURATION = 300;    // ~5 sec cruising between visits
const APPROACH_DURATION = 180;  // ~3 sec flying toward star
const ORBIT_DURATION = 360;     // ~6 sec orbiting around star
const DEPART_DURATION = 120;    // ~2 sec flying away

// Camera position (smooth interpolated)
let camX = 0, camY = 2, camZ = 5;
let camTargetX = 0, camTargetY = 2, camTargetZ = 5;
let camVelX = 0, camVelY = 0, camVelZ = 0;

// Cruise path
let cruiseAngle = 0;
const CRUISE_SPEED = 0.002;     // Slow drift
const CRUISE_RADIUS = 6;
const CRUISE_HEIGHT = 3;

// Orbit parameters
let orbitAngle = 0;
let orbitRadius = 1.5;          // Distance from star when orbiting
let orbitCenterX = 0, orbitCenterY = 0, orbitCenterZ = 0;

// Look-at interpolation for smooth focus shifts
let lookTargetX = 0, lookTargetY = 2, lookTargetZ = -10;
let lookCurrentX = 0, lookCurrentY = 2, lookCurrentZ = -10;
const LOOK_SMOOTHING = 0.025;   // Smooth look interpolation

// Camera roll for cinematic feel
let targetRoll = 0;
let currentRoll = 0;

// ═══════════════════════════════════════════════════════════════════════════
// 🎶 AMBIENT MUSIC SYSTEM - Weather Channel smooth jazz vibes
// ═══════════════════════════════════════════════════════════════════════════

let musicEnabled = true;
let currentChord = null;
let chordTimer = 0;
let noteTimer = 0;
let percTimer = 0;
let activeSounds = [];
let soundAPI = null;
let musicStarted = false;

// Display state for visualization - scrolling chord list
let displayChord = null;      // Current chord for display
let chordFadeAlpha = 0;       // Fade animation for chord change
let chordDisplayTimer = 0;    // How long chord has been displayed
let lastPlayedNotes = [];     // Recently played notes for visualization
let chordScrollX = 0;         // Horizontal scroll position for chord display
let chordScrollTarget = 0;    // Target scroll position (for smooth scroll)
let chordHistory = [];        // List of all chords played for scrolling display
const MAX_CHORD_HISTORY = 20; // Keep last N chords in history

// Chord progressions for different weather moods (7th chords for smooth jazz feel)
const CHORD_PROGRESSIONS = {
  clear: [
    ['C', 'E', 'G', 'B'],    // Cmaj7
    ['D', 'F', 'A', 'C'],    // Dm7
    ['E', 'G', 'B', 'D'],    // Em7
    ['F', 'A', 'C', 'E'],    // Fmaj7
    ['G', 'B', 'D', 'F'],    // G7
    ['A', 'C', 'E', 'G'],    // Am7
  ],
  cloudy: [
    ['A', 'C', 'E', 'G'],    // Am7
    ['D', 'F', 'A', 'C'],    // Dm7
    ['G', 'B', 'D', 'F#'],   // Gmaj7
    ['C', 'E', 'G', 'B'],    // Cmaj7
    ['F', 'A', 'C', 'E'],    // Fmaj7
  ],
  rain: [
    ['D', 'F', 'A', 'C'],    // Dm7
    ['G', 'B', 'D', 'F'],    // G7
    ['C', 'E', 'G', 'B'],    // Cmaj7
    ['F', 'A', 'C', 'E'],    // Fmaj7
    ['A', 'C', 'E', 'G'],    // Am7
  ],
  snow: [
    ['G', 'B', 'D', 'F#'],   // Gmaj7
    ['E', 'G', 'B', 'D'],    // Em7
    ['A', 'C', 'E', 'G'],    // Am7
    ['D', 'F#', 'A', 'C'],   // D7
    ['C', 'E', 'G', 'B'],    // Cmaj7
  ],
  storm: [
    ['A', 'C', 'E'],         // Am
    ['D', 'F', 'A'],         // Dm
    ['E', 'G#', 'B', 'D'],   // E7
    ['F', 'A', 'C'],         // F
  ],
  night: [
    ['F', 'A', 'C', 'E'],    // Fmaj7
    ['E', 'G', 'B', 'D'],    // Em7
    ['D', 'F', 'A', 'C'],    // Dm7
    ['C', 'E', 'G', 'B'],    // Cmaj7
    ['A', 'C', 'E', 'G'],    // Am7
  ],
};

let musicOctave = 3;
let chordIndex = 0;

function getMusicMood() {
  if (isNight) return 'night';
  if (visualState === 'storm') return 'storm';
  if (visualState === 'rain') return 'rain';
  if (visualState === 'snow') return 'snow';
  if (visualState === 'cloudy' || visualState === 'fog') return 'cloudy';
  return 'clear';
}

function noteToTone(note, octave) {
  const noteMap = {
    'C': 'C', 'D': 'D', 'E': 'E', 'F': 'F', 'G': 'G', 'A': 'A', 'B': 'B',
    'C#': 'C#', 'D#': 'D#', 'F#': 'F#', 'G#': 'G#', 'A#': 'A#',
  };
  return `${octave}${noteMap[note] || note}`;
}

function playChord(chord, sound) {
  if (!sound || !musicEnabled || !chord) return;
  
  // Fade out existing sounds
  for (const s of activeSounds) {
    if (s && s.kill) try { s.kill(0.5); } catch(e) {}
  }
  activeSounds = [];
  
  // Update display state
  displayChord = chord;
  chordFadeAlpha = 255;
  chordDisplayTimer = 0;
  lastPlayedNotes = [];
  
  // Add to chord history for scrolling display
  const chordName = getChordName(chord);
  chordHistory.push({ chord, name: chordName, time: Date.now() });
  if (chordHistory.length > MAX_CHORD_HISTORY) {
    chordHistory.shift();
  }
  // Scroll to show new chord (smooth animation handled in sim)
  chordScrollTarget += 1;
  
  console.log(`🎶 Weather music: playing chord`, chordName);
  
  // Play each note with slight stagger for organic feel
  chord.forEach((note, i) => {
    setTimeout(() => {
      if (!musicEnabled || !sound) return;
      const noteOctave = musicOctave + (i < 2 ? 0 : 1);
      const tone = noteToTone(note, noteOctave);
      lastPlayedNotes.push({ note, octave: noteOctave, time: Date.now() });
      try {
        const synthSound = sound.synth({
          type: 'triangle',
          tone: tone,
          duration: 6,
          attack: 1.5,
          decay: 0.7,
          volume: 0.15 - (i * 0.02),
        });
        activeSounds.push(synthSound);
      } catch(e) { console.warn('Music synth error:', e); }
    }, i * 80);
  });
}

// Get human-readable chord name (e.g., "Cmaj7", "Dm7")
function getChordName(chord) {
  if (!chord || chord.length < 3) return chord?.join('') || '?';
  const root = chord[0];
  // Simple chord detection
  if (chord.length === 4) {
    // 7th chord - check quality
    if (chord[1] === 'E' || chord[1] === 'F#') return root + 'maj7';
    if (chord[1] === 'Eb' || chord[1] === 'F') return root + 'm7';
    return root + '7';
  }
  return chord.join('');
}

function playMelodyNote(chord, sound) {
  if (!sound || !musicEnabled || !chord || Math.random() > 0.3) return;
  const note = chord[Math.floor(Math.random() * chord.length)];
  const tone = noteToTone(note, musicOctave + 2);
  lastPlayedNotes.push({ note, octave: musicOctave + 2, time: Date.now(), melody: true });
  try {
    const melodySynth = sound.synth({
      type: 'sine',
      tone: tone,
      duration: 2,
      attack: 0.1,
      decay: 0.9,
      volume: 0.08,
    });
    activeSounds.push(melodySynth);
  } catch(e) {}
}

// Play ambient percussion / noise based on weather
function playPercussion(sound) {
  if (!sound || !musicEnabled) return;
  
  const mood = getMusicMood();
  
  // Rain: soft white noise bursts like raindrops
  if (mood === 'rain' || mood === 'storm') {
    if (Math.random() < 0.15) {
      try {
        sound.synth({
          type: 'noise',
          tone: '2C',  // Low filtered noise
          duration: 0.3 + Math.random() * 0.3,
          attack: 0.01,
          decay: 0.99,
          volume: 0.03 + Math.random() * 0.02,
        });
      } catch(e) {}
    }
  }
  
  // Storm: occasional thunder rumble
  if (mood === 'storm' && Math.random() < 0.02) {
    try {
      sound.synth({
        type: 'noise',
        tone: '1C',  // Very low rumble
        duration: 1.5 + Math.random() * 1,
        attack: 0.1,
        decay: 0.9,
        volume: 0.08 + Math.random() * 0.05,
      });
    } catch(e) {}
  }
  
  // Snow: crystalline high-frequency sparkles
  if (mood === 'snow' && Math.random() < 0.08) {
    try {
      sound.synth({
        type: 'sine',
        tone: `${6 + Math.floor(Math.random() * 2)}${['C', 'E', 'G'][Math.floor(Math.random() * 3)]}`,
        duration: 0.5,
        attack: 0.01,
        decay: 0.99,
        volume: 0.02,
      });
    } catch(e) {}
  }
  
  // Clear/cloudy: subtle breath-like swooshes
  if ((mood === 'clear' || mood === 'cloudy') && Math.random() < 0.03) {
    try {
      sound.synth({
        type: 'noise',
        tone: '4C',
        duration: 2 + Math.random() * 2,
        attack: 0.8,
        decay: 0.8,
        volume: 0.015,
      });
    } catch(e) {}
  }
  
  // Night: soft distant tones
  if (mood === 'night' && Math.random() < 0.05) {
    const nightTones = ['3E', '3G', '3B', '4D'];
    try {
      sound.synth({
        type: 'sine',
        tone: nightTones[Math.floor(Math.random() * nightTones.length)],
        duration: 3,
        attack: 1.5,
        decay: 0.9,
        volume: 0.025,
      });
    } catch(e) {}
  }
}

function updateMusic(sound) {
  if (!musicEnabled || !sound) return;
  soundAPI = sound;
  
  if (!musicStarted) {
    console.log('🎶 Weather ambient music system starting...');
    musicStarted = true;
  }
  
  const mood = getMusicMood();
  const progression = CHORD_PROGRESSIONS[mood] || CHORD_PROGRESSIONS.clear;
  
  // Update display fade
  if (chordFadeAlpha > 180) chordFadeAlpha -= 0.5;
  chordDisplayTimer++;
  
  chordTimer++;
  if (chordTimer > 240 || currentChord === null) {
    chordTimer = 0;
    chordIndex = (chordIndex + 1) % progression.length;
    currentChord = progression[chordIndex];
    playChord(currentChord, sound);
  }
  
  noteTimer++;
  if (noteTimer > 60 + Math.random() * 60) {
    noteTimer = 0;
    playMelodyNote(currentChord, sound);
  }
  
  // Percussion/ambient noise
  percTimer++;
  if (percTimer > 10) {
    percTimer = 0;
    playPercussion(sound);
  }
}

// Helper: Calculate angles to look at a 3D point from camera position
function lookAtAngles(camX, camY, camZ, targetX, targetY, targetZ) {
  const dx = targetX - camX;
  const dy = targetY - camY;
  const dz = targetZ - camZ;
  const distXZ = Math.sqrt(dx * dx + dz * dz);
  const rotY = Math.atan2(dx, -dz) * (180 / Math.PI);
  const rotX = Math.atan2(dy, distXZ) * (180 / Math.PI);
  return { rotX, rotY };
}

// Current visual state
let visualState = "clear"; // clear, cloudy, rain, snow, storm, fog
let isNight = false;

// Dynamic colors (computed based on time/weather)
let colors = THEMES.dayClear;

// Initialize 3D backdrop
function init3DBackdrop({ Form, Camera, Dolly, TRI, QUAD, painting }) {
  if (!Form || !Camera) {
    use3D = false;
    return;
  }
  
  use3D = true;
  Form3D = Form;
  
  // Create camera looking at the sky
  cam = new Camera(60, { x: 0, y: 0, z: 0 });
  dolly = new Dolly(cam);
  
  // Create 3D stars - mix of triangles and line crosses
  starForms = [];
  for (let i = 0; i < 60; i++) {
    const x = (Math.random() - 0.5) * 24;
    const y = (Math.random() - 0.5) * 12 + 4;
    const z = -5 - Math.random() * 20;
    const size = 0.03 + Math.random() * 0.05;
    
    let star;
    if (Math.random() < 0.4) {
      // Line cross star (+ shape)
      const linePositions = [
        [-size, 0, 0, 1], [size, 0, 0, 1],  // horizontal
        [0, -size, 0, 1], [0, size, 0, 1],  // vertical
      ];
      const lineColors = [
        [1, 1, 0.9, 1], [1, 1, 0.7, 1],
        [1, 1, 0.8, 1], [1, 1, 0.6, 1],
      ];
      star = new Form(
        { type: "line", positions: linePositions, colors: lineColors },
        { pos: [x, y, z], rot: [0, 0, Math.random() * 45], scale: 1 }
      );
      star.rotSpeed = (Math.random() - 0.5) * 2;
    } else if (Math.random() < 0.5) {
      // Diamond star (4 triangles)
      const d = size;
      const diamondPositions = [
        [0, d, 0, 1], [-d*0.6, 0, 0, 1], [0, 0, 0, 1],
        [0, d, 0, 1], [0, 0, 0, 1], [d*0.6, 0, 0, 1],
        [0, -d, 0, 1], [0, 0, 0, 1], [-d*0.6, 0, 0, 1],
        [0, -d, 0, 1], [d*0.6, 0, 0, 1], [0, 0, 0, 1],
      ];
      const diamondColors = [
        [1, 1, 0.95, 1], [1, 0.95, 0.8, 1], [1, 1, 1, 1],
        [1, 1, 0.95, 1], [1, 1, 1, 1], [1, 0.95, 0.8, 1],
        [1, 0.9, 0.7, 1], [1, 1, 1, 1], [1, 0.95, 0.8, 1],
        [1, 0.9, 0.7, 1], [1, 0.95, 0.8, 1], [1, 1, 1, 1],
      ];
      star = new Form(
        { type: "triangle", positions: diamondPositions, colors: diamondColors },
        { pos: [x, y, z], rot: [0, 0, Math.random() * 360], scale: 1 }
      );
      star.rotSpeed = (Math.random() - 0.5) * 1.5;
    } else {
      // Simple triangle star
      const starPositions = [
        [0, size, 0, 1],
        [-size * 0.6, -size * 0.4, 0, 1],
        [size * 0.6, -size * 0.4, 0, 1],
      ];
      const starColors = [
        [1, 1, 0.9, 1],
        [1, 1, 0.8, 1],
        [1, 1, 0.85, 1],
      ];
      star = new Form(
        { type: "triangle", positions: starPositions, colors: starColors },
        { pos: [x, y, z], rot: [0, 0, Math.random() * 360], scale: 1 }
      );
      star.rotSpeed = (Math.random() - 0.5) * 0.8;
    }
    star.twinklePhase = Math.random() * Math.PI * 2;
    star.twinkleSpeed = 0.015 + Math.random() * 0.025;
    starForms.push(star);
  }
  
  // Create moon (layered triangles for depth)
  const moonPositions = [
    // Main moon disc (2 triangles forming quad)
    [-0.4, -0.4, 0, 1], [-0.4, 0.4, 0, 1], [0.4, 0.4, 0, 1],
    [-0.4, -0.4, 0, 1], [0.4, 0.4, 0, 1], [0.4, -0.4, 0, 1],
    // Crescent shadow overlay
    [0.1, -0.35, 0.01, 1], [0.1, 0.35, 0.01, 1], [0.45, 0, 0.01, 1],
  ];
  const moonColors = [
    [1, 1, 0.92, 1], [0.98, 0.98, 0.88, 1], [0.95, 0.95, 0.85, 1],
    [1, 1, 0.92, 1], [0.95, 0.95, 0.85, 1], [0.92, 0.92, 0.82, 1],
    [0.3, 0.3, 0.35, 0.6], [0.3, 0.3, 0.35, 0.6], [0.2, 0.2, 0.25, 0.4],
  ];
  moonForm = new Form(
    { type: "triangle", positions: moonPositions, colors: moonColors },
    { pos: [5, 4, -10], rot: [0, 0, -15], scale: 1.2 }
  );
  
  // Create sun with rays (central disc + radiating lines)
  sunForm = [];
  // Sun disc (hexagon made of triangles)
  const sunDiscPositions = [];
  const sunDiscColors = [];
  const sunSegments = 8;
  for (let i = 0; i < sunSegments; i++) {
    const a1 = (i / sunSegments) * Math.PI * 2;
    const a2 = ((i + 1) / sunSegments) * Math.PI * 2;
    const r = 0.4;
    sunDiscPositions.push(
      [0, 0, 0, 1],
      [Math.cos(a1) * r, Math.sin(a1) * r, 0, 1],
      [Math.cos(a2) * r, Math.sin(a2) * r, 0, 1]
    );
    const bright = 0.9 + Math.random() * 0.1;
    sunDiscColors.push(
      [1, 0.95, 0.6, 1],
      [1, bright, 0.3, 1],
      [1, bright - 0.05, 0.25, 1]
    );
  }
  const sunDisc = new Form(
    { type: "triangle", positions: sunDiscPositions, colors: sunDiscColors },
    { pos: [6, 5, -12], rot: [0, 0, 0], scale: 1.5 }
  );
  sunDisc.rotSpeed = 0.3;
  sunForm.push(sunDisc);
  
  // Sun rays (lines radiating outward)
  const rayPositions = [];
  const rayColors = [];
  for (let i = 0; i < 12; i++) {
    const angle = (i / 12) * Math.PI * 2;
    const innerR = 0.5;
    const outerR = 0.8 + Math.random() * 0.3;
    rayPositions.push(
      [Math.cos(angle) * innerR, Math.sin(angle) * innerR, 0, 1],
      [Math.cos(angle) * outerR, Math.sin(angle) * outerR, 0, 1]
    );
    rayColors.push(
      [1, 0.9, 0.4, 0.9],
      [1, 0.8, 0.2, 0.3]
    );
  }
  const sunRays = new Form(
    { type: "line", positions: rayPositions, colors: rayColors },
    { pos: [6, 5, -12], rot: [0, 0, 0], scale: 1.5 }
  );
  sunRays.rotSpeed = -0.5;
  sunForm.push(sunRays);
  
  // Create 3D clouds (billowy shapes with triangles and soft edges)
  cloudForms = [];
  for (let i = 0; i < 8; i++) {
    const cx = (Math.random() - 0.5) * 20 - 5;
    const cy = 1.5 + Math.random() * 4;
    const cz = -5 - Math.random() * 10;
    const cloudScale = 0.6 + Math.random() * 1.0;
    const baseSpeed = 0.003 + Math.random() * 0.004;
    
    // Each cloud is multiple overlapping rounded shapes
    const puffs = 4 + Math.floor(Math.random() * 3);
    for (let j = 0; j < puffs; j++) {
      const ox = (Math.random() - 0.5) * 1.2;
      const oy = (Math.random() - 0.5) * 0.4;
      const oz = j * 0.05;
      const puffScale = 0.7 + Math.random() * 0.5;
      
      // Create rounded puff shape (6 triangles in a fan)
      const puffPositions = [];
      const puffColors = [];
      const segments = 6;
      const alpha = 0.4 + Math.random() * 0.3;
      const gray = 0.75 + Math.random() * 0.2;
      
      for (let s = 0; s < segments; s++) {
        const a1 = (s / segments) * Math.PI * 2;
        const a2 = ((s + 1) / segments) * Math.PI * 2;
        const r = 0.3 * puffScale;
        puffPositions.push(
          [0, 0, 0, 1],
          [Math.cos(a1) * r, Math.sin(a1) * r * 0.6, 0, 1],
          [Math.cos(a2) * r, Math.sin(a2) * r * 0.6, 0, 1]
        );
        const edgeGray = gray - 0.1;
        puffColors.push(
          [gray + 0.1, gray + 0.1, gray + 0.15, alpha],
          [edgeGray, edgeGray, edgeGray + 0.05, alpha * 0.8],
          [edgeGray, edgeGray, edgeGray + 0.05, alpha * 0.8]
        );
      }
      
      const cloud = new Form(
        { type: "triangle", positions: puffPositions, colors: puffColors },
        { pos: [cx + ox, cy + oy, cz + oz], rot: [0, 0, Math.random() * 360], scale: cloudScale }
      );
      cloud.driftSpeed = baseSpeed;
      cloud.wobblePhase = Math.random() * Math.PI * 2;
      cloud.wobbleSpeed = 0.02 + Math.random() * 0.02;
      cloud.baseY = cy + oy;
      cloudForms.push(cloud);
    }
    
    // Add wispy line trails to some clouds
    if (Math.random() < 0.4) {
      const wispPositions = [];
      const wispColors = [];
      const wispCount = 2 + Math.floor(Math.random() * 3);
      for (let w = 0; w < wispCount; w++) {
        const wx = (Math.random() - 0.5) * 0.8;
        const wy = -0.2 - Math.random() * 0.3;
        wispPositions.push(
          [wx, 0, 0, 1],
          [wx + (Math.random() - 0.5) * 0.3, wy, 0, 1]
        );
        wispColors.push(
          [0.9, 0.9, 0.95, 0.4],
          [0.85, 0.85, 0.9, 0.1]
        );
      }
      const wisp = new Form(
        { type: "line", positions: wispPositions, colors: wispColors },
        { pos: [cx, cy - 0.2, cz + 0.1], rot: [0, 0, 0], scale: cloudScale }
      );
      wisp.driftSpeed = baseSpeed;
      cloudForms.push(wisp);
    }
  }
  
  // Floating atmospheric particles throughout the view
  for (let i = 0; i < 30; i++) {
    const x = (Math.random() - 0.5) * 20;
    const y = (Math.random() - 0.5) * 10; // Full vertical range
    const z = -3 - Math.random() * 15;
    const size = 0.01 + Math.random() * 0.02;
    
    // Small floating dust/atmosphere motes
    const motePositions = [
      [-size, 0, 0, 1], [size, 0, 0, 1],
      [0, -size, 0, 1], [0, size, 0, 1],
    ];
    const alpha = 0.1 + Math.random() * 0.2;
    const moteColors = [
      [0.8, 0.85, 1, alpha], [0.8, 0.85, 1, alpha * 0.5],
      [0.8, 0.85, 1, alpha], [0.8, 0.85, 1, alpha * 0.5],
    ];
    
    const mote = new Form(
      { type: "line", positions: motePositions, colors: moteColors },
      { pos: [x, y, z], rot: [0, 0, Math.random() * 360], scale: 1 }
    );
    mote.driftSpeed = 0.001 + Math.random() * 0.002;
    mote.floatPhase = Math.random() * Math.PI * 2;
    mote.floatSpeed = 0.01 + Math.random() * 0.02;
    mote.baseY = y;
    mote.isMote = true;
    cloudForms.push(mote);
  }
  
  // Lower atmosphere haze layers (triangular bands)
  for (let i = 0; i < 4; i++) {
    const y = -3 + i * 0.8;
    const z = -8 - i * 2;
    const width = 15 + i * 3;
    const alpha = 0.08 + i * 0.02;
    
    const hazePositions = [
      [-width, y - 0.5, z, 1],
      [0, y + 0.3, z, 1],
      [width, y - 0.5, z, 1],
    ];
    const hazeColors = [
      [0.6, 0.65, 0.8, alpha * 0.5],
      [0.7, 0.75, 0.9, alpha],
      [0.6, 0.65, 0.8, alpha * 0.5],
    ];
    
    const haze = new Form(
      { type: "triangle", positions: hazePositions, colors: hazeColors },
      { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
    );
    haze.isHaze = true;
    cloudForms.push(haze);
  }
  
  // Add some low clouds/fog near bottom
  for (let i = 0; i < 5; i++) {
    const cx = (Math.random() - 0.5) * 25;
    const cy = -2 - Math.random() * 2; // Below center
    const cz = -4 - Math.random() * 8;
    const fogScale = 1.5 + Math.random() * 2;
    
    const fogPositions = [];
    const fogColors = [];
    const segments = 5;
    const alpha = 0.15 + Math.random() * 0.15;
    
    for (let s = 0; s < segments; s++) {
      const a1 = (s / segments) * Math.PI;
      const a2 = ((s + 1) / segments) * Math.PI;
      const r = 0.8;
      fogPositions.push(
        [0, 0, 0, 1],
        [Math.cos(a1) * r, Math.sin(a1) * r * 0.3, 0, 1],
        [Math.cos(a2) * r, Math.sin(a2) * r * 0.3, 0, 1]
      );
      fogColors.push(
        [0.8, 0.82, 0.88, alpha],
        [0.7, 0.72, 0.78, alpha * 0.6],
        [0.7, 0.72, 0.78, alpha * 0.6]
      );
    }
    
    const fog = new Form(
      { type: "triangle", positions: fogPositions, colors: fogColors },
      { pos: [cx, cy, cz], rot: [0, 0, 0], scale: fogScale }
    );
    fog.driftSpeed = 0.002 + Math.random() * 0.002;
    fog.isLowFog = true;
    cloudForms.push(fog);
  }
}

// Initialize 2D backdrop particles (fallback)
function initBackdrop(sw, sh) {
  particles = [];
  stars = [];
  clouds = [];
  
  // Create stars for night scenes
  for (let i = 0; i < 50; i++) {
    stars.push({
      x: Math.random() * sw,
      y: Math.random() * (sh - 40),
      size: Math.random() < 0.3 ? 2 : 1,
      twinkle: Math.random() * Math.PI * 2,
      speed: 0.02 + Math.random() * 0.03,
    });
  }
  
  // Create clouds
  for (let i = 0; i < 6; i++) {
    clouds.push({
      x: Math.random() * sw * 1.5 - sw * 0.25,
      y: 30 + Math.random() * 60,
      width: 40 + Math.random() * 60,
      height: 15 + Math.random() * 15,
      speed: 0.1 + Math.random() * 0.2,
      opacity: 0.3 + Math.random() * 0.4,
    });
  }
}

// Create precipitation particles
function spawnParticle(sw, sh, type) {
  if (type === "rain") {
    return {
      x: Math.random() * sw,
      y: -5,
      speed: 3 + Math.random() * 2,
      length: 4 + Math.random() * 4,
      opacity: 0.4 + Math.random() * 0.4,
    };
  } else if (type === "snow") {
    return {
      x: Math.random() * sw,
      y: -5,
      speed: 0.5 + Math.random() * 1,
      size: 1 + Math.random() * 2,
      wobble: Math.random() * Math.PI * 2,
      wobbleSpeed: 0.05 + Math.random() * 0.05,
      drift: (Math.random() - 0.5) * 0.5,
    };
  }
  return null;
}

// Create 3D rain/snow particles
function spawn3DParticle(type) {
  if (!Form3D) return null;
  
  const x = (Math.random() - 0.5) * 12;
  const y = 6;
  const z = -3 - Math.random() * 8;
  
  if (type === "rain") {
    const rainPositions = [
      [0, 0.15, 0, 1],
      [-0.01, -0.15, 0, 1],
      [0.01, -0.15, 0, 1],
    ];
    const rainColors = [
      [0.6, 0.7, 1, 0.7],
      [0.5, 0.6, 0.9, 0.4],
      [0.5, 0.6, 0.9, 0.4],
    ];
    const rain = new Form3D(
      { type: "triangle", positions: rainPositions, colors: rainColors },
      { pos: [x, y, z], rot: [0, 0, 0], scale: 0.5 }
    );
    rain.fallSpeed = 0.15 + Math.random() * 0.1;
    return rain;
  } else if (type === "snow") {
    const snowPositions = [
      [0, 0.05, 0, 1],
      [-0.04, -0.03, 0, 1],
      [0.04, -0.03, 0, 1],
    ];
    const snowColors = [
      [1, 1, 1, 0.9],
      [0.95, 0.95, 1, 0.8],
      [0.95, 0.95, 1, 0.8],
    ];
    const snow = new Form3D(
      { type: "triangle", positions: snowPositions, colors: snowColors },
      { pos: [x, y, z], rot: [0, 0, Math.random() * 360], scale: 0.8 }
    );
    snow.fallSpeed = 0.03 + Math.random() * 0.02;
    snow.wobblePhase = Math.random() * Math.PI * 2;
    snow.wobbleSpeed = 0.05 + Math.random() * 0.03;
    snow.drift = (Math.random() - 0.5) * 0.02;
    return snow;
  }
  return null;
}

// Draw an analog clock graphic
function drawClock({ ink, line, box }, cx, cy, radius, hours, minutes, color, bgColor) {
  // Clock face background
  ink(...bgColor, 180);
  for (let dy = -radius; dy <= radius; dy++) {
    const w = Math.floor(Math.sqrt(radius * radius - dy * dy));
    box(cx - w, cy + dy, w * 2, 1);
  }
  
  // Clock rim
  ink(...color);
  for (let a = 0; a < Math.PI * 2; a += 0.15) {
    const x = cx + Math.cos(a) * radius;
    const y = cy + Math.sin(a) * radius;
    box(Math.floor(x), Math.floor(y), 1, 1);
  }
  
  // Hour markers
  for (let h = 0; h < 12; h++) {
    const a = (h / 12) * Math.PI * 2 - Math.PI / 2;
    const x1 = cx + Math.cos(a) * (radius - 2);
    const y1 = cy + Math.sin(a) * (radius - 2);
    const x2 = cx + Math.cos(a) * (radius - 4);
    const y2 = cy + Math.sin(a) * (radius - 4);
    line(x1, y1, x2, y2);
  }
  
  // Hour hand
  const hourAngle = ((hours % 12) + minutes / 60) / 12 * Math.PI * 2 - Math.PI / 2;
  const hourLen = radius * 0.5;
  ink(...color);
  line(cx, cy, cx + Math.cos(hourAngle) * hourLen, cy + Math.sin(hourAngle) * hourLen);
  
  // Minute hand
  const minAngle = (minutes / 60) * Math.PI * 2 - Math.PI / 2;
  const minLen = radius * 0.75;
  line(cx, cy, cx + Math.cos(minAngle) * minLen, cy + Math.sin(minAngle) * minLen);
  
  // Center dot
  box(cx - 1, cy - 1, 2, 2);
}

// Draw a thermometer graphic
function drawThermometer({ ink, line, box }, x, y, height, temp, minT, maxT, hotColor, coldColor, bgColor) {
  const bulbR = 5;
  const tubeW = 4;
  const tubeH = height - bulbR * 2;
  
  // Normalize temp to 0-1 range
  const range = Math.max(maxT - minT, 1);
  const fillPct = Math.max(0, Math.min(1, (temp - minT) / range));
  const fillH = Math.floor(tubeH * fillPct);
  
  // Choose color based on temp
  const t = fillPct;
  const r = Math.round(coldColor[0] * (1 - t) + hotColor[0] * t);
  const g = Math.round(coldColor[1] * (1 - t) + hotColor[1] * t);
  const b = Math.round(coldColor[2] * (1 - t) + hotColor[2] * t);
  
  // Tube background
  ink(...bgColor, 200);
  box(x - tubeW / 2, y, tubeW, tubeH);
  
  // Bulb background  
  ink(...bgColor, 200);
  for (let dy = -bulbR; dy <= bulbR; dy++) {
    const w = Math.floor(Math.sqrt(bulbR * bulbR - dy * dy));
    box(x - w, y + tubeH + bulbR + dy, w * 2, 1);
  }
  
  // Fill
  ink(r, g, b);
  box(x - tubeW / 2 + 1, y + tubeH - fillH, tubeW - 2, fillH);
  
  // Bulb fill
  for (let dy = -bulbR + 1; dy <= bulbR - 1; dy++) {
    const w = Math.floor(Math.sqrt((bulbR - 1) * (bulbR - 1) - dy * dy));
    box(x - w, y + tubeH + bulbR + dy, w * 2, 1);
  }
  
  // Tick marks
  ink(...colors.textDim);
  for (let i = 0; i <= 4; i++) {
    const tickY = y + tubeH - (tubeH * i / 4);
    line(x + tubeW / 2 + 1, tickY, x + tubeW / 2 + 3, tickY);
  }
}

// Draw a horizontal bar graph
function drawBarGraph({ ink, box }, x, y, width, height, value, maxVal, fillColor, bgColor) {
  const fillW = Math.floor((value / maxVal) * width);
  ink(...bgColor, 150);
  box(x, y, width, height);
  ink(...fillColor);
  box(x, y, fillW, height);
}

// Build text forecast narrative
function buildTextForecast() {
  if (!weatherData || !location) return [];
  
  const lines = [];
  const daily = weatherData.daily;
  const current = weatherData.current;
  
  // Header
  lines.push("═══════════════════════════════════");
  lines.push("    LOCAL FORECAST DISCUSSION");
  lines.push("═══════════════════════════════════");
  lines.push("");
  
  // Current conditions summary
  const code = current?.weather_code || 0;
  const weather = WEATHER_CODES[code] || { text: "Unknown" };
  const temp = Math.round(current.temperature_2m * 9/5 + 32);
  const humidity = Math.round(current.relative_humidity_2m);
  const wind = Math.round(current.wind_speed_10m * 0.621371);
  const windDir = getWindDirection(current.wind_direction_10m);
  
  lines.push("CURRENT CONDITIONS:");
  lines.push(`  ${weather.text}. Temperature ${temp}°F.`);
  lines.push(`  Humidity ${humidity}%. Wind ${windDir} ${wind} mph.`);
  lines.push("");
  
  // Today's outlook
  if (daily?.time?.length > 0) {
    const todayHi = Math.round(daily.temperature_2m_max[0] * 9/5 + 32);
    const todayLo = Math.round(daily.temperature_2m_min[0] * 9/5 + 32);
    const todayCode = daily.weather_code[0];
    const todayWeather = WEATHER_CODES[todayCode] || { text: "Variable" };
    const todayPrecip = daily.precipitation_probability_max?.[0] || 0;
    
    lines.push("TODAY:");
    lines.push(`  ${todayWeather.text}. High ${todayHi}°F, Low ${todayLo}°F.`);
    if (todayPrecip > 20) {
      lines.push(`  ${todayPrecip}% chance of precipitation.`);
    }
    lines.push("");
  }
  
  // Extended narrative
  lines.push("───────────────────────────────────");
  lines.push("    EXTENDED FORECAST");
  lines.push("───────────────────────────────────");
  lines.push("");
  
  for (let i = 1; i < Math.min(7, daily?.time?.length || 0); i++) {
    const date = new Date(daily.time[i]);
    const dayName = FULL_DAYS[date.getDay()];
    const hi = Math.round(daily.temperature_2m_max[i] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[i] * 9/5 + 32);
    const code = daily.weather_code[i];
    const weather = WEATHER_CODES[code] || { text: "Variable" };
    const precip = daily.precipitation_probability_max?.[i] || 0;
    
    lines.push(`${dayName.toUpperCase()}:`);
    let desc = `  ${weather.text}. High ${hi}°F, Low ${lo}°F.`;
    lines.push(desc);
    if (precip > 20) {
      lines.push(`  Precipitation: ${precip}% chance.`);
    }
    lines.push("");
  }
  
  lines.push("───────────────────────────────────");
  lines.push("     END OF FORECAST");
  lines.push("═══════════════════════════════════");
  
  return lines;
}

async function boot({ params, screen, Form, Camera, Dolly, TRI, QUAD, painting }) {
  loading = true;
  error = null;
  frameCount = 0;
  currentView = 0;
  viewTimer = 0;
  particles = [];
  rainForms = [];
  snowForms = [];
  
  // Reset camera navigation state
  cameraMode = "cruise";
  targetStarIndex = 0;
  prevStarIndex = -1;
  phaseTimer = 0;
  cruiseAngle = 0;
  orbitAngle = 0;
  camX = camTargetX = 0;
  camY = camTargetY = CRUISE_HEIGHT;
  camZ = camTargetZ = 5;
  camVelX = camVelY = camVelZ = 0;
  lookTargetX = lookCurrentX = 0;
  lookTargetY = lookCurrentY = 2;
  lookTargetZ = lookCurrentZ = -10;
  targetRoll = currentRoll = 0;
  
  // Initialize 3D backdrop (if Form and Camera are available)
  if (Form && Camera && Dolly) {
    init3DBackdrop({ Form, Camera, Dolly, TRI, QUAD, painting });
  } else {
    use3D = false;
  }
  
  // Initialize 2D backdrop with screen size
  if (screen) initBackdrop(screen.width, screen.height);
  
  let query = params.join(" ").trim();
  
  try {
    if (!query) query = "New York";
    
    const coordMatch = query.match(/^(-?\d+\.?\d*)\s*,\s*(-?\d+\.?\d*)$/);
    
    if (coordMatch) {
      location = {
        name: "Custom Location",
        region: "",
        country: "",
        latitude: parseFloat(coordMatch[1]),
        longitude: parseFloat(coordMatch[2]),
      };
    } else {
      location = await geocode(query);
      if (!location) {
        error = "Location not found: " + query;
        loading = false;
        return;
      }
    }
    
    weatherData = await getWeather(location.latitude, location.longitude);
    // Store timezone from API response
    locationTimezone = weatherData.timezone || "UTC";
    buildCrawlText();
    textForecastLines = buildTextForecast();
    textForecastScroll = 0;
    
  } catch (err) {
    console.error("Weather error:", err);
    error = "Weather unavailable: " + err.message;
  }
  
  loading = false;
}

function sim({ screen, sound }) {
  frameCount++;
  
  if (!screen) return;
  const { width: sw, height: sh } = screen;
  
  // Poll speaker for waveform data
  sound?.speaker?.poll();
  
  // 🎶 Update ambient music
  if (sound) updateMusic(sound);
  
  // Smooth scroll for chord display
  if (chordScrollX !== chordScrollTarget) {
    chordScrollX += (chordScrollTarget - chordScrollX) * 0.08;
    if (Math.abs(chordScrollX - chordScrollTarget) < 0.01) {
      chordScrollX = chordScrollTarget;
    }
  }
  
  // Fade chord display alpha over time
  if (chordFadeAlpha > 100) {
    chordFadeAlpha -= 0.5;
  }
  
  // Init backdrop if needed
  if (stars.length === 0) initBackdrop(sw, sh);
  
  viewTimer++;
  if (viewTimer >= VIEW_DURATION) {
    viewTimer = 0;
    currentView = (currentView + 1) % NUM_VIEWS;
  }
  
  // Very slow crawl: 0.25 pixels per frame for leisurely reading
  if (crawlText) {
    crawlAccum += 0.25;
    if (crawlAccum >= 1) {
      crawlX -= 1;
      crawlAccum = 0;
    }
    const crawlWidth = crawlText.length * 6;
    if (crawlX < -crawlWidth) {
      crawlX = sw;
    }
  }
  
  // Update color theme based on time and weather
  updateColorTheme();
  
  // Animate stars (twinkle)
  for (const star of stars) {
    star.twinkle += star.speed;
  }
  
  // Animate clouds
  for (const cloud of clouds) {
    cloud.x += cloud.speed;
    if (cloud.x > sw + cloud.width) {
      cloud.x = -cloud.width;
    }
  }
  
  // Spawn and update precipitation
  const maxParticles = visualState === "snow" ? 80 : 120;
  if ((visualState === "rain" || visualState === "snow" || visualState === "storm") && particles.length < maxParticles) {
    const spawn = visualState === "snow" ? 1 : 3;
    for (let i = 0; i < spawn; i++) {
      const p = spawnParticle(sw, sh, visualState === "storm" ? "rain" : visualState);
      if (p) particles.push(p);
    }
  }
  
  // Update particles
  for (let i = particles.length - 1; i >= 0; i--) {
    const p = particles[i];
    p.y += p.speed;
    if (p.wobble !== undefined) {
      // Snow wobble
      p.wobble += p.wobbleSpeed;
      p.x += Math.sin(p.wobble) * 0.5 + p.drift;
    }
    // Remove if off screen
    if (p.y > sh) {
      particles.splice(i, 1);
    }
  }
  
  // Lightning for storms
  if (visualState === "storm") {
    lightningTimer++;
    if (lightningTimer > 120 && Math.random() < 0.02) {
      lightningFlash = true;
      lightningTimer = 0;
    } else {
      lightningFlash = false;
    }
  } else {
    lightningFlash = false;
  }
  
  // Update 3D elements if active
  if (use3D) {
    // Twinkle 3D stars and rotate them
    for (const star of starForms) {
      star.twinklePhase += star.twinkleSpeed;
      // Individual rotation speeds
      if (star.rotation && star.rotSpeed !== undefined) {
        star.rotation[2] += star.rotSpeed;
      }
    }
    
    // Drift 3D clouds across the sky with gentle wobble
    for (const cloudForm of cloudForms) {
      // Skip static elements like haze layers
      if (cloudForm.isHaze) continue;
      
      if (cloudForm.driftSpeed !== undefined && cloudForm.position) {
        cloudForm.position[0] += cloudForm.driftSpeed;
        
        // Gentle vertical wobble for organic feel
        if (cloudForm.wobblePhase !== undefined && cloudForm.baseY !== undefined) {
          cloudForm.wobblePhase += cloudForm.wobbleSpeed || 0.02;
          cloudForm.position[1] = cloudForm.baseY + Math.sin(cloudForm.wobblePhase) * 0.05;
        }
        
        // Floating motes have gentle float animation
        if (cloudForm.isMote && cloudForm.floatPhase !== undefined) {
          cloudForm.floatPhase += cloudForm.floatSpeed || 0.01;
          cloudForm.position[1] = cloudForm.baseY + Math.sin(cloudForm.floatPhase) * 0.15;
          // Motes also slowly rotate
          if (cloudForm.rotation) {
            cloudForm.rotation[2] += 0.3;
          }
        }
        
        // Reset cloud/fog/mote position when it drifts too far right
        const resetX = cloudForm.isLowFog ? 18 : (cloudForm.isMote ? 12 : 14);
        if (cloudForm.position[0] > resetX) {
          cloudForm.position[0] = -resetX;
        }
      }
    }
    
    // Rotate sun disc and rays (sunForm is now an array)
    if (sunForm && Array.isArray(sunForm)) {
      for (const part of sunForm) {
        if (part.rotation && part.rotSpeed !== undefined) {
          part.rotation[2] += part.rotSpeed;
        }
      }
    }
    
    // Smooth galaxy fly-through with orbital star visits (only at night)
    if (cam && isNight && starForms.length > 0) {
      phaseTimer++;
      
      // === CRUISE MODE: Drift through space, looking around ===
      if (cameraMode === "cruise") {
        cruiseAngle += CRUISE_SPEED;
        
        // Smooth elliptical cruise path
        camTargetX = Math.sin(cruiseAngle) * CRUISE_RADIUS;
        camTargetZ = Math.cos(cruiseAngle) * CRUISE_RADIUS * 0.5 - 6;
        camTargetY = CRUISE_HEIGHT + Math.sin(cruiseAngle * 0.5) * 1;
        
        // Look generally forward with slight wander
        const forwardX = Math.sin(cruiseAngle + 0.3) * 5;
        const forwardZ = Math.cos(cruiseAngle + 0.3) * 5 - 15;
        lookTargetX = forwardX;
        lookTargetY = 2 + Math.sin(cruiseAngle * 0.7) * 1;
        lookTargetZ = forwardZ;
        
        targetRoll = Math.sin(cruiseAngle * 1.5) * 4;
        
        // Time to visit a star?
        if (phaseTimer >= CRUISE_DURATION) {
          phaseTimer = 0;
          cameraMode = "approach";
          prevStarIndex = targetStarIndex;
          
          // Find a nice star to visit (prefer ones ahead and not too far)
          let bestIndex = 0;
          let bestScore = -Infinity;
          for (let i = 0; i < starForms.length; i++) {
            if (i === prevStarIndex) continue;
            const star = starForms[i];
            const dx = star.position[0] - camX;
            const dy = star.position[1] - camY;
            const dz = star.position[2] - camZ;
            const dist = Math.sqrt(dx*dx + dy*dy + dz*dz);
            
            // Score based on being ahead and reachable
            const score = -dz * 0.3 + (8 - Math.abs(dist - 5)) + Math.random() * 2;
            if (score > bestScore && dist > 1.5 && dist < 12) {
              bestScore = score;
              bestIndex = i;
            }
          }
          targetStarIndex = bestIndex;
          
          // Set orbit center to target star
          const star = starForms[targetStarIndex];
          if (star) {
            orbitCenterX = star.position[0];
            orbitCenterY = star.position[1];
            orbitCenterZ = star.position[2];
          }
        }
      }
      
      // === APPROACH MODE: Fly toward the target star ===
      else if (cameraMode === "approach") {
        const t = Math.min(1, phaseTimer / APPROACH_DURATION);
        const ease = t * t * (3 - 2 * t);  // Smoothstep
        
        // Target position: orbit distance behind the star
        const approachX = orbitCenterX;
        const approachY = orbitCenterY;
        const approachZ = orbitCenterZ + orbitRadius;
        
        // Interpolate toward orbit start position
        camTargetX = camX + (approachX - camX) * ease * 0.05;
        camTargetY = camY + (approachY - camY) * ease * 0.05;
        camTargetZ = camZ + (approachZ - camZ) * ease * 0.05;
        
        // Look at the star we're approaching
        lookTargetX = orbitCenterX;
        lookTargetY = orbitCenterY;
        lookTargetZ = orbitCenterZ;
        
        targetRoll = Math.sin(t * Math.PI) * 5;
        
        // Transition to orbit when close enough or time's up
        const distToOrbit = Math.sqrt(
          Math.pow(camX - approachX, 2) + 
          Math.pow(camY - approachY, 2) + 
          Math.pow(camZ - approachZ, 2)
        );
        if (phaseTimer >= APPROACH_DURATION || distToOrbit < 0.5) {
          phaseTimer = 0;
          cameraMode = "orbit";
          orbitAngle = Math.atan2(camX - orbitCenterX, camZ - orbitCenterZ);
        }
      }
      
      // === ORBIT MODE: Circle around the star while looking at it ===
      else if (cameraMode === "orbit") {
        orbitAngle += 0.012;  // Slow orbit speed
        
        // Circular orbit around the star
        camTargetX = orbitCenterX + Math.sin(orbitAngle) * orbitRadius;
        camTargetZ = orbitCenterZ + Math.cos(orbitAngle) * orbitRadius;
        camTargetY = orbitCenterY + Math.sin(orbitAngle * 2) * 0.3;  // Gentle vertical wave
        
        // Always look at the star
        lookTargetX = orbitCenterX;
        lookTargetY = orbitCenterY;
        lookTargetZ = orbitCenterZ;
        
        // Bank into the turn
        targetRoll = Math.sin(orbitAngle) * 8;
        
        // Time to depart?
        if (phaseTimer >= ORBIT_DURATION) {
          phaseTimer = 0;
          cameraMode = "depart";
        }
      }
      
      // === DEPART MODE: Fly away from the star back to cruise ===
      else if (cameraMode === "depart") {
        const t = Math.min(1, phaseTimer / DEPART_DURATION);
        const ease = t * t * (3 - 2 * t);
        
        // Ease back toward cruise path
        const cruiseX = Math.sin(cruiseAngle) * CRUISE_RADIUS;
        const cruiseZ = Math.cos(cruiseAngle) * CRUISE_RADIUS * 0.5 - 6;
        const cruiseY = CRUISE_HEIGHT;
        
        camTargetX = camX + (cruiseX - camX) * ease * 0.03;
        camTargetY = camY + (cruiseY - camY) * ease * 0.03;
        camTargetZ = camZ + (cruiseZ - camZ) * ease * 0.03;
        
        // Transition look from star to forward
        const forwardZ = camZ - 10;
        lookTargetX = orbitCenterX * (1 - ease) + camX * ease;
        lookTargetY = orbitCenterY * (1 - ease) + cruiseY * ease;
        lookTargetZ = orbitCenterZ * (1 - ease) + forwardZ * ease;
        
        targetRoll = (1 - ease) * currentRoll;
        
        if (phaseTimer >= DEPART_DURATION) {
          phaseTimer = 0;
          cameraMode = "cruise";
        }
      }
      
      // === Apply smooth camera movement ===
      // Smooth position interpolation
      const posSmoothness = 0.04;
      camX += (camTargetX - camX) * posSmoothness;
      camY += (camTargetY - camY) * posSmoothness;
      camZ += (camTargetZ - camZ) * posSmoothness;
      
      cam.x = camX;
      cam.y = camY;
      cam.z = camZ;
      
      // Smooth look interpolation
      lookCurrentX += (lookTargetX - lookCurrentX) * LOOK_SMOOTHING;
      lookCurrentY += (lookTargetY - lookCurrentY) * LOOK_SMOOTHING;
      lookCurrentZ += (lookTargetZ - lookCurrentZ) * LOOK_SMOOTHING;
      
      const look = lookAtAngles(camX, camY, camZ, lookCurrentX, lookCurrentY, lookCurrentZ);
      cam.rotX = look.rotX;
      cam.rotY = look.rotY;
      
      // Smooth roll
      currentRoll += (targetRoll - currentRoll) * 0.03;
      cam.rotZ = currentRoll;
      
    } else if (cam) {
      // Daytime: gentle pan/bob (original behavior)
      camRotY += 0.08;
      camBobPhase += 0.015;
      cam.rotY = Math.sin(camRotY * 0.02) * 8;
      cam.rotX = Math.sin(camBobPhase) * 2 - 5;
      cam.x = 0;
      cam.y = 0;
      cam.z = 0;
    }
    
    // Update dolly if active
    if (dolly) dolly.sim();
  }
  
  // Scroll text forecast when on that view
  if (currentView === 4 && textForecastLines.length > 0) {
    textForecastScroll += 0.3;
    const lineH = 12;
    const maxScroll = textForecastLines.length * lineH;
    if (textForecastScroll > maxScroll) {
      textForecastScroll = -sh + 60;
    }
  }
}

function updateColorTheme() {
  if (!weatherData?.daily?.sunrise || !weatherData?.daily?.sunset) {
    colors = THEMES.dayClear;
    visualState = "clear";
    isNight = false;
    return;
  }
  
  // Get current time in location's timezone
  const { totalMins: nowMins } = getLocationTime();
  // Sunrise/sunset times from API are already in location's local time
  const sunrise = new Date(weatherData.daily.sunrise[0]);
  const sunset = new Date(weatherData.daily.sunset[0]);
  const sunriseMins = sunrise.getHours() * 60 + sunrise.getMinutes();
  const sunsetMins = sunset.getHours() * 60 + sunset.getMinutes();
  
  const code = weatherData?.current?.weather_code || 0;
  const weather = WEATHER_CODES[code] || { type: "clear" };
  const weatherType = weather.type || "clear";
  
  // Check for sunrise/sunset windows (30 mins each side)
  const nearSunrise = Math.abs(nowMins - sunriseMins) < 30;
  const nearSunset = Math.abs(nowMins - sunsetMins) < 30;
  isNight = nowMins < sunriseMins || nowMins > sunsetMins;
  
  // Update visual state for backdrops
  visualState = weatherType;
  
  // Select theme based on time and weather
  if (nearSunrise && weatherType === "clear") {
    colors = THEMES.sunrise;
  } else if (nearSunset && weatherType === "clear") {
    colors = THEMES.sunset;
  } else if (isNight) {
    colors = weatherType === "clear" ? THEMES.nightClear : THEMES.night;
  } else {
    // Daytime - pick based on weather
    switch (weatherType) {
      case "rain": colors = THEMES.dayRain; break;
      case "snow": colors = THEMES.daySnow; break;
      case "storm": colors = THEMES.dayStorm; break;
      case "cloudy": 
      case "fog": colors = THEMES.dayCloudy; break;
      default: colors = THEMES.dayClear;
    }
  }
}

// Paint 3D backdrop forms (must be defined before paint)
function paint3DBackdrop({ ink, form, line, screen }) {
  if (!cam) return;
  const sw = screen?.width || 320;
  const sh = screen?.height || 200;
  
  // Night: render 3D stars and moon
  if (isNight) {
    // Render all stars with the target star highlighted
    for (let i = 0; i < starForms.length; i++) {
      const star = starForms[i];
      const brightness = 0.5 + 0.5 * Math.sin(star.twinklePhase);
      let alpha = Math.floor(150 + brightness * 105);
      
      // Make target star glow extra bright with gentle pulse
      if (i === targetStarIndex) {
        alpha = 255;
        const pulse = 0.8 + 0.2 * Math.sin(frameCount * 0.05);
        ink(255, 255, 200, Math.floor(alpha * pulse)).form(star, cam);
      } else {
        ink(255, 255, 220, alpha).form(star, cam);
      }
    }
    if (moonForm) ink(255, 255, 230).form(moonForm, cam);
  }
  
  // Daytime clear: render 3D sun (now an array of disc + rays)
  if (!isNight && visualState === "clear" && sunForm && Array.isArray(sunForm)) {
    for (const part of sunForm) {
      ink(255, 220, 100).form(part, cam);
    }
  }
  
  // Clouds
  if (visualState === "cloudy" || visualState === "fog" || visualState === "rain" || visualState === "storm") {
    const cloudColor = visualState === "storm" ? [80, 70, 90, 200] : [220, 230, 240, 180];
    for (const cloudForm of cloudForms) ink(...cloudColor).form(cloudForm, cam);
  }
  
  // 3D Rain/Snow particles
  if (visualState === "rain" || visualState === "storm") {
    for (const rain of rainForms) ink(150, 180, 255, 180).form(rain, cam);
  }
  if (visualState === "snow") {
    for (const snow of snowForms) ink(255, 255, 255, 220).form(snow, cam);
  }
}

function paint({ wipe, ink, screen, line, box, text, form, sound, help, api }) {
  const { width: sw, height: sh } = screen;
  
  // MatrixChunky8 everywhere for consistent micro aesthetic (8px tall)
  const font = "MatrixChunky8";
  const smallFont = "MatrixChunky8";
  
  // Lightning flash override
  if (lightningFlash) {
    wipe(255, 255, 255);
    return;
  }
  
  // Responsive sizing - simplified for micro design
  const isTiny = sw < 128 || sh < 128;      // Gameboy-ish
  const isSmall = sw < 180 || sh < 180;
  const isNarrow = sw < 200;
  const isVertical = sh > sw;
  
  // Calculate crawl bar dimensions early (needed by multiple sections)
  const crawlH = isTiny ? 10 : (isSmall ? 14 : 20);
  const crawlY = sh - crawlH;
  
  // Dynamic gradient background
  let chain = wipe(...colors.bgTop);
  for (let y = 0; y < sh; y++) {
    const t = y / sh;
    const r = Math.round(colors.bgTop[0] * (1-t) + colors.bgBottom[0] * t);
    const g = Math.round(colors.bgTop[1] * (1-t) + colors.bgBottom[1] * t);
    const b = Math.round(colors.bgTop[2] * (1-t) + colors.bgBottom[2] * t);
    ink(r, g, b).line(0, y, sw, y);
  }
  
  // Draw 3D backdrop if available, otherwise fall back to 2D
  if (use3D && cam) {
    paint3DBackdrop({ ink, form, line, screen });
  } else {
    // Draw 2D animated backdrop elements
    paintBackdrop({ ink, box, line, screen });
  }
  
  if (loading) {
    ink(...colors.text).write("LOADING...", { center: "xy" }, 
      undefined, undefined, false, font);
    return;
  }
  
  if (error) {
    ink(255, 100, 100).write("WEATHER", { center: "x", y: sh/2 - 20 },
      undefined, undefined, false, font);
    ink(255, 100, 100).write("UNAVAILABLE", { center: "x", y: sh/2 - 5 },
      undefined, undefined, false, font);
    ink(...colors.textDim).write(error.slice(0, 30), { center: "x", y: sh/2 + 15 },
      undefined, undefined, false, font);
    return;
  }
  
  if (!weatherData || !location) return;
  
  // Header bar at top (no more chord bar here - it's at the bottom now)
  // Header bar - offset down to avoid prompt HUD corner label
  const hudOffset = isTiny ? 8 : 12;  // Space for prompt HUD in top-left
  const headerY = hudOffset + (isTiny ? 0 : 2);
  const headerH = isTiny ? 10 : (isSmall ? 14 : 24);
  ink(...colors.headerBg).box(0, headerY, sw, headerH);
  ink(...colors.divider).line(0, headerY + headerH, sw, headerY + headerH);
  
  // Location - truncate for narrow screens
  let locStr = location.region 
    ? location.name + ", " + location.region 
    : location.name;
  if (isTiny && locStr.length > 8) locStr = location.name.slice(0, 8);
  else if (isSmall && locStr.length > 12) locStr = location.name.slice(0, 12);
  ink(...colors.text).write(locStr.toUpperCase(), { x: 2, y: headerY + (isTiny ? 1 : 3) },
    undefined, undefined, false, font);
  
  // Time - position from right edge (in location's timezone)
  const timeStr = formatLocationTime();
  const timeW = isTiny ? timeStr.length * 4 : (timeStr.length * 6);
  ink(...colors.accent).write(timeStr, { x: sw - timeW - 2, y: headerY + (isTiny ? 1 : (isSmall ? 2 : 6)) },
    undefined, undefined, false, smallFont);
  
  const contentY = headerY + headerH + 1;
  
  // Calculate available content height (between header and crawl bar)
  const dotsH = isTiny ? 8 : 14;  // Space for progress bar and view dots
  const maxContentY = crawlY - dotsH;  // Content must stop before this Y
  const contentH = maxContentY - contentY;
  
  // Pass responsive layout vars to view painters
  const layout = { isTiny, isSmall, isVertical, font, smallFont, contentH, maxContentY };
  
  switch (currentView) {
    case 0: paintCurrentConditions({ ink, box, line, screen, contentY, text, layout }); break;
    case 1: paintForecast({ ink, box, line, screen, contentY, text, layout }); break;
    case 2: paintDetails({ ink, box, line, screen, contentY, text, layout }); break;
    case 3: paintExtendedForecast({ ink, box, line, screen, contentY, text, layout }); break;
    case 4: paintTextForecast({ ink, box, line, screen, contentY, text, layout }); break;
  }
  
  // 🎶 FULLSCREEN WAVEFORM VISUALIZATION (overlay on content)
  if (sound && musicEnabled) {
    const rawWaveforms = sound.speaker?.waveforms?.left;
    const amplitude = sound.speaker?.amplitudes?.left || 0;
    
    // Get mood-based colors for waveform
    const mood = getMusicMood();
    let waveColor;
    if (mood === 'storm') waveColor = [255, 100, 255];
    else if (mood === 'rain') waveColor = [100, 180, 255];
    else if (mood === 'snow') waveColor = [220, 240, 255];
    else if (mood === 'night') waveColor = [180, 140, 255];
    else if (mood === 'cloudy') waveColor = [200, 210, 230];
    else waveColor = [255, 230, 120]; // Clear/sunny
    
    // Use built-in sound.paint.waveform if available
    if (sound.paint?.waveform && rawWaveforms && rawWaveforms.length > 0) {
      const waveH = sh * 0.4;
      const waveY = (sh - waveH) / 2;
      try {
        sound.paint.waveform(
          api,
          amplitude || 0.5,
          help.resampleArray(rawWaveforms, Math.min(sw, 128)),
          0,
          waveY,
          sw,
          waveH,
          [...waveColor, 100],
          { direction: "left-to-right" }
        );
      } catch(e) {
        // Fallback to manual drawing
      }
    } else if (rawWaveforms && rawWaveforms.length > 0 && help) {
      // Manual fallback waveform drawing
      const targetPoints = Math.min(sw, 128);
      const resampled = help.resampleArray(rawWaveforms, targetPoints);
      const xStep = sw / (resampled.length - 1);
      const yMid = sh / 2;
      const yMax = sh * 0.35;
      
      const points = resampled.map((v, i) => {
        const val = typeof v === 'number' && Number.isFinite(v) ? v : 0;
        return [i * xStep, yMid + val * yMax];
      });
      
      if (points.length > 1) {
        const topPoints = points.map(([x, y]) => [x, y]);
        const bottomPoints = points.map(([x, y]) => [x, sh - (y - yMid) + yMid]).reverse();
        ink(...waveColor, 80).poly([...topPoints, ...bottomPoints]);
        ink(...waveColor, 150).poly(points);
      }
    }
    
    // 🎯 Draw crosshairs through recently played notes
    const now = Date.now();
    const recentNotes = lastPlayedNotes.filter(n => now - n.time < 3000);
    for (const noteData of recentNotes) {
      const age = now - noteData.time;
      const fadeAlpha = Math.max(0, 255 - (age / 3000) * 255);
      
      // Map note to Y position (higher notes = higher on screen)
      const noteNames = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
      const noteIdx = noteNames.indexOf(noteData.note.replace('b', '#').replace('Db', 'C#').replace('Eb', 'D#').replace('Gb', 'F#').replace('Ab', 'G#').replace('Bb', 'A#'));
      const octaveOffset = (noteData.octave - 3) * 12;
      const totalSemitones = noteIdx + octaveOffset;
      // Map semitones 0-48 to screen height (low notes at bottom, high at top)
      const noteY = Math.floor(sh - (totalSemitones / 48) * sh * 0.8 - sh * 0.1);
      
      // Random X position for variety, but consistent for same note instance
      const noteX = Math.floor((noteData.time % 1000) / 1000 * sw);
      
      // Draw crosshair
      const crossSize = noteData.melody ? 6 : 10;
      const crossColor = noteData.melody ? [255, 255, 255, fadeAlpha] : [...waveColor, fadeAlpha];
      
      // Horizontal line through note
      ink(...crossColor).line(0, noteY, sw, noteY);
      // Vertical line at note position
      ink(...crossColor).line(noteX, 0, noteX, sh);
      // Center dot
      ink(...crossColor).box(noteX - 1, noteY - 1, 3, 3);
    }
  }

  // 🎶 Scrolling chord progression display - above crawl bar with piano icon
  if (sound && musicEnabled && chordHistory.length > 0) {
    const chordH = isTiny ? 8 : 10;
    const chordDisplayY = crawlY - chordH - (isTiny ? 6 : 12);
    
    // Get mood-based color for active chord
    const mood = getMusicMood();
    let activeColor;
    if (mood === 'storm') activeColor = [255, 100, 255];
    else if (mood === 'rain') activeColor = [100, 200, 255];
    else if (mood === 'snow') activeColor = [200, 230, 255];
    else if (mood === 'night') activeColor = [180, 140, 255];
    else if (mood === 'cloudy') activeColor = [200, 220, 255];
    else activeColor = [255, 240, 150]; // Clear/sunny
    
    // Calculate chord widths
    const charW = isTiny ? 4 : (isSmall ? 5 : 6);
    const chordGap = isTiny ? 8 : 12;
    
    // Get upcoming chords from progression
    const progression = CHORD_PROGRESSIONS[getMusicMood()] || CHORD_PROGRESSIONS.clear;
    const upcomingCount = isTiny ? 2 : 4;
    
    // Build chord items array
    const chordItems = [];
    
    // Past chords from history (show last few)
    const showHistory = Math.min(chordHistory.length, isTiny ? 2 : 4);
    for (let i = chordHistory.length - showHistory; i < chordHistory.length; i++) {
      const h = chordHistory[i];
      const name = h.name || h.chord.join('');
      const isCurrent = (i === chordHistory.length - 1);
      chordItems.push({ name, isCurrent, isPast: !isCurrent, isFuture: false });
    }
    
    // Future chords from progression
    for (let i = 1; i <= upcomingCount; i++) {
      const futureIdx = (chordIndex + i) % progression.length;
      const chord = progression[futureIdx];
      const name = getChordName(chord);
      chordItems.push({ name, isCurrent: false, isPast: false, isFuture: true });
    }
    
    // Draw piano icon on the left
    const pianoX = isTiny ? 2 : 4;
    const pianoW = isTiny ? 8 : 12;
    const pianoH = isTiny ? 5 : 7;
    const keyW = isTiny ? 2 : 3;
    // White keys background
    ink(...activeColor, 80).box(pianoX, chordDisplayY + 1, pianoW, pianoH);
    // Black keys
    for (let k = 0; k < 3; k++) {
      const kx = pianoX + 1 + k * keyW;
      if (k !== 1) { // Skip middle for piano look
        ink(40, 40, 50, 150).box(kx, chordDisplayY + 1, keyW - 1, Math.floor(pianoH * 0.6));
      }
    }
    
    // Center the current chord
    const currentChordIdx = showHistory - 1;
    let preCurrentW = 0;
    for (let i = 0; i < currentChordIdx; i++) {
      preCurrentW += chordItems[i].name.length * charW + chordGap;
    }
    const currentW = chordItems[currentChordIdx]?.name.length * charW || 0;
    
    // Smooth scroll offset based on chordScrollX
    const scrollOffset = (chordScrollX % 1) * (chordGap + currentW);
    const centerX = Math.floor(sw / 2) - Math.floor(currentW / 2);
    let drawX = centerX - preCurrentW - scrollOffset;
    
    // Draw each chord
    for (let i = 0; i < chordItems.length; i++) {
      const item = chordItems[i];
      const itemW = item.name.length * charW;
      
      // Only draw if visible (and not overlapping piano icon area)
      if (drawX + itemW > pianoX + pianoW + 4 && drawX < sw) {
        let alpha, color;
        if (item.isCurrent) {
          alpha = Math.floor(chordFadeAlpha);
          color = [...activeColor, alpha];
          // Highlight box around current chord
          ink(...activeColor, 40).box(drawX - 2, chordDisplayY - 1, itemW + 4, chordH + 2);
        } else if (item.isPast) {
          const fadeIdx = chordItems.filter((x, j) => j < i && x.isPast).length;
          alpha = Math.max(30, 80 - fadeIdx * 20);
          color = [200, 200, 200, alpha];
        } else {
          const futureIdx = chordItems.filter((x, j) => j > i && x.isFuture).length;
          alpha = Math.max(40, 100 - futureIdx * 15);
          color = [180, 180, 220, alpha];
        }
        
        ink(...color).write(item.name, { x: Math.floor(drawX), y: chordDisplayY }, 
          undefined, undefined, false, smallFont);
      }
      
      drawX += itemW + chordGap;
    }
    
    // Progress bar under chord display (time until next chord)
    const progW = Math.floor((1 - chordTimer / 240) * (isTiny ? 20 : 40));
    const progX = Math.floor(sw / 2) - Math.floor(progW / 2);
    ink(...activeColor, 60).box(progX, chordDisplayY + chordH + 1, progW, 1);
  }

  // Crawl bar at bottom - compact for tiny screens
  ink(...colors.crawlBg).box(0, crawlY, sw, crawlH);
  ink(...colors.divider).line(0, crawlY, sw, crawlY);
  ink(...colors.crawlText).write(crawlText, { x: crawlX, y: crawlY + (isTiny ? 1 : (isSmall ? 2 : 4)) },
    undefined, undefined, false, smallFont);
  
  // Progress bar showing time until next view
  const progressY = crawlY - 2;
  const progressW = Math.floor((viewTimer / VIEW_DURATION) * sw);
  ink(...colors.divider, 80).box(0, progressY, sw, 1);
  ink(...colors.accent, 200).box(0, progressY, progressW, 1);
  
  // View indicator dots with labels
  const dotY = crawlY - 12;
  const dotSize = isTiny ? 2 : (isSmall ? 3 : 4);
  const dotSpacing = isTiny ? 6 : (isSmall ? 8 : 12);
  for (let i = 0; i < NUM_VIEWS; i++) {
    const dotX = sw/2 - (NUM_VIEWS * (dotSpacing/2)) + i * dotSpacing;
    if (i === currentView) {
      ink(...colors.accent).box(dotX, dotY, dotSize, dotSize);
    } else {
      ink(...colors.textDim, 150).box(dotX, dotY, dotSize, dotSize);
    }
  }
}

// Paint animated backdrop elements behind text (2D fallback)
function paintBackdrop({ ink, box, line, screen }) {
  const { width: sw, height: sh } = screen;
  
  // Night: stars and moon
  if (isNight) {
    // Twinkling stars
    for (const star of stars) {
      const brightness = 0.5 + 0.5 * Math.sin(star.twinkle);
      const alpha = Math.floor(100 + brightness * 155);
      ink(255, 255, 255, alpha).box(star.x, star.y, star.size, star.size);
    }
    
    // Moon (top right area)
    const moonX = sw - 50;
    const moonY = 60;
    ink(255, 255, 230, 200).box(moonX - 8, moonY - 8, 16, 16);
    ink(255, 255, 220, 220).box(moonX - 6, moonY - 6, 12, 12);
    ink(255, 255, 200, 240).box(moonX - 4, moonY - 4, 8, 8);
  }
  
  // Daytime clear: sun rays
  if (!isNight && visualState === "clear") {
    const sunX = sw - 40;
    const sunY = 55;
    const pulse = 0.8 + 0.2 * Math.sin(frameCount * 0.02);
    
    // Sun glow
    ink(255, 240, 150, Math.floor(40 * pulse)).box(sunX - 20, sunY - 20, 40, 40);
    ink(255, 220, 100, Math.floor(80 * pulse)).box(sunX - 12, sunY - 12, 24, 24);
    ink(255, 200, 50, Math.floor(150 * pulse)).box(sunX - 8, sunY - 8, 16, 16);
    ink(255, 220, 100).box(sunX - 5, sunY - 5, 10, 10);
    
    // Rays
    const rayLen = 6 + 2 * Math.sin(frameCount * 0.05);
    for (let angle = 0; angle < Math.PI * 2; angle += Math.PI / 4) {
      const rx = sunX + Math.cos(angle + frameCount * 0.01) * (12 + rayLen);
      const ry = sunY + Math.sin(angle + frameCount * 0.01) * (12 + rayLen);
      ink(255, 230, 100, 180).line(sunX, sunY, rx, ry);
    }
  }
  
  // Clouds (for cloudy/fog or partly cloudy scenes)
  if (visualState === "cloudy" || visualState === "fog" || visualState === "rain" || visualState === "storm") {
    for (const cloud of clouds) {
      const alpha = Math.floor(cloud.opacity * 255);
      const c = visualState === "storm" ? [60, 50, 70, alpha] : [200, 210, 220, alpha];
      // Simple cloud shape: overlapping ellipses simulated with boxes
      ink(...c).box(cloud.x, cloud.y, cloud.width, cloud.height);
      ink(...c).box(cloud.x + cloud.width * 0.2, cloud.y - cloud.height * 0.3, cloud.width * 0.6, cloud.height * 0.8);
      ink(...c).box(cloud.x + cloud.width * 0.5, cloud.y - cloud.height * 0.1, cloud.width * 0.4, cloud.height * 0.7);
    }
  }
  
  // Rain particles
  if (visualState === "rain" || visualState === "storm") {
    for (const p of particles) {
      const alpha = Math.floor(p.opacity * 255);
      ink(150, 180, 255, alpha).line(p.x, p.y, p.x - 1, p.y + p.length);
    }
  }
  
  // Snow particles
  if (visualState === "snow") {
    for (const p of particles) {
      ink(255, 255, 255, 200).box(Math.floor(p.x), Math.floor(p.y), Math.ceil(p.size), Math.ceil(p.size));
    }
  }
  
  // Fog overlay
  if (visualState === "fog") {
    // Horizontal fog bands that drift
    for (let i = 0; i < 4; i++) {
      const fogY = 50 + i * 40 + Math.sin(frameCount * 0.01 + i) * 5;
      const fogAlpha = 30 + Math.sin(frameCount * 0.02 + i * 0.5) * 15;
      ink(200, 210, 220, Math.floor(fogAlpha)).box(0, fogY, sw, 20);
    }
  }
}

function paintCurrentConditions({ ink, box, line, screen, contentY, text, layout }) {
  const { width: sw, height: sh } = screen;
  const { isTiny, isSmall, isVertical, font, smallFont, contentH } = layout || {};
  const current = weatherData.current;
  const code = current.weather_code;
  const weather = WEATHER_CODES[code] || { text: "Unknown", short: "???", color: [200,200,200], icon: "?" };
  
  // Responsive layout
  const isNarrow = sw < 200;
  const margin = isTiny ? 1 : (isSmall ? 2 : 6);
  const panelH = contentH - 4;  // Use calculated content height
  
  // Main panel background
  ink(...colors.panelBg).box(margin, contentY + 2, sw - margin * 2, panelH);
  ink(...colors.divider).line(margin, contentY + 2, sw - margin, contentY + 2);
  ink(...colors.divider).line(margin, contentY + 2 + panelH, sw - margin, contentY + 2 + panelH);
  
  // Title bar with background
  const titleH = isTiny ? 7 : (isSmall ? 9 : 12);
  ink(...colors.headerBg, 200).box(margin, contentY + 2, sw - margin * 2, titleH);
  ink(...colors.accent).write(isTiny ? "NOW" : "CURRENT CONDITIONS", { x: margin + 2, y: contentY + (isTiny ? 2 : 4) },
    undefined, undefined, false, font);
  ink(...colors.divider).line(margin, contentY + 2 + titleH, sw - margin, contentY + 2 + titleH);
  
  // Get temperature values
  const temp = Math.round(current.temperature_2m * 9/5 + 32);
  const feelsLike = Math.round(current.apparent_temperature * 9/5 + 32);
  const humidity = Math.round(current.relative_humidity_2m);
  const wind = Math.round(current.wind_speed_10m * 0.621371);
  const windDir = getWindDirection(current.wind_direction_10m);
  const { hour: hours, minute: mins } = getLocationTime();
  
  const dataY = contentY + 2 + titleH + 2;
  
  if (isTiny) {
    // Ultra compact layout for gameboy-style
    // Just temp and condition
    ink(...colors.temp).write(temp + "°", { x: margin + 2, y: dataY }, undefined, undefined, false, font);
    ink(...weather.color).write(weather.icon + weather.short.slice(0,4), 
      { x: margin + 25, y: dataY }, undefined, undefined, false, font);
    // One line details
    ink(...colors.textDim).write("FL:" + feelsLike + " H:" + humidity + "%", 
      { x: margin + 2, y: dataY + 10 }, undefined, undefined, false, smallFont);
  } else if (isNarrow || isSmall) {
    // Compact layout for narrow/small screens
    // Temperature
    ink(...colors.temp).write(temp + "°", { x: margin + 2, y: dataY + 4, size: 2 },
      undefined, undefined, false);
    
    // Mini thermometer
    const thermoX = margin + 45;
    const thermoY = dataY + 4;
    drawThermometer({ ink, line, box }, thermoX, thermoY, 30, temp, 0, 100, 
      [255, 80, 60], [60, 150, 255], [40, 40, 60]);
    
    // Weather condition
    ink(...weather.color).write(weather.icon + " " + weather.short, 
      { x: margin + 2, y: dataY + 32 }, undefined, undefined, false, smallFont);
    
    // Details
    let detailY = dataY + 46;
    ink(...colors.divider, 100).line(margin + 2, detailY - 3, sw - margin - 2, detailY - 3);
    ink(...colors.textDim).write("FEELS " + feelsLike + "° HUM " + humidity + "%", 
      { x: margin + 2, y: detailY }, undefined, undefined, false, smallFont);
    detailY += 10;
    ink(...colors.textDim).write("WIND " + windDir + " " + wind + " MPH", 
      { x: margin + 2, y: detailY }, undefined, undefined, false, smallFont);
  } else {
    // Full layout - left side: temp + thermometer
    const leftPanelW = isVertical ? sw - margin * 2 : (sw - margin * 2) / 2 - 10;
    
    // Big temperature
    ink(...colors.temp).write("" + temp, { x: margin + 8, y: contentY + 28, size: 4 },
      undefined, undefined, false);
    const degX = temp >= 100 ? margin + 73 : (temp >= 10 ? margin + 57 : margin + 40);
    ink(...colors.text).write("°F", { x: degX, y: contentY + 35, size: 2 },
      undefined, undefined, false);
    
    // Thermometer graphic
    const thermoX = margin + 95;
    const thermoY = contentY + 28;
    drawThermometer({ ink, line, box }, thermoX, thermoY, 55, temp, 0, 100, 
      [255, 80, 60], [60, 150, 255], [40, 40, 60]);
    
    // Weather condition with colorful icon
    const icon = weather.icon || "";
    ink(...weather.color).write(icon + " " + weather.text, 
      { x: margin + 8, y: contentY + 90 }, undefined, undefined, false, font);
    
    // Divider line
    if (!isVertical) {
      const divX = sw / 2;
      ink(...colors.divider).line(divX, contentY + 22, divX, contentY + panelH);
    }
    
    // Right side: clock + details
    const rightX = isVertical ? margin + 8 : sw / 2 + 10;
    const rightStartY = isVertical ? contentY + 105 : contentY + 24;
    
    // Clock graphic
    if (!isVertical) {
      const clockX = rightX + 30;
      const clockY = rightStartY + 25;
      drawClock({ ink, line, box }, clockX, clockY, 20, hours, mins, 
        colors.accent, [30, 40, 60]);
    }
    
    // Details table with row backgrounds
    let detailY = isVertical ? rightStartY + 5 : rightStartY + 55;
    const detailW = isVertical ? sw - margin * 2 - 8 : (sw - margin * 2) / 2 - 20;
    
    // Row 1: Feels like
    ink(...colors.headerBg, 100).box(rightX - 2, detailY - 2, detailW, 14);
    ink(...colors.textDim).write("FEELS LIKE", { x: rightX, y: detailY }, undefined, undefined, false, font);
    ink(...(colors.accent2 || colors.accent)).write(feelsLike + "°F", { x: rightX + detailW - 40, y: detailY }, undefined, undefined, false, font);
    detailY += 16;
    
    // Row 2: Humidity with bar
    ink(...colors.panelBg, 150).box(rightX - 2, detailY - 2, detailW, 14);
    ink(...colors.textDim).write("HUMIDITY", { x: rightX, y: detailY }, undefined, undefined, false, font);
    ink(...(colors.accent2 || colors.accent)).write(humidity + "%", { x: rightX + detailW - 35, y: detailY }, undefined, undefined, false, font);
    drawBarGraph({ ink, box }, rightX + 55, detailY + 2, 40, 8, humidity, 100, colors.accent2 || colors.accent, [30, 40, 60]);
    detailY += 16;
    
    // Row 3: Wind
    ink(...colors.headerBg, 100).box(rightX - 2, detailY - 2, detailW, 14);
    ink(...colors.textDim).write("WIND", { x: rightX, y: detailY }, undefined, undefined, false, font);
    ink(...(colors.accent2 || colors.accent)).write(windDir + " " + wind + " mph", { x: rightX + detailW - 70, y: detailY }, undefined, undefined, false, font);
  }
}

function paintForecast({ ink, box, line, screen, contentY, layout }) {
  const { width: sw, height: sh } = screen;
  const { isTiny, isSmall, font, smallFont, contentH } = layout || {};
  const daily = weatherData.daily;
  
  const isNarrow = sw < 200;
  const margin = isTiny ? 1 : (isSmall ? 2 : 6);
  const panelH = contentH - 4;  // Use calculated content height
  const panelW = sw - margin * 2;
  const titleH = isTiny ? 7 : (isSmall ? 9 : 12);
  
  // Main panel with borders
  ink(...colors.panelBg).box(margin, contentY + 2, panelW, panelH);
  ink(...colors.divider).line(margin, contentY + 2, margin + panelW, contentY + 2);
  ink(...colors.divider).line(margin, contentY + 2 + panelH, margin + panelW, contentY + 2 + panelH);
  
  // Title bar
  ink(...colors.headerBg, 200).box(margin, contentY + 2, panelW, titleH);
  ink(...colors.accent).write(isTiny ? "7D" : "7-DAY FORECAST", { x: margin + 2, y: contentY + (isTiny ? 2 : 4) },
    undefined, undefined, false, font);
  ink(...colors.divider).line(margin, contentY + 2 + titleH, margin + panelW, contentY + 2 + titleH);
  
  // Calculate how many days fit
  const headerH = isTiny ? 0 : 10;
  const availableH = panelH - titleH - headerH - 4;
  const rowH = isTiny ? 9 : (isSmall ? 11 : 13);
  const numDays = Math.min(Math.floor(availableH / rowH), daily.time?.length || 7);
  
  // Column headers
  const startY = contentY + 2 + titleH + 2;
  if (!isTiny) {
    ink(...colors.textDim).write("DAY", { x: margin + 2, y: startY }, undefined, undefined, false, smallFont);
    ink(...colors.textDim).write("HI/LO", { x: sw - margin - (isSmall ? 35 : 50), y: startY }, undefined, undefined, false, smallFont);
    ink(...colors.divider, 100).line(margin + 2, startY + 8, margin + panelW - 2, startY + 8);
  }
  
  const dataStartY = isTiny ? startY : startY + 10;
  
  // Find temp range for bar graphs
  let minTemp = 999, maxTemp = -999;
  for (let i = 0; i < numDays; i++) {
    const lo = Math.round(daily.temperature_2m_min[i] * 9/5 + 32);
    const hi = Math.round(daily.temperature_2m_max[i] * 9/5 + 32);
    if (lo < minTemp) minTemp = lo;
    if (hi > maxTemp) maxTemp = hi;
  }
  
  for (let i = 0; i < numDays; i++) {
    const y = dataStartY + i * rowH;
    const date = new Date(daily.time[i]);
    const dayName = i === 0 ? (isTiny ? "TD" : "TODAY") : (isTiny ? DAYS[date.getDay()].slice(0,2) : DAYS[date.getDay()]);
    const code = daily.weather_code[i];
    const weather = WEATHER_CODES[code] || { short: "???", color: [200,200,200], icon: "?" };
    const hi = Math.round(daily.temperature_2m_max[i] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[i] * 9/5 + 32);
    
    // Alternating row backgrounds
    if (i % 2 === 0) {
      ink(...colors.headerBg, 60).box(margin + 1, y - 1, panelW - 2, rowH);
    }
    
    // Day name
    ink(...colors.text).write(isTiny ? dayName : (isNarrow ? dayName.slice(0,3) : dayName.padEnd(6)), 
      { x: margin + 2, y }, undefined, undefined, false, smallFont);
    
    // Weather icon + condition (colorful!)
    const icon = weather.icon || "";
    const iconX = isTiny ? margin + 16 : (isNarrow ? margin + 30 : 55);
    ink(...weather.color).write(icon + ((isTiny || isNarrow) ? "" : " " + weather.short), { x: iconX, y },
      undefined, undefined, false, smallFont);
    
    // Temperature range bar (only on larger screens)
    if (!isNarrow && !isTiny && !isSmall) {
      const barX = sw - margin - 100;
      const barW = 35;
      const range = maxTemp - minTemp || 1;
      const loPos = Math.floor(((lo - minTemp) / range) * barW);
      const hiPos = Math.floor(((hi - minTemp) / range) * barW);
      
      // Background bar
      ink(...colors.textDim, 80).box(barX, y + 3, barW, 6);
      // Temperature range bar
      ink(...colors.accent2 || colors.accent).box(barX + loPos, y + 3, hiPos - loPos + 2, 6);
    }
    
    // Temps
    const tempX = sw - margin - (isTiny ? 30 : (isNarrow ? 40 : 55));
    ink(...colors.temp).write(hi + "°", { x: tempX, y }, undefined, undefined, false, smallFont);
    ink(...colors.textDim).write(lo + "°", { x: tempX + (isTiny ? 15 : 22), y }, undefined, undefined, false, smallFont);
  }
}

function paintDetails({ ink, box, line, screen, contentY, layout }) {
  const { width: sw, height: sh } = screen;
  const { isTiny, isSmall, font, smallFont, contentH } = layout || {};
  const current = weatherData.current;
  const daily = weatherData.daily;
  
  const isNarrow = sw < 200;
  const margin = isTiny ? 1 : (isSmall ? 2 : 10);
  const panelH = contentH - 4; // Use calculated content height
  const titleH = isTiny ? 8 : (isSmall ? 10 : 14);
  
  ink(...colors.panelBg).box(margin, contentY + 2, sw - margin * 2, panelH);
  
  // Title
  ink(...colors.headerBg, 200).box(margin, contentY + 2, sw - margin * 2, titleH);
  ink(...colors.accent).write(isTiny ? "DTL" : "TODAY'S DETAILS", { x: margin + 2, y: contentY + (isTiny ? 2 : 4) },
    undefined, undefined, false, font);
  
  const col1X = margin + 2;
  const col2X = isTiny ? col1X : (isNarrow ? col1X : sw / 2 + 2);
  let y = contentY + 2 + titleH + 2;
  const lineH = isTiny ? 8 : (isSmall ? 10 : 16);
  const rowW = isNarrow ? sw - margin * 2 - 4 : (sw - margin * 2) / 2 - 8;
  
  // Row backgrounds for table look
  let rowIdx = 0;
  const drawRow = (x, w) => {
    if (rowIdx % 2 === 0) {
      ink(...colors.headerBg, 60).box(x, y - 1, w, lineH - 1);
    }
    rowIdx++;
  };
  
  if (daily.sunrise && daily.sunset) {
    const sunriseDate = new Date(daily.sunrise[0]);
    const sunsetDate = new Date(daily.sunset[0]);
    const sunrise = sunriseDate.toLocaleTimeString("en-US", { 
      timeZone: locationTimezone, hour: "2-digit", minute: "2-digit" 
    });
    const sunset = sunsetDate.toLocaleTimeString("en-US", { 
      timeZone: locationTimezone, hour: "2-digit", minute: "2-digit" 
    });
    
    drawRow(col1X - 2, isNarrow ? rowW + 4 : rowW);
    if (isNarrow) {
      ink(...colors.textDim).write("* RISE", { x: col1X, y }, undefined, undefined, false, font);
      ink(...(colors.accent2 || colors.accent)).write(sunrise, { x: col1X + 50, y }, undefined, undefined, false, font);
    } else {
      ink(...colors.textDim).write("SUNRISE", { x: col1X, y }, undefined, undefined, false, font);
      ink(...(colors.accent2 || colors.accent)).write("* " + sunrise, { x: col1X + 60, y }, undefined, undefined, false, font);
      ink(...colors.textDim).write("SUNSET", { x: col2X, y }, undefined, undefined, false, font);
      ink(...(colors.accent2 || colors.accent)).write("v " + sunset, { x: col2X + 55, y }, undefined, undefined, false, font);
    }
    y += lineH;
    if (isNarrow) {
      drawRow(col1X - 2, rowW + 4);
      ink(...colors.textDim).write("v SET", { x: col1X, y }, undefined, undefined, false, font);
      ink(...(colors.accent2 || colors.accent)).write(sunset, { x: col1X + 50, y }, undefined, undefined, false, font);
      y += lineH;
    }
  }
  
  if (daily.temperature_2m_max && daily.temperature_2m_min) {
    const hi = Math.round(daily.temperature_2m_max[0] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[0] * 9/5 + 32);
    
    drawRow(col1X - 2, isNarrow ? rowW + 4 : rowW);
    if (isNarrow) {
      ink(...colors.textDim).write("HI/LO", { x: col1X, y }, undefined, undefined, false, font);
      ink(...colors.temp).write(hi + "°", { x: col1X + 40, y }, undefined, undefined, false, font);
      ink(...colors.textDim).write("/" + lo + "°", { x: col1X + 62, y }, undefined, undefined, false, font);
    } else {
      ink(...colors.textDim).write("HIGH", { x: col1X, y }, undefined, undefined, false, font);
      ink(...colors.temp).write(hi + "°F", { x: col1X + 45, y }, undefined, undefined, false, font);
      ink(...colors.textDim).write("LOW", { x: col2X, y }, undefined, undefined, false, font);
      ink(...colors.textDim).write(lo + "°F", { x: col2X + 40, y }, undefined, undefined, false, font);
    }
    y += lineH;
  }
  
  // Precipitation and wind row
  drawRow(col1X - 2, isNarrow ? rowW + 4 : rowW);
  if (daily.precipitation_probability_max) {
    const precip = daily.precipitation_probability_max[0];
    ink(...colors.textDim).write(isNarrow ? "~ RAIN" : "PRECIP", { x: col1X, y }, undefined, undefined, false, isNarrow ? font : font);
    ink(...(colors.accent2 || colors.text)).write(precip + "%", { x: col1X + (isNarrow ? 55 : 50), y }, undefined, undefined, false, font);
  }
  
  const wind = Math.round(current.wind_speed_10m * 0.621371);
  const windDir = getWindDirection(current.wind_direction_10m);
  if (!isNarrow) {
    ink(...colors.textDim).write("WIND", { x: col2X, y }, undefined, undefined, false, font);
    ink(...(colors.accent2 || colors.text)).write("> " + windDir + " " + wind + " mph", { x: col2X + 40, y }, undefined, undefined, false, font);
  }
  y += lineH;
  
  // New row: cloud cover and visibility
  drawRow(col1X - 2, isNarrow ? rowW + 4 : rowW);
  const cloudCover = Math.round(current.cloud_cover || 0);
  ink(...colors.textDim).write(isNarrow ? "= CLOUD" : "CLOUDS", { x: col1X, y }, undefined, undefined, false, isNarrow ? font : font);
  ink(...(colors.accent2 || colors.text)).write(cloudCover + "%", { x: col1X + (isNarrow ? 55 : 50), y }, undefined, undefined, false, font);
  
  if (!isNarrow && current.visibility) {
    const visMiles = Math.round(current.visibility / 1609.34);
    ink(...colors.textDim).write("VISIB", { x: col2X, y }, undefined, undefined, false, font);
    ink(...(colors.accent2 || colors.text)).write(visMiles + " mi", { x: col2X + 45, y }, undefined, undefined, false, font);
  }
  y += lineH;
  
  // New row: UV index and wind gusts
  if (daily.uv_index_max?.[0] !== undefined || current.wind_gusts_10m) {
    drawRow(col1X - 2, isNarrow ? rowW + 4 : rowW);
    if (daily.uv_index_max?.[0] !== undefined) {
      const uvIndex = Math.round(daily.uv_index_max[0]);
      const uvColor = uvIndex <= 2 ? colors.textDim : uvIndex <= 5 ? [255, 200, 50] : [255, 100, 50];
      ink(...colors.textDim).write(isNarrow ? "* UV" : "UV INDEX", { x: col1X, y }, undefined, undefined, false, isNarrow ? font : font);
      ink(...uvColor).write("" + uvIndex, { x: col1X + (isNarrow ? 45 : 65), y }, undefined, undefined, false, font);
    }
    if (!isNarrow && current.wind_gusts_10m) {
      const gusts = Math.round(current.wind_gusts_10m * 0.621371);
      ink(...colors.textDim).write("GUSTS", { x: col2X, y }, undefined, undefined, false, font);
      ink(...(colors.accent2 || colors.text)).write(gusts + " mph", { x: col2X + 45, y }, undefined, undefined, false, font);
    }
  }
}

function paintExtendedForecast({ ink, box, screen, contentY, layout }) {
  const { width: sw, height: sh } = screen;
  const { isTiny, isSmall, font, smallFont, contentH } = layout || {};
  const daily = weatherData.daily;
  
  const isNarrow = sw < 200;
  const margin = isTiny ? 1 : (isSmall ? 2 : 10);
  const panelH = contentH - 4; // Use calculated content height
  const titleH = isTiny ? 8 : (isSmall ? 10 : 14);
  
  ink(...colors.panelBg).box(margin, contentY + 2, sw - margin * 2, panelH);
  
  // Title
  ink(...colors.headerBg, 200).box(margin, contentY + 2, sw - margin * 2, titleH);
  ink(...colors.accent).write(isTiny ? "OUT" : (isNarrow ? "OUTLOOK" : "EXTENDED OUTLOOK"), { x: margin + 2, y: contentY + (isTiny ? 2 : 4) },
    undefined, undefined, false, font);
  
  // Calculate how many rows can fit
  const availableH = panelH - titleH - 6;
  const rowH = isTiny ? 12 : (isSmall ? 14 : 28);
  const numDays = Math.min(Math.floor(availableH / rowH), daily.time?.length - 4 || 3);
  const startY = contentY + 2 + titleH + 2;
  
  for (let i = 4; i < Math.min(4 + numDays, daily.time?.length || 0); i++) {
    const idx = i - 4;
    const y = startY + idx * rowH;
    const date = new Date(daily.time[i]);
    const dayName = isTiny ? DAYS[date.getDay()].slice(0,2) : (isNarrow ? DAYS[date.getDay()] : FULL_DAYS[date.getDay()]);
    const code = daily.weather_code[i];
    const weather = WEATHER_CODES[code] || { text: "Unknown", short: "???", color: [200,200,200], icon: "?" };
    const hi = Math.round(daily.temperature_2m_max[i] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[i] * 9/5 + 32);
    
    // Day name
    ink(...colors.text).write(dayName, { x: margin + 2, y }, undefined, undefined, false, smallFont);
    
    // Weather with icon (colorful!)
    const icon = weather.icon || "";
    if (isTiny) {
      ink(...weather.color).write(icon, { x: margin + 20, y }, undefined, undefined, false);
      ink(...colors.temp).write(hi + "/" + lo, { x: sw - 30, y }, undefined, undefined, false);
    } else if (isNarrow || isSmall) {
      ink(...weather.color).write(icon, { x: margin + 35, y }, undefined, undefined, false, smallFont);
      ink(...colors.temp).write(hi + "°", { x: sw - 45, y }, undefined, undefined, false, smallFont);
      ink(...colors.textDim).write(lo + "°", { x: sw - 25, y }, undefined, undefined, false, smallFont);
    } else {
      ink(...weather.color).write(icon + " " + weather.text, { x: 100, y },
        undefined, undefined, false, smallFont);
      ink(...colors.temp).write("Hi " + hi + "°", { x: sw - 90, y }, undefined, undefined, false, smallFont);
      ink(...colors.textDim).write("Lo " + lo + "°", { x: sw - 90, y: y + 12 }, undefined, undefined, false, smallFont);
    }
  }
}

// Scrolling text forecast (Weather Channel style)
function paintTextForecast({ ink, box, line, screen, contentY, layout }) {
  const { width: sw, height: sh } = screen;
  const { isTiny, isSmall, font, smallFont, contentH } = layout || {};
  const isNarrow = sw < 200;
  const margin = isTiny ? 1 : (isSmall ? 2 : 10);
  const panelH = contentH - 4; // Use calculated content height
  const titleH = isTiny ? 8 : (isSmall ? 10 : 16);
  
  // Panel background
  ink(...colors.panelBg).box(margin, contentY + 2, sw - margin * 2, panelH);
  
  // Title bar
  ink(...colors.headerBg).box(margin, contentY + 2, sw - margin * 2, titleH);
  ink(...colors.accent).write(isTiny ? "TXT" : "LOCAL FORECAST", { x: margin + 2, y: contentY + (isTiny ? 2 : 4) },
    undefined, undefined, false, font);
  
  if (!weatherData?.daily || !location) return;
  
  const daily = weatherData.daily;
  const current = weatherData.current;
  const lineH = isTiny ? 8 : (isSmall ? 10 : 12);
  const textStartY = contentY + 2 + titleH + 2;
  const visibleH = panelH - titleH - 4;
  
  // Build forecast text lines
  const lines = [];
  
  // Current conditions summary with more detail
  const code = current?.weather_code || 0;
  const weather = WEATHER_CODES[code] || { text: "Unknown", icon: "?" };
  const temp = Math.round((current?.temperature_2m || 0) * 9/5 + 32);
  const feelsLike = Math.round((current?.apparent_temperature || 0) * 9/5 + 32);
  const humidity = Math.round(current?.relative_humidity_2m || 0);
  const wind = Math.round((current?.wind_speed_10m || 0) * 0.621371);
  const gusts = Math.round((current?.wind_gusts_10m || 0) * 0.621371);
  const cloudCover = Math.round(current?.cloud_cover || 0);
  const visibility = current?.visibility ? Math.round(current.visibility / 1609.34) : null; // meters to miles
  
  lines.push({ text: "══════ CURRENT CONDITIONS ══════", color: colors.accent });
  lines.push({ text: "", color: colors.text });
  lines.push({ text: weather.icon + " " + weather.text, color: weather.color });
  lines.push({ text: "Temperature: " + temp + "°F", color: colors.temp });
  lines.push({ text: "Feels like: " + feelsLike + "°F", color: colors.textDim });
  lines.push({ text: "", color: colors.text });
  lines.push({ text: "Humidity: " + humidity + "%", color: colors.textDim });
  lines.push({ text: "Cloud Cover: " + cloudCover + "%", color: colors.textDim });
  if (visibility) {
    lines.push({ text: "Visibility: " + visibility + " mi", color: colors.textDim });
  }
  lines.push({ text: "Wind: " + wind + " mph, Gusts: " + gusts + " mph", color: colors.textDim });
  lines.push({ text: "", color: colors.text });
  
  // Today's summary
  if (daily.uv_index_max?.[0] !== undefined) {
    const uvIndex = Math.round(daily.uv_index_max[0]);
    const uvLevel = uvIndex <= 2 ? "Low" : uvIndex <= 5 ? "Moderate" : uvIndex <= 7 ? "High" : uvIndex <= 10 ? "Very High" : "Extreme";
    lines.push({ text: "UV Index: " + uvIndex + " (" + uvLevel + ")", color: uvIndex > 5 ? [255, 150, 50] : colors.textDim });
  }
  if (daily.sunshine_duration?.[0] !== undefined) {
    const sunHours = Math.round(daily.sunshine_duration[0] / 3600);
    lines.push({ text: "Sunshine: " + sunHours + " hours today", color: [255, 220, 100] });
  }
  lines.push({ text: "", color: colors.text });
  lines.push({ text: "", color: colors.text });
  
  // Extended forecast with more data
  lines.push({ text: "══════ 7-DAY FORECAST ══════", color: colors.accent });
  lines.push({ text: "", color: colors.text });
  
  for (let i = 0; i < Math.min(7, daily.time?.length || 0); i++) {
    const date = new Date(daily.time[i]);
    const dayName = i === 0 ? "TODAY" : FULL_DAYS[date.getDay()].toUpperCase();
    const dayCode = daily.weather_code[i];
    const dayWeather = WEATHER_CODES[dayCode] || { text: "Unknown", short: "???", icon: "?" };
    const hi = Math.round(daily.temperature_2m_max[i] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[i] * 9/5 + 32);
    const precip = daily.precipitation_probability_max?.[i] || 0;
    const precipAmt = daily.precipitation_sum?.[i] || 0;
    const windMax = Math.round((daily.wind_speed_10m_max?.[i] || 0) * 0.621371);
    const gustMax = Math.round((daily.wind_gusts_10m_max?.[i] || 0) * 0.621371);
    const uvDay = daily.uv_index_max?.[i];
    
    lines.push({ text: "── " + dayName + " ──", color: colors.accent2 || colors.accent });
    lines.push({ text: dayWeather.icon + " " + dayWeather.text, color: dayWeather.color });
    lines.push({ text: "High " + hi + "°F  Low " + lo + "°F", color: colors.temp });
    if (precip > 0) {
      const precipText = precipAmt > 0 ? " (" + precipAmt.toFixed(1) + "mm)" : "";
      lines.push({ text: "Precip chance: " + precip + "%" + precipText, color: [100, 180, 255] });
    }
    lines.push({ text: "Wind: up to " + windMax + " mph, gusts " + gustMax + " mph", color: colors.textDim });
    if (uvDay !== undefined) {
      lines.push({ text: "UV Index: " + Math.round(uvDay), color: colors.textDim });
    }
    lines.push({ text: "", color: colors.text });
  }
  
  // Footer
  lines.push({ text: "", color: colors.text });
  lines.push({ text: "════════════════════════════════", color: colors.divider });
  lines.push({ text: "Data: Open-Meteo API", color: colors.textDim });
  lines.push({ text: "", color: colors.text });
  
  // Scroll through lines
  const totalH = lines.length * lineH;
  const scrollOffset = Math.floor(textForecastScroll) % (totalH + visibleH);
  
  for (let i = 0; i < lines.length; i++) {
    const y = textStartY + i * lineH - scrollOffset;
    if (y >= textStartY - lineH && y < textStartY + visibleH) {
      ink(...lines[i].color).write(lines[i].text, { x: margin + 6, y },
        undefined, undefined, false, font);
    }
  }
  
  // Gradient fade at top and bottom
  for (let i = 0; i < 8; i++) {
    const alpha = 255 - i * 30;
    ink(...colors.panelBg.slice(0, 3), alpha).box(margin + 1, textStartY + i, sw - margin * 2 - 2, 1);
    ink(...colors.panelBg.slice(0, 3), alpha).box(margin + 1, textStartY + visibleH - i - 1, sw - margin * 2 - 2, 1);
  }
}

function buildCrawlText() {
  if (!weatherData || !location) {
    crawlText = "  Loading weather data...  ";
    return;
  }
  
  const current = weatherData.current;
  const daily = weatherData.daily;
  const code = current.weather_code;
  const weather = WEATHER_CODES[code] || { text: "Unknown", icon: "?" };
  const temp = Math.round(current.temperature_2m * 9/5 + 32);
  const hi = daily.temperature_2m_max ? Math.round(daily.temperature_2m_max[0] * 9/5 + 32) : temp;
  const lo = daily.temperature_2m_min ? Math.round(daily.temperature_2m_min[0] * 9/5 + 32) : temp;
  
  const locStr = location.region ? location.name + ", " + location.region : location.name;
  const icon = weather.icon || "";
  
  crawlText = "  ★ " + locStr + " ★  " + icon + " Currently " + temp + "°F " + weather.text + "  ★  Today's High " + hi + "°F  Low " + lo + "°F  ★  ";
  crawlText += crawlText;
}

function meta() {
  return {
    title: "Weather",
    desc: "Your Local Forecast - Weather Channel Style",
  };
}

export { boot, sim, paint, meta };

async function geocode(query) {
  const url = "https://geocoding-api.open-meteo.com/v1/search?name=" + encodeURIComponent(query) + "&count=1&language=en&format=json";
  const res = await fetch(url);
  const data = await res.json();
  
  if (data.results?.length > 0) {
    const r = data.results[0];
    return {
      name: r.name,
      region: r.admin1 || r.country || "",
      country: r.country_code || "",
      latitude: r.latitude,
      longitude: r.longitude,
    };
  }
  return null;
}

async function getWeather(lat, lon) {
  const params = new URLSearchParams({
    latitude: lat,
    longitude: lon,
    current: "temperature_2m,relative_humidity_2m,apparent_temperature,weather_code,wind_speed_10m,wind_direction_10m,wind_gusts_10m,precipitation,cloud_cover,visibility,is_day",
    daily: "weather_code,temperature_2m_max,temperature_2m_min,apparent_temperature_max,apparent_temperature_min,precipitation_sum,precipitation_probability_max,sunrise,sunset,uv_index_max,wind_speed_10m_max,wind_gusts_10m_max,sunshine_duration",
    timezone: "auto",
    forecast_days: 7,
  });
  
  const res = await fetch("https://api.open-meteo.com/v1/forecast?" + params);
  if (!res.ok) throw new Error("API error: " + res.status);
  return await res.json();
}

function getWindDirection(degrees) {
  const dirs = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"];
  return dirs[Math.round(degrees / 45) % 8];
}

// Get current time in the location's timezone
function getLocationTime() {
  const now = new Date();
  // Use Intl.DateTimeFormat to get time in location's timezone
  const formatter = new Intl.DateTimeFormat("en-US", {
    timeZone: locationTimezone,
    hour: "2-digit",
    minute: "2-digit",
    hour12: false,
  });
  const parts = formatter.formatToParts(now);
  const hour = parseInt(parts.find(p => p.type === "hour").value);
  const minute = parseInt(parts.find(p => p.type === "minute").value);
  return { hour, minute, totalMins: hour * 60 + minute };
}

// Format time for display in location's timezone
function formatLocationTime() {
  const now = new Date();
  return now.toLocaleTimeString("en-US", {
    timeZone: locationTimezone,
    hour: "2-digit",
    minute: "2-digit",
  });
}
