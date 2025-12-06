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

// WMO Weather Code Mapping
const WEATHER_CODES = {
  0: { text: "Clear", short: "CLEAR", color: [255, 220, 64], icon: "☀", type: "clear" },
  1: { text: "Mostly Clear", short: "MSTLY CLR", color: [255, 220, 100], icon: "🌤", type: "clear" },
  2: { text: "Partly Cloudy", short: "PTLY CLDY", color: [200, 200, 200], icon: "⛅", type: "cloudy" },
  3: { text: "Overcast", short: "OVERCAST", color: [150, 150, 150], icon: "☁", type: "cloudy" },
  45: { text: "Fog", short: "FOG", color: [180, 180, 180], icon: "🌫", type: "fog" },
  48: { text: "Freezing Fog", short: "FRZG FOG", color: [180, 200, 220], icon: "🌫", type: "fog" },
  51: { text: "Light Drizzle", short: "LT DRZL", color: [100, 150, 200], icon: "🌧", type: "rain" },
  53: { text: "Drizzle", short: "DRIZZLE", color: [80, 130, 180], icon: "🌧", type: "rain" },
  55: { text: "Heavy Drizzle", short: "HVY DRZL", color: [60, 110, 160], icon: "🌧", type: "rain" },
  56: { text: "Freezing Drizzle", short: "FRZG DRZL", color: [100, 180, 220], icon: "🌧", type: "rain" },
  57: { text: "Heavy Freezing Drizzle", short: "HVY FRZ DRZ", color: [80, 160, 200], icon: "🌧", type: "rain" },
  61: { text: "Light Rain", short: "LT RAIN", color: [80, 140, 200], icon: "🌧", type: "rain" },
  63: { text: "Rain", short: "RAIN", color: [60, 120, 180], icon: "🌧", type: "rain" },
  65: { text: "Heavy Rain", short: "HVY RAIN", color: [40, 80, 140], icon: "🌧", type: "rain" },
  66: { text: "Freezing Rain", short: "FRZG RAIN", color: [100, 180, 240], icon: "🌧", type: "rain" },
  67: { text: "Heavy Freezing Rain", short: "HVY FRZ RN", color: [80, 160, 220], icon: "🌧", type: "rain" },
  71: { text: "Light Snow", short: "LT SNOW", color: [220, 230, 255], icon: "❄", type: "snow" },
  73: { text: "Snow", short: "SNOW", color: [200, 210, 240], icon: "❄", type: "snow" },
  75: { text: "Heavy Snow", short: "HVY SNOW", color: [180, 190, 220], icon: "❄", type: "snow" },
  77: { text: "Snow Grains", short: "SNOW GRNS", color: [210, 220, 240], icon: "❄", type: "snow" },
  80: { text: "Light Showers", short: "LT SHWRS", color: [100, 160, 220], icon: "🌦", type: "rain" },
  81: { text: "Showers", short: "SHOWERS", color: [80, 140, 200], icon: "🌦", type: "rain" },
  82: { text: "Heavy Showers", short: "HVY SHWRS", color: [60, 100, 160], icon: "🌧", type: "rain" },
  85: { text: "Light Snow Showers", short: "LT SNW SHW", color: [200, 210, 240], icon: "🌨", type: "snow" },
  86: { text: "Heavy Snow Showers", short: "HVY SNW SHW", color: [180, 190, 220], icon: "🌨", type: "snow" },
  95: { text: "Thunderstorm", short: "T-STORM", color: [180, 100, 200], icon: "⛈", type: "storm" },
  96: { text: "Thunderstorm w/ Hail", short: "T-STRM HAIL", color: [200, 120, 220], icon: "⛈", type: "storm" },
  99: { text: "Severe Thunderstorm", short: "SVR T-STRM", color: [220, 80, 180], icon: "⛈", type: "storm" },
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
const VIEW_DURATION = 600;
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
let starForms = [];
let moonForm = null;
let sunForm = null;
let cloudForms = [];
let rainForms = [];
let snowForms = [];
let use3D = false;
let Form3D = null;

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

function sim({ screen }) {
  frameCount++;
  
  if (!screen) return;
  const { width: sw, height: sh } = screen;
  
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
      if (cloudForm.driftSpeed !== undefined && cloudForm.position) {
        cloudForm.position[0] += cloudForm.driftSpeed;
        // Gentle vertical wobble for organic feel
        if (cloudForm.wobblePhase !== undefined && cloudForm.baseY !== undefined) {
          cloudForm.wobblePhase += cloudForm.wobbleSpeed || 0.02;
          cloudForm.position[1] = cloudForm.baseY + Math.sin(cloudForm.wobblePhase) * 0.05;
        }
        // Reset cloud position when it drifts too far right
        if (cloudForm.position[0] > 14) {
          cloudForm.position[0] = -14;
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
function paint3DBackdrop({ ink, form }) {
  if (!cam) return;
  
  // Night: render 3D stars and moon
  if (isNight) {
    for (const star of starForms) {
      const brightness = 0.5 + 0.5 * Math.sin(star.twinklePhase);
      const alpha = Math.floor(150 + brightness * 105);
      ink(255, 255, 220, alpha).form(star, cam);
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

function paint({ wipe, ink, screen, line, box, text, form }) {
  const { width: sw, height: sh } = screen;
  
  // Lightning flash override
  if (lightningFlash) {
    wipe(255, 255, 255);
    return;
  }
  
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
    paint3DBackdrop({ ink, form });
  } else {
    // Draw 2D animated backdrop elements
    paintBackdrop({ ink, box, line, screen });
  }
  
  if (loading) {
    ink(...colors.text).write("Loading weather data...", { center: "xy" }, 
      undefined, undefined, false, "unifont");
    return;
  }
  
  if (error) {
    ink(255, 100, 100).write("WEATHER", { center: "x", y: sh/2 - 20 },
      undefined, undefined, false, "MatrixChunky8");
    ink(255, 100, 100).write("UNAVAILABLE", { center: "x", y: sh/2 - 5 },
      undefined, undefined, false, "MatrixChunky8");
    ink(...colors.textDim).write(error.slice(0, 30), { center: "x", y: sh/2 + 15 },
      undefined, undefined, false, "unifont");
    return;
  }
  
  if (!weatherData || !location) return;
  
  // Responsive layout detection
  const isNarrow = sw < 200;
  const isVertical = sh > sw;
  
  // Header bar positioned below corner label
  const headerY = 20;
  const headerH = isNarrow ? 18 : 24;
  ink(...colors.headerBg).box(0, headerY, sw, headerH);
  ink(...colors.divider).line(0, headerY + headerH, sw, headerY + headerH);
  
  // Location - truncate for narrow screens
  let locStr = location.region 
    ? location.name + ", " + location.region 
    : location.name;
  if (isNarrow && locStr.length > 12) locStr = location.name.slice(0, 12);
  ink(...colors.text).write(locStr.toUpperCase(), { x: 4, y: headerY + 3 },
    undefined, undefined, false, "MatrixChunky8");
  
  // Time - position from right edge (in location's timezone)
  const timeStr = formatLocationTime();
  const timeBox = text?.box?.(timeStr, { x: 0, y: 0 }, undefined, 1, false, "unifont");
  const timeWidth = timeBox?.box?.width || 45;
  ink(...colors.accent).write(timeStr, { x: sw - timeWidth - 4, y: headerY + (isNarrow ? 4 : 6) },
    undefined, undefined, false, "unifont");
  
  const contentY = headerY + headerH + 2;
  
  switch (currentView) {
    case 0: paintCurrentConditions({ ink, box, line, screen, contentY, text }); break;
    case 1: paintForecast({ ink, box, line, screen, contentY, text }); break;
    case 2: paintDetails({ ink, box, line, screen, contentY, text }); break;
    case 3: paintExtendedForecast({ ink, box, line, screen, contentY, text }); break;
    case 4: paintTextForecast({ ink, box, line, screen, contentY, text }); break;
  }
  
  // Crawl bar at bottom
  const crawlH = isNarrow ? 14 : 18;
  const crawlY = sh - crawlH;
  ink(...colors.crawlBg).box(0, crawlY, sw, crawlH);
  ink(...colors.divider).line(0, crawlY, sw, crawlY);
  ink(...colors.crawlText).write(crawlText, { x: crawlX, y: crawlY + (isNarrow ? 2 : 4) },
    undefined, undefined, false, "unifont");
  
  // View indicator dots
  const dotY = crawlY - 8;
  const dotSize = isNarrow ? 3 : 4;
  const dotSpacing = isNarrow ? 8 : 12;
  for (let i = 0; i < NUM_VIEWS; i++) {
    const dotX = sw/2 - (NUM_VIEWS * (dotSpacing/2)) + i * dotSpacing;
    if (i === currentView) {
      ink(...colors.accent).box(dotX, dotY, dotSize, dotSize);
    } else {
      ink(...colors.textDim).box(dotX, dotY, dotSize, dotSize);
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

function paintCurrentConditions({ ink, box, line, screen, contentY, text }) {
  const { width: sw, height: sh } = screen;
  const current = weatherData.current;
  const code = current.weather_code;
  const weather = WEATHER_CODES[code] || { text: "Unknown", short: "???", color: [200,200,200], icon: "?" };
  
  // Responsive layout
  const isNarrow = sw < 200;
  const isVertical = sh > sw;
  const margin = isNarrow ? 4 : 10;
  const panelH = isVertical ? sh - contentY - 30 : 120;
  
  // Main panel background
  ink(...colors.panelBg).box(margin, contentY + 6, sw - margin * 2, panelH);
  ink(...colors.divider).line(margin, contentY + 6, sw - margin, contentY + 6);
  ink(...colors.divider).line(margin, contentY + 6 + panelH, sw - margin, contentY + 6 + panelH);
  
  // Title bar with background
  ink(...colors.headerBg, 200).box(margin, contentY + 6, sw - margin * 2, 14);
  ink(...colors.accent).write("CURRENT CONDITIONS", { x: margin + 4, y: contentY + 8 },
    undefined, undefined, false, "MatrixChunky8");
  ink(...colors.divider).line(margin, contentY + 20, sw - margin, contentY + 20);
  
  // Get temperature values
  const temp = Math.round(current.temperature_2m * 9/5 + 32);
  const feelsLike = Math.round(current.apparent_temperature * 9/5 + 32);
  const humidity = Math.round(current.relative_humidity_2m);
  const wind = Math.round(current.wind_speed_10m * 0.621371);
  const windDir = getWindDirection(current.wind_direction_10m);
  const { hours, mins } = getLocationTime();
  
  if (isNarrow) {
    // Compact layout for narrow screens
    // Big temperature with small thermometer
    ink(...colors.temp).write("" + temp + "°", { x: margin + 4, y: contentY + 28, size: 3 },
      undefined, undefined, false);
    
    // Mini thermometer
    const thermoX = margin + 55;
    const thermoY = contentY + 28;
    drawThermometer({ ink, line, box }, thermoX, thermoY, 35, temp, 0, 100, 
      [255, 80, 60], [60, 150, 255], [40, 40, 60]);
    
    // Weather condition
    ink(...weather.color).write(weather.icon + " " + weather.short, 
      { x: margin + 4, y: contentY + 68 }, undefined, undefined, false, "unifont");
    
    // Details with separators
    let detailY = contentY + 85;
    ink(...colors.divider, 100).line(margin + 4, detailY - 4, sw - margin - 4, detailY - 4);
    ink(...colors.textDim).write("FEELS " + feelsLike + "°  HUM " + humidity + "%", 
      { x: margin + 4, y: detailY }, undefined, undefined, false, "unifont");
    detailY += 12;
    ink(...colors.textDim).write("WIND " + windDir + " " + wind + " MPH", 
      { x: margin + 4, y: detailY }, undefined, undefined, false, "unifont");
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
      { x: margin + 8, y: contentY + 90 }, undefined, undefined, false, "unifont");
    
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
    ink(...colors.textDim).write("FEELS LIKE", { x: rightX, y: detailY }, undefined, undefined, false, "MatrixChunky8");
    ink(...(colors.accent2 || colors.accent)).write(feelsLike + "°F", { x: rightX + detailW - 40, y: detailY }, undefined, undefined, false, "unifont");
    detailY += 16;
    
    // Row 2: Humidity with bar
    ink(...colors.panelBg, 150).box(rightX - 2, detailY - 2, detailW, 14);
    ink(...colors.textDim).write("HUMIDITY", { x: rightX, y: detailY }, undefined, undefined, false, "MatrixChunky8");
    ink(...(colors.accent2 || colors.accent)).write(humidity + "%", { x: rightX + detailW - 35, y: detailY }, undefined, undefined, false, "unifont");
    drawBarGraph({ ink, box }, rightX + 55, detailY + 2, 40, 8, humidity, 100, colors.accent2 || colors.accent, [30, 40, 60]);
    detailY += 16;
    
    // Row 3: Wind
    ink(...colors.headerBg, 100).box(rightX - 2, detailY - 2, detailW, 14);
    ink(...colors.textDim).write("WIND", { x: rightX, y: detailY }, undefined, undefined, false, "MatrixChunky8");
    ink(...(colors.accent2 || colors.accent)).write(windDir + " " + wind + " mph", { x: rightX + detailW - 70, y: detailY }, undefined, undefined, false, "unifont");
  }
}

function paintForecast({ ink, box, line, screen, contentY }) {
  const { width: sw, height: sh } = screen;
  const daily = weatherData.daily;
  
  const isNarrow = sw < 200;
  const isVertical = sh > sw;
  const margin = isNarrow ? 4 : 10;
  const panelH = isVertical ? sh - contentY - 30 : 120;
  const panelW = sw - margin * 2;
  
  // Main panel with borders
  ink(...colors.panelBg).box(margin, contentY + 6, panelW, panelH);
  ink(...colors.divider).line(margin, contentY + 6, margin + panelW, contentY + 6);
  ink(...colors.divider).line(margin, contentY + 6 + panelH, margin + panelW, contentY + 6 + panelH);
  
  // Title bar
  ink(...colors.headerBg, 200).box(margin, contentY + 6, panelW, 14);
  ink(...colors.accent).write("7-DAY FORECAST", { x: margin + 4, y: contentY + 8 },
    undefined, undefined, false, "MatrixChunky8");
  ink(...colors.divider).line(margin, contentY + 20, margin + panelW, contentY + 20);
  
  // Column headers
  const startY = contentY + 23;
  ink(...colors.textDim).write("DAY", { x: margin + 4, y: startY }, undefined, undefined, false, "MatrixChunky8");
  if (!isNarrow) ink(...colors.textDim).write("CONDITIONS", { x: margin + 55, y: startY }, undefined, undefined, false, "MatrixChunky8");
  ink(...colors.textDim).write("HI/LO", { x: sw - margin - 55, y: startY }, undefined, undefined, false, "MatrixChunky8");
  ink(...colors.divider, 100).line(margin + 4, startY + 10, margin + panelW - 4, startY + 10);
  
  const numDays = isNarrow ? 4 : Math.min(5, daily.time?.length || 0);
  const rowH = isVertical ? Math.min(18, (panelH - 40) / numDays) : 16;
  const dataStartY = startY + 13;
  
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
    const dayName = i === 0 ? "TODAY" : DAYS[date.getDay()];
    const code = daily.weather_code[i];
    const weather = WEATHER_CODES[code] || { short: "???", color: [200,200,200], icon: "?" };
    const hi = Math.round(daily.temperature_2m_max[i] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[i] * 9/5 + 32);
    
    // Alternating row backgrounds
    if (i % 2 === 0) {
      ink(...colors.headerBg, 60).box(margin + 2, y - 1, panelW - 4, rowH);
    }
    
    // Day name
    ink(...colors.text).write(isNarrow ? dayName.slice(0,3) : dayName.padEnd(6), 
      { x: margin + 4, y }, undefined, undefined, false, "MatrixChunky8");
    
    // Weather icon + condition (colorful!)
    const icon = weather.icon || "";
    const iconX = isNarrow ? margin + 30 : 55;
    ink(...weather.color).write(icon + (isNarrow ? "" : " " + weather.short), { x: iconX, y },
      undefined, undefined, false, "unifont");
    
    // Temperature range bar
    if (!isNarrow) {
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
    const tempX = sw - margin - (isNarrow ? 45 : 55);
    ink(...colors.temp).write(hi + "°", { x: tempX, y }, undefined, undefined, false, "unifont");
    ink(...colors.textDim).write(lo + "°", { x: tempX + 25, y }, undefined, undefined, false, "unifont");
  }
}

function paintDetails({ ink, box, screen, contentY }) {
  const { width: sw, height: sh } = screen;
  const current = weatherData.current;
  const daily = weatherData.daily;
  
  const isNarrow = sw < 200;
  const isVertical = sh > sw;
  const margin = isNarrow ? 4 : 10;
  const panelH = isVertical ? sh - contentY - 30 : 120;
  
  ink(...colors.panelBg).box(margin, contentY + 6, sw - margin * 2, panelH);
  
  // Title
  ink(...colors.accent).write("TODAY'S DETAILS", { x: margin + 4, y: contentY + 10 },
    undefined, undefined, false, "MatrixChunky8");
  
  const col1X = margin + 4;
  const col2X = isNarrow ? col1X : sw / 2 + 4;
  let y = contentY + 30;
  const lineH = isNarrow ? 16 : 20;
  
  if (daily.sunrise && daily.sunset) {
    const sunriseDate = new Date(daily.sunrise[0]);
    const sunsetDate = new Date(daily.sunset[0]);
    const sunrise = sunriseDate.toLocaleTimeString("en-US", { 
      timeZone: locationTimezone, hour: "2-digit", minute: "2-digit" 
    });
    const sunset = sunsetDate.toLocaleTimeString("en-US", { 
      timeZone: locationTimezone, hour: "2-digit", minute: "2-digit" 
    });
    
    if (isNarrow) {
      ink(...colors.textDim).write("☀", { x: col1X, y }, undefined, undefined, false, "unifont");
      ink(...(colors.accent2 || colors.accent)).write(sunrise, { x: col1X + 16, y }, undefined, undefined, false, "unifont");
      ink(...colors.textDim).write("🌅", { x: col1X + 65, y }, undefined, undefined, false, "unifont");
      ink(...(colors.accent2 || colors.accent)).write(sunset, { x: col1X + 80, y }, undefined, undefined, false, "unifont");
    } else {
      ink(...colors.textDim).write("SUNRISE", { x: col1X, y }, undefined, undefined, false, "MatrixChunky8");
      ink(...(colors.accent2 || colors.accent)).write("☀ " + sunrise, { x: col1X + 60, y }, undefined, undefined, false, "unifont");
      ink(...colors.textDim).write("SUNSET", { x: col2X, y }, undefined, undefined, false, "MatrixChunky8");
      ink(...(colors.accent2 || colors.accent)).write("🌅 " + sunset, { x: col2X + 55, y }, undefined, undefined, false, "unifont");
    }
    y += lineH;
  }
  
  if (daily.temperature_2m_max && daily.temperature_2m_min) {
    const hi = Math.round(daily.temperature_2m_max[0] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[0] * 9/5 + 32);
    
    if (isNarrow) {
      ink(...colors.textDim).write("HI", { x: col1X, y }, undefined, undefined, false, "MatrixChunky8");
      ink(...colors.temp).write(hi + "°", { x: col1X + 22, y }, undefined, undefined, false, "unifont");
      ink(...colors.textDim).write("LO", { x: col1X + 55, y }, undefined, undefined, false, "MatrixChunky8");
      ink(...colors.textDim).write(lo + "°", { x: col1X + 75, y }, undefined, undefined, false, "unifont");
    } else {
      ink(...colors.textDim).write("HIGH", { x: col1X, y }, undefined, undefined, false, "MatrixChunky8");
      ink(...colors.temp).write(hi + "°F", { x: col1X + 45, y }, undefined, undefined, false, "unifont");
      ink(...colors.textDim).write("LOW", { x: col2X, y }, undefined, undefined, false, "MatrixChunky8");
      ink(...colors.textDim).write(lo + "°F", { x: col2X + 40, y }, undefined, undefined, false, "unifont");
    }
    y += lineH;
  }
  
  if (daily.precipitation_probability_max) {
    const precip = daily.precipitation_probability_max[0];
    ink(...colors.textDim).write(isNarrow ? "💧" : "PRECIP", { x: col1X, y }, undefined, undefined, false, isNarrow ? "unifont" : "MatrixChunky8");
    ink(...(colors.accent2 || colors.text)).write((isNarrow ? "" : "💧 ") + precip + "%", { x: col1X + (isNarrow ? 16 : 50), y }, undefined, undefined, false, "unifont");
  }
  
  const wind = Math.round(current.wind_speed_10m * 0.621371);
  const windDir = getWindDirection(current.wind_direction_10m);
  if (isNarrow) {
    ink(...colors.textDim).write("💨", { x: col1X + 55, y }, undefined, undefined, false, "unifont");
    ink(...(colors.accent2 || colors.text)).write(windDir + " " + wind, { x: col1X + 70, y }, undefined, undefined, false, "unifont");
  } else {
    ink(...colors.textDim).write("WIND", { x: col2X, y }, undefined, undefined, false, "MatrixChunky8");
    ink(...(colors.accent2 || colors.text)).write("💨 " + windDir + " " + wind + " mph", { x: col2X + 40, y }, undefined, undefined, false, "unifont");
  }
}

function paintExtendedForecast({ ink, box, screen, contentY }) {
  const { width: sw, height: sh } = screen;
  const daily = weatherData.daily;
  
  const isNarrow = sw < 200;
  const isVertical = sh > sw;
  const margin = isNarrow ? 4 : 10;
  const panelH = isVertical ? sh - contentY - 30 : 120;
  
  ink(...colors.panelBg).box(margin, contentY + 6, sw - margin * 2, panelH);
  
  // Title
  ink(...colors.accent).write(isNarrow ? "OUTLOOK" : "EXTENDED OUTLOOK", { x: margin + 4, y: contentY + 10 },
    undefined, undefined, false, "MatrixChunky8");
  
  const numDays = isNarrow ? 3 : 3;
  const rowH = isVertical ? Math.min(32, (panelH - 30) / numDays) : 28;
  const startY = contentY + 30;
  
  for (let i = 4; i < Math.min(4 + numDays, daily.time?.length || 0); i++) {
    const idx = i - 4;
    const y = startY + idx * rowH;
    const date = new Date(daily.time[i]);
    const dayName = isNarrow ? DAYS[date.getDay()] : FULL_DAYS[date.getDay()];
    const code = daily.weather_code[i];
    const weather = WEATHER_CODES[code] || { text: "Unknown", short: "???", color: [200,200,200], icon: "?" };
    const hi = Math.round(daily.temperature_2m_max[i] * 9/5 + 32);
    const lo = Math.round(daily.temperature_2m_min[i] * 9/5 + 32);
    
    // Day name
    ink(...colors.text).write(dayName, { x: margin + 4, y }, undefined, undefined, false, "MatrixChunky8");
    
    // Weather with icon (colorful!)
    const icon = weather.icon || "";
    if (isNarrow) {
      ink(...weather.color).write(icon, { x: margin + 35, y }, undefined, undefined, false, "unifont");
      ink(...colors.temp).write(hi + "°", { x: sw - 45, y }, undefined, undefined, false, "unifont");
      ink(...colors.textDim).write(lo + "°", { x: sw - 25, y }, undefined, undefined, false, "unifont");
    } else {
      ink(...weather.color).write(icon + " " + (isNarrow ? weather.short : weather.text), { x: 100, y },
        undefined, undefined, false, "unifont");
      ink(...colors.temp).write("Hi " + hi + "°", { x: sw - 90, y }, undefined, undefined, false, "unifont");
      ink(...colors.textDim).write("Lo " + lo + "°", { x: sw - 90, y: y + 12 }, undefined, undefined, false, "unifont");
    }
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
    current: "temperature_2m,relative_humidity_2m,apparent_temperature,weather_code,wind_speed_10m,wind_direction_10m,precipitation",
    daily: "weather_code,temperature_2m_max,temperature_2m_min,precipitation_probability_max,sunrise,sunset",
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
