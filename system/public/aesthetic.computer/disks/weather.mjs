// Weather - Weather Channel style with smooth jazz

const TOP_BAR_BOTTOM = 21; // Reserved space for prompt HUD label
const REFRESH_INTERVAL = 5 * 60 * 1000; // Refresh weather every 5 minutes

let loading = true;
let error = null;
let location = null;
let weatherData = null;
let frame = 0;
let useFahrenheit = false;
let lastRefresh = 0;

// Animation
let crawlX = 0;
let particles = [];

// Music state - chord changes based on clock seconds
let lastSecond = -1;
let chordIndex = 0;
let activeSounds = [];
let currentChordName = "";

// Melody note visualization
let melodyNote = null;
let melodyNoteTime = 0;
const MELODY_DISPLAY_MS = 800;

// Weather codes
const WEATHER = {
  0: { text: "Clear", type: "clear" },
  1: { text: "Mostly Clear", type: "clear" },
  2: { text: "Partly Cloudy", type: "cloudy" },
  3: { text: "Overcast", type: "cloudy" },
  45: { text: "Fog", type: "fog" },
  48: { text: "Freezing Fog", type: "fog" },
  51: { text: "Light Drizzle", type: "rain" },
  53: { text: "Drizzle", type: "rain" },
  55: { text: "Heavy Drizzle", type: "rain" },
  61: { text: "Light Rain", type: "rain" },
  63: { text: "Rain", type: "rain" },
  65: { text: "Heavy Rain", type: "rain" },
  71: { text: "Light Snow", type: "snow" },
  73: { text: "Snow", type: "snow" },
  75: { text: "Heavy Snow", type: "snow" },
  95: { text: "Thunderstorm", type: "storm" },
};

// Music - ii-V-I jazz progression
const NOTE_FREQS = {
  'C': 261.63, 'D': 293.66, 'E': 329.63, 'F': 349.23,
  'G': 392.00, 'A': 440.00, 'B': 493.88,
  'Bb': 466.16, 'C#': 277.18, 'F#': 369.99, 'G#': 415.30,
};

const CHORD_NAMES = ['Dm7', 'G7', 'Cmaj7', 'Fmaj7', 'Bm7b5', 'E7', 'Am7', 'A7'];
const CHORDS = [
  ['D', 'F', 'A', 'C'],     // Dm7
  ['G', 'B', 'D', 'F'],     // G7
  ['C', 'E', 'G', 'B'],     // Cmaj7
  ['F', 'A', 'C', 'E'],     // Fmaj7
  ['B', 'D', 'F', 'A'],     // Bm7b5
  ['E', 'G#', 'B', 'D'],    // E7
  ['A', 'C', 'E', 'G'],     // Am7
  ['A', 'C#', 'E', 'G'],    // A7
];

function noteToFreq(note, octave) {
  return (NOTE_FREQS[note] || 440) * Math.pow(2, octave - 4);
}

function playChord(chord, name, sound) {
  if (!sound || !chord) return;
  
  currentChordName = name;
  
  for (const s of activeSounds) {
    try { s?.kill?.(0.4); } catch(e) {}
  }
  activeSounds = [];
  
  chord.forEach((note, i) => {
    setTimeout(() => {
      const freq = noteToFreq(note, 3 + (i < 2 ? 0 : 1));
      try {
        const s = sound.synth({
          type: 'triangle',
          tone: freq,
          duration: 6,
          attack: 1.5,
          decay: 0.5,
          volume: 0.09 - (i * 0.012),
        });
        activeSounds.push(s);
      } catch(e) {}
    }, i * 60);
  });
}

function playMelody(chord, sound) {
  if (!sound || !chord || Math.random() > 0.35) return;
  const note = chord[Math.floor(Math.random() * chord.length)];
  melodyNote = note;
  melodyNoteTime = Date.now();
  try {
    sound.synth({
      type: 'sine',
      tone: noteToFreq(note, 5),
      duration: 1.8,
      attack: 0.1,
      decay: 0.85,
      volume: 0.045,
    });
  } catch(e) {}
}

async function fetchWeather() {
  if (!location) return;
  
  // Build URL with more realtime current values
  // Available current values: temperature_2m, relative_humidity_2m, apparent_temperature,
  // precipitation, rain, snowfall, weather_code, cloud_cover, pressure_msl, surface_pressure,
  // wind_speed_10m, wind_direction_10m, wind_gusts_10m
  const tempUnit = useFahrenheit ? "fahrenheit" : "celsius";
  const windUnit = useFahrenheit ? "mph" : "kmh";
  const weatherUrl = `https://api.open-meteo.com/v1/forecast?latitude=${location.lat}&longitude=${location.lon}&current=temperature_2m,apparent_temperature,weather_code,wind_speed_10m,wind_gusts_10m,wind_direction_10m,relative_humidity_2m,precipitation,cloud_cover,pressure_msl&daily=weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset&timezone=auto&forecast_days=5&temperature_unit=${tempUnit}&wind_speed_unit=${windUnit}`;
  
  try {
    const weatherRes = await fetch(weatherUrl);
    weatherData = await weatherRes.json();
    lastRefresh = Date.now();
  } catch (err) {
    console.warn("Weather refresh failed:", err);
  }
}

async function boot({ params, screen }) {
  loading = true;
  error = null;
  frame = 0;
  crawlX = screen?.width || 320;
  particles = [];
  melodyNote = null;
  
  const query = params.join(" ").trim() || "New York";
  
  try {
    const geoUrl = `https://geocoding-api.open-meteo.com/v1/search?name=${encodeURIComponent(query)}&count=1&language=en&format=json`;
    const geoRes = await fetch(geoUrl);
    const geoData = await geoRes.json();
    
    if (!geoData.results?.length) {
      error = "Location not found: " + query;
      loading = false;
      return;
    }
    
    const r = geoData.results[0];
    location = { 
      name: r.name, 
      region: r.admin1 || "", 
      country: r.country_code || "",
      lat: r.latitude, 
      lon: r.longitude 
    };
    
    // Use Fahrenheit for USA locations
    useFahrenheit = location.country === "US";
    
    await fetchWeather();
    
  } catch (err) {
    error = err.message;
  }
  
  loading = false;
}

function sim({ sound, screen }) {
  frame++;
  
  // Auto-refresh weather every 5 minutes (Open-Meteo updates every 15 min)
  if (location && Date.now() - lastRefresh > REFRESH_INTERVAL) {
    fetchWeather();
  }
  
  // Music - chord changes every 8 seconds based on clock
  if (sound) {
    const now = new Date();
    const sec = now.getSeconds();
    const chordSlot = Math.floor(sec / 8);
    
    if (Math.floor(lastSecond / 8) !== chordSlot || lastSecond === -1) {
      chordIndex = chordSlot % CHORDS.length;
      playChord(CHORDS[chordIndex], CHORD_NAMES[chordIndex], sound);
    }
    
    // Melody notes on certain seconds
    if (sec !== lastSecond && sec % 3 === 0) {
      playMelody(CHORDS[chordIndex], sound);
    }
    
    lastSecond = sec;
  }
  
  // Clear melody note after display time
  if (melodyNote && Date.now() - melodyNoteTime > MELODY_DISPLAY_MS) {
    melodyNote = null;
  }
  
  // Crawl text
  crawlX -= 0.6;
  const crawlWidth = 900;
  if (crawlX < -crawlWidth) crawlX = screen?.width || 320;
  
  // Weather particles
  if (weatherData && screen) {
    const code = weatherData.current?.weather_code || 0;
    const type = WEATHER[code]?.type || "clear";
    
    if ((type === "rain" || type === "snow" || type === "storm") && particles.length < 60) {
      particles.push({
        x: Math.random() * screen.width,
        y: -5,
        speed: type === "snow" ? 0.5 + Math.random() : 2 + Math.random() * 2,
        type,
      });
    }
    
    for (let i = particles.length - 1; i >= 0; i--) {
      particles[i].y += particles[i].speed;
      if (particles[i].type === "snow") {
        particles[i].x += Math.sin(frame * 0.05 + i) * 0.3;
      }
      if (particles[i].y > screen.height) particles.splice(i, 1);
    }
  }
}

function paint({ wipe, ink, screen, line, box }) {
  const sw = screen.width;
  const sh = screen.height;
  
  // Layout constants - everything below HUD
  const contentTop = TOP_BAR_BOTTOM;
  const tickerHeight = 20;
  const tickerY = sh - tickerHeight;
  const forecastHeight = 30;
  const forecastY = tickerY - forecastHeight;
  const timelineHeight = 26;
  const timelineY = forecastY - timelineHeight - 2;
  
  // Sky gradient (full screen background)
  for (let y = 0; y < sh; y++) {
    const t = y / sh;
    ink(20 + t * 30, 60 + t * 40, 140 - t * 40).line(0, y, sw, y);
  }
  
  // Particles
  for (const p of particles) {
    if (p.type === "snow") {
      ink(255, 255, 255, 200).box(p.x, p.y, 2, 2);
    } else {
      ink(150, 180, 255, 150).line(p.x, p.y, p.x, p.y + 4);
    }
  }
  
  if (loading) {
    ink(255).write("LOADING...", { center: "xy" });
    return;
  }
  
  if (error) {
    ink(255, 100, 100).write("ERROR", { center: "x", y: sh / 2 - 10 });
    ink(180).write(error.slice(0, 35), { center: "x", y: sh / 2 + 5 });
    return;
  }
  
  if (!weatherData || !location) return;
  
  const current = weatherData.current;
  const code = current?.weather_code || 0;
  const weather = WEATHER[code] || { text: "Unknown" };
  const temp = Math.round(current?.temperature_2m || 0);
  const feelsLike = Math.round(current?.apparent_temperature || temp);
  const wind = Math.round(current?.wind_speed_10m || 0);
  const windGusts = Math.round(current?.wind_gusts_10m || 0);
  const windDir = Math.round(current?.wind_direction_10m || 0);
  const humidity = Math.round(current?.relative_humidity_2m || 0);
  const cloudCover = Math.round(current?.cloud_cover || 0);
  const pressure = Math.round(current?.pressure_msl || 0);
  const precip = current?.precipitation || 0;
  
  const tempUnit = useFahrenheit ? "°F" : "°C";
  const windUnit = useFahrenheit ? "mph" : "km/h";
  
  // Wind direction to compass
  const dirs = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"];
  const windCompass = dirs[Math.round(windDir / 45) % 8];
  
  // Time with seconds (granular clock)
  const now = new Date();
  const timeStr = now.toLocaleTimeString("en-US", {
    timeZone: weatherData.timezone,
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: true,
  });
  
  // === HEADER BAR - below HUD ===
  const headerY = contentTop;
  const headerH = 16;
  ink(20, 50, 120).box(0, headerY, sw, headerH);
  ink(100, 150, 255).line(0, headerY + headerH, sw, headerY + headerH);
  
  // Location (left)
  const locText = location.region ? `${location.name}, ${location.region}` : location.name;
  ink(255).write(locText.toUpperCase(), { x: 4, y: headerY + 4 });
  
  // Time (right)
  ink(255, 220, 100).write(timeStr, { x: sw - timeStr.length * 6 - 4, y: headerY + 4 });
  
  // === MAIN CONTENT AREA ===
  const mainY = headerY + headerH + 4;
  
  // Temperature - large centered
  const tempStr = `${temp}${tempUnit}`;
  ink(255, 255, 180).write(tempStr, { center: "x", y: mainY + 6, size: 2 }, undefined, undefined, false, "unifont");
  
  // Conditions
  ink(200, 220, 255).write(weather.text.toUpperCase(), { center: "x", y: mainY + 38 });
  
  // Details - two lines with more data
  const detailY = mainY + 52;
  ink(180, 200, 220).write(`Feels: ${feelsLike}${tempUnit}  Wind: ${wind}${windUnit} ${windCompass}`, { center: "x", y: detailY });
  ink(160, 180, 200).write(`Humidity: ${humidity}%  Cloud: ${cloudCover}%  Pressure: ${pressure}hPa`, { center: "x", y: detailY + 12 });
  
  // === MUSICAL TIMELINE ===
  ink(15, 35, 90).box(0, timelineY, sw, timelineHeight);
  ink(80, 120, 180).line(0, timelineY, sw, timelineY);
  
  const sec = now.getSeconds();
  const currentSlot = Math.floor(sec / 8);
  const slotW = sw / 8;
  
  for (let i = 0; i < 8; i++) {
    const sx = i * slotW;
    const isActive = i === currentSlot;
    const chordIdx = i % CHORDS.length;
    
    // Active slot highlight
    if (isActive) {
      ink(60, 100, 180).box(sx + 1, timelineY + 1, slotW - 2, timelineHeight - 2);
    }
    
    // Chord name
    const cname = CHORD_NAMES[chordIdx];
    const nameAlpha = isActive ? 255 : 90;
    ink(255, 220, 100, nameAlpha).write(cname, { x: sx + 3, y: timelineY + 3 });
    
    // Note dots
    const chord = CHORDS[chordIdx];
    for (let n = 0; n < chord.length; n++) {
      const dotX = sx + 3 + n * 6;
      const dotY = timelineY + 14;
      const pulse = isActive ? Math.sin(frame * 0.15 + n * 0.5) * 1 : 0;
      const dotAlpha = isActive ? 255 : 50;
      ink(255, 180, 80, dotAlpha).box(dotX, dotY + pulse, 3, 3);
    }
  }
  
  // Progress line
  const slotProgress = (sec % 8) / 8;
  const progressX = Math.floor(currentSlot * slotW + slotProgress * slotW);
  ink(255, 255, 200, 180).line(progressX, timelineY + 1, progressX, timelineY + timelineHeight - 1);
  
  // === MELODY NOTE VISUALIZATION ===
  if (melodyNote) {
    const elapsed = Date.now() - melodyNoteTime;
    const alpha = Math.max(0, 255 - (elapsed / MELODY_DISPLAY_MS) * 255);
    const noteY = timelineY - 16;
    const noteX = sw - 40;
    
    // Pulsing circle for the note
    const pulseSize = 8 + Math.sin(frame * 0.3) * 2;
    ink(255, 255, 150, alpha).box(noteX - pulseSize/2, noteY - pulseSize/2, pulseSize, pulseSize);
    ink(255, 220, 100, alpha).write(melodyNote, { x: noteX + 12, y: noteY - 4 });
  }
  
  // === 5-DAY FORECAST ===
  ink(20, 50, 120).box(0, forecastY, sw, forecastHeight);
  ink(100, 150, 255).line(0, forecastY, sw, forecastY);
  
  if (weatherData.daily) {
    const days = ["SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"];
    const dayW = sw / 5;
    
    for (let i = 0; i < 5; i++) {
      const x = i * dayW + dayW / 2;
      const date = new Date();
      date.setDate(date.getDate() + i);
      const dayName = i === 0 ? "TODAY" : days[date.getDay()];
      
      const hi = Math.round(weatherData.daily.temperature_2m_max[i]);
      const lo = Math.round(weatherData.daily.temperature_2m_min[i]);
      
      ink(150, 180, 220).write(dayName, { x: x - dayName.length * 3, y: forecastY + 4 });
      ink(255, 200, 100).write(`${hi}°`, { x: x - 10, y: forecastY + 15 });
      ink(120, 160, 200).write(`${lo}°`, { x: x + 6, y: forecastY + 15 });
    }
  }
  
  // === BOTTOM TICKER - solid bg then text ===
  ink(180, 140, 0).box(0, tickerY, sw, tickerHeight);
  const crawlText = `NOW: ${temp}${tempUnit} ${weather.text.toUpperCase()}  ·  FEELS LIKE ${feelsLike}${tempUnit}  ·  WIND ${wind}${windUnit} ${windCompass}  ·  GUSTS ${windGusts}${windUnit}  ·  HUMIDITY ${humidity}%  ·  CLOUD COVER ${cloudCover}%  ·  PRESSURE ${pressure}hPa  ·  ${location.name.toUpperCase()} LOCAL FORECAST  ·  `;
  ink(40, 30, 0).write(crawlText, { x: Math.floor(crawlX), y: tickerY + 2 }, undefined, undefined, false, "unifont");
}

function meta() {
  return { title: "Weather", desc: "Local forecast with smooth jazz" };
}

export { boot, sim, paint, meta };
