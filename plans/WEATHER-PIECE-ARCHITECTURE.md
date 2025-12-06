# Weather.mjs AC Piece - Architectural Plan

**Created:** December 6, 2025  
**Status:** ✅ Implemented  

## 🎉 Implementation Complete

The `weather.mjs` piece has been implemented as a **non-interactive, Weather Channel style passive display** featuring:

- **Auto-cycling views** (10 second intervals): Current Conditions → 7-Day Forecast → Today's Details → Extended Outlook
- **Scrolling crawl bar** at bottom with current conditions summary  
- **Open-Meteo API** (free, no API key required)
- **Geocoding** for city name lookup
- **Classic Weather Channel "Local on the 8s" aesthetic** with blue gradient background, panel layouts, and view indicator dots

### Usage
```
weather              # Weather for New York (default)
weather los angeles  # Weather for LA
weather tokyo        # Weather for Tokyo
weather 48.8567,2.3508  # Weather by coordinates
```

---

## 📋 Original Overview

This document outlines the architecture and implementation plan for a `weather.mjs` piece that allows users to query and display weather information through the AC prompt system.

## 🎯 User Experience Goals

### From AC Prompt Usage:
```
weather                     → Get weather for current location (IP-based)
weather los angeles         → Get weather for Los Angeles
weather 90210               → Get weather by ZIP code
weather tokyo               → Get weather for Tokyo, Japan  
weather 48.8567,2.3508      → Get weather by coordinates (Paris)
weather forecast london     → Get multi-day forecast for London
weather --help              → Show usage information
```

### Display Options:
- **Current conditions** - Temperature, humidity, wind, conditions
- **Forecast mode** - Multi-day forecast view
- **Visual representation** - Weather icons, colors based on conditions
- **Sound cues** - Audio feedback for weather conditions (rain, thunder, etc.)

---

## 🔬 Weather API Research & Recommendations

### Option 1: **Open-Meteo** ⭐ RECOMMENDED
**URL:** https://open-meteo.com/en/docs

| Pros | Cons |
|------|------|
| ✅ **No API key required** | ❌ No city name lookup (needs geocoding first) |
| ✅ Completely free for non-commercial use | ❌ Requires separate geocoding API call |
| ✅ No rate limits for reasonable use | |
| ✅ Open source | |
| ✅ High accuracy with multiple weather models | |
| ✅ 16-day forecasts available | |
| ✅ Hourly, daily, and 15-minute data | |
| ✅ No account creation needed | |

**Example API Call:**
```
https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current=temperature_2m,relative_humidity_2m,weather_code,wind_speed_10m&daily=weather_code,temperature_2m_max,temperature_2m_min&timezone=auto
```

**Geocoding API (also free, no key):**
```
https://geocoding-api.open-meteo.com/v1/search?name=London&count=1
```

---

### Option 2: **WeatherAPI.com**
**URL:** https://www.weatherapi.com/

| Pros | Cons |
|------|------|
| ✅ Built-in city/ZIP/IP lookup | ❌ Requires API key |
| ✅ Rich data (astronomy, marine, sports) | ❌ Rate limits on free tier |
| ✅ 14-day forecast | ❌ Account creation required |
| ✅ Air quality & pollen data | ❌ Free tier: 1M calls/month |
| ✅ Weather alerts | |

**Example API Call:**
```
http://api.weatherapi.com/v1/current.json?key=YOUR_KEY&q=London
```

---

### Option 3: **OpenWeatherMap**
**URL:** https://openweathermap.org/api

| Pros | Cons |
|------|------|
| ✅ Well-documented | ❌ Requires API key |
| ✅ One Call API 3.0 comprehensive | ❌ Free tier: 1000 calls/day |
| ✅ Weather maps available | ❌ Account creation required |
| | ❌ Premium features cost money |

---

### 🏆 Final Recommendation: **Open-Meteo**

**Why Open-Meteo is best for AC:**
1. **No API key management** - Simplifies deployment, no env vars needed
2. **No account creation** - Zero friction for contributors
3. **Free forever** - No risk of hitting rate limits or unexpected costs
4. **Open source philosophy** - Aligns with AC's spirit
5. **Excellent data quality** - Uses multiple national weather services

**Implementation Strategy:**
1. Use Open-Meteo Geocoding API to convert city names → coordinates
2. Use Open-Meteo Weather API with coordinates for weather data
3. Support direct lat/lon input for power users

---

## 🏗️ AC Piece Architecture

### File Location
```
system/public/aesthetic.computer/disks/weather.mjs
```

### Core Structure
```javascript
// Weather, 2025.12.06
// Get weather information from the AC prompt.

/* #region 📚 README 
  Usage:
    weather              - Weather for current location (IP geolocation)
    weather [location]   - Weather for a city, ZIP code, or coordinates
    weather forecast [location] - Multi-day forecast
    
  Examples:
    weather
    weather new york
    weather 90210
    weather 48.8567,2.3508
    weather forecast london
#endregion */

/* #region 🏁 TODO 
  - [] Basic weather display
  - [] Location geocoding
  - [] IP-based geolocation fallback
  - [] Multi-day forecast view
  - [] Weather condition icons
  - [] Sound effects for conditions
  - [] Temperature unit toggle (C/F)
#endregion */

let weatherData = null;
let location = null;
let loading = true;
let error = null;
let mode = 'current'; // 'current' or 'forecast'

// 🥾 Boot
async function boot({ params, net, num }) {
  // Parse command parameters
  // Fetch weather data
  // Handle geocoding if needed
}

// 🎨 Paint
function paint({ wipe, ink, screen, write }) {
  // Display weather information
  // Show loading state
  // Render weather icons
}

// 🎪 Act
function act({ event: e }) {
  // Handle keyboard input (refresh, toggle units)
}

// 📰 Meta
function meta({ piece }) {
  return {
    title: "Weather",
    desc: "Check the weather from the prompt.",
  };
}

export { boot, paint, act, meta };
```

### API Wrapper Functions

```javascript
// Geocoding: City name → Coordinates
async function geocode(query) {
  const url = `https://geocoding-api.open-meteo.com/v1/search?name=${encodeURIComponent(query)}&count=1`;
  const res = await fetch(url);
  const data = await res.json();
  
  if (data.results && data.results.length > 0) {
    return {
      name: data.results[0].name,
      country: data.results[0].country,
      latitude: data.results[0].latitude,
      longitude: data.results[0].longitude,
    };
  }
  return null;
}

// Weather: Coordinates → Weather Data
async function getWeather(lat, lon, forecast = false) {
  const params = new URLSearchParams({
    latitude: lat,
    longitude: lon,
    current: 'temperature_2m,relative_humidity_2m,apparent_temperature,weather_code,wind_speed_10m,wind_direction_10m,precipitation',
    daily: 'weather_code,temperature_2m_max,temperature_2m_min,precipitation_probability_max,sunrise,sunset',
    timezone: 'auto',
    forecast_days: forecast ? 7 : 1,
  });
  
  const url = `https://api.open-meteo.com/v1/forecast?${params}`;
  const res = await fetch(url);
  return await res.json();
}

// IP Geolocation fallback (using ipinfo.io free tier or similar)
async function getLocationFromIP() {
  // Could use various free IP geolocation services
  // Or default to a fallback location
}
```

### WMO Weather Codes → Display Mapping

```javascript
const WEATHER_CODES = {
  0: { text: 'Clear sky', icon: '☀️', color: 'yellow' },
  1: { text: 'Mainly clear', icon: '🌤️', color: 'yellow' },
  2: { text: 'Partly cloudy', icon: '⛅', color: 'gray' },
  3: { text: 'Overcast', icon: '☁️', color: 'gray' },
  45: { text: 'Foggy', icon: '🌫️', color: 'gray' },
  48: { text: 'Depositing rime fog', icon: '🌫️', color: 'gray' },
  51: { text: 'Light drizzle', icon: '🌧️', color: 'blue' },
  53: { text: 'Moderate drizzle', icon: '🌧️', color: 'blue' },
  55: { text: 'Dense drizzle', icon: '🌧️', color: 'blue' },
  56: { text: 'Light freezing drizzle', icon: '🌧️', color: 'teal' },
  57: { text: 'Dense freezing drizzle', icon: '🌧️', color: 'teal' },
  61: { text: 'Slight rain', icon: '🌧️', color: 'blue' },
  63: { text: 'Moderate rain', icon: '🌧️', color: 'blue' },
  65: { text: 'Heavy rain', icon: '🌧️', color: 'darkblue' },
  66: { text: 'Light freezing rain', icon: '🌨️', color: 'teal' },
  67: { text: 'Heavy freezing rain', icon: '🌨️', color: 'teal' },
  71: { text: 'Slight snow', icon: '❄️', color: 'white' },
  73: { text: 'Moderate snow', icon: '❄️', color: 'white' },
  75: { text: 'Heavy snow', icon: '❄️', color: 'white' },
  77: { text: 'Snow grains', icon: '❄️', color: 'white' },
  80: { text: 'Slight rain showers', icon: '🌦️', color: 'blue' },
  81: { text: 'Moderate rain showers', icon: '🌦️', color: 'blue' },
  82: { text: 'Violent rain showers', icon: '⛈️', color: 'darkblue' },
  85: { text: 'Slight snow showers', icon: '🌨️', color: 'white' },
  86: { text: 'Heavy snow showers', icon: '🌨️', color: 'white' },
  95: { text: 'Thunderstorm', icon: '⛈️', color: 'purple' },
  96: { text: 'Thunderstorm with slight hail', icon: '⛈️', color: 'purple' },
  99: { text: 'Thunderstorm with heavy hail', icon: '⛈️', color: 'purple' },
};
```

---

## 📊 Data Flow

```
┌─────────────────────────────────────────────────────────────┐
│                     AC Prompt Input                         │
│              weather [location] [options]                    │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Parameter Parsing                         │
│  - Extract location string                                   │
│  - Detect coordinates vs city name vs ZIP                    │
│  - Parse options (forecast, units, etc.)                     │
└─────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
     ┌────────────┐   ┌────────────┐   ┌────────────┐
     │ Coordinates│   │ City Name  │   │ No Location│
     │ Detected   │   │ Detected   │   │ (use IP)   │
     └────────────┘   └────────────┘   └────────────┘
              │               │               │
              │               ▼               │
              │    ┌─────────────────┐        │
              │    │ Open-Meteo      │        │
              │    │ Geocoding API   │        │
              │    └─────────────────┘        │
              │               │               │
              └───────────────┼───────────────┘
                              │
                              ▼
              ┌─────────────────────────────┐
              │     Open-Meteo Weather API  │
              │     (with lat/lon coords)    │
              └─────────────────────────────┘
                              │
                              ▼
              ┌─────────────────────────────┐
              │     Process & Display       │
              │     - Format temperatures    │
              │     - Map weather codes      │
              │     - Render to screen       │
              └─────────────────────────────┘
```

---

## 🎨 Visual Design

### Current Weather View
```
┌───────────────────────────────────────────────┐
│                                               │
│            ☀️ Clear sky                       │
│                                               │
│         Los Angeles, CA                       │
│                                               │
│              72°F                             │
│         Feels like 74°F                       │
│                                               │
│    💨 Wind: 8 mph NW    💧 Humidity: 45%      │
│                                               │
│         Press R to refresh                    │
│         Press F to toggle °F/°C               │
└───────────────────────────────────────────────┘
```

### Forecast View
```
┌───────────────────────────────────────────────┐
│        7-Day Forecast - Los Angeles           │
│                                               │
│  Today   ☀️   72°F / 55°F                     │
│  Sat     🌤️   74°F / 58°F                     │
│  Sun     ⛅   70°F / 56°F                     │
│  Mon     🌧️   65°F / 52°F   40% rain         │
│  Tue     🌧️   63°F / 50°F   60% rain         │
│  Wed     🌤️   68°F / 54°F                     │
│  Thu     ☀️   72°F / 56°F                     │
│                                               │
│         Press R to refresh                    │
│         Press C for current                   │
└───────────────────────────────────────────────┘
```

---

## 🔊 Audio Integration (Future)

Weather-appropriate ambient sounds:
- Rain: Gentle rain audio loop
- Thunder: Thunder sound effects
- Wind: Wind ambience for high wind speeds
- Clear: Optional pleasant ambience

Could integrate with AC's sound system via `sound.synth()` or audio samples.

---

## 🛠️ Implementation Phases

### Phase 1: MVP (This PR)
- [x] Basic piece structure
- [ ] Open-Meteo API integration
- [ ] City name geocoding
- [ ] Current weather display
- [ ] Temperature in both C and F (toggle)

### Phase 2: Enhanced Features
- [ ] Multi-day forecast view
- [ ] Better visual design with pixel weather icons
- [ ] Coordinate input support
- [ ] ZIP code support
- [ ] IP-based location fallback

### Phase 3: Polish
- [ ] Animated weather effects (rain, snow particles)
- [ ] Sound effects for conditions
- [ ] Caching to reduce API calls
- [ ] Sunrise/sunset theming

---

## 📁 Files to Create

```
system/public/aesthetic.computer/disks/weather.mjs  (main piece)
```

Optionally:
```
system/public/aesthetic.computer/disks/common/weather-icons.mjs (icon sprites)
system/public/aesthetic.computer/disks/common/weather-api.mjs (API helpers)
```

---

## 🔗 References

- [Open-Meteo API Docs](https://open-meteo.com/en/docs)
- [Open-Meteo Geocoding API](https://open-meteo.com/en/docs/geocoding-api)
- [WMO Weather Codes](https://open-meteo.com/en/docs#weathervariables)
- [AC Piece Architecture](../WRITE-A-PIECE.md)
- [AC Profile Piece (fetch example)](../system/public/aesthetic.computer/disks/profile.mjs)
- [AC Rain Piece (visual example)](../system/public/aesthetic.computer/disks/rain.mjs)

---

## ✅ Success Criteria

1. User can type `weather london` and see current weather
2. User can type `weather` and get weather based on IP location
3. User can type `weather forecast paris` and see 7-day forecast
4. Temperature can be toggled between °C and °F
5. Weather conditions are clearly displayed with icons/colors
6. Errors are handled gracefully (no location found, API down, etc.)
