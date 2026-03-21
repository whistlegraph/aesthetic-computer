# 🌤️ Weather Display - Responsive Design System

## Overview

A Weather Channel style ambient display with generative music, designed to work from gameboy (128x128) to full HD resolutions. Uses a **continuous vertical scroll** instead of discrete cards, creating a smooth, TV-like experience.

---

## Fonts

**Current Design: MatrixChunky8 everywhere** for a consistent micro/pixel aesthetic.

| Font | Height | Usage |
|------|--------|-------|
| `MatrixChunky8` | 8px | All text - headers, labels, values, crawl |

Future iterations may add font variations for different breakpoints.

---

## Breakpoints

| Name | Width | Height | Description |
|------|-------|--------|-------------|
| `tiny` | <128px | <128px | Gameboy / tiny displays |
| `small` | <180px | <180px | Small embedded displays |
| `narrow` | <200px | any | Portrait mobile |
| `large` | ≥320px | any | Desktop / TV |

```javascript
const isTiny = sw < 128 || sh < 128;
const isSmall = sw < 180 || sh < 180;
const isNarrow = sw < 200;
const isVertical = sh > sw;
const font = "MatrixChunky8";  // Universal
```

---

## ASCII Icons

All weather icons use basic ASCII for MatrixChunky8 compatibility:

| Weather | Icon | Description |
|---------|------|-------------|
| Clear | `*` | Star/sun |
| Partly Cloudy | `~` | Wavy clouds |
| Overcast | `=` | Heavy clouds |
| Light Rain | `'` | Drizzle |
| Rain | `\|` | Rain drops |
| Heavy Rain | `#` | Downpour |
| Snow | `o` | Snowflakes |
| Heavy Snow | `O` | Big flakes |
| Storm | `!` | Alert |

---

## Layout Architecture: Continuous Scroll

Instead of discrete "cards" that jump between views, we use a **single continuous vertical scroll** that loops infinitely. This creates the classic Weather Channel feel.

### Scroll Content (in order, repeating):

```
┌────────────────────────────────────────┐
│ [MUSIC BAR] Waveform + Scrolling Chords│  Fixed at top
├────────────────────────────────────────┤
│ [HEADER] Location + Time               │  Fixed below music
├────────────────────────────────────────┤
│                                        │
│   CURRENT CONDITIONS                   │  ← Scroll content
│   Temperature, Feels Like              │
│   Icon + Condition Text                │
│   Wind, Humidity, UV                   │
│                                        │
│   ─────────────────────                │
│                                        │
│   HOURLY FORECAST                      │
│   Next 6-12 hours in columns           │
│                                        │
│   ─────────────────────                │
│                                        │
│   7-DAY OUTLOOK                        │
│   Day | Icon | Hi/Lo                   │
│   (repeat for 7 days)                  │
│                                        │
│   ─────────────────────                │
│                                        │
│   WEATHER DETAILS                      │
│   Sunrise/Sunset                       │
│   Moon Phase                           │
│   Pressure, Visibility                 │
│   Precipitation %                      │
│                                        │
│   ─────────────────────                │
│                                        │
│   TEXT FORECAST                        │
│   (narrative description)              │
│                                        │
│   ─────────────────────                │
│   [loops back to CURRENT CONDITIONS]   │
│                                        │
├────────────────────────────────────────┤
│ [CRAWL BAR] Scrolling news ticker      │  Fixed at bottom
└────────────────────────────────────────┘
```

---

## Music Visualization: Scrolling Chord Display

The chord display at the top shows a **horizontally scrolling list** of chords, not just prev/current/next. This creates a piano roll / karaoke-style effect.

```
┌──────────────────────────────────────────────────────┐
│ ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁  (waveform visualization)           │
│ Am7  Dm7  [G7]  Cmaj7  Fmaj7  Am7  Dm7  G7  ...   │
│      ←── scroll direction ←──                        │
└──────────────────────────────────────────────────────┘
```

- **Current chord** is highlighted (brighter, maybe box around it)
- **Past chords** fade to the left
- **Upcoming chords** visible to the right
- Smooth horizontal scroll animation
- Progress bar shows time until next chord

---

## Responsive Element Sizing

### Music Bar (fixed at top)
| Breakpoint | Height | Waveform H | Chord Text |
|------------|--------|------------|------------|
| tiny | 12px | 6px | System font, compact |
| small | 16px | 8px | unifont |
| medium | 20px | 10px | unifont |
| large | 28px | 14px | MatrixChunky8 |

### Header Bar
| Breakpoint | Height | Location Text | Time Text |
|------------|--------|---------------|-----------|
| tiny | 8px | Truncate to 6 chars | HH:MM |
| small | 12px | Truncate to 10 chars | HH:MM AM |
| medium | 18px | Full city name | HH:MM AM/PM |
| large | 26px | City, Region | HH:MM:SS AM/PM TZ |

### Crawl Bar (fixed at bottom)
| Breakpoint | Height | Speed |
|------------|--------|-------|
| tiny | 8px | Fast |
| small | 12px | Medium |
| medium | 16px | Medium |
| large | 22px | Slow |

### Content Area (scrollable)
| Breakpoint | Margins | Line Height | Section Gap |
|------------|---------|-------------|-------------|
| tiny | 1px | 8px | 4px |
| small | 2px | 10px | 8px |
| medium | 6px | 14px | 12px |
| large | 12px | 18px | 20px |

---

## Animation System

### Scroll Animation
```javascript
let scrollY = 0;
const SCROLL_SPEED = 0.3;  // pixels per frame
const contentHeight = calculateContentHeight(); // varies by breakpoint

function sim() {
  scrollY += SCROLL_SPEED;
  if (scrollY >= contentHeight) scrollY = 0;
}
```

### Chord Scroll Animation
```javascript
let chordScrollX = 0;
const CHORD_SCROLL_SPEED = 0.5; // when transitioning
const CHORD_SNAP_TARGET = null; // null = scrolling, number = snapping to position

function simChords() {
  if (newChordPlayed) {
    // Smooth scroll to center new chord
    CHORD_SNAP_TARGET = calculateChordCenterX(chordIndex);
  }
  if (CHORD_SNAP_TARGET !== null) {
    chordScrollX = lerp(chordScrollX, CHORD_SNAP_TARGET, 0.1);
    if (Math.abs(chordScrollX - CHORD_SNAP_TARGET) < 0.5) {
      CHORD_SNAP_TARGET = null;
    }
  }
}
```

### Value Tweening
```javascript
// Animated number display (temperature changes, etc.)
let displayTemp = 0;
let targetTemp = 72;

function simValues() {
  displayTemp = lerp(displayTemp, targetTemp, 0.05);
}

function paint() {
  ink(...).write(Math.round(displayTemp) + "°", ...);
}
```

---

## Color Palettes (by weather mood)

### Clear Day
```javascript
colors = {
  bgTop: [135, 206, 250],     // Light sky blue
  bgBottom: [70, 130, 180],   // Steel blue
  accent: [255, 200, 50],     // Sunny yellow
  text: [255, 255, 255],
}
```

### Night
```javascript
colors = {
  bgTop: [20, 24, 45],        // Deep navy
  bgBottom: [10, 12, 30],     // Near black
  accent: [180, 140, 255],    // Soft purple
  text: [200, 200, 220],
}
```

### Rain
```javascript
colors = {
  bgTop: [70, 80, 100],       // Gray-blue
  bgBottom: [40, 50, 70],     // Darker gray
  accent: [100, 180, 255],    // Rain blue
  text: [220, 230, 240],
}
```

---

## Test Locations (World Tour)

```javascript
const WORLD_LOCATIONS = {
  // Americas
  'nyc': { name: 'New York', coords: [40.7128, -74.0060], tz: 'America/New_York' },
  'la': { name: 'Los Angeles', coords: [34.0522, -118.2437], tz: 'America/Los_Angeles' },
  'miami': { name: 'Miami', coords: [25.7617, -80.1918], tz: 'America/New_York' },
  'denver': { name: 'Denver', coords: [39.7392, -104.9903], tz: 'America/Denver' },
  'cdmx': { name: 'Mexico City', coords: [19.4326, -99.1332], tz: 'America/Mexico_City' },
  'saopaulo': { name: 'São Paulo', coords: [-23.5505, -46.6333], tz: 'America/Sao_Paulo' },
  
  // Europe
  'london': { name: 'London', coords: [51.5074, -0.1278], tz: 'Europe/London' },
  'paris': { name: 'Paris', coords: [48.8566, 2.3522], tz: 'Europe/Paris' },
  'berlin': { name: 'Berlin', coords: [52.5200, 13.4050], tz: 'Europe/Berlin' },
  'rome': { name: 'Rome', coords: [41.9028, 12.4964], tz: 'Europe/Rome' },
  'moscow': { name: 'Moscow', coords: [55.7558, 37.6173], tz: 'Europe/Moscow' },
  
  // Asia
  'tokyo': { name: 'Tokyo', coords: [35.6762, 139.6503], tz: 'Asia/Tokyo' },
  'seoul': { name: 'Seoul', coords: [37.5665, 126.9780], tz: 'Asia/Seoul' },
  'beijing': { name: 'Beijing', coords: [39.9042, 116.4074], tz: 'Asia/Shanghai' },
  'mumbai': { name: 'Mumbai', coords: [19.0760, 72.8777], tz: 'Asia/Kolkata' },
  'dubai': { name: 'Dubai', coords: [25.2048, 55.2708], tz: 'Asia/Dubai' },
  'singapore': { name: 'Singapore', coords: [1.3521, 103.8198], tz: 'Asia/Singapore' },
  
  // Oceania
  'sydney': { name: 'Sydney', coords: [-33.8688, 151.2093], tz: 'Australia/Sydney' },
  'auckland': { name: 'Auckland', coords: [-36.8509, 174.7645], tz: 'Pacific/Auckland' },
  
  // Africa
  'cairo': { name: 'Cairo', coords: [30.0444, 31.2357], tz: 'Africa/Cairo' },
  'lagos': { name: 'Lagos', coords: [6.5244, 3.3792], tz: 'Africa/Lagos' },
  'capetown': { name: 'Cape Town', coords: [-33.9249, 18.4241], tz: 'Africa/Johannesburg' },
  
  // Extreme weather locations (for testing)
  'reykjavik': { name: 'Reykjavik', coords: [64.1466, -21.9426], tz: 'Atlantic/Reykjavik' }, // Cold
  'phoenix': { name: 'Phoenix', coords: [33.4484, -112.0740], tz: 'America/Phoenix' }, // Hot
  'seattle': { name: 'Seattle', coords: [47.6062, -122.3321], tz: 'America/Los_Angeles' }, // Rainy
  'mumbai-monsoon': { name: 'Mumbai', coords: [19.0760, 72.8777], tz: 'Asia/Kolkata' }, // Monsoon
};
```

---

## Artery Test Structure

```
test-weather.mjs
├── main()
│   ├── Parse args (location, --audio, --duration, --world-tour)
│   └── Run selected test
├── testSingleLocation(location)
│   ├── Connect with audio context
│   ├── Jump to weather piece
│   ├── Verify music playing
│   └── Display for duration
├── testWorldTour(duration)
│   ├── Cycle through all WORLD_LOCATIONS
│   ├── 30 seconds each
│   └── Report weather at each location
└── testBreakpoints()
    ├── Resize window to each breakpoint
    └── Verify layout adapts correctly
```

---

## Implementation Priorities

1. **Scrolling chord display** - Replace jumping prev/current/next with smooth scroll
2. **Continuous vertical scroll** - Replace card views with single scrolling page
3. **Fix text overlap** - Recalculate all spacing based on actual font metrics
4. **Audio context in tests** - Add --audio flag like test-notepat
5. **World tour test** - Cycle through locations automatically
6. **Value animations** - Tween temperature and other values smoothly
