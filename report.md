# In-Progress Report

---

## 📻 KPBJ Radio Piece Implementation (2026-02-01)

### Overview

Create a new `kpbj.mjs` piece similar to `r8dio.mjs` for [KPBJ.FM](https://www.kpbj.fm/) radio, and extract shared functionality into a reusable `radio.mjs` lib module.

### Stream Details

| Property | Value |
|----------|-------|
| **Station** | KPBJ.FM - Sun Valley Arts and Culture (501(c)(3) non-profit) |
| **Stream URL** | `https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3` |
| **Format** | Audio/MPEG, 192kbps, 44.1kHz stereo |
| **Metadata** | Available via ICY headers (`icy-name: KPBJ test station`) |

### Implementation Plan

#### 1. Create `lib/radio.mjs` - Shared Radio Module

Extract reusable functionality from `r8dio.mjs`:

| Component | Description |
|-----------|-------------|
| **State Management** | `createRadioState()` - isPlaying, isLoading, volume, frequencyData, etc. |
| **Playback Controls** | `togglePlayback()`, `startPlayback()`, `pausePlayback()`, `stopPlayback()` |
| **Volume Control** | `updateVolume()`, volume slider hit detection |
| **Visualizer Bars** | `initBars()`, `updateBars()` - animated frequency/waveform display |
| **QR Code Generation** | `generateQRCode()` - using `@akamfoad/qr` |
| **Message Handling** | `handleStreamMessage()` - process BIOS stream events |
| **UI Rendering** | `drawVisualizerBars()`, `drawPlayButton()`, `drawVolumeSlider()`, `drawQRCode()`, `drawStatusText()` |

#### 2. Create `disks/kpbj.mjs` - KPBJ Piece

UI Layout:
```
┌─────────────────────────────────────┐
│                          ┌─────┐   │
│    K P B J               │ QR  │   │
│    Sun Valley Radio      │CODE │   │
│                          └─────┘   │
│  ┌─────────────────────────────┐   │
│  │ ▄ ▄▄█▄▄▄ ▄ █▄▄█▄█▄▄ ▄▄█    │   │
│  │ █ ███████ ██████████ ███    │   │  ← Visualizer
│  │ █████████████████████████   │   │
│  └─────────────────────────────┘   │
│                                     │
│           ┌──────────┐             │
│           │  ▶ PLAY  │             │  ← Play/Pause Button
│           └──────────┘             │
│                                     │
│      ─────────●───────── 50%       │  ← Volume Slider
│                                     │
│            ● LIVE                   │
│     current track info here         │
└─────────────────────────────────────┘
```

#### 3. Theme Configuration

| Property | r8dio (existing) | kpbj (new) |
|----------|------------------|------------|
| **Background** | Purple tint `(25, 20, 35)` | Mountain blue `(20, 30, 45)` |
| **Primary** | Pink/Magenta | Earthy orange/amber |
| **Accent** | Purple gradients | Blue-green gradients |
| **QR URL** | `https://prompt.ac/r8dio` | `https://prompt.ac/kpbj` |
| **Title Font** | unifont with color codes | unifont with color codes |
| **Subtitle** | "Danmarks snakke-radio" | "Sun Valley Community Radio" |

#### 4. File Structure

```
system/public/aesthetic.computer/
├── lib/
│   └── radio.mjs          ← NEW: Shared radio utilities
├── disks/
│   ├── r8dio.mjs          ← UPDATE: Refactor to use radio.mjs
│   └── kpbj.mjs           ← NEW: KPBJ radio piece
```

### Theme Details for KPBJ

Inspired by Sun Valley, Idaho (mountain/nature aesthetic):

```javascript
const theme = {
  // Background - deep mountain blue
  bg: [20, 30, 45],
  
  // Visualizer gradient - sunrise over mountains
  barColors: (t) => ({
    r: Math.floor(200 + t * 55),   // Orange to yellow
    g: Math.floor(100 + t * 100),  
    b: Math.floor(60 + t * 80),    
  }),
  
  // UI elements - earthy warm tones
  primary: [230, 160, 80],      // Amber
  secondary: [180, 130, 90],    // Tan
  accent: [100, 180, 160],      // Sage green
  
  // Text
  title: [255, 200, 140],       // Warm white
  subtitle: [160, 140, 120],    // Muted tan
  
  // Button
  buttonBg: [60, 50, 40],
  buttonHover: [80, 70, 55],
  buttonOutline: [120, 100, 80],
  
  // QR
  qrFg: [220, 180, 140],
  qrBg: [35, 45, 60],
};
```

### Metadata

KPBJ uses AzuraCast. Potential metadata endpoint:
- AzuraCast API: `https://kpbj.hasnoskills.com/api/nowplaying/kpbj_test_station`
- ICY metadata from stream headers (already working: `icy-name`)

### QR Code

- **URL**: `https://prompt.ac/kpbj`
- **Position**: Top-right corner
- **Library**: Existing `@akamfoad/qr` dependency
- **Label**: "listen" below QR code

### Estimated Effort

| Task | Time |
|------|------|
| Create `lib/radio.mjs` | ~30 min |
| Create `disks/kpbj.mjs` | ~20 min |
| Refactor `disks/r8dio.mjs` | ~15 min |
| Testing both pieces | ~10 min |
| **Total** | **~75 min** |

### Questions/Decisions

1. **Metadata API**: Should we try to fetch current track from AzuraCast API, or rely on ICY metadata?
2. **Color scheme**: The suggested mountain/sunrise theme—want adjustments?
3. **Additional features**: Should KPBJ show schedule info or link to shows page?

### Ready to Implement

Once approved, I'll:
1. Create `lib/radio.mjs` with shared utilities
2. Create `disks/kpbj.mjs` with the mountain theme
3. Refactor `disks/r8dio.mjs` to use the shared lib
4. Test both pieces work correctly

---

## 🤖 Android App Distribution (2026-01-31)

### Status
- **Play Store Account**: Created with `me@jas.life`, identity verification pending
- **GitHub Release**: Published at [android-v1.1.0](https://github.com/whistlegraph/aesthetic-computer/releases/tag/android-v1.1.0)
- **Sideload APK**: `aesthetic-computer-v1.1.0-debug.apk` (~10MB)

### Completed
- [x] Consumer Android app working on Uniherz Jelly (tested via WiFi debugging)
- [x] APK uploaded to GitHub releases
- [x] `/mobile` piece updated with Android sideload option

### Pending
- [ ] Google Play identity verification (email: me@jas.life)
- [ ] Generate signing keystore for production release
- [ ] Build signed release AAB for Play Store submission
- [ ] Create app listing (screenshots, description, etc.)

### Build Flavors
- `consumer`: Points to https://aesthetic.computer (Play Store version)
- `kiosk`: Points to localhost:8443 (device installations)

---

# Aesthetic News — Architecture Notes (2026-01-18)

## Summary of request
- Keep the visual design unchanged.
- Remove visited color on “Report the News” link (done).
- Consider shifting news.aesthetic.computer to a single-page app with proper routing, similar to how sotce-net works.
- Check for any KidLisp-related parts in the news app.

## Findings
### sotce-net architecture
- sotce-net is a **single Netlify function** with an internal router: [system/netlify/functions/sotce-net.mjs](system/netlify/functions/sotce-net.mjs).
- It inspects `event.path` and routes within the handler. This keeps the subdomain effectively “single-function” and centralized for auth/session logic.

### news.aesthetic.computer current architecture
- News is **server-rendered** by [system/netlify/functions/news.mjs](system/netlify/functions/news.mjs) with internal routing logic (e.g., `/`, `/new`, `/comments`, `/item`, `/report`), and assets in [system/public/news.aesthetic.computer](system/public/news.aesthetic.computer).
- There’s a **separate function** for guidelines: [system/netlify/functions/news-guidelines.mjs](system/netlify/functions/news-guidelines.mjs), plus redirects in [system/netlify.toml](system/netlify.toml).
- Netlify redirects already point the subdomain and `/news.aesthetic.computer/*` paths to the `news` function, with static assets served from `system/public/news.aesthetic.computer/`.

### KidLisp presence in News
- Only a CSS comment reference exists: the main page background variable references “kidlisp.com” as a color inspiration in [system/public/news.aesthetic.computer/main.css](system/public/news.aesthetic.computer/main.css).
- No functional KidLisp code paths were found inside the news front-end or news Netlify functions.

## Recommended direction (SPA without design change)
Two viable ways to match sotce-net’s “single function” feel while keeping visuals intact:

### Option A — Single Netlify function entry (minimal change, still SSR)
- Fold `news-guidelines.mjs` into `news.mjs` and route `/guidelines` internally.
- Update `netlify.toml` to point all News routes (including `/guidelines`) to `news.mjs`.
- Result: **single function** for all News pages and auth logic, still server-rendered, zero design change.

### Option B — True SPA shell + client router (larger change)
- Serve a single HTML shell from `news.mjs` for **all** page routes.
- Move route rendering into a client-side router (history API), calling the existing `/api/news` endpoints.
- Keep the same markup/CSS to preserve visuals; simply render via JS instead of server HTML.
- Result: **SPA behavior** with consistent auth state and simplified routing, but higher implementation cost.

## Suggested next step
If you want the fastest flip with minimal risk to design, start with **Option A** (single-function consolidation). If the goal is full SPA behavior, proceed with Option B and migrate the existing SSR render functions into client-side templates without altering styles.

