# Android App Store Screenshots Feature

## Overview
Add capability to generate screenshots for the Android (Google Play) app store listing from the `prompt.mjs` piece using the Oven capture service.

## Requirements from Google Play

### Phone Screenshots (Required: 2-8)
- Format: PNG or JPEG
- Max size: 8 MB each
- Aspect ratio: 16:9 or 9:16
- Dimensions: Each side between 320px and 3,840px
- **For promotion eligibility**: At least 4 screenshots, with at least 3 in 16:9 or 9:16 and at least 1080px

### 7-inch Tablet Screenshots (Required: 2-8)
- Format: PNG or JPEG
- Max size: 8 MB each
- Aspect ratio: 16:9 or 9:16
- Dimensions: Each side between 320px and 3,840px

### 10-inch Tablet Screenshots (Required: 2-8)
- Format: PNG or JPEG
- Max size: 8 MB each
- Aspect ratio: 16:9 or 9:16
- Dimensions: Each side between 1,080px and 7,680px

## Proposed Target Resolutions

### Phone (Portrait 9:16)
- `1080x1920` - Standard phone (1080p portrait, meets promotion minimum)

### Phone (Landscape 16:9)
- `1920x1080` - Standard phone landscape (1080p)

### 7-inch Tablet (Portrait 9:16)
- `1200x1920` - 7" tablet portrait

### 7-inch Tablet (Landscape 16:9)
- `1920x1200` - 7" tablet landscape

### 10-inch Tablet (Portrait 9:16)
- `1600x2560` - 10" tablet portrait (WQXGA)

### 10-inch Tablet (Landscape 16:9)
- `2560x1600` - 10" tablet landscape (WQXGA)

## Implementation Plan

### 1. New Oven Endpoint: `/app-screenshots/:piece`

Add a dedicated endpoint that generates all required screenshots for app store submission.

```javascript
// GET /app-screenshots/:piece
// Returns: HTML page with all screenshots + download links
// Also: /app-screenshots/:piece/:preset/:format
//   preset: phone-portrait, phone-landscape, tablet7-portrait, tablet7-landscape, tablet10-portrait, tablet10-landscape
//   format: png (default), jpeg
```

### 2. Screenshot Presets Configuration

```javascript
const APP_SCREENSHOT_PRESETS = {
  // Phone screenshots (9:16 portrait, meets 1080px promotion requirement)
  'phone-portrait': { width: 1080, height: 1920, density: 1 },
  'phone-landscape': { width: 1920, height: 1080, density: 1 },
  
  // 7-inch tablet (9:16 portrait)
  'tablet7-portrait': { width: 1200, height: 1920, density: 1 },
  'tablet7-landscape': { width: 1920, height: 1200, density: 1 },
  
  // 10-inch tablet (9:16 portrait, higher res for larger screen)
  'tablet10-portrait': { width: 1600, height: 2560, density: 1 },
  'tablet10-landscape': { width: 2560, height: 1600, density: 1 },
};
```

### 3. Dashboard Page: `/app-screenshots`

A dedicated HTML page at `/app-screenshots` that:
- Shows all generated screenshots in a grid
- Provides download buttons for each screenshot
- Shows zip download for all screenshots
- Displays Google Play compliance status (checkmarks for valid dimensions)
- Allows piece selection (defaults to `prompt`)
- Allows regeneration with force refresh

### 4. Modifications to `server.mjs`

Add new routes:
```javascript
// Dashboard page
app.get('/app-screenshots', (req, res) => {
  // Serve HTML dashboard
});

// Individual screenshot endpoint
app.get('/app-screenshots/:preset/:piece.png', async (req, res) => {
  // Generate/cache screenshot with specified preset
});

// Bulk download endpoint
app.get('/app-screenshots/download/:piece', async (req, res) => {
  // Return zip file with all screenshots
});

// JSON status/listing
app.get('/api/app-screenshots/:piece', async (req, res) => {
  // Return JSON with all screenshot URLs and status
});
```

### 5. Modifications to `grabber.mjs`

- Update `captureFrame()` to support larger viewport sizes (up to 2560x1600 for 10" tablet)
- Add new helper for generating screenshots at specific app store presets
- Ensure proper caching with preset-specific cache keys

### 6. Piece-Specific Considerations for `prompt.mjs`

The `prompt.mjs` piece is the main entry point of the app. For screenshots:
- May want to show it in different states (empty prompt, with text, after command)
- Consider adding URL params to control prompt state for varied screenshots
- Example: `/prompt?screenshot-state=welcome` or `/prompt?screenshot-state=typing`

### 7. Caching Strategy

Cache key format:
```
app-screenshots/{piece}-{preset}-{gitVersion}.png
```

- Cache TTL: 7 days (longer since app store submissions are infrequent)
- Force regeneration available via `?force=true` query param

## File Changes Required

1. **`/oven/server.mjs`**
   - Add `/app-screenshots` dashboard route
   - Add `/app-screenshots/:preset/:piece.png` capture route
   - Add `/api/app-screenshots/:piece` JSON status route
   - Add `/app-screenshots/download/:piece` bulk download route

2. **`/oven/grabber.mjs`**
   - Ensure viewport can handle 2560x1600 (may need to increase max limits)
   - Add `APP_SCREENSHOT_PRESETS` constant export

## Dashboard UI Mockup

```
┌─────────────────────────────────────────────────────────────┐
│ 📱 App Store Screenshots                                    │
│ Piece: [prompt     ▼]  [🔄 Regenerate All]  [📦 Download ZIP]│
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  PHONE SCREENSHOTS                                           │
│  ┌──────────┐ ┌──────────────────┐                          │
│  │          │ │                  │                          │
│  │ Portrait │ │    Landscape     │                          │
│  │ 1080x1920│ │    1920x1080     │                          │
│  │   ✅     │ │       ✅         │                          │
│  └──────────┘ └──────────────────┘                          │
│   [Download]   [Download]                                    │
│                                                              │
│  7-INCH TABLET SCREENSHOTS                                   │
│  ┌──────────┐ ┌──────────────────┐                          │
│  │          │ │                  │                          │
│  │ Portrait │ │    Landscape     │                          │
│  │ 1200x1920│ │    1920x1200     │                          │
│  │   ✅     │ │       ✅         │                          │
│  └──────────┘ └──────────────────┘                          │
│   [Download]   [Download]                                    │
│                                                              │
│  10-INCH TABLET SCREENSHOTS                                  │
│  ┌──────────┐ ┌──────────────────┐                          │
│  │          │ │                  │                          │
│  │ Portrait │ │    Landscape     │                          │
│  │ 1600x2560│ │    2560x1600     │                          │
│  │   ✅     │ │       ✅         │                          │
│  └──────────┘ └──────────────────┘                          │
│   [Download]   [Download]                                    │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Implementation Steps

1. [x] Add `APP_SCREENSHOT_PRESETS` constant to `grabber.mjs`
2. [x] Update viewport limits in puppeteer config if needed
3. [x] Add `/app-screenshots` dashboard HTML route to `server.mjs`
4. [x] Add `/app-screenshots/:preset/:piece.png` capture route
5. [x] Add `/api/app-screenshots/:piece` JSON API route
6. [x] Add bulk ZIP download functionality
7. [ ] Test with `prompt` piece at all resolutions
8. [ ] Deploy to oven.aesthetic.computer
9. [ ] Generate screenshots and verify Google Play compliance

## URLs After Implementation

- **Dashboard**: https://oven.aesthetic.computer/app-screenshots
- **Individual Screenshots**: https://oven.aesthetic.computer/app-screenshots/phone-portrait/prompt.png
- **JSON API**: https://oven.aesthetic.computer/api/app-screenshots/prompt
- **Bulk Download**: https://oven.aesthetic.computer/app-screenshots/download/prompt

## Notes

- The existing `/grab/:format/:width/:height/:piece` endpoint can handle these sizes, but the dedicated `/app-screenshots` endpoint provides:
  - Preset configurations matching Google Play requirements
  - Dashboard for easy preview and download
  - Compliance verification
  - Bulk download capability
  
- Consider capturing multiple pieces beyond just `prompt` for app variety:
  - `painting` or `nopaint` - drawing mode
  - `tone` or `song` - music creation
  - `wand` - generative art
  - `$roz` or other KidLisp pieces - code examples
