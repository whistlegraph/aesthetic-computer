# In-Progress Report

---

## 📖 Sotce Canvas2D Garden Rewrite Plan (2026-02-04)

### Why Canvas?
The current DOM-based virtualized scroll has inherent problems:
- Browser scroll events fire unpredictably during smooth animations
- DOM mutations (adding/removing wrappers) can cause layout thrashing
- `scrollIntoView` fights with scroll-snap and manual scroll updates
- Synchronizing `currentPageIndex` with actual scroll position is fragile

A Canvas 2D approach gives us **full control** over rendering and physics.

---

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│  #garden (container div)                                    │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  <canvas id="garden-canvas">                          │  │
│  │    - Renders all visible page cards                   │  │
│  │    - Custom scroll physics (momentum, snap)           │  │
│  │    - Hit detection for ears, page numbers, links      │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  Offscreen text cache (optional)                      │  │
│  │    - Pre-rendered page text as ImageBitmap            │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

---

### Core Components

#### 1. **GardenCanvas Class**
```javascript
class GardenCanvas {
  constructor(container, pages, options) {
    this.canvas = document.createElement("canvas");
    this.ctx = this.canvas.getContext("2d");
    this.pages = pages;           // Array of page data
    this.scrollY = 0;             // Current scroll offset (pixels)
    this.velocity = 0;            // For momentum scrolling
    this.targetPage = null;       // For snap animation
    this.pageHeight = 0;          // Computed from aspect ratio
    this.visibleRange = [0, 0];   // [startIdx, endIdx] for culling
  }
}
```

#### 2. **Layout Calculation**
```javascript
computeLayout() {
  const { width, height } = this.canvas;
  // Page cards are 4:5 aspect ratio, centered horizontally
  const cardWidth = Math.min(width - 32, 600); // max 600px, 16px padding
  const cardHeight = cardWidth * (5/4);
  const gap = 24; // space between pages
  
  this.pageHeight = cardHeight + gap;
  this.totalHeight = this.pages.length * this.pageHeight;
  this.cardRect = { width: cardWidth, height: cardHeight, x: (width - cardWidth) / 2 };
}
```

#### 3. **Render Loop**
```javascript
render() {
  const { ctx, canvas, scrollY, pageHeight, pages } = this;
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  
  // Determine visible range (cull off-screen pages)
  const startIdx = Math.max(0, Math.floor(scrollY / pageHeight) - 1);
  const endIdx = Math.min(pages.length - 1, Math.ceil((scrollY + canvas.height) / pageHeight) + 1);
  
  for (let i = startIdx; i <= endIdx; i++) {
    const y = i * pageHeight - scrollY;
    this.renderPage(pages[i], i, y);
  }
  
  requestAnimationFrame(() => this.render());
}

renderPage(page, index, y) {
  const { ctx, cardRect } = this;
  const { x, width, height } = cardRect;
  
  // Background
  ctx.fillStyle = "#f5f0e8";
  ctx.fillRect(x, y, width, height);
  
  // Border
  ctx.strokeStyle = "#ccc";
  ctx.strokeRect(x, y, width, height);
  
  // Title (date)
  ctx.fillStyle = "#333";
  ctx.font = "16px serif";
  ctx.fillText(page.title, x + 20, y + 40);
  
  // Body text (wrapped)
  this.renderWrappedText(page.words, x + 20, y + 70, width - 40);
  
  // Page number
  ctx.fillStyle = "#888";
  ctx.font = "14px monospace";
  ctx.textAlign = "center";
  ctx.fillText(`- ${index + 1} -`, x + width/2, y + height - 20);
  ctx.textAlign = "left";
  
  // Ear (corner triangle)
  this.renderEar(x + width - 30, y + height - 30, 30);
}
```

#### 4. **Physics & Scrolling**
```javascript
update(dt) {
  // Apply velocity (momentum)
  if (Math.abs(this.velocity) > 0.1) {
    this.scrollY += this.velocity * dt;
    this.velocity *= 0.95; // friction
  }
  
  // Snap to nearest page when velocity is low
  if (this.targetPage !== null) {
    const targetY = this.targetPage * this.pageHeight;
    const diff = targetY - this.scrollY;
    this.scrollY += diff * 0.15; // ease toward target
    if (Math.abs(diff) < 1) {
      this.scrollY = targetY;
      this.targetPage = null;
    }
  }
  
  // Clamp scroll bounds
  this.scrollY = Math.max(0, Math.min(this.scrollY, this.totalHeight - this.canvas.height));
}

snapToNearestPage() {
  const currentPage = Math.round(this.scrollY / this.pageHeight);
  this.targetPage = Math.max(0, Math.min(currentPage, this.pages.length - 1));
}
```

#### 5. **Input Handling**
```javascript
setupInput() {
  let isDragging = false;
  let dragStartY = 0;
  let dragStartScroll = 0;
  
  this.canvas.addEventListener("pointerdown", (e) => {
    isDragging = true;
    dragStartY = e.clientY;
    dragStartScroll = this.scrollY;
    this.velocity = 0;
    this.targetPage = null;
  });
  
  this.canvas.addEventListener("pointermove", (e) => {
    if (!isDragging) return;
    const deltaY = dragStartY - e.clientY;
    this.scrollY = dragStartScroll + deltaY;
    // Track velocity for momentum
    this.velocity = deltaY / 16; // rough estimate
  });
  
  this.canvas.addEventListener("pointerup", (e) => {
    isDragging = false;
    this.snapToNearestPage();
  });
}
```

#### 6. **Hit Detection**
```javascript
getElementAtPoint(x, y) {
  const scrolledY = y + this.scrollY;
  const pageIdx = Math.floor(scrolledY / this.pageHeight);
  const page = this.pages[pageIdx];
  if (!page) return null;
  
  const pageY = pageIdx * this.pageHeight;
  const localY = scrolledY - pageY;
  const { cardRect } = this;
  
  // Check if in ear region (bottom-right corner)
  if (x > cardRect.x + cardRect.width - 40 && localY > cardRect.height - 40) {
    return { type: "ear", pageIdx, page };
  }
  
  // Check if in page number region
  if (localY > cardRect.height - 50 && localY < cardRect.height - 10) {
    return { type: "pageNumber", pageIdx, page };
  }
  
  return { type: "page", pageIdx, page };
}
```

---

### Data Flow

1. **Initial Load**: Fetch first batch of pages, initialize canvas
2. **Scroll/Drag**: Update `scrollY`, re-render visible pages
3. **Prefetch**: When approaching edges of loaded data, fetch more pages
4. **Cache**: Keep page data in memory Map (same as current `pageCache`)

---

### Migration Steps

| Step | Description | Effort |
|------|-------------|--------|
| 1 | Create `GardenCanvas` class with basic rendering | 30 min |
| 2 | Implement drag physics and snap | 20 min |
| 3 | Add text wrapping and proper typography | 30 min |
| 4 | Implement ear/backpage flip interaction | 30 min |
| 5 | Add hit detection for page numbers (open chat) | 15 min |
| 6 | Prefetch/cache integration | 20 min |
| 7 | Polish: loading states, transitions | 30 min |
| **Total** | | **~3 hours** |

---

### Pros
- **No DOM/scroll sync issues** — we control everything
- **Smooth 60fps** — requestAnimationFrame, no reflows
- **Predictable physics** — custom momentum and snap
- **Simpler mental model** — scrollY is just a number

### Cons
- **Text rendering** — Canvas text is less crisp than DOM (can mitigate with high DPI)
- **Accessibility** — Need to manually expose content to screen readers
- **Selection** — Can't select/copy text (could add overlay for that)
- **Ear flip animation** — More complex to animate in canvas (but doable)

---

### Decision
Ready to implement? This would replace the current DOM-based garden rendering with a single canvas element and custom scroll physics.

---

## 📖 Sotce Layout Engine Study (2026-02-04) [ARCHIVED]

### Scope
Review the current virtualized page system and drag/scroll flow in [system/netlify/functions/sotce-net.mjs](system/netlify/functions/sotce-net.mjs) to explain “double switches,” page advances, and visible text swaps/flashes.

### Current Layout Engine (Summary)
- **Virtualization:** `updateVisiblePages(centerIdx, skipScroll)` keeps 5 pages in DOM (center ±2).
- **Rendering:** `renderPageContent()` inserts a loading placeholder, then swaps in page data when fetched.
- **Scroll Tracking:** `scroll` listener finds the centered page among `renderedPages` and calls `updateVisiblePages()`.
- **Drag Release:** `scrollIntoView({ behavior: "smooth" })` animates to target page, then calls `updateVisiblePages()` after a timeout.

### Observed Symptoms
- **“Double switches” / extra advances** during smooth scroll animations.
- **Page text flashes / swaps** while scrolling or after a drag release.
- **Current page index drifting** from actual scroll position (detected via `actualPage` logging).

### Likely Root Causes
1. **Programmatic smooth scroll triggers the scroll listener**
  - During `scrollIntoView`, the `scroll` event fires repeatedly.
  - The scroll listener computes `closestPage` based on transient positions and calls `updateVisiblePages()` mid-animation.
  - This can result in **multiple re-centers** and **double page updates** during one animation.

2. **Virtualization updates during animation**
  - `updateVisiblePages()` adds/removes wrappers and updates content while the viewport is still moving.
  - The placeholder + async render path causes **visible text swaps** if a visible wrapper is re-created.

3. **Index vs. position mismatch**
  - `currentPageIndex` is updated optimistically before the smooth scroll completes.
  - If the scroll listener re-computes `closestPage` during the animation, it can diverge from the intended target.

### Evidence
- Logs show mismatches like `currentPageIndex: 238` while `actualPage: 236` at `scrollTop: 0`.
- The scroll listener and animation callback both call `updateVisiblePages()` within ~400ms.
- Placeholder insertion is visible when pages are re-added during motion.

### Recommendations
1. **Add an “isAnimating” guard**
  - Set a flag during `scrollIntoView` animation.
  - Skip the scroll listener’s `updateVisiblePages()` while `isAnimating` is true.
  - Clear the flag on `scrollend` (if supported) or after the animation timeout.

2. **Only update virtualization after animation settles**
  - Move all `updateVisiblePages()` calls for programmatic scroll into the animation completion path.
  - Ensure the scroll listener *only* handles user-driven scroll.

3. **Avoid visible placeholders**
  - Keep the previous content until new content is ready (no placeholder swap while visible).
  - Prefetch more aggressively to reduce placeholder exposure.

### Next Action (If Approved)
- Add `isAnimating` gating and unify all programmatic scroll updates into a single post-animation update.
- Adjust `renderPageContent()` to avoid placeholder replacement when a wrapper is visible.

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

