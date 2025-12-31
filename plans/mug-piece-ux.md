# Mug Piece UX Plan

## Status: ✅ Core Complete

### Completed Features
- ✅ `mug.mjs` piece with animated WebP preview
- ✅ Unifont UI with colored title ("BLUE MUG of #code")
- ✅ Pre-fetched Stripe checkout URL (instant buy button)
- ✅ Button states: grayed (loading), normal, hover, pulsing (pending)
- ✅ Back/cancel from Stripe returns to `mug~#CODE~color`
- ✅ Guest painting upload: `mug` or `mug yellow` uploads current painting
- ✅ Branded Stripe product name: "RED MUG of #abc · 11oz Ceramic"
- ✅ WebP color decoding fix (RGBA format)
- ✅ URL `#` preservation via `§HASH§` placeholder

### KidLisp.com Integration (2025.01)
- ✅ Added ☕ mug button to console screenshot entries
- ✅ Added ☕ Mug button to screenshot popup modal
- ✅ `openMugFromScreenshot()` uploads to guest bucket → opens mug piece

---

## Goal
When user enters `mug` command, jump to a `mug.mjs` piece that:
1. Shows the animated WebP preview of the mug
2. Displays purchase button with price
3. Handles the Stripe checkout flow

**ALSO:** ✅ Add ☕ mug emoji button to KidLisp.com console screenshots for instant mug ordering.

---

## AC Piece Architecture Reference

Based on analysis of existing pieces (`keep.mjs`, `camera.mjs`, `amp.mjs`, `painting.mjs`, `profile.mjs`):

### Standard Piece Structure
```javascript
// piece-name, YYYY.MM.DD
// Description

// Module-level state only - no cached API references needed!
let btn;
let loading = true;
let data = null;

// 🥾 Boot - Initialize state, parse params, start async operations
function boot({ params, colon, store, ui, screen, cursor, hud, system }) {
  cursor("native");
  hud.labelBack(); // Show back arrow in header
  
  // Parse params: piece param0 param1
  const code = params[0];
  const option = params[1];
  
  // Store references: store["key"] for persistence
  // System state: system.painting, system.nopaint, etc.
  
  // Create UI: new ui.TextButton("Label", { center: "xy", screen })
  btn = new ui.TextButton("Action", { center: "xy", screen });
  
  // Start async work
  fetchData();
}

// 🎨 Paint - Render each frame (receives full API)
function paint({ wipe, ink, paste, screen, pen, num }) {
  wipe(30); // Background color
  
  // Draw bitmap: paste(bitmap, x, y, { scale: 2 })
  // Draw text: ink(255).write("Text", { center: "xy", screen })
  // Draw button: btn?.paint({ ink, paste, pen });
}

// 🎪 Act - Handle events (receives full API including jump, needsPaint)
function act({ event: e, screen, jump, needsPaint }) {
  // Reposition on resize
  if (e.is("reframed")) {
    btn?.reposition({ center: "xy", screen });
  }
  
  // Button interaction
  const result = btn?.act(e);
  if (result?.down) {
    doSomething(jump, needsPaint);
  }
}

// 🧮 Sim - Logic tick (optional)
function sim() {
  // Animation state updates
}

// 🚪 Leave - Cleanup (optional)
function leave() {
  // Cancel pending requests, cleanup
}

export { boot, paint, act, sim, leave };
```

### Key APIs for mug.mjs

**Animated WebP Loading (from keep.mjs):**
```javascript
// In boot:
_preloadAnimatedWebp = net.preloadAnimatedWebp;

// Usage:
const result = await _preloadAnimatedWebp(url);
// Returns: { frameCount, width, height, loopCount, frames: [{ pixels, delay }] }

if (result.frameCount > 1) {
  previewFrames = {
    width: result.width,
    height: result.height,
    frames: result.frames,
  };
  // Set first frame
  previewBitmap = {
    width: result.width,
    height: result.height,
    pixels: result.frames[0].pixels,
  };
}
```

**Frame Animation in paint():**
```javascript
if (previewFrames) {
  const now = performance.now();
  const delay = previewFrames.frames[frameIndex]?.delay || 800;
  if (now - lastFrameTime > delay) {
    frameIndex = (frameIndex + 1) % previewFrames.frames.length;
    previewBitmap.pixels = previewFrames.frames[frameIndex].pixels;
    lastFrameTime = now;
  }
}

// Draw centered and scaled
const scale = Math.min(
  (screen.width * 0.8) / previewBitmap.width,
  (screen.height * 0.6) / previewBitmap.height
);
paste(previewBitmap, x, y, { scale });
```

**TextButton (from amp.mjs, prompt.mjs):**
```javascript
// Create
btn = new ui.TextButton("☕ BUY $18", { center: "xy", screen });

// Paint
btn.paint({ ink, paste, pen }, 
  ["green", "yellow", "yellow", "green"],  // up colors
  ["yellow", "green", "green", "yellow"]   // down colors
);

// Act
const result = btn.act(e);
if (result?.down) { /* clicked */ }

// Reposition on resize
if (e.is("reframed")) {
  btn.reposition({ center: "xy", screen });
}
```

**Authorization (from disk.mjs):**
```javascript
// Get auth token for API calls
const token = await api.authorize();
if (token) headers.Authorization = `Bearer ${token}`;
```

**Jump to URL/piece:**
```javascript
jump("prompt");           // Go to piece
jump("out:https://...");  // External URL (Stripe checkout)
jump(`mug ${code} blue`); // Piece with params
```

---

## Current Flow
```
prompt.mjs (slug === "mug") 
  → disk.mjs mug() function
  → POST /api/mug?new=true&pixels=CODE.png&color=white
  → Backend creates Stripe checkout
  → jump(data.location) → Stripe checkout page
```

**Problem:** User jumps straight to Stripe without seeing the mug preview first.

## Proposed Flow
```
prompt.mjs (slug === "mug")
  → jump("mug CODE color") OR jump("mug") if using system.painting
  
mug.mjs piece
  → Fetch product preview via /api/mug?preview=true
  → Display animated WebP (like keep.mjs thumbnailFrames)
  → Show "Buy $18" button
  → On click: POST /api/mug?new=true → jump to Stripe
```

## Implementation

### 1. Create `mug.mjs` piece
Location: `system/public/aesthetic.computer/disks/mug.mjs`

```javascript
// mug, 24.12.20
// Preview and purchase ceramic mugs with your paintings

// Usage:
//   mug           → Use current painting
//   mug CODE      → Use painting by code
//   mug CODE blue → Use painting with color variant

// Module state only - APIs come fresh in each function
let productCode = null;     // Product code from backend (e.g., "abc123")
let sourceCode = null;      // Painting code (e.g., "hjq")
let color = "white";
let previewUrl = null;      // Animated WebP URL
let previewFrames = null;   // Decoded frames for animation
let previewBitmap = null;   // Current frame bitmap
let frameIndex = 0;
let lastFrameTime = 0;
let loading = true;
let error = null;
let btn = null;
let preloadAnimatedWebp = null; // Cached for async use

function boot({ params, store, net, ui, screen, cursor, system, hud }) {
  cursor("native");
  hud.labelBack(); // Show back arrow
  preloadAnimatedWebp = net.preloadAnimatedWebp; // Cache for async fetchPreview
  
  // Parse params: mug CODE color
  sourceCode = params[0] || store["painting:code"] || system.painting?.code;
  color = params[1] || "white";
  
  if (!sourceCode) {
    error = "No painting selected";
    loading = false;
    return;
  }
  
  btn = new ui.TextButton("☕ BUY $18", { center: "xy", screen });
  
  // Fetch preview (async)
  fetchPreview();
}

async function fetchPreview() {
  try {
    const res = await fetch(`/api/mug?new=true&pixels=${sourceCode}.png&color=${color}&preview=true`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ quantity: 1 }),
    });
    
    const data = await res.json();
    productCode = data.productCode;
    previewUrl = data.animatedWebpUrl;
    
    // Decode animated WebP
    if (previewUrl && preloadAnimatedWebp) {
      const result = await preloadAnimatedWebp(previewUrl);
      if (result.frameCount > 1) {
        previewFrames = {
          width: result.width,
          height: result.height,
          frames: result.frames,
        };
        previewBitmap = {
          width: result.width,
          height: result.height,
          pixels: result.frames[0].pixels,
        };
      } else {
        previewBitmap = {
          width: result.width,
          height: result.height,
          pixels: result.frames[0].pixels,
        };
      }
    }
    
    loading = false;
  } catch (e) {
    error = e.message;
    loading = false;
  }
}

function paint({ wipe, ink, paste, screen, pen }) {
  wipe(30);
  
  if (loading) {
    ink(255).write("Loading...", { center: "xy", screen });
    return;
  }
  
  if (error) {
    ink(255, 100, 100).write(error, { center: "xy", screen });
    return;
  }
  
  // Animate frames
  if (previewFrames) {
    const now = performance.now();
    const delay = previewFrames.frames[frameIndex]?.delay || 800;
    if (now - lastFrameTime > delay) {
      frameIndex = (frameIndex + 1) % previewFrames.frames.length;
      previewBitmap.pixels = previewFrames.frames[frameIndex].pixels;
      lastFrameTime = now;
    }
  }
  
  // Draw mug preview centered
  if (previewBitmap) {
    const scale = Math.min(
      (screen.width * 0.8) / previewBitmap.width,
      (screen.height * 0.6) / previewBitmap.height
    );
    const w = previewBitmap.width * scale;
    const h = previewBitmap.height * scale;
    const x = (screen.width - w) / 2;
    const y = (screen.height - h) / 2 - 40;
    
    paste(previewBitmap, x, y, { scale });
  }
  
  // Color label
  ink(200).write(`${color.toUpperCase()} MUG`, { x: screen.width / 2, y: 20, center: "x" });
  
  // Buy button
  btn?.paint({ ink, paste, pen });
}

function act({ event: e, screen, jump, api }) {
  // Reposition button on window resize
  if (e.is("reframed")) {
    btn?.reposition({ center: "xy", screen });
  }
  
  if (btn?.act(e)?.down) {
    purchaseMug(jump, api);
  }
}

async function purchaseMug(jump, api) {
  loading = true;
  
  try {
    const headers = { "Content-Type": "application/json" };
    const token = await api?.authorize?.();
    if (token) headers.Authorization = `Bearer ${token}`;
    
    const res = await fetch(`/api/mug?new=true&pixels=${sourceCode}.png&color=${color}`, {
      method: "POST",
      headers,
      body: JSON.stringify({ quantity: 1 }),
    });
    
    const data = await res.json();
    if (data.location) {
      jump(data.location); // Go to Stripe checkout (handles external URLs automatically)
    }
  } catch (e) {
    error = e.message;
    loading = false;
  }
}

export { boot, paint, act };
```

### 2. Update `prompt.mjs` mug handler

Change from calling `mug()` directly to jumping to the piece:

```javascript
} else if (slug === "mug") {
  // ☕ Jump to mug preview piece
  let code = system.painting?.code;
  let color = "white";
  let paramIndex = 0;
  
  if (params[0]?.startsWith("#")) {
    code = params[0].slice(1);
    paramIndex = 1;
  }
  
  if (params[paramIndex] && isNaN(parseInt(params[paramIndex]))) {
    color = params[paramIndex];
  }
  
  if (!code) {
    flashColor = [255, 0, 0];
    makeFlash($);
    notice("NO PAINTING", ["yellow", "red"]);
    return true;
  }
  
  jump(`mug ${code} ${color}`);
  return true;
}
```

### 3. Key Features from keep.mjs to reuse

**Animated WebP decoding:**
```javascript
const result = await _preloadAnimatedWebp(url);
thumbnailFrames = {
  frames: result.frames,
  width: result.width,
  height: result.height,
};
```

**Frame animation in paint():**
```javascript
if (thumbnailFrames) {
  const now = performance.now();
  const delay = thumbnailFrames.frames[frameIndex]?.delay || 800;
  if (now - lastFrameTime > delay) {
    frameIndex = (frameIndex + 1) % thumbnailFrames.frames.length;
    thumbnailBitmap.pixels = thumbnailFrames.frames[frameIndex].pixels;
    lastFrameTime = now;
  }
}
```

**Drawing bitmap:**
```javascript
paste(thumbnailBitmap, x, y, { scale });
```

### 4. Nice-to-haves

- [ ] Color selector buttons (white, blue, red, etc.)
- [ ] Quantity selector
- [ ] Show cached preview if available (from S3)
- [ ] Loading spinner animation
- [ ] Back button to return to prompt
- [ ] Price breakdown tooltip

---

## KidLisp.com Console Mug Button

### Goal
Add a ☕ mug emoji button next to each screenshot in the KidLisp.com console output, allowing users to instantly order a mug with that screenshot printed on it.

### Flow
```
User runs KidLisp code → Screenshot appears in console
  → User clicks ☕ button next to screenshot
  → Upload screenshot to AC as "provisional" painting (guest/temp)
  → Create product via /api/mug?preview=true
  → Open mug preview modal or redirect to mug piece
  → User clicks Buy → Stripe checkout
```

### Implementation

#### 1. Add mug button to console screenshot output

In `system/public/kidlisp.com/index.html`, where screenshots are rendered:

```javascript
// When adding screenshot to console output
function addScreenshotToConsole(canvas, timestamp) {
  const container = document.createElement('div');
  container.className = 'console-screenshot';
  
  // Screenshot image
  const img = document.createElement('img');
  img.src = canvas.toDataURL('image/png');
  container.appendChild(img);
  
  // Mug button
  const mugBtn = document.createElement('button');
  mugBtn.className = 'mug-btn';
  mugBtn.textContent = '☕';
  mugBtn.title = 'Order as mug ($18)';
  mugBtn.onclick = () => orderMug(canvas);
  container.appendChild(mugBtn);
  
  consoleOutput.appendChild(container);
}
```

#### 2. Upload screenshot and create mug product

```javascript
async function orderMug(canvas) {
  const mugBtn = event.target;
  mugBtn.disabled = true;
  mugBtn.textContent = '⏳';
  
  try {
    // Convert canvas to blob
    const blob = await new Promise(resolve => canvas.toBlob(resolve, 'image/png'));
    
    // Upload as provisional painting (no auth required, temp storage)
    const formData = new FormData();
    formData.append('file', blob, 'kidlisp-screenshot.png');
    formData.append('source', 'kidlisp.com');
    formData.append('provisional', 'true');
    
    const uploadRes = await fetch('https://aesthetic.computer/api/upload-provisional', {
      method: 'POST',
      body: formData,
    });
    const { code } = await uploadRes.json();
    
    // Get mug preview
    const previewRes = await fetch(`https://aesthetic.computer/api/mug?new=true&pixels=${code}.png&color=white&preview=true`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ quantity: 1 }),
    });
    const previewData = await previewRes.json();
    
    // Option A: Open mug piece in new tab
    window.open(`https://aesthetic.computer/mug/${code}/white`, '_blank');
    
    // Option B: Show modal with preview and buy button
    // showMugModal(previewData);
    
    mugBtn.textContent = '☕';
    mugBtn.disabled = false;
  } catch (e) {
    console.error('Mug order failed:', e);
    mugBtn.textContent = '❌';
    setTimeout(() => {
      mugBtn.textContent = '☕';
      mugBtn.disabled = false;
    }, 2000);
  }
}
```

#### 3. CSS for mug button

```css
.console-screenshot {
  position: relative;
  display: inline-block;
  margin: 4px;
}

.console-screenshot img {
  max-width: 200px;
  border-radius: 4px;
}

.mug-btn {
  position: absolute;
  top: 4px;
  right: 4px;
  background: rgba(0, 0, 0, 0.7);
  border: none;
  border-radius: 4px;
  padding: 4px 8px;
  cursor: pointer;
  font-size: 16px;
  opacity: 0.7;
  transition: opacity 0.2s;
}

.mug-btn:hover {
  opacity: 1;
  background: rgba(0, 0, 0, 0.9);
}

.mug-btn:disabled {
  cursor: wait;
}
```

#### 4. Backend: Provisional upload endpoint

Create `/api/upload-provisional` that:
- Accepts image uploads without auth
- Stores in temporary S3 location (`provisional/` prefix)
- Returns a short code
- Auto-expires after 24-48 hours if not ordered
- On successful mug order, moves to permanent storage

```javascript
// system/netlify/functions/upload-provisional.js
export async function handler(event) {
  // Accept multipart form data
  // Upload to S3: provisional/{code}.png
  // Store in MongoDB: provisional collection with TTL index
  // Return { code, expires }
}
```

### Files to create/modify

1. **Modify:** `system/public/kidlisp.com/index.html` - Add mug button to screenshots
2. **Create:** `system/netlify/functions/upload-provisional.js` - Handle temp uploads
3. **Modify:** `system/backend/products.mjs` - Support provisional source type

### Alternative: Direct blob upload to mug endpoint

Instead of a separate provisional upload, we could modify `/api/mug` to accept base64 image data directly:

```javascript
// POST /api/mug with body: { imageData: "data:image/png;base64,..." }
const res = await fetch('https://aesthetic.computer/api/mug?new=true&color=white&preview=true', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    imageData: canvas.toDataURL('image/png'),
    source: 'kidlisp.com',
    quantity: 1,
  }),
});
```

This is simpler but puts more load on the mug endpoint.

---

## Files to modify

1. **Create:** `system/public/aesthetic.computer/disks/mug.mjs`
2. **Modify:** `system/public/aesthetic.computer/disks/prompt.mjs` - Change mug handler to jump to piece
3. **Optional:** Keep `disk.mjs` mug function as fallback or remove it
4. **Modify:** `system/public/kidlisp.com/index.html` - Add ☕ button to console screenshots
5. **Create:** `system/netlify/functions/upload-provisional.js` (if using separate upload)

## Testing

```bash
# Local test
https://localhost:8888/mug/hjq/blue

# From prompt with existing painting
mug
mug blue
mug hjq
mug hjq blue

# KidLisp.com test
1. Go to kidlisp.com
2. Run some code that produces output
3. Click ☕ on a screenshot
4. Should open mug preview or redirect to checkout
```

