# KidLisp Console Screenshots

**Status: ✅ IMPLEMENTED**

## Overview
Enable KidLisp pieces to output visual screenshots/thumbnails directly to the kidlisp.com console, creating a scrollable visual history of execution frames.

## Existing Implementation (PACK Mode)

### Location
[bios.mjs](../system/public/aesthetic.computer/bios.mjs#L15807)

### Current Behavior
Every 500 frames in PACK mode, captures and logs a thumbnail to the browser console:
```javascript
// 📸 PACK mode: Log frame to console every 500 frames
if (window.acPACK_MODE && frameCount % 500n === 0n && canvas) {
  // ... capture logic
  console.log(
    `%c📸 ${pieceCode} %c@ ${ts} %c• Frame ${frameCount} %c[${w}×${h}]`,
    // ... styled console output
  );
  console.log(
    `%c `,
    `font-size: 1px; padding: ${displayH/2}px; background: url("${dataUrl}") ...`
  );
}
```

### Key Details
- Uses canvas `toDataURL('image/png')` for capture
- Scales to 3x for crisp pixelated display
- Display size constrained to ~200px max dimension
- Styled with CSS colors for piece code, timestamp, frame count, dimensions

## KidLisp Console Protocol

### Message Format
[kidlisp.mjs](../system/public/aesthetic.computer/lib/kidlisp.mjs#L327)

```javascript
function postKidlispConsole(level, message, meta) {
  const payload = { type: "kidlisp-console", level, message };
  if (meta) {
    if (meta.loc) payload.loc = meta.loc;
    if (meta.kind) payload.kind = meta.kind;
    if (meta.embeddedSource) payload.embeddedSource = meta.embeddedSource;
  }
  postToParent(payload);
}
```

### Console Receiver
[kidlisp.com/index.html](../system/public/kidlisp.com/index.html#L6568)

```javascript
} else if (event.data && event.data.type === 'kidlisp-console') {
  addConsoleEntry(event.data.message, event.data.level, event.data.loc, {
    embeddedSource: event.data.embeddedSource,
    embeddedSourceCode: event.data.embeddedSourceCode
  });
}
```

## Implementation Plan

### Phase 1: Extend Console Protocol for Images

#### 1.1 Add New Message Type
Extend `postKidlispConsole` to support image data:

```javascript
// kidlisp.mjs
function postKidlispConsoleImage(imageDataUrl, meta = {}) {
  if (!isKidlispConsoleEnabled()) return;
  const payload = { 
    type: "kidlisp-console-image",
    imageDataUrl,
    frameCount: meta.frameCount,
    timestamp: meta.timestamp || Date.now(),
    dimensions: meta.dimensions,
    pieceCode: meta.pieceCode,
    embeddedSource: meta.embeddedSource
  };
  postToParent(payload);
}
```

#### 1.2 Export Function
Add to KidLisp exports so it can be called from bios.mjs and kidlisp pieces.

### Phase 2: Create Reusable Frame Capture Utility

#### 2.1 New Module: `frame-capture.mjs`
Location: `system/public/aesthetic.computer/lib/frame-capture.mjs`

```javascript
/**
 * Capture a frame from a canvas and generate a data URL
 * @param {HTMLCanvasElement} canvas - Source canvas
 * @param {Object} options - Capture options
 * @returns {Object} - { dataUrl, displayWidth, displayHeight, dimensions }
 */
export function captureFrame(canvas, options = {}) {
  const {
    scaleFactor = 3,        // For crisp pixelated display
    displayMax = 200,       // Max display dimension
    format = 'image/png',   // Output format
    quality = 1.0           // Quality for jpeg/webp
  } = options;
  
  const w = canvas.width;
  const h = canvas.height;
  
  // Create scaled thumbnail
  const thumbW = w * scaleFactor;
  const thumbH = h * scaleFactor;
  const thumbCanvas = document.createElement('canvas');
  thumbCanvas.width = thumbW;
  thumbCanvas.height = thumbH;
  const thumbCtx = thumbCanvas.getContext('2d');
  thumbCtx.imageSmoothingEnabled = false;
  thumbCtx.drawImage(canvas, 0, 0, thumbW, thumbH);
  
  const dataUrl = thumbCanvas.toDataURL(format, quality);
  
  // Calculate display dimensions
  const aspect = w / h;
  const displayWidth = aspect >= 1 ? displayMax : Math.round(displayMax * aspect);
  const displayHeight = aspect >= 1 ? Math.round(displayMax / aspect) : displayMax;
  
  return {
    dataUrl,
    displayWidth,
    displayHeight,
    dimensions: { width: w, height: h }
  };
}

/**
 * Format timestamp for display
 */
export function formatTimestamp(date = new Date()) {
  return date.toLocaleTimeString('en-US', { 
    hour: 'numeric', 
    minute: '2-digit', 
    second: '2-digit', 
    hour12: true 
  });
}
```

#### 2.2 Update bios.mjs
Refactor to use the shared module:

```javascript
import { captureFrame, formatTimestamp } from './lib/frame-capture.mjs';

// In render loop:
if (window.acPACK_MODE && frameCount % 500n === 0n && canvas) {
  try {
    const { dataUrl, displayWidth, displayHeight, dimensions } = captureFrame(canvas);
    const ts = formatTimestamp();
    const pieceCode = window.acPACK_PIECE || 'piece';
    
    // Log to browser console (existing behavior)
    console.log(/* ... existing styled output ... */);
    
    // Also send to kidlisp.com console if enabled
    if (isKidlispConsoleEnabled()) {
      postKidlispConsoleImage(dataUrl, {
        frameCount: Number(frameCount),
        timestamp: ts,
        pieceCode,
        dimensions
      });
    }
  } catch (e) { /* ... */ }
}
```

### Phase 3: Console Image Display

#### 3.1 Add CSS Styles
In kidlisp.com/index.html:

```css
.console-image-entry {
  display: flex;
  align-items: flex-start;
  gap: 12px;
  padding: 8px 12px;
  border-radius: 6px;
  background: rgba(78, 205, 196, 0.1);
  border-left: 3px solid #4ecdc4;
  margin: 4px 0;
}

.console-image-thumbnail {
  image-rendering: pixelated;
  border-radius: 4px;
  cursor: pointer;
  transition: transform 0.2s;
}

.console-image-thumbnail:hover {
  transform: scale(1.5);
  z-index: 100;
}

.console-image-actions {
  display: flex;
  gap: 6px;
  margin-top: 6px;
}

.console-image-download {
  background: #4ecdc4;
  color: #1e1e2e;
  border: none;
  border-radius: 4px;
  padding: 4px 8px;
  font-size: 11px;
  cursor: pointer;
  font-weight: bold;
}

.console-image-download:hover {
  background: #3dbdb5;
}

.console-image-meta {
  font-size: 11px;
  font-family: monospace;
  color: #888;
}

.console-image-meta .piece-code {
  color: #4ecdc4;
  font-weight: bold;
}

.console-image-meta .timestamp {
  color: #f8b500;
}

.console-image-meta .frame-count {
  color: #666;
}
```

#### 3.2 Add Message Handler
```javascript
} else if (event.data && event.data.type === 'kidlisp-console-image') {
  addConsoleImageEntry(event.data);
}
```

#### 3.3 Create Image Entry Function
```javascript
function addConsoleImageEntry(data) {
  const { imageDataUrl, frameCount, timestamp, dimensions, pieceCode } = data;
  
  const entry = document.createElement('div');
  entry.className = 'console-entry console-image-entry';
  
  // Thumbnail image
  const img = document.createElement('img');
  img.src = imageDataUrl;
  img.className = 'console-image-thumbnail';
  img.style.width = '120px';  // Fixed preview width
  img.style.height = 'auto';
  img.title = 'Click to view full size';
  img.addEventListener('click', () => showImagePopup(imageDataUrl, dimensions, pieceCode, frameCount));
  
  // Metadata
  const meta = document.createElement('div');
  meta.className = 'console-image-meta';
  meta.innerHTML = `
    <span class="piece-code">📸 ${pieceCode || '$piece'}</span><br>
    <span class="timestamp">@ ${timestamp}</span><br>
    <span class="frame-count">Frame ${frameCount}</span><br>
    <span class="dimensions">[${dimensions?.width}×${dimensions?.height}]</span>
  `;
  
  // Actions (download button)
  const actions = document.createElement('div');
  actions.className = 'console-image-actions';
  
  const downloadBtn = document.createElement('button');
  downloadBtn.className = 'console-image-download';
  downloadBtn.textContent = '⬇ Download';
  downloadBtn.addEventListener('click', () => downloadImage(imageDataUrl, pieceCode, frameCount));
  actions.appendChild(downloadBtn);
  
  meta.appendChild(actions);
  entry.appendChild(img);
  entry.appendChild(meta);
  
  consoleOutput.appendChild(entry);
  consoleOutput.scrollTop = consoleOutput.scrollHeight;
}

function downloadImage(dataUrl, pieceCode, frameCount) {
  const filename = `${pieceCode || 'piece'}-frame${frameCount || 0}-${Date.now()}.png`;
  const link = document.createElement('a');
  link.href = dataUrl;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
}

function showImagePopup(dataUrl, dimensions, pieceCode, frameCount) {
  // Similar to showEmbeddedSourcePopup but for images
  const existing = document.getElementById('console-image-popup');
  if (existing) existing.remove();
  
  const popup = document.createElement('div');
  popup.id = 'console-image-popup';
  popup.style.cssText = `
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    background: #1e1e2e;
    border: 2px solid #4ecdc4;
    border-radius: 8px;
    padding: 16px;
    z-index: 10000;
    box-shadow: 0 4px 20px rgba(0,0,0,0.5);
  `;
  
  const img = document.createElement('img');
  img.src = dataUrl;
  img.style.cssText = 'image-rendering: pixelated; max-width: 80vw; max-height: 70vh; display: block;';
  
  // Download button in popup
  const downloadBtn = document.createElement('button');
  downloadBtn.textContent = '⬇ Download PNG';
  downloadBtn.style.cssText = `
    display: block;
    width: 100%;
    margin-top: 12px;
    padding: 10px;
    background: #4ecdc4;
    color: #1e1e2e;
    border: none;
    border-radius: 6px;
    font-size: 14px;
    font-weight: bold;
    cursor: pointer;
  `;
  downloadBtn.onclick = () => downloadImage(dataUrl, pieceCode, frameCount);
  
  const closeBtn = document.createElement('button');
  closeBtn.textContent = '✕';
  closeBtn.style.cssText = 'position: absolute; top: 8px; right: 8px; background: none; border: none; color: #888; cursor: pointer; font-size: 18px;';
  closeBtn.onclick = () => popup.remove();
  
  popup.appendChild(closeBtn);
  popup.appendChild(img);
  popup.appendChild(downloadBtn);
  document.body.appendChild(popup);
  
  // Close on escape
  const closeHandler = (e) => {
    if (e.key === 'Escape') {
      popup.remove();
      document.removeEventListener('keydown', closeHandler);
    }
  };
  document.addEventListener('keydown', closeHandler);
}
```

### Phase 4: KidLisp `snap` Function

#### 4.1 Add `snap` to KidLisp API
Allow KidLisp pieces to explicitly capture and output screenshots:

```javascript
// In globalEnvCache
snap: (api, args = []) => {
  // Capture current frame to console
  if (api.screen?.canvas) {
    const { dataUrl, displayWidth, displayHeight, dimensions } = captureFrame(api.screen.canvas);
    
    // Log to browser console
    console.log(
      `%c📸 snap`,
      `color: #4ecdc4; font-weight: bold;`
    );
    console.log(
      `%c `,
      `font-size: 1px; padding: ${displayHeight/2}px ${displayWidth/2}px; background: url("${dataUrl}") no-repeat center; background-size: ${displayWidth}px ${displayHeight}px; image-rendering: pixelated;`
    );
    
    // Send to kidlisp.com console
    if (isKidlispConsoleEnabled()) {
      postKidlispConsoleImage(dataUrl, {
        frameCount: api.frameCount || 0,
        timestamp: formatTimestamp(),
        pieceCode: api.slug || 'piece',
        dimensions
      });
    }
  }
}
```

#### 4.2 Usage Example
```lisp
; Take a snapshot every second
(1s (snap))

; Or on tap
(tap (snap))
```

### Phase 5: Auto-Screenshot on Errors

Automatically capture the frame state when validation/runtime errors occur:

```javascript
// In parse() error handling
if (validationErrors.length > 0 && this.api?.screen?.canvas) {
  try {
    const { dataUrl } = captureFrame(this.api.screen.canvas, { displayMax: 150 });
    postKidlispConsoleImage(dataUrl, {
      frameCount: this.api.frameCount,
      timestamp: formatTimestamp(),
      pieceCode: this.embeddedSourceId || this.api.slug,
      isErrorSnapshot: true
    });
  } catch (e) { /* fail silently */ }
}
```

## File Changes Summary

| File | Changes |
|------|---------|
| `lib/frame-capture.mjs` | **NEW** - Shared frame capture utility |
| `lib/kidlisp.mjs` | Add `postKidlispConsoleImage`, `snap` function, error snapshots |
| `bios.mjs` | Refactor to use shared module, add console image posting |
| `kidlisp.com/index.html` | Add CSS, message handler, image display functions |

## Migration Path

1. Create `frame-capture.mjs` with shared utilities
2. Update `kidlisp.mjs` with new protocol and `snap` function  
3. Update `kidlisp.com/index.html` with image display
4. Refactor `bios.mjs` to use shared code
5. Test in both PACK mode and kidlisp.com editor

## Future Enhancements

- **GIF Animation**: Capture multiple frames as animated GIF for loops
- **Image Gallery View**: Toggle between list/grid view of captured images
- **Comparison Mode**: Side-by-side comparison of snapshots
- **Auto-capture on specific events**: Capture on `tap`, `draw`, timing events
- **Size optimization**: Use WebP format with quality settings for smaller payloads
- **Batch Download**: Download all captured frames as a ZIP archive
