# Ableton Live Integration Progress

## Overview
Creating offline HTML bundles of AC pieces (specifically `notepat`) that can be loaded inside Ableton Live's Max for Live devices.

## Goals
1. ✅ Generate self-contained HTML bundles for JavaScript pieces like `notepat`
2. ✅ Bundle should work completely offline (no network requests)
3. ✅ Show correct author (`@jeffrey`) instead of `@aesthetic`
4. ✅ No `$` prefix for JS pieces (only for KidLisp)
5. ✅ Suppress MIDI connection errors in console
6. ✅ Piece loads correctly (was showing `noise16` default instead of `notepat`)
7. ❌ **Audio engine not working** - currently silent
8. ❌ **Prompt HUD label missing** - font glyph issue
9. ❌ **Fonts making network requests** instead of loading from VFS

---

## Completed Work

### 1. Bundle Generation Infrastructure
- **File**: `system/netlify/functions/bundle-html.js`
- Added support for JS piece bundles via `?piece=notepat` parameter
- Created `createJSPieceBundle()` function that:
  - Discovers all ES module dependencies recursively
  - Loads font glyphs from `disks/drawings/font_1/`
  - Minifies code using `@swc/wasm`
  - Creates import maps for blob URLs
  - Generates gzip-compressed self-extracting HTML

### 2. Fixed Author Display
- Changed hardcoded `@aesthetic` to `@jeffrey` for JS pieces
- Made `$` prefix conditional on `isKidLisp` flag

### 3. Fixed MIDI Error Spam
- Added MIDI-related messages to console suppression filter

### 4. Fixed Piece Not Loading
- Issue: Piece was staying on default `noise16` instead of loading `notepat`
- Root cause: VFS fetch interceptor path resolution
- Added debug logging to VFS fetch interceptor
- Parse flow: `parse("notepat")` → `path="aesthetic.computer/disks/notepat"`
- In pack mode: `fullUrl` becomes `../disks/notepat.mjs#timestamp`
- VFS interceptor strips `../` and `#...` to get `disks/notepat.mjs`

### 5. VFS (Virtual File System) Implementation
```javascript
// Fetch interceptor in bundle HTML
window.fetch = function(url, options) {
  let vfsPath = decodeURIComponent(urlStr)
    .replace(/^https?:\/\/[^\/]+\//g, '')
    .replace(/^aesthetic\.computer\//g, '')
    .replace(/#.*$/g, '')
    .replace(/\?.*$/g, '');
  
  vfsPath = vfsPath.replace(/^\.\.\/+/g, '').replace(/^\.\/+/g, '').replace(/^\/+/g, '');
  
  console.log('[VFS] fetch:', urlStr, '->', vfsPath, 'exists:', !!window.VFS[vfsPath]);
  // ...
};
```

---

## Current Issues

### Issue 1: Audio Silent
**Status**: In Progress

**Root Cause Found**: Audio worklet loading is explicitly skipped in PACK mode!

In `bios.mjs` lines 2056-2059:
```javascript
if (isPackMode) {
  if (debug) console.log("🎭 Skipping audio worklet loading in PACK mode");
  return;
}
```

**Why it was skipped**: AudioWorklet.addModule() requires a URL, and the worklet files (`speaker.mjs`) have ES module imports that can't be resolved without a server.

**Solution in progress**:
1. ✅ Created bundled version of speaker worklet: `lib/speaker-bundled.mjs` (~50KB)
   - Used esbuild to bundle `speaker.mjs` with all dependencies:
     - `sound/volume.mjs`
     - `sound/synth.mjs`
     - `sound/bubble.mjs`
     - `num.mjs` (lerp, within, clamp functions)
     - `pack-mode.mjs`

2. ✅ Added `speaker-bundled.mjs` to ESSENTIAL_FILES in bundle-html.js

3. 🔄 Need to modify `bios.mjs` to:
   - In PACK mode, load worklet from VFS via blob URL
   - Create blob URL from `window.VFS['lib/speaker-bundled.mjs']`
   - Use that blob URL with `audioContext.audioWorklet.addModule()`

### Issue 2: Fonts Making Network Requests
**Status**: Investigating

Font glyphs ARE being bundled into VFS (lines 406-425 of bundle-html.js), but requests might not be matching VFS paths correctly.

Line 743 returns empty response for missing fonts instead of serving bundled content - this might be the issue.

### Issue 3: HUD Label Missing
**Status**: Blocked by Issue 2

The prompt HUD corner label uses font glyphs. If fonts aren't loading from VFS, the label won't render.

---

## Files Modified

| File | Changes |
|------|---------|
| `system/netlify/functions/bundle-html.js` | Added JS piece bundling, VFS fetch interceptor, font loading |
| `system/public/aesthetic.computer/bios.mjs` | (Pending) Worklet blob URL loading for PACK mode |
| `system/public/aesthetic.computer/lib/speaker-bundled.mjs` | NEW - Pre-bundled speaker worklet |

---

## Bundle Architecture

```
HTML Bundle (gzip compressed)
├── Self-extracting script (decompresses on load)
├── VFS (Virtual File System) JSON object
│   ├── boot.mjs
│   ├── bios.mjs
│   ├── lib/*.mjs (all dependencies)
│   ├── disks/notepat.mjs (the piece)
│   ├── disks/drawings/font_1/**/*.json (font glyphs)
│   └── lib/speaker-bundled.mjs (bundled audio worklet)
├── Import Map (maps module paths to blob URLs)
├── Fetch Interceptor (serves VFS content for fetch() calls)
└── Bootstrap (loads boot.mjs from blob URL)
```

---

## Commands Used

```bash
# Bundle speaker worklet with esbuild
/workspaces/aesthetic-computer/system/node_modules/@esbuild/linux-arm64/bin/esbuild \
  --bundle --format=esm \
  --outfile=/tmp/speaker-bundled.mjs \
  /workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/speaker.mjs

# Generate bundle (from browser)
https://localhost:8888/api/bundle-html?piece=notepat&nocache=1

# Copy bundle to Mac
scp notepat.html jas@host.docker.internal:~/Desktop/
```

---

## Next Steps

1. **Fix Audio**: Modify `bios.mjs` to load bundled worklet from VFS blob URL in PACK mode
2. **Fix Fonts**: Debug VFS path matching for font glyph requests  
3. **Test in M4L**: Load bundle in Max for Live and verify full functionality
4. **Cleanup**: Remove debug logging, optimize bundle size

---

## Related Files

- `ac-m4l/` - Max for Live devices
- `system/netlify/functions/bundle-html.js` - Bundle generation
- `system/public/aesthetic.computer/bios.mjs` - Audio system
- `system/public/aesthetic.computer/lib/speaker.mjs` - Speaker worklet (original)
- `system/public/aesthetic.computer/lib/speaker-bundled.mjs` - Speaker worklet (bundled)
- `system/public/aesthetic.computer/disks/notepat.mjs` - The piece being bundled
