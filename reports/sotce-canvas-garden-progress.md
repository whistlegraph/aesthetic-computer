# Sotce.net Canvas Garden Progress Report

**Date:** February 4, 2026  
**File:** `system/netlify/functions/sotce-net.mjs`  
**Feature Flag:** `USE_CANVAS_GARDEN = true` (around line 5351)

## Overview

Replaced the DOM-based virtualized scroll garden with a Canvas2D renderer for smoother FYP-style page navigation. The original DOM approach had performance issues with IntersectionObserver firing with 239 entries causing 4-second delays on gate/garden transitions.

## Architecture

### Single Page Model
- Displays one page at a time, centered in viewport
- Ghost (blank) cards slide in during transitions
- Text fades in when page becomes current
- No continuous scroll - discrete page transitions

### Key State Variables (around line 5360)
```javascript
let currentPageIndex = totalPages;
let displayedPageIndex = totalPages;
let transitionProgress = 0; // 0 = showing current, 1 = showing next
let transitionDirection = 0; // -1 = prev, 0 = none, 1 = next
let transitionTarget = null;
let textFadeIn = 1; // 0 to 1, fades in text when page becomes current
let hoverEar = false;
let hoverPageNum = false;
let isFlipping = false;
let flipProgress = 0;
let flipDirection = 1;
```

## Features Implemented

### Page Rendering
- **Card dimensions:** 4:5 aspect ratio, max 600px width, centered horizontally and vertically
- **Background:** Pink `#FFD1DC` (matching `--garden-background`)
- **Card background:** `#f8f4ec`
- **Font:** Helvetica, 17px base at 600px card width, scales proportionally
- **Line height:** 1.76em (matching CSS `--line-height`)
- **Padding:** 2em horizontal
- **Title:** Centered at 6.5% from top
- **Body text:** Starts at 15% from top
- **Page number:** Centered, 2em from bottom
- **Ear (corner fold):** 8% of card width, bottom-right corner

### Navigation
- **Drag gestures:** Pointer events for cross-platform support
- **Threshold:** 20% of card height to trigger page change
- **Smooth continuation:** Transition continues from drag position (not restart)
- **Keyboard:** Arrow keys trigger animated transitions
- **Boundary resistance:** Elastic resistance at first/last page

### Transitions
- **Current page:** Keeps text visible during transition
- **Incoming page:** Ghost (blank card) until it lands
- **Text fade-in:** `textFadeIn` animates 0→1 when page settles
- **Animation speed:** `transitionProgress += 0.12` per frame

### Interactive Elements
- **Hover states:** `hoverEar` and `hoverPageNum` tracked via mousemove
- **Debug boxes:** Pink stroke rectangles when hovering (for debugging)
- **Cursor:** Changes to `pointer` over ear/page number, `grab` elsewhere
- **Touch support:** `touchstart`/`touchend` events for mobile hover highlight

### Ear Flip Animation
- Triggered on ear click
- Uses X-scale transform to simulate 3D flip
- Shows white "back" side at midpoint
- Auto-flips back after 500ms

### Page Number Click
- Opens chat with prefilled message `-{pageNumber}- `
- Cursor moves to end of input via `setSelectionRange`

## Code Locations

| Feature | Approximate Line |
|---------|------------------|
| Canvas setup & state | 5350-5385 |
| `resizeCanvas()` | 5400-5430 |
| `fetchPage()` with deduplication | 5435-5455 |
| `wrapText()` with newline handling | 5470-5498 |
| `renderPage()` | 5500-5620 |
| `render()` main function | 5625-5665 |
| `update()` animation loop | 5670-5710 |
| `goToPage()` | 5715-5730 |
| Pointer event handlers | 5735-5785 |
| Keyboard navigation | 5810-5825 |
| Click detection | 5830-5870 |
| Mousemove hover detection | 5875-5925 |
| Touch hover support | 5935-5975 |

## Known Issues / TODO

1. **Ear flip animation** - Basic implementation, could be more polished
2. **Back of page content** - Currently just white, could show something
3. **Text justification** - Original CSS had `text-align: justify` with hyphens
4. **Page number font** - Uses monospace, original may have been different

## Testing Notes

- Works on desktop with mouse
- Works on mobile with touch (pointer events)
- Page cache uses both in-memory Map and IndexedDB
- Request deduplication via `fetchingPages` Set prevents duplicate fetches

## Related Files

- Original DOM garden code still exists below the canvas code (after `} else if (totalPages > 0`)
- CSS variables defined around lines 335-360
- `openChatWithMessage()` helper at line 2523
