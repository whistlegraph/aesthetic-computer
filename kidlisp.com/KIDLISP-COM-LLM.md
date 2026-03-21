# KidLisp.com Editor - LLM Context

This document captures key architectural decisions, known issues, and implementation details for the KidLisp.com web editor to help maintain context across AI sessions.

## Overview

KidLisp.com is a browser-based editor for KidLisp, a visual programming language. The main file is:
- **Location**: `system/public/kidlisp.com/index.html` (~7400 lines)
- **CSS Modules**: `system/public/kidlisp.com/css/` (split into variables, branding, components, etc.)
- **JS Modules**: `system/public/kidlisp.com/js/` (events, logger, playback, state)

## Architecture

### Platform Selector
The editor supports multiple target platforms:
- **Aesthetic.Computer** (primary, fully functional)
- **FF1 Art Computer** (functional)
- **Ableton Live** (functional)
- **Playdate** (coming soon)
- **Game Boy** (coming soon)
- **Nintendo 64** (experimental)

Each platform has:
- Brand colors (header background)
- Custom fonts (PP Mori for FF1, Roobert for Playdate, Futura PT for Ableton)
- Colored dropdown options matching brand

### Layout System
The editor uses a **Split.js** based layout with 4 panels:
1. **Editor Panel** (top-left) - CodeMirror editor
2. **Preview Panel** (top-right) - iframe running pieces
3. **Reference Panel** (bottom-left) - documentation
4. **Console Panel** (bottom-right) - logs/output

**Two modes:**
- **Desktop (>768px)**: 2x2 grid using Split.js with resizable gutters
  - Can enter "Perfect +" mode when all gutters centered (4 independent quadrants)
  - Standard mode: top-row/bottom-row structure with horizontal splits
- **Mobile (≤768px)**: Single column, NO Split.js
  - Uses flexbox layout
  - Headers are tappable (collapse/expand) and draggable (resize)
  - Collapse indicator arrows on right side of headers

### Key DOM Structure
```
#main-split
├── #top-row (desktop only - recreated dynamically)
│   ├── #editor-panel
│   └── #preview-panel
├── #bottom-row (desktop only - recreated dynamically)
│   ├── #reference-panel
│   └── #console-panel
└── (mobile: panels are direct children, no rows)
```

**Important**: The `#top-row` and `#bottom-row` elements are **destroyed** when switching to mobile (via `innerHTML = ''`) and must be **recreated** when switching back to desktop.

## Known Issues & Fixes

### 1. appendChild Error on Resize (Fixed Dec 2024)
**Problem**: `Uncaught TypeError: Failed to execute 'appendChild' on 'Node': parameter 1 is not of type 'Node'`

**Cause**: When switching from mobile to desktop, `topRow` and `bottomRow` are null because they were removed from DOM in mobile mode.

**Fix**: Check if row elements exist before using them, recreate if needed:
```javascript
let topRow = document.getElementById('top-row');
let bottomRow = document.getElementById('bottom-row');

if (!topRow && !isMobile) {
  topRow = document.createElement('div');
  topRow.id = 'top-row';
  topRow.className = 'top-row';
}
// Same for bottomRow
```

### 2. Auth Buttons Overlapping Collapse Arrow (Fixed Dec 2024)
**Problem**: In single-column (mobile) mode, login/signup buttons overlapped with the dropdown arrow on the right.

**Fix**: Increased `margin-right` for `.header-auth-buttons` in mobile media query from `6px` to `28px` to match `.header-user-menu`.

### 3. QR Code / Auth on Non-AC Platforms
**Behavior**: QR codes and auth buttons (Login, I'm new, @handle) only appear when platform is "aesthetic-computer". Other platforms hide these elements.

**Implementation**: `updateACLoginUI()` checks `currentPlatform` and sets `display: none` for non-AC platforms.

### 4. Platform Title Reset on Refresh (Fixed Dec 2024)
**Problem**: When changing platforms and refreshing the page, the title reverted to "Aesthetic.Computer" even though a different platform was selected.

**Cause**: Multiple places in the code hardcoded "Aesthetic.Computer" when resetting the preview title (on code changes, stop button, code creation, example clicks).

**Fix**: Modified these locations to either:
1. Use the current platform name from the DOM (`querySelector('.platform-option[data-platform="${currentPlatform}"] .platform-opt-name')`)
2. Only run AC-specific title updates when `currentPlatform === 'aesthetic-computer'`

**Affected code locations**:
- Code change handler (editor onChange)
- Stop button handler
- Code created message handler
- Example click handler

### 5. Play Button on Non-AC Platforms
**Behavior**: Shows "coming soon" toast instead of playing.

**Implementation**: `sendCode()` checks platform before executing.

## CSS Variables (Theme)
```css
--bg-primary, --bg-secondary, --bg-tertiary
--text-primary, --text-secondary, --text-tertiary
--border-color, --border-subtle
--code-bg, --code-text
--editor-bg, --editor-text, --editor-border
```

Theme controlled by `data-theme="light|dark"` attribute on root, with system preference fallback.

## Key Functions

### `initSplits()`
Initializes or reinitializes the split layout. Called:
- On initial load
- On window resize (via debounced handler)
- When gutter states change (perfect + mode transitions)

### `setupMobileHeaders()`
Sets up tap-to-toggle and drag-to-resize behavior for mobile panel headers.

### `updateACLoginUI()`
Updates auth UI visibility based on login state and current platform.

### `generateAndDisplayQR()`
Generates QR code for current piece URL. Only runs on AC platform.

## Platform-Specific Styling

### Header Colors (by platform)
- aesthetic-computer: `rgb(88, 78, 108)` (purple)
- ff1: `rgb(26, 26, 26)` (near black)
- ableton: `rgb(255, 128, 0)` (orange)
- playdate: `rgb(255, 204, 0)` (yellow)
- gameboy: `rgb(155, 188, 15)` (DMG green)
- n64: `rgb(0, 100, 0)` (dark green)

### Platform Fonts
- FF1: PP Mori
- Playdate: Roobert
- Ableton: Futura PT
- Others: Default system fonts

## Testing Checklist

When making layout changes, test:
1. [ ] Desktop mode at various sizes
2. [ ] Mobile mode (≤768px)
3. [ ] Resize from mobile → desktop (row recreation)
4. [ ] Resize from desktop → mobile (row destruction)
5. [ ] Perfect + mode transitions (all gutters centered)
6. [ ] Panel collapse/expand on mobile
7. [ ] Auth buttons don't overlap collapse arrows
8. [ ] Platform switching updates UI correctly

## File Dependencies

- **Split.js**: CDN loaded for desktop layout
- **CodeMirror**: Editor component
- **QRious**: QR code generation
- **Aesthetic.Computer iframe**: Preview rendering

## Related Files

- `system/public/aesthetic.computer/lib/kidlisp.mjs` - KidLisp interpreter integration
- `system/public/aesthetic.computer/lib/logs.mjs` - Logging utilities (must export `log`)
- `kidlisp/` - KidLisp interpreter source
- `spec/` - KidLisp language specifications
