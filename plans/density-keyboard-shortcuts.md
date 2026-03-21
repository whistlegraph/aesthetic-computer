# Plan: Density Keyboard Shortcuts

## Problem Statement
In VS Code Simple Browser (and potentially other embedded contexts), **Cmd+/Cmd-** (or Ctrl+/Ctrl-) are intercepted by the host app for zoom controls and never reach the AC iframe/webview. Users want to be able to change AC pixel density via keyboard shortcuts even when browser zoom shortcuts are blocked.

## Goals
1. Add keyboard shortcuts to increase/decrease density: **Ctrl+= / Ctrl+-** (or Cmd+= / Cmd+-)
2. Make density changes **live** (immediate visual update, no page reload)
3. Persist density to localStorage (already exists: `ac-density`)
4. Optional: Add visual feedback (brief toast/HUD showing new density value)

---

## Current Architecture

### Density Flow (Boot Time)
1. **boot.mjs** (lines 593-605):
   - Parses `?density=X` URL param
   - Saves to `localStorage["ac-density"]`
   - Falls back to stored value if no URL param
   - Sets `window.acPACK_DENSITY` (implied)

2. **bios.mjs** (lines 980-1095):
   - In `frame()` function, reads density:
     ```javascript
     let density = resolution.density !== undefined 
       ? resolution.density 
       : (window.acPACK_DENSITY !== undefined ? window.acPACK_DENSITY : 2);
     ```
   - Uses density for canvas subdivision and projection

### Keyboard Event Flow
1. **bios.mjs** creates keyboard events from DOM → pushes to `keyboard.events[]`
2. Events sent to worker via `send({ keyboard: keyboard.events })`
3. **disk.mjs** processes events in `content.keyboard.forEach()` (line 9929+)
4. Global shortcuts handled for Escape, Backspace, Tab, Shift, $, Ctrl+X, etc.

### Key Observations
- Keyboard events include `ctrl`, `alt`, `shift` flags (lines 10630-10631)
- Need to capture at **bios.mjs level** (before iframe may intercept)
- Density change requires calling `frame()` to update canvas

---

## Implementation Plan

### Phase 1: Add Keyboard Capture in bios.mjs

**File:** `system/public/aesthetic.computer/bios.mjs`

Add a global keydown listener that captures Cmd/Ctrl +/- before the browser can handle them:

```javascript
// Near line ~3100 (after enableAudioPlayback listener)
window.addEventListener("keydown", (e) => {
  // Density controls: Cmd/Ctrl + Plus/Minus
  const isMeta = e.metaKey || e.ctrlKey;
  
  if (isMeta && (e.key === "=" || e.key === "+")) {
    e.preventDefault();
    e.stopPropagation();
    changeDensity(1); // Increase density
  } else if (isMeta && e.key === "-") {
    e.preventDefault();
    e.stopPropagation();
    changeDensity(-1); // Decrease density
  }
}, { capture: true }); // Use capture phase to intercept early
```

### Phase 2: Create Live Density Change Function

**File:** `system/public/aesthetic.computer/bios.mjs`

Add a `changeDensity(direction)` function:

```javascript
// Define density limits
const DENSITY_MIN = 0.5;
const DENSITY_MAX = 4;
const DENSITY_STEP = 0.5; // Or 1 for whole numbers only

// Add to bios.mjs scope
function changeDensity(direction) {
  // Get current density
  const current = window.acPACK_DENSITY ?? 2;
  
  // Calculate new density
  let newDensity = current + (direction * DENSITY_STEP);
  newDensity = Math.max(DENSITY_MIN, Math.min(DENSITY_MAX, newDensity));
  newDensity = Math.round(newDensity * 2) / 2; // Round to nearest 0.5
  
  if (newDensity === current) return; // No change
  
  // Update global
  window.acPACK_DENSITY = newDensity;
  
  // Persist to localStorage
  try {
    localStorage.setItem("ac-density", newDensity.toString());
  } catch {}
  
  // Trigger reframe to apply new density
  frame(); // Call frame() to recalculate canvas size
  
  // Optional: Play feedback sound
  // sound.synth({ tone: direction > 0 ? 1400 : 1000, duration: 0.05, volume: 0.2 });
  
  // Optional: Show brief HUD notification
  console.log(`🔍 Density: ${newDensity}`);
}
```

### Phase 3: Ensure frame() Uses Updated Density

**File:** `system/public/aesthetic.computer/bios.mjs`

The `frame()` function already reads from `window.acPACK_DENSITY`, so it should pick up the change automatically. Verify at line ~987:
```javascript
let density = resolution.density !== undefined 
  ? resolution.density 
  : (window.acPACK_DENSITY !== undefined ? window.acPACK_DENSITY : 2);
```

This is already correct—`window.acPACK_DENSITY` will be read fresh each time `frame()` is called.

### Phase 4: Key Bindings (Non-Conflicting with Browser Zoom)

**Important:** We must NOT use Cmd/Ctrl +/- because:
1. In regular browsers: Would cause BOTH browser zoom AND density change (double effect)
2. Browser zoom should remain functional for accessibility

**Chosen bindings:**

| Shortcut | Action | Notes |
|----------|--------|-------|
| `Ctrl/Cmd + [` | Decrease density | Rarely conflicts, easy to reach |
| `Ctrl/Cmd + ]` | Increase density | Rarely conflicts, easy to reach |
| `Ctrl/Cmd + 0` | Reset density to 2 | Matches browser "reset zoom" concept |

These keys are:
- Not used by browser zoom (Cmd+/-)
- Not used by VS Code for anything critical in webviews
- Intuitive bracket notation (smaller [ / larger ])

### Phase 5: Visual Feedback (Optional Enhancement)

Add a brief HUD toast showing the current density:

```javascript
function showDensityToast(density) {
  // Use existing overlay system or create simple DOM toast
  const toast = document.createElement('div');
  toast.textContent = `Density: ${density}`;
  toast.style.cssText = `
    position: fixed; bottom: 20px; right: 20px;
    background: rgba(0,0,0,0.8); color: white;
    padding: 8px 16px; border-radius: 4px;
    font-family: monospace; font-size: 14px;
    z-index: 9999; transition: opacity 0.3s;
  `;
  document.body.appendChild(toast);
  setTimeout(() => { toast.style.opacity = 0; }, 1000);
  setTimeout(() => { toast.remove(); }, 1300);
}
```

---

## Testing Strategy

1. **Local browser test**: Open `https://localhost:8888/prompt`, press Cmd/Ctrl +/-, verify density changes
2. **VS Code Simple Browser test**: Open AC in VS Code webview, verify shortcuts work
3. **Verify persistence**: Change density, refresh page, confirm it persists
4. **Edge cases**: Test at min/max density limits

---

## Files Modified

| File | Change |
|------|--------|
| `bios.mjs` | Add keydown listener (~line 3100), add `changeDensity()` function |
| `boot.mjs` | No changes (already handles localStorage persistence) |

---

## Questions to Resolve

1. **Step size**: Should density change by 0.5 or 1? (e.g., 1 → 1.5 → 2 or 1 → 2 → 3)
2. **Range**: What are sensible min/max? (Suggest: 0.5 to 4)
3. **Alternative keys**: If Cmd+/- still blocked, which alternates to use?
4. **Visual feedback**: Do we want a toast/HUD notification?

---

## Next Steps

- [x] Implement Phase 1: Add keyboard capture in bios.mjs
- [x] Implement Phase 2: Create changeDensity() function  
- [x] Phase 4: Use non-conflicting keys (Cmd+[ / Cmd+] / Cmd+0)
- [ ] Test in VS Code Simple Browser
- [ ] Optional: Add visual feedback (toast)

## Implementation Details

**Keys chosen:**
- `Cmd/Ctrl + [` → Decrease density by 0.5
- `Cmd/Ctrl + ]` → Increase density by 0.5  
- `Cmd/Ctrl + 0` → Reset density to 2

**Range:** 0.5 to 4 (step 0.5)

**Browser zoom:** Unaffected (Cmd+/- still work normally)
