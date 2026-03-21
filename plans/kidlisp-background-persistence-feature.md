# KidLisp Background Color Persistence During Window Reframe

## Problem Statement

When a KidLisp piece uses `wipe "purple"` (or any color) to set a background and the browser window is resized, the expanded canvas areas show transparent pixels instead of extending the background color. This creates an inconsistent visual experience where the original canvas area maintains the purple background but newly expanded areas remain transparent.

## Current Architecture Analysis

### Multi-Buffer Rendering System
- **Main Screen Buffer**: `screen.pixels` - accessible but doesn't affect visual output
- **KidLisp Rendering Buffer**: Separate rendering system used by KidLisp pieces
- **Graphics Engine**: `$paintApiUnwrapped` provides wipe functions but targeting wrong buffer during reframe

### Current Implementation Status
The following mechanisms have been implemented but are not working:

1. **Persistent Storage System** ✅ Working
   - `storePersistentFirstLineColor()` / `getPersistentFirstLineColor()`
   - Cross-context communication via `globalThis`
   - KidLisp fallback system that correctly detects "purple" during reframe

2. **KidLisp onReframe Integration** ✅ Working (but ineffective)
   - `globalKidLispInstance.onReframe()` gets called successfully
   - `globalThis.$paintApiUnwrapped.wipe()` executes without errors
   - Logs confirm background color detection and wipe execution

3. **Direct Pixel Manipulation** ❌ Failed
   - Attempts to directly modify `screen.pixels` buffer
   - Buffer exists and can be modified but changes don't appear visually
   - Indicates wrong buffer being targeted

## Key Investigation Findings

### Console Logs Analysis
```
🎨 onReframe: this.firstLineColor = purple ✅
🎨 Re-wiping background after reframe: purple ✅  
🎨 Using globalThis.$paintApiUnwrapped.wipe ✅
```

**Problem**: Despite successful execution, expanded canvas areas remain transparent.

### Buffer Architecture Issue
- KidLisp uses a separate rendering buffer system
- `wipe()` calls during reframe don't target the correct visual buffer
- Timing issues may prevent proper buffer updates

## Reproduction Steps

1. Navigate to `http://localhost:8888/kidlisp`
2. Enter KidLisp code: `wipe "purple"`
3. Resize browser window to expand canvas
4. **Expected**: Purple background extends to all areas
5. **Actual**: Original area remains purple, expanded areas are transparent

## Technical Deep Dive Required

### 1. Buffer Architecture Investigation
- **Research Question**: What is the actual rendering buffer used by KidLisp?
- **Investigation**: Trace the rendering pipeline from `wipe()` call to visual output
- **Key Files**: 
  - `/system/public/aesthetic.computer/lib/kidlisp.mjs` (KidLisp rendering)
  - `/system/public/aesthetic.computer/lib/disk.mjs` (Graphics engine)
  - Graphics/canvas management systems

### 2. Reframe Timing Analysis  
- **Research Question**: Are wipe operations being overwritten by subsequent operations?
- **Investigation**: Add comprehensive logging to track all rendering operations during reframe
- **Hypothesis**: Canvas expansion might clear buffers after our wipe operations

### 3. Alternative Rendering Paths
- **Research Question**: Can we trigger full KidLisp re-execution instead of manual wipe?
- **Investigation**: Test forcing complete piece reload during reframe
- **Approach**: Modify reframe handling to re-run entire KidLisp boot sequence

## Proposed Solutions (In Priority Order)

### Solution 1: Canvas Expansion with Background Preservation
**Concept**: Modify the canvas expansion logic to sample existing background color and extend it to new areas
- **Pros**: Works at the graphics engine level, affects all pieces
- **Cons**: Requires deep canvas/WebGL modifications
- **Implementation**: Modify canvas resize logic to preserve and extend existing background

### Solution 2: Full KidLisp Re-execution on Reframe
**Concept**: Instead of manual wipe operations, trigger complete KidLisp piece re-execution after reframe
- **Pros**: Uses existing, proven KidLisp rendering pipeline
- **Cons**: Performance impact, may reset piece state
- **Implementation**: Call full piece reload in reframe timeout instead of just `onReframe()`

### Solution 3: Background-Aware Buffer Management
**Concept**: Identify and use the correct KidLisp rendering buffer for background operations
- **Pros**: Surgical fix, maintains performance
- **Cons**: Requires understanding complex buffer architecture
- **Implementation**: Deep investigation of KidLisp's actual rendering target

### Solution 4: Pre-expansion Background Detection
**Concept**: Before canvas expansion, detect the dominant background color and apply it to new areas
- **Pros**: Works for any piece, not just KidLisp
- **Cons**: Color detection complexity, edge cases
- **Implementation**: Sample pixels from existing canvas edges and extend pattern

## Development Environment

- **System**: Fedora Linux 42 (Container Image) in dev container
- **Server**: `http://localhost:8888` 
- **Test URL**: `http://localhost:8888/kidlisp`
- **Browser**: Console logging enabled for debugging
- **Key Files Modified**:
  - `kidlisp.mjs`: Added fallback system, onReframe handling, persistent storage
  - `disk.mjs`: Added global function exposure, reframe timeout logic

## Next Steps for Implementation

1. **Deep Buffer Investigation**: Use browser dev tools to identify actual KidLisp rendering target
2. **Timing Analysis**: Add frame-by-frame logging to understand operation sequence
3. **Alternative Approach Testing**: Try full piece re-execution instead of targeted wipe operations
4. **Graphics Engine Analysis**: Understand the relationship between different buffer systems

## Current Code Status

**Working Components**:
- ✅ Background color detection and persistence across contexts
- ✅ KidLisp fallback system for retrieving colors during reframe
- ✅ Global function exposure and cross-worker communication
- ✅ Reframe event handling and timeout logic

**Non-Working Components**:
- ❌ Visual background extension to expanded canvas areas
- ❌ Correct buffer targeting for wipe operations
- ❌ Effective coordination between buffer systems

## Success Criteria

The feature will be considered complete when:
1. A KidLisp piece with `wipe "purple"` maintains purple background in all areas after window resize
2. The solution works for all colors, not just purple
3. Performance impact is minimal
4. The solution doesn't break existing KidLisp functionality
5. The fix works consistently across different resize scenarios (width, height, both)

---

**Created**: September 23, 2025  
**Status**: Investigation Required  
**Priority**: Medium - affects user experience but has workarounds  
**Complexity**: High - requires deep understanding of multi-buffer rendering architecture