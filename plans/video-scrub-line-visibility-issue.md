````markdown
# Video Scrub Line Visibility & Physics Issue

**Date**: 2025-11-05  
**Status**: 🔧 IN PROGRESS - Physics Refinement Phase  
**File**: `system/public/aesthetic.computer/disks/video.mjs`

---

## 📊 Current State

### ✅ Phase 1: Line Rendering (COMPLETED)
- **Root Cause:** `line()` API doesn't accept thickness parameter (5th arg silently ignored)
- **Solution:** Use `box()` API to draw thick vertical bars
- **Result:** Yellow target line and cyan needle line now visible at 1px width

### ✅ Phase 2: STAMPLE-style Dual Lines (COMPLETED)
- Yellow line shows drag position (target)
- Cyan line shows actual playback position (needle)
- White connecting line shows lag distance
- Lines drawn in paint() function (~710-735)

### ✅ Phase 3: Basic Physics System (COMPLETED)
- Unified sim() physics loop for scrubbing and inertia
- Velocity-based motion with acceleration
- Exponential spring force (quadratic)
- Edge bounce with overshoot
- Physics runs continuously in sim() (~800-870)

### 🔧 Phase 4: Smoothness & Feel (IN PROGRESS)

**Current Issues:**
- Motion feels jittery, not smooth
- Lacks classic "Asteroids drift" feel
- Physics may be inconsistent between frames
- Need proper spring-mass-damper system

**Target Feel:**
- Smooth, continuous motion like floating through space
- Momentum builds naturally with sustained drag
- Long, smooth drift after release with gradual slowdown
- No jitter or stuttering - buttery smooth
- Like STAMPLE or classic Asteroids ship physics

---

## 🎯 Next Steps

1. **Refine Physics Integration**
   - Ensure physics runs at consistent rate independent of drag events
   - Consider fixed timestep physics loop
   - Use proper velocity integration (Verlet or RK4)

2. **Tune Spring-Mass-Damper System**
   - Find ideal spring constant for smooth follow
   - Adjust damping for that classic space drift
   - Test momentum accumulation during fast drags

3. **Improve Frame Consistency**
   - Decouple physics from visual updates
   - Test at different frame rates
   - Ensure smooth interpolation

4. **Reference Implementation**
   - Study STAMPLE's smooth motion code
   - Look at classic Asteroids physics
   - Apply lessons learned

---

## 📐 Technical Details

### Current Physics Constants
```javascript
springStrength = 0.08;         // Base spring force (exponential)
acceleration = 0.6;             // Velocity response rate  
velocityScale = 0.3;            // Motion speed (30% of normal)
friction = 0.88 (scrubbing);    // Dampening while dragging
friction = 0.98 (inertia);      // Dampening during drift (98% retained)
overshootLimit = 0.08;          // Edge bounce (8%)
bounceReflection = -0.5;        // Edge bounce strength
```

### Physics Formula (Current)
```javascript
// Exponential spring force - gets stronger with distance
lag = targetProgress - needleProgress;
lagSquared = lag * Math.abs(lag);  // Quadratic but keeps sign
targetVelocity = lagSquared * springStrength;

// Smooth acceleration
needleVelocity += (targetVelocity - needleVelocity) * acceleration;

// Apply motion (scaled for slower scrubbing)
needleProgress += needleVelocity * velocityScale;

// Apply friction
needleVelocity *= friction;
```

### Key Variables
- `targetProgress` - Where user is dragging (yellow line, 0-1)
- `needleProgress` - Where video is playing (cyan line, 0-1)
- `needleVelocity` - Current velocity of needle (progress units per frame)
- `isScrubbing` - True during drag
- `inertiaActive` - True during drift after release

---

## 🔍 Issue Analysis

### Why It Feels Jittery
1. **Inconsistent physics timing** - May be frame-rate dependent
2. **Abrupt force transitions** - Need smoother curves
3. **Missing interpolation** - Position updates may be too discrete
4. **Event-based updates** - Should be purely time-based in sim()

### What "Smooth Asteroids Drift" Means
- **Continuous simulation** - Physics runs every frame, not just on events
- **Smooth integration** - Position/velocity updated with proper calculus
- **Consistent timestep** - Same physics rate regardless of frame rate
- **Natural damping** - Gradual slowdown, not sudden stops
- **Momentum preservation** - Fast movements build up lasting velocity

---

## 📝 Code Locations

### 1. Paint Function (Line Rendering)
**Location:** `video.mjs` ~710-735  
**Purpose:** Draw yellow target line and cyan needle line
```javascript
// Yellow line - where you're dragging
ink(...scrubColor).box(targetX, 0, 1, screen.height);

// Cyan line - where video is playing
ink(...needleColor).box(needleX, 0, 1, screen.height);

// White connecting line - shows lag distance
if (isScrubbing && Math.abs(targetX - needleX) > 2) {
  ink(255, 255, 255, 128).line(targetX, h/2, needleX, h/2);
}
```

### 2. Sim Function (Physics Loop)
**Location:** `video.mjs` ~800-870  
**Purpose:** Run physics simulation every frame
```javascript
if ((isScrubbing || inertiaActive) && rec?.presenting) {
  // Calculate exponential spring force
  lag = targetProgress - needleProgress;
  lagSquared = lag * Math.abs(lag);
  targetVelocity = lagSquared * springStrength;
  
  // Integrate velocity
  needleVelocity += (targetVelocity - needleVelocity) * acceleration;
  needleProgress += needleVelocity * velocityScale;
  needleVelocity *= friction;
  
  // Seek video and update audio pitch
  send({ type: "recorder:present:seek", content: needleProgress });
  send({ type: "tape:audio-shift", content: pitchShift });
}
```

### 3. Act Function (Input Handling)
**Location:** `video.mjs` ~1895-1920  
**Purpose:** Handle drag events
```javascript
// Start scrub
if (e.is("draw") && !isScrubbing) {
  isScrubbing = true;
  targetProgress = needleProgress = rec.presentProgress;
  needleVelocity = 0;
}

// During drag - update target only
if (e.drag && isScrubbing) {
  targetProgress = scrubStartProgress + (deltaX / screen.width);
}

// Release - activate inertia
if (e.is("lift") && isScrubbing) {
  isScrubbing = false;
  inertiaActive = true;  // Let physics continue drifting
}
```

---

## 📚 Evolution History

1. **Original Problem** - Lines invisible (line() API limitation)
2. **First Fix** - Use box() for visible lines
3. **STAMPLE Inspiration** - Add dual lines (target + needle)
4. **Basic Lazy Follow** - Simple interpolation with lag
5. **Velocity Physics** - Add velocity and acceleration
6. **Exponential Spring** - Quadratic force for rubber band feel
7. **Current** - Need smoother, more consistent simulation

---

## 🎮 Design Goals

### Feel Targets
- [ ] Smooth as butter, no jitter
- [ ] Builds momentum with fast drags
- [ ] Long drift after throwing it
- [ ] Feels weighted and intentional
- [ ] Like sliding pucks on air hockey table
- [ ] Classic Asteroids ship inertia

### Technical Targets
- [ ] Physics runs at consistent rate
- [ ] Frame-rate independent
- [ ] Proper numerical integration
- [ ] Smooth force curves
- [ ] Predictable behavior

---

## 💡 Ideas for Next Session

1. **Fixed Timestep Loop**
   - Run physics at fixed 60Hz or 120Hz
   - Interpolate visuals between physics steps
   - Guarantees consistent simulation

2. **Better Integration Method**
   - Current: Euler integration (simple but can drift)
   - Consider: Verlet integration (stable, energy-preserving)
   - Consider: RK4 (very accurate)

3. **Separate Physics Thread**
   - Decouple physics from rendering
   - Run physics in sim() independent of paint()
   - Smoother updates

4. **Study STAMPLE Implementation**
   - Look at stample.mjs lines ~200-210 for motion code
   - See how orange/blue lines move smoothly
   - Copy the feel

5. **Momentum Accumulation**
   - Track drag velocity during scrubbing
   - Transfer to needle velocity on release
   - Faster drag = more throw distance

---

**Priority:** HIGH - Core scrubbing feature needs polish  
**Effort:** Medium - Physics tuning is iterative  
**Impact:** High - Makes scrubbing feel professional and satisfying

````

## 🎯 Solution Summary

**Root Cause:** The `line()` function **does NOT accept a thickness/width parameter**! The code calls:
```javascript
ink(...scrubColor).line(progressX, 0, progressX, screen.height, scrubLineWidth);
                                                                  ^^^^^^^^^^^^^ IGNORED!
```

The `line()` function in `graph.mjs` only accepts coordinates (1, 2, or 4 arguments). The 5th parameter (`scrubLineWidth = 8`) is **silently ignored**, resulting in a 1-pixel wide line that's nearly invisible.

**Fix:** Use `box()` to draw thick vertical rectangles instead of `line()`, or use `pline()` which DOES support thickness.

**Quick Fix:** Replace `line()` calls with `box()` for thick vertical bars.

---

## Problem Summary

Vertical scrub lines are **being drawn** but are **NOT VISIBLE** during video scrubbing. Console logs confirm the `line()` function is being called with correct coordinates.

## ROOT CAUSE IDENTIFIED ✅

**The `line()` function doesn't accept a width/thickness parameter!**

From `graph.mjs` line 2253:
```javascript
function line() {
  let x0, y0, x1, y1;
  if (arguments.length === 1) {
    // handles { x0, y0, x1, y1 }
  } else if (arguments.length === 4) {
    // handles x0, y0, x1, y1  ← ONLY THESE ARGUMENTS!
  } else if (arguments.length === 2) {
    // handles [x,y], [x,y] or {x,y}, {x,y}
  }
  // ... no thickness parameter handling!
}
```

The code in `video.mjs` (line 714-729) calls:
```javascript
const scrubLineWidth = 8;
ink(...scrubColor).line(
  progressX,      // x0
  0,              // y0  
  progressX,      // x1
  screen.height,  // y1
  scrubLineWidth  // ← 5TH PARAMETER IS IGNORED!
);
```

**Result:** A 1-pixel wide vertical line is drawn, which is nearly invisible on screen!

## The Solution

Use `box()` instead of `line()` to draw thick vertical rectangles:

```javascript
// OLD (doesn't work):
ink(...scrubColor).line(progressX, 0, progressX, screen.height, scrubLineWidth);

// NEW (works!):
const scrubLineWidth = 8;
ink(...scrubColor).box(
  progressX - Math.floor(scrubLineWidth / 2),  // x (center the box)
  0,                                            // y
  scrubLineWidth,                               // width
  screen.height                                 // height
);
```

Alternative: Use `pline()` which DOES support thickness, but requires array of coordinates.

## Technical Details

### Why `line()` Doesn't Work
- **graph.mjs line 2253**: `function line()` only accepts 1, 2, or 4 arguments for coordinates
- No thickness/width parameter exists
- Extra arguments are silently ignored
- Results in 1-pixel wide line (nearly invisible)

### APIs That Support Thickness
- `pline(coords, thickness)` - polyline with thickness support
- `box(x, y, w, h)` - filled rectangle (perfect for thick vertical/horizontal bars)
- `circle(x, y, radius, filled, thickness)` - circles with thickness
- `oval(x, y, w, h, filled, thickness)` - ovals with thickness

### Current Drawing Code (video.mjs ~line 710-730)
```javascript
// THIS CODE DOESN'T WORK:
const scrubLineWidth = 8;
ink(...scrubColor).line(progressX, 0, progressX, screen.height, scrubLineWidth);
                                                  // ^^^^^^^^^ 5th param ignored!
// Draws 1-pixel wide line, barely visible
```

### Evidence From Logs
Console logs show correct drawing attempts:
- X: 23-168px (valid for 332px width)
- Y: 0-230px (full height)
- Color: [255,255,0,255] (bright yellow)
- **Width: 8px** ← This parameter is IGNORED by line()!

### What Works ✅
- Scrubbing mechanics (velocity, inertia)
- Frame seeking  
- Audio pitch shifting
- Waveform visualization
- Progress text
- VHS progress bar
- **Everything except the vertical line thickness!**

---

## Summary

**Problem:** Scrub lines invisible  
**Root Cause:** `line()` doesn't accept width parameter (5th argument ignored)  
**Solution:** Use `box()` instead of `line()`  
**Result:** 8-pixel wide yellow/cyan bars now visible during scrubbing! ✅

**One-line fix:**
```javascript
// Change from:
ink(255,255,0,255).line(x, 0, x, screen.height, 8);

// To:
ink(255,255,0,255).box(x-4, 0, 8, screen.height);
```

## Implementation

### Code Fix in video.mjs (around line 710-730)

Replace the current `line()` calls with `box()` calls:

```javascript
// Draw vertical lines for scrub and playback positions

if ((isScrubbing || inertiaActive) && rec?.presenting) {
  const currentProgress = isScrubbing 
    ? Math.max(0, Math.min(1, scrubStartProgress + (scrubAccumulatedDelta / screen.width)))
    : (rec.presentProgress || 0);
  
  const progressX = Math.floor(currentProgress * screen.width);
  
  // 1. Draw current scrub/drag position (yellow bar)
  const scrubLineWidth = 8;
  const scrubColor = [255, 255, 0, 255];
  
  console.log(`📍 Drawing scrub line: x=${progressX}, y1=0, y2=${screen.height}, color=[${scrubColor}], progress=${currentProgress.toFixed(3)}`);
  
  ink(...scrubColor).box(
    progressX - Math.floor(scrubLineWidth / 2),  // Center the bar on progressX
    0,
    scrubLineWidth,
    screen.height
  );
  
  // 2. Draw actual playback needle (cyan bar during inertia)
  if (inertiaActive && rec?.presentProgress !== undefined) {
    const actualProgress = rec.presentProgress;
    const needleX = Math.floor(actualProgress * screen.width);
    
    console.log(`📍 Drawing needle line: x=${needleX}, y1=0, y2=${screen.height}, actualProgress=${actualProgress.toFixed(3)}`);
    
    ink(0, 255, 255, 255).box(
      needleX - Math.floor(scrubLineWidth / 2),
      0,
      scrubLineWidth,
      screen.height
    );
  }
  
  // Draw progress info at top corners (keep existing text code)
  // ...
}
```

That's it! No overlay system needed, no z-index changes - just use the correct API.

## Priority

**HIGH** - Core feature blocker. Scrubbing works mechanically but lacks visual feedback, making it hard to use.
