# TV FYP (For You Page) Implementation

**Date**: 2025-11-11  
**Status**: 🔧 IN PROGRESS - AV Sync & Multi-Tape Refinement  
**File**: `system/public/aesthetic.computer/disks/tv.mjs`

---

## 📊 Current State

### ✅ Phase 1: Core Architecture (COMPLETED)
- **TapeManager Integration**: Multi-tape playback with independent render loop
- **Vertical Scrolling**: TikTok-style swipe navigation between tapes
- **Gesture Commitment**: 10px threshold determines horizontal (scrub) vs vertical (scroll)
- **Loading System**: Per-tape loading state with spinner UI
- **Boundary Handling**: Graceful snapping at start/end of feed

### ✅ Phase 2: Horizontal Scrubbing (COMPLETED)
- **Scrub Module**: `disks/common/scrub.mjs` - Physics-based lazy needle
  - Spring force (stiffness=0.15) + damping (0.85)
  - Inertia drift after release
  - Lazy needle follows target with lag
- **Gesture Detection**: Fixed - only set gestureStart on initial touch
- **BIOS Integration**: `tape:seek` handler for seeking during scrub
- **Visual Feedback**: Target/needle lines during active scrub

### ✅ Phase 3: Playback State Management (COMPLETED)
- **Per-Tape State**: TapeManager tracks play/pause per tape
- **Smart Transitions**: `setActive()` pauses old, resumes new automatically
- **Auto-Play on Load**: New tapes auto-play if audio enabled
- **Aspect Ratio**: Per-frame calculation based on video dimensions

### 🔧 Phase 4: AV Sync & Performance (IN PROGRESS)
**Current Issues:**
1. **Seek Command Spam**: Throttled to 0.2% threshold but still some jank
2. **Progress Message Spam**: Getting hundreds of `tape:playback-progress` per second
3. **Multi-Tape Loading**: Need to test with 2+ tapes in feed
4. **Scrubbing Smoothness**: Physics runs in `sim()` but seek throttling may affect sync

**Recent Changes:**
- Added `sim()` function to run physics every frame
- Throttled seek commands (only when needle moves >0.2%)
- Updated currentProgress from needle position for progress bar
- Auto-stop inertia when velocity < 0.0001

---

## 🎯 Architecture

### Files Structure
```
/disks/
  tv.mjs                    # Main FYP piece (920 lines)
  common/scrub.mjs          # Reusable scrubbing module (159 lines)
  
/bios.mjs
  class Tape                # Individual tape with frames/audio
  class TapeManager         # Multi-tape orchestrator
  - renderFrame()           # Composite all tapes to underlay
  - setActive()             # Switch active tape (handles pause/resume)
```

### Message Flow
```
tv.mjs                    →  BIOS
┌─────────────────────────────────────────┐
│ USER GESTURE                            │
├─────────────────────────────────────────┤
│ Horizontal drag (>10px)                 │
│   → gestureMode = "scrub"               │
│   → scrubber.start()                    │
│   → scrubber.drag() [every frame]       │
│   → tape:seek (throttled >0.2%)         │  → Tape.seekToFrame()
│                                         │
│ Vertical swipe (>30px)                  │
│   → gestureMode = "scroll"              │
│   → targetIndex++/--                    │
│   → tape:set-active                     │  → TapeManager.setActive()
│                                         │     - Pauses old tape
│                                         │     - Resumes new tape
│ Tap (no drag)                           │
│   → tape:toggle-play                    │  → Tape.play() / pause()
└─────────────────────────────────────────┘

BIOS                      →  tv.mjs
┌─────────────────────────────────────────┐
│ tape:playback-progress [60fps]          │
│   → currentProgress = content.progress  │
│   → isPlaying = content.isPlaying       │
│   → rec.tapeProgress (for VHS bar)      │
│                                         │
│ tape:load-progress                      │
│   → loadingTapes.set(tapeId, state)    │
│   → Show spinner if phase=downloading   │
│                                         │
│ tape:load-progress (phase=complete)     │
│   → loadingTapes.delete(tapeId)        │
│   → preloadedTapeIds.add(tapeId)       │
│   → tape:set-active (if current)       │
│   → tape:toggle-play (if audio on)     │
└─────────────────────────────────────────┘
```

### State Variables (tv.mjs)
```javascript
// Feed state
tapes = []                    // Array of tape metadata
currentIndex = 0              // Current tape position in feed
targetIndex = 0               // Where we're scrolling to
displayIndex = 0              // Fractional position (smooth scroll)

// Gesture state
gestureMode = null            // null | "scrub" | "scroll"
gestureStartX/Y = 0           // Initial touch position

// Scrubber state
scrubber = createScrubber()   // Physics module instance
wasPlayingBeforeScrub = false
lastSeekProgress = 0          // For throttling
seekThreshold = 0.002         // 0.2% minimum movement

// Loading state
loadingTapes = Map            // tapeId → {progress, phase, code}
preloadedTapeIds = Set        // Successfully loaded tape IDs
firstTapeReady = false        // Clears loading screen

// Playback state
isPlaying = false
isPaused = false
currentProgress = 0           // From tape:playback-progress
```

---

## 🐛 Known Issues

### 1. Progress Message Spam (CRITICAL)
**Symptom:** Hundreds of identical `tape:playback-progress` messages
**Example:**
```
tape:playback-progress {progress: 0.462, frameIndex: 442, isPlaying: false}
[... repeated 100+ times with same values ...]
```

**Possible Causes:**
- TapeManager sending on every frame even when paused?
- Message not being filtered/throttled in BIOS?
- Multiple event listeners registered?

**Impact:** Performance degradation, console spam

### 2. AV Sync Jank
**Symptom:** Video/audio slightly out of sync during scrubbing
**Possible Causes:**
- Seek throttling (0.2%) creates small gaps
- Physics simulation lag between target and needle
- Audio doesn't follow needle position (no pitch shifting yet)

**Comparison to video.mjs:**
- `video.mjs` sends seek EVERY frame (no throttling)
- We throttle to reduce spam but may lose sync

### 3. Multi-Tape Testing Needed
**Current:** Only loading first tape (`loadTapeAtIndex(0)`)
**Disabled:** Preload queue system
**Need to:**
- Re-enable preload for tapes ±1 from current
- Test swipe transitions between 2+ tapes
- Verify loading states don't conflict

---

## 🎯 Next Steps

### Immediate (Fix Console Spam)
1. **Reduce Logging**: Remove/comment excessive console.logs
   - Keep: Errors, state changes, user actions
   - Remove: Per-frame updates, progress spam, receive logs
2. **Investigate Progress Spam**: Why so many identical messages?
   - Check TapeManager renderFrame frequency
   - Look for duplicate event handlers
   - Consider throttling in BIOS or disk.mjs

### Short Term (AV Sync)
1. **Test Different Seek Thresholds**:
   - Current: 0.002 (0.2%)
   - Try: 0.001 (0.1%) for tighter sync
   - Or: Remove throttling entirely (like video.mjs)
2. **Add Audio Pitch Shifting**:
   - Scrubber already calculates `getPitchShift()`
   - Need to send `tape:audio-shift` during scrub
   - Match video.mjs implementation
3. **Measure Actual Lag**:
   - Log `target - needle` delta
   - Tune spring/damping constants

### Medium Term (Multi-Tape)
1. **Re-enable Preload**: Load tapes ±1 from current
2. **Test Transitions**: Swipe while previous tape loading
3. **Loading UI**: Show which tapes are loading in queue
4. **Memory Management**: Unload tapes far from current

### Long Term (Polish)
1. **Waveform Overlay**: During scrub (like video.mjs)
2. **Smooth Audio Crossfade**: Between tape transitions
3. **Infinite Scroll**: Fetch more tapes when near end
4. **Feed Pagination**: Load 50 tapes, then fetch next page

---

## 📝 Implementation Notes

### Scrubbing Physics (scrub.mjs)
```javascript
// Spring force (exponential for stronger catch-up)
const displacement = targetProgress - needleProgress;
const springForce = displacement * stiffness; // 0.15

// Integrate velocity
needleVelocity += springForce;
needleVelocity *= damping; // 0.85

// Update position
needleProgress += needleVelocity;
```

**Tuning:**
- Higher `stiffness` = faster catch-up (snappier)
- Higher `damping` = less oscillation (smoother)
- Current values feel good but may need tweaking for AV sync

### Seek Throttling
```javascript
// Only seek if needle moved significantly
const progressDelta = Math.abs(scrubber.needleProgress - lastSeekProgress);
if (progressDelta > seekThreshold) {
  globalSend({ type: "tape:seek", content: { tapeId, progress: needle } });
  lastSeekProgress = needle;
}
```

**Trade-off:**
- More throttling = less spam, worse sync
- Less throttling = better sync, more spam
- Need to find sweet spot or match video.mjs (no throttling)

### Loading State Machine
```
INITIAL → FETCHING → DOWNLOADING → COMPLETE
  ↓         ↓            ↓            ↓
 null   /api/get-tape  tape:preload  tape:set-active
                                      firstTapeReady=true
```

**Edge Cases:**
- User swipes away while loading → tape loads in background
- User swipes back → check if already loaded
- Multiple tapes loading → track in Map, show queue UI

---

## 🔍 Debug Commands

```javascript
// Check current state
console.log({
  currentIndex,
  targetIndex,
  tapes: tapes.length,
  loading: loadingTapes.size,
  preloaded: preloadedTapeIds.size,
  gestureMode,
  scrubbing: scrubber.isScrubbing,
  inertia: scrubber.inertiaActive
});

// Check scrubber state
console.log({
  target: scrubber.targetProgress,
  needle: scrubber.needleProgress,
  velocity: scrubber.needleVelocity,
  lag: scrubber.targetProgress - scrubber.needleProgress
});
```

---

## 📚 References

- Original discussion: Refactor tv.mjs to notepat-tv.mjs, create FYP-style tv.mjs
- Scrubbing inspiration: video.mjs STAMPLE-style physics
- TapeManager: BIOS lines 51-350
- Gesture commitment: 10px threshold (industry standard for drag detection)
