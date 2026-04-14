# Arena.mjs Graphics & Framework Error Report

**Date**: 2026-04-14  
**Status**: 🔴 BLOCKING - Graphics not rendering, framework messaging error  
**Severity**: High

---

## Summary

The `arena.mjs` piece partially runs but exhibits two critical issues:
1. **No graphics rendering** - The 3D scene, ground plane, and mobile buttons don't display
2. **Framework messaging error** - Persistent `ReferenceError: data is not defined` in `bios.mjs:11603` in the `receivedChange` handler

The piece boots successfully, responds to input (gamepad/keyboard), and executes paint/sim functions, but the graphics pipeline appears broken.

---

## Error Details

### Error Stack Trace
```
bios.mjs:11603 Uncaught (in promise) ReferenceError: data is not defined
    at receivedChange (bios.mjs:11603:27)
    at onMessage (bios.mjs:3928:5)
    at worker.onmessage (bios.mjs:4148:11)
```

### Observations
- Error occurs repeatedly during piece execution
- Error happens AFTER "Proxy set up" message (piece has booted)
- Error is in framework's message handler, not piece code
- Suggests malformed message being sent from piece to main thread
- Try-catch in paint function catches no errors
- Commented-out large console.log didn't fix the issue

---

## Changes Made (Attempted Fixes)

### 1. Mobile Button Implementation
- ✅ Created color-coded buttons (green jump, orange crouch, blue directionals)
- ✅ Made action buttons wider (56px vs 32px)
- ✅ Added arrow characters (↑↓←→) for directionals
- ✅ Implemented touch/click and keyboard responsiveness
- ❌ Buttons don't render to screen

### 2. Graphics Buffer Approach
- Attempted to use `painting()` constructor to pre-bake button icons
- Discovered `painting` not available in boot scope despite usage in `1v1.mjs`
- Reverted to simple text-based button rendering
- ❌ Did not resolve graphics issue

### 3. Error Isolation
- Removed large console.log JSON snapshot → error persists
- Added try-catch wrapper around paint function → no caught errors
- Removed buffer/paste references → error persists
- ❌ None resolved the bios.mjs error

---

## Root Cause Analysis

### Hypothesis 1: Graphics Pipeline Broken
The `wipe`, `ink`, `box`, `line`, `write` functions may not be working as expected, or the painting context is not initialized properly. However:
- No errors caught in paint function
- Piece executes paint code without exceptions
- Mobile buttons have no text but still don't display

### Hypothesis 2: Message Serialization Error
The framework's `receivedChange` handler receives a malformed message where `data` is undefined. This could be triggered by:
- Piece sending invalid state/snapshot format
- Framework expecting specific message structure that arena.mjs violates
- Uninitialized variable in piece being sent back to main thread

### Hypothesis 3: Incompatible with FPS System
The arena.mjs forks from `fps.mjs` and uses `system: "fps"` export. The fps system might have specific requirements about:
- What can be logged/sent
- How graphics are rendered
- Message format expectations

---

## Files Modified

- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/arena.mjs`
  - Mobile button setup (lines ~310-335)
  - Button rendering in paint (lines ~1240-1250)
  - Try-catch wrapper in paint (lines ~885-1226)
  - Removed buffer graphics creation code
  - Commented out snapshot console.log

---

## Next Steps / Recommendations

### Immediate
1. **Compare with fps.mjs** - Check how `fps.mjs` handles graphics rendering vs arena.mjs
2. **Check system.nopaint** - Investigate if arena needs to use `system.nopaint.buffer` pattern instead of direct drawing
3. **Review FPS system requirements** - Verify what the fps system expects from pieces

### Investigation
1. Add explicit error reporting in paint function
2. Check if graphics APIs (wipe, ink, box, line) are actually available
3. Inspect what message is being sent that causes `receivedChange` to fail
4. Test with minimal piece that just calls `wipe()` to verify graphics work at all

### Alternative Approaches
1. Test if issue is specific to arena.mjs or affects all fps-based pieces
2. Try reverting arena.mjs to original fps.mjs to confirm it displays graphics
3. Check if mobile button implementation is interfering with graphics rendering

---

## Test Case

To reproduce:
1. Navigate to `aesthetic.computer/arena`
2. Observe: No 3D scene, no ground plane, no buttons visible
3. Observe: Console shows repeated `data is not defined` error
4. Input still works: Keyboard/gamepad commands are processed
5. Game logic runs: Internally piece updates state correctly

---

## Attachments

- Modified file: `arena.mjs` (current state with buttons and try-catch)
- Related files: `fps.mjs` (parent), `system/public/aesthetic.computer/lib/disk.mjs` (graphics API)

---

## Questions for Investigation

1. Why does `painting()` work in `1v1.mjs` but appears undefined in arena.mjs?
2. What format does `receivedChange` in bios.mjs expect for `data`?
3. Are graphics APIs available in paint context for fps system pieces?
4. Does the nopaint system work differently than direct graphics calls?

---

*Report compiled during arena.mjs development session. Graphics pipeline and framework messaging require deeper investigation into the AC graphics/messaging architecture.*
