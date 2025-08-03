# Pen Handling Regression Analysis Report

**Date**: August 2, 2025  
**Issue**: Touch/pen handling regressions on iPhone affecting brushes like 'rect.mjs', multi-tap on 'notepat', and button sticking in 'toss'  

## Executive Summary

The pen handling regressions can be traced to **commit a36b733a** ("ui pen updates (prompt, stample, toss)") from June 27, 2025. This commit introduced a complex new button interaction system with drag-between-buttons behavior, sticky scrubbing, and rollover activation that fundamentally changed how touch events are processed and coordinated between buttons.

## Root Cause Analysis

### Primary Issue: Overly Complex Button Interaction System

The June 27 commit introduced several new button behavior flags and a global `activeButtons` Set that tracks button states across the entire UI system:

```javascript
// New global state tracking
const activeButtons = new Set(); // Track which buttons are currently active

// New button properties added:
noRolloverActivation = false; // Prevent activation via rollover
stickyScrubbing = false; // Prevent rollover activation when scrubbing  
offScreenScrubbing = false; // Allow scrubbing off-screen
```

### Specific Problems Identified

#### 1. **Button State Interference**

The new system creates complex interdependencies between buttons:

```javascript
// From ui.mjs - problematic rollover logic
if (isDraggingFromOtherButton && !hasStickyButton && !btn.noRolloverActivation) {
  // Deactivate all other buttons first
  for (const otherBtn of activeButtons) {
    if (otherBtn !== btn) {
      otherBtn.down = false;
      otherBtn.over = false;
      otherBtn.actions?.up?.(otherBtn);
      activeButtons.delete(otherBtn);
    }
  }
}
```

This creates a cascade effect where touching one button can affect the state of all other buttons, potentially interfering with normal drawing operations.

#### 2. **Edge Detection System Issues**

A new "edge detection cancellation" system was added:

```javascript
// Edge detection that cancels button interactions
window.addEventListener("pointerleave", function (e) {
  if (window.acSEND) {
    window.acSEND({ type: "ui:cancel-interactions", content: {} });
  }
});
```

This system can incorrectly cancel legitimate drawing operations when the pen moves near screen edges - a common occurrence during normal drawing.

#### 3. **Multi-Touch Coordination Problems**

The new system tracks multiple pointers but creates confusion about which pointer "owns" which button:

```javascript
// Problematic pointer ownership logic
if (e.pointer === btn.downPointer || 
    (inActiveButtons && !btn.stickyScrubbing) || 
    (btn.stickyScrubbing && btn.down)) {
  callbacks.scrub?.(btn);
}
```

This can interfere with multi-touch gestures in pieces like 'notepat'.

#### 4. **Coordinate System Disruption**

The system adds timing delays and state checks that can disrupt the precise coordinate tracking needed for brushes:

```javascript
// Adds artificial delays that disrupt drawing timing
btn._justProcessed = true;
btn._lastProcessedTime = Date.now();
setTimeout(() => {
  btn._justProcessed = false;
}, 250); // 250ms delay can disrupt fast drawing gestures
```

## Evidence from Git History

### Key Commits

1. **a36b733a** (June 27, 2025) - "ui pen updates (prompt, stample, toss)"
   - **This is the primary regression source**
   - Introduced complex button interaction system
   - Added global activeButtons tracking
   - Added sticky/rollover behaviors

2. **17e1c8cd** (June 27, 2025) - "emacs improvements and prompt button/touch handle improvements"  
   - Added edge detection cancellation system
   - **Secondary regression source**

3. **b529a250** (August 2, 2025) - "better cursor rendering on tapes"
   - Recent changes to cursor handling
   - May have compounded existing issues

### Pre-Regression State

Before commit a36b733a, the button system was much simpler:

- Buttons operated independently without global state coordination
- No complex rollover/sticky behaviors
- No edge detection cancellation system
- Direct, predictable touch event handling

## Impact Assessment

### Affected Components

1. **Drawing Brushes** (like 'rect.mjs')
   - Coordinates may be interrupted by button state changes
   - Edge detection can cancel mid-stroke
   - Timing delays disrupt smooth drawing

2. **Multi-Touch Pieces** (like 'notepat')
   - Complex pointer ownership logic interferes with multi-finger gestures
   - Global button state affects independent touch points

3. **Touch-Sensitive UI** (like 'toss' buttons)
   - Buttons "stick" due to complex activation/deactivation logic
   - Rollover behaviors create unexpected interactions

### Platform-Specific Issues (iPhone)

iPhone touch handling is particularly affected because:

- iOS touch events are more sensitive to timing disruptions
- Edge detection is more aggressive on mobile Safari
- Multi-touch coordination is more critical on touch-only devices

## Recommended Fix Strategy

### Immediate Solution: Simplify Button System

1. **Remove Global Button Coordination**
   - Eliminate the `activeButtons` Set
   - Return to independent button operation
   - Remove rollover activation between buttons

2. **Disable Edge Detection Cancellation**
   - Remove or make optional the "ui:cancel-interactions" system
   - Allow drawing operations to continue normally near edges

3. **Restore Simple Touch Handling**
   - Remove timing delays (`_justProcessed` logic)
   - Simplify pointer ownership to direct 1:1 mapping
   - Remove sticky/rollover behavior flags

### Implementation Steps

1. **Revert Core Button Logic** (Priority 1)
   ```javascript
   // Remove from ui.mjs:
   const activeButtons = new Set();
   
   // Remove complex rollover logic
   // Remove sticky scrubbing logic  
   // Remove offScreenScrubbing logic
   ```

2. **Disable Edge Detection** (Priority 1)
   ```javascript
   // Comment out or remove from pen.mjs:
   window.addEventListener("pointerleave", function (e) {
     // window.acSEND({ type: "ui:cancel-interactions", content: {} });
   });
   ```

3. **Restore Simple Button Properties** (Priority 2)
   ```javascript
   // Remove from Button class:
   // noRolloverActivation = false;
   // stickyScrubbing = false; 
   // offScreenScrubbing = false;
   ```

4. **Test on iPhone** (Priority 1)
   - Verify 'rect.mjs' drawing works smoothly
   - Test 'notepat' multi-touch functionality
   - Confirm 'toss' buttons don't stick

### Long-term Solution

If drag-between-buttons behavior is desired, implement it as:

1. **Opt-in System**: Only specific pieces enable complex button behaviors
2. **Isolated Implementation**: Don't affect global touch handling
3. **Platform Detection**: Disable complex behaviors on touch devices
4. **Gradual Rollout**: Test extensively on all platforms before deployment

## Testing Checklist

### Regression Tests

- [ ] 'rect.mjs' drawing smoothness on iPhone
- [ ] 'notepat' multi-touch functionality  
- [ ] 'toss' button responsiveness
- [ ] General pen coordinate accuracy
- [ ] Edge-of-screen drawing operations
- [ ] Multi-finger gesture recognition

### Platform Coverage

- [ ] iPhone Safari
- [ ] iPad Safari  
- [ ] Android Chrome
- [ ] Desktop browsers
- [ ] Touch-enabled laptops

## Risk Assessment

### Fix Complexity: **Low to Medium**
- Most changes involve removing code rather than adding it
- Core pen handling logic remains intact
- Risk of breaking existing functionality is minimal

### Testing Requirements: **Medium**
- Need extensive testing on mobile devices
- Multi-touch scenarios require careful validation
- UI pieces like 'prompt', 'stample', 'toss' need verification

## Conclusion

The pen handling regression is caused by an overly complex button interaction system that interferes with normal touch event processing. The fix involves simplifying the button system back to its pre-June 27 state while preserving any genuinely needed functionality. The regression is well-contained and should be straightforward to resolve with proper testing.

The key insight is that **simplicity in touch handling is critical** - any system that adds delays, global state coordination, or complex event interdependencies will disrupt the precise, real-time nature of drawing and touch interactions.
