# KidLisp Fill/Outline Mode System

**Date**: 2025-10-16  
**Status**: 📋 Planning  
**Author**: Assistant + whistlegraph

## 🎯 Executive Summary

Add Processing-style `(fill)` / `(outline)` global state commands to KidLisp, plus explicit mode parameters for all shape primitives. This enables both stateful workflow (like Processing) and explicit per-shape control (like current box/tri).

**Key Changes**:
- ✅ Add global `(fill)` and `(outline)` commands
- ✅ Circle gets fill capability: `(circle x y r "fill"`
- ✅ All shapes respect global state: `(fill)` affects circle, box, tri, shape
- ✅ Explicit modes override global state
- ✅ **Default is FILL mode** (matches box's default, fixes inconsistency)

## 🎯 Problem Statement

Currently, KidLisp had an inconsistency:
- `box()` defaulted to **filled** (graph.mjs: `mode = "fill"`)
- `circle()` defaulted to **outline** (graph.mjs: `filled = false`)

This created confusion! There was no way to easily switch modes or have consistent defaults.

### Desired Behavior (Two Approaches)

#### Approach 1: Explicit Mode Parameter
```lisp
(circle 100 100 50)            ; Filled (new default, matches box)
(circle 100 100 50 "fill")     ; Explicitly filled
(circle 100 100 50 "outline")  ; Explicit outline
(circle 100 100 50 "outline:5") ; Thick outline
```

#### Approach 2: Stateful Mode Commands ⭐ PREFERRED
```lisp
; Default is fill mode (matches box)
(circle 100 100 50)  ; Filled
(box 50 50 40 40)    ; Also filled

; Switch to outline mode globally
(outline)
(circle 200 100 50)  ; Now outlined
(box 200 50 40 40)   ; Also outlined

; Switch back to fill mode
(fill)
(circle 100 200 50)  ; Filled again
```

**Both approaches are supported!** Explicit mode parameter overrides global state.

## 🏗️ Architecture Overview

### Current Stack (Bottom to Top)

```
┌─────────────────────────────────────────────────────────┐
│ KidLisp Code: (circle 100 100 50)                      │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│ kidlisp.mjs (apiRegistry)                               │
│ circle: (api, args) => api.circle(...args)             │
│ • Just passes through args directly                     │
│ • No mode parsing                                       │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│ disk.mjs ($paintApiUnwrapped)                           │
│ Wraps graph functions for piece API                     │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│ graph.mjs                                                │
│ circle(x, y, radius, filled=false, thickness, precision)│
│ • filled: boolean parameter (DEFAULT FALSE = OUTLINE)   │
│ • If filled=true → delegates to oval()                  │
│ • oval() uses shape() for filling                       │
│ • Already accepts string modes like "fill"!             │
└─────────────────────────────────────────────────────────┘
```

### Related Shape Functions (For Comparison)

#### Box (Already supports modes)
```javascript
// graph.mjs
box(x, y, w, h, mode = "fill")
// Modes: "fill", "outline", "outline:N", "inline", etc.
```

```lisp
; KidLisp
(box 10 10 50 50)           ; Filled
(box 10 10 50 50 "outline") ; Outline
```

#### Tri (Already supports modes)
```javascript
// graph.mjs  
tri(x1, y1, x2, y2, x3, y3, mode = "fill")
// Modes: "fill", "outline", "inline"
```

```lisp
; KidLisp
(tri 10 10 50 10 30 50)           ; Filled
(tri 10 10 50 10 30 50 "outline") ; Outline
```

#### Shape (Generic polygon - has filled parameter)
```javascript
// graph.mjs
shape({ points, filled, thickness })
```

## 💡 Solution Design

### Two-Part Solution

#### Part 1: Explicit Mode Parameter (Already Partially Works!)
Add an optional 4th parameter to circle for the mode string, maintaining backward compatibility.

#### Part 2: Global Fill/Outline State Commands ⭐ NEW
Add Processing-style `(fill)` and `(outline)` commands that set global state, affecting all subsequent shape commands until changed.

**Key Insight**: Graph.mjs already accepts string modes! We mainly need to wire it through kidlisp properly and add the global state system.

### API Signature Changes

#### 1. graph.mjs - Already supports string modes!
```javascript
// Current signature (NO CHANGE NEEDED!)
function circle(x0, y0, radius, filled = false, thickness, precision)
// filled can be:
//   - boolean: true/false
//   - string: "fill", "outline" (already works in some pieces!)
```

**Discovery**: Graph.mjs already accepts mode strings in several pieces! See:
- `/disks/ucla-5.mjs`: `circle(x, y, r, "fill")`
- `/disks/ucla-6.mjs`: `circle(x, y, r, "fill")`
- `/disks/marker.mjs`: `circle(x, y, r, true)`

#### 2. kidlisp.mjs - Add global state + pass through modes

**New Global State**:
```javascript
// Add to KidLisp class
class KidLisp {
  constructor() {
    // ... existing code ...
    this.fillMode = false; // Default to outline (filled=false)
  }
  
  resetState() {
    // ... existing resets ...
    this.fillMode = false; // Reset to outline
  }
}
```

**New Mode Commands in apiRegistry**:
```javascript
fill: (api) => {
  this.fillMode = true;
  return undefined; // No visual output
},

outline: (api) => {
  this.fillMode = false;
  return undefined; // No visual output
},

stroke: (api) => {
  // Alias for outline (Processing compatibility)
  this.fillMode = false;
  return undefined;
},

nofill: (api) => {
  // Alias for outline (Processing compatibility)
  this.fillMode = false;
  return undefined;
},

nostroke: (api) => {
  // Controversial: maybe this means filled with no outline?
  // For now, treat as alias for fill
  this.fillMode = true;
  return undefined;
},
```

**Updated circle handler**:
```javascript
circle: (api, args = []) => {
  // args[0] = x
  // args[1] = y  
  // args[2] = radius
  // args[3] = mode (optional) - overrides global fillMode

  const drawCircle = () => {
    if (args.length >= 4) {
      // Explicit mode provided - use it directly
      api.circle(args[0], args[1], args[2], args[3], ...args.slice(4));
    } else {
      // Use global fillMode state
      api.circle(args[0], args[1], args[2], this.fillMode, ...args.slice(3));
    }
  };

  // ... existing bake/embed logic ...
  drawCircle();
}
```

**Apply to other shapes too**:
```javascript
box: (api, args = []) => {
  // If no mode arg provided and we have 4 numeric args, append mode
  if (args.length === 4 && args.every(a => typeof a === 'number')) {
    args = [...args, this.fillMode ? "fill" : "outline"];
  }
  // ... rest of existing box logic ...
},

tri: (api, args = []) => {
  // If no mode arg provided and we have 6 numeric args, append mode
  if (args.length === 6 && args.every(a => typeof a === 'number')) {
    args = [...args, this.fillMode ? "fill" : "outline"];
  }
  // ... rest of existing tri logic ...
},

shape: (api, args = []) => {
  // Shape needs filled boolean in object
  // If args is array of points without mode, add filled property
  // ... apply fillMode logic ...
}
```

### Recommended Implementation Path

**Hybrid Approach: State + Explicit Modes** ⭐ BEST OF BOTH WORLDS

1. **Add global `fillMode` state** to KidLisp class
2. **Add mode commands**: `(fill)`, `(outline)`, `(stroke)`, `(nofill)`
3. **Update shape functions** to use global state as default
4. **Allow explicit mode override** via parameter: `(circle x y r "fill")`
5. **Apply to all shapes**: `circle`, `box`, `tri`, `shape`

This gives users flexibility:
```lisp
; Explicit mode (always works, overrides global state)
(circle 100 100 50 "fill")

; Global state mode (affects all shapes)
(fill)
(circle 100 100 50)     ; Filled
(box 10 10 50 50)       ; Also filled
(tri 0 0 50 0 25 50)   ; Also filled

(outline)
(circle 200 100 50)     ; Back to outline

; Mixed usage
(fill)
(circle 100 100 50)           ; Filled (uses global)
(circle 200 100 50 "outline") ; Outline (explicit override)
(circle 300 100 50)           ; Filled again (back to global)
```

**Benefits**:
- ✅ Processing-style workflow for creative coding
- ✅ Explicit overrides for precision control  
- ✅ Backward compatible (default is outline)
- ✅ Applies to ALL shape primitives uniformly
- ✅ Reduces repetition in code
- ✅ State resets between frames (optional)

## 📋 Implementation Checklist

### Phase 1: Core Implementation ✅

#### 1.1: Add global fillMode state to KidLisp class ✅
- [x] Add `this.fillMode = false` to constructor (line ~300 in kidlisp.mjs)
- [x] Add fillMode reset in `resetState()` method
- [x] Document the state in class comments

#### 1.2: Add mode command functions to apiRegistry ✅
- [x] Add `fill: (api) => { this.fillMode = true; }`
- [x] Add `outline: (api) => { this.fillMode = false; }`
- [x] Add aliases: `stroke`, `nofill`, `nostroke` (Processing compatibility)
- [x] Location: apiRegistry in kidlisp.mjs (around line ~4000-5000)

#### 1.3: Update circle() handler to use global state ✅
- [x] Location: apiRegistry, circle: (api, args) => ... (line ~4533)
- [x] Check if args[3] exists → use it (explicit mode override)
- [x] Otherwise → use `this.fillMode` (global state)
- [x] Pass the mode through to `api.circle()`
- [x] Ensure existing embedded/bake logic still works

#### 1.4: Update other shape handlers ✅
- [x] Update `box()` to use global fillMode when no mode arg
- [x] Update `tri()` to use global fillMode when no mode arg  
- [x] Update `shape()` to use global fillMode in filled property
- [x] Keep explicit mode parameters as overrides

#### 1.5: Verify graph.mjs ✅
- [x] Confirm circle() already accepts string modes
- [x] Test that "fill" and "outline" strings work
- [x] Verify thickness parsing for "outline:N"

### Phase 2: Documentation & Examples 📚

#### 2.1: Update KidLisp Language Reference ✅
- [x] File: `/kidlisp/COMPLETE_API_MAP.md`
- [x] Add `fill`, `outline`, `stroke`, `nofill` commands
- [x] Update circle entry with mode parameter examples
- [x] Add "Drawing Modes" section explaining global state
- [x] Show examples of explicit override vs global state

#### 2.2: Update inline LLM documentation ✅
- [x] File: `/system/public/aesthetic.computer/lib/kidlisp.mjs`
- [x] Update `#region 🤖 LLM API SPECIFICATION` section (line ~16)
- [x] Add mode commands: `(fill)`, `(outline)`
- [x] Add circle mode examples: `(circle x y r "fill")`
- [x] Explain global state vs explicit override
- [x] Add Processing-style workflow examples

#### 2.3: Create example files ✅
- [x] Create `/kidlisp/examples/fill-outline-modes.lisp`
- [x] Show global state switching: `(fill)` then multiple shapes
- [x] Show explicit overrides: shapes with mode strings
- [x] Show mixed usage patterns
- [x] Demonstrate with circle, box, tri together

#### 2.4: Update function drilldown ✅
- [x] Create `/kidlisp/docs/functions/fill.md`
- [x] Create `/kidlisp/docs/functions/outline.md`
- [x] Detailed explanation of all modes and state system
- [x] Visual examples of each mode

### Phase 3: Testing ✅

#### 3.1: Unit tests
- [ ] Test outline circles (default): `(circle x y r)`
- [ ] Test filled circles with global state: `(fill)` then `(circle x y r)`
- [ ] Test explicit fill: `(circle x y r "fill")`
- [ ] Test explicit outline: `(circle x y r "outline")`
- [ ] Test thickness: `(circle x y r "outline:5")`
- [ ] Test state switching: `(fill)` `(outline)` `(fill)`
- [ ] Test explicit override: `(fill)` then `(circle x y r "outline")`
- [ ] Test backward compatibility (3-arg circles)
- [ ] Test with embedded/bake contexts
- [ ] Test state reset between evaluations

#### 3.2: Integration tests - All Shapes
- [ ] Test box with global fillMode
- [ ] Test tri with global fillMode
- [ ] Test shape with global fillMode
- [ ] Test mixed shapes: `(fill)` affects circle, box, tri all at once
- [ ] Test that explicit modes override global state for all shapes

#### 3.2: Integration tests
- [ ] Create test piece: `/system/public/aesthetic.computer/disks/circle-test.mjs`
- [ ] Test all circle modes side-by-side
- [ ] Verify with make/kidlisp generation
- [ ] Test in notebooks

#### 3.3: Visual regression tests
- [ ] Compare before/after renderings
- [ ] Ensure no unintended visual changes
- [ ] Test edge cases (radius=0, radius=1, huge radius)

### Phase 4: Polish & Release 🎨

#### 4.1: Performance check
- [ ] Profile circle rendering with modes
- [ ] Ensure no performance regression
- [ ] Compare filled vs outline performance

#### 4.2: Update chatmode prompts
- [ ] File: `/modes/chatmodes/kidlisp.chatmode.md`
- [ ] Add circle mode information for AI assistants

#### 4.3: Changelog entry
- [ ] Document the new feature
- [ ] Note backward compatibility
- [ ] Include example code

## 🧪 Test Cases

### Basic Functionality - Global State
```lisp
; Test 1: Default outline circle (backward compatible)
(wipe black)
(ink red)
(circle 50 50 30)  ; Should be outline (default)

; Test 2: Fill mode globally
(fill)
(ink blue)
(circle 150 50 30)  ; Should be filled

; Test 3: Switch back to outline
(outline)
(ink green)
(circle 250 50 30)  ; Should be outline again

; Test 4: Multiple shapes affected by global state
(fill)
(ink yellow)
(circle 50 150 20)
(box 100 130 40 40)
(tri 200 130 230 130 215 160)
; All should be filled

; Test 5: Switch to outline for all
(outline)
(ink cyan)
(circle 50 220 20)
(box 100 200 40 40)
(tri 200 200 230 200 215 230)
; All should be outline
```

### Explicit Mode Override
```lisp
; Test 6: Explicit fill overrides outline state
(outline)  ; Global is outline
(ink magenta)
(circle 50 50 20)         ; Outline (follows global)
(circle 100 50 20 "fill") ; Filled (explicit override)
(circle 150 50 20)        ; Outline (back to global)

; Test 7: Explicit outline overrides fill state
(fill)  ; Global is fill
(ink orange)
(circle 50 100 20)            ; Filled (follows global)
(circle 100 100 20 "outline") ; Outline (explicit override)
(circle 150 100 20)           ; Filled (back to global)

; Test 8: Thickness with explicit mode
(outline)
(ink white)
(circle 100 150 30 "outline:5")  ; 5px thick outline
```

### Edge Cases
```lisp
; Test 9: Very small circles
(outline)
(circle 10 10 1)
(fill)
(circle 20 10 2)

; Test 10: Very large circles
(outline)
(circle 200 200 100)
(fill)
(circle 300 300 100)

; Test 11: Zero radius (should handle gracefully)
(circle 50 50 0)

; Test 12: Animation - mode switching per frame
(def time (/ frame 60))
(if (= (% frame 30) 0)
  (if (= (% frame 60) 0) (fill) (outline)))
(ink rainbow)
(circle 150 150 (+ 20 (* 10 (sin time))))
```

### Integration with Other Features
```lisp
; Test 13: With transformations
(fill)
(zoom 2)
(circle 100 100 25)

; Test 14: With transparency
(fill)
(ink red 128)
(circle 100 100 30)
(outline)
(ink blue 255)
(circle 110 110 30 "outline:3")

; Test 15: In embedded context
(fill)
(bake
  (ink yellow)
  (circle 50 50 20))

; Test 16: State persists correctly
(fill)
(circle 50 50 20)  ; Filled
; ... many other commands ...
(circle 100 100 20)  ; Still filled (state persists)
```

### Processing-Style Workflow Test
```lisp
; Test 17: Full Processing-style example
(wipe "lightgray")

; Background shapes - filled
(fill)
(ink "darkblue" 128)
(circle 100 100 80)
(circle 200 150 60)

; Foreground shapes - outlined
(outline)
(ink "yellow")
(circle 100 100 40 "outline:3")
(circle 200 150 30 "outline:2")

; Mixed - some filled, some outlined
(fill)
(ink "red")
(circle 150 200 25)
(outline)
(ink "green")
(box 120 170 60 60 "outline:2")
```

## 🎨 Design Rationale

### Why BOTH Global State AND Mode Parameters?

**We now support BOTH approaches** based on user feedback:

1. **Global State Commands**: `(fill)`, `(outline)` - Processing-style workflow
2. **Explicit Mode Parameters**: `(circle x y r "fill")` - Precise control

#### Approach 1: Global State (Processing-style)
```lisp
; Set mode once, affects all shapes
(fill)
(circle 100 100 30)    ; Filled
(box 50 50 40 40)      ; Also filled
(tri 0 0 50 0 25 50)  ; Also filled

(outline)
; Now everything is outlined
```

**Benefits**:
- ✅ Less repetition in code
- ✅ Familiar to Processing/p5.js users
- ✅ Natural for creative coding workflows
- ✅ State changes are explicit and visible
- ✅ Reduces visual clutter in code

**Drawbacks**:
- ⚠️ Requires tracking state mentally
- ⚠️ State can persist unexpectedly
- ⚠️ Less explicit per-shape

#### Approach 2: Explicit Parameters
```lisp
; Specify per shape
(circle 100 100 30 "fill")
(circle 200 100 30 "outline")
(circle 300 100 30 "outline:5")
```

**Benefits**:
- ✅ Explicit and self-documenting
- ✅ No state to track
- ✅ Easy to understand at a glance
- ✅ Consistent with box and tri

**Drawbacks**:
- ⚠️ More repetitive
- ⚠️ Longer code for similar shapes
- ⚠️ Less familiar to Processing users

#### Why Support Both? ⭐

**Best of both worlds**:
```lisp
(fill)                        ; Set global default
(circle 100 100 30)           ; Uses global (filled)
(circle 200 100 30)           ; Uses global (filled)
(circle 300 100 30 "outline") ; Explicit override!
(circle 400 100 30)           ; Back to global (filled)
```

This gives maximum flexibility:
- Use global state for consistency
- Use explicit modes for exceptions
- Switch between styles as needed
- Familiar to Processing AND modern shader-style coding

### Similar Systems in Other Platforms

#### Processing
```processing
fill(255, 0, 0);
noStroke();
circle(100, 100, 50);  // Filled red, no outline

noFill();
stroke(0, 0, 255);
circle(200, 100, 50);  // Hollow, blue outline
```

#### P5.js (similar to Processing)
```javascript
fill('red');
noStroke();
circle(100, 100, 50);
```

#### Canvas API (what graph.mjs wraps)
```javascript
ctx.fillStyle = 'red';
ctx.fill();
ctx.arc(100, 100, 50, 0, Math.PI * 2);
```

Our mode parameter approach is more functional and explicit.

## 🔄 Backward Compatibility

### Guaranteed Compatible Code ✅
```lisp
; All existing code continues to work:
(circle 100 100 50)              ; ✅ Outline (current default)
(circle x y r)                   ; ✅ Outline with variables
(repeat 10 (circle (wiggle 100) (wiggle 100) 20))  ; ✅ All outlines
```

**Key Point**: Default remains **OUTLINE** (filled=false), which is the current behavior in graph.mjs. No existing code will break!

### New Capabilities Added
```lisp
; NEW: Global fill mode
(fill)
(circle 100 100 50)  ; Now filled

; NEW: Explicit mode parameter
(circle 100 100 50 "fill")  ; Filled regardless of global state

; NEW: Global outline mode (though it's already the default)
(outline)
(circle 100 100 50)  ; Explicitly outlined
```

### Breaking Changes
**NONE** - This is a pure addition. 
- Default behavior is unchanged (outline)
- All existing 3-argument circles work identically
- New commands are new names that didn't exist before

## 📊 Comparison with Other Shapes

| Shape | Default | Explicit Fill | Explicit Outline | Global State Support | Status |
|-------|---------|---------------|------------------|---------------------|--------|
| box   | fill | ✅ `"fill"` | ✅ `"outline:N"` | 🔜 **ADDING** | ✅ Has modes |
| tri   | fill | ✅ `"fill"` | ✅ `"outline"` | 🔜 **ADDING** | ✅ Has modes |
| circle | outline | 🔜 **ADDING** | ✅ default | 🔜 **ADDING** | ❌ **MISSING** |
| shape | via object | ✅ `filled: true` | ✅ `filled: false` | 🔜 **ADDING** | ⚠️ Partial |
| oval  | via pieces | ✅ piece-level | ✅ piece-level | 🔜 **ADDING** | ⚠️ Piece only |

After this implementation:
- ✅ All shapes will support global `(fill)` / `(outline)` state
- ✅ All shapes will support explicit mode parameters
- ✅ Consistent API across all drawing primitives

## 🌟 Key Features Added

### New Commands
1. `(fill)` - Set global fill mode for all shapes
2. `(outline)` - Set global outline mode for all shapes  
3. `(stroke)` - Alias for outline (Processing compatibility)
4. `(nofill)` - Alias for outline (Processing compatibility)

### Enhanced Commands
1. `(circle x y r)` - Now respects global fill/outline mode
2. `(circle x y r "fill")` - Explicit fill (overrides global)
3. `(circle x y r "outline")` - Explicit outline (overrides global)
4. `(circle x y r "outline:N")` - Explicit outline with thickness
5. `(box ...)` - Now respects global fill/outline mode
6. `(tri ...)` - Now respects global fill/outline mode

## 🎯 Success Criteria

- [ ] Can draw filled circles: `(fill)` then `(circle x y r)` OR `(circle x y r "fill")`
- [ ] Can draw outline circles: `(outline)` then `(circle x y r)` OR default `(circle x y r)`
- [ ] Global state affects ALL shapes: circle, box, tri, shape
- [ ] Explicit mode parameters override global state
- [ ] Can specify thickness: `(circle x y r "outline:5")`
- [ ] All existing KidLisp code works unchanged (default is outline)
- [ ] Performance is equivalent to current implementation
- [ ] Documentation clearly explains both approaches
- [ ] Examples demonstrate stateful workflow (Processing-style)
- [ ] Examples demonstrate explicit override patterns
- [ ] Tests cover global state, explicit modes, and mixed usage
- [ ] AI assistants understand the dual-mode system

## 💭 Design Philosophy

This implementation embraces **progressive disclosure**:

1. **Simple default**: `(circle x y r)` draws outline (current behavior)
2. **One-line switch**: `(fill)` changes mode for everything
3. **Explicit override**: `(circle x y r "fill")` when you need it
4. **Familiar patterns**: Like Processing, but with explicit options too

**Best for creative coding**: Switch mode globally, override when needed.

## 🚀 Future Enhancements (Not in this PR)

1. **Per-frame state reset**: Option to reset fillMode at start of each frame
2. **State stack**: `(push-style)` / `(pop-style)` like Processing
3. **Separate fill and stroke**: `(fill red)` + `(stroke blue)` for outlined filled shapes
4. **Stroke weight**: `(stroke-weight 5)` separate from outline thickness
5. **Inline mode for circle**: True inward offset (not just outline)
6. **Gradient fills**: `(circle x y r "fill:gradient")`
7. **Dashed outlines**: `(circle x y r "outline:dashed")`
8. **Arc support**: `(arc x y r start end)` with modes
9. **Ellipse mode parameter**: `(oval x y rx ry "outline")`
10. **No-fill + stroke**: Ability to have outlined shape without fill color

## 🧠 State Management Considerations

### When Does State Reset?

**Options to consider**:

1. **Never reset** (current proposal)
   - State persists throughout entire program execution
   - User must explicitly switch modes
   - Pro: Predictable, explicit
   - Con: Can be confusing if you forget what mode you're in

2. **Reset per frame** (alternative)
   - State resets to default (outline) at start of each paint call
   - Pro: Prevents state bugs between frames
   - Con: Must set mode every frame in animations

3. **Reset per function** (alternative)
   - State resets when entering/exiting `later` functions
   - Pro: Functions are isolated
   - Con: More complex implementation

**Recommendation**: Start with Option 1 (never reset), add Option 2 as feature later if needed via `(reset-styles)` or automatic frame reset toggle.

### State Inspection Commands (Future)

```lisp
(debug (fill-mode))  ; Returns current fill state (true/false)
(log "Fill mode:" (fill-mode))
```

This could help users track state during development.

## 📝 Notes

- The TODO comment in graph.mjs (line 2489) mentions implementing a "nice filled option" - this addresses that partially
- Consider if `shape` function's `filled` parameter should also support mode strings
- May want to add `circle` to common KidLisp function list for better AI generation

## 🔗 Related Files

- `/system/public/aesthetic.computer/lib/graph.mjs` - circle function (~line 2492)
- `/system/public/aesthetic.computer/lib/kidlisp.mjs` - apiRegistry circle (~line 4533)
- `/system/public/aesthetic.computer/lib/disk.mjs` - $paintApiUnrapped
- `/kidlisp/COMPLETE_API_MAP.md` - API documentation
- `/kidlisp/docs/functions/` - Function drilldowns
- `/system/public/aesthetic.computer/disks/oval.mjs` - Reference for mode parsing
- `/system/public/aesthetic.computer/disks/blank.mjs` - Test with tri modes

## 🏁 Getting Started

To implement this plan:

1. Read through this entire plan
2. Start with Phase 1.1: Update graph.mjs circle()
3. Test at each step
4. Move through phases sequentially
5. Update this document with progress (check boxes)
6. Ask questions if anything is unclear!

---

## 📸 Visual Summary

### Current State (Before)
```lisp
(circle 100 100 50)  ; ⭕ Outline only
; No way to fill!
```

### After Implementation
```lisp
; Option 1: Global state (Processing-style)
(fill)
(circle 100 100 50)  ; ⚫ Filled!

(outline)
(circle 200 100 50)  ; ⭕ Outline

; Option 2: Explicit per-shape
(circle 100 200 50 "fill")     ; ⚫ Filled!
(circle 200 200 50 "outline")  ; ⭕ Outline

; Option 3: Mix both!
(fill)                         ; Global default
(circle 100 300 50)            ; ⚫ Filled (uses global)
(circle 200 300 50 "outline")  ; ⭕ Outline (explicit override!)
(circle 300 300 50)            ; ⚫ Filled (back to global)
```

### Applies to All Shapes
```lisp
(fill)
(circle 50 50 30)     ; ⚫ Filled circle
(box 100 30 40 40)    ; ⬛ Filled box
(tri 180 30 220 30 200 60)  ; ▲ Filled triangle

(outline)
(circle 50 120 30)    ; ⭕ Outline circle
(box 100 100 40 40)   ; ⬜ Outline box
(tri 180 100 220 100 200 130)  ; △ Outline triangle
```

---

**Ready to implement?** Start with Phase 1.1: Add `fillMode` state to KidLisp class, then add the mode commands. Test each step! 🎨

