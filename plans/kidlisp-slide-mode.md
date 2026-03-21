# KidLisp Slide Mode

**Status:** Planning  
**Created:** 2025-01-19

## Overview

A special evaluative/communicative mode where all numeric parameters in KidLisp code become draggable "sliders" while all other code is locked. Dragging a number button adjusts its value based on decimal precision:
- Integer `150` → ±1
- Float `1.5` → ±0.1  
- Float `1.05` → ±0.01

## Core Behavior

### Activation
- Toggle button to the left of the X (close) button
- Icon: `123` symbol (represents numeric values)
- Keyboard shortcut TBD

### While Active
1. **Numbers become buttons** - Highlighted/styled as draggable elements
2. **Other code locked** - Can't edit text, only drag numbers
3. **Immediate visual feedback** - Partial re-evaluation happens on drag (no new `$code` created)
4. **No code persistence during slide** - Changes are ephemeral until explicitly kept

### Deactivation
- **Keep changes** (optional): Creates new `$code` with modified values
- **Discard changes** (default): Reverts to original `$code`/values

## Implementation Phases

### Phase 1: Monaco Editor UI (kidlisp.com) ✅ INITIAL IMPLEMENTATION
- [x] Add slide toggle button (123 icon) next to X button
- [x] Parse code to find all numeric literals with positions
- [x] Create Monaco decorations for number overlays (purple highlight)
- [x] Implement drag-to-adjust interaction
- [x] Visual feedback (highlight on hover, drag indicator, tooltip)
- [x] Lock non-numeric editing while slide mode active (readOnly)
- [x] Keep/Discard buttons for accepting or reverting changes
- [ ] Touch support for mobile drag interactions
- [ ] Keyboard shortcuts (e.g., Ctrl+Shift+S to toggle)

### Phase 2: KidLisp Interpreter Hooks
- [ ] Add `slideUpdate(varName, newValue)` API for partial re-evaluation
- [ ] Scope tracking to know which values to update
- [ ] Efficient re-render without full re-parse
- [ ] State snapshot/restore for discard functionality

### Phase 3: Keep/Discard UI
- [ ] "Keep Changes" button (creates new $code)
- [ ] "Discard" as default on toggle-off
- [ ] Confirmation if large changes made
- [ ] Undo support

### Phase 4: Prompt HUD Integration (FUTURE)
- [ ] Corner label showing current slide mode state
- [ ] Touch-friendly number adjustment on mobile
- [ ] Visual indicator in the canvas area
- [ ] Works with PJ (Piece Jockey) mode

## Technical Notes

### Number Detection
Use `tokenizeForParser()` which returns position info:
```javascript
tokens.push({ value: token, pos: tokenStart });
```

Numeric regex: `/^-?\d+\.?\d*$/` or similar

### Decimal Precision Detection
```javascript
function getStep(numStr) {
  if (!numStr.includes('.')) return 1;
  const decimals = numStr.split('.')[1].length;
  return Math.pow(10, -decimals);
}
// "150" → 1, "1.5" → 0.1, "1.05" → 0.01
```

### Monaco Decoration API
- Use `editor.deltaDecorations()` for highlighting
- Use `editor.addContentWidget()` for interactive overlays
- Or use inline CSS styling via decoration options

### Partial Re-evaluation
Need to track which AST nodes contain modified values and re-evaluate only those branches. May require:
- AST node → canvas command mapping
- Value replacement without full parse
- Frame-specific state isolation

## Open Questions

1. Should drag sensitivity be linear or logarithmic for large numbers?
2. How to handle negative numbers (drag left for more negative)?
3. Should there be min/max constraints?
4. How to handle numbers inside strings (ignore them)?
5. Color values like `(ink 255 128 0)` - special RGB slider UI?

## Related Files

- `/system/public/kidlisp.com/index.html` - Main editor UI
- `/system/public/aesthetic.computer/lib/kidlisp.mjs` - Interpreter
- `/system/public/aesthetic.computer/disks/prompt.mjs` - Prompt HUD (future)
