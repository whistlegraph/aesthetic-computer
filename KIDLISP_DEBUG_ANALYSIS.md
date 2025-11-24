# KidLisp Rendering Pipeline Analysis

## Issue Description
The user reported "jumping" modes and incorrect alpha blending when using embedded layers (specifically `$39i`).
Investigation revealed that the embedded layer was being rendered twice:
1. Once into `layer0` (the main drawing buffer).
2. Once as an overlay during the composite phase.

This caused:
- **Double Alpha Application**: 50% alpha became 75% opaque (pasted once, then pasted again on top).
- **Layer Contamination**: `layer0` contained the embedded layer image, preventing clean background clearing or proper layering.
- **Visual Artifacts**: "Jumping" or flickering if the two render passes were slightly out of sync or if `layer0` wasn't cleared properly.

## Rendering Pipeline Analysis

### Current Flow (Problematic)
1. **`paint()` Start**:
   - `layer0` is cleared (`fill(0)`).
   - `evaluate(ast)` is called.

2. **`evaluate()` Execution**:
   - Executes user code, e.g., `($39i)`.
   - Calls `embed()`.
   - `embed()` finds the layer in cache.
   - **ISSUE**: `embed()` calls `pasteWithAlpha()` to paste the layer into `layer0` immediately.
   - `layer0` is now "dirty" with the embedded image.

3. **Embedded Layer Update**:
   - `paint()` iterates `embeddedLayers`.
   - Calls `renderSingleLayer()`.
   - Updates the embedded layer's internal buffer (evaluates its code).

4. **Composite Phase**:
   - Checks `layer0HasContent`. It is **TRUE** because of step 2.
   - **Paste 1**: Pastes `layer0` (containing the embedded image) to the screen.
   - **Paste 2**: Pastes `embeddedLayers` (containing the embedded image) to the screen.
   - Result: The image is drawn twice.

### Proposed Fix
The `embed()` function should **not** paste into `layer0` when the rendering pipeline is handling the compositing of embedded layers separately.
Embedded layers are treated as persistent overlays that sit *above* `layer0`.

**Action**: Remove (or comment out) the `pasteWithAlpha` call inside the `embed` function in `kidlisp.mjs`.

## Verification Plan
1. Apply the fix in `kidlisp.mjs`.
2. Run the `$39i` piece.
3. Check logs:
   - `layer0HasContent` should be `false` (assuming no other drawing commands).
   - `COMPOSITE: Skipping layer0 paste` should appear.
   - Only one paste operation (for the embedded layer) should occur per frame.
