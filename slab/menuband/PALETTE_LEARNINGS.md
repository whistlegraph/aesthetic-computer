## Piano / Waveform Palette Learnings

This file captures the current behavior and implementation constraints for the unified floating/collapsed palette work.

### Current architecture

- `PianoWaveformPalette` is the facade for the two UI states.
- Expanded state is `FloatingPlayPaletteController`.
- Collapsed state is `MenuBarWaveformStrip`.
- `AppDelegate` now treats both as one conceptual element through `pianoWaveformPalette`.

### Current state model

- `PianoWaveformPalette.State.expanded` means the floating palette is the active representation.
- `PianoWaveformPalette.State.collapsed` means the strip is the active representation, even if the strip is currently hidden.
- Hidden collapsed strip is intentional state, not a reset back to expanded.
- In collapsed state, key presses and menubar piano mouse-drag can revive the strip.
- In expanded state, note activity must not auto-show the strip.

### Position persistence

- Expanded palette position is persisted in `FloatingPlayPaletteController` via `UserDefaults`.
- Collapsed strip position is persisted separately in `MenuBarWaveformStrip`.
- Collapsed `Dock` resets the strip back under the menubar piano by clearing the custom origin.

### API learnings

- `NSGlassEffectView` is the primary AppKit Liquid Glass surface for this work.
- `NSGlassEffectView.contentView` is the supported place to embed real content.
- `NSGlassEffectView.cornerRadius` is the direct way to shape the glass surface.
- `NSGlassEffectView.style` can be set to `.clear` for button-like glass chrome.
- `NSGlassEffectView.tintColor` is the supported API for pushing the glass toward the current instrument/accent color.
- `NSGlassEffectContainerView` is useful when several nearby glass views should merge efficiently.
- `NSGlassEffectContainerView.contentView` is the descendant subtree the container manages and merges.
- Apple’s AppKit docs explicitly note that `NSGlassEffectContainerView` can improve performance by batching nearby similar glass views.
- `NSGlassEffectView` only guarantees correct glass behavior for its `contentView`; arbitrary subviews are not guaranteed to behave correctly relative to the glass surface or z-order.
- For fallback styling on older macOS versions, plain layer-backed `NSButton` plus translucent background/border is a practical substitute.

### Liquid Glass implementation rules

- Prefer one larger glass container plus a few intentional child glass surfaces instead of many unrelated decorative glass views.
- Use clear-style glass only for chrome-like controls; use the main strip/palette glass views for the actual body surfaces.
- Keep the overlay buttons and their glass backplates as siblings in the same ancestor view hierarchy.
- Add the interactive button to the hierarchy first, then add and constrain the glass backplate to it.
- If hover-only controls are used, initialize both the button and its glass background to hidden.
- Reapply tinting when the instrument family or MIDI visual mode changes.

### Expanded palette controls

- `FloatingPlayPalette` has hover-only corner controls.
- Left: close.
- Right: dock, expand/collapse.
- On macOS 26+, these use `NSGlassEffectView` with clear-style glass backplates.
- Older macOS versions use a translucent layer-backed fallback.

### Collapsed strip controls

- The strip no longer uses the footer-row controls for close/dock/expand.
- Those controls are now top overlay controls.
- Left: close.
- Right: dock, expand.
- They are intended to be visible only while the pointer is inside the strip.
- Legacy implementation uses layer-backed circular buttons.
- Liquid implementation uses clear `NSGlassEffectView` backplates plus overlay buttons.

### Important crash gotcha

- Do not create glass backplates for buttons before the buttons are attached to the same ancestor view hierarchy.
- We hit an `NSGenericException` from constraints between `StripMovableGlassEffectView` and `NSButton` with no common ancestor.
- Correct order:
  1. add the buttons to `rootView`
  2. then create/install glass backplates constrained to those buttons

### Important hover/visibility gotcha

- Overlay controls must be explicitly reset hidden on strip `show()` and `dismiss()`.
- Relying only on `button.alphaValue = 0` during setup is not enough once the strip has already been shown and hovered before.
- For the liquid strip, glass backplates also need `alphaValue = 0` at creation time.
- Hover state should be treated as transient UI state, not inferred from visibility state.

### Symbol visibility gotcha

- The collapsed strip overlay buttons were effectively invisible when using the weaker default tint assumptions.
- Explicit symbol tinting is safer here.
- Current working tint is `NSColor.white.withAlphaComponent(0.92)`.

### Best practices found so far

- Keep `PianoWaveformPalette` as the only place that understands expanded vs collapsed state transitions.
- Let `AppDelegate` talk to the facade instead of branching on raw strip/palette types.
- Treat “collapsed but hidden” as a real persisted state rather than as a reset.
- Keep note-driven revival behavior gated by palette state, not by global strip availability.
- Preserve origin when resizing floating panels; avoid recentering after every content-size change.
- When moving controls from inline layout to overlay layout, remove their old constraints completely instead of trying to reuse them.
- For hover-only corner controls, separate layout concerns from visibility concerns.
- After crashes involving view hierarchy or constraints, assume later UI errors may be follow-on failures until the first exception is fixed.

### Behavior intentionally disabled

- The old independent menubar waveform-strip behavior is no longer the primary model.
- The strip still exists, but it should behave as the collapsed state of `PianoWaveformPalette`.
- Do not reintroduce unconditional note-driven strip showing unless that behavior is explicitly wanted again.

### Future work areas

- Wire the collapsed `Dock` button to any broader palette-level docking semantics if the expanded state should also understand docking.
- Consider whether collapsed-state `Close` should remain hidden-state collapsed, or switch the conceptual state elsewhere.
- If hover-only controls still appear immediately on show, investigate initial tracking-area / pointer-enter timing rather than button styling.
