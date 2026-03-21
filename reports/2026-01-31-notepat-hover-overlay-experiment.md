# Notepat hover overlay experiment

Date: 2026-01-31

## Goal
Give new users immediate visual feedback on hover/near-hover so they can discover controls and understand mappings without reading instructions.

## Scope
Applies to the Notepat UI only, starting with mouse hover (desktop) and optionally extending to touch proximity/press states later.

## UX proposal (high level)
- **Hover highlight**: Any interactive element (pads, top bar buttons, mini piano, waveform selector, metronome controls, slider bars) lightly tints and shows a subtle border glow on hover.
- **Context cue**: When hovering a pad or key, show a faint “ghost note” indicator in the mini bar (e.g., note label + key) without triggering sound.
- **Hover grouping**: Highlight related elements (e.g., hover BPM toggles lightly highlight the metronome segment in the mini bar).
- **Consistency**: Use a single hover theme (color + alpha) derived from existing palette per control type.

## Implementation plan (phased)
1. **Hover state plumbing**
   - Standardize hover detection for all buttons/rects in `notepat.mjs` by ensuring each `btn` sets `over` consistently.
   - Add a `hoveredControl` registry (id + type + note) updated during paint/act.
2. **Visual layer for hover**
   - Add a `drawHoverOverlay()` pass at end of `paint()` (before tooltips) that uses `hoveredControl`.
   - For pads/keys: draw a translucent overlay with subtle outline (alpha 30–60).
   - For top bar/mini bar buttons: highlight segment backgrounds with a gentle tint.
3. **Ghost note preview**
   - When hovering a key/pad, render a short label in the mini bar (e.g., “C# / W”) and a faint note color chip.
   - Do **not** change audio state.
4. **Newbie-first defaults**
   - Default enabled for desktop pointer devices.
   - Auto-disable after first successful interaction sequence (e.g., after 10 notes) or on explicit toggle.

## Mapping (initial targets)
- **Pads**: 24 main pads → overlay on `btn.over` + ghost note preview in mini bar.
- **Mini piano (top bar)**: hover key highlights key fill + subtle strip glow.
- **Metronome controls**: hover BPM/-/+ shows segment tint and soft glow.
- **Wave selector**: hover shows elevated border and text emphasis.
- **Room/Glitch/Quick toggles**: hover shows tinted background + underline.

## Telemetry (optional)
- Count hover events and first-time interactions to validate onboarding impact.
- Track whether hover cues lead to a first note within 2 seconds.

## Risks
- Visual noise in compact layouts; mitigate with alpha caps and hide in recital or projector modes.
- Performance: avoid per-frame recompute by caching hover rects and only updating on pointer movement.

## Rollout
- Add a URL flag (e.g., `?hover=1`) for testing.
- Ship as default for desktop; keep a toggle in settings or key command (`H`) for power users.

## Next steps
- Identify UI elements lacking `btn.over` coverage and unify.
- Implement `hoveredControl` registry in `act()` + `paint()`.
- Add `drawHoverOverlay()` and integrate in render order.
