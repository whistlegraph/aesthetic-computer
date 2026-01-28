# Video Underlay + UI Layering — Diagnostics (2026-01-28)

## Summary of request
- Underlay video should sit **behind** the UI and any in-canvas controls.
- UI buttons must remain visible during presentation.
- “RECORDING” indicators should be overlay-only, not baked into the buffer.
- Black/opaque background and subtle border should not obscure the video.

## Current layering model
- Underlay: DOM element inserted during recorder presentation.
- Main canvas: draws piece UI (buttons, labels) and transparent background in video.mjs.
- Glaze canvas: WebGL effects layer (opaque by default due to alpha: false).
- UI canvas: HUD controls and overlays (z-index 6).
- Recording UI canvas: overlay-only by design (not captured).

## Findings
1. Glaze canvas is opaque by construction (WebGL2 alpha: false), so it must be hidden during presentation.
2. The draw loop applies a global `opacity = 0.95` whenever an underlay exists, which darkens the entire canvas and creates the “black wash” impression.
3. Body has a checkerboard background (dark) which can appear at the edges if any layout gaps exist.
4. The video piece already uses `wipe(0,0,0,0)` when presenting to keep the buffer transparent.

## Changes applied
### 1) Stop dimming the main canvas during underlay playback
- Remove forced `opacity: 0.95` during `underlayFrame` playback.
- Ensure canvas remains transparent only while presenting.

### 2) Hide glaze canvas during presentation
- Keep main canvas visible for UI drawing.
- Hide glaze canvas (display none) while underlay is active.

### 3) Keep wrapper background transparent while presenting
- Avoid body or wrapper backgrounds bleeding through.

## Open questions
- If “RECORDING” is still appearing in-buffer, confirm whether it is being drawn by the piece or by a non-overlay pipeline in BIOS.
- If residual borders remain, validate whether #aesthetic-computer has any scaling or letterboxing logic that can expose the body background.

## Next validation checklist
- Present a tape in video.mjs and confirm:
  - Buttons are visible (drawn by the piece).
  - The underlay video is fully visible beneath the buffer.
  - No darkening of the canvas when presenting.
  - “RECORDING” appears only in overlay (recording UI canvas), not in recorded frames.
