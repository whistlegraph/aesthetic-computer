# Simplified Brief — SIGGRAPH Asia 2026 Tech

Date: 2026-05-07

## One-sentence version

AC Native OS makes interactive pieces feel faster by removing the layers that
quietly hold the next input event, video frame, or audio period.

## Keep

- **Browser to native:** `notepat` at 417 ms in the browser made the OS
  necessary.
- **Compositor to DRM-direct:** removing Cage/Wayland made the runtime own
  input and presentation timing.
- **SDL stream to CoreAudio direct:** the same hardware dropped from 6.47 ms
  median to 0.65 ms when one audio abstraction was removed.
- **HDA vs SOF:** HDA-direct can run near the musical floor; SOF hardware
  forces larger buffers even when Linux and the synth are ready.

## Cut or compress

- Long phase-by-phase commit history.
- Named personal framing that deanonymizes the paper.
- AC lore that does not explain a measured latency turn.
- Future-work lists that read like optimization chores.

## Proposed paper shape

1. **Introduction:** latency is not one number. It is the sum of whoever owns
   the next input event, video frame, and audio period.
2. **System:** describe the runtime only as much as needed: input, display,
   audio, and the test piece.
3. **Measurements:** browser baseline, HDA native, SOF native, macOS SDL,
   macOS CoreAudio direct.
4. **Four buffer turns:** browser, compositor, audio abstraction, firmware.
5. **Design rule:** make buffering optional and visible.
6. **Limits:** hardware can still impose a floor; SOF is the example.
7. **Conclusion:** low latency is mostly architectural subtraction.

## Abstract seed

Interactive creative systems often lose responsiveness not through expensive
algorithms, but through layers that quietly buffer input, display, or audio.
This paper studies a small creative runtime built for keyboard-driven visual
and musical pieces, following a keypress from device event to rendered frame
and audio output. Across browser, Linux, and macOS implementations, the largest
latency reductions came from removing whole buffer turnarounds: leaving the
browser runtime, replacing a kiosk compositor with DRM-direct presentation,
and replacing an SDL audio stream with direct CoreAudio output. On Linux, the
runtime reaches roughly 3-4 ms key-to-DAC latency on HDA-direct hardware, while
Sound Open Firmware platforms require larger buffers that set a much higher
floor. The result is a practical rule for real-time creative tools: optimize
only after making every owner of the next frame or audio period visible.

## What the platter contributes

The papers platter keeps the argument grounded. It gives this submission its
source materials: the latency paper, the Notepat history, the OS paper, and the
voice guide. Use the platter to choose evidence, not to add scope. The rewrite
should feel like one clean card pulled from a wall full of material.
