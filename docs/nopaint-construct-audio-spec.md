# No Paint Construct interaction/audio specification

This document treats the preserved Construct export as evidence for No Paint's
interaction grammar. The canonical source is
`system/public/nopaint.art/data.json`: `project[6]` contains event sheets,
`project[7]` contains the audio manifest, and object `189`, action `95` is
Audio `PlayByName`. Runtime playback is implemented in
`scripts/c3runtime.js` around lines 1897–1904.

## Decision controls

| Control phase | Canonical asset | Event-sheet evidence |
|---|---|---|
| No down | `generic - no button pressed (metal brush)` | Core / `CheckButtonsCore`, event 270 |
| No held crossing | `generic - no button pressed (metal brush)` | Core / `CheckButtonsCore`, event 270 |
| No release | `generic - no button released (middle)` | Core / `No & Paint` / `No` |
| Paint down | `generic - paint button pressed (psst)` | Core / `CheckButtonsCore`, event 281 |
| Paint held crossing | `generic - paint button pressed (psst)` | Core / `CheckButtonsCore`, event 281 |
| Paint release | `generic - paint button released (cha)` | Core / `No & Paint` / `Paint` |
| Passive hover | `generic - button rollover` | Core / `CheckButtonsCore`, events 278 and 289 |

A pointer may go down on one decision, remain held, cross the boundary, and
release on the other. Crossing replays the destination's press cue; only the
control under release performs the decision. Passive mouse hover uses the
rollover cue once on entry rather than on every movement tick.

The Construct sheets contain no passive hover condition for `Painting`,
`PaintingBuffer`, or `PaintingOverlay`. No Paint 3.0 deliberately extends the
grammar by playing the same restrained rollover sample once when the pointer
enters the painting. This is a new behavior requested for the native version,
not a claim of restoration fidelity.

## Pause, completion, and saving

| Action | Canonical asset |
|---|---|
| Pause down | `generic - pressing pause` |
| Enter pause | `generic - entering pause` |
| Leave pause / Back | `generic - pause release` |
| Generic control down/up | `generic - button press` / `generic - button release` |
| Save down/up | `generic - save button pressed` / `generic - save button released` |
| Save processing | `generic - processing save` (loop) |
| Save complete | `generic - saved` |

In No Paint 3.0, tapping the painting enters completion mode. Back returns to
the decision loop; tapping the painting again is the same Back transition.
Done invokes the existing AC prompt `done` command. Space and a drag beginning
on the painting both perform pause: pressing plays `pressing pause`, entering
plays `entering pause`, and resuming plays `pause release`.

Long brush/theme playback is proposal-owned. It must be ended when a proposal
is replaced, the piece pauses, completion mode opens, the seed changes, or the
piece exits. Resuming restarts the current proposal's cue. One-shot decision
and primitive samples end naturally.

No Paint 3.0 adds a held-decision inspection state: while No or Paint remains
down, proposal-frame stepping stops and the active proposal sample is reduced
to `sampleSpeed: 0.18` for a scratch-like slowdown. Sliding between decisions
preserves the hold. Release chooses the destination and the next proposal begins
at normal speed; cancellation restores the current proposal to speed `1`.

## Brush and tool cues

Primitive starts are `box - start`, `line - start`, `triangle - start`, and the
historically misspelled `elipse - start`; their common release is
`primitive - released`. Theme-on-start tools use their matching theme assets:
Rainbow, Grid Worm, Banner, Quicksand, Light Bump, Contrast, Blur, Spin,
Scroll (`scroll - theme 2021`), Mirror, Aura, Breathe, Vignette, Saturate,
Sharpen, and Bubbles. Additional implemented native mappings are Softy
`softy - landed`, Wafer `wafer - nibble appear`, Wash
`wipe - individual wipe`, and Camera `camera - fx`.

The original background loops are `no paint - theme` and
`no paint - painting music`. They are not automatically enabled by the native
piece; ambient music requires a separate product decision from interaction
cues.

## Source-map limits

No `.map`, `.c3p`, or `sourceMappingURL` survives in the export. A conventional
JavaScript source map could only improve Construct engine/custom-script symbol
provenance. It cannot reconstruct declarative event-sheet meaning, which is
encoded in `data.json` and referenced by runtime IDs.

The useful map is therefore semantic: JSON path → sheet/group/function/event →
condition/action → audio asset. Exact audited decision paths include:

- No release: `project.6.39.1.44.8.0.7.3.6.0.1.0`
- Paint release: `project.6.39.1.44.8.1.7.9.6.0.1.0`
- No rollover: `project.6.39.1.50.8.1.8.2.8.0.8.0.7.0.6.0.1.0`
- Paint rollover: `project.6.39.1.50.8.2.8.2.8.0.8.0.7.0.6.0.1.0`

## Verification contract

For every mapped cue:

1. The asset exists and appears in the Construct audio manifest.
2. AC preloads it from the same-origin `/nopaint.art/media/` archive mount.
3. The browser journey records `path: "legacy"`; a synth fallback cannot make
   the legacy-sample assertion pass.
4. Held-pointer tests cover down, rollover, release, and the resulting decision.
5. Assets under `media/unused - ...` are noncanonical unless new evidence says
   otherwise.
