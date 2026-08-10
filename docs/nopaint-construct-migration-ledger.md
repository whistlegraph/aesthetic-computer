# No Paint Construct operation migration ledger

The surviving export has no `.c3p` source or source map. Its authorities are
`nopaint.art/data.json`, the expression table in
`nopaint.art/scripts/c3runtime.js`, the sprite sheets, and `nopaint.art/media/`.

## Reading the export

No numeric constant is stored in `data.json`. An event parameter is
`[type, [expressionNumber, ...nodes]]`, and the value lives in
`C3_ExpressionFuncs[expressionNumber]` — a compiled function inside
`c3runtime.js`. Two tools make that readable, and a recovered constant is not
recovered until one of them has been run:

```sh
node toolchain/nopaint/decode-sheet.mjs Caterpillar   # a sheet, expressions resolved
node toolchain/nopaint/expressions.mjs 376            # one expression by number
node toolchain/nopaint/expressions.mjs --grep hsla    # search the table
```

Reading raw index numbers as if they were literals is how earlier passes
produced constants that were never in the original. Two are corrected below.

The canonical picker vocabulary contains 38 names:

`Aura, Banner, Blur, Breathe, Bubbles, Build, Box, Camera, Caterpillar,
Contrast, Dark Window, Ellipse, Flip, Grid Worm, Invert, Light Bump, Line,
Load, Mirror, Noise, Quicksand, Rainbow, Recurse, Saturate, Scroll, Sharpen,
Softy, Spin, Stamp, Turn, Triangle, Vignette, Wafer, Walker, Wipe, Zoom, Frame,
Playlist`.

## Brush migration

The non-conflicting brush names migrated as deterministic proposal contracts
are `aura`, `banner`, `breathe`, `bubbles`, `build`, `caterpillar`,
`dark-window`, `ellipse`, `frame`, `grid-worm`, `rainbow`, `softy`, `triangle`,
`vignette`, `wafer`, and `walker`.

Exact disk-name conflicts are `blur`, `box`, `camera`, `line`, `noise`, and
`wipe`. They are deliberately not recreated under those names. The existing AC
`line` contract remains usable by No Paint, but is not claimed as a migration
of the Construct implementation.

`stamp` was on that list until the existing `disks/stamp.mjs` grew a
`nopaintProposal` beside its own brush, the way `box` already had.

## Pieces that own their slug

`bubbles`, `walker`, `dark-window`, `line`, `box`, `stamp`, `frame`, and
`caterpillar` are real AC pieces that export their own `nopaintProposal`;
`disks/nopaint.mjs` imports each one and registers it after the fallback
catalog, so the piece's contract wins by slug. A name leaves
`nopaint-construct-catalog.mjs` on the day a piece takes it over — that catalog
shrinking to nothing is the finish line.

## Corrections

Two constants in the first pass came from misreading expression indices as
values. Both are now read out of `C3_ExpressionFuncs`:

- **Frame** cycles every **1** second, not 5, starting at index 1 and advancing
  `(frameIndex + 1) % AnimationFrameCount`. The knock plays at a
  `random(.25, 2)` playback rate each cycle.
- **Caterpillar** has no `1 / 3 / 32` segment choice and no `.2 / .7` scale
  range. `length = ProcessNumericParameter(1, 3, 32) - 1` is parameter slot 1
  over the range 3–32, and `.2`–`.7` is `saturation = random(.2, .7)`. Asking
  for seven segments (`Caterpillar: 7`) is the original rainbow-road easter
  egg: `length == 6` swaps the cue and cycles hue along the body instead of
  fading lightness.

The safe image filters/transforms `contrast`, `flip`, `invert`, `light-bump`,
`mirror`, `quicksand`, `recurse`, `saturate`, `scroll`, `sharpen`, `spin`,
`turn`, and `zoom` are also migrated and wired into the proposal conductor.
`load` and `playlist` are control/meta operations and are deliberately not
presented as visual brushes.

## Fidelity boundary

Recovered score constants, deterministic state, timing values, grid/frame
choices, and available primary sound samples are retained. Construct-only
effects including Exclusion blending, Bulge, AdjustHSL, Vignette and sprite
timeline behaviors have no equivalent in the current AC ink API. Their AC
renderers use bounded translucent primitives and do not claim pixel parity.

Dark Window selects and plays each of its four original note samples, and
Caterpillar picks between `trotting along` and `rain bow road` from its own
score. Build and Wafer have multi-cue event vocabularies, while the current No
Paint conductor owns one active brush sample at a time. Their primary sample is
wired; exact per-event cue sequencing requires a conductor audio-timeline API.

Stamp's `mirrored` is real: Construct mirrored on a negative X scale, and
`paste` now reaches that by handing `grid` a per-axis `{x, y}` scale.
`grid` had the flip scaffolding already but measured each reversed cell
backwards, so every negative-scale draw came out empty; `spritePaste` flips the
frame origin to `1 - ox` to keep a mirrored sprite in the same box.

Two recovered behaviours are still carried in the score but not drawn, because
the AC surface has no equivalent yet: Caterpillar's per-segment tint
(Construct's Set color — `paste` has no colour multiply) and Frame's per-cycle
knock playback rate.
