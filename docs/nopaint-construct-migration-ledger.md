# No Paint Construct operation migration ledger

The surviving export has no `.c3p` source or source map. Its authorities are
`nopaint.art/data.json`, the expression table in
`nopaint.art/scripts/c3runtime.js`, the sprite sheets, and `nopaint.art/media/`.

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

Exact disk-name conflicts are `blur`, `box`, `camera`, `line`, `noise`,
`stamp`, and `wipe`. They are deliberately not recreated under those names.
The existing AC `line` contract remains usable by No Paint, but is not claimed
as a migration of the Construct implementation.

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

Dark Window selects and plays each of its four original note samples. Build,
Wafer, and Caterpillar have multi-cue event vocabularies, while the current No
Paint conductor owns one active brush sample at a time. Their primary sample is
wired; exact per-event cue sequencing requires a conductor audio-timeline API.
