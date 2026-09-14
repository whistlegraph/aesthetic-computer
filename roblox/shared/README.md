# Shared Roblox components

`UI.bitmapLabel` uses original AC `font_1` glyphs first and the Unifont-derived
`AC Unicode fallback` for missing glyphs, preserving each glyph’s advance width.
The fallback includes the GNU Unifont 17.0.05 Basic Multilingual Plane data,
including Danish, symbols and wide CJK characters. Unknown upper-plane glyphs
retain native text fallback. This is a pixel renderer, not an imported font file.
It does not implement bidirectional layout, script shaping or combining placement.

`node roblox/shared/build-unifont.mjs` regenerates the compact hex data and lazy
glyph decoder from the pinned upstream release. Source URL, SHA-256 and full SIL
OFL license are in `licenses/`; the full license is also embedded in the module
published with the game. Only glyphs actually displayed are decoded into runs.
The place grows by approximately 3.8 MB to include Plane 0 coverage.

`/tmp/ac-luau/luau roblox/arena/tests/unifont.luau` is the local verification used
on Blueberry; use an installed Luau interpreter for a portable invocation.

`GrenadeMath`, `BlastFX` and `ShoveAnimation` share trajectories, explosion visuals
and procedural throwing between the arena’s player and bot.
