# AC Easel Unifont

GNU Unifont 16.0.03, converted directly from the BDF served by Aesthetic Computer's
`bdf-glyph` loader. This terminal subset keeps 2,923 glyphs: narrow Latin, Greek,
Cyrillic, punctuation, mathematics, box lines, blocks, and Braille. Omitted and
wide glyphs use the system fallback; this is not full GNU Unifont coverage.

```css
@font-face {
  font-family: "AC Easel Unifont";
  src: url("./fonts/ac-easel-unifont.woff") format("woff");
  font-weight: 400;
  font-style: normal;
  font-display: swap;
}
```

For xterm: `fontFamily: '"AC Easel Unifont", Menlo, monospace'`, `fontSize: 16`,
`lineHeight: 1`, `letterSpacing: 0`, and both normal/bold weights `'400'`.
Load with `await document.fonts.load('16px "AC Easel Unifont"')` before fitting.
At 100% browser zoom ASCII occupies its native **8×16 CSS pixels**. No doubled
font scale is needed. Device-pixel ratio maps CSS pixels to the physical display.

Each BDF pixel becomes one square outline; `generate-unifont.py --check` verifies
every glyph against the original bitmap plus font metrics and WOFF parity.
Generation requires isolated `fonttools==4.65.0`; runtime does not. The original
compressed BDF and source/output hashes are bundled for reproducibility.

Unifont is dual licensed upstream. This subset, conversion script and outputs
are distributed under **SIL OFL 1.1**, with the upstream copyright retained.
See `UNIFONT-OFL-1.1.txt`, `UNIFONT-COPYRIGHT.txt`, and `unifont-provenance.json`.
This font license is distinct from Easel's application license and the older
public-domain Misc Fixed font in this directory.
