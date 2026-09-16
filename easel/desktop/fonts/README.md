# AC Easel Pixel

An exact square-outline conversion of the public-domain Misc Fixed 6×10 bitmap
font used by AC native (`fedac/native/src/font-6x10.h`). Bundles the full upstream
Unicode source (1,597 glyphs), including accented Latin, Greek, Cyrillic, box
lines, and blocks. Missing characters fall back to the platform font.

```css
@font-face {
  font-family: "AC Easel Pixel";
  src: url("./fonts/ac-easel-pixel.woff") format("woff");
  font-style: normal;
  font-weight: 400;
  font-display: swap;
}
```

For xterm use `fontFamily: '"AC Easel Pixel", Menlo, monospace'`,
`fontSize: 20`, `lineHeight: 1`, `letterSpacing: 0`, `fontWeight: '400'`, and
`fontWeightBold: '400'`. This makes a 12×20 CSS-pixel cell (2× original pixels).
Use 10/20/30 px at browser zoom 100% for integer scaling; arbitrary fractional
zoom or sizes may soften the outlines. The TTF is also included for native use.
Wait for `document.fonts.load('20px "AC Easel Pixel"')` before fitting xterm.
Menlo is a system fallback; it is not bundled.

Rebuild with Python and `fonttools==4.65.0` in a separate environment:

```sh
python generate.py
python generate.py --check
```

Generation preserves each source pixel as a clockwise rectangular contour,
with 600-unit fixed advances in a 1000-unit em (ascent 800, descent 200).
The check compares all 1,597 glyph outlines to the BDF pixels, checks fixed
metrics, and confirms TTF/WOFF character-map parity. Font timestamps are fixed
so the same tool version produces identical files. Runtime needs no fonttools.

`provenance.json` records source and generated hashes. See `LICENSE.txt` for the
font-specific public-domain notice, which is distinct from Easel's license.
