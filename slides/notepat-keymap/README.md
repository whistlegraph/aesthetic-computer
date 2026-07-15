# notepat keymap slide

Single 16:9 image (2400×1350, Google Slides retina) explaining the
notepat keyboard layout — piano keyboard up top with the QWERTY letter
that plays each note, then the QWERTY layout itself with hand-split
tint, then a "why this layout" rationale and the implementation list
with QR codes to `notepat.com` and `prompt.ac/menuband`.

Designed for casey's social-software meeting (April 2026) but reusable
as a template for future AC slides.

## Build

```fish
./build.fish                                       # → notepat-keymap.png
./build.fish ~/Desktop/notepat-keymap.png          # → custom path
./build-figure.fish                                # → color figure only
./build-figure-bw.fish                             # → shaded B&W figure
./build-figure-bw-line.fish                        # → stripped line-art figure
./build-bw.fish canonical-bw-notepat-keymap.html canonical-bw-notepat-keymap-musical-below.png 4 0.70 154.286 bottom
./build-bw.fish canonical-bw-notepat-keymap.html canonical-bw-notepat-keymap-musical-above.png 4 0.70 154.286 top
```

The canonical B&W figure has matching keyboard widths, Mac-style modifier and
arrow keys, and separate musical-keyboard-above and -below outputs. The generic
renderer accepts optional output path, device scale factor, piano scale, octave
gap, and piano position arguments after the template path.

Deps: `qrencode` (`brew install qrencode`) and `sips` for the complete slide;
`python3`, ImageMagick, and Google Chrome at `/Applications/Google Chrome.app`
for the figure renders.

## Files

- `template.html` — the slide layout. Image src attributes use placeholder
  strings (`__QR_NOTEPAT__`, `__QR_MENUBAND__`, `__PALS_LOGO__`,
  `__JEFFREY_PIC__`); `build.fish` substitutes data URIs at render time.
- `build.fish` — generates QRs, crops portrait, injects data URIs, runs
  headless Chrome to capture the PNG.
- `canonical-bw-notepat-keymap.html` — canonical monochrome figure source,
  designed for legible halftone printing at small sizes.
- `canonical-bw-notepat-keymap-musical-{above,below}.png` — canonical 4x PNG
  exports with the musical keyboard above or below the Mac keyboard.
- `build-figure*.fish` — fixed renderers for the primary figure variants.
- `build-bw.fish` — generic renderer for B&W HTML variants, including font
  embedding, print scaling, and tight whitespace trimming.

## Editing

Open `template.html` directly in a browser to iterate on layout — the
placeholders render as broken-image icons but everything else works.
Run `build.fish` to produce the final PNG with all assets embedded.

The piano + QWERTY positions are hard-coded so any change to key sizing
needs matching updates to the absolute `left:` values in the piano keys
and to `.qrow` padding-left for the QWERTY stagger.
