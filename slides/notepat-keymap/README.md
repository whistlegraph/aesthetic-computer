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
```

Deps: `qrencode` (`brew install qrencode`), `sips` (built-in), `python3`,
Google Chrome at `/Applications/Google Chrome.app`.

## Files

- `template.html` — the slide layout. Image src attributes use placeholder
  strings (`__QR_NOTEPAT__`, `__QR_MENUBAND__`, `__PALS_LOGO__`,
  `__JEFFREY_PIC__`); `build.fish` substitutes data URIs at render time.
- `build.fish` — generates QRs, crops portrait, injects data URIs, runs
  headless Chrome to capture the PNG.

## Editing

Open `template.html` directly in a browser to iterate on layout — the
placeholders render as broken-image icons but everything else works.
Run `build.fish` to produce the final PNG with all assets embedded.

The piano + QWERTY positions are hard-coded so any change to key sizing
needs matching updates to the absolute `left:` values in the piano keys
and to `.qrow` padding-left for the QWERTY stagger.
