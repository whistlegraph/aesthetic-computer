# L5Lua.org Style Study

Date: 2026-02-26
Source: https://l5lua.org/

## Stack
- Site generator appears to be `mkdocs` with `mkdocs-material` theme.
- HTML reports `mkdocs-1.6.1` and `mkdocs-material-9.7.3`.
- Primary CSS files:
  - `https://l5lua.org/assets/stylesheets/main.484c7ddc.min.css`
  - `https://l5lua.org/assets/stylesheets/palette.ab4e12ef.min.css`
- Additional override stylesheet:
  - `https://l5lua.org/stylesheets/extra.css`

## Palette + Theme Tokens
- Page theme attributes on body:
  - `data-md-color-scheme="default"`
  - `data-md-color-primary="gold"`
  - `data-md-color-accent="blue"`
- Material palette token values include:
  - `--md-primary-fg-color: #ffec3d` (gold variant)
  - `--md-primary-fg-color--light: #ffee57`
  - `--md-primary-bg-color: #000000de` (dark text/background contrast token)
  - `--md-accent-fg-color: #4287ff` (blue accent)

## Typography + Code Treatment
- `extra.css` declares custom body/typeface as `Manrope`.
- Body and typeset stacks use sans-serif for prose.
- Code/mono stack in `extra.css` uses `Consolas, Menlo, Monaco, 'Courier New', monospace`.
- Code blocks are styled with high-contrast black/yellow treatment in overrides:
  - dark background (`black`)
  - bright foreground (`gold`)

## Logo
- Logo asset reference:
  - `https://l5lua.org/assets/L5-logo-blob.png`
- Used for favicon and nav logo.
- Observed dimensions from asset metadata: `323 x 203` PNG.
- Visual character: grayscale/black-white blob mark, no chroma in logo pixels.

## Header / Background Motif
- `extra.css` applies a yellow dot-field style via layered radial gradients:
  - `radial-gradient(circle at 40% 40%, #FFD700 25%, transparent 26%)`
  - `radial-gradient(circle at 60% 60%, #FFD70080 25%, transparent 26%)`
  - `background-size: 6em 6em`
- Header uses white base + black text with the yellow dot motif.

## AC L5 Page Direction (Applied)
- Keep yellow/black/white as primary visual language.
- Use the radial dot motif as an atmospheric background/header treatment.
- Keep high-contrast code/editor surface (dark panel + yellow highlights).
- Keep logo usage explicit and resilient through absolute asset path fallback.
