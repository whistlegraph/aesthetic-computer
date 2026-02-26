# Study: l5lua.org Visual Style + Logo

Date: 2026-02-26
Source: https://l5lua.org/

## Snapshot
- Framework: MkDocs Material
- Theme flags on `<body>`:
  - `data-md-color-scheme="default"`
  - `data-md-color-primary="gold"`
  - `data-md-color-accent="blue"`
- Header/logo asset:
  - `assets/L5-logo-blob.png`
  - Downloaded size: 323x203 PNG

## Key CSS Signals
From page source + `stylesheets/extra.css`:
- Header is white with gold radial-dot pattern.
- Gold is the dominant brand color:
  - `#FFD700` and transparent variants.
- Strong black/white contrast for structure.
- Code blocks use black background with gold text in one pass of custom styles.
- Font direction:
  - Sans body stack (`Manrope`, system sans)
  - Monospace for code (Consolas/Menlo/Courier New)

## Practical Token Set
For AC `/l5` alignment, these are the useful tokens:
- `--l5-gold: #FFD700`
- `--l5-gold-soft: #FFEE57`
- `--l5-black: #111111`
- `--l5-white: #FFFFFF`
- Pattern: repeating radial gold dots over white background.

## Structural Patterns Worth Mirroring
- Bold high-contrast header treatment.
- Gold-accented nav/controls.
- Black borders as framing language.
- Monospace code blocks with dark inversion.

## Logo Notes
- File captured locally for AC page integration:
  - `system/public/l5.aesthetic.computer/l5-logo-blob.png`
- Recommendation:
  - Keep original proportions.
  - Place in hero and/or top-left brand block.
  - Surround with black-line frame and gold backdrop for cohesion.

## AC Application Decision
Apply a yellow/black/white theme to `/l5` with:
- Dot-pattern white background
- Black-lined panels
- Gold interactive states
- Black code blocks with gold text
- Local L5 logo in hero
