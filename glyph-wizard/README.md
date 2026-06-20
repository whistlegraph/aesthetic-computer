# glyph-wizard

**GlyphWizard** — a homegrown macOS type-authoring wizard for **Aesthetic Inc**.
From-scratch *parametric* glyph generation: you author glyph **skeletons**
(centerlines); the **TypeWizard** axes expand them into filled outlines, live.
Then export a UFO and compile a real font.

Sibling to `date-wizard/`, `wave-wizard/`, `clip-wizard/`, `juke-wizard/`,
`shot-wizard/` — same conventions: a Swift Package executableTarget, AppKit
(`NSApplication` + `AppDelegate` + custom-drawn `NSView`). **Not SwiftUI.**

## Build & run
```
swift build
swift run GlyphWizard
```
Requires macOS 12+ and a Swift 5.9 toolchain.

## The pipeline
```
SKELETON ──▶ EXPANDER ──▶ TypeWizard axes ──▶ UFO export ──▶ fontTools ──▶ OTF/TTF
(centerlines) (offset by    (weight·contrast·   (.ufo)        (bin/        (real font)
              a pen)         width·slant·x-ht)                 compile.sh)
```

- **GlyphEngine.swift** — the model (`Glyph` = `Stroke` skeletons), the stroke
  **Expander** (offsets each centerline by a pen whose width tracks the local
  tangent → contrast; closed skeletons become annuli so counters stay open),
  and the v0.1 starting skeletons for `r e g a d`.
- **CanvasView.swift** — draws the word from skeletons + axes; the skeleton
  overlay is **draggable** — move points to author letterforms live.
- **AppDelegate.swift** — the TypeWizard panel (weight, contrast, width, slant,
  x-height, tracking, round terminals) + wordmark field + UFO export.
- **UFOExport.swift** — writes a UFO3 package with the axes baked in.

## Compile a font
```
bin/compile.sh Regarde.ufo      # → Regarde.otf / Regarde.ttf
```
Needs fontmake: `pipx install fontmake` (pulls fontTools + ufo2ft).

## Status — v0.1
Engine + live editable canvas + axes + UFO export are in. The `r e g a d`
skeletons are rough placeholders — refine them in-app (drag the points). Next:
proper apertures (e/a/g), per-glyph contrast modeling, the **platter** of
reference specimens carried over from the regarde prototype, and a GlyphWizard
(per-glyph editor) / TypeWizard (global) split mirroring the /pop wizards.
